{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Generic.Inference
( tag
, probs
, marginals
, expectedFeaturesIn
, accuracy
, zx
, zx'
) where

import Data.Ord (comparing)
import Data.List (maximumBy)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Number.LogFloat as L

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)

import Data.CRF.Chain2.Generic.Base
import Data.CRF.Chain2.Generic.Model
import Data.CRF.Chain2.Generic.Util (partition)
import qualified Data.CRF.Chain2.Generic.DP as DP


-- Interface on top of internal implementation

-- | Accumulation function.
type AccF = [L.LogFloat] -> L.LogFloat

type ProbArray = LbIx -> LbIx -> LbIx -> L.LogFloat

computePsi :: Ord f => Model o t f -> Xs o t -> Int -> LbIx -> L.LogFloat
computePsi crf xs i = (A.!) $ A.array (0, lbNum xs i - 1)
    [ (k, onWord crf xs i k)
    | k <- lbIxs xs i ]

forward :: Ord f => AccF -> Model o t f -> Xs o t -> ProbArray
forward acc crf sent = alpha where
    alpha = DP.flexible3 (-1, V.length sent - 1)
                (\i   -> (0, lbNum sent i - 1))
                (\i _ -> (0, lbNum sent (i - 1) - 1))
                (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi alpha i j k
        | i == -1 = 1.0
        | otherwise = acc
            [ alpha (i - 1) k h * psi j
            * onTransition crf sent i j k h
            | h <- lbIxs sent (i - 2) ]

backward :: Ord f => AccF -> Model o t f -> Xs o t -> ProbArray
backward acc crf sent = beta where
    beta = DP.flexible3 (0, V.length sent)
               (\i   -> (0, lbNum sent (i - 1) - 1))
               (\i _ -> (0, lbNum sent (i - 2) - 1))
               (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi beta i j k
        | i == V.length sent = 1.0
        | otherwise = acc
            [ beta (i + 1) h j * psi h
            * onTransition crf sent i h j k
            | h <- lbIxs sent i ]

zxBeta :: ProbArray -> L.LogFloat
zxBeta beta = beta 0 0 0

zxAlpha :: AccF -> Xs o t -> ProbArray -> L.LogFloat
zxAlpha acc sent alpha = acc
    [ alpha (n - 1) i j
    | i <- lbIxs sent (n - 1)
    , j <- lbIxs sent (n - 2) ]
    -- | (i, j) <- lbIxs2 sent (n - 1) ]
    where n = V.length sent

zx :: Ord f => Model o t f -> Xs o t -> L.LogFloat
zx crf = zxBeta . backward sum crf

zx' :: Ord f => Model o t f -> Xs o t -> L.LogFloat
zx' crf sent = zxAlpha sum sent (forward sum crf sent)

argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

tagIxs :: Ord f => Model o t f -> Xs o t -> [Int]
tagIxs crf sent = collectMaxArg (0, 0, 0) [] mem where
    mem = DP.flexible3 (0, V.length sent)
                       (\i   -> (0, lbNum sent (i - 1) - 1))
                       (\i _ -> (0, lbNum sent (i - 2) - 1))
                       (\t i -> withMem (computePsi crf sent i) t i)
    withMem psiMem mem i j k
        | i == V.length sent = (-1, 1)
        | otherwise = argmax eval $ lbIxs sent i
        where eval h =
                  (snd $ mem (i + 1) h j) * psiMem h
                  * onTransition crf sent i h j k
    collectMaxArg (i, j, k) acc mem =
        collect $ mem i j k
        where collect (h, _)
                  | h == -1 = reverse acc
                  | otherwise = collectMaxArg (i + 1, h, j) (h:acc) mem

tag :: Ord f => Model o t f -> Xs o t -> [t]
tag crf sent =
    let ixs = tagIxs crf sent
    in  [lbAt x i | (x, i) <- zip (V.toList sent) ixs]

probs :: Ord f => Model o t f -> Xs o t -> [[L.LogFloat]]
probs crf sent =
    let alpha = forward maximum crf sent
        beta = backward maximum crf sent
        normalize xs =
            let d = - sum xs
            in map (*d) xs
        m1 k x = maximum
            [ alpha k x y * beta (k + 1) x y
            | y <- lbIxs sent (k - 1) ]
    in  [ normalize [m1 i k | k <- lbIxs sent i]
        | i <- [0 .. V.length sent - 1] ]

marginals :: Ord f => Model o t f -> Xs o t -> [[L.LogFloat]]
marginals crf sent =
    let alpha = forward sum crf sent
        beta = backward sum crf sent
    in  [ [ prob1 crf alpha beta sent i k
          | k <- lbIxs sent i ]
        | i <- [0 .. V.length sent - 1] ]

goodAndBad :: (Eq t, Ord f) => Model o t f -> Xs o t -> Ys t -> (Int, Int)
goodAndBad crf xs ys =
    foldl gather (0, 0) $ zip labels labels'
  where
    labels  = [ (best . unY) (ys V.! i)
              | i <- [0 .. V.length ys - 1] ]
    best zs
        | null zs   = Nothing
        | otherwise = Just . fst $ maximumBy (comparing snd) zs
    labels' = map Just $ tag crf xs
    gather (good, bad) (x, y)
        | x == y = (good + 1, bad)
        | otherwise = (good, bad + 1)

goodAndBad' :: (Eq t, Ord f) => Model o t f -> [(Xs o t, Ys t)] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]

-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: (Eq t, Ord f) => Model o t f -> [(Xs o t, Ys t)] -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

prob3
    :: Ord f => Model o t f -> ProbArray -> ProbArray -> Xs o t
    -> Int -> (LbIx -> L.LogFloat) -> LbIx -> LbIx -> LbIx
    -> L.LogFloat
prob3 crf alpha beta sent k psiMem x y z =
    alpha (k - 1) y z * beta (k + 1) x y * psiMem x
    * onTransition crf sent k x y z / zxBeta beta
{-# INLINE prob3 #-}

prob2
    :: Model o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> LbIx -> LbIx -> L.LogFloat
prob2 crf alpha beta sent k x y =
    alpha k x y * beta (k + 1) x y / zxBeta beta
{-# INLINE prob2 #-}

prob1
    :: Model o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> LbIx -> L.LogFloat
prob1 crf alpha beta sent k x = sum
    [ prob2 crf alpha beta sent k x y
    | y <- lbIxs sent (k - 1) ]

expectedFeaturesOn
    :: Ord f => Model o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> [(f, L.LogFloat)]
expectedFeaturesOn crf alpha beta sent k =
    fs3 ++ fs1
    where psi = computePsi crf sent k
          pr1 = prob1 crf alpha beta sent k
          pr3 = prob3 crf alpha beta sent k psi
          fs1 = [ (ft, pr) 
                | a <- lbIxs sent k
                , let pr = pr1 a
                , ft <- obFeatsOn crf sent k a ]
    	  fs3 = [ (ft, pr) 
                | a <- lbIxs sent k
                , b <- lbIxs sent $ k - 1
                , c <- lbIxs sent $ k - 2
                , let pr = pr3 a b c
                , ft <- trFeatsOn crf sent k a b c ]

expectedFeaturesIn :: Ord f => Model o t f -> Xs o t -> [(f, L.LogFloat)]
expectedFeaturesIn crf sent =
    -- force parallel computation of alpha and beta tables
    zx1 `par` zx2 `pseq` zx1 `pseq` concat
      [ expectedFeaturesOn crf alpha beta sent k
      | k <- [0 .. V.length sent - 1] ]
    where alpha = forward sum crf sent
          beta = backward sum crf sent
          zx1 = zxAlpha sum sent alpha
          zx2 = zxBeta beta
