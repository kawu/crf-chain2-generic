{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Generic
(
) where

import Data.Binary (Binary)
import Data.Ord (comparing)
import Data.Maybe (maybeToList)
import Data.List (maximumBy)
import Data.Vector.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Number.LogFloat as L

import Control.Parallel.Strategies (rseq, parMap)
import Control.Parallel (par, pseq)
import GHC.Conc (numCapabilities)

import Data.CRF.Chain2.Util (partition)
import qualified Data.CRF.Chain2.DP as DP

-- | Ascending vector of unique interger elements.
newtype AVec a = AVec { unAVec :: V.Vector a }
    deriving (Show, Read, Eq, Ord, Binary)

-- | Smart AVec constructor which ensures that the
-- underlying vector satisfies the AVec properties.
fromList :: Ord a => [a] -> AVec a
fromList = fromSet . S.fromList 
{-# INLINE fromList #-}

-- | Smart AVec constructor which ensures that the
-- underlying vector satisfies the AVec properties.
fromSet :: Ord a => S.Set a -> AVec a
fromSet = AVec . V.fromList . S.toAscList 
{-# INLINE fromSet #-}

toList :: AVec a -> [a]
toList = V.toList . unAVec 
{-# INLINE toList #-}

-- | Feature generation specification.
data FeatGen o t f = FeatGen
    { obFeats   :: o -> t -> [f]
    , trFeats1  :: t -> [f]
    , trFeats2  :: t -> t -> [f]
    , trFeats3  :: t -> t -> t -> [f] }

-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Eq, Ord, Binary
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )

-- | A conditional random field.
data CRF o t f = CRF
    { values    :: U.Vector Double
    , ixMap     :: M.Map f FeatIx
    , featGen   :: FeatGen o t f }

-- | Potential assigned to the feature -- exponential of the
-- corresonding parameter.
phi :: Ord f => CRF o t f -> f -> L.LogFloat
phi CRF{..} ft = case M.lookup ft ixMap of
    Just ix -> L.logToLogFloat (values U.! unFeatIx ix)
    Nothing -> L.logToLogFloat (0 :: Float)
{-# INLINE phi #-}

newtype Y t = Y { _unY :: AVec (t, Double) }
    deriving (Show, Read, Eq, Ord)

-- | Y constructor.
mkY :: Ord t => [(t, Double)] -> Y t
mkY = Y . fromList
{-# INLINE mkY #-}

-- | Y deconstructor symetric to mkY.
unY :: Y t -> [(t, Double)]
unY = V.toList . unAVec . _unY
{-# INLINE unY #-}

-- | Sentence of Y (label choices).
type Ys t = V.Vector (Y t)

-- | A word represented by a list of its observations
-- and a list of its potential label interpretations.
data X o t = X
    { _unX :: AVec o
    , _unR :: AVec t }
    deriving (Show, Read, Eq, Ord)

-- | Sentence of words.
type Xs o t = V.Vector (X o t)

-- | X constructor.
mkX :: (Ord o, Ord t) => [o] -> [t] -> X o t
mkX x r  = X (fromList x) (fromList r)
{-# INLINE mkX #-}

-- | List of observations.
unX :: X o t -> [o]
unX = toList . _unX
{-# INLINE unX #-}

-- | List of potential labels.
unR :: X o t -> [t]
unR = toList . _unR
{-# INLINE unR #-}

-- | Vector of potential labels on the given position of the sentence.
lbVec :: Xs o t -> Int -> AVec t
lbVec xs = _unR . (xs V.!)
{-# INLINE lbVec #-}

-- | Number of potential labels on the given position of the sentence.
lbNum :: Xs o t -> Int -> Int
lbNum xs = V.length . unAVec . lbVec xs
{-# INLINE lbNum #-}

-- | Potential label on the given vector position.
lbOn :: Xs o t -> Int -> LbIx -> t
lbOn xs = (V.!) . unAVec . lbVec xs
{-# INLINE lbOn #-}

lbAt :: X o t -> LbIx -> t
lbAt x = (unAVec (_unR x) V.!)
{-# INLINE lbAt #-}

lbIxs :: Xs o t -> Int -> [LbIx]
lbIxs xs i = [0 .. lbNum xs i - 1]
{-# INLINE lbIxs #-}

-- Interface on top of internal implementation

obPhi :: Ord f => CRF o t f -> X o t -> t -> L.LogFloat
obPhi crf x u = product 
    [ phi crf ft
    | ob <- unX x
    , ft <- feats ob u ]
  where
    feats = obFeats (featGen crf)

-- trPhi1 :: Ord f => CRF o t f -> t -> L.LogFloat
-- trPhi1 crf u = product 
--     [ phi crf ft
--     | ft <- feats u ]
--   where
--     feats = trFeats1 (featGen crf)
-- 
-- trPhi2 :: Ord f => CRF o t f -> t -> t -> L.LogFloat
-- trPhi2 crf u v = product 
--     [ phi crf ft
--     | ft <- feats u v ]
--   where
--     feats = trFeats2 (featGen crf)
-- 
-- trPhi3 :: Ord f => CRF o t f -> t -> t -> t -> L.LogFloat
-- trPhi3 crf u v w = product 
--     [ phi crf ft
--     | ft <- feats u v w ]
--   where
--     feats = trFeats3 (featGen crf)

-- | An index of the label.
type LbIx = Int

-- | Accumulation function.
type AccF = [L.LogFloat] -> L.LogFloat

type ProbArray = LbIx -> LbIx -> LbIx -> L.LogFloat

computePsi :: Ord f => CRF o t f -> Xs o t -> Int -> LbIx -> L.LogFloat
computePsi crf xs i = (A.!) $ A.array bounds
    [ (k, psi crf x k)
    | k <- lbIxs xs i ]
  where
    bounds = (0, lbNum xs i - 1)
    psi crf x = obPhi crf x . lbAt x
    x = xs V.! i

lbNumM :: Xs o t -> Int -> Int
lbNumM xs i
    | i < 0 || i >= n   = 1
    | otherwise         = lbNum xs i
  where
    n = V.length xs
{-# INLINE lbNumM #-}

lbOnM :: Xs o t -> Int -> LbIx -> Maybe t
lbOnM xs i
    | i < 0 || i >= n   = const Nothing
    | otherwise         = Just . lbOn xs i
  where
    n = V.length xs
{-# INLINE lbOnM #-}

lbIxsM :: Xs o t -> Int -> [LbIx]
lbIxsM xs i
    | i < 0 || i >= n   = [0]
    | otherwise         = lbIxs xs i
  where
    n = V.length xs
{-# INLINE lbIxsM #-}

obFeatsOn :: CRF o t f -> Xs o t -> Int -> LbIx -> [f]
obFeatsOn crf xs i u = concat
    [ feats ob e
    | e  <- lbs
    , ob <- unX (xs V.! i) ]
  where 
    feats   = obFeats (featGen crf)
    lbs     = maybeToList (lbOnM xs i u)
{-# INLINE obFeatsOn #-}

trFeatsOn
    :: CRF o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> [f]
trFeatsOn crf xs i u v w =
    doIt a b c
  where
    a = lbOnM xs i       u
    b = lbOnM xs (i - 1) v
    c = lbOnM xs (i - 2) w
    doIt (Just u) (Just v) (Just w) = trFeats3 (featGen crf) u v w
    doIt (Just u) (Just v) _        = trFeats2 (featGen crf) u v
    doIt (Just u) _ _               = trFeats1 (featGen crf) u
{-# INLINE trFeatsOn #-}

onTransition
    :: Ord f => CRF o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> L.LogFloat
onTransition crf xs i u w v =
    product . map (phi crf) $ trFeatsOn crf xs i u w v
{-# INLINE onTransition #-}

forward :: Ord f => AccF -> CRF o t f -> Xs o t -> ProbArray
forward acc crf sent = alpha where
    alpha = DP.flexible3 (-1, V.length sent - 1)
                (\i   -> (0, lbNumM sent i - 1))
                (\i _ -> (0, lbNumM sent (i - 1) - 1))
                (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi alpha i j k
        | i == -1 = 1.0
        | otherwise = acc
            [ alpha (i - 1) k h * psi j
            * onTransition crf sent i j k h
            | h <- lbIxsM sent (i - 2) ]

backward :: Ord f => AccF -> CRF o t f -> Xs o t -> ProbArray
backward acc crf sent = beta where
    beta = DP.flexible3 (0, V.length sent)
               (\i   -> (0, lbNumM sent (i - 1) - 1))
               (\i _ -> (0, lbNumM sent (i - 2) - 1))
               (\t i -> withMem (computePsi crf sent i) t i)
    withMem psi beta i j k
        | i == V.length sent = 1.0
        | otherwise = acc
            [ beta (i + 1) h j * psi h
            * onTransition crf sent i h j k
            | h <- lbIxsM sent i ]

zxBeta :: ProbArray -> L.LogFloat
zxBeta beta = beta 0 0 0

zxAlpha :: AccF -> Xs o t -> ProbArray -> L.LogFloat
zxAlpha acc sent alpha = acc
    [ alpha (n - 1) i j
    | i <- lbIxsM sent (n - 1)
    , j <- lbIxsM sent (n - 2) ]
    -- | (i, j) <- lbIxsM2 sent (n - 1) ]
    where n = V.length sent

zx :: Ord f => CRF o t f -> Xs o t -> L.LogFloat
zx crf = zxBeta . backward sum crf

zx' :: Ord f => CRF o t f -> Xs o t -> L.LogFloat
zx' crf sent = zxAlpha sum sent (forward sum crf sent)

argmax :: (Ord b) => (a -> b) -> [a] -> (a, b)
argmax f l = foldl1 choice $ map (\x -> (x, f x)) l
    where choice (x1, v1) (x2, v2)
              | v1 > v2 = (x1, v1)
              | otherwise = (x2, v2)

tagIxs :: Ord f => CRF o t f -> Xs o t -> [Int]
tagIxs crf sent = collectMaxArg (0, 0, 0) [] mem where
    mem = DP.flexible3 (0, V.length sent)
                       (\i   -> (0, lbNumM sent (i - 1) - 1))
                       (\i _ -> (0, lbNumM sent (i - 2) - 1))
                       (\t i -> withMem (computePsi crf sent i) t i)
    withMem psiMem mem i j k
        | i == V.length sent = (-1, 1)
        | otherwise = argmax eval $ lbIxsM sent i
        where eval h =
                  (snd $ mem (i + 1) h j) * psiMem h
                  * onTransition crf sent i h j k
    collectMaxArg (i, j, k) acc mem =
        collect $ mem i j k
        where collect (h, _)
                  | h == -1 = reverse acc
                  | otherwise = collectMaxArg (i + 1, h, j) (h:acc) mem

tag :: Ord f => CRF o t f -> Xs o t -> [t]
tag crf sent =
    let ixs = tagIxs crf sent
    in  [lbAt x i | (x, i) <- zip (V.toList sent) ixs]

tagProbs :: Ord f => CRF o t f -> Xs o t -> [[L.LogFloat]]
tagProbs crf sent =
    let alpha = forward maximum crf sent
        beta = backward maximum crf sent
        normalize xs =
            let d = - sum xs
            in map (*d) xs
        m1 k x = maximum
            [ alpha k x y * beta (k + 1) x y
            | y <- lbIxsM sent (k - 1) ]
    in  [ normalize [m1 i k | k <- lbIxsM sent i]
        | i <- [0 .. V.length sent - 1] ]

marginals :: Ord f => CRF o t f -> Xs o t -> [[L.LogFloat]]
marginals crf sent =
    let alpha = forward sum crf sent
        beta = backward sum crf sent
    in  [ [ prob1 crf alpha beta sent i k
          | k <- lbIxsM sent i ]
        | i <- [0 .. V.length sent - 1] ]

goodAndBad :: (Eq t, Ord f) => CRF o t f -> Xs o t -> Ys t -> (Int, Int)
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

goodAndBad' :: (Eq t, Ord f) => CRF o t f -> [(Xs o t, Ys t)] -> (Int, Int)
goodAndBad' crf dataset =
    let add (g, b) (g', b') = (g + g', b + b')
    in  foldl add (0, 0) [goodAndBad crf x y | (x, y) <- dataset]

-- | Compute the accuracy of the model with respect to the labeled dataset.
accuracy :: (Eq t, Ord f) => CRF o t f -> [(Xs o t, Ys t)] -> Double
accuracy crf dataset =
    let k = numCapabilities
    	parts = partition k dataset
        xs = parMap rseq (goodAndBad' crf) parts
        (good, bad) = foldl add (0, 0) xs
        add (g, b) (g', b') = (g + g', b + b')
    in  fromIntegral good / fromIntegral (good + bad)

prob3
    :: Ord f => CRF o t f -> ProbArray -> ProbArray -> Xs o t
    -> Int -> (LbIx -> L.LogFloat) -> LbIx -> LbIx -> LbIx
    -> L.LogFloat
prob3 crf alpha beta sent k psiMem x y z =
    alpha (k - 1) y z * beta (k + 1) x y * psiMem x
    * onTransition crf sent k x y z / zxBeta beta
{-# INLINE prob3 #-}

prob2
    :: CRF o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> LbIx -> LbIx -> L.LogFloat
prob2 crf alpha beta sent k x y =
    alpha k x y * beta (k + 1) x y / zxBeta beta
{-# INLINE prob2 #-}

prob1
    :: CRF o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> LbIx -> L.LogFloat
prob1 crf alpha beta sent k x = sum
    [ prob2 crf alpha beta sent k x y
    | y <- lbIxsM sent (k - 1) ]

expectedFeaturesOn
    :: Ord f => CRF o t f -> ProbArray -> ProbArray
    -> Xs o t -> Int -> [(f, L.LogFloat)]
expectedFeaturesOn crf alpha beta sent k =
    fs3 ++ fs1
    where psi = computePsi crf sent k
          pr1 = prob1 crf alpha beta sent k
          pr3 = prob3 crf alpha beta sent k psi
          fs1 = [ (ft, pr) 
                | a <- lbIxsM sent k
                , let pr = pr1 a
                , ft <- obFeatsOn crf sent k a ]
    	  fs3 = [ (ft, pr) 
                | a <- lbIxsM sent k
                , b <- lbIxsM sent $ k - 1
                , c <- lbIxsM sent $ k - 2
                , let pr = pr3 a b c
                , ft <- trFeatsOn crf sent k a b c ]

expectedFeaturesIn :: Ord f => CRF o t f -> Xs o t -> [(f, L.LogFloat)]
expectedFeaturesIn crf sent =
    -- force parallel computation of alpha and beta tables
    zx1 `par` zx2 `pseq` zx1 `pseq` concat
      [ expectedFeaturesOn crf alpha beta sent k
      | k <- [0 .. V.length sent - 1] ]
    where alpha = forward sum crf sent
          beta = backward sum crf sent
          zx1 = zxAlpha sum sent alpha
          zx2 = zxBeta beta
