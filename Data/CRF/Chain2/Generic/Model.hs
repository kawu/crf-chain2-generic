{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.CRF.Chain2.Generic.Model
( FeatGen (..)
, FeatSel
, selectPresent
, selectHidden
, Model (..)
, mkModel
, Core (..)
, core
, withCore
, phi
, index
, presentFeats
, hiddenFeats
, obFeatsOn
, trFeatsOn
, onWord
, onTransition
, lbNum
, lbOn
, lbIxs
) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList)
import Data.Binary (Binary, put, get)
import Data.Vector.Binary ()
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain2.Generic.Internal
import Data.CRF.Chain2.Generic.FeatMap

-- | Feature generation specification.
data FeatGen o t f = FeatGen
    { obFeats   :: o -> t -> [f]
    , trFeats1  :: t -> [f]
    , trFeats2  :: t -> t -> [f]
    , trFeats3  :: t -> t -> t -> [f] }

-- | A conditional random field.
data Model m o t f = Model
    { values    :: U.Vector Double
    , ixMap     :: m f
    , featGen   :: FeatGen o t f }

-- | A core of the model with no feature generation function.
-- Unlike the 'Model', the core can be serialized. 
data Core m f = Core
    { valuesC   :: U.Vector Double
    , ixMapC    :: m f }

instance Binary (m f) => Binary (Core m f) where
    put Core{..} = put valuesC >> put ixMapC
    get = Core <$> get <*> get

-- | Extract the model core.
core :: Model m o t f -> Core m f
core Model{..} = Core values ixMap

-- | Construct model with the given core and feature generation function.
withCore :: Core m f -> FeatGen o t f -> Model m o t f
withCore Core{..} ftGen = Model valuesC ixMapC ftGen

-- | Features present in the dataset element together with corresponding
-- occurence probabilities.
presentFeats :: FeatGen o t f -> Xs o t -> Ys t -> [(f, L.LogFloat)]
presentFeats fg xs ys = concat
    [ obFs i ++ trFs i
    | i <- [0 .. V.length xs - 1] ]
  where
    obFs i =
        [ (ft, L.logFloat pr)
        | o <- unX (xs V.! i)
        , (u, pr) <- unY (ys V.! i)
        , ft <- obFeats fg o u ]
    trFs 0 =
        [ (ft, L.logFloat pr)
        | (u, pr) <- unY (ys V.! 0)
        , ft <- trFeats1 fg u ]
    trFs 1 =
        [ (ft, L.logFloat pr1 * L.logFloat pr2)
        | (u, pr1) <- unY (ys V.! 1)
        , (v, pr2) <- unY (ys V.! 0)
        , ft <- trFeats2 fg u v ]
    trFs i =
        [ (ft, L.logFloat pr1 * L.logFloat pr2 * L.logFloat pr3)
        | (u, pr1) <- unY (ys V.! i)
        , (v, pr2) <- unY (ys V.! (i-1))
        , (w, pr3) <- unY (ys V.! (i-2))
        , ft <- trFeats3 fg u v w ]

-- | Features hidden in the dataset element.
hiddenFeats :: FeatGen o t f -> Xs o t -> [f]
hiddenFeats fg xs =
    obFs ++ trFs
  where
    obFs = concat
        [ obFeatsOn fg xs i u
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i ]
    trFs = concat
        [ trFeatsOn fg xs i u v w
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i
        , v <- lbIxs xs $ i - 1
        , w <- lbIxs xs $ i - 2 ]

-- | A feature selection function type.
type FeatSel o t f = FeatGen o t f -> Xs o t -> Ys t -> [f]

-- | The 'presentFeats' adapted to fit feature selection specs.
selectPresent :: FeatSel o t f
selectPresent fg xs = map fst . presentFeats fg xs

-- | The 'hiddenFeats' adapted to fit feature selection specs.
selectHidden :: FeatSel o t f
selectHidden fg xs _ = hiddenFeats fg xs

mkModel
    :: (Ord f, FeatMap m f)
    => FeatGen o t f -> FeatSel o t f
    -> [(Xs o t, Ys t)] -> Model m o t f
mkModel fg ftSel dataset = Model
    { values    = U.replicate (S.size fs) 0.0 
    , ixMap     =
        let featIxs = map FeatIx [0..]
            featLst = S.toList fs
        in  mkFeatMap (zip featLst featIxs)
    , featGen   = fg }
  where
    fs = S.fromList $ concatMap select dataset
    select = uncurry (ftSel fg)

-- | Potential assigned to the feature -- exponential of the
-- corresonding parameter.
phi :: FeatMap m f => Model m o t f -> f -> L.LogFloat
phi Model{..} ft = case featIndex ft ixMap of
    Just ix -> L.logToLogFloat (values U.! unFeatIx ix)
    Nothing -> L.logToLogFloat (0 :: Float)
{-# INLINE phi #-}

-- | Index of the feature.
index :: FeatMap m f => Model m o t f -> f -> Maybe FeatIx
index Model{..} ft = featIndex ft ixMap
{-# INLINE index #-}

obFeatsOn :: FeatGen o t f -> Xs o t -> Int -> LbIx -> [f]
obFeatsOn featGen xs i u = concat
    [ feats ob e
    | e  <- lbs
    , ob <- unX (xs V.! i) ]
  where 
    feats   = obFeats featGen
    lbs     = maybeToList (lbOn xs i u)
{-# INLINE obFeatsOn #-}

trFeatsOn
    :: FeatGen o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> [f]
trFeatsOn featGen xs i u' v' w' =
    doIt a b c
  where
    a = lbOn xs i       u'
    b = lbOn xs (i - 1) v'
    c = lbOn xs (i - 2) w'
    doIt (Just u) (Just v) (Just w) = trFeats3 featGen u v w
    doIt (Just u) (Just v) _        = trFeats2 featGen u v
    doIt (Just u) _ _               = trFeats1 featGen u
    doIt _ _ _                      = []
{-# INLINE trFeatsOn #-}

onWord :: FeatMap m f => Model m o t f -> Xs o t -> Int -> LbIx -> L.LogFloat
onWord crf xs i u =
    product . map (phi crf) $ obFeatsOn (featGen crf) xs i u
{-# INLINE onWord #-}

onTransition
    :: FeatMap m f => Model m o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> L.LogFloat
onTransition crf xs i u w v =
    product . map (phi crf) $ trFeatsOn (featGen crf) xs i u w v
{-# INLINE onTransition #-}
