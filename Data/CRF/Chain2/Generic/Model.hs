{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Generic.Model
( FeatIx (..)
, FeatGen (..)
, Model (..)
, mkModel
, phi
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
import Data.Binary (Binary)
import Data.Vector.Binary ()
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as G
import qualified Data.Vector.Generic.Mutable as G
import qualified Data.Number.LogFloat as L

import Data.CRF.Chain2.Generic.Base
import qualified Data.CRF.Chain2.Generic.Internal as I

-- | A feature index.  To every model feature a unique index is assigned.
newtype FeatIx = FeatIx { unFeatIx :: Int }
    deriving ( Show, Eq, Ord, Binary
             , G.Vector U.Vector, G.MVector U.MVector, U.Unbox )

-- | Feature generation specification.
data FeatGen o t f = FeatGen
    { obFeats   :: o -> t -> [f]
    , trFeats1  :: t -> [f]
    , trFeats2  :: t -> t -> [f]
    , trFeats3  :: t -> t -> t -> [f] }

-- | A conditional random field.
data Model o t f = Model
    { values    :: U.Vector Double
    , ixMap     :: M.Map f FeatIx
    , featGen   :: FeatGen o t f }

-- | FINISH: Dodać ekstrację liczby cech ze zbioru danych,
-- zmienić funkcję mkModel.
mkModel :: Ord f => FeatGen o t f -> [Xs o t] -> Model o t f
mkModel fg dataset = Model
    { values    = U.replicate (S.size fs) 0.0 
    , ixMap     =
        let featIxs = map FeatIx [0..]
            featLst = S.toList fs
        in  M.fromList (zip featLst featIxs)
    , featGen   = fg }
  where
    fs = S.fromList $ concatMap ((++) <$> obFs <*> trFs) dataset
    obFs xs = concat
        [ _obFeatsOn fg xs i u
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i ]
    trFs xs = concat
        [ _trFeatsOn fg xs i u v w
        | i <- [0 .. V.length xs - 1]
        , u <- lbIxs xs i
        , v <- lbIxs xs $ i - 1
        , w <- lbIxs xs $ i - 2 ]

-- | Potential assigned to the feature -- exponential of the
-- corresonding parameter.
phi :: Ord f => Model o t f -> f -> L.LogFloat
phi Model{..} ft = case M.lookup ft ixMap of
    Just ix -> L.logToLogFloat (values U.! unFeatIx ix)
    Nothing -> L.logToLogFloat (0 :: Float)
{-# INLINE phi #-}

_obFeatsOn :: FeatGen o t f -> Xs o t -> Int -> LbIx -> [f]
_obFeatsOn featGen xs i u = concat
    [ feats ob e
    | e  <- lbs
    , ob <- unX (xs V.! i) ]
  where 
    feats   = obFeats featGen
    lbs     = maybeToList (lbOn xs i u)
{-# INLINE _obFeatsOn #-}

obFeatsOn :: Model o t f -> Xs o t -> Int -> LbIx -> [f]
obFeatsOn crf = _obFeatsOn (featGen crf)
{-# INLINE obFeatsOn #-}

_trFeatsOn
    :: FeatGen o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> [f]
_trFeatsOn featGen xs i u v w =
    doIt a b c
  where
    a = lbOn xs i       u
    b = lbOn xs (i - 1) v
    c = lbOn xs (i - 2) w
    doIt (Just u) (Just v) (Just w) = trFeats3 featGen u v w
    doIt (Just u) (Just v) _        = trFeats2 featGen u v
    doIt (Just u) _ _               = trFeats1 featGen u
{-# INLINE _trFeatsOn #-}

trFeatsOn
    :: Model o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> [f]
trFeatsOn crf = _trFeatsOn (featGen crf)
{-# INLINE trFeatsOn #-}

onWord :: Ord f => Model o t f -> Xs o t -> Int -> LbIx -> L.LogFloat
onWord crf xs i u =
    product . map (phi crf) $ obFeatsOn crf xs i u
{-# INLINE onWord #-}

onTransition
    :: Ord f => Model o t f -> Xs o t -> Int
    -> LbIx -> LbIx -> LbIx -> L.LogFloat
onTransition crf xs i u w v =
    product . map (phi crf) $ trFeatsOn crf xs i u w v
{-# INLINE onTransition #-}

lbNum :: Xs o t -> Int -> Int
lbNum xs i
    | i < 0 || i >= n   = 1
    | otherwise         = I.lbNum xs i
  where
    n = V.length xs
{-# INLINE lbNum #-}

lbOn :: Xs o t -> Int -> LbIx -> Maybe t
lbOn xs i
    | i < 0 || i >= n   = const Nothing
    | otherwise         = Just . I.lbOn xs i
  where
    n = V.length xs
{-# INLINE lbOn #-}

lbIxs :: Xs o t -> Int -> [LbIx]
lbIxs xs i
    | i < 0 || i >= n   = [0]
    | otherwise         = I.lbIxs xs i
  where
    n = V.length xs
{-# INLINE lbIxs #-}
