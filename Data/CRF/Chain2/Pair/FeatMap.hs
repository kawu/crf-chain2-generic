{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Pair.FeatMap
( FeatMap (..)
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (guard)
import Data.List (foldl1')
import Data.Maybe (catMaybes)
import Data.Ix (Ix, inRange, range)
import Data.Binary (Binary, put, get)
import qualified Data.Array.Unboxed as A
import qualified Data.Map as M

import Data.CRF.Chain2.Pair.Base
import Data.CRF.Chain2.Generic.Internal (FeatIx(..))
import qualified Data.CRF.Chain2.Generic.FeatMap as C

-- | Dummy feature index.
dummy :: FeatIx
dummy = FeatIx (-1)
{-# INLINE dummy #-}

data FeatMap a = FeatMap
    { trMap3'1  :: A.UArray (Lb1, Lb1, Lb1) FeatIx
    , trMap3'2  :: A.UArray (Lb2, Lb2, Lb2) FeatIx
    , otherMap  :: M.Map Feat FeatIx }

(!?) :: (Ix i, A.IArray a b) => a i b -> i -> Maybe b
m !? x = if inRange (A.bounds m) x
    then Just (m A.! x)
    else Nothing
{-# INLINE (!?) #-}

instance C.FeatMap FeatMap Feat where
    featIndex (TFeat3'1 x y z) (FeatMap m _ _)  = do
        ix <- m !? (x, y, z)
        guard (ix /= dummy)
        return ix
    featIndex (TFeat3'2 x y z) (FeatMap _ m _)  = do
        ix <- m !? (x, y, z)
        guard (ix /= dummy)
        return ix
    featIndex x (FeatMap _ _ m)                 = M.lookup x m
    mkFeatMap xs = FeatMap
        (mkArray (catMaybes $ map getTFeat3'1 xs))
        (mkArray (catMaybes $ map getTFeat3'2 xs))
        (M.fromList (filter (isOther . fst) xs))
      where
        getTFeat3'1 (TFeat3'1 x y z, v) = Just ((x, y, z), v)
        getTFeat3'1 _                   = Nothing
        getTFeat3'2 (TFeat3'2 x y z, v) = Just ((x, y, z), v)
        getTFeat3'2 _                   = Nothing
        isOther (TFeat3'1 _ _ _)        = False
        isOther (TFeat3'2 _ _ _)        = False
        isOther _                       = True
        mkArray ys =
            let p = foldl1' updateMin (map fst ys)
                q = foldl1' updateMax (map fst ys)
                updateMin (x, y, z) (x', y', z') =
                    (min x x', min y y', min z z')
                updateMax (x, y, z) (x', y', z') =
                    (max x x', max y y', max z z')
                zeroed pq = A.array pq [(k, dummy) | k <- range pq]
            in  zeroed (p, q) A.// ys

instance Binary (FeatMap Feat) where
    put FeatMap{..} = put trMap3'1 >> put trMap3'2 >> put otherMap
    get = FeatMap <$> get <*> get <*> get
