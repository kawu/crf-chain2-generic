{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Chain2.Generic.FeatMap.Map
( FeatMap (..)
) where

import Data.Binary (Binary)
import qualified Data.Map as M

import Data.CRF.Chain2.Generic.Internal
import qualified Data.CRF.Chain2.Generic.FeatMap as C

newtype FeatMap f = FeatMap { unFeatMap :: M.Map f FeatIx }
    deriving (Show, Eq, Ord, Binary)

instance Ord f => C.FeatMap FeatMap f where
    featIndex x (FeatMap m) = M.lookup x m
    mkFeatMap = FeatMap . M.fromList
