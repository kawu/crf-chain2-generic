{-# LANGUAGE MultiParamTypeClasses #-}

module Data.CRF.Chain2.Generic.FeatMap
( FeatMap (..)
) where

import Data.CRF.Chain2.Generic.Internal

class FeatMap m f where
    featIndex   :: f -> m f -> Maybe FeatIx
    mkFeatMap   :: [(f, FeatIx)] -> m f
