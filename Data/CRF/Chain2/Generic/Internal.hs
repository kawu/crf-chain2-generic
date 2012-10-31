module Data.CRF.Chain2.Generic.Internal
( lbNum
, lbOn
, lbIxs
) where

import qualified Data.Vector as V

import Data.CRF.Chain2.Generic.Base

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

lbIxs :: Xs o t -> Int -> [LbIx]
lbIxs xs i = [0 .. lbNum xs i - 1]
{-# INLINE lbIxs #-}
