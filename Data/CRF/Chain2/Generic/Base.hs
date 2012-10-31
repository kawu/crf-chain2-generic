module Data.CRF.Chain2.Generic.Base
( AVec (unAVec)
, mkAVec
, AVec2 (unAVec2)
, mkAVec2

, X (_unX, _unR)
, Xs
, mkX
, unX
, unR
, lbAt

, Y (_unY)
, Ys
, mkY
, unY

, LbIx
) where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V

-- | An index of the label.
type LbIx = Int

newtype AVec a = AVec { unAVec :: V.Vector a }
    deriving (Show, Read, Eq, Ord)

-- | Smart AVec constructor which ensures that the
-- underlying vector is strictly ascending.
mkAVec :: Ord a => [a] -> AVec a
mkAVec = AVec . V.fromList . S.toAscList  . S.fromList 
{-# INLINE mkAVec #-}

newtype AVec2 a b = AVec2 { unAVec2 :: V.Vector (a, b) }
    deriving (Show, Read, Eq, Ord)

-- | Smart AVec constructor which ensures that the
-- underlying vector is strictly ascending with respect
-- to fst values.
mkAVec2 :: Ord a => [(a, b)] -> AVec2 a b
mkAVec2 = AVec2 . V.fromList . M.toAscList  . M.fromList 
{-# INLINE mkAVec2 #-}

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
mkX x r  = X (mkAVec x) (mkAVec r)
{-# INLINE mkX #-}

-- | List of observations.
unX :: X o t -> [o]
unX = V.toList . unAVec . _unX
{-# INLINE unX #-}

-- | List of potential labels.
unR :: X o t -> [t]
unR = V.toList . unAVec . _unR
{-# INLINE unR #-}

lbAt :: X o t -> LbIx -> t
lbAt x = (unAVec (_unR x) V.!)
{-# INLINE lbAt #-}

newtype Y t = Y { _unY :: AVec2 t Double }
    deriving (Show, Read, Eq, Ord)

-- | Y constructor.
mkY :: Ord t => [(t, Double)] -> Y t
mkY = Y . mkAVec2
{-# INLINE mkY #-}

-- | Y deconstructor symetric to mkY.
unY :: Y t -> [(t, Double)]
unY = V.toList . unAVec2 . _unY
{-# INLINE unY #-}

-- | Sentence of Y (label choices).
type Ys t = V.Vector (Y t)
