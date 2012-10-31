module Data.CRF.Chain2.Generic.External
( Word (..)
, mkWord
, Sent
, Dist (unDist)
, mkDist
, WordL
, SentL
) where

import qualified Data.Set as S
import qualified Data.Map as M

-- | A word with 'a' representing the observation type and 'b' representing
-- the compound label type.
data Word a b = Word {
    -- | Set of observations.
      obs   :: S.Set a
    -- | Non-empty set of potential labels.
    , lbs   :: S.Set b }
    deriving (Show, Eq, Ord)

-- | A word constructor which checks non-emptiness of the potential
-- set of labels.
mkWord :: S.Set a -> S.Set b -> Word a b
mkWord _obs _lbs
    | S.null _lbs   = error "mkWord: empty set of potential labels"
    | otherwise     = Word _obs _lbs

type Sent a b = [Word a b]

-- | A probability distribution defined over elements of type a.
-- All elements not included in the map have probability equal
-- to 0.
newtype Dist a = Dist { unDist :: M.Map a Double }

-- | Construct the probability distribution.
mkDist :: Ord a => [(a, Double)] -> Dist a
mkDist =
    Dist . normalize . M.fromListWith (+)
  where
    normalize dist =
        let z = sum (M.elems dist)
        in  fmap (/z) dist

-- | A WordL is a labeled word, i.e. a word with probability distribution
-- defined over labels.  We assume that every label from the distribution
-- domain is a member of the set of potential labels corresponding to the
-- word.  TODO: Ensure the assumption using the smart constructor.
type WordL a b = (Word a b, Dist b)

-- | A sentence of labeled words.
type SentL a b = [WordL a b]
