{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Pair
( 
-- * Data types
-- ** External
  Word (..)
, mkWord
, Sent
, Dist (unDist)
, mkDist
, WordL
, SentL
-- * Internal
, Ob (..)
, Lb1 (..)
, Lb2 (..)
, Lb
, Feat (..)

-- * CRF
, CRF (..)
-- ** Training
, train
-- ** Tagging
, tag

-- * Feature selection
, FeatSel
, selectHidden
, selectPresent
) where

import Control.Applicative ((<$>), (<*>)) 
import Data.Binary (Binary, get, put)
import qualified Numeric.SGD as SGD

import Data.CRF.Chain2.Generic.Model
    (Model, FeatSel, selectHidden, selectPresent, core, withCore)
import Data.CRF.Chain2.Generic.External
import qualified Data.CRF.Chain2.Generic.Inference as I
import qualified Data.CRF.Chain2.Generic.Train as T

import Data.CRF.Chain2.Pair.Base
import Data.CRF.Chain2.Pair.Codec
import Data.CRF.Chain2.Pair.FeatMap

data CRF a b c = CRF
    { codec :: Codec a b c
    , model :: Model FeatMap Ob Lb Feat }

instance (Ord a, Ord b, Ord c, Binary a, Binary b, Binary c)
    => Binary (CRF a b c) where
    put CRF{..} = put codec >> put (core model)
    get = CRF <$> get <*> do
        _core <- get
        return $ withCore _core featGen

codecSpec :: (Ord a, Ord b, Ord c) => T.CodecSpec a (b, c) (Codec a b c) Ob Lb
codecSpec = T.CodecSpec
    { T.mkCodec = mkCodec
    , T.encode  = encodeDataL }

-- | Train the CRF using the stochastic gradient descent method.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
-- Use the provided feature selection function to determine model
-- features.
train
    :: (Ord a, Ord b, Ord c)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> FeatSel Ob Lb Feat           -- ^ Feature selection
    -> IO [SentL a (b, c)]          -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a (b, c)])  -- ^ Maybe evalation data
    -> IO (CRF a b c)               -- ^ Resulting codec and model
train sgdArgs featSel trainIO evalIO'Maybe = do
    (_codec, _model) <- T.train
        sgdArgs
        codecSpec
        featGen
        featSel
        trainIO
        evalIO'Maybe
    return $ CRF _codec _model

-- | Find the most probable label sequence.
tag :: (Ord a, Ord b, Ord c) => CRF a b c -> Sent a (b, c) -> [(b, c)]
tag CRF{..} sent
    = onWords . decodeLabels codec
    . I.tag model . encodeSent codec
    $ sent
  where
    onWords xs =
        [ unJust codec word x
        | (word, x) <- zip sent xs ]
