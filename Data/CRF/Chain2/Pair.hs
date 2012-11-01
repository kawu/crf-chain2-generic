{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Pair
( 
-- * Data types
  Word (..)
, mkWord
, Sent
, Dist (unDist)
, mkDist
, WordL
, SentL

-- * CRF
, CRF (..)
-- ** Training
, train
-- ** Tagging
, tag
) where

import Control.Applicative ((<$>), (<*>)) 
import Data.Binary (Binary, get, put)
import qualified Numeric.SGD as SGD

import Data.CRF.Chain2.Generic.Model (Model, core, withCore)
import Data.CRF.Chain2.Generic.External
import qualified Data.CRF.Chain2.Generic.Inference as I
import qualified Data.CRF.Chain2.Generic.Train as T

import Data.CRF.Chain2.Pair.Base
import Data.CRF.Chain2.Pair.Codec

data CRF a b c = CRF
    { codec :: Codec a b c
    , model :: Model Ob Lb Feat }

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
-- TODO: Add custom feature extraction function.
train
    :: (Ord a, Ord b, Ord c)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> IO [SentL a (b, c)]          -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a (b, c)])  -- ^ Maybe evalation data
    -> IO (CRF a b c)               -- ^ Resulting codec and model
train sgdArgs trainIO evalIO'Maybe = do
    (_codec, _model) <- T.train
        sgdArgs
        codecSpec
        featGen
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
