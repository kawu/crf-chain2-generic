{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Chain2.Generic.Train
( CodecSpc (..)
, CRF (..)
, train
) where

import System.IO (hSetBuffering, stdout, BufferMode (..))
import Control.Applicative ((<$>), (<*>))
import Data.Maybe (maybeToList)
import Data.Binary (Binary, put, get)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Chain2.Generic.Base
import Data.CRF.Chain2.Generic.External (SentL)
import Data.CRF.Chain2.Generic.Model
import Data.CRF.Chain2.Generic.Inference (expectedFeatures, accuracy)

-- | A codec specification.
data CodecSpc a b c o t = CodecSpc
    { mkCodec :: [SentL a b] -> (c, [(Xs o t, Ys t)])
    , encode  :: c -> [SentL a b] -> [(Xs o t, Ys t)] }

-- | A conditional random field model with additional codec used for
-- data encoding.
data CRF c o t f = CRF {
    -- | The codec is used to transform data into internal representation,
    -- where each observation and each label is represented by a unique
    -- integer number.
    codec :: c,
    -- | The actual model, which is a map from 'Feature's to potentials.
    model :: Model o t f }

-- Dodać informacje o typie funckji FeatGen używanej w modelu.
-- instance (Ord a, Ord b, Binary a, Binary b) => Binary (CRF a b) where
--     put CRF{..} = put codec >> put model
--     get = CRF <$> get <*> get

-- | Train the CRF using the stochastic gradient descent method.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
-- TODO: Add custom feature extraction function.
train
    :: (Ord a, Ord b, Eq t, Ord f)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> CodecSpc a b c o t           -- ^ Codec specification
    -> FeatGen o t f                -- ^ Feature generation
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a b])       -- ^ Maybe evalation data
    -> IO (CRF c o t f)             -- ^ Resulting model
train sgdArgs CodecSpc{..} ftGen trainIO evalIO'Maybe = do
    hSetBuffering stdout NoBuffering
    (_codec, trainData) <- mkCodec <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just evalIO -> Just . encode _codec <$> evalIO
        Nothing     -> return Nothing
    let crf = mkModel ftGen (map fst trainData)
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (gradOn crf) (V.fromList trainData) (values crf)
    return $ CRF _codec (crf { values = para })

gradOn :: Ord f => Model o t f -> SGD.Para -> (Xs o t, Ys t) -> SGD.Grad
gradOn crf para (xs, ys) = SGD.fromLogList $
    [ (ix, L.fromPos val)
    | (ft, val) <- presentFeats (featGen curr) xs ys
    , FeatIx ix <- maybeToList (index curr ft) ] ++
    [ (ix, L.fromNeg val)
    | (ft, val) <- expectedFeatures curr xs
    , FeatIx ix <- maybeToList (index curr ft) ]
  where
    curr = crf { values = para }

notify
    :: (Eq t, Ord f) => SGD.SgdArgs -> Model o t f -> [(Xs o t, Ys t)]
    -> Maybe [(Xs o t, Ys t)] -> SGD.Para -> Int -> IO ()
notify SGD.SgdArgs{..} crf trainData evalDataM para k 
    | doneTotal k == doneTotal (k - 1) = putStr "."
    | Just dataSet <- evalDataM = do
        let x = accuracy (crf { values = para }) dataSet
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = " ++ show x)
    | otherwise =
        putStrLn ("\n" ++ "[" ++ show (doneTotal k) ++ "] f = #")
  where
    doneTotal :: Int -> Int
    doneTotal = floor . done
    done :: Int -> Double
    done i
        = fromIntegral (i * batchSize)
        / fromIntegral trainSize
    trainSize = length trainData
