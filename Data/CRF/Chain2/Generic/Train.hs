{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternGuards #-}

module Data.CRF.Chain2.Generic.Train
( CodecSpec (..)
, train
) where

import System.IO (hSetBuffering, stdout, BufferMode (..))
import Control.Applicative ((<$>))
import Data.Maybe (maybeToList)
import qualified Data.Vector as V
import qualified Numeric.SGD as SGD
import qualified Numeric.SGD.LogSigned as L

import Data.CRF.Chain2.Generic.Base
import Data.CRF.Chain2.Generic.External (SentL)
import Data.CRF.Chain2.Generic.Model
import Data.CRF.Chain2.Generic.Inference (expectedFeatures, accuracy)

-- | A codec specification.
data CodecSpec a b c o t = CodecSpec
    { mkCodec :: [SentL a b] -> (c, [(Xs o t, Ys t)])
    , encode  :: c -> [SentL a b] -> [(Xs o t, Ys t)] }

-- | Train the CRF using the stochastic gradient descent method.
-- When the evaluation data 'IO' action is 'Just', the iterative
-- training process will notify the user about the current accuracy
-- on the evaluation part every full iteration over the training part.
-- TODO: Add custom feature extraction function.
train
    :: (Ord a, Ord b, Eq t, Ord f)
    => SGD.SgdArgs                  -- ^ Args for SGD
    -> CodecSpec a b c o t          -- ^ Codec specification
    -> FeatGen o t f                -- ^ Feature generation
    -> IO [SentL a b]               -- ^ Training data 'IO' action
    -> Maybe (IO [SentL a b])       -- ^ Maybe evalation data
    -> IO (c, Model o t f)          -- ^ Resulting codec and model
train sgdArgs CodecSpec{..} ftGen trainIO evalIO'Maybe = do
    hSetBuffering stdout NoBuffering
    (codec, trainData) <- mkCodec <$> trainIO
    evalDataM <- case evalIO'Maybe of
        Just evalIO -> Just . encode codec <$> evalIO
        Nothing     -> return Nothing
    let crf = mkModel ftGen (map fst trainData)
    para <- SGD.sgdM sgdArgs
        (notify sgdArgs crf trainData evalDataM)
        (gradOn crf) (V.fromList trainData) (values crf)
    return (codec, crf { values = para })

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
