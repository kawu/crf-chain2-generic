{-# LANGUAGE RecordWildCards #-}

module Data.CRF.Chain2.Generic.Codec
( CodecM
, Codec (..)

, encodeWord'Cu
, encodeWord'Cn
, encodeSent'Cu
, encodeSent'Cn
, encodeSent

, encodeWordL'Cu
, encodeWordL'Cn
, encodeSentL'Cu
, encodeSentL'Cn
, encodeSentL

, decodeLabel
, decodeLabels
, unJust

, mkCodec
, encodeData
, encodeDataL
) where

import Control.Applicative (pure, (<$>), (<*>))
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain2.Generic.Internal
import Data.CRF.Chain2.Generic.External

-- | A codec monad.
type CodecM c a = C.Codec c a

-- | An abstract codec representation with external observation type
-- 'a', external label type 'b', codec data type 'c', internal
-- observation type 'o' and internal label type 'e'.
data Codec a b c o e = Codec {
    -- | Empty codec.
      empty     :: c
    -- | Encode the observation and update the codec
    -- (only in the encoding direction).
    , encodeObU :: a -> CodecM c o
    -- | Encode the observation and do *not* update the codec.
    , encodeObN :: a -> CodecM c (Maybe o)
    -- | Encode the label and update the codec.
    , encodeLbU :: b -> CodecM c e
    -- | Encode the label and do *not* update the codec.
    -- In case the label is not a member of the codec,
    -- return the label code assigned to Nothing label.
    , encodeLbN :: b -> CodecM c e
    -- | Decode the label within the codec monad.
    , decodeLbC :: e -> CodecM c (Maybe b)
    -- | Is label a member of the codec?
    , hasLabel  :: c -> b -> Bool }

-- | Encode the labeled word and update the codec.
encodeWordL'Cu
    :: (Ord e, Ord o) => Codec a b c o e
    -> WordL a b -> CodecM c (X o e, Y e)
encodeWordL'Cu Codec{..} (word, choice) = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbU lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
encodeWordL'Cn
    :: (Ord e, Ord o) => Codec a b c o e
    -> WordL a b -> CodecM c (X o e, Y e)
encodeWordL'Cn Codec{..} (word, choice) = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbN lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encode the word and update the codec.
encodeWord'Cu
    :: (Ord e, Ord o) => Codec a b c o e
    -> Word a b -> CodecM c (X o e)
encodeWord'Cu Codec{..} word = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn
    :: (Ord e, Ord o) => Codec a b c o e
    -> Word a b -> CodecM c (X o e)
encodeWord'Cn Codec{..} word = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu
    :: (Ord e, Ord o) => Codec a b c o e
    -> SentL a b -> CodecM c (Xs o e, Ys e)
encodeSentL'Cu cdc sent = do
    ps <- mapM (encodeWordL'Cu cdc) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn
    :: (Ord e, Ord o) => Codec a b c o e
    -> SentL a b -> CodecM c (Xs o e, Ys e)
encodeSentL'Cn cdc sent = do
    ps <- mapM (encodeWordL'Cn cdc) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL
    :: (Ord e, Ord o) => Codec a b c o e
    -> c -> SentL a b -> (Xs o e, Ys e)
encodeSentL cdc cdcData = C.evalCodec cdcData . encodeSentL'Cn cdc

-- | Encode the sentence and update the codec.
encodeSent'Cu
    :: (Ord e, Ord o) => Codec a b c o e
    -> Sent a b -> CodecM c (Xs o e)
encodeSent'Cu cdc = fmap V.fromList . mapM (encodeWord'Cu cdc)

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn
    :: (Ord e, Ord o) => Codec a b c o e
    -> Sent a b -> CodecM c (Xs o e)
encodeSent'Cn cdc = fmap V.fromList . mapM (encodeWord'Cn cdc)

-- | Encode the sentence using the given codec.
encodeSent
    :: (Ord e, Ord o) => Codec a b c o e
    -> c -> Sent a b -> Xs o e
encodeSent cdc cdcData = C.evalCodec cdcData . encodeSent'Cn cdc

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec
    :: (Ord e, Ord o) => Codec a b c o e
    -> [SentL a b] -> (c, [(Xs o e, Ys e)])
mkCodec cdc
    = swap
    . C.runCodec (empty cdc)
    . mapM (encodeSentL'Cu cdc)
  where
    swap (x, y) = (y, x)

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL
    :: (Ord e, Ord o) => Codec a b c o e
    -> c -> [SentL a b] -> [(Xs o e, Ys e)]
encodeDataL cdc cdcData = C.evalCodec cdcData . mapM (encodeSentL'Cn cdc)

-- | Encode the dataset with the codec.
encodeData
    :: (Ord e, Ord o) => Codec a b c o e
    -> c -> [Sent a b] -> [Xs o e]
encodeData cdc cdcData = map (encodeSent cdc cdcData)

-- | Decode the label.
decodeLabel :: Codec a b c o e -> c -> e -> Maybe b
decodeLabel cdc cdcData = C.evalCodec cdcData . decodeLbC cdc

-- | Decode the sequence of labels.
decodeLabels :: Codec a b c o e -> c -> [e] -> [Maybe b]
decodeLabels cdc cdcData = C.evalCodec cdcData . mapM (decodeLbC cdc)

-- | Return the label when 'Just' or one of the unknown values
-- when 'Nothing'.
unJust :: Codec a b c o e -> c -> Word a b -> Maybe b -> b
unJust _ _ _ (Just x) = x
unJust cdc cdcData word Nothing = case allUnk of
    (x:_)   -> x
    []      -> error "unJust: Nothing and all values known"
  where
    allUnk = filter (not . hasLabel cdc cdcData) (S.toList $ lbs word)

-- -- | Replace 'Nothing' labels with all unknown labels from
-- -- the set of potential interpretations.
-- unJusts :: Ord b => Codec a b -> Word a b -> [Maybe b] -> [b]
-- unJusts codec word xs =
--     concatMap deJust xs
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
--     deJust (Just x) = [x]
--     deJust Nothing  = allUnk
