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

import Data.CRF.Chain2.Pair.Base
import Data.CRF.Chain2.Generic.Internal
import Data.CRF.Chain2.Generic.External

-- | A codec monad.
type CodecM c a = C.Codec c a

-- | An abstract codec representation.
data Codec a b c = Codec {
    -- | Empty codec.
      empty     :: c
    -- | Encode the observation and update the codec
    -- (only in the encoding direction).
    , encodeObU :: a -> CodecM c Ob
    -- | Encode the observation and do *not* update the codec.
    , encodeObN :: a -> CodecM c (Maybe Ob)
    -- | Encode the label and update the codec.
    , encodeLbU :: b -> CodecM c Lb
    -- | Encode the label and do *not* update the codec.
    -- In case the label is not a member of the codec,
    -- return the label code assigned to Nothing label.
    , encodeLbN :: b -> CodecM c Lb
    -- | Decode the label within the codec monad.
    , decodeLbC :: Lb -> CodecM c (Maybe b)
    -- | Is label a member of the codec?
    , hasLabel  :: c -> b -> Bool }

-- | Encode the labeled word and update the codec.
encodeWordL'Cu
    :: Codec a b c
    -> WordL a b
    -> CodecM c (X Ob Lb, Y Lb)
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
    :: Codec a b c
    -> WordL a b
    -> CodecM c (X Ob Lb, Y Lb)
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
    :: Codec a b c
    -> Word a b
    -> CodecM c (X Ob Lb)
encodeWord'Cu Codec{..} word = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn
    :: Codec a b c
    -> Word a b
    -> CodecM c (X Ob Lb)
encodeWord'Cn Codec{..} word = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu
    :: Codec a b c
    -> SentL a b
    -> CodecM c (Xs Ob Lb, Ys Lb)
encodeSentL'Cu cdc sent = do
    ps <- mapM (encodeWordL'Cu cdc) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn
    :: Codec a b c
    -> SentL a b
    -> CodecM c (Xs Ob Lb, Ys Lb)
encodeSentL'Cn cdc sent = do
    ps <- mapM (encodeWordL'Cn cdc) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL :: Codec a b c -> c -> SentL a b -> (Xs Ob Lb, Ys Lb)
encodeSentL cdc cdcData = C.evalCodec cdcData . encodeSentL'Cn cdc

-- | Encode the sentence and update the codec.
encodeSent'Cu :: Codec a b c -> Sent a b -> CodecM c (Xs Ob Lb)
encodeSent'Cu cdc = fmap V.fromList . mapM (encodeWord'Cu cdc)

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn :: Codec a b c -> Sent a b -> CodecM c (Xs Ob Lb)
encodeSent'Cn cdc = fmap V.fromList . mapM (encodeWord'Cn cdc)

-- | Encode the sentence using the given codec.
encodeSent :: Codec a b c -> c -> Sent a b -> Xs Ob Lb
encodeSent cdc cdcData = C.evalCodec cdcData . encodeSent'Cn cdc

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec :: Codec a b c -> [SentL a b] -> (c, [(Xs Ob Lb, Ys Lb)])
mkCodec cdc
    = swap
    . C.runCodec (empty cdc)
    . mapM (encodeSentL'Cu cdc)
  where
    swap (x, y) = (y, x)

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL :: Codec a b c -> c -> [SentL a b] -> [(Xs Ob Lb, Ys Lb)]
encodeDataL cdc cdcData = C.evalCodec cdcData . mapM (encodeSentL'Cn cdc)

-- | Encode the dataset with the codec.
encodeData :: Codec a b c -> c -> [Sent a b] -> [Xs Ob Lb]
encodeData cdc cdcData = map (encodeSent cdc cdcData)

-- -- | Decode the label within the codec monad.
-- decodeLabel'C
--     :: (Ord b, Ord c) => Lb
--     -> CodecM a b c (Maybe (b, c))
-- decodeLabel'C (x, y) = do
--     x' <- C.decode _2Lens (unLb1 x)
--     y' <- C.decode _3Lens (unLb2 y)
--     return $ (,) <$> x' <*> y'

-- | Decode the label.
decodeLabel :: Codec a b c -> c -> Lb -> Maybe b
decodeLabel cdc cdcData = C.evalCodec cdcData . decodeLbC cdc

-- | Decode the sequence of labels.
decodeLabels :: Codec a b c -> c -> [Lb] -> [Maybe b]
decodeLabels cdc cdcData = C.evalCodec cdcData . mapM (decodeLbC cdc)

-- hasLabel :: (Ord b, Ord c) => Codec a b c -> (b, c) -> Bool
-- hasLabel codec (x, y)
--     =  M.member (Just x) (C.to $ _2 codec)
--     && M.member (Just y) (C.to $ _3 codec)
-- {-# INLINE hasLabel #-}

-- | Return the label when 'Just' or one of the unknown values
-- when 'Nothing'.
unJust :: Codec a b c -> c -> Word a b -> Maybe b -> b
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
