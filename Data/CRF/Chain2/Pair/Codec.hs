module Data.CRF.Chain2.Pair.Codec
( Codec (..)
, CodecM
, obMax
, lb1Max
, lb2Max

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

, mkCodec
, encodeData
, encodeDataL
) where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Comonad.Trans.Store (store)
import Data.Maybe (fromJust, catMaybes)
import Data.Lens.Common (Lens(..))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Control.Monad.Codec as C

import Data.CRF.Chain2.Pair.Base
import Data.CRF.Chain2.Generic.Base
import Data.CRF.Chain2.Generic.External

-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b,
-- third -- labels of type c from the third level.
type Codec a b c =
    ( C.AtomCodec a
    , C.AtomCodec (Maybe b)
    , C.AtomCodec (Maybe c) )

_1 :: (a, b, c) -> a
_1 (x, _, _) = x
{-# INLINE _1 #-}

_2 :: (a, b, c) -> b
_2 (_, x, _) = x
{-# INLINE _2 #-}

_3 :: (a, b, c) -> c
_3 (_, _, x) = x
{-# INLINE _3 #-}

_1Lens :: Lens (a, b, c) a
_1Lens = Lens $ \(a, b, c) -> store (\a' -> (a', b, c)) a

_2Lens :: Lens (a, b, c) b
_2Lens = Lens $ \(a, b, c) -> store (\b' -> (a, b', c)) b

_3Lens :: Lens (a, b, c) c
_3Lens = Lens $ \(a, b, c) -> store (\c' -> (a, b, c')) c

-- | The maximum internal observation included in the codec.
obMax :: Codec a b c -> Ob
obMax =
    let idMax m = M.size m - 1
    in  Ob . idMax . C.to . _1

-- | The maximum internal label included in the codec.
lb1Max :: Codec a b c -> Lb1
lb1Max =
    let idMax m = M.size m - 1
    in  Lb1 . idMax . C.to . _2

-- | The maximum internal label included in the codec.
lb2Max :: Codec a b c -> Lb2
lb2Max =
    let idMax m = M.size m - 1
    in  Lb2 . idMax . C.to . _3

-- | The empty codec.  The label part is initialized with Nothing
-- member, which represents unknown labels.  It is taken on account
-- in the model implementation because it is assigned to the
-- lowest label code and the model assumes that the set of labels
-- is of the {0, ..., 'lbMax'} form.
empty :: (Ord b, Ord c) => Codec a b c
empty =
    ( C.empty
    , C.execCodec C.empty (C.encode C.idLens Nothing)
    , C.execCodec C.empty (C.encode C.idLens Nothing) )

-- | Type synonym for the codec monad.  It is important to notice that by a
-- codec we denote here a structure of three 'C.AtomCodec's while in the
-- monad-codec package it denotes a monad.
type CodecM a b c d = C.Codec (Codec a b c) d

-- | Encode the observation and update the codec (only in the encoding
-- direction).
encodeObU :: Ord a => a -> CodecM a b c Ob
encodeObU = fmap Ob . C.encode' _1Lens

-- | Encode the observation and do *not* update the codec.
encodeObN :: Ord a => a -> CodecM a b c (Maybe Ob)
encodeObN = fmap (fmap Ob) . C.maybeEncode _1Lens

-- | Encode the label and update the codec.
encodeLbU :: (Ord b, Ord c) => (b, c) -> CodecM a b c Lb
encodeLbU (x, y) = do
    x' <- C.encode _2Lens (Just x)
    y' <- C.encode _3Lens (Just y)
    return (Lb1 x', Lb2 y')

-- | Encode the label and do *not* update the codec.
encodeLbN :: (Ord b, Ord c) => (b, c) -> CodecM a b c Lb
encodeLbN (x, y) = do
    x' <- C.maybeEncode _2Lens (Just x) >>= \mx -> case mx of
        Just x' -> return x'
        Nothing -> fromJust <$> C.maybeEncode _2Lens Nothing
    y' <- C.maybeEncode _3Lens (Just y) >>= \my -> case my of
        Just y' -> return y'
        Nothing -> fromJust <$> C.maybeEncode _3Lens Nothing
    return (Lb1 x', Lb2 y')

-- | Encode the labeled word and update the codec.
encodeWordL'Cu
    :: (Ord a, Ord b, Ord c)
    => WordL a (b, c)
    -> CodecM a b c (X Ob Lb, Y Lb)
encodeWordL'Cu (word, choice) = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbU lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encodec the labeled word and do *not* update the codec.
encodeWordL'Cn
    :: (Ord a, Ord b, Ord c)
    => WordL a (b, c)
    -> CodecM a b c (X Ob Lb, Y Lb)
encodeWordL'Cn (word, choice) = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    let x = mkX x' r'
    y  <- mkY <$> sequence
    	[ (,) <$> encodeLbN lb <*> pure pr
	| (lb, pr) <- (M.toList . unDist) choice ]
    return (x, y)

-- | Encode the word and update the codec.
encodeWord'Cu
    :: (Ord a, Ord b, Ord c)
    => Word a (b, c)
    -> CodecM a b c (X Ob Lb)
encodeWord'Cu word = do
    x' <- mapM encodeObU (S.toList (obs word))
    r' <- mapM encodeLbU (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the word and do *not* update the codec.
encodeWord'Cn
    :: (Ord a, Ord b, Ord c)
    => Word a (b, c)
    -> CodecM a b c (X Ob Lb)
encodeWord'Cn word = do
    x' <- catMaybes <$> mapM encodeObN (S.toList (obs word))
    r' <- mapM encodeLbN (S.toList (lbs word))
    return $ mkX x' r'

-- | Encode the labeled sentence and update the codec.
encodeSentL'Cu
    :: (Ord a, Ord b, Ord c)
    => SentL a (b, c)
    -> CodecM a b c (Xs Ob Lb, Ys Lb)
encodeSentL'Cu sent = do
    ps <- mapM (encodeWordL'Cu) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence and do *not* update the codec.
-- Substitute the default label for any label not present in the codec.
encodeSentL'Cn
    :: (Ord a, Ord b, Ord c)
    => SentL a (b, c)
    -> CodecM a b c (Xs Ob Lb, Ys Lb)
encodeSentL'Cn sent = do
    ps <- mapM (encodeWordL'Cn) sent
    return (V.fromList (map fst ps), V.fromList (map snd ps))

-- | Encode the labeled sentence with the given codec.  Substitute the
-- default label for any label not present in the codec.
encodeSentL
    :: (Ord a, Ord b, Ord c) => Codec a b c
    -> SentL a (b, c) -> (Xs Ob Lb, Ys Lb)
encodeSentL codec = C.evalCodec codec . encodeSentL'Cn

-- | Encode the sentence and update the codec.
encodeSent'Cu
    :: (Ord a, Ord b, Ord c) => Sent a (b, c)
    -> CodecM a b c (Xs Ob Lb)
encodeSent'Cu = fmap V.fromList . mapM encodeWord'Cu

-- | Encode the sentence and do *not* update the codec.
encodeSent'Cn
    :: (Ord a, Ord b, Ord c) => Sent a (b, c)
    -> CodecM a b c (Xs Ob Lb)
encodeSent'Cn = fmap V.fromList . mapM encodeWord'Cn

-- | Encode the sentence using the given codec.
encodeSent
    :: (Ord a, Ord b, Ord c) => Codec a b c
    -> Sent a (b, c) -> Xs Ob Lb
encodeSent codec = C.evalCodec codec . encodeSent'Cn

-- | Create the codec on the basis of the labeled dataset, return the
-- resultant codec and the encoded dataset.
mkCodec
    :: (Ord a, Ord b, Ord c) => [SentL a (b, c)]
    -> (Codec a b c, [(Xs Ob Lb, Ys Lb)])
mkCodec
    = swap
    . C.runCodec empty
    . mapM encodeSentL'Cu
  where
    swap (x, y) = (y, x)

-- | Encode the labeled dataset using the codec.  Substitute the default
-- label for any label not present in the codec.
encodeDataL
    :: (Ord a, Ord b, Ord c) => Codec a b c
    -> [SentL a (b, c)] -> [(Xs Ob Lb, Ys Lb)]
encodeDataL codec = C.evalCodec codec . mapM encodeSentL'Cn

-- | Encode the dataset with the codec.
encodeData
    :: (Ord a, Ord b, Ord c) => Codec a b c
    -> [Sent a (b, c)] -> [Xs Ob Lb]
encodeData codec = map (encodeSent codec)

-- | Decode the label within the codec monad.
decodeLabel'C
    :: (Ord b, Ord c) => Lb
    -> CodecM a b c (Maybe (b, c))
decodeLabel'C (x, y) = do
    x' <- C.decode _2Lens (unLb1 x)
    y' <- C.decode _3Lens (unLb2 y)
    return $ (,) <$> x' <*> y'

-- | Decode the label.
decodeLabel :: (Ord b, Ord c) => Codec a b c -> Lb -> Maybe (b, c)
decodeLabel codec = C.evalCodec codec . decodeLabel'C

-- | Decode the sequence of labels.
decodeLabels :: (Ord b, Ord c) => Codec a b c -> [Lb] -> [Maybe (b, c)]
decodeLabels codec = C.evalCodec codec . mapM decodeLabel'C

-- hasLabel :: Ord b => Codec a b -> b -> Bool
-- hasLabel codec x = M.member (Just x) (C.to $ snd codec)
-- {-# INLINE hasLabel #-}
-- 
-- -- | Return the label when 'Just' or one of the unknown values
-- -- when 'Nothing'.
-- unJust :: Ord b => Codec a b -> Word a b -> Maybe b -> b
-- unJust _ _ (Just x) = x
-- unJust codec word Nothing = case allUnk of
--     (x:_)   -> x
--     []      -> error "unJust: Nothing and all values known"
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
-- 
-- -- | Replace 'Nothing' labels with all unknown labels from
-- -- the set of potential interpretations.
-- unJusts :: Ord b => Codec a b -> Word a b -> [Maybe b] -> [b]
-- unJusts codec word xs =
--     concatMap deJust xs
--   where
--     allUnk = filter (not . hasLabel codec) (S.toList $ lbs word)
--     deJust (Just x) = [x]
--     deJust Nothing  = allUnk
