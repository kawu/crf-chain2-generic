module Data.CRF.Chain2.Pair.Codec
( CodecData
, obMax
, lb1Max
, lb2Max
, codec
) where

import Control.Applicative ((<$>), (<*>))
import Control.Comonad.Trans.Store (store)
import Data.Maybe (fromJust)
import Data.Lens.Common (Lens(..))
import qualified Data.Map as M
import qualified Control.Monad.Codec as C

import Data.CRF.Chain2.Generic.Codec (Codec(..))
import Data.CRF.Chain2.Pair.Base

-- | A codec.  The first component is used to encode observations
-- of type a, the second one is used to encode labels of type b,
-- third -- labels of type c from the third level.
type CodecData a b c =
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
obMax :: CodecData a b c -> Ob
obMax =
    let idMax m = M.size m - 1
    in  Ob . idMax . C.to . _1

-- | The maximum internal label included in the codec.
lb1Max :: CodecData a b c -> Lb1
lb1Max =
    let idMax m = M.size m - 1
    in  Lb1 . idMax . C.to . _2

-- | The maximum internal label included in the codec.
lb2Max :: CodecData a b c -> Lb2
lb2Max =
    let idMax m = M.size m - 1
    in  Lb2 . idMax . C.to . _3

codec :: (Ord a, Ord b, Ord c) => Codec a (b, c) (CodecData a b c) Ob Lb
codec = Codec
    { empty = 
        ( C.empty
        , C.execCodec C.empty (C.encode C.idLens Nothing)
        , C.execCodec C.empty (C.encode C.idLens Nothing) )
    , encodeObU = fmap Ob . C.encode' _1Lens
    , encodeObN = fmap (fmap Ob) . C.maybeEncode _1Lens
    , encodeLbU = \ (x, y) -> do
        x' <- C.encode _2Lens (Just x)
        y' <- C.encode _3Lens (Just y)
        return (Lb1 x', Lb2 y')
    , encodeLbN = \ (x, y) -> do
        x' <- C.maybeEncode _2Lens (Just x) >>= \mx -> case mx of
            Just x' -> return x'
            Nothing -> fromJust <$> C.maybeEncode _2Lens Nothing
        y' <- C.maybeEncode _3Lens (Just y) >>= \my -> case my of
            Just y' -> return y'
            Nothing -> fromJust <$> C.maybeEncode _3Lens Nothing
        return (Lb1 x', Lb2 y')
    , decodeLbC = \ (x, y) -> do
        x' <- C.decode _2Lens (unLb1 x)
        y' <- C.decode _3Lens (unLb2 y)
        return $ (,) <$> x' <*> y'
    , hasLabel = \ cdcData (x, y)
        -> M.member (Just x) (C.to $ _2 cdcData)
        && M.member (Just y) (C.to $ _3 cdcData) }
