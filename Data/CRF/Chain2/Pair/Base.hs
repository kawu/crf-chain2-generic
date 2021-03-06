{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Chain2.Pair.Base
( Ob (..)
, Lb1 (..)
, Lb2 (..)
, Lb
, Feat (..)
, featGen
) where

import Control.Applicative ((<$>), (<*>)) 
import Data.Ix (Ix)
import Data.Binary (Binary, get, put, Put, Get)

import Data.CRF.Chain2.Generic.Model (FeatGen(..))

newtype Ob  = Ob  { unOb  :: Int } deriving (Show, Eq, Ord, Ix, Binary)
newtype Lb1 = Lb1 { unLb1 :: Int } deriving (Show, Eq, Ord, Ix, Binary)
newtype Lb2 = Lb2 { unLb2 :: Int } deriving (Show, Eq, Ord, Ix, Binary)
type Lb = (Lb1, Lb2)

data Feat
    = TFeat3'1  {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1
    | TFeat3'2  {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2
    | TFeat2'1  {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1
    | TFeat2'2  {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2
    | TFeat1'1  {-# UNPACK #-} !Lb1
    | TFeat1'2  {-# UNPACK #-} !Lb2
    | OFeat'1   {-# UNPACK #-} !Ob  {-# UNPACK #-} !Lb1
    | OFeat'2   {-# UNPACK #-} !Ob  {-# UNPACK #-} !Lb2
    deriving (Show, Eq, Ord)

instance Binary Feat where
    put (OFeat'1 o x)       = putI 0 >> put o >> put x
    put (OFeat'2 o x)       = putI 1 >> put o >> put x
    put (TFeat3'1 x y z)    = putI 2 >> put x >> put y >> put z
    put (TFeat3'2 x y z)    = putI 3 >> put x >> put y >> put z
    put (TFeat2'1 x y)      = putI 4 >> put x >> put y
    put (TFeat2'2 x y)      = putI 5 >> put x >> put y
    put (TFeat1'1 x)        = putI 6 >> put x
    put (TFeat1'2 x)        = putI 7 >> put x
    get = getI >>= \i -> case i of
        0   -> OFeat'1  <$> get <*> get
        1   -> OFeat'2  <$> get <*> get
        2   -> TFeat3'1 <$> get <*> get <*> get
        3   -> TFeat3'2 <$> get <*> get <*> get
        4   -> TFeat2'1 <$> get <*> get
        5   -> TFeat2'2 <$> get <*> get
        6   -> TFeat1'1 <$> get
        7   -> TFeat1'2 <$> get
        _   -> error "get feature: unknown code"

putI :: Int -> Put
putI = put
{-# INLINE putI #-}

getI :: Get Int
getI = get
{-# INLINE getI #-}

featGen :: FeatGen Ob (Lb1, Lb2) Feat
featGen = FeatGen
    { obFeats   = obFeats'
    , trFeats1  = trFeats1'
    , trFeats2  = trFeats2'
    , trFeats3  = trFeats3' }
  where
    obFeats' ob (x, y) =
        [ OFeat'1 ob x
        , OFeat'2 ob y ]
    trFeats1' (x, y) =
        [ TFeat1'1 x
        , TFeat1'2 y ]
    trFeats2' (x1, y1) (x2, y2) =
        [ TFeat2'1 x1 x2
        , TFeat2'2 y1 y2 ]
    trFeats3' (x1, y1) (x2, y2) (x3, y3) =
        [ TFeat3'1 x1 x2 x3
        , TFeat3'2 y1 y2 y3 ]
