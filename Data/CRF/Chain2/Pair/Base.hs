{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.CRF.Chain2.Pair.Base
( Ob (..)
, Lb1 (..)
, Lb2 (..)
, Lb
, Feat (..)
, pairFeatGen
) where

import Data.Binary (Binary)

import Data.CRF.Chain2.Generic.Model (FeatGen(..))

newtype Ob  = Ob  { unOb  :: Int } deriving (Show, Eq, Ord, Binary)
newtype Lb1 = Lb1 { unLb1 :: Int } deriving (Show, Eq, Ord, Binary)
newtype Lb2 = Lb2 { unLb2 :: Int } deriving (Show, Eq, Ord, Binary)
type Lb = (Lb1, Lb2)

data Feat
    = OFeat'1   {-# UNPACK #-} !Ob  {-# UNPACK #-} !Lb1
    | OFeat'2   {-# UNPACK #-} !Ob  {-# UNPACK #-} !Lb2
    | TFeat3'1  {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1
    | TFeat3'2  {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2
    | TFeat2'1  {-# UNPACK #-} !Lb1 {-# UNPACK #-} !Lb1
    | TFeat2'2  {-# UNPACK #-} !Lb2 {-# UNPACK #-} !Lb2
    | TFeat1'1  {-# UNPACK #-} !Lb1
    | TFeat1'2  {-# UNPACK #-} !Lb2
    deriving (Show, Eq, Ord)

pairFeatGen :: FeatGen Ob (Lb1, Lb2) Feat
pairFeatGen = FeatGen
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
