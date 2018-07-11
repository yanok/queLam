{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereFalse where

import           Data.Row.Records

import           QueLam.Core


data WhereFalse repr (schema :: Row (Row *)) a where
  BFalse :: WhereFalse repr schema Bool
  Unknown :: repr schema a -> WhereFalse repr schema a

whereFalse :: Symantics repr => WhereFalse repr schema a -> repr schema a
whereFalse BFalse = bool False
whereFalse (Unknown x)  = x


instance Symantics repr => RR WhereFalse repr where
  fwd = Unknown
  bwd = whereFalse

instance Symantics repr => Symantics (WhereFalse repr) where
  bool False = BFalse
  bool b = fwd $ bool b
  where' BFalse _ = nil
  where' (Unknown c) xs = fwd $ where' c $ bwd xs
