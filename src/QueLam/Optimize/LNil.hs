{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.LNil where

import           Data.Row.Records

import           QueLam.Core


data LNil repr (schema :: Row (Row *)) a where
  Empty :: LNil repr schema [a]
  Unknown :: repr schema a -> LNil repr schema a

lnil :: Symantics repr => LNil repr schema a -> repr schema a
lnil Empty       = nil
lnil (Unknown x) = x


instance Symantics repr => RR LNil repr where
  fwd = Unknown
  bwd = lnil

instance Symantics repr => Symantics (LNil repr) where
  nil = Empty
  Empty @% y = y
  (Unknown x) @% y = Unknown $ x @% lnil y
