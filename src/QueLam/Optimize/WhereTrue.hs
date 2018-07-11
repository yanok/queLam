{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereTrue where

import           Data.Row.Records

import           QueLam.Core


data WhereTrue repr (schema :: Row (Row *)) a where
  BTrue :: WhereTrue repr schema Bool
  Unknown :: repr schema a -> WhereTrue repr schema a

whereTrue :: Symantics repr => WhereTrue repr schema a -> repr schema a
whereTrue BTrue = bool True
whereTrue (Unknown x)  = x


instance Symantics repr => RR WhereTrue repr where
  fwd = Unknown
  bwd = whereTrue

instance Symantics repr => Symantics (WhereTrue repr) where
  bool True = BTrue
  bool b = fwd $ bool b
  where' BTrue xs = xs
  where' (Unknown c) xs = fwd $ where' c $ bwd xs
