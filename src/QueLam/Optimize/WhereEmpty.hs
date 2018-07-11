{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereEmpty where

import           Data.Row.Records

import           QueLam.Core


data WhereEmpty repr (schema :: Row (Row *)) a where
  Nil :: WhereEmpty repr schema [a]
  Unknown :: repr schema a -> WhereEmpty repr schema a

whereEmpty :: Symantics repr => WhereEmpty repr schema a -> repr schema a
whereEmpty Nil = nil
whereEmpty (Unknown x)  = x

instance Symantics repr => RR WhereEmpty repr where
  fwd = Unknown
  bwd = whereEmpty

instance Symantics repr => Symantics (WhereEmpty repr) where
  nil = Nil
  where' _ Nil = nil
  where' c (Unknown xs) = fwd $ where' (bwd c) xs
