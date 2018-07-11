{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereWhere where

import           Data.Row.Records

import           QueLam.Core


data WhereWhere repr (schema :: Row (Row *)) a where
  Where :: WhereWhere repr schema Bool
        -> WhereWhere repr schema [a]
        -> WhereWhere repr schema [a]
  Unknown :: repr schema a -> WhereWhere repr schema a

whereWhere :: Symantics repr => WhereWhere repr schema a -> repr schema a
whereWhere (Where c xs) = where' (whereWhere c) (whereWhere xs)
whereWhere (Unknown x)  = x


instance Symantics repr => RR WhereWhere repr where
  fwd = Unknown
  bwd = whereWhere

instance Symantics repr => Symantics (WhereWhere repr) where
  where' c (Where c' xs) = Where (c &&% c') xs
  where' c xs = Where c xs
