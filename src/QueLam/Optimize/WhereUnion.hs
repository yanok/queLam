{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereUnion where

import           Data.Row.Records

import           QueLam.Core


data WhereUnion repr (schema :: Row (Row *)) a where
  Union :: WhereUnion repr schema [a]
        -> WhereUnion repr schema [a]
        -> WhereUnion repr schema [a]
  Unknown :: repr schema a -> WhereUnion repr schema a

whereUnion :: Symantics repr => WhereUnion repr schema a -> repr schema a
whereUnion (Union xs ys) = whereUnion xs @% whereUnion ys
whereUnion (Unknown x)  = x

instance Symantics repr => RR WhereUnion repr where
  fwd = Unknown
  bwd = whereUnion

-- TODO: this is ugly, inefficient and unsafe! revisit later.
instance Symantics repr => Symantics (WhereUnion repr) where
  xs @% ys = Union xs ys
  where' c (Union xs ys) = where' c xs @% where' c ys
  where' c (Unknown xs) = fwd $ where' (bwd c) xs
