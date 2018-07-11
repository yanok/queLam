{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForUnionAll1 where

import           Data.Row.Records

import           QueLam.Core


data ForUnionAll1 repr (schema :: Row (Row *)) a where
  Union :: ForUnionAll1 repr schema [a]
        -> ForUnionAll1 repr schema [a]
        -> ForUnionAll1 repr schema [a]
  Unknown :: repr schema a -> ForUnionAll1 repr schema a

forUnionAll1 :: Symantics repr => ForUnionAll1 repr schema a -> repr schema a
forUnionAll1 (Union xs ys) = forUnionAll1 xs @% forUnionAll1 ys
forUnionAll1 (Unknown x)  = x


instance Symantics repr => RR ForUnionAll1 repr where
  fwd = Unknown
  bwd = forUnionAll1

instance Symantics repr => Symantics (ForUnionAll1 repr) where
  xs @% ys = Union xs ys
  for (Union xs ys) f = for xs f @% for ys f
  for (Unknown xs) f = fwd $ for xs $ \x -> bwd $ f $ fwd x
