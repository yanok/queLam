{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.WhereFor where

import           Data.Row.Records

import           QueLam.Core


data WhereFor repr (schema :: Row (Row *)) a where
  For :: WhereFor repr schema [a]
      -> (WhereFor repr schema a -> WhereFor repr schema [b])
      -> WhereFor repr schema [b]
  Unknown :: repr schema a -> WhereFor repr schema a

whereFor :: Symantics repr => WhereFor repr schema a -> repr schema a
whereFor (For xs f)  = for (whereFor xs) $ \x -> whereFor $ f $ Unknown x
whereFor (Unknown x) = x


instance Symantics repr => RR WhereFor repr where
  fwd = Unknown
  bwd = whereFor

instance Symantics repr => Symantics (WhereFor repr) where
  for xs f = For xs f
  where' c (Unknown xs) = fwd $ where' (bwd c) xs
  where' c (For xs f) = For xs $ \x -> where' c (f x)
