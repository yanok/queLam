{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForEmpty2 where

import           Data.Row.Records

import           QueLam.Core


data ForEmpty2 repr (schema :: Row (Row *)) a where
  Dummy :: ForEmpty2 repr schema a
  Nil :: ForEmpty2 repr schema [a]
  Unknown :: repr schema a -> ForEmpty2 repr schema a

forEmpty2 :: Symantics repr => ForEmpty2 repr schema a -> repr schema a
forEmpty2 Nil = nil
forEmpty2 (Unknown x)  = x
forEmpty2 Dummy = error "should not happen"

instance Symantics repr => RR ForEmpty2 repr where
  fwd = Unknown
  bwd = forEmpty2

-- TODO: this is better than ForUnionAll2 but still...
instance Symantics repr => Symantics (ForEmpty2 repr) where
  nil = Nil
  for vs f = case f Dummy of
    Nil -> nil
    _ -> fwd $ for (bwd vs) $ \v -> bwd $ f $ fwd v
