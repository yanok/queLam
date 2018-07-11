{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForEmpty1 where

import           Data.Row.Records

import           QueLam.Core


data ForEmpty1 repr (schema :: Row (Row *)) a where
  Nil :: ForEmpty1 repr schema [a]
  Unknown :: repr schema a -> ForEmpty1 repr schema a

forEmpty1 :: Symantics repr => ForEmpty1 repr schema a -> repr schema a
forEmpty1 Nil = nil
forEmpty1 (Unknown x)  = x


instance Symantics repr => RR ForEmpty1 repr where
  fwd = Unknown
  bwd = forEmpty1

instance Symantics repr => Symantics (ForEmpty1 repr) where
  nil = Nil
  for Nil _ = Nil
  for (Unknown xs) f = fwd $ for xs $ \x -> bwd $ f $ fwd x
