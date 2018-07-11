{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForWhere where

import           Data.Row.Records

import           QueLam.Core


data ForWhere repr (schema :: Row (Row *)) a where
  Where :: ForWhere repr schema Bool
        -> ForWhere repr schema [a]
        -> ForWhere repr schema [a]
  Unknown :: repr schema a -> ForWhere repr schema a

forWhere :: Symantics repr => ForWhere repr schema a -> repr schema a
forWhere (Where c xs) = where' (forWhere c) (forWhere xs)
forWhere (Unknown x)  = x


instance Symantics repr => RR ForWhere repr where
  fwd = Unknown
  bwd = forWhere

instance Symantics repr => Symantics (ForWhere repr) where
  where' c xs = Where c xs
  for (Unknown xs) f = fwd $ for xs $ \x -> bwd $ f $ Unknown x
  for (Where c xs) f = Where c (for xs f)
