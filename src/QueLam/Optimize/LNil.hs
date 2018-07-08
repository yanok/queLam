{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module QueLam.Optimize.LNil where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core


data AnnRepr repr (schema :: [(Symbol, [*])]) a where
  Empty :: AnnRepr repr schema [a]
  Unknown :: repr schema a -> AnnRepr repr schema a

dyn :: Symantics repr => AnnRepr repr schema a -> repr schema a
dyn Empty       = nil
dyn (Unknown x) = x


instance Symantics repr => Symantics (AnnRepr repr) where
  type Obs (AnnRepr repr) a = Obs repr a
  type Handle (AnnRepr repr) schema = Handle repr schema
  int n = Unknown $ int n
  bool b = Unknown $ bool b
  string s = Unknown $ string s
  lam f = Unknown $ lam $ \x -> dyn $ f $ Unknown x
  x $$ y = Unknown $ dyn x $$ dyn y
  for xs f = Unknown $ for (dyn xs) $ \x -> dyn $ f $ Unknown x
  where' c xs = Unknown $ where' (dyn c) (dyn xs)
  yield x = Unknown $ yield $ dyn x
  nil = Empty
  Empty @% y = y
  (Unknown x) @% y = Unknown $ x @% dyn y
  x =% y = Unknown $ dyn x =% dyn y
  x *% y = Unknown $ dyn x *% dyn y
  r .% l = Unknown $ dyn r .% l
  rnil' = Unknown rnil'
  (l := v) &% r = Unknown $ (l := dyn v) &% dyn r
  table h t = Unknown $ table h t
  observe = observe . dyn
