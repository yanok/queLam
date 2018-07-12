{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module QueLam.Extension.SetUnion where

import           Data.Text.Prettyprint.Doc
import           Data.List                 (union)

import           QueLam.Core
import           QueLam.P
import           QueLam.R

infixl 5 @^

class SymanticsS repr where
  (@^) :: Eq a => repr schema [a] -> repr schema [a] -> repr schema [a]
  default (@^) :: (RR t repr', SymanticsS repr', t repr' ~ repr, Eq a)
               => repr schema [a] -> repr schema [a] -> repr schema [a]
  xs @^ ys = fwd $ bwd xs @^ bwd ys

instance SymanticsS P where
  (P e1) @^ (P e2) = P $ \n l ->
    parenthesize (l > 5) $ e1 n 5 <+> "@^" <+> e2 n 6

instance SymanticsS R where
  (R xs) @^ (R ys) = R $ union xs ys
