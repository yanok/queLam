{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module QueLam.R where

import           Data.Coerce
import Data.Type.Equality
import           Data.Functor.Identity
import GHC.TypeLits
import           SuperRecord
import Unsafe.Coerce

import           QueLam.Core

newtype R (schema :: [(Symbol, [*])]) a = R { getR :: a }

type family DB' (schema :: [(Symbol, [*])]) where
  DB' '[] = '[]
  DB' ('(l, r) ': fs) = l := [Rec r] ': DB' fs

type DB schema = Record (DB' schema)

recSizeOfDB :: forall schema. RecSize (DB' schema) :~: RecSize schema
recSizeOfDB = undefined

instance Symantics R where
  type Obs R = Identity
  type Handle R schema = DB schema
  int i = R i
  bool b = R b
  string s = R s
  lam f = coerce f
  (R e1) $$ (R e2) = R $ e1 e2
  for (R []) _     = R []
  for (R (x:xs)) f = R $ getR (f $ R x) ++ getR (for (R xs) f)
  where' (R True) e  = e
  where' (R False) _ = R []
  yield (R e) = R [e]
  nil = R []
  (R e1) @% (R e2) = R $ e1 ++ e2
  (R e1) =% (R e2) = R $ e1 == e2
  (R e1) *% (R e2) = R $ e1 * e2
  (R e) .% l = R $ get l e
  rnil' = R rnil
  (l := (R x)) &% (R r) = R (l := x & r)
  table h t = R $ unsafeCoerce $ get t h
  observe = coerce
