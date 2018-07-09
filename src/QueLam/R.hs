{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
module QueLam.R where

import           Data.Coerce
import Data.Type.Equality
import           Data.Functor.Identity
import GHC.TypeLits
import           SuperRecord
import Data.Constraint
import Unsafe.Coerce

import           QueLam.Core

newtype R (schema :: [(Symbol, [*])]) a = R { getR :: a }

type family DB' (schema :: [(Symbol, [*])]) where
  DB' '[] = '[]
  DB' ('(l, r) ': fs) = l := [Rec r] ': DB' fs

type DB schema = Record (DB' schema)

-- recSizeOfDB :: forall schema. RecSize (DB' schema) :~: RecSize schema
-- recSizeOfDB = undefined

instance Symantics R where
  type Obs R a = a
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
  (R e1) &&% (R e2) = R $ e1 && e2
  (R e) .% l = R $ get l e
  rnil' = R rnil
  (l := (R x)) &% (R r) = R (l := x & r)
  table  :: forall t schema r . HasT t schema r => Handle R schema -> FldProxy t -> R schema [Record r]
  table h t = R $ withDict (unsafeCoerce (Dict :: Dict (HasT t schema r)) :: Dict (Has t (Sort (DB' schema)) [Record r]))
            $ get t h
  observe = coerce
