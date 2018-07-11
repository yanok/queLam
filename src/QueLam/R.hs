{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module QueLam.R where

import           Data.Coerce
import           Data.Constraint
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Row.Extra
import           Data.Row.Records
import qualified Data.Row.Records      as R
import           Data.Type.Equality
import           GHC.TypeLits
import           Unsafe.Coerce

import           QueLam.Core

newtype R (schema :: Row (Row *)) a = R { getR :: a }
  deriving (Functor)

instance Applicative (R schema) where
  pure = R
  R f <*> R x = R $ f x

type DB schema = Rec (Map (Compose [] Rec) schema)

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
  (R e) .% l = R $ e .! l
  rcrd = R.sequence
  table :: forall t schema r . (KnownSymbol t, schema .! t ≈ r)
        => Handle R schema -> Label t -> R schema [Rec r]
  table h t = R $ getCompose $ h .! t \\ mapHas @(Compose [] Rec) @schema @t @r
  observe = coerce
