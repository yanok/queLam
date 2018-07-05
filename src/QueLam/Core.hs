{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module QueLam.Core where

import           Data.Proxy
import           GHC.TypeLits
import           SuperRecord

-- Implementation of Suzuki, Kiselyov and Kameyama paper


-- type level stuff for schema
-- the goal is to match superrecord, so we could use
-- records to represent DB in R representation
type family SchemaSize (schema :: [(Symbol, [*])]) :: Nat where
  SchemaSize '[] = 0
  SchemaSize (_ ': rs) = 1 + SchemaSize rs

type family SchemaIdxH (i :: Nat) (t :: Symbol) (schema :: [(Symbol, [*])]) :: Nat where
  SchemaIdxH idx t ('(t, r) ': rs) = idx
  SchemaIdxH idx t1 ('(t2, r) ': rs) = SchemaIdxH (1 + idx) t1 rs
  SchemaIdxH idx t '[] = TypeError ( 'Text "Could not find table " ':<>: 'Text t :<>: 'Text " in schema")

type SchemaIdx t schema = SchemaSize schema - SchemaIdxH 0 t schema - 1

type family SchemaTys (t :: Symbol) (schema :: [(Symbol, [*])]) :: [*] where
  SchemaTys t ('(t, r) ': rs) = r
  SchemaTys t1 ('(t2, r) ': rs) = SchemaTys t1 rs

type HasT t schema r =
  ( SchemaTys t schema ~ r
  , KnownNat (SchemaSize schema)
  , KnownNat (SchemaIdx t schema))

infixl 1 $$
infix 4 =%
infixl 5 @%
infixl 8 .%
infixr 2 &%
infixl 7 *%

class Symantics repr where
  type Obs repr a :: *
  type Handle repr (schema :: [(Symbol, [*])]) :: *
  int    :: Int    -> repr schema Int
  bool   :: Bool   -> repr schema Bool
  string :: String -> repr schema String
  lam    :: (repr schema a -> repr schema b) -> repr schema (a -> b)
  ($$)   :: repr schema (a -> b) -> repr schema a -> repr schema b
  for    :: repr schema [a] -> (repr schema a -> repr schema [b]) -> repr schema [b]
  where' :: repr schema Bool -> repr schema [a] -> repr schema [a]
  yield  :: repr schema a -> repr schema [a]
  nil    :: repr schema [a]
  (@%)   :: repr schema [a] -> repr schema [a] -> repr schema [a]
  (=%)   :: Eq a => repr schema a -> repr schema a -> repr schema Bool
  (*%)   :: Num a => repr schema a -> repr schema a -> repr schema a
  (.%)   :: (KnownSymbol l, Has l rs t)
         => repr schema (Rec rs) -> FldProxy l -> repr schema t
  rnil'  :: repr schema (Rec '[])
  (&%)   ::
    ( KnownSymbol l
    , RecSize flds ~ s
    , sortedFlds ~ Sort ((l := t) ': flds)
    , KnownNat s
    , KnownNat (RecVecIdxPos l sortedFlds)
    , KeyDoesNotExist l flds
    , RecCopy flds flds sortedFlds) =>
    (l := repr schema t) -> repr schema (Rec flds) -> repr schema (Rec sortedFlds)
  table  :: (KnownSymbol t, HasT t schema r)
         => Handle repr schema -> FldProxy t -> repr schema [Record r]
  observe :: repr schema a -> Obs repr a

compose :: Symantics repr => repr schema ((a -> [b]) -> (b -> [c]) -> (a -> [c]))
compose = lam $ \q -> lam $ \r -> lam $ \x -> for (q $$ x) $ \y -> r $$ y
