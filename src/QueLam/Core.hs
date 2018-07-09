{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
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

-- RR stuff to reduce boilerplate, as proposed by Roman Cheplyaka
class RR t (repr :: [(Symbol, [*])] -> * -> *) where
  fwd :: repr schema a -> t repr schema a
  bwd :: t repr schema a -> repr schema a

type family Peel (repr :: [(Symbol, [*])] -> * -> *) :: [(Symbol, [*])] -> * -> * where
  Peel (t repr) = repr

infixl 1 $$
infix 4 =%
infixl 5 @%
infixl 8 .%
infixr 2 &%
infixl 7 *%

class Symantics repr where
  type Obs repr a :: *
  type Obs repr a = Obs (Peel repr) a

  type Handle repr (schema :: [(Symbol, [*])]) :: *
  type Handle repr schema = Handle (Peel repr) schema

  int    :: Int    -> repr schema Int
  default int :: (RR t repr', Symantics repr', repr ~ t repr')
              => Int -> repr schema Int
  int = fwd . int
  bool   :: Bool   -> repr schema Bool
  default bool :: (RR t repr', Symantics repr', repr ~ t repr')
               => Bool -> repr schema Bool
  bool = fwd . bool
  string :: String -> repr schema String
  default string :: (RR t repr', Symantics repr', repr ~ t repr')
                 => String -> repr schema String
  string = fwd . string
  lam    :: (repr schema a -> repr schema b) -> repr schema (a -> b)
  default lam :: (RR t repr', Symantics repr', repr ~ t repr')
             => (repr schema a -> repr schema b)
             -> repr schema (a -> b)
  lam f = fwd $ lam $ bwd . f . fwd
  ($$)   :: repr schema (a -> b) -> repr schema a -> repr schema b
  default ($$) :: (RR t repr', Symantics repr', repr ~ t repr')
               => repr schema (a -> b)
               -> repr schema a
               -> repr schema b
  f $$ x = fwd $ bwd f $$ bwd x
  for    :: repr schema [a] -> (repr schema a -> repr schema [b]) -> repr schema [b]
  default for :: (RR t repr', Symantics repr', repr ~ t repr')
              => repr schema [a]
              -> (repr schema a -> repr schema [b])
              -> repr schema [b]
  for xs f = fwd $ for (bwd xs) $ \x -> bwd (f $ fwd x)
  where' :: repr schema Bool -> repr schema [a] -> repr schema [a]
  default where' :: (RR t repr', Symantics repr', repr ~ t repr')
                 => repr schema Bool -> repr schema [a] -> repr schema [a]
  where' c xs = fwd $ where' (bwd c) (bwd xs)
  yield  :: repr schema a -> repr schema [a]
  default yield :: (RR t repr', Symantics repr', repr ~ t repr')
                => repr schema a -> repr schema [a]
  yield = fwd . yield . bwd
  nil    :: repr schema [a]
  default nil :: (RR t repr', Symantics repr', repr ~ t repr')
              => repr schema [a]
  nil = fwd nil
  (@%)   :: repr schema [a] -> repr schema [a] -> repr schema [a]
  default (@%) :: (RR t repr', Symantics repr', repr ~ t repr')
               => repr schema [a] -> repr schema [a] -> repr schema [a]
  xs @% ys = fwd $ bwd xs @% bwd ys
  (=%)   :: Eq a => repr schema a -> repr schema a -> repr schema Bool
  default (=%) :: (RR t repr', Symantics repr', repr ~ t repr', Eq a)
               => repr schema a -> repr schema a -> repr schema Bool
  x =% y = fwd $ bwd x =% bwd y
  (*%)   :: Num a => repr schema a -> repr schema a -> repr schema a
  default (*%) :: (RR t repr', Symantics repr', repr ~ t repr', Num a)
               => repr schema a -> repr schema a -> repr schema a
  x *% y = fwd $ bwd x *% bwd y
  (.%)   :: (KnownSymbol l, Has l rs t)
         => repr schema (Rec rs) -> FldProxy l -> repr schema t
  default (.%) :: ( RR t repr', Symantics repr', repr ~ t repr'
                  , KnownSymbol l, Has l rs a)
               => repr schema (Rec rs) -> FldProxy l -> repr schema a
  x .% l = fwd $ bwd x .% l
  rnil'  :: repr schema (Rec '[])
  default rnil' :: (RR t repr', Symantics repr', repr ~ t repr')
                => repr schema (Rec '[])
  rnil' = fwd rnil'
  (&%)   ::
    ( KnownSymbol l
    , RecSize flds ~ s
    , sortedFlds ~ Sort ((l := t) ': flds)
    , KnownNat s
    , KnownNat (RecVecIdxPos l sortedFlds)
    , KeyDoesNotExist l flds
    , RecCopy flds flds sortedFlds) =>
    (l := repr schema t) -> repr schema (Rec flds) -> repr schema (Rec sortedFlds)
  default (&%) :: ( RR t repr'
                  , Symantics repr'
                  , repr ~ t repr'
                  , KnownSymbol l
                  , RecSize flds ~ s
                  , sortedFlds ~ Sort ((l := a) ': flds)
                  , KnownNat s
                  , KnownNat (RecVecIdxPos l sortedFlds)
                  , KeyDoesNotExist l flds
                  , RecCopy flds flds sortedFlds)
               => (l := repr schema a)
               -> repr schema (Rec flds)
               -> repr schema (Rec sortedFlds)
  l := x &% r = fwd $ l := bwd x &% bwd r
  table  :: (KnownSymbol t, HasT t schema r)
         => Handle repr schema -> FldProxy t -> repr schema [Record r]
  default table :: ( RR t repr'
                   , Symantics repr'
                   , repr ~ t repr'
                   , Handle repr schema ~ Handle repr' schema
                   , KnownSymbol tbl, HasT tbl schema r)
                => Handle repr schema -> FldProxy tbl -> repr schema [Record r]
  table h t = fwd $ table h t
  observe :: repr schema a -> Obs repr a
  default observe :: ( RR t repr'
                     , Symantics repr'
                     , repr ~ t repr'
                     , Obs repr a ~ Obs repr' a)
                  => repr schema a -> Obs repr a
  observe = observe . bwd

compose :: Symantics repr => repr schema ((a -> [b]) -> (b -> [c]) -> (a -> [c]))
compose = lam $ \q -> lam $ \r -> lam $ \x -> for (q $$ x) $ \y -> r $$ y
