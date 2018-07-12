{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module QueLam.Core where

import           Data.Row.Records

-- Implementation of Suzuki, Kiselyov and Kameyama paper


-- schema is Row (Row *)

-- RR stuff to reduce boilerplate, as proposed by Roman Cheplyaka
class RR t (repr :: Row (Row *) -> * -> *) where
  fwd :: repr schema a -> t repr schema a
  bwd :: t repr schema a -> repr schema a

type family Peel (repr :: Row (Row *) -> * -> *) :: Row (Row *) -> * -> * where
  Peel (t repr) = repr

infixl 1 $$
infix 4 =%
infixl 5 @%
infixl 8 .%
infixl 7 *%
infixl 3 &&%

class Symantics (repr :: Row (Row *) -> * -> *) where
  type Obs repr a :: *
  type Obs repr a = Obs (Peel repr) a

  type Handle repr (schema :: Row (Row *)) :: *
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
  (&&%)  :: repr schema Bool -> repr schema Bool -> repr schema Bool
  default (&&%) :: (RR t repr', Symantics repr', repr ~ t repr')
                => repr schema Bool -> repr schema Bool -> repr schema Bool
  x &&% y = fwd $ bwd x &&% bwd y
  (.%)   :: (KnownSymbol l, r .! l ≈ a)
         => repr schema (Rec r) -> Label l -> repr schema a
  default (.%) :: ( RR t repr', Symantics repr', repr ~ t repr'
                  , KnownSymbol l, rs .! l ≈ a)
               => repr schema (Rec rs) -> Label l -> repr schema a
  x .% l = fwd $ bwd x .% l
  rcrd :: WellBehaved r => Rec (Map (repr schema) r) -> repr schema (Rec r)
  default rcrd :: forall schema row t repr'
                . (RR t repr', Symantics repr', repr ~ t repr', WellBehaved row)
               => Rec (Map (repr schema) row) -> repr schema (Rec row)
  rcrd r = fwd $ rcrd $ transform' @row @(repr schema) bwd r
  table  :: (KnownSymbol t, HasType t r schema)
         => Handle repr schema -> Label t -> repr schema [Rec r]
  default table :: ( RR t repr'
                   , Symantics repr'
                   , repr ~ t repr'
                   , Handle repr schema ~ Handle repr' schema
                   , KnownSymbol tbl, HasType tbl r schema)
                => Handle repr schema -> Label tbl -> repr schema [Rec r]
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
