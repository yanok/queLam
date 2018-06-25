{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QueLam.Core where

import           Data.Proxy
import           GHC.TypeLits
import           SuperRecord

-- Implementation of Suzuki, Kiselyov and Kameyama paper

infix 5 =%
infixl 2 @%
infixl 8 .%

class HasTable schema (t :: Symbol) r

class Symantics repr where
  type Obs repr :: * -> *
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
  (=%)   :: repr schema a -> repr schema a -> repr schema Bool
  (.%)   :: Has l rs t => repr schema (Rec rs) -> FldProxy l -> repr schema t
  rnil'  :: repr schema (Rec '[])
  (&%)   :: forall l schema t flds. (l := repr schema t) -> repr schema (Rec flds) -> repr schema (Record (l := t ': flds))
  table  :: HasTable schema t r => Proxy t -> repr schema [r]
  observe :: repr schema a -> Obs repr a
