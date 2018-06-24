{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module QueLam.Core where

import           GHC.TypeLits

-- Implementation of Suzuki, Kiselyov and Kameyama paper

infix 5 =%
infixl 2 @%

class HasTable db (t :: Symbol) r

class Symantics repr where
  type Obs repr :: * -> *
  int    :: Int    -> repr Int
  bool   :: Bool   -> repr Bool
  string :: String -> repr String
  lam    :: (repr a -> repr b) -> repr (a -> b)
  ($$)   :: repr (a -> b) -> repr a -> repr b
  for    :: repr [a] -> (repr a -> repr [b]) -> repr [b]
  where' :: repr Bool -> repr [a] -> repr [a]
  yield  :: repr a -> repr [a]
  nil    :: repr [a]
  (@%)   :: repr [a] -> repr [a] -> repr [a]
  (=%)   :: repr a -> repr a -> repr Bool
  table  :: forall db t r. HasTable db t r => repr [r]
  observe :: repr a -> Obs repr a
