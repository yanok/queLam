{-# LANGUAGE TypeFamilies #-}
module QueLam.Core where

-- Implementation of Suzuki, Kiselyov and Kameyama paper

infix 5 =%
infixl 2 @%

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
  observe :: repr a -> Obs repr a
