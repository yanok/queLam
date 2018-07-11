{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForUnionAll2 where

import           Data.Row.Records

import           QueLam.Core


data ForUnionAll2 repr (schema :: Row (Row *)) a where
  Dummy :: ForUnionAll2 repr schema a
  Union :: ForUnionAll2 repr schema [a]
        -> ForUnionAll2 repr schema [a]
        -> ForUnionAll2 repr schema [a]
  Unknown :: repr schema a -> ForUnionAll2 repr schema a

forUnionAll2 :: Symantics repr => ForUnionAll2 repr schema a -> repr schema a
forUnionAll2 (Union xs ys) = forUnionAll2 xs @% forUnionAll2 ys
forUnionAll2 (Unknown x)  = x
forUnionAll2 Dummy = error "should not happen"

instance Symantics repr => RR ForUnionAll2 repr where
  fwd = Unknown
  bwd = forUnionAll2

-- TODO: this is ugly, inefficient and unsafe! revisit later.
instance Symantics repr => Symantics (ForUnionAll2 repr) where
  xs @% ys = Union xs ys
  for vs f = case f Dummy of
    Union _ _ -> for vs (\v -> case f v of
                                 Union xs _ -> xs
                                 _ -> error "impossible"
                        ) @%
                 for vs (\v -> case f v of
                                 Union _ ys -> ys
                                 _ -> error "impossible")
    _ -> fwd $ for (bwd vs) $ \v -> bwd $ f $ fwd v
