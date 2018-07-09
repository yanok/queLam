{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForYield where

import           GHC.TypeLits

import           QueLam.Core


data ForYield repr (schema :: [(Symbol, [*])]) a where
  Yield :: ForYield repr schema a
        -> ForYield repr schema [a]
  Unknown :: repr schema a -> ForYield repr schema a

forYield :: Symantics repr => ForYield repr schema a -> repr schema a
forYield (Yield x) = yield $ forYield x
forYield (Unknown x) = x


instance Symantics repr => RR ForYield repr where
  fwd = Unknown
  bwd = forYield

instance Symantics repr => Symantics (ForYield repr) where
  yield = Yield
  for (Unknown xs) f = fwd $ for xs $ \x -> bwd $ f $ Unknown x
  for (Yield x) f = f x
