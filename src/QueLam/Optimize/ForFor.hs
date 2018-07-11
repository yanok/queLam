{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.ForFor where

import           Data.Row.Records

import           QueLam.Core


data ForFor repr (schema :: Row (Row *)) a where
  For :: ForFor repr schema [a]
      -> (ForFor repr schema a -> ForFor repr schema [b])
      -> ForFor repr schema [b]
  Unknown :: repr schema a -> ForFor repr schema a

forFor :: Symantics repr => ForFor repr schema a -> repr schema a
forFor (For xs f)  = for (forFor xs) $ \x -> forFor $ f $ Unknown x
forFor (Unknown x) = x


instance Symantics repr => RR ForFor repr where
  fwd = Unknown
  bwd = forFor

instance Symantics repr => Symantics (ForFor repr) where
  for xs@Unknown{} f = For xs f
  for (For ys g) f = fwd $ for (bwd ys) $ \y ->
                             for (bwd $ g $ fwd y) $ \x -> bwd $ f $ fwd x
