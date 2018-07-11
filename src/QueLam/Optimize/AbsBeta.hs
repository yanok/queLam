{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module QueLam.Optimize.AbsBeta where

import           Data.Row.Records

import           QueLam.Core


data AbsBeta repr (schema :: Row (Row *)) a where
  Lam :: (AbsBeta repr schema a -> AbsBeta repr schema b)
      -> AbsBeta repr schema (a -> b)
  Unknown :: repr schema a -> AbsBeta repr schema a

absBeta :: Symantics repr => AbsBeta repr schema a -> repr schema a
absBeta (Lam f)     = lam $ \x -> absBeta $ f $ Unknown x
absBeta (Unknown x) = x


instance Symantics repr => RR AbsBeta repr where
  fwd = Unknown
  bwd = absBeta

instance Symantics repr => Symantics (AbsBeta repr) where
  lam f = Lam f
  (Unknown f) $$ x = fwd $ f $$ bwd x
  (Lam f) $$ x = f x
