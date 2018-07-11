{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
module QueLam.Optimize.RecordBeta where

import           Data.Constraint
import           Data.Row.Extra
import           Data.Row.Records

import           QueLam.Core

data RecordBeta repr (schema :: Row (Row *)) a where
  Rcrd :: forall repr schema row. WellBehaved row
       => Rec (Map (RecordBeta repr schema) row)
       -> RecordBeta repr schema (Rec row)
  Unknown :: repr schema a -> RecordBeta repr schema a

recordBeta :: forall repr schema a. Symantics repr
           => RecordBeta repr schema a -> repr schema a
recordBeta (Unknown x)        = x
recordBeta (Rcrd (r :: Rec (Map (RecordBeta repr schema) row)))
  = rcrd @repr @row $
    transform' @row @(RecordBeta repr schema) @(repr schema) recordBeta r

instance Symantics repr => RR RecordBeta repr where
  fwd = Unknown
  bwd = recordBeta

instance Symantics repr => Symantics (RecordBeta repr) where
  (.%)   :: forall schema row l a. (KnownSymbol l, row .! l ≈ a)
         => RecordBeta repr schema (Rec row) -> Label l -> RecordBeta repr schema a
  Unknown x .% l = fwd $ x .% l
  Rcrd r .% l = r .! l \\ mapHas @(RecordBeta repr schema) @row @l @a
  rcrd = Rcrd
