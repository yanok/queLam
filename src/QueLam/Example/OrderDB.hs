{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module QueLam.Example.OrderDB where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.R

--type OrderDBSchema = '[ '("orders", '[ "oid" := Int])]

orderDB :: Handle R '[ '("orders", '[ "oid" := Int])]
orderDB = #orders :=
  [ #oid := 0 & rnil
  , #oid := 1 & rnil
  ]
  & rnil
