{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module QueLam.Example.OrderDB where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.R

type ProductTable = '[ "name" := String, "pid" := Int, "price" := Int] -- has to be sorted
type OrderTable = '[ "oid" := Int, "pid" := Int, "qty" := Int]
type OrderDBSchema = '[ '("orders", OrderTable), '("products", ProductTable)]

orderDB :: Handle R OrderDBSchema
orderDB = #orders :=
  [ #oid := 1 & #pid := 1 & #qty := 5 & rnil
  , #oid := 1 & #pid := 2 & #qty := 5 & rnil
  , #oid := 1 & #pid := 4 & #qty := 2 & rnil
  , #oid := 1 & #pid := 5 & #qty := 10 & rnil
  --, #oid := 2 & #pid := 6 & #qty := 20 & rnil
  , #oid := 3 & #pid := 2 & #qty := 50 & rnil
  ] &
  #products :=
  [ #pid := 1 & #name := "Tablet" & #price := 500 & rnil
  , #pid := 2 & #name := "Laptop" & #price := 1000 & rnil
  , #pid := 3 & #name := "Desktop" & #price := 1000 & rnil
  , #pid := 4 & #name := "Router" & #price := 150 & rnil
  , #pid := 5 & #name := "HDD" & #price := 100 & rnil
  , #pid := 6 & #name := "SSD" & #price := 500 & rnil
  ] &
  rnil
