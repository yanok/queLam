{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module QueLam.Example.OrderDB where

import           Data.Functor.Compose
import           Data.Row.Records
import           GHC.TypeLits

import           QueLam.Core
import           QueLam.R

type ProductTable = "name" .== String .+ "pid" .== Int .+ "price" .== Int
type OrderTable = "oid" .== Int .+ "pid" .== Int .+ "qty" .== Int
type OrderDBSchema = "orders" .== OrderTable .+ "products" .== ProductTable

orderDB :: Handle R OrderDBSchema
orderDB = #orders .== Compose
  [ #oid .== 1 .+ #pid .== 1 .+ #qty .== 5
  , #oid .== 1 .+ #pid .== 2 .+ #qty .== 5
  , #oid .== 1 .+ #pid .== 4 .+ #qty .== 2
  , #oid .== 2 .+ #pid .== 5 .+ #qty .== 10
  , #oid .== 2 .+ #pid .== 6 .+ #qty .== 20
  , #oid .== 3 .+ #pid .== 2 .+ #qty .== 50
  ] .+
  #products .== Compose
  [ #pid .== 1 .+ #name .== "Tablet" .+ #price .== 500
  , #pid .== 2 .+ #name .== "Laptop" .+ #price .== 1000
  , #pid .== 3 .+ #name .== "Desktop" .+ #price .== 1000
  , #pid .== 4 .+ #name .== "Router" .+ #price .== 150
  , #pid .== 5 .+ #name .== "HDD" .+ #price .== 100
  , #pid .== 6 .+ #name .== "SSD" .+ #price .== 500
  ]
