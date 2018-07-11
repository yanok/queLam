{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module QueLam.Example.Q3 where

import           Data.Row.Records  hiding (compose)

import           QueLam.Core

import           QueLam.Example.Q1
import           QueLam.Example.Q2

q3 ::
  forall repr schema prod order.
  ( Symantics repr
  , schema .! "orders" ≈ order
  , schema .! "products" ≈ prod
  , order .! "oid" ≈ Int
  , order .! "pid" ≈ Int
  , order .! "qty" ≈ Int
  , prod .! "pid" ≈ Int
  , prod .! "name" ≈ String
  , prod .! "price" ≈ Int)
  => Handle repr schema
  -> repr schema (Int -> [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
q3 h = compose $$ q1 h $$ q2 h
