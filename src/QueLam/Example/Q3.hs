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

import           Data.Proxy
import           QueLam.Core
import           SuperRecord

import QueLam.Example.Q1
import QueLam.Example.Q2

q3 ::
  forall repr schema prod order.
  ( Symantics repr
  , HasT "orders" schema order
  , HasT "products" schema prod
  , Has "oid" (Sort order) Int
  , Has "pid" (Sort order) Int
  , Has "qty" (Sort order) Int
  , Has "pid" (Sort prod) Int
  , Has "name" (Sort prod) String
  , Has "price" (Sort prod) Int)
  => Handle repr schema -> repr schema (Int -> [Record '["pid" := Int, "name" := String, "sale" := Int]])
q3 h = compose $$ q1 h $$ q2 h
