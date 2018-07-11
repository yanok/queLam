{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module QueLam.Example.TestQ2 where

import           Data.Row.Records

import           QueLam.Example.OrderDB
import           QueLam.Example.Q2
import           QueLam.P
import           QueLam.R

testQ2R :: R OrderDBSchema
             (Rec OrderTable ->
              [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ2R = q2 orderDB

testQ2P :: P OrderDBSchema
             (Rec OrderTable ->
              [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ2P = q2 ()
