{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE OverloadedLabels #-}
module QueLam.Example.TestQ1 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q1
import           QueLam.R

testQ1 :: R OrderDBSchema (Int -> [Record OrderTable])
testQ1 = q1 orderDB

testO :: R OrderDBSchema ([Record OrderTable])
testO = table orderDB #orders

testP :: R OrderDBSchema ([Record ProductTable])
testP = table orderDB #products
