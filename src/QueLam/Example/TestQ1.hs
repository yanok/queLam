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
import           QueLam.P

testQ1R :: R OrderDBSchema (Int -> [Record OrderTable])
testQ1R = q1 orderDB

testQ1P :: P OrderDBSchema (Int -> [Record OrderTable])
testQ1P = q1 ()

testO :: R OrderDBSchema ([Record OrderTable])
testO = table orderDB #orders

testP :: R OrderDBSchema ([Record ProductTable])
testP = table orderDB #products
