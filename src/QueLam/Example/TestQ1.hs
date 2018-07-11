{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
module QueLam.Example.TestQ1 where

import           Data.Row.Records

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q1
import           QueLam.P
import           QueLam.R

testQ1R :: R OrderDBSchema (Int -> [Rec OrderTable])
testQ1R = q1 orderDB

testQ1P :: P OrderDBSchema (Int -> [Rec OrderTable])
testQ1P = q1 ()

testO :: R OrderDBSchema ([Rec OrderTable])
testO = table orderDB #orders

testP :: R OrderDBSchema ([Rec ProductTable])
testP = table orderDB #products
