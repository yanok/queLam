{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
module QueLam.Example.TestQ3 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q3
import           QueLam.R
import           QueLam.P

testQ3R :: R OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3R = q3 orderDB

testQ3P :: P OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3P = q3 ()
