{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
module QueLam.Example.TestQ2 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q2
import           QueLam.R
import           QueLam.P

testQ2R :: R OrderDBSchema (Record OrderTable -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ2R = q2 orderDB

testQ2P :: P OrderDBSchema (Record OrderTable -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ2P = q2 ()
