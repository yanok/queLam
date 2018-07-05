{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
module QueLam.Example.TestQ2 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q2
import           QueLam.R

testQ2 :: R OrderDBSchema (Record OrderTable -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ2 = q2 orderDB
