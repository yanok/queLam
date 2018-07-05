{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
module QueLam.Example.TestQ3 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q3
import           QueLam.R

testQ3 :: R OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3 = q3 orderDB
