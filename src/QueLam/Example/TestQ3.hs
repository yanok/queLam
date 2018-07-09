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
import           QueLam.Optimize.AbsBeta
import           QueLam.Optimize.ForFor
import           QueLam.Optimize.ForWhere
import           QueLam.Optimize.ForYield
import           QueLam.Optimize.WhereFor
import           QueLam.Optimize.WhereWhere
import           QueLam.Optimize.LNil

testQ3 :: Symantics repr
       => Handle repr OrderDBSchema
       -> repr OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3 = q3

testQ3R :: R OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3R = q3 orderDB

testQ3RLNil :: LNil R OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3RLNil = q3 orderDB

testQ3P :: P OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3P = q3 ()

testQ3PLNil :: LNil P OrderDBSchema (Int -> [Record '[ "pid" := Int, "name" := String, "sale" := Int]])
testQ3PLNil = q3 ()
