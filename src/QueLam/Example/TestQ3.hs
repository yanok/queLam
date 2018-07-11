{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module QueLam.Example.TestQ3 where

import           Data.Row.Records

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q3
import           QueLam.Optimize.AbsBeta
import           QueLam.Optimize.ForFor
import           QueLam.Optimize.ForUnionAll2
import           QueLam.Optimize.ForWhere
import           QueLam.Optimize.ForYield
import           QueLam.Optimize.LNil
import           QueLam.Optimize.RecordBeta
import           QueLam.Optimize.WhereFor
import           QueLam.Optimize.WhereUnion
import           QueLam.Optimize.WhereWhere
import           QueLam.P
import           QueLam.R

testQ3 :: Symantics repr
       => Handle repr OrderDBSchema
       -> repr OrderDBSchema
               (Int -> [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ3 = q3

testQ3' :: Symantics repr
        => Handle repr OrderDBSchema
        -> repr OrderDBSchema
                (Int -> [Int])
testQ3' = q3'

testQ3'' :: Symantics repr
         => Handle repr OrderDBSchema
         -> repr OrderDBSchema
                (Int -> [Int])
testQ3'' = q3''

testQ3R :: R OrderDBSchema
             (Int -> [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ3R = q3 orderDB

testQ3RLNil :: LNil R OrderDBSchema
                    (Int ->
                     [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ3RLNil = q3 orderDB

testQ3P :: P OrderDBSchema
             (Int -> [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ3P = q3 ()

testQ3PLNil :: LNil P OrderDBSchema
                    (Int ->
                     [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
testQ3PLNil = q3 ()
