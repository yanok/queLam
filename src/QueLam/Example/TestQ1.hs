{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeOperators        #-}
module QueLam.Example.TestQ1 where

import           GHC.TypeLits
import           SuperRecord

import           QueLam.Core
import           QueLam.Example.OrderDB
import           QueLam.Example.Q1
import           QueLam.R

test :: R '[ '("orders", '[ "oid" := Int])] (Int -> [Record '["oid" := Int]])
test = q1 orderDB
