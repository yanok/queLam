module QueLam.Example.Q1 where

import           QueLam.Core

type Order = Int -- just for testing, should be Record

table_orders :: Symantics repr => repr [Order]
table_orders = undefined

q1 :: Symantics repr => repr (Int -> [Order])
q1 = lam $ \xoid ->
  for table_orders $ \o ->
    where' (o =% xoid) $
      yield o
