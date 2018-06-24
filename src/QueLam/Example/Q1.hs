{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module QueLam.Example.Q1 where

import           QueLam.Core

type Order = Int -- just for testing, should be Record

table_orders :: forall repr db. (Symantics repr, HasTable db "orders" Order) => repr [Order]
table_orders = table @repr @db @"orders"

q1 :: forall repr db. (Symantics repr, HasTable db "orders" Order) => repr (Int -> [Order])
q1 = lam $ \xoid ->
  for (table @repr @db @"orders") $ \o ->
    where' (o =% xoid) $
      yield o
