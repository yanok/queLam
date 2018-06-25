{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module QueLam.Example.Q1 where

import           Data.Proxy
import           QueLam.Core
import           SuperRecord

q1
    :: forall repr schema order.
       (Symantics repr, HasTable schema "orders" (Rec order), Has "oid" order Int)
    => repr schema (Int -> [Rec order])
q1 = lam $ \xoid ->
  for (table $ Proxy @ "orders") $ \o ->
    where' (o .% #oid =% xoid) $
      yield o
