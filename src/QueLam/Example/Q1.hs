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

q1 ::
  ( Symantics repr
  , HasTable schema "orders" order
  , Has "oid" (Sort order) Int) -- need to say Sort...
  => repr schema (Int -> [Record order])
q1 = lam $ \xoid ->
  for (table $ Proxy @ "orders") $ \o ->
    where' (o .% #oid =% xoid) $
      yield o
