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
  , HasT "orders" schema order
  , Has "oid" (Sort order) Int) -- need to say Sort...
  => Handle repr schema -> repr schema (Int -> [Record order])
q1 h = lam $ \xoid ->
  for (table h #orders) $ \o ->
    where' (o .% #oid =% xoid) $
      yield o
