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

import           Data.Row.Records

import           QueLam.Core

q1 ::
  ( Symantics repr
  , schema .! "orders" ≈ order
  , order .! "oid" ≈ Int)
  => Handle repr schema -> repr schema (Int -> [Rec order])
q1 h = lam $ \xoid ->
  for (table h #orders) $ \o ->
    where' (o .% #oid =% xoid) $
      yield o
