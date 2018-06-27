{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module QueLam.Example.Q2 where

import           Data.Proxy
import           QueLam.Core
import           SuperRecord

q2 ::
  forall repr schema prod order.
  ( Symantics repr
  , HasT "products" schema prod
  , Has "oid" order Int
  , Has "qty" order Int
  , Has "pid" (Sort prod) Int
  , Has "name" (Sort prod) String
  , Has "price" (Sort prod) Int)
  => Handle repr schema -> repr schema (Rec order -> [Record '["pid" := Int, "name" := String, "sale" := Int]])
q2 h = lam $ \o ->
  for (table h #products) $ \p ->
    where' (p .% #pid =% o .% #oid) $
      yield (#pid := p .% #pid &% #name := p .% #name &% #sale := p .% #price *% o .% #qty &% rnil')
