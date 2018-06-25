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
  , HasTable schema "products" (Rec prod)
  , Has "oid" order Int
  , Has "pid" prod Int)
  => repr schema (Rec order -> [Record '["pid" := Int]])
q2 = lam $ \o ->
  for (table $ Proxy @ "products") $ \p ->
    where' (p .% #pid =% o .% #oid) $
      yield ((#pid := (p .% #pid)) &% rnil')
