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

import           Data.Row.Records

import           QueLam.Core

q2 ::
  forall repr schema prod order.
  ( Symantics repr
  , schema .! "products" ≈ prod
  , order .! "pid" ≈ Int
  , order .! "qty" ≈ Int
  , prod .! "pid" ≈ Int
  , prod .! "name" ≈ String
  , prod .! "price" ≈ Int)
  => Handle repr schema
  -> repr schema
          (Rec order -> [Rec ("pid" .== Int .+ "name" .== String .+ "sale" .== Int)])
q2 h = lam $ \o ->
  for (table h #products) $ \p ->
    where' (p .% #pid =% o .% #pid) $
      yield $ rcrd (#pid .== p .% #pid .+
                    #name .== p .% #name .+
                    #sale .== (p .% #price *% o .% #qty))
