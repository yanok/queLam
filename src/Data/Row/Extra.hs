{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Data.Row.Extra where

import           Data.Constraint
import           Data.Row.Records
import           Unsafe.Coerce

mapHas :: forall f r l t. (r .! l ≈ t) :- (Map f r .! l ≈ f t)
mapHas = Sub $ unsafeCoerce $ Dict @(r .! l ≈ t)
