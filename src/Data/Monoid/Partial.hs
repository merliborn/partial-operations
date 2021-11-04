{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module    : Data.Monoid.Partial
  Copyright : (c) merliborn 2021
  License   : MIT
-}

module Data.Monoid.Partial(
  PartialMonoid(munit, (<*?>)),
) where

import Control.Partial
  ( 
    PartFunc(..),
    PartFuncFail(..)
    )
import Data.Semigroup.Partial (PartialSemigroup( (<?>) ))

import Data.Monoid(Monoid(..))                -- base >= 4.11.0.0
import Control.Exception.Safe(MonadThrow(..)) -- safe-exceptions

{-|
  Instances should satisfy the following property:

  [Right Unit Law] @x \<?\> munit == return x@

  [Left Unit Law] @munit \<?\> y == return y@
-}
class (PartialSemigroup a) => PartialMonoid a where
  -- |
  -- The unit element of @a@.
  munit :: a

  -- |
  -- For the same reason as for the 'Monoid' class, @\<*?\>@ should be a synonym for @\<?\>@.
  (<*?>) :: (MonadThrow m) => a -> a -> m a
  (<*?>) = (<?>)

instance (PartialMonoid a, PartFuncFail f) => Monoid (f a) where
  mempty = pass munit
