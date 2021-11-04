{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module    : Data.Semigroup.Partial
  Copyright : (c) merliborn 2021
  License   : MIT
-}

module Data.Semigroup.Partial(
  -- ** Class
  PartialSemigroup ( (<?>), tryconcat, timestry ),

  -- ** Exception
  PartialSemigroupException(
    NonPositiveMultiplier
  ),
  nonpositiveMultiplier,
) where

import Control.Partial
  ( 
    PartFunc(..),
    PartFuncFail(..)
    )

import Data.Bool(Bool(..), otherwise)
import Data.Eq(Eq(..))
import Data.Ord(Ord((<=)))
import GHC.Real
  ( Integral(..),
    even
  )
import Text.Show(Show(..), showString)        -- base >= 4.7.0.0

import Data.Semigroup(Semigroup(..))          -- base >= 4.9.0.0
import Data.List.NonEmpty(NonEmpty(..))       -- base >= 4.9.0.0
import Control.Monad(Monad(..))
import Control.Exception.Safe                 -- safe-exceptions
  ( Exception(..),
    SomeException(..),
    throw,

    MonadThrow(..)
  )

{-|
  Instances should satisfy the following property:

  [Partial Associativity] @(x \<?\> y) >>= (λt.t \<?\> z) == (y \<?\> z) >>= (x \<?\>)@
-}
class PartialSemigroup a where
  -- |
  -- An associative (but failable) binary operation.
  (<?>) :: (MonadThrow m) => a -> a -> m a

  -- |
  -- 'sconcat'-like operation for 'PartialSemigroup' data.
  tryconcat :: (MonadThrow m) => NonEmpty a -> m a
  tryconcat (x :| xs) = conc x xs
    where
      conc x (y:ys) = conc y ys >>= (x <?>)
      conc x []     = return x

  -- |
  -- 'stimes'-like operation for 'PartialSemigroup' data.
  timestry :: (MonadThrow m,Integral b) => b -> a -> m a
  x `timestry` g
    | x <= 0    = throw NonPositiveMultiplier
    | otherwise = repl x g
    where
      repl x g
        | even x    = (g <?> g) >>= repl (x `quot` 2)
        | x == 1    = return g
        | otherwise = (g <?> g) >>= repl' (x `quot` 2) g
      repl' x r g
        | even x    = (g <?> g) >>= repl' (x `quot` 2) r
        | x == 1    = g <?> r
        | otherwise = (g <?> r) >>= (\s -> (g <?> g) >>= repl' (x `quot` 2) s)

-- |
-- It needs to be instance of 'PartFuncFail' because @f@ should be an instance of 'MonadThrow'.
instance (PartialSemigroup a, PartFuncFail f) => Semigroup (f a) where
  x <> y
    | isFail x  = x
    | isFail y  = y
    | otherwise = liftBinop (<?>) x y

-- Exception
data PartialSemigroupException
  = NonPositiveMultiplier -- ^ @timestry@: positive multiplier expected
  deriving (Eq)

instance Exception PartialSemigroupException
instance Show PartialSemigroupException where
  showsPrec _ NonPositiveMultiplier = showString "timestry: positive multiplier expected"

nonpositiveMultiplier :: SomeException
nonpositiveMultiplier = toException NonPositiveMultiplier

