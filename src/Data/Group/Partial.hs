{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

{-|
  Module    : Data.Group.Partial
  Copyright : (c) merliborn 2021
  License   : MIT
-}

module Data.Group.Partial(
  PartialGroup (inv, (<~~>), (<^>))
) where

import Control.Partial
  ( 
    PartFunc(..),
    PartFuncFail(..)
    )
import Data.Monoid.Partial(PartialMonoid(munit, (<*?>)))

import Data.Bool(otherwise)
import Data.Eq(Eq(..))
import Data.Ord(Ord(compare), Ordering(..))
import GHC.Num(Num(negate))
import GHC.Real
  ( Integral(..),
    even
  )
import Data.Group(Group(..))                  -- groups >= 0.5
import Control.Monad(Monad(..))
import Control.Exception.Safe(MonadThrow(..)) -- safe-exceptions

{-|
  Instance should satisfy the following property:

  [Invertive law] @x \<?\> (inv x) == (inv x) \<?\> x == return munit@
-}
class (PartialMonoid a) => PartialGroup a where
  -- |
  -- Relate to 'invert' function.
  inv :: (MonadThrow m) => a -> m a

  -- |
  -- Partial '(~~)' operation.
  (<~~>) :: (MonadThrow m) => a -> a -> m a
  x <~~> y = inv y >>= (x <*?>)

  -- |
  -- Partial 'pow' function. It tries compute @a \<^\> n = a \<*\> a \<*\> ... \<*\> a@, @n@ times @a@'s.
  (<^>) :: (MonadThrow m, Integral i) => a -> i -> m a
  b <^> e = case compare e 0 of
    LT -> repl (negate e) b >>= inv
    EQ -> return munit
    GT -> repl e b
    where
      repl n g
        | even n    = (g <*?> g) >>= repl (n `quot` 2)
        | n == 1    = return g
        | otherwise = repl' n g g
      repl' n r g
        | even n    = (g <*?> g) >>= repl' (n `quot` 2) r
        | n == 1    = g <*?> r
        | otherwise = (g <*?> r) >>= (\s -> (g <*?> g) >>= repl' (n `quot` 2) s)

instance (PartialGroup a, PartFuncFail f) => Group (f a) where
  invert x
    | isFail x  = x
    | otherwise = x >>= inv