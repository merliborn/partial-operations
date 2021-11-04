{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
  Module    : Control.Partial
  Copyright : (c) merliborn 2021
  License   : MIT

  A wrapper data monad 'Partially' and 'PartialWith'@ a@ for (algebraic) but failable data (such as \(\mathbb{Z}/N\mathbb{Z}\)).
-}

module Control.Partial(
  -- * Data types
  PartialWith (FailWith, PassWith),
  Partially (Fail, Pass),

  -- * Classes
  --
  -- | Class for partial data wrapper (i.e. 'PartialWith'@ e@ and 'Partially').
  PrePartial(
    eq, neq
  ),
  PartFunc(
    pass,
    castM, castBy, castApp, liftBinop,
    isFail, isPass
  ),

  PartFuncFail(
    fail,
    toEither,
    fromMaybe,
    fromEither
  )
) where

import Control.Exception.Safe                 -- safe-exception
  ( Exception(..),
    SomeException(..),
    throwString
  )

import Data.Functor (Functor(..))             -- base >= 4.7.0.0
import Control.Applicative (Applicative(..))  -- base >= 4.7.0.0
import Control.Monad (Monad(..))              -- base >= 4.7.0.0
import Control.Monad.Catch                    -- exceptions >= 0.4
  ( MonadThrow(..),
    MonadCatch(..),
  )

import Data.Function (const, (.), ($))
import Data.Eq (Eq(..))
import Data.Bool
import Data.Maybe (Maybe(..), isJust)
import Data.Either(Either(..), isRight)
import Data.List (unwords, (++), null)
import Text.Show (Show(..), showString)

import Data.List.NonEmpty (NonEmpty(..))

{-|
  Partial data types with explicit errors.

  The data of the type @PartialWith e a@ behaves as if it were [Either](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.15.0.0/Data-Either.html#t:Either)@ e a@ type for some exception @e@.
-}
data PartialWith e a where
  FailWith :: Exception e => e -> PartialWith e a
  PassWith :: a -> PartialWith e a

{-|
  Partial data types with implicit errors.
-}
data Partially a where
  Fail :: Exception e => e-> Partially a
  Pass :: a -> Partially a

{-|
  A class for assureing types @f a@ to be instance of 'Eq'.

  ==== Examples

  >>> (Just 1) `eq` (Just 1)
  True

  >>> Nothing `eq` (Just 0)
  False
-}
class PrePartial f where
  {-# MINIMAL eq | neq #-}
  eq  :: forall a. Eq a => f a-> f a-> Bool
  neq :: forall a. Eq a => f a-> f a-> Bool

  neq x y = not (eq  x y)
  eq  x y = not (neq x y)

instance (PrePartial f, Eq a) => Eq (f a) where
  (==) = eq

instance PrePartial Maybe where
  Nothing  `eq` Nothing  = True
  Nothing  `eq` (Just _) = False
  (Just _) `eq` Nothing  = False
  (Just x) `eq` (Just y) = x == y
instance Eq a => PrePartial (Either a) where
  Left  _ `eq` Right _ = False
  Right _ `eq` Left  _ = False
  Left  a `eq` Left  b = a == b
  Right x `eq` Right y = x == y
instance PrePartial [] where
  (x:xs) `eq` (y:ys) = (x==y) && (xs `eq` ys)
  (x:xs) `eq` []     = False
  []     `eq` (y:ys) = False
  []     `eq` []     = True
instance PrePartial NonEmpty where
  (x :| xs) `eq` (y :| ys) = (x==y) && (xs `eq` ys)

{-|
  A class for assuring types @f@ to be an instance of 'Monad'.

  The default definitions of methods 'castBy', 'liftBinop' and 'castApp' are the following:

  > castBy f = castM (pass . f)
  > liftBinop op x = castM (\z-> castM (`op` z) x)
  > castApp f = liftBinop (\x-> pass . f x)

  It must behave the same if you define them.

  Any definition should satisfy the following conditions:

  [Left Monad Identity]     @castM f (pass a) = f a@
  [Right Monad Identity]    @castM pass x = x@
  [Monad Arrow Composition] @castM g (castM f a) = castM (\\x-> castM g (f x)) a@
-}
class PrePartial f => PartFunc f where
  {-# MINIMAL pass, castM, (isPass | isFail) #-}
  -- |
  -- It should work as 'pure' and 'return'.
  pass :: forall a. a-> f a
  -- |
  -- It should satisfy:
  -- 
  -- > castM f x = x >>= f
  castM :: forall a b. (a-> f b)-> f a-> f b

  -- |
  -- It should work as 'fmap',
  castBy :: forall a b. (a-> b)-> f a-> f b
  castBy f = castM (pass . f)

  liftBinop :: forall a b c. (a-> b-> f c)-> f a-> f b-> f c
  liftBinop op x = castM (\z-> castM (`op` z) x)

  -- |
  -- It should work as 'liftA2'.
  castApp :: forall a b c. (a-> b-> c)-> f a-> f b-> f c
  castApp f = liftBinop (\x-> pass . f x)

  -- |
  -- Check if the argument is passed or failed. Of course these methods satisfy:
  -- 
  -- @
  --    isFail = not . isPass
  --    isPass = not . isFail
  -- @
  isPass :: forall a. f a-> Bool
  isFail :: forall a. f a-> Bool
  isFail = not . isPass
  isPass = not . isFail

instance PartFunc Maybe where
  pass x = Just x
  castM f Nothing  = Nothing
  castM f (Just x) = f x
  isPass = isJust
instance Eq a => PartFunc (Either a) where
  pass x = Right x
  castM f (Left  a) = Left a
  castM f (Right x) = f x
  isPass = isRight
instance PartFunc [] where
  pass x = [x]
  castM f xs = [y | x <- xs, y<- f x]
  isFail = null
instance PartFunc NonEmpty where
  pass x = x :| []
  castM f (x :| xs) = a :| (as++ss)
    where
      a :| as = f x
      ss = castM (toList . f) xs
      toList (y :| ys) = y : ys
  isPass _ = True

{-|
  Class to implement the \'failure\' function.

  @PartFuncFail f@ should be a 'MonadCatch'.
-}
class PartFunc f => PartFuncFail f where
  fail :: forall a e. Exception e => e-> f a
  toEither :: forall a. f a-> Either SomeException a

  fromMaybe :: forall a e. Exception e => Maybe a-> e-> f a
  fromMaybe (Just a) _ = pass a
  fromMaybe Nothing  e = fail e
  fromEither :: forall a b e. Exception e => Either a b-> (a-> e)-> f b
  fromEither (Right a) _ = pass a
  fromEither (Left  b) f = fail $ f b

instance PartFunc f => Functor f where
  fmap = castBy
instance PartFunc f => Applicative f where
  pure = pass
  liftA2 = castApp
instance PartFunc f => Monad f where
  x >>= f = castM f x
instance PartFuncFail f => MonadThrow f where
  throwM = fail
instance PartFuncFail f => MonadCatch f where
  catch x f =
    case toEither x of
      Right x -> pass x
      Left  e -> fail e

instance Show a => Show (PartialWith e a) where
  showsPrec n (FailWith e) = showsPrec n e . showString "FailWith "
  showsPrec _ (PassWith x) = showString ("PassWith (" ++ show x ++ ")")

instance Show a => Show (Partially a) where
  showsPrec n (Fail e) = showsPrec n e . showString "Fail "
  showsPrec _ (Pass x) = showString ("Pass (" ++ show x ++ ")")

-- -------------------------------------------------------------------------
-- Implementations
-- -------------------------------------------------------------------------

-- |
-- Note that @(FailWith e) == (FailWith e') = True@ even @e /= e'@.
instance Exception e => PrePartial (PartialWith e) where
  (FailWith _) `eq` (FailWith _) = True
  (FailWith _) `eq` (PassWith _) = False
  (PassWith _) `eq` (FailWith _) = False
  (PassWith x) `eq` (PassWith y) = x == y
instance Exception e => PartFunc (PartialWith e) where
  pass = PassWith
  castApp f (FailWith e) _            = FailWith e
  castApp f (PassWith x) (FailWith e) = FailWith e
  castApp f (PassWith x) (PassWith y) = PassWith (f x y)
  castM f (FailWith e) = FailWith e
  castM f (PassWith x) = f x
  isFail (FailWith e) = True
  isFail (PassWith x) = False
instance e ~ SomeException => PartFuncFail (PartialWith e) where
  fail = FailWith . toException
  toEither (PassWith x) = Right x
  toEither (FailWith e) = Left (toException e)

instance PrePartial Partially where
  (Fail _) `eq` (Fail _) = True
  (Fail _) `eq` (Pass _) = False
  (Pass _) `eq` (Fail _) = False
  (Pass x) `eq` (Pass y) = True
instance PartFunc Partially where
  pass = Pass
  castApp f (Fail e) _        = Fail e
  castApp f (Pass x) (Fail e) = Fail e
  castApp f (Pass x) (Pass y) = Pass (f x y)
  castM f (Fail e) = Fail e
  castM f (Pass x) = f x
  isFail (Fail e) = True
  isFail (Pass x) = False
instance PartFuncFail Partially where
  fail = Fail
  toEither (Pass x) = Right x
  toEither (Fail e) = Left (toException e)
