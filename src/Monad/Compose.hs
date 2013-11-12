{-# LANGUAGE NoImplicitPrelude #-}
module Monad.Compose where

import Core
import Monad.Functor
import Monad.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

class Functor f => Applicable f where
  pure ::
    a
    -> f a
  ap ::
    f (a -> b)
    -> f a
    -> f b

newtype Compose f g a =
  Compose (f (g a))

-- Exercise 1
-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  fmap f (Compose k) =
      Compose (fmap (fmap f) k)

instance (Applicable f, Applicable g) =>
    Applicable (Compose f g) where
-- Exercise 2
-- Implement the pure function for an Applicable instance for Compose
  pure =
    Compose . pure . pure
-- Exercise 3
-- Implement the ap function for an Applicable instance for Compose
  Compose f `ap` Compose a =
    Compose (liftA2 (<*>) f a)

instance (Monad f, Monad g) =>
    Monad (Compose f g) where
-- Exercise 4
-- Implement the return function for a Monad instance for Compose
  return =
    Compose . return . return
-- Exercise 5
-- Implement the bind function for a Monad instance for Compose
  bind =
    undefined -- impossible
