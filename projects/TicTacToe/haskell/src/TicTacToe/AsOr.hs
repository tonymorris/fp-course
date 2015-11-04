{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.AsOr(
  AsOr(_Or)
) where

import Control.Applicative(Applicative)
import Control.Lens(Optic, Profunctor, Choice, iso, _Right, _Just)
import Data.Either(Either)
import Data.Functor(Functor)
import Data.Functor.Identity(Identity(runIdentity, Identity))
import Data.Maybe(Maybe)
import Data.Traversable(traverse)

class AsOr p f o where
  _Or ::
    Optic p f (o a) (o b) a b

instance (Profunctor p, Functor f) => AsOr p f Identity where
  _Or =
    iso
      runIdentity
      Identity

instance (Choice p, Applicative f) => AsOr p f Maybe where
  _Or =
    _Just

instance (Choice p, Applicative f) => AsOr p f (Either t) where
  _Or =
    _Right

instance (p ~ (->), Applicative f) => AsOr p f [] where
  _Or =
    traverse
