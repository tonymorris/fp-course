{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.AsWin(
  AsWin(_Win)
) where

import Control.Applicative(Applicative)
import Control.Lens(Optic, Choice, _1, _Left)
import Data.Either(Either)
import Data.Functor(Functor)

class AsWin p f o where
  _Win ::
    Optic p f (o w a) (o x a) w x

instance (Choice p, Applicative f) => AsWin p f Either where
  _Win =
    _Left

instance (p ~ (->), Functor f) => AsWin p f (,) where
  _Win =
    _1
