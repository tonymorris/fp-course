{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module TicTacToe.AsOccupied(
  AsOccupied(_Occupied)
, occupied
) where

import Control.Applicative(Applicative)
import Control.Category(id)
import Control.Lens(Optic', Choice, (#), _Empty, _Nothing)
import Data.Functor.Identity(Identity)
import Data.Maybe(Maybe)
import Data.Tagged(Tagged)

class AsOccupied p f s where
  _Occupied ::
    Optic' p f s ()

instance AsOccupied p f () where
  _Occupied =
    id

instance (Choice p, Applicative f) => AsOccupied p f (Maybe a) where
  _Occupied =
    _Nothing

instance (Choice p, Applicative f) => AsOccupied p f [a] where
  _Occupied =
    _Empty

occupied ::
  AsOccupied Tagged Identity a => a
occupied =
  _Occupied # ()
