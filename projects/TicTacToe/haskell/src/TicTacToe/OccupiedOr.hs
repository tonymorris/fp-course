{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TicTacToe.OccupiedOr(
  OccupiedOr(Occupied, Or)
, AsOccupiedOr(_OccupiedOr)
) where

import Control.Applicative(Applicative((<*>), pure))
import Control.Category(id)
import Control.Lens(Optic, Profunctor, Choice, prism', prism, iso)
import Control.Monad(Monad((>>=), return))
import Data.Either(Either(Left, Right))
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Maybe(Maybe(Nothing, Just), maybe)
import Data.Ord(Ord)
import Prelude(Show)
import TicTacToe.AsOccupied(AsOccupied(_Occupied))
import TicTacToe.AsOr(AsOr(_Or))

data OccupiedOr a =
  Occupied
  | Or a
  deriving (Eq, Ord, Show)

class AsOccupiedOr p f o where
  _OccupiedOr ::
    Optic p f (o a) (o b) (OccupiedOr a) (OccupiedOr b)

instance AsOccupiedOr p f OccupiedOr where
  _OccupiedOr =
    id

instance (Profunctor p, Functor f) => AsOccupiedOr p f Maybe where
  _OccupiedOr =
    iso
      (maybe Occupied Or)
      (\o -> case o of
               Occupied -> Nothing
               Or a -> Just a)

instance Functor OccupiedOr where
  fmap _ Occupied =
    Occupied
  fmap f (Or a) =
    Or (f a)

instance Applicative OccupiedOr where
  pure =
    Or
  Occupied <*> _ =
    Occupied
  Or _ <*> Occupied =
    Occupied
  Or f <*> Or a =
    Or (f a)  

instance Monad OccupiedOr where
  return =
    Or
  Occupied >>= _ =
    Occupied
  Or a >>= f =
    f a

instance (Choice p, Applicative f) => AsOccupied p f (OccupiedOr a) where
  _Occupied =
    prism'
      (\() -> Occupied)
      (\o -> case o of 
               Occupied -> Just ()
               Or _  -> Nothing)

instance (Choice p, Applicative f) => AsOr p f OccupiedOr where
  _Or =
    prism
      Or
      (\o -> case o of 
               Occupied -> Left Occupied
               Or a -> Right a)
