{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TicTacToe.WinOccupiedOr(
  WinOccupiedOr(Win, IsOccupiedOr)
, AsWinOccupiedOr(_WinOccupiedOr)
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category((.), id)
import Control.Lens(Optic, Choice, prism)
import Control.Monad(Monad((>>=), return))
import Data.Either(Either(Left, Right))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Functor(Functor(fmap))
import TicTacToe.AsOccupied(AsOccupied(_Occupied))
import TicTacToe.AsOr(AsOr(_Or))
import TicTacToe.OccupiedOr(OccupiedOr(Occupied, Or), AsOccupiedOr(_OccupiedOr))
import TicTacToe.AsWin(AsWin(_Win))
import Prelude(Show)

data WinOccupiedOr w a =
  Win w
  | IsOccupiedOr (OccupiedOr a)
  deriving (Eq, Ord, Show)

class AsWinOccupiedOr p f o where
  _WinOccupiedOr ::
    Optic p f (o w a) (o x b) (WinOccupiedOr w a) (WinOccupiedOr x b)

instance AsWinOccupiedOr p f WinOccupiedOr where
  _WinOccupiedOr =
    id

instance Functor (WinOccupiedOr w) where
  fmap _ (Win w) =
    Win w
  fmap f (IsOccupiedOr m) =
    IsOccupiedOr (fmap f m)

instance Applicative (WinOccupiedOr w) where
  pure =
    IsOccupiedOr . pure
  Win w <*> _ =
    Win w
  IsOccupiedOr _ <*> Win w =
    Win w
  IsOccupiedOr f <*> IsOccupiedOr a =
    IsOccupiedOr (f <*> a)

instance Monad (WinOccupiedOr w) where
  return =
    IsOccupiedOr . return
  Win w >>= _ =
    Win w
  IsOccupiedOr m >>= f =
    case m of
      Occupied -> IsOccupiedOr Occupied
      Or a -> f a

instance (Choice p, Applicative f) => AsWin p f WinOccupiedOr where
  _Win =
    prism
      Win
      (\b -> case b of
               Win w -> Right w
               IsOccupiedOr m -> Left (IsOccupiedOr m))

instance (Choice p, Applicative f) => AsOccupiedOr p f (WinOccupiedOr w) where
  _OccupiedOr =
    prism
      IsOccupiedOr
      (\b -> case b of
               Win w -> Left (Win w)
               IsOccupiedOr m -> Right m)

instance (Choice p, Applicative f) => AsOccupied p f (WinOccupiedOr w a) where
  _Occupied =
    _OccupiedOr . _Occupied
    
instance (Choice p, Applicative f) => AsOr p f (WinOccupiedOr w) where
  _Or =
    _OccupiedOr . _Or
