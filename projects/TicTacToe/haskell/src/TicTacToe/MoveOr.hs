{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.MoveOr (
  Move2Or(Move2Or)
, AsMove2Or(_Move2Or)
, Move3Or(Move3Or)
, AsMove3Or(_Move3Or)
, Move4Or(Move4Or)
, AsMove4Or(_Move4Or)
, Move5Or(Move5Or)
, AsMove5Or(_Move5Or)
, Move6Or(Move6Or)
, AsMove6Or(_Move6Or)
, Move7Or(Move7Or)
, AsMove7Or(_Move7Or)
, Move8Or(Move8Or)
, AsMove8Or(_Move8Or)
, Move9Or(Move9Or)
, AsMove9Or(_Move9Or)
, MoveOr6Or(MoveOr6OrWin5, MoveOr6Or)
, MoveOr7Or(MoveOr7OrWin5, MoveOr7OrWin6, MoveOr7Or)
, MoveOr8Or(MoveOr8OrWin5, MoveOr8OrWin6, MoveOr8OrWin7, MoveOr8Or)
, MoveOr9Or(MoveOr9OrWin5, MoveOr9OrWin6, MoveOr9OrWin7, MoveOr9OrWin8, MoveOr9Or)
) where

import Control.Applicative(Applicative)
import Control.Category((.), id)
import Control.Lens(Profunctor, Choice, Prism', Optic', prism', failing, iso, from)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Maybe(Maybe(Nothing, Just))
import Data.Ord(Ord)
import Prelude(Show)
import TicTacToe.Move(Move2, Move3, Move4, Move5, Move6, Move7, Move8, Move9, Win5, Win6, Win7, Win8, Win9, AsMove2(_Move2), AsMove3(_Move3), AsMove4(_Move4), AsMove5(_Move5), AsMove6(_Move6), AsMove7(_Move7), AsMove8(_Move8), AsMove9(_Move9), AsWin5(_Win5), AsWin6(_Win6), AsWin7(_Win7), AsWin8(_Win8))
import TicTacToe.AsOccupied(AsOccupied(_Occupied))
import TicTacToe.OccupiedOr(OccupiedOr)
import TicTacToe.AsOr(AsOr(_Or))
import TicTacToe.WinOccupiedOr(WinOccupiedOr)
import TicTacToe.Position(AsPosition(_Position))

newtype Move2Or =
  Move2Or (OccupiedOr Move2)
  deriving (Eq, Ord, Show)

class AsMove2Or p f s where
  _Move2Or ::
    Optic' p f s Move2Or

instance AsMove2Or p f Move2Or where
  _Move2Or =
    id

instance (Profunctor p, Functor f) => AsMove2Or p f (OccupiedOr Move2) where
  _Move2Or =
    iso
      Move2Or
      (\(Move2Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move2Or where
  _Occupied =
    _Move2Or . _Occupied

instance (Choice p, Applicative f) => AsMove2 p f Move2Or where
  _Move2 =
    from _Move2Or . (_Or :: Prism' (OccupiedOr Move2) Move2)

instance (p ~ (->), Applicative f) => AsPosition p f Move2Or where
  _Position =
    _Move2 . _Position

newtype Move3Or =
  Move3Or (OccupiedOr Move3)
  deriving (Eq, Ord, Show)

class AsMove3Or p f s where
  _Move3Or ::
    Optic' p f s Move3Or

instance AsMove3Or p f Move3Or where
  _Move3Or =
    id

instance (Profunctor p, Functor f) => AsMove3Or p f (OccupiedOr Move3) where
  _Move3Or =
    iso
      Move3Or
      (\(Move3Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move3Or where
  _Occupied =
    _Move3Or . _Occupied

instance (Choice p, Applicative f) => AsMove3 p f Move3Or where
  _Move3 =
    from _Move3Or . (_Or :: Prism' (OccupiedOr Move3) Move3)

instance (p ~ (->), Applicative f) => AsPosition p f Move3Or where
  _Position =
    _Move3 . _Position

newtype Move4Or =
  Move4Or (OccupiedOr Move4)
  deriving (Eq, Ord, Show)

class AsMove4Or p f s where
  _Move4Or ::
    Optic' p f s Move4Or

instance AsMove4Or p f Move4Or where
  _Move4Or =
    id

instance (Profunctor p, Functor f) => AsMove4Or p f (OccupiedOr Move4) where
  _Move4Or =
    iso
      Move4Or
      (\(Move4Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move4Or where
  _Occupied =
    _Move4Or . _Occupied

instance (Choice p, Applicative f) => AsMove4 p f Move4Or where
  _Move4 =
    from _Move4Or . (_Or :: Prism' (OccupiedOr Move4) Move4)

instance (p ~ (->), Applicative f) => AsPosition p f Move4Or where
  _Position =
    _Move4 . _Position

newtype Move5Or =
  Move5Or (WinOccupiedOr Win5 Move5)
  deriving (Eq, Ord, Show)

class AsMove5Or p f s where
  _Move5Or ::
    Optic' p f s Move5Or

instance AsMove5Or p f Move5Or where
  _Move5Or =
    id

instance (Profunctor p, Functor f) => AsMove5Or p f (WinOccupiedOr Win5 Move5) where
  _Move5Or =
    iso
      Move5Or
      (\(Move5Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move5Or where
  _Occupied =
    _Move5Or . _Occupied

instance (Choice p, Applicative f) => AsMove5 p f Move5Or where
  _Move5 =
    from _Move5Or . (_Or :: Prism' (WinOccupiedOr Win5 Move5) Move5)

instance (p ~ (->), Applicative f) => AsPosition p f Move5Or where
  _Position =
    _Move5 . _Position

newtype Move6Or =
  Move6Or (WinOccupiedOr Win6 Move6)
  deriving (Eq, Ord, Show)

class AsMove6Or p f s where
  _Move6Or ::
    Optic' p f s Move6Or

instance AsMove6Or p f Move6Or where
  _Move6Or =
    id

instance (Profunctor p, Functor f) => AsMove6Or p f (WinOccupiedOr Win6 Move6) where
  _Move6Or =
    iso
      Move6Or
      (\(Move6Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move6Or where
  _Occupied =
    _Move6Or . _Occupied

instance (Choice p, Applicative f) => AsMove6 p f Move6Or where
  _Move6 =
    from _Move6Or . (_Or :: Prism' (WinOccupiedOr Win6 Move6) Move6)

instance (p ~ (->), Applicative f) => AsPosition p f Move6Or where
  _Position =
    _Move6 . _Position

newtype Move7Or =
  Move7Or (WinOccupiedOr Win7 Move7)
  deriving (Eq, Ord, Show)

class AsMove7Or p f s where
  _Move7Or ::
    Optic' p f s Move7Or

instance AsMove7Or p f Move7Or where
  _Move7Or =
    id

instance (Profunctor p, Functor f) => AsMove7Or p f (WinOccupiedOr Win7 Move7) where
  _Move7Or =
    iso
      Move7Or
      (\(Move7Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move7Or where
  _Occupied =
    _Move7Or . _Occupied

instance (Choice p, Applicative f) => AsMove7 p f Move7Or where
  _Move7 =
    from _Move7Or . (_Or :: Prism' (WinOccupiedOr Win7 Move7) Move7)

instance (p ~ (->), Applicative f) => AsPosition p f Move7Or where
  _Position =
    _Move7 . _Position

newtype Move8Or =
  Move8Or (WinOccupiedOr Win8 Move8)
  deriving (Eq, Ord, Show)

class AsMove8Or p f s where
  _Move8Or ::
    Optic' p f s Move8Or

instance AsMove8Or p f Move8Or where
  _Move8Or =
    id

instance (Profunctor p, Functor f) => AsMove8Or p f (WinOccupiedOr Win8 Move8) where
  _Move8Or =
    iso
      Move8Or
      (\(Move8Or m) -> m)

instance (Choice p, Applicative f) => AsOccupied p f Move8Or where
  _Occupied =
    _Move8Or . _Occupied

instance (Choice p, Applicative f) => AsMove8 p f Move8Or where
  _Move8 =
    from _Move8Or . (_Or :: Prism' (WinOccupiedOr Win8 Move8) Move8)

instance (p ~ (->), Applicative f) => AsPosition p f Move8Or where
  _Position =
    _Move8 . _Position

newtype Move9Or =
  Move9Or (WinOccupiedOr Win9 Move9)
  deriving (Eq, Ord, Show)

class AsMove9Or p f s where
  _Move9Or ::
    Optic' p f s Move9Or

instance AsMove9Or p f Move9Or where
  _Move9Or =
    id

instance (Choice p, Applicative f) => AsOccupied p f Move9Or where
  _Occupied =
    _Move9Or . _Occupied

instance (Profunctor p, Functor f) => AsMove9Or p f (WinOccupiedOr Win9 Move9) where
  _Move9Or =
    iso
      Move9Or
      (\(Move9Or m) -> m)

instance (Choice p, Applicative f) => AsMove9 p f Move9Or where
  _Move9 =
    from _Move9Or . (_Or :: Prism' (WinOccupiedOr Win9 Move9) Move9)

instance (p ~ (->), Applicative f) => AsPosition p f Move9Or where
  _Position =
    _Move9 . _Position

data MoveOr6Or =
  MoveOr6OrWin5 Win5
  | MoveOr6Or Move6Or
  deriving (Eq, Ord, Show)

instance (Choice p, Applicative f) => AsWin5 p f MoveOr6Or where
  _Win5 =
    prism'
      MoveOr6OrWin5
      (\m -> case m of
               MoveOr6OrWin5 w -> Just w
               MoveOr6Or _ -> Nothing)

instance (Choice p, Applicative f) => AsMove6Or p f MoveOr6Or where
  _Move6Or =
    prism'
      MoveOr6Or
      (\m -> case m of
               MoveOr6OrWin5 _ -> Nothing
               MoveOr6Or r -> Just r)

instance (Choice p, Applicative f) => AsOccupied p f MoveOr6Or where
  _Occupied =
    _Move6Or . _Occupied

instance (p ~ (->), Applicative f) => AsPosition p f MoveOr6Or where
  _Position =
    failing (_Win5 . _Position) (_Move6Or . _Position)    

data MoveOr7Or =
  MoveOr7OrWin5 Win5
  | MoveOr7OrWin6 Win6
  | MoveOr7Or Move7Or
  deriving (Eq, Ord, Show)

instance (Choice p, Applicative f) => AsWin5 p f MoveOr7Or where
  _Win5 =
    prism'
      MoveOr7OrWin5
      (\m -> case m of
               MoveOr7OrWin5 w -> Just w
               MoveOr7OrWin6 _ -> Nothing
               MoveOr7Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin6 p f MoveOr7Or where
  _Win6 =
    prism'
      MoveOr7OrWin6
      (\m -> case m of
               MoveOr7OrWin5 _ -> Nothing
               MoveOr7OrWin6 w -> Just w
               MoveOr7Or _ -> Nothing)

instance (Choice p, Applicative f) => AsMove7Or p f MoveOr7Or where
  _Move7Or =
    prism'
      MoveOr7Or
      (\m -> case m of
               MoveOr7OrWin5 _ -> Nothing
               MoveOr7OrWin6 _ -> Nothing
               MoveOr7Or r -> Just r)

instance (Choice p, Applicative f) => AsOccupied p f MoveOr7Or where
  _Occupied =
    _Move7Or . _Occupied

instance (p ~ (->), Applicative f) => AsPosition p f MoveOr7Or where
  _Position =
    failing (_Win5 . _Position) (failing (_Win6 . _Position) (_Move7Or . _Position))

data MoveOr8Or =
  MoveOr8OrWin5 Win5
  | MoveOr8OrWin6 Win6
  | MoveOr8OrWin7 Win7
  | MoveOr8Or Move8Or
  deriving (Eq, Ord, Show)

instance (Choice p, Applicative f) => AsWin5 p f MoveOr8Or where
  _Win5 =
    prism'
      MoveOr8OrWin5
      (\m -> case m of
               MoveOr8OrWin5 w -> Just w
               MoveOr8OrWin6 _ -> Nothing
               MoveOr8OrWin7 _ -> Nothing
               MoveOr8Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin6 p f MoveOr8Or where
  _Win6 =
    prism'
      MoveOr8OrWin6
      (\m -> case m of
               MoveOr8OrWin5 _ -> Nothing
               MoveOr8OrWin6 w -> Just w
               MoveOr8OrWin7 _ -> Nothing
               MoveOr8Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin7 p f MoveOr8Or where
  _Win7 =
    prism'
      MoveOr8OrWin7
      (\m -> case m of
               MoveOr8OrWin5 _ -> Nothing
               MoveOr8OrWin6 _ -> Nothing
               MoveOr8OrWin7 w -> Just w
               MoveOr8Or _ -> Nothing)

instance (Choice p, Applicative f) => AsMove8Or p f MoveOr8Or where
  _Move8Or =
    prism'
      MoveOr8Or
      (\m -> case m of
               MoveOr8OrWin5 _ -> Nothing
               MoveOr8OrWin6 _ -> Nothing
               MoveOr8OrWin7 _ -> Nothing
               MoveOr8Or r -> Just r)

instance (Choice p, Applicative f) => AsOccupied p f MoveOr8Or where
  _Occupied =
    _Move8Or . _Occupied

instance (p ~ (->), Applicative f) => AsPosition p f MoveOr8Or where
  _Position =
    failing (_Win5 . _Position) (failing (_Win6 . _Position) (failing (_Win7 . _Position) (_Move8Or . _Position)))

data MoveOr9Or =
  MoveOr9OrWin5 Win5
  | MoveOr9OrWin6 Win6
  | MoveOr9OrWin7 Win7
  | MoveOr9OrWin8 Win8
  | MoveOr9Or Move9Or
  deriving (Eq, Ord, Show)

instance (Choice p, Applicative f) => AsWin5 p f MoveOr9Or where
  _Win5 =
    prism'
      MoveOr9OrWin5
      (\m -> case m of
               MoveOr9OrWin5 w -> Just w
               MoveOr9OrWin6 _ -> Nothing
               MoveOr9OrWin7 _ -> Nothing
               MoveOr9OrWin8 _ -> Nothing
               MoveOr9Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin6 p f MoveOr9Or where
  _Win6 =
    prism'
      MoveOr9OrWin6
      (\m -> case m of
               MoveOr9OrWin5 _ -> Nothing
               MoveOr9OrWin6 w -> Just w
               MoveOr9OrWin7 _ -> Nothing
               MoveOr9OrWin8 _ -> Nothing
               MoveOr9Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin7 p f MoveOr9Or where
  _Win7 =
    prism'
      MoveOr9OrWin7
      (\m -> case m of
               MoveOr9OrWin5 _ -> Nothing
               MoveOr9OrWin6 _ -> Nothing
               MoveOr9OrWin7 w -> Just w
               MoveOr9OrWin8 _ -> Nothing
               MoveOr9Or _ -> Nothing)

instance (Choice p, Applicative f) => AsWin8 p f MoveOr9Or where
  _Win8 =
    prism'
      MoveOr9OrWin8
      (\m -> case m of
               MoveOr9OrWin5 _ -> Nothing
               MoveOr9OrWin6 _ -> Nothing
               MoveOr9OrWin7 _ -> Nothing
               MoveOr9OrWin8 w -> Just w
               MoveOr9Or _ -> Nothing)

instance (Choice p, Applicative f) => AsMove9Or p f MoveOr9Or where
  _Move9Or =
    prism'
      MoveOr9Or
      (\m -> case m of
               MoveOr9OrWin5 _ -> Nothing
               MoveOr9OrWin6 _ -> Nothing
               MoveOr9OrWin7 _ -> Nothing
               MoveOr9OrWin8 _ -> Nothing
               MoveOr9Or r -> Just r)

instance (Choice p, Applicative f) => AsOccupied p f MoveOr9Or where
  _Occupied =
    _Move9Or . _Occupied

instance (p ~ (->), Applicative f) => AsPosition p f MoveOr9Or where
  _Position =
    failing (_Win5 . _Position) (failing (_Win6 . _Position) (failing (_Win7 . _Position) (failing (_Win8 . _Position) (_Move9Or . _Position))))
