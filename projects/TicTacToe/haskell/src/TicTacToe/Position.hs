{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.Position(
  Position(P1, P2, P3, P4, P5, P6, P7, P8, P9)
, AsPosition(_Position)
, magic
, positionIndex
, positionPlayer
, whoseTurn
, IndexingN(IndexingN, runIndexingN)
) where

import Control.Applicative(Applicative((<*>), pure), Const)
import Control.Category(id, (.))
import Control.Lens(Iso', Optic', Choice, Indexable(indexed), (^.), ( # ), iso, prism', elemIndexOf, lengthOf, _2)
import Data.Char(Char)
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Int(Int)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Monoid(First, Endo)
import Data.Ord(Ord)
import Prelude(Show, Num((+)), seq, even)
import TicTacToe.Player(Player(X), _Player, playerswap)

data Position =
  P1
  | P2
  | P3
  | P4
  | P5
  | P6
  | P7
  | P8
  | P9
  deriving (Eq, Ord, Show)

magic ::
 Iso'
   Position
   Position
magic =
  iso
    (\p -> case p of
             P1 -> P2
             P2 -> P9
             P3 -> P4
             P4 -> P7
             P5 -> P5
             P6 -> P3
             P7 -> P6
             P8 -> P1
             P9 -> P8)
    (\p -> case p of
             P1 -> P8
             P2 -> P1
             P3 -> P6
             P4 -> P3
             P5 -> P5
             P6 -> P7
             P7 -> P4
             P8 -> P9
             P9 -> P2)

class AsPosition p f s where
  _Position ::
    Optic' p f s Position

instance AsPosition p f Position where
  _Position =
    id

instance (p ~ (->), Applicative f) => AsPosition p f () where
  _Position _ () =
    pure ()

instance (Choice p, Applicative f) => AsPosition p f Int where
  _Position =
    prism'
      (\p -> case p of
               P1 -> 1
               P2 -> 2
               P3 -> 3
               P4 -> 4
               P5 -> 5
               P6 -> 6
               P7 -> 7
               P8 -> 8
               P9 -> 9)
      (\p -> case p of
               1 -> Just P1
               2 -> Just P2
               3 -> Just P3
               4 -> Just P4
               5 -> Just P5
               6 -> Just P6
               7 -> Just P7
               8 -> Just P8
               9 -> Just P9
               _ -> Nothing)

instance (Choice p, Applicative f) => AsPosition p f Char where
  _Position =
    prism'
      (\p -> case p of
               P1 -> '1'
               P2 -> '2'
               P3 -> '3'
               P4 -> '4'
               P5 -> '5'
               P6 -> '6'
               P7 -> '7'
               P8 -> '8'
               P9 -> '9')
      (\p -> case p of
               '1' -> Just P1
               '2' -> Just P2
               '3' -> Just P3
               '4' -> Just P4
               '5' -> Just P5
               '6' -> Just P6
               '7' -> Just P7
               '8' -> Just P8
               '9' -> Just P9
               _ -> Nothing)

newtype IndexingN n f a =
  IndexingN {
    runIndexingN :: n -> (n, f a)
  }

instance Functor f => Functor (IndexingN n f) where
  fmap f (IndexingN m) =
    IndexingN (fmap (fmap f) . m)

instance Applicative f => Applicative (IndexingN n f) where
  pure x = 
    IndexingN (\i -> (i, pure x))
  IndexingN f <*> IndexingN a =
    IndexingN (\i -> let (o, g) = f i
                         ~(p, b) = a o                     
                     in (p, g <*> b))

indexingN ::
  Indexable i p =>
  a
  -> (i -> i)
  -> ((d -> IndexingN i g c) -> t -> IndexingN a f b)
  -> p d (g c)
  -> t
  -> f b
indexingN x k l iafb s =
  runIndexingN (l (\a -> IndexingN (\i -> i `seq` (k i, indexed iafb i a))) s) x ^. _2

positionIndex ::
  (Num i, AsPosition (->) (IndexingN i (Const (First i))) a) =>
  Position
  -> a
  -> Maybe i
positionIndex =
  elemIndexOf (indexingN 0 (+1) _Position)

positionPlayer ::
  AsPosition (->) (IndexingN Player (Const (First Player))) a =>
  Position
  -> a
  -> Maybe Player
positionPlayer =
  elemIndexOf (indexingN X (playerswap #) _Position)

whoseTurn ::
  AsPosition (->) (Const (Endo (Endo Int))) g =>
  g
  -> Player
whoseTurn x =
  even (lengthOf _Position x) ^. _Player
