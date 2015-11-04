{-# LANGUAGE NoImplicitPrelude #-}

module TicTacToe.Winpaths(
  Winpaths(winpaths)
, hasWin
) where

import Control.Category((.))
import Control.Lens((#))
import Data.Bool(Bool)
import Data.Eq((==))
import Data.Foldable(any, sum)
import Data.Functor(fmap)
import Data.Int(Int)
import TicTacToe.Position(Position, _Position, magic)

class Winpaths w where
  winpaths ::
    w
    -> [(Position, Position)]

hasWin ::
  Winpaths w =>
  Position
  -> w
  -> Bool
hasWin p m =
  any (\(p2, p3) -> sum (fmap (_Position . magic #) [p, p2, p3]) == (15 :: Int)) (winpaths m)
