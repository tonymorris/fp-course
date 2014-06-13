-- | A position is one of the nine places on a tic-tac-toe grid.
module Data.TicTacToe.Position
(
  Position(..)
, magic
) where

-- A tic-tac-toe position.
data Position =
  NW -- ^ North-west (top left).
  | N -- ^ North (top centre).
  | NE -- ^ North-east (top right).
  | W -- ^ West (middle left).
  | C -- ^ Centre.
  | E -- ^ East (middle right)
  | SW -- ^ South-west (bottom left).
  | S -- ^ South (bottom centre).
  | SE -- ^ South-east (bottom right).
  deriving (Eq, Ord, Bounded)

-- |
--
-- prop> length (show p) == 2
instance Show Position where
  show NW = "NW"
  show N  = "N "
  show NE = "NE"
  show E  = "E "
  show SE = "SE"
  show S  = "S "
  show SW = "SW"
  show W  = "W "
  show C  = "C "

-- todo Prism
magic ::
  Position
  -> Int
magic NW =
  8
magic N =
  1
magic NE =
  6
magic W =
  3
magic C =
  5
magic E =
  7
magic SW =
  4
magic S =
  9
magic SE =
  2
