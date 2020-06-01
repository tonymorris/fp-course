{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module TicTacToe.Player(
  Player(X, O)
, AsPlayer(_Player)
, playerswap
) where

import Control.Applicative(Applicative)
import Control.Category(id)
import Control.Lens(Choice, Optic', Profunctor, Iso', iso, prism')
import Data.Bool(Bool(False, True), bool)
import Data.Eq(Eq)
import Data.Functor(Functor)
import Data.Maybe
import Data.Int(Int)
import Data.Ord(Ord)
import Prelude(Show)

data Player =
  X
  | O
  deriving (Eq, Ord, Show)

class AsPlayer p f s where
  _Player ::
    Optic' p f s Player

instance AsPlayer p f Player where
  _Player =
    id

instance (Profunctor p, Functor f) => AsPlayer p f Bool where
  _Player =
    iso
      (bool O X)
      (\p -> case p of
             X -> True
             O -> False)      

instance (Choice p, Applicative f) => AsPlayer p f Int where
  _Player =
    prism'
      (\p -> case p of
             X -> 1
             O -> 2) 
      (\n -> case n of
             1 -> Just X
             2 -> Just O
             _ -> Nothing)

playerswap ::
  Iso'
    Player
    Player
playerswap =
  let swap X = O
      swap O = X
  in iso
       swap
       swap

