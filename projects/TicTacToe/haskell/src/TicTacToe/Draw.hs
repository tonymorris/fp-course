{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.Draw(
  Draw(isDraw)
) where

import Control.Category
import Control.Lens.Extras(is)
import Data.Bool(Bool)
import Prelude()
import TicTacToe.AsOr(AsOr(_Or))
import TicTacToe.Move(Win9, Move9)
import TicTacToe.MoveOr(Move9Or(Move9Or))
import TicTacToe.WinOccupiedOr(WinOccupiedOr)

class Draw g where
  isDraw ::
    g
    -> Bool

instance Draw (WinOccupiedOr Win9 Move9) where
  isDraw =
    is _Or

instance Draw Move9Or where
  isDraw (Move9Or m) =
    isDraw m

instance Draw Bool where
  isDraw =
    id
