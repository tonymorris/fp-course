{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TicTacToe.WithPosition(
  WithPosition((-->))
) where

import Control.Category((.))
import Control.Monad((>>=))
import TicTacToe.Position(Position)
import TicTacToe.MoveOr(Move2Or(Move2Or), Move3Or(Move3Or), Move4Or(Move4Or), Move5Or(Move5Or), Move6Or(Move6Or), Move7Or(Move7Or), Move8Or(Move8Or), Move9Or(Move9Or), MoveOr6Or(MoveOr6OrWin5, MoveOr6Or), MoveOr7Or(MoveOr7OrWin5, MoveOr7OrWin6, MoveOr7Or), MoveOr8Or(MoveOr8OrWin5, MoveOr8OrWin6, MoveOr8OrWin7, MoveOr8Or), MoveOr9Or(MoveOr9OrWin5, MoveOr9OrWin6, MoveOr9OrWin7, MoveOr9OrWin8, MoveOr9Or))
import Control.Lens(( # ))
import TicTacToe.Move(Move1, Move2, Move3, Move4, Move5, Move6, Move7, Move8, start, move2, move3, move4, move5, move6, move7, move8, move9)
import TicTacToe.OccupiedOr(OccupiedOr(Occupied, Or), AsOccupiedOr(_OccupiedOr))
import TicTacToe.AsOr(AsOr(_Or))
import TicTacToe.WinOccupiedOr(WinOccupiedOr(IsOccupiedOr, Win))

class WithPosition f g | f -> g where
  (-->) ::
    Position
    -> f
    -> g

infixr 6 -->

instance WithPosition () Move1 where
  p --> () =
    start p

instance WithPosition Move1 Move2Or where
  p --> m =
    Move2Or (move2 p m)

instance WithPosition Move2Or Move3Or where
  p --> Move2Or m =
    Move3Or (m >>= move3 p)

instance WithPosition Move2 Move3Or where
  (-->) p =
    (-->) p . Move2Or . (#) _Or

instance WithPosition Move3Or Move4Or where
  p --> Move3Or m =
    Move4Or (m >>= move4 p)

instance WithPosition Move3 Move4Or where
  (-->) p =
    (-->) p . Move3Or . (#) _Or

instance WithPosition Move4Or Move5Or where
  p --> Move4Or m =
    Move5Or ((_OccupiedOr # m) >>= move5 p)

instance WithPosition Move4 Move5Or where
  (-->) p =
    (-->) p . Move4Or . (#) _Or

instance WithPosition Move5Or MoveOr6Or where
  _ --> Move5Or (Win w) =
    MoveOr6OrWin5 w
  _ --> Move5Or (IsOccupiedOr Occupied) =
    MoveOr6Or (Move6Or (IsOccupiedOr Occupied))
  p --> Move5Or (IsOccupiedOr (Or m)) =
    MoveOr6Or (p --> m)
    
instance WithPosition Move5 Move6Or where
  p --> m =
    Move6Or (move6 p m)

instance WithPosition Move6 Move7Or where
  p --> m =
    Move7Or (move7 p m)

instance WithPosition Move6Or MoveOr7Or where
  _ --> Move6Or (Win w) =
    MoveOr7OrWin6 w
  _ --> Move6Or (IsOccupiedOr Occupied) =
    MoveOr7Or (Move7Or (IsOccupiedOr Occupied))
  p --> Move6Or (IsOccupiedOr (Or m)) =
    MoveOr7Or (p --> m)

instance WithPosition MoveOr6Or MoveOr7Or where
  _ --> MoveOr6OrWin5 w =
    MoveOr7OrWin5 w
  p --> MoveOr6Or m =
    p --> m

instance WithPosition Move7 Move8Or where
  p --> m =
    Move8Or (move8 p m)

instance WithPosition Move7Or MoveOr8Or where
  _ --> Move7Or (Win w) =
    MoveOr8OrWin7 w
  _ --> Move7Or (IsOccupiedOr Occupied) =
    MoveOr8Or (Move8Or (IsOccupiedOr Occupied))
  p --> Move7Or (IsOccupiedOr (Or m)) =
    MoveOr8Or (p --> m)

instance WithPosition MoveOr7Or MoveOr8Or where
  _ --> MoveOr7OrWin5 w =
    MoveOr8OrWin5 w
  _ --> MoveOr7OrWin6 w =
    MoveOr8OrWin6 w
  p --> MoveOr7Or m =
    p --> m

instance WithPosition Move8 Move9Or where
  p --> m =
    Move9Or (move9 p m)

instance WithPosition Move8Or MoveOr9Or where
  _ --> Move8Or (Win w) =
    MoveOr9OrWin8 w
  _ --> Move8Or (IsOccupiedOr Occupied) =
    MoveOr9Or (Move9Or (IsOccupiedOr Occupied))
  p --> Move8Or (IsOccupiedOr (Or m)) =
    MoveOr9Or (p --> m)

instance WithPosition MoveOr8Or MoveOr9Or where
  _ --> MoveOr8OrWin5 w =
    MoveOr9OrWin5 w
  _ --> MoveOr8OrWin6 w =
    MoveOr9OrWin6 w
  _ --> MoveOr8OrWin7 w =
    MoveOr9OrWin7 w
  p --> MoveOr8Or m =
    p --> m
