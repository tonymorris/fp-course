{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module TicTacToe.Back(
  Back(back)
) where

import Control.Lens((^.))
import TicTacToe.Move(Move1, Move2, Move3, Move4, Move5, Move6, Move7, Move8, Move9, _Move1, _Move2, _Move3, _Move4, _Move5, _Move6, _Move7, _Move8, Win5, Win6, Win7, Win8, Win9)

class Back g f | g -> f where
  back ::
    g
    -> f

instance Back Move9 Move8 where
  back =
    (^. _Move8)

instance Back Win9 Move8 where
  back =
    (^. _Move8)

instance Back Move8 Move7 where
  back =
    (^. _Move7)

instance Back Win8 Move7 where
  back =
    (^. _Move7)

instance Back Move7 Move6 where
  back =
    (^. _Move6)

instance Back Win7 Move6 where
  back =
    (^. _Move6)

instance Back Move6 Move5 where
  back =
    (^. _Move5)

instance Back Win6 Move5 where
  back =
    (^. _Move5)

instance Back Move5 Move4 where
  back =
    (^. _Move4)

instance Back Win5 Move4 where
  back =
    (^. _Move4)

instance Back Move4 Move3 where
  back =
    (^. _Move3)

instance Back Move3 Move2 where
  back =
    (^. _Move2)

instance Back Move2 Move1 where
  back =
    (^. _Move1)

instance Back Move1 () where
  back _ =
    ()
