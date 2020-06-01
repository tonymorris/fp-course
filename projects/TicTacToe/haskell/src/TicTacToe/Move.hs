{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module TicTacToe.Move(
  Move1
, AsMove1(_Move1)
, start
, Move2
, AsMove2(_Move2)
, move2
, Move3
, AsMove3(_Move3)
, move3
, Move4
, AsMove4(_Move4)
, move4
, Move5
, AsMove5(_Move5)
, Win5
, AsWin5(_Win5)
, move5
, Move6
, AsMove6(_Move6)
, Win6
, AsWin6(_Win6)
, move6
, Move7
, AsMove7(_Move7)
, Win7
, AsWin7(_Win7)
, move7
, Move8
, AsMove8(_Move8)
, Win8
, AsWin8(_Win8)
, move8
, Move9
, AsMove9(_Move9)
, Win9
, AsWin9(_Win9)
, move9
) where

import Control.Applicative(Applicative((<*>)), Const)
import Control.Category((.), id)
import Control.Lens(Optic', Profunctor, lens, iso, from, elemOf, (#))
import Data.Bool(bool)
import Data.Eq(Eq)
import Data.Functor(Functor, (<$>))
import Data.Functor.Identity(Identity)
import Data.Monoid(Any)
import Data.Ord(Ord)
import Data.Tagged(Tagged)
import Prelude(Show)
import TicTacToe.AsOccupied(AsOccupied, occupied)
import TicTacToe.AsOr(AsOr(_Or))
import TicTacToe.AsWin(AsWin(_Win))
import TicTacToe.OccupiedOr(OccupiedOr)
import TicTacToe.Position(AsPosition(_Position), Position)
import TicTacToe.WinOccupiedOr(WinOccupiedOr)
import TicTacToe.Winpaths(Winpaths(winpaths), hasWin)

newtype Move1 =
  Move1 Position
  deriving (Eq, Ord, Show)

class AsMove1 p f s where
  _Move1 ::
    Optic' p f s Move1

instance AsMove1 p f Move1 where
  _Move1 =
    id

instance (Profunctor p, Functor f) => AsMove1 p f Position where
  _Move1 =
    iso    
      Move1
      (\(Move1 p) -> p)
      
instance (Profunctor p, Functor f) => AsPosition p f Move1 where
  _Position =
    from _Move1

move ::
  (AsPosition (->) (Const Data.Monoid.Any) s, AsOccupied Tagged Identity (o a), AsOr Tagged Identity o) =>
  (Position -> s -> a)
  -> Position
  -> s
  -> o a
move f p m =
  bool (_Or # f p m) occupied (elemOf _Position p m)

wmove :: 
  (AsPosition (->) (Const Any) s, AsOccupied Tagged Identity (o w a), AsWin Tagged Identity o, Winpaths s, AsOr Tagged Identity (o w)) =>
  (Position -> s -> w)
  -> (Position -> s -> a)
  -> Position
  -> s
  -> o w a
wmove f g p m =  
  bool
    (bool (_Or # g p m) (_Win # f p m) (hasWin p m))
    occupied
    (elemOf _Position p m)

start ::
 Position
 -> Move1
start =
  Move1

data Move2 =
  Move2 Position Move1
  deriving (Eq, Ord, Show)

class AsMove2 p f s where
  _Move2 ::
    Optic' p f s Move2

instance AsMove2 p f Move2 where
  _Move2 =
    id

instance (p ~ (->), Functor f) => AsMove1 p f Move2 where
  _Move1 =
    lens    
      (\(Move2 _ m) -> m)
      (\(Move2 p _) m -> Move2 p m)

instance (p ~ (->), Applicative f) => AsPosition p f Move2 where
  _Position f (Move2 p2 (Move1 p1)) =
    (\q1 q2 -> Move2 q2 (Move1 q1)) <$> f p1 <*> f p2

move2 ::
 Position
 -> Move1
 -> OccupiedOr Move2
move2 =
  move Move2

data Move3 =
  Move3 Position Move2
  deriving (Eq, Ord, Show)

class AsMove3 p f s where
  _Move3 ::
    Optic' p f s Move3

instance AsMove3 p f Move3 where
  _Move3 =
    id

instance (p ~ (->), Functor f) => AsMove2 p f Move3 where
  _Move2 =
    lens    
      (\(Move3 _ m) -> m)
      (\(Move3 p _) m -> Move3 p m)

instance (p ~ (->), Functor f) => AsMove1 p f Move3 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move3 where
  _Position f (Move3 p3 (Move2 p2 (Move1 p1))) =
    (\q1 q2 q3 -> Move3 q3 (Move2 q2 (Move1 q1))) <$> f p1 <*> f p2 <*> f p3

move3 ::
 Position
 -> Move2
 -> OccupiedOr Move3
move3 =
  move Move3

data Move4 =
  Move4 Position Move3
  deriving (Eq, Ord, Show)

class AsMove4 p f s where
  _Move4 ::
    Optic' p f s Move4

instance AsMove4 p f Move4 where
  _Move4 =
    id

instance (p ~ (->), Functor f) => AsMove3 p f Move4 where
  _Move3 =
    lens    
      (\(Move4 _ m) -> m)
      (\(Move4 p _) m -> Move4 p m)

instance (p ~ (->), Functor f) => AsMove2 p f Move4 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move4 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move4 where
  _Position f (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1)))) =
    (\q1 q2 q3 q4 -> Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1)))) <$> f p1 <*> f p2 <*> f p3 <*> f p4

instance Winpaths Move4 where
  winpaths (Move4 _ (Move3 m3 (Move2 _ (Move1 m1)))) =
    [(m3, m1)]

move4 ::
 Position
 -> Move3
 -> OccupiedOr Move4
move4 =
  move Move4

data Move5 =
  Move5 Position Move4
  deriving (Eq, Ord, Show)

class AsMove5 p f s where
  _Move5 ::
    Optic' p f s Move5

instance AsMove5 p f Move5 where
  _Move5 =
    id

instance (p ~ (->), Functor f) => AsMove4 p f Move5 where
  _Move4 =
    lens    
      (\(Move5 _ m) -> m)
      (\(Move5 p _) m -> Move5 p m)

instance (p ~ (->), Functor f) => AsMove3 p f Move5 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Move5 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move5 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move5 where
  _Position f (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))) =
    (\q1 q2 q3 q4 q5 -> Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5

instance Winpaths Move5 where
  winpaths (Move5 _ (Move4 m4 (Move3 _ (Move2 m2 (Move1 _))))) =
    [(m4, m2)]

data Win5 =
  Win5 Position Move4
  deriving (Eq, Ord, Show)

class AsWin5 p f s where
  _Win5 ::
    Optic' p f s Win5

instance AsWin5 p f Win5 where
  _Win5 =
    id

instance (p ~ (->), Functor f) => AsMove4 p f Win5 where
  _Move4 =
    lens    
      (\(Win5 _ m) -> m)
      (\(Win5 p _) m -> Win5 p m)

instance (p ~ (->), Functor f) => AsMove3 p f Win5 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Win5 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Win5 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Win5 where
  _Position f (Win5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))) =
    (\q1 q2 q3 q4 q5 -> Win5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5

move5 ::
 Position
 -> Move4
 -> WinOccupiedOr Win5 Move5
move5 =
  wmove Win5 Move5

data Move6 =
  Move6 Position Move5
  deriving (Eq, Ord, Show)

class AsMove6 p f s where
  _Move6 ::
    Optic' p f s Move6

instance AsMove6 p f Move6 where
  _Move6 =
    id

instance (p ~ (->), Functor f) => AsMove5 p f Move6 where
  _Move5 =
    lens    
      (\(Move6 _ m) -> m)
      (\(Move6 p _) m -> Move6 p m)

instance (p ~ (->), Functor f) => AsMove4 p f Move6 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Move6 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Move6 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move6 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move6 where
  _Position f (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1)))))) =
    (\q1 q2 q3 q4 q5 q6 -> Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1)))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6

instance Winpaths Move6 where
  winpaths (Move6 _ (Move5 m5 (Move4 _ (Move3 m3 (Move2 _ (Move1 m1)))))) =
    [(m5, m3), (m5, m1), (m3, m1)]

data Win6 =
  Win6 Position Move5
  deriving (Eq, Ord, Show)

class AsWin6 p f s where
  _Win6 ::
    Optic' p f s Win6

instance AsWin6 p f Win6 where
  _Win6 =
    id

instance (p ~ (->), Functor f) => AsMove5 p f Win6 where
  _Move5 =
    lens    
      (\(Win6 _ m) -> m)
      (\(Win6 p _) m -> Win6 p m)

instance (p ~ (->), Functor f) => AsMove4 p f Win6 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Win6 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Win6 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Win6 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Win6 where
  _Position f (Win6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1)))))) =
    (\q1 q2 q3 q4 q5 q6 -> Win6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1)))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6

move6 ::
 Position
 -> Move5
 -> WinOccupiedOr Win6 Move6
move6 =
  wmove Win6 Move6

data Move7 =
  Move7 Position Move6
  deriving (Eq, Ord, Show)

class AsMove7 p f s where
  _Move7 ::
    Optic' p f s Move7

instance AsMove7 p f Move7 where
  _Move7 =
    id

instance (p ~ (->), Functor f) => AsMove6 p f Move7 where
  _Move6 =
    lens    
      (\(Move7 _ m) -> m)
      (\(Move7 p _) m -> Move7 p m)

instance (p ~ (->), Functor f) => AsMove5 p f Move7 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Move7 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Move7 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Move7 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move7 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move7 where
  _Position f (Move7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 -> Move7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7

instance Winpaths Move7 where
  winpaths (Move7 _ (Move6 m6 (Move5 _ (Move4 m4 (Move3 _ (Move2 m2 (Move1 _))))))) =
    [(m6, m4), (m6, m2), (m4, m2)]

data Win7 =
  Win7 Position Move6
  deriving (Eq, Ord, Show)

class AsWin7 p f s where
  _Win7 ::
    Optic' p f s Win7

instance AsWin7 p f Win7 where
  _Win7 =
    id

instance (p ~ (->), Functor f) => AsMove6 p f Win7 where
  _Move6 =
    lens    
      (\(Win7 _ m) -> m)
      (\(Win7 p _) m -> Win7 p m)

instance (p ~ (->), Functor f) => AsMove5 p f Win7 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Win7 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Win7 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Win7 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Win7 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Win7 where
  _Position f (Win7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 -> Win7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7

move7 ::
 Position
 -> Move6
 -> WinOccupiedOr Win7 Move7
move7 =
  wmove Win7 Move7

data Move8 =
  Move8 Position Move7
  deriving (Eq, Ord, Show)

class AsMove8 p f s where
  _Move8 ::
    Optic' p f s Move8

instance AsMove8 p f Move8 where
  _Move8 =
    id

instance (p ~ (->), Functor f) => AsMove7 p f Move8 where
  _Move7 =
    lens    
      (\(Move8 _ m) -> m)
      (\(Move8 p _) m -> Move8 p m)

instance (p ~ (->), Functor f) => AsMove6 p f Move8 where
  _Move6 =
    _Move7 . _Move6

instance (p ~ (->), Functor f) => AsMove5 p f Move8 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Move8 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Move8 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Move8 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move8 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move8 where
  _Position f (Move8 p8 (Move7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1)))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 q8 -> Move8 q8 (Move7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1)))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7 <*> f p8

instance Winpaths Move8 where
  winpaths (Move8 _ (Move7 m7 (Move6 _ (Move5 m5 (Move4 _ (Move3 m3 (Move2 _ (Move1 m1)))))))) =
    [(m7, m5), (m7, m3), (m7, m1), (m5, m3), (m5, m1), (m3, m1)]

data Win8 =
  Win8 Position Move7
  deriving (Eq, Ord, Show)

class AsWin8 p f s where
  _Win8 ::
    Optic' p f s Win8

instance AsWin8 p f Win8 where
  _Win8 =
    id

instance (p ~ (->), Functor f) => AsMove7 p f Win8 where
  _Move7 =
    lens    
      (\(Win8 _ m) -> m)
      (\(Win8 p _) m -> Win8 p m)

instance (p ~ (->), Functor f) => AsMove6 p f Win8 where
  _Move6 =
    _Move7 . _Move6

instance (p ~ (->), Functor f) => AsMove5 p f Win8 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Win8 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Win8 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Win8 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Win8 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Win8 where
  _Position f (Win8 p8 (Move7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1)))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 q8 -> Win8 q8 (Move7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1)))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7 <*> f p8

move8 ::
 Position
 -> Move7
 -> WinOccupiedOr Win8 Move8
move8 =
  wmove Win8 Move8

data Move9 =
  Move9 Position Move8
  deriving (Eq, Ord, Show)

class AsMove9 p f s where
  _Move9 ::
    Optic' p f s Move9

instance AsMove9 p f Move9 where
  _Move9 =
    id

instance (p ~ (->), Functor f) => AsMove8 p f Move9 where
  _Move8 =
    lens    
      (\(Move9 _ m) -> m)
      (\(Move9 p _) m -> Move9 p m)

instance (p ~ (->), Functor f) => AsMove7 p f Move9 where
  _Move7 =
    _Move8 . _Move7

instance (p ~ (->), Functor f) => AsMove6 p f Move9 where
  _Move6 =
    _Move7 . _Move6

instance (p ~ (->), Functor f) => AsMove5 p f Move9 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Move9 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Move9 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Move9 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Move9 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Move9 where
  _Position f (Move9 p9 (Move8 p8 (Move7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 q8 q9 -> Move9 q9 (Move8 q8 (Move7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7 <*> f p8 <*> f p9

data Win9 =
  Win9 Position Move8
  deriving (Eq, Ord, Show)

class AsWin9 p f s where
  _Win9 ::
    Optic' p f s Win9

instance AsWin9 p f Win9 where
  _Win9 =
    id

instance (p ~ (->), Functor f) => AsMove8 p f Win9 where
  _Move8 =
    lens    
      (\(Win9 _ m) -> m)
      (\(Win9 p _) m -> Win9 p m)

instance (p ~ (->), Functor f) => AsMove7 p f Win9 where
  _Move7 =
    _Move8 . _Move7

instance (p ~ (->), Functor f) => AsMove6 p f Win9 where
  _Move6 =
    _Move7 . _Move6

instance (p ~ (->), Functor f) => AsMove5 p f Win9 where
  _Move5 =
    _Move6 . _Move5

instance (p ~ (->), Functor f) => AsMove4 p f Win9 where
  _Move4 =
    _Move5 . _Move4

instance (p ~ (->), Functor f) => AsMove3 p f Win9 where
  _Move3 =
    _Move4 . _Move3

instance (p ~ (->), Functor f) => AsMove2 p f Win9 where
  _Move2 =
    _Move3 . _Move2

instance (p ~ (->), Functor f) => AsMove1 p f Win9 where
  _Move1 =
    _Move2 . _Move1

instance (p ~ (->), Applicative f) => AsPosition p f Win9 where
  _Position f (Win9 p9 (Move8 p8 (Move7 p7 (Move6 p6 (Move5 p5 (Move4 p4 (Move3 p3 (Move2 p2 (Move1 p1))))))))) =
    (\q1 q2 q3 q4 q5 q6 q7 q8 q9 -> Win9 q9 (Move8 q8 (Move7 q7 (Move6 q6 (Move5 q5 (Move4 q4 (Move3 q3 (Move2 q2 (Move1 q1))))))))) <$> f p1 <*> f p2 <*> f p3 <*> f p4 <*> f p5 <*> f p6 <*> f p7 <*> f p8 <*> f p9

move9 ::
 Position
 -> Move8
 -> WinOccupiedOr Win9 Move9
move9 =
  wmove Win9 Move9
