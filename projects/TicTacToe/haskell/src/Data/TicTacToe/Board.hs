{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | A tic-tac-toe board is one of nine positions, each position occupied by either player 1, player 2 or neither and with invariants specific to the rules of tic-tac-toe.
--
-- For example, the number of positions occupied by player 1 is equal to, or one more, than the positions occupied by player 2.
module Data.TicTacToe.Board
{-
(
-- * Board data types
  EmptyBoard
, Board
, FinishedBoard
-- * Start new game
, empty
-- * Game completed
, getResult
-- * Make a move on a board
, Move(..)
, MoveResult
, foldMoveResult
, keepPlayingOr
, keepPlaying
-- * Taking a move back from a board
, TakeBack(..)
, TakenBack
, foldTakenBack
, takenBackBoard
-- * Operations common to boards in-play and completed
, BoardLike(..)
-- * Printing
, showEachPosition
, showEachPositionFlat
, showLinePosition
) -} where


import Prelude hiding (any, all, concat, foldr, sum)
import Data.TicTacToe.Position
import Data.TicTacToe.Player
import Data.TicTacToe.GameResult
import qualified Data.Set as S
import Data.Foldable
import Data.List(intercalate)
import Data.List.NonEmpty
import Data.Maybe

data Empty =
  Empty
  deriving (Eq, Show)

data Token =
  EmptyToken
  | PlayerToken Player
  deriving (Eq, Show)

data Board =
  Board
    (NonEmpty Position)
  deriving (Eq, Show)

data Finished =
  Finished
    Board
    Position
    !GameResult
  deriving (Eq, Show)

data Unfinished =
  UnfinishedEmpty
  | UnfinishedBoard Board
  deriving (Eq, Show)

data Unempty =
  UnemptyFinished Finished
  | UnemptyBoard Board
  deriving (Eq, Show)

data BoardMoveResult =
  UnemptyResult Unempty
  | AlreadyOccupied
  deriving (Eq, Show)

class Move from to | from -> to where
  (-->) ::
    Position
    -> from
    -> to

infixr 5 -->

instance Move Empty Board where
  p --> Empty =
    Board (return p)

instance Move Board BoardMoveResult where
  p --> bd@(Board (p' :| ps)) =
    let twos ::
          [a]
          -> [(a, a)]
        twos [] =
          []
        twos (h:t) =
          fmap (\a -> (h, a)) t ++ twos t
        threes ::
          [a]
          -> [(a, a, a)]
        threes [] =
          []
        threes (h:t) =
          fmap (\(a1, a2) -> (h, a1, a2)) (twos t) ++ threes t
        split ::
          Eq a =>
          a
          -> [a]
          -> Maybe ([a], [a], Int)
        split w =
          let at ::
                (Int -> Int)
                -> (a -> a)
                -> (a, a, Int)
                -> (a, a, Int)
              at n f (a1, a2, b) =
                if even b
                then (f a1, a2, n b)
                else (a1, f a2, n b)
          in fmap (\v -> at id (w:) v) .
               foldr (\a r -> r >>= \v ->
                 if w == a
                 then Nothing
                 else Just (at (+1) (a:) v)) (Just ([], [], 0))
        positions ::
          [Position]
        positions =
          p' : ps
        splat ::
          Maybe ([Position], [Position], Int)
        splat =
          split p positions
        pthrees ::
          Maybe ([(Position, Position, Position)], [(Position, Position, Position)], Int)
        pthrees =
          fmap (\(p1, p2, x) -> (threes p1, threes p2, x)) splat
    in case pthrees of
         Nothing ->
           AlreadyOccupied
         Just (_, _, 9) ->
           UnemptyResult (UnemptyFinished (Finished bd p draw))
         Just (l1, l2, n) ->
           let findWin (l, pl) e = if any (\(q1, q2, q3) -> sum (fmap magic [q1, q2, q3]) == 15) l
                                   then UnemptyResult (UnemptyFinished (Finished bd p (win pl)))
                                   else e
           in if n == 9
              then UnemptyResult (UnemptyFinished (Finished bd p draw))
              else findWin (if even n then (l1, player1) else (l2, player2)) (UnemptyResult (UnemptyBoard (Board (p :| positions))))

instance Move Unfinished BoardMoveResult where
  p --> UnfinishedEmpty =
    UnemptyResult (UnemptyBoard (p --> Empty))
  p --> UnfinishedBoard b =
    p --> b

instance Move Unempty BoardMoveResult where
  _ --> UnemptyFinished f =
    UnemptyResult (UnemptyFinished f)
  p --> UnemptyBoard b =
    p --> b

instance Move BoardMoveResult BoardMoveResult where
  p --> UnemptyResult e =
    p --> e
  _ --> AlreadyOccupied =
    AlreadyOccupied

{-
data EmptyBoard =
  EmptyBoard
  deriving Eq

instance Show EmptyBoard where
  show EmptyBoard =
    ".=?=.=?=.=?=.=?=.=?=.=?=.=?=.=?=.=?=. [ Player 1 to move ]"

class Move from to | from -> to where
  (-->) ::
    Position
    -> from
    -> to

infixr 5 -->

instance Move EmptyBoard Board where
  p --> _ =
    [(p, player1)] `Board` M.singleton p player1

instance Move Board MoveResult where
  p --> bd@(Board q m) =
    let w       = whoseTurn bd
        (j, m') = M.insertLookupWithKey (\_ x _ -> x) p w m
        wins =
          [
            (NW, W , SW)
          , (N , C , S )
          , (NE, E , SE)
          , (NW, N , NE)
          , (W , C , E )
          , (SW, S , SE)
          , (NW, C , SE)
          , (SW, C , NE)
          ]
        allEq (d:e:t) = d == e && allEq (e:t)
        allEq _       = True
        isWin         = any (\(a, b, c) -> any allEq $ mapM (`M.lookup` m') [a, b, c]) wins
        isD           = all (`M.member` m') [minBound ..]
        b'            = Board ((p, w):q) m'
    in maybe (if isWin
              then
                GameFinished (b' `FinishedBoard` win w)
              else
                if isD
                then
                  GameFinished (b' `FinishedBoard` draw)
                else
                  KeepPlaying b') (const PositionAlreadyOccupied) j

instance Move MoveResult MoveResult where
  p --> r =
    keepPlayingOr r (\b -> p --> b) r

-- | The result of making a move on a tic-tac-toe board.
data MoveResult =
  PositionAlreadyOccupied -- ^ The move was to a position that is already occupied by a player.
  | KeepPlaying Board -- ^ The move was valid and the board is in a new state.
  | GameFinished FinishedBoard -- ^ The move was valid and the game is complete.
  deriving Eq

-- | Deconstruct a move result.
foldMoveResult ::
  a -- ^ The move was to a position that is already occupied by a player.
  -> (Board -> a) -- ^ The move was valid and the board is in a new state.
  -> (FinishedBoard -> a) -- ^ The move was valid and the game is complete.
  -> MoveResult
  -> a
foldMoveResult occ _ _ PositionAlreadyOccupied =
  occ
foldMoveResult _ kp _ (KeepPlaying b) =
  kp b
foldMoveResult _ _ gf (GameFinished b) =
  gf b

-- | Return the value after function application to the board to keep playing.
keepPlayingOr ::
  a -- ^ The value to return if there is no board to keep playing with.
  -> (Board -> a) -- ^ A function to apply to the board to keep playing with.
  -> MoveResult
  -> a
keepPlayingOr def kp =
  foldMoveResult def kp (const def)

-- | Return the possible board from a move result. A board is returned if the result is to continue play.
--
-- prop> (\b' -> whoseTurn b /= whoseTurn b') `all` keepPlaying (p --> (b :: Board))
keepPlaying ::
  MoveResult
  -> Maybe Board
keepPlaying (KeepPlaying b) =
  Just b
keepPlaying _               =
  Nothing

instance Show MoveResult where
  show PositionAlreadyOccupied = "*Position already occupied*"
  show (KeepPlaying b)         = concat ["{", show b, "}"]
  show (GameFinished b)        = concat ["{{", show b, "}}"]

class TakeBack to from | to -> from where
  takeBack ::
    to
    -> from

-- |
--
-- prop> foldMoveResult True (foldTakenBack False (==b) . takeBack) (\fb -> takeBack fb == b) (p --> b)
instance TakeBack FinishedBoard Board where
  takeBack (FinishedBoard (Board ((p, _):t) m) _) =
    Board t (p `M.delete` m)
  takeBack (FinishedBoard (Board [] _) _) =
    error "Broken invariant: board-in-play with empty move list. This is a program bug."

data TakenBack =
  TakeBackIsEmpty
  | TakeBackIsBoard Board
  deriving Eq

foldTakenBack ::
  a
  -> (Board -> a)
  -> TakenBack
  -> a
foldTakenBack e _ TakeBackIsEmpty =
  e
foldTakenBack _ k (TakeBackIsBoard b) =
  k b

takenBackBoard ::
  TakenBack
  -> Maybe Board
takenBackBoard =
  foldTakenBack Nothing Just

instance TakeBack Board TakenBack where
  takeBack (Board (_:[]) _) =
    TakeBackIsEmpty
  takeBack (Board ((p, _):t) m) =
    TakeBackIsBoard (Board t (p `M.delete` m))
  takeBack (Board [] _) =
    error "Broken invariant: board-in-play with empty move list. This is a program bug."

-- | A tic-tac-toe board.
data Board =
  Board [(Position, Player)] !(M.Map Position Player)
  deriving Eq

instance Show Board where
  show b@(Board _ m) =
    intercalate " " [showPositionMap m, "[", show (whoseTurn b), "to move ]"]

-- | A finished board is a completed tic-tac-toe game and does not accept any more moves.
data FinishedBoard =
  FinishedBoard Board GameResult
  deriving Eq

-- | Return the result of a completed tic-tac-toe game.
getResult ::
  FinishedBoard
  -> GameResult
getResult (FinishedBoard _ r) =
  r

instance Show FinishedBoard where
  show (FinishedBoard (Board _ m) r) =
    let summary = gameResult (\p -> show p ++ " wins") "draw" r
    in intercalate " " [showPositionMap m, "[[", summary, "]]"]

-- | Start an empty tic-tac-toe board.
empty ::
  EmptyBoard
empty =
  EmptyBoard

-- | Shows a board using ASCII notation and substituting the returned string for each position.
showEachPosition ::
  (Position -> String) -- ^ The function returning the string to substitute each position.
  -> String
showEachPosition k =
  let z = ".===.===.===."
      each = [
               z
             , concat ["| ", k NW, " | ", k N , " | ", k NE, " |"]
             , z
             , concat ["| ", k W , " | ", k C , " | ", k E , " |"]
             , z
             , concat ["| ", k SW, " | ", k S , " | ", k SE, " |"]
             , z
             ]
  in intercalate "\n" each

showEachPositionFlat ::
  (Position -> String) -- ^ The function returning the string to substitute each position.
  -> String
showEachPositionFlat k =
  concat ("1 2 3 4 5 6 7 8 9\n" : map (k$) [NW ..])

showLinePosition ::
  (Position -> String)
  -> String
showLinePosition k =
  concat ["|", k NW, k N, k NE, "|", k W, k C, k E, "|", k SW, k S, k SE, "|"]

-- | Functions that work on boards that are in play or have completed.
--
-- This class specifically does not specify moving on a board, since this is illegal on a completed board.
class BoardLike b where
  -- | Returns whose turn it is on a tic-tac-toe board.
  whoseTurn ::
    b
    -> Player
  whoseTurn =
    alternate . whoseNotTurn

  -- | Returns whose turn it is not on a tic-tac-toe board.
  whoseNotTurn ::
    b
    -> Player
  whoseNotTurn =
    alternate . whoseTurn

  -- | Returns whether or not the board is empty.
  isEmpty ::
    b
    -> Bool

  -- | Returns positions that are occupied.
  occupiedPositions ::
    b
    -> S.Set Position

  -- | Returns the number of moves that have been played.
  moves ::
    b
    -> Int

  -- | Returns whether or not the first given board can transition to the second given board.
  isSubboardOf ::
    b
    -> b
    -> Bool

  -- | Returns whether or not the first given board can transition to the second given board and they are inequal.
  isProperSubboardOf ::
    b
    -> b
    -> Bool

  -- | Returns the player at the given position.
  playerAt ::
    b
    -> Position
    -> Maybe Player

  -- | Returns the player at the given position or the given default.
  playerAtOr ::
    b
    -> Position
    -> Player
    -> Player
  playerAtOr b p q =
    q `fromMaybe` playerAt b p

  -- | Returns whether or not the given position is occupied on the board. @true@ if occupied.
  isOccupied ::
    b
    -> Position
    -> Bool
  isOccupied b p =
    isJust $ playerAt b p

  -- | Returns whether or not the given position is occupied on the board. @false@ if occupied.
  isNotOccupied ::
    b
    -> Position
    -> Bool
  isNotOccupied b p =
    not (isOccupied b p)

  -- | Show the board using an ASCII grid representation.
  showBoard ::
    b
    -> String

  -- | Show the board using a single line.
  showLine ::
    b
    -> String

instance BoardLike EmptyBoard where
  whoseTurn _ =
    player1

  isEmpty _ =
    True

  occupiedPositions _ =
    S.empty

  moves _ =
    0

  isSubboardOf _ _ =
    True

  isProperSubboardOf _ _ =
    False

  playerAt _ _ =
    Nothing

  showBoard _ =
    showEachPosition (pos M.empty " ")

  showLine _ =
    showLinePosition (pos M.empty ".")

-- |
--
-- prop> whoseTurn (b :: Board) /= whoseNotTurn b
instance BoardLike Board where
  whoseTurn (Board [] _) =
    player1

  whoseTurn (Board ((_, q):_) _) =
    alternate q

  isEmpty _ =
    False

  occupiedPositions (Board _ m) =
    M.keysSet m

  moves (Board _ m) =
    M.size m

  isSubboardOf (Board _ m) (Board _ m') =
    m `M.isSubmapOf` m'

  isProperSubboardOf (Board _ m) (Board _ m') =
    m `M.isProperSubmapOf` m'

  playerAt (Board _ m) p =
    p `M.lookup` m

  showBoard (Board _ m) =
    showEachPosition (pos m " ")

  showLine (Board _ m) =
    showLinePosition (pos m ".")

instance BoardLike FinishedBoard where
  isEmpty (FinishedBoard b _) =
    isEmpty b

  occupiedPositions (FinishedBoard b _) =
    occupiedPositions b

  moves (FinishedBoard b _) =
    moves b

  isSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isSubboardOf` b'

  isProperSubboardOf (FinishedBoard b _) (FinishedBoard b' _) =
    b `isProperSubboardOf` b'

  playerAt (FinishedBoard b _) p =
    b `playerAt` p

  showBoard (FinishedBoard b _) =
    showBoard b

  showLine (FinishedBoard b _) =
    showLine b

-- not exported

pos ::
  Ord k =>
  M.Map k Player
  -> String
  -> k
  -> String
pos m e p =
  maybe e (return . toSymbol) (p `M.lookup` m)

showPositionMap ::
  M.Map Position Player
  -> String
showPositionMap m =
  let pos' = pos m "?"
  in concat [ ".=",  pos' NW, "=.=", pos' N , "=.=", pos' NE
            , "=.=", pos' W , "=.=", pos' C , "=.=", pos' E
            , "=.=", pos' SW, "=.=", pos' S , "=.=", pos' SE, "=."
            ]
  -}