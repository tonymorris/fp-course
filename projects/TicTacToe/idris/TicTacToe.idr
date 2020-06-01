-- idris v0.12.1
module Main

import Data.So
import Data.Vect

%default total

-- A player is either an X or an O
data Player = X | O

Show Player where
  show X = "X"
  show O = "O"

Eq Player where
  X == X = True
  O == O = True
  _ == _ = False

-- A grid is a Vect of 3 rows, which are each Vects of 3 cells
Grid : Type
Grid = Vect 3 (Vect 3 (Maybe Player))

-- Given x < 3 and y < 3, get the cell at (x,y)
get : Grid
  -> (x : Fin 3)
  -> (y : Fin 3)
  -> Maybe Player
get g x y = index x (index y g)

-- Given x < 3 and y < 3, update the cell at (x,y)
set : Grid
  -> (x : Fin 3)
  -> (y : Fin 3)
  -> Maybe Player
  -> Grid
set g x y a = replaceAt y (replaceAt x a $ index y g) g

-- A game of tic tac toe is either won by a player, or drawn
data Result = Draw | Win Player

-- A game is always either in play or finished. If a game is finished, it will have a result
data Status = InPlay | Finished Result

-- Is the cell at (x,y) occupied?
occupied : Grid -> Fin 3 -> Fin 3 -> Bool
occupied g x y = case get g x y of
                      Nothing => False
                      _       => True

-- A position on the board
-- Lifts x and y to type level allow them to be used in proofs
data Position : Fin 3 -> Fin 3 -> Type where
  MkPosition : (x : Fin 3) -> (y : Fin 3) -> Position x y

toggle : Player -> Player
toggle X = O
toggle O = X

-- A board consists of a Grid, the current Player, and the number of moves played
--
-- A valid empty board can always be constructed
-- The result of a turn is only valid if the previous board was valid, and the Position
--   to play at was not occupied
data Board : Grid -> Player -> Nat -> Type where
  NewBoard : Board (replicate 3 (replicate 3 Nothing)) X 0
  Turn : Board g p m
      -> Position x y
      -> {notOccupied : So (not (occupied g x y))}
      -> Board (set g x y (Just p)) (toggle p) (S m)

-- A board has a previous state if it was contructed with Turn
data HasPrevious : Board g p m -> Type where
  MkHasPrevious : HasPrevious (Turn b pos)

-- Determine the status of a Board
status : Board g p m -> Status
status b {g} {m} = case choice . map winner $ conditions g of
                        Nothing => if m == 9
                                      then Finished Draw
                                      else InPlay
                        Just p  => Finished $ Win p
  where
    -- Win conditions
    conditions : Grid -> List (Vect 3 (Maybe Player))
    conditions g = let g' = sequence g in [
      index 0 g -- First row of g
      , index 1 g -- Second row of g
      , index 2 g -- Third row of g
      , index 0 g' -- First column of g (first row of transpose of g)
      , index 1 g' -- Second column of g (second row of transpose of g)
      , index 2 g' -- Third column of g (third row of transpose of g)
      , [get g 0 0, get g 1 1, get g 2 2] -- Top left to bottom right
      , [get g 2 0, get g 1 1, get g 0 2] -- Bottom left to top right
      ]

    winner : Vect 3 (Maybe Player) -> Maybe Player
    winner (Nothing :: b :: c ::[]) = Nothing
    winner (a :: b :: c ::[]) = if a == b && b == c then a else Nothing

-- Lift a valid Board to a Game and calculate the Status of the Game
data Game : Board g p m -> Status -> Type where
  MkGame : (b : Board g p m) -> Game b (status b)

--
-- API Functions
--

-- Cannot be called on a game that is Finished, and cannot be called with a position
--   that is occpuied
--
-- Returns a new Game which has a Status that is known at runtime
move : {b : Board g p m}
    -> Game b InPlay
    -> (pos : Position x y)
    -> {notOccupied : So (not (occupied g x y))}
    -> (s' : Status ** Game (Turn b pos {notOccupied=notOccupied}) s')
move game pos {b} {notOccupied} = (_ ** MkGame (Turn b pos {notOccupied=notOccupied}))

-- Can only be called on a game that is Finshed
whoWon : Game b (Finished res) -> Result
whoWon game {res} = res

-- Can be called on any Game
playerAt : {b : Board g p m} -> Game b s -> Position x y -> Maybe Player
playerAt game (MkPosition x y) {g} = get g x y

-- Can only be called on a Game that is Finished and has exactly 9 moves played
isDraw : {b : Board g p 9} -> Game b (Finished res) -> Bool
isDraw game {res = Draw} = True
isDraw game {res = (Win x)} = False

-- Can only be called on a Game that has a previous state
--
-- Returns a Board that has a Grid, Player and number of moves that are known at runtime
takeMoveBack : (b : Board g p m) -> {hasPrev : HasPrevious b} -> (g' : Grid ** p' : Player ** m' : Nat ** Board g' p' m')
takeMoveBack (Turn b pos) {hasPrev=MkHasPrevious} = (_ ** _ ** _ ** b)

data Command = Place (x : Fin 3 ** y : Fin 3 ** Position x y)
             | Back

parseInput : String -> Either String Command
parseInput str = case str of
                      "tl" => Right . Place $ (_ ** _ ** MkPosition 0 0)
                      "tc" => Right . Place $ (_ ** _ ** MkPosition 1 0)
                      "tr" => Right . Place $ (_ ** _ ** MkPosition 2 0)
                      "ml" => Right . Place $ (_ ** _ ** MkPosition 0 1)
                      "mc" => Right . Place $ (_ ** _ ** MkPosition 1 1)
                      "mr" => Right . Place $ (_ ** _ ** MkPosition 2 1)
                      "bl" => Right . Place $ (_ ** _ ** MkPosition 0 2)
                      "bc" => Right . Place $ (_ ** _ ** MkPosition 1 2)
                      "br" => Right . Place $ (_ ** _ ** MkPosition 2 2)
                      "back" => Right Back
                      _    => Left "Invalid command"

showGrid : Grid -> String
showGrid (t :: m :: b :: []) = "\n" ++ showRow t ++ "\n\n" ++ showRow m ++ "\n\n" ++ showRow b ++ "\n"
  where
    showCell : Maybe Player -> String
    showCell Nothing = "-"
    showCell (Just p) = show p

    showRow : Vect 3 (Maybe Player) -> String
    showRow (l :: c :: r :: []) = showCell l ++ " " ++ showCell c ++ " " ++ showCell r

-- Proof that NewBoard does not have a previous state
Uninhabited (HasPrevious NewBoard) where
  uninhabited MkHasPrevious impossible

-- Decide if a Board has a previous state and return the proof
decHasPrevious : (b : Board g p m) -> Dec (HasPrevious b)
decHasPrevious NewBoard = No uninhabited
decHasPrevious (Turn z w) = Yes MkHasPrevious

mutual
  partial
  runGame : {b : Board g p m} -> Game b InPlay -> IO Result
  runGame game {g} = do
    putStrLn $ showGrid g
    getInput game

  partial
  gameStep : {b' : Board g' p' m'} -> (s' : Status ** Game b' s') -> IO Result
  gameStep (InPlay ** game) = runGame game
  gameStep {g'} (Finished x ** game) = do
    putStrLn $ showGrid g'
    return x

  partial
  getInput : {b : Board g p m} -> Game b InPlay -> IO Result
  getInput game {b} {g} {p} = do
    putStr $ show p ++ "> "
    input <- getLine
    case parseInput input of
      Right (Place (x ** y ** pos)) => case choose $ occupied g x y of
        Right prf => do
          putStrLn ""
          gameStep $ move game pos {notOccupied=prf}
        Left _ => do
          putStrLn "That position is occupied"
          getInput game
      Right Back => case decHasPrevious b of
        Yes prf => let (_ ** _ ** _ ** b') = takeMoveBack b {hasPrev=prf} in gameStep (_ ** MkGame b')
        No _    => do
          putStrLn "Can't go back"
          getInput game
      Left err => do
        putStrLn err
        getInput game

showResult : Result -> String
showResult Draw = "The game was a draw."
showResult (Win p) = show p ++ " won."

partial
main : IO ()
main = do
  res <- runGame $ MkGame NewBoard
  putStrLn $ showResult res
