module Main

import Data.So
import Data.Vect

%default total

data Player = X | O

Show Player where
  show X = "X"
  show O = "O"

Eq Player where
  X == X = True
  O == O = True
  _ == _ = False

Grid : Type
Grid = Vect 3 (Vect 3 (Maybe Player))

get : Grid
  -> (x : Fin 3)
  -> (y : Fin 3)
  -> Maybe Player
get g x y = index x (index y g)

set : Grid
  -> (x : Fin 3)
  -> (y : Fin 3)
  -> Maybe Player
  -> Grid
set g x y a = replaceAt y (replaceAt x a $ index y g) g

data Result = Draw | Win Player
data Status = InPlay | Finished Result

occupied : Grid -> Fin 3 -> Fin 3 -> Bool
occupied g x y = case get g x y of
                      Nothing => False
                      _       => True

data Position : Fin 3 -> Fin 3 -> Type where
  MkPosition : (x : Fin 3) -> (y : Fin 3) -> Position x y

toggle : Player -> Player
toggle X = O
toggle O = X

data Board : Grid -> Player -> Nat -> Type where
  NewBoard : Board (replicate 3 (replicate 3 Nothing)) X 0
  Turn : Board g p m
      -> Position x y
      -> {notOccupied : So (not (occupied g x y))}
      -> Board (set g x y (Just p)) (toggle p) (S m)

data HasPrevious : Board g p m -> Type where
  MkHasPrevious : HasPrevious (Turn b pos)

status : Board g p m -> Status
status b {g} {m} = case choice . map winner $ conditions g of
                        Nothing => if m == 9
                                      then Finished Draw
                                      else InPlay
                        Just p  => Finished $ Win p
  where
    conditions : Grid -> List (Vect 3 (Maybe Player))
    conditions g = let g' = sequence g in [
      index 0 g
      , index 1 g
      , index 2 g
      , index 0 g'
      , index 1 g'
      , index 2 g'
      , [get g 0 0, get g 1 1, get g 2 2]
      , [get g 2 0, get g 1 1, get g 0 2]
      ]

    winner : Vect 3 (Maybe Player) -> Maybe Player
    winner (Nothing :: b :: c ::[]) = Nothing
    winner (a :: b :: c ::[]) = if a == b && b == c then a else Nothing

data Game : Board g p m -> Status -> Type where
  MkGame : (b : Board g p m) -> Game b (status b)

-- API Functions

move : {b : Board g p m}
    -> Game b InPlay
    -> (pos : Position x y)
    -> {notOccupied : So (not (occupied g x y))}
    -> (s' : Status ** Game (Turn b pos {notOccupied=notOccupied}) s')
move game pos {b} {notOccupied} = (_ ** MkGame (Turn b pos {notOccupied=notOccupied}))

whoWon : Game b (Finished res) -> Result
whoWon game {res} = res

playerAt : {b : Board g p m} -> Game b s -> Position x y -> Maybe Player
playerAt game (MkPosition x y) {g} = get g x y

isDraw : {b : Board g p 9} -> Game b (Finished res) -> Bool
isDraw game {res = Draw} = True
isDraw game {res = (Win x)} = False

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

Uninhabited (HasPrevious NewBoard) where
  uninhabited MkHasPrevious impossible

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
