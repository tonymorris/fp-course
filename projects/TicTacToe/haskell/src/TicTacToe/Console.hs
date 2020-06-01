{-# LANGUAGE FlexibleContexts #-}

module Main(
  main
) where

import Control.Applicative(Const)
import Control.Category((.))
import Control.Lens(( # ), (^?), (%~))

import Data.Foldable(mapM_)
import Data.Function(const)
import Data.Int(Int)
import Data.List(intercalate, concat, elem, (++))
import Data.Maybe
import Data.Monoid(Endo, First)
import Data.String(String)
import System.IO(hSetBuffering, stdin, BufferMode(NoBuffering), putStrLn, getChar, print, IO)
import TicTacToe(Position(P1, P2, P3, P4, P5, P6, P7, P8, P9), AsWin(_Win), AsPlayer(_Player), Player, AsPosition(_Position), IndexingN, whoseTurn, WinOccupiedOr(IsOccupiedOr, Win), OccupiedOr(Occupied, Or), positionPlayer, start, move2, move3, move4, move5, move6, move7, move8, move9, Win5, Win6, Win7, Win8, Win9, Move1, Move2, Move3, Move4, Move5, Move6, Move7, Move8, Move9)
import Prelude(show)

main :: 
  IO ()
main =
  do hSetBuffering stdin NoBuffering
     let moveto5 ::
           (AsPosition (->) (Const (Endo (Endo Int))) g, AsPosition (->) (IndexingN Player (Const (First Player))) g) =>
           (Position -> g -> OccupiedOr t)
           -> (t -> IO ())
           -> g
           -> IO ()
         moveto5 =
           pernextmove IsOccupiedOr

         movefrom5 ::
           (AsPosition (->) (Const (Endo (Endo Int))) h, AsPosition (->) (IndexingN Player (Const (First Player))) g, AsPosition (->) (IndexingN Player (Const (First Player))) h) =>
           (Position -> h -> WinOccupiedOr g t)
           -> (t -> IO ())
           -> h
           -> IO ()
         movefrom5 =
           pernextmove (_Win %~ (\a t -> do putStrLn (showWithoutPositions a)
                                            putStrLn (showPlayer (whoseTurn t) ++ " wins")))
         
     play (permove . const) moveto5 moveto5 moveto5 movefrom5 movefrom5 movefrom5 movefrom5 movefrom5 print

play ::
  ((Position -> c) -> () -> t)
  -> ((Position -> Move1 -> OccupiedOr Move2) -> t1 -> Move1 -> c)
  -> ((Position -> Move2 -> OccupiedOr Move3) -> t2 -> t1)
  -> ((Position -> Move3 -> OccupiedOr Move4) -> t3 -> t2)
  -> ((Position -> Move4 -> WinOccupiedOr Win5 Move5) -> t4 -> t3)
  -> ((Position -> Move5 -> WinOccupiedOr Win6 Move6) -> t5 -> t4)
  -> ((Position -> Move6 -> WinOccupiedOr Win7 Move7) -> t6 -> t5)
  -> ((Position -> Move7 -> WinOccupiedOr Win8 Move8) -> t7 -> t6)
  -> ((Position -> Move8 -> WinOccupiedOr Win9 Move9) -> t8 -> t7)
  -> t8
  -> t
play pm m1 m2 m3 m4 m5 m6 m7 m8 m9 =
  pm (m1 move2 (m2 move3 (m3 move4 (m4 move5 (m5 move6 (m6 move7 (m7 move8 (m8 move9 m9))))))) . start) ()

pernextmove ::
  (AsPosition (->) (Const (Endo (Endo Int))) g, AsPosition (->) (IndexingN Player (Const (First Player))) g) =>
  (b -> WinOccupiedOr (g -> IO ()) t)
  -> (Position -> g -> b)
  -> (t -> IO ())
  -> g
  -> IO ()
pernextmove k fr j g =
  permove (\t p -> case k (fr p g) of
                     Win w -> w t
                     IsOccupiedOr Occupied -> do putStrLn "That position is already occupied. Please try again."
                                                 pernextmove k fr j g
                     IsOccupiedOr (Or m) -> j m) g

permove ::
  (AsPosition (->) (Const (Endo (Endo Int))) g, AsPosition (->) (IndexingN Player (Const (First Player))) g) =>
  (g -> Position -> IO ())
  -> g
  -> IO ()
permove k g =
  let t = whoseTurn g
  in do putStrLn (showWithoutPositions g)
        mapM_ putStrLn
          [
             showPlayer t ++ " to move"
           , "  * [1-9] to Move"
           , "  * q to Quit"
           , "  * v to view positions"
          ]
        c <- getChar
        putStrLn []
        putStrLn "--------------------------------"
        putStrLn []
        if c `elem` "vV"
          then
            do putStrLn (showWithPositions g)
               permove k g
          else 
            if c `elem` "qQ"
              then
                putStrLn "Cheerio"
              else
                case c ^? _Position of
                  Nothing -> do putStrLn ("Invalid selection '" ++ c : "'. Please try again.")
                                permove k g
                  Just p -> k g p
  
showPlayer ::
  Player 
  -> String
showPlayer t =
  "Player " ++ show (_Player # t :: Int) ++ " [" ++ show t ++ "]"

showPositionsUnoccupied ::
  AsPosition (->) (IndexingN Player (Const (First Player))) g =>
  (Position -> String)
  -> g
  -> String
showPositionsUnoccupied f g =
  showEachPosition (\p -> case positionPlayer p g of
                            Nothing -> f p
                            Just q -> show q) 

showWithPositions ::
  AsPosition (->) (IndexingN Player (Const (First Player))) g =>
  g
  -> String
showWithPositions =
  showPositionsUnoccupied (\p -> show (_Position # p :: Int))

showWithoutPositions ::
  AsPosition (->) (IndexingN Player (Const (First Player))) g =>
  g
  -> String
showWithoutPositions =
  showPositionsUnoccupied (const " ")

-- | Shows a board using ASCII notation and substituting the returned string for each position.
showEachPosition ::
  (Position -> String) -- ^ The function returning the string to substitute each position.
  -> String
showEachPosition k =
  let z = ".===.===.===."
      e = [
            z
          , concat ["| ", k P1, " | ", k P2, " | ", k P3, " |"]
          , z
          , concat ["| ", k P4, " | ", k P5, " | ", k P6, " |"]
          , z
          , concat ["| ", k P7, " | ", k P8, " | ", k P9, " |"]
          , z
          ]
  in intercalate "\n" e
