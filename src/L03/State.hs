{-# OPTIONS_GHC -fno-warn-orphans #-}

module L03.State where

import L01.Optional
import L02.List
import L03.Fluffy
import L03.Misty
import Data.Char
import qualified Data.Set as S
import qualified Data.Foldable as F

-- A `State` is a function from a state value `s` to (a produced value `a`, and a resulting state `s`).
newtype State s a =
  State {
    runState ::
      s
      -> (a, s)
  }

-- Exercise 1
-- Relative Difficulty: 2
-- Implement the `Fluffy` instance for `State s`.
instance Fluffy (State s) where
  furry f (State k) =
      State (\s -> let (a, t) = k s in (f a, t))

-- Exercise 2
-- Relative Difficulty: 3
-- Implement the `Misty` instance for `State s`.
-- Make sure the state value is passed through in `banana`.
instance Misty (State s) where
  banana f (State k) =
    State (\s -> let (a, t) = k s in runState (f a) t)
  unicorn a =
    State (\s -> (a, s))

-- Exercise 3
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
exec ::
  State s a
  -> s
  -> s
exec (State k) =
  snd . k

-- Exercise 4
-- Relative Difficulty: 1
-- Run the `State` seeded with `s` and retrieve the resulting state.
eval ::
  State s a
  -> s
  -> a
eval (State k) =
  fst . k

-- Exercise 5
-- Relative Difficulty: 2
-- A `State` where the state also distributes into the produced value.
get ::
  State s s
get =
  State (\s -> (s, s))

-- Exercise 6
-- Relative Difficulty: 2
-- A `State` where the resulting state is seeded with the given value.
put ::
  s
  -> State s ()
put =
  State . const . (,) ()

-- Exercise 7
-- Relative Difficulty: 5
-- This function finds the first element in a `List` that satisfies a given predicate.
-- It is possible that no element is found, hence an `Optional` result.
-- However, while performing the search, we sequence some `Misty` effect through.
--
-- Note the similarity of the type signature to List#find
-- where the effect appears in every return position:
--   find ::  (a ->   Bool) -> List a ->    Optional a
--   findM :: (a -> f Bool) -> List a -> f (Optional a)
findM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (Optional a)
findM _ Nil =
  unicorn Empty
findM p (h :| t) =
  banana (\q -> if q then unicorn (Full h) else findM p t) (p h)

-- Exercise 8
-- Relative Difficulty: 4
-- This function finds the first element in a `List` that repeats.
-- It is possible that no element repeats, hence an `Optional` result.
-- ~~~ Use findM and State with a Data.Set#Set. ~~~
firstRepeat ::
  Ord a =>
  List a
  -> Optional a
firstRepeat x =
  eval (findM (\a -> State (\s -> (a `S.member` s, a `S.insert` s))) x) S.empty

-- Exercise 9
-- Relative Difficulty: 5
-- This function removes all elements in a `List` that fail a given predicate.
-- However, while performing the filter, we sequence some `Misty` effect through.
--
-- Note the similarity of the type signature to List#filter
-- where the effect appears in every return position:
--   filter ::  (a ->   Bool) -> List a ->    List a
--   filterM :: (a -> f Bool) -> List a -> f (List a)
filterM ::
  Misty f =>
  (a -> f Bool)
  -> List a
  -> f (List a)
filterM _ Nil =
  unicorn Nil
filterM p (h :| t) =
 banana (\q -> furry' (if q
                         then
                           (h:|)
                         else
                           id) (filterM p t)) (p h)

-- Exercise 10
-- Relative Difficulty: 4
-- This function removes all duplicate elements in a `List`.
-- ~~~ Use filterM and State with a Data.Set#Set. ~~~
distinct ::
  Ord a =>
  List a
  -> List a
distinct x =
  eval (filterM (\a -> State (\s -> (a `S.notMember` s, a `S.insert` s))) x) S.empty

-- Exercise 11
-- Relative Difficulty: 3
-- This function produces an infinite `List` that seeds with the given value at its head,
-- then runs the given function for subsequent elements
produce ::
  (a -> a)
  -> a
  -> List a
produce f a =
  a :| produce f (f a)

-- Exercise 12
-- Relative Difficulty: 10
-- A happy number is a positive integer, where the sum of the square of its digits eventually reaches 1 after repetition.
-- In contrast, a sad number (not a happy number) is where the sum of the square of its digits never reaches 1
-- because it results in a recurring sequence.
-- ~~~ Use findM with State and produce
-- ~~~ Use jellybean to write a square function
-- ~~~ Use library functions: Data.Foldable#elem, Data.Char#digitToInt
isHappy ::
  Integer
  -> Bool
isHappy =
  F.elem 1 .
    (`eval` S.empty) .
    findM (\j -> State $ \s -> (j == 1 || S.member j s, S.insert j s)) .
    produce (sum .
             map (jellybean (*) .
                  toInteger .
                  digitToInt) .
             show)

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance F.Foldable Optional where
  foldr _ z Empty = z
  foldr f z (Full a) = f a z
