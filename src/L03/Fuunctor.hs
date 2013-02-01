module L03.Fuunctor where

import L01.Id
import L01.Optional
import L01.Validation
import L02.List

class Fuunctor f where
  fmaap :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fuunctor Id where
  fmaap f (Id a) = Id (f a)

-- Exercise 2
-- Relative Difficulty: 2
instance Fuunctor List where
  fmaap _ Nil = Nil
  fmaap f (x :| xs) = (f x) :| fmaap f xs

-- Exercise 3
-- Relative Difficulty: 2
instance Fuunctor Optional where
  fmaap _ Empty = Empty
  fmaap f (Full a) = Full (f a)

-- Exercise 4
-- Relative Difficulty: 3
instance Fuunctor ((->) t) where
  fmaap f g = f.g

-- Exercise 4
-- Relative Difficulty: 2
instance Fuunctor IO where
  fmaap =
    error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Fuunctor [] where
  fmaap = fmap
