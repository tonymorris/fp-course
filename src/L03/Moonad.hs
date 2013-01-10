module L03.Moonad where

import L01.Id
import L01.Optional
import L02.List


class Moonad m where
  bind :: (a -> m b) -> m a -> m b
  reeturn :: a -> m a
  -- Exercise 4
  -- Relative Difficulty: 3
  -- (use bind and reeturn)
  fmaap' :: (a -> b) -> m a -> m b
  fmaap' f x = bind (reeturn.f) x

-- Exercise 5
-- Relative Difficulty: 1
instance Moonad Id where
  bind f (Id x) = f x
  reeturn x = Id x

-- Exercise 6
-- Relative Difficulty: 2
instance Moonad List where
  bind _ Nil = Nil
  bind f (x :| xs) = append (f x) (bind f xs) 
  reeturn x = x :| Nil

-- Exercise 7
-- Relative Difficulty: 2
instance Moonad Optional where
  bind _ Empty = Empty
  bind f (Full x) = f x 
  reeturn x = Full x

-- Exercise 8
-- Relative Difficulty: 3
instance Moonad ((->) t) where
  bind f g h = (f.g) h h
  reeturn a _ = a

ffff :: Monad m => m (a -> b) -> m a -> m b
ffff mf ma =
    do f <- mf
       a <- ma
       return (f a)

-- Exercise 9
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten x = bind (\ma -> ma) x

-- Exercise 10
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
-- apply mf ma = reeturn (bind (\f -> f) mf) (bind (\a -> a) ma)
apply mf ma = bind (\f -> bind (\a -> reeturn (f a)) ma) mf


-- Exercise 11
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 = error "todo"

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 = error "todo"

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 = error "todo"

-- Exercise 14
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence = error "todo"

-- Exercise 15
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse = error "todo"

-- Exercise 16
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate = error "todo"

-- Exercise 17
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering = error "todo"

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
