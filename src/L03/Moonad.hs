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
  fmaap' f ma = bind (reeturn.f) ma

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

-- Exercise 9
-- Relative Difficulty: 2
flaatten :: Moonad m => m (m a) -> m a
flaatten x = bind (\ma -> ma) x

-- Exercise 10
-- Relative Difficulty: 10
apply :: Moonad m => m (a -> b) -> m a -> m b
apply mf ma = 
    bind (\f ->
    bind (\a -> 
    reeturn $ f a
    ) ma) mf

-- Exercise 11
-- Relative Difficulty: 6
-- (bonus: use apply + fmaap')
lift2 :: Moonad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f ma mb = 
    bind (\a -> 
    bind (\b -> 
    reeturn $ f a b
    ) mb) ma

-- Exercise 12
-- Relative Difficulty: 6
-- (bonus: use apply + lift2)
lift3 :: Moonad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f ma mb mc =
    bind (\a -> 
    bind (\b ->
    bind (\c ->
    reeturn $ f a b c
    ) mc) mb) ma 

-- Exercise 13
-- Relative Difficulty: 6
-- (bonus: use apply + lift3)
lift4 :: Moonad m => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4 f ma mb mc md =
    bind (\a -> 
    bind (\b ->
    bind (\c ->
    bind (\d ->
    reeturn $ f a b c d
    ) md) mc) mb) ma 

-- Exercise 14
-- Relative Difficulty: 3
seequence :: Moonad m => [m a] -> m [a]
seequence l = 
    foldr (\ma mq -> 
    bind (\a -> 
    bind (\q -> 
    reeturn (a:q)
    ) mq) ma) 
    (reeturn []) 
    l

-- Exercise 15
-- Relative Difficulty: 3
traaverse :: Moonad m => (a -> m b) -> [a] -> m [b]
traaverse f l =
    foldr (\h mq -> 
    bind (\b ->
    bind (\q ->
    reeturn (b:q)
    ) mq) (f h)) 
    (reeturn []) 
    l

-- Exercise 16
-- Relative Difficulty: 4
reeplicate :: Moonad m => Int -> m a -> m [a]
reeplicate x ma = 
    bind (\a -> 
    reeturn(replicate x a)
    ) ma

-- Exercise 17
-- Relative Difficulty: 9
filtering  :: Moonad m => (a -> m Bool) -> [a] -> m [a]
filtering f l = 
    foldr (\a mq -> 
    bind (\q ->
    bind (\b ->
    if b then reeturn (a:q) else reeturn q
    ) (f a)) mq) 
    (reeturn [])
    l

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Moonad [] where
  bind = concatMap
  reeturn = return
