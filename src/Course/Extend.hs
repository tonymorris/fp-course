{-# LANGUAGE NoImplicitPrelude #-}

module Course.Extend where

import Course.Core
import Course.Id
import Course.List
import Course.Optional
import Course.Functor

class Functor f => Extend f where
  (<<=) ::
    (f a -> b)
    -> f a
    -> f b

infixr 1 <<=

-- | Implement the @Extend@ instance for @Id@.
instance Extend Id where
  f <<= i =
    Id (f i)

-- | Implement the @Extend@ instance for @List@.
instance Extend List where
  _ <<= Nil =
    Nil
  f <<= x@(_ :. t) =
    f x :. (f <<= t)

-- | Implement the @Extend@ instance for @Optional@.
instance Extend Optional where
  f <<= o =
    f . Full <$> o

cojoin ::
  Extend f =>
  f a
  -> f (f a)
cojoin =
  error "todo"
