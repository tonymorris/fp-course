{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Person where

import Course.Core
import Course.List

-- Suppose we have a data structure to represent a person. The person data structure has these attributes:
--     * Age: positive integer
--     * First Name: non-empty string that starts with a capital letter and is followed by zero or more lower-case letters
--     * Surname: string that starts with a capital letter and is followed by 5 or more lower-case letters
--     * Smoker: character that must be 'y' or 'n'
--     * Phone: string of digits, dots or hyphens but must start with a digit and end with a hash (#)
data Person = Person {
  age :: Int,
  firstName :: Chars,
  surname :: Chars,
  smoker :: Char,
  phone :: Chars
} deriving (Eq, Show)

