module L06.MoreParser where

import L01.Validation
import L05.Parser
import Data.Char
import Numeric
import Control.Applicative
import Control.Monad

-- Parses the given input and returns the result.
-- The remaining input is ignored.
(<.>) ::
  Parser a
  -> Input
  -> Validation a
(<.>) i =
  mapValidation snd . parse i

-- Exercise 1
-- Write a parser that will parse zero or more spaces.
spaces ::
  Parser String
spaces =
  list space

-- Exercise 2
-- Write a function that applies the given parser, then parses 0 or more spaces,
-- then produces the result of the original parser.
-- ~~~ Use the monad instance ~~~
tok ::
  Parser a
  -> Parser a
tok p =
  do v <- p
     spaces
     return v

-- Exercise 3
-- Write a function that parses the given char followed by 0 or more spaces.
-- ~~~ Use tok and is ~~~
charTok ::
  Char
  -> Parser Char
charTok =
  tok . is

-- Exercise 4
-- Write a parser that parses a comma ',' followed by 0 or more spaces.
-- ~~~ Use charTok ~~~
commaTok ::
  Parser Char
commaTok =
  charTok ','

-- Exercise 5
-- Write a parser that parses either a double-quote or a single-quote.
-- ~~~ Use is and (|||) ~~~
quote ::
  Parser Char
quote =
  is '"' ||| is '\"'

-- Exercise 6
-- Write a function that parses the given string (fails otherwise).
-- ~~~ Use is and mapM ~~~
string ::
  String
  -> Parser String
string =
  mapM is

-- Exercise 7
-- Write a function that parsers the given string, followed by 0 or more spaces.
-- ~~~ Use tok and string ~~~
stringTok ::
  String
  -> Parser String
stringTok =
  tok . string

-- Exercise 8
-- Write a function that tries the given parser, otherwise succeeds by producing the given value.
-- ~~~ Use (|||) ~~~
option ::
  a
  -> Parser a
  -> Parser a
option a p =
  p ||| return a

-- Exercise 9
-- Write a parser that parses 1 or more digits.
-- ~~~ Use many1 and digit ~~~
digits1 ::
  Parser String
digits1 =
  many1 digit

-- Exercise 10
-- Write a function that parses one of the characters in the given string.
-- ~~~ Use satisfy and elem ~~~
oneof ::
  String
  -> Parser Char
oneof s =
  satisfy (`elem` s)

-- Exercise 11
-- Write a function that parses any character, but fails if it is in the given string.
-- ~~~ Use satisfy and notElem ~~~
noneof ::
  String
  -> Parser Char
noneof s =
  satisfy (`notElem` s)

-- Exercise 12
-- Write a function that applies the first parser, runs the second parser keeping the result,
-- then runs the third parser and produces the obtained result.
-- ~~~ Use the Monad instance ~~~
between ::
  Parser o
  -> Parser c
  -> Parser a
  -> Parser a
between o c a =
  do o
     v <- a
     c
     return v

-- Exercise 13
-- Write a function that applies the given parser in between the two given characters.
-- ~~~ Use between and charTok ~~~
betweenCharTok ::
  Char
  -> Char
  -> Parser a
  -> Parser a
betweenCharTok a b =
  between (charTok a) (charTok b)

-- Exercise 14
-- Write a function that parses the character 'u' followed by 4 hex digits and return the character value.
-- ~~~ Use readHex, isHexDigit, replicateM, satisfy and the Monad instance ~~~
hex ::
  Parser Char
hex =
  let hInt s = case readHex s of
                 [] -> 0
                 ((n, _):_) -> n
  in do is 'u'
        h <- replicateM 4 (satisfy isHexDigit)
        return . chr . hInt $ h

-- Exercise 15
-- Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
-- separated by the second given parser.
-- ~~~ Use list and the Monad instance ~~~
sepby1 ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby1 p s =
  do v <- p
     w <- list (s >> p)
     return (v:w)

-- Exercise 16
-- Write a function that produces a list of values coming off the given parser,
-- separated by the second given parser.
-- ~~~ Use sepby1 and (|||) ~~~
sepby ::
  Parser a
  -> Parser s
  -> Parser [a]
sepby p s =
  sepby1 p s ||| return []

-- Exercise 17
-- Write a parser that asserts that there is no remaining input.
eof ::
  Parser ()
eof =
  P (\s -> case s of
             [] -> Value ([], ())
             x -> Error ("Expected EOF but got " ++ x))

-- Exercise 18
-- Write a parser that produces a characer that satisfies all of the given predicates.
-- ~~~ Use sequence and Data.List.and ~~~
satisfyAll ::
  [Char -> Bool]
  -> Parser Char
satisfyAll ps =
  satisfy (and  . sequence ps)

-- Exercise 19
-- Write a parser that produces a characer that satisfies any of the given predicates.
-- ~~~ Use sequence and Data.List.or ~~~
satisfyAny ::
  [Char -> Bool]
  -> Parser Char
satisfyAny ps =
  satisfy (or  . sequence ps)

-- Exercise 20
-- Write a parser that parses between the two given characters, separated by a comma character ','.
-- ~~~ Use betweenCharTok, sepby and charTok ~~~
betweenSepbyComma ::
  Char
  -> Char
  -> Parser a
  -> Parser [a]
betweenSepbyComma a b g =
  betweenCharTok a b $ g `sepby` charTok ','
