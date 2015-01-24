{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RebindableSyntax #-}

module Course.MoreParser where

import Course.Core
import Course.Parser
import Course.List
import Course.Optional
import Course.Applicative
import Course.Apply
import Course.Bind
import Course.Functor
import Course.Traversable

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Course.Parser(isErrorResult, character, lower, is)
-- >>> import Data.Char(isUpper, isLower)

-- | Parses the given input and returns the result.
-- The remaining input is ignored.
(<.>) ::
  Parser a
  -> Input
  -> Optional a
P p <.> i =
  case p i of
    Result _ a -> Full a
    _          -> Empty

-- | Write a parser that will parse zero or more spaces.
--
-- >>> parse spaces " abc"
-- Result >abc< " "
--
-- >>> parse spaces "abc"
-- Result >abc< ""
spaces ::
  Parser Chars
spaces =
  list space

-- | Write a function that applies the given parser, then parses 0 or more spaces,
-- then produces the result of the original parser.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (tok (is 'a')) "a bc"
-- Result >bc< 'a'
--
-- >>> parse (tok (is 'a')) "abc"
-- Result >bc< 'a'
tok ::
  Parser a
  -> Parser a
tok p =
  do v <- p
     spaces
     pure v

-- | Write a function that parses the given char followed by 0 or more spaces.
--
-- /Tip:/ Use `tok` and `is`.
charTok ::
  Char
  -> Parser Char
charTok =
  tok . is

-- | Write a parser that parses a comma ',' followed by 0 or more spaces.
--
-- /Tip:/ Use `charTok`.
commaTok ::
  Parser Char
commaTok =
  charTok ','

-- | Write a parser that parses either a double-quote or a single-quote.
--
-- /Tip:/ Use `is` and `|||`
--
-- >>> parse quote "'abc"
-- Result >abc< '\''
--
-- >>> parse quote "\"abc"
-- Result >abc< '"'
--
-- >>> isErrorResult (parse quote "abc")
-- True
quote ::
  Parser Char
quote =
  is '"' ||| is '\''

-- | Write a function that parses the given string (fails otherwise).
--
-- /Tip:/ Use `is` and `traverse`.
--
-- >>> parse (string "abc") "abcdef"
-- Result >def< "abc"
--
-- >>> isErrorResult (parse (string "abc") "bcdef")
-- True
string ::
  Chars
  -> Parser Chars
string =
  traverse is

-- | Write a function that parsers the given string, followed by 0 or more spaces.
--
-- /Tip:/ Use `tok` and `string`.
--
-- >>> parse (stringTok "abc") "abc  "
-- Result >< "abc"
--
-- >>> isErrorResult (parse (stringTok "abc") "bc  ")
-- True
stringTok ::
  Chars
  -> Parser Chars
stringTok =
  tok . string

-- | Write a function that tries the given parser, otherwise succeeds by producing the given value.
--
-- /Tip:/ Use `|||`.
--
-- >>> parse (option 'x' character) "abc"
-- Result >bc< 'a'
--
-- >>> parse (option 'x' character) ""
-- Result >< 'x'
option ::
  a
  -> Parser a
  -> Parser a
option a p =
  p ||| pure a

-- | Write a parser that parses 1 or more digits.
--
-- /Tip:/ Use `list1` and `digit`.
--
-- >>> parse digits1 "123"
-- Result >< "123"
--
-- >>> isErrorResult (parse digits1 "abc123")
-- True
digits1 ::
  Parser Chars
digits1 =
  list1 digit

-- | Write a function that parses one of the characters in the given string.
--
-- /Tip:/ Use `satisfy` and `elem`.
--
-- >>> parse (oneof "abc") "bcdef"
-- Result >cdef< 'b'
--
-- >>> isErrorResult (parse (oneof "abc") "def")
-- True
oneof ::
  Chars
  -> Parser Char
oneof s =
  satisfy (`elem` s)

-- | Write a function that parses any character, but fails if it is in the given string.
--
-- /Tip:/ Use `satisfy` and `notElem`.
--
-- >>> parse (noneof "bcd") "abc"
-- Result >bc< 'a'
--
-- >>> isErrorResult (parse (noneof "abcd") "abc")
-- True
noneof ::
  Chars
  -> Parser Char
noneof s =
  satisfy (`notElem` s)

-- | Write a function that applies the first parser, runs the third parser keeping the result,
-- then runs the second parser and produces the obtained result.
--
-- /Tip:/ Use the monad instance.
--
-- >>> parse (between (is '[') (is ']') character) "[a]"
-- Result >< 'a'
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc]")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "[abc")
-- True
--
-- >>> isErrorResult (parse (between (is '[') (is ']') character) "abc]")
-- True
between ::
  Parser o
  -> Parser c
  -> Parser a
  -> Parser a
between o c a =
  do o
     v <- a
     c
     pure v

-- | Write a function that applies the given parser in between the two given characters.
--
-- /Tip:/ Use `between` and `charTok`.
--
-- λ> parse (betweenCharTok '[' ']' character) "[a]"
-- Result >< 'a'
--
-- λ> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc]")
-- True
--
-- λ> isErrorResult (parse (betweenCharTok '[' ']' character) "[abc")
-- True
--
-- λ> isErrorResult (parse (betweenCharTok '[' ']' character) "abc]")
-- True
betweenCharTok ::
  Char
  -> Char
  -> Parser a
  -> Parser a
betweenCharTok a b =
  between (charTok a) (charTok b)

-- | Write a function that parses 4 hex digits and return the character value.
--
-- /Tip:/ Use `readHex`, `isHexDigit`, `replicate`, `satisfy`.
--
-- >>> parse hex "0010"
-- Result >< '\DLE'
--
-- >>> parse hex "0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hex "001")
-- True
--
-- >>> isErrorResult (parse hex "0axf")
-- True
hex ::
  Parser Char
hex =
  let hInt s = case readHex s of
                 Empty -> 0
                 Full n -> n
  in chr . hInt <$> thisMany 4 (satisfy isHexDigit) 

-- | Write a function that parses the character 'u' followed by 4 hex digits and return the character value.
--
-- /Tip:/ Use `is` and `hex`.
--
-- >>> parse hexu "u0010"
-- Result >< '\DLE'
--
-- >>> parse hexu "u0a1f"
-- Result >< '\2591'
--
-- >>> isErrorResult (parse hexu "0010")
-- True
--
-- >>> isErrorResult (parse hexu "u001")
-- True
--
-- >>> isErrorResult (parse hexu "u0axf")
-- True
hexu ::
  Parser Char
hexu =
  do is 'u'
     hex

-- | Write a function that produces a non-empty list of values coming off the given parser (which must succeed at least once),
-- separated by the second given parser.
--
-- /Tip:/ Use `list` and the monad instance.
--
-- >>> parse (sepby1 character (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby1 character (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby1 character (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
--
-- >>> isErrorResult (parse (sepby1 character (is ',')) "")
-- True
sepby1 ::
  Parser a
  -> Parser s
  -> Parser (List a)
sepby1 p s =
  do v <- p
     w <- list (s *> p)
     pure (v:.w)

-- | Write a function that produces a list of values coming off the given parser,
-- separated by the second given parser.
--
-- /Tip:/ Use `sepby1` and `|||`.
--
-- >>> parse (sepby character (is ',')) ""
-- Result >< ""
--
-- >>> parse (sepby character (is ',')) "a"
-- Result >< "a"
--
-- >>> parse (sepby character (is ',')) "a,b,c"
-- Result >< "abc"
--
-- >>> parse (sepby character (is ',')) "a,b,c,,def"
-- Result >def< "abc,"
sepby ::
  Parser a
  -> Parser s
  -> Parser (List a)
sepby p s =
  sepby1 p s ||| pure Nil

-- | Write a parser that asserts that there is no remaining input.
--
-- >>> parse eof ""
-- Result >< ()
--
-- >>> isErrorResult (parse eof "abc")
-- True
eof ::
  Parser ()
eof =
  P (\s -> case s of
             Nil -> Result Nil ()
             x -> ErrorResult (ExpectedEof x))

-- | Write a parser that produces a characer that satisfies all of the given predicates.
--
-- /Tip:/ Use `sequence` and @Data.List#and@.
--
-- >>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABC"
-- Result >BC< 'A'
--
-- >>> parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "ABc"
-- Result >Bc< 'A'
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "XBc")
-- True
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "")
-- True
--
-- >>> isErrorResult (parse (satisfyAll (isUpper :. (/= 'X') :. Nil)) "abc")
-- True
satisfyAll ::
  List (Char -> Bool)
  -> Parser Char
satisfyAll ps =
  satisfy (and  . sequence ps)

-- | Write a parser that produces a characer that satisfies any of the given predicates.
--
-- /Tip:/ Use `sequence` and @Data.List#or@.
--
-- >>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "abc"
-- Result >bc< 'a'
--
-- >>> parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "ABc"
-- Result >Bc< 'A'
--
-- >>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "XBc")
-- True
--
-- >>> isErrorResult (parse (satisfyAny (isLower :. (/= 'X') :. Nil)) "")
-- True
satisfyAny ::
  List (Char -> Bool)
  -> Parser Char
satisfyAny ps =
  satisfy (or  . sequence ps)

-- | Write a parser that parses between the two given characters, separated by a comma character ','.
--
-- /Tip:/ Use `betweenCharTok`, `sepby` and `charTok`.
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[a]"
-- Result >< "a"
--
-- >>> parse (betweenSepbyComma '[' ']' lower) "[]"
-- Result >< ""
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[A]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[abc]")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "[a")
-- True
--
-- >>> isErrorResult (parse (betweenSepbyComma '[' ']' lower) "a]")
-- True
betweenSepbyComma ::
  Char
  -> Char
  -> Parser a
  -> Parser (List a)
betweenSepbyComma a b g =
  betweenCharTok a b $ g `sepby` charTok ','
