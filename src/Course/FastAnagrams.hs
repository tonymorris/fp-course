{-# LANGUAGE NoImplicitPrelude #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Data.Char
import Data.Function
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Str
  -> Filename
  -> Str
fastAnagrams name f =
  (flip (filter . flip S.member) (permutations name) . S.fromList . lines) `fmap` readFile f

newtype NoCaseString =
  NoCaseString {
    ncString :: Str
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
