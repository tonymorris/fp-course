{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams name =
    (elems . S.intersection (fromList (permutations name)) . fromList . lines <$>) . readFile
  where
    fromList = S.fromList . hlist . map NoCaseString
    elems = map ncString . listh . S.elems

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Ord NoCaseString where
  compare =
    compare `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
