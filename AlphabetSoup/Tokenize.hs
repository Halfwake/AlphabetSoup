module AlphabetSoup.Tokenize
( patternTokenizer
) where

import Data.Char

-- patternTokenizer (not . any isSpace) "A shame  this is \n\n far too\ngreedy\nto\ndo\nwhat\t\n  \nI\twant."

patternTokenizer :: (String -> Bool) -> String -> [String]
patternTokenizer pattern [] = []
patternTokenizer pattern text
  | null match && null text = []
  | null match              = patternTokenizer pattern $ drop 1 text
  | otherwise               = [match] ++ (patternTokenizer pattern newText)
  where match = patternGrow pattern text
        newText = drop (length match) text

patternGrow :: (String -> Bool) -> String -> String -> String
patternGrow _ [] accum = accum 
patternGrow pattern (c:cs) accum = if pattern newAccum
  then patternGrow pattern cs newAccum
  else accum
  where newAccum = accum ++ [c]
