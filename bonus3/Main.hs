-- Bunyamin Kurt - 150140145
-- FP Bonus 3

import Prelude hiding (Word)
import Data.Char
import Data.List

type Word = String
type Sentence = [Word]
type CharacterCount = [(Char,Int)]

wordCharCounts :: Word -> CharacterCount
wordCharCounts xs = wordCharCounts' (nub xs)
    where
        wordCharCounts' :: Word -> CharacterCount
        wordCharCounts' [] = []
        wordCharCounts' (y:ys) = (toLower y, length $ filter (== toLower y) (map toLower xs)) : wordCharCounts ys

sentenceCharCounts :: Sentence -> CharacterCount
sentenceCharCounts [] = []
sentenceCharCounts (x:xs) = wordCharCounts x ++ sentenceCharCounts xs

dictCharCounts :: Sentence -> [CharacterCount]
dictCharCounts xs = map wordCharCounts xs

--dictWordsByCharCounts :: [CharacterCount] -> [Word]
dictWordsByCharCounts = undefined

wordAnagrams :: Word -> [Word] -> [Word]
wordAnagrams = undefined

charCountsSubsets :: CharacterCount -> [CharacterCount]
charCountsSubsets = undefined