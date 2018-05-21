import qualified Data.Map as M --(can be shortened)
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word,empty)

data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show)
type Word = String

empty :: Trie
empty = Trie {end=False, children=M.empty}

insert :: Word -> Trie -> Trie
insert []     trie@(Trie _ m)    = Trie {end = True, children = m }
insert (x:xs) trie@(Trie end m)  = case M.lookup x ts of
                                        Nothing -> trie {children=M.insert x (insert xs childNode) newChildren}
                                        Just t  -> trie {children=M.insert x (insert xs t) ts}
                                        where
                                          ts = children trie
                                          childNode = empty
                                          newChildren = M.insert x childNode ts

insertList :: [Word] -> Trie
insertList xs = foldr insert empty xs

search :: Word -> Trie -> Bool
search []     trie@(Trie e _) = e
search (x:xs) trie@(Trie _ m) = fromMaybe False (search xs <$> M.lookup x m)

getWords :: Trie -> [Word]
getWords trie@(Trie e m)= getWords' (M.toList m) []
  where
    --getWords' :: [] -> [] -> [Words]
    getWords' (x:xs) w = undefined

d = insertList ["ali", "abla", "baba", "sade"]
ch = children d
ts = M.toList ch

prefix :: Word -> Trie -> Maybe [Word]
prefix [x]     trie@(Trie e _)     = Just ["s"] 
prefix (x:xs)  trie@(Trie e _)     = case M.lookup x (children trie) of
                                          Nothing -> Nothing
                                          Just t' -> prefix xs t'
        

printMenu = do putStrLn "a) Add word"
               putStrLn "s) Search word"
               putStrLn "f) Find words with prefix"
               putStrLn "p) print all words"
               putStrLn "e) exit"
               putStrLn "Enter Action: "


main = do args <- getArgs
          let fileName = head args
          handle <- openFile fileName ReadMode
          contents <- hGetContents handle
          let singlewords = words contents
          --print singlewords
          let mainTrie = insertList singlewords
          --let mainTrie = insertList $ words contents
          --print mainTrie
          printMenu
          selected <- getLine
          if selected == "a" then
            do putStrLn "Enter Word/Prefix:"
               wrd <- getLine
               let x = insert wrd mainTrie
               print x
            else if selected == "s" then
              do putStrLn "Enter Word/Prefix:"
                 wrd <- getLine
                 if search wrd mainTrie then putStrLn "Exists in vocabulary" else putStrLn "NOT Exists!"
                   else do putStrLn "sss"