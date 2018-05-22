-- Bünyamin KURT Functional Programming Term Project
-- 150140145
-- empty, insert, insertList and search functions done. getWords and prefix could not implemented.
import qualified Data.Map as M 
import Data.Maybe
import System.Environment
import System.IO
import Prelude hiding (Word,empty)

-- Define Trie
data Trie = Trie {end :: Bool, children :: M.Map Char Trie} deriving (Show)
type Word = String

-- Create empty trie 
empty :: Trie
empty = Trie {end=False, children=M.empty}

-- insert a word into a Trie
-- First check if trie have first character of word. If have then continue with children trie
-- If not add new children 
insert :: Word -> Trie -> Trie
insert []     trie@(Trie _ m)    = Trie {end = True, children = m }
insert (x:xs) trie@(Trie end m)  = case M.lookup x m of
                                        Nothing -> trie {children=M.insert x (insert xs childNode) newChildren}
                                        Just t  -> trie {children=M.insert x (insert xs t) m}
                                        where
                                          childNode = empty
                                          newChildren = M.insert x childNode m

-- insert all words to trie start with empty trie
insertList :: [Word] -> Trie
insertList xs = foldr insert empty xs

-- search char by char if trie's children have first character of word, then continue children of trie
-- when last character come control if this is end or not. If end is true then return true 
search :: Word -> Trie -> Bool
search [x]   trie@(Trie _ m) = case M.lookup x m of
                                     Nothing -> False
                                     Just t  -> if (end t) == True then True else False
search (x:xs) trie@(Trie _ m) = case M.lookup x m of
                                     Nothing -> False
                                     Just t  -> search xs t

getWords :: Trie -> [Word]
getWords trie@(Trie e m)= getWords' (M.toList m) []
  where
    --getWords' :: [] -> [] -> [Words]
    getWords' (x:xs) w = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix [x]     trie@(Trie e _)     = Just ["s"] 
prefix (x:xs)  trie@(Trie e _)     = case M.lookup x (children trie) of
                                          Nothing -> Nothing
                                          Just t' -> prefix xs t'

-- print main menu										  
printMenu :: IO ()
printMenu = do putStrLn "a) Add word"
               putStrLn "s) Search word"
               putStrLn "f) Find words with prefix"
               putStrLn "p) print all words"
               putStrLn "e) exit"
               putStrLn "Enter Action: "

-- take a trie 
-- print menu, get character from user
-- according to character do functiıns and call again mainLoop
-- if user enter "e" exit mainLoop
mainLoop :: Trie -> IO ()
mainLoop t = do printMenu
                selected <- getLine
                case selected of 
                     "a" -> do putStrLn "Enter Word/Prefix:"
                               wrd <- getLine
                               let x = insert wrd t
                               putStrLn "New word is added!"
                               mainLoop x
                     "s" -> do putStrLn "Enter Word/Prefix:"
                               wrd <- getLine
                               if search wrd t then putStrLn "Exists in vocabulary" else putStrLn "NOT Exists!"
                               mainLoop t
                     "f" -> do putStrLn "prefix did not implemented"
                               mainLoop t
                     "p" -> do print t
                               mainLoop t
                     "e" -> do return ()

-- main function get arguments from command line
-- find fileName from command line
-- read file and create list of words (singlewords)
-- insert all words into to Trie (mainTrie)
-- go mainLoop
main = do args <- getArgs
          let fileName = head args
          handle <- openFile fileName ReadMode
          contents <- hGetContents handle
          let singlewords = words contents
          let mainTrie = insertList singlewords
          mainLoop mainTrie
          