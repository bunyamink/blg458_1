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
insertList = undefined

search :: Word -> Trie -> Bool
search = undefined

getWords :: Trie -> [Word]
getWords = undefined

prefix :: Word -> Trie -> Maybe [Word]
prefix = undefined

ss = do selected <- getLine
        case selected of "e" -> return "e"
                         "a" -> return "a"
                         "f" -> return "f"
                         "s" -> return "s"
                         "p" -> return "p"
        

printMenu = do putStrLn "a) Add word"
               putStrLn "s) Search word"
               putStrLn "f) Find words with prefix"
               putStrLn "p) print all words"
               putStrLn "e) exit"


main = do args <- getArgs
          let incomming = head args
          putStrLn incomming