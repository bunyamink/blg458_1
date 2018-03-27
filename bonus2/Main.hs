-- Start bonus2

import Data.Char

data Color = Red | Black
  deriving (Show, Eq)
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Show, Eq)
data Rank = Num Int | Jack | Queen | King | Ace
  deriving (Show, Eq)
data Card = Card { suit :: Suit, rank :: Rank }
  deriving (Show, Eq)
data Move = Draw | Discard Card
  deriving (Show, Eq)

cardColor :: Card -> Color
cardColor (Card {suit = s, rank = r})
  | s == Clubs || s == Spades = Black
  | otherwise = Black

cardValue :: Card -> Integer
cardValue (Card {suit = s, rank = r})
  | r == Ace = 10
  | otherwise = 11
  
removeCard :: [Card] -> Card -> [Card]
removeCard [] _ = []
removeCard cs@(y:ys) c
  | y == c = removeCard ys c
  | otherwise = y : removeCard ys c

allSameColor :: [Card] -> Bool
allSameColor [] = True
allSameColor (c:cs)
  | c /= head cs = False
  | otherwise = allSameColor cs
  
sumCards :: [Card] -> Integer
sumCards cs = helper cs 0
  where 
    helper :: [Card] -> Integer -> Integer
    helper [] acc = acc
    helper (c:cs) acc = helper cs (acc + cardValue c)

score :: [Card] -> Integer -> Integer
score cs g
  | allSameColor cs == True = quot ((sumCards cs) - g) 2
  | sumCards cs >= g = 3 * ((sumCards cs) - g)
  | otherwise = ((sumCards cs) - g)

data State = State {cardList :: [Card], heldCard :: [Card], goal :: Integer}

runGame :: [Card] -> [Move] -> Integer -> Integer
runGame = undefined

-- Part 2 --

convertSuit :: Char -> Suit
convertSuit c 
  | c == 'd' || c == 'D' = Diamonds
  | c == 'c' || c == 'C' = Clubs
  | c == 'h' || c == 'H' = Hearts
  | c == 's' || c == 's' = Spades
  | otherwise = error "There is no Suit starting your chracter"
  
convertRank :: Char -> Rank
convertRank c
  | c == 'a' || c == 'A' = Ace
  | c == 'k' || c == 'K' = King
  | c == 'q' || c == 'Q' = Queen
  | c == 'j' || c == 'J' = Jack
  | isDigit c == True = Num (digitToInt c)
  | otherwise = error "Wrong chracter"

convertCard :: Char -> Char -> Card
convertCard s r = (Card {suit = (convertSuit s), rank = (convertRank r)})

-- Card listesi döndürülecek
readCards :: IO ()--[Card]
readCards = do line <- getLine
               if line == "."
                   then return () --return ([(convertCard 'd' 'a')])
                   else readCards --(convertCard 'd' 'a') : readCards

convertMove :: Char -> Char -> Char -> Move
convertMove m s r
  | m == 'm' || m == 'M' = Draw
  | m == 'r' || m == 'R' = Discard (Card {suit = (convertSuit s), rank = (convertRank r)})
  
-- Move listesi döndürülecek
readMoves :: IO () --[Move]
readMoves = do line <- getLine
               if line == "."
                    then return () --return ([(convertMove 'm' 'c' 'k')])
                    else readMoves --(convertMove 'm' 'c' 'k') : readMoves

main :: IO ()
main = do putStrLn "Enter cards:"
          --cards <- readCards
          -- putStrLn (show cards)
          putStrLn "Enter moves:"
          --moves <- readMoves
          -- putStrLn (show moves)
          putStrLn "Enter goal:"
          --line <- getLine
          --let goal = read line :: Integer
          --let score = runGame cards moves goal
          --putStrLn ("Score: " ++ show score)