-- Start bonus2

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