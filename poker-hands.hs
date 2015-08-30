module Main where

import Data.List ((\\), lookup, nub, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import System.Environment (getArgs)

data Rank       = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Bounded, Enum, Eq, Ord)
data Suit       = Spades | Hearts | Diamonds | Clubs deriving (Bounded, Enum, Eq)
data Card       = Card {rank :: Rank, suit :: Suit}
data HandType   = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush deriving (Eq, Ord)
data PokerHand  = PokerHand {handType :: HandType, kickers :: [Rank]} deriving (Eq, Ord, Show)
data Color      = Hero | Villain deriving (Show, Read)
data Player     = Player {color :: Color, cards :: [Card], pokerHand :: PokerHand} deriving (Show)

instance Show Rank where
  show Ten      = "T"
  show Jack     = "J"
  show Queen    = "Q"
  show King     = "K"
  show Ace      = "A"
  show v        = show $ (2 +) $ fromEnum v
  
instance Read Rank where
  readsPrec _ "T"   = [(Ten, "")]
  readsPrec _ "J"   = [(Jack, "")]
  readsPrec _ "Q"   = [(Queen, "")]
  readsPrec _ "K"   = [(King, "")]
  readsPrec _ "A"   = [(Ace, "")]
  readsPrec _ v     = [(toEnum ((read v :: Int)-2), "")]

instance Show Suit where
  show Spades   = "S"
  show Hearts   = "H"
  show Diamonds = "D"
  show Clubs    = "C"
  
instance Read Suit where
  readsPrec _ "S"   = [(Spades, "")]
  readsPrec _ "H"   = [(Hearts, "")]
  readsPrec _ "D"   = [(Diamonds, "")]
  readsPrec _ "C"   = [(Clubs, "")]
    
instance Show Card where
  show (Card rank suit) = show rank ++ show suit
  
instance Read Card where
  readsPrec _ (r:s) = [((Card (read [r]) (read s)), "")]
  
instance Show HandType where
  show HighCard         = "high card"
  show Pair             = "pair"
  show TwoPair          = "two pair"
  show ThreeOfAKind     = "three of a kind"
  show Straight         = "straight"
  show Flush            = "flush"
  show FullHouse        = "full house"
  show FourOfAKind      = "four of a kind"
  show StraightFlush    = "straight flush"
  
main :: IO ()
main = do
    (_:h1:h2:h3:h4:h5:_:villainCards) <- getArgs
    let hero = Player Hero (map read [h1,h2,h3,h4,h5]) (calculatePokerHand (map read [h1,h2,h3,h4,h5]))
    let villain = Player Villain (map read villainCards) (calculatePokerHand (map read villainCards))
    putStrLn $ case compare (pokerHand hero) (pokerHand villain) of
                    EQ -> "Tie."
                    GT -> "Hero wins. - With " ++ show (handType (pokerHand hero)) ++ if (handType (pokerHand hero)) == (handType (pokerHand villain)) then ": " ++ (show $ head $ (kickers (pokerHand hero)) \\ (kickers (pokerHand villain))) else ""
                    LT -> "Villain wins. - With " ++ show (handType (pokerHand villain)) ++ if (handType (pokerHand hero)) == (handType (pokerHand villain)) then ": " ++ (show $ head $ (kickers (pokerHand villain)) \\ (kickers (pokerHand hero))) else ""

groupCardsByRank :: [Card] -> [(Rank, Int)]
groupCardsByRank cards = nub [(r, occurrencesOfRank r) | c <- cards, let r = rank c]
    where occurrencesOfRank r = length $ filter (\c -> (rank c) == r) cards
  
isPair :: [Card] -> Bool
isPair = ([2,1,1,1] ==) . reverse . sort . map snd . groupCardsByRank

isThreeOfAKind :: [Card] -> Bool
isThreeOfAKind = ([3,1,1] ==) . reverse . sort . map snd . groupCardsByRank

isTwoPair :: [Card] -> Bool
isTwoPair = ([2,2,1] ==) . reverse . sort . map snd . groupCardsByRank

isStraight :: [Card] -> Bool
isStraight cards = elem ranks straights
    where
      ranks =  sort [rank c | c <- cards]
      straights = map sort $ ([Five, Four .. Two] ++ [Ace]) : (map (reverse . sort) $ [take 5 [minRank ..] | minRank <- [Two .. Ten]])

isFlush :: [Card] -> Bool
isFlush cards = all (== head suits) suits
    where suits = map suit cards

isFullHouse :: [Card] -> Bool
isFullHouse = ([3,2] ==) . reverse . sort . map snd . groupCardsByRank

isFourOfAKind :: [Card] -> Bool
isFourOfAKind = ([4,1] ==) . reverse . sort . map snd . groupCardsByRank

isStraightFlush :: [Card] -> Bool
isStraightFlush cards = isStraight cards && isFlush cards

calculatePokerHand :: [Card] -> PokerHand
calculatePokerHand cards
    | isStraightFlush cards = PokerHand StraightFlush straightKicker
    | isFourOfAKind cards   = PokerHand FourOfAKind ranks
    | isFullHouse cards     = PokerHand FullHouse ranks
    | isFlush cards         = PokerHand Flush ranks
    | isStraight cards      = PokerHand Straight straightKicker
    | isTwoPair cards       = PokerHand TwoPair ranks
    | isThreeOfAKind cards  = PokerHand ThreeOfAKind ranks
    | isPair cards          = PokerHand Pair ranks
    | otherwise             = PokerHand HighCard ranks
    where
      ranks = [r | (r, _) <- sortBy compareKickers $ groupCardsByRank cards]
      straightKicker = if elem Ace ranks && elem Two ranks then [Five] else [head ranks]

compareKickers :: (Rank, Int) -> (Rank, Int) -> Ordering
compareKickers (r1, o1) (r2, o2)
    | o1 < o2   = GT
    | o1 > o2   = LT
    | o1 == o2  = compare r2 r1
