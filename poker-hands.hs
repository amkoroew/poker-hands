module Main where

import Data.List ((\\), isInfixOf, lookup, nub, sort, sortBy)
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
  
isStraight :: [Card] -> Bool
isStraight cards = isInfixOf (sort $ map rank cards) straightRanks
    where straightRanks = [minBound .. maxBound] ++ [Two, Three, Four, Five, Ace]

calculatePokerHand :: [Card] -> PokerHand
calculatePokerHand cards
    | isStraightFlush   = PokerHand StraightFlush straightKicker
    | isFlush           = PokerHand Flush ranks
    | isStraight cards  = PokerHand Straight straightKicker
    | otherwise         = PokerHand (fromJust $ lookup rankPattern rankPatterns) ranks
    where
      suits  = map suit cards
      rankPattern = reverse . sort . map snd $ groupCardsByRank cards
      rankPatterns = [([1,1,1,1,1], HighCard), ([2,1,1,1], Pair), ([2,2,1], TwoPair), ([3,1,1], ThreeOfAKind), ([3,2], FullHouse), ([4,1], FourOfAKind)]
      ranks = [r | (r, _) <- sortBy compareKickers $ groupCardsByRank cards]
      straightKicker = if elem Ace ranks && elem Two ranks then [Five] else [head ranks]
      isFlush = all (== head suits) suits
      isStraightFlush = isFlush && (isStraight cards)

compareKickers :: (Rank, Int) -> (Rank, Int) -> Ordering
compareKickers (r1, o1) (r2, o2)
    | o1 < o2   = GT
    | o1 > o2   = LT
    | o1 == o2  = compare r2 r1
