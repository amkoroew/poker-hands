module Main where

import Data.List ((\\), isInfixOf, lookup, nub, sort, sortBy)
import Data.Maybe (fromJust)
import System.Environment (getArgs)

data Rank       = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Bounded, Enum, Eq, Ord)
data Suit       = Spades | Hearts | Diamonds | Clubs deriving (Enum, Eq)
data Card       = Card {rank :: Rank, suit :: Suit}
data HandType   = HighCard | Pair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush deriving (Eq, Ord)
data PokerHand  = PokerHand {handType :: HandType, kickers :: [Rank]} deriving (Eq, Ord)
data Color      = Hero | Villain
data Player     = Player {color :: Color, cards :: [Card], pokerHand :: PokerHand}

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

occurrencesOfRanks :: [Rank] -> [(Rank, Int)]
occurrencesOfRanks ranks = nub [(r, occurrencesOfRank r) | r <- ranks]
    where occurrencesOfRank r = length $ filter (== r) ranks
   
calculatePokerHand :: [Card] -> PokerHand
calculatePokerHand cards
    | isStraightFlush   = PokerHand StraightFlush straightKicker
    | isFlush           = PokerHand Flush kickers
    | isStraight        = PokerHand Straight straightKicker
    | otherwise         = PokerHand (fromJust $ lookup rankPattern rankPatterns) kickers
    where
      ranks = sort $ map rank cards
      suits  = map suit cards
      rankPattern = reverse . sort . map snd $ occurrencesOfRanks ranks
      rankPatterns = [([1,1,1,1,1], HighCard), ([2,1,1,1], Pair), ([2,2,1], TwoPair), ([3,1,1], ThreeOfAKind), ([3,2], FullHouse), ([4,1], FourOfAKind)]
      straightRanks = [minBound .. maxBound] ++ [Two, Three, Four, Five, Ace]
      kickers = [r | (r, _) <- sortBy compareKickers $ occurrencesOfRanks ranks]
      straightKicker = if elem Ace kickers && elem Two kickers then [Five] else [head kickers]
      isStraight = isInfixOf ranks straightRanks
      isFlush = all (== head suits) suits
      isStraightFlush = isFlush && isStraight
      

compareKickers :: (Rank, Int) -> (Rank, Int) -> Ordering
compareKickers (r1, o1) (r2, o2)
    | o1 < o2   = GT
    | o1 > o2   = LT
    | o1 == o2  = compare r2 r1
