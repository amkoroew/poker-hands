module PokerHands where

import Data.List ((\\), lookup, nub, sort, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isNothing)
import Data.Ord (comparing)
import System.Environment (getArgs)

data Rank       = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Bounded, Enum, Eq, Ord, Show)
data Suit       = Spades | Hearts | Diamonds | Clubs deriving (Bounded, Enum, Eq, Show)
data Card       = Card {rank :: Rank, suit :: Suit} deriving (Show)
data HandType   = HighCard | Pair | ThreeOfAKind | TwoPair | Straight | Flush | FullHouse | FourOfAKind | StraightFlush deriving (Eq, Ord, Show)
data PokerHand  = PokerHand {handType :: HandType, kickers :: [Rank]} deriving (Eq, Ord, Show)
data Color      = White | Black deriving (Show, Read)
data Player     = Player {color :: Color, cards :: [Card], pokerHand :: PokerHand} deriving (Show)

main :: IO ()
main = do
    (_:b1:b2:b3:b4:b5:_:whiteCards) <- getArgs
    let black = Player Black (readCards [b1,b2,b3,b4,b5]) (calculatePokerHand (readCards [b1,b2,b3,b4,b5]))
    let white = Player White (readCards whiteCards) (calculatePokerHand (readCards whiteCards))
    putStrLn $ case compare (pokerHand white) (pokerHand black) of
                    EQ -> "Tie."
                    GT -> "White wins. - With " ++ show (handType (pokerHand white)) ++ if (handType (pokerHand white)) == (handType (pokerHand black)) then ": " ++ (show $ head $ (kickers (pokerHand white)) \\ (kickers (pokerHand black))) else ""
                    LT -> "Black wins. - With " ++ show (handType (pokerHand black)) ++ if (handType (pokerHand white)) == (handType (pokerHand black)) then ": " ++ (show $ head $ (kickers (pokerHand black)) \\ (kickers (pokerHand white))) else ""

readRank :: Char -> Rank
readRank r = fromJust $ lookup r ranks
    where ranks = [('2',Two),('3',Three),('4',Four),('5',Five),('6',Six),('7',Seven),('8',Eight),('9', Nine),('T', Ten),('J', Jack),('Q', Queen),('K', King),('A', Ace)]

readSuit :: Char -> Suit
readSuit s = fromJust $ lookup s suits
    where suits = [('S',Spades),('H',Hearts),('D',Diamonds),('C',Clubs)]

readCard :: String -> Card
readCard (r:s:[]) = Card (readRank r) (readSuit s)
    
readCards :: [String] -> [Card]
readCards = map readCard

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
isFlush cards = all (\c -> (suit c) == s) cards
    where s = (suit (head cards))

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
