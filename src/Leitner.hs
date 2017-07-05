{-# LANGUAGE TupleSections #-}
module Leitner where

import Control.Arrow ((***), first)
import Data.List (partition)

-- General functions

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `rem` b == 0

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a],[a])
partitionM f [] = pure ([],[])
partitionM f (x:xs) = do
  b <- f x
  (if b then \(a,b) -> (a,x:b) else \(a,b) -> (x:a,b)) <$> partitionM f xs

data Card = Card { question :: String, answer :: String }

instance Show Card where
  show (Card q a) = q ++ ";" ++ a

type Deck = [Card]

session :: (Card -> IO Bool) -> Int -> [Deck] -> IO [Deck]
session frontend sessionNumber boxes = do
  result <- sequence $ map (\(n,deck) -> fmap ((1,) *** (n+1,)) $ partitionM frontend deck) toReview
  return $ makeBoxes $ noReview ++ (result >>= \(a,b) -> [a,b])
  where
    (toReview,noReview) = partition ((divisibleBy sessionNumber . (^2)) . fst) $ zip [1..] boxes

makeBoxes :: [(Int,Deck)] -> [Deck]
makeBoxes = makeBoxes' 1
  where
    makeBoxes' _ [] = []
    makeBoxes' n xs = concatMap snd a : makeBoxes' (n+1) b
      where (a,b) = partition ((== n) . fst) xs

-- | The most simple frontend, it just asks the question on the command line and then gets the user input as an answer. Beware: Previous answers are visible.
simple :: Card -> IO Bool
simple (Card q a) = do
  putStrLn q
  ans <- getLine
  if ans == a
    then putStrLn "Correct!" >> return True
    else putStrLn ("Incorrect, the right answer is: " ++ show a) >> return False


