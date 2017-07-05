{-# LANGUAGE TupleSections #-}
module Main where

import Text.Read (readMaybe)
import System.Exit
import Leitner
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Function
import Control.Applicative (Alternative (..))
import Data.Maybe

data Error = NoSeparator Int
           | NoNumber    Int

instance Show Error where
  show (NoSeparator n) = "No semicolon seperator on line " ++ show n
  show (NoNumber n) = "Line " ++ show n ++ " should contain a number, but it doesn't"

data Config = New String | Resume String | Empty

config :: Parser Config
config = New    <$> strOption (long "cards"    <> short 'c' <> metavar "FILENAME" <> help "Cards file")
     <|> Resume <$> strOption (long "progress" <> short 'p' <> metavar "FILENAME" <> help "Progress file")

opts :: ParserInfo Config
opts = info (config <**> helper)
  (  fullDesc
  <> progDesc "Flashcards based learning based on the Leitner method"
  <> header "leitner - command line learning"
  )

main :: IO ()
main = do
  options <- execParser opts
  eitherProgress <- 
    case options of 
      New cardsFile       -> parseCards    <$> readFile cardsFile
      Resume progressFile -> parseProgress <$> readFile progressFile
  case eitherProgress of
    Left e -> putStrLn (show e) >> exitFailure
    Right (sessionNumber, decks) -> do
      newDecks <- session simple sessionNumber decks
      writeFile "progress" (show (sessionNumber + 1) ++ "\n" ++ concat (map (\(a,b) -> show a ++ "\n" ++ unlines (map show b)) (zip [1..] newDecks)))

parseCards :: String -> Either Error (Int,[Deck])
parseCards str = (\a -> (1,[a])) <$> traverse parseCard (zip [1..] (lines str))

parseCard :: (Int,String) -> Either Error Card
parseCard (n,str)
  | ';' `elem` str = Right $ let (a,b) = span (/= ';') str in Card a (tail b)
  | otherwise = Left (NoSeparator n)

parseProgress :: String -> Either Error (Int, [Deck])
parseProgress str = let (session:rest) = lines str in case readMaybe session of
  Just n -> case parseDecks (zip [2..] rest) of
    Right decks -> Right (n, makeBoxes decks)
    Left e -> Left e
  Nothing -> Left (NoNumber 1)

parseDecks :: [(Int, String)] -> Either Error [(Int,Deck)]
parseDecks [] = Right []
parseDecks ((line,x):xs) = case readMaybe x of 
  Nothing -> Left (NoNumber line)
  Just n -> let (a,b) = span (isNothing . (readMaybe :: String -> Maybe Int) . snd) xs 
    in traverse parseCard a >>= \cards -> parseDecks b >>= \decks -> Right $ (n,cards) : decks
      -- Left e -> Left e
      -- Right cards -> case parseDecks b of
      --   Left e -> Left e
      --   Right decks -> Right $ (n,cards) : decks
