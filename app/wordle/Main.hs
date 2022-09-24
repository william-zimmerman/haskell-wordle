module Main
  ( main
  ) where

import           Lib
import           System.IO
import           System.Random

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Worlde!"
  answer <- getAnswerFromFile
  putStrLn "Enter guesses one at a time; :q to quit"
  mainLoop answer

getAnswerFromFile :: IO String
getAnswerFromFile = do
  fileContents <- readFile "words.txt"
  let allPotentialAnswers = lines fileContents
  getRandomElement allPotentialAnswers

mainLoop :: Answer -> IO ()
mainLoop answer = do
  putStr "> "
  guess <- getLine
  if guess == ":q"
    then do
      putStrLn ("The word was '" ++ answer ++ "'")
    else
      let evaluation = evaluateGuess answer guess
      in  if guessIsCorrect evaluation
            then do
              putStrLn "You solved Wordle!"
            else do
              putStrLn (formatEval evaluation)
              mainLoop answer

formatEval :: [LetterEval] -> String
formatEval = concatMap
  (\(LetterEval _ char status) -> case status of
    NotInAnswer       -> ['<', char, '>']
    IncorrectPosition -> ['{', char, '}']
    CorrectPosition   -> ['[', char, ']']
  )

getRandomElement :: [a] -> IO a
getRandomElement xs = do
  index <- getStdRandom (randomR (0, length xs))
  return (xs !! index)
