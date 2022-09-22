module Main
  ( main
  ) where

import           Lib
import           System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Welcome to Worlde!"
  answer <- getAnswer
  putStrLn "Enter guesses one at a time; :q to quit"
  mainLoop answer

getAnswer :: IO String
getAnswer = do
  putStr "Enter the answer: "
  getLine

mainLoop :: Answer -> IO ()
mainLoop answer = do
  putStr "> "
  guess <- getLine
  if guess == ":q"
    then do
      return ()
    else do
      let evaluation = evaluateGuess answer guess
      if guessIsCorrect evaluation
        then do
          putStrLn "You solved Wordle!"
          return ()
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
