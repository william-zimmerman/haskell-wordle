module BotLib
  ( isLogicalGuess
  ) where

import           Lib

isLogicalGuess :: GuessEval -> [Letter] -> Bool
-- isLogicalGuess [] _ = True -- Does this make sense?
isLogicalGuess = allCorrectLettersMatch

allCorrectLettersMatch :: GuessEval -> [Letter] -> Bool
allCorrectLettersMatch guessEval guess =
  let requiredLetters = map
        (\(LetterEval index char CorrectPosition) -> Letter index char)
        guessEval
  in  all (`elem` guess) requiredLetters

