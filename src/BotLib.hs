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

-- allLettersInIncorrectPositionAreInGuess :: GuessEval -> [Letter] -> Bool
-- Identify all letters in incorrect position from GuessEval
-- Remove all letters from guess that have matched to letters in correct position
-- iterate over letters in incorrect position and match them to remaining letters in guess
