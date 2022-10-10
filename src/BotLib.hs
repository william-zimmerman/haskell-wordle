module BotLib
  ( isLogicalGuess
  ) where

import           Lib

isLogicalGuess :: GuessEval -> [Letter] -> Bool
-- isLogicalGuess [] _ = True -- Does this make sense?
isLogicalGuess eval guess =
  allCorrectLettersMatch eval guess
    && allLettersInIncorrectPositionAreInGuess eval guess

allCorrectLettersMatch :: GuessEval -> [Letter] -> Bool
allCorrectLettersMatch guessEval guess =
  let requiredLetters =
        [ Letter index char | (CorrectPosition index char) <- guessEval ]
  in  all (`elem` guess) requiredLetters

allLettersInIncorrectPositionAreInGuess :: GuessEval -> [Letter] -> Bool
allLettersInIncorrectPositionAreInGuess guessEval guess =
  let lettersInCorrectPosition =
        [ Letter index char | (CorrectPosition index char) <- guessEval ]
      guessWithoutCorrectLetters = guess `without` lettersInCorrectPosition
      lettersInIncorrectPosition =
        [ Letter index char | (IncorrectPosition index char) <- guessEval ]
  in  incorrectPositionsMatch lettersInIncorrectPosition
                              guessWithoutCorrectLetters

incorrectPositionsMatch :: [Letter] -> [Letter] -> Bool
incorrectPositionsMatch [] _  = True
incorrectPositionsMatch _  [] = False
incorrectPositionsMatch (evalLetter@(Letter _ char) : xs) guess =
  evalLetter `existsAtDifferentIndex` guess && incorrectPositionsMatch
    xs
    (popFirst char guess)
 where
  existsAtDifferentIndex _ [] = False
  existsAtDifferentIndex targetLetter@(Letter evalIndex evalChar) ((Letter guessIndex guessChar) : ys)
    | (evalChar == guessChar) && (evalIndex /= guessIndex)
    = True
    | otherwise
    = existsAtDifferentIndex targetLetter ys
  popFirst _ [] = []
  popFirst targetChar ((Letter _ c) : ys) | targetChar == c = ys
                                          | otherwise = popFirst targetChar ys
