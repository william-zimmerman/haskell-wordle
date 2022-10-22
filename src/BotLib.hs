module BotLib
  ( isLogicalGuess
  ) where

import           Lib

isLogicalGuess :: GuessEval -> Guess -> Bool
-- isLogicalGuess [] _ = True -- Does this make sense?
isLogicalGuess eval guess =
  allCorrectLettersMatch eval guess
    && allLettersInIncorrectPositionAreInGuess eval guess

filterOutCorrectLetters :: GuessEval -> Guess -> (Bool, Guess)
filterOutCorrectLetters guessEval guess =
  let
    lettersInCorrectPosition =
      [ Letter index char | (CorrectPosition index char) <- guessEval ]
    guessWithoutCorrectLetters = guess `without` lettersInCorrectPosition
    allLettersInCorrectPositionMatch =
      all (`elem` guess) lettersInCorrectPosition
  in
    (allLettersInCorrectPositionMatch, guessWithoutCorrectLetters)

-- filterOutLettersInIncorrectPosition :: GuessEval -> Guess -> (Bool, Guess)
-- filterOutLettersInIncorrectPosition guessEval guessWithoutCorrectLetters = 
--   let
--     lettersInIncorrectPosition = [ Letter index char | (IncorrectPosition index char) <- guessEval ]
    

-- firstWithSameCharDifferentIndex :: Letter -> [Letter] -> Maybe Letter
-- firstWithSameCharDifferentIndex _ [] -> Nothing
-- firstWithSameCharDifferentIndex (Letter targetIndex targetChar) (Letter(index char):xs)
--   |  

allCorrectLettersMatch :: GuessEval -> Guess -> Bool
allCorrectLettersMatch guessEval guess =
  let requiredLetters =
        [ Letter index char | (CorrectPosition index char) <- guessEval ]
  in  all (`elem` guess) requiredLetters

allLettersInIncorrectPositionAreInGuess :: GuessEval -> Guess -> Bool
allLettersInIncorrectPositionAreInGuess guessEval guess =
  let lettersInCorrectPosition =
        [ Letter index char | (CorrectPosition index char) <- guessEval ]
      guessWithoutCorrectLetters = guess `without` lettersInCorrectPosition
      lettersInIncorrectPosition =
        [ Letter index char | (IncorrectPosition index char) <- guessEval ]
  in  incorrectPositionsMatch lettersInIncorrectPosition
                              guessWithoutCorrectLetters

incorrectPositionsMatch :: [Letter] -> Guess -> Bool
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
