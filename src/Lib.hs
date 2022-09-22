module Lib
  ( getLettersInCorrectPosition
  , toLetters
  , getLettersInIncorrectPosition
  , without
  , evaluateGuess
  , guessIsCorrect
  , Letter(..)
  , LetterEval(..)
  , Status(..)
  , Answer
  , Guess
  ) where

import           Data.List                      ( find
                                                , sort
                                                )

data Status = NotInAnswer | IncorrectPosition | CorrectPosition
  deriving(Eq, Show)

type Index = Int
type Answer = String
type Guess = String

data Letter = Letter Index Char
  deriving (Eq, Show)

data LetterEval = LetterEval Index Char Status
  deriving (Eq, Show)

instance Ord LetterEval where
  (<=) (LetterEval indexN _ _) (LetterEval indexM _ _) = indexN <= indexM

toLetters :: String -> [Letter] -- TODO: It feels like this should be a set?
toLetters s =
  map (\(character, index) -> Letter index character) (zip s [1 .. (length s)])

getLettersInCorrectPosition :: [Letter] -> [Letter] -> [Letter] -- TODO: Introduce Guess and Answer types here
getLettersInCorrectPosition answer guess =
  [ guessLetter
  | answerLetter <- answer
  , guessLetter  <- guess
  , answerLetter == guessLetter
  ]

-- Haskell wiki thinks the last parameter should be the one we're operating on - 
-- https://wiki.haskell.org/Parameter_order
without :: [Letter] -> [Letter] -> [Letter]
without = foldl withoutElem
 where
  withoutElem [] _ = []
  withoutElem (x : xs) value | -- This doesn't work when there are multiple, identical Letters in source list 
                               x == value = xs
                             | otherwise  = x : (xs `withoutElem` value)

getLettersInIncorrectPosition :: [Letter] -> [Letter] -> [Letter]
getLettersInIncorrectPosition _ [] = []
getLettersInIncorrectPosition answer ((Letter guessIndex guessChar) : xs) =
  maybe
    (getLettersInIncorrectPosition answer xs)
    (\answerLetter ->
      Letter guessIndex guessChar
        : getLettersInIncorrectPosition (answer `without` [answerLetter]) xs
    )
    (find
      (\(Letter answerIndex answerChar) ->
        guessChar == answerChar && guessIndex /= answerIndex
      )
      answer
    )

-- TODO: It feels weird that this is sorting the return value; 
-- ideally this should return a set that the caller can sort if needed
-- TODO: How are we going to handle case sensitivity
evaluateGuess :: Answer -> Guess -> [LetterEval]
evaluateGuess answer guess =
  sort
    $  map (\(Letter index char) -> LetterEval index char CorrectPosition)
           lettersInCorrectPosition
    ++ map (\(Letter index char) -> LetterEval index char IncorrectPosition)
           lettersInIncorrectPosition
    ++ map (\(Letter index char) -> LetterEval index char NotInAnswer)
           lettersNotInAnswer
 where
  answerLetters = toLetters answer
  guessLetters  = toLetters guess
  lettersInCorrectPosition =
    getLettersInCorrectPosition answerLetters guessLetters
  lettersInIncorrectPosition = getLettersInIncorrectPosition
    (answerLetters `without` lettersInCorrectPosition)
    (guessLetters `without` lettersInCorrectPosition)
  lettersNotInAnswer =
    (guessLetters `without` lettersInCorrectPosition)
      `without` lettersInIncorrectPosition

guessIsCorrect :: [LetterEval] -> Bool
guessIsCorrect [] = False
guessIsCorrect xs = all (\ (LetterEval _ _ status) -> status == CorrectPosition) xs
