module Lib
  ( guessFromString
  , getLettersInCorrectPosition
  , toLetters
  , getLettersInIncorrectPosition
  , without
  , makeGuess
  , Letter(..)
  , Guess(..)
  , LetterEval(..)
  , Status(..)
  ) where

import           Data.List                      ( find
                                                , sort
                                                )

data Status = NotInAnswer | IncorrectPosition | CorrectPosition
  deriving(Eq, Show)

type Index = Int

data Letter = Letter Index Char
  deriving (Eq, Show)

data LetterEval = LetterEval Index Char Status
  deriving (Eq, Show)

instance Ord LetterEval where
  (<=) (LetterEval indexN _ _) (LetterEval indexM _ _) = indexN <= indexM

data Guess = Guess
  { firstLetter  :: Char
  , secondLetter :: Char
  , thirdLetter  :: Char
  , fourthLetter :: Char
  , fifthLetter  :: Char
  }
  deriving (Show, Eq)

guessFromString :: String -> Guess
guessFromString s = Guess (head s) (s !! 1) (s !! 2) (s !! 3) (s !! 4)

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
makeGuess :: String -> String -> [LetterEval]
makeGuess answer guess =
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
