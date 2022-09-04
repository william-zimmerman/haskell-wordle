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

import           Data.List

data Status = NotInAnswer | IncorrectPosition | CorrectPosition
  deriving(Eq, Show)
type Position = Int
data LetterEval = LetterEval Char Status Position
  deriving (Eq, Show)
data Letter = Letter Char Int
  deriving (Eq, Show)

data WordEval = WordEval LetterEval
                         LetterEval
                         LetterEval
                         LetterEval
                         LetterEval

getLetterEvals :: WordEval -> [LetterEval]
getLetterEvals (WordEval one two three four five) =
  [one, two, three, four, five]

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

guessIntoCharAndPosition :: Guess -> [(Char, Int)]
guessIntoCharAndPosition (Guess a b c d e) = zip [a, b, c, d, e] [1 .. 5]

guessIntoLetters :: Guess -> [Letter]
guessIntoLetters guess =
  [ Letter character position
  | (character, position) <- guessIntoCharAndPosition guess
  ]

toLetters :: String -> [Letter] -- TODO: It feels like this should be a set?
toLetters s =
  map (\(character, index) -> Letter character index) (zip s [1 .. (length s)])

getLettersInCorrectPosition :: [Letter] -> [Letter] -> [Letter] -- TODO: Introduce Guess and Answer types here
getLettersInCorrectPosition answer guess =
  [ guessLetter
  | answerLetter <- answer
  , guessLetter  <- guess
  , answerLetter == guessLetter
  ]

without :: [Letter] -> [Letter] -> [Letter]
without = foldl withoutElem
 where
  withoutElem [] _ = []
  withoutElem (x : xs) value | x == value = xs
                             | otherwise  = x : (xs `without` [value])

getLettersInIncorrectPosition :: [Letter] -> [Letter] -> [Letter]
getLettersInIncorrectPosition _ [] = []
getLettersInIncorrectPosition answer ((Letter guessChar guessIndex) : xs) =
  maybe
    (getLettersInIncorrectPosition answer xs)
    (\answerLetter ->
      Letter guessChar guessIndex
        : getLettersInIncorrectPosition (answer `without` [answerLetter]) xs
    )
    (find
      (\(Letter answerChar answerIndex) ->
        guessChar == answerChar && guessIndex /= answerIndex
      )
      answer
    )

makeGuess :: String -> String -> [LetterEval]
makeGuess answer guess =
  map (\(Letter char index) -> LetterEval char CorrectPosition index)
      lettersInCorrectPosition
    ++ map (\(Letter char index) -> LetterEval char IncorrectPosition index)
           lettersInIncorrectPosition
    ++ map (\(Letter char index) -> LetterEval char NotInAnswer index)
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
