module Lib
  ( guessFromString
  , getLettersInCorrectPosition
  , toLetters
  , Letter(..)
  , Guess(..)
  ) where

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
