module BotLib
  ( isLogicalGuess
  ) where

import           Lib

isLogicalGuess :: GuessEval -> [Letter] -> Bool
-- isLogicalGuess [] _ = True -- Does this make sense?
isLogicalGuess = allCorrectLettersMatch

allCorrectLettersMatch :: GuessEval -> [Letter] -> Bool
allCorrectLettersMatch guessEval guess =
  let correctLetters = filter
        (\(LetterEval _ _ status) -> status == CorrectPosition)
        guessEval
  in  all
        (\(LetterEval index char _) ->
          guess `letterAtIndex` index == Just (Letter index char)
        )
        correctLetters

letterAtIndex :: [Letter] -> Int -> Maybe Letter
letterAtIndex [] _ = Nothing
letterAtIndex (letter@(Letter i _) : xs) index
  | index == i = Just letter
  | otherwise  = letterAtIndex xs index
