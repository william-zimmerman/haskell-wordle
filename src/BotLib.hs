module BotLib
  ( isLogicalGuess
  ) where

import           Data.List                      ( delete,find )
import           Lib

isLogicalGuess :: GuessEval -> Guess -> Bool
isLogicalGuess guessEval guess =
  allCorrectPositionsMatchOnPosition guessEval guess
    && noIncorrectPositionsMatchOnPosition guessEval guess
  where
    remainingMatches = getMatches guessEval guess
    unmatchedEvals = map fst remainingMatches
    allUnmatchedEvalsAreNotInAnswer = all 
      (\case 
        (NotInAnswer _ _) -> True
        _ -> False
      ) unmatchedEvals
    

allCorrectPositionsMatchOnPosition :: GuessEval -> Guess -> Bool
allCorrectPositionsMatchOnPosition guessEval guess =
  let requiredLetters =
        [ Letter index char | (CorrectPosition index char) <- guessEval ]
  in  all (`elem` guess) requiredLetters

noIncorrectPositionsMatchOnPosition :: GuessEval -> Guess -> Bool
noIncorrectPositionsMatchOnPosition guessEval guess =
  let incorrectLetters =
        [ Letter index char | (IncorrectPosition index char) <- guessEval ]
  in  not $ any (`elem` guess) incorrectLetters

getMatches :: GuessEval -> Guess -> [(LetterEval, Letter)]
getMatches [] _ = []
getMatches _ [] = []
getMatches guessEval (x:xs) = 
  let maybeLetterEvalMatch = findMatchingEval guessEval x
  in case maybeLetterEvalMatch of
    Just match -> (match, x) : getMatches (delete match guessEval) xs
    Nothing -> getMatches guessEval xs

atIndex :: GuessEval -> Index -> Maybe LetterEval
atIndex [] _ = Nothing
atIndex (x : xs) index | getIndex x == index = Just x
                       | otherwise           = xs `atIndex` index

-- TODO: Maybe this should be renamed findMatchingLetterInCorrectOrIncorrectPosition
findMatchingEval :: GuessEval -> Letter -> Maybe LetterEval
findMatchingEval [] _ = Nothing
findMatchingEval guessEval (Letter letterIndex letterChar) =
  let firstInIncorrectPosition char = find
        (\case
          (IncorrectPosition _ incorrectChar) -> char == incorrectChar
          _ -> False
        )
  in  case guessEval `atIndex` letterIndex of
        Just letterEval@(CorrectPosition _ correctChar)
          | correctChar == letterChar -> Just letterEval
        Just (IncorrectPosition _ _) ->
          firstInIncorrectPosition letterChar guessEval
        _ -> firstInIncorrectPosition letterChar guessEval
