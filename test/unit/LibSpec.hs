module LibSpec
  ( spec
  ) where

import           Lib
import           Test.Hspec

spec :: Spec
spec = do
  describe "toLetters" $ do
    it "should return empty list with empty string" $ do
      toLetters "" `shouldBe` []

    it "should return Letters in order with actual word" $ do
      toLetters "cat" `shouldBe` [Letter 1 'c', Letter 2 'a', Letter 3 't']

  describe "getLettersInCorrectPosition" $ do
    it "should return empty list with no letters in correct position" $ do
      getLettersInCorrectPosition (toLetters "cat") (toLetters "dog")
        `shouldBe` []

    it "should return single letter when one letter matches" $ do
      getLettersInCorrectPosition (toLetters "apple") (toLetters "stare")
        `shouldBe` [Letter 5 'e']

    it "should return multple letters when more than one letter matches" $ do
      getLettersInCorrectPosition (toLetters "grape") (toLetters "grown")
        `shouldBe` [Letter 1 'g', Letter 2 'r']

    it "should return entire word when guess equals answer" $ do
      getLettersInCorrectPosition (toLetters "peach") (toLetters "peach")
        `shouldBe` toLetters "peach"

-- TODO: QuickCheck test that when guess = answer, result = guess

  describe "without" $ do
    it "should return an empty list when given an empty list" $ do
      [] `without` [Letter 1 'w'] `shouldBe` []

    it "should return source when looking for empty list" $ do
      [Letter 1 'a'] `without` [] `shouldBe` [Letter 1 'a']

    it "should remove the letter if present in the list" $ do
      [Letter 5 'a', Letter 3 'b']
        `without`  [Letter 3 'b']
        `shouldBe` [Letter 5 'a']

    it "shouldn't make any changes to list if element is not present" $ do
      [Letter 1 'a', Letter 2 'b']
        `without`  [Letter 2 'a']
        `shouldBe` [Letter 1 'a', Letter 2 'b']

    it "should remove all letters from source" $ do
      (         [Letter 1 'a', Letter 2 'b', Letter 3 'c']
        `without` [Letter 1 'a', Letter 3 'c']
        )
        `shouldBe` [Letter 2 'b']

    it "should remove all letters if source is subset of values to remove" $ do
      (         [Letter 1 'a', Letter 2 'b']
        `without` [Letter 1 'a', Letter 2 'b', Letter 3 'c']
        )
        `shouldBe` []

  describe "getLettersInIncorrectPosition" $ do
    it "should return an empty list with empty list inputs" $ do
      getLettersInIncorrectPosition [] [] `shouldBe` []

    it "should return an empty list when there are no letters to match on" $ do
      getLettersInIncorrectPosition [] [Letter 1 'a'] `shouldBe` []

    it
        "should return an empty list when we're not looking for any letters to match"
      $ do
          getLettersInIncorrectPosition [Letter 1 'a'] [] `shouldBe` []

    it "should return empty list when no characters match" $ do
      getLettersInIncorrectPosition [Letter 1 'a', Letter 2 'b']
                                    [Letter 1 'y', Letter 2 'z']
        `shouldBe` []

    it "should return letter with matching character but different index" $ do
      getLettersInIncorrectPosition [Letter 1 'a'] [Letter 2 'a']
        `shouldBe` [Letter 2 'a']

    it "should not return letter when character and index both match" $ do
      getLettersInIncorrectPosition [Letter 1 'a'] [Letter 1 'a'] `shouldBe` []

    it
        "should only return one letter in incorrect position when there are repeating letters in guess"
      $ do
          getLettersInIncorrectPosition [Letter 1 'a', Letter 2 'b']
                                        [Letter 2 'a', Letter 3 'a']
            `shouldBe` [Letter 2 'a']

    it
        "should return all letters in incorrect position where there are more than one"
      $ do
          getLettersInIncorrectPosition
              [Letter 1 'a', Letter 2 'b', Letter 3 'c']
              [Letter 1 'c', Letter 2 'a', Letter 3 'b']
            `shouldBe` [Letter 1 'c', Letter 2 'a', Letter 3 'b']

    it
        "should return repeating characters in the wrong position when there are more than one in the answer"
      $ do
          getLettersInIncorrectPosition [Letter 1 'a', Letter 2 'a']
                                        [Letter 3 'a', Letter 4 'a']
            `shouldBe` [Letter 3 'a', Letter 4 'a']

  describe "makeGuess" $ do
    it
        "should return list with letters not in answer when guess does not overlap with answer"
      $ do
          makeGuess "bat" "hop"
            `shouldBe` [ LetterEval 1 'h' NotInAnswer
                       , LetterEval 2 'o' NotInAnswer
                       , LetterEval 3 'p' NotInAnswer
                       ]

    it
        "should return list with letters in correct place when guess equals answer"
      $ do
          makeGuess "william" "william"
            `shouldBe` [ LetterEval 1 'w' CorrectPosition
                       , LetterEval 2 'i' CorrectPosition
                       , LetterEval 3 'l' CorrectPosition
                       , LetterEval 4 'l' CorrectPosition
                       , LetterEval 5 'i' CorrectPosition
                       , LetterEval 6 'a' CorrectPosition
                       , LetterEval 7 'm' CorrectPosition
                       ]

    it "should return list of letter evals in order of their index" $ do
      makeGuess "spend" "waxed"
        `shouldBe` [ LetterEval 1 'w' NotInAnswer
                   , LetterEval 2 'a' NotInAnswer
                   , LetterEval 3 'x' NotInAnswer
                   , LetterEval 4 'e' IncorrectPosition
                   , LetterEval 5 'd' CorrectPosition
                   ]
