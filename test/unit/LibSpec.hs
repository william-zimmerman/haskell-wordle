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
      toLetters "cat" `shouldBe` [Letter 'c' 1, Letter 'a' 2, Letter 't' 3]

  describe "getLettersInCorrectPosition" $ do
    it "should return empty list with no letters in correct position" $ do
      getLettersInCorrectPosition (toLetters "cat") (toLetters "dog")
        `shouldBe` []

    it "should return single letter when one letter matches" $ do
      getLettersInCorrectPosition (toLetters "apple") (toLetters "stare")
        `shouldBe` [Letter 'e' 5]

    it "should return multple letters when more than one letter matches" $ do
      getLettersInCorrectPosition (toLetters "grape") (toLetters "grown")
        `shouldBe` [Letter 'g' 1, Letter 'r' 2]

    it "should return entire word when guess equals answer" $ do
      getLettersInCorrectPosition (toLetters "peach") (toLetters "peach")
        `shouldBe` toLetters "peach"

-- TODO: QuickCheck test that when guess = answer, result = guess

  describe "without" $ do
    it "should return an empty list when given an empty list" $ do
      [] `without` Letter 'w' 1 `shouldBe` []

    it "should remove the letter if present in the list" $ do
      [Letter 'a' 5, Letter 'b' 3] `without` Letter 'b' 3 `shouldBe` [Letter 'a' 5]

    it "should remove the only element in the list if it matches" $ do
      [Letter 'z' 10] `without` Letter 'z' 10 `shouldBe` []

    it "shouldn't make any changes to list if element is not present" $ do
      [Letter 'a' 1, Letter 'b' 2] `without` Letter 'a' 2 `shouldBe` [Letter 'a' 1, Letter 'b' 2]

  describe "getLettersInIncorrectPosition" $ do
    it "should return an empty list with empty list inputs" $ do
      getLettersInIncorrectPosition [] [] `shouldBe` []

    it "should return an empty list when there are no letters to match on" $ do
      getLettersInIncorrectPosition [] [Letter 'a' 1] `shouldBe` []

    it "should return an empty list when we're not looking for any letters to match" $ do
      getLettersInIncorrectPosition [Letter 'a' 1] [] `shouldBe` []

    it "should return empty list when no characters match" $ do
      getLettersInIncorrectPosition [Letter 'a' 1, Letter 'b' 2] [Letter 'y' 1, Letter 'z' 2] `shouldBe` []

    it "should return letter with matching character but different index" $ do
      getLettersInIncorrectPosition [Letter 'a' 1] [Letter 'a' 2] `shouldBe` [Letter 'a' 2]

    it "should not return letter when character and index both match" $ do
      getLettersInIncorrectPosition [Letter 'a' 1] [Letter 'a' 1] `shouldBe` []

    it "should only return one letter in incorrect position when there are repeating letters in guess" $ do
      getLettersInIncorrectPosition [Letter 'a' 1, Letter 'b' 2] [Letter 'a' 2, Letter 'a' 3] `shouldBe` [Letter 'a' 2]

    it "should return all letters in incorrect position where there are more than one" $ do
      getLettersInIncorrectPosition [Letter 'a' 1, Letter 'b' 2, Letter 'c' 3] [Letter 'c' 1, Letter 'a' 2, Letter 'b' 3] `shouldBe` [Letter 'c' 1, Letter 'a' 2, Letter 'b' 3]

    it "should return repeating characters in the wrong position when there are more than one in the answer" $ do
      getLettersInIncorrectPosition [Letter 'a' 1, Letter 'a' 2] [Letter 'a' 3, Letter 'a' 4] `shouldBe` [Letter 'a' 3, Letter 'a' 4]