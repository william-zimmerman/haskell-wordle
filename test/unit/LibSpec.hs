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