module BotLibSpec
  ( spec
  ) where

import           BotLib
import           Lib
import           Test.Hspec

spec :: Spec
spec = do
  describe "isLogicalGuess" $ do
    it "should return false when all letters in correct position do not match"
      $ do
          isLogicalGuess [CorrectPosition 1 'a'] [Letter 1 'b'] `shouldBe` False

    it "should return true when all letters in correct position match" $ do
      isLogicalGuess [CorrectPosition 1 'a'] [Letter 1 'a'] `shouldBe` True

    it "should return false when letters in incorrect position are not in guess"
      $ do
          isLogicalGuess [IncorrectPosition 1 'a'] [Letter 1 'b']
            `shouldBe` False

    it "should return true when letters in incorrect position are in guess" $ do
      isLogicalGuess [IncorrectPosition 1 'a'] [Letter 2 'a'] `shouldBe` True

    it
        "should return false when guess has letter in incorrect position in same index as evaluation"
      $ do
          isLogicalGuess [IncorrectPosition 1 'a'] [Letter 1 'a']
            `shouldBe` False

    it "should return false with multiple letters in incorrect position" $ do
      isLogicalGuess [IncorrectPosition 1 'a', IncorrectPosition 2 'a']
                     [Letter 3 'a']
        `shouldBe` False

    it "should respect letters in correct and incorrect position" $ do
      isLogicalGuess [CorrectPosition 1 'a', IncorrectPosition 2 'b']
                     [Letter 1 'a', Letter 2 'b']
        `shouldBe` False
