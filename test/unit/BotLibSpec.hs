module BotLibSpec
  ( spec
  ) where

import           Lib
import           BotLib
import           Test.Hspec

spec::Spec
spec = do
    describe "isLogicalGuess" $ do
        it "should return false when all letters in correct position do not match" $ do
            isLogicalGuess [LetterEval 1 'a' CorrectPosition] [Letter 1 'b'] `shouldBe` False

        it "should return true when all letters in correct position match" $ do
            isLogicalGuess [LetterEval 1 'a' CorrectPosition] [Letter 1 'a'] `shouldBe` True