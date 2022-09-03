module LibSpec
  ( spec
  ) where

import           Data.String.Utils
-- import           Test.HUnit
import           Test.Hspec

{-  TODO - Do we want to launch "legacy"  HUnit tests?
testToLetters :: Test
testToLetters = TestCase
  (assertEqual "toLetters with actual word"
               [Letter 'c' 1, Letter 'a' 2, Letter 't' 3]
               (toLetters "cat")
  )

testToLettersWithEmptyString :: Test
testToLettersWithEmptyString =
  TestCase (assertEqual "toLetters with empty string" [] (toLetters ""))
-}

spec :: Spec
spec = do
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
