module LibSpec
  ( spec
  ) where

import           Lib
import           Test.HUnit
import           Test.Hspec
import           Test.Hspec.Contrib.HUnit

testToLetters :: Test
testToLetters = TestCase
  (assertEqual "toLetters with actual word"
               [Letter 'c' 1, Letter 'a' 2, Letter 't' 3]
               (toLetters "cat")
  )

testToLettersWithEmptyString :: Test
testToLettersWithEmptyString =
  TestCase (assertEqual "toLetters with empty string" [] (toLetters ""))

testSuite :: Test
testSuite = TestList [testToLetters, testToLettersWithEmptyString]

spec :: Spec
spec = do
  describe "running legacy HUnit tests" $ do
    fromHUnitTest testSuite
