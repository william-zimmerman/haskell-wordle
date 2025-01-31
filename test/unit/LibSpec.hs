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

  describe "evaluateGuess" $ do
    it
        "should return list with letters not in answer when guess does not overlap with answer"
      $ do
          evaluateGuess (toLetters "bat") (toLetters "hop")
            `shouldBe` [ NotInAnswer 1 'h'
                       , NotInAnswer 2 'o'
                       , NotInAnswer 3 'p'
                       ]

    it
        "should return list with letters in correct place when guess equals answer"
      $ do
          evaluateGuess (toLetters "william") (toLetters "william")
            `shouldBe` [ CorrectPosition 1 'w'
                       , CorrectPosition 2 'i'
                       , CorrectPosition 3 'l'
                       , CorrectPosition 4 'l'
                       , CorrectPosition 5 'i'
                       , CorrectPosition 6 'a'
                       , CorrectPosition 7 'm'
                       ]

    it "should return list of letter evals in order of their index" $ do
      evaluateGuess (toLetters "spend") (toLetters "waxed")
        `shouldBe` [ NotInAnswer 1 'w'
                   , NotInAnswer 2 'a'
                   , NotInAnswer 3 'x'
                   , IncorrectPosition 4 'e'
                   , CorrectPosition 5 'd'
                   ]

    it
        "should mark the first repeated letter in guess as IncorrectPosition and the second as NotInAnswer"
      $ do
          evaluateGuess (toLetters "planet") (toLetters "apples")
            `shouldBe` [ IncorrectPosition 1 'a'
                       , IncorrectPosition 2 'p'
                       , NotInAnswer 3 'p'
                       , IncorrectPosition 4 'l' 
                       , CorrectPosition 5 'e' 
                       , NotInAnswer 6 's' 
                       ]

  describe "guessIsCorrect" $ do
    it "should return false with an empty list" $ do
      guessIsCorrect [] `shouldBe` False

    it "should return true if all elements are in correct position" $ do
      guessIsCorrect
          [ CorrectPosition 1 'a' 
          , CorrectPosition 2 'b' 
          , CorrectPosition 3 'c' 
          ]
        `shouldBe` True

    it "should return false if single element is not in answer" $ do
      guessIsCorrect
        [CorrectPosition 1 'a', NotInAnswer 2 'b']
        `shouldBe` False
    
    it "should return false if single element is in incorrect position" $ do
      guessIsCorrect [CorrectPosition 1 'a', IncorrectPosition 2 'b']
      `shouldBe` False
