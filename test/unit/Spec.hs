{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{-
module Main(main) where

import Test.Hspec

spec :: Spec
spec = do
  describe "My first test" $ do
    it "returns the first element of a list" $ do
      head [23, 24] `shouldBe` (23 :: Int)

main :: IO ()
main = hspec spec
-}