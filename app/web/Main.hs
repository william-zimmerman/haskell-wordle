module Main where

import Control.Monad.Trans
import Data.IORef
import Data.Text (empty)
import qualified Data.Text as T
import Text.Printf (printf)
import Web.Spock
import Web.Spock.Config

data MySession = EmptySession

newtype MyAppState = InitState [String]

main :: IO ()
main =
  do
    availableWords <- readWords
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (InitState availableWords)
    runSpock 8080 (spock spockCfg app)

readWords :: IO [String]
readWords = words <$> readFile "words.txt"

app :: SpockM () MySession MyAppState ()
app =
  do
    (InitState allWords) <- getState
    get root $ text $ T.pack (printf "Words: %s" (unwords allWords))
