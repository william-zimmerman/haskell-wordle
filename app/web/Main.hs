module Main (main) where

import qualified Data.Text as T
import System.Random.Stateful (getStdRandom, randomR)
import Text.Printf (printf)
import Web.Spock
  ( HasSpock (getState),
    SpockM,
    get,
    root,
    runSpock,
    spock,
    text,
  )
import Web.Spock.Config
  ( PoolOrConn (PCNoDatabase),
    defaultSpockCfg,
  )

data MySession = EmptySession

newtype MyAppState = InitState String

main :: IO ()
main =
  do
    availableWords <- readWords
    answer <- getRandomElement availableWords
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (InitState answer)
    runSpock 8080 (spock spockCfg app)

readWords :: IO [String]
readWords = words <$> readFile "words.txt"

getRandomElement :: [a] -> IO a
getRandomElement xs = do
  index <- getStdRandom (randomR (0, length xs))
  return (xs !! index)

app :: SpockM () MySession MyAppState ()
app =
  do
    (InitState answer) <- getState
    get root $ text $ T.pack (printf "Answer: %s" answer)
