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
readWords = do
  entireFile <- readFile "words.txt"
  let linesOfFile = words entireFile
  putStrLn $ printf "read file with %d entries" (length linesOfFile)
  return linesOfFile

app :: SpockM () MySession MyAppState ()
app =
  do get root $ text $ T.pack "Hello world!"

--    get ("hello" <//> var) $ \name ->
--        do (DummyAppState ref) <- getState
--           visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
--           text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))
