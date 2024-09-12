{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import qualified Data.Text as T
import System.Random.Stateful (getStdRandom, randomR)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 as H
  ( Html,
    body,
    docTypeHtml,
    head,
    p,
    span,
    title,
    toHtml,
  )
import Web.Spock
  ( HasSpock (getState),
    SpockM,
    get,
    html,
    root,
    runSpock,
    spock,
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
    get root $ Web.Spock.html $ T.pack (renderHtml $ generateHtml answer)

generateHtml :: String -> Html
generateHtml answer = docTypeHtml $ do
  H.head $
    H.title "Haskell wordle"
  H.body $ do
    H.p "Welcome to Haskell wordle!"
    H.span $ toHtml ("Answer: " ++ answer)
