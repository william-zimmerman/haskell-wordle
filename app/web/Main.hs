{-# LANGUAGE OverloadedStrings #-}

module Main (Main.main) where

import qualified Data.Text as T
import Lib (AnswerResponse (AnswerResponse), GuessResponse (GuessResponse))
import Network.Wai.Middleware.Static (addBase, staticPolicy)
import System.Random.Stateful (getStdRandom, randomR)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html5 as H
  ( Html,
    body,
    docTypeHtml,
    head,
    script,
    title,
    (!),
  )
import qualified Text.Blaze.Html5.Attributes as A
import Web.Spock (HasSpock (getState), SpockM, get, html, json, middleware, root, runSpock, spock, text, var, (<//>))
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
    middleware $ staticPolicy (addBase "static")
    get root $ Web.Spock.html $ T.pack (renderHtml generateHtml)
    get "answer" $ Web.Spock.json (AnswerResponse answer)
    get ("guess" <//> var) $ \value -> Web.Spock.json (GuessResponse value)

generateHtml :: Html
generateHtml = docTypeHtml $ do
  H.head $
    H.title "Haskell wordle"
  H.body $ do
    H.script ! A.src "js/script.js" $ ""
