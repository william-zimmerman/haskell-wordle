{-# LANGUAGE DeriveGeneric #-}

module Lib (AnswerResponse (..), GuessResponse (..)) where

import Data.Aeson (ToJSON, defaultOptions, genericToEncoding)
import Data.Aeson.Types (toEncoding)
import GHC.Generics

newtype GuessResponse = GuessResponse
  { guess :: String
  }
  deriving (Generic, Show)

instance ToJSON GuessResponse where
  toEncoding = genericToEncoding defaultOptions

newtype AnswerResponse = AnswerResponse
  { answer :: String
  }
  deriving (Generic, Show)

instance ToJSON AnswerResponse where
  toEncoding = genericToEncoding defaultOptions