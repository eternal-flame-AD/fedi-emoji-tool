{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Mastodon.Base where

import Data.Aeson
import Data.Char (isAsciiUpper, toLower)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (HttpException)
import Web.URL (URLFrag)

unTagSnakeCaseOptions :: String -> Options
unTagSnakeCaseOptions prefix = defaultOptions{fieldLabelModifier = toSnakeCase . drop (length prefix)}
 where
  toSnakeCase :: String -> String
  toSnakeCase input = dropWhile (== '_') $ toSnakeCase' input
  toSnakeCase' :: String -> String
  toSnakeCase' [] = []
  toSnakeCase' (x : xs) | isAsciiUpper x = '_' : toLower x : toSnakeCase xs
  toSnakeCase' (x : xs) = x : toSnakeCase xs

mastodonVariants :: [Text]
mastodonVariants = ["mastodon", "tootdon", "tootcat", "tootdog", "tootfox"]

data MastoError
  = MastoHTTPError HttpException
  | MastoAPIError MastoApiError
  | MastoBadBodyError String
  deriving (Show)

data MastoApiError = MastoApiError
  { mastoApiError :: Text
  , mastoApiErrorDescription :: Text
  }
  deriving (Show, Generic)

instance FromJSON MastoApiError where
  parseJSON = genericParseJSON $ unTagSnakeCaseOptions "mastoApiError"

newtype MastoServerConf = MastoServerConf
  { mastoBaseURL :: URLFrag
  }
  deriving (Show)
