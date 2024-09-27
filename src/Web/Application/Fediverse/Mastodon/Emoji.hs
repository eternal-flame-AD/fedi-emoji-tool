{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Mastodon.Emoji where

import Control.Exception (catch)
import Data.Aeson
import Data.Text (Text, unpack)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Types (ok200)
import Web.Application.Fediverse.Mastodon.Base (MastoError (..), MastoServerConf (mastoBaseURL), unTagSnakeCaseOptions)
import Web.URL (ToURL (toURL), URLFrag)

data GetCustomEmojisResponse = GetCustomEmojisResponse
    { getCustomEmojisShortcode :: Text
    , getCustomEmojisUrl :: Text
    , getCustomEmojisStaticUrl :: Text
    , getCustomEmojisCategory :: Maybe Text
    }
    deriving (Show, Generic)

instance FromJSON GetCustomEmojisResponse where
    parseJSON = genericParseJSON (unTagSnakeCaseOptions "getCustomEmojis")

viewAllCustomEmojiEndpoint :: URLFrag
viewAllCustomEmojiEndpoint = "/api/v1/custom_emojis"

viewAllCustomEmoji :: Manager -> MastoServerConf -> IO (Either MastoError [GetCustomEmojisResponse])
viewAllCustomEmoji man conf = do
    req <- (parseRequest . unpack . toURL) (mastoBaseURL conf <> viewAllCustomEmojiEndpoint)
    result <-
        catch
            (Right <$> httpLbs req{decompress = const True} man)
            (pure . Left . MastoHTTPError)
    case result of
        Left e -> pure $ Left e
        Right response -> do
            let body = responseBody response
            pure $
                if responseStatus response == ok200
                    then case eitherDecode body of
                        Left e -> Left $ MastoBadBodyError e
                        Right r -> Right r
                    else case eitherDecode body of
                        Left e -> Left $ MastoBadBodyError e
                        Right r -> Left $ MastoAPIError r