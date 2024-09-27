{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Misskey.Emoji where

import Control.Exception (catch)
import Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (Default (def))
import Data.Maybe (fromMaybe)
import Data.Text
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestBearerAuth, setRequestBodyJSON, setRequestMethod)
import Network.HTTP.Types.Status (ok200, status204)
import Web.Application.Fediverse.Misskey.Base (
    MisskeyErrorWrapper (MisskeyErrorWrapper),
    RequestError (..),
    ServerConf (baseURL, bearerToken),
    unTagCamelCaseOptions,
 )
import Web.URL (ToURL (toURL), URLFrag)

-- | The endpoint for emojis on a Misskey instance.
emojiEndpoint :: URLFrag
emojiEndpoint = "/api/emojis"

admEmojiEndpoint :: URLFrag
admEmojiEndpoint = "/api/admin/emoji"

data EmojiItem = EmojiItem
    { emojiId :: Maybe Text
    , emojiAliases :: [Text]
    , emojiName :: Text
    , emojiCategory :: Maybe Text
    , emojiUrl :: Text
    , emojiLocalOnly :: Maybe Bool
    , emojiIsSensitive :: Maybe Bool
    }
    deriving (Generic, Show)

instance Default EmojiItem where
    def = EmojiItem Nothing [] "" Nothing "" Nothing Nothing

instance FromJSON EmojiItem where
    parseJSON = genericParseJSON $ (unTagCamelCaseOptions "emoji"){omitNothingFields = True}

instance ToJSON EmojiItem where
    toJSON = genericToJSON $ (unTagCamelCaseOptions "emoji"){omitNothingFields = True}

newtype EmojisWrapper = EmojisWrapper
    { emojis :: [EmojiItem]
    }
    deriving (Generic, Show)

instance FromJSON EmojisWrapper

listEmojis :: Manager -> ServerConf -> IO (Either RequestError [EmojiItem])
listEmojis man serverConf =
    do
        req <- (parseRequest . unpack . toURL) $ baseURL serverConf <> emojiEndpoint
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == ok200
                        then case eitherDecode body of
                            Right (EmojisWrapper e) -> Right e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e

data ListEmojisOptions = ListEmojisOptions
    { listEmojisQuery :: Maybe Text
    , listEmojisLimit :: Maybe Int
    , listEmojisSinceId :: Maybe Text
    , listEmojisUntilId :: Maybe Text
    }
    deriving (Generic, Show)

instance Default ListEmojisOptions where
    def = ListEmojisOptions Nothing Nothing Nothing Nothing

instance ToJSON ListEmojisOptions where
    toJSON = genericToJSON $ (unTagCamelCaseOptions "listEmojis"){omitNothingFields = True}

admListRemoteEmojis :: Manager -> ServerConf -> ListEmojisOptions -> IO (Either RequestError [EmojiItem])
admListRemoteEmojis man serverConf options =
    do
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> emojiEndpoint
        let req =
                setRequestBodyJSON options $
                    setRequestMethod
                        "POST"
                        req0
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == ok200
                        then case eitherDecode body of
                            Right (EmojisWrapper e) -> Right e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e

admBulkDeleteEmojis :: Manager -> ServerConf -> [Text] -> IO (Either RequestError ())
admBulkDeleteEmojis man conf ids =
    do
        let bearer = Data.Maybe.fromMaybe (error "This request requires authentication") $ bearerToken conf
        req0 <- (parseRequest . unpack . toURL) $ baseURL conf <> admEmojiEndpoint <> "/delete-bulk"
        let req =
                setRequestBearerAuth ((BSC.pack . unpack) bearer) $
                    setRequestBodyJSON (object ["ids" .= ids]) $
                        setRequestMethod
                            "POST"
                            req0
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == status204
                        then Right ()
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e

admListEmojis :: Manager -> ServerConf -> ListEmojisOptions -> IO (Either RequestError [EmojiItem])
admListEmojis man serverConf options =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> admEmojiEndpoint <> "/list"
        let req =
                setRequestBearerAuth ((BSC.pack . unpack) bearer) $
                    setRequestBodyJSON options $
                        setRequestMethod
                            "POST"
                            req0
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == ok200
                        then case eitherDecode body of
                            Right e -> Right e
                            Left e -> Left $ BadBodyError $ "adm_list_emojis: Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "adm_list_emojis: Could not decode response body" ++ show e

admEmojiCopy :: Manager -> ServerConf -> Text -> IO (Either RequestError EmojiItem)
admEmojiCopy man serverConf emojiIdOpt =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> emojiEndpoint <> "/copy"
        let req =
                setRequestBearerAuth ((BSC.pack . unpack) bearer) $
                    setRequestBodyJSON (object ["emojiId" .= emojiIdOpt]) $
                        setRequestMethod
                            "POST"
                            req0
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == ok200
                        then case eitherDecode body of
                            Right e -> Right e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e

data EmojiAddOpts = EmojiAddOpts
    { emojiAddName :: Text
    , emojiAddAliases :: [Text]
    , emojiAddCategory :: Maybe Text
    , emojiAddIsSensitive :: Bool
    , emojiAddLocalOnly :: Bool
    , emojiAddFileId :: Text
    }
    deriving (Show, Generic)

instance Default EmojiAddOpts where
    def = EmojiAddOpts "" [] Nothing False False ""

instance ToJSON EmojiAddOpts where
    toJSON = genericToJSON $ unTagCamelCaseOptions "emojiAdd"

admEmojiAdd :: Manager -> ServerConf -> EmojiAddOpts -> IO (Either RequestError EmojiItem)
admEmojiAdd man serverConf opts =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> admEmojiEndpoint <> "/add"
        let req =
                setRequestBearerAuth ((BSC.pack . unpack) bearer) $
                    setRequestBodyJSON opts $
                        setRequestMethod
                            "POST"
                            req0
        result <-
            catch
                (Right <$> httpLbs req{decompress = const True} man)
                (pure . Left . HTTPError)
        case result of
            Left e -> pure $ Left e
            Right response -> do
                let body = responseBody response
                pure $
                    if responseStatus response == ok200
                        then case eitherDecode body of
                            Right e -> Right e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
