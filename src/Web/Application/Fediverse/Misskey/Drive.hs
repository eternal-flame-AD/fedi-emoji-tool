{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Misskey.Drive where

import Control.Exception (catch)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Default.Class (Default (def))
import Data.Function ((&))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Simple (setRequestBearerAuth, setRequestBodyJSON, setRequestHeader, setRequestMethod)
import Network.HTTP.Types (hContentType, ok200, status204)
import Web.Application.Fediverse.Misskey.Base (
    MisskeyErrorWrapper (MisskeyErrorWrapper),
    RequestError (..),
    ServerConf (baseURL),
    bearerToken,
    unTagCamelCaseOptions,
 )
import Web.URL (ToURL (toURL), URLFrag)

driveCreateEndpoint :: URLFrag
driveCreateEndpoint = "/api/drive/files/create"

data DriveCreateOpts = DriveCreateOpts
    { driveCreateFolderId :: Maybe Text
    , driveCreateName :: Maybe Text
    , driveCreateComment :: Maybe Text
    , driveCreateIsSensitive :: Maybe Bool
    , driveCreateForce :: Maybe Bool
    , driveCreateFileContent :: ByteString
    , driveCreateFileContentType :: ByteString
    , driveCreateFilePath :: Text
    }
    deriving (Show)

instance Default DriveCreateOpts where
    def = DriveCreateOpts Nothing Nothing Nothing Nothing Nothing "" "" ""

data DriveCreateResponse = DriveCreateResponse
    { driveCreateResponseId :: Text
    , driveCreateResponseCreatedAt :: UTCTime
    , driveCreateResponseName :: Text
    , driveCreateResponseType :: Text
    , driveCreateResponseSize :: Int
    , driveCreateResponseUrl :: Text
    }
    deriving (Show, Generic)

instance FromJSON DriveCreateResponse where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "driveCreateResponse"

instance ToJSON DriveCreateResponse where
    toJSON = genericToJSON $ unTagCamelCaseOptions "driveCreateResponse"

driveUploadFile :: Manager -> ServerConf -> DriveCreateOpts -> IO (Either RequestError DriveCreateResponse)
driveUploadFile man serverConf opts =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        let filePart =
                partFileRequestBodyM
                    "file"
                    (unpack $ driveCreateFilePath opts)
                    (pure $ RequestBodyBS $ driveCreateFileContent opts)
                    & flip addPartHeaders [("Content-Type", driveCreateFileContentType opts)]

        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> driveCreateEndpoint
        req <-
            formDataBody
                ( catMaybes
                    [ Just filePart
                    , partBS "folderId" . encodeUtf8 <$> driveCreateFolderId opts
                    , partBS "name" . encodeUtf8 <$> driveCreateName opts
                    , partBS "comment" . encodeUtf8 <$> driveCreateComment opts
                    , if driveCreateIsSensitive opts == Just True
                        then Just $ partBS "isSensitive" "true"
                        else Just $ partBS "isSensitive" "false"
                    , if driveCreateForce opts == Just True
                        then Just $ partBS "force" "true"
                        else Just $ partBS "force" "false"
                    ]
                )
                $ setRequestHeader hContentType ["multipart/form-data"]
                $ setRequestBearerAuth (encodeUtf8 bearer)
                $ setRequestMethod "POST" req0
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
                            Right r -> Right r
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right me -> Left $ APIError me
                            Left e ->
                                Left $
                                    BadBodyError $
                                        "Could not decode error response body (status: "
                                            ++ show (responseStatus response)
                                            ++ "): "
                                            ++ show e

data DriveListFilesOpts = DriveListFilesOpts
    { driveListFilesFolderId :: Maybe Text
    , driveListFilesLimit :: Maybe Int
    , driveListFilesSinceId :: Maybe Text
    , driveListFilesUntilId :: Maybe Text
    }
    deriving (Show, Generic)

instance Default DriveListFilesOpts where
    def = DriveListFilesOpts Nothing Nothing Nothing Nothing

instance ToJSON DriveListFilesOpts where
    toJSON = genericToJSON $ (unTagCamelCaseOptions "driveListFiles"){omitNothingFields = True}

driveListFiles :: Manager -> ServerConf -> DriveListFilesOpts -> IO (Either RequestError [DriveCreateResponse])
driveListFiles man serverConf opts =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> "/api/drive/files"
        let req =
                setRequestBearerAuth (encodeUtf8 bearer) $
                    setRequestMethod "POST" $
                        setRequestBodyJSON opts req0
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
                            Right r -> Right r
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper me) -> Left $ APIError me
                            Left e ->
                                Left $
                                    BadBodyError $
                                        "Could not decode error response body (status: "
                                            ++ show (responseStatus response)
                                            ++ "): "
                                            ++ show e

driveDeleteFile :: Manager -> ServerConf -> Text -> IO (Either RequestError ())
driveDeleteFile man serverConf fileId =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> "/api/drive/files/delete"
        let req =
                setRequestBearerAuth (encodeUtf8 bearer) $
                    setRequestBodyJSON (object ["fileId" .= fileId]) $
                        setRequestMethod "POST" req0
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
                            Right (MisskeyErrorWrapper me) -> Left $ APIError me
                            Left e ->
                                Left $
                                    BadBodyError $
                                        "Could not decode error response body (status: "
                                            ++ show (responseStatus response)
                                            ++ "): "
                                            ++ show e

driveDeleteFolder :: Manager -> ServerConf -> Text -> IO (Either RequestError ())
driveDeleteFolder man serverConf folderId =
    do
        let bearer = fromMaybe (error "This request requires authentication") $ bearerToken serverConf
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> "/api/drive/folders/delete"
        let req =
                setRequestBearerAuth (encodeUtf8 bearer) $
                    setRequestBodyJSON (object ["folderId" .= folderId]) $
                        setRequestMethod "POST" req0
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
                            Right (MisskeyErrorWrapper me) -> Left $ APIError me
                            Left e ->
                                Left $
                                    BadBodyError $
                                        "Could not decode error response body (status: "
                                            ++ show (responseStatus response)
                                            ++ "): "
                                            ++ show e