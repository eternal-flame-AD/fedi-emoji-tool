{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Misskey.Federation where

import Control.Exception (catch)
import Data.Aeson
import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Text
import Data.Time (UTCTime)
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Simple (setRequestBodyJSON, setRequestMethod)
import Network.HTTP.Types.Status (ok200)
import Web.Application.Fediverse.Misskey.Base
import Web.URL (ToURL (toURL), URLFrag)

federationInstanceEndpoint :: URLFrag
federationInstanceEndpoint = "/api/federation/instances"

data FederationInstance = FederationInstance
    { fedId :: Text
    , fedHost :: Text
    , fedFirstRetrievedAt :: UTCTime
    , fedUsersCount :: Int
    , fedNotesCount :: Int
    , fedIsNotResponding :: Bool
    , fedIsSuspended :: Bool
    , fedIsBlocked :: Bool
    , fedSuspensionState :: Text
    , fedSoftwareName :: Maybe Text
    , fedName :: Maybe Text
    , fedDescription :: Maybe Text
    }
    deriving (Show, Generic)

instanceDescribe :: FederationInstance -> Text
instanceDescribe i =
    let summary = fromMaybe "no_name" (fedName i) <> " (" <> fedHost i <> ")"
     in case () of
            _
                | fedIsSuspended i -> summary <> " [suspended]"
                | fedIsBlocked i -> summary <> " [blocked]"
                | fedIsNotResponding i -> summary <> " [unresponsive]"
                | otherwise ->
                    summary
                        <> " [active] [users: "
                        <> pack (show $ fedUsersCount i)
                        <> "] [notes: "
                        <> pack (show $ fedNotesCount i)
                        <> "] [software: "
                        <> fromMaybe "unknown" (fedSoftwareName i)
                        <> "]"

instance FromJSON FederationInstance where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "fed"

data ListFederationInstancesOptions = ListFederationInstancesOptions
    { blocked :: Maybe Bool
    , suspended :: Maybe Bool
    , silenced :: Maybe Bool
    , federating :: Maybe Bool
    , subscribing :: Maybe Bool
    , publishing :: Maybe Bool
    }
    deriving (Show, Generic)

instance Default ListFederationInstancesOptions where
    def = ListFederationInstancesOptions (Just False) (Just False) Nothing Nothing Nothing Nothing

instance ToJSON ListFederationInstancesOptions

data CombinedListFederationInstancesOptions = CombinedListFederationInstancesOptions
    { listOpts :: ListFederationInstancesOptions
    , paginationOpts :: PaginationOptions
    }
    deriving (Show, Generic)

instance ToJSON CombinedListFederationInstancesOptions where
    toJSON (CombinedListFederationInstancesOptions l p) =
        object
            [ "blocked" .= blocked l
            , "suspended" .= suspended l
            , "silenced" .= silenced l
            , "federating" .= federating l
            , "subscribing" .= subscribing l
            , "publishing" .= publishing l
            , "limit" .= limit p
            , "offset" .= offset p
            ]

listFederationInstances :: Manager -> ServerConf -> ListFederationInstancesOptions -> PaginationOptions -> IO (Either RequestError [FederationInstance])
listFederationInstances man serverConf opts pag =
    do
        req0 <- (parseRequest . unpack . toURL) $ baseURL serverConf <> federationInstanceEndpoint
        let req =
                setRequestBodyJSON (CombinedListFederationInstancesOptions opts pag) $
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
                            Right r -> Right r
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
                        else case eitherDecode body of
                            Right (MisskeyErrorWrapper e) -> Left $ APIError e
                            Left e -> Left $ BadBodyError $ "Could not decode response body" ++ show e
