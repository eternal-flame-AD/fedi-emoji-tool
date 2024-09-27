{-# LANGUAGE DeriveGeneric #-}

module Web.Application.Fediverse.Misskey.Base where

import Data.Aeson
import Data.Char (toLower)
import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (HttpException (HttpExceptionRequest, InvalidUrlException), Request, host, method, path)
import Web.URL (URLFrag)

misskeyVariants :: [Text]
misskeyVariants = ["misskey", "sharkey", "foundkey", "firefish", "iceshrimp"]

unTagCamelCaseOptions :: String -> Options
unTagCamelCaseOptions prefix = defaultOptions{fieldLabelModifier = lowerFirst . drop (length prefix)}
  where
    lowerFirst :: String -> String
    lowerFirst [] = []
    lowerFirst (x : xs) = toLower x : xs

data ServerConf = ServerConf
    { baseURL :: URLFrag
    , bearerToken :: Maybe Text
    }

unauthenticated :: URLFrag -> ServerConf
unauthenticated url = ServerConf url Nothing

bearerAuthenticated :: URLFrag -> Text -> ServerConf
bearerAuthenticated url token = ServerConf url (Just token)

describeHTTPException :: HttpException -> String
describeHTTPException (InvalidUrlException url reason) = "Invalid URL: " ++ url ++ " (" ++ reason ++ ")"
describeHTTPException (HttpExceptionRequest r c) = "HTTP exception on " ++ formatRequest r ++ ": " ++ show c
  where
    formatRequest :: Request -> String
    formatRequest req = show (method req) ++ " " ++ show (host req) ++ show (path req)

data RequestError = APIError MisskeyError | BadBodyError String | HTTPError HttpException
    deriving (Show)

describeRequestError :: RequestError -> String
describeRequestError (APIError e) = "API error: " ++ show e
describeRequestError (BadBodyError b) = "Bad body: " ++ b
describeRequestError (HTTPError e) = "HTTP error: " ++ describeHTTPException e

data MisskeyError = MisskeyError
    { errorCode :: Maybe Text
    , errorMessage :: Maybe Text
    , errorId :: Maybe Text
    , errorInfo :: Maybe Object
    }
    deriving (Show, Generic)

instance Eq MisskeyError where
    (==) a b = errorId a == errorId b

instance FromJSON MisskeyError where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "error"

newtype MisskeyErrorWrapper = MisskeyErrorWrapper
    { error :: MisskeyError
    }
    deriving (Show, Generic)

instance FromJSON MisskeyErrorWrapper

data PaginationOptions = PaginationOptions
    { limit :: Int
    , offset :: Maybe Int
    }
    deriving (Show, Generic)

instance ToJSON PaginationOptions

instance Default PaginationOptions where
    def = PaginationOptions 64 Nothing

paginationOptStream :: PaginationOptions -> [PaginationOptions]
paginationOptStream (PaginationOptions l o) = PaginationOptions l <$> (Just <$> [max 0 (fromMaybe 0 o), l ..])

whileForM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m [b]
whileForM [] _ = pure []
whileForM (x : xs) f = do
    r <- f x
    case r of
        Nothing -> pure []
        Just v -> do
            vs <- whileForM xs f
            pure $ v : vs

untilForM :: (Monad m) => [a] -> (a -> m (Maybe b)) -> m (Maybe b)
untilForM [] _ = pure Nothing
untilForM (x : xs) f = do
    r <- f x
    case r of
        Nothing -> untilForM xs f
        Just v -> pure $ Just v

untilM ::
    (Monad m, Show a) =>
    (a -> Bool) ->
    (a -> m a) ->
    a ->
    m [a]
untilM predic f x = do
    r <- f x
    if predic r
        then pure [r]
        else do
            rs <- untilM predic f r
            pure $ r : rs
