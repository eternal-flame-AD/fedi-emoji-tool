{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, readMVar, threadDelay, withMVar)
import Control.Concurrent.Async (forConcurrently, forConcurrently_)
import Control.Exception (IOException, catch, throwIO)
import Control.Monad (forM_, void, when, (>=>))
import Data.Aeson
import Data.Bits (shiftL)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Functor (($>), (<&>))
import Data.HashMap.Strict (HashMap, insertWith, toList, unionWith)
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)
import Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, isNothing, mapMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, ManagerSettings (..), Request (decompress, host, path, requestHeaders), Response (responseBody, responseHeaders), httpLbs, newManager, parseRequest, responseTimeoutMicro)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (setRequestHeaders)
import Options.Applicative
import System.Directory (createDirectoryIfMissing)
import System.Environment.Blank (getEnvDefault)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), hPutStrLn, openFile, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.ProgressBar (Label (..), Progress (Progress, progressCustom, progressDone, progressTodo), Style (stylePrefix), defStyle, incProgress, newProgressBar, updateProgress)
import System.Random (randomRIO)
import qualified Web.Application.Fediverse.Mastodon.Base as MAS
import qualified Web.Application.Fediverse.Mastodon.Emoji as MASE
import Web.Application.Fediverse.Misskey.Base (
    PaginationOptions (limit),
    RequestError (HTTPError),
    bearerAuthenticated,
    describeRequestError,
    misskeyVariants,
    paginationOptStream,
    unTagCamelCaseOptions,
    unauthenticated,
    untilForM,
    untilM,
    whileForM,
 )
import Web.Application.Fediverse.Misskey.Drive (DriveCreateResponse (driveCreateResponseId), DriveListFilesOpts (driveListFilesFolderId, driveListFilesLimit), driveDeleteFile, driveDeleteFolder, driveListFiles)
import Web.Application.Fediverse.Misskey.Emoji (
    EmojiItem (EmojiItem, emojiAliases, emojiCategory, emojiId, emojiIsSensitive, emojiLocalOnly, emojiName, emojiUrl),
    ListEmojisOptions (..),
    admBulkDeleteEmojis,
    admListEmojis,
    listEmojis,
 )
import Web.Application.Fediverse.Misskey.Federation (FederationInstance (FederationInstance, fedHost, fedSoftwareName), instanceDescribe, listFederationInstances)
import Web.URL (URLFrag (URLFrag))

data CommonOpts = CommonOpts
    { homeOpt :: URLFrag
    , paginationLimitOpt :: Int
    , httpTimeoutOpt :: Int
    , headOpt :: Maybe Int
    , verboseOpt :: Bool
    }
data Command
    = SelfEmojis
    | SelfInstances
    | FetchRemoteEmojis String
    | MetaIntersect FilePath FilePath Bool FilePath
    | MetaMerge String [FilePath]
    | PackRemoteEmojis String String Float Int Int64
    | ClearLocalOnlyEmojis
    | DeleteFolder String Bool

opts :: Parser (CommonOpts, Command)
opts =
    (,)
        <$> ( CommonOpts
                <$> option
                    str
                    ( long "home"
                        <> short 'h'
                        <> metavar "URL"
                        <> help "Home URL of the Misskey instance, will read MISSKEY_HOME environment variable if not provided"
                        <> value
                            ( fromString $ case unsafePerformIO $ getEnvDefault "MISSKEY_HOME" "" of
                                "" -> error "MISSKEY_HOME is not set"
                                x -> x
                            )
                    )
                <*> option auto (long "pagination" <> short 'l' <> metavar "INT" <> help "Pagination limit" <> value 64)
                <*> option auto (long "timeout" <> short 't' <> metavar "INT" <> help "HTTP timeout" <> value 10)
                <*> option
                    (Just <$> auto)
                    ( long "head"
                        <> metavar "INT"
                        <> help "Only process the first N entries as a test run"
                        <> value Nothing
                    )
                <*> flag False True (long "verbose" <> short 'v' <> help "Verbose output")
            )
        <*> subparser
            ( command "home-emojis" (info (pure SelfEmojis) (progDesc "List custom emojis at home, sanity check mainly"))
                <> command "instances" (info (pure SelfInstances) (progDesc "List federation instances at home, sanity check mainly"))
                <> command
                    "fetch-remote-emojis"
                    ( info
                        ( FetchRemoteEmojis
                            <$> option
                                str
                                ( metavar "OUTPUT"
                                    <> short 'o'
                                    <> long "output"
                                    <> help "Output file to save remote emojis"
                                )
                        )
                        (progDesc "Fetch remote non-licensed emojis into a JSON file describing unique emojis and instances with them")
                    )
                <> command
                    "meta-intersect"
                    ( info
                        ( MetaIntersect
                            <$> option
                                str
                                ( metavar "INPUT1"
                                    <> short 'i'
                                    <> long "input1"
                                    <> help "Input file 1 to intersect"
                                )
                            <*> option
                                str
                                ( metavar "INPUT2"
                                    <> short 'j'
                                    <> long "input2"
                                    <> help "Input file 2 to intersect"
                                )
                            <*> switch
                                ( long "negate"
                                    <> short 'n'
                                    <> help "Negate the intersection, outputting only emojis in the first file that are not in the second"
                                )
                            <*> option
                                str
                                ( metavar "OUTPUT"
                                    <> short 'o'
                                    <> long "output"
                                    <> help "Output file to save the intersection"
                                )
                        )
                        (progDesc "Intersect or complement two meta emoji files")
                    )
                <> command
                    "meta-merge"
                    ( info
                        ( MetaMerge
                            <$> option
                                str
                                ( metavar "OUTPUT"
                                    <> short 'o'
                                    <> long "output"
                                    <> help "Output file to save the merged emojis"
                                )
                            <*> many
                                ( argument
                                    str
                                    ( metavar "INPUT"
                                        <> help "Input file to merge"
                                    )
                                )
                        )
                        (progDesc "Merge multiple meta emoji files")
                    )
                <> command
                    "pack-remote-emojis"
                    ( info
                        ( PackRemoteEmojis
                            <$> option
                                str
                                ( metavar "INPUT"
                                    <> short 'i'
                                    <> long "input"
                                    <> help "Input file to upload remote emojis"
                                )
                            <*> option
                                str
                                ( metavar "DIR"
                                    <> short 'o'
                                    <> long "output-dir"
                                    <> help "Working directory for the emojis"
                                )
                            <*> option
                                auto
                                ( metavar "DELAY"
                                    <> short 'd'
                                    <> long "delay"
                                    <> help "Delay between requests to the same host in seconds, default 0.66"
                                    <> value 0.66
                                )
                            <*> option
                                auto
                                ( metavar "MIN_COUNT"
                                    <> short 'm'
                                    <> long "min-count"
                                    <> help "Minimum count of instances to use an emoji for it to be included, default 5"
                                    <> value 5
                                )
                            <*> option
                                auto
                                ( metavar "SIZE_LIMIT"
                                    <> short 's'
                                    <> long "size-limit"
                                    <> help "Size limit for downloaded files in MiB, default 32"
                                    <> value 32
                                )
                        )
                        (progDesc "Pack the collected remote emojis into a directory that can be ZIPped and imported into Misskey-like instances")
                    )
                <> command
                    "clear-local-only-emojis"
                    ( info
                        (pure ClearLocalOnlyEmojis)
                        (progDesc "[Auth required] Clear local-only emojis, a mistake fixer")
                    )
                <> command
                    "delete-folder"
                    ( info
                        ( DeleteFolder
                            <$> argument
                                str
                                ( metavar "FOLDER"
                                    <> help "Folder to delete"
                                )
                            <*> switch
                                ( long "recursive"
                                    <> short 'r'
                                    <> help "Recursively delete the folder"
                                )
                        )
                        (progDesc "[Auth required] Delete a folder")
                    )
            )

parser :: ParserInfo (CommonOpts, Command)
parser =
    info
        opts
        ( header "fedi-emoji-tool - A tool to fetch and pack custom emojis from Misskey and Mastodon instances for Misskey"
            <> progDesc "A tool to fetch and pack custom emojis from Misskey and Mastodon instances"
            <> footer "Author: Yume <@yume@mi.yumechi.jp>"
        )

data EmojiMapEntry = EmojiMapEntry
    { emojiMName :: T.Text
    , emojiMSeenAt :: [EmojiItem]
    }
    deriving (Show, Generic)

instance Semigroup EmojiMapEntry where
    (EmojiMapEntry n1 s1) <> (EmojiMapEntry n2 s2) = if n1 == n2 then EmojiMapEntry n1 (s1 <> s2) else error "EmojiMapEntry: names do not match"

newEmojiEntry :: EmojiItem -> EmojiMapEntry
newEmojiEntry item@EmojiItem{emojiName = name'} = EmojiMapEntry name' [item]

entriesToMap :: [EmojiMapEntry] -> EmojiMap
entriesToMap = EmojiMap . foldr (\(EmojiMapEntry n s) -> insertWith (<>) n (EmojiMapEntry n s)) mempty

mapToEntries :: EmojiMap -> [EmojiMapEntry]
mapToEntries (EmojiMap m) = map snd $ toList m

instance FromJSON EmojiMapEntry where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "emojiM"

instance ToJSON EmojiMapEntry where
    toJSON = genericToJSON $ unTagCamelCaseOptions "emojiM"

newtype EmojiMap = EmojiMap (HashMap T.Text EmojiMapEntry)
    deriving (Show)

instance Semigroup EmojiMap where
    (EmojiMap m1) <> (EmojiMap m2) = EmojiMap $ unionWith (<>) m1 m2

instance Monoid EmojiMap where
    mempty = EmojiMap mempty

retryM :: Int -> IO (Either e a) -> IO (Either e a)
retryM 0 act = act
retryM n act =
    act >>= \case
        Left _ -> retryM (n - 1) act
        Right a -> pure $ Right a

data FetchAPIType = FetchMisskeyAPI | FetchMastodonAPI

fetchWorker ::
    FetchAPIType ->
    MVar PoliteWaiter ->
    MVar Handle ->
    FederationInstance ->
    Manager ->
    MVar EmojiMap ->
    IO Bool
fetchWorker FetchMisskeyAPI pw stderr' FederationInstance{fedHost = host'} manager emap =
    pwWait pw (lockHPutStrLn stderr') "dns"
        >> retryM
            3
            ( listEmojis manager (unauthenticated $ "https://" <> URLFrag host' <> "/")
            )
        >>= \case
            Left err -> lockHPutStrLn stderr' ("Error fetching emojis from " ++ T.unpack host' ++ ": " ++ describeRequestError err) $> False
            Right emojis' -> do
                let entries = map newEmojiEntry emojis'
                lockHPutStrLn stderr' $ "Fetched " ++ show (length entries) ++ " emojis from " ++ T.unpack host'
                modifyMVar_ emap $ pure . (<> entriesToMap entries)
                pure True
fetchWorker FetchMastodonAPI pw stderr' FederationInstance{fedHost = host'} manager emap =
    pwWait pw (lockHPutStrLn stderr') "dns"
        >> retryM
            3
            ( MASE.viewAllCustomEmoji
                manager
                (MAS.unauthenticated $ "https://" <> URLFrag host' <> "/")
            )
        >>= \case
            Left err -> lockHPutStrLn stderr' ("Error fetching emojis from " ++ T.unpack host' ++ ": " ++ MAS.describeMastoError err) $> False
            Right resp -> do
                let entries =
                        map
                            ( \e ->
                                EmojiMapEntry
                                    (MASE.getCustomEmojisShortcode e)
                                    [ def
                                        { emojiName = MASE.getCustomEmojisShortcode e
                                        , emojiUrl = MASE.getCustomEmojisUrl e
                                        , emojiCategory = MASE.getCustomEmojisCategory e
                                        , emojiIsSensitive = Just False
                                        , emojiLocalOnly = Just False
                                        }
                                    ]
                            )
                            resp
                lockHPutStrLn stderr' $ "Fetched " ++ show (length entries) ++ " emojis from " ++ T.unpack host'
                modifyMVar_ emap $ pure . (<> entriesToMap entries)
                pure True

downloadFile :: Manager -> Text -> Int64 -> IO (Either RequestError (Maybe (ByteString, ByteString)))
downloadFile manager url sizeLimit = do
    req0 <- parseRequest $ T.unpack url
    let req = req0{decompress = const True}
    res <- catch (Right <$> httpLbs req manager) (pure . Left . HTTPError)
    case res of
        Left e -> pure $ Left e
        Right res' -> do
            let bs = responseBody res'
            let ct =
                    fromMaybe "application/octet-stream" $
                        lookup "Content-Type" $
                            responseHeaders res'
            pure . Right $
                ( (,) ct <$> (BSL.toStrict <$> bslForceLength sizeLimit bs)
                )

bslForceLength :: Int64 -> BSL.ByteString -> Maybe BSL.ByteString
bslForceLength n bsl =
    let
        probeLength = n + 1
        probeList = BSL.take probeLength bsl
     in
        if BSL.length probeList <= fromIntegral n then Just probeList else Nothing

headPls :: [a] -> a
headPls [] = error "Empty list"
headPls (x : _) = x

imageContentTypes :: (IsString a, IsString b) => [(a, b)]
imageContentTypes =
    [ ("image/png", ".png")
    , ("image/jpeg", ".jpg")
    , ("image/gif", ".gif")
    , ("image/webp", ".webp")
    , ("image/svg+xml", ".svg")
    , ("image/svg", ".svg")
    , ("image/icon", ".icon")
    , ("image/apng", ".apng")
    ]

shuffle :: [a] -> IO [a]
shuffle [] = pure []
shuffle [x] = pure [x]
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    case splitAt i xs of
        (left, x : right) -> (x :) <$> shuffle (left ++ right)
        _ -> error "Impossible"

lockHPutStrLn :: MVar Handle -> String -> IO ()
lockHPutStrLn mvar msg = withMVar mvar $ \h -> hPutStrLn h msg

packWorker ::
    MVar PoliteWaiter ->
    (MVar Handle, MVar Handle) ->
    Manager ->
    Int64 ->
    FilePath ->
    EmojiMapEntry ->
    IO (Maybe (FilePath, EmojiItem))
packWorker
    polite
    (_, stderr')
    remoteManager
    limitMb
    outDir
    EmojiMapEntry{emojiMName = name', emojiMSeenAt = seenAt} = do
        urls <- shuffle $ map emojiUrl seenAt -- shuffle the URLs to be polite
        let aliases =
                filter (not . T.null) . nub $ concatMap emojiAliases seenAt
        let categoryBest =
                tallyOn emojiCategory (filter (isJust . emojiCategory) seenAt)
                    & sortOn (negate . snd)
                    & uncons
                    <&> fst
                    <&> (fromJust . emojiCategory . fst)

        untilForM
            urls
            ( \url -> do
                pwWaitURL polite (lockHPutStrLn stderr') url
                res <-
                    timeIO (lockHPutStrLn stderr') ("download_file " ++ T.unpack (domainOf url)) $
                        downloadFile remoteManager url (limitMb `shiftL` 20)
                case res of
                    Left err ->
                        Nothing
                            <$ lockHPutStrLn stderr' ("Error downloading " ++ T.unpack url ++ ": " ++ describeRequestError err)
                    Right Nothing ->
                        Nothing
                            <$ lockHPutStrLn stderr' ("File too large: " ++ T.unpack url)
                    Right (Just (ct, bs)) ->
                        if BS.null bs
                            then
                                Nothing
                                    <$ lockHPutStrLn stderr' ("Empty file: " ++ T.unpack url)
                            else case lookup ct imageContentTypes of
                                Nothing ->
                                    Nothing
                                        <$ lockHPutStrLn stderr' ("Unsupported content type: " ++ BSC.unpack ct)
                                Just ext -> do
                                    let bName = T.unpack (sanitizeNonAlNum '_' name') ++ ext
                                    let fPath = outDir </> bName
                                    BS.writeFile fPath bs
                                    pure $
                                        Just
                                            ( bName
                                            , def
                                                { emojiName = sanitizeNonAlNum '_' name'
                                                , emojiIsSensitive = Just False
                                                , emojiCategory = categoryBest
                                                , emojiAliases = aliases
                                                , emojiLocalOnly = Just False
                                                }
                                            )
            )
      where
        tallyOn :: (Ord b) => (a -> b) -> [a] -> [(a, Int)]
        tallyOn f input =
            let
                zipped = zip input (map f input)
                grouped = groupBy (\(_, b1) (_, b2) -> b1 == b2) zipped
             in
                map (\g -> (fst $ headPls g, length g)) grouped

data PoliteWaiter = PoliteWaiter NominalDiffTime (HashMap Text (MVar UTCTime))

delay :: NominalDiffTime -> IO ()
delay ms = threadDelay (round $ ms * 1000000)

pwWait :: MVar PoliteWaiter -> (String -> IO ()) -> Text -> IO ()
pwWait mvar say name = do
    PoliteWaiter ms m <- readMVar mvar
    case HM.lookup name m of
        Nothing -> do
            now <- getCurrentTime
            mv <- newMVar $ addUTCTime ms now
            modifyMVar_ mvar $ \(PoliteWaiter _ m') -> pure $ PoliteWaiter ms $ HM.insert name mv m'
        Just mv -> do
            now <- getCurrentTime
            modifyMVar_ mv $ \next -> do
                let diff =
                        diffUTCTime next now
                            & min ms
                if diff < 0
                    then pure next
                    else do
                        say $ "Waiting for " ++ T.unpack name ++ " for " ++ show diff ++ " seconds"
                        delay diff $> addUTCTime ms now

textMaybeStripPrefix :: Text -> Text -> Text
textMaybeStripPrefix prefix text = fromMaybe text $ T.stripPrefix prefix text

domainOf :: Text -> Text
domainOf url =
    url
        & textMaybeStripPrefix "https://"
        & textMaybeStripPrefix "http://"
        & T.takeWhile (/= '/')

pwWaitURL :: MVar PoliteWaiter -> (String -> IO ()) -> Text -> IO ()
pwWaitURL mvar say url = pwWait mvar say $ domainOf url

labelAbsProgress :: Label Int
labelAbsProgress =
    Label $
        \Progress
            { progressDone = done
            , progressTodo = total
            , progressCustom = err
            }
         _ -> TL.pack $ show (done - err) ++ "/" ++ show err ++ "/" ++ show total ++ "(OK/ERR/TOTAL)"

myModifyRequest :: (String -> IO ()) -> Request -> IO Request
myModifyRequest say req =
    say ("Requesting " ++ show (host req) ++ show (path req) ++ " HEADERS: " ++ show (requestHeaders req))
        $> setRequestHeaders
            [ ("User-Agent", "haskell-http-client/0.7 fedi-emoji-tool/0.1")
            , ("Accept", "application/json")
            , ("X-Contacts", "@yume@mi.yumechi.jp")
            ]
            req
                { decompress = const True
                }

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

timeIO :: (String -> IO ()) -> String -> IO a -> IO a
timeIO say msg act = do
    start <- getCurrentTime
    res <- act
    end <- getCurrentTime
    say $ msg ++ " took " ++ show (diffUTCTime end start)
    pure res

data MetaEmojiEntry = MetaEmojiEntry
    { metaEmojiDownloaded :: Bool
    , metaEmojiFileName :: Text
    , metaEmojiEmoji :: EmojiItem
    }
    deriving (Show, Generic)

instance FromJSON MetaEmojiEntry where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "metaEmoji"

instance ToJSON MetaEmojiEntry where
    toJSON = genericToJSON $ unTagCamelCaseOptions "metaEmoji"

data MetaEmojiPack = MetaEmojiPack
    { metaEmojiMetaVersion :: Int
    , metaEmojiHost :: Text
    , metaEmojiExportedAt :: UTCTime
    , metaEmojiEmojis :: [MetaEmojiEntry]
    }
    deriving (Show, Generic)

instance FromJSON MetaEmojiPack where
    parseJSON = genericParseJSON $ unTagCamelCaseOptions "metaEmoji"

instance ToJSON MetaEmojiPack where
    toJSON = genericToJSON $ unTagCamelCaseOptions "metaEmoji"

sanitizeNonAlNum :: Char -> Text -> Text
sanitizeNonAlNum repl = T.map (\c -> if isAsciiLower c || isAsciiUpper c || isDigit c then c else repl)

joinEither :: [Either a b] -> Either a [b]
joinEither [] = Right []
joinEither (x : xs) = case joinEither xs of
    Left e -> Left e
    Right xs' -> case x of
        Left e -> Left e
        Right x' -> Right (x' : xs')

doMetaIntersect :: FilePath -> FilePath -> Bool -> FilePath -> IO ()
doMetaIntersect in1 in2 negateopt out = do
    e1 <- eitherDecodeFileStrict in1
    e2 <- eitherDecodeFileStrict in2
    case joinEither [e1, e2] of
        Left e -> error e
        Right [MetaEmojiPack{metaEmojiEmojis = e1', metaEmojiHost = h1, metaEmojiExportedAt = et1}, MetaEmojiPack{metaEmojiEmojis = e2'}] -> do
            let e1Map = HM.fromList $ map (\e -> (emojiName $ metaEmojiEmoji e, metaEmojiEmoji e)) e1'
            let e2Map = HM.fromList $ map (\e -> (emojiName $ metaEmojiEmoji e, metaEmojiEmoji e)) e2'

            if negateopt
                then do
                    let diff = HM.difference e1Map e2Map
                    let intersect' = HM.intersection e1Map diff
                    let diff' = map (\(_, e) -> MetaEmojiEntry True (sanitizeNonAlNum '_' $ emojiName e) e) $ HM.toList intersect'
                    encodeFile out $ MetaEmojiPack 2 h1 et1 diff'
                else do
                    let common = HM.intersectionWith (,) e1Map e2Map
                    let common' =
                            map
                                ( \(a, _) ->
                                    MetaEmojiEntry
                                        True
                                        (sanitizeNonAlNum '_' $ "merged_" <> T.pack in1 <> "_" <> T.pack in2 <> "_" <> emojiName a)
                                        a
                                )
                                (HM.elems common)
                    encodeFile out $ MetaEmojiPack 2 h1 et1 common'
        _ -> error "unreachable"

doMetaMerge :: FilePath -> [FilePath] -> IO ()
doMetaMerge out inFiles =
    mapM eitherDecodeFileStrict inFiles
        >>= ( \case
                Left err -> error err
                Right [] -> error "No input files"
                Right (p : ps) ->
                    do
                        let host' = metaEmojiHost p
                        let emojis = concatMap metaEmojiEmojis (p : ps)
                        encodeFile out $
                            MetaEmojiPack 2 host' (metaEmojiExportedAt p) emojis
            )
            . joinEither

confirmIO :: String -> IO Bool
confirmIO msg = do
    putStr $ msg ++ " [y/N]: "
    getLine <&> \case
        "y" -> True
        "Y" -> True
        "yes" -> True
        "Yes" -> True
        "YES" -> True
        _ -> False

{-# ANN module ("HLint: ignore Redundant <&>" :: String) #-} -- hurts readability
main :: IO ()
main =
    customExecParser
        (prefs (showHelpOnEmpty <> showHelpOnError <> helpShowGlobals))
        parser
        >>= \(CommonOpts{homeOpt = home, paginationLimitOpt = limitVal, httpTimeoutOpt = timeoutVal, headOpt = headCount, verboseOpt = ver}, cmd) -> do
            token <- getEnvDefault "MISSKEY_TOKEN" ""
            let conf = if null token then unauthenticated home else bearerAuthenticated home (T.pack token)
            manager <- newManager tlsManagerSettings{managerResponseTimeout = responseTimeoutMicro $ timeoutVal * 1000000}
            case cmd of
                MetaIntersect in1 in2 negateopt out -> doMetaIntersect in1 in2 negateopt out
                MetaMerge out inFiles -> doMetaMerge out inFiles
                SelfEmojis -> listEmojis manager conf >>= print
                SelfInstances ->
                    concat
                        <$> whileForM
                            (paginationOptStream def{limit = limitVal})
                            ( listFederationInstances manager conf def
                                >=> ( \case
                                        Left err ->
                                            hPutStrLn
                                                stderr
                                                ("Error fetching instances: " ++ describeRequestError err)
                                                $> Nothing
                                        Right [] -> pure Nothing
                                        Right insts ->
                                            Just insts
                                                <$ forM_
                                                    insts
                                                    ( \inst ->
                                                        putStrLn $ "Instance: " ++ T.unpack (instanceDescribe inst)
                                                    )
                                    )
                            )
                        >>= \instances -> print ("# of instances: " ++ show (length instances))
                FetchRemoteEmojis outFilename -> do
                    hPutStrLn stderr "Fetching own emojis..."
                    myEmojis <- listEmojis manager conf >>= either (error . describeRequestError) pure
                    hPutStrLn stderr $ "Fetched " ++ show (length myEmojis) ++ " emojis"
                    hPutStrLn stderr "Fetching remote instances..."
                    let pagination = paginationOptStream def{limit = limitVal}
                    instances <-
                        concat
                            <$> whileForM
                                pagination
                                ( listFederationInstances manager conf def
                                    >=> ( \case
                                            Left err -> print err >> pure Nothing
                                            Right [] -> pure Nothing
                                            Right insts -> pure (Just insts)
                                        )
                                )
                    let misskeyInstances =
                            filter
                                ( maybe
                                    False
                                    ( \name' ->
                                        any
                                            (`T.isInfixOf` name')
                                            misskeyVariants
                                    )
                                    . fedSoftwareName
                                )
                                instances
                                & maybe id take headCount
                    let mastodonInstances =
                            filter
                                ( maybe
                                    False
                                    ( \name' ->
                                        any
                                            (`T.isInfixOf` name')
                                            MAS.mastodonVariants
                                    )
                                    . fedSoftwareName
                                )
                                instances
                                & maybe id take headCount
                    hPutStrLn stderr $
                        "Fetched "
                            ++ show (length instances)
                            ++ " instances, "
                            ++ show (length misskeyInstances)
                            ++ " are Misskey instances, "
                            ++ show (length mastodonInstances)
                            ++ " are Mastodon instances."
                    hPutStrLn stderr "Fetching remote emojis..."
                    pb <-
                        newProgressBar
                            defStyle{stylePrefix = labelAbsProgress}
                            5
                            (Progress 0 (length misskeyInstances + length mastodonInstances) 0)
                    let incError = updateProgress pb (\p -> p{progressCustom = progressCustom p + 1})
                    emap <- newMVar mempty
                    stdErrLock <- newMVar =<< (if ver then pure stderr else openFile "/dev/null" WriteMode)
                    dnsWaiter <- newMVar $ PoliteWaiter 0.1 HM.empty
                    let chunks = chunksOf 128 (zip (repeat FetchMisskeyAPI) misskeyInstances ++ zip (repeat FetchMastodonAPI) mastodonInstances)
                    forConcurrently_ chunks $
                        mapM_
                            ( \(ty, inst) ->
                                catch @IOException
                                    ( fetchWorker ty dnsWaiter stdErrLock inst manager emap >>= \case
                                        True -> pure ()
                                        False -> incError
                                    )
                                    ( \e ->
                                        incError >> hPutStrLn stderr ("Fatal Error on instance " ++ T.unpack (fedHost inst) ++ ": " ++ show e ++ ", skipping...")
                                    )
                                    >> incProgress pb 1
                            )

                    entries <-
                        mapToEntries
                            <$> readMVar emap
                    hPutStrLn stderr $ "Fetched " ++ show (length entries) ++ " unique emojis from " ++ show (length misskeyInstances) ++ " instances."
                    encodeFile outFilename entries
                PackRemoteEmojis inFilename outputDir hostDelay minThres limitMb -> do
                    createDirectoryIfMissing True outputDir
                    hPutStrLn stderr "Fetching own emojis..."
                    myEmojis <- listEmojis manager conf >>= either (error . describeRequestError) pure
                    hPutStrLn stderr $ "Local instance has " ++ show (length myEmojis) ++ " emojis."
                    emojiList <-
                        ( eitherDecodeFileStrict inFilename
                            <&> either (error . show) id
                            -- if the emoji is not seen at least 5 times
                            <&> filter (\e -> length (emojiMSeenAt e) >= minThres)
                            -- if we already have the emoji
                            <&> filter (\e -> not $ any (\e' -> emojiMName e == emojiName e') myEmojis)
                            -- if any instance say it's sensitive
                            <&> filter
                                ( \e -> Just True `notElem` (emojiIsSensitive <$> emojiMSeenAt e)
                                )
                        )
                            >>= shuffle
                                . maybe id take headCount

                    hPutStrLn stderr $ "Processing " ++ show (length emojiList) ++ " emojis..."

                    pb <-
                        newProgressBar
                            defStyle{stylePrefix = labelAbsProgress}
                            5
                            (Progress 0 (length emojiList) 0)
                    let incError = updateProgress pb (\p -> p{progressCustom = progressCustom p + 1})
                    pw <- newMVar $ PoliteWaiter (realToFrac hostDelay) HM.empty
                    stdoutLock <- newMVar stdout
                    stderrLock <- newMVar =<< (if ver then pure stderr else openFile "/dev/null" WriteMode)

                    let bigChunks = chunksOf 2048 emojiList

                    forM_ [0 .. length bigChunks - 1] $ \i ->
                        let chunkDir = outputDir </> "chunk-" ++ show i
                         in createDirectoryIfMissing True chunkDir

                    forM_ (zip [0 :: Int ..] bigChunks) $ \(chunkIdx, chunk) -> do
                        chunkMetas <- forConcurrently (chunksOf 128 chunk) $ mapM $ \entry -> do
                            remoteManager <-
                                newManager
                                    tlsManagerSettings
                                        { managerResponseTimeout = responseTimeoutMicro $ timeoutVal * 1000000
                                        , managerConnCount = 5 -- be more polite?
                                        , managerModifyRequest = myModifyRequest (lockHPutStrLn stderrLock)
                                        }
                            catch @IOException
                                ( packWorker
                                    pw
                                    (stdoutLock, stderrLock)
                                    remoteManager
                                    limitMb
                                    (outputDir </> "chunk-" ++ show chunkIdx)
                                    entry
                                    >>= \case
                                        Nothing -> incError $> Nothing
                                        r -> pure r
                                )
                                ( \e ->
                                    incError
                                        >> hPutStrLn
                                            stderr
                                            ("Fatal Error on emoji " ++ T.unpack (emojiMName entry) ++ ": " ++ show e ++ ", skipping...")
                                            $> Nothing
                                )
                                <* incProgress pb 1

                        ( MetaEmojiPack
                                2
                                "yume-emoji-tool"
                                <$> getCurrentTime
                                <*> pure
                                    ( map
                                        ( \(f, e) ->
                                            MetaEmojiEntry
                                                True
                                                (T.pack f)
                                                e
                                        )
                                        (catMaybes . concat $ chunkMetas)
                                    )
                            )
                            >>= encodeFile (outputDir </> "chunk-" ++ show chunkIdx </> "meta.json")
                ClearLocalOnlyEmojis ->
                    confirmIO "WARNING: This will delete all emojis marked local only, are you sure?" >>= \case
                        False -> hPutStrLn stderr "Aborted" >> exitFailure
                        True ->
                            let go (prevIds, untilId) =
                                    let leopts = def{listEmojisUntilId = untilId, listEmojisLimit = Just limitVal}
                                     in admListEmojis manager (bearerAuthenticated home (T.pack token)) leopts
                                            >>= either (error . describeRequestError) pure
                                            >>= \case
                                                [] -> pure (prevIds, Nothing)
                                                emojis ->
                                                    let nextIds = mapMaybe emojiId emojis
                                                     in case nextIds of
                                                            [] -> throwIO $ userError "Upstream failed to provide next token"
                                                            ix ->
                                                                let
                                                                    nextToken = last ix
                                                                    localOnlyEmojis = filter ((== Just True) . emojiLocalOnly) emojis
                                                                    emojiIds = mapMaybe emojiId localOnlyEmojis
                                                                 in
                                                                    (emojiIds ++ prevIds, Just nextToken)
                                                                        <$ hPutStrLn stderr ("Located " ++ show (length emojiIds) ++ " local-only emojis")
                             in when (null token) (throwIO $ userError "MISSKEY_TOKEN is not set")
                                    >> untilM (isNothing . snd) go ([], Nothing)
                                    >>= (mapM (admBulkDeleteEmojis manager conf) . filter (not . null))
                                        . nub
                                        . map fst
                                    >>= ( \case
                                            Left err ->
                                                throwIO $
                                                    userError $
                                                        "Error deleting emojis: " ++ describeRequestError err
                                            Right _ -> pure ()
                                        )
                                        . sequence
                DeleteFolder folder recursive -> do
                    confirmIO "DANGER: All notes attached to the folder will also be affected. Are you sure?" >>= \case
                        False -> hPutStrLn stderr "Aborted" >> exitFailure
                        True -> do
                            when recursive $
                                void $
                                    untilM
                                        (== False)
                                        ( \_ -> do
                                            files <- driveListFiles manager conf def{driveListFilesFolderId = Just (T.pack folder), driveListFilesLimit = Just limitVal}
                                            case files of
                                                Left err -> error $ describeRequestError err
                                                Right [] -> pure False
                                                Right resp -> do
                                                    let fs = map driveCreateResponseId resp
                                                    forConcurrently_ fs $ driveDeleteFile manager conf
                                                    pure True
                                        )
                                        True
                            driveDeleteFolder manager conf (T.pack folder) >> hPutStrLn stderr "Folder deleted"