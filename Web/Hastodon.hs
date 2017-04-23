{-# LANGUAGE DeriveGeneric #-}

module Web.Hastodon
  (
    mkHastodonClient

  , getAccountById
  , getCurrentAccount
  , getRelationships
  , postStatuses
  , getHomeTimeline
  , getPublicTimeline
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as T
import Data.String.Utils
import Network.HTTP.Simple
import Network.HTTP.Types.Header

--
-- Mastodon API endpoints
--
pAccountById     = "/api/v1/accounts/:id"
pCurrentAccounts = "/api/v1/accounts/verify_credentials"
pRelationships   = "/api/v1/accounts/relationships"
pStatuses        = "/api/v1/statuses"
pHomeTimeline    = "/api/v1/timelines/home"
pPublicTimeline  = "/api/v1/timelines/public"

data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}

data Account = Account {
  accountId :: Int,
  accountUsername :: String,
  accountAcct :: String,
  accountDisplayName :: String,
  accountLocked :: Bool,
  accountCreatedAt :: String,
  accountFollowersCount :: Int,
  accountFollowingCount :: Int,
  accountStatusesCount :: Int,
  accountNote :: String,
  accountUrl :: String,
  accountAvatar :: String,
  accountAvatarStatic :: String,
  accountHeader :: String,
  accountHeaderStatic :: String
} deriving (Show)
instance FromJSON Account where
  parseJSON (Object v) =
    Account <$> (v .: T.pack "id")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "display_name")
            <*> (v .: T.pack "locked")
            <*> (v .: T.pack "created_at")
            <*> (v .: T.pack "followers_count")
            <*> (v .: T.pack "following_count")
            <*> (v .: T.pack "statuses_count")
            <*> (v .: T.pack "note")
            <*> (v .: T.pack "url")
            <*> (v .: T.pack "avatar")
            <*> (v .: T.pack "avatar_static")
            <*> (v .: T.pack "header")
            <*> (v .: T.pack "header_static")

data Relationship = Relationship {
  relationshipId :: Int,
  relationshipFollowing :: Bool,
  relationshipFollowed_by :: Bool,
  relationshipBlocking :: Bool,
  relationshipMuting :: Bool,
  relationshipRequested :: Bool
} deriving (Show)
instance FromJSON Relationship where
  parseJSON (Object v) =
    Relationship <$> (v .: T.pack "id")
                 <*> (v .: T.pack "following")
                 <*> (v .: T.pack "followed_by")
                 <*> (v .: T.pack "blocking")
                 <*> (v .: T.pack "muting")
                 <*> (v .: T.pack "requested")

-- TODO
-- data Status = Status {
--   id :: Int,
--   uri :: String,
--   url :: String,
--   account :: Account,
--   in_reply_to_id :: Maybe Int,
--   in_reply_to_account_id :: Maybe Int,
--   reblog :: Maybe Status,
--   content :: String,
--   created_at :: String,
--   reblogs_count :: Int,
--   favourites_count :: Int,
--   reblogged :: Bool,
--   favourited :: Bool,
--   sensitive :: Bool,
--   spoiler_text :: String,
--   visibility :: String,
--   media_attachments :: Attachments,
--   mentions :: Mentions,
--   tags :: Tags,
--   application :: Application
-- }

-- 
-- helpers
-- 

toIOString :: LChar8.ByteString -> IO String
toIOString bs = return $ LChar8.unpack bs

mkHastodonHeader :: String -> Request -> Request
mkHastodonHeader token =
  addRequestHeader hAuthorization $ Char8.pack $ "Bearer " ++ token

mkHastodonRequest :: String -> HastodonClient -> IO Request
mkHastodonRequest path client = do
  initReq <- parseRequest $ "https://" ++ (host client) ++ path
  return $ mkHastodonHeader (token client) $ initReq

getHastodonResponseBody :: String -> HastodonClient -> IO LChar8.ByteString
getHastodonResponseBody path client = do
  req <- mkHastodonRequest path client
  res <- httpLBS req
  return $ getResponseBody res

getHastodonResponseJSON path client = mkHastodonRequest path client >>= httpJSON

postHastodonRequestBody :: String -> [(Char8.ByteString, Char8.ByteString)] -> HastodonClient -> IO String
postHastodonRequestBody path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  res <- httpLBS req
  return $ LChar8.unpack $ getResponseBody res

-- 
-- exported functions
-- 

mkHastodonClient :: String -> String -> HastodonClient
mkHastodonClient host token = HastodonClient host token

getAccountById :: Int -> HastodonClient -> IO Account
getAccountById id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountById) client
  return (getResponseBody res :: Account)

getCurrentAccount :: HastodonClient -> IO Account
getCurrentAccount client = do
  res <- getHastodonResponseJSON pCurrentAccounts client
  return (getResponseBody res :: Account)

getRelationships :: [Int] -> HastodonClient -> IO [Relationship]
getRelationships ids client = do
  let intIds = map (show) ids
  let params = foldl (\x y -> x ++ (if x == "" then "?" else "&") ++ "id%5b%5d=" ++ y) "" intIds
  res <- getHastodonResponseJSON (pRelationships ++ params) client
  return (getResponseBody res :: [Relationship])

postStatuses :: String -> HastodonClient -> IO String
postStatuses statuses = postHastodonRequestBody pStatuses [(Char8.pack "status", Char8.pack statuses)]

-- TODO return IO Status
getHomeTimeline :: HastodonClient -> IO String
getHomeTimeline client = getHastodonResponseBody pHomeTimeline client >>= toIOString

-- TODO return IO Status
getPublicTimeline :: HastodonClient -> IO String
getPublicTimeline client = getHastodonResponseBody pPublicTimeline client >>= toIOString
