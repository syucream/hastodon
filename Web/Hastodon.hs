module Web.Hastodon
  (
    mkHastodonClient

  , getAccountById
  , getCurrentAccount
  , getFollowers
  , getFollowing
  , getAccountStatuses
  , postFollow
  , postUnfollow
  , postBlock
  , postUnblock
  , postMute
  , postUnmute
  , getRelationships
  , getSearchedAccounts
  , postApps
  , getBlocks
  , getMutes
  , getRebloggedBy
  , getFavoritedBy
  , postStatus
  , postReblog
  , postUnreblog
  , postFavorite
  , postUnfavorite
  , getHomeTimeline
  , getPublicTimeline
  , getTaggedTimeline
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
pFollowers       = "/api/v1/accounts/:id/followers"
pFollowing       = "/api/v1/accounts/:id/following"
pAccountStatuses = "/api/v1/accounts/:id/statuses"
pFollow          = "/api/v1/accounts/:id/follow"
pUnfollow        = "/api/v1/accounts/:id/unfollow"
pBlock           = "/api/v1/accounts/:id/block"
pUnblock         = "/api/v1/accounts/:id/unblock"
pMute            = "/api/v1/accounts/:id/mute"
pUnmute          = "/api/v1/accounts/:id/unmute"
pRelationships   = "/api/v1/accounts/relationships"
pSearch          = "/api/v1/accounts/search"
pApps            = "/api/v1/apps"
pBlocks          = "/api/v1/blocks"
pMutes           = "/api/v1/mutes"
pRebloggedBy     = "/api/v1/statuses/:id/reblogged_by"
pFavoritedBy     = "/api/v1/statuses/:id/favourited_by"
pStatuses        = "/api/v1/statuses"
pDeleteStatus    = "/api/v1/statuses/:id"
pHomeTimeline    = "/api/v1/timelines/home"
pPublicTimeline  = "/api/v1/timelines/public"
pReblog          = "/api/v1/statuses/:id/reblog"
pUnreblog        = "/api/v1/statuses/:id/unreblog"
pFavorite        = "/api/v1/statuses/:id/favourite"
pUnfavorite      = "/api/v1/statuses/:id/unfavourite"
pTaggedTimeline  = "/api/v1/timelines/tag/:hashtag"

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

data Application = Application {
  applicationName :: String,
  applicationWebsite :: Maybe String
} deriving (Show)
instance FromJSON Application where
  parseJSON (Object v) =
    Application <$> (v .:  T.pack "name")
                <*> (v .:? T.pack "website")

data OAuthClient = OAuthClient {
  oauthClientId :: Int,
  oauthClientRedirectUri :: String,
  oauthClientClientId :: String,
  oauthClientClientSecret :: String
} deriving (Show)
instance FromJSON OAuthClient where
  parseJSON (Object v) =
    OAuthClient <$> (v .: T.pack "id")
                <*> (v .: T.pack "redirect_uri")
                <*> (v .: T.pack "client_id")
                <*> (v .: T.pack "client_secret")

data Attachment = Attachment {
  attachmentId :: Int,
  attachmentType :: String,
  attachmentUrl :: String,
  attachmentRemoteUrl :: String,
  attachmentPreviewUrl :: String,
  attachmentTextUrl :: Maybe String
} deriving (Show)
instance FromJSON Attachment where
  parseJSON (Object v) =
    Attachment <$> (v .:  T.pack "id")
               <*> (v .:  T.pack "type")
               <*> (v .:  T.pack "url")
               <*> (v .:  T.pack "remote_url")
               <*> (v .:  T.pack "preview_url")
               <*> (v .:? T.pack "text_url")

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

data Mention = Mention {
  mentionUrl :: String,
  mentionUsername :: String,
  mentionAcct :: String,
  mentionId :: Int
} deriving (Show)
instance FromJSON Mention where
  parseJSON (Object v) =
    Mention <$> (v .: T.pack "url")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "id")

data Tag = Tag {
  name :: String,
  url :: String
} deriving (Show)
instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: T.pack "name")
        <*> (v .: T.pack "url")

data Status = Status {
  statusId :: Int,
  statusUri :: String,
  statusUrl :: String,
  statusAccount :: Account,
  statusInReplyToId :: Maybe Int,
  statusInReplyToAccountId :: Maybe Int,
  statusReblog :: Maybe Status,
  statusContent :: String,
  statusCreatedAt :: String,
  statusReblogsCount :: Int,
  statusFavouritesCount :: Int,
  statusReblogged :: Maybe Bool,
  statusFavourited :: Maybe Bool,
  statusSensitive :: Maybe Bool,
  statusSpoilerText :: String,
  statusVisibility :: String,
  statusMediaAttachments :: [Attachment],
  statusMentions :: [Mention],
  statusTags :: [Tag],
  statusApplication :: Maybe Application
} deriving (Show)
instance FromJSON Status where
  parseJSON (Object v) =
    Status <$> (v .:  T.pack "id")
           <*> (v .:  T.pack "uri")
           <*> (v .:  T.pack "url")
           <*> (v .:  T.pack "account")
           <*> (v .:? T.pack "in_reply_to_id")
           <*> (v .:? T.pack "in_reply_to_account_id")
           <*> (v .:? T.pack "reblog")
           <*> (v .:  T.pack "content")
           <*> (v .:  T.pack "created_at")
           <*> (v .:  T.pack "reblogs_count")
           <*> (v .:  T.pack "favourites_count")
           <*> (v .:? T.pack "reblogged")
           <*> (v .:? T.pack "favourited")
           <*> (v .:? T.pack "sensitive")
           <*> (v .:  T.pack "spoiler_text")
           <*> (v .:  T.pack "visibility")
           <*> (v .:  T.pack "media_attachments")
           <*> (v .:  T.pack "mentions")
           <*> (v .:  T.pack "tags")
           <*> (v .:? T.pack "application")

-- 
-- helpers
-- 

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

postAndGetHastodonResponseJSON path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  httpJSON req

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

-- TODO support options
getFollowers :: Int -> HastodonClient -> IO [Account]
getFollowers id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFollowers) client
  return (getResponseBody res :: [Account])

getFollowing :: Int -> HastodonClient -> IO [Account]
getFollowing id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFollowing) client
  return (getResponseBody res :: [Account])

getAccountStatuses :: Int -> HastodonClient -> IO [Status]
getAccountStatuses id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountStatuses) client
  return (getResponseBody res :: [Status])

getRelationships :: [Int] -> HastodonClient -> IO [Relationship]
getRelationships ids client = do
  let intIds = map (show) ids
  let params = foldl (\x y -> x ++ (if x == "" then "?" else "&") ++ "id%5b%5d=" ++ y) "" intIds
  res <- getHastodonResponseJSON (pRelationships ++ params) client
  return (getResponseBody res :: [Relationship])

getSearchedAccounts :: String -> HastodonClient -> IO [Account]
getSearchedAccounts query client = do
  res <- getHastodonResponseJSON (pSearch ++ "?q=" ++ query) client
  return (getResponseBody res :: [Account])

postFollow :: Int -> HastodonClient -> IO Relationship
postFollow id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFollow) [] client
  return (getResponseBody res :: Relationship)

postUnfollow :: Int -> HastodonClient -> IO Relationship
postUnfollow id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfollow) [] client
  return (getResponseBody res :: Relationship)

postBlock :: Int -> HastodonClient -> IO Relationship
postBlock id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pBlock) [] client
  return (getResponseBody res :: Relationship)

postUnblock :: Int -> HastodonClient -> IO Relationship
postUnblock id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnblock) [] client
  return (getResponseBody res :: Relationship)

postMute :: Int -> HastodonClient -> IO Relationship
postMute id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pMute) [] client
  return (getResponseBody res :: Relationship)

postUnmute :: Int -> HastodonClient -> IO Relationship
postUnmute id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnmute) [] client
  return (getResponseBody res :: Relationship)

postApps :: String -> HastodonClient -> IO OAuthClient
postApps clientName client = do
  let reqBody = [(Char8.pack "client_name", Char8.pack clientName),
                 (Char8.pack "redirect_uris", Char8.pack "urn:ietf:wg:oauth:2.0:oob"),
                 (Char8.pack "scopes", Char8.pack "read write follow")]
  res <- postAndGetHastodonResponseJSON pApps reqBody client
  return (getResponseBody res :: OAuthClient)

getBlocks :: HastodonClient -> IO [Account]
getBlocks client = do
  res <- getHastodonResponseJSON pBlocks client
  return (getResponseBody res :: [Account])

getMutes :: HastodonClient -> IO [Account]
getMutes client = do
  res <- getHastodonResponseJSON pMutes client
  return (getResponseBody res :: [Account])

getRebloggedBy :: Int -> HastodonClient -> IO [Account]
getRebloggedBy id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pRebloggedBy) client
  return (getResponseBody res :: [Account])

getFavoritedBy :: Int -> HastodonClient -> IO [Account]
getFavoritedBy id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFavoritedBy) client
  return (getResponseBody res :: [Account])

postStatus :: String -> HastodonClient -> IO Status
postStatus status client = do
  res <- postAndGetHastodonResponseJSON pStatuses [(Char8.pack "status", Char8.pack status)] client
  return (getResponseBody res :: Status)

postReblog :: Int -> HastodonClient -> IO Status
postReblog id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pReblog) [] client
  return (getResponseBody res :: Status)

postUnreblog :: Int -> HastodonClient -> IO Status
postUnreblog id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnreblog) [] client
  return (getResponseBody res :: Status)

postFavorite :: Int -> HastodonClient -> IO Status
postFavorite id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFavorite) [] client
  return (getResponseBody res :: Status)

postUnfavorite :: Int -> HastodonClient -> IO Status
postUnfavorite id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfavorite) [] client
  return (getResponseBody res :: Status)

getHomeTimeline :: HastodonClient -> IO [Status]
getHomeTimeline client = do
  res <- getHastodonResponseJSON pHomeTimeline client
  return (getResponseBody res :: [Status])

getPublicTimeline :: HastodonClient -> IO [Status]
getPublicTimeline client = do
  res <- getHastodonResponseJSON pPublicTimeline client
  return (getResponseBody res :: [Status])

getTaggedTimeline :: String -> HastodonClient -> IO [Status]
getTaggedTimeline hashtag client = do
  res <- getHastodonResponseJSON (replace ":hashtag" hashtag pTaggedTimeline) client
  return (getResponseBody res :: [Status])
