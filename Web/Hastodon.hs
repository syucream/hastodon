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
  , getFavorites
  , getFollowRequests
  , postAuthorizeRequest
  , postRejectRequest
  , getInstance
  , getMutes
  , getNotifications
  , getNotificationById
  , postNotificationsClear
  , getReports
  , getSearchedResults
  , getStatus
  , getCard
  , getContext
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
pAccountById       = "/api/v1/accounts/:id"
pCurrentAccounts   = "/api/v1/accounts/verify_credentials"
pFollowers         = "/api/v1/accounts/:id/followers"
pFollowing         = "/api/v1/accounts/:id/following"
pAccountStatuses   = "/api/v1/accounts/:id/statuses"
pFollow            = "/api/v1/accounts/:id/follow"
pUnfollow          = "/api/v1/accounts/:id/unfollow"
pBlock             = "/api/v1/accounts/:id/block"
pUnblock           = "/api/v1/accounts/:id/unblock"
pMute              = "/api/v1/accounts/:id/mute"
pUnmute            = "/api/v1/accounts/:id/unmute"
pRelationships     = "/api/v1/accounts/relationships"
pSearchAccounts    = "/api/v1/accounts/search"
pApps              = "/api/v1/apps"
pBlocks            = "/api/v1/blocks"
pFavorites         = "/api/v1/favourites"
pFollowRequests    = "/api/v1/follow_requests"
pAuthorizeRequest  = "/api/v1/follow_requests/:id/authorize"
pRejectRequest     = "/api/v1/follow_requests/:id/reject"
pInstance          = "/api/v1/instance"
pMutes             = "/api/v1/mutes"
pNotifications     = "/api/v1/notifications"
pNotificationById  = "/api/v1/notifications/:id"
pNotificationClear = "/api/v1/notifications/clear"
pReports           = "/api/v1/reports"
pSearch            = "/api/v1/search"
pStatus            = "/api/v1/statuses/:id"
pContext           = "/api/v1/statuses/:id/context"
pCard              = "/api/v1/statuses/:id/card"
pRebloggedBy       = "/api/v1/statuses/:id/reblogged_by"
pFavoritedBy       = "/api/v1/statuses/:id/favourited_by"
pStatuses          = "/api/v1/statuses"
pDeleteStatus      = "/api/v1/statuses/:id"
pHomeTimeline      = "/api/v1/timelines/home"
pPublicTimeline    = "/api/v1/timelines/public"
pReblog            = "/api/v1/statuses/:id/reblog"
pUnreblog          = "/api/v1/statuses/:id/unreblog"
pFavorite          = "/api/v1/statuses/:id/favourite"
pUnfavorite        = "/api/v1/statuses/:id/unfavourite"
pTaggedTimeline    = "/api/v1/timelines/tag/:hashtag"

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

data Card = Card {
  cardUrl :: String,
  cardTitle :: String,
  cardDescription :: String,
  cardImage :: String
} deriving (Show)
instance FromJSON Card where
  parseJSON (Object v) =
    Card <$> (v .: T.pack "url")
         <*> (v .: T.pack "title")
         <*> (v .: T.pack "description")
         <*> (v .: T.pack "image")

data Context = Context {
  contextAncestors :: [Status],
  contextDescendants :: [Status]
} deriving (Show)
instance FromJSON Context where
  parseJSON (Object v) =
    Context <$> (v .: T.pack "ancestors")
            <*> (v .: T.pack "descendants")

data Instance = Instance {
  instanceUri :: String,
  instanceTitle :: String,
  instanceDescription :: String,
  instanceEmail :: String
} deriving (Show)
instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> (v .: T.pack "uri")
             <*> (v .: T.pack "title")
             <*> (v .: T.pack "description")
             <*> (v .: T.pack "email")

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

data Notification = Notification {
  notificationId :: Int,
  notificationType :: String,
  notificationCreatedAt :: String,
  notificationAccount :: Account,
  notificationStatus :: Maybe Status
} deriving (Show)
instance FromJSON Notification where
  parseJSON (Object v) =
    Notification <$> (v .:  T.pack "id")
                 <*> (v .:  T.pack "type")
                 <*> (v .:  T.pack "created_at")
                 <*> (v .:  T.pack "account")
                 <*> (v .:? T.pack "status")

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

data Report = Report {
  reportId :: Int,
  reportActionToken :: String
} deriving (Show)
instance FromJSON Report where
  parseJSON (Object v) =
    Report <$> (v .: T.pack "id")
           <*> (v .: T.pack "action_taken")

data Results = Results {
  resultAccounts :: [Account],
  resultStatus :: [Status],
  resultHashtags :: [String]
} deriving (Show)
instance FromJSON Results where
  parseJSON (Object v) =
    Results <$> (v .: T.pack "accounts")
            <*> (v .: T.pack "statuses")
            <*> (v .: T.pack "hashtags")

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

data Tag = Tag {
  name :: String,
  url :: String
} deriving (Show)
instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: T.pack "name")
        <*> (v .: T.pack "url")

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

getHastodonResponseJSON path client = mkHastodonRequest path client >>= httpJSONEither

postAndGetHastodonResult path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  res <- httpNoBody req
  return $ (getResponseStatusCode res) == 200

postAndGetHastodonResponseJSON path body client = do
  initReq <- mkHastodonRequest path client
  let req = setRequestBodyURLEncoded body $ initReq
  httpJSONEither req

-- 
-- exported functions
-- 

mkHastodonClient :: String -> String -> HastodonClient
mkHastodonClient host token = HastodonClient host token

getAccountById :: Int -> HastodonClient -> IO (Either JSONException Account)
getAccountById id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountById) client
  return (getResponseBody res :: Either JSONException Account)

getCurrentAccount :: HastodonClient -> IO (Either JSONException Account)
getCurrentAccount client = do
  res <- getHastodonResponseJSON pCurrentAccounts client
  return (getResponseBody res :: Either JSONException Account)

-- TODO support options
getFollowers :: Int -> HastodonClient -> IO (Either JSONException [Account])
getFollowers id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFollowers) client
  return (getResponseBody res :: Either JSONException [Account])

getFollowing :: Int -> HastodonClient -> IO (Either JSONException [Account])
getFollowing id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFollowing) client
  return (getResponseBody res :: Either JSONException [Account])

getAccountStatuses :: Int -> HastodonClient -> IO (Either JSONException [Status])
getAccountStatuses id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pAccountStatuses) client
  return (getResponseBody res :: Either JSONException [Status])

getRelationships :: [Int] -> HastodonClient -> IO (Either JSONException [Relationship])
getRelationships ids client = do
  let intIds = map (show) ids
  let params = foldl (\x y -> x ++ (if x == "" then "?" else "&") ++ "id%5b%5d=" ++ y) "" intIds
  res <- getHastodonResponseJSON (pRelationships ++ params) client
  return (getResponseBody res :: Either JSONException [Relationship])

getSearchedAccounts :: String -> HastodonClient -> IO (Either JSONException [Account])
getSearchedAccounts query client = do
  res <- getHastodonResponseJSON (pSearchAccounts ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Account])

postFollow :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postFollow id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnfollow :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postUnfollow id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postBlock :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postBlock id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pBlock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnblock :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postUnblock id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnblock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postMute :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postMute id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pMute) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnmute :: Int -> HastodonClient -> IO (Either JSONException Relationship)
postUnmute id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnmute) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postApps :: String -> HastodonClient -> IO (Either JSONException OAuthClient)
postApps clientName client = do
  let reqBody = [(Char8.pack "client_name", Char8.pack clientName),
                 (Char8.pack "redirect_uris", Char8.pack "urn:ietf:wg:oauth:2.0:oob"),
                 (Char8.pack "scopes", Char8.pack "read write follow")]
  res <- postAndGetHastodonResponseJSON pApps reqBody client
  return (getResponseBody res :: Either JSONException OAuthClient)

getBlocks :: HastodonClient -> IO (Either JSONException [Account])
getBlocks client = do
  res <- getHastodonResponseJSON pBlocks client
  return (getResponseBody res :: Either JSONException [Account])

getFavorites :: HastodonClient -> IO (Either JSONException [Status])
getFavorites client = do
  res <- getHastodonResponseJSON pFavorites client
  return (getResponseBody res :: Either JSONException [Status])

getFollowRequests :: HastodonClient -> IO (Either JSONException [Account])
getFollowRequests client = do
  res <- getHastodonResponseJSON pFollowRequests client
  return (getResponseBody res :: Either JSONException [Account])

postAuthorizeRequest :: Int -> HastodonClient -> IO Bool
postAuthorizeRequest id = postAndGetHastodonResult (replace ":id" (show id) pAuthorizeRequest) []

postRejectRequest :: Int -> HastodonClient -> IO Bool
postRejectRequest id = postAndGetHastodonResult (replace ":id" (show id) pRejectRequest) []

getInstance :: HastodonClient -> IO (Either JSONException Instance)
getInstance client = do
  res <- getHastodonResponseJSON pInstance client
  return (getResponseBody res :: Either JSONException Instance)

getMutes :: HastodonClient -> IO (Either JSONException [Account])
getMutes client = do
  res <- getHastodonResponseJSON pMutes client
  return (getResponseBody res :: Either JSONException [Account])

getNotifications :: HastodonClient -> IO (Either JSONException [Notification])
getNotifications client = do
  res <- getHastodonResponseJSON pNotifications client
  return (getResponseBody res :: Either JSONException [Notification])

getNotificationById :: Int -> HastodonClient -> IO (Either JSONException Notification)
getNotificationById id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pNotificationById) client
  return (getResponseBody res :: Either JSONException Notification)

postNotificationsClear :: HastodonClient -> IO Bool
postNotificationsClear = postAndGetHastodonResult pNotificationClear []

getReports :: HastodonClient -> IO (Either JSONException [Report])
getReports client = do
  res <- getHastodonResponseJSON pReports client
  return (getResponseBody res :: Either JSONException [Report])

getSearchedResults :: String -> HastodonClient -> IO (Either JSONException [Results])
getSearchedResults query client = do
  res <- getHastodonResponseJSON (pSearch ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Results])

getStatus :: Int -> HastodonClient -> IO (Either JSONException Status)
getStatus id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pStatus) client
  return (getResponseBody res :: Either JSONException Status)

getCard :: Int -> HastodonClient -> IO (Either JSONException Card)
getCard id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pCard) client
  return (getResponseBody res :: Either JSONException Card)

getContext :: Int -> HastodonClient -> IO (Either JSONException Context)
getContext id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pContext) client
  return (getResponseBody res :: Either JSONException Context)

getRebloggedBy :: Int -> HastodonClient -> IO (Either JSONException [Account])
getRebloggedBy id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pRebloggedBy) client
  return (getResponseBody res :: Either JSONException [Account])

getFavoritedBy :: Int -> HastodonClient -> IO (Either JSONException [Account])
getFavoritedBy id client = do
  res <- getHastodonResponseJSON (replace ":id" (show id) pFavoritedBy) client
  return (getResponseBody res :: Either JSONException [Account])

postStatus :: String -> HastodonClient -> IO (Either JSONException Status)
postStatus status client = do
  res <- postAndGetHastodonResponseJSON pStatuses [(Char8.pack "status", Char8.pack status)] client
  return (getResponseBody res :: Either JSONException Status)

postReblog :: Int -> HastodonClient -> IO (Either JSONException Status)
postReblog id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pReblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnreblog :: Int -> HastodonClient -> IO (Either JSONException Status)
postUnreblog id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnreblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postFavorite :: Int -> HastodonClient -> IO (Either JSONException Status)
postFavorite id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pFavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnfavorite :: Int -> HastodonClient -> IO (Either JSONException Status)
postUnfavorite id client = do
  res <- postAndGetHastodonResponseJSON (replace ":id" (show id) pUnfavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

getHomeTimeline :: HastodonClient -> IO (Either JSONException [Status])
getHomeTimeline client = do
  res <- getHastodonResponseJSON pHomeTimeline client
  return (getResponseBody res :: Either JSONException [Status])

getPublicTimeline :: HastodonClient -> IO (Either JSONException [Status])
getPublicTimeline client = do
  res <- getHastodonResponseJSON pPublicTimeline client
  return (getResponseBody res :: Either JSONException [Status])

getTaggedTimeline :: String -> HastodonClient -> IO (Either JSONException [Status])
getTaggedTimeline hashtag client = do
  res <- getHastodonResponseJSON (replace ":hashtag" hashtag pTaggedTimeline) client
  return (getResponseBody res :: Either JSONException [Status])
