module Web.Hastodon.API
  ( mkHastodonClient
  , getAccountById
  , getCurrentAccount
  , getFollowers
  , getFollowersWithOption
  , getFollowing
  , getFollowingWithOption
  , getAccountStatuses
  , getAccountStatusesWithOption
  , postFollow
  , postUnfollow
  , postBlock
  , postUnblock
  , postMute
  , postMuteWithOption
  , postUnmute
  , getRelationships
  , getSearchedAccounts
  , getSearchedAccountsWithOption
  , postApps
  , getBlocks
  , getBlocksWithOption
  , getFavorites
  , getFavoritesWithOption
  , getFollowRequests
  , getFollowRequestsWithOption
  , postAuthorizeRequest
  , postRejectRequest
  , getInstance
  , postMediaFile
  , getMutes
  , getMutesWithOption
  , getNotifications
  , getNotificationsWithOption
  , getNotificationById
  , postNotificationsClear
  , getReports
  , getSearchedResults
  , getSearchedResultsWithOption
  , getStatus
  , getCard
  , getContext
  , getRebloggedBy
  , getRebloggedByWithOption
  , getFavoritedBy
  , getFavoritedByWithOption
  , postStatus
  , postStatusWithOption
  , postStatusWithMediaIds
  , postReblog
  , postUnreblog
  , postFavorite
  , postUnfavorite
  , getHomeTimeline
  , getHomeTimelineWithOption
  , getPublicTimeline
  , getPublicTimelineWithOption
  , getTaggedTimeline
  , getTaggedTimelineWithOption
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as LChar8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.String.Utils
import Network.HTTP.Simple
import Network.HTTP.Client.MultipartFormData
import Network.Mime

import Web.Hastodon.Types
import Web.Hastodon.Util
import Web.Hastodon.Option


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
pMedia             = "/api/v1/media"
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


--
-- helpers
--

getOAuthToken :: String -> String -> String -> String -> String -> IO (Either JSONException OAuthResponse)
getOAuthToken clientId clientSecret username password host = do
  initReq <- parseRequest $ "https://" ++ host ++ "/oauth/token"
  let reqBody = [(Char8.pack "client_id", utf8ToChar8 clientId),
                 (Char8.pack "client_secret", utf8ToChar8 clientSecret),
                 (Char8.pack "username", utf8ToChar8 username),
                 (Char8.pack "password", utf8ToChar8 password),
                 (Char8.pack "grant_type", Char8.pack "password"),
                 (Char8.pack "scope", Char8.pack "read write follow")]
  let req = setRequestBodyURLEncoded reqBody $ initReq
  res <- httpJSONEither req
  return $ (getResponseBody res :: Either JSONException OAuthResponse)

getHastodonResponseBody :: String -> HastodonClient -> IO LChar8.ByteString
getHastodonResponseBody path client = do
  req <- mkHastodonRequest path client
  res <- httpLBS req
  return $ getResponseBody res

getHastodonResponseJSONWithOption opt path client =
  mkHastodonRequestWithQuery opt path client >>= httpJSONEither

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

mkHastodonClient :: String -> String -> String -> String -> String -> IO (Maybe HastodonClient)
mkHastodonClient clientId clientSecret username password host = do
  oauthRes <- getOAuthToken clientId clientSecret username password host
  case oauthRes of
    Left err -> return $ Nothing
    Right oauthData -> return $ Just $ HastodonClient host (accessToken oauthData)

getAccountById :: HastodonClient -> HastodonId -> IO (Either JSONException Account)
getAccountById client id = do
  res <- getHastodonResponseJSON (replace ":id" id pAccountById) client
  return (getResponseBody res :: Either JSONException Account)

getCurrentAccount :: HastodonClient -> IO (Either JSONException Account)
getCurrentAccount client = do
  res <- getHastodonResponseJSON pCurrentAccounts client
  return (getResponseBody res :: Either JSONException Account)

getFollowers :: HastodonClient -> String -> IO (Either JSONException [Account])
getFollowers client = getFollowersWithOption client mempty

getFollowersWithOption :: HastodonClient -> RangeOption -> HastodonId -> IO (Either JSONException [Account])
getFollowersWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" id pFollowers)
           client
  return (getResponseBody res :: Either JSONException [Account])

getFollowing :: HastodonClient -> String -> IO (Either JSONException [Account])
getFollowing client = getFollowingWithOption client mempty

getFollowingWithOption :: HastodonClient -> RangeOption -> HastodonId -> IO (Either JSONException [Account])
getFollowingWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" id pFollowing)
           client
  return (getResponseBody res :: Either JSONException [Account])

getAccountStatusesWithOption :: HastodonClient -> GetAccountStatusesOption -> HastodonId -> IO (Either JSONException [Status])
getAccountStatusesWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":id" id pAccountStatuses)
           client
  return (getResponseBody res :: Either JSONException [Status])

getAccountStatuses :: HastodonClient -> HastodonId -> IO (Either JSONException [Status])
getAccountStatuses client = getAccountStatusesWithOption client mempty

getRelationships :: HastodonClient -> [HastodonId] ->  IO (Either JSONException [Relationship])
getRelationships client ids = do
  let params = foldl (\x y -> x ++ (if x == "" then "?" else "&") ++ "id%5b%5d=" ++ y) "" ids
  res <- getHastodonResponseJSON (pRelationships ++ params) client
  return (getResponseBody res :: Either JSONException [Relationship])

getSearchedAccountsWithOption ::
  HastodonClient -> AccountSearchOption -> HastodonId -> IO (Either JSONException [Account])
getSearchedAccountsWithOption client opt query = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) (pSearchAccounts ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Account])

getSearchedAccounts :: HastodonClient -> HastodonId -> IO (Either JSONException [Account])
getSearchedAccounts client = getSearchedAccountsWithOption client mempty

postFollow :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postFollow client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pFollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnfollow :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postUnfollow client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pUnfollow) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postBlock :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postBlock client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pBlock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postUnblock :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postUnblock client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pUnblock) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postMuteWithOption ::
  HastodonClient -> PostMuteOption -> HastodonId -> IO (Either JSONException Relationship)
postMuteWithOption client opt id = do
  let prms = optionAsForm opt
  res <- postAndGetHastodonResponseJSON (replace ":id" id pMute) prms client
  return (getResponseBody res :: Either JSONException Relationship)

postMute :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postMute client = postMuteWithOption client mempty

postUnmute :: HastodonClient -> HastodonId -> IO (Either JSONException Relationship)
postUnmute client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pUnmute) [] client
  return (getResponseBody res :: Either JSONException Relationship)

postApps :: String -> String -> IO (Either JSONException OAuthClient)
postApps host clientName = do
  let reqBody = [(Char8.pack "client_name", utf8ToChar8 clientName),
                 (Char8.pack "redirect_uris", Char8.pack "urn:ietf:wg:oauth:2.0:oob"),
                 (Char8.pack "scopes", Char8.pack "read write follow")]
  initReq <- parseRequest $ "https://" ++ host ++ pApps
  let req = setRequestBodyURLEncoded reqBody $ initReq
  res <- httpJSONEither req
  return (getResponseBody res :: Either JSONException OAuthClient)

getBlocksWithOption :: HastodonClient -> RangeOption -> IO (Either JSONException [Account])
getBlocksWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pBlocks client
  return (getResponseBody res :: Either JSONException [Account])

getBlocks :: HastodonClient -> IO (Either JSONException [Account])
getBlocks client = getBlocksWithOption client mempty

getFavoritesWithOption :: HastodonClient -> RangeOption -> IO (Either JSONException [Status])
getFavoritesWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pFavorites client
  return (getResponseBody res :: Either JSONException [Status])

getFavorites :: HastodonClient -> IO (Either JSONException [Status])
getFavorites client = getFavoritesWithOption client mempty

getFollowRequestsWithOption :: HastodonClient -> RangeOption -> IO (Either JSONException [Account])
getFollowRequestsWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pFollowRequests client
  return (getResponseBody res :: Either JSONException [Account])

getFollowRequests :: HastodonClient -> IO (Either JSONException [Account])
getFollowRequests client = getFollowRequestsWithOption client mempty

postAuthorizeRequest :: HastodonClient -> HastodonId ->  IO Bool
postAuthorizeRequest client id = postAndGetHastodonResult (replace ":id" id pAuthorizeRequest) [] client

postRejectRequest :: HastodonClient -> HastodonId -> IO Bool
postRejectRequest client id = postAndGetHastodonResult (replace ":id" id pRejectRequest) [] client

getInstance :: HastodonClient -> IO (Either JSONException Instance)
getInstance client = do
  res <- getHastodonResponseJSON pInstance client
  return (getResponseBody res :: Either JSONException Instance)

postMediaFile :: HastodonClient -> String -> String -> IO (Either JSONException Attachment)
postMediaFile client filename description = do
  initReq <- mkHastodonRequest pMedia client
  let file = partFileSource (T.pack "file") filename
  let mimetype = defaultMimeLookup (T.pack filename)
  req <- formDataBody [file { partContentType = Just mimetype },
                       partBS (T.pack "description") (utf8ToChar8 description)
                      ] initReq
  res <- httpJSONEither req
  return (getResponseBody res :: Either JSONException Attachment)

getMutesWithOption :: HastodonClient -> RangeOption -> IO (Either JSONException [Account])
getMutesWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pMutes client
  return (getResponseBody res :: Either JSONException [Account])

getMutes :: HastodonClient -> IO (Either JSONException [Account])
getMutes client = getMutesWithOption client mempty

getNotifications :: HastodonClient -> IO (Either JSONException [Notification])
getNotifications client = getNotificationsWithOption client mempty

getNotificationsWithOption :: HastodonClient -> GetNotificationsOption -> IO (Either JSONException [Notification])
getNotificationsWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pNotifications client
  return (getResponseBody res :: Either JSONException [Notification])

getNotificationById :: HastodonClient -> HastodonId -> IO (Either JSONException Notification)
getNotificationById client id = do
  res <- getHastodonResponseJSON (replace ":id" id pNotificationById) client
  return (getResponseBody res :: Either JSONException Notification)

postNotificationsClear :: HastodonClient -> IO Bool
postNotificationsClear = postAndGetHastodonResult pNotificationClear []

getReports :: HastodonClient -> IO (Either JSONException [Report])
getReports client = do
  res <- getHastodonResponseJSON pReports client
  return (getResponseBody res :: Either JSONException [Report])

getSearchedResults :: HastodonClient -> String ->  IO (Either JSONException [Results])
getSearchedResults client = getSearchedResultsWithOption client mempty

getSearchedResultsWithOption ::
  HastodonClient -> StatusSearchOption -> String -> IO (Either JSONException [Results])
getSearchedResultsWithOption client opt query = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) (pSearch ++ "?q=" ++ query) client
  return (getResponseBody res :: Either JSONException [Results])

getStatus :: HastodonClient -> HastodonId -> IO (Either JSONException Status)
getStatus client id = do
  res <- getHastodonResponseJSON (replace ":id" id pStatus) client
  return (getResponseBody res :: Either JSONException Status)

getCard :: HastodonClient -> HastodonId -> IO (Either JSONException Card)
getCard client id = do
  res <- getHastodonResponseJSON (replace ":id" id pCard) client
  return (getResponseBody res :: Either JSONException Card)

getContext :: HastodonClient -> HastodonId -> IO (Either JSONException Context)
getContext client id = do
  res <- getHastodonResponseJSON (replace ":id" id pContext) client
  return (getResponseBody res :: Either JSONException Context)

getRebloggedByWithOption :: HastodonClient -> RangeOption -> HastodonId -> IO (Either JSONException [Account])
getRebloggedByWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) (replace ":id" id pRebloggedBy) client
  return (getResponseBody res :: Either JSONException [Account])

getRebloggedBy :: HastodonClient -> HastodonId -> IO (Either JSONException [Account])
getRebloggedBy client = getRebloggedByWithOption client mempty

getFavoritedByWithOption :: HastodonClient -> RangeOption -> HastodonId -> IO (Either JSONException [Account])
getFavoritedByWithOption client opt id = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) (replace ":id" id pFavoritedBy) client
  return (getResponseBody res :: Either JSONException [Account])

getFavoritedBy :: HastodonClient -> HastodonId -> IO (Either JSONException [Account])
getFavoritedBy client = getFavoritedByWithOption client mempty

postStatus :: HastodonClient -> String -> IO (Either JSONException Status)
postStatus client = postStatusWithOption client mempty

postStatusWithOption ::
  HastodonClient -> PostStatusOption -> String -> IO (Either JSONException Status)
postStatusWithOption client opt status = do
  let prms = [(Char8.pack "status", utf8ToChar8 status)] ++ optionAsForm opt
  res <- postAndGetHastodonResponseJSON pStatuses prms client
  return (getResponseBody res :: Either JSONException Status)

postStatusWithMediaIds :: HastodonClient -> String -> [HastodonId] -> IO (Either JSONException Status)
postStatusWithMediaIds client status mediaIds = do
  let unpackedMediaIds = [(Char8.pack "media_ids[]",utf8ToChar8 media)|media <- mediaIds] -- Rails array parameter convention?
  let body = (Char8.pack "status",utf8ToChar8 status):unpackedMediaIds
  res <- postAndGetHastodonResponseJSON pStatuses body client
  return (getResponseBody res :: Either JSONException Status)

postReblog :: HastodonClient -> HastodonId -> IO (Either JSONException Status)
postReblog client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pReblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnreblog :: HastodonClient -> HastodonId -> IO (Either JSONException Status)
postUnreblog client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pUnreblog) [] client
  return (getResponseBody res :: Either JSONException Status)

postFavorite :: HastodonClient -> HastodonId -> IO (Either JSONException Status)
postFavorite client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pFavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

postUnfavorite :: HastodonClient -> HastodonId -> IO (Either JSONException Status)
postUnfavorite client id = do
  res <- postAndGetHastodonResponseJSON (replace ":id" id pUnfavorite) [] client
  return (getResponseBody res :: Either JSONException Status)

getHomeTimeline :: HastodonClient -> IO (Either JSONException [Status])
getHomeTimeline client = getHomeTimelineWithOption client mempty

getHomeTimelineWithOption :: HastodonClient -> TimelineOption -> IO (Either JSONException [Status])
getHomeTimelineWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pHomeTimeline client
  return (getResponseBody res :: Either JSONException [Status])

getPublicTimeline :: HastodonClient -> IO (Either JSONException [Status])
getPublicTimeline client = getPublicTimelineWithOption client mempty

getPublicTimelineWithOption :: HastodonClient -> TimelineOption -> IO (Either JSONException [Status])
getPublicTimelineWithOption client opt = do
  res <- getHastodonResponseJSONWithOption (optionAsQuery opt) pPublicTimeline client
  return (getResponseBody res :: Either JSONException [Status])

getTaggedTimeline :: HastodonClient -> String ->  IO (Either JSONException [Status])
getTaggedTimeline client hashtag = do
  res <- getHastodonResponseJSON (replace ":hashtag" hashtag pTaggedTimeline) client
  return (getResponseBody res :: Either JSONException [Status])

getTaggedTimelineWithOption ::
  HastodonClient -> TimelineOption -> String -> IO (Either JSONException [Status])
getTaggedTimelineWithOption client opt hashtag = do
  res <- getHastodonResponseJSONWithOption
           (optionAsQuery opt)
           (replace ":hashtag" hashtag pTaggedTimeline)
           client
  return (getResponseBody res :: Either JSONException [Status])
