module Web.Hastodon
  (
    mkHastodonClient

  , getAccountsById
  , getHomeTimeline
  , getPublicTimeline
  ) where

import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Char8 as LChar8
import Network.HTTP.Simple
import Network.HTTP.Types.Header

-- Mastodon API endpoints
uAccountsById = "/api/v1/accounts/" -- :id
uHomeTimeline = "/api/v1/timelines/home"
uPublicTimeline = "/api/v1/timelines/public"

data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}

mkHastodonHeader :: String -> Request -> Request
mkHastodonHeader token =
  addRequestHeader hAuthorization $ Char8.pack $ "Bearer " ++ token

getHastodonResponseBody url client = do
  initReq <- parseRequest $ "https://" ++ (host client) ++ url
  let req = mkHastodonHeader (token client) initReq
  res <- httpLBS req
  return $ LChar8.unpack $ getResponseBody res

-- 
-- exported functions
-- 

mkHastodonClient :: String -> String -> HastodonClient
mkHastodonClient host token = HastodonClient host token

getAccountsById :: String -> HastodonClient -> IO String
getAccountsById id = getHastodonResponseBody $ uAccountsById ++ id

getHomeTimeline :: HastodonClient -> IO String
getHomeTimeline = getHastodonResponseBody uHomeTimeline

getPublicTimeline :: HastodonClient -> IO String
getPublicTimeline = getHastodonResponseBody uPublicTimeline

