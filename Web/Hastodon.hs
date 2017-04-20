module Web.Hastodon
  (
    mkHastodonClient

  , getAccountsById
  , postStatuses
  , getHomeTimeline
  , getPublicTimeline
  ) where

import Data.ByteString.Char8 as Char8
import Data.ByteString.Lazy.Char8 as LChar8
import Network.HTTP.Simple
import Network.HTTP.Types.Header

--
-- Mastodon API endpoints
--
pAccountsById   = "/api/v1/accounts/" -- :id
pStatuses       = "/api/v1/statuses"
pHomeTimeline   = "/api/v1/timelines/home"
pPublicTimeline = "/api/v1/timelines/public"

data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}

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

getHastodonResponseBody :: String -> HastodonClient -> IO String
getHastodonResponseBody path client = do
  req <- mkHastodonRequest path client
  res <- httpLBS req
  return $ LChar8.unpack $ getResponseBody res

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

getAccountsById :: String -> HastodonClient -> IO String
getAccountsById id = getHastodonResponseBody $ pAccountsById ++ id

postStatuses :: String -> HastodonClient -> IO String
postStatuses statuses = postHastodonRequestBody pStatuses [(Char8.pack "status", Char8.pack statuses)]

getHomeTimeline :: HastodonClient -> IO String
getHomeTimeline = getHastodonResponseBody pHomeTimeline

getPublicTimeline :: HastodonClient -> IO String
getPublicTimeline = getHastodonResponseBody pPublicTimeline

