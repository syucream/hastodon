module Web.Hastodon.Util
  ( HastodonClient(..)
  , mkHastodonHeader
  , mkHastodonRequest
  , utf8ToChar8
  ) where


import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Simple
import Network.HTTP.Types.Header


data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}


mkHastodonRequest :: String -> HastodonClient -> IO Request
mkHastodonRequest path client = do
  initReq <- parseRequest $ "https://" ++ (host client) ++ path
  return $ mkHastodonHeader (token client) $ initReq


mkHastodonHeader :: String -> Request -> Request
mkHastodonHeader token =
  addRequestHeader hAuthorization $ utf8ToChar8 $ "Bearer " ++ token


utf8ToChar8 :: String -> Char8.ByteString
utf8ToChar8 = T.encodeUtf8 . T.pack
