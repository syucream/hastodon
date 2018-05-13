module Web.Hastodon.Util
  ( HastodonClient(..)
  , mkHastodonHeader
  , mkHastodonRequestWithQuery
  , mkHastodonRequest
  , mkOption
  , mkArrayOption
  , optionMempty
  , optionMappend
  , optionAsQuery
  , optionAsForm
  , utf8ToChar8
  ) where


import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as Map
import Network.HTTP.Simple
import Network.HTTP.Types.Header

import Web.Hastodon.Types

data HastodonClient = HastodonClient {
  host :: String,
  token :: String
}


mkHastodonRequestWithQuery ::
  [(Char8.ByteString, Maybe Char8.ByteString)] -> String -> HastodonClient -> IO Request
mkHastodonRequestWithQuery opt path client = do
  initReq <- parseRequest $ "https://" ++ (host client) ++ path
  return $
    mkHastodonHeader (token client) $
    setRequestQueryString opt $ initReq

mkHastodonRequest :: String -> HastodonClient -> IO Request
mkHastodonRequest = mkHastodonRequestWithQuery []

mkHastodonHeader :: String -> Request -> Request
mkHastodonHeader token =
  addRequestHeader hAuthorization $ utf8ToChar8 $ "Bearer " ++ token


utf8ToChar8 :: String -> Char8.ByteString
utf8ToChar8 = T.encodeUtf8 . T.pack


mkOption :: IsOption a => String -> Maybe String -> a
mkOption key val = fromOptionImpl $ Map.singleton (utf8ToChar8 key) (Right $ utf8ToChar8 <$> val)


mkArrayOption :: IsOption a => String -> [String] -> a
mkArrayOption key val = fromOptionImpl $ Map.singleton (utf8ToChar8 key) (Left $ utf8ToChar8 <$> val)


optionMempty :: IsOption a => a
optionMempty = fromOptionImpl Map.empty


optionMappend :: IsOption a => a -> a -> a
optionMappend x y = fromOptionImpl $ Map.union (toOptionImpl x) (toOptionImpl y)


optionAsQuery :: IsOption a => a -> [(Char8.ByteString, Maybe Char8.ByteString)]
optionAsQuery x = do
  (k, v) <- Map.toList $ toOptionImpl x
  let k' = k `mappend` Char8.pack "[]" -- The Rails convention of list query parameter
  case v of
    Left l -> [(k, Just x) | x <- l]
    Right r -> [(k, r)]


optionAsForm :: IsOption a => a -> [(Char8.ByteString, Char8.ByteString)]
optionAsForm opt = fmap cnv $ optionAsQuery opt
  where cnv (x, y) = (x, fromMaybe (Char8.pack "true") y)
