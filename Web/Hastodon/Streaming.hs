{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Hastodon.Streaming
  ( StreamingPayload(..)
  , StreamingResponse
  , streamUser
  , streamPublic
  , streamLocal
  , streamHashtag
  , streamList
  ) where

import           Prelude hiding (takeWhile)
import           Control.Applicative ((<|>), many, some)
import           Data.Aeson
import           Data.Attoparsec.ByteString as A
import           Data.Attoparsec.ByteString.Char8 as C8
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Maybe (maybeToList)
import           Conduit
import           Network.HTTP.Simple
import           Web.Hastodon.Util
import           Web.Hastodon.Types


----
-- Public API
----

pStreamUser        = "/api/v1/streaming/user"
pStreamPublic      = "/api/v1/streaming/public"
pStreamLocal       = "/api/v1/streaming/public/local"
pStreamHashtag     = "/api/v1/streaming/hashtag"
pStreamList        = "/api/v1/streaming/list"

type StreamingResponse m =
  forall m. MonadResource m => ConduitT () StreamingPayload m ()

data StreamingPayload = SUpdate Status             |
                        SNotification Notification |
                        SDelete StatusId           |
                        Thump
                        deriving (Show)

data EventType = EUpdate | ENotification | EDelete


streamUser :: HastodonClient -> StreamingResponse m
streamUser client = getStreamingResponse pStreamUser client

streamPublic :: HastodonClient -> StreamingResponse m
streamPublic client = getStreamingResponse pStreamPublic client

streamLocal :: HastodonClient -> StreamingResponse m
streamLocal client = getStreamingResponse pStreamLocal client

streamHashtag :: HastodonClient -> String -> StreamingResponse m
streamHashtag client hashtag = getStreamingResponse ph client where
  ph = pStreamHashtag ++ "?hashtag=" ++ hashtag

streamList :: HastodonClient -> String -> StreamingResponse m
streamList client list = getStreamingResponse l client where
  l = pStreamList ++ "?list=" ++ list


----
-- Private stuff
----

stream :: ByteString -> ByteString -> (ByteString, [StreamingPayload])
stream i a | isThump i = ("", [Thump])
           | isEvent i = (i, [])
           | otherwise = parseE a i
  where parseE et d =
          case parseET et of
            (Just EDelete) -> ("", p parseDelete d)
            (Just ENotification) -> ("", p parseNotification d)
            (Just EUpdate) -> ("",p parseUpdate d)
            Nothing -> ("", [])
        p r s = maybeToList $ maybeResult $ parse r s
        isThump = (":thump" `B8.isPrefixOf`)
        isEvent = ("event: " `B8.isPrefixOf`)
        parseET s = maybeResult $ parse parseEvent s

parseEvent :: Parser EventType
parseEvent = do
  string "event: "
  try ("delete" *> return EDelete)   <|>
    try ("update" *> return EUpdate) <|>
    try ("notification" *> return ENotification)

pd = string "data: "

parseDelete :: Parser StreamingPayload
parseDelete = do
  pd
  i <- StatusId <$> many C8.digit
  return $ SDelete i


eoc :: String -> Char -> Maybe String
eoc "\n" '\n' = Nothing
eoc acc c = Just (c:acc)

parseNotification :: Parser StreamingPayload
parseNotification = do
  pd
  s <- C8.takeTill (== '\n')
  case (decodeStrict' s :: Maybe Notification) of
    Nothing -> fail $ "decode error"
    (Just n) -> return $ SNotification n

parseUpdate :: Parser StreamingPayload
parseUpdate = do
  pd
  s <- C8.takeTill (== '\n')
  case (decodeStrict' s :: Maybe Status) of
    Nothing -> fail $ "decode error"
    (Just s) -> return $ SUpdate s

parseStream :: forall m. MonadResource m =>
  ConduitT ByteString StreamingPayload m ()
parseStream = concatMapAccumC stream ""

getStreamingResponse path client = do
  req <- liftIO $ mkHastodonRequest path client
  httpSource req getResponseBody .| parseStream
