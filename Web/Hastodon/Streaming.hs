{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Web.Hastodon.Streaming
  ( StreamingPayload
  , StreamingResponse
  , streamUser
  , streamPublic
  , streamLocal
  , streamHashtag
  , streamList
  ) where

import           Control.Monad (join)
import           Data.Aeson
import           Data.Attoparsec.ByteString
import           Data.Attoparsec.ByteString.Char8 (decimal)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (fromStrict)
import           Conduit
import           Network.HTTP.Simple
import           Web.Hastodon.Util
import           Web.Hastodon.Types (Notification, Status)


----
-- Public API
----

pStreamUser        = "/api/v1/streaming/user"
pStreamPublic      = "/api/v1/streaming/public"
pStreamLocal       = "/api/v1/streaming/public/local"
pStreamHashtag     = "/api/v1/streaming/hashtag"
pStreamList        = "/api/v1/streaming/list"

type StreamingResponse m =
  forall m. MonadResource m => ConduitT () (Maybe StreamingPayload) m ()

data StreamingPayload = SUpdate Status | SNotification Notification | SDelete Int | Thump


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

decodeNotification :: ByteString -> Maybe StreamingPayload
decodeNotification s = SNotification <$> (decode' $ fromStrict s)

decodeUpdate :: ByteString -> Maybe StreamingPayload
decodeUpdate s = SUpdate <$> (decode' $ fromStrict s)

notification :: Parser (Maybe StreamingPayload)
notification = string "event: notification\ndata: " >$< decodeNotification

update :: Parser (Maybe StreamingPayload)
update = string "event: update\ndata: " >$< decodeUpdate

thump :: Parser (Maybe StreamingPayload)
thump = ":thump\n" *> return (Just Thump)

delete :: Parser (Maybe StreamingPayload)
delete = "event: delete\ndata: " *> decimal >$< (Just . SDelete)

stream :: Parser (Maybe StreamingPayload)
stream = choice [thump, notification, update, delete]

parseStream :: ByteString -> Maybe StreamingPayload
parseStream = join . maybeResult . parse stream

getStreamingResponse :: String -> HastodonClient -> StreamingResponse m
getStreamingResponse path client = do
  req <- liftIO $ mkHastodonRequest path client
  httpSource req getResponseBody .| mapC parseStream

(>$<) :: (Functor f) => f a -> (a -> b) -> f b
(>$<) = flip fmap
