import Web.Hastodon
import Data.Monoid ((<>))
import Control.Concurrent (threadDelay)

--
-- example main()
--
main :: IO ()
main = do
  let clientId = "???"
  let clientSecret = "???"
  let username = "???"
  let password = "???"
  maybeClient <- mkHastodonClient clientId clientSecret username password "mastodon.social"
  case maybeClient of
    Just client -> do
      let accId = 93150

      account <- getAccountById client accId
      print account

      timelineE <- getAccountStatusesWithOption client (limit 5) accId
      timelime <- either (fail . show) return timelineE
      print $ take 10 . statusContent <$> timelime

      postedE <- postStatus client "test toot from hastodon!"
      posted <- either (fail . show) return postedE
      print posted

      threadDelay 50000
      let stId = read (statusId posted)
      result <- postStatusWithOption client
          (inReplyToId stId <> visibility VisibilityPrivate)
          "test reply"
      print result
