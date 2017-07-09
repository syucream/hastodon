import Web.Hastodon

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
      timeline <- getAccountById client 93150 
      print timeline
      result <- postStatus client "test toot from hastodon!" 
      print result
