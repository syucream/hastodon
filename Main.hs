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
      timeline <- getAccountById 93150 client
      print timeline
      result <- postStatus "test toot from hastodon!" client
      print result
