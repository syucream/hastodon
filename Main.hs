import Web.Hastodon

--
-- example main()
--
main :: IO ()
main = do
  let token = "???"
  let client = mkHastodonClient "pawoo.net" token
  tl <- getHomeTimeline client
  -- tl <- getPublicTimeline client
  print tl

