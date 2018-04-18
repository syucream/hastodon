# Hastodon

[![Build Status](https://travis-ci.org/syucream/hastodon.svg?branch=master)](https://travis-ci.org/syucream/hastodon)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/Hastodon.svg)](http://packdeps.haskellers.com/feed?needle=Hastodon)

mastodon client module for Haskell

## Quickstart

If you don't have client id and client secret, call postApps at first. (Alternatively, in the web client, under the development section of your mastodon profile settings, 

```haskell
import Web.Hastodon
  appRes <- postApps "mastodon.social" "HastodonClientApp"
    case appRes of
      Right app -> do
        let clientId = oauthClientClientId app
        let clientSecret = oauthClientClientSecret app
```

Fill client id, client secret, the email address used to sign up for the instance and password, then call functions.

```haskell
import Web.Hastodon

main :: IO ()
main = do
  let clientId = "???"
  let clientSecret = "???"
  let email = "???"
  let password = "???"
  maybeClient <- mkHastodonClient clientId clientSecret email password "mastodon.social"
  case maybeClient of
    Just client -> do
      timeline <- getAccountById client 93150
      print timeline
      result <- postStatus client "test toot from hastodon!"
      print result
    Nothing -> do
      putStrLn "Failed to log in.  Be careful regarding the spelling of your email and password."
```

## Status of implementations

### Mastodon APIs

- [x]  GET /api/v1/accounts/:id
- [x]  GET /api/v1/accounts/verify_credentials
- [ ]  PATCH /api/v1/accounts/update_credentials
- [x]  GET /api/v1/accounts/:id/followers
- [x]  GET /api/v1/accounts/:id/following
- [x]  GET /api/v1/accounts/:id/statuses
- [x]  POST /api/v1/accounts/:id/follow
- [x]  POST /api/v1/accounts/:id/unfollow
- [x]  POST /api/v1/accounts/:id/block
- [x]  POST /api/v1/accounts/:id/unblock
- [x]  POST /api/v1/accounts/:id/mute
- [x]  POST /api/v1/accounts/:id/unmute
- [x]  GET /api/v1/accounts/relationships
- [x]  GET /api/v1/accounts/search
- [x]  POST /api/v1/apps
- [x]  GET /api/v1/blocks
- [x]  GET /api/v1/favourites
- [x]  GET /api/v1/follow_requests
- [x]  POST /api/v1/follow_requests/:id/authorize
- [x]  POST /api/v1/follow_requests/:id/reject
- [ ]  POST /api/v1/follows
- [x]  GET /api/v1/instance
- [x]  POST /api/v1/media
- [x]  GET /api/v1/mutes
- [x]  GET /api/v1/notifications
- [x]  GET /api/v1/notifications/:id
- [x]  POST /api/v1/notifications/clear
- [x]  GET /api/v1/reports
- [ ]  POST /api/v1/reports
- [x]  GET /api/v1/search
- [x]  GET /api/v1/statuses/:id
- [x]  GET /api/v1/statuses/:id/context
- [x]  GET /api/v1/statuses/:id/card
- [x]  GET /api/v1/statuses/:id/reblogged_by
- [x]  GET /api/v1/statuses/:id/favourited_by
- [x]  POST /api/v1/statuses
- [ ]  DELETE /api/v1/statuses/:id
- [x]  POST /api/v1/statuses/:id/reblog
- [x]  POST /api/v1/statuses/:id/unreblog
- [x]  POST /api/v1/statuses/:id/favourite
- [x]  POST /api/v1/statuses/:id/unfavourite
- [x]  GET /api/v1/timelines/home
- [x]  GET /api/v1/timelines/public
- [x]  GET /api/v1/timelines/tag/:hashtag

### Auth

- [x] the interface of POST /oauth/token

## License

MIT

## Author

Ryo Okubo <syucream1031@gmail.com>

### Contributors

* [KirinDave](https://github.com/KirinDave)
* [as-capabl](https://github.com/as-capabl)
* [william42](https://github.com/william42)
* [swizzard](https://github.com/swizzard)
