# Hastodon

[![Build Status](https://travis-ci.org/syucream/hastodon.svg?branch=master)](https://travis-ci.org/syucream/hastodon)

mastodon client module for Haskell

## Quickstart

```haskell
import Web.Hastodon

main :: IO ()
main = do
  let token = "your OAuth token"
  let client = mkHastodonClient "mastodon.social" token
  tl <- getPublicTimeline client
  print body
  result <- postStatuses "test toot from hastodon!" client
  print result 
```

## Status of implementations

- [x]  GET /api/v1/accounts/:id
- [x]  GET /api/v1/accounts/verify_credentials
- [ ]  PATCH /api/v1/accounts/update_credentials
- [ ]  GET /api/v1/accounts/:id/followers
- [ ]  GET /api/v1/accounts/:id/following
- [ ]  GET /api/v1/accounts/:id/statuses
- [ ]  POST /api/v1/accounts/:id/follow
- [ ]  POST /api/v1/accounts/:id/unfollow
- [ ]  GET /api/v1/accounts/:id/block
- [ ]  GET /api/v1/accounts/:id/unblock
- [ ]  GET /api/v1/accounts/:id/mute
- [ ]  GET /api/v1/accounts/:id/unmute
- [x]  GET /api/v1/accounts/relationships
- [ ]  GET /api/v1/accounts/search
- [ ]  POST /api/v1/apps
- [ ]  GET /api/v1/blocks
- [ ]  GET /api/v1/favourites
- [ ]  GET /api/v1/follow_requests
- [ ]  POST /api/v1/follow_requests/:id/authorize
- [ ]  POST /api/v1/follow_requests/:id/reject
- [ ]  POST /api/v1/follows
- [ ]  GET /api/v1/instance
- [ ]  POST /api/v1/media
- [ ]  GET /api/v1/mutes
- [ ]  GET /api/v1/notifications
- [ ]  GET /api/v1/notifications/:id
- [ ]  POST /api/v1/notifications/clear
- [ ]  GET /api/v1/reports
- [ ]  POST /api/v1/reports
- [ ]  GET /api/v1/search
- [ ]  GET /api/v1/statuses/:id
- [ ]  GET /api/v1/statuses/:id/context
- [ ]  GET /api/v1/statuses/:id/card
- [ ]  GET /api/v1/statuses/:id/reblogged_by
- [ ]  GET /api/v1/statuses/:id/favourited_by
- [x]  POST /api/v1/statuses
- [ ]  DELETE /api/v1/statuses/:id
- [ ]  POST /api/v1/statuses/:id/reblog
- [ ]  POST /api/v1/statuses/:id/unreblog
- [ ]  POST /api/v1/statuses/:id/favourite
- [ ]  POST /api/v1/statuses/:id/unfavourite
- [x]  GET /api/v1/timelines/home
- [x]  GET /api/v1/timelines/public
- [ ]  GET /api/v1/timelines/tag/:hashtag

## License

MIT

## Author

Ryo Okubo
