module Web.Hastodon.Option
  (
    -- * Limit Options
    IsLimitOption
  , limit

    -- * Range Options
  , RangeOption ()
  , IsRangeOption
  , maxId
  , sinceId

    -- * Timeline Options
  , TimelineOption ()
  , IsTimelineOption
  , instanceLocal

    -- * Status search Options
  , StatusSearchOption ()
  , IsStatusSearchOption
  , resolve

    -- * Account search Options
  , AccountSearchOption ()
  , IsAccountSearchOption
  , following

    -- * Status getting options
  , IsGetStatusesOption
  , onlyMedia

    -- * Account status getting options
  , GetAccountStatusesOption ()
  , IsGetAccountStatusesOption
  , pinned
  , excludeReplies

    -- * Notification getting options
  , GetNotificationsOption ()
  , IsGetNotificationsOption
  , NotificationType (..)
  , excludeTypes

    -- * Status posting options
  , PostStatusOption ()
  , IsPostStatusOption
  , Visibility (..)
  , inReplyToId
  , mediaIds
  , sensitive
  , spoilerText
  , visibility

    -- * Mute options
  , PostMuteOption ()
  , IsPostMuteOption
  , muteNotifications
  )
where

import Data.Semigroup
import Data.Monoid hiding ((<>))
import Control.Applicative
import qualified Data.Map as Map

import Web.Hastodon.Types
import Web.Hastodon.Util


--
-- Limit option
--
class IsOption a => IsLimitOption a where {}

limit :: IsLimitOption a => Int -> a
limit i = mkOption "limit" $ Just (show i)


--
-- Range options
--
newtype RangeOption = RangeOption { unRangeOption :: OptionImpl } deriving Show

instance IsOption RangeOption where
  fromOptionImpl = RangeOption
  toOptionImpl = unRangeOption

instance Semigroup RangeOption where
  (<>) = optionMappend

instance Monoid RangeOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsRangeOption a where {}

instance IsLimitOption RangeOption where {}
instance IsRangeOption RangeOption where {}

sinceId :: IsRangeOption a => StatusId -> a
sinceId i = mkOption "since_id" $ Just $ show i

maxId :: IsRangeOption a => StatusId -> a
maxId i = mkOption "max_id" $ Just $ show i

--
-- Timeline options
--
newtype TimelineOption = TimelineOption { unTimelineOption :: OptionImpl } deriving Show

instance IsOption TimelineOption where
  fromOptionImpl = TimelineOption
  toOptionImpl = unTimelineOption

instance Semigroup TimelineOption where
  (<>) = optionMappend

instance Monoid TimelineOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsTimelineOption a where {}

instance IsLimitOption TimelineOption where {}
instance IsRangeOption TimelineOption where {}
instance IsGetStatusesOption TimelineOption where {}
instance IsTimelineOption TimelineOption where {}

instanceLocal :: IsTimelineOption a => a
instanceLocal = mkOption "local" $ Nothing

--
-- Status getting options
--
class IsOption a => IsGetStatusesOption a where {}

onlyMedia :: IsGetStatusesOption a => a
onlyMedia = mkOption "only_media" Nothing

--
-- AccountStatus getting options
--
newtype GetAccountStatusesOption = GetAccountStatusesOption { unGetAccountStatusesOption :: OptionImpl } deriving Show

instance IsOption GetAccountStatusesOption where
  fromOptionImpl = GetAccountStatusesOption
  toOptionImpl = unGetAccountStatusesOption

instance Semigroup GetAccountStatusesOption where
  (<>) = optionMappend

instance Monoid GetAccountStatusesOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsGetAccountStatusesOption a where {}

instance IsLimitOption GetAccountStatusesOption where {}
instance IsRangeOption GetAccountStatusesOption where {}
instance IsGetStatusesOption GetAccountStatusesOption where {}
instance IsGetAccountStatusesOption GetAccountStatusesOption where {}

pinned :: IsGetAccountStatusesOption a => a
pinned = mkOption "pinned" Nothing

excludeReplies :: IsGetAccountStatusesOption a => a
excludeReplies = mkOption "exclude_replies" Nothing

--
-- Notification getting options
--
newtype GetNotificationsOption = GetNotificationsOption {
  unGetNotificationsOption :: OptionImpl
  } deriving Show

instance IsOption GetNotificationsOption where
  fromOptionImpl = GetNotificationsOption
  toOptionImpl = unGetNotificationsOption

instance Semigroup GetNotificationsOption where
  (<>) = optionMappend

instance Monoid GetNotificationsOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsGetNotificationsOption a where {}

instance IsLimitOption GetNotificationsOption where {}
instance IsRangeOption GetNotificationsOption where {}
instance IsGetNotificationsOption GetNotificationsOption where {}

data NotificationType =
  NotificationFollow | NotificationFavourite | NotificationReblog | NotificationMention
  deriving (Eq, Show, Ord,Enum, Bounded)

formatNt :: NotificationType -> String
formatNt NotificationFollow = "follow"
formatNt NotificationFavourite = "favourite"
formatNt NotificationReblog = "reblog"
formatNt NotificationMention = "mention"

excludeTypes :: IsGetNotificationsOption a => [NotificationType] -> a
excludeTypes ts = mkArrayOption "exclude_types" $ map formatNt ts

--
-- AccountSearch options
--
newtype AccountSearchOption = AccountSearchOption { unAccountSearchOption :: OptionImpl } deriving Show

instance IsOption AccountSearchOption where
  fromOptionImpl = AccountSearchOption
  toOptionImpl = unAccountSearchOption

instance Semigroup AccountSearchOption where
  (<>) = optionMappend

instance Monoid AccountSearchOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsAccountSearchOption a where {}

instance IsAccountSearchOption AccountSearchOption where {}

following :: IsAccountSearchOption a => a
following = mkOption "following" $ Nothing

--
-- StatusSearch options
--
newtype StatusSearchOption = StatusSearchOption { unStatusSearchOption :: OptionImpl } deriving Show

instance IsOption StatusSearchOption where
  fromOptionImpl = StatusSearchOption
  toOptionImpl = unStatusSearchOption

instance Semigroup StatusSearchOption where
  (<>) = optionMappend

instance Monoid StatusSearchOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsStatusSearchOption a where {}

instance IsLimitOption StatusSearchOption where {}
instance IsStatusSearchOption StatusSearchOption where {}

resolve :: IsStatusSearchOption a => a
resolve = mkOption "resolve" $ Nothing

--
-- Status posting options
--
newtype PostStatusOption = PostStatusOption { unPostStatusOption :: OptionImpl } deriving Show

instance IsOption PostStatusOption where
  fromOptionImpl = PostStatusOption
  toOptionImpl = unPostStatusOption

instance Semigroup PostStatusOption where
  (<>) = optionMappend

instance Monoid PostStatusOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsPostStatusOption a where {}

instance IsPostStatusOption PostStatusOption where {}

data Visibility =
  VisibilityDirect | VisibilityPrivate | VisibilityUnlisted | VisibilityPublic
  deriving (Eq, Show, Ord, Enum, Bounded)

formatVis :: Visibility -> String
formatVis VisibilityDirect = "direct"
formatVis VisibilityPrivate = "private"
formatVis VisibilityUnlisted = "unlisted"
formatVis VisibilityPublic = "public"

inReplyToId :: IsPostStatusOption a => StatusId -> a
inReplyToId i = mkOption "in_reply_to_id" (Just $ show i)

mediaIds :: IsPostStatusOption a => [MediaId] -> a
mediaIds l = mkArrayOption "media_ids" $ show <$> l

sensitive :: IsPostStatusOption a => a
sensitive = mkOption "sensitive" Nothing

spoilerText :: IsPostStatusOption a => String -> a
spoilerText str = mkOption "spoiler_text" (Just str)

visibility :: IsPostStatusOption a => Visibility -> a
visibility vis = mkOption "visibility" (Just $ formatVis vis)

--
-- PostMute options
--
newtype PostMuteOption = PostMuteOption { unPostMuteOption :: OptionImpl } deriving Show

instance IsOption PostMuteOption where
  fromOptionImpl = PostMuteOption
  toOptionImpl = unPostMuteOption

instance Semigroup PostMuteOption where
  (<>) = optionMappend

instance Monoid PostMuteOption where
  mempty = optionMempty
  mappend = (<>)

class IsOption a => IsPostMuteOption a where {}

instance IsLimitOption PostMuteOption where {}
instance IsPostMuteOption PostMuteOption where {}

muteNotifications :: IsPostMuteOption a => a
muteNotifications = mkOption "notifications" $ Nothing
