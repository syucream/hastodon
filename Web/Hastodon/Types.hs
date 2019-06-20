{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.Hastodon.Types
  ( OAuthResponse(..)
  , Account(..)
  , AccountId(..)
  , Application(..)
  , Attachment(..)
  , AttachmentId(..)
  , Card(..)
  , Context(..)
  , Instance(..)
  , MediaId(..)
  , Mention(..)
  , Notification(..)
  , NotificationId(..)
  , OAuthClient(..)
  , OAuthClientId(..)
  , Relationship(..)
  , RelationshipId(..)
  , Report(..)
  , ReportId(..)
  , Results(..)
  , Status(..)
  , StatusId(..)
  , Tag(..)
  , OptionVal
  , OptionImpl
  , IsOption(..)
  ) where

import Data.Aeson
import Data.String (IsString, fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map as Map

newtype AccountId = AccountId { unAccountId :: String } deriving (FromJSON, IsString, Eq, Ord)

newtype AttachmentId = AttachmentId { unAttachmentId :: String } deriving (FromJSON, IsString, Show, Eq, Ord)

newtype OAuthClientId = OAuthClientId { unOAuthClientId :: String } deriving (FromJSON, IsString, Show, Eq, Ord)

newtype MediaId = MediaId { unMediaId :: String } deriving (FromJSON, IsString, Show, Eq, Ord)

newtype NotificationId = NotificationId { unNotificationId :: String } deriving (FromJSON, IsString, Eq, Ord)

newtype RelationshipId = RelationshipId { unRelationshipId :: String } deriving (FromJSON, IsString, Show, Eq, Ord)

newtype ReportId = ReportId { unReportId :: String } deriving (FromJSON, IsString, Show, Eq, Ord)

newtype StatusId = StatusId { unStatusId :: String } deriving (FromJSON, IsString, Eq, Ord)


-- This is a very suboptimal soluion for argument genration
-- but is a temporary patch on a better solution.
instance Show StatusId where
  show = unStatusId

instance Show NotificationId where
  show = unNotificationId

instance Show AccountId where
  show = unAccountId
  

data OAuthResponse = OAuthResponse {
  accessToken :: String
  -- NOTE currently ignore other fields.
} deriving (Show)
instance FromJSON OAuthResponse where
  parseJSON (Object v) =
    OAuthResponse <$> (v .: "access_token")

data Account = Account {
  accountId :: AccountId,
  accountUsername :: String,
  accountAcct :: String,
  accountDisplayName :: String,
  accountLocked :: Bool,
  accountCreatedAt :: String,
  accountFollowersCount :: Int,
  accountFollowingCount :: Int,
  accountStatusesCount :: Int,
  accountNote :: String,
  accountUrl :: String,
  accountAvatar :: String,
  accountAvatarStatic :: String,
  accountHeader :: String,
  accountHeaderStatic :: String
} deriving (Show)
instance FromJSON Account where
  parseJSON (Object v) =
    Account <$> (v .: "id")
            <*> (v .: "username")
            <*> (v .: "acct")
            <*> (v .: "display_name")
            <*> (v .: "locked")
            <*> (v .: "created_at")
            <*> (v .: "followers_count")
            <*> (v .: "following_count")
            <*> (v .: "statuses_count")
            <*> (v .: "note")
            <*> (v .: "url")
            <*> (v .: "avatar")
            <*> (v .: "avatar_static")
            <*> (v .: "header")
            <*> (v .: "header_static")

data Application = Application {
  applicationName :: String,
  applicationWebsite :: Maybe String
} deriving (Show)
instance FromJSON Application where
  parseJSON (Object v) =
    Application <$> (v .:  "name")
                <*> (v .:? "website")

data Attachment = Attachment {
  attachmentId :: AttachmentId,
  attachmentType :: String,
  attachmentUrl :: String,
  attachmentRemoteUrl :: Maybe String,
  attachmentPreviewUrl :: String,
  attachmentTextUrl :: Maybe String
} deriving (Show)
instance FromJSON Attachment where
  parseJSON (Object v) =
    Attachment <$> (v .:  "id")
               <*> (v .:  "type")
               <*> (v .:  "url")
               <*> (v .:? "remote_url")
               <*> (v .:  "preview_url")
               <*> (v .:? "text_url")

data Card = Card {
  cardUrl :: String,
  cardTitle :: String,
  cardDescription :: String,
  cardImage :: String
} deriving (Show)
instance FromJSON Card where
  parseJSON (Object v) =
    Card <$> (v .: "url")
         <*> (v .: "title")
         <*> (v .: "description")
         <*> (v .: "image")

data Context = Context {
  contextAncestors :: [Status],
  contextDescendants :: [Status]
} deriving (Show)
instance FromJSON Context where
  parseJSON (Object v) =
    Context <$> (v .: "ancestors")
            <*> (v .: "descendants")

data Instance = Instance {
  instanceUri :: String,
  instanceTitle :: String,
  instanceDescription :: String,
  instanceEmail :: String
} deriving (Show)
instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> (v .: "uri")
             <*> (v .: "title")
             <*> (v .: "description")
             <*> (v .: "email")

data Mention = Mention {
  mentionUrl :: String,
  mentionUsername :: String,
  mentionAcct :: String,
  mentionId :: AccountId
} deriving (Show)
instance FromJSON Mention where
  parseJSON (Object v) =
    Mention <$> (v .: "url")
            <*> (v .: "username")
            <*> (v .: "acct")
            <*> (v .: "id")

data Notification = Notification {
  notificationId :: NotificationId,
  notificationType :: String,
  notificationCreatedAt :: String,
  notificationAccount :: Account,
  notificationStatus :: Maybe Status
} deriving (Show)
instance FromJSON Notification where
  parseJSON (Object v) =
    Notification <$> (v .:  "id")
                 <*> (v .:  "type")
                 <*> (v .:  "created_at")
                 <*> (v .:  "account")
                 <*> (v .:? "status")

data OAuthClient = OAuthClient {
  oauthClientId :: OAuthClientId,
  oauthClientRedirectUri :: String,
  oauthClientClientId :: String,
  oauthClientClientSecret :: String
} deriving (Show)
instance FromJSON OAuthClient where
  parseJSON (Object v) =
    OAuthClient <$> (v .: "id")
                <*> (v .: "redirect_uri")
                <*> (v .: "client_id")
                <*> (v .: "client_secret")

data Relationship = Relationship {
  relationshipId :: RelationshipId,
  relationshipFollowing :: Bool,
  relationshipFollowed_by :: Bool,
  relationshipBlocking :: Bool,
  relationshipMuting :: Bool,
  relationshipRequested :: Bool
} deriving (Show)
instance FromJSON Relationship where
  parseJSON (Object v) =
    Relationship <$> (v .: "id")
                 <*> (v .: "following")
                 <*> (v .: "followed_by")
                 <*> (v .: "blocking")
                 <*> (v .: "muting")
                 <*> (v .: "requested")

data Report = Report {
  reportId :: ReportId,
  reportActionToken :: String
} deriving (Show)
instance FromJSON Report where
  parseJSON (Object v) =
    Report <$> (v .: "id")
           <*> (v .: "action_taken")

data Results = Results {
  resultAccounts :: [Account],
  resultStatus :: [Status],
  resultHashtags :: [String]
} deriving (Show)
instance FromJSON Results where
  parseJSON (Object v) =
    Results <$> (v .: "accounts")
            <*> (v .: "statuses")
            <*> (v .: "hashtags")

data Emoji = Emoji {
  emojiShortcode :: String,
  emojiStaticUrl :: String,
  emojiUrl :: String
} deriving (Show)
instance FromJSON Emoji where
 parseJSON (Object v) =
   Emoji <$> (v .: "shortcode")
         <*> (v .: "static_url")
         <*> (v .: "url")

data Status = Status {
  statusId :: StatusId,
  statusUri :: String,
  statusUrl :: String,
  statusAccount :: Account,
  statusInReplyToId :: Maybe String,
  statusInReplyToAccountId :: Maybe String,
  statusReblog :: Maybe Status,
  statusContent :: String,
  statusCreatedAt :: String,
  statusReblogsCount :: Int,
  statusFavouritesCount :: Int,
  statusReblogged :: Maybe Bool,
  statusFavourited :: Maybe Bool,
  statusMuted :: Maybe Bool,
  statusSensitive :: Maybe Bool,
  statusSpoilerText :: String,
  statusVisibility :: String,
  statusMediaAttachments :: [Attachment],
  statusMentions :: [Mention],
  statusTags :: [Tag],
  statusApplication :: Maybe Application,
  statusEmojis :: [Emoji],
  statusLanguage :: Maybe String
} deriving (Show)
instance FromJSON Status where
  parseJSON (Object v) =
    Status <$> (v .:  "id")
           <*> (v .:  "uri")
           <*> (v .:  "url")
           <*> (v .:  "account")
           <*> (v .:? "in_reply_to_id")
           <*> (v .:? "in_reply_to_account_id")
           <*> (v .:? "reblog")
           <*> (v .:  "content")
           <*> (v .:  "created_at")
           <*> (v .:  "reblogs_count")
           <*> (v .:  "favourites_count")
           <*> (v .:? "reblogged")
           <*> (v .:? "favourited")
           <*> (v .:? "muted")
           <*> (v .:? "sensitive")
           <*> (v .:  "spoiler_text")
           <*> (v .:  "visibility")
           <*> (v .:  "media_attachments")
           <*> (v .:  "mentions")
           <*> (v .:  "tags")
           <*> (v .:? "application")
           <*> (v .: "emojis")
           <*> (v .:? "language")

data Tag = Tag {
  name :: String,
  url :: String
} deriving (Show)
instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: "name")
        <*> (v .: "url")

-- Left : Array parameter, Right : Single parameter
type OptionVal = Either [Char8.ByteString] (Maybe Char8.ByteString)

type OptionImpl = Map.Map Char8.ByteString OptionVal

class IsOption a where
  fromOptionImpl :: OptionImpl -> a
  toOptionImpl :: a -> OptionImpl

