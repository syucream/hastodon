module Web.Hastodon.Types
  ( HastodonId
  , OAuthResponse(..)
  , Account(..)
  , Application(..)
  , Attachment(..)
  , Card(..)
  , Context(..)
  , Instance(..)
  , Mention(..)
  , Notification(..)
  , OAuthClient(..)
  , Relationship(..)
  , Report(..)
  , Results(..)
  , Status(..)
  , Tag(..)
  ) where

import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


type HastodonId = String

data OAuthResponse = OAuthResponse {
  accessToken :: String
  -- NOTE currently ignore other fields.
} deriving (Show)
instance FromJSON OAuthResponse where
  parseJSON (Object v) =
    OAuthResponse <$> (v .: T.pack "access_token")

data Account = Account {
  accountId :: HastodonId,
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
    Account <$> (v .: T.pack "id")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "display_name")
            <*> (v .: T.pack "locked")
            <*> (v .: T.pack "created_at")
            <*> (v .: T.pack "followers_count")
            <*> (v .: T.pack "following_count")
            <*> (v .: T.pack "statuses_count")
            <*> (v .: T.pack "note")
            <*> (v .: T.pack "url")
            <*> (v .: T.pack "avatar")
            <*> (v .: T.pack "avatar_static")
            <*> (v .: T.pack "header")
            <*> (v .: T.pack "header_static")

data Application = Application {
  applicationName :: String,
  applicationWebsite :: Maybe String
} deriving (Show)
instance FromJSON Application where
  parseJSON (Object v) =
    Application <$> (v .:  T.pack "name")
                <*> (v .:? T.pack "website")

data Attachment = Attachment {
  attachmentId :: HastodonId,
  attachmentType :: String,
  attachmentUrl :: String,
  attachmentRemoteUrl :: Maybe String,
  attachmentPreviewUrl :: String,
  attachmentTextUrl :: Maybe String
} deriving (Show)
instance FromJSON Attachment where
  parseJSON (Object v) =
    Attachment <$> (v .:  T.pack "id")
               <*> (v .:  T.pack "type")
               <*> (v .:  T.pack "url")
               <*> (v .:? T.pack "remote_url")
               <*> (v .:  T.pack "preview_url")
               <*> (v .:? T.pack "text_url")

data Card = Card {
  cardUrl :: String,
  cardTitle :: String,
  cardDescription :: String,
  cardImage :: String
} deriving (Show)
instance FromJSON Card where
  parseJSON (Object v) =
    Card <$> (v .: T.pack "url")
         <*> (v .: T.pack "title")
         <*> (v .: T.pack "description")
         <*> (v .: T.pack "image")

data Context = Context {
  contextAncestors :: [Status],
  contextDescendants :: [Status]
} deriving (Show)
instance FromJSON Context where
  parseJSON (Object v) =
    Context <$> (v .: T.pack "ancestors")
            <*> (v .: T.pack "descendants")

data Instance = Instance {
  instanceUri :: String,
  instanceTitle :: String,
  instanceDescription :: String,
  instanceEmail :: String
} deriving (Show)
instance FromJSON Instance where
  parseJSON (Object v) =
    Instance <$> (v .: T.pack "uri")
             <*> (v .: T.pack "title")
             <*> (v .: T.pack "description")
             <*> (v .: T.pack "email")

data Mention = Mention {
  mentionUrl :: String,
  mentionUsername :: String,
  mentionAcct :: String,
  mentionId :: HastodonId
} deriving (Show)
instance FromJSON Mention where
  parseJSON (Object v) =
    Mention <$> (v .: T.pack "url")
            <*> (v .: T.pack "username")
            <*> (v .: T.pack "acct")
            <*> (v .: T.pack "id")

data Notification = Notification {
  notificationId :: HastodonId,
  notificationType :: String,
  notificationCreatedAt :: String,
  notificationAccount :: Account,
  notificationStatus :: Maybe Status
} deriving (Show)
instance FromJSON Notification where
  parseJSON (Object v) =
    Notification <$> (v .:  T.pack "id")
                 <*> (v .:  T.pack "type")
                 <*> (v .:  T.pack "created_at")
                 <*> (v .:  T.pack "account")
                 <*> (v .:? T.pack "status")

data OAuthClient = OAuthClient {
  oauthClientId :: HastodonId,
  oauthClientRedirectUri :: String,
  oauthClientClientId :: String,
  oauthClientClientSecret :: String
} deriving (Show)
instance FromJSON OAuthClient where
  parseJSON (Object v) =
    OAuthClient <$> (v .: T.pack "id")
                <*> (v .: T.pack "redirect_uri")
                <*> (v .: T.pack "client_id")
                <*> (v .: T.pack "client_secret")

data Relationship = Relationship {
  relationshipId :: HastodonId,
  relationshipFollowing :: Bool,
  relationshipFollowed_by :: Bool,
  relationshipBlocking :: Bool,
  relationshipMuting :: Bool,
  relationshipRequested :: Bool
} deriving (Show)
instance FromJSON Relationship where
  parseJSON (Object v) =
    Relationship <$> (v .: T.pack "id")
                 <*> (v .: T.pack "following")
                 <*> (v .: T.pack "followed_by")
                 <*> (v .: T.pack "blocking")
                 <*> (v .: T.pack "muting")
                 <*> (v .: T.pack "requested")

data Report = Report {
  reportId :: HastodonId,
  reportActionToken :: String
} deriving (Show)
instance FromJSON Report where
  parseJSON (Object v) =
    Report <$> (v .: T.pack "id")
           <*> (v .: T.pack "action_taken")

data Results = Results {
  resultAccounts :: [Account],
  resultStatus :: [Status],
  resultHashtags :: [String]
} deriving (Show)
instance FromJSON Results where
  parseJSON (Object v) =
    Results <$> (v .: T.pack "accounts")
            <*> (v .: T.pack "statuses")
            <*> (v .: T.pack "hashtags")

data Status = Status {
  statusId :: HastodonId,
  statusUri :: String,
  statusUrl :: String,
  statusAccount :: Account,
  statusInReplyToId :: Maybe Int,
  statusInReplyToAccountId :: Maybe Int,
  statusReblog :: Maybe Status,
  statusContent :: String,
  statusCreatedAt :: String,
  statusReblogsCount :: Int,
  statusFavouritesCount :: Int,
  statusReblogged :: Maybe Bool,
  statusFavourited :: Maybe Bool,
  statusSensitive :: Maybe Bool,
  statusSpoilerText :: String,
  statusVisibility :: String,
  statusMediaAttachments :: [Attachment],
  statusMentions :: [Mention],
  statusTags :: [Tag],
  statusApplication :: Maybe Application
} deriving (Show)
instance FromJSON Status where
  parseJSON (Object v) =
    Status <$> (v .:  T.pack "id")
           <*> (v .:  T.pack "uri")
           <*> (maybe "" id <$> (v .:?  T.pack "url"))
           <*> (v .:  T.pack "account")
           <*> (v .:? T.pack "in_reply_to_id")
           <*> (v .:? T.pack "in_reply_to_account_id")
           <*> (v .:? T.pack "reblog")
           <*> (v .:  T.pack "content")
           <*> (v .:  T.pack "created_at")
           <*> (v .:  T.pack "reblogs_count")
           <*> (v .:  T.pack "favourites_count")
           <*> (v .:? T.pack "reblogged")
           <*> (v .:? T.pack "favourited")
           <*> (v .:? T.pack "sensitive")
           <*> (v .:  T.pack "spoiler_text")
           <*> (v .:  T.pack "visibility")
           <*> (v .:  T.pack "media_attachments")
           <*> (v .:  T.pack "mentions")
           <*> (v .:  T.pack "tags")
           <*> (v .:? T.pack "application")

data Tag = Tag {
  name :: String,
  url :: String
} deriving (Show)
instance FromJSON Tag where
  parseJSON (Object v) =
    Tag <$> (v .: T.pack "name")
        <*> (v .: T.pack "url")
