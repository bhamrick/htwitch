module Twitch.Datatypes where

import Data.Maybe

import Text.JSON

data ClientAuthorization = CA
  { client_id :: String
  , access_token :: Maybe String
  }

data TwitchBlock = TwitchBlock
  { block_updated_at :: String -- TODO: datetime?
  , block_display_name :: String
  , block_is_staff :: Bool
  , block_name :: String
  , block_id :: Int
  , block_logo :: String
  , block_created_at :: String -- TODO: datetime?
  } deriving (Eq, Show)

data TwitchUser = TwitchUser
  { user_updated_at :: String -- TODO: datetime?
  , user_display_name :: String
  , user_is_staff :: Bool
  , user_name :: String
  , user_id :: Int
  , user_logo :: String
  , user_created_at :: String -- TODO: datetime?
  } deriving (Eq, Show)

data TwitchChannel = TwitchChannel
  { channel_game :: String
  , channel_name :: String
  , channel_stream_key :: Maybe String
  , channel_created_at :: String
  -- , channel_teams :: [TwitchTeam]
  , channel_title :: String
  , channel_updated_at :: String
  , channel_banner :: String
  , channel_video_banner :: String
  , channel_background :: String
  , channel_logo :: String
  , channel_id :: Int
  , channel_is_mature :: Bool
  , channel_url :: String
  , channel_display_name :: String
  } deriving (Eq, Show)

data TwitchStream = TwitchStream
  { stream_broadcaster :: String
  , stream_preview :: String
  , stream_id :: Int
  , stream_viewers :: Int
  , stream_channel :: TwitchChannel
  , stream_name :: String
  , stream_game :: String
  } deriving (Eq, Show)

instance JSON TwitchBlock where
  readJSON (JSObject o) = do
    updated_at      <- valFromObj "updated_at" o
    display_name    <- valFromObj "display_name" o
    is_staff        <- valFromObj "staff" o
    name            <- valFromObj "name" o
    _id             <- valFromObj "_id" o
    logo            <- valFromObj "logo" o
    created_at      <- valFromObj "created_at" o
    return TwitchBlock
      { block_updated_at     = updated_at
      , block_display_name   = display_name
      , block_is_staff       = is_staff
      , block_name           = name
      , block_id             = _id
      , block_logo           = logo
      , block_created_at     = created_at
      }
  readJSON _ = Error "Could not parse block object"

  showJSON block = JSObject . toJSObject $
    [ ("updated_at",    showJSON . block_updated_at $ block)
    , ("display_name",  showJSON . block_display_name  $ block)
    , ("staff",         showJSON . block_is_staff $ block)
    , ("name",          showJSON . block_name $ block)
    , ("_id",           showJSON . block_id $ block)
    , ("logo",          showJSON . block_logo $ block)
    , ("created_at",    showJSON . block_created_at $ block)
    ]

-- TODO: Update this with all fields
instance JSON TwitchUser where
  readJSON (JSObject o) = do
    updated_at      <- valFromObj "updated_at" o
    display_name    <- valFromObj "display_name" o
    is_staff        <- valFromObj "staff" o
    name            <- valFromObj "name" o
    _id             <- valFromObj "_id" o
    logo            <- valFromObj "logo" o
    created_at      <- valFromObj "created_at" o
    return TwitchUser
      { user_updated_at     = updated_at
      , user_display_name   = display_name
      , user_is_staff       = is_staff
      , user_name           = name
      , user_id             = _id
      , user_logo           = logo
      , user_created_at     = created_at
      }
  readJSON _ = Error "Could not parse user object"

  showJSON user = JSObject . toJSObject $
    [ ("updated_at",    showJSON . user_updated_at $ user)
    , ("display_name",  showJSON . user_display_name  $ user)
    , ("staff",         showJSON . user_is_staff $ user)
    , ("name",          showJSON . user_name $ user)
    , ("_id",           showJSON . user_id $ user)
    , ("logo",          showJSON . user_logo $ user)
    , ("created_at",    showJSON . user_created_at $ user)
    ]

instance JSON TwitchChannel where
  readJSON (JSObject o) = do
    game         <- valFromObj "game" o
    name         <- valFromObj "name" o
    stream_key   <- valFromObj "stream_key" o
    created_at   <- valFromObj "created_at" o
    title        <- valFromObj "title" o
    updated_at   <- valFromObj "updated_at" o
    banner       <- valFromObj "banner" o
    video_banner <- valFromObj "video_banner" o
    background   <- valFromObj "background" o
    logo         <- valFromObj "logo" o
    _id          <- valFromObj "_id" o
    mature       <- valFromObj "mature" o
    url          <- valFromObj "url" o
    display_name <- valFromObj "display_name" o
    return TwitchChannel
      { channel_game = game
      , channel_name = name
      , channel_stream_key = stream_key
      , channel_created_at = created_at
      , channel_title = title
      , channel_updated_at = updated_at
      , channel_banner = banner
      , channel_video_banner = video_banner
      , channel_background = background
      , channel_logo = logo
      , channel_id = _id
      , channel_is_mature = mature
      , channel_url = url
      , channel_display_name = display_name
      }
  readJSON _ = Error "Could not parse channel object"

  showJSON channel = JSObject . toJSObject $
    [ ("game",         showJSON . channel_game $ channel)
    , ("name",         showJSON . channel_name $ channel)
    , ("stream_key",   showJSON . channel_stream_key $ channel)
    , ("created_at",   showJSON . channel_created_at $ channel)
    , ("title",        showJSON . channel_title $ channel)
    , ("updated_at",   showJSON . channel_updated_at $ channel)
    , ("banner",       showJSON . channel_banner $ channel)
    , ("video_banner", showJSON . channel_video_banner $ channel)
    , ("background",   showJSON . channel_background $ channel)
    , ("logo",         showJSON . channel_logo $ channel)
    , ("_id",          showJSON . channel_id $ channel)
    , ("mature",       showJSON . channel_is_mature $ channel)
    , ("url",          showJSON . channel_url $ channel)
    , ("display_name", showJSON . channel_display_name $ channel)
    ]

instance JSON TwitchStream where
  readJSON (JSObject o) = do
    broadcaster <- valFromObj "broadcaster" o
    preview     <- valFromObj "preview" o
    _id         <- valFromObj "_id" o
    viewers     <- valFromObj "viewers" o
    channel     <- valFromObj "channel" o
    name        <- valFromObj "name" o
    game        <- valFromObj "game" o
    return TwitchStream
      { stream_broadcaster = broadcaster
      , stream_preview = preview
      , stream_id = _id
      , stream_viewers = viewers
      , stream_channel = channel
      , stream_name = name
      , stream_game = game
      }
  readJSON _ = Error "Could not parse stream object"

  showJSON stream = JSObject . toJSObject $
    [ ("broadcaster", showJSON . stream_broadcaster $ stream)
    , ("preview",     showJSON . stream_preview $ stream)
    , ("_id",         showJSON . stream_id $ stream)
    , ("viewers",     showJSON . stream_viewers $ stream)
    , ("channel",     showJSON . stream_channel $ stream)
    , ("name",        showJSON . stream_name $ stream)
    , ("game",        showJSON . stream_game $ stream)
    ]
