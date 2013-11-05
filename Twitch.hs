{-# LANGUAGE OverloadedStrings #-}
module Twitch where

import Control.Monad

import qualified Network.HTTP.Conduit as C

import qualified Twitch.APIRequests as R
import Twitch.Datatypes
import Twitch.Util

import Text.JSON

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.ByteString.Lazy.Internal as LBS

data APICall r = APICall
  { request :: R.APIRequest
  , parser :: C.Response LBS.ByteString -> Result r
  }

makeCall auth call = R.makeRequest_ auth (request call) >>= (extractResult . (parser call))

-- Blocks
getBlocks :: ClientAuthorization -> String -> IO [TwitchUser] -- This can be in a more general monad
getBlocks auth login = R.makeRequest auth (R.getBlocks login) >>= (extractResult . (unpackJSON >=> (.! "blocks")))

putBlock :: ClientAuthorization -> String -> String -> IO TwitchUser
putBlock auth user target = R.makeRequest auth (R.putBlock user target) >>= (extractResult . (unpackJSON >=> (.! "user")))

deleteBlock :: ClientAuthorization -> String -> String -> IO ()
deleteBlock auth user target = R.makeRequest auth (R.deleteBlock user target) >> return ()

-- Channels
getChannel :: String -> APICall TwitchChannel
getChannel name = APICall
  { request = R.getChannel name
  , parser = unpackJSON . C.responseBody
  }

getMyChannel :: APICall TwitchChannel
getMyChannel = APICall
  { request = R.getMyChannel
  , parser = unpackJSON . C.responseBody
  }

-- getChannelVideos

getChannelFollows :: String -> APICall [TwitchUser]
getChannelFollows channel = APICall
  { request = R.getChannelFollows channel
  , parser = (unpackJSON >=> (.! "follows") >=> mapM (.! "user")) . C.responseBody
  }

getChannelEditors :: String -> APICall [TwitchUser]
getChannelEditors channel = APICall
  { request = R.getChannelEditors channel
  , parser = (unpackJSON >=> (.! "users")) . C.responseBody
  }

-- Slightly better interface for updating stream status/game
putChannel :: String -> APICall TwitchChannel
putChannel channel = APICall
  { request = R.putChannel channel
  , parser = unpackJSON . C.responseBody
  }

-- Convenience methods
putChannelStatus :: String -> String -> APICall TwitchChannel
putChannelStatus channel status = APICall
  { request = R.addParam ("channel[status]", status) (R.putChannel channel)
  , parser = unpackJSON . C.responseBody
  }

putChannelGame :: String -> String -> APICall TwitchChannel
putChannelGame channel game = APICall
  { request = R.addParam ("channel[game]", game) (R.putChannel channel)
  , parser = unpackJSON . C.responseBody
  }

-- TODO: return success
deleteStreamKey :: String -> APICall ()
deleteStreamKey channel = APICall
  { request = R.deleteStreamKey channel
  , parser = const $ return ()
  }

-- TODO: return success
runCommercial :: String -> Int -> APICall ()
runCommercial channel length = APICall
  { request = R.addParam ("length", show length) (R.runCommercial channel)
  , parser = const $ return ()
  }

-- Chat

-- getChat
-- getEmoticons -- TODO: Build a datatype for this

-- Follows

getUserFollows :: String -> APICall [TwitchChannel]
getUserFollows user = APICall
  { request = R.getUserFollows user
  , parser = (unpackJSON >=> (.! "follows") >=> mapM (.! "channel")) . C.responseBody
  }

-- getFollowStatus -- TODO: Need to handle non-2xx gracefully

putFollow :: String -> String -> APICall TwitchChannel
putFollow user channel = APICall
  { request = R.putFollow user channel
  , parser = (unpackJSON >=> (.! "channel")) . C.responseBody
  }

deleteFollow :: String -> String -> APICall ()
deleteFollow user channel = APICall
  { request = R.deleteFollow user channel
  , parser = const $ return ()
  }

-- Games
-- getGames

-- Ingests
-- getIngests

-- Root
-- getRoot

-- Search
-- searchStreams
-- searchGames

-- Streams
getStream :: String -> APICall (Maybe TwitchStream)
getStream channel = APICall
  { request = R.getStream channel
  , parser = (unpackJSON >=> (maybeResult . (.! "stream"))) . C.responseBody
  }

getStreams :: APICall [TwitchStream]
getStreams = APICall
  { request = R.getStreams
  , parser = (unpackJSON >=> (.! "streams")) . C.responseBody
  }

getFeaturedStreams :: APICall [TwitchStream]
getFeaturedStreams = APICall
  { request = R.getFeaturedStreams
  , parser = (unpackJSON >=> (.! "featured") >=> mapM (.! "stream")) . C.responseBody
  }

-- getStreamsSummary

getFollowedStreams :: APICall [TwitchStream]
getFollowedStreams = APICall
  { request = R.getFollowedStreams
  , parser = (unpackJSON >=> (.! "streams")) . C.responseBody
  }

-- Subscriptions
getSubscribers :: String -> APICall [TwitchUser]
getSubscribers channel = APICall
  { request = R.getSubscribers channel
  , parser = (unpackJSON >=> (.! "subscriptions") >=> mapM (.! "user")) . C.responseBody
  }

-- getSubscriberStatus

-- Teams
-- TODO (don't have team datatype)

-- Users
getUser :: String -> APICall TwitchUser
getUser user = APICall
  { request = R.getUser user
  , parser = unpackJSON . C.responseBody
  }

getMyUser :: APICall TwitchUser
getMyUser = APICall
  { request = R.getMyUser
  , parser = unpackJSON . C.responseBody
  }

-- Videos
-- TODO (don't have video datatype)
