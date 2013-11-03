{-# LANGUAGE OverloadedStrings #-}
module Twitch where

import Control.Monad

import qualified Twitch.APICalls as C
import Twitch.Datatypes

import Text.JSON

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)

unpackJSON :: JSON a => Data.ByteString.Lazy.Internal.ByteString -> Result a
unpackJSON = decode . unpack

-- helper function to extract a key from a dictionary
(.!) :: (JSON a) => JSValue -> String -> Result a
(JSObject o) .! k = valFromObj k o
_ .! _ = Error $ "Cannot extract from a non-JSObject JSValue"

-- This is probably not good style
extractResult :: Monad m => Result a -> m a
extractResult (Ok x) = return x
extractResult (Error s) = fail s

-- TODO: Return error codes

-- Blocks
getBlocks :: ClientAuthorization -> String -> IO [TwitchUser] -- This can be in a more general monad
getBlocks auth login = C.makeCall auth (C.getBlocks login) >>= (extractResult . (unpackJSON >=> (.! "blocks")))

putBlock :: ClientAuthorization -> String -> String -> IO TwitchUser
putBlock auth user target = C.makeCall auth (C.putBlock user target) >>= (extractResult . (unpackJSON >=> (.! "user")))

deleteBlock :: ClientAuthorization -> String -> String -> IO ()
deleteBlock auth user target = C.makeCall auth (C.deleteBlock user target) >> return ()

-- Channels
getChannel :: ClientAuthorization -> String -> IO TwitchChannel
getChannel auth name = C.makeCall auth (C.getChannel name) >>= (extractResult . unpackJSON)

getMyChannel :: ClientAuthorization -> IO TwitchChannel
getMyChannel auth = C.makeCall auth (C.getMyChannel) >>= (extractResult . unpackJSON)

-- getChannelVideos
-- getChannelFollows

getChannelEditors :: ClientAuthorization -> String -> IO [TwitchUser]
getChannelEditors auth channel = C.makeCall auth (C.getChannelEditors channel) >>= (extractResult . (unpackJSON >=> (.! "users")))

-- Mediocre interface for updating stream status/game
putChannelStatus :: ClientAuthorization -> String -> String -> IO TwitchChannel
putChannelStatus auth channel status = do
    let req = C.addParam ("channel[status]", status) (C.putChannel channel)
    resp <- C.makeCall auth req
    extractResult . unpackJSON $ resp

putChannelGame :: ClientAuthorization -> String -> String -> IO TwitchChannel
putChannelGame auth channel game = do
    let req = C.addParam ("channel[game]", game) (C.putChannel channel)
    C.makeCall auth req >>= (extractResult . unpackJSON)

deleteStreamKey :: ClientAuthorization -> String -> IO ()
deleteStreamKey auth channel = do
    C.makeCall auth (C.deleteStreamKey channel)
    return ()

runCommercial :: ClientAuthorization -> String -> Int -> IO ()
runCommercial auth channel length = do
    let req = C.addParam ("length", show length) (C.runCommercial channel)
    C.makeCall auth req
    return ()

-- Chat

-- Follows

-- Games

-- Ingests

-- Root

-- Search

-- Streams

-- Subscriptions

-- Teams

-- Users

-- Videos
