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

getChannelFollows :: ClientAuthorization -> String -> IO [TwitchUser]
getChannelFollows auth channel = do
    resp <- C.makeCall auth (C.getChannelFollows channel)
    extractResult $ do
        obj <- unpackJSON resp
        follows <- obj .! "follows"
        mapM (.! "user") follows

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

-- getChat
-- getEmoticons -- TODO: Build a datatype for this

-- Follows

getUserFollows :: ClientAuthorization -> String -> IO [TwitchChannel]
getUserFollows auth channel = do
    resp <- C.makeCall auth (C.getUserFollows channel)
    extractResult $ do
        obj <- unpackJSON resp
        follows <- obj .! "follows"
        mapM (.! "channel") follows

-- getFollowStatus -- TODO: Need to handle non-2xx gracefully

putFollow :: ClientAuthorization -> String -> String -> IO TwitchChannel
putFollow auth user channel = do
    resp <- C.makeCall auth (C.putFollow user channel)
    extractResult $ unpackJSON resp >>= (.! "channel")

deleteFollow :: ClientAuthorization -> String -> String -> IO ()
deleteFollow auth user channel = C.makeCall auth (C.deleteFollow user channel) >> return ()

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
getStream :: ClientAuthorization -> String -> IO (Maybe TwitchStream)
getStream auth channel = do
    resp <- C.makeCall auth (C.getStream channel)
    extractResult $ do
        obj <- unpackJSON resp
        maybeResult $ valFromObj "stream" obj

getStreams :: ClientAuthorization -> IO [TwitchStream]
getStreams auth = do
    resp <- C.makeCall auth (C.getStreams)
    extractResult $ unpackJSON resp >>= (.! "streams")

getFeaturedStreams :: ClientAuthorization -> IO [TwitchStream]
getFeaturedStreams auth = do
    resp <- C.makeCall auth (C.getFeaturedStreams)
    extractResult $ do
        obj <- unpackJSON resp
        featured <- obj .! "featured"
        mapM (.! "stream") featured

-- getStreamsSummary

getFollowedStreams :: ClientAuthorization -> IO [TwitchStream]
getFollowedStreams auth = do
    resp <- C.makeCall auth (C.getFollowedStreams)
    extractResult $ unpackJSON resp >>= (.! "streams")

-- Subscriptions
getSubscribers :: ClientAuthorization -> String -> IO [TwitchUser]
getSubscribers auth channel = do
    resp <- C.makeCall auth (C.getSubscribers channel)
    extractResult $ do
        obj <- unpackJSON resp
        featured <- obj .! "subscriptions"
        mapM (.! "user") featured

-- getSubscriberStatus

-- Teams
-- TODO (don't have team datatype)

-- Users
getUser :: ClientAuthorization -> String -> IO TwitchUser
getUser auth user = do
    resp <- C.makeCall auth (C.getUser user)
    extractResult $ unpackJSON resp

getMyUser :: ClientAuthorization -> IO TwitchUser
getMyUser auth = do
    resp <- C.makeCall auth C.getMyUser
    extractResult $ unpackJSON resp

-- Videos
-- TODO (don't have video datatype)
