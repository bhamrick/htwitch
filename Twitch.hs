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

getBlocks :: ClientAuthorization -> String -> IO [TwitchBlock] -- This can be in a more general monad
getBlocks auth login = C.makeCall auth (C.getBlocks login) >>= (extractResult . (unpackJSON >=> (.! "blocks")))
-- TODO: rest of blocks

getChannel :: ClientAuthorization -> String -> IO TwitchChannel
getChannel auth name = C.makeCall auth (C.getChannel name) >>= (extractResult . unpackJSON)
-- TODO: rest of channels
