{-# LANGUAGE OverloadedStrings #-}
module Twitch where

import qualified Twitch.APICalls as C
import Twitch.Datatypes

import Text.JSON

import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Internal (ByteString)

-- unpackJSON :: JSON a => Data.ByteString.Lazy.Internal.ByteString -> Result JSValue
unpackJSON :: Data.ByteString.Lazy.Internal.ByteString -> Result JSValue
unpackJSON = decode . unpack

-- getBlocks :: ClientAuthorization -> String -> IO [TwitchBlock] -- This can be in a more general monad
-- getBlocks auth login = valFromObj "blocks" $ 
