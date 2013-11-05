{-# LANGUAGE OverloadedStrings #-}
module Twitch.APIRequests where

import qualified Network.HTTP.Base as B
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as W
import Data.List (intercalate)
import Data.CaseInsensitive hiding (map)

import Data.ByteString.Char8 hiding (intercalate, map)

import qualified Data.ByteString.Lazy.Internal as LBS

import Twitch.Datatypes
import Twitch.Util

import Text.JSON

data APIRequest = APIRequest
  { path :: String
  , params :: [(String, String)]
  , method :: W.Method
  , headers :: [(String, String)]
  } deriving (Eq, Show)

prepHeader :: (String, String) -> W.Header
prepHeader (h, v) = (mk $ pack h, pack v)

addParam :: (String, String) -> APIRequest -> APIRequest
addParam p c = c { params = p : (params c) }

-- makeCall_ :: ClientAuthorization -> APIRequest -> IO (Response Data.ByteString.Lazy.Internal.ByteString)
makeRequest_ auth request = do
    let url = "https://api.twitch.tv/kraken" ++ (path request)
    let body = B.urlEncodeVars (params request)
    let hdrs' = ("Client-ID", client_id auth) :
                ("Accept", "application/vnd.twitchtv.v2+json") :
                ("Content-Type", "application/x-www-form-urlencoded") :
                (headers request)
    let hdrs = case access_token auth of
               Nothing    -> hdrs'
               Just token -> ("Authorization", "OAuth " ++ token) : hdrs'
    initReq <- C.parseUrl $ url
    let req = initReq { C.method = method request
                      , C.requestHeaders = Prelude.map prepHeader hdrs
                      , C.requestBody = C.RequestBodyBS (pack body)
                      , C.checkStatus = const . const . const $ Nothing -- Always accept the response
                      }
    C.withManager $ C.httpLbs req

-- Temporary function for old code
makeRequest auth request = do
    response <- makeRequest_ auth request
    return $ C.responseBody response

baseAPIRequest = APIRequest
  { path = ""
  , params = []
  , method = "GET"
  , headers = []
  }

-- Blocks
getBlocks login = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode login) ++ "/blocks"
  , method = "GET"
  }
putBlock user target = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/blocks/" ++ (B.urlEncode target)
  , method = "PUT"
  }
deleteBlock user target = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/blocks/" ++ (B.urlEncode target)
  , method = "DELETE"
  }

-- Channels
getChannel name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name)
  , method = "GET"
  }
getMyChannel = baseAPIRequest
  { path = "/channel"
  , method = "GET"
  }
getChannelVideos name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name) ++ "/videos"
  , method = "GET"
  }
getChannelFollows name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name) ++ "/follows"
  , method = "GET"
  }
getChannelEditors name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name) ++ "/editors"
  , method = "GET"
  }
putChannel name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name)
  , method = "PUT"
  }
deleteStreamKey name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name) ++ "/stream_key"
  , method = "DELETE"
  }
runCommercial name = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode name) ++ "/commercial"
  , method = "POST"
  }

-- Chat
getChat channel = baseAPIRequest
  { path = "/chat/" ++ (B.urlEncode channel)
  , method = "GET"
  }
getEmoticons = baseAPIRequest
  { path = "/chat/emoticons"
  , method = "GET"
  }

-- Follows
getUserFollows user = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels"
  , method = "GET"
  }
getFollowStatus user channel = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "GET"
  }
putFollow user channel = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "PUT"
  }
deleteFollow user channel = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "DELETE"
  }

-- Games
getGames = baseAPIRequest
  { path = "/games/top"
  , method = "GET"
  }

-- Ingests
getIngests = baseAPIRequest
  { path = "/ingests"
  , method = "GET"
  }

-- Root
getRoot = baseAPIRequest
  { path = "/"
  , method = "GET"
  }

-- Search
searchStreams = baseAPIRequest
  { path = "/search/streams"
  , method = "GET"
  }
searchGames = baseAPIRequest
  { path = "/search/games"
  , method = "GET"
  }

-- Streams
getStream channel = baseAPIRequest
  { path = "/streams/" ++ (B.urlEncode channel)
  , method = "GET"
  }
getStreams = baseAPIRequest
  { path = "/streams"
  , method = "GET"
  }
getFeaturedStreams = baseAPIRequest
  { path = "/streams/featured"
  , method = "GET"
  }
getStreamsSummary = baseAPIRequest
  { path = "/streams/summary"
  , method = "GET"
  }
getFollowedStreams = baseAPIRequest
  { path = "/streams/followed"
  , method = "GET"
  }

-- Subscriptions
getSubscribers channel = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode channel) ++ "/subscriptions"
  , method = "GET"
  }
getSubscriberStatus channel user = baseAPIRequest
  { path = "/channels/" ++ (B.urlEncode channel) ++ "/subscriptions/" ++ (B.urlEncode user)
  , method = "GET"
  }

-- Teams
getTeams = baseAPIRequest
  { path = "/teams"
  , method = "GET"
  }
getTeam team = baseAPIRequest
  { path = "/teams/" ++ (B.urlEncode team)
  , method = "GET"
  }

-- Users
getUser user = baseAPIRequest
  { path = "/users/" ++ (B.urlEncode user)
  , method = "GET"
  }
getMyUser = baseAPIRequest
  { path = "/user"
  , method = "GET"
  }

-- Videos
getVideo id = baseAPIRequest
  { path = "/videos/" ++ (B.urlEncode id)
  , method = "GET"
  }
getTopVideos = baseAPIRequest
  { path = "/videos/top"
  , method = "GET"
  }
