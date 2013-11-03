{-# LANGUAGE OverloadedStrings #-}
module Twitch.APICalls where

import qualified Network.HTTP.Base as B
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as W
import Data.List (intercalate)
import Data.ByteString.Char8 hiding (intercalate, map)
import Data.CaseInsensitive hiding (map)

import Twitch.Datatypes

data APICall = APICall
  { path :: String
  , params :: [(String, String)]
  , method :: W.Method
  , headers :: [(String, String)]
  } deriving (Eq, Show)

prepHeader :: (String, String) -> W.Header
prepHeader (h, v) = (mk $ pack h, pack v)

addParam :: (String, String) -> APICall -> APICall
addParam p c = c { params = p : (params c) }

-- makeCall :: ClientAuthorization -> APICall -> IO (Data.ByteString.Lazy.Internal.ByteString)
makeCall auth call = do
    let url = "https://api.twitch.tv/kraken" ++ (path call)
    let body = B.urlEncodeVars (params call)
    let hdrs' = ("Client-ID", client_id auth) :
                ("Accept", "application/vnd.twitchtv.v2+json") :
                ("Content-Type", "application/x-www-form-urlencoded") :
                (headers call)
    let hdrs = case access_token auth of
               Nothing    -> hdrs'
               Just token -> ("Authorization", "OAuth " ++ token) : hdrs'
    initReq <- C.parseUrl $ url
    let req = initReq { C.method = method call
                      , C.requestHeaders = Prelude.map prepHeader hdrs
                      , C.requestBody = C.RequestBodyBS (pack body)
                      }
    response <- C.withManager $ C.httpLbs req
    return $ C.responseBody response

baseAPICall = APICall
  { path = ""
  , params = []
  , method = "GET"
  , headers = []
  }

-- Blocks
getBlocks login = baseAPICall
  { path = "/users/" ++ (B.urlEncode login) ++ "/blocks"
  , method = "GET"
  }
putBlock user target = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/blocks/" ++ (B.urlEncode target)
  , method = "PUT"
  }
deleteBlock user target = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/blocks/" ++ (B.urlEncode target)
  , method = "DELETE"
  }

-- Channels
getChannel name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name)
  , method = "GET"
  }
getMyChannel = baseAPICall
  { path = "/channel"
  , method = "GET"
  }
getChannelVideos name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name) ++ "/videos"
  , method = "GET"
  }
getChannelFollows name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name) ++ "/follows"
  , method = "GET"
  }
getChannelEditors name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name) ++ "/editors"
  , method = "GET"
  }
putChannel name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name)
  , method = "PUT"
  }
deleteStreamKey name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name) ++ "/stream_key"
  , method = "DELETE"
  }
runCommercial name = baseAPICall
  { path = "/channels/" ++ (B.urlEncode name) ++ "/commercial"
  , method = "POST"
  }

-- Chat
getChat channel = baseAPICall
  { path = "/chat/" ++ (B.urlEncode channel)
  , method = "GET"
  }
getEmoticons = baseAPICall
  { path = "/chat/emoticons"
  , method = "GET"
  }

-- Follows
getUserFollows user = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels"
  , method = "GET"
  }
getFollowStatus user channel = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "GET"
  }
putFollow user channel = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "PUT"
  }
deleteFollow user channel = baseAPICall
  { path = "/users/" ++ (B.urlEncode user) ++ "/follows/channels/" ++ (B.urlEncode channel)
  , method = "DELETE"
  }

-- Games
getGames = baseAPICall
  { path = "/games/top"
  , method = "GET"
  }

-- Ingests
getIngests = baseAPICall
  { path = "/ingests"
  , method = "GET"
  }

-- Root
getRoot = baseAPICall
  { path = "/"
  , method = "GET"
  }

-- Search
searchStreams = baseAPICall
  { path = "/search/streams"
  , method = "GET"
  }
searchGames = baseAPICall
  { path = "/search/games"
  , method = "GET"
  }

-- Streams
getStream channel = baseAPICall
  { path = "/streams/" ++ (B.urlEncode channel)
  , method = "GET"
  }
getStreams = baseAPICall
  { path = "/streams"
  , method = "GET"
  }
getFeaturedStreams = baseAPICall
  { path = "/streams/featured"
  , method = "GET"
  }
getStreamsSummary = baseAPICall
  { path = "/streams/summary"
  , method = "GET"
  }
getFollowedStreams = baseAPICall
  { path = "/streams/followed"
  , method = "GET"
  }

-- Subscriptions
getSubscribers channel = baseAPICall
  { path = "/channels/" ++ (B.urlEncode channel) ++ "/subscriptions"
  , method = "GET"
  }
getSubscriberStatus channel user = baseAPICall
  { path = "/channels/" ++ (B.urlEncode channel) ++ "/subscriptions/" ++ (B.urlEncode user)
  , method = "GET"
  }

-- Teams
getTeams = baseAPICall
  { path = "/teams"
  , method = "GET"
  }
getTeam team = baseAPICall
  { path = "/teams/" ++ (B.urlEncode team)
  , method = "GET"
  }

-- Users
getUser user = baseAPICall
  { path = "/users/" ++ (B.urlEncode user)
  , method = "GET"
  }
getMyUser = baseAPICall
  { path = "/user"
  , method = "GET"
  }

-- Videos
getVideo id = baseAPICall
  { path = "/videos/" ++ (B.urlEncode id)
  , method = "GET"
  }
getTopVideos = baseAPICall
  { path = "/videos/top"
  , method = "GET"
  }
