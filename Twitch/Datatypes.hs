module Twitch.Datatypes where

import Data.Maybe

data ClientAuthorization = CA
  { client_id :: String
  , access_token :: Maybe String
  }
