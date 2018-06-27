module API.Shutterstock.Requests where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Types (License, Request, DownloadImage)
import Data.Argonaut (Json)
import Data.Either (Either(Left))
import Data.FormURLEncoded (FormURLEncoded, encode, fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import Network.HTTP.Affjax (AffjaxRequest, defaultRequest)
import Network.HTTP.Affjax.Request (RequestContent, toRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Simple.JSON (writeJSON)

toUrlEncoded :: Request -> FormURLEncoded
toUrlEncoded {query, page, perPage} = fromArray
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

request :: Request -> AffjaxRequest Unit
request r =
  let url = r # toUrlEncoded # encode
  in defaultRequest {
    url = "https://api.shutterstock.com/v2/images/search?" <> url
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

details :: String -> AffjaxRequest Unit
details id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/" <> id
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

download :: DownloadImage -> AffjaxRequest String
download image =
  { method: Left POST
  , url: "https://api.shutterstock.com/v2/images/licenses/"<> image.id <>"/downloads"
  , headers: 
      [ RequestHeader "Authorization" ("Bearer " <> accessToken)
      , RequestHeader "Content-Type" "application/json" 
      ]
  , content: Just $ writeJSON {size: image.size}
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  }

license ::  License -> AffjaxRequest String
license lic =   
  { method: Left POST
  , url: "https://api.shutterstock.com/v2/images/licenses"
  , headers: 
      [ RequestHeader "Authorization" ("Bearer " <> accessToken)
      , RequestHeader "Content-Type" "application/json" 
      ]
  , content: Just $ writeJSON lic
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  } 

