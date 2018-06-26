module API.Shutterstock.Requests where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Types (Request)
import Data.Either (Either(Left))
import Data.FormURLEncoded (FormURLEncoded, encode, fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))

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

download :: String -> AffjaxRequest Unit
download id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/licenses/" <> id <> "/downloads"
    , method = Left POST
    --, content = Just " "
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

license ::  String -> String -> AffjaxRequest String
license id size = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/licenses/"
    , method = Left POST
    , content = Just "{\"id\": id}"
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }