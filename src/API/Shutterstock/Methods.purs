module API.Shutterstock.Methods where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Types (Image, ImageDetails, Request, Search)
import API.Shutterstock.Validation.Validation (searchAndRetrieveValidation, searchValidation)
import Control.Monad.Aff (Aff)
import Data.Either (Either(Left))
import Data.FormURLEncoded (FormURLEncoded, encode, fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V, runValidation)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow)
import Validators.Json (JsError)

type SearchErrorRow err = 
  ( JsonErrorRow
  ( JsError
  ( HttpErrorRow
  ( AffjaxErrorRow err
  ))))

toUrlEncoded :: Request -> FormURLEncoded
toUrlEncoded {query, page, perPage} = fromArray
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]

buildRequest :: Request -> AffjaxRequest Unit
buildRequest r =
  let url = r # toUrlEncoded # encode
  in defaultRequest {
    url = "https://api.shutterstock.com/v2/images/search?" <> url
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

buildDetailsRequest :: String -> AffjaxRequest Unit
buildDetailsRequest id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/" <> id
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }


search 
  :: forall t err
   . Request
  -> Aff ( ajax :: AJAX| t)
      (V
        (Array (Variant (SearchErrorRow err))) 
        (Search Image))
search req = (runValidation searchValidation) (buildRequest req)

searchAndRetrieve 
  :: forall err ext
   . Request
  -> Aff (ajax :: AJAX | ext)
      (V
        (Array (Variant (SearchErrorRow err)))
        (Array ImageDetails)
      )
searchAndRetrieve req = runValidation searchAndRetrieveValidation (buildRequest req)