module API.Shutterstock.Validation where

import Prelude

import API.Shutterstock.Json (getImageWithDetailsfromJson, getResultfromJson)
import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Types (Image, ImageDetails, Request, Search)
import Control.Monad.Aff (Aff)
import Control.Parallel (parTraverse)
import Data.Array (filter)
import Data.Either (Either(Left))
import Data.FormURLEncoded (FormURLEncoded, encode, fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V(..), Validation, hoistFn, hoistFnMV, runValidation)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow, affjaxJson)
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

searchValidation
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array(Variant(SearchErrorRow err)))
      (AffjaxRequest Unit)
      (Search Image)
searchValidation = getResultfromJson <<< affjaxJson

retrieve 
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array(Variant(SearchErrorRow err)))
      (AffjaxRequest Unit)
      ImageDetails
retrieve = getImageWithDetailsfromJson <<< affjaxJson

catV :: forall t err. Monoid err => Array (V err t) -> V err (Array t)
catV = sequence <<< filter (case _ of 
  Valid _ _ -> true
  otherwise -> false)

getCatVDetails 
  :: forall t err
   . Array String
  -> Aff ( ajax :: AJAX | t)
      (V
       (Array (Variant (SearchErrorRow err)))
       (Array ImageDetails)
      )
getCatVDetails ids = catV <$> parTraverse getDetails ids
  where
    getDetails id = runValidation retrieve (buildDetailsRequest id)

searchAndRetrieveValidation
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array (Variant (SearchErrorRow err)))
      (AffjaxRequest Unit)
      (Array ImageDetails)
searchAndRetrieveValidation = 
  hoistFnMV getCatVDetails
  <<< hoistFn getIds 
  <<< searchValidation
  where
    getIds = map _.id <<< _.photos