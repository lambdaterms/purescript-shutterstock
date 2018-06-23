module API.Shutterstock.Api where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Types (BasicAssets, Image, ImageDetails, Search, Thumb, Detail, DetailsAssets, Request)
import Control.Monad.Aff (Aff)
import Control.Parallel (parTraverse)
import Data.Argonaut (Json)
import Data.Array (filter)
import Data.Either (Either(Left))
import Data.FormURLEncoded (FormURLEncoded, encode, fromArray)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid)
import Data.Record.Fold (collect)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V(..), Validation, hoistFn, hoistFnMV, runValidation)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow, affjaxJson)
import Validators.Json (JsError, arrayOf, field, int, number, string)

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

getResultfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      (Search Image)
getResultfromJson = collect
  { page: field "page" int
  , perPage: field "per_page" int
  , totalCount: field "total_count" int
  , searchId: field "search_id" string
  , photos: field "data" $ arrayOf getImagefromJson
  }

-- TODO: put these json functions somewhere else
getImageWithDetailsfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      ImageDetails
getImageWithDetailsfromJson = collect
  { id: field "id" string
  , description: field "description" string
  , imageType: field "image_type" string
  , mediaType: field  "image_type" string
  , aspect: field "aspect" number
  , assets: field "assets" $ getAssetsDetailsfromJson}

getImagefromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Image
getImagefromJson = collect
  { id: field "id" string
  , description: field "description" string
  , imageType: field "image_type" string
  , mediaType: field  "image_type" string
  , aspect: field "aspect" number
  , assets: field "assets" $ getAssetfromJson}

getAssetfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      BasicAssets
getAssetfromJson = collect
  { preview: field "preview" getThumbfromJson
  , smallThumb: field "small_thumb" getThumbfromJson
  , largeThumb: field "large_thumb" getThumbfromJson 
  }

getAssetsDetailsfromJson
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      DetailsAssets
getAssetsDetailsfromJson = collect
  { preview: field "preview" getThumbfromJson
  , smallThumb: field "small_thumb" getThumbfromJson
  , largeThumb: field "large_thumb" getThumbfromJson
  , huge: field "huge_jpg" getDetailfromJson
  }

getDetailfromJson
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Detail
getDetailfromJson = collect
  { height: field "height" int
  , width: field "width" int
  }

getThumbfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Thumb
getThumbfromJson = collect
  { height: field "height" int
  , width: field "width" int
  , url: field "url" string
  }

buildRequest :: Request -> AffjaxRequest Unit
buildRequest r =
  let url = r # toUrlEncoded # encode
  in defaultRequest {
    url = "https://api.shutterstock.com/v2/images/search?" <> url
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

search 
  :: forall t err
   . Request
  -> Aff ( ajax :: AJAX| t)
      (V (Array (Variant (SearchErrorRow err))) (Search Image))
search req = (runValidation searchValidation) (buildRequest req)

buildDetailsRequest :: String -> AffjaxRequest Unit
buildDetailsRequest id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/" <> id
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

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

searchAndRetrieve 
  :: forall err ext
   . Request
  -> Aff (ajax :: AJAX | ext)
      (V
        (Array (Variant (SearchErrorRow err)))
        (Array ImageDetails)
      )
searchAndRetrieve req = runValidation searchAndRetrieveValidation (buildRequest req)