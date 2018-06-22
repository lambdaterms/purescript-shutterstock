module API.Shutterstock.Api where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Search (Request, toUrlEncoded)
import API.Shutterstock.Types (BasicAssets, Image, ImageDetails, Search, Thumb, ProductionImage, DetailsAssets)
import Control.Monad.Aff (Aff)
import Control.Monad.State (get)
import Control.Parallel (parTraverse)
import DOM.HTML.HTMLElement (offsetWidth)
import Data.Argonaut (Json)
import Data.Array (filter, zip, zipWith)
import Data.Either (Either(Left))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Monoid (class Monoid)
import Data.Record (insert)
import Data.Record.Fold (collect)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (SProxy(..), Variant)
import Debug.Trace (traceAnyA, traceAnyM)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V(..), Validation, hoistFn, hoistFnMV, runValidation)
import Type.Data.Boolean (True)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow, affjaxJson)
import Validators.Json (JsError, arrayOf, field, int, number, string)

type SearchErrorRow err = 
  ( JsonErrorRow
  ( JsError
  ( HttpErrorRow
  ( AffjaxErrorRow err
  ))))

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

-- TODO: add info about full sizes
-- TODO: put these json functions somewhere else
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
  , huge: field "huge_jpg" getDetailPhotofromJson
  }

getDetailPhotofromJson
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      ProductionImage
getDetailPhotofromJson = collect
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

search 
  :: forall t292 err
   . Request
  -> Aff ( ajax :: AJAX| t292)
      (V (Array (Variant (SearchErrorRow err))) (Search Image))
search req = (runValidation $ getResultfromJson <<< affjaxJson) (buildRequest req)

buildDetailsRequest :: String -> AffjaxRequest Unit
buildDetailsRequest id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/" <> id
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

retrieve :: forall ext err.
  Validation
    ( Aff( ajax :: AJAX| ext))
    (Array(Variant(SearchErrorRow err)))
    (AffjaxRequest Unit)
    ProductionImage
retrieve = (getDetailPhotofromJson <<< affjaxJson)

catV :: forall t err. Monoid err => Array (V err t) -> V err (Array t)
catV = sequence <<< filter (case _ of 
  Valid _ _ -> true
  otherwise -> false) 

addDetailsToArray array = do
  a'<-parTraverse getDetails (map _.id array)
  pure $ catV (zipWith addImageDetails array a')
  where
  getDetails id = runValidation retrieve (buildDetailsRequest id)

addImageDetails :: Image -> V _ _ -> _
addImageDetails photo = map (\details -> photo{assets = insert (SProxy::SProxy "huge") details photo.assets})


searchAndRetrieveValidation = 
  (hoistFnMV addDetailsToArray) 
  <<< hoistFn (_.photos) 
  <<< getResultfromJson 
  <<< affjaxJson

searchAndRetrieve req = runValidation searchAndRetrieveValidation (buildRequest req)

-- searchAndRetrieve req = do
--   let search = (hoistFn(map _.photos) <<< getResultfromJson <<< affjaxJson)
--   (vPhotos::_) <- (map _.photos) <$> (search req)
--   vPhotos
--   pure vPhotos
  
 -- ((\y-> parTraverse  y)) <$> vPhotos
