module API.Shutterstock.Api where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Search (Request, toUrlEncoded)
import API.Shutterstock.Types (BasicAssets, Image, ImageDetails, Search, Thumb, ProductionImage, DetailsAssets)
import Control.Monad.Aff (Aff)
import Control.Monad.State (get)
import Data.Argonaut (Json)
import Data.Either (Either(Left))
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Record.Fold (collect)
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, AffjaxResponse, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (V, Validation, runValidation)
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

search :: forall t292 err.
  { page :: Int
  , perPage :: Int
  , query :: String
  }
  -> Aff
       ( ajax :: AJAX
       | t292
       )
       (V
          (Array (Variant (SearchErrorRow err)))
          (Search Image)
       )
search req = (runValidation $ getResultfromJson <<< affjaxJson) (buildRequest req)

buildDetailsRequest :: String -> AffjaxRequest Unit
buildDetailsRequest id = defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/" <> id
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }

-- image ∷ ∀ eff. ImageId → Aff (ajax ∷ AJAX | eff) (Maybe { result ∷ ImageDetails, raw ∷ String })
-- image imageId = do
--   r ← get (Path ("/v2/images/" <> unwrap imageId)) Nothing
--   pure $ (over _result ImageDetails) <$>  r

-- searchAndRetrieve
--   :: forall eff
--   . Request
--   → Aff (ajax :: AJAX | eff) (Maybe (Search ImageDetails))
-- searchAndRetrieve r = do
--   search r >>= case _ of
--     Just { result: Search result } → do
--       let images = result.data
--       images' ← (map _.result <<< catMaybes) <$> parTraverse (image <<< _.id <<< unwrap) images
--       pure (Just (Search $ result { data = images' }))
--     Nothing → pure Nothing