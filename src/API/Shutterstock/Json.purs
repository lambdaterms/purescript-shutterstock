module API.Shutterstock.Json where


import Prelude

import API.Shutterstock.Types (BasicAssets, Detail, DetailsAssets, Image, ImageDetails, Search, Thumb)
import Data.Argonaut (Json)
import Data.Record.Fold (collect)
import Data.Variant (Variant)
import Polyform.Validation (Validation)
import Validators.Json (JsError, arrayOf, field, int, number, string)

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