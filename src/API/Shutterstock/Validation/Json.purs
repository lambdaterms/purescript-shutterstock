module API.Shutterstock.Validation.Json where

import Prelude

import API.Shutterstock.Types (BasicAssets, Detail, DetailsAssets, Image, ImageDetails, Thumb, SearchResult)
import Data.Argonaut (Json)
import Data.Record.Fold (collect)
import Data.Variant (Variant)
import Polyform.Validation (Validation)
import Validators.Json (JsError, arrayOf, field, int, number, string)

searchResult
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      (SearchResult Image)
searchResult = collect
  { page: field "page" int
  , perPage: field "per_page" int
  , totalCount: field "total_count" int
  , searchId: field "search_id" string
  , photos: field "data" $ arrayOf image
  }

imageWithDetails 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      ImageDetails
imageWithDetails = collect
  { id: field "id" string
  , description: field "description" string
  , imageType: field "image_type" string
  , mediaType: field  "image_type" string
  , aspect: field "aspect" number
  , assets: field "assets" $ assetsDetails}

image 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Image
image = collect
  { id: field "id" string
  , description: field "description" string
  , imageType: field "image_type" string
  , mediaType: field  "image_type" string
  , aspect: field "aspect" number
  , assets: field "assets" $ asset}

asset 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      BasicAssets
asset = collect
  { preview: field "preview" thumb
  , smallThumb: field "small_thumb" thumb
  , largeThumb: field "large_thumb" thumb 
  }

assetsDetails
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      DetailsAssets
assetsDetails = collect
  { preview: field "preview" thumb
  , smallThumb: field "small_thumb" thumb
  , largeThumb: field "large_thumb" thumb
  , huge: field "huge_jpg" detail
  }

detail
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Detail
detail = collect
  { height: field "height" int
  , width: field "width" int
  }

thumb 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      Thumb
thumb = collect
  { height: field "height" int
  , width: field "width" int
  , url: field "url" string
  }