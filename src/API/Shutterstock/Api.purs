module API.Shutterstock.Api where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Search (Request, toUrlEncoded)
import API.Shutterstock.Types (Image, ImageType(..), Search)
import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Either (Either(Left), hush)
import Data.FormURLEncoded (encode)
import Data.HTTP.Method (Method(..))
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Record.Fold (collect)
import Data.Tuple (Tuple(..))
import Data.URI (AbsoluteURI(AbsoluteURI), Authority(Authority), HierarchicalPart(HierarchicalPart), Host(NameAddress), Scheme(Scheme))
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX, AffjaxRequest, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Polyform.Validation (V, Validation, runValidation, toEither)
import Type.Prelude (SProxy(..))
import Validators.Affjax (affjaxJson)
import Validators.Json (JsError, arrayOf, field, int, number, object, string)



getResultfromJson 
  :: forall err m
   . Monad m
  => Validation m
      (Array (Variant (JsError err)))
      Json
      (Search Image )
getResultfromJson = collect
  {page: field "page" int
  , perPage: field "per_page" int
  , totalCount: field "total_count" int
  , searchId: field "search_id" string
  , photos: field "data" $ arrayOf getImagefromJson
  }

-- TODO: assets to record

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
    , assets: field "assets" object  }

  -- id ∷ ImageId
  -- , description ∷ String
  -- , image_type ∷ String
  -- , media_type ∷ String
  -- , aspect ∷ Number
  -- , assets ∷ Record (AssetsRow a)

buildRequest :: Request -> AffjaxRequest Unit
buildRequest r =
  let url = r # toUrlEncoded # encode
  in defaultRequest { 
    url = "https://api.shutterstock.com/v2/images/search?" <> url
    , method = Left GET
    , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ] 
  }


--Search Image, raw ∷ String })
search :: forall t292 t293.
  { page :: Int
  , perPage :: Int
  , query :: String
  }
  -> Aff
       ( ajax :: AJAX
       | t292
       )
       (V
          (Array
             (Variant
                ( jsError :: { path :: List String
                             , msg :: String
                             }
                , parsingError :: String
                , remoteError :: String
                , wrongHttpStatus :: StatusCode
                | t293
                )
             )
          )
          (Search Image)
       )
search req = (runValidation $ getResultfromJson <<< affjaxJson) (buildRequest req)
--      : i "image_type" "photo"
  -- r ← get req
  -- pure r
  -- pure $ (over _result Search) <$> r

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