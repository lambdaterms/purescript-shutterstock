module API.Shutterstock.Api where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Search (Request(..))
import API.Shutterstock.Types (Image, ImageDetails(ImageDetails), ImageId, Search(Search))
import Control.Monad.Aff (Aff, attempt)
import Control.Parallel (parTraverse)
import Data.Array (catMaybes)
import Data.Either (Either(Left), hush)
import Data.HTTP.Method (Method(..))
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Lens.Setter (set)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.URI (AbsoluteURI(..), Authority(..), HierarchicalPart(..), Host(..), Path(..), Query(..), Scheme(..))
import Data.URI.AbsoluteURI (_hierPart, _query, print)
import Data.URI.HierarchicalPart (_path)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Simple.JSON (readJSON)
import Type.Prelude (SProxy(..))

apiEndpoint ∷ AbsoluteURI
apiEndpoint =
  (AbsoluteURI
     (Scheme "https")
     (HierarchicalPart
        (Just
           (Authority
              Nothing
              [(Tuple (NameAddress "api.shutterstock.com") Nothing)]))
        Nothing)
      Nothing)

get path query = do
  let
    url = print $ set (_path >>> _hierPart) (Just path) >>> set _query query $ apiEndpoint
    request = defaultRequest
      { url = url
      , method = Left GET
      , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ]
      }
  r ← attempt (affjax request)
  pure $ do
    r' ← hush r
    result ← hush $ readJSON r'.response
    -- pure { raw: r'.response, result }
    pure { result, raw: r'.response }

_result = prop (SProxy ∷ SProxy "result")

search :: forall t82.
   Request
   -> Aff
        ( ajax :: AJAX
        | t82
        )
        (Maybe { result ∷ Search Image, raw ∷ String })
search (Request { query, page, perPage }) = do
  let
    i n v = Tuple n (Just v)
    q
      = (i "query" query)
      : i "per_page" (show perPage)
      : i "page" (show page)
      : i "image_type" "photo"
      : Nil

  r ← get (Path "/v2/images/search") (Just (Query q))
  pure $ (over _result Search) <$> r

image ∷ ∀ eff. ImageId → Aff (ajax ∷ AJAX | eff) (Maybe { result ∷ ImageDetails, raw ∷ String })
image imageId = do
  r ← get (Path ("/v2/images/" <> unwrap imageId)) Nothing
  pure $ (over _result ImageDetails) <$>  r

searchAndRetrieve
  :: forall eff
  . Request
  → Aff (ajax :: AJAX | eff) (Maybe (Search ImageDetails))
searchAndRetrieve r = do
  search r >>= case _ of
    Just { result: Search result } → do
      let images = result.data
      images' ← (map _.result <<< catMaybes) <$> parTraverse (image <<< _.id <<< unwrap) images
      pure (Just (Search $ result { data = images' }))
    Nothing → pure Nothing
