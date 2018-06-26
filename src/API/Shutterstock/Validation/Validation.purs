module API.Shutterstock.Validation.Validation where

import Prelude

import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Requests (details, request)
import API.Shutterstock.Types (Image, ImageDetails, Request, SearchResult)
import API.Shutterstock.Validation.Json (imageWithDetails, searchResult, searchResult)
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

searchValidation
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array(Variant(SearchErrorRow err)))
      (AffjaxRequest Unit)
      (SearchResult Image)
searchValidation = searchResult <<< affjaxJson

retrieve 
  :: forall ext err
   . Validation
      ( Aff( ajax :: AJAX| ext))
      (Array(Variant(SearchErrorRow err)))
      (AffjaxRequest Unit)
      ImageDetails
retrieve = imageWithDetails <<< affjaxJson

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
    getDetails id = runValidation retrieve (details id)

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