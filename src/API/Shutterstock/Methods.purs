module API.Shutterstock.Methods where


import API.Shutterstock.Requests (request)
import API.Shutterstock.Types (Image, ImageDetails, Request, SearchResult)
import API.Shutterstock.Validation.Validation (searchAndRetrieveValidation, searchValidation)
import Control.Monad.Aff (Aff)
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX)
import Polyform.Validation (V, runValidation)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow)
import Validators.Json (JsError)

type SearchErrorRow err = 
  ( JsonErrorRow
  ( JsError
  ( HttpErrorRow
  ( AffjaxErrorRow err
  ))))


search 
  :: forall t err
   . Request
  -> Aff ( ajax :: AJAX| t)
      (V
        (Array (Variant (SearchErrorRow err))) 
        (SearchResult Image))
search req = (runValidation searchValidation) (request req)

searchAndRetrieve 
  :: forall err ext
   . Request
  -> Aff (ajax :: AJAX | ext)
      (V
        (Array (Variant (SearchErrorRow err)))
        (Array ImageDetails)
      )
searchAndRetrieve req = runValidation searchAndRetrieveValidation (request req)