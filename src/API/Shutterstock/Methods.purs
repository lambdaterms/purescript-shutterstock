module API.Shutterstock.Methods where


import API.Shutterstock.Requests (license, request, download) as Requests
import API.Shutterstock.Types (DownloadImage, Image, ImageDetails, Request, SearchResult, License)
import API.Shutterstock.Validation.Validation (searchAndRetrieveValidation, searchValidation)
import Control.Monad.Aff (Aff)
import Data.Argonaut (Json)
import Data.Variant (Variant)
import Network.HTTP.Affjax (AJAX)
import Polyform.Validation (V, runValidation)
import Validators.Affjax (AffjaxErrorRow, HttpErrorRow, JsonErrorRow, affjaxJson)
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
search req = (runValidation searchValidation) (Requests.request req)

searchAndRetrieve 
  :: forall err ext
   . Request
  -> Aff (ajax :: AJAX | ext)
      (V
        (Array (Variant (SearchErrorRow err)))
        (Array ImageDetails)
      )
searchAndRetrieve req = runValidation searchAndRetrieveValidation (Requests.request req)


download :: forall err t29.
  DownloadImage
  -> Aff
       ( ajax :: AJAX
       | t29
       )
       (V 
          (Array (Variant (SearchErrorRow err)))
          Json
       )
download image = runValidation affjaxJson (Requests.download image)


license :: forall err t29.
  License
  -> Aff
       ( ajax :: AJAX
       | t29
       )
       (V 
          (Array (Variant (SearchErrorRow err)))
          Json
       )
license req = runValidation affjaxJson (Requests.license req)