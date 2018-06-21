module Test.Main where
  
import Prelude

import API.Shutterstock.Api (getResultfromJson, search, buildRequest)
import API.Shutterstock.Key (accessToken)
import API.Shutterstock.Search (Request(..))
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Polyform.Validation (runValidation)
import Validators.Affjax (affjaxJson)

simpleRequest :: Request
simpleRequest =
  { page: 1
  , perPage: 15
  , query: "dog"
  }

simpleRequest1 = defaultRequest
      { url = "https://api.shutterstock.com/v2/images/search?query=dog&page=1&per_page=15"
      , method = Left GET
      , headers = [ RequestHeader "Authorization" ("Bearer " <> accessToken) ]
      }

main 
  :: forall t22
   . Eff ( ajax :: AJAX, console :: CONSOLE| t22)
      (Fiber( ajax :: AJAX, console :: CONSOLE| t22) Unit )
main = launchAff $ do
  -- res0 <- (runValidation $ getResultfromJson <<< affjaxJson) (buildRequest simpleRequest)
  -- log $ unsafeStringify res0

  res1 <- (unsafeStringify <$> search simpleRequest)
  log res1
--   res2 <- (unsafeStringify <$> searchAndRetrieve simpleRequest)
--   log res2