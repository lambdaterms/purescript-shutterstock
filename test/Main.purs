module Test.Main where
  
import Prelude

import API.Shutterstock.Api (search)--, searchAndRetrieve)
import API.Shutterstock.Search (Request(..))
import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (AJAX)

simpleRequest :: Request
simpleRequest =
  { page: 1
  , perPage: 15
  , query: "dog"
  }

main 
  :: forall t22
   . Eff ( ajax :: AJAX, console :: CONSOLE| t22)
      (Fiber( ajax :: AJAX, console :: CONSOLE| t22) Unit )
main = launchAff $ do 
  res1 <- (unsafeStringify <$> search simpleRequest)
  log res1
--   res2 <- (unsafeStringify <$> searchAndRetrieve simpleRequest)
--   log res2