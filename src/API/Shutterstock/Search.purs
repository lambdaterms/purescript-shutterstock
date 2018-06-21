module API.Shutterstock.Search where

import Prelude

import Data.Foldable (intercalate)
import Data.FormURLEncoded (FormURLEncoded(..), fromArray)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type Request =
  { page :: Int
  , perPage :: Int
  , query :: String
  }


toUrlEncoded :: Request -> FormURLEncoded
toUrlEncoded {query, page, perPage} = fromArray
  [ Tuple "query" (Just query)
  , Tuple "per_page" (Just $ show perPage)
  , Tuple "page" (Just $ show page)
  ]
