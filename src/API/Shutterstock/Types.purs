module API.Shutterstock.Types where

import Prelude

import Data.FormURLEncoded (FormURLEncoded(..), fromArray)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Simple.JSON (class ReadForeign, class WriteForeign)

type Url = String

type DetailsRow ext = ( height ∷ Int, width ∷ Int | ext )
type Thumb = Record(DetailsRow(url ∷ Url))

type ProductionImage = Record(DetailsRow ())

data ImageType = Photo | Illustration | Vector

data Format = Jpeg

type AssetsRow a =
  ( largeThumb ∷ Thumb
  , preview ∷ Thumb
  , smallThumb ∷ Thumb
  | a
  )
type BasicAssets = Record(AssetsRow ())
type DetailsAssets = Record(AssetsRow( huge :: ProductionImage))

type ImageRow ext =
  ( id ∷ String
  , description ∷ String
  , imageType ∷ String
  , mediaType ∷ String
  , aspect ∷ Number
  , assets ∷ Record (AssetsRow ext)
  )

type Image = Record(ImageRow ())
type ImageDetails = Record( ImageRow( huge :: ProductionImage))

type Search image =
  { page ∷ Int
  , perPage ∷  Int
  , totalCount ∷ Int
  , searchId ∷ String
  , photos ∷ Array image
  }

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
