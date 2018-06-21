module API.Shutterstock.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype)
import Simple.JSON (class ReadForeign, class WriteForeign)

newtype SearchId = SearchId String
derive instance newtypeSearchId ∷ Newtype SearchId _
derive instance genericSearchId ∷ Generic SearchId _
derive instance eqSearchId ∷ Eq SearchId
derive instance ordSearchId ∷ Ord SearchId
instance showSearchId ∷ Show SearchId where
  show = genericShow

newtype ImageId = ImageId String
derive instance newtypeImageId ∷ Newtype ImageId _
derive instance genericImageId ∷ Generic ImageId _
derive newtype instance readForeignImageId ∷ ReadForeign ImageId
derive newtype instance writeForeignImageId ∷ WriteForeign ImageId
derive instance eqImageId ∷ Eq ImageId
derive instance ordImageId ∷ Ord ImageId
instance showImageId ∷ Show ImageId where
  show = genericShow

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
  ( id ∷ String --ImageId
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
