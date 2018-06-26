module API.Shutterstock.Types where

type Url = String

type DetailsRow ext = ( height ∷ Int, width ∷ Int | ext )
type Thumb = Record(DetailsRow(url ∷ Url))
type Detail = Record(DetailsRow ())

-- Assets types
type AssetsRow a =
  ( largeThumb ∷ Thumb
  , preview ∷ Thumb
  , smallThumb ∷ Thumb
  | a
  )
type BasicAssets = Record(AssetsRow ())
type DetailsAssets = Record(AssetsRow( huge :: Detail))

-- Image description types
type ImageRow ext =
  ( id ∷ String
  , description ∷ String
  , imageType ∷ String
  , mediaType ∷ String
  , aspect ∷ Number
  , assets ∷ Record (AssetsRow ext)
  )
type Image = Record(ImageRow ())
type ImageDetails = Record( ImageRow( huge :: Detail))

-- search response with array of images as param
type SearchResult image =
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