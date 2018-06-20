module API.Shutterstock.Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Global.Unsafe (unsafeStringify)
import Simple.JSON (class ReadForeign, class WriteForeign)
import Type.Prelude (SProxy(SProxy))
-- import Simple.JSON (enumReadForeignLowercase)

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

newtype Height = Height Int
derive instance newtypeHeight ∷ Newtype Height _
derive instance genericHeight ∷ Generic Height _
derive newtype instance readForeignHeight ∷ ReadForeign Height
derive newtype instance writeForeignHeight ∷ WriteForeign Height
derive instance eqHeight ∷ Eq Height
derive instance ordHeight ∷ Ord Height
instance showHeight ∷ Show Height where
  show = genericShow

newtype Width = Width Int
derive instance newtypeWidth ∷ Newtype Width _
derive instance genericWidth ∷ Generic Width _
derive newtype instance readForeignWidth ∷ ReadForeign Width
derive newtype instance writeForeignWidth ∷ WriteForeign Width
derive instance eqWidth ∷ Eq Width
derive instance ordWidth ∷ Ord Width
instance showWidth ∷ Show Width where
  show = genericShow

newtype Url = Url String
derive instance newtypeUrl ∷ Newtype Url _
derive instance genericUrl ∷ Generic Url _
derive newtype instance readForeignUrl ∷ ReadForeign Url
derive newtype instance writeForeignUrl ∷ WriteForeign Url
derive instance eqUrl ∷ Eq Url
derive instance ordUrl ∷ Ord Url
instance showUrl ∷ Show Url where
  show = genericShow

type Thumb =
  { height ∷ Height
  , width ∷ Width
  , url ∷ Url
  }
_url = prop (SProxy ∷ SProxy "url")
_width = prop (SProxy ∷ SProxy "width")
_height = prop (SProxy ∷ SProxy "height")

data ImageType = Photo | Illustration | Vector
derive instance genericImageType ∷ Generic ImageType _
derive instance eqImageType ∷ Eq ImageType
derive instance ordImageType ∷ Ord ImageType
instance showImageType ∷ Show ImageType where
  show = genericShow
-- instance readForeignImageType :: ReadForeign ImageType where
--   readImpl = enumReadForeignLowercase

data Format = Jpeg
derive instance genericFormat ∷ Generic Format _
derive instance eqFormat ∷ Eq Format
instance showFormat ∷ Show Format where
  show = unsafeStringify

type ProductionImage =
  { --display_name ∷ String
  -- , dpi ∷ Int
  -- , file_size ∷ Int
  -- , format ∷ Format
  -- , is_licensable ∷ Boolean
    height ∷ Int
  , width ∷ Int
  }
type AssetsRow a =
  ( large_thumb ∷ Thumb
  , preview ∷ Thumb
  , small_thumb ∷ Thumb
  | a
  )

type ImageRecord a =
  { id ∷ ImageId
  , description ∷ String
  , image_type ∷ String
  , media_type ∷ String
  , aspect ∷ Number
  , assets ∷ Record (AssetsRow a)
  }
newtype Image = Image (ImageRecord ())
derive instance newtypeImage ∷ Newtype Image _
derive instance genericImage ∷ Generic Image _
derive newtype instance writeImage ∷ WriteForeign Image
derive newtype instance readImage ∷ ReadForeign Image
derive instance eqImage ∷ Eq Image
-- | Required by purescript-test-unit
instance showImage ∷ Show Image where
  show = unsafeStringify

newtype ImageDetails = ImageDetails (ImageRecord (huge_jpg ∷ ProductionImage))
derive instance newtypeImageDetails ∷ Newtype ImageDetails _
derive instance genericImageDetails ∷ Generic ImageDetails _
derive newtype instance writeImageDetails ∷ WriteForeign ImageDetails
derive newtype instance readImageDetails ∷ ReadForeign ImageDetails
derive instance eqImageDetails ∷ Eq ImageDetails
-- | Required by purescript-test-unit
instance showImageDetails ∷ Show ImageDetails where
  show = unsafeStringify

newtype Search image = Search
  { page ∷ Int
  , per_page ∷  Int
  , total_count ∷ Int
  , search_id ∷ String
  , data ∷ Array image
  }
derive instance newtypeSearch ∷ Newtype (Search image) _
derive instance genericSearch ∷ Generic (Search image) _
derive instance eqSearch ∷ Eq image ⇒ Eq (Search image)
instance showSearch ∷ Show (Search image) where
  show = unsafeStringify
derive instance functorSearch ∷ Functor Search


_images = prop (SProxy ∷ SProxy "data")
_assets = prop (SProxy ∷ SProxy "assets")
_preview = prop (SProxy ∷ SProxy "preview")

