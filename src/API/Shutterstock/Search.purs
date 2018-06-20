module API.Shutterstock.Search where

newtype Request = Request
  { page :: Int
  , perPage :: Int
  , query :: String
  }
