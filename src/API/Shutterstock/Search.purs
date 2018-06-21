module API.Shutterstock.Search where

type Request =
  { page :: Int
  , perPage :: Int
  , query :: String
  }
