{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Linode where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Attoparsec.Lazy (parse, Result(..))
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import Network.HTTP.Enumerator

-- | Build a POST request for the Linode API from an API key and parameters.
-- The API key can be obtained from a Linode user's profile
-- (https://manager.linode.com/profile/).
apiRequest :: String -> [(B.ByteString, B.ByteString)] -> IO (Request IO)
apiRequest apiKey parameters = do
  request <- parseUrl "https://api.linode.com/"
  let request' = flip urlEncodedBody request $
        ("api_key", B.pack apiKey) : parameters
  return request'

-- | Post a request built with `apiRequest` to Linode and return an Aeson
-- value.
apiCall :: String -> [(B.ByteString, B.ByteString)] -> IO (Maybe ApiResponse)
apiCall apiKey parameters = do
  request <- apiRequest apiKey parameters
  Response{..} <- withManager $ httpLbs request
  case parse json responseBody of
    Done _ value -> do
      case fromJSON value of
        Success apiResponse -> do
          return $ Just apiResponse
        _ -> return Nothing
    _ -> return Nothing

-- | List domains the API key have access to.
domainList :: String -> IO (Maybe [Domain])
domainList apiKey = do
  mApiResponse <- apiCall apiKey [("api_action", "domain.list")]
  -- print mApiResponse
  case (fromJSON . apiResponseData) <$> mApiResponse of
    Just (Success domains) -> return $ Just domains
    x -> do
      -- print x
      return Nothing

-- | List the resources associated to a domain.
domainResourceList :: String -> Int -> IO (Maybe ApiResponse)
domainResourceList apiKey domainId = apiCall apiKey
  [ ("api_action", "domain.resource.list")
  , ("DomainID", B.pack $ show domainId)
  ]

data Domain = Domain
  { domainId :: Int
  }
  deriving Show

instance FromJSON Domain where
  parseJSON (Object v) = Domain <$>
    v .: "DOMAINID"
  parseJSON _ = mzero

data ApiResponse = ApiResponse
  { apiResponseErrors :: [Value]
  , apiResponseAction :: Text
  , apiResponseData :: Value
  }
  deriving Show

instance FromJSON ApiResponse where
  parseJSON (Object v) = ApiResponse <$>
    v .: "ERRORARRAY" <*>
    v .: "ACTION" <*>
    v .: "DATA"
  parseJSON _ = mzero
