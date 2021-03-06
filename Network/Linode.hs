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
apiCall' :: String -> [(B.ByteString, B.ByteString)] -> IO (Maybe ApiResponse)
apiCall' apiKey parameters = do
  request <- apiRequest apiKey parameters
  Response{..} <- withManager $ httpLbs request
  case parse json responseBody of
    Done _ value -> do
      case fromJSON value of
        Success apiResponse -> do
          return $ Just apiResponse
        _ -> return Nothing
    _ -> return Nothing

-- | Similar to `apiCall'` but extract the `apiResponseData`,
-- parsed it (using `fromJSON`), and return the result.
apiCall :: FromJSON a =>
  String -> [(B.ByteString, B.ByteString)] -> IO (Maybe a)
apiCall apiKey parameters = do
  mApiResponse <- apiCall' apiKey parameters
  -- print mApiResponse
  case (fromJSON . apiResponseData) <$> mApiResponse of
    Just (Success x) -> return $ Just x
    _ -> do
      return Nothing

-- | List domains the API key have access to.
domainList :: String -> IO (Maybe [Domain])
domainList apiKey = apiCall apiKey [("api_action", "domain.list")]

-- | List the resources associated to a domain.
domainResourceList :: String -> Int -> IO (Maybe [Resource])
domainResourceList apiKey domainId = apiCall apiKey
  [ ("api_action", "domain.resource.list")
  , ("DomainID", B.pack $ show domainId)
  ]

-- | Create a domain record.
domainResourceCreateA :: String -> Int -> String -> String -> IO (Maybe ResourceId)
domainResourceCreateA apiKey domainId fqdn target = apiCall apiKey
  [ ("api_action", "domain.resource.create")
  , ("DomainID", B.pack $ show domainId)
  , ("Type", "A")
  , ("Name", B.pack fqdn)
  , ("Target", B.pack target)
  ]

-- | Create a domain record.
domainResourceCreateCNAME :: String -> Int -> String -> String -> IO (Maybe ResourceId)
domainResourceCreateCNAME apiKey domainId fqdn target = apiCall apiKey
  [ ("api_action", "domain.resource.create")
  , ("DomainID", B.pack $ show domainId)
  , ("Type", "CNAME")
  , ("Name", B.pack fqdn)
  , ("Target", B.pack target)
  ]

-- | Delete a domain record.
domainResourceDelete :: String -> Int -> Int -> IO (Maybe ResourceId)
domainResourceDelete apiKey domainId resourceId = apiCall apiKey
  [ ("api_action", "domain.resource.delete")
  , ("DomainID", B.pack $ show domainId)
  , ("ResourceID", B.pack $ show resourceId)
  ]

-- | Represent a domain.
data Domain = Domain
  { domainId :: Int
  , domainDescription :: Text
  , domainType :: Text
  , domainStatus :: Int
  , domainSoaEmail :: Text
  , domainDomain :: Text
  , domainRetrySec :: Int
  , domainMasterIps :: Text
  , domainExpireSec :: Int
  , domainRefreshSec :: Int
  , domainTtlSec :: Int
  }
  deriving Show

instance FromJSON Domain where
  parseJSON (Object v) = Domain <$>
    v .: "DOMAINID" <*>
    v .: "DESCRIPTION" <*>
    v .: "TYPE" <*>
    v .: "STATUS" <*>
    v .: "SOA_EMAIL" <*>
    v .: "DOMAIN" <*>
    v .: "RETRY_SEC" <*>
    v .: "MASTER_IPS" <*>
    v .: "EXPIRE_SEC" <*>
    v .: "REFRESH_SEC" <*>
    v .: "TTL_SEC"
  parseJSON _ = mzero

-- | Represent a domain resource
data Resource = Resource
  { resourceProtocol :: Text
  , resourceTtlSec :: Int
  , resourcePriority :: Int
  , resourceType :: Text
  , resourceTarget :: Text
  , resourceWeight :: Int
  , resourceId :: Int
  , resourcePort :: Int
  , resourceDomainId :: Int
  , resourceName :: Text
  }
  deriving Show

instance FromJSON Resource where
  parseJSON (Object v) = Resource <$>
    v .: "PROTOCOL" <*>
    v .: "TTL_SEC" <*>
    v .: "PRIORITY" <*>
    v .: "TYPE" <*>
    v .: "TARGET" <*>
    v .: "WEIGHT" <*>
    v .: "RESOURCEID" <*>
    v .: "PORT" <*>
    v .: "DOMAINID" <*>
    v .: "NAME"
  parseJSON _ = mzero

-- | Represent a domain resource ID, e.g. the result of
-- domain.resource.create.
data ResourceId = ResourceId Int
  deriving Show

instance FromJSON ResourceId where
  parseJSON (Object v) = ResourceId <$>
    v .: "ResourceID"
  parseJSON _ = mzero

-- | Represent a Linode response in a slightly more structured data type than
-- just JSON. Still some parts are kept in JSON.
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
