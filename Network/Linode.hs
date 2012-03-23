{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Linode where

import Data.Aeson
import Data.Attoparsec.Lazy (parse, Result(..))
import qualified Data.ByteString.Char8 as B
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
apiCall :: String -> [(B.ByteString, B.ByteString)] -> IO (Maybe Value)
apiCall apiKey parameters = do
  request <- apiRequest apiKey parameters
  Response{..} <- withManager $ httpLbs request
  case parse json responseBody of
    Done _ value -> return $ Just value
    _ -> return Nothing

-- | List domains the API key have access to.
domainList :: String -> IO (Maybe Value)
domainList apiKey = apiCall apiKey [("api_action", "domain.list")]

-- | List the resources associated to a domain.
domainResourceList :: String -> Int -> IO (Maybe Value)
domainResourceList apiKey domainId = apiCall apiKey
  [ ("api_action", "domain.resource.list")
  , ("DomainID", B.pack $ show domainId)
  ]
