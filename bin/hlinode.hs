{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Version (showVersion)
import Paths_hlinode (version)
import System.Console.CmdArgs.Implicit
import System.Directory (doesFileExist)
import System.Exit

import Network.Linode

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdDomainList
    , cmdDomainResourceList
    , cmdDomainResourceCreateA
    , cmdDomainResourceDelete
    ]
  &= summary versionString
  &= program "hlinode"

versionString :: String
versionString =
  "hlinode " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    CmdDomainList
  { cmdDomainListApiKey :: String
  }
  | CmdDomainResourceList
  { cmdDomainResourceListApiKey :: String
  , cmdDomainResourceListDomainID :: Int
  }
  | CmdDomainResourceCreateA
  { cmdDomainResourceCreateAApiKey :: String
  , cmdDomainResourceCreateADomainID :: Int
  , cmdDomainResourceCreateAFQDN :: String
  , cmdDomainResourceCreateATarget :: String
  }
  | CmdDomainResourceDelete
  { cmdDomainResourceDeleteApiKey :: String
  , cmdDomainResourceDeleteDomainID :: Int
  , cmdDomainResourceDeleteResourceID :: Int
  }
  deriving (Data, Typeable)

cmdDomainList :: Cmd
cmdDomainList = CmdDomainList
  { cmdDomainListApiKey = "api-key.txt"
    &= typ "PATH"
    &= explicit
    &= name "k"
    &= name "key"
    &= help "Path to a file containing a Linode API key."
  } &= help "List domains the API key have access to."
    &= explicit
    &= name "list-domains"

cmdDomainResourceList :: Cmd
cmdDomainResourceList = CmdDomainResourceList
  { cmdDomainResourceListApiKey = "api-key.txt"
    &= typ "PATH"
    &= explicit
    &= name "k"
    &= name "key"
    &= help "Path to a file containing a Linode API key."
  , cmdDomainResourceListDomainID = def
    &= typ "DomainID"
    &= explicit
    &= name "d"
    &= name "domain"
  } &= help "List the resources associated to a domain."
    &= explicit
    &= name "list-resources"

cmdDomainResourceCreateA :: Cmd
cmdDomainResourceCreateA = CmdDomainResourceCreateA
  { cmdDomainResourceCreateAApiKey = "api-key.txt"
    &= typ "PATH"
    &= explicit
    &= name "k"
    &= name "key"
    &= help "Path to a file containing a Linode API key."
  , cmdDomainResourceCreateADomainID = def
    &= typ "DomainID"
    &= explicit
    &= name "d"
    &= name "domain"
  , cmdDomainResourceCreateAFQDN = def
    &= typ "FQDN"
    &= explicit
    &= name "fqdn"
  , cmdDomainResourceCreateATarget = def
    &= typ "Target"
    &= explicit
    &= name "target"
  } &= help "Create a new domain record."
    &= explicit
    &= name "create-resource-a"

cmdDomainResourceDelete :: Cmd
cmdDomainResourceDelete = CmdDomainResourceDelete
  { cmdDomainResourceDeleteApiKey = "api-key.txt"
    &= typ "PATH"
    &= explicit
    &= name "k"
    &= name "key"
    &= help "Path to a file containing a Linode API key."
  , cmdDomainResourceDeleteDomainID = def
    &= typ "DomainID"
    &= explicit
    &= name "d"
    &= name "domain"
  , cmdDomainResourceDeleteResourceID = def
    &= typ "ResourceID"
    &= explicit
    &= name "r"
    &= name "resource"
  } &= help "Delete a domain record."
    &= explicit
    &= name "delete-resource"

processCmd :: Cmd -> IO ()
processCmd CmdDomainList{..} = do
  apiKey <- readApiKey cmdDomainListApiKey
  mdomains <- domainList apiKey
  case mdomains of
    Nothing -> putStrLn "Some error occured."
    Just domains -> print domains

processCmd CmdDomainResourceList{..} = do
  apiKey <- readApiKey cmdDomainResourceListApiKey
  resources <- domainResourceList apiKey cmdDomainResourceListDomainID
  print resources

processCmd CmdDomainResourceCreateA{..} = do
  apiKey <- readApiKey cmdDomainResourceCreateAApiKey
  resource <- domainResourceCreateA apiKey cmdDomainResourceCreateADomainID
    cmdDomainResourceCreateAFQDN cmdDomainResourceCreateATarget
  print resource

processCmd CmdDomainResourceDelete{..} = do
  apiKey <- readApiKey cmdDomainResourceDeleteApiKey
  resource <- domainResourceDelete apiKey cmdDomainResourceDeleteDomainID
    cmdDomainResourceDeleteResourceID
  print resource

readApiKey :: FilePath -> IO String
readApiKey filename = do
  b <- doesFileExist filename
  if b
    then readFile filename
    else do
      putStrLn $ "API key file `" ++ filename ++ "` not found."
      exitWith (ExitFailure 1)
