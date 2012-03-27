{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Version (showVersion)
import Paths_hlinode (version)
import System.Console.CmdArgs.Implicit

import Network.Linode

main :: IO ()
main = (processCmd =<<) $ cmdArgs $
  modes
    [ cmdDomainList
    , cmdDomainResourceList
    , cmdDomainResourceCreateA
    ]
  &= summary versionString
  &= program "hlinode"

versionString :: String
versionString =
  "hlinode " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    CmdDomainList
  | CmdDomainResourceList { cmdDomainResourceListDomainID :: Int }
  | CmdDomainResourceCreateA
  { cmdDomainResourceCreateADomainID :: Int
  , cmdDomainResourceCreateAFQDN :: String
  , cmdDomainResourceCreateATarget :: String
  }
  deriving (Data, Typeable)

cmdDomainList :: Cmd
cmdDomainList = CmdDomainList
  &= help "List domains the API key have access to."
  &= explicit
  &= name "list-domains"

cmdDomainResourceList :: Cmd
cmdDomainResourceList = CmdDomainResourceList
  { cmdDomainResourceListDomainID = def
    &= typ "DomainID"
    &= explicit
    &= name "d"
    &= name "domain"
  } &= help "List the resources associated to a domain."
    &= explicit
    &= name "list-resources"

cmdDomainResourceCreateA :: Cmd
cmdDomainResourceCreateA = CmdDomainResourceCreateA
  { cmdDomainResourceCreateADomainID = def
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

processCmd :: Cmd -> IO ()
processCmd CmdDomainList{..} = do
  apiKey <- readFile "api-key.txt"
  mdomains <- domainList apiKey
  case mdomains of
    Nothing -> putStrLn "Some error occured."
    Just domains -> print domains

processCmd CmdDomainResourceList{..} = do
  apiKey <- readFile "api-key.txt"
  resources <- domainResourceList apiKey cmdDomainResourceListDomainID
  print resources

processCmd CmdDomainResourceCreateA{..} = do
  apiKey <- readFile "api-key.txt"
  resource <- domainResourceCreateA apiKey cmdDomainResourceCreateADomainID
    cmdDomainResourceCreateAFQDN cmdDomainResourceCreateATarget
  print resource
