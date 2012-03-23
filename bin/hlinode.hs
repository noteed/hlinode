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
    ]
  &= summary versionString
  &= program "hlinode"

versionString :: String
versionString =
  "hlinode " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    CmdDomainList
  | CmdDomainResourceList { cmdDomainResourceListDomainID :: Int }
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
