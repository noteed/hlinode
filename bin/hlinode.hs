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
    ]
  &= summary versionString
  &= program "hlinode"

versionString :: String
versionString =
  "hlinode " ++ showVersion version ++ " Copyright (c) 2012 Vo Minh Thu."

data Cmd =
    CmdDomainList
  deriving (Data, Typeable)

cmdDomainList :: Cmd
cmdDomainList = CmdDomainList
  &= help "List domains the API key have access to."

processCmd :: Cmd -> IO ()
processCmd CmdDomainList{..} = do
  apiKey <- readFile "api-key.txt"
  domains <- domainList apiKey
  print domains
