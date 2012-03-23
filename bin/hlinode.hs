{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Network.Linode

main :: IO ()
main = do
  apiKey <- readFile "api-key.txt"
  domains <- domainList apiKey
  print domains
