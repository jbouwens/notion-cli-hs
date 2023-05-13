{-# LANGUAGE RecordWildCards #-}

module Notion.Config where

import           Data.ConfigFile          (ConfigParser (..), emptyCP, get,
                                           readfile)
import           System.Directory         (getHomeDirectory)
import           System.Exit              (die)
import           System.FilePath.Posix    (takeFileName)

data Config = Config { tokenV2 :: String, userAgent :: String }
  deriving (Show, Eq, Read)

getConfig :: FilePath -> IO Config
getConfig filePath = do
  let handle e = die $ "invalid configuration file\n" ++ show e
  val <- readfile emptyCP{optionxform = id} filePath
  either handle return $ do
    cp <- val
    tokenV2 <- get cp "Cookie" "token_v2"
    userAgent <- get cp "UserAgent" "useragent"
    return Config{..}

defaultConfigFile :: FilePath -> FilePath
defaultConfigFile home = home ++ "/.notion-cli.conf"
