{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Notion.SubmitTransaction where

import           Control.Exception.Safe             (MonadThrow)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Data.Aeson                         (Options (..),
                                                     SumEncoding (..),
                                                     ToJSON (..), encode,
                                                     genericToJSON)
import           Data.Aeson.Casing                  (aesonDrop, snakeCase)
import qualified Data.ByteString.Char8              as BC
import qualified Data.Text                          as T
import qualified Data.UnixTime                      as UT
import qualified Data.UUID                          as UUID
import qualified Data.UUID.V4                       as UUIDv4
import           GHC.Generics                       (Generic)
import           Network.HTTP.Simple
import           Network.Mime                       (defaultMimeLookup)
import           Notion.GetUserAnalyticsSettings    (getUserID, Config(..))
import           Notion.SubmitTransaction.Operation (Arguments (..),
                                                     Operation (..), URL, UUID)
import qualified Notion.SubmitTransaction.Operation as Op

type Token = String

newtype ReqBody = ReqBody { _reqOperations :: [Operation] }
  deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON = genericToJSON $ aesonDrop 4 snakeCase

endpoint :: URL
endpoint = "https://www.notion.so/api/v3/submitTransaction"

genUUID :: MonadIO m => m UUID
genUUID = UUID.toString <$> liftIO UUIDv4.nextRandom

getUnixTime :: MonadIO m => m Int
getUnixTime = do
  time <- liftIO UT.getUnixTime
  unixtime <- liftIO $ UT.formatUnixTime "%s" time
  return . read . BC.unpack $ unixtime

post :: (MonadIO m, MonadThrow m) => Config -> [Operation] -> m ()
post Config{..} ops = do
  let body = ReqBody { _reqOperations = ops }
  req <- parseRequest endpoint
  let req' = setRequestMethod "POST"
           . setRequestHeader "Cookie" [BC.pack $ "token_v2=" ++ tokenV2]
           . setRequestHeader "User-Agent" [BC.pack userAgent]
           . setRequestBodyJSON body
           $ req
  httpNoBody req'
  return ()

appendRecord :: (MonadIO m, MonadThrow m) => Config -> UUID -> String -> m UUID
appendRecord conf collectionID recordTitle = do
  blockID <- genUUID
  userID <- getUserID conf
  unixTime <- getUnixTime
  post conf $ Op.appendRecord blockID userID unixTime collectionID recordTitle
  return blockID

appendS3File :: (MonadIO m, MonadThrow m) => Config -> UUID -> URL -> m UUID
appendS3File conf pageID url = do
  blockID <- genUUID
  userID <- getUserID conf
  unixTime <- getUnixTime
  post conf $ Op.appendS3File blockID userID unixTime pageID url
  return blockID

appendText :: (MonadIO m, MonadThrow m) => Config -> UUID -> String -> m UUID
appendText conf pageID content = do
  blockID <- genUUID
  userID <- getUserID conf
  unixTime <- getUnixTime
  post conf $ Op.appendText blockID userID unixTime pageID content
  return blockID
