{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Interests where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import GHC.Generics (Generic)
import Control.Monad
import Control.Applicative (pure, liftA)

import Facebook
import Facebook.Graph
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Control (MonadBaseControl)

data Interest = Interest {
  id :: T.Text
} deriving (Show, Generic, Eq)

instance ToJSON Interest
instance FromJSON Interest

data TargetingType = AdInterest | Income deriving (Show, Eq)

instance ToJSON TargetingType where
  toJSON AdInterest = "adinterest"
  toJSON Income = "income"

instance FromJSON TargetingType where
  parseJSON (String "adinterest") = return AdInterest
  parseJSON (String "income") = return Income


data QueryResult = QueryResult {
    id_ :: T.Text
  , name :: T.Text
  , audience_size :: Int
  , path :: [T.Text]
  , description :: Maybe T.Text
  , cat :: Maybe T.Text
  , topic :: Maybe T.Text
} deriving (Show, Generic)

data QueryResults = QueryResults {
  data_ :: [QueryResult]
} deriving (Show, Generic)

instance FromJSON QueryResults where
  parseJSON  = genericParseJSON defaultOptions {fieldLabelModifier = take $ length ("data" :: String)}
instance ToJSON QueryResults where
  toJSON (QueryResults res) = toJSON res

instance ToJSON QueryResult
instance FromJSON QueryResult where
  parseJSON (Object v) = do
    id <- v .: "id"
    name <- v .: "name"
    audience <- v .: "audience_size"
    path <- v .: "path"
    desc <- v .:? "description"
    cat <- v .:? "disambiguation_category"
    topic <- v .:? "topic"
    return $ QueryResult id name audience path desc cat topic
  parseJSON x = fail $ show x

queryInterest :: (R.MonadResource m, MonadBaseControl IO m)
  => T.Text -> UserAccessToken -> FacebookT Auth m QueryResults
queryInterest query tok =
  getObject "/v2.7/search" [("type", "adinterest"), ("q", T.encodeUtf8 query)] $ Just tok

queryTargeting :: (R.MonadResource m, MonadBaseControl IO m)
  => TargetingType -> T.Text -> UserAccessToken -> FacebookT Auth m QueryResults
queryTargeting AdInterest query tok =
  getObject "/v2.7/search" [("type", "adinterest"), ("q", T.encodeUtf8 query)] $ Just tok
queryTargeting Income query tok =
  case query of
    "" -> getObject "/v2.7/search" [("type", "adTargetingCategory"), ("class", "income")] $ Just tok
    q  -> getObject "/v2.7/search" [("type", "adTargetingCategory"), ("class", "income"), ("q", T.encodeUtf8 query)] $ Just tok
