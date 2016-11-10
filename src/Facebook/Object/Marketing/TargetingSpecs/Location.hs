{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Location where

import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Types
import Data.String
import GHC.Generics

import Facebook.Records

-- | Location
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#location

data LocationTypes = Recent
                   | Home
                   | TravelIn
                     deriving (Eq, Show, Generic)

instance FromJSON LocationTypes
instance ToJSON LocationTypes

data TargetLocation = TargetLocation
  { countries :: Maybe [Text]
  , regions :: Maybe [KeyText]
  --, cities :: Maybe [Text]
  --, zips :: Maybe [Text]
  --, custom_locations :: Maybe [Text]
  --, geo_markets :: Maybe [Text]
  --, location_types :: Maybe LocationTypes
  } deriving (Show, Eq, Generic)

instance ToJSON TargetLocation where
    toJSON = genericToJSON defaultOptions { omitNothingFields = True }

instance FromJSON TargetLocation where
    parseJSON = genericParseJSON defaultOptions { omitNothingFields = True }

instance ToBS TargetLocation where
    toBS = toBS . toJSON

data KeyText = KeyText Text
  deriving (Show, Eq)

instance ToJSON KeyText where
    toJSON (KeyText t) = object [
        "key" .= t
      ]

instance FromJSON KeyText where
    parseJSON (Object o) = KeyText <$> (o .: "key")
    parseJSON _ = fail "Could not parse non-Object to key-wrapped text"

instance IsString KeyText where
    fromString s = KeyText (fromString s)

