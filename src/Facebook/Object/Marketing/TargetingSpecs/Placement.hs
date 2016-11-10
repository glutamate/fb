{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Placement where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Char

-- API v2.7 introduced the following placement options:

data DevicePlatform = Mobile | Desktop
  deriving (Show, Eq, Generic)

instance ToJSON DevicePlatform where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON DevicePlatform where
    parseJSON (String "mobile") = pure Mobile
    parseJSON (String "desktop") = pure Desktop
    parseJSON v = typeMismatch "DevicePlatform" v

data PublisherPlatform = Facebook | Instagram | AudienceNetwork
  deriving (Show, Eq, Generic)

instance ToJSON PublisherPlatform where
    toJSON Facebook = String "facebook"
    toJSON Instagram = String "instagram"
    toJSON AudienceNetwork = String "audience_network"

instance FromJSON PublisherPlatform where
    parseJSON (String "facebook") = pure Facebook
    parseJSON (String "instagram") = pure Instagram
    parseJSON (String "audience_network") = pure AudienceNetwork
    parseJSON v = typeMismatch "PublisherPlatform" v

-- it is unclear if there is also instagram_positions and if so
-- what the values can be
data FacebookPosition = Feed | RightHandColumn
  deriving (Show, Eq, Generic)

instance ToJSON FacebookPosition where
    toJSON Feed = String "feed"
    toJSON RightHandColumn = String "right_hand_column"

instance FromJSON FacebookPosition where
    parseJSON (String "feed") = pure Feed
    parseJSON (String "right_hand_column") = pure RightHandColumn
    parseJSON v = typeMismatch "FacebookPosition" v

{-
device_platforms, - mobile, desktop - any number of.

publisher_platforms - facebook, instagram, audience_network - any number of.

facebook_positions - feed, right_hand_column
-}
