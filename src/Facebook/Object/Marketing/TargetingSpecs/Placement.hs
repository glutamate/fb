{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.Placement where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics (Generic)
import Data.Char
import Control.Applicative (pure)

-- | Placement
--   See https://developers.facebook.com/docs/marketing-api/reference/ad-campaign#location
-- In API v2.7, this form of placement became read only; it is scheduled to disappear
-- entirely in API v2.8

data PlacementOption = Desktopfeed
                     | RightColumn
                     | MobileFeed
                     | MobileExternal
                     | Home
                     | InstagramStream
                     deriving (Show, Eq, Generic)
instance ToJSON PlacementOption where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON PlacementOption where
    parseJSON (String "desktopfeed") = pure Desktopfeed
    parseJSON (String "rightcolumn") = pure RightColumn
    parseJSON (String "mobilefeed") = pure MobileFeed
    parseJSON (String "mobileexternal") = pure MobileExternal
    parseJSON (String "home") = pure Home
    parseJSON (String "instagramstream") = pure InstagramStream

-- API v2.7 introduced the following placement options:

data DevicePlatform = Mobile | Desktop
  deriving (Show, Eq, Generic)

instance ToJSON DevicePlatform where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON DevicePlatform where
    parseJSON (String "mobile") = pure Mobile
    parseJSON (String "desktop") = pure Desktop

-- TODO: AudienceNetwork should be JSONified as audience_network
data PublisherPlatform = Facebook | Instagram | AudienceNetwork
  deriving (Show, Eq, Generic)

instance ToJSON PublisherPlatform where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON PublisherPlatform where
    parseJSON (String "facebook") = pure Facebook
    parseJSON (String "instagram") = pure Instagram
    parseJSON (String "audience_network") = pure AudienceNetwork

-- it is unclear if there is also instagram_positions and if so
-- what the values can be
data FacebookPosition = Feed | RightHandColumn
  deriving (Show, Eq, Generic)

instance ToJSON FacebookPosition where
    toJSON = genericToJSON defaultOptions {constructorTagModifier = map toLower}

instance FromJSON FacebookPosition where
    parseJSON (String "feed") = pure Feed
    parseJSON (String "right_hand_column") = pure RightHandColumn

{-
device_platforms, - mobile, desktop - any number of.

publisher_platforms - facebook, instagram, audience_network - any number of.

facebook_positions - feed, right_hand_column
-}

