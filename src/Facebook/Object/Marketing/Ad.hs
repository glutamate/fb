
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.Ad where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics

import qualified Control.Monad.Trans.Resource as R
import Data.Aeson

import Facebook.Types
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Object.Checkin
import Facebook.Object.Marketing.AdUser (AdUser)

import Facebook.Object.Marketing.Types hiding (Id)
import Facebook.Object.Marketing.Utility

-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/adgroup/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

data AdGroupStatus = Active
                   | Paused
                   | PendingReview
                    deriving (Eq, Show, Typeable, Generic)


instance FromJSON AdGroupStatus
instance ToJSON AdGroupStatus

data ExeceutionOption = ValidateOnly
                        deriving (Eq, Show, Typeable, Generic)

instance FromJSON ExeceutionOption
instance ToJSON ExeceutionOption

data SocialPreference = AllowShares
                      | None
                        deriving (Eq, Show, Typeable, Generic)

instance FromJSON SocialPreference
instance ToJSON SocialPreference

data Ad = Ad
  { a_name              :: Maybe Text
  , a_adgroup_status    :: Maybe Text
  , a_bid_amount        :: Maybe Int
  , a_campaign_id       :: Maybe Integer
  , a_creative            :: Maybe Value
  , a_engagement_audience :: Maybe Bool
  , a_execution_options    :: Maybe [ExeceutionOption]
  , a_redownloadb         :: Maybe Bool
  , a_tracking_specs      :: Maybe Value
  , a_social_prefs        :: Maybe [SocialPreference]
  , a_id                  :: Text
  } deriving (Eq, Show, Typeable, Generic)

instance FromJSON Ad where
  parseJSON = parseJSONWithPrefix "a_"

instance ToJSON Ad where
  toJSON = toJSONWithPrefix "a_"

getAdSetAds :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> [Argument]
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager Ad)
getAdSetAds (Id id_) query tok = getObject ("/v2.5/" <> id_ <> "/ads") query (Just tok)
