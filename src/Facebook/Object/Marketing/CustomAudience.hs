{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Facebook.Object.Marketing.CustomAudience where

import Facebook.Records hiding (get)
import qualified Facebook.Records as Rec
import Facebook.Types hiding (Id)
import Facebook.Pager
import Facebook.Monad
import Facebook.Graph
import Facebook.Base (FacebookException(..))
import qualified Data.Aeson as A
import Data.Time.Format
import Data.Aeson hiding (Value)
import Control.Applicative
import Data.Text (Text)
import Data.Text.Read (decimal)
import Data.Scientific (toBoundedInteger)
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import qualified Control.Monad.Trans.Resource as R
import Control.Monad.Trans.Control (MonadBaseControl)
#if MIN_VERSION_time(1,5,0)
import System.Locale hiding (defaultTimeLocale, rfc822DateFormat)
import Data.Time.Clock
#else
import System.Locale
import Data.Time.Clock hiding (defaultTimeLocale, rfc822DateFormat)
#endif
import Facebook.Object.Marketing.Types

data OriginAudienceId = OriginAudienceId
newtype OriginAudienceId_ = OriginAudienceId_ Text deriving (Show, Generic)
instance Field OriginAudienceId where
  type FieldValue OriginAudienceId = OriginAudienceId_
  fieldName _ = "origin_audience_id"
  fieldLabel = OriginAudienceId
unOriginAudienceId_ :: OriginAudienceId_ -> Text
unOriginAudienceId_ (OriginAudienceId_ x) = x

data LookalikeAudienceIds = LookalikeAudienceIds
newtype LookalikeAudienceIds_ = LookalikeAudienceIds_ (Vector Text) deriving (Show, Generic)
instance Field LookalikeAudienceIds where
  type FieldValue LookalikeAudienceIds = LookalikeAudienceIds_
  fieldName _ = "lookalike_audience_ids"
  fieldLabel = LookalikeAudienceIds
unLookalikeAudienceIds_ :: LookalikeAudienceIds_ -> Vector Text
unLookalikeAudienceIds_ (LookalikeAudienceIds_ x) = x

data ExternalEventSource = ExternalEventSource
newtype ExternalEventSource_ = ExternalEventSource_ AdsPixel deriving (Show, Generic)
instance Field ExternalEventSource where
  type FieldValue ExternalEventSource = ExternalEventSource_
  fieldName _ = "external_event_source"
  fieldLabel = ExternalEventSource
unExternalEventSource_ :: ExternalEventSource_ -> AdsPixel
unExternalEventSource_ (ExternalEventSource_ x) = x

data DeliveryStatus = DeliveryStatus
newtype DeliveryStatus_ = DeliveryStatus_ CustomAudienceStatus deriving (Show, Generic)
instance Field DeliveryStatus where
  type FieldValue DeliveryStatus = DeliveryStatus_
  fieldName _ = "delivery_status"
  fieldLabel = DeliveryStatus
unDeliveryStatus_ :: DeliveryStatus_ -> CustomAudienceStatus
unDeliveryStatus_ (DeliveryStatus_ x) = x

data DataSource = DataSource
newtype DataSource_ = DataSource_ CustomAudienceDataSource deriving (Show, Generic)
instance Field DataSource where
  type FieldValue DataSource = DataSource_
  fieldName _ = "data_source"
  fieldLabel = DataSource
unDataSource_ :: DataSource_ -> CustomAudienceDataSource
unDataSource_ (DataSource_ x) = x

data ApproximateCount = ApproximateCount
newtype ApproximateCount_ = ApproximateCount_ Integer deriving (Show, Generic)
instance Field ApproximateCount where
  type FieldValue ApproximateCount = ApproximateCount_
  fieldName _ = "approximate_count"
  fieldLabel = ApproximateCount
unApproximateCount_ :: ApproximateCount_ -> Integer
unApproximateCount_ (ApproximateCount_ x) = x
instance A.FromJSON OriginAudienceId_
instance A.ToJSON OriginAudienceId_
instance A.FromJSON LookalikeAudienceIds_
instance A.ToJSON LookalikeAudienceIds_
instance A.FromJSON ExternalEventSource_
instance A.ToJSON ExternalEventSource_
instance A.FromJSON DeliveryStatus_
instance A.ToJSON DeliveryStatus_
instance A.FromJSON DataSource_
instance A.ToJSON DataSource_
instance A.FromJSON ApproximateCount_
instance A.ToJSON ApproximateCount_

instance ToBS OriginAudienceId_ where
  toBS (OriginAudienceId_ a) = toBS a

instance ToBS LookalikeAudienceIds_ where
  toBS (LookalikeAudienceIds_ a) = toBS a

instance ToBS ExternalEventSource_ where
  toBS (ExternalEventSource_ a) = toBS a

instance ToBS DeliveryStatus_ where
  toBS (DeliveryStatus_ a) = toBS a

instance ToBS DataSource_ where
  toBS (DataSource_ a) = toBS a

instance ToBS ApproximateCount_ where
  toBS (ApproximateCount_ a) = toBS a

origin_audience_id r = r `Rec.get` OriginAudienceId
lookalike_audience_ids r = r `Rec.get` LookalikeAudienceIds
external_event_source r = r `Rec.get` ExternalEventSource
delivery_status r = r `Rec.get` DeliveryStatus
data_source r = r `Rec.get` DataSource
approximate_count r = r `Rec.get` ApproximateCount
-- Entity:CustomAudience, mode:Reading
class IsCustomAudienceGetField r
instance (IsCustomAudienceGetField h, IsCustomAudienceGetField t) => IsCustomAudienceGetField (h :*: t)
instance IsCustomAudienceGetField Nil
instance IsCustomAudienceGetField Subtype
instance IsCustomAudienceGetField Name
instance IsCustomAudienceGetField LookalikeAudienceIds
instance IsCustomAudienceGetField ExternalEventSource
instance IsCustomAudienceGetField Description
instance IsCustomAudienceGetField DeliveryStatus
instance IsCustomAudienceGetField DataSource
instance IsCustomAudienceGetField ApproximateCount
instance IsCustomAudienceGetField AccountId
instance IsCustomAudienceGetField Id

type CustomAudienceGet fl r = (A.FromJSON r, IsCustomAudienceGetField r, FieldListToRec fl r)
type CustomAudienceGetRet r = r -- Default fields
getCustomAudience :: (R.MonadResource m, MonadBaseControl IO m, CustomAudienceGet fl r) =>
  Id_    -- ^ Ad Account Id
  -> fl     -- ^ Arguments to be passed to Facebook.
  ->  UserAccessToken -- ^ Optional user access token.
  -> FacebookT anyAuth m (Pager (CustomAudienceGetRet r))
getCustomAudience (Id_ id) fl mtoken = getObject ("/v2.7/" <> id <> "/customaudiences") [("fields", textListToBS $ fieldNameList $ fl)] $ Just mtoken


-- Entity:CustomAudience, mode:Creating
class IsCustomAudienceSetField r
instance (IsCustomAudienceSetField h, IsCustomAudienceSetField t) => IsCustomAudienceSetField (h :*: t)
instance IsCustomAudienceSetField Nil
instance IsCustomAudienceSetField OriginAudienceId
instance IsCustomAudienceSetField Subtype
instance IsCustomAudienceSetField Name
instance IsCustomAudienceSetField Description
data CreateCustomAudienceId = CreateCustomAudienceId {
  customAudienceId :: Text
  } deriving Show
instance FromJSON CreateCustomAudienceId where
    parseJSON (Object v) =
       CreateCustomAudienceId <$> v .: "id"

type CustomAudienceSet r = (A.FromJSON r, IsCustomAudienceSetField r, ToForm r)
setCustomAudience :: (R.MonadResource m, MonadBaseControl IO m, CustomAudienceSet r) =>
  Id_    -- ^ Ad Account Id
  -> r     -- ^ Arguments to be passed to Facebook.
  ->  UserAccessToken -- ^ Optional user access token.
  -> FacebookT Auth m (Either FacebookException CreateCustomAudienceId)
setCustomAudience (Id_ id) r mtoken = postForm ("/v2.7/" <> id <> "/customaudiences") (toForm r) mtoken

