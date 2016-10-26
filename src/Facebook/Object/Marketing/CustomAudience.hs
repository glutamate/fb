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
instance A.FromJSON DeliveryStatus_
instance A.ToJSON DeliveryStatus_
instance A.FromJSON DataSource_
instance A.ToJSON DataSource_
instance A.FromJSON ApproximateCount_
instance A.ToJSON ApproximateCount_

instance ToBS DeliveryStatus_ where
  toBS (DeliveryStatus_ a) = toBS a

instance ToBS DataSource_ where
  toBS (DataSource_ a) = toBS a

instance ToBS ApproximateCount_ where
  toBS (ApproximateCount_ a) = toBS a

delivery_status r = r `Rec.get` DeliveryStatus
data_source r = r `Rec.get` DataSource
approximate_count r = r `Rec.get` ApproximateCount
-- Entity:CustomAudience, mode:Reading
class IsCustomAudienceGetField r
instance (IsCustomAudienceGetField h, IsCustomAudienceGetField t) => IsCustomAudienceGetField (h :*: t)
instance IsCustomAudienceGetField Nil
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

