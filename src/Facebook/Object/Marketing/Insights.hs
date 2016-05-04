{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings, ConstraintKinds, CPP #-}

module Facebook.Object.Marketing.Insights where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time
import Data.Time.Format
#if MIN_VERSION_time(1,5,0)
import System.Locale hiding (defaultTimeLocale, rfc822DateFormat)
import Data.Time.Clock
#else
import System.Locale
import Data.Time.Clock hiding (defaultTimeLocale, rfc822DateFormat)
#endif
import Data.Typeable (Typeable)

import qualified Control.Monad.Trans.Resource as R
import qualified Data.Aeson as A
import qualified Data.Text as T

import Facebook.Types hiding (Id)
import Facebook.Monad
import Facebook.Graph
import Facebook.Pager
import Facebook.Records

import Facebook.Object.Marketing.Types hiding (Id)
import Facebook.Object.Marketing.Utility

data Age = Age
newtype Age_ = Age_ T.Text deriving (Show, Generic)
instance Field Age where
  type FieldValue Age = Age_
  fieldName _ = "age"
  fieldLabel = Age
instance A.FromJSON Age_
unAge_ :: Age_ -> T.Text
unAge_ (Age_ x) = x

data GenderAdt = Male | Female | Unknown deriving Show
data Gender = Gender
newtype Gender_ = Gender_ GenderAdt deriving (Show, Generic)
instance Field Gender where
  type FieldValue Gender = Gender_
  fieldName _ = "gender"
  fieldLabel = Gender
instance A.FromJSON Gender_ where
  parseJSON (A.String "female") = pure $ Gender_ Female
  parseJSON (A.String "male") = pure $ Gender_ Male
  parseJSON (A.String "unknown") = pure $ Gender_ Unknown
  parseJSON x = error $ show x
unGender_ :: Gender_ -> GenderAdt
unGender_ (Gender_ x) = x

data Country = Country
newtype Country_ = Country_ T.Text deriving (Show, Generic)
instance Field Country where
  type FieldValue Country = Country_
  fieldName _ = "country"
  fieldLabel = Country
instance A.FromJSON Country_
unCountry_ :: Country_ -> T.Text
unCountry_ (Country_ x) = x

data DateStart = DateStart
newtype DateStart_ = DateStart_ Day deriving (Show, Generic)
instance Field DateStart where
  type FieldValue DateStart = DateStart_
  fieldName _ = "date_start"
  fieldLabel = DateStart
instance A.FromJSON DateStart_ where
  parseJSON (A.String time) = parseDay time DateStart_
unDateStart_ :: DateStart_ -> Day
unDateStart_ (DateStart_ x) = x

parseDay time constr = do
    day <- parseTimeM True defaultTimeLocale "%F" $ T.unpack time
    pure $ constr day

data DateStop = DateStop
newtype DateStop_ = DateStop_ Day deriving (Show, Generic)
instance Field DateStop where
  type FieldValue DateStop = DateStop_
  fieldName _ = "date_stop"
  fieldLabel = DateStop
instance A.FromJSON DateStop_ where
  parseJSON (A.String time) = parseDay time DateStop_
unDateStop_ :: DateStop_ -> Day
unDateStop_ (DateStop_ x) = x

data Impressions = Impressions
newtype Impressions_ = Impressions_ Integer deriving (Show, Generic)
instance Field Impressions where
  type FieldValue Impressions = Impressions_
  fieldName _ = "impressions"
  fieldLabel = Impressions
instance A.FromJSON Impressions_ where
  parseJSON (A.String num) = do
    let int = read $ T.unpack num
    pure $ Impressions_ int
unImpressions_ :: Impressions_ -> Integer
unImpressions_ (Impressions_ x) = x

class IsInsightField r
instance (IsInsightField h, IsInsightField t) => IsInsightField (h :*: t)
instance IsInsightField Age
instance IsInsightField Gender
instance IsInsightField Country
instance IsInsightField Nil

type Insights fl r = (A.FromJSON r, IsInsightField r, FieldListToRec fl r)
type InsightsRet r = DateStart :*: DateStop :*: Impressions :*: r
-- see restrictions on breakdown factor combination: https://developers.facebook.com/docs/marketing-api/insights/breakdowns/v2.5
getInsights :: (R.MonadResource m, MonadBaseControl IO m, Insights fl r)  =>
                     Id_
                  -> fl
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager (InsightsRet r))
getInsights (Id_ id_) fl tok =
  getObject ("/v2.5/" <> id_ <> "/insights")
    [("fields", "impressions"), ("breakdowns", textListToBS $ fieldNameList fl)] (Just tok)
