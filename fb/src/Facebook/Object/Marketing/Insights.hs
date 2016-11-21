{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings, ConstraintKinds, CPP #-}

module Facebook.Object.Marketing.Insights where

import Control.Applicative
import Control.Monad (mzero)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Time
import Data.Time.Format
import Text.Read

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

import Facebook.Types
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

data TotalActions = TotalActions
newtype TotalActions_ = TotalActions_ Integer deriving (Show, Generic)
instance Field TotalActions where
  type FieldValue TotalActions = TotalActions_
  fieldName _ = "total_actions"
  fieldLabel = TotalActions
  defaultValue _ = Just (TotalActions_ 0)
instance A.FromJSON TotalActions_ where
  parseJSON (A.String num) = do
    let int = read $ T.unpack num
    pure $ TotalActions_ int
unTotalActions_ :: TotalActions_ -> Integer
unTotalActions_ (TotalActions_ x) = x

data Clicks = Clicks
newtype Clicks_ = Clicks_ Integer deriving (Show, Generic)
instance Field Clicks where
  type FieldValue Clicks = Clicks_
  fieldName _ = "clicks"
  fieldLabel = Clicks
instance A.FromJSON Clicks_ where
  parseJSON (A.String num) = do
    let int = read $ T.unpack num
    pure $ Clicks_ int
unClicks_ :: Clicks_ -> Integer
unClicks_ (Clicks_ x) = x

data CallToActionClicks = CallToActionClicks
instance Field CallToActionClicks where
  type FieldValue CallToActionClicks = Integer
  fieldName _ = "call_to_action_clicks"
  fieldLabel = CallToActionClicks
  defaultValue _ = Just 0


data Spend = Spend
newtype Spend_ = Spend_ Float deriving (Show, Generic)
instance Field Spend where
  type FieldValue Spend = Spend_
  fieldName _ = "spend"
  fieldLabel = Spend
instance A.FromJSON Spend_
unSpend_ :: Spend_ -> Float
unSpend_ (Spend_ x) = x

class IsInsightField r
instance (IsInsightField h, IsInsightField t) => IsInsightField (h :*: t)
instance IsInsightField Age
instance IsInsightField Gender
instance IsInsightField Country
instance IsInsightField Nil

type InsightsConst fl r = (A.FromJSON r, IsInsightField r, FieldListToRec fl r)
type InsightsRet r = DateStart :*: DateStop :*: Impressions :*: TotalActions :*: Clicks :*: Spend :*: r
-- see restrictions on breakdown factor combination: https://developers.facebook.com/docs/marketing-api/insights/breakdowns/v2.7
getInsightsBreak :: (R.MonadResource m, MonadBaseControl IO m, InsightsConst fl r)  =>
                     Id_
                  -> fl
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager (InsightsRet r))
getInsightsBreak (Id_ id_) fl tok =
  getObject ("/v2.7/" <> id_ <> "/insights")
    [("fields", "impressions,total_actions,clicks,spend"), ("date_preset", "lifetime"), ("breakdowns", textListToBS $ fieldNameList fl)] (Just tok)


data Action a = Action { action_action_type :: Text,
                         action_value :: a }
                deriving (Eq, Show, Typeable, Generic)

instance A.FromJSON a => A.FromJSON (Action a) where
  parseJSON = parseJSONWithPrefix "action_"

instance A.ToJSON a => A.ToJSON (Action a) where
  toJSON = toJSONWithPrefix "action_"


data Insights = Insights
  { ins_actions :: [Action Int],
    ins_unique_actions :: [Action Double],
    ins_cost_per_action_type :: [Action Double],
    ins_cost_per_unique_action_type :: [Action Double],
    ins_call_to_action_clicks :: SInt,
    ins_unique_clicks :: SInt,
    ins_cpm :: Double,
    ins_ctr :: Double,
    ins_cpp :: Double,
    ins_unique_ctr :: Double,
    ins_unique_impressions:: SInt,
    ins_reach :: SInt,
    ins_spend:: Double } deriving (Eq, Show, Typeable, Generic)

newtype SInt = SInt { unSInt :: Int } deriving (Eq, Show, Typeable, Generic)

instance A.FromJSON SInt where
  parseJSON (A.String v) = case readMaybe $ T.unpack v of
                           Just x -> return $ SInt x
                           Nothing -> fail $ "fromJSON SInt fail String "++ T.unpack v
  parseJSON (A.Number x) = return $ SInt $ round x
  parseJSON v = fail $ "fromJSON SInt fail "++ show v

instance A.ToJSON SInt where
  toJSON = A.toJSON . unSInt


instance A.FromJSON Insights where
  parseJSON = parseJSONWithPrefix "ins_"

instance A.ToJSON Insights where
  toJSON = toJSONWithPrefix "ins_"

getInsights :: (R.MonadResource m, MonadBaseControl IO m)  =>
                     Id
                  -> [Argument]
                  -> UserAccessToken
                  -> FacebookT Auth m (Pager (WithJSON Insights))
getInsights (Id id_) query tok = do -- NOTE: in v2.5 of the API, this fields were returned by default
  -- quickly reproduce v2.5 behaviour. I'm sure there are better ways
  let q = ("date_preset", "lifetime") :
          ("fields", "actions, unique_actions, cost_per_action_type, cost_per_unique_action_type, call_to_action_clicks, unique_clicks, cpm, ctr, cpp, unique_ctr, unique_impressions, reach, spend") : query
  getObject ("/v2.7/" <> id_ <> "/insights") q (Just tok)
