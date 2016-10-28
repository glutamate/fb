import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Data.Typeable (Typeable)
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Encoding as TE
import Facebook.Object.Marketing.Utility hiding (toBS)
import Facebook.Object.Marketing.TargetingSpecs
import Text.Read (readMaybe)
newtype Money = Money {getMoneyInPennies :: Int} deriving (Eq, Ord, Show, Read, Typeable, Generic, Num)

instance FromJSON Money where
  parseJSON (String val) =
    case readMaybe $ unpack val of
      Just x -> return $ Money x
      Nothing -> fail $ "Money parser string: "++show (unpack val)
    where
      textToBSL :: Text -> BSL.ByteString
      textToBSL = BSL.fromStrict . TE.encodeUtf8
  parseJSON n@(Number _) = do
    x <- parseJSON n
    return $ Money x
  parseJSON v = fail $ "Money parser value: "++show v

instance ToJSON Money where
  toJSON (Money cents) = String $ pack $ show cents

data FbNumeric = FbStringNumeric Text
    | FbIntegerNumeric Int
  deriving (Show, Read, Generic)
instance FromJSON FbNumeric
instance ToJSON FbNumeric

data FBMObjectCreated = FBMObjectCreated
  { oc_id :: Text
  , oc_success :: Bool
  } deriving (Show, Generic)
instance FromJSON FBMObjectCreated where
  parseJSON = parseJSONWithPrefix "oc_"

class ToFbText a where
  toFbText :: a -> Text
data ConfigureStatusADT = ACTIVE_ | PAUSED_ | DELETED_ | ARCHIVED_ deriving Generic
instance Show ConfigureStatusADT where
   show ACTIVE_ = "ACTIVE"
   show PAUSED_ = "PAUSED"
   show DELETED_ = "DELETED"
   show ARCHIVED_ = "ARCHIVED"
instance ToJSON ConfigureStatusADT where
  toJSON ACTIVE_ = String "ACTIVE"
  toJSON PAUSED_ = String "PAUSED"
  toJSON DELETED_ = String "DELETED"
  toJSON ARCHIVED_ = String "ARCHIVED"
instance FromJSON ConfigureStatusADT where
  parseJSON (String "ACTIVE") = pure ACTIVE_
  parseJSON (String "PAUSED") = pure PAUSED_
  parseJSON (String "DELETED") = pure DELETED_
  parseJSON (String "ARCHIVED") = pure ARCHIVED_
instance ToBS ConfigureStatusADT
data EffectiveStatusADT = ACTIVE | PAUSED | DELETED | ARCHIVED | PENDING_REVIEW | DISAPPROVED | PREAPPROVED | PENDING_BILLING_INFO | CAMPAIGN_PAUSED | ADSET_PAUSED deriving (Show, Generic)
instance FromJSON EffectiveStatusADT
instance ToJSON EffectiveStatusADT
instance ToBS EffectiveStatusADT

data ExecOption = VALIDATE_ONLY | INCLUDE_WARNINGS deriving (Show, Generic)
instance ToJSON ExecOption
instance FromJSON ExecOption
instance ToBS ExecOption
data OptGoal = NONE | APP_INSTALLS | CLICKS | ENGAGED_USERS | EXTERNAL | EVENT_RESPONSES | IMPRESSIONS | LEAD_GENERATION | LINK_CLICKS | OFFER_CLAIMS | OFFSITE_CONVERSIONS | PAGE_ENGAGEMENT | PAGE_LIKES | POST_ENGAGEMENT | REACH | SOCIAL_IMPRESSIONS | VIDEO_VIEWS deriving (Show, Generic)
instance FromJSON OptGoal
instance ToJSON OptGoal
instance ToBS OptGoal

data BidTypeADT = CPC | CPM | MULTI_PREMIUM | ABSOLUTE_OCPM | CPA deriving (Show, Generic)
instance FromJSON BidTypeADT
instance ToJSON BidTypeADT
instance ToBS BidTypeADT
data CallToActionTypeADT = OPEN_LINK | LIKE_PAGE | SHOP_NOW | PLAY_GAME | INSTALL_APP | USE_APP |INSTALL_MOBILE_APP | USE_MOBILE_APP | BOOK_TRAVEL | LISTEN_MUSIC | WATCH_VIDEO | LEARN_MORE |SIGN_UP | DOWNLOAD | WATCH_MORE | NO_BUTTON | CALL_NOW | BUY_NOW | GET_OFFER | GET_DIRECTIONS |MESSAGE_PAGE | SUBSCRIBE | DONATE_NOW | GET_QUOTE | CONTACT_US | RECORD_NOW | OPEN_MOVIES deriving (Show, Generic)
instance FromJSON CallToActionTypeADT
instance ToJSON CallToActionTypeADT
instance ToBS CallToActionTypeADT
data RunStatusADT = RS_ACTIVE | RS_DELETED
instance Show RunStatusADT where
  show RS_ACTIVE = "ACTIVE"
  show RS_DELETED = "DELETED"
instance FromJSON RunStatusADT where
  parseJSON (String "ACTIVE") = pure RS_ACTIVE
  parseJSON (String "DELETED") = pure RS_DELETED
instance ToBS RunStatusADT
data ObjectiveADT = OBJ_BRAND_AWARENESS | OBJ_CANVAS_APP_ENGAGEMENT | OBJ_CANVAS_APP_INSTALLS | OBJ_CONVERSIONS | OBJ_EVENT_RESPONSES | OBJ_EXTERNAL | OBJ_LEAD_GENERATION | OBJ_LINK_CLICKS | OBJ_LOCAL_AWARENESS | OBJ_MOBILE_APP_ENGAGEMENT | OBJ_MOBILE_APP_INSTALLS | OBJ_OFFER_CLAIMS | OBJ_PAGE_LIKES | OBJ_POST_ENGAGEMENT | OBJ_PRODUCT_CATALOG_SALES | OBJ_VIDEO_VIEWS | OBJ_NONE
instance Show ObjectiveADT where
  show OBJ_BRAND_AWARENESS = "BRAND_AWARENESS"
  show OBJ_CANVAS_APP_ENGAGEMENT = "CANVAS_APP_ENGAGEMENT"
  show OBJ_CANVAS_APP_INSTALLS = "CANVAS_APP_INSTALLS"
  show OBJ_CONVERSIONS = "CONVERSIONS"
  show OBJ_EVENT_RESPONSES = "EVENT_RESPONSES"
  show OBJ_EXTERNAL = "EXTERNAL"
  show OBJ_LEAD_GENERATION = "LEAD_GENERATION"
  show OBJ_LINK_CLICKS = "LINK_CLICKS"
  show OBJ_LOCAL_AWARENESS = "LOCAL_AWARENESS"
  show OBJ_MOBILE_APP_ENGAGEMENT = "MOBILE_APP_ENGAGEMENT"
  show OBJ_MOBILE_APP_INSTALLS = "MOBILE_APP_INSTALLS"
  show OBJ_OFFER_CLAIMS = "OFFER_CLAIMS"
  show OBJ_PAGE_LIKES = "PAGE_LIKES"
  show OBJ_POST_ENGAGEMENT = "POST_ENGAGEMENT"
  show OBJ_PRODUCT_CATALOG_SALES = "PRODUCT_CATALOG_SALES"
  show OBJ_NONE = "NONE"
  show OBJ_VIDEO_VIEWS = "VIDEO_VIEWS"
instance ToBS ObjectiveADT
instance ToJSON ObjectiveADT where
  toJSON = toJSON . show
instance FromJSON ObjectiveADT where
  parseJSON (String "BRAND_AWARENESS") = pure OBJ_BRAND_AWARENESS
  parseJSON (String "CANVAS_APP_ENGAGEMENT") = pure OBJ_CANVAS_APP_ENGAGEMENT
  parseJSON (String "CANVAS_APP_INSTALLS") = pure OBJ_CANVAS_APP_INSTALLS
  parseJSON (String "CONVERSIONS") = pure OBJ_CONVERSIONS
  parseJSON (String "EVENT_RESPONSES") = pure OBJ_EVENT_RESPONSES
  parseJSON (String "EXTERNAL") = pure OBJ_EXTERNAL
  parseJSON (String "LEAD_GENERATION") = pure OBJ_LEAD_GENERATION
  parseJSON (String "LINK_CLICKS") = pure OBJ_LINK_CLICKS
  parseJSON (String "LOCAL_AWARENESS") = pure OBJ_LOCAL_AWARENESS
  parseJSON (String "MOBILE_APP_ENGAGEMENT") = pure OBJ_MOBILE_APP_ENGAGEMENT
  parseJSON (String "MOBILE_APP_INSTALLS") = pure OBJ_MOBILE_APP_INSTALLS
  parseJSON (String "OFFER_CLAIMS") = pure OBJ_OFFER_CLAIMS
  parseJSON (String "PAGE_LIKES") = pure OBJ_PAGE_LIKES
  parseJSON (String "POST_ENGAGEMENT") = pure OBJ_POST_ENGAGEMENT
  parseJSON (String "PRODUCT_CATALOG_SALES") = pure OBJ_PRODUCT_CATALOG_SALES
  parseJSON (String "NONE") = pure OBJ_NONE
  parseJSON (String "VIDEO_VIEWS") = pure OBJ_VIDEO_VIEWS
data BuyingTypeADT = AUCTION | RESERVED deriving (Show, Generic)
instance FromJSON BuyingTypeADT
instance ToJSON BuyingTypeADT
instance ToBS BuyingTypeADT
data DeleteStrategyADT = DELETE_ANY | DELETE_OLDEST | DELETE_ARCHIVED_BEFORE deriving (Show, Generic)
instance FromJSON DeleteStrategyADT
instance ToJSON DeleteStrategyADT
instance ToBS DeleteStrategyADT
data BillingEventADT = APP_INSTALLS_ | CLICKS_ | IMPRESSIONS_ | LINK_CLICKS_ | OFFER_CLAIMS_ | PAGE_LIKES_ | POST_ENGAGEMENT_ | VIDEO_VIEWS_
instance Show BillingEventADT where
   show APP_INSTALLS_ = "APP_INSTALLS"
   show CLICKS_ = "CLICKS"
   show IMPRESSIONS_ = "IMPRESSIONS"
   show LINK_CLICKS_ = "LINK_CLICKS"
   show OFFER_CLAIMS_ = "OFFER_CLAIMS"
   show PAGE_LIKES_ = "PAGE_LIKES"
   show POST_ENGAGEMENT_ = "POST_ENGAGEMENT"
   show VIDEO_VIEWS_ = "VIDEO_VIEWS"
instance ToBS BillingEventADT
instance ToJSON BillingEventADT where
  toJSON = toJSON . show
instance FromJSON BillingEventADT where
  parseJSON (String "APP_INSTALLS") = pure APP_INSTALLS_
  parseJSON (String "IMPRESSIONS") = pure IMPRESSIONS_
  parseJSON (String "CLICKS") = pure CLICKS_
  parseJSON (String "LINK_CLICKS") = pure LINK_CLICKS_
  parseJSON (String "OFFER_CLAIMS") = pure OFFER_CLAIMS_
  parseJSON (String "PAGE_LIKES") = pure PAGE_LIKES_
  parseJSON (String "POST_ENGAGEMENT") = pure POST_ENGAGEMENT_
  parseJSON (String "VIDEO_VIEWS") = pure VIDEO_VIEWS_
data ObjectStorySpecADT = ObjectStorySpecADT {
    linkData  :: AdCreativeLinkData,
    storyPageId  :: FBPageId,
    igId  :: Maybe IgId}
  | ObjectStorySpecVideoLink {
    videoData  :: AdCreativeVideoData,
    vStoryPageId  :: FBPageId,
    igId  :: Maybe IgId
  } deriving (Show, Generic)
newtype FBPageId = FBPageId Text deriving (Show, Generic)
instance FromJSON FBPageId
newtype IgId = IgId Text deriving (Show, Generic)
instance FromJSON IgId
instance ToJSON ObjectStorySpecADT where
  toJSON (ObjectStorySpecADT ld (FBPageId pi) Nothing) =
    object [ "link_data" .= ld,
             "page_id" .= pi] 
  toJSON (ObjectStorySpecADT ld (FBPageId pi) (Just (IgId ig))) =
    object [ "link_data" .= ld,
             "page_id" .= pi, 
             "instagram_actor_id" .= ig] 
  toJSON (ObjectStorySpecVideoLink vidData (FBPageId pi) (Just (IgId ig))) =
    object [ "video_data" .= vidData,
             "page_id" .= pi,
             "instagram_actor_id" .= ig]
instance FromJSON ObjectStorySpecADT where
  parseJSON (Object v) = do
   typ <- v .:? "link_data" :: Parser (Maybe AdCreativeLinkData)
   case typ of
     Nothing -> parseObjectStorySpecADT v
     Just _  -> parseObjectStorySpecVideoLink v
parseObjectStorySpecADT v =   ObjectStorySpecADT <$> v .: "link_data"
                      <*> v .: "page_id"
                      <*> v .:? "instagram_actor_id"
parseObjectStorySpecVideoLink v =   ObjectStorySpecVideoLink <$> v .: "video_data"
                      <*> v .: "page_id"
                      <*> v .:? "instagram_actor_id"
instance ToBS ObjectStorySpecADT where
  toBS a = toBS $ toJSON a
data AdCreativeLinkData = AdCreativeLinkData {
    caption  :: Text,
    imageHash ::  Hash_,
    link, message :: Text,
    description  :: Maybe Text,
    call_to_action :: Maybe CallToActionADT}
  | CreativeCarouselData {
    caption_carousel, message_carousel :: Text,
    child_attachments ::  CarouselChildren,
    link :: Text }
  deriving (Show, Generic)
instance ToJSON AdCreativeLinkData where
  toJSON (AdCreativeLinkData c i l m (Just d) (Just cta)) =
    object [ "caption" .= c,
             "image_hash" .= i,
             "link" .= l,
             "message" .= m]
             --"description" .= d,
             --"call_to_action" .= cta]
  toJSON (CreativeCarouselData c m cs l) =
    object [ "caption" .= c,
             "message" .= m,
             "child_attachments" .= toJSON cs,
             "link" .= l]
instance FromJSON AdCreativeLinkData where
  parseJSON (Object v) = do
   typ <- v .:? "child_attachments" :: Parser (Maybe CarouselChildren)
   case typ of
     Nothing -> parseAdCreativeLinkData v
     Just _  -> parseCreativeCarouselData v

parseAdCreativeLinkData v =
   AdCreativeLinkData <$> v .: "caption"
                      <*> v .: "image_hash"
                      <*> v .: "link"
                      <*> v .: "message"
                      <*> v .:? "description"
                      <*> v .:? "call_to_action"

parseCreativeCarouselData v =
   CreativeCarouselData <$> v .: "caption"
                        <*> v .: "message"
                        <*> v .: "child_attachments"
                        <*> v .: "link"
instance ToBS AdCreativeLinkData where
  toBS a = toBS $ toJSON a
data AdCreativeVideoData = AdCreativeVideoData {
    vid_call_to_action :: CallToActionADT,
    desc ::  Text,
    thumb_url ::  Text,
    video_id :: Integer} -- FIXME: newtype wrapper
  deriving (Show, Generic)
instance ToJSON AdCreativeVideoData where
  toJSON (AdCreativeVideoData cta d thumbUrl vId) =
    object [ "call_to_action" .= cta,
             "description" .= d,
             "image_url" .= thumbUrl,
             "video_id" .= vId]
instance FromJSON AdCreativeVideoData where
  parseJSON (Object v) =
    AdCreativeVideoData <$> v .: "call_to_action"
                        <*> v .: "description"
                        <*> v .: "image_url"
                        <*> v .: "video_id"
type CarouselChildren = [CarouselChild]
data CarouselChild = CarouselChild {
    name_car_child :: Text,
    imageHash_car_child ::  Hash_,
    link_car_child :: Text,
  description_car_child  :: Maybe Text}
  deriving (Show, Generic)
instance ToJSON CarouselChild where
  toJSON (CarouselChild n i l (Just d)) =
    object [ "name" .= n,
             "image_hash" .= i,
             "link" .= l,
             "description" .= d]
instance FromJSON CarouselChild where
  parseJSON (Object v) =
   CarouselChild <$> v .: "name"
                 <*> v .: "image_hash"
                 <*> v .: "link"
                 <*> v .:? "description"
instance ToBS CarouselChild where
  toBS a = toBS $ toJSON a
data AdCreativeADT = AdCreativeADT {
  creative_id  :: Text
  } deriving (Show, Generic)
instance FromJSON AdCreativeADT
instance ToJSON AdCreativeADT
instance ToBS AdCreativeADT where
  toBS = toBS . toJSON
data CallToActionValue = CallToActionValue {
  ctav_link, ctav_link_caption :: Text
  } deriving (Show, Generic)
instance ToJSON CallToActionValue where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length ("ctav_" :: String)}
instance FromJSON CallToActionValue where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length ("ctav_" :: String)}
data CallToActionADT = CallToActionADT {
    cta_type :: CallToActionTypeADT,
    cta_value :: CallToActionValue
  } deriving (Show, Generic)
instance ToJSON CallToActionADT where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop $ length ("cta_" :: String)}
instance FromJSON CallToActionADT where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop $ length ("cta_" :: String)}

data Success = Success {
  success :: Bool
  } deriving (Show, Generic)
instance FromJSON Success

data SuccessId = SuccessId {
     id_ :: Text
  } deriving (Show, Generic)
instance FromJSON SuccessId where 
  parseJSON (Object v) =
      SuccessId <$> v .: "id"


-- BENC NOTIMPL !!!
data CustomAudienceDataSource = CustomAudienceDataSource {
    type_ :: Text, -- BENC TODO: make an ENUM
    sub_type :: Text, -- BENC TODO: make an ENUM
    creation_params :: Text -- this really is a string not an enum
  }
  deriving Show

instance ToJSON CustomAudienceDataSource
  where toJSON = error "BENC NOTIMPL custom audience data source toJSON"

instance FromJSON CustomAudienceDataSource where
  parseJSON (Object v) =
    -- BENC TODO: make a genericParseJSON
    CustomAudienceDataSource <$> v .: "type"
                             <*> v .: "sub_type"
                             <*> v .: "creation_params"
  parseJSON _ = error $ "could not parse non-Object as a CustomAudienceDataSource"

instance ToBS CustomAudienceDataSource
  where toBS = error "BENC NOTIMPL custom audience data source toBS"


-- BENC: NOTIMPL
data CustomAudienceStatus = CustomAudienceStatus {
    cas_code :: Int,
    cas_description :: Text
} deriving (Show, Generic)

instance ToJSON CustomAudienceStatus
  where toJSON = error "BENC NOTIMPL custom audience status"

instance FromJSON CustomAudienceStatus where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop $ length ("cas_" :: String)}

instance ToBS CustomAudienceStatus
  where toBS = error "BENC NOTIMPL custom audience status"

-- BENC: NOTIMPL
-- WARNING: deserialisation of this is not testing in MarketingTests
-- (or anywhere) because there is no accessor that will return a single
-- CustomAudience, and external_event_source :: AdsPixel cannot be
-- requested for all audiences when some audiences do not have that field.
data AdsPixel = AdsPixel {
  stuff :: Text
   -- TODO: what fields are we going to get?
} deriving Show

instance ToJSON AdsPixel where
  toJSON = error "BENC NOTIMPL tojson adspixel"

instance FromJSON AdsPixel where
  parseJSON x = pure $ AdsPixel (pack $ show x)

instance ToBS AdsPixel where
  toBS = error "BENC NOTIMPL tobs adspixel"

data CustomAudienceSubtypeADT = CUSTOM | WEBSITE | APP | OFFLINE_CONVERSION | CLAIM | PARTNER | MANAGED | VIDEO | LOOKALIKE | ENGAGEMENT | DATA_SET | BAG_OF_ACCOUNTS | STUDY_RULE_AUDIENCE
  deriving Show

instance ToJSON CustomAudienceSubtypeADT where
  toJSON = toJSON . show

instance FromJSON CustomAudienceSubtypeADT where
  parseJSON = error "BENC NOTIMPL fromjson customaudiencesubtypeadt"

instance ToBS CustomAudienceSubtypeADT where
  toBS = toBS . toJSON

