{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Facebook.Object.Marketing.AdImage where

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

data Filename = Filename
newtype Filename_ = Filename_ Text deriving (Show, Generic)
instance Field Filename where
	type FieldValue Filename = Filename_
	fieldName _ = "filename"
	fieldLabel = Filename
unFilename_ :: Filename_ -> Text
unFilename_ (Filename_ x) = x

data Creatives = Creatives
newtype Creatives_ = Creatives_ (Vector Text) deriving (Show, Generic)
instance Field Creatives where
	type FieldValue Creatives = Creatives_
	fieldName _ = "creatives"
	fieldLabel = Creatives
unCreatives_ :: Creatives_ -> Vector Text
unCreatives_ (Creatives_ x) = x

data Height = Height
newtype Height_ = Height_ Int deriving (Show, Generic)
instance Field Height where
	type FieldValue Height = Height_
	fieldName _ = "height"
	fieldLabel = Height
unHeight_ :: Height_ -> Int
unHeight_ (Height_ x) = x

data PermalinkUrl = PermalinkUrl
newtype PermalinkUrl_ = PermalinkUrl_ Text deriving (Show, Generic)
instance Field PermalinkUrl where
	type FieldValue PermalinkUrl = PermalinkUrl_
	fieldName _ = "permalink_url"
	fieldLabel = PermalinkUrl
unPermalinkUrl_ :: PermalinkUrl_ -> Text
unPermalinkUrl_ (PermalinkUrl_ x) = x

data Url128 = Url128
newtype Url128_ = Url128_ Text deriving (Show, Generic)
instance Field Url128 where
	type FieldValue Url128 = Url128_
	fieldName _ = "url_128"
	fieldLabel = Url128
unUrl128_ :: Url128_ -> Text
unUrl128_ (Url128_ x) = x

data OriginalHeight = OriginalHeight
newtype OriginalHeight_ = OriginalHeight_ Int deriving (Show, Generic)
instance Field OriginalHeight where
	type FieldValue OriginalHeight = OriginalHeight_
	fieldName _ = "original_height"
	fieldLabel = OriginalHeight
unOriginalHeight_ :: OriginalHeight_ -> Int
unOriginalHeight_ (OriginalHeight_ x) = x

data Url = Url
newtype Url_ = Url_ Text deriving (Show, Generic)
instance Field Url where
	type FieldValue Url = Url_
	fieldName _ = "url"
	fieldLabel = Url
unUrl_ :: Url_ -> Text
unUrl_ (Url_ x) = x

data Status = Status
newtype Status_ = Status_ Bool deriving (Show, Generic)
instance Field Status where
	type FieldValue Status = Status_
	fieldName _ = "status"
	fieldLabel = Status
unStatus_ :: Status_ -> Bool
unStatus_ (Status_ x) = x

data OriginalWidth = OriginalWidth
newtype OriginalWidth_ = OriginalWidth_ Int deriving (Show, Generic)
instance Field OriginalWidth where
	type FieldValue OriginalWidth = OriginalWidth_
	fieldName _ = "original_width"
	fieldLabel = OriginalWidth
unOriginalWidth_ :: OriginalWidth_ -> Int
unOriginalWidth_ (OriginalWidth_ x) = x

data Width = Width
newtype Width_ = Width_ Int deriving (Show, Generic)
instance Field Width where
	type FieldValue Width = Width_
	fieldName _ = "width"
	fieldLabel = Width
unWidth_ :: Width_ -> Int
unWidth_ (Width_ x) = x
instance A.FromJSON Filename_
instance A.ToJSON Filename_
instance A.FromJSON Creatives_
instance A.ToJSON Creatives_
instance A.FromJSON Height_
instance A.ToJSON Height_
instance A.FromJSON PermalinkUrl_
instance A.ToJSON PermalinkUrl_
instance A.FromJSON Url128_
instance A.ToJSON Url128_
instance A.FromJSON OriginalHeight_
instance A.ToJSON OriginalHeight_
instance A.FromJSON Url_
instance A.ToJSON Url_
instance A.FromJSON Status_
instance A.ToJSON Status_
instance A.FromJSON OriginalWidth_
instance A.ToJSON OriginalWidth_
instance A.FromJSON Width_
instance A.ToJSON Width_

instance ToBS Filename_ where
	toBS (Filename_ a) = toBS a

instance ToBS Creatives_ where
	toBS (Creatives_ a) = toBS a

instance ToBS Height_ where
	toBS (Height_ a) = toBS a

instance ToBS PermalinkUrl_ where
	toBS (PermalinkUrl_ a) = toBS a

instance ToBS Url128_ where
	toBS (Url128_ a) = toBS a

instance ToBS OriginalHeight_ where
	toBS (OriginalHeight_ a) = toBS a

instance ToBS Url_ where
	toBS (Url_ a) = toBS a

instance ToBS Status_ where
	toBS (Status_ a) = toBS a

instance ToBS OriginalWidth_ where
	toBS (OriginalWidth_ a) = toBS a

instance ToBS Width_ where
	toBS (Width_ a) = toBS a

filename r = r `Rec.get` Filename
creatives r = r `Rec.get` Creatives
height r = r `Rec.get` Height
permalink_url r = r `Rec.get` PermalinkUrl
url_128 r = r `Rec.get` Url128
original_height r = r `Rec.get` OriginalHeight
url r = r `Rec.get` Url
status r = r `Rec.get` Status
original_width r = r `Rec.get` OriginalWidth
width r = r `Rec.get` Width
-- Entity:AdImage, mode:Reading
class IsAdImageGetField r
instance (IsAdImageGetField h, IsAdImageGetField t) => IsAdImageGetField (h :*: t)
instance IsAdImageGetField Nil
instance IsAdImageGetField AccountId
instance IsAdImageGetField Creatives
instance IsAdImageGetField Hash
instance IsAdImageGetField Height
instance IsAdImageGetField PermalinkUrl
instance IsAdImageGetField CreatedTime
instance IsAdImageGetField Url128
instance IsAdImageGetField UpdatedTime
instance IsAdImageGetField Id
instance IsAdImageGetField OriginalHeight
instance IsAdImageGetField Url
instance IsAdImageGetField Status
instance IsAdImageGetField Name
instance IsAdImageGetField OriginalWidth
instance IsAdImageGetField Width

type AdImageGet fl r = (A.FromJSON r, IsAdImageGetField r, FieldListToRec fl r)
type AdImageGetRet r = Hash :*: r -- Default fields
getAdImage :: (R.MonadResource m, MonadBaseControl IO m, AdImageGet fl r) =>
	Id_    -- ^ Ad Account Id
	-> fl     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (Pager (AdImageGetRet r))
getAdImage (Id_ id) fl mtoken = getObject ("/v2.6/" <> id <> "/adimages") [("fields", textListToBS $ fieldNameList $ Hash ::: fl)] $ Just mtoken


-- Entity:AdImage, mode:Creating
class IsAdImageSetField r
instance (IsAdImageSetField h, IsAdImageSetField t) => IsAdImageSetField (h :*: t)
instance IsAdImageSetField Nil
instance IsAdImageSetField Filename
data SetImgs = SetImgs { -- as seen when using curl
	images  :: Map.Map Text SetImg
	} deriving (Show, Generic)
instance FromJSON SetImgs
data SetImg = SetImg {
	hash, url_ :: Text
	} deriving Show
instance FromJSON SetImg where
	parseJSON (Object v) =
		SetImg <$> v .: "hash"
				<*> v .: "url"

type AdImageSet r = (Has Filename r, A.FromJSON r, IsAdImageSetField r, ToForm r)
setAdImage :: (R.MonadResource m, MonadBaseControl IO m, AdImageSet r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m (Either FacebookException SetImgs)
setAdImage (Id_ id) r mtoken = postForm ("/v2.6/" <> id <> "/adimages") (toForm r) mtoken


-- Entity:AdImage, mode:Deleting
class IsAdImageDelField r
instance (IsAdImageDelField h, IsAdImageDelField t) => IsAdImageDelField (h :*: t)
instance IsAdImageDelField Nil
instance IsAdImageDelField Hash

type AdImageDel r = (Has Hash r, A.FromJSON r, IsAdImageDelField r, ToForm r)
delAdImage :: (R.MonadResource m, MonadBaseControl IO m, AdImageDel r) =>
	Id_    -- ^ Ad Account Id
	-> r     -- ^ Arguments to be passed to Facebook.
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m (Either FacebookException Success)
delAdImage (Id_ id) r mtoken = deleteForm ("/v2.6/" <> id <> "") (toForm r) mtoken

