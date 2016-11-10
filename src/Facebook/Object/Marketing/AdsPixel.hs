{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Facebook.Object.Marketing.AdsPixel where

import Facebook.Records hiding (get)
import qualified Facebook.Records as Rec
import Facebook.Types hiding (Id)
import Facebook.Pager
import Facebook.Monad
import Facebook.Graph
import Facebook.Base (FacebookException(..))
import qualified Data.Aeson as A
import Data.Aeson hiding (Value)
import Control.Applicative ( (<|>) )
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

data Code = Code
newtype Code_ = Code_ Text deriving (Show, Generic)
instance Field Code where
  type FieldValue Code = Code_
  fieldName _ = "code"
  fieldLabel = Code
unCode_ :: Code_ -> Text
unCode_ (Code_ x) = x
instance A.FromJSON Code_
instance A.ToJSON Code_

instance ToBS Code_ where
  toBS (Code_ a) = toBS a

code :: Has Code r => r -> Code_
code r = r `Rec.get` Code
-- Entity:AdsPixel, mode:Reading
class IsAdsPixelGetField r
instance (IsAdsPixelGetField h, IsAdsPixelGetField t) => IsAdsPixelGetField (h :*: t)
instance IsAdsPixelGetField Nil
instance IsAdsPixelGetField Name
instance IsAdsPixelGetField Code
instance IsAdsPixelGetField Id

type AdsPixelGet fl r = (A.FromJSON r, IsAdsPixelGetField r, FieldListToRec fl r)
type AdsPixelGetRet r = r -- Default fields
getAdsPixel :: (R.MonadResource m, MonadBaseControl IO m, AdsPixelGet fl r) =>
  Id_    -- ^ Ad Account Id
  -> fl     -- ^ Arguments to be passed to Facebook.
  -> Maybe UserAccessToken -- ^ Optional user access token.
  -> FacebookT anyAuth m (Pager (AdsPixelGetRet r))
getAdsPixel (Id_ id) fl mtoken = getObject ("/v2.7/" <> id <> "/adspixels") [("fields", textListToBS $ fieldNameList $ fl)] mtoken

