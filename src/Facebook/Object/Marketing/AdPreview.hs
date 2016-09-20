{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Facebook.Object.Marketing.AdPreview where

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
import Facebook.Object.Marketing.TargetingSpecs.Placement (PlacementOption (..))

getAdPreview :: (R.MonadResource m, MonadBaseControl IO m) =>
	Id_    -- ^ Ad Account Id
        -> ObjectStorySpecADT
        -> PlacementOption
	-> UserAccessToken -- ^ Optional user access token.
	-> FacebookT anyAuth m (Pager PreviewResponse)
getAdPreview (Id_ id) oss place mtoken =
  let creative = Map.fromList [("object_story_spec"::String, oss)]
      adformat = case place of
                   InstagramStream -> "INSTAGRAM_STANDARD"
                   RightColumn -> "RIGHT_COLUMN_STANDARD"
                   Desktopfeed -> "DESKTOP_FEED_STANDARD"
                   MobileFeed -> "MOBILE_FEED_STANDARD"
                   MobileExternal -> "AUDIENCE_NETWORK_OUTSTREAM_VIDEO"
      arg = [("creative", BSL.toStrict $ encode $ toJSON creative), ("ad_format", adformat)]
  in getObject ("/v2.7/" <> id <> "/generatepreviews") arg $ Just mtoken

data PreviewResponse = PreviewResponse { body :: Text } deriving (Show, Generic)

instance FromJSON PreviewResponse
