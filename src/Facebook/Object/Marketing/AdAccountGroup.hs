{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.AdAccountGroup where

import Control.Monad (mzero)
import Data.Text (Text)
import GHC.Generics

import qualified Data.Aeson as A
import Data.Aeson.Types

import Facebook.Types

import Facebook.Object.Marketing.Utility


-- | A Facebook user profile (see
-- <https://developers.facebook.com/docs/marketing-api/adaccountgroup/>).
--
-- /NOTE:/ We still don't support all fields supported by
-- Facebook. Please fill an issue if you need access to any other
-- fields.

data AdAccountGroupStatus = Active
                          | Deleted
                          deriving (Show, Eq, Generic)
instance A.FromJSON AdAccountGroupStatus where
  parseJSON val = do
    intCode <- A.parseJSON val :: Parser Int
    case intCode of
      1 -> return Active
      2 -> return Deleted
      _ -> mzero

instance A.ToJSON AdAccountGroupStatus where
  toJSON Active = Number 1
  toJSON Deleted = Number 2

data AdAccountGroup =
  AdAccountGroup
    { accg_id :: Id
    , accg_name :: Text
    , accg_status :: AdAccountGroupStatus
    , accg_users :: [Text]
    , accg_accounts :: [Text]
    } deriving (Eq, Show, Generic)

instance A.FromJSON AdAccountGroup where
  parseJSON = parseJSONWithPrefix "accg_"

instance A.ToJSON AdAccountGroup where
  toJSON = toJSONWithPrefix "accg_"
