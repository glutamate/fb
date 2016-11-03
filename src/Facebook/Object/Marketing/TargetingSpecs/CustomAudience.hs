{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.CustomAudience where

import Data.Text (Text, unpack, pack)
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Control.Monad
import Control.Applicative (pure)

-- | Custom audience IDs. Possibly nothing richer than
-- Text is needed.

type CustomAudience = Text

