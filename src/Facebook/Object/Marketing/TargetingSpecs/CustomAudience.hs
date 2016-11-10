{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}

module Facebook.Object.Marketing.TargetingSpecs.CustomAudience where

import Data.Text (Text)

-- | Custom audience IDs. Possibly nothing richer than
-- Text is needed.

type CustomAudience = Text

