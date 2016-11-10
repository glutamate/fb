module Facebook.Object.Marketing.Utility where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import GHC.Generics
import Data.ByteString
import qualified Data.Aeson.Encode as AE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Builder as TLB

dropString :: String -> String -> String
dropString pre s = case stripPrefix pre s of
  Nothing      -> s
  Just dropped -> dropped

dropOption :: String -> Options
dropOption pre = defaultOptions
  { fieldLabelModifier = dropString pre
  , omitNothingFields = True
  }

parseJSONWithPrefix
  :: forall a. (GHC.Generics.Generic a, GFromJSON (GHC.Generics.Rep a))
  => String -> Value -> Parser a
parseJSONWithPrefix pre = genericParseJSON (dropOption pre)

toJSONWithPrefix
  :: forall a.  (GHC.Generics.Generic a, GToJSON (GHC.Generics.Rep a))
  => String -> a -> Value
toJSONWithPrefix pre = genericToJSON (dropOption pre)

toBS :: Value -> ByteString
toBS = TE.encodeUtf8 . TL.toStrict . TLB.toLazyText . AE.encodeToTextBuilder

data WithJSON a = WithJSON { withJsonValue :: Value,
                             unWithJson :: a }deriving Show

instance FromJSON a => FromJSON (WithJSON a) where
  parseJSON v = do
    x <- parseJSON v
    return $ WithJSON v x
