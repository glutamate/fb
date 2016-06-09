{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
module Facebook.Object.Marketing.AdVideo where

import Facebook.Records hiding (get)
import qualified Facebook.Records as Rec
import Facebook.Types hiding (Id)
import Facebook.Pager
import Facebook.Monad
import Facebook.Graph
import Facebook.Base (FacebookException(..))
import Data.Time.Format
import Data.Aeson
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
import Control.Monad.IO.Class
import qualified Data.Text as T
import System.IO
import System.Directory (removeFile)

data UploadPhaseADT = Start | Transfer | Finish deriving (Show, Generic)
instance FromJSON UploadPhaseADT
instance ToJSON UploadPhaseADT

data UploadPhase = UploadPhase
newtype UploadPhase_ = UploadPhase_ UploadPhaseADT deriving (Show, Generic)
instance Field UploadPhase where
	type FieldValue UploadPhase = UploadPhase_
	fieldName _ = "upload_phase"
	fieldLabel = UploadPhase
unUploadPhase_ :: UploadPhase_ -> UploadPhaseADT
unUploadPhase_ (UploadPhase_ x) = x
instance FromJSON UploadPhase_
instance ToJSON UploadPhase_

instance ToBS UploadPhaseADT where
  toBS Start = toBS ("start" :: String)
  toBS Transfer = toBS ("transfer" :: String)
  toBS Finish = toBS ("finish" :: String)

instance ToBS UploadPhase_ where
	toBS (UploadPhase_ a) = toBS a

uploadPhase r = r `Rec.get` UploadPhase

data Filesize = Filesize
newtype Filesize_ = Filesize_ Int deriving (Show, Generic)
instance Field Filesize where
	type FieldValue Filesize = Filesize_
	fieldName _ = "file_size"
	fieldLabel = Filesize
unFilesize_ :: Filesize_ -> Int
unFilesize_ (Filesize_ x) = x
instance FromJSON Filesize_
instance ToJSON Filesize_

instance ToBS Filesize_ where
	toBS (Filesize_ a) = toBS a

data UploadStartResp = UploadStartResp {
    uploadSessionId :: Int
  , videoId :: Integer
  , startOffset :: Int
  , endOffset :: Int
} deriving Show

instance FromJSON UploadStartResp where
  parseJSON (Object v) = do
    sessionId <- liftA read $ v .: "upload_session_id"
    videoId <- liftA read $ v .: "video_id"
    start <- liftA read $ v .: "start_offset"
    end <- liftA read $ v .: "end_offset"
    return $ UploadStartResp sessionId videoId start end

data UploadSessId = UploadSessId
newtype UploadSessId_ = UploadSessId_ Int deriving (Show, Generic)
instance Field UploadSessId where
	type FieldValue UploadSessId = UploadSessId_
	fieldName _ = "upload_session_id"
	fieldLabel = UploadSessId
unUploadSessId_ :: UploadSessId_ -> Int
unUploadSessId_ (UploadSessId_ x) = x
instance FromJSON UploadSessId_
instance ToJSON UploadSessId_

instance ToBS UploadSessId_ where
	toBS (UploadSessId_ a) = toBS a

data StartOffset = StartOffset
newtype StartOffset_ = StartOffset_ Int deriving (Show, Generic)
instance Field StartOffset where
	type FieldValue StartOffset = StartOffset_
	fieldName _ = "start_offset"
	fieldLabel = StartOffset
unStartOffset_ :: StartOffset_ -> Int
unStartOffset_ (StartOffset_ x) = x
instance FromJSON StartOffset_
instance ToJSON StartOffset_

instance ToBS StartOffset_ where
	toBS (StartOffset_ a) = toBS a

data VideoChunk = VideoChunk
newtype VideoChunk_ = VideoChunk_ FilePath deriving (Show, Generic)
instance Field VideoChunk where
	type FieldValue VideoChunk = VideoChunk_
	fieldName _ = "video_file_chunk"
	fieldLabel = VideoChunk
unVideoChunk_ :: VideoChunk_ -> FilePath
unVideoChunk_ (VideoChunk_ x) = x

instance ToBS VideoChunk_ where
	toBS (VideoChunk_ a) = toBS a

data UploadTransferResp = UploadTransferResp {
    start :: Int
  , end :: Int
} deriving Show

instance FromJSON UploadTransferResp where
  parseJSON (Object v) = do
    start <- liftA read $ v .: "start_offset"
    end <- liftA read $ v .: "end_offset"
    return $ UploadTransferResp start end

type VideoId = Integer
uploadVideo :: (R.MonadResource m, MonadBaseControl IO m) =>
	Id_    -- ^ Ad Account Id
	-> FilePath    -- ^ Arguments to be passed to Facebook.
  ->  T.Text
	->  UserAccessToken -- ^ Optional user access token.
	-> FacebookT Auth m (Either FacebookException VideoId)
uploadVideo (Id_ id) fp videoTitle mtoken = do
  bs <- liftIO $ BS.readFile fp
  ret <- sendVideoStart id mtoken bs
  case ret of
    Left fbExp -> return $ Left fbExp
    Right resp -> do
      ret <- sendVideoChunks id mtoken bs $ startResp2ChunkResp resp
      case ret of
        Left fbExp -> return $ Left fbExp
        Right _ -> do
          ret <- sendVideoFinish id mtoken videoTitle resp
          case ret of
            Left fbExp -> return $ Left fbExp
            Right _ -> return $ Right $ videoId resp

startResp2ChunkResp (UploadStartResp sess _ start end) =
  (sess, UploadTransferResp start end)

sendVideoFinish :: (R.MonadResource m, MonadBaseControl IO m) =>
  T.Text
  -> UserAccessToken
  -> T.Text
  -> UploadStartResp
	-> FacebookT Auth m (Either FacebookException Success)
sendVideoFinish id tok title (UploadStartResp sess _ _ _) = do
  let r = toForm $ (UploadPhase, UploadPhase_ Finish) :*:
            (UploadSessId, UploadSessId_ sess) :*:
            (Title, Title_ title) :*: Nil
  postFormVideo ("/v2.6/" <> id <> "/advideos") r tok

sendVideoChunks :: (R.MonadResource m, MonadBaseControl IO m) =>
  T.Text
  -> UserAccessToken
  -> BS.ByteString
  -> (Int, UploadTransferResp)
	-> FacebookT Auth m (Either FacebookException Bool)
sendVideoChunks id tok bs (sess, transResp) = do
    (tmpFp, hdl) <- liftIO $ openBinaryTempFile "/tmp" "foo.bar"
    liftIO $ hClose hdl
    ret <- go transResp tmpFp
    liftIO $ removeFile tmpFp
    return ret
  where
    go (UploadTransferResp start end) fp
      | start == end = return $ Right True
      | otherwise = do
        let chunk = BS.take (end-start) $ BS.drop start bs
        liftIO $ BS.writeFile fp chunk
        let r = toForm $ (UploadPhase, UploadPhase_ Transfer) :*:
              (UploadSessId, UploadSessId_ sess) :*:
              (StartOffset, StartOffset_ start) :*:
              (VideoChunk, VideoChunk_ fp) :*: Nil
        ret <- postFormVideo ("/v2.6/" <> id <> "/advideos") r tok
        case ret of
          Right resp -> go resp fp
          Left x -> return $ Left x

sendVideoStart :: (R.MonadResource m, MonadBaseControl IO m) =>
	T.Text -- ^ Ad Account Id
	-> UserAccessToken -- ^ Optional user access token.
  -> BS.ByteString
	-> FacebookT Auth m (Either FacebookException UploadStartResp)
sendVideoStart id mtoken bs = do
  let r = toForm $ (UploadPhase, UploadPhase_ Start) :*:
                  (Filesize, Filesize_ $ BS.length bs) :*:
                  Nil
  postFormVideo ("/v2.6/" <> id <> "/advideos") r mtoken
