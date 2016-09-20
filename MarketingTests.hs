{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

import System.Environment
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as TE
import Facebook hiding (Id, Male, Female)
import qualified Facebook as FB
import Facebook.Records
import           Control.Monad.Trans.Resource
import           Control.Monad
import Data.Time
--import Control.Monad.Trans
import Control.Monad.IO.Class
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdCampaign
import Facebook.Object.Marketing.AdCreative
import qualified Facebook.Object.Marketing.AdCampaign as AdC
import qualified Facebook.Object.Marketing.AdSet as AdS
import qualified Facebook.Object.Marketing.AdPreview as AdPreview
import qualified Facebook.Object.Marketing.AdImage as AdI
import qualified Facebook.Object.Marketing.AdVideo as AdV
--import Facebook.Object.Marketing.AdLabel
import Facebook.Object.Marketing.TargetingSpecs
import Facebook.Object.Marketing.TargetingSpecs.Location
import Facebook.Object.Marketing.TargetingSpecs.Demographies
import Facebook.Object.Marketing.TargetingSpecs.Placement
import qualified Facebook.Object.Marketing.TargetingSpecs.Interests as Int
import Facebook.Object.Marketing.AdSet
import Facebook.Object.Marketing.Ad
import qualified Facebook.Object.Marketing.Ad as Ad
import Facebook.Object.Marketing.AdImage
import Facebook.Object.Marketing.Utility hiding (toBS)
import qualified Facebook.Object.Marketing.Insights as In
import Prelude hiding (id)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Either
import qualified Prelude as P

main = do
  appId <- getEnv "FB_APP_ID"
  appSecret <- getEnv "FB_APP_SECRET"
  tokenBody <- getEnv "FB_ACCESS_TOKEN"
  fbUid <- getEnv "FB_USER_ID"
  pageId <- liftM T.pack $ getEnv "FB_PAGE_ID"
  fbUrl <- liftM T.pack $ getEnv "FB_URL"
  --let fbUrl = "http://me.example.com"
  man <- newManager conduitManagerSettings
  now <- getCurrentTime
  let creds = Credentials "bdpromo" (T.pack appId) (T.pack appSecret)
      fbuser = FB.Id (T.pack fbUid)
      tokExpires = addUTCTime 1000 now
      tok = UserAccessToken fbuser (T.pack tokenBody) now
  runResourceT $ runFacebookT creds man $ do
    u <- getUser "me" [] (Just tok)
    --liftIO $ print u
    Pager adaccids _ _ <- getAdAccountId $ Just tok
    ret <- AdV.uploadVideo (id $ head adaccids) "tomn.mp4" "Tom's test video" tok
    liftIO $ print ret
    let videoId = either (error . show) P.id ret
    AdV.waitForVideo videoId tok
    liftIO $ print "Video ready!"
    thumb <- AdV.getPrefThumbnail videoId tok
    --Pager (igId':_) _ _ <- getIgId tok $ FBPageId pageId
    qs <- Int.queryInterest "travel" tok
    let ids = map (Int.Interest <$> Int.id_) $ Int.data_ qs
    ----liftIO $ print ids
    ----let igId = unId_ $ id igId'
    let igId = "143574149129350" -- BD
    ----liftIO $ print igId
    liftIO $ print "AdAccount Ids"
    liftIO $ print adaccids
    let acc = id $ head adaccids
    --adAcc <- getAdAccount acc
    --            (Balance ::: AmountSpent ::: Age ::: Owner ::: Name ::: Nil)
    --            (Just tok)
    --liftIO $ print adAcc
    ----adAcc2 <- getAdAccount (id $ head $ tail adaccids)
    ----            (Balance ::: AmountSpent ::: Age ::: Owner ::: Name ::: Nil)
    ----            (Just tok)
    ----liftIO $ print adAcc2
    ----adAcc3 <- getAdAccount (id $ head $ tail $ tail adaccids)
    ----            (Balance ::: AmountSpent ::: Age ::: Owner ::: Name ::: Nil)
    ----            (Just tok)
    ----liftIO $ print adAcc3
    Pager adCamps _ _ <- getAdCampaign acc (Name ::: Nil) tok
    liftIO $ print "AdCamps"
    liftIO $ print adCamps
    let adCampId = (id $ head adCamps)
    --let adCampId = Id_ "6044657233872"
    Pager adSets _ _ <- getAdSet adCampId (ConfiguredStatus ::: EffectiveStatus ::: DailyBudget ::: Nil) tok
    liftIO $ print "AdSets"
    liftIO $ print adSets
    --let (Id_ idText) = id $ head adSets
    --Pager insights _ _ <- In.getInsights (FB.Id idText) [] tok
    --liftIO $ print (insights:: [WithJSON In.Insights])
    let adSetId = (id $ head adSets)
    Pager insights _ _ <- In.getInsightsBreak adSetId (In.Age ::: In.Gender ::: Nil) tok
    ----Pager insights _ _ <- In.getInsightsBreak adSetId (In.Country ::: Nil) tok
    ----liftIO $ print idText
    liftIO $ print insights
    --Pager images _ _ <- getAdImage (id $ head adaccids)
    --        (Id ::: Name ::: Nil) tok
    --liftIO $ print $ (id $ head adaccids)
    --liftIO $ print $ length images
    --let rec = (Filename, Filename_ "/home/alex/code/beautilytics/fb/bridge.jpg") :*: Nil
    --adImg' <- setAdImage acc rec tok
    --let adImg = either (error . show) P.id adImg'
    --liftIO $ print adImg
    --Pager images' _ _ <- getAdImage acc
    --    (Id ::: Nil) tok
    --liftIO $ print $ length images'
    ----delRet <- delAdImage (id $ head adaccids) ((Hash, Hash_ "5f73a7d1df0252ac7f012224dde315d0") :*: Nil) tok
    ----liftIO $ print delRet
    --Pager images'' _ _ <- getAdImage acc
    --    Nil tok
    --liftIO $ print images''
    let campaign = (Name, Name_ "Test Campaign Video") :*: (Objective, Objective_ OBJ_LINK_CLICKS)
                   :*: (AdC.Status, AdC.Status_ PAUSED_) :*: (BuyingType, BuyingType_ AUCTION) :*: Nil
    ret' <- setAdCampaign acc campaign tok
    liftIO $ print ret'
    let ret = either (error . show) P.id ret'
    let campaign = ret
    let location = TargetLocation ["US", "GB"]
    let demo = Demography Female (Just $ mkAge 20) $ Just $ mkAge 35
    let target = TargetingSpecs location (Just demo) (Just [InstagramStream]) $ Just ids
    let adset = (IsAutobid, IsAutobid_ True) :*: (AdS.Status, AdS.Status_ PAUSED_) :*: (Name, Name_ "Test AdSet Video API")
                :*: (CampaignId, CampaignId_ $ campaignId ret) :*: (Targeting, Targeting_ target)
                :*: (OptimizationGoal, OptimizationGoal_ REACH)
                :*: (BillingEvent, BillingEvent_ IMPRESSIONS_) :*: (DailyBudget, DailyBudget_ 500) :*: Nil
    adsetRet' <- setAdSet acc adset tok
    liftIO $ print adsetRet'
    let adsetRet = either (error . show) P.id adsetRet'
    ----bla <- delAdSet adsetRet Nil tok
    ----liftIO $ print bla
    ----Pager adCr _ _ <- getAdCreative (id $ head adaccids) (Name ::: ObjectStoryId ::: Nil) $ Just tok
    ----liftIO $ print adCr
    ----let pageId = unObjectStoryId_ $ object_story_id $ head adCr
    ----Pager adSets _ _ <- getAdSet acc (Name ::: BillingEvent ::: OptimizationGoal ::: Targeting ::: Nil) tok
    ----liftIO $ print adSets
    --let imgHash = AdI.hash $ AdI.images adImg Map.! "bridge.jpg"
    let cta_value = CallToActionValue fbUrl "FIXME"
    --let call_to_action = Just $ CallToActionADT LEARN_MORE cta_value
    --let msg = "Planning your holiday travel? Discover the best destinations, hotels, and more!"
    ---- regular ad
    let call_to_action = CallToActionADT LEARN_MORE cta_value
    let videoLink = AdCreativeVideoData call_to_action "This is a description" (AdV.uri thumb) videoId
    ----let link = AdCreativeLinkData "This is a caption" (Hash_ imgHash) fbUrl msg
    ----                (Just ("This is a description" :: T.Text)) call_to_action
    ---- carousel ad
    --let carousel_child = CarouselChild "Child name" (Hash_ imgHash) fbUrl (Just "this is a carousel child description")
    --let link = CreativeCarouselData "This is a carousel caption" msg (replicate 4 carousel_child) "http://example.com" -- <-- cannot be fbUrl
   ---- Left (FacebookException {..., fbeErrorUserTitle = Just "Objective of campaign requires creative with external link or call to action", fbeErrorUserMsg = Just "For this campaign objective, a creative is required to have either external link or Call to Action.", ...})
    --let oss = ObjectStorySpecADT link (FBPageId pageId) $ Just $ IgId igId
    let oss = ObjectStorySpecVideoLink videoLink (FBPageId pageId) $ Just $ IgId igId
    let adcreative = (Name, Name_ "Test Video AdCreative")
                    :*: (ObjectStorySpec, ObjectStorySpec_ oss) :*: Nil
    creativeRet' <- setAdCreative acc adcreative tok
    liftIO $ print ("creativeRet'", creativeRet')
    let !creativeRet = either (error . show) P.id creativeRet'
    let ad = (Creative, Creative_ $ creativeToCreative creativeRet) :*: (AdsetId, AdsetId_ $ adsetIdToInt adsetRet)
            :*: (Name, Name_ "Another Test Ad Video API") :*: (Ad.Status, Ad.Status_ PAUSED_) :*: Nil
    Pager ads _ _ <- getAd acc (Name ::: BidType ::: Nil) tok
    liftIO $ print ("ads", ads)
    adId' <- setAd acc ad tok
    liftIO $ print ("adId'", adId')
    let adId = either (error "haha") P.id adId'
    liftIO $ print ("adId", adId)
    Pager (AdPreview.PreviewResponse preview:_) _ _ <- AdPreview.getAdPreview acc  oss InstagramStream tok
    liftIO $ T.putStrLn preview


    -------- in order to run an ad, we have to set the status of the campaign, adset, and ad to ACTIVE
    --------updAdCampaign campaign ((AdC.Status, AdC.Status_ DELETED_) :*: Nil) tok
    --------liftIO $ print aaa
    --------bbb <- updAdSet adsetRet ((AdS.Status, AdS.Status_ ACTIVE_) :*: (DailyBudget, DailyBudget_ 510) :*: Nil) tok
    --------liftIO $ print bbb
    --------ccc <- updAd adId ((Ad.Status, Ad.Status_ ACTIVE_) :*: Nil) tok
    --------liftIO $ print ccc
    --------let delId = (Id, Id_ $ campaignId ret) :*: (DeleteStrategy, DeleteStrategy_ Nil
    --------delCampaign <- delAdCampaign ret delId tok -- (id $ head adaccids) delId tok
    --------liftIO $ print delCampaign
    --return ()
