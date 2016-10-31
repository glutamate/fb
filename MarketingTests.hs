{-# Language BangPatterns #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

import System.Environment
import Network.HTTP.Conduit
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Facebook hiding (Id, Male, Female)
import qualified Facebook as FB
import Facebook.Records
import           Control.Monad.Trans.Resource
import           Control.Monad
import Data.Time
import Control.Monad.IO.Class
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdCampaign
import Facebook.Object.Marketing.AdCreative
import qualified Facebook.Object.Marketing.AdCampaign as AdC
import qualified Facebook.Object.Marketing.AdSet as AdS
import qualified Facebook.Object.Marketing.AdImage as AdI
import qualified Facebook.Object.Marketing.AdVideo as AdV
import Facebook.Object.Marketing.CustomAudience
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
import Data.Function (on)
import Data.List (sortBy)
import qualified Prelude as P

main = do
  appId <- getEnv "FB_APP_ID"
  appSecret <- getEnv "FB_APP_SECRET"
  tokenBody <- getEnv "FB_ACCESS_TOKEN"
  fbUid <- getEnv "FB_USER_ID"
  pageId <- liftM T.pack $ getEnv "FB_PAGE_ID"
  fbUrl <- liftM T.pack $ getEnv "FB_URL"
  igId <- liftM T.pack $ getEnv "IG_ID"

  man <- newManager conduitManagerSettings
  now <- getCurrentTime

  -- "bdpromo" is wrongly here, but it doesn't seem
  -- to affect any of the tests actually run by this
  -- test set.
  let creds = Credentials "bdpromo" (T.pack appId) (T.pack appSecret)
      fbuser = FB.Id (T.pack fbUid)
      tokExpires = addUTCTime 1000 now
      tok = UserAccessToken fbuser (T.pack tokenBody) now
  runResourceT $ runFacebookT creds man $ do

    testGetUser tok

    acc <- testGetAdAccId tok

    new_audience <- testCreateCustomAudience acc tok

    audiences_pager <- testGetCustomAudience acc tok

    let audiences = pagerData audiences_pager
        nonlookalike_audiences = filter (\audience -> (unSubtype_ $ subtype audience) /= "LOOKALIKE") audiences
        sorted_audiences = sortBy ((flip compare) `on` (unApproximateCount_ . approximate_count)) nonlookalike_audiences
        biggest_audience = head sorted_audiences

    -- so we can get the audience in the first page with
    -- the biggest approximate count.

    -- this is failing -- maybe because the
    -- just-created audience is too small or not ready?
    -- testCreateLookalikeAudience acc tok (customAudienceId new_audience)

    -- this won't work when run multiple times because a lookalike
    -- audience can only be created once with a particular spec.
    testCreateLookalikeAudience acc tok ((unId_ . id) biggest_audience)

    error "BENC: enough tests"

    (videoId, thumb) <- testUploadVideo acc tok

    testQueryInterests tok

    campaign <- testCreateCampaign acc tok

    testGetAdCampaigns acc tok

    adsetRet <- testCreateAdSet campaign acc tok

    creativeRet <- testCreateCreative fbUrl videoId thumb igId pageId acc tok

    testGetCreative acc tok

    testGetAds acc tok

    testSetAd adsetRet creativeRet acc tok

    liftIO $ putStrLn "Test suite finished without failure."

testGetUser tok = do
    liftIO $ putStrLn "TEST: getUser"
    u <- getUser "me" [] (Just tok)
    return ()


testGetAdAccId tok = do
    liftIO $ putStrLn "TEST: getAdAccountId"
    Pager adaccids _ _ <- getAdAccountId $ Just tok

    when (length adaccids == 0) $ error "Test failure: the test user does not seem to have any ad accounts"
    liftIO $ print ("AdAccount Ids", adaccids)
    let acc = id $ head adaccids

    return acc


testUploadVideo acc tok = do

    liftIO $ putStrLn "TEST: upload video"

    ret <- AdV.uploadVideo acc "tomn.mp4" "Tom's test video" tok
    liftIO $ print ("Return value from uploadVideo", ret)
    let videoId = either (error . show) P.id ret
    AdV.waitForVideo videoId tok
    liftIO $ putStrLn "Video ready"
    thumb <- AdV.getPrefThumbnail videoId tok
    liftIO $ print ("videoid,thumb", videoId, thumb)
    return (videoId, thumb)


testQueryInterests tok = do
    liftIO $ putStrLn "TEST: querying travel interest"
    qs <- Int.queryInterest "travel" tok
    let ids = map (Int.Interest <$> Int.id_) $ Int.data_ qs
    return ()


testGetAdCampaigns acc tok = do
    liftIO $ putStrLn "TEST: get ad campaigns and sets"
    -- this test requires there to be an ad campaign in the
    -- test account already? That isn't necessarily the
    -- case with a plain test user?
    Pager adCamps _ _ <- getAdCampaign acc (Name ::: Nil) tok
    liftIO $ print "Ad campaigns"
    liftIO $ print adCamps
    when (length adCamps == 0) $ error "Test failure: adCamps == []"
    let adCampId = (id $ head adCamps)
    --let adCampId = Id_ "6044657233872"
    Pager adSets _ _ <- getAdSet adCampId (ConfiguredStatus ::: EffectiveStatus ::: DailyBudget ::: Nil) tok
    liftIO $ print "AdSets"
    liftIO $ print adSets
    return ()

testCreateCampaign acc tok = do
    liftIO $ putStrLn "TEST: Create campaign"
    let campaign = (Name, Name_ "Test campaign created by Haskell fb module test suite") :*: (Objective, Objective_ OBJ_POST_ENGAGEMENT)
                   :*: (AdC.Status, AdC.Status_ PAUSED_) :*: (BuyingType, BuyingType_ AUCTION) :*: Nil
    ret' <- setAdCampaign acc campaign tok

    liftIO $ print ("return value from setAdCampaign", ret')
    let campaign = either (\e -> error $ "error returned from setCampaign: " ++ show e)
                          P.id ret'

    return campaign

testCreateAdSet campaign acc tok = do

    liftIO $ putStrLn "TEST: create adset"

    let location = TargetLocation ["US", "GB"]
    let demo = Demography Female (Just $ mkAge 20) $ Just $ mkAge 35
    let target = TargetingSpecs location (Just demo) Nothing (Just [Facebook]) Nothing Nothing -- $ Just (zip (repeat Int.AdInterest) ids)
    let adset = (IsAutobid, IsAutobid_ True) :*: (AdS.Status, AdS.Status_ PAUSED_) :*: (Name, Name_ "Test AdSet Video API")
                :*: (CampaignId, CampaignId_ $ campaignId campaign) :*: (Targeting, Targeting_ target)
                :*: (OptimizationGoal, OptimizationGoal_ POST_ENGAGEMENT)
                :*: (BillingEvent, BillingEvent_ IMPRESSIONS_) :*: (DailyBudget, DailyBudget_ 500) :*: Nil
    liftIO $ print ("creating adset", (acc, adset, tok))
    adsetRet' <- setAdSet acc adset tok
    liftIO $ print ("adset ret", adsetRet')
    let adsetRet = either (error . show) P.id adsetRet'
    return adsetRet

testCreateCreative fbUrl videoId thumb igId pageId acc tok = do
    liftIO $ putStrLn "TEST: create ad creative"
    let cta_value = CallToActionValue fbUrl "Test link caption"
    let call_to_action = CallToActionADT LEARN_MORE cta_value

    let videoLink = AdCreativeVideoData call_to_action "This is a description" (AdV.uri thumb) videoId
    let oss = ObjectStorySpecVideoLink videoLink (FBPageId pageId) $ Just $ IgId igId
    let adcreative = (Name, Name_ "Test AdCreative created by Haskell fb module test suite")
                    :*: (ObjectStorySpec, ObjectStorySpec_ oss) :*: Nil
    creativeRet' <- setAdCreative acc adcreative tok
    liftIO $ print creativeRet'
    let !creativeRet = either (error . show) P.id creativeRet'
    return creativeRet

testGetCreative acc tok = do
    liftIO $ putStrLn "TEST: get ad creative"
    creative <- getAdCreative acc (Name {- ::: InstagramPermalinkUrl -} ::: InstagramActorId ::: Nil) tok
    liftIO $ print creative
    return ()

testGetAds acc tok = do
    liftIO $ putStrLn "TEST: get ads"
    Pager ads _ _ <- getAd acc (Name ::: BidType ::: Nil) tok
    liftIO $ print ads


testSetAd adsetRet creativeRet acc tok = do
    liftIO $ putStrLn "TEST: set ad"
    let ad = (Creative, Creative_ $ creativeToCreative creativeRet) :*: (AdsetId, AdsetId_ $ adsetIdToInt adsetRet)
            :*: (Name, Name_ "Another Test Ad Video API") :*: (Ad.Status, Ad.Status_ PAUSED_) :*: Nil

    adId' <- setAd acc ad tok
    liftIO $ print adId'
    let adId = either (\e -> error $ "setAd returned: " ++ show e)
                      P.id adId'
    liftIO $ print adId
    return ()

testGetCustomAudience acc tok = do
    liftIO $ putStrLn "TEST: getCustomAudience"
    audiences <- getCustomAudience acc (Id ::: ApproximateCount ::: AccountId ::: DataSource ::: DeliveryStatus ::: Description ::: Subtype ::: Nil) tok
    --- can't be tested with get-of-everything as not present in every custom audience: ::: LookalikeAudienceIds 
    -- doesn't test ExternalEventSource because it is not present in every custom audience - only ones based round an external event source
    

    liftIO $ print ("audiences", audiences)
    return audiences

testCreateCustomAudience acc tok = do
    liftIO $ putStrLn "TEST: create custom audience"
    let params = (Subtype, Subtype_ "CUSTOM") -- TODO: enumerated Subtype_ doesn't work despite apparently being the same in JSON serialisation.
          :*: (Description, Description_ "custom audience for fb test - description")
          :*: (Name, Name_ "fb test custom audience")
          :*: Nil
    -- let params = (Subtype, Subtype_ CUSTOM) :*: Nil
  
    liftIO $ print $ toJSON params
    ret <- setCustomAudience acc params tok
    let customAudienceId = either (\e -> error $ "setCustomAudience returned: " ++ show e)
                                  P.id ret
    liftIO $ print ("ret", ret)
    liftIO $ print ("custom audience id", customAudienceId)
    return customAudienceId

testCreateLookalikeAudience acc tok audience = do
    liftIO $ putStrLn "TEST: create lookalike audience"
    let params = (Subtype, Subtype_ "LOOKALIKE")
             :*: (Name, Name_ "fb test lookalike audience")
             :*: (OriginAudienceId, OriginAudienceId_ audience)
             :*: (LookalikeSpec, LookalikeSpec_ (LookalikeSpecADT (Just "similarity") "US"))
             :*: Nil
    liftIO $ print $ toJSON params
    ret <- setCustomAudience acc params tok
    let customAudienceId = either (\e -> error $ "setCustomAudience returned: " ++ show e)
                                  P.id ret
    liftIO $ print ("ret", ret)
    liftIO $ print ("custom audience id", customAudienceId)
    return customAudienceId

