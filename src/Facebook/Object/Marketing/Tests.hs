{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}

module Facebook.Object.Marketing.Tests (testAdCreation, getAdAccs) where
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
import Facebook.Object.Marketing.AdAccount
import Facebook.Object.Marketing.Types
import Facebook.Object.Marketing.AdCampaign
import Facebook.Object.Marketing.AdCreative
import qualified Facebook.Object.Marketing.Types as FBT
import qualified Facebook.Object.Marketing.AdCampaign as AdC
import qualified Facebook.Object.Marketing.AdSet as AdS
import qualified Facebook.Object.Marketing.AdImage as AdI
--import Facebook.Object.Marketing.AdLabel
import Facebook.Object.Marketing.TargetingSpecs
import Facebook.Object.Marketing.TargetingSpecs.Location
import Facebook.Object.Marketing.TargetingSpecs.Demographies
import Facebook.Object.Marketing.TargetingSpecs.Placement
import Facebook.Object.Marketing.AdSet
import Facebook.Object.Marketing.Ad
import qualified Facebook.Object.Marketing.Ad as Ad
import Facebook.Object.Marketing.AdImage
import Facebook.Object.Marketing.Utility hiding (toBS)
--import Facebook.Object.Marketing.Insights
import Prelude hiding (id)
import qualified Data.Map.Strict as Map
import Data.Aeson
import Data.Either
import qualified Prelude as P
import Data.Monoid
import Control.Monad.IO.Class

getAdAccs fbUserId fbToken creds = do
  man <- newManager conduitManagerSettings
  now <- getCurrentTime
  let fbuser = FB.Id (T.pack $ show fbUserId)
      tokExpires = addUTCTime 1000 now
      tok = UserAccessToken fbuser fbToken now
  runResourceT $ runFacebookT creds man $ do
    Pager adaccids _ _ <- getAdAccountId $ Just tok
    mapM (\acc -> getAdAccount (id acc) (Name ::: Nil) $ Just tok) adaccids

testAdCreation fbAccId igId fbUserId fbToken fbPageId igTargetId creds = do
  let fbId = Id_ $ T.pack fbAccId
  let fbUrl = "https://www.facebook.com/BeautifulDestinations"
  man <- newManager conduitManagerSettings
  now <- getCurrentTime
  let fbuser = FB.Id (T.pack $ show fbUserId)
      tokExpires = addUTCTime 1000 now
      tok = UserAccessToken fbuser fbToken now
  runResourceT $ runFacebookT creds man $ do
    let campaign = (Name, Name_ "Test Campaign API ad creation") :*: (Objective, Objective_ OBJ_LINK_CLICKS)
                   :*: (AdC.Status, AdC.Status_ PAUSED_) :*: (BuyingType, BuyingType_ AUCTION) :*: Nil
    ret' <- setAdCampaign fbId campaign tok
    let ret = either (error . show) P.id ret'
    let campaign = ret
    let location = TargetLocation (Just ["US", "GB"]) Nothing
    let demo = Demography Female (Just $ mkAge 20) $ Just $ mkAge 35
    -- let target = TargetingSpecs location (Just demo) (Just [InstagramStream]) Nothing
    let target = TargetingSpecs location (Just demo) Nothing (Just [Instagram]) Nothing Nothing Nothing
    let adset = (IsAutobid, IsAutobid_ True) :*: (AdS.Status, AdS.Status_ PAUSED_) :*: (Name, Name_ "Test AdSet API")
                :*: (CampaignId, CampaignId_ $ campaignId ret) :*: (Targeting, Targeting_ target)
                :*: (OptimizationGoal, OptimizationGoal_ REACH)
                :*: (BillingEvent, BillingEvent_ IMPRESSIONS_) :*: (DailyBudget, DailyBudget_ 500) :*: Nil
    adsetRet' <- setAdSet fbId adset tok
    let adsetRet = either (error . show) P.id adsetRet'
    Pager images _ _ <- getAdImage fbId Nil tok
    let imgHash = FBT.hash $ head images
    let cta_value = CallToActionValue fbUrl "FIXME"
    let call_to_action = Just $ CallToActionADT LEARN_MORE cta_value
    let msg = "Planning your holiday travel? Discover the best destinations, hotels, and more!"
    let carousel_child = CarouselChild "Child name" imgHash fbUrl (Just "this is a carousel child description")
    let link = CreativeCarouselData "This is a carousel caption" msg (replicate 4 carousel_child) "http://example.com" -- <-- cannot be fbUrl
    let oss = ObjectStorySpecADT link fbPageId $ Just $ IgId igId
    let adcreative = (Name, Name_ "Test AdCreative")
                    :*: (ObjectStorySpec, ObjectStorySpec_ oss) :*: Nil
    creativeRet' <- setAdCreative fbId adcreative tok
    let !creativeRet = either (error . show) P.id creativeRet'
    let ad = (Creative, Creative_ $ creativeToCreative creativeRet) :*: (AdsetId, AdsetId_ $ adsetIdToInt adsetRet)
            :*: (Name, Name_ "Another Test Ad! API") :*: (Ad.Status, Ad.Status_ PAUSED_) :*: Nil
    adId' <- setAd fbId ad tok

    updAdCampaign campaign ((AdC.Status, AdC.Status_ DELETED_) :*: Nil) tok
    return ()
