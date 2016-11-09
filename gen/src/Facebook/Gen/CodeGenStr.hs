module Facebook.Gen.CodeGenStr
    --(genFiles)
where

import Data.Monoid ((<>))
import Data.Text
import qualified Data.Text as T
import Data.Vector hiding (singleton)
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import System.IO.Unsafe (unsafePerformIO)

import Facebook.Gen.Environment
import Facebook.Gen.Types

import Debug.Trace

typesImport = "import Facebook.Object.Marketing.Types"

imports =
    V.fromList ["import Facebook.Records hiding (get)",
                "import qualified Facebook.Records as Rec",
                "import Facebook.Types hiding (Id)",
                "import Facebook.Pager",
                "import Facebook.Monad",
                "import Facebook.Graph",
                "import Facebook.Base (FacebookException(..))",
                "import qualified Data.Aeson as A",
                "import Data.Time.Format",
                "import Data.Aeson hiding (Value)",
                "import Control.Applicative",
                "import Data.Text (Text)",
                "import Data.Text.Read (decimal)",
                "import Data.Scientific (toBoundedInteger)",
                "import qualified Data.Text.Encoding as TE",
                "import GHC.Generics (Generic)",
                "import qualified Data.Map.Strict as Map",
                "import Data.Vector (Vector)",
                "import qualified Data.Vector as V",
                "import qualified Data.ByteString as BS",
                "import qualified Data.ByteString.Char8 as B8",
                "import qualified Data.ByteString.Builder as BSB",
                "import qualified Data.ByteString.Lazy as BSL",
                "import qualified Control.Monad.Trans.Resource as R",
                "import Control.Monad.Trans.Control (MonadBaseControl)",
                "#if MIN_VERSION_time(1,5,0)\n\
                                \import System.Locale hiding (defaultTimeLocale, rfc822DateFormat)\n\
                                \import Data.Time.Clock\n\
                \#else\n\
                                \import System.Locale\n\
                                \import Data.Time.Clock hiding (defaultTimeLocale, rfc822DateFormat)\n\
                \#endif"]
langExts = V.fromList ["DeriveDataTypeable", "DeriveGeneric", "FlexibleContexts", "OverloadedStrings",
                       "ConstraintKinds", "CPP"]

-- What to add after /id to the URL
entityUrlPostfixMap =
    Map.fromList [((Entity "AdCampaign", Reading), "/campaigns"),
                  ((Entity "AdCampaign", Creating), "/campaigns"),
                  ((Entity "Insights", Reading), "/insights"),
                  ((Entity "AdImage", Reading), "/adimages"),
                  ((Entity "AdImage", Creating), "/adimages"),
                  ((Entity "Ad", Reading), "/ads"),
                  ((Entity "Ad", Creating), "/ads"),
                  ((Entity "AdCreative", Reading), "/adcreatives"),
                  ((Entity "AdCreative", Creating), "/adcreatives"),
                  ((Entity "AdSet", Reading), "/adsets"),
                  ((Entity "AdSet", Creating), "/adsets"),
                  ((Entity "CustomAudience", Reading), "/customaudiences"),
                  ((Entity "CustomAudience", Creating), "/customaudiences"),
                  ((Entity "AdsPixel", Reading), "/adspixels")
                 ]

-- Does the generated function return a Pager?
entityModePagerSet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "Insights", Reading),
                  (Entity "AdImage", Reading),
                  (Entity "AdCreative", Reading),
                  (Entity "Ad", Reading),
                  (Entity "AdSet", Reading),
                  (Entity "CustomAudience", Reading),
                  (Entity "AdsPixel", Reading)]

entityModeIdNotInURL  =
    Set.fromList [(Entity "AdCreative", Deleting)]

-- function return type
entityModeRetType = -- FIXME!
    Map.fromList [((Entity "AdImage", Creating), "(Either FacebookException SetImgs)"),
                  ((Entity "AdImage", Deleting), "(Either FacebookException Success)"),
                  ((Entity "AdCampaign", Deleting), "(Either FacebookException Success)"),
                  ((Entity "AdCampaign", Updating), "(Either FacebookException Success)"),
                  ((Entity "AdCreative", Deleting), "Success"),
                  ((Entity "AdSet", Creating), "(Either FacebookException CreateAdSetId)"),
                  ((Entity "AdSet", Deleting), "(Either FacebookException r)"),
                  ((Entity "AdSet", Updating), "(Either FacebookException Success)"),
                  ((Entity "AdCreative", Creating), "(Either FacebookException CreateAdCreativeId)"),
                  ((Entity "AdCreative", Updating), "(Either FacebookException r)"),
                  ((Entity "AdCreative", Deleting), "(Either FacebookException r)"),
                  ((Entity "Ad", Creating), "(Either FacebookException CreateAdId)"),
                  ((Entity "Ad", Deleting), "(Either FacebookException r)"),
                  ((Entity "Ad", Updating), "(Either FacebookException Success)"),
                  ((Entity "AdAccount", Creating), "(Either FacebookException r)"),
                  ((Entity "AdAccount", Deleting), "(Either FacebookException r)"),
                  ((Entity "AdAccount", Updating), "(Either FacebookException r)"),
                  ((Entity "AdCampaign", Creating), "(Either FacebookException CreateCampaignId)"),
                  ((Entity "CustomAudience", Creating), "(Either FacebookException CreateCustomAudienceId)")]

idTypeMap =
    Map.fromList [((Entity "AdCampaign", Deleting), "CreateCampaignId"),
                  ((Entity "AdCampaign", Updating), "CreateCampaignId"),
                  ((Entity "AdCreative", Deleting), "CreateAdCreativeId"),
                  ((Entity "Ad", Deleting), "CreateAdId"),
                  ((Entity "Ad", Updating), "CreateAdId"),
                  ((Entity "AdSet", Updating), "CreateAdSetId"),
                  ((Entity "AdSet", Deleting), "CreateAdSetId")]

-- Does the generated function return a Pager?
entityModeRetDefs :: Map.Map (Entity, InteractionMode) Text
entityModeRetDefs =
    Map.fromList [((Entity "AdImage", Creating), imgCreate),
                  ((Entity "AdCampaign", Creating), campaignCreate),
                  ((Entity "AdCreative", Creating), adcreativeCreate),
                  ((Entity "Ad", Creating), adCreate),
                  ((Entity "AdSet", Creating), adsetCreate),
                  ((Entity "CustomAudience", Creating), customAudienceCreate)]

imgCreate = "data SetImgs = SetImgs { -- as seen when using curl\n\
                \  images  :: Map.Map Text SetImg\n\
                \  } deriving (Show, Generic)\n\
            \instance FromJSON SetImgs\n\
            \data SetImg = SetImg {\n\
                \  hash, url_ :: Text\n\
            \  } deriving Show\n\
            \instance FromJSON SetImg where\n\
                \  parseJSON (Object v) =\n\
                    \    SetImg <$> v .: \"hash\"\n\
                           \        <*> v .: \"url\"\n"

campaignCreate =
    "data CreateCampaignId = CreateCampaignId {\n\
     \  campaignId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateCampaignId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateCampaignId <$> v .: \"id\"\n"

-- TODO: BENC: it is unclear to me that this should
-- be a separate "create" response type -- what is
-- coming back is a graph node ID (or possibly a
-- graph node ID that we know the type of - a
-- custom audience)
-- Other types do this too but unclear to me if
-- that makes sense?
customAudienceCreate =
    "data CreateCustomAudienceId = CreateCustomAudienceId {\n\
     \  customAudienceId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateCustomAudienceId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateCustomAudienceId <$> v .: \"id\"\n"

adsetCreate =
    "data CreateAdSetId = CreateAdSetId {\n\
     \  adsetId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdSetId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdSetId <$> v .: \"id\"\n"
     <> hackSet

adcreativeCreate =
    "data CreateAdCreativeId = CreateAdCreativeId {\n\
     \  adcreativeId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdCreativeId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdCreativeId <$> v .: \"id\"\n"
     <> hackCreative

adCreate =
    "data CreateAdId = CreateAdId {\n\
     \  adId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdId <$> v .: \"id\"\n"

-- Doees the API call need a token?
isTokenNecessarySet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "AdCampaign", Creating),
                  (Entity "AdCampaign", Updating),
                  (Entity "AdCampaign", Deleting),
                  (Entity "AdImage", Reading),
                  (Entity "AdImage", Creating),
                  (Entity "AdAccount", Creating),
                  (Entity "AdAccount", Updating),
                  (Entity "AdAccount", Deleting),
                  (Entity "AdImage", Deleting),
                  (Entity "Ad", Deleting),
                  (Entity "Ad", Updating),
                  (Entity "Ad", Creating),
                  (Entity "Ad", Reading),
                  (Entity "AdCreative", Reading),
                  (Entity "AdCreative", Deleting),
                  (Entity "AdCreative", Updating),
                  (Entity "AdCreative", Creating),
                  (Entity "AdSet", Deleting),
                  (Entity "AdSet", Updating),
                  (Entity "AdSet", Creating),
                  (Entity "AdSet", Reading),
                  (Entity "CustomAudience", Reading),
                  (Entity "CustomAudience", Creating)]

modPrefix = "Facebook.Object.Marketing."
modNameToPath = replace "." "/"

-- needed for POST
toBsInstances :: Text
toBsInstances = -- FIXME, look at old SimpleType class for instances
  "\ninstance ToBS Text where\n\
  \  toBS = TE.encodeUtf8\n\
  \instance ToBS Char where\n\
  \  toBS = B8.singleton\n\
  \instance ToBS Integer\n\
  \instance ToBS Int\n\
  \instance ToBS Bool where\n\
  \  toBS True = toBS (\"true\" :: String)\n\
  \  toBS False = toBS (\"false\" :: String)\n\
  \--instance ToBS Value where\n\
  \--  toBS = BSL.toStrict . encode\n\
  \instance ToBS Float\n\
  \instance ToBS a => ToBS (Vector a) where\n\
  \  toBS xs = V.foldl' BS.append BS.empty $ V.map toBS xs\n\
  \instance ToBS UTCTime where\n\
  \  toBS t = B8.pack $ formatTime defaultTimeLocale rfc822DateFormat t\n"

hackSet :: Text
hackSet =
    "\nadsetIdToInt :: CreateAdSetId -> Int\n\
    \adsetIdToInt (CreateAdSetId id) =\n\
    \      case decimal id of\n\
    \        Right (num, _) -> num\n\
    \        Left err -> error $ \"Could not convert CreateAdSetId to Int:\" ++ show err\n"

hackCreative :: Text
hackCreative =
    "creativeToCreative :: CreateAdCreativeId -> AdCreativeADT\n\
    \creativeToCreative (CreateAdCreativeId id) = AdCreativeADT id\n"

genFiles :: Env -> Vector (FilePath, Text)
genFiles (Env env) =
    let (typesMap, rest) = Map.partitionWithKey (\k _ -> k == Entity "Types") env
    in if Map.null typesMap -- there won't be a Types module
        then genCode (Env env) V.empty
        else let typesCode = genTypes typesMap
                 typesFieldInfos = (typesMap Map.! (Entity "Types")) Map.! Types
             in genCode (Env env) typesFieldInfos

genCode :: Env -> Vector FieldInfo -> Vector (FilePath, Text)
genCode (Env env) types =
    V.fromList $ Map.elems $ Map.mapWithKey (\k a -> genEntity k a types) env

genTypes :: EntityModeMap -> (FilePath, Text)
genTypes entMap =
  let typesEnt = Prelude.head $ Map.keys entMap
      typesCode = genEntity typesEnt Map.empty V.empty
  in typesCode

genEntity :: Entity -> ModeFieldInfoMap -> V.Vector FieldInfo -> (FilePath, Text)
genEntity ent@(Entity nameEnt) map types =
    let modName = modPrefix <> nameEnt
        path = T.unpack $ modNameToPath modName <> ".hs"
        head = header modName
        top = genLangExts <> head <> genImports ent <>
                if nameEnt == "Types"
                    then toBsInstances
                    else typesImport <> "\n"
        fis = collectFieldInfosMode map
        filter x = if nameEnt == "Types"
                    then types
                    else V.filter (\fi -> not $ V.elem fi types) x
        filtered = filter fis
        dataDecl = dataAndFieldInstances $ removeNameTypeDups filtered
        getter = myConcat $ V.map getterFct $ removeNameDups filtered
        getterFct fi = getterField fi
        bsInstances = genToBsInstances $ removeNameDups filtered
    in (path, top <> dataDecl <> bsInstances <> getter <> Prelude.foldl append "" (
            Map.elems $ Map.mapWithKey (\mode fis -> genMode ent mode fis) map))

genMode :: Entity -> InteractionMode -> V.Vector FieldInfo -> Text
genMode _ Types _ = T.empty -- Types.hs doesn't include any functions (except record getters)
genMode ent@(Entity nameEnt) mode unfiltered = -- source code for one entity(/mode ?)
    let doc = "\n-- Entity:" <> nameEnt <> ", mode:" <> T.pack (show mode)
        retDef = getRetDef ent mode
        m = genFcts mode ent unfiltered
        isInstances = genClassWitnesses ent mode unfiltered
    in doc <> isInstances <> retDef <> m <>
        if nameEnt == "AdAccount" && mode == Reading -- special cases
            then adAccIdDetails <> genGetId
            else if nameEnt == "AdCreative" && mode == Reading
                  then igIdDetails <> genFBPageIdToIgId
                  else ""

myConcat :: V.Vector Text -> Text
myConcat = V.foldl' append ""

genToBsInstances :: V.Vector FieldInfo -> Text
genToBsInstances fis = myConcat $ V.map go fis
    where
        go fi =
         let nt = fieldToAdt fi <> "_" -- FIXME
         in "\ninstance ToBS " <> nt <> " where\n"
            <> "  toBS (" <> nt <> " a) = toBS a\n"

getRetDef :: Entity -> InteractionMode -> Text
getRetDef ent mode =
    case Map.lookup (ent, mode) entityModeRetDefs of
        Nothing -> ""
        Just code -> code

genClassWitnesses :: Entity -> InteractionMode -> Vector FieldInfo -> Text
genClassWitnesses _ Types _ = T.empty
genClassWitnesses ent mode fis =
    let isOfClass = isFieldClass ent mode
        isOfInstances = myConcat $ V.map (\fi -> instanceFct fi) fis
        nilIsOfInstance = isFieldClassInstanceText (entityToIsField ent mode) "Nil" -- ugly hack
        instanceFct field = isFieldClassInstance ent mode field
    in isOfClass <> nilIsOfInstance <> isOfInstances

genReqFields :: Vector FieldInfo -> Vector Text
genReqFields fis = V.map fieldToAdt $ V.filter isRequired fis

modeStr :: InteractionMode -> Text
modeStr Reading = "Get"
modeStr Deleting = "Del"
modeStr Updating = "Upd"
modeStr Creating = "Set"
modeStr Types = error "FIXME"

genFcts :: InteractionMode -> Entity -> V.Vector FieldInfo -> Text
genFcts mode@Reading ent fis =
  let constr = genConstraint mode V.empty ent
      retConstr = genRetConstraint mode ent fis <> " -- Default fields"
      fctType = genFctType ent mode
      fct = genFct ent mode $ genDefFields fis
  in constr <> "\n" <> retConstr <> "\n" <> fctType <> "\n" <> fct
genFcts mode ent fis =
    let constr = genConstraint mode fis ent
        fctType = genFctType ent mode
        fct = genFct ent mode "" -- quick and dirty
    in constr <> "\n" <> fctType <> "\n" <> fct

genDefFields :: V.Vector FieldInfo -> Text
genDefFields fis =
  let ds = genReqFields fis
      defs = V.foldr' append "" $ V.map (\req -> req <> " ::: ") ds
  in defs

genRetConstraintName :: InteractionMode -> Entity -> Text
genRetConstraintName Reading ent =
  genClassName ent Reading <> "Ret"
genRetConstraintName _ _ = ""

genRetConstraint :: InteractionMode -> Entity -> V.Vector FieldInfo -> Text
genRetConstraint Reading ent fis =
  let ds = genReqFields fis
      defs = V.foldr' append "r" $ V.map (\req -> req <> " :*: ") ds
      synName = genRetConstraintName Reading ent
  in "type " <> synName <> " r = " <> defs

header modName = "module " <> modName <> " where\n\n"

concatNewline xs = V.foldl' append "" $ V.map (\x -> x <> "\n") xs

genImports (Entity "Types")  = concatNewline imports <> (pack $ unsafePerformIO $ readFile "data/manual-types.hs")
genImports _ = concatNewline imports
genLangExts = concatNewline $ V.map (\x -> "{-# LANGUAGE " <> x <> " #-}") langExts

genConstraint :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraint Types _ _ = ""
genConstraint mode fis ent = genConstraintLabel mode fis ent

genConstraintLabel :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraintLabel mode fis ent@(Entity entity) =
    let reqs  = genReqFields fis
        reqsHas = V.foldl' append "" $ V.map (\req -> "Has " <> req <> " r, ") reqs
        arg = if mode == Reading
                then " fl r"
                else " r"
        constr = if mode == Reading
                  then "FieldListToRec fl r)"
                  else "ToForm r)"
    in "\ntype " <> genClassName ent mode <> arg <> " = (" <> reqsHas
       <> "A.FromJSON r, " <> entityToIsField ent mode <> " r, " <> constr

dataAndFieldInstances :: V.Vector FieldInfo -> Text
dataAndFieldInstances fis =
    let dups = findDups fis
    in if List.null dups -- Do all fields with the same name have the same type?
        then let dataDecls = myConcat $ V.map dataAndFieldInstance fis
                 new = myConcat $ V.map newtypeInstances fis
             in dataDecls <> new
        else -- this usually happens because Reading mode returns strings while Creating expects unsigned int32
            let dups' = V.concat dups
                unique = V.filter (\fi -> not $ List.elem fi dups') fis -- check
                a@(maxs, mins) = (List.map List.maximum dups, --FIXME... let's hope we only have to convert to one type...
                                List.map List.minimum dups)
                dataDecls = myConcat $ V.map dataAndFieldInstance unique
                new = myConcat $ V.map newtypeInstances unique
                maxNew = T.concat $ List.map dataAndFieldInstance maxs -- these are the real types of our data types
                jsonMax = genJsonNewtype $ List.zip maxs mins
            in dataDecls <> new <> maxNew <> jsonMax

genJsonNewtype :: [(FieldInfo, FieldInfo)] -> Text
genJsonNewtype fis =
    let types = List.map (\(f1, f2) -> (newtypeName f1, type_ f1, type_ f2)) fis
    in T.concat $ List.map typesToJsonInstances types

typesToJsonInstances :: (Text, Text, Text) -> Text
typesToJsonInstances (nt, "Int", "Text") =
    let create = "pure $ " <> nt <> " num"
    in "instance A.FromJSON " <> nt <> " where\n\
        \  parseJSON (Number x) =\n\
        \   case toBoundedInteger x of\n\
        \     Just num -> " <> create <> "\n" <>
        "     Nothing -> error \"parseJSON toBoundedInteger failed\"\n\
        \  parseJSON (String str) =\n\
        \   case decimal str of\n\
        \     Left err -> error err\n\
        \     Right (num, _) -> " <> create <> "\n" <> -- FIXME
        "instance A.ToJSON " <> nt <> "\n"
typesToJsonInstances (nt, "AdCreativeADT", "Text") =
    let create = "pure $ " <> nt <> " creativeId"
    in "instance A.FromJSON " <> nt <> " where\n\
        \  parseJSON (Object v) = " <> nt <> " <$> AdCreativeADT <$>\n\
        \   v .: \"id\" <|> v .: \"creative_id\"\n" <>
        "instance A.ToJSON " <> nt <> "\n"
typesToJsonInstances x = error $ show x

-- "acc_id" and Text turn into:
--
-- data AccId = AccId
-- instance Field AccId where
--  type FieldValue AccId = Text
--  fieldName _ = "acc_id"
--  fieldLabel  = AccId
dataAndFieldInstance :: FieldInfo -> Text
dataAndFieldInstance fi =
    let adtName = fieldToAdt fi
        nt = newtypeName fi
        fieldType = type_ fi
        fieldName = name fi
    in "\ndata "  <> adtName <> " = " <> adtName <> "\n"
        <> "newtype " <> nt <> " = " <> nt <> " "
        <> fieldTypeParan fieldType <> " deriving " <> derivings fieldType <> "\n"
        <> "instance Field " <> adtName <> " where\n  "
        <> "type FieldValue " <> adtName <> " = " <> nt <> "\n  "
        <> "fieldName _ = \"" <> fieldName <> "\"\n  "
        <> "fieldLabel = " <> adtName <> "\n"
        <> "un" <> nt <> " :: " <> nt <> " -> " <> fieldType <> "\n"
        <> "un" <> nt <> " (" <> nt <> " x) = x\n"

newtypeName :: FieldInfo -> Text
newtypeName fi =
    let adtName = fieldToAdt fi
    in adtName <> "_"

fieldTypeParan :: Text -> Text
fieldTypeParan ft =
    if Prelude.length (split (==' ') ft) > 1
        then "(" <> ft <> ")"
        else ft

derivings :: Text -> Text
derivings "UTCTime" = "Generic"
derivings _ = "(Show, Generic)"

newtypeInstances :: FieldInfo -> Text
newtypeInstances fi =
    let nt = newtypeName fi
    in "instance A.FromJSON " <> nt <> "\n"
        <> "instance A.ToJSON " <> nt <> "\n"

entityToIsField :: Entity -> InteractionMode -> Text
entityToIsField (Entity entity) mode = "Is" <> entity <> modeStr mode <> "Field"

isFieldClass :: Entity -> InteractionMode -> Text
isFieldClass entity mode =
    let className = entityToIsField entity mode
    in "\nclass " <> className <> " r\n"
       <> "instance (" <> className <> " h, "
       <> className <> " t) => " <> className
       <> " (h :*: t)\n"

isFieldClassInstanceText :: Text -> Text -> Text
isFieldClassInstanceText className instName =
    "instance " <> className <> " " <> instName <> "\n"

isFieldClassInstance :: Entity -> InteractionMode -> FieldInfo -> Text
isFieldClassInstance ent mode fi =
    let className = entityToIsField ent mode
        instName = fieldToAdt fi
    in isFieldClassInstanceText className instName

genClassName :: Entity -> InteractionMode -> Text
genClassName (Entity e) mode =
    let mStr = modeStr mode
    in e <> mStr

getterField :: FieldInfo -> Text
getterField fi =
    let fieldName = name fi
        adtName = fieldToAdt fi
    in "\n" <> fieldName <> " r = r `Rec.get` " <> adtName

adAccIdDetails :: Text
adAccIdDetails = "type AdAccountIdDetails = Id :*: AccountId :*: Nil\n"

genGetId :: Text
genGetId =
    "\ngetAdAccountId :: (R.MonadResource m, MonadBaseControl IO m) =>\n  \
              \ Maybe UserAccessToken -- ^ User access token.\n  \
            \-> FacebookT anyAuth m (Pager AdAccountIdDetails)\n\
    \getAdAccountId token = getObject \"/v2.7/me/adaccounts\" [] token\n"

igIdDetails :: Text
igIdDetails = "type IgIdDetails = Id :*: Nil"

genFBPageIdToIgId :: Text
genFBPageIdToIgId =
    "\ngetIgId :: (R.MonadResource m, MonadBaseControl IO m) =>\n  \
              \ UserAccessToken -- ^ User access token.\n  \
            \-> FBPageId \n  \
            \-> FacebookT anyAuth m (Pager IgIdDetails)\n\
    \getIgId token (FBPageId pageId) = getObject (\"/v2.7/\" <> pageId <> \"/instagram_accounts\") [(\"fields\", textListToBS $ fieldNameList $ Id ::: Nil)] $ Just token\n"

genFctName :: Entity -> InteractionMode -> Text
genFctName _ Types = ""
genFctName (Entity ent) Reading = "get" <> ent
genFctName (Entity ent) Deleting = "del" <> ent
genFctName (Entity ent) Updating = "upd" <> ent
genFctName (Entity ent) Creating = "set" <> ent

getArgName :: InteractionMode -> Text
getArgName Reading = "fl"
getArgName _ = "r"

genFctType :: Entity -> InteractionMode -> Text
genFctType ent mode =
  let retType = if mode == Reading
                  then "(" <> genRetConstraintName mode ent <> " r)"
                  else case Map.lookup (ent, mode) entityModeRetType of
                          Nothing -> "r"
                          Just ret -> ret
      pager' = if Set.member (ent, mode) entityModePagerSet
                  then "(Pager " <> retType <> ")"
                  else retType
      maybeToken = if Set.member (ent, mode) isTokenNecessarySet
                      then ""
                      else "Maybe"
      fctName = genFctName ent mode
      className = genClassName ent mode
      auth = if mode == Reading
              then "anyAuth"
              else "Auth"
      argName = getArgName mode
      param = if mode == Reading  -- Reading uses extensible records
                  then "fl r"
                  else "r"
      idConstr = case Map.lookup (ent, mode) idTypeMap of
                  Just x -> x
                  Nothing -> "Id_"
  in
  fctName <> " :: (R.MonadResource m, MonadBaseControl IO m, " <> className <> " " <> param <> ") =>\n  "
          <> idConstr <> "    -- ^ Ad Account Id\n  \
          \-> " <> argName <> "     -- ^ Arguments to be passed to Facebook.\n  \
          \-> " <> maybeToken <> " UserAccessToken -- ^ Optional user access token.\n  \
          \-> FacebookT " <> auth <> " m " <> pager'

genFct :: Entity -> InteractionMode -> Text -> Text
genFct ent mode defFields =
    let fctName = genFctName ent mode
        url  = Map.findWithDefault "" (ent,mode) entityUrlPostfixMap
        maybeToken = if Set.member (ent, mode) isTokenNecessarySet && mode == Reading
                        then "$ Just "
                        else ""
        httpMethod = modeToMethod mode
        args = modeToArgs mode
        argName = getArgName mode
        idConstr = case Map.lookup (ent, mode) idTypeMap of
                    Just x -> x
                    Nothing -> "Id_"
        idUrl = if Set.member (ent, mode) entityModeIdNotInURL
                    then ""
                    else " <> id"
    in fctName <> " (" <> idConstr <> " id) " <> argName <> " mtoken = " <> httpMethod <> " (\"/v2.7/\"" <> idUrl <> " <> \"" <> url
       <> "\") " <> args defFields <> maybeToken <> "mtoken\n\n"

modeToArgs Types _ = ""
modeToArgs Reading defFields = "[(\"fields\", textListToBS $ fieldNameList $ " <> defFields <> "fl)] "
modeToArgs Creating _ = "(toForm r) "
modeToArgs Updating _ = "(toForm r) "
modeToArgs Deleting _ = "(toForm r) "

modeToMethod :: InteractionMode -> Text
modeToMethod Reading = "getObject"
modeToMethod Creating = "postForm"
modeToMethod Updating = "postForm"
modeToMethod Deleting = "deleteForm"

-- turns foo_bar into FooBar
fieldToAdt :: FieldInfo -> Text
fieldToAdt (FieldInfo str _ _ _ _)
    | T.null $ dropUnderscore str = str
    | otherwise =
        let str' = dropUnderscore str
            first = charToUpper $ T.head str'
            camel = toCamelCase "" $ T.tail str'
        in first <> camel
    where
        dropUnderscore = T.dropWhile (=='_')
        charToUpper = toUpper . singleton
        toCamelCase acc str
            | T.null str = acc
            | otherwise =
                let (a, b) = breakOn "_" str
                    b' = dropUnderscore b
                    first =  charToUpper $ T.head b'
                in if T.null b'
                    then toCamelCase (acc <> a) b'
                    else toCamelCase (acc <> a <> first) $ T.tail b'
