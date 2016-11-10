module Facebook.Gen.CodeGenStr
 (genFiles)
 where

import Data.Monoid ((<>))
import Data.String
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

typesImport :: Text
typesImport = "import Facebook.Object.Marketing.Types"

typeFileImports :: Vector Text
typeFileImports =
    V.fromList ["import Facebook.Records hiding (get)",
                "import qualified Facebook.Records as Rec",
                "import qualified Data.Aeson as A",
                "import Data.Aeson hiding (Value)",
                "import Data.Text (Text)",
                "import Data.Vector (Vector)",
                "#if MIN_VERSION_time(1,5,0)\n\
                                \import Data.Time.Clock\n\
                \#else\n\
                                \import Data.Time.Clock hiding (defaultTimeLocale, rfc822DateFormat)\n\
                \#endif"]

nodeFileImports :: Vector Text
nodeFileImports =
    V.fromList ["import Facebook.Records hiding (get)",
                "import qualified Facebook.Records as Rec",
                "import Facebook.Types hiding (Id)",
                "import Facebook.Pager",
                "import Facebook.Monad",
                "import Facebook.Graph",
                "import Facebook.Base (FacebookException(..))",
                "import qualified Data.Aeson as A",
                -- "import Data.Time.Format",
                "import Data.Aeson hiding (Value)",
                "import Control.Applicative ( (<|>) )",
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

langExts :: Vector Text
langExts = V.fromList ["DeriveDataTypeable", "DeriveGeneric", "FlexibleContexts", "OverloadedStrings",
                       "ConstraintKinds", "CPP"]

-- What to add after /id to the URL
entityUrlPostfixMap :: Map.Map (Entity, InteractionMode) Text
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
entityModePagerSet :: Set.Set (Entity, InteractionMode)
entityModePagerSet =
    Set.fromList [(Entity "AdCampaign", Reading),
                  (Entity "Insights", Reading),
                  (Entity "AdImage", Reading),
                  (Entity "AdCreative", Reading),
                  (Entity "Ad", Reading),
                  (Entity "AdSet", Reading),
                  (Entity "CustomAudience", Reading),
                  (Entity "AdsPixel", Reading)]

entityModeIdNotInURL :: Set.Set (Entity, InteractionMode)
entityModeIdNotInURL  =
    Set.fromList [(Entity "AdCreative", Deleting)]

-- function return type
entityModeRetType :: Map.Map (Entity, InteractionMode) Text
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

idTypeMap :: Map.Map (Entity, InteractionMode) Text
idTypeMap =
    Map.fromList [((Entity "AdCampaign", Deleting), "CreateCampaignId"),
                  ((Entity "AdCampaign", Updating), "CreateCampaignId"),
                  ((Entity "AdCreative", Deleting), "CreateAdCreativeId"),
                  ((Entity "Ad", Deleting), "CreateAdId"),
                  ((Entity "Ad", Updating), "CreateAdId"),
                  ((Entity "AdSet", Updating), "CreateAdSetId"),
                  ((Entity "AdSet", Deleting), "CreateAdSetId")]

-- | explicit return types for specific operations
entityModeRetDefs :: Map.Map (Entity, InteractionMode) Text
entityModeRetDefs =
    Map.fromList [((Entity "AdImage", Creating), imgCreate),
                  ((Entity "AdCampaign", Creating), campaignCreate),
                  ((Entity "AdCreative", Creating), adcreativeCreate),
                  ((Entity "Ad", Creating), adCreate),
                  ((Entity "AdSet", Creating), adsetCreate),
                  ((Entity "CustomAudience", Creating), customAudienceCreate)]

imgCreate :: Text
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

campaignCreate :: Text
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
customAudienceCreate :: Text
customAudienceCreate =
    "data CreateCustomAudienceId = CreateCustomAudienceId {\n\
     \  customAudienceId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateCustomAudienceId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateCustomAudienceId <$> v .: \"id\"\n"

adsetCreate :: Text
adsetCreate =
    "data CreateAdSetId = CreateAdSetId {\n\
     \  adsetId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdSetId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdSetId <$> v .: \"id\"\n\
     \adsetIdToInt :: CreateAdSetId -> Int\n\
     \adsetIdToInt (CreateAdSetId id) =\n\
     \      case decimal id of\n\
     \        Right (num, _) -> num\n\
     \        Left err -> error $ \"Could not convert CreateAdSetId to Int:\" ++ show err\n"

adcreativeCreate :: Text
adcreativeCreate =
    "data CreateAdCreativeId = CreateAdCreativeId {\n\
     \  adcreativeId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdCreativeId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdCreativeId <$> v .: \"id\"\n\
     \creativeToCreative :: CreateAdCreativeId -> AdCreativeADT\n\
     \creativeToCreative (CreateAdCreativeId id) = AdCreativeADT id\n"

adCreate :: Text
adCreate =
    "data CreateAdId = CreateAdId {\n\
     \  adId :: Text\n\
     \  } deriving Show\n\
     \instance FromJSON CreateAdId where\n\
     \    parseJSON (Object v) =\n\
     \       CreateAdId <$> v .: \"id\"\n"

-- Doees the API call need a token?
isTokenNecessarySet :: Set.Set (Entity, InteractionMode)
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

modPrefix :: Text
modPrefix = "Facebook.Object.Marketing."

modNameToPath :: Text -> Text
modNameToPath = replace "." "/"

genFiles :: Env -> Vector (FilePath, Text)
genFiles env@(Env env_map) =
    let (typesMap', _) = Map.partitionWithKey (\k _ -> k == Entity "Types") env_map
        typesInfo = if Map.null typesMap' -- there won't be a Types module
          then V.empty
          else (typesMap' Map.! (Entity "Types")) Map.! Types
    in genCode env typesInfo

genCode :: Env -> Vector FieldInfo -> Vector (FilePath, Text)
genCode (Env env) types =
    V.fromList $ Map.elems $ Map.mapWithKey (\k a -> genEntity k a types) env

genEntity :: Entity -> ModeFieldInfoMap -> V.Vector FieldInfo -> (FilePath, Text)
genEntity ent@(Entity nameEnt) m types =
    let modName = modPrefix <> nameEnt
        path = T.unpack $ modNameToPath modName <> ".hs"
        h = header modName
        top = genLangExts <> h <> genImports ent <>
                if nameEnt == "Types"
                    then ""
                    else typesImport <> "\n"
        fis = collectFieldInfosMode m
        f x = if nameEnt == "Types"
              then types
              else V.filter (\fi -> not $ V.elem fi types) x
        filtered = f fis
        dataDecl = dataAndFieldInstances $ removeNameTypeDups filtered
        getter = myConcat $ V.map getterFunction $ removeNameDups filtered
        getterFunction fi = getterField fi
        bsInstances = genToBsInstances $ removeNameDups filtered
    in (path, top <> dataDecl <> bsInstances <> getter <> Prelude.foldl append "" (
            Map.elems $ Map.mapWithKey (genMode ent) m))

genMode :: Entity -> InteractionMode -> V.Vector FieldInfo -> Text
genMode _ Types _ = T.empty -- Types.hs doesn't include any functions (except record getters)
genMode ent@(Entity nameEnt) mode unfiltered = -- source code for one entity(/mode ?)
    let doc = "\n-- Entity:" <> nameEnt <> ", mode:" <> T.pack (show mode)
        retDef = getRetDef ent mode
        m = genFunctions mode ent unfiltered
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
        isOfInstances = myConcat $ V.map (\fi -> instanceFunction fi) fis
        nilIsOfInstance = isFieldClassInstanceText (entityToIsField ent mode) "Nil" -- ugly hack
        instanceFunction field = isFieldClassInstance ent mode field
    in isOfClass <> nilIsOfInstance <> isOfInstances

genReqFields :: Vector FieldInfo -> Vector Text
genReqFields fis = V.map fieldToAdt $ V.filter isRequired fis

modeStr :: InteractionMode -> Text
modeStr Reading = "Get"
modeStr Deleting = "Del"
modeStr Updating = "Upd"
modeStr Creating = "Set"
modeStr Types = error "FIXME"

genFunctions :: InteractionMode -> Entity -> V.Vector FieldInfo -> Text
genFunctions mode@Reading ent fis =
  let constr = genConstraint mode V.empty ent
      retConstr = genRetConstraint mode ent fis <> " -- Default fields"
      functionType = genFunctionType ent mode
      function = genFunction ent mode $ genDefFields fis
  in constr <> "\n" <> retConstr <> "\n" <> functionType <> "\n" <> function
genFunctions mode ent fis =
    let constr = genConstraint mode fis ent
        functionType = genFunctionType ent mode
        function = genFunction ent mode "" -- quick and dirty
    in constr <> "\n" <> functionType <> "\n" <> function

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
genRetConstraint _ _ _ = error "genRetConstraint called in invalid context"

header :: (IsString m, Monoid m) => m -> m
header modName = "module " <> modName <> " where\n\n"

concatNewline :: Vector Text -> Text
concatNewline xs = V.foldl' append "" $ V.map (\x -> x <> "\n") xs

genImports :: Entity -> Text
genImports (Entity "Types")  = concatNewline typeFileImports <> (pack $ unsafePerformIO $ readFile "data/manual-types.hs")
genImports _ = concatNewline nodeFileImports

genLangExts :: Text
genLangExts = concatNewline $ V.map (\x -> "{-# LANGUAGE " <> x <> " #-}") langExts

genConstraint :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraint Types _ _ = ""
genConstraint mode fis ent = genConstraintLabel mode fis ent

genConstraintLabel :: InteractionMode -> Vector FieldInfo -> Entity -> Text
genConstraintLabel mode fis ent@(Entity _) =
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
                (maxs, mins) = (List.map List.maximum dups, --FIXME... let's hope we only have to convert to one type...
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
    let create' = "pure $ " <> nt <> " num"
    in "instance A.FromJSON " <> nt <> " where\n\
        \  parseJSON (Number x) =\n\
        \   case toBoundedInteger x of\n\
        \     Just num -> " <> create' <> "\n" <>
        "     Nothing -> error \"parseJSON toBoundedInteger failed\"\n\
        \  parseJSON (String str) =\n\
        \   case decimal str of\n\
        \     Left err -> error err\n\
        \     Right (num, _) -> " <> create' <> "\n" <> -- FIXME
        "instance A.ToJSON " <> nt <> "\n"
typesToJsonInstances (nt, "AdCreativeADT", "Text") =
       "instance A.FromJSON " <> nt <> " where\n\
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
    in  "\n" <> fieldName <> " :: Has " <> adtName <> " r => r -> " <> adtName <> "_"
     <> "\n" <> fieldName <> " r = r `Rec.get` " <> adtName


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

genFunctionName :: Entity -> InteractionMode -> Text
genFunctionName _ Types = ""
genFunctionName (Entity ent) Reading = "get" <> ent
genFunctionName (Entity ent) Deleting = "del" <> ent
genFunctionName (Entity ent) Updating = "upd" <> ent
genFunctionName (Entity ent) Creating = "set" <> ent

getArgName :: InteractionMode -> Text
getArgName Reading = "fl"
getArgName _ = "r"

genFunctionType :: Entity -> InteractionMode -> Text
genFunctionType ent mode =
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
      functionName = genFunctionName ent mode
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
  functionName <> " :: (R.MonadResource m, MonadBaseControl IO m, " <> className <> " " <> param <> ") =>\n  "
          <> idConstr <> "    -- ^ Ad Account Id\n  \
          \-> " <> argName <> "     -- ^ Arguments to be passed to Facebook.\n  \
          \-> " <> maybeToken <> " UserAccessToken -- ^ Optional user access token.\n  \
          \-> FacebookT " <> auth <> " m " <> pager'

genFunction :: Entity -> InteractionMode -> Text -> Text
genFunction ent mode defFields =
    let functionName = genFunctionName ent mode
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
    in functionName <> " (" <> idConstr <> " id) " <> argName <> " mtoken = " <> httpMethod <> " (\"/v2.7/\"" <> idUrl <> " <> \"" <> url
       <> "\") " <> args defFields <> maybeToken <> "mtoken\n\n"

modeToArgs
  :: (Data.String.IsString a, Monoid a)
  => InteractionMode -> a -> a
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
modeToMethod Types = error "modeToMethod cannot be called on Types"

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
        toCamelCase acc s
            | T.null s = acc
            | otherwise =
                let (a, b) = breakOn "_" s
                    b' = dropUnderscore b
                    first =  charToUpper $ T.head b'
                in if T.null b'
                    then toCamelCase (acc <> a) b'
                    else toCamelCase (acc <> a <> first) $ T.tail b'
