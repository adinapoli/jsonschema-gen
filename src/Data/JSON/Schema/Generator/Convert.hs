{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Data.JSON.Schema.Generator.Convert
    ( convert
    ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<*>))
import Data.Monoid (mappend)
#endif

import Data.JSON.Schema.Generator.Types (Schema(..), SchemaChoice(..), getNullable)
import Data.Text (Text)

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as Vector
import Data.Aeson.Key (fromString)
import qualified Data.Text as T

--------------------------------------------------------------------------------

convert :: A.Options -> Schema -> A.Value
convert = convert' False

convert' :: Bool -> A.Options -> Schema -> A.Value
convert' = (((A.Object . KM.fromList) .) .) . convertToList

convertToList :: Bool -> A.Options -> Schema -> [(A.Key,A.Value)]
convertToList inArray opts s = foldr1 (++) $
    [ jsId
    , jsSchema
    , jsSchemaType           opts
    , jsTitle
    , jsDescription
    , jsReference
    , jsType         inArray opts
    , jsFormat
    , jsLowerBound
    , jsUpperBound
    , jsValue
    , jsItems                opts
    , jsProperties           opts
    , jsPatternProps         opts
    , jsOneOf                opts
    , jsRequired             opts
    , jsDefinitions          opts
    ] <*> [s]

--------------------------------------------------------------------------------

jsId :: Schema -> [(A.Key,A.Value)]
jsId (SCSchema {scId = i}) = [("id", string i)]
jsId _ = []

jsSchema :: Schema -> [(A.Key,A.Value)]
jsSchema SCSchema {scUsedSchema = s} = [("$schema", string s)]
jsSchema _ = []

jsSchemaType :: A.Options -> Schema -> [(A.Key,A.Value)]
jsSchemaType opts SCSchema {scSchemaType = s} = convertToList False opts s
jsSchemaType _ _ = []

jsTitle :: Schema -> [(A.Key,A.Value)]
jsTitle SCConst  {scTitle = "" } = []
jsTitle SCConst  {scTitle = t  } = [("title", string t)]
jsTitle SCObject {scTitle = "" } = []
jsTitle SCObject {scTitle = t  } = [("title", string t)]
jsTitle SCArray  {scTitle = "" } = []
jsTitle SCArray  {scTitle = t  } = [("title", string t)]
jsTitle SCOneOf  {scTitle = "" } = []
jsTitle SCOneOf  {scTitle = t  } = [("title", string t)]
jsTitle _ = []

jsDescription :: Schema -> [(A.Key,A.Value)]
jsDescription SCString  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCInteger {scDescription = (Just d) } = [("description", string d)]
jsDescription SCNumber  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCBoolean {scDescription = (Just d) } = [("description", string d)]
jsDescription SCConst   {scDescription = (Just d) } = [("description", string d)]
jsDescription SCObject  {scDescription = (Just d) } = [("description", string d)]
jsDescription SCArray   {scDescription = (Just d) } = [("description", string d)]
jsDescription SCOneOf   {scDescription = (Just d) } = [("description", string d)]
jsDescription _ = []

jsReference :: Schema -> [(A.Key,A.Value)]
jsReference SCRef {scReference = r} = [("$ref", string r)]
jsReference _ = []

jsType :: Bool -> A.Options -> Schema -> [(A.Key,A.Value)]
jsType _ _ s
  | Just (string -> ty) <- schemaToType s
  = if getNullable s then [ ("type", array [ty, string "null"]) ] else [ ( "type", ty ) ]
  | otherwise
  = []

schemaToType :: Schema -> Maybe T.Text
schemaToType = \case
  SCString{}  -> Just "string"
  SCInteger{} -> Just "integer"
  SCNumber{}  -> Just "number"
  SCBoolean{} -> Just "boolean"
  SCObject{}  -> Just "object"
  SCArray{}   -> Just "array"
  SCNull{}    -> Just "null"
  _           -> Nothing

jsFormat :: Schema -> [(A.Key,A.Value)]
jsFormat SCString {scFormat = Just f} = [("format", string f)]
jsFormat _ = []

jsLowerBound :: Schema -> [(A.Key,A.Value)]
jsLowerBound SCString {scLowerBound = (Just n)} = [("minLength", number n)]
jsLowerBound SCNumber {scLowerBound = (Just n)} = [("minimum",   number n)]
jsLowerBound SCArray  {scLowerBound = (Just n)} = [("minItems",  number n)]
jsLowerBound _ = []

jsUpperBound :: Schema -> [(A.Key,A.Value)]
jsUpperBound SCString {scUpperBound = (Just n)} = [("maxLength", number n)]
jsUpperBound SCNumber {scUpperBound = (Just n)} = [("maximum",   number n)]
jsUpperBound SCArray  {scUpperBound = (Just n)} = [("maxItems",  number n)]
jsUpperBound _ = []

jsValue :: Schema -> [(A.Key,A.Value)]
jsValue SCConst {scValue = v} = [("const", v)] ++ [tyFromValue v]
jsValue _ = []

tyFromValue :: A.Value -> (A.Key, A.Value)
tyFromValue v =
  let ty = case v of
        A.String{} -> "string"
        A.Array{}  -> "array"
        A.Object{} -> "object"
        A.Bool{}   -> "boolean"
        A.Null     -> "null"
        A.Number{} -> "number"
  in ("type", ty)

jsItems :: A.Options -> Schema -> [(A.Key,A.Value)]
jsItems opts SCArray {scItems = itemsSchema} = [("items", convert' True opts $ itemsSchema)]
jsItems _ _ = []

jsProperties :: A.Options -> Schema -> [(A.Key,A.Value)]
jsProperties opts SCObject {scProperties = p} = [("properties", object $ toMap opts p)]
jsProperties _ _ = []

jsPatternProps :: A.Options -> Schema -> [(A.Key,A.Value)]
jsPatternProps _    SCObject {scPatternProps = []} = []
jsPatternProps opts SCObject {scPatternProps = p } = [("patternProperties", object $ toMap opts p)]
jsPatternProps _ _ = []

jsOneOf :: A.Options -> Schema -> [(A.Key,A.Value)]
jsOneOf opts SCOneOf {scChoices = cs, scNullable = nullable} = choices opts nullable cs
jsOneOf _ _ = []

jsRequired :: A.Options -> Schema -> [(A.Key,A.Value)]
jsRequired      opts@(A.omitNothingFields -> True) SCObject {scRequired = r  } =
  [("required", array $ map (fromString . A.fieldLabelModifier opts . T.unpack) r)]
jsRequired opts@(A.omitNothingFields -> _   ) SCObject {scProperties = p} =
  [("required", array . map fst $ toMap opts p)]
jsRequired _ _ = []

jsDefinitions :: A.Options -> Schema -> [(A.Key,A.Value)]
jsDefinitions _    SCSchema {scDefinitions = []} = []
jsDefinitions opts SCSchema {scDefinitions = d } = [("definitions", object $ toMap opts d)]
jsDefinitions _ _ = []

--------------------------------------------------------------------------------

array :: A.ToJSON a => [a] -> A.Value
array = A.Array . Vector.fromList . map A.toJSON

string :: Text -> A.Value
string = A.String

number :: Integer -> A.Value
number = A.Number . fromInteger

object :: [A.Pair] -> A.Value
object = A.object

false :: A.Value
false = A.Bool False

--------------------------------------------------------------------------------

choices :: A.Options -> Bool -> [SchemaChoice] -> [(A.Key,A.Value)]
choices opts nullable cs
    | isEnum && nullable = [ ("oneOf", array $ [object [("enum", array $ map consAsEnum cs)], object [("type", string "null")]]) ]
    | isEnum             = [ ("enum", array $ map consAsEnum cs) ]
    | isUnit && nullable = [ ("type", array $ map string ["object", "null"])
                           , ("properties", head $ map (conAsObject opts) cs)
                           ]
    | isUnit             = [ ("type", array $ map string ["object"])
                           , ("properties", head $ map (conAsObject opts) cs)
                           ]
    | nullable           = [ ("oneOf", array $ object [("type", string "null")] : map (conAsObject opts) cs) ]
    | otherwise          = [ ("oneOf", array $ map (conAsObject opts) cs) ]
  where
    isEnum = A.allNullaryToStringTag opts && all enumerable cs
    isUnit = length cs == 1

enumerable :: SchemaChoice -> Bool
enumerable (SCChoiceEnum _ _) = True
enumerable _                  = False

consAsEnum :: SchemaChoice -> Text
consAsEnum (SCChoiceEnum tag _) = tag
consAsEnum s = error ("conAsEnum could not handle: " ++ show s)

conAsObject :: A.Options -> SchemaChoice -> A.Value
conAsObject opts@(A.sumEncoding -> A.TwoElemArray) sc =
    object $
        if sctTitle sc == "" then [] else [ ("title", string $ sctTitle sc) ]
        `mappend`
        [ ("type", "array")
        , ("items" , conAsObject' opts sc)
        , ("minItems", number 2)
        , ("maxItems", number 2)
        , ("additionalItems", false)
        ]
conAsObject opts sc =
    object $
        if sctTitle sc == "" then [] else [ ("title", string $ sctTitle sc) ]
        `mappend`
        [ ("type", "object")
        , ("properties", conAsObject' opts sc)
        , ("additionalProperties", false)
        ]

conAsObject' :: A.Options -> SchemaChoice -> A.Value
conAsObject' opts@(A.sumEncoding -> A.TaggedObject tFld cFld) sc =
  conAsTag   opts (fromString $ A.constructorTagModifier opts tFld) (fromString cFld) sc
conAsObject' opts@(A.sumEncoding -> A.TwoElemArray          ) sc =
  conAsArray opts sc
conAsObject' opts@(A.sumEncoding -> A.ObjectWithSingleField ) sc =
  conAsMap   opts sc
conAsObject' _opts {- @(A.sumEncoding -> A.UntaggedValue) -} _sc = error "Unsupported option"

conAsTag :: A.Options -> A.Key -> A.Key ->  SchemaChoice -> A.Value
conAsTag opts tFld cFld (SCChoiceEnum  tag _)      =
  object [(tFld, object [("enum", array [tag])]), (cFld, conToArray opts [])]
conAsTag opts tFld cFld (SCChoiceArray tag _ ar)   =
  object [(tFld, object [("enum", array [tag])]), (cFld, conToArray opts ar)]
conAsTag opts tFld _    (SCChoiceMap   tag _ mp _) =
  object ((tFld, object [("enum", array [tag])]) : toMap opts mp)

keyFieldModifier :: A.Options -> A.Key -> A.Key
keyFieldModifier opts = fromString . A.fieldLabelModifier opts . T.unpack . K.toText

conAsArray :: A.Options -> SchemaChoice -> A.Value
conAsArray opts (SCChoiceEnum  tag _)       = array [object [("enum", array [tag])], conToArray  opts []]
conAsArray opts (SCChoiceArray tag _ ar)    = array [object [("enum", array [tag])], conToArray  opts ar]
conAsArray opts (SCChoiceMap   tag _ mp rq) = array [object [("enum", array [tag])], conToObject opts mp rq]

conAsMap :: A.Options -> SchemaChoice -> A.Value
conAsMap opts (SCChoiceEnum  tag _)       = object [(K.fromText tag, conToArray  opts [])]
conAsMap opts (SCChoiceArray tag _ ar)    = object [(K.fromText tag, conToArray  opts ar)]
conAsMap opts (SCChoiceMap   tag _ mp rq) = object [(K.fromText tag, conToObject opts mp rq)]

conToArray :: A.Options -> [Schema] -> A.Value
conToArray opts ar = object
    [ ("type", "array")
    , ("items", array . map (convert' True opts) $ ar)
    , ("minItems", A.toJSON $ length ar)
    , ("maxItems", A.toJSON $ length ar)
    , ("additionalItems", false)
    ]

conToObject :: A.Options -> [(Text,Schema)] -> [Text] -> A.Value
conToObject opts mp rq = object
    [ ("type", "object")
    , ("properties", object $ toMap opts mp)
    , ("required", required opts)
    , ("additionalProperties", false)
    ]
  where
    required (A.omitNothingFields -> True) = array rq
    required (A.omitNothingFields -> _   ) = array . map fst $ toMap opts mp

toMap :: A.Options -> [(Text,Schema)] -> [(A.Key,A.Value)]
toMap opts mp = map (\(n,v) -> (keyFieldModifier opts $ K.fromText n,convert' False opts v)) mp

