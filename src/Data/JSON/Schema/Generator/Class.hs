{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.JSON.Schema.Generator.Class where

import Data.JSON.Schema.Generator.Types (Schema)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy (Proxy)
import Data.Typeable (TypeRep)
import GHC.Generics (Generic(from), Rep)

--------------------------------------------------------------------------------

class JSONSchemaGen a where
    toSchema :: Options -> Proxy a -> Schema
    
    default toSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
    toSchema = genericToSchema

class JSONSchemaPrim a where
    toSchemaPrim :: Options -> Proxy a -> Schema

--------------------------------------------------------------------------------

class GJSONSchemaGen f where
    gToSchema :: Options -> Proxy (f a) -> Schema

genericToSchema :: (Generic a, GJSONSchemaGen (Rep a)) => Options -> Proxy a -> Schema
genericToSchema opts = gToSchema opts . fmap from

--------------------------------------------------------------------------------

-- | Options that specify how to generate schema definition automatically
-- from your datatype.
--
data Options = Options
    { baseUri :: String -- ^ schema id prefix.
    , schemaIdSuffix :: String -- ^ schema id suffix. File extension for example.
    , typeRefMap     :: Map TypeRep String -- ^ a mapping from datatypes to referenced schema ids.
    , fieldTypeMap   :: Map String FieldType -- ^ a mapping to assign a preffered type to a field.
    , fieldSchemaMap :: Map String Schema -- ^ a mapping to assign a preffered type to a schema.
    }

data FieldType = forall a. (JSONSchemaPrim a) => FieldType (Proxy a)

instance Show Options where
    showsPrec p opts =
        showParen (p > 10)
        $ showString "Options { baseUri = "
        . showsPrec 11 (baseUri opts)
        . showString ", schemaIdSuffix = "
        . showsPrec 11 (schemaIdSuffix opts)
        . showString ", typeRefMap = "
        . showsPrec 11 (typeRefMap opts)
        . showString ", fieldTypeMap = {..} }"
        . showString ", fieldSchemaMap = {..} }"

-- | Default geerating 'Options':
--
-- @
-- 'Options'
-- { 'baseUri'        = ""
-- , 'schemaIdSuffix' = ""
-- , 'refSchemaMap'   = Map.empty
-- }
-- @
--
defaultOptions :: Options
defaultOptions = Options
    { baseUri = ""
    , schemaIdSuffix = ""
    , typeRefMap = Map.empty
    , fieldTypeMap = Map.empty
    , fieldSchemaMap = Map.empty
    }

