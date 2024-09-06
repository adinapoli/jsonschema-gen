module Data.JSON.Schema.Generator.Types
    ( Schema (..)
    , SchemaChoice (..)
    , scString
    , scInteger
    , scNumber
    , scBoolean
    , setNullable
    , getNullable
    ) where

import Data.Text (Text)
import qualified Data.Aeson as JSON

--------------------------------------------------------------------------------

-- | A schema for a JSON value.
--
data Schema =
      SCSchema
        { scId           :: !Text
        , scUsedSchema   :: !Text
        , scSchemaType   :: !Schema
        , scDefinitions  :: ![(Text, Schema)]
        }
    | SCString
        { scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scFormat       :: !(Maybe Text)
        , scLowerBound   :: !(Maybe Integer)
        , scUpperBound   :: !(Maybe Integer)
        }
    | SCInteger
        { scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scLowerBound   :: !(Maybe Integer)
        , scUpperBound   :: !(Maybe Integer)
        }
    | SCNumber
        { scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scLowerBound   :: !(Maybe Integer)
        , scUpperBound   :: !(Maybe Integer)
        }
    | SCBoolean
        { scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        }
    | SCConst
        { scTitle        :: !Text
        , scDescription  :: !(Maybe Text)
        , scValue        :: !JSON.Value
        }
    | SCObject
        { scTitle        :: !Text
        , scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scProperties   :: ![(Text, Schema)]
        -- | According to the 2020-12 draft:
        -- The additionalProperties keyword is used to control the handling of extra stuff, that is,
        -- properties whose names are not listed in the properties keyword or match any of the regular
        -- expressions in the patternProperties keyword. By default any additional properties are allowed.
        , scAdditionalProperties :: !(Either Bool Schema)
        , scPatternProps :: ![(Text, Schema)]
        , scRequired     :: ![Text]
        }
    | SCArray
        { scTitle        :: !Text
        , scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scItems        :: !Schema
        , scLowerBound   :: !(Maybe Integer)
        , scUpperBound   :: !(Maybe Integer)
        }
    | SCOneOf
        { scTitle        :: !Text
        , scDescription  :: !(Maybe Text)
        , scNullable     :: !Bool
        , scChoices      :: ![SchemaChoice]
        }
    | SCRef
        { scReference    :: !Text
        , scNullable     :: !Bool
        }
    | SCNull
    deriving (Show, Eq)

-- | A sum encoding for ADT.
--
data SchemaChoice =
      SCChoiceEnum
        { sctName     :: !Text             --   constructor name.
        , sctTitle    :: !Text             --   an arbitrary text. e.g. Types.UnitType1.UnitData11.
        }
      -- ^ Encoding for constructors that are all unit type.
      -- e.g. "test": {"enum": ["xxx", "yyy", "zzz"]}
    | SCChoiceArray
        { sctName     :: !Text             --   constructor name.
        , sctTitle    :: !Text             --   an arbitrary text. e.g. Types.ProductType1.ProductData11.
        , sctArray    :: ![Schema]         --   parametes of constructor.
        }
      -- ^ Encoding for constructors that are non record type.
      -- e.g. "test": [{"tag": "xxx", "contents": []},...] or "test": [{"xxx": [],},...]
    | SCChoiceMap
        { sctName     :: !Text             --   constructor name.
        , sctTitle    :: !Text             --   an arbitrary text. e.g. Types.RecordType1.RecordData11.
        , sctMap      :: ![(Text, Schema)] --   list of record field name and schema in this constructor.
        , sctRequired :: ![Text]           --   required field names.
        }
      -- ^ Encoding for constructos that are record type.
      -- e.g. "test": [{"tag": "xxx", "contents": {"aaa": "yyy",...}},...] or "test": [{"xxx": []},...]
    deriving (Show, Eq)

-- ^ A smart consturctor for String.
--
scString :: Schema
scString = SCString
    { scDescription = Nothing
    , scNullable = False
    , scFormat = Nothing
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

-- ^ A smart consturctor for Integer.
--
scInteger :: Schema
scInteger = SCInteger
    { scDescription = Nothing
    , scNullable = False
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

-- ^ A smart consturctor for Number.
--
scNumber :: Schema
scNumber = SCNumber
    { scDescription = Nothing
    , scNullable = False
    , scLowerBound = Nothing
    , scUpperBound = Nothing
    }

-- ^ A smart consturctor for Boolean.
--
scBoolean :: Schema
scBoolean = SCBoolean
    { scDescription = Nothing
    , scNullable = False
    }

setNullable :: Bool -> Schema -> Schema
setNullable notRequired wrapped = case wrapped of
  SCSchema{}  -> wrapped
  SCString{}  -> wrapped { scNullable = notRequired }
  SCInteger{} -> wrapped { scNullable = notRequired }
  SCNumber{}  -> wrapped { scNullable = notRequired }
  SCBoolean{} -> wrapped { scNullable = notRequired }
  SCConst{}   -> wrapped
  SCObject{}  -> wrapped { scNullable = notRequired }
  SCArray{}   -> wrapped { scNullable = notRequired }
  SCOneOf{}   -> wrapped { scNullable = notRequired }
  SCRef{}     -> wrapped { scNullable = notRequired }
  SCNull      -> wrapped

getNullable :: Schema -> Bool
getNullable wrapped = case wrapped of
  SCSchema{}  -> False
  SCString{}  -> scNullable wrapped
  SCInteger{} -> scNullable wrapped
  SCNumber{}  -> scNullable wrapped
  SCBoolean{} -> scNullable wrapped
  SCConst{}   -> False
  SCObject{}  -> scNullable wrapped
  SCArray{}   -> scNullable wrapped
  SCOneOf{}   -> scNullable wrapped
  SCRef{}     -> scNullable wrapped
  SCNull      -> False
