-- |
-- Module      :  Codec.Fast.Data
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-#LANGUAGE GADTs, MultiParamTypeClasses, ExistentialQuantification, TypeFamilies #-}

module Codec.Fast.Data 
(
TypeWitness (..),
Value (..),
Primitive (..),
Templates (..),
Template (..),
TypeRef (..),
Instruction (..),
TemplateReferenceContent (..),
FieldInstrContent (..),
Field (..),
IntegerField (..),
DecimalField (..),
AsciiStringField (..),
UnicodeStringField (..),
ByteVectorField (..),
ByteVectorLength (..),
Sequence (..),
Group (..),
Length (..),
PresenceAttr (..),
FieldOp (..),
DecFieldOp (..),
Dictionary (..),
DictKey (..),
DictValue (..),
OpContext (..),
DictionaryAttr (..),
NsKey (..),
KeyAttr (..),
InitialValueAttr (..),
NsName (..),
TemplateNsName (..),
NameAttr (..),
NsAttr (..),
TemplateNsAttr (..),
IdAttr (..),
Token (..),
UnicodeString,
AsciiString,
Decimal
)

where

import Prelude hiding (exponent)
import qualified Data.ByteString as B
import Data.Int
import Data.Word
import qualified Data.Map as M
import Data.Attoparsec (Parser) 

-- | We need type witnesses to handle manipulation of dictionaries with entries of all possible 
-- primitive types in generic code.
data TypeWitness a where 
    TypeWitnessI32   :: Int32         -> TypeWitness Int32
    TypeWitnessW32   :: Word32        -> TypeWitness Word32
    TypeWitnessI64   :: Int64         -> TypeWitness Int64
    TypeWitnessW64   :: Word64        -> TypeWitness Word64
    TypeWitnessASCII :: AsciiString   -> TypeWitness AsciiString
    TypeWitnessUNI   :: UnicodeString -> TypeWitness UnicodeString
    TypeWitnessBS    :: B.ByteString  -> TypeWitness B.ByteString
    TypeWitnessDec   :: Decimal       -> TypeWitness Decimal

-- | Primitive type class.
class Primitive a where
    data Delta a       :: *
    witnessType        :: a -> TypeWitness a
    assertType         :: (Primitive b) => TypeWitness b -> a
    toValue            :: a -> Value
    defaultBaseValue   :: a
    ivToPrimitive      :: InitialValueAttr -> a
    delta              :: a -> Delta a -> a
    ftail              :: a -> a -> a
    readP              :: Parser a
    readD              :: Parser (Delta a)
    readT              :: Parser a

    readT = readP

-- |The values in a messages.
data Value = I32 Int32
           | UI32 Word32
           | I64 Int64
           | UI64 Word64
           | Dec Double
           | A  AsciiString
           | U  UnicodeString
           | B  B.ByteString
           | Sq Word32 [[(NsName, Maybe Value)]]
           | Gr [(NsName, Maybe Value)]
           deriving (Show)

-- |Some basic types, renamed for readability.
type UnicodeString = String
type AsciiString = String
type Decimal = (Int32, Int64)

--
-- The following definitions follow allmost one to one the FAST specification.
--

-- |A collection of templates, i.e. a template file.
data Templates = Templates {
    tsNs              :: Maybe NsAttr,
    tsTemplateNs      :: Maybe TemplateNsAttr,
    tsDictionary      :: Maybe DictionaryAttr,
    tsTemplates       :: [Template]
    } deriving (Show)

-- |FAST template.
data Template = Template {
    tName         :: TemplateNsName,
    tNs           :: Maybe NsAttr,
    tDictionary   :: Maybe DictionaryAttr,
    tTypeRef      :: Maybe TypeRef,
    tInstructions :: [Instruction]
    } deriving (Show)

-- |A typeRef element of a template.
data TypeRef = TypeRef {
    trName :: NameAttr,
    trNs   :: Maybe NsAttr
    } deriving (Show)

-- |An Instruction in a template is either a field instruction or a template reference.
data Instruction = Instruction Field
                    |TemplateReference (Maybe TemplateReferenceContent)
                    deriving (Show)

-- |This is a helper data structure, NOT defined in the reference.
data TemplateReferenceContent = TemplateReferenceContent {
        trcName       :: NameAttr,
        trcTemplateNs :: Maybe TemplateNsAttr
        } deriving (Show)

-- |Field Instruction content.
data FieldInstrContent = FieldInstrContent {
    ficFName    :: NsName,
    ficPresence :: Maybe PresenceAttr,
    ficFieldOp  :: Maybe FieldOp
    } deriving (Show)

-- |FAST field instructions.
data Field = IntField IntegerField
           | DecField DecimalField
           | AsciiStrField AsciiStringField
           | UnicodeStrField UnicodeStringField
           | ByteVecField ByteVectorField
           | Seq Sequence
           | Grp Group
			deriving (Show)

-- |Integer Fields.
data IntegerField = Int32Field FieldInstrContent
                    |UInt32Field FieldInstrContent
                    |Int64Field FieldInstrContent
                    |UInt64Field FieldInstrContent
                    deriving (Show)

-- |Decimal Field.
data DecimalField = DecimalField {
        dfiFName    :: NsName,
        dfiPresence :: Maybe PresenceAttr,
        dfiFieldOp  :: Either FieldOp DecFieldOp
        } deriving (Show)

-- |Ascii string field.
data AsciiStringField = AsciiStringField FieldInstrContent deriving (Show)

-- |Unicode string field.
data UnicodeStringField = UnicodeStringField {
        usfContent :: FieldInstrContent,
        usfLength  :: Maybe ByteVectorLength
        } deriving (Show)

-- |Bytevector field.
data ByteVectorField = ByteVectorField {
        bvfContent :: FieldInstrContent,
        bvfLength  :: Maybe ByteVectorLength
        } deriving (Show)

-- |Sequence field.
data Sequence = Sequence {
        sFName        :: NsName,
        sPresence     :: Maybe PresenceAttr,
        sDictionary   :: Maybe DictionaryAttr,
        sTypeRef      :: Maybe TypeRef,
        sLength       :: Maybe Length,
        sInstructions :: [Instruction]
        } deriving (Show)

-- |Group field.
data Group = Group {
        gFName        :: NsName,
        gPresence     :: Maybe PresenceAttr,
        gDictionary   :: Maybe DictionaryAttr,
        gTypeRef      :: Maybe TypeRef,
        gInstructions :: [Instruction]
        } deriving (Show)

-- |ByteVectorLenght is logically a uInt32, but it is not a field instruction 
-- and it is not physically present in the stream. Obviously no field operator 
-- is needed.
data ByteVectorLength = ByteVectorLength {
    bvlNsName::NsName
    } deriving (Show)

-- |SeqLength is logically a uInt32. The name maybe 'implicit' or 'explicit' 
-- in the template.
-- implicit: the name is generated and is unique to the name of the sequence 
-- field.
-- explicit: the name is explicitly given in the template.
-- If the length field is not present in the template, the length field has an 
-- implicit name and the length of the sequence is not present in the stream 
-- and therefore the length field neither contains a field operator.
data Length = Length {
    lFName   :: Maybe NsName,
    lFieldOp :: Maybe FieldOp
    } deriving (Show)


-- |Presence of a field value is either mandatory or optional.
data PresenceAttr = Mandatory | Optional deriving (Show)

-- |FAST field operators.
data FieldOp = Constant InitialValueAttr
             | Default (Maybe InitialValueAttr)
             | Copy OpContext
             | Increment OpContext
             | Delta OpContext
             | Tail OpContext
				deriving (Show)
 
-- |The decimal field operator consists of two standart operators.
data DecFieldOp = DecFieldOp {
    dfoExponent :: Maybe FieldOp,
    dfoMantissa :: Maybe FieldOp
    } deriving (Show)

-- |Dictionary consists of a name and a list of key value pairs.
data Dictionary = Dictionary String (M.Map DictKey DictValue) 

instance Show Dictionary where
    show _ = ""

data DictKey = N NsName
             | K NsKey
				deriving (Eq, Ord, Show)


-- |Entry in a dictionary can be in one of three states.
data DictValue = Undefined
               | Empty
               | forall a. Primitive a => Assigned (TypeWitness a)

-- |Operator context.
data OpContext = OpContext {
    ocDictionary   :: Maybe DictionaryAttr,
    ocNsKey        :: Maybe NsKey,
    ocInitialValue :: Maybe InitialValueAttr
    } deriving (Show)

-- |Dictionary attribute. Three predefined dictionaries are "template", "type" 
-- and "global".
data DictionaryAttr = DictionaryAttr String deriving (Show)

-- |nsKey attribute.
data NsKey = NsKey {
    nkKey :: KeyAttr,
    nkNs  :: Maybe NsAttr
    } deriving (Eq, Ord, Show)

-- |Key attribute.
data KeyAttr = KeyAttr {
    kaToken :: Token
    } deriving (Eq, Ord, Show)

-- |Initial value attribute. The value is a string of unicode characters and needs to 
-- be converted to the type of the field in question.
data InitialValueAttr = InitialValueAttr {
    text :: UnicodeString
    } deriving (Show)

-- |A full name in a template is given by a namespace URI and localname. For 
-- application types, fields and operator keys the namespace URI is given by 
-- the 'ns' attribute. For templates the namespace URI is given by the 
-- 'templateNs' attribute.
-- Note that full name constructors in the data structures are named 'fname'.
-- QUESTION: What is the 'idAttribute' for?

-- |A full name for an application type, field or operator key.
data NsName = NsName NameAttr (Maybe NsAttr) (Maybe IdAttr) deriving (Eq, Ord, Show)

-- |A full name for a template.
data TemplateNsName = TemplateNsName NameAttr (Maybe TemplateNsAttr) (Maybe IdAttr) deriving (Show)

-- |The very basic name related attributes.
newtype NameAttr = NameAttr String deriving (Eq, Ord, Show)
newtype NsAttr = NsAttr String deriving (Eq, Ord, Show)
newtype TemplateNsAttr = TemplateNsAttr String deriving (Eq, Ord, Show)
newtype IdAttr = IdAttr Token deriving (Eq, Ord, Show)
newtype Token = Token String deriving (Eq, Ord, Show)
