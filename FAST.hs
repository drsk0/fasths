-- |A FAST protocoll implementation.
module FAST where

import Prelude hiding (exponent)
import qualified Data.ByteString as B
import Data.Ix (inRange)
import qualified Data.Map as M

-- *Ranges of integer types

-- |int32 range.
i32Range::(Int,Int)
i32Range = (minBound::Int, maxBound::Int)
--i32Range = (-2147483648, 2147483647)

-- |uint32 range.
ui32Range::(Int,Int)
ui32Range = (0, maxBound::Int)
-- ui32Range = (0, 4294967295)

-- |int64 range.
i64Range::(Int,Int)
i64Range = (minBound::Int, maxBound::Int)
-- i64Range = (-9223372036854775808, 9223372036854775807)

-- |uint64 range.
ui64Range::(Int,Int)
ui64Range = (0, maxBound::Int)
--ui64Range = (0,18446744073709551615)

decExpRange::(Int,Int)
decExpRange = (-63,63)

-- |Some basic types, renamed for readability.
type UnicodeString = String
type AsciiString = String
type Namespace = String

--
-- The following definitions follow allmost one to one the FAST specification.
--

-- |FAST primitive types and their corresponding Haskell types.
data Primitive = Int32 Int
                    |UInt32 Int
                    |Int64 Int
                    |UInt64 Int
                    |Ascii AsciiString
                    |Unicode UnicodeString
                    |Decimal {exponent::Primitive, mantissa::Primitive}
                    |Bytevector B.ByteString
                    deriving (Show)

-- |The default namespace.
td::Namespace
td = "http://www.fixprotocol.org/ns/fast/td/1.1"

-- |The local namespace.
local::Namespace
local = ""

-- |A collection of templates, i.e. a template file.
data Templates = Templates {
    tsNs           ::Maybe NsAttr,
    tsTemplateNs   ::Maybe TemplateNsAttr,
    tsDictionary   ::Maybe DictionaryAttr,
    tsTemplates    ::[Template]
    } deriving (Show)

-- |FAST template.
data Template = Template {
    tName          ::TemplateNsName,
    tNs            ::Maybe NsAttr,
    tDictionary    ::Maybe DictionaryAttr,
    tTypeRef       ::Maybe TypeRef,
    tInstructions  ::[Instruction]
    } deriving (Show)

-- |A typeRef element of a template.
data TypeRef = TypeRef {
    trName    ::NameAttr,
    trNs      ::Maybe NsAttr 
    } deriving (Show)

-- |An Instruction in a template is either a field instruction or a template reference.
data Instruction = Instruction Field
                    |TemplateReference (Maybe TemplateReferenceContent)
                    deriving (Show)

-- |This is a helper data structure, NOT defined in the reference.
data TemplateReferenceContent = TemplateReferenceContent {
        trcName        ::NameAttr,
        trcTemplateNs  ::Maybe TemplateNsAttr 
        } deriving (Show)

-- |Field Instruction content.
data FieldInstrContent = FieldInstrContent {
    ficFName       ::NsName,
    ficPresence    ::Maybe PresenceAttr,
    ficFieldOp         ::Maybe FieldOp
    } deriving (Show)

-- |FAST field instructions.
data Field = IntField IntegerField
                |DecField DecimalField
                |AsciiStrField AsciiStringField
                |UnicodeStrField UnicodeStringField
                |ByteVecField ByteVectorField
                |Seq Sequence
                |Grp Group
                deriving (Show)

-- |Integer Fields.
data IntegerField = Int32Field FieldInstrContent
                    |UInt32Field FieldInstrContent
                    |Int64Field FieldInstrContent
                    |UInt64Field FieldInstrContent
                    deriving (Show)

-- |Decimal Field.
data DecimalField = DecimalField {
        dfiFName           ::NsName, 
        dfiPresence        ::Maybe PresenceAttr,
        dfiFieldOp         ::Either FieldOp DecFieldOp
        } deriving (Show)

-- |Ascii string field.
data AsciiStringField = AsciiStringField FieldInstrContent deriving (Show)

-- |Unicode string field.
data UnicodeStringField = UnicodeStringField {
        usfContent         ::FieldInstrContent, 
        usfLength          ::Maybe ByteVectorLength
        } deriving (Show)

-- |Bytevector field.
data ByteVectorField = ByteVectorField {
        bvfContent         ::FieldInstrContent, 
        bvfLength          ::Maybe ByteVectorLength
        } deriving (Show)

-- |Sequence field.
data Sequence = Sequence {
        sFName             ::NsName,
        sPresence          ::Maybe PresenceAttr,
        sDictionary        ::Maybe DictionaryAttr,
        sTypeRef           ::Maybe TypeRef,
        sLength            ::Maybe Length,
        sInstructions      ::[Instruction]
        } deriving (Show)

-- |Group field.
data Group = Group {
        gFName             ::NsName,
        gPresence          ::Maybe PresenceAttr,
        gDictionary        ::Maybe DictionaryAttr,
        gTypeRef           ::Maybe TypeRef,
        gInstructions      ::[Instruction]
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
    lFName    ::Maybe NsName,
    lFieldOp  ::Maybe FieldOp
    } deriving (Show)


-- |Presence of a field value is either mandatory or optional.
data PresenceAttr = Mandatory | Optional deriving (Show)

-- |FAST field operators.
data FieldOp = Constant InitialValueAttr
                |Default (Maybe InitialValueAttr)
                |Copy OpContext
                |Increment OpContext
                |Delta OpContext
                |Tail OpContext
                deriving (Show)
 
-- |The decimal field operator consists of two standart operators.
data DecFieldOp = DecFieldOp {
    dfoExponent    ::FieldOp,
    dfoMantissa    ::FieldOp
    } deriving (Show)

-- |Dictionary consists of a name and a list of key value pairs.
data Dictionary = Dictionary String (M.Map DictKey DictValue) deriving (Show)

data DictKey = N NsName
                |K NsKey
                deriving (Eq, Ord, Show)


-- |Entry in a dictionary can be in one of three states.
data DictValue = Undefined 
                    | Empty 
                    | Assigned Primitive
                    deriving (Show)

-- |Operator context.
data OpContext = OpContext {
    ocDictionary      ::Maybe DictionaryAttr,
    ocNsKey           ::Maybe NsKey,
    ocInitialValue    ::Maybe InitialValueAttr
    } deriving (Show)

-- |Dictionary attribute. Three predefined dictionaries are "template", "type" 
-- and "global".
data DictionaryAttr = DictionaryAttr String deriving (Show)

-- |nsKey attribute.
data NsKey = NsKey {
    nkKey     ::KeyAttr,
    nkNs    ::Maybe NsAttr
    } deriving (Eq, Ord, Show)

-- |Key attribute.
data KeyAttr = KeyAttr {
    kaToken::Token
    } deriving (Eq, Ord, Show)

-- |Initial value attribute. The value is a string of unicode characters and needs to 
-- be converted to the type of the field in question.
data InitialValueAttr = InitialValueAttr {
    text::UnicodeString
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

-- *FAST maps.

-- |Reset all entries of a dictionary to 'Undefined'.
reset::Dictionary -> Dictionary
reset (Dictionary name xs) = Dictionary name (M.map h xs)
    where h _ = Undefined

-- |Delta data. Not in the FAST specification.
data Delta = Int32Delta Primitive
            |UInt32Delta Primitive
            |Int64Delta Primitive
            |UInt64Delta Primitive
            |DecimalDelta Primitive
            |AsciiDelta Primitive Primitive
            |ByteVectorDelta Primitive Primitive

-- |Delta operation.
delta::Primitive -> Delta -> Primitive
-- delta for integer types is just addition.
delta (Int32 b) (Int32Delta (Int32 d)) = Int32 (checkRange i32Range (b + d))
delta (Int64 b) (Int64Delta (Int64 d)) = Int64 (checkRange i64Range (b + d))
delta (UInt32 b) (UInt32Delta (Int32 d)) = UInt32 (checkRange ui32Range (b + d))
delta (UInt64 b) (UInt64Delta (Int64 d)) = UInt64 (checkRange ui64Range (b + d))
-- delta for decimal type is addition of exponents and mantissas.
delta (Decimal (Int32 b) (Int64 b')) (DecimalDelta (Decimal (Int32 d) (Int64 d'))) 
    = Decimal (Int32 (checkRange decExpRange (b + d))) (Int64 (checkRange i64Range (b' + d')))
delta (Ascii str) (AsciiDelta (Int32 l) (Ascii str')) | l < 0 = Ascii (str' ++ str'') where str'' = drop (l + 1) str
delta (Ascii str) (AsciiDelta (Int32 l) (Ascii str')) | l >= 0 = Ascii (str'' ++ str') where str'' = take (length str - l) str
delta (Bytevector bv) (ByteVectorDelta (Int32 l) (Bytevector bv')) | l < 0 = Bytevector (bv'' `B.append` bv') where bv'' = B.drop (l + 1) bv
delta (Bytevector bv) (ByteVectorDelta (Int32 l) (Bytevector bv')) | l >= 0 = Bytevector (bv'' `B.append` bv') where bv'' = B.take (B.length bv - l) bv
delta _ _ = undefined

-- |Tail data. Not present in the FAT specification.
data Tail = AsciiTail Primitive
            |UnicodeTail Primitive
            |ByteVectorTail Primitive

-- |Tail operation.
tail::Primitive -> Tail -> Primitive
tail (Ascii str) (AsciiTail (Ascii str')) | length str' < length str = Ascii (take (length str - length str') str ++ str')
tail (Bytevector bv) (ByteVectorTail (Bytevector bv')) = Bytevector (B.take (B.length bv - B.length bv') bv `B.append` bv')
tail _ _ = undefined


-- |Default base value for Int32.
dfbInt32::Primitive
dfbInt32 = Int32 0

-- |Default base value for UInt32.
dfbUInt32::Primitive
dfbUInt32 = UInt32 0

-- |Default base value for Int64.
dfbInt64::Primitive
dfbInt64 = Int64 0

-- |Default base value for UInt64.
dfbUInt64::Primitive
dfbUInt64 = UInt64 0

-- |Default base value for ascii string.
dfbAscii::Primitive
dfbAscii = Ascii ""

-- |Default base value for unicode string.
dfbUnicode::Primitive
dfbUnicode = Unicode ""

-- |Default base value for Bytevector.
dfbByteVector::Primitive
dfbByteVector = Bytevector B.empty

-- |Default base value for Decimal.
dfbDecimal::Primitive
dfbDecimal = Decimal (Int32 0) (Int64 0)

-- |Increment an integer in an increment operator.
inc::Primitive -> Primitive
inc (Int32 i) | i == snd i32Range = Int32 $ fst i32Range
inc (Int32 i) = Int32(i + 1)
inc (Int64 i) | i == snd i64Range = Int64 $ fst i64Range 
inc (Int64 i) = Int64(i + 1)
inc (UInt32 i) | i == snd i32Range = UInt32 $ fst ui32Range
inc (UInt32 i) = UInt32(i + 1)
inc (UInt64 i) | i == snd ui64Range = UInt64 $ fst ui64Range
inc (UInt64 i) = UInt64(i + 1)
inc _ = error "S2: Impossible to increment an non-integer type field."

-- *Conversions from initial values.

-- |Convert an initial value to an Int32.
ivToInt32::InitialValueAttr -> Primitive
ivToInt32 = undefined 

-- |Convert an initial value to an UInt32
ivToUInt32::InitialValueAttr -> Primitive
ivToUInt32 = undefined 

-- |Convert an initial value to an Int64
ivToInt64::InitialValueAttr -> Primitive
ivToInt64 = undefined 

-- |Convert an initial value to an UInt64
ivToUInt64::InitialValueAttr -> Primitive
ivToUInt64 = undefined 

ivToDec::InitialValueAttr -> Primitive
ivToDec = undefined

ivToAscii::InitialValueAttr -> Primitive
ivToAscii = undefined

ivToByteVector::InitialValueAttr -> Primitive
ivToByteVector = undefined
-- *Helper functions

-- |Check wether a value is in a given range.
checkRange::(Int,Int) -> Int -> Int
checkRange r x = if inRange r x then x else 
    error "R4: Integer type can not be represented in the target integer type."
