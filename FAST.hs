-- |A FAST protocoll implementation.
module FAST where

import qualified Data.ByteString as B
import Data.Ix (inRange)

-- *Ranges of integer types

-- |int32 range.
i32Range::(Int,Int)
i32Range = (-2147483648, 2147483647)

-- |uint32 range.
ui32Range::(Int,Int)
ui32Range = (0, 4294967295)

-- |int64 range.
i64Range::(Int,Int)
i64Range = (-9223372036854775808, 9223372036854775807)

-- |uint64 range.
ui64Range::(Int,Int)
ui64Range = (0,18446744073709551615)

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
                    |Decimal
                    |Bytevector B.ByteString

-- |The default namespace.
td::Namespace
td = "http://www.fixprotocol.org/ns/fast/td/1.1"

-- |The local namespace.
local::Namespace
local = ""

-- |A collection of templates, i.e. a template file.
data Templates = Templates {
    ts_ns           ::Maybe NsAttr,
    ts_templateNs   ::Maybe TemplateNsAttr,
    ts_dictionary   ::Maybe DictionaryAttr,
    ts_templates    ::[Template]
    }

-- |FAST template.
data Template = Template {
    t_fname         ::TemplateNsName,
    t_ns            ::Maybe NsAttr,
    t_dictionary    ::Maybe DictionaryAttr,
    t_typeRef       ::Maybe TypeRef,
    t_instructions  ::[Instruction]
    }

-- |A typeRef element of a template.
data TypeRef = TypeRef {
    trf_name    ::NameAttr,
    trf_ns      ::Maybe NsAttr 
    }

-- |An Instruction in a template is either a field instruction or a template reference.
data Instruction = Instruction Field
                    |TemplateReference (Maybe TemplateReferenceContent)

-- |This is a helper data structure, NOT defined in the reference.
data TemplateReferenceContent = TemplateReferenceContent {
        trc_name        ::NameAttr,
        trc_templateNs  ::Maybe TemplateNsAttr 
        }

-- |The instruction context. This is NOT directly a data construct mentioned 
-- in the FAST reference, but probably helpful.
data InstructionContext = InstructionContext {
    templates       ::Templates,
    template        ::Template,
    appTypes        ::[TypeRef],
    appType         ::TypeRef,
    dictionaries    ::[Dictionary],
    ic_initialValue ::Primitive
    }

-- |Field Instruction content.
data FieldInstrContent = FieldInstrContent {
    fic_fname       ::NsName,
    fic_presence    ::Maybe PresenceAttr,
    fieldOp         ::Maybe FieldOp
    }

-- |FAST field instructions.
data Field = IntField IntegerField
                |DecField DecimalField
                |AsciiStrField AsciiStringField
                |UnicodeStrField UnicodeStringField
                |ByteVecField ByteVectorField
                |Seq Sequence
                |Grp Group

-- |Integer Fields.
data IntegerField = Int32Field FieldInstrContent
                    |UInt32Field FieldInstrContent
                    |Int64Field FieldInstrContent
                    |UInt64Field FieldInstrContent

-- |Decimal Field.
data DecimalField = DecimalField {
        dfi_fname           ::NsName, 
        dfi_presence        ::Maybe PresenceAttr,
        dfi_fieldOp         ::Either FieldOp DecFieldOp
        }

-- |Ascii string field.
data AsciiStringField = AsciiStringField FieldInstrContent 

-- |Unicode string field.
data UnicodeStringField = UnicodeStringField {
        usf_content         ::FieldInstrContent, 
        usf_length          ::Maybe ByteVectorLength
        }

-- |Bytevector field.
data ByteVectorField = ByteVectorField {
        bvf_content         ::FieldInstrContent, 
        bvf_length          ::Maybe ByteVectorLength
        } 

-- |Sequence field.
data Sequence = Sequence {
        s_fname             ::NsName,
        s_presence          ::Maybe PresenceAttr,
        s_dictionary        ::Maybe DictionaryAttr,
        s_typeRef           ::Maybe TypeRef,
        s_length            ::Maybe Length,
        s_instructions      ::[Instruction]
        }

-- |Group field.
data Group = Group {
        g_fname             ::NsName,
        g_presence          ::Maybe PresenceAttr,
        g_dictionary        ::Maybe DictionaryAttr,
        g_typeRef           ::Maybe TypeRef,
        g_instructions      ::[Instruction]
        }

-- |ByteVectorLenght is logically a uInt32, but it is not a field instruction 
-- and it is not physically present in the stream. Obviously no field operator 
-- is needed.
data ByteVectorLength = ByteVectorLength {
    bvl_nsName::String
    }

-- |SeqLength is logically a uInt32. The name maybe 'implicit' or 'explicit' 
-- in the template.
-- implicit: the name is generated and is unique to the name of the sequence 
-- field.
-- explicit: the name is explicitly given in the template.
-- If the length field is not present in the template, the length field has an 
-- implicit name and the length of the sequence is not present in the stream 
-- and therefore the length field neither contains a field operator.
data Length = Length {
    sl_fname    ::Maybe NsName,
    sl_fieldOp  ::Maybe FieldOp
    }


-- |Presence of a field value is either mandatory or optional.
data PresenceAttr = Mandatory | Optional

-- |FAST field operators.
data FieldOp = Constant InitialValueAttr
                |Default (Maybe InitialValueAttr)
                |Copy OpContext
                |Increment OpContext
                |Delta OpContext
                |Tail OpContext
 
-- |The decimal field operator consists of two standart operators.
data DecFieldOp = DecFieldOp {
    exponent    ::FieldOp,
    mantissa    ::FieldOp
    }

-- |Dictionary consists of a name and a list of key value pairs.
data Dictionary = Dictionary String [(String, DictValue)]

-- |Entry in a dictionary can be in one of three states.
data DictValue = Undefined 
                    | Empty 
                    | Assigned Primitive

-- |Operator context.
data OpContext = OpContext {
    dictionary      ::Maybe DictionaryAttr,
    nsKey           ::Maybe NsKey,
    initialValue    ::Maybe InitialValueAttr
    }

-- |Dictionary attribute. Three predefined dictionaries are "template", "type" 
-- and "global".
data DictionaryAttr = TemplateDict
                        |TypeDict
                        |GlobalDict
                        |UserDict String

-- |nsKey attribute.
data NsKey = NsKey {
    key     ::KeyAttr,
    ns      ::Maybe NsAttr
    }

-- |Key attribute.
data KeyAttr = KeyAttr {
    token::Token
    }

-- |Initial value attribute. The value is a string of unicode characters and needs to 
-- be converted to the type of the field in question.
data InitialValueAttr = InitialValueAttr {
    text::UnicodeString
    }

-- |A full name in a template is given by a namespace URI and localname. For 
-- application types, fields and operator keys the namespace URI is given by 
-- the 'ns' attribute. For templates the namespace URI is given by the 
-- 'templateNs' attribute.
-- Note that full name constructors in the data structures are named 'fname'.
-- QUESTION: What is the 'idAttribute' for?

-- |A full name for an application type, field or operator key.
data NsName = NsName NameAttr (Maybe NsAttr) (Maybe IdAttr)

-- |A full name for a template.
data TemplateNsName = TemplateNsName NameAttr (Maybe TemplateNsAttr) (Maybe IdAttr)

-- |The very basic name related attributes.
newtype NameAttr = NameAttr String
newtype NsAttr = NsAttr String
newtype TemplateNsAttr = TemplateNsAttr String
newtype IdAttr = IdAttr Token
newtype Token = Token String

-- *FAST maps.

-- |Reset all entries of a dictionary to 'Undefined'.
reset::Dictionary -> Dictionary
reset (Dictionary name xs) = Dictionary name (map h xs)
    where h (key, value) = (key, Undefined)

-- |Delta operation.
delta::Primitive -> Primitive -> Primitive
-- delta for integer types is just addition.
delta (Int32 b) (Int32 p) = Int32 (checkRange i32Range (b + p))
delta (Int64 b) (Int64 p) = Int64 (checkRange i64Range (b + p))
delta (UInt32 b) (UInt32 p) = UInt32 (checkRange ui32Range (b + p))
delta (UInt64 b) (UInt64 p) = UInt64 (checkRange ui64Range (b + p))
delta _ _ = undefined

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
dfbBytevector::Primitive
dfbBytevector = Bytevector B.empty

-- |Increment an integer in an increment operator.
inc::Primitive -> Primitive
inc (Int32 i) | i == (snd i32Range) = Int32 $ fst i32Range
inc (Int32 i) = Int32(i + 1)
inc (Int64 i) | i == (snd i64Range) = Int64 $ fst i64Range 
inc (Int64 i) = Int64(i + 1)
inc (UInt32 i) | i == (snd i32Range) = UInt32 $ fst ui32Range
inc (UInt32 i) = UInt32(i + 1)
inc (UInt64 i) | i == (snd ui64Range) = UInt64 $ fst ui64Range
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
ivToInt64= undefined 

-- |Convert an initial value to an UInt64
ivToUInt64::InitialValueAttr -> Primitive
ivToUInt64= undefined 

-- *Helper functions

-- |Check wether a value is in a given range.
checkRange::(Int,Int) -> Int -> Int
checkRange r x = case (inRange r x) of
                    True -> x
                    False -> error "R4: Integer type can not be represented in the target integer type."
