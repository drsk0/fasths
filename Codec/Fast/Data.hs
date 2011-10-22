-- |
-- Module      :  Codec.Fast.Data
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-#LANGUAGE TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleInstances, GADTs, MultiParamTypeClasses, ExistentialQuantification, TypeFamilies, DeriveDataTypeable #-}

module Codec.Fast.Data 
(
TypeWitness (..),
Value (..),
Primitive (..),
Delta (..),
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
Decimal,
anySBEEntity,
FASTException (..)
)

where

import Prelude hiding (exponent, dropWhile)
import Data.ListLike (dropWhile, genericDrop, genericTake, genericLength)
import Data.Char (digitToInt)
import Data.Bits
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack, pack) 
import Data.Int
import Data.Word
import qualified Data.Map as M
import qualified Data.Attoparsec as A
import qualified Data.Binary.Put as P
import Control.Applicative 
import Control.Exception
import Data.Typeable

-- | FAST exception.
data FASTException = S1 String
                   | S2 String
                   | S3 String
                   | S4 String 
                   | S5 String
                   | D1 String
                   | D2 String 
                   | D3 String
                   | D4 String
                   | D5 String
                   | D6 String
                   | D7 String
                   | D8 String
                   | D9 String
                   | D10 String
                   | D11 String
                   | D12 String
                   | R1 String
                   | R2 String
                   | R3 String
                   | R4 String
                   | R5 String 
                   | R6 String
                   | R7 String
                   | R8 String
                   | R9 String
                   | OtherException String
                   deriving (Show, Typeable)

instance Exception FASTException

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

type Coparser = P.Put

-- | Primitive type class.
class Primitive a where
    data Delta a     :: *
    witnessType      :: a -> TypeWitness a
    assertType       :: (Primitive b) => TypeWitness b -> a
    toValue          :: a -> Value
    defaultBaseValue :: a
    ivToPrimitive    :: InitialValueAttr -> a
    delta            :: a -> Delta a -> a
    ftail            :: a -> a -> a
    decodeP          :: A.Parser a
    decodeD          :: A.Parser (Delta a)
    decodeT          :: A.Parser a
    encodeP          :: a -> Coparser
    encodeD          :: Delta a -> Coparser
    encodeT          :: a -> Coparser

    decodeT = decodeP
    encodeT = encodeP

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

instance Primitive Int32 where
    newtype Delta Int32 = Di32 Int32 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessI32
    assertType (TypeWitnessI32 i) = i
    assertType _ = throw $ D4 "Type mismatch."
    toValue = I32
    defaultBaseValue = 0 
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di32 i') = i + i'
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = int
    decodeD = Di32 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _int
    encodeD (Di32 i) = encodeP i

instance Primitive Word32 where
    newtype Delta Word32 = Dw32 Int32 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW32
    assertType (TypeWitnessW32 w) = w
    assertType _ = throw $ D4 "Type mismatch."
    toValue = UI32
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw32 i) = fromIntegral (fromIntegral w + i)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = uint
    decodeD = Dw32 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _uint
    encodeD (Dw32 w) = encodeP w

instance Primitive Int64 where
    newtype Delta Int64 = Di64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessI64
    assertType (TypeWitnessI64 i) = i
    assertType _ = throw $ D4 "Type mismatch."
    toValue = I64
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di64 i')= i + i'
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = int
    decodeD = Di64 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _int
    encodeD (Di64 i) = encodeP i

instance Primitive Word64 where
    newtype Delta Word64 = Dw64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW64
    assertType (TypeWitnessW64 w) = w
    assertType _ = throw $ D4 "Type mismatch."
    toValue = UI64
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw64 i) = fromIntegral (fromIntegral w + i)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
    decodeP = uint
    decodeD = Dw64 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _uint
    encodeD (Dw64 i) = encodeP i

instance Primitive AsciiString where
    newtype Delta AsciiString = Dascii (Int32, String)
    witnessType = TypeWitnessASCII
    assertType (TypeWitnessASCII s) = s
    assertType _ = throw $ D4 "Type mismatch."
    toValue = A
    defaultBaseValue = ""
    ivToPrimitive = text
    delta s1 (Dascii (l, s2)) | l < 0 = s2 ++ s1' where s1' = genericDrop (l + 1) s1
    delta s1 (Dascii (l, s2)) | l >= 0 = s1' ++ s2 where s1' = genericTake (genericLength s1 - l) s1
    delta _ _ = throw $ D4 "Type mismatch."
    ftail s1 s2 = take (length s1 - length s2) s1 ++ s2
    decodeP = asciiString
    decodeD = do 
                l <- int
                s <- decodeP
                return (Dascii (l, s))
    decodeT = decodeP
    encodeP = _asciiString
    encodeD (Dascii (i, s)) = encodeP i >> encodeP s

{-instance Primitive UnicodeString  where-}
    {-data TypeWitness UnicodeString = TypeWitnessUNI UnicodeString -}
    {-assertType (TypeWitnessUNI s) = s-}
    {-assertType _ = error "Type mismatch."-}
    {-type Delta UnicodeString = (Int32, B.ByteString)-}
    {-defaultBaseValue = ""-}
    {-ivToPrimitive = text-}
    {-delta s d =  B.unpack (delta (B.pack s) d)-}
    {-ftail s1 s2 = B.unpack (ftail (B.pack s1) s2)-}
    {-decodeP = B.unpack <$> byteVector-}
    {-decodeD = do-}
                {-l <- int-}
                {-bv <- decodeP-}
                {-return (l, bv)-}
    {-decodeT = decodeP-}

instance Primitive (Int32, Int64) where
    newtype Delta (Int32, Int64) = Ddec (Int32, Int64)
    witnessType = TypeWitnessDec
    assertType (TypeWitnessDec (e, m)) = (e, m)
    assertType _ = throw $ D4 "Type mismatch."
    toValue (e, m) = Dec (fromRational (toRational m * 10^^e))
    defaultBaseValue = (0, 0)
    ivToPrimitive (InitialValueAttr s) = let    s' = trimWhiteSpace s 
                                                mant = read (filter (/= '.') s')
                                                expo = h s'
                                                h ('-':xs) = h xs
                                                h ('.':xs) = -1 * toEnum (length (takeWhile (=='0') xs) + 1)
                                                h ('0':'.':xs) = h ('.':xs)
                                                h xs = toEnum (length (takeWhile (/= '.') xs))
                                         in (mant, expo)
    delta (e1, m1) (Ddec (e2, m2)) = (e1 + e2, m1 + m2)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = do 
        e <- int::A.Parser Int32
        m <- int::A.Parser Int64
        return (e, m)
    decodeD = Ddec <$> decodeP
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP (e, m) = encodeP e >> encodeP m
    encodeD (Ddec (e, m)) = encodeP e >> encodeP m

instance Primitive B.ByteString where
    newtype Delta B.ByteString = Dbs (Int32, B.ByteString)
    witnessType = TypeWitnessBS
    assertType (TypeWitnessBS bs) = bs
    assertType _ = throw $ D4 "Type mismatch."
    toValue = B 
    defaultBaseValue = B.empty
    ivToPrimitive iv = B.pack (map (toEnum . digitToInt) (filter whiteSpace (text iv)))
    delta bv (Dbs (l, bv')) | l < 0 = bv'' `B.append` bv' where bv'' = genericDrop (l + 1) bv 
    delta bv (Dbs (l, bv')) | l >= 0 = bv'' `B.append` bv' where bv'' = genericTake (genericLength bv - l) bv
    delta _ _ = throw $ D4 "Type mismatch."
    ftail b1 b2 = B.take (B.length b1 - B.length b2) b1 `B.append` b2
    decodeP = byteVector
    decodeD = do 
                l <- int
                bv <- decodeP
                return (Dbs (l, bv))
    encodeP = _byteVector
    encodeD (Dbs (i, bs)) = encodeP i >> encodeP bs

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
        dfiFieldOp  :: Maybe (Either FieldOp DecFieldOp)
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


-- |Get a Stopbit encoded entity.
anySBEEntity :: A.Parser B.ByteString
anySBEEntity = takeTill' stopBitSet

_anySBEEntity :: B.ByteString -> Coparser
_anySBEEntity bs = P.putByteString ((B.init bs) `B.append` (B.singleton (setBit (B.last bs) 7)))

-- |Like takeTill, but takes the matching byte as well.
takeTill' :: (Word8 -> Bool) -> A.Parser B.ByteString
takeTill' f = do
    str <- A.takeTill f
    c <- A.take 1
    return (str `B.append` c)

-- |Test wether the stop bit is set of a Char. (Note: Chars are converted to
-- Word8's. 
-- TODO: Is this unsafe?
stopBitSet :: Word8 -> Bool
stopBitSet c = testBit c 7

-- |Bytevector size preamble parser.
byteVector :: A.Parser B.ByteString
byteVector = do
    s <- uint::A.Parser Word32
    byteVector' s

-- |Bytevector field parser. The first argument is the size of the bytevector.
-- If the length of the bytevector is bigger than maxBound::Int an exception 
-- will be trown.
byteVector' :: Word32 -> A.Parser B.ByteString
byteVector' c = A.take (fromEnum c)

_byteVector :: B.ByteString -> Coparser
_byteVector bs = (_uint ((fromIntegral(B.length bs)) :: Word32)) >> (P.putByteString bs)

-- |Unsigned integer parser, doesn't check for bounds.
-- TODO: should we check for R6 errors, i.e overlong fields?
uint :: (Bits a) => A.Parser a
uint = do 
    bs <- anySBEEntity
    return (B.foldl h 0 bs)
    where   h::(Bits a, Num a) => a -> Word8 -> a
            h r w = fromIntegral (clearBit w 7) .|. shiftL r 7
        
_uint :: (Bits a, Eq a, Integral a) => a -> Coparser
_uint ui = _anySBEEntity (_uintBS ui)

_uintBS :: (Bits a, Eq a, Integral a) => a -> B.ByteString
_uintBS ui = if ui' /= 0 
            then (_uintBS ui') `B.snoc` (fromIntegral (ui .&. 127) :: Word8)
            else B.empty
            where ui' = shiftR ui 7

-- |Signed integer parser, doesn't check for bounds.
int :: (Bits a) => A.Parser a
int = do
    bs <- anySBEEntity
    return (if testBit (B.head bs) 6 
            then B.foldl h (shiftL (-1) 7 .|. fromIntegral (setBit (B.head bs) 7)) (B.tail bs)
            else B.foldl h 0 bs)
    where   
            h::(Bits a, Num a) => a -> Word8 -> a
            h r w = fromIntegral (clearBit w 7) .|. shiftL r 7

_int :: (Bits a, Ord a, Integral a) => a -> Coparser
_int i = if i < 0 
         then _anySBEEntity (_uintBS i)
         else _anySBEEntity ((setBit (B.head (_uintBS i)) 6) `B.cons` B.tail (_uintBS i))

-- |ASCII string field parser, non-Nullable.
asciiString :: A.Parser AsciiString
asciiString = do
    bs <- anySBEEntity
    let bs' = B.init bs `B.append` B.singleton (clearBit (B.last bs) 7) in
        return (unpack bs')

_asciiString :: AsciiString -> Coparser
_asciiString s = _anySBEEntity (pack s)

trimWhiteSpace :: String -> String
trimWhiteSpace = reverse . dropWhile whiteSpace . reverse . dropWhile whiteSpace

whiteSpace :: Char -> Bool
whiteSpace c =  c `elem` " \t\r\n"
