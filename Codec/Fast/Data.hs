-- |
-- Module      :  Codec.Fast.Data
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-#LANGUAGE FlexibleContexts, TypeSynonymInstances, GeneralizedNewtypeDeriving, FlexibleInstances, GADTs, MultiParamTypeClasses, ExistentialQuantification, TypeFamilies, DeriveDataTypeable #-}

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
FASTException (..),
Context (..),
tname2fname,
fname2tname,
_anySBEEntity,
Coparser,
DualType,
contramap,
sequenceD,
prevValue,
updatePrevValue,
setPMap,
uniqueFName,
needsSegment,
needsPm,
tempRefCont2TempNsName,
pmToBs,
bsToPm,
assertNameIs,
initState,
rmPreamble,
rmPreamble',
addPreamble,
addPreamble',
-- testing properties.
prop_decodeP_dot_encodeP_is_ID,
prop_decodeD_dot_encodeD_is_ID,
prop_decodeT_dot_encodeT_is_ID,
prop_fromValue_dot_toValue_is_ID,
prop_delta_dot_delta__is_ID,
prop_ftail_dot_ftail__is_ID,
prop_rmPreamble_dot_addPreamble_is_ID,
)

where

import Prelude hiding (exponent, dropWhile, reverse, zip)
import Data.ListLike (ListLike, dropWhile, genericDrop, genericTake, genericLength, reverse, zip)
import Data.Char (digitToInt)
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import Data.Bits
import Numeric
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Char8 (unpack, pack) 
import Data.Int
import Data.Word
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Attoparsec as A
import qualified Data.Binary.Builder as BU
import Control.Applicative 
import Control.Monad.State
import Control.Exception
import Data.Typeable
import Test.QuickCheck hiding ((.&.))

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
                   | EncoderException String
                   | OtherException String
                   deriving (Show, Typeable)

instance Exception FASTException

-- |State of the (co)parser.
data Context = Context {
    -- |Presence map
    pm   :: [Bool],
    -- |Dictionaries.
    dict :: M.Map String Dictionary
    }

-- |The initial state of the parser depending on the templates.
initState::Templates -> Context
initState ts = Context [] (M.fromList [(k,d) | d@(Dictionary k _) <- concatMap initDicts (tsTemplates ts)])

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

type DualType a m = a -> m

contramap :: (a -> b) -> DualType b m -> DualType a m
contramap f cp = cp . f

append :: (Monoid m) => DualType a m -> DualType b m -> DualType (a, b) m
append cp1 cp2 (x, y) = cp1 x `mappend` cp2 y

append' :: (Monoid m) => DualType a m -> DualType a m -> DualType a m
append' cp1 cp2 = contramap (\x -> (x, x)) (cp1 `append` cp2)

sequenceD :: (Monoid m) => [DualType a m] -> DualType [a] m
sequenceD ds xs = mconcat (zipWith (\x d -> d x) xs ds)

type Coparser a = DualType a BU.Builder

-- | Primitive type class.
class Primitive a where
    data Delta a     :: *
    witnessType      :: a -> TypeWitness a
    assertType       :: (Primitive b) => TypeWitness b -> a
    toValue          :: a -> Value
    fromValue        :: Value -> a
    defaultBaseValue :: a
    ivToPrimitive    :: InitialValueAttr -> a
    delta            :: a -> Delta a -> a
    delta_           :: a -> a -> Delta a
    ftail            :: a -> a -> a
    ftail_           :: a -> a -> a
    decodeP          :: A.Parser a
    decodeD          :: A.Parser (Delta a)
    decodeT          :: A.Parser a
    encodeP          :: Coparser a
    encodeD          :: Coparser (Delta a)
    encodeT          :: Coparser a

    decodeT = decodeP
    encodeT = encodeP

prop_decodeP_dot_encodeP_is_ID :: (Primitive a, Eq a) => a -> Bool
prop_decodeP_dot_encodeP_is_ID x = A.maybeResult (A.feed (A.parse decodeP ((B.concat . BL.toChunks . BU.toLazyByteString . encodeP) x)) B.empty) == Just x

prop_decodeD_dot_encodeD_is_ID :: (Primitive a, Eq (Delta a)) => Delta a -> Bool
prop_decodeD_dot_encodeD_is_ID x = A.maybeResult (A.feed (A.parse decodeD ((B.concat . BL.toChunks . BU.toLazyByteString . encodeD) x)) B.empty) == Just x

prop_decodeT_dot_encodeT_is_ID :: (Primitive a, Eq a) => a -> Bool
prop_decodeT_dot_encodeT_is_ID x = A.maybeResult (A.feed (A.parse decodeT ((B.concat . BL.toChunks . BU.toLazyByteString . encodeT) x)) B.empty) == Just x

prop_fromValue_dot_toValue_is_ID :: (Primitive a, Eq a) => a -> Bool
prop_fromValue_dot_toValue_is_ID x =  (f . fromValue . f) x == f x where f = toValue

prop_delta_dot_delta__is_ID :: (Primitive a, Eq a) => (a, a) -> Bool
prop_delta_dot_delta__is_ID (x, y) = delta y (delta_ x y) == x

prop_ftail_dot_ftail__is_ID :: (ListLike a c, Primitive a, Eq a) => (a, a) -> Bool
prop_ftail_dot_ftail__is_ID (x, y) | (genericLength x :: Word32) >= genericLength y = ftail y (ftail_ x y) == x
prop_ftail_dot_ftail__is_ID _ = True

prop_rmPreamble_dot_addPreamble_is_ID :: AsciiString -> Bool
prop_rmPreamble_dot_addPreamble_is_ID s = (rmPreamble . addPreamble) s == s && (rmPreamble' . addPreamble') s == s

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
           deriving (Show, Eq)

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
    fromValue (I32 i) = i
    fromValue _ = throw $ D4 "Type mismatch."
    defaultBaseValue = 0 
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di32 i') = i + i'
    delta_ i1 i2 = Di32 $ i1 - i2
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    ftail_ = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = int
    decodeD = Di32 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _int
    encodeD  =  encodeP . (\(Di32 i) -> i)

instance Arbitrary (Delta Int32) where
    arbitrary = fmap Di32 (arbitrary :: Gen Int32)
    shrink (Di32 i) = map Di32 $ (shrink :: (Int32 -> [Int32])) i

instance Primitive Word32 where
    newtype Delta Word32 = Dw32 Int32 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW32
    assertType (TypeWitnessW32 w) = w
    assertType _ = throw $ D4 "Type mismatch."
    toValue = UI32
    fromValue (UI32 i) = i
    fromValue _ = throw $ OtherException "Type mismatch."
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw32 i) = fromIntegral (fromIntegral w + i)
    delta_ w1 w2 = Dw32 (fromIntegral w1 - fromIntegral w2)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    ftail_ = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = uint
    decodeD = Dw32 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _uint
    encodeD =  encodeP . (\(Dw32 w) -> w)

instance Arbitrary (Delta Word32) where
    arbitrary = fmap Dw32 (arbitrary :: Gen Int32)
    shrink (Dw32 i) = map Dw32 $ (shrink :: (Int32 -> [Int32])) i

instance Primitive Int64 where
    newtype Delta Int64 = Di64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessI64
    assertType (TypeWitnessI64 i) = i
    assertType _ = throw $ D4 "Type mismatch."
    toValue = I64
    fromValue (I64 i) = i
    fromValue _ = throw $ OtherException "Type mismatch."
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di64 i')= i + i'
    delta_ i1 i2 = Di64 (i1 - i2)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    ftail_ = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = int
    decodeD = Di64 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _int
    encodeD =  encodeP . (\(Di64 i) -> i)

instance Arbitrary (Delta Int64) where
    arbitrary = fmap Di64 (arbitrary :: Gen Int64)
    shrink (Di64 i) = map Di64 $ (shrink :: (Int64 -> [Int64])) i

instance Primitive Word64 where
    newtype Delta Word64 = Dw64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW64
    assertType (TypeWitnessW64 w) = w
    assertType _ = throw $ D4 "Type mismatch."
    toValue = UI64
    fromValue (UI64 i) = i
    fromValue _ = throw $ OtherException "Type mismatch."
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw64 i) = fromIntegral (fromIntegral w + i)
    delta_ w1 w2 = Dw64 (fromIntegral w1 - fromIntegral w2)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
    ftail_ = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = uint
    decodeD = Dw64 <$> int
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = _uint
    encodeD = encodeP . (\(Dw64 w) -> w)

instance Arbitrary (Delta Word64) where
    arbitrary = fmap Dw64 (arbitrary :: Gen Int64)
    shrink (Dw64 i) = map Dw64 $ (shrink :: (Int64 -> [Int64])) i

instance Primitive AsciiString where
    newtype Delta AsciiString = Dascii (Int32, String)
    witnessType = TypeWitnessASCII
    assertType (TypeWitnessASCII s) = s
    assertType _ = throw $ D4 "Type mismatch."
    toValue = A
    fromValue (A s) = s
    fromValue _ = throw $ OtherException "Type mismatch."
    defaultBaseValue = ""
    ivToPrimitive = text
    delta s1 (Dascii (l, s2)) | l < 0 = s2 ++ s1' where s1' = genericDrop (-(l + 1)) s1
    delta s1 (Dascii (l, s2)) | l >= 0 = s1' ++ s2 where s1' = genericTake (genericLength s1 - l) s1
    delta _ _ = throw $ D4 "Type mismatch."
    delta_ s1 s2 =  if (genericLength l1 :: Int32) >= genericLength l2 
                    then Dascii (genericLength s2 - genericLength l1, genericDrop (genericLength l1 :: Int32) s1)
                    else Dascii (genericLength l2 - genericLength s2 - 1, genericTake ((genericLength s1 - genericLength l2) :: Int32) s1)
                    where   l1 = map fst $ takeWhile (uncurry (==) ) (zip s1 s2)
                            l2 = map fst $ takeWhile (uncurry (==)) (zip (reverse s1) (reverse s2))
                            
    ftail s1 s2 = take (length s1 - length s2) s1 ++ s2
    ftail_ s1 s2 | (genericLength s1 :: Word32) > genericLength s2 = s1
    ftail_ s1 s2 | (genericLength s1 :: Word32) == genericLength s2 = map fst (dropWhile (\(x, y) -> x == y) (zip s1 s2))
    ftail_ _ _ = throw $ OtherException "Can't encode a smaller string with a tail operator."
    decodeP = asciiString
    decodeD = do 
                l <- int
                s <- decodeP
                return (Dascii (l, s))
    decodeT = decodeP
    encodeP = _asciiString
    encodeD = (encodeP `append` encodeP) . (\(Dascii (i, s)) -> (i, s))

instance Primitive (Int32, Int64) where
    newtype Delta (Int32, Int64) = Ddec (Int32, Int64) deriving (Eq, Show)
    witnessType = TypeWitnessDec
    assertType (TypeWitnessDec (e, m)) = (e, m)
    assertType _ = throw $ D4 "Type mismatch."
    toValue (e, m) = Dec $ (fromIntegral m) * 10^^e
    fromValue (Dec 0.0) = (0, 0)
    fromValue (Dec d) | d > 0.0 = (fromIntegral e  - genericLength digits, m) 
        where   (digits, e) = floatToDigits 10 d
                m = foldl (\r x -> 10*r + (fromIntegral x)) 0 digits
    fromValue (Dec d) = (e, -1 * m) where (e, m) = fromValue (Dec (-1 * d))
    fromValue _ = throw $ D4 "Type mismatch."
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
    delta_ (e2, m2) (e1, m1) = Ddec (e2 - e1, m2 - m1)
    ftail = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    ftail_ = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    decodeP = do 
        e <- int :: A.Parser Int32
        m <- int :: A.Parser Int64
        return (e, m)
    decodeD = Ddec <$> decodeP
    decodeT = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields."
    encodeP = encodeP `append` encodeP
    encodeD = encodeP . (\(Ddec (e, m)) -> (e, m))

instance Arbitrary (Delta (Int32, Int64)) where
    arbitrary = fmap Ddec (arbitrary :: Gen (Int32, Int64))
    shrink (Ddec (e, m)) = map Ddec $ (shrink :: (Int32, Int64) -> [(Int32, Int64)]) (e, m)

instance Primitive B.ByteString where
    newtype Delta B.ByteString = Dbs (Int32, B.ByteString) deriving (Eq, Show)
    witnessType = TypeWitnessBS
    assertType (TypeWitnessBS bs) = bs
    assertType _ = throw $ D4 "Type mismatch."
    toValue = B 
    fromValue (B bs) = bs
    fromValue _ = throw $ D4 "Type mismatch."
    defaultBaseValue = B.empty
    ivToPrimitive iv = B.pack (map (toEnum . digitToInt) (filter whiteSpace (text iv)))
    delta bv1 (Dbs (l, bv2)) | l < 0 = bv2 `B.append` bv'' where bv'' = genericDrop (-(l + 1)) bv1 
    delta bv1 (Dbs (l, bv2)) | l >= 0 = bv'' `B.append` bv2 where bv'' = genericTake (genericLength bv1 - l) bv1
    delta _ _ = throw $ D4 "Type mismatch."
    delta_ bv1 bv2 =  if (genericLength l1 :: Word32) >= genericLength l2 
                    then Dbs (genericLength bv2 - genericLength l1, genericDrop (genericLength l1 :: Word32) bv1)
                    else Dbs (genericLength l2 - genericLength bv2 - 1, genericTake ((genericLength bv1 - genericLength l2) :: Word32) bv1)
                    where   l1 = takeWhile (uncurry (==)) (zip bv1 bv2)
                            l2 = takeWhile (uncurry (==)) (zip (reverse bv1) (reverse bv2))
    ftail b1 b2 = B.take (B.length b1 - B.length b2) b1 `B.append` b2
    ftail_ b1 b2 | (genericLength b1 :: Word32) > genericLength b2 = b1
    ftail_ b1 b2 | (genericLength b1 :: Word32) == genericLength b2 = B.pack (map fst (dropWhile (\(x, y) -> x == y) (B.zip b1 b2)))
    ftail_ _ _ = throw $ OtherException "Can't encode a smaller string with a tail operator."
    decodeP = byteVector
    decodeD = do 
                l <- int
                bv <- decodeP
                return (Dbs (l, bv))
    encodeP = _byteVector
    encodeD = (encodeP `append` encodeP) . (\(Dbs (i, bs)) -> (i, bs)) 

instance Arbitrary (Delta B.ByteString) where
    arbitrary = fmap Dbs (arbitrary :: Gen (Int32, B.ByteString))
    shrink (Dbs (i, bs)) = map Dbs (zip (shrink i) (replicate (length $ shrink i) bs))

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack (listOf (arbitrary :: Gen Word8))
    shrink = shrinkNothing

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

instance Arbitrary Templates where
    arbitrary = do
                n <- arbitrary
                ns <- arbitrary
                d <- arbitrary
                ts <- arbitrary
                return $ Templates n ns d ts 

-- |FAST template.
data Template = Template {
    tName         :: TemplateNsName,
    tNs           :: Maybe NsAttr,
    tDictionary   :: Maybe DictionaryAttr,
    tTypeRef      :: Maybe TypeRef,
    tInstructions :: [Instruction]
    } deriving (Show)

instance Arbitrary Template where
    arbitrary = do
                    n <- arbitrary
                    ns <- arbitrary
                    d <- arbitrary
                    tr <- arbitrary
                    is <- arbitrary
                    return $ Template n ns d tr is

-- |A typeRef element of a template.
data TypeRef = TypeRef {
    trName :: NameAttr,
    trNs   :: Maybe NsAttr
    } deriving (Show)

instance Arbitrary TypeRef where
    arbitrary = do
                    n <- arbitrary 
                    ns <- arbitrary
                    return $ TypeRef n ns

-- |An Instruction in a template is either a field instruction or a template reference.
data Instruction = Instruction Field
                    |TemplateReference (Maybe TemplateReferenceContent)
                    deriving (Show)

instance Arbitrary Instruction where
    arbitrary = liftM Instruction arbitrary
    {-arbitrary = oneof [liftM Instruction arbitrary, liftM TemplateReference arbitrary]-}
    -- TemplateReferences are really hard to test.

-- |This is a helper data structure, NOT defined in the reference.
data TemplateReferenceContent = TemplateReferenceContent {
        trcName       :: NameAttr,
        trcTemplateNs :: Maybe TemplateNsAttr
        } deriving (Show)

instance Arbitrary TemplateReferenceContent where
    arbitrary = do
                    n <- arbitrary
                    ns <- arbitrary
                    return $ TemplateReferenceContent n ns

tempRefCont2TempNsName :: TemplateReferenceContent -> TemplateNsName
tempRefCont2TempNsName (TemplateReferenceContent n maybe_ns) = TemplateNsName n maybe_ns Nothing 

-- |Field Instruction content.
data FieldInstrContent = FieldInstrContent {
    ficFName    :: NsName,
    ficPresence :: Maybe PresenceAttr,
    ficFieldOp  :: Maybe FieldOp
    } deriving (Show)

instance Arbitrary FieldInstrContent where
    arbitrary = do
                    n <- arbitrary
                    p <- arbitrary
                    op <- arbitrary
                    return $ FieldInstrContent n p op

-- |FAST field instructions.
data Field = IntField IntegerField
           | DecField DecimalField
           | AsciiStrField AsciiStringField
           | UnicodeStrField UnicodeStringField
           | ByteVecField ByteVectorField
           | Seq Sequence
           | Grp Group
			deriving (Show)

instance Arbitrary Field where
    arbitrary = oneof [liftM IntField arbitrary, liftM DecField arbitrary, liftM AsciiStrField arbitrary, liftM UnicodeStrField arbitrary, liftM ByteVecField arbitrary, liftM Seq arbitrary, liftM Grp arbitrary]

-- |Integer Fields.
data IntegerField = Int32Field FieldInstrContent
                    |UInt32Field FieldInstrContent
                    |Int64Field FieldInstrContent
                    |UInt64Field FieldInstrContent
                    deriving (Show)

instance Arbitrary IntegerField where
    arbitrary = oneof [fmap Int32Field arbitrary, fmap UInt32Field arbitrary, fmap Int64Field arbitrary, fmap UInt64Field arbitrary]

-- |Decimal Field.
data DecimalField = DecimalField {
        dfiFName    :: NsName,
        dfiPresence :: Maybe PresenceAttr,
        dfiFieldOp  :: Maybe (Either FieldOp DecFieldOp)
        } deriving (Show)

instance Arbitrary DecimalField where
    arbitrary = do
                n <- arbitrary
                p <- arbitrary
                op <- arbitrary
                return $ DecimalField n p op

-- |Ascii string field.
data AsciiStringField = AsciiStringField FieldInstrContent deriving (Show)

instance Arbitrary AsciiStringField where
    arbitrary = fmap AsciiStringField arbitrary

-- |Unicode string field.
data UnicodeStringField = UnicodeStringField {
        usfContent :: FieldInstrContent,
        usfLength  :: Maybe ByteVectorLength
        } deriving (Show)

instance Arbitrary UnicodeStringField where
    arbitrary = do
                c <- arbitrary
                l <- arbitrary
                return $ UnicodeStringField c l

-- |Bytevector field.
data ByteVectorField = ByteVectorField {
        bvfContent :: FieldInstrContent,
        bvfLength  :: Maybe ByteVectorLength
        } deriving (Show)

instance Arbitrary ByteVectorField where
    arbitrary = do
                c <- arbitrary
                l <- arbitrary
                return $ ByteVectorField c l

-- |Sequence field.
data Sequence = Sequence {
        sFName        :: NsName,
        sPresence     :: Maybe PresenceAttr,
        sDictionary   :: Maybe DictionaryAttr,
        sTypeRef      :: Maybe TypeRef,
        sLength       :: Maybe Length,
        sInstructions :: [Instruction]
        } deriving (Show)

instance Arbitrary Sequence where
    arbitrary = do
                n <- arbitrary
                p <- arbitrary
                d <- arbitrary
                tr <- arbitrary
                l <- arbitrary
                is <- listOf arbitrary
                return $ Sequence n p d tr l is

-- |Group field.
data Group = Group {
        gFName        :: NsName,
        gPresence     :: Maybe PresenceAttr,
        gDictionary   :: Maybe DictionaryAttr,
        gTypeRef      :: Maybe TypeRef,
        gInstructions :: [Instruction]
        } deriving (Show)

instance Arbitrary Group where
    arbitrary = do
                n <- arbitrary
                p <- arbitrary
                d <- arbitrary
                tr <- arbitrary
                is <- listOf arbitrary
                return $ Group n p d tr is

-- |ByteVectorLenght is logically a uInt32, but it is not a field instruction 
-- and it is not physically present in the stream. Obviously no field operator 
-- is needed.
data ByteVectorLength = ByteVectorLength {
    bvlNsName::NsName
    } deriving (Show)

instance Arbitrary ByteVectorLength where
    arbitrary = fmap ByteVectorLength arbitrary

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

instance Arbitrary Length where
    arbitrary = do 
                    l <- arbitrary
                    op <- arbitrary
                    return $ Length l op


-- |Presence of a field value is either mandatory or optional.
data PresenceAttr = Mandatory | Optional deriving (Show)

instance Arbitrary PresenceAttr where
    arbitrary = oneof [return Mandatory, return Optional ]

-- |FAST field operators.
data FieldOp = Constant InitialValueAttr
             | Default (Maybe InitialValueAttr)
             | Copy OpContext
             | Increment OpContext
             | Delta OpContext
             | Tail OpContext
				deriving (Show)

instance Arbitrary FieldOp where
    arbitrary = oneof [fmap Constant arbitrary, fmap Default arbitrary, fmap Copy arbitrary, fmap Increment arbitrary, fmap Delta arbitrary, fmap Tail arbitrary]
 
-- |The decimal field operator consists of two standart operators.
data DecFieldOp = DecFieldOp {
    dfoExponent :: Maybe FieldOp,
    dfoMantissa :: Maybe FieldOp
    } deriving (Show)

instance Arbitrary DecFieldOp where
    arbitrary = do
                    ex <- arbitrary
                    ma <- arbitrary
                    return $ DecFieldOp ex ma

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

instance Arbitrary OpContext where
    arbitrary = do
                    d <- arbitrary
                    ns <- arbitrary
                    iv <- arbitrary
                    return $ OpContext d ns iv

-- |Dictionary attribute. Three predefined dictionaries are "template", "type" 
-- and "global".
newtype DictionaryAttr = DictionaryAttr String deriving (Show, Arbitrary)

-- |nsKey attribute.
data NsKey = NsKey {
    nkKey :: KeyAttr,
    nkNs  :: Maybe NsAttr
    } deriving (Eq, Ord, Show)

instance Arbitrary NsKey where  
    arbitrary = do 
                    k <- arbitrary
                    ns <- arbitrary
                    return $ NsKey k ns

-- |Key attribute.
data KeyAttr = KeyAttr {
    kaToken :: Token
    } deriving (Eq, Ord, Show)

instance Arbitrary KeyAttr where
    arbitrary = fmap KeyAttr arbitrary
-- |Initial value attribute. The value is a string of unicode characters and needs to 
-- be converted to the type of the field in question.
data InitialValueAttr = InitialValueAttr {
    text :: UnicodeString
    } deriving (Show)

instance Arbitrary InitialValueAttr where
    arbitrary = arbitrary

-- |A full name in a template is given by a namespace URI and localname. For 
-- application types, fields and operator keys the namespace URI is given by 
-- the 'ns' attribute. For templates the namespace URI is given by the 
-- 'templateNs' attribute.
-- Note that full name constructors in the data structures are named 'fname'.

-- |A full name for an application type, field or operator key.
data NsName = NsName NameAttr (Maybe NsAttr) (Maybe IdAttr) deriving (Eq, Ord, Show)

instance Arbitrary NsName where
    arbitrary = do 
                    n <- arbitrary
                    ns <- arbitrary
                    i <- arbitrary
                    return $ NsName n ns i

-- |A full name for a template.
data TemplateNsName = TemplateNsName NameAttr (Maybe TemplateNsAttr) (Maybe IdAttr) deriving (Show, Eq, Ord)

instance Arbitrary TemplateNsName where
    arbitrary =  do
                    n <- arbitrary
                    ns <- arbitrary
                    i <- arbitrary
                    return $ TemplateNsName n ns i

-- |Translates a TemplateNsName into a NsName. Its the same anyway.
tname2fname :: TemplateNsName -> NsName
tname2fname (TemplateNsName n (Just (TemplateNsAttr ns)) maybe_id) = NsName n (Just (NsAttr ns)) maybe_id
tname2fname (TemplateNsName n Nothing maybe_id) = NsName n Nothing maybe_id

fname2tname :: NsName -> TemplateNsName
fname2tname (NsName n (Just (NsAttr ns)) maybe_id) = TemplateNsName n (Just (TemplateNsAttr ns)) maybe_id
fname2tname (NsName n Nothing maybe_id) = TemplateNsName n Nothing maybe_id

-- |The very basic name related attributes.
newtype NameAttr = NameAttr String deriving (Eq, Ord, Show, Arbitrary)
newtype NsAttr = NsAttr String deriving (Eq, Ord, Show, Arbitrary)
newtype TemplateNsAttr = TemplateNsAttr String deriving (Eq, Ord, Show, Arbitrary)
newtype IdAttr = IdAttr Token deriving (Eq, Ord, Show, Arbitrary)
newtype Token = Token String deriving (Eq, Ord, Show, Arbitrary)


-- |Get a Stopbit encoded entity.
anySBEEntity :: A.Parser B.ByteString
anySBEEntity = takeTill' stopBitSet

_anySBEEntity :: Coparser B.ByteString
_anySBEEntity bs | B.null bs = BU.empty
_anySBEEntity bs = BU.fromByteString (B.init bs `B.append` B.singleton (setBit (B.last bs) 7))

-- |Like takeTill, but takes the matching byte as well.
takeTill' :: (Word8 -> Bool) -> A.Parser B.ByteString
takeTill' f = do
    str <- A.takeTill f
    c <- A.take 1
    return (str `B.append` c)

-- |Test wether the stop bit is set of a Char.
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

_byteVector :: Coparser B.ByteString
_byteVector = (_uint . (\bs -> fromIntegral(B.length bs) :: Word32)) `append'` BU.fromByteString

-- |Unsigned integer parser, doesn't check for bounds.
-- TODO: should we check for R6 errors, i.e overlong fields?
uint :: (Bits a) => A.Parser a
uint = do 
    bs <- anySBEEntity
    return (B.foldl h 0 bs)
    where   h::(Bits a, Num a) => a -> Word8 -> a
            h r w = fromIntegral (clearBit w 7) .|. shiftL r 7
        
_uint :: (Bits a, Eq a, Integral a) => Coparser a
_uint = _anySBEEntity . uintBS 

uintBS :: (Bits a, Eq a, Integral a) => a -> B.ByteString
uintBS ui = if ui' /= 0 
            then uintBS ui' `B.snoc` (fromIntegral (ui .&. 127) :: Word8)
            else B.singleton (fromIntegral ui :: Word8)
            where ui' = shiftR ui 7

-- |Signed integer parser, doesn't check for bounds.
int :: (Bits a) => A.Parser a
int = do
        bs <- anySBEEntity
        return (if testBit (B.head bs) 6 
                then B.foldl h (shiftL (-1) 6 .|. fromIntegral (B.head bs)) (B.tail bs)
                else B.foldl h 0 bs)
        where   
                h :: (Bits a, Num a) => a -> Word8 -> a
                h r w = fromIntegral (clearBit w 7) .|. shiftL r 7

_int :: (Bits a, Ord a, Integral a) => Coparser a
_int  = _anySBEEntity . intBS 

intBS :: (Bits a, Ord a, Integral a) => a -> B.ByteString
intBS i =   if i < 0 
            then h i 
            else   if (testBit (B.head (uintBS i)) 6)
                   then (B.singleton 0) `mappend` (uintBS i)
                   else uintBS i
            where h x = if x' /= -1 || not (testBit (fromIntegral x :: Word8) 6)
                        then h x' `B.snoc` (fromIntegral (x .&. 127) :: Word8)
                        else B.singleton (fromIntegral (x .&. 127) :: Word8)
                        where x' = shiftR x 7

-- |ASCII string field parser, non-Nullable.
asciiString :: A.Parser AsciiString
asciiString = do
    bs <- anySBEEntity
    let bs' = B.init bs `mappend` B.singleton (clearBit (B.last bs) 7) in
        return (unpack bs')

_asciiString :: Coparser AsciiString
_asciiString = _anySBEEntity . pack 

trimWhiteSpace :: String -> String
trimWhiteSpace = reverse . dropWhile whiteSpace . reverse . dropWhile whiteSpace

whiteSpace :: Char -> Bool
whiteSpace c =  c `elem` " \t\r\n"

-- *Previous value related functions.

-- |Get previous value.
prevValue :: (Monad m) => NsName -> OpContext -> StateT Context m DictValue
prevValue name (OpContext (Just (DictionaryAttr dname)) Nothing _ ) 
    = pv dname (N name)

prevValue _ (OpContext (Just (DictionaryAttr dname)) (Just dkey) _ ) 
    = pv dname (K dkey)

prevValue name (OpContext Nothing Nothing _ ) 
    = pv "global" (N name)

prevValue _ (OpContext Nothing (Just dkey) _ ) 
    = pv "global" (K dkey)

pv :: (Monad m) => String -> DictKey -> StateT Context m DictValue
pv d k = do
       st <- get
       case M.lookup d (dict st) >>= \(Dictionary _ xs) -> M.lookup k xs of
        Nothing -> throw $ OtherException ("Could not find specified dictionary/key." ++ show d ++ " " ++ show k)
        Just dv -> return dv

-- |Update the previous value.
updatePrevValue :: (Monad m) => NsName -> OpContext -> DictValue -> StateT Context m ()
updatePrevValue name (OpContext (Just (DictionaryAttr dname)) Nothing _ ) dvalue
    = uppv dname (N name) dvalue

updatePrevValue _ (OpContext (Just (DictionaryAttr dname)) (Just dkey) _ ) dvalue
    = uppv dname (K dkey) dvalue

updatePrevValue name (OpContext Nothing Nothing _ ) dvalue
    = uppv "global" (N name) dvalue

updatePrevValue _ (OpContext Nothing (Just dkey) _ ) dvalue
    = uppv "global" (K dkey) dvalue

uppv :: (Monad m) => String -> DictKey -> DictValue -> StateT Context m ()
uppv d k v = do
    st <- get
    put (Context (pm st) (M.adjust (\(Dictionary n xs) -> Dictionary n (M.adjust (\_ -> v) k xs)) d (dict st)))

setPMap :: (Monad m) => Bool -> StateT Context m ()
setPMap b = do 
                 st <- get
                 put (Context (pm st ++ [b]) (dict st))

-- |Create a unique fname out of a given one and a string.
uniqueFName::NsName -> String -> NsName
uniqueFName fname s = NsName (NameAttr(n ++ s)) ns ide
    where (NsName (NameAttr n) ns ide) = fname

needsSegment ::[Instruction] -> M.Map TemplateNsName Template -> Bool
needsSegment ins ts = any (needsPm ts) ins 

-- |Decides wether an instruction uses the presence map or not. We need to know all the templates,
-- to process template reference instructions recursivly.
needsPm::M.Map TemplateNsName Template -> Instruction -> Bool
-- static template reference
needsPm ts (TemplateReference (Just trc)) = all (needsPm ts) (tInstructions t) where t = ts M.! tempRefCont2TempNsName trc
-- dynamic template reference
needsPm _ (TemplateReference Nothing) = False
needsPm _ (Instruction (IntField (Int32Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (Int64Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (UInt32Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (UInt64Field fic))) = intFieldNeedsPm fic
needsPm ts (Instruction (DecField (DecimalField fname Nothing eitherOp))) = needsPm ts (Instruction(DecField (DecimalField fname (Just Mandatory) eitherOp)))
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) Nothing ))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Constant _)))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Default _)))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Copy _)))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Increment _)))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Delta _)))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Just (Left (Tail _)))))) = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) Nothing ))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Constant _)))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Default _)))))) = True 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Copy _)))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Increment _)))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Delta _)))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Just (Left (Tail _)))))) = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
needsPm ts (Instruction (DecField (DecimalField fname (Just Mandatory) (Just (Right (DecFieldOp maybe_opE maybe_opM)))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Mandatory) maybe_opE)))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) maybe_opM)))
needsPm ts (Instruction (DecField (DecimalField fname (Just Optional) (Just (Right (DecFieldOp maybe_opE maybe_opM)))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Optional) maybe_opE)))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) maybe_opM)))
needsPm ts (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent fname Nothing maybeOp)))) = needsPm ts (Instruction(AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) maybeOp))))
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) Nothing)))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Constant _)))))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Copy _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Increment _)))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Delta _)))))) =  False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) Nothing)))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Constant _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Copy _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Increment _)))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Delta _)))))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Tail _)))))) = True
needsPm ts (Instruction (ByteVecField (ByteVectorField (FieldInstrContent fname Nothing maybeOp) maybe_length))) = needsPm ts (Instruction(ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybeOp) maybe_length)))
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Constant _))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))) _))) = throw $ S5 " No initial value given for mandatory default operator."
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just _)))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Copy _ ))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Copy _ ))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) _))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _))) _))) = throw $ S2 "Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Delta _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Delta _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Tail _))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Tail _))) _))) = True
needsPm ts (Instruction (UnicodeStrField (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length))) = needsPm ts (Instruction(ByteVecField (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)))
needsPm ts (Instruction (Seq s)) = all h (sInstructions s)
    where   h (TemplateReference Nothing) = False
            h (TemplateReference (Just trc)) = all (needsPm ts) (tInstructions (ts M.! tempRefCont2TempNsName trc))
            h f = needsPm ts f
needsPm ts (Instruction (Grp g)) = all h (gInstructions g)
    where   h (TemplateReference Nothing) = False
            h (TemplateReference (Just trc)) = all (needsPm ts) (tInstructions (ts M.! tempRefCont2TempNsName trc))
            h f = needsPm ts f

-- |Maps a integer field to a triple (DictionaryName, Key, Value).
intFieldNeedsPm::FieldInstrContent -> Bool
intFieldNeedsPm (FieldInstrContent fname Nothing maybeOp) = intFieldNeedsPm $ FieldInstrContent fname (Just Mandatory) maybeOp
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) Nothing) = False
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) = False
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Default _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Copy _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Delta _))) = False
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Tail _))) = throw $ S2 " Tail operator can not be applied on an integer type field." 
intFieldNeedsPm (FieldInstrContent _ (Just Optional) Nothing) = False
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Constant _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Default _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Copy _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Increment _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Delta _))) = False
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Tail _))) = throw $ S2 " Tail operator can not be applied on an integer type field." 

-- |Convert a bytestring into a presence map.
bsToPm::B.ByteString -> [Bool]
bsToPm bs = concatMap h (B.unpack bs) 
    where   h::Word8 -> [Bool]
            h w = map (testBit w) [6,5..0] 

pmToBs :: [Bool] -> B.ByteString
pmToBs xs = B.pack (map h (sublistsOfLength 7 xs))
    where   h :: [Bool] -> Word8
            h = fst . foldl (\(r,n) y -> if y then (setBit r n, n-1) else (r, n-1)) (0, 6)

sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength _ [] = []
sublistsOfLength n xs = take n xs : sublistsOfLength n (drop n xs)

assertNameIs :: NsName -> (NsName, a) -> a
assertNameIs n1 (n2, x) = if n1 == n2 then x else throw $ OtherException $ "Template doesn't fit message, in the field: " ++ show n1


-- |Creates a list of dictionaries depending on the fields of a template.
initDicts::Template -> [Dictionary]
initDicts t = createDicts $ catMaybes $ concatMap h (tInstructions t)
    where   h (TemplateReference _) = []
            h (Instruction f) = dictOfField f

-- |Maps triples of the form (DictionaryName, Key, Value) to a list of dictionaries.
createDicts::[(String, DictKey, DictValue)] -> [Dictionary]
createDicts es =  map h (groupBy (\ (d, _ , _) (d', _ , _) -> d == d') es)
    where   h xs = Dictionary name (M.fromList (map (\(_,y,z) -> (y,z)) xs))
                where (name, _, _) = head xs

-- |Maps a field to a triple (DictionaryName, Key, Value).
dictOfField::Field -> [Maybe (String, DictKey, DictValue)]
dictOfField (IntField (Int32Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp] 
dictOfField (IntField (Int64Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (IntField (UInt32Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (IntField (UInt64Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (DecField (DecimalField fname Nothing eitherOp)) = dictOfField $ DecField $ DecimalField fname (Just Mandatory) eitherOp
dictOfField (DecField (DecimalField _ (Just Mandatory) Nothing )) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Just (Left (Constant _))))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))) = throw $ S5 " No initial value given for mandatory default operator."
dictOfField (DecField (DecimalField _ (Just Mandatory) (Just (Left (Default (Just _)))))) = [Nothing]
dictOfField (DecField (DecimalField fname (Just Mandatory) (Just (Left (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Just (Left (Increment _))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (DecField (DecimalField fname (Just Mandatory) (Just (Left (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))) = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
dictOfField (DecField (DecimalField _ (Just Optional) Nothing )) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Optional) (Just (Left (Constant _))))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Optional) (Just (Left (Default Nothing))))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Optional) (Just (Left (Default (Just _)))))) = [Nothing]
dictOfField (DecField (DecimalField fname (Just Optional) (Just (Left (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Optional) (Just (Left (Increment _))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (DecField (DecimalField fname (Just Optional) (Just (Left (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Optional) (Just (Left (Tail _))))) = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
dictOfField (DecField (DecimalField fname (Just Optional) (Just (Right (DecFieldOp maybe_opE maybe_opM))))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Optional) maybe_opE))) 
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) maybe_opM)))
dictOfField (DecField (DecimalField fname (Just Mandatory) (Just (Right (DecFieldOp maybe_opE maybe_opM))))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Mandatory) maybe_opE)))
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) maybe_opM)))
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname Nothing maybeOp))) = dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) maybeOp)))
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) Nothing))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))) = throw $ S5 " No initial value given for mandatory default operator."
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Tail oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) Nothing))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Constant _))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default (Just _)))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Increment _))))) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Tail oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname Nothing maybeOp) maybe_length)) = dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybeOp) maybe_length))
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Constant _))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))) _)) = throw $ S5 " No initial value given for mandatory default operator."
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just _)))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) _)) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _))) _)) = throw $ S2 "Increment operator is only applicable to integer fields." 
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Delta oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Delta oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Tail oc))) _)) = [Just $ dictOfOpContext oc fname] 
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Tail oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (UnicodeStrField (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)) = dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length))
dictOfField (Seq s) = concatMap h (sInstructions s)
    where   h (TemplateReference _) = [Nothing]
            h (Instruction f) = dictOfField f
dictOfField (Grp g) = concatMap h (gInstructions g)
    where   h (TemplateReference _) = [Nothing]
            h (Instruction f) = dictOfField f

-- |Maps a integer field to a triple (DictionaryName, Key, Value).
dictOfIntField::FieldInstrContent -> Maybe (String, DictKey, DictValue)
dictOfIntField (FieldInstrContent fname Nothing maybeOp) = dictOfIntField $ FieldInstrContent fname (Just Mandatory) maybeOp
dictOfIntField (FieldInstrContent _ (Just Mandatory) Nothing) =  Nothing
dictOfIntField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) = Nothing
dictOfIntField (FieldInstrContent _ (Just Mandatory) (Just (Default _))) = Nothing
dictOfIntField (FieldInstrContent fname (Just Mandatory) (Just (Copy oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Mandatory) (Just (Increment oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Mandatory) (Just (Delta oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent _ (Just Mandatory) (Just (Tail _))) = throw $ S2 " Tail operator can not be applied on an integer type field." 
dictOfIntField (FieldInstrContent _ (Just Optional) Nothing) =  Nothing
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Constant _))) = Nothing
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Default _))) = Nothing
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Copy oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Increment oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Delta oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Tail _))) = throw $ S2 " Tail operator can not be applied on an integer type field." 

-- |Outputs a triple (DictionaryName, Key, Value) depending on OpContext and 
-- the NsName of a field.
dictOfOpContext::OpContext -> NsName -> (String, DictKey, DictValue)
dictOfOpContext (OpContext Nothing Nothing _) n = ("global", N n, Undefined)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) Nothing _) n = (d, N n, Undefined)
dictOfOpContext (OpContext Nothing (Just k) _) _ = ("global", K k, Undefined)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) (Just k) _) _ = (d, K k, Undefined)

-- |Remove Preamble of an ascii string, non-Nullable situation.
rmPreamble :: AsciiString -> AsciiString
rmPreamble (['\0']) = []
rmPreamble (['\0', '\0']) = "\NUL"
-- overlong string.
rmPreamble s = dropWhile (=='\0') s

-- |Remove preamble of an ascii string, NULLable situation.
rmPreamble'::AsciiString -> AsciiString
rmPreamble' (['\0','\0']) = []
rmPreamble' (['\0','\0','\0']) = "\NUL"
-- overlong string.
rmPreamble' s = dropWhile (=='\0') s

addPreamble :: AsciiString -> AsciiString
addPreamble [] = (['\0'])
addPreamble "\NUL" = (['\0','\0'])
addPreamble s = dropWhile (=='\0') s

addPreamble' :: AsciiString -> AsciiString
addPreamble' [] = (['\0','\0'])
addPreamble' "\NUL" = (['\0','\0','\0'])
addPreamble' s = dropWhile (=='\0') s


