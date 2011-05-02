-- |A FAST protocoll parser.
module FParser where 

import Prelude hiding (take)
import qualified Data.ByteString as B
import qualified Codec.Binary.UTF8.String as U
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (ord)
import Data.Attoparsec.Char8
import Control.Monad.State
import Control.Applicative 
import Data.Bits
import Data.Word (Word8)
import Data.Ix (inRange)
import FAST


-- |Proviosonal state of the parser, will be changed according to reference.
data FState = FState {
    -- |bitmap
    pm  ::[Bool]
    } deriving (Show)

updateFState::FState -> FState
updateFState = undefined

data OrderBook = OrderBook

type FParser a = StateT FState Parser a

-- |Make StateT s m an instance of Applicative, such that FParser becomes an
-- instance of Applicative.
instance (Monad m) => Applicative (StateT s m) where
    pure = return
    (<*>) = ap

-- |Make StateT s p an instance of Alternative, such that FParser becomes an 
-- instance of Alternative.
instance (Alternative p, MonadPlus p) => Alternative (StateT s p) where
    empty = lift $ empty
    (<|>) = mplus

parseFStream::B.ByteString -> [OrderBook]
parseFStream str = undefined

-- |Parse PreseneceMap.
presenceMap::FParser ()
presenceMap = do
    bs <- anySBEEntity
    -- update state
    put FState {pm = bsToPm bs}

-- |Get a Stopbit encoded entity.
anySBEEntity::FParser B.ByteString
anySBEEntity = lift (takeTill' stopBitSet)

-- |Test wether the stop bit is set of a Char. (Note: Chars are converted to
-- Word8's. 
-- TODO: Is this unsafe?
stopBitSet::Char -> Bool
stopBitSet c = testBit (c2w c) 8

-- |Like takeTill, but takes the matching byte as well.
takeTill'::(Char -> Bool) -> Parser B.ByteString
takeTill' f = do
    str <- takeTill f
    c <- take 1
    return (str `B.append` c)

previousValue::NsName -> OpContext -> Primitive
previousValue = undefined

-- |Constructs a parser out of a field. The FParser monad has underlying type
-- Maybe Primitive, the Nothing constructor represents a NULL value in the
-- FAST stream.
fieldToParser::Field -> FParser (Maybe Primitive)
fieldToParser (IntField f) = intF2P f
fieldToParser (DecField f) = decF2P f
fieldToParser (AsciiStrField f) = asciiStrF2P f
fieldToParser (UnicodeStrField f) = unicodeF2P f
fieldToParser (ByteVecField f) = bytevecF2P f
fieldToParser (Seq s) = seqF2P s
fieldToParser (Grp g) = groupF2P g

-- |Maps an integer field to its parser.
intF2P::IntegerField -> FParser (Maybe Primitive)
-- Every possible case for the Int32 field.
-- pm: No, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) Nothing)) 
    = fmap Just int32

-- pm: No, Nullable: Yes
intF2P (Int32Field (FieldInstrContent _ (Just Optional) Nothing)) 
    = intNULL <|> fmap (Just . minusOne) int32

-- pm: No, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) 
    = return $ Just(ivToInt32 iv)

-- pm: Yes, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv))))) 
    = (notPresent *> (return $ Just(ivToInt32 iv))) <|> fmap Just int32

-- pm: Yes, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Default (Nothing))))) 
    = fail "S5: No initial value given for mandatory default delta operator."

-- pm: Yes, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Copy oc)))) 
    = undefined
    {-= notPresent *> <|> fmap Just int32-}

-- pm: Yes, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Increment iv)))) = undefined 

-- pm: No, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Delta iv)))) = undefined

-- pm: ?, Nullable: ?
intF2P (Int32Field (FieldInstrContent _ (Just Mandatory) (Just (Tail iv)))) = undefined

-- pm: Yes, Nullable: No
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Constant iv)))) 
    = notPresent <|> (return $ Just(ivToInt32 iv))

-- pm: Yes, Nullable: Yes
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Default iv)))) = undefined

-- pm: Yes, Nullable: Yes
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Copy iv)))) = undefined

-- pm: Yes, Nullable: Yes
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Increment iv)))) = undefined

-- pm: No, Nullable: Yes
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Delta iv)))) = undefined

-- pm: ?, Nullable: ?
intF2P (Int32Field (FieldInstrContent _ (Just Optional) (Just (Tail iv)))) = undefined

-- Every possible case for the UInt32 field.
intF2P (UInt32Field (FieldInstrContent _ (Just Mandatory) Nothing)) = fmap Just uint32
-- Every possible case for the Int64 field.
intF2P (Int64Field (FieldInstrContent _ (Just Mandatory) Nothing)) = fmap Just int64
-- Every possible case for the UInt64 field.
intF2P (UInt64Field (FieldInstrContent _ (Just Mandatory) Nothing)) = fmap Just uint64

-- |Maps an decimal field to its parser.
decF2P::DecimalField -> FParser (Maybe Primitive)
decF2P = undefined

-- |Maps an ascii field to its parser.
asciiStrF2P::AsciiStringField -> FParser (Maybe Primitive)
asciiStrF2P = undefined

-- |Maps an unicode field to its parser.
unicodeF2P::UnicodeStringField -> FParser (Maybe Primitive)
unicodeF2P = undefined

-- |Maps a bytevector field to its parser.
bytevecF2P::ByteVectorField -> FParser (Maybe Primitive)
bytevecF2P = undefined

-- |Maps a sequence field to its parser.
seqF2P::Sequence -> FParser (Maybe Primitive)
seqF2P = undefined

-- |Maps a group field to its parser.
groupF2P::Group -> FParser (Maybe Primitive)
groupF2P = undefined

-- *Conversions from initial values.

-- |Convert an initial value to an Int32.
ivToInt32::InitialValueAttr -> Primitive
ivToInt32 = undefined 


-- *Raw Parsers for basic FAST primitives
-- These parsers are unaware of nullability, presence map, deltas etc.

-- |Checks wether a field is NOT present according to presence map.
notPresent::FParser (Maybe Primitive)
notPresent = do
    s <- get
    put (FState (tail (pm s)))
    let pmap = pm s in
        case head pmap of
            True -> fail "Presence bit set."
            False -> return Nothing

-- |NULL parser for integer fields.
intNULL::FParser (Maybe Primitive)
intNULL = lift intNull'
    where intNull' = do 
            char (w2c 0x80)
            return Nothing

-- |UInt32 field parser.
uint32::FParser Primitive
uint32 = do 
    x <- p
    return $ UInt32 x
    where p = checkBounds ui32Range uint

-- |UInt64 field parser.
uint64::FParser Primitive
uint64 = do
    x <- p
    return $ UInt64 x
    where p = checkBounds ui64Range uint

-- |Int32 field parser.
int32::FParser Primitive
int32 = do
    x <- p
    return $ Int32 x
    where p = checkBounds i32Range uint
    

-- |Int64 field parser.
int64::FParser Primitive
int64 = do
    x <- p
    return $ Int64 x
    where p = checkBounds i64Range uint

-- |Unsigned integer parser, doesn't check for bounds.
-- TODO: should we check for R6 errors, i.e overlong fields?
uint::FParser Int
uint = do 
    bs <- anySBEEntity
    return (snd((B.foldl h (0,0) bs)))
    where
        h::(Int,Int) -> Word8 -> (Int,Int)
        h (c,r) w = (c + 1, r + fromEnum((clearBit w 8)) * 2^(7*c))
        
-- |Signed integer parser, doesn't check for bounds.
int::FParser Int
int = do
    bs <- anySBEEntity
    return $ snd(B.foldl h (0,0) bs)
    where 
        h::(Int,Int) -> Word8 -> (Int,Int)
        h (c,r) w = (c + 1, r') 
            where  r' = case (testBit w 8) of
                            True -> case testBit w 7 of
                                        True -> -1 * (r + fromEnum(w .&. 0xc0) * 2^(7*c)) 
                                        False -> r + fromEnum((clearBit w 8)) * 2^(7*c)
                            False -> r + (fromEnum w) * 2^(7*c)

-- |Check wether parsed integer is in given range.
checkBounds::(Int,Int) -> FParser Int -> FParser Int
checkBounds r p = do
    x <- p
    case (inRange r x) of
            True -> return x
            False -> fail "R4: Integer type can not be represented in the target integer type."

-- |ASCII string field parser.
asciiString::FParser Primitive
asciiString = do
    bs <- anySBEEntity
    let bs' = (B.init bs) `B.append` B.singleton (clearBit (B.last bs) 8) in
        return (Ascii (map w2c (B.unpack bs')))
    
-- |Unicode string field parser. The first argument is the size of the string.
unicodeString::Int -> FParser Primitive
unicodeString c = do
    bv <- byteVector c
    let (Bytevector bs) = bv in
        return (Unicode (U.decode (B.unpack bs)))
    
-- |Bytevector size preamble parser.
-- TODO: is it simply a byte or itself a uint32 field?
bvSize::FParser Int
bvSize = lift bvSize'
    where bvSize' = do
            c <- anyChar
            return (ord c)

-- |Bytevector field parser. The first argument is the size of the bytevector.
byteVector::Int -> FParser Primitive
byteVector c = lift p
    where p = fmap Bytevector (take c)

--
-- *Helper functions.
--

-- |Modify underlying bits of a Char.
modBits::Char -> (Word8 -> Word8) -> Char
modBits c f = (w2c . f . c2w) c

-- |Decrement the value of an integer, when it is positive.
minusOne::Primitive -> Primitive
minusOne (Int32 x) | x > 0 = Int32(x - 1)
minusOne (Int64 x) | x > 0 = Int64(x - 1)
minusOne (UInt32 x)| x > 0 = UInt32(x - 1)
minusOne (UInt64 x)| x > 0 = UInt64(x - 1)
minusOne x = x

-- *Presence map related functions

-- |Convert a bytestring into a presence map.
bsToPm::B.ByteString -> [Bool]
bsToPm bs = concat (map h (B.unpack bs)) 
    where   h::Word8 -> [Bool]
            h w = map (testBit w) [1..7] 
            
