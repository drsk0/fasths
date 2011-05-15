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
import FAST


-- |Proviosonal state of the parser, will be changed according to reference.
data FState = FState {
    -- |bitmap
    pm  ::[Bool]
    } deriving (Show)

data OrderBook = OrderBook

type FParser a = StateT FState Parser a

-- |Make StateT s m an instance of Applicative, such that FParser becomes an
-- instance of Applicative.
{-instance (Monad m) => Applicative (StateT s m) where-}
    {-pure = return-}
    {-(<*>) = ap-}

{--- |Make StateT s p an instance of Alternative, such that FParser becomes an -}
{--- instance of Alternative.-}
{-instance (Alternative p, MonadPlus p) => Alternative (StateT s p) where-}
    {-empty = lift $ empty-}
    {-(<|>) = mplus-}

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
-- Maybe Primitive, the Nothing constructor represents a field that was not
-- present in the stream.
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
intF2P (Int32Field fic) = intF2P' fic int32 ivToInt32 dfbInt32
intF2P (UInt32Field fic) = intF2P' fic uint32 ivToUInt32 dfbUInt32
intF2P (Int64Field fic) = intF2P' fic int64 ivToInt64 dfbInt64
intF2P (UInt64Field fic) = intF2P' fic uint64 ivToUInt64 dfbUInt64

-- |Maps an integer field to a parser, given the field instruction context, the 
-- raw integer parser,a function to convert initial values to the given 
-- primitive and a default base value for default operators.
intF2P'::FieldInstrContent
        -> FParser Primitive 
        -> (InitialValueAttr -> Primitive)  
        -> Primitive
        -> FParser (Maybe Primitive)

-- Every possible case for the Int32 field.

-- if the presence attribute is not specified, it is mandatory.
intF2P' (FieldInstrContent fname Nothing maybe_op) intParser ivToInt defaultBaseValue 
    = intF2P' (FieldInstrContent fname (Just Mandatory) maybe_op) intParser ivToInt defaultBaseValue
-- pm: No, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) Nothing) intParser ivToInt _
    = Just <$> intParser

-- pm: No, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) Nothing) intParser ivToInt _
    = nULL 
    <|> (Just . minusOne) <$> intParser

-- pm: No, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) intParser ivToInt _
    = return $ Just(ivToInt iv)

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv)))) intParser ivToInt _
    = (notPresent *> (return $ Just(ivToInt iv))) 
        <|> Just <$> intParser

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default (Nothing)))) _ _ _
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Copy oc))) intParser ivToInt _
    =   (notPresent *>  
            (let 
                h (Assigned p) = Just p
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = Just (ivToInt iv)
                            h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                              \for mandatory copy operator with undefined dictionary\
                                                              \value."
                h (Empty) = error "D6: Previous value is empty in madatory copy operator."
            in 
                (prevValue fname oc) >>= return . h 
            )
        )
        <|> Just <$> ((Assigned <$> intParser) >>= updatePrevValue fname oc)
                            
-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Increment oc))) intParser ivToInt _
    = (notPresent *> 
        (let 
            h (Assigned p) = Just <$> (return (Assigned(inc p)) >>= (updatePrevValue fname oc))
            h (Undefined) = h' oc
                where   h' (OpContext _ _ (Just iv)) = Just <$> (return (Assigned(ivToInt iv)) >>= (updatePrevValue fname oc))
                        h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context given for\
                                                        \mandatory increment operator with undefined dictionary\
                                                        \value."
            h (Empty) = error "D6: Previous value is empty in mandatory increment operator."
          in
            (prevValue fname oc) >>= h
        )
    )
    <|> Just <$> ((Assigned <$> (intParser)) >>= updatePrevValue fname oc)


-- pm: No, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Delta oc))) intParser ivToInt defaultBaseValue
    = let   baseValue (Assigned p) = p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToInt iv
                        h (OpContext _ _ Nothing) = defaultBaseValue
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

    in
        do 
            i <- intParser
            Just <$> (((flip  delta) i) <$> (baseValue <$> (prevValue fname oc)))
    
-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Tail iv))) _ _ _
    = error "S2: Tail operator can not be applied on an integer type field." 

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Optional) (Just (Constant iv))) _ ivToInt _
    = (notPresent *> (return $ Nothing))
    <|> (return $ Just(ivToInt iv))

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))) intParser ivToInt _
    = (notPresent *> (return $ Just $ (ivToInt iv)))
    <|> nULL
    <|> (Just <$> intParser)

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) (Just (Default Nothing))) intParser _ _
    = (notPresent *> return Nothing)
    <|> nULL
    <|> (Just <$> intParser)

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Copy oc))) intParser ivToInt _
    =   (notPresent *>  
            (let 
                h (Assigned p) = return $ Just p
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToInt iv)
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> (return $ Nothing)  
                h (Empty) = return $ Nothing
            in 
                (prevValue fname oc) >>= h 
            )
        )
        <|> nULL <* updatePrevValue fname oc Empty
        <|> Just <$> (Assigned <$> intParser >>= updatePrevValue fname oc)

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Increment oc))) intParser ivToInt _
    = (notPresent *> 
        (let 
            h (Assigned p) = Just <$> (return (Assigned(inc p)) >>= (updatePrevValue fname oc))
            h (Undefined) = h' oc
                where   h' (OpContext _ _ (Just iv)) = Just <$> (return (Assigned(ivToInt iv)) >>= (updatePrevValue fname oc))
                        h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> (return $ Nothing) 
            h (Empty) = return Nothing
         in
            (prevValue fname oc) >>= h
        )
    )
    <|> nULL <* updatePrevValue fname oc Empty
    <|> Just <$> ((Assigned <$> (intParser)) >>= updatePrevValue fname oc)

-- pm: No, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Delta oc))) intParser ivToInt defaultBaseValue
    = nULL
    <|> let     baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToInt iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

        in
            do 
                i <- intParser
                Just <$> (((flip  delta) i) <$> (baseValue <$> (prevValue fname oc)))

-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Optional) (Just (Tail iv))) _ _ _
    = error "S2: Tail operator can not be applied on an integer type field." 

-- |Maps an decimal field to its parser.
decF2P::DecimalField -> FParser (Maybe Primitive)

-- If the presence attribute is not specified, the field is considered mandatory.
decF2P (DecimalField fname Nothing either_op) 
    = decF2P (DecimalField fname (Just Mandatory) either_op)

-- pm: No, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Constant iv))) 
    = return $ Just(ivToDec iv)

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Default Nothing))) 
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Default (Just iv)))) 
    = (notPresent *> (return $ Just(ivToDec iv)))
    <|> (Just <$> dec)

-- pm: Yes, Nullable: No
decF2P (DecimalField fname (Just Mandatory) (Left (Copy oc))) 
    =   (notPresent *>  
            (let 
                h (Assigned p) = Just p
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = Just (ivToDec iv)
                            h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                              \for mandatory copy operator with undefined dictionary\
                                                              \value."
                h (Empty) = error "D6: Previous value is empty in madatory copy operator."
            in 
                (prevValue fname oc) >>= return . h 
            )
        )
        <|> Just <$> ((Assigned <$> dec) >>= updatePrevValue fname oc)

-- pm: Yes, Nullable: No
decF2P (DecimalField fname (Just Mandatory) (Left (Increment oc))) 
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
decF2P (DecimalField fname (Just Mandatory) (Left (Delta oc))) 
    = let   baseValue (Assigned p) = p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToDec iv
                        h (OpContext _ _ Nothing) = dfbDecimal
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

    in
        do 
            d <- dec
            Just <$> (((flip  delta) d) <$> (baseValue <$> (prevValue fname oc)))

decF2P (DecimalField _ (Just Mandatory) (Left (Tail oc))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Optional) (Left (Constant iv))) 
    = (notPresent *> (return $ Nothing))
    <|> (return $ Just(ivToDec iv))

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default Nothing))) 
    = (notPresent *> return Nothing)
    <|> nULL
    <|> (Just <$> dec)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default (Just iv)))) 
    = (notPresent *> (return $ Just $ (ivToDec iv)))
    <|> nULL
    <|> (Just <$> dec)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Copy oc))) 
    =   (notPresent *>  
            (let 
                h (Assigned p) = return $ Just p
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToDec iv)
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> (return $ Nothing)  
                h (Empty) = return $ Nothing
            in 
                (prevValue fname oc) >>= h 
            )
        )
        <|> nULL <* updatePrevValue fname oc Empty
        <|> Just <$> (Assigned <$> dec >>= updatePrevValue fname oc)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Increment oc))) 
    = error "S2: Increment operator is applicable only to integer fields."

-- pm: No, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Delta oc))) 
    = nULL
    <|> let     baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToDec iv
                            h (OpContext _ _ Nothing) = dfbDecimal
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

        in
            do 
                d <- dec
                Just <$> (((flip  delta) d) <$> (baseValue <$> (prevValue fname oc)))

-- pm: No, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Tail oc))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- Both operators are handled individually as mandatory operators.
-- TODO: the fname's for exponent and mantissa need to be changed!
decF2P (DecimalField fname (Just Mandatory) (Right (DecFieldOp ex_op ma_op))) 
    = do 
        e <- (intF2P (Int32Field (FieldInstrContent (fname) (Just Mandatory) (Just ex_op))))
        m <- (intF2P (Int64Field (FieldInstrContent (fname) (Just Mandatory) (Just ma_op))))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just (Int32 e')) (Just m') = Just (Decimal (Int32 (checkRange decExpRange e')) m')

-- The exponent field is considered as an optional field, the mantissa field as a mandatory field.
-- TODO: the fname's for exponent and mantissa need to be changed!
decF2P (DecimalField fname (Just Optional) (Right (DecFieldOp ex_op ma_op)))
    = do 
        e <- (intF2P (Int32Field (FieldInstrContent fname (Just Optional) (Just ex_op))))
        m <- (intF2P (Int64Field (FieldInstrContent fname (Just Mandatory) (Just ma_op))))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just (Int32 e')) (Just m') = Just (Decimal (Int32 (checkRange decExpRange e')) m')

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

-- *Previous value related functions.

-- |Get previous value.
prevValue::NsName -> OpContext -> FParser DictValue
prevValue = undefined

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

-- |nULL parser.
nULL::FParser (Maybe Primitive)
nULL = lift nULL'
    where nULL' = do 
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
    
exint32::FParser Primitive 
exint32 = do 
    x <- p
    return $ Int32 x
    where p = checkBounds decExpRange uint

-- |Int64 field parser.
int64::FParser Primitive
int64 = do
    x <- p
    return $ Int64 x
    where p = checkBounds i64Range uint
-- |Dec field parser.
dec::FParser Primitive
dec = do
        e <- exint32 
        m <- int64 
        return (Decimal e m)

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
    return (checkRange r x)

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

-- |Update the previous value.
updatePrevValue::NsName -> OpContext -> DictValue -> FParser Primitive
updatePrevValue n oc p = undefined

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
            
