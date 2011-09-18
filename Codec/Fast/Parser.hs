-- |
-- Module      :  Codec.Fast.Parser
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, GADTs, TypeSynonymInstances, TupleSections, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Codec.Fast.Parser 
(
Context (..),
initState,
initEnv,
segment'
)
where 

import Prelude as P hiding (dropWhile)
import Data.ListLike (dropWhile, genericDrop, genericTake, genericLength)
import Data.Char (digitToInt)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack) 
import qualified Data.ByteString.UTF8 as U
import qualified Data.Attoparsec as A
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative 
import Data.Bits
import Data.Int
import Data.Word 
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import Codec.Fast.Data as F

instance Primitive Int32 where
    newtype Delta Int32 = Di32 Int32 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessI32
    assertType (TypeWitnessI32 i) = i
    assertType _ = error "Type mismatch."
    toValue = I32
    defaultBaseValue = 0 
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di32 i') = i + i'
    ftail = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."
    readP = int
    readD = Di32 <$> int
    readT = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."

instance Primitive Word32 where
    newtype Delta Word32 = Dw32 Int32 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW32
    assertType (TypeWitnessW32 w) = w
    assertType _ = error "Type mismatch."
    toValue = UI32
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw32 i) = fromIntegral (fromIntegral w + i)
    ftail = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."
    readP = uint
    readD = Dw32 <$> int
    readT = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."

instance Primitive Int64 where
    newtype Delta Int64 = Di64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessI64
    assertType (TypeWitnessI64 i) = i
    assertType _ = error "Type mismatch."
    toValue = I64
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta i (Di64 i')= i + i'
    ftail = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."
    readP = int
    readD = Di64 <$> int
    readT = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."

instance Primitive Word64 where
    newtype Delta Word64 = Dw64 Int64 deriving (Num, Ord, Show, Eq)
    witnessType = TypeWitnessW64
    assertType (TypeWitnessW64 w) = w
    assertType _ = error "Type mismatch."
    toValue = UI64
    defaultBaseValue = 0
    ivToPrimitive = read . trimWhiteSpace . text
    delta w (Dw64 i) = fromIntegral (fromIntegral w + i)
    ftail = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 
    readP = uint
    readD = Dw64 <$> int
    readT = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."

instance Primitive AsciiString where
    newtype Delta AsciiString = Dascii (Int32, String)
    witnessType = TypeWitnessASCII
    assertType (TypeWitnessASCII s) = s
    assertType _ = error "Type mismatch."
    toValue = A
    defaultBaseValue = ""
    ivToPrimitive = text
    delta s1 (Dascii (l, s2)) | l < 0 = s2 ++ s1' where s1' = genericDrop (l + 1) s1
    delta s1 (Dascii (l, s2)) | l >= 0 = s1' ++ s2 where s1' = genericTake (genericLength s1 - l) s1
    delta _ _ = error "Type mismatch."
    ftail s1 s2 = take (length s1 - length s2) s1 ++ s2
    readP = asciiString
    readD = do 
                l <- int
                s <- readP
                return (Dascii (l, s))
    readT = readP

{-instance Primitive UnicodeString  where-}
    {-data TypeWitness UnicodeString = TypeWitnessUNI UnicodeString -}
    {-assertType (TypeWitnessUNI s) = s-}
    {-assertType _ = error "Type mismatch."-}
    {-type Delta UnicodeString = (Int32, B.ByteString)-}
    {-defaultBaseValue = ""-}
    {-ivToPrimitive = text-}
    {-delta s d =  B.unpack (delta (B.pack s) d)-}
    {-ftail s1 s2 = B.unpack (ftail (B.pack s1) s2)-}
    {-readP = B.unpack <$> byteVector-}
    {-readD = do-}
                {-l <- int-}
                {-bv <- readP-}
                {-return (l, bv)-}
    {-readT = readP-}

instance Primitive (Int32, Int64) where
    newtype Delta (Int32, Int64) = Ddec (Int32, Int64)
    witnessType = TypeWitnessDec
    assertType (TypeWitnessDec (e, m)) = (e, m)
    assertType _ = error "Type mismatch."
    toValue (e, m) = Dec (fromRational ((toRational m) * 10^^e))
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
    ftail = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."
    readP = do 
        e <- int::A.Parser Int32
        m <- int::A.Parser Int64
        return (e, m)
    readD = Ddec <$> readP
    readT = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields."

instance Primitive B.ByteString where
    newtype Delta B.ByteString = Dbs (Int32, B.ByteString)
    witnessType = TypeWitnessBS
    assertType (TypeWitnessBS bs) = bs
    assertType _ = error "Type mismatch."
    toValue = B 
    defaultBaseValue = B.empty
    ivToPrimitive iv = B.pack (map (toEnum . digitToInt) (filter whiteSpace (text iv)))
    delta bv (Dbs (l, bv')) | l < 0 = bv'' `B.append` bv' where bv'' = genericDrop (l + 1) bv 
    delta bv (Dbs (l, bv')) | l >= 0 = bv'' `B.append` bv' where bv'' = genericTake (genericLength bv - l) bv
    delta _ _ = error "Type mismatch."
    ftail b1 b2 = B.take (B.length b1 - B.length b2) b1 `B.append` b2
    readP = byteVector
    readD = do 
                l <- int
                bv <- readP
                return (Dbs (l, bv))
    readT = readP

-- |State of the parser.
data Context = Context {
    -- |Presence map
    pm   :: [Bool],
    -- |Dictionaries.
    dict :: M.Map String Dictionary
    } deriving (Show)

-- |Environment of the parser.
data Env = Env {
    -- |All known templates.
    templates   ::M.Map String Template,
    -- |The application needs to define how uint32 values are mapped to template names.
    tid2temp   ::Word32 -> String
    }

type FParser a = ReaderT Env (StateT Context A.Parser) a

-- |The initial state of the parser depending on the templates.
initState::Templates -> Context
initState ts = Context [] (M.fromList [(k,d) | d@(Dictionary k _) <- concatMap initDicts (tsTemplates ts)])

-- |Creates a list of dictionaries depending on the fields of a template.
initDicts::Template -> [Dictionary]
initDicts t = createDicts $ catMaybes $ concatMap h (tInstructions t)
    where   h (TemplateReference _) = []
            h (Instruction f) = dictOfField f

-- |Maps triples of the form (DictionaryName, Key, Value) to a list of dictionaries.
createDicts::[(String, DictKey, DictValue)] -> [Dictionary]
createDicts es =  map h (groupBy (\ (d, _ , _) (d', _ , _) -> d P.== d') es)
    where   h xs = Dictionary name (M.fromList (map (\(_,y,z) -> (y,z)) xs))
                where (name, _, _) = head xs

-- |Maps a field to a triple (DictionaryName, Key, Value).
dictOfField::Field -> [Maybe (String, DictKey, DictValue)]
dictOfField (IntField (Int32Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp] 
dictOfField (IntField (Int64Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (IntField (UInt32Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (IntField (UInt64Field (FieldInstrContent fname maybePr maybeOp))) = [dictOfIntField $ FieldInstrContent fname maybePr maybeOp]
dictOfField (DecField (DecimalField fname Nothing eitherOp)) = dictOfField $ DecField $ DecimalField fname (Just Mandatory) eitherOp
dictOfField (DecField (DecimalField _ (Just Mandatory) (Left (Constant _)))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Left (Default Nothing)))) = error "S5: No initial value given for mandatory default operator."
dictOfField (DecField (DecimalField _ (Just Mandatory) (Left (Default (Just _))))) = [Nothing]
dictOfField (DecField (DecimalField fname (Just Mandatory) (Left (Copy oc)))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Left (Increment _)))) = error "S2:Increment operator is only applicable to integer fields." 
dictOfField (DecField (DecimalField fname (Just Mandatory) (Left (Delta oc)))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Mandatory) (Left (Tail _)))) = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 
dictOfField (DecField (DecimalField _ (Just Optional) (Left (Constant _)))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Optional) (Left (Default Nothing)))) = [Nothing]
dictOfField (DecField (DecimalField _ (Just Optional) (Left (Default (Just _))))) = [Nothing]
dictOfField (DecField (DecimalField fname (Just Optional) (Left (Copy oc)))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Optional) (Left (Increment _)))) = error "S2:Increment operator is only applicable to integer fields." 
dictOfField (DecField (DecimalField fname (Just Optional) (Left (Delta oc)))) = [Just $ dictOfOpContext oc fname]
dictOfField (DecField (DecimalField _ (Just Optional) (Left (Tail _)))) = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 
dictOfField (DecField (DecimalField fname (Just Optional) (Right (DecFieldOp maybe_opE maybe_opM)))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Optional) maybe_opE))) 
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) maybe_opM)))
dictOfField (DecField (DecimalField fname (Just Mandatory) (Right (DecFieldOp maybe_opE maybe_opM)))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Mandatory) maybe_opE)))
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) maybe_opM)))
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname Nothing maybeOp))) = dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) maybeOp)))
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) Nothing))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))) = error "S5: No initial value given for mandatory default operator."
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))))) = error "S2:Increment operator is only applicable to integer fields." 
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) (Just (Tail oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) Nothing))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Constant _))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default (Just _)))))) = [Nothing]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Copy oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Increment _))))) = error "S2:Increment operator is only applicable to integer fields." 
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Delta oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Optional) (Just (Tail oc))))) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname Nothing maybeOp) maybe_length)) = dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybeOp) maybe_length))
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Constant _))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))) _)) = error "S5: No initial value given for mandatory default operator."
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just _)))) _)) = [Nothing]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _)) = [Just $ dictOfOpContext oc fname]
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) _)) = error "S2:Increment operator is only applicable to integer fields." 
dictOfField (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _))) _)) = error "S2:Increment operator is only applicable to integer fields." 
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
dictOfIntField (FieldInstrContent _ (Just Mandatory) (Just (Tail _))) = error "S2: Tail operator can not be applied on an integer type field." 
dictOfIntField (FieldInstrContent _ (Just Optional) Nothing) =  Nothing
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Constant _))) = Nothing
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Default _))) = Nothing
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Copy oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Increment oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent fname (Just Optional) (Just (Delta oc))) = Just $ dictOfOpContext oc fname
dictOfIntField (FieldInstrContent _ (Just Optional) (Just (Tail _))) = error "S2: Tail operator can not be applied on an integer type field." 

-- |Outputs a triple (DictionaryName, Key, Value) depending on OpContext and 
-- the NsName of a field.
dictOfOpContext::OpContext -> NsName -> (String, DictKey, DictValue)
dictOfOpContext (OpContext Nothing Nothing _) n = ("global", N n, Undefined)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) Nothing _) n = (d, N n, Undefined)
dictOfOpContext (OpContext Nothing (Just k) _) _ = ("global", K k, Undefined)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) (Just k) _) _ = (d, K k, Undefined)

-- |The environment of the parser depending on the templates and
-- the tid2temp function provided by the application.
initEnv::Templates -> (Word32 -> String) -> Env
initEnv ts f = Env (M.fromList [(h t,t) | t <- tsTemplates ts]) f
    where h (Template (TemplateNsName (NameAttr n) _ _) _ _ _ _) = n

-- |Maps a template to its corresponding parser.
-- We treat a template as a group with NsName equal the TemplateNsName.
template2P::Template -> FParser (NsName, Maybe Value)
template2P t = (tname2fname (tName t), ) <$> Just . Gr <$> mapM instr2P (tInstructions t)

-- |Translates a TemplateNsName into a NsName. Its the same anyway.
tname2fname::TemplateNsName -> NsName
tname2fname (TemplateNsName n (Just (TemplateNsAttr ns)) maybe_id) = NsName n (Just (NsAttr ns)) maybe_id
tname2fname (TemplateNsName n Nothing maybe_id) = NsName n Nothing maybe_id

-- |Maps an instruction to its corresponding parser.
instr2P::Instruction -> FParser (NsName, Maybe Value)
instr2P (Instruction f) = field2Parser f

-- Static template reference.
instr2P (TemplateReference (Just trc)) = do
    env <- ask
    template2P (templates env M.! name) 
    where (TemplateReferenceContent (NameAttr name) _) = trc

-- Dynamic template reference.  
instr2P (TemplateReference Nothing) = segment'

-- |Constructs a parser out of a field. The FParser monad has underlying type
-- Maybe Primitive, the Nothing constructor represents a field that was not
-- present in the stream.
field2Parser::Field -> FParser (NsName, Maybe Value)
field2Parser (IntField f@(Int32Field (FieldInstrContent fname _ _))) = (fname, ) <$> (fmap toValue) <$> (intF2P f :: FParser (Maybe Int32))
field2Parser (IntField f@(Int64Field (FieldInstrContent fname _ _))) = (fname, ) <$> (fmap toValue) <$> (intF2P f :: FParser (Maybe Int64))
field2Parser (IntField f@(UInt32Field (FieldInstrContent fname _ _))) = (fname, ) <$> (fmap toValue) <$> (intF2P f :: FParser (Maybe Word32))
field2Parser (IntField f@(UInt64Field (FieldInstrContent fname _ _))) = (fname, ) <$> (fmap toValue) <$> (intF2P f :: FParser (Maybe Word64))
field2Parser (DecField f@(DecimalField fname _ _ )) = (fname, ) <$> (fmap toValue) <$> decF2P f
field2Parser (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = (fname, ) <$> (fmap toValue) <$> asciiStrF2P f
field2Parser (UnicodeStrField f@(UnicodeStringField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> (fmap toValue) <$> unicodeF2P f
field2Parser (ByteVecField f@(ByteVectorField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> (fmap toValue) <$> bytevecF2P f
field2Parser (Seq s) = seqF2P s
field2Parser (Grp g) = groupF2P g

l2::A.Parser a -> FParser a
l2 = lift . lift

-- |Maps an integer field to its parser.
intF2P :: (Primitive a, Num a, Ord a,  Ord (Delta a), Num (Delta a)) => IntegerField -> FParser (Maybe a)
intF2P (Int32Field fic) = intF2P' fic 
intF2P (UInt32Field fic) = intF2P' fic 
intF2P (Int64Field fic) = intF2P' fic 
intF2P (UInt64Field fic) = intF2P' fic 

intF2P' :: (Primitive a, Num a, Ord a, Ord (Delta a), Num (Delta a)) => FieldInstrContent -> FParser (Maybe a)

-- if the presence attribute is not specified, it is mandatory.
intF2P' (FieldInstrContent fname Nothing maybe_op) = intF2P' (FieldInstrContent fname (Just Mandatory) maybe_op)
-- pm: No, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) Nothing)
    = Just <$> (l2 readP)

-- pm: No, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) Nothing)
    = nULL 
    <|> (Just . minusOne) <$> (l2 readP)

-- pm: No, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))
    = return $ Just(ivToPrimitive iv)

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv))))
    = ifPresentElse (Just <$> (l2 readP)) (return (Just(ivToPrimitive iv)))

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing)))
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Copy oc)))
    = ifPresentElse   
    (do 
        i <- (l2 readP)
        updatePrevValue fname oc (Assigned (witnessType i))
        return (Just i)
    )
    ( 
        let 
            h (Assigned v) = Just (assertType v)
            h (Undefined) = h' oc
                where   h' (OpContext _ _ (Just iv)) = Just (ivToPrimitive iv)
                        h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                          \for mandatory copy operator with undefined dictionary\
                                                          \value."
            h (Empty) = error "D6: Previous value is empty in madatory copy operator."
        in 
            fmap h (prevValue fname oc)
    )
                            
-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Increment oc)))
    = ifPresentElse
    (
    do 
        i <- (l2 readP)
        updatePrevValue fname oc (Assigned (witnessType i))
        return (Just i)
    )
    (
    let 
        h (Assigned v) = updatePrevValue fname oc (Assigned (witnessType v')) >> return (Just v') where v' = (assertType v) + 1
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType i)) >> return (Just i) where i =ivToPrimitive iv
                    h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context given for\
                                                    \mandatory increment operator with undefined dictionary\
                                                    \value."
        h (Empty) = error "D6: Previous value is empty in mandatory increment operator."
      in
        prevValue fname oc >>= h
    )
    
-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = error "S2: Tail operator can not be applied on an integer type field." 

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Optional) (Just (Constant iv)))
    = ifPresentElse (return (Just(ivToPrimitive iv))) (return Nothing)

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) (Just (Default (Just iv))))
    = ifPresentElse (nULL <|> ((Just . minusOne) <$> (l2 readP))) (return (Just $ ivToPrimitive iv))

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) (Just (Default Nothing)))
    = ifPresentElse (nULL <|> ((Just . minusOne) <$> (l2 readP))) (return Nothing)

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Copy oc)))
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> updatePrevValue fname oc Empty >> return Nothing)
    <|> do 
        i <- l2 readP
        updatePrevValue fname oc (Assigned (witnessType i))
        return (Just $ minusOne i)
    )
    (
    let 
        h (Assigned v) = return (Just (assertType v))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToPrimitive iv)
                    h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing  
        h (Empty) = return Nothing
    in 
        prevValue fname oc >>= h 
    )

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Increment oc)))
    = ifPresentElse
    (
    (nULL :: FParser (Maybe Int32))*> (updatePrevValue fname oc Empty >> return Nothing)
    <|> do 
        i <- l2 readP
        updatePrevValue fname oc (Assigned (witnessType i))
        return (Just $ minusOne i)
    )
    (
    let 
        h (Assigned v) = updatePrevValue fname oc (Assigned (witnessType v')) >> Just <$> return v' where v' = (assertType v) + 1
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType i)) >> (Just <$> return i) where i = ivToPrimitive iv
                    h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
        h (Empty) = return Nothing
     in
        prevValue fname oc >>= h
    )


-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = error "S2: Tail operator can not be applied on an integer type field." 

-- pm: No, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Delta oc)))
    = let   baseValue (Assigned p) = assertType p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                        h (OpContext _ _ Nothing) = defaultBaseValue
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

    in
        do 
            d <- l2 readD
            Just <$> (flip  delta d <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Delta oc)))
    = nULL
    <|> let     baseValue (Assigned p) = assertType p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

        in
            do 
                d <- l2 readD
                Just <$> (flip delta (minusOne d) <$> (baseValue <$> prevValue fname oc))

-- |Maps an decimal field to its parser.
decF2P::DecimalField -> FParser (Maybe Decimal)

-- If the presence attribute is not specified, the field is considered mandatory.
decF2P (DecimalField fname Nothing either_op) 
    = decF2P (DecimalField fname (Just Mandatory) either_op)

-- pm: No, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Constant iv))) 
    = return $ Just(ivToPrimitive iv)

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Default Nothing))) 
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Default (Just iv)))) 
    = ifPresentElse (Just <$> l2 readP) (return(Just(ivToPrimitive iv)))

-- pm: Yes, Nullable: No
decF2P (DecimalField fname (Just Mandatory) (Left (Copy oc))) 
    = ifPresentElse
    (
    do
        d <- l2 readP
        updatePrevValue fname oc (Assigned (witnessType d))
        return (Just d)
    )
    (
    let 
        h (Assigned p) = Just (assertType p)
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = Just (ivToPrimitive iv)
                    h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                      \for mandatory copy operator with undefined dictionary\
                                                      \value."
        h (Empty) = error "D6: Previous value is empty in madatory copy operator."
    in 
        fmap h (prevValue fname oc)
    )

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Increment _))) 
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
decF2P (DecimalField fname (Just Mandatory) (Left (Delta oc))) 
    = let   baseValue (Assigned p) = assertType p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                        h (OpContext _ _ Nothing) = defaultBaseValue
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

    in
        do 
            d <- l2 readD
            Just <$> (flip  delta d <$> (baseValue <$> prevValue fname oc))

decF2P (DecimalField _ (Just Mandatory) (Left (Tail _))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Optional) (Left (Constant iv))) 
    = ifPresentElse (return(Just(ivToPrimitive iv))) (return Nothing)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default Nothing))) 
    = ifPresentElse (nULL <|> (Just <$> l2 readP)) (return Nothing)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default (Just iv)))) 
    = ifPresentElse (nULL <|> (Just <$> l2 readP)) (return (Just $ ivToPrimitive iv))

-- pm: Yes, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Copy oc))) 
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> updatePrevValue fname oc Empty >> return Nothing)
    <|> do
            d <- l2 readP
            updatePrevValue fname oc (Assigned (witnessType d))
            return (Just d)
    )
    (
    let 
        h (Assigned p) = return (Just (assertType p))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToPrimitive iv)
                    h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing  
        h (Empty) = return Nothing
    in 
        prevValue fname oc >>= h 
    )

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Increment _))) 
    = error "S2: Increment operator is applicable only to integer fields."

-- pm: No, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Delta oc))) 
    = nULL
    <|> let     baseValue (Assigned p) = assertType p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

        in
            do 
                d <- l2 readD
                Just <$> (flip delta d <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Tail _))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- Both operators are handled individually as mandatory operators.
decF2P (DecimalField fname (Just Mandatory) (Right (DecFieldOp maybe_exOp maybe_maOp))) 
-- make fname unique for exponent and mantissa
    = let fname' = uniqueFName fname "e"
          fname'' = uniqueFName fname "m"
    in do 
        e <- intF2P (Int32Field (FieldInstrContent fname' (Just Mandatory) maybe_exOp))
        m <- intF2P (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just e') (Just m') = Just (e', m')


-- The exponent field is considered as an optional field, the mantissa field as a mandatory field.
decF2P (DecimalField fname (Just Optional) (Right (DecFieldOp maybe_exOp maybe_maOp)))
-- make fname unique for exponent and mantissa
    = let fname' = uniqueFName fname "e"
          fname'' = uniqueFName fname  "m"
    in do 
        e <- intF2P (Int32Field (FieldInstrContent fname' (Just Optional) maybe_exOp))
        m <- intF2P (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just e') (Just m') = Just (e', m')

-- |Maps an ascii field to its parser.
asciiStrF2P::AsciiStringField -> FParser (Maybe AsciiString)
-- If the presence attribute is not specified, its a mandatory field.
asciiStrF2P (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing))
    = Just <$> l2 readP
-- pm: No, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) Nothing))
    = nULL
    <|> do
        str <- l2 readP
        return (Just (rmPreamble' str))

-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) 
    = return $ Just (ivToPrimitive iv)

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv)))))
    = ifPresentElse
    (
    do
        str <- l2 readP
        return $ Just (rmPreamble str)
    )
    (
     return (Just (ivToPrimitive iv))
    )

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Copy oc))))
    = ifPresentElse
    (
    do
            s <- l2 readP
            updatePrevValue fname oc (Assigned (witnessType s))
            return (Just (rmPreamble s))
    )
    (
    let 
        h (Assigned p) = return (Just (assertType p))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) =  updatePrevValue fname oc (Assigned (witnessType i)) >> return (Just i) where i = ivToPrimitive iv
                    h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                      \for mandatory copy operator with undefined dictionary\
                                                      \value."
        h (Empty) = error "D6: Previous value is empty in madatory copy operator."
    in 
        prevValue fname oc >>= h 
    )

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Delta oc))))
    = let   baseValue (Assigned p) = assertType p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                        h (OpContext _ _ Nothing) = defaultBaseValue 
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
    in
        do 
            str <- l2 readD
            Just <$> (flip delta str <$> (baseValue <$> prevValue fname oc))

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Tail oc))))
    = ifPresentElse
    (
    let baseValue (Assigned p) = assertType p
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = defaultBaseValue

        baseValue (Empty) = h oc
            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = defaultBaseValue
    in
        do
            pva <- prevValue fname oc
            t <- l2 readT
            return (Just (baseValue pva `ftail` t))
    )
    (
    let baseValue (Assigned p) = return (Just (assertType p))
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType i)) >> return (Just i) where i = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = error "D6: No initial value in operator context\
                                                              \for mandatory tail operator with undefined dictionary\
                                                              \value."
        baseValue (Empty) = error "D7: previous value in a mandatory tail operator can not be empty."
    in
        prevValue fname oc >>= baseValue
    )

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv)))) 
    = ifPresentElse ( return (Just (ivToPrimitive iv))) (return Nothing)

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default Nothing))))
    = ifPresentElse (nULL <|> (Just . rmPreamble' <$> l2 readP)) (return Nothing)
-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))))
    = ifPresentElse (nULL <|> (Just . rmPreamble' <$> l2 readP)) (return (Just (ivToPrimitive iv)))

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Copy oc))))
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> (updatePrevValue fname oc Empty >> return Nothing))
    <|>
        do 
            s <- rmPreamble' <$> l2 readP
            updatePrevValue fname oc (Assigned (witnessType s))
            return (Just s)
    )
    (
    let 
        h (Assigned p) = return (Just (assertType p))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType i)) >> return (Just i) where i =ivToPrimitive iv
                    h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
        h (Empty) = return Nothing
    in 
        prevValue fname oc >>= h 
    )

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Increment _ ))))
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Delta oc))))
    = nULL 
    <|> (let    baseValue (Assigned p) = assertType p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
        in
            do 
                (Dascii d) <- l2 readD
                Just <$> (flip delta (Dascii (fst d, rmPreamble' (snd d))) <$> (baseValue <$> prevValue fname oc))) 

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Tail oc))))
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> updatePrevValue fname oc Empty >> return Nothing)
    <|> let baseValue (Assigned p) = return (assertType p)
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToPrimitive iv)
                        h (OpContext _ _ Nothing) = return defaultBaseValue
            baseValue (Empty) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToPrimitive iv)
                        h (OpContext _ _ Nothing) = return defaultBaseValue
        in
            do
                bv <- prevValue fname oc >>= baseValue
                t <- rmPreamble' <$> l2 readT
                return (Just (bv `ftail` t))
    )
    (
    let baseValue (Assigned p) = return (Just (assertType p))
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType i)) >> return (Just i) where i = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
        baseValue (Empty) = return Nothing
    in
        prevValue fname oc >>= baseValue
    )

-- |Maps a bytevector field to its parser.
bytevecF2P::ByteVectorField -> FParser (Maybe B.ByteString)
bytevecF2P (ByteVectorField (FieldInstrContent fname Nothing maybe_op) len) 
    = bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybe_op) len)

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing ) _ ) 
    = Just <$> l2 readP

-- pm: No, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing ) _ ) 
    = nULL
    <|> do
        bv <- l2 readP
        return $ Just bv

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _ ) 
    = return $ Just (ivToPrimitive iv)

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ ) 
    = ifPresentElse (return (Just (ivToPrimitive iv))) (return Nothing)
-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _ ) 
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default (Just iv)))) _ ) 
    = ifPresentElse
    (
    do
        bv <- l2 readP
        return (Just bv)
    )
    (
    return (Just (ivToPrimitive iv))
    )

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default Nothing))) _ ) 
    = ifPresentElse (nULL <|> (Just <$> l2 readP)) (return Nothing)
-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just iv)))) _ ) 
    = ifPresentElse (nULL <|> Just <$> l2 readP) (return (Just (ivToPrimitive iv)))
-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _ ) 
    = ifPresentElse   
    (
    do
        bv <- l2 readP 
        updatePrevValue fname oc (Assigned (witnessType bv))
        return (Just bv)
    )
    (
    let 
        h (Assigned p) = return (Just (assertType p))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType bv)) >> return (Just bv) where bv = ivToPrimitive iv 
                    h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                      \for mandatory copy operator with undefined dictionary\
                                                      \value."
        h (Empty) = error "D6: Previous value is empty in madatory copy operator."
    in 
        prevValue fname oc >>= h 
    )
-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _ ) 
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> (updatePrevValue fname oc Empty >> return Nothing))
    <|> do
            bv <- l2 readP
            updatePrevValue fname oc (Assigned (witnessType bv))
            return (Just bv)
    )
    (
    let 
        h (Assigned p) = return (Just (assertType p))
        h (Undefined) = h' oc
            where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType bv)) >> return (Just bv) where bv = ivToPrimitive iv
                    h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
        h (Empty) = return Nothing
    in 
        prevValue fname oc >>= h 
    )
-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _ ) 
    = error "S2:Increment operator is only applicable to integer fields." 
-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ ) 
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Delta oc))) _ ) 
    = let   baseValue (Assigned p) = assertType p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                        h (OpContext _ _ Nothing) = defaultBaseValue
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
    in
        do 
            bv <- l2 readD
            Just <$> (flip delta bv <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Delta oc))) _ ) 
    = nULL
    <|> (let    baseValue (Assigned p) = assertType p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
        in
            do 
                bv <- l2 readD
                Just <$> (flip delta bv <$> (baseValue <$> prevValue fname oc)))

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Tail oc))) _ ) 
    = ifPresentElse
    (
    let baseValue (Assigned p) = assertType p
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = defaultBaseValue

        baseValue (Empty) = h oc
            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = defaultBaseValue
    in
        do
            pva <- prevValue fname oc
            t <- l2 readT
            return (Just(baseValue pva `ftail` t))
    )
    (
    let baseValue (Assigned p) = return (Just (assertType p))
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType bv)) >> return (Just bv) where bv = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = error "D6: No initial value in operator context\
                                              \for mandatory tail operator with undefined dictionary\
                                              \value."
        baseValue (Empty) = error "D7: previous value in a mandatory tail operator can not be empty."
    in
        prevValue fname oc >>= baseValue
    )

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Tail oc))) _ ) 
    = ifPresentElse
    (
    ((nULL :: FParser (Maybe Int32)) *> updatePrevValue fname oc Empty >> return Nothing)
    <|> let baseValue (Assigned p) = return (assertType p)
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToPrimitive iv)
                        h (OpContext _ _ Nothing) = return defaultBaseValue
            baseValue (Empty) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToPrimitive iv)
                        h (OpContext _ _ Nothing) = return defaultBaseValue
        in
            do
                bv <- prevValue fname oc >>= baseValue
                t <- l2 readT
                return (Just (bv `ftail` t))
    )
    (
    let baseValue (Assigned p) = return (Just (assertType p))
        baseValue (Undefined) = h oc
            where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned (witnessType bv)) >> return (Just bv) where bv = ivToPrimitive iv
                    h (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
        baseValue (Empty) = return Nothing
    in
        prevValue fname oc >>= baseValue
    )

-- |Maps an unicode field to its parser.
unicodeF2P::UnicodeStringField -> FParser (Maybe UnicodeString)
unicodeF2P (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
    = h <$> bytevecF2P (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
        where   h (Just bv) = Just (U.toString bv)
                h (Nothing) = Nothing

-- |Maps a sequence field to its parser.
seqF2P::Sequence -> FParser (NsName, Maybe Value)
seqF2P (Sequence fname maybe_presence _ _ maybe_length instrs) 
    = do 
        i <- h maybe_presence maybe_length
        g i
        where   g Nothing = return (fname, Nothing)
                g (Just i') = do
                                        s <- ask >>= \env -> A.count (fromEnum i') (segmentDep instrs (templates env) >> mapM instr2P instrs)
                                        return (fname, Just (Sq i' s))
                -- get the correct parser for the length field.
                fname' = uniqueFName fname "l" 
                h p Nothing = intF2P (UInt32Field (FieldInstrContent fname' p Nothing))
                h p (Just (Length Nothing op)) = intF2P (UInt32Field (FieldInstrContent fname' p op))
                h p (Just (Length (Just fn) op)) = intF2P (UInt32Field (FieldInstrContent fn p op))
                
-- |Maps a group field to its parser.
groupF2P::Group -> FParser (NsName, Maybe Value)
groupF2P (Group fname Nothing maybe_dict maybe_typeref instrs)
    = groupF2P (Group fname (Just Mandatory) maybe_dict maybe_typeref instrs)

groupF2P (Group fname (Just Mandatory) _ _ instrs) 
    = ask >>= \env -> (fname,) . Just . Gr <$> (segmentGrp instrs (templates env) >> mapM instr2P instrs)

groupF2P (Group fname (Just Optional) _ _ instrs) 
    = ifPresentElse 
    (ask >>= \env -> (fname,) . Just . Gr <$> (segmentGrp instrs (templates env) >> mapM instr2P instrs)) 
    (return (fname, Nothing))

-- *Previous value related functions.

-- |Get previous value.
prevValue::NsName -> OpContext -> FParser DictValue
prevValue name (OpContext (Just (DictionaryAttr dname)) Nothing _ ) 
    = pv dname (N name)

prevValue _ (OpContext (Just (DictionaryAttr dname)) (Just dkey) _ ) 
    = pv dname (K dkey)

prevValue name (OpContext Nothing Nothing _ ) 
    = pv "global" (N name)

prevValue _ (OpContext Nothing (Just dkey) _ ) 
    = pv "global" (K dkey)

pv::String -> DictKey -> FParser DictValue
pv d k = do
       st <- get
       case M.lookup d (dict st) >>= \(Dictionary _ xs) -> M.lookup k xs of
        Nothing -> error ("Could not find specified dictionary/key." ++ show d ++ " " ++ show k)
        Just dv -> return dv

-- |Update the previous value.
updatePrevValue::NsName -> OpContext -> DictValue -> FParser ()
updatePrevValue name (OpContext (Just (DictionaryAttr dname)) Nothing _ ) dvalue
    = uppv dname (N name) dvalue

updatePrevValue _ (OpContext (Just (DictionaryAttr dname)) (Just dkey) _ ) dvalue
    = uppv dname (K dkey) dvalue

updatePrevValue name (OpContext Nothing Nothing _ ) dvalue
    = uppv "global" (N name) dvalue

updatePrevValue _ (OpContext Nothing (Just dkey) _ ) dvalue
    = uppv "global" (K dkey) dvalue

uppv::String -> DictKey -> DictValue -> FParser ()
uppv d k v = do
    st <- get
    put (Context (pm st) (M.adjust (\(Dictionary n xs) -> Dictionary n (M.adjust (\_ -> v) k xs)) d (dict st)))

-- *Raw Parsers for basic FAST primitives
-- These parsers are unaware of nullability, presence map, deltas etc.

-- |nULL parser.
nULL :: Primitive a => FParser (Maybe a)
nULL = l2 nULL'
    where nULL' = do 
            -- discard result.
            _ <- A.word8 0x80
            return Nothing

-- |Unsigned integer parser, doesn't check for bounds.
-- TODO: should we check for R6 errors, i.e overlong fields?
uint::(Bits a, Num a) => A.Parser a
uint = do 
    bs <- anySBEEntity
    return (B.foldl h 0 bs)
    where   h::(Bits a, Num a) => a -> Word8 -> a
            h r w = fromIntegral (clearBit w 7) .|. shiftL r 7
        
-- |Signed integer parser, doesn't check for bounds.
int::(Bits a, Num a) => A.Parser a
int = do
    bs <- anySBEEntity
    return (if testBit (B.head bs) 6 
            then B.foldl h (shiftL (-1) 7 .|. fromIntegral (setBit (B.head bs) 7)) (B.tail bs)
            else B.foldl h 0 bs)
    where   
            h::(Bits a, Num a) => a -> Word8 -> a
            h r w = fromIntegral (clearBit w 7) .|. shiftL r 7

-- |ASCII string field parser, non-Nullable.
asciiString::A.Parser AsciiString
asciiString = do
    bs <- anySBEEntity
    let bs' = B.init bs `B.append` B.singleton (clearBit (B.last bs) 8) in
        return (unpack bs')
    
-- |Remove Preamble of an ascii string, non-Nullable situation.
rmPreamble::AsciiString -> AsciiString
rmPreamble (['\0']) = []
rmPreamble (['\0', '\0']) = "\NUL"
-- overlong string.
rmPreamble s = filter (/= '\0') s

-- |Remove preamble of an ascii string, NULLable situation.
rmPreamble'::AsciiString -> AsciiString
rmPreamble' (['\0','\0']) = []
rmPreamble' (['\0','\0','\0']) = "\NUL"
-- overlong string.
rmPreamble' s = filter (/= '\0') s

-- |Bytevector size preamble parser.
byteVector::A.Parser B.ByteString
byteVector = do
    s <- int::A.Parser Word32
    byteVector' s

-- |Bytevector field parser. The first argument is the size of the bytevector.
-- If the length of the bytevector is bigger than maxBound::Int an exception 
-- will be trown.
byteVector'::Word32 -> A.Parser B.ByteString
byteVector' c = A.take (fromEnum c)

-- *Presence map parsers. 

-- |Parse PreseneceMap.
presenceMap::FParser ()
presenceMap = do
    bs <- l2 anySBEEntity
    -- update state
    st <- get
    put (Context (bsToPm bs) (dict st))

-- |Convert a bytestring into a presence map.
bsToPm::B.ByteString -> [Bool]
bsToPm bs = concatMap h (B.unpack bs) 
    where   h::Word8 -> [Bool]
            h w = map (testBit w) [6,5..0] 

ifPresentElse::FParser a -> FParser a -> FParser a
ifPresentElse p1 p2 = do 
                        s <- get
                        put (Context (P.tail (pm s)) (dict s))
                        let pmap = pm s in
                            if head pmap 
                            then p1
                            else p2

-- |Get a Stopbit encoded entity.
anySBEEntity::A.Parser B.ByteString
anySBEEntity = takeTill' stopBitSet

-- |Like takeTill, but takes the matching byte as well.
takeTill'::(Word8 -> Bool) -> A.Parser B.ByteString
takeTill' f = do
    str <- A.takeTill f
    c <- A.take 1
    return (str `B.append` c)

-- |Test wether the stop bit is set of a Char. (Note: Chars are converted to
-- Word8's. 
-- TODO: Is this unsafe?
stopBitSet::Word8 -> Bool
stopBitSet c = testBit c 7

-- *Stream parsers.

-- |Parses the beginning of a new segment.
segment::FParser ()
segment = presenceMap

-- |Parses presence map and template identifier.
segment'::FParser (NsName, Maybe Value)
segment' = presenceMap >> templateIdentifier

-- |Returns newSegment | id, depending on presence bit usage of a group of fields.
segmentDep::[Instruction] -> M.Map String Template -> FParser ()
segmentDep ins ts = when (any (needsPm ts) ins) segment

-- |New segment depending on the instructions in the group, creates a presence map for the group.
segmentGrp::[Instruction] -> M.Map String Template -> FParser ()
segmentGrp ins ts = case any (needsPm ts) ins of
    True -> do
        st <- get
        put (Context pm' (dict st))
        where pm' = replicate (length ins) True
    False -> return ()

-- |Decides wether an instruction uses the presence map or not. We need to know all the templates,
-- to process template reference instructions recursivly.
needsPm::M.Map String Template -> Instruction -> Bool
-- static template reference
needsPm ts (TemplateReference (Just (TemplateReferenceContent (NameAttr n) _))) = all (needsPm ts) (tInstructions t) where t = ts M.! n
-- dynamic template reference
needsPm _ (TemplateReference Nothing) = False
needsPm _ (Instruction (IntField (Int32Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (Int64Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (UInt32Field fic))) = intFieldNeedsPm fic
needsPm _ (Instruction (IntField (UInt64Field fic))) = intFieldNeedsPm fic
needsPm ts (Instruction (DecField (DecimalField fname Nothing eitherOp))) = needsPm ts (Instruction(DecField (DecimalField fname (Just Mandatory) eitherOp)))
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Constant _))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Default _))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Copy _))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Increment _))))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Delta _))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Mandatory) (Left (Tail _))))) = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Constant _))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Default _))))) = True 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Copy _))))) = True
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Increment _))))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Delta _))))) = False
needsPm _ (Instruction (DecField (DecimalField _ (Just Optional) (Left (Tail _))))) = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 
needsPm ts (Instruction (DecField (DecimalField fname (Just Mandatory) (Right (DecFieldOp maybe_opE maybe_opM))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Mandatory) maybe_opE)))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) maybe_opM)))
needsPm ts (Instruction (DecField (DecimalField fname (Just Optional) (Right (DecFieldOp maybe_opE maybe_opM))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Optional) maybe_opE)))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) maybe_opM)))
needsPm ts (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent fname Nothing maybeOp)))) = needsPm ts (Instruction(AsciiStrField (AsciiStringField (FieldInstrContent fname (Just Mandatory) maybeOp))))
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) Nothing)))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Constant _)))))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Default _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Copy _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Increment _)))))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Delta _)))))) =  False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) Nothing)))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Constant _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Default _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Copy _)))))) = True
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Increment _)))))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Delta _)))))) = False
needsPm _ (Instruction (AsciiStrField (AsciiStringField (FieldInstrContent _ (Just Optional) (Just (Tail _)))))) = True
needsPm ts (Instruction (ByteVecField (ByteVectorField (FieldInstrContent fname Nothing maybeOp) maybe_length))) = needsPm ts (Instruction(ByteVecField (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybeOp) maybe_length)))
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Constant _))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))) _))) = error "S5: No initial value given for mandatory default operator."
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just (Default Nothing))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just _)))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Copy _ ))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Copy _ ))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) _))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _))) _))) = error "S2:Increment operator is only applicable to integer fields." 
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Delta _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Delta _))) _))) = False
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Tail _))) _))) = True
needsPm _ (Instruction (ByteVecField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Tail _))) _))) = True
needsPm ts (Instruction (UnicodeStrField (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length))) = needsPm ts (Instruction(ByteVecField (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)))
needsPm ts (Instruction (Seq s)) = all h (sInstructions s)
    where   h (TemplateReference Nothing) = False
            h (TemplateReference (Just (TemplateReferenceContent (NameAttr tname) _))) = all (needsPm ts) (tInstructions (ts M.! tname))
            h f = needsPm ts f
needsPm ts (Instruction (Grp g)) = all h (gInstructions g)
    where   h (TemplateReference Nothing) = False
            h (TemplateReference (Just (TemplateReferenceContent (NameAttr tname) _))) = all (needsPm ts) (tInstructions (ts M.! tname))
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
intFieldNeedsPm (FieldInstrContent _ (Just Mandatory) (Just (Tail _))) = error "S2: Tail operator can not be applied on an integer type field." 
intFieldNeedsPm (FieldInstrContent _ (Just Optional) Nothing) = False
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Constant _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Default _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Copy _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Increment _))) = True
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Delta _))) = False
intFieldNeedsPm (FieldInstrContent _ (Just Optional) (Just (Tail _))) = error "S2: Tail operator can not be applied on an integer type field." 

-- |Parses template identifier and returns the corresponding parser.
-- template identifier is considered a mandatory copy operator UIn32 field.
-- TODO: Check wether mandatory is right.
-- TODO: Which token should this key have, define a policy?
templateIdentifier::FParser (NsName, Maybe Value)
templateIdentifier = do
    (_ , maybe_i) <- p
    case maybe_i of
        (Just (UI32 i')) -> do 
            env <- ask
            template2P (templates env M.! tid2temp env i')
        (Just _) ->  error "Coding error: templateId field must be of type I."
        Nothing -> error "Failed to parse template identifier."

    where p = field2Parser (IntField (UInt32Field (FieldInstrContent 
                            (NsName (NameAttr "templateId") Nothing Nothing) 
                            (Just Mandatory) 
                            (Just (Copy (OpContext (Just (DictionaryAttr "global")) (Just (NsKey(KeyAttr (Token "tid")) Nothing)) Nothing)))
                            )))

-- *Helper functions.
--

-- |Create a unique fname out of a given one and a string.
uniqueFName::NsName -> String -> NsName
uniqueFName fname s = NsName (NameAttr(n ++ s)) ns ide
    where (NsName (NameAttr n) ns ide) = fname

-- |Decrement the value of an integer, when it is positive.
minusOne::(Ord a, Num a) => a -> a
minusOne x | x > 0 = x - 1
minusOne x | x > 0 = x - 1
minusOne x | x > 0 = x - 1
minusOne x | x > 0 = x - 1
minusOne x = x

trimWhiteSpace :: String -> String
trimWhiteSpace = reverse . dropWhile whiteSpace . reverse . dropWhile whiteSpace

whiteSpace::Char -> Bool
whiteSpace c =  c `elem` " \t\r\n"
