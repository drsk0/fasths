{-# LANGUAGE TupleSections #-}

-- |A FAST protocoll parser.
module FParser where 

import Prelude as P
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Attoparsec.Char8 as A
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative 
import Data.Bits
import Data.Word (Word8)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (groupBy)
import FAST


-- |State of the parser.
data FState = FState {
    -- |Presence map
    pm          ::[Bool],
    -- |Dictionaries.
    dict        ::M.Map String Dictionary
    }

-- |Environment of the parser.
data FEnv = FEnv {
    -- |All known templates.
    templates   ::M.Map String Template,
    -- |The application needs to define how uint32 values are mapped to template names.
    tid2temp   ::Int -> String
    }

type FParser a = ReaderT FEnv (StateT FState A.Parser) a

data FValue = I Int
              |S String
              |D Double
              |BS B.ByteString
              |Sq Int [[(NsName, Maybe FValue)]]
              |Gr [(NsName, Maybe FValue)]

-- |Maps a fast primitive to its corresponding fast value.
p2FValue::Primitive -> FValue
p2FValue (Int32 i) = I i
p2FValue (UInt32 i) = I i
p2FValue (Int64 i) = I i
p2FValue (UInt64 i) = I i
p2FValue (Ascii s) = S s
p2FValue (Unicode s) = S s
p2FValue (Decimal (Int32 e) (Int64 m)) = D (10^e * fromIntegral m)
p2FValue (Decimal _ _) = error "Decimal is defined only for Int32 exponent, Int64 mantissa."
p2FValue (Bytevector bs) = BS bs

-- |The initial state of the parser depending on the templates.
initState::Templates -> FState
initState ts = FState [] (M.fromList [(k,d) | d@(Dictionary k _) <- concatMap initDicts (tsTemplates ts)])

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
dictOfField (DecField (DecimalField fname (Just Optional) (Right (DecFieldOp opE opM)))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Optional) (Just opE)))) 
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) (Just opM))))
dictOfField (DecField (DecimalField fname (Just Mandatory) (Right (DecFieldOp opE opM)))) = dictOfField (IntField (Int32Field (FieldInstrContent (uniqueFName fname "e") (Just Mandatory) (Just opE))))
    ++ dictOfField (IntField (Int64Field (FieldInstrContent (uniqueFName fname "m") (Just Mandatory) (Just opM))))
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
dictOfOpContext (OpContext Nothing Nothing _) n = ("global", N n, Empty)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) Nothing _) n = (d, N n, Empty)
dictOfOpContext (OpContext Nothing (Just k) _) _ = ("global", K k, Empty)
dictOfOpContext (OpContext (Just (DictionaryAttr d)) (Just k) _) _ = (d, K k, Empty)

-- |The environment of the parser depending on the templates and
-- the tid2temp function provided by the application.
initEnv::Templates -> (Int -> String) -> FEnv
initEnv ts = FEnv (M.fromList [(k,t) | (TemplateNsName (NameAttr k) _ _) <- map tName (tsTemplates ts), t <- tsTemplates ts])

-- |Maps several templates to a list of corresponding parsers.
templates2P::Templates -> [(TemplateNsName, FParser (NsName, Maybe FValue))]
templates2P t = [(n, p) | n <- fmap tName (tsTemplates t), p <- fmap template2P (tsTemplates t)]

-- |Maps a template to its corresponding parser.
-- We treat a template as a group with NsName equal the TemplateNsName.
template2P::Template -> FParser (NsName, Maybe FValue)
template2P t = (tname2fname (tName t), ) <$> Just . Gr <$> mapM instr2P (tInstructions t)

-- |Translates a TemplateNsName into a NsName. Its the same anyway.
tname2fname::TemplateNsName -> NsName
tname2fname (TemplateNsName n (Just (TemplateNsAttr ns)) maybe_id) = NsName n (Just (NsAttr ns)) maybe_id
tname2fname (TemplateNsName n Nothing maybe_id) = NsName n Nothing maybe_id

-- |Maps an instruction to its corresponding parser.
instr2P::Instruction -> FParser (NsName, Maybe FValue)
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
field2Parser::Field -> FParser (NsName, Maybe FValue)
field2Parser (IntField f@(Int32Field (FieldInstrContent fname _ _))) = (fname, ) <$> fmap p2FValue <$> intF2P f
field2Parser (IntField f@(Int64Field (FieldInstrContent fname _ _))) = (fname, ) <$> fmap p2FValue <$> intF2P f
field2Parser (IntField f@(UInt32Field (FieldInstrContent fname _ _))) = (fname, ) <$> fmap p2FValue <$> intF2P f
field2Parser (IntField f@(UInt64Field (FieldInstrContent fname _ _))) = (fname, ) <$> fmap p2FValue <$> intF2P f
field2Parser (DecField f@(DecimalField fname _ _ )) = (fname, ) <$> fmap p2FValue <$> decF2P f
field2Parser (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = (fname, ) <$> fmap p2FValue <$> asciiStrF2P f
field2Parser (UnicodeStrField f@(UnicodeStringField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> fmap p2FValue <$> unicodeF2P f
field2Parser (ByteVecField f@(ByteVectorField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> fmap p2FValue <$> bytevecF2P f
field2Parser (Seq s) = seqF2P s
field2Parser (Grp g) = groupF2P g

-- |Maps an integer field to its parser.
intF2P::IntegerField -> FParser (Maybe Primitive)
-- Delta operator needs delta parser!
intF2P (Int32Field fic@(FieldInstrContent _ _ (Just (Delta _)))) = intF2P'' fic int32Delta ivToUInt32 dfbUInt32
intF2P (Int32Field fic) = intF2P' fic int32 ivToInt32 dfbInt32
intF2P (UInt32Field fic@(FieldInstrContent _ _ (Just (Delta _)))) = intF2P'' fic int32Delta ivToUInt32 dfbUInt32
intF2P (UInt32Field fic) = intF2P' fic uint32 ivToUInt32 dfbUInt32
intF2P (Int64Field fic@(FieldInstrContent _ _ (Just (Delta _)))) = intF2P'' fic int64Delta ivToUInt32 dfbUInt32
intF2P (Int64Field fic) = intF2P' fic int64 ivToInt64 dfbInt64
intF2P (UInt64Field fic@(FieldInstrContent _ _ (Just (Delta _)))) = intF2P'' fic int64Delta ivToUInt32 dfbUInt32
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
intF2P' (FieldInstrContent _ (Just Mandatory) Nothing) intParser _ _
    = Just <$> intParser

-- pm: No, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) Nothing) intParser _ _
    = nULL 
    <|> (Just . minusOne) <$> intParser

-- pm: No, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _ ivToInt _
    = return $ Just(ivToInt iv)

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv)))) intParser ivToInt _
    = (notPresent *> return (Just(ivToInt iv)))
        <|> Just <$> intParser

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))) _ _ _
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Copy oc))) intParser ivToInt _
    =   (notPresent *>  
            (let 
                h (Assigned v) = Just v
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = Just (ivToInt iv)
                            h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                              \for mandatory copy operator with undefined dictionary\
                                                              \value."
                h (Empty) = error "D6: Previous value is empty in madatory copy operator."
            in 
                fmap h (prevValue fname oc)
            )
        )
        <|> (((Assigned <$> p) >>= updatePrevValue fname oc) >> Just <$> p) where p = intParser
                            
-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent fname (Just Mandatory) (Just (Increment oc))) intParser ivToInt _
    = (notPresent *> 
        (let 
            h (Assigned v) = updatePrevValue fname oc (Assigned v') >> return (Just v') where v' = inc v
            h (Undefined) = h' oc
                where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned i) >> return (Just i) where i =ivToInt iv
                        h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context given for\
                                                        \mandatory increment operator with undefined dictionary\
                                                        \value."
            h (Empty) = error "D6: Previous value is empty in mandatory increment operator."
          in
            prevValue fname oc >>= h
        )
    )
    <|> (((Assigned <$> p) >>= updatePrevValue fname oc) >> (Just <$> p)) where p = intParser


    
-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Mandatory) (Just (Tail _))) _ _ _
    = error "S2: Tail operator can not be applied on an integer type field." 

-- pm: Yes, Nullable: No
intF2P' (FieldInstrContent _ (Just Optional) (Just (Constant iv))) _ ivToInt _
    = (notPresent *> return Nothing)
    <|> return (Just(ivToInt iv))

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))) intParser ivToInt _
    = (notPresent *> return (Just $ ivToInt iv))
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
                h (Assigned v) = return (Just v)
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToInt iv)
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing  
                h (Empty) = return Nothing
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> nULL <* updatePrevValue fname oc Empty
        <|> ((Assigned <$> p >>= updatePrevValue fname oc) >> (Just <$> p)) where p = intParser

-- pm: Yes, Nullable: Yes
intF2P' (FieldInstrContent fname (Just Optional) (Just (Increment oc))) intParser ivToInt _
    = (notPresent *> 
        (let 
            h (Assigned v) = updatePrevValue fname oc (Assigned v') >> Just <$> return v' where v' = inc v
            h (Undefined) = h' oc
                where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned i) >> (Just <$> return i) where i = ivToInt iv
                        h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
            h (Empty) = return Nothing
         in
            prevValue fname oc >>= h
        )
    )
    <|> nULL <* (updatePrevValue fname oc Empty >> return Nothing)
    <|> (((Assigned <$> p) >>= updatePrevValue fname oc) >> Just <$> p ) where p = intParser


-- pm: -, Nullable: -
intF2P' (FieldInstrContent _ (Just Optional) (Just (Tail _))) _ _ _
    = error "S2: Tail operator can not be applied on an integer type field." 

-- Don't use intF2P' for delta operators.
intF2P' (FieldInstrContent _ _ (Just (Delta _))) _ _ _ 
    = error "Coding Error: intF2P' can not be applied on delta operatos, use intF2P''."

-- |Maps an integer field with delta operator to a parser, given the field instruction context, the 
-- integer delta parser,a function to convert initial values to the given 
-- primitive and a default base value for default operators.
intF2P''::FieldInstrContent
        -> FParser Delta
        -> (InitialValueAttr -> Primitive)  
        -> Primitive
        -> FParser (Maybe Primitive)

-- pm: No, Nullable: No
intF2P'' (FieldInstrContent fname (Just Mandatory) (Just (Delta oc))) deltaParser ivToInt defaultBaseValue
    = let   baseValue (Assigned p) = p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToInt iv
                        h (OpContext _ _ Nothing) = defaultBaseValue
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

    in
        do 
            d <- deltaParser
            Just <$> (flip  delta d <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
intF2P'' (FieldInstrContent fname (Just Optional) (Just (Delta oc))) deltaParser ivToInt defaultBaseValue
    = nULL
    <|> let     baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToInt iv
                            h (OpContext _ _ Nothing) = defaultBaseValue
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."

        in
            do 
                d <- deltaParser
                Just <$> (flip delta d <$> (baseValue <$> prevValue fname oc))

intF2P'' _ _ _ _
    = error "Coding Error: intF2P'' can only be used for delta operatos."

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
    = (notPresent *> return(Just(ivToDec iv)))
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
                fmap h (prevValue fname oc)
            )
        )
        <|> (((Assigned <$> dec) >>= updatePrevValue fname oc) >> Just <$> dec)

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Mandatory) (Left (Increment _))) 
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
            d <- decDelta
            Just <$> (flip  delta d <$> (baseValue <$> prevValue fname oc))

decF2P (DecimalField _ (Just Mandatory) (Left (Tail _))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- pm: Yes, Nullable: No
decF2P (DecimalField _ (Just Optional) (Left (Constant iv))) 
    = (notPresent *> return Nothing)
    <|> return(Just(ivToDec iv))

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default Nothing))) 
    = (notPresent *> return Nothing)
    <|> nULL
    <|> (Just <$> dec)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Default (Just iv)))) 
    = (notPresent *> return (Just $ ivToDec iv))
    <|> nULL
    <|> (Just <$> dec)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField fname (Just Optional) (Left (Copy oc))) 
    =   (notPresent *>  
            (let 
                h (Assigned p) = return $ Just p
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = return $ Just (ivToDec iv)
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing  
                h (Empty) = return Nothing
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> nULL <* updatePrevValue fname oc Empty
        <|> ((Assigned <$> dec >>= updatePrevValue fname oc) >> Just <$> dec)

-- pm: Yes, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Increment _))) 
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
                d <- decDelta
                Just <$> (flip delta d <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
decF2P (DecimalField _ (Just Optional) (Left (Tail _))) 
    = error "S2:Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- Both operators are handled individually as mandatory operators.
decF2P (DecimalField fname (Just Mandatory) (Right (DecFieldOp ex_op ma_op))) 
-- make fname unique for exponent and mantissa
    = let fname' = uniqueFName fname "e"
          fname'' = uniqueFName fname "m"
    in do 
        e <- (intF2P (Int32Field (FieldInstrContent fname' (Just Mandatory) (Just ex_op))))
        m <- (intF2P (Int64Field (FieldInstrContent fname'' (Just Mandatory) (Just ma_op))))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just (Int32 e')) (Just m') = Just (Decimal (Int32 (checkRange decExpRange e')) m')
                        h _ _ = error "Coding error: expontent must always be of type Int32."


-- The exponent field is considered as an optional field, the mantissa field as a mandatory field.
decF2P (DecimalField fname (Just Optional) (Right (DecFieldOp ex_op ma_op)))
-- make fname unique for exponent and mantissa
    = let fname' = uniqueFName fname "e"
          fname'' = uniqueFName fname  "m"
    in do 
        e <- (intF2P (Int32Field (FieldInstrContent fname' (Just Optional) (Just ex_op))))
        m <- (intF2P (Int64Field (FieldInstrContent fname'' (Just Mandatory) (Just ma_op))))
        return (h e m) where   
                        h Nothing _ = Nothing
                        h _ Nothing = Nothing
                        h (Just (Int32 e')) (Just m') = Just (Decimal (Int32 (checkRange decExpRange e')) m')
                        h _ _ = error "Coding error: expontent must always be of type Int32."

-- |Maps an ascii field to its parser.
asciiStrF2P::AsciiStringField -> FParser (Maybe Primitive)
-- If the presence attribute is not specified, its a mandatory field.
asciiStrF2P (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing))
    = Just <$> asciiString
-- pm: No, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) Nothing))
    = nULL
    <|> do
        str <- asciiString'
        return $ Just str

-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) 
    = return $ Just (ivToAscii iv)

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv)))))
    = notPresent *> return (Just (ivToAscii iv))
    <|> do
        str <- asciiString 
        return $ Just (rmPreamble str)

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Copy oc))))
    =   (notPresent *>  
            (let 
                h (Assigned p) = return (Just p)
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) =  updatePrevValue fname oc (Assigned i) >> return (Just i) where i = ivToAscii iv
                            h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                              \for mandatory copy operator with undefined dictionary\
                                                              \value."
                h (Empty) = error "D6: Previous value is empty in madatory copy operator."
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> (((Assigned <$> byteVector) >>= updatePrevValue fname oc) >> (Just <$> byteVector))

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Delta oc))))
    = let   baseValue (Assigned p) = p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToAscii iv
                        h (OpContext _ _ Nothing) = dfbAscii
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
    in
        do 
            str <- asciiDelta
            Just <$> (flip delta str <$> (baseValue <$> prevValue fname oc))

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Tail oc))))
    = notPresent *> (let    baseValue (Assigned p) = return (Just p)
                            baseValue (Undefined) = h oc
                                where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned i) >> return (Just i) where i = ivToAscii iv
                                        h (OpContext _ _ Nothing) = error "D6: No initial value in operator context\
                                                              \for mandatory tail operator with undefined dictionary\
                                                              \value."
                            baseValue (Empty) = error "D7: previous value in a mandatory tail operator can not be empty."
                    in
                        prevValue fname oc >>= baseValue)
    <|> (let    baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToAscii iv
                            h (OpContext _ _ Nothing) = dfbAscii

                baseValue (Empty) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToAscii iv
                            h (OpContext _ _ Nothing) = dfbAscii
        in
            do
                pva <- (prevValue fname oc)
                t <- asciiTail
                return (Just (baseValue pva `FAST.tail` t)))

-- pm: Yes, Nullable: No
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv)))) 
    = (notPresent *> return Nothing)
    <|> return (Just (ivToAscii iv))

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default Nothing))))
    = (notPresent *> return Nothing)
    <|> nULL
    <|> (Just <$> asciiString')

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))))
    = notPresent *> return (Just (ivToAscii iv))
    <|> nULL
    <|> Just <$> asciiString'

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Copy oc))))
    =   (notPresent *>  
            (let 
                h (Assigned p) = return (Just p)
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned i) >> return (Just i) where i =ivToAscii iv
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
                h (Empty) = return Nothing
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> (nULL *> (updatePrevValue fname oc Empty >> return Nothing))
        <|> (((Assigned <$> asciiString') >>= updatePrevValue fname oc) >> (Just <$> asciiString'))

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Increment _ ))))
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Delta oc))))
    = nULL 
    <|> (let    baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToAscii iv
                            h (OpContext _ _ Nothing) = dfbAscii
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
        in
            do 
                str <- asciiDelta'
                Just <$> (flip delta str <$> (baseValue <$> prevValue fname oc)))

-- pm: Yes, Nullable: Yes
asciiStrF2P (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Tail oc))))
    = notPresent *> (let    baseValue (Assigned p) = return (Just p)
                            baseValue (Undefined) = h oc
                                where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned i) >> return (Just i) where i = ivToAscii iv
                                        h (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
                            baseValue (Empty) = return Nothing
                    in
                        prevValue fname oc >>= baseValue)
    <|> (nULL >> updatePrevValue fname oc Empty >> return Nothing)
    <|> let baseValue (Assigned p) = return p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToAscii iv)
                        h (OpContext _ _ Nothing) = return dfbAscii
            baseValue (Empty) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToAscii iv)
                        h (OpContext _ _ Nothing) = return dfbAscii
        in
            do
                bv <- prevValue fname oc >>= baseValue
                t <- asciiString'
                return (Just (bv `FAST.tail` AsciiTail t))

-- |Maps a bytevector field to its parser.
bytevecF2P::ByteVectorField -> FParser (Maybe Primitive)
bytevecF2P (ByteVectorField (FieldInstrContent fname Nothing maybe_op) len) 
    = bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybe_op) len)

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing ) _ ) 
    = Just <$> byteVector

-- pm: No, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing ) _ ) 
    = nULL
    <|> do
        bv <- byteVector
        return $ Just bv

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _ ) 
    = return $ Just (ivToByteVector iv)

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ ) 
    = (notPresent *> return Nothing)
    <|> return (Just (ivToByteVector iv))

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _ ) 
    = error "S5: No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default (Just iv)))) _ ) 
    = notPresent *> return (Just (ivToByteVector iv))
    <|> do
        bv <- byteVector
        return (Just bv)

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default Nothing))) _ ) 
    = (notPresent *> return Nothing)
    <|> nULL
    <|> (Just <$> byteVector)

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just iv)))) _ ) 
    = notPresent *> return (Just (ivToByteVector iv))
    <|> nULL
    <|> Just <$> byteVector

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _ ) 
    =   (notPresent *>  
            (let 
                h (Assigned p) = return (Just p)
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned bv) >> return (Just bv) where bv = ivToByteVector iv 
                            h' (OpContext _ _ Nothing) = error "D5: No initial value in operator context\
                                                              \for mandatory copy operator with undefined dictionary\
                                                              \value."
                h (Empty) = error "D6: Previous value is empty in madatory copy operator."
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> (((Assigned <$> byteVector) >>= updatePrevValue fname oc) >> (Just <$> byteVector))

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _ ) 
    =   (notPresent *>  
            (let 
                h (Assigned p) = return (Just p)
                h (Undefined) = h' oc
                    where   h' (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned bv) >> return (Just bv) where bv = ivToByteVector iv
                            h' (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
                h (Empty) = return Nothing
            in 
                prevValue fname oc >>= h 
            )
        )
        <|> (nULL *> (updatePrevValue fname oc Empty >> return Nothing))
        <|> (((Assigned <$> byteVector) >>= updatePrevValue fname oc) >> (Just <$> byteVector))

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _ ) 
    = error "S2:Increment operator is only applicable to integer fields." 
-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ ) 
    = error "S2:Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Delta oc))) _ ) 
    = let   baseValue (Assigned p) = p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = ivToByteVector iv
                        h (OpContext _ _ Nothing) = dfbByteVector
            baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
    in
        do 
            bv <- byteVectorDelta
            Just <$> (flip delta bv <$> (baseValue <$> prevValue fname oc))

-- pm: No, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Delta oc))) _ ) 
    = nULL
    <|> (let    baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToByteVector iv
                            h (OpContext _ _ Nothing) = dfbByteVector
                baseValue (Empty) = error "D6: previous value in a delta operator can not be empty."
        in
            do 
                bv <- byteVectorDelta
                Just <$> (flip delta bv <$> (baseValue <$> prevValue fname oc)))

-- pm: Yes, Nullable: No
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Tail oc))) _ ) 
    = notPresent *> (let    baseValue (Assigned p) = return (Just p)
                            baseValue (Undefined) = h oc
                                where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned bv) >> return (Just bv) where bv = ivToByteVector iv
                                        h (OpContext _ _ Nothing) = error "D6: No initial value in operator context\
                                                              \for mandatory tail operator with undefined dictionary\
                                                              \value."
                            baseValue (Empty) = error "D7: previous value in a mandatory tail operator can not be empty."
                    in
                        prevValue fname oc >>= baseValue)
    <|> (let    baseValue (Assigned p) = p
                baseValue (Undefined) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToByteVector iv
                            h (OpContext _ _ Nothing) = dfbByteVector

                baseValue (Empty) = h oc
                    where   h (OpContext _ _ (Just iv)) = ivToAscii iv
                            h (OpContext _ _ Nothing) = dfbByteVector
        in
            do
                pva <- prevValue fname oc
                t <- bytevectorTail
                return (Just(baseValue pva `FAST.tail` t)))

-- pm: Yes, Nullable: Yes
bytevecF2P (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Tail oc))) _ ) 
    = notPresent *> (let    baseValue (Assigned p) = return (Just p)
                            baseValue (Undefined) = h oc
                                where   h (OpContext _ _ (Just iv)) = updatePrevValue fname oc (Assigned bv) >> return (Just bv) where bv = ivToByteVector iv
                                        h (OpContext _ _ Nothing) = updatePrevValue fname oc Empty >> return Nothing
                            baseValue (Empty) = return Nothing
                    in
                        prevValue fname oc >>= baseValue)
    <|> (nULL >> updatePrevValue fname oc Empty >> return Nothing)
    <|> let baseValue (Assigned p) = return p
            baseValue (Undefined) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToByteVector iv)
                        h (OpContext _ _ Nothing) = return dfbByteVector
            baseValue (Empty) = h oc
                where   h (OpContext _ _ (Just iv)) = return (ivToByteVector iv)
                        h (OpContext _ _ Nothing) = return dfbByteVector
        in
            do
                bv <- prevValue fname oc >>= baseValue
                t <- bytevectorTail
                return (Just (bv `FAST.tail` t))

-- |Maps an unicode field to its parser.
unicodeF2P::UnicodeStringField -> FParser (Maybe Primitive)
unicodeF2P (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
    = h <$> bytevecF2P (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
        where   h (Just (Bytevector bv)) = Just (Unicode (U.toString bv))
                h (Just _ ) = error "Coding error: Bytevector parser must parse a bytevector."
                h (Nothing) = Nothing

-- |Maps a sequence field to its parser.
seqF2P::Sequence -> FParser (NsName, Maybe FValue)
seqF2P (Sequence fname maybe_presence _ _ maybe_length instrs) 
    = do 
        i <- (h maybe_presence maybe_length)
        g i
        where   g Nothing = return (fname, Nothing)
                g (Just (Int32 i')) = do
                                        s <- (ask >>= \env -> A.count i' (segmentDep instrs (templates env) >> mapM instr2P instrs))
                                        return (fname, Just (Sq i' s))
                g (Just _) = error "Coding error: Length field for sequences must return an Int32."
                -- get the correct parser for the length field.
                fname' = uniqueFName fname "l" 
                h p Nothing = intF2P (Int32Field (FieldInstrContent fname' p Nothing))
                h p (Just (Length Nothing op)) = intF2P (Int32Field (FieldInstrContent fname' p op))
                h p (Just (Length (Just fn) op)) = intF2P (Int32Field (FieldInstrContent fn p op))
                
-- |Maps a group field to its parser.
groupF2P::Group -> FParser (NsName, Maybe FValue)
groupF2P (Group fname Nothing maybe_dict maybe_typeref instrs)
    = groupF2P (Group fname (Just Mandatory) maybe_dict maybe_typeref instrs)

groupF2P (Group fname (Just Mandatory) _ _ instrs) 
    = ask >>= \env -> (fname,) . Just . Gr <$> (segmentGrp instrs (templates env) >> mapM instr2P instrs)

groupF2P (Group fname (Just Optional) _ _ instrs) 
    = notPresent *> return (fname, Nothing)
    <|> (ask >>= \env -> (fname,) . Just . Gr <$> (segmentGrp instrs (templates env) >> mapM instr2P instrs))

-- *Previous value related functions.

-- |Get previous value.
-- TODO: Is the default key the full uri, or just the local name?
-- TODO: Do I look up values by the name of the key or by namespace/name uri?
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
        Nothing -> error "Could not find specified dictionary/key."
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
    put (FState (pm st) (M.adjust (\(Dictionary n xs) -> Dictionary n (M.adjust (\_ -> v) k xs)) d (dict st)))

-- *Raw Parsers for basic FAST primitives
-- These parsers are unaware of nullability, presence map, deltas etc.

-- |nULL parser.
nULL::FParser (Maybe Primitive)
nULL = lift $ lift nULL'
    where nULL' = do 
            -- discard result.
            _ <- A.char (w2c 0x80)
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
    return (snd (B.foldl h (0,0) bs))
    where
        h::(Int,Int) -> Word8 -> (Int,Int)
        h (c,r) w = (c + 1, r + fromEnum (clearBit w 8) * 2^(7*c))
        
-- |Signed integer parser, doesn't check for bounds.
int::FParser Int
int = do
    bs <- anySBEEntity
    return $ snd(B.foldl h (0,0) bs)
    where 
        h::(Int,Int) -> Word8 -> (Int,Int)
        h (c,r) w = (c + 1, r') 
            where  r' = if testBit w 8 
                        then (  if testBit w 7 
                                then -1 * (r + fromEnum(w .&. 0xc0) * 2^(7*c)) 
                                else r + fromEnum(clearBit w 8) * 2^(7*c))
                        else r + fromEnum w * 2^(7*c)

-- |Check wether parsed integer is in given range.
checkBounds::(Int,Int) -> FParser Int -> FParser Int
checkBounds r p = do
    x <- p
    return (checkRange r x)

-- |ASCII string field parser, non-Nullable.
asciiString::FParser Primitive
asciiString = do
    bs <- anySBEEntity
    let bs' = B.init bs `B.append` B.singleton (clearBit (B.last bs) 8) in
        return (rmPreamble(Ascii (map w2c (B.unpack bs'))))

-- |ASCII string field parser, Nullable.
asciiString'::FParser Primitive
asciiString' = do
    bs <- anySBEEntity
    let bs' = B.init bs `B.append` B.singleton (clearBit (B.last bs) 8) in
        return (rmPreamble'(Ascii (map w2c (B.unpack bs'))))
    
-- |Remove Preamble of an ascii string, non-Nullable situation.
rmPreamble::Primitive -> Primitive
rmPreamble (Ascii ['\0']) = Ascii []
rmPreamble (Ascii ['\0', '\0']) = Ascii "\NUL"
-- overlong string.
rmPreamble (Ascii x) = Ascii (filter (/= '\0') x)
rmPreamble _ = error "Coding error: rmPreamble only applicable for Ascii primitives."

-- |Remove preamble of an ascii string, NULLable situation.
rmPreamble'::Primitive -> Primitive
rmPreamble' (Ascii ['\0','\0']) = Ascii []
rmPreamble' (Ascii ['\0','\0','\0']) = Ascii "\NUL"
-- overlong string.
rmPreamble' (Ascii x) = Ascii (filter (/= '\0') x)
rmPreamble' _ = error "Coding error: rmPreamble' only applicable for Ascii primitives."

-- |Unicode string field parser. The first argument is the size of the string.
unicodeString::FParser Primitive
unicodeString = do
    bv <- byteVector
    let (Bytevector bs) = bv in
        return (Unicode (U.toString bs))
    
-- |Bytevector size preamble parser.
-- TODO: Is it a UInt32 or a UInt64?
byteVector::FParser Primitive
byteVector = do
    s <- uint32
    let (UInt32 s') = s in
            byteVector' s'

-- |Bytevector field parser. The first argument is the size of the bytevector.
byteVector'::Int -> FParser Primitive
byteVector' c = lift $ lift p
    where p = Bytevector <$> A.take c

-- * Delta parsers.
-- |Int32 delta parser.
int32Delta::FParser Delta
int32Delta = Int32Delta <$> int32

-- |Uint32 delta parser.
uint32Delta::FParser Delta
uint32Delta = UInt32Delta <$> int32

-- |Int64 delta parser.
int64Delta::FParser Delta
int64Delta = Int64Delta <$> int64

-- |UInt64 delta parser.
uint64Delta::FParser Delta
uint64Delta = UInt64Delta <$> int64

-- |Decimal delta parser.
decDelta::FParser Delta
decDelta = DecimalDelta <$> dec

-- |Ascii delta parser, non-Nullable.
asciiDelta::FParser Delta
asciiDelta = do
               l <- int32
               str <- asciiString
               return (AsciiDelta l str)
-- |Ascii delta parser, Nullable.
asciiDelta'::FParser Delta
asciiDelta' = do
               l <- int32
               str <- asciiString'
               return (AsciiDelta l str)

-- |Bytevector delta parser.
byteVectorDelta::FParser Delta
byteVectorDelta = do
                    l <- int32
                    bv <- byteVector
                    return (ByteVectorDelta l bv)

-- * Tail parsers.
-- | Ascii tail parser, non-nullable case.
asciiTail::FParser Tail
asciiTail = AsciiTail <$> asciiString

-- | Ascii tail parser, nullable case.
asciiTail'::FParser Tail
asciiTail' = AsciiTail <$> asciiString'

-- | Bytevector tail parser.
bytevectorTail::FParser Tail
bytevectorTail = ByteVectorTail <$> byteVector

-- *Presence map parsers. 

-- |Parse PreseneceMap.
presenceMap::FParser ()
presenceMap = do
    bs <- anySBEEntity
    -- update state
    st <- get
    put (FState (bsToPm bs) (dict st))

-- |Convert a bytestring into a presence map.
bsToPm::B.ByteString -> [Bool]
bsToPm bs = concatMap h (B.unpack bs) 
    where   h::Word8 -> [Bool]
            h w = map (testBit w) [1..7] 

-- |Checks wether a field is NOT present according to presence map.
-- TODO: unsafe head usage here.
notPresent::FParser (Maybe Primitive)
notPresent = do
    s <- get
    put (FState (P.tail (pm s)) (dict s))
    let pmap = pm s in
        if head pmap 
        then fail "Presence bit set."
        else return Nothing

-- |Get a Stopbit encoded entity.
anySBEEntity::FParser B.ByteString
anySBEEntity = lift $ lift (takeTill' stopBitSet)

-- |Like takeTill, but takes the matching byte as well.
takeTill'::(Char -> Bool) -> A.Parser B.ByteString
takeTill' f = do
    str <- A.takeTill f
    c <- A.take 1
    return (str `B.append` c)

-- |Test wether the stop bit is set of a Char. (Note: Chars are converted to
-- Word8's. 
-- TODO: Is this unsafe?
stopBitSet::Char -> Bool
stopBitSet c = testBit (c2w c) 8

-- *Stream parsers.

-- |Parses the beginning of a new segment.
segment::FParser ()
segment = presenceMap

-- |Parses presence map and template identifier.
segment'::FParser (NsName, Maybe FValue)
segment' = presenceMap >> templateIdentifier

-- |Returns newSegment | id, depending on presence bit usage of a group of fields.
segmentDep::[Instruction] -> M.Map String Template -> FParser ()
segmentDep ins ts = when (any (needsPm ts) ins) segment

-- |New segment depending on the instructions in the group, creates a presence map for the group.
segmentGrp::[Instruction] -> M.Map String Template -> FParser ()
segmentGrp ins ts = case any (needsPm ts) ins of
    True -> do
        st <- get
        put (FState pm' (dict st))
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
needsPm ts (Instruction (DecField (DecimalField fname (Just Mandatory) (Right (DecFieldOp opE opM))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Mandatory) (Just opE))))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) (Just opM))))
needsPm ts (Instruction (DecField (DecimalField fname (Just Optional) (Right (DecFieldOp opE opM))))) = needsPm ts insE && needsPm ts insM 
    where   insE = Instruction (IntField (Int32Field (FieldInstrContent fname (Just Optional) (Just opE))))
            insM =  Instruction (IntField (Int64Field (FieldInstrContent fname (Just Mandatory) (Just opM))))
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
templateIdentifier::FParser (NsName, Maybe FValue)
templateIdentifier = do
    (_ , maybe_i) <- p
    case maybe_i of
        (Just (I i')) -> do 
            env <- ask
            template2P $ templates env M.! tid2temp env i'
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
