-- |
-- Module      :  Codec.Fast.Parser
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-#LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module Codec.Fast.Coparser 
(
_segment',
_initEnv,
template2Cop,
)
where 

import Codec.Fast.Data
import qualified Data.Map as M
import Data.Word
import Data.Int
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Binary.Builder as BU

data Env_ = Env_ {
    -- |All known templates.
    templates :: M.Map TemplateNsName Template,
    -- |The application needs to define how uint32 values are mapped to template names and vice versa.
    temp2tid :: TemplateNsName -> Word32
    }

_initEnv :: Templates -> (TemplateNsName -> Word32) -> Env_
_initEnv ts f = Env_ (M.fromList [(tName t,t) | t <- tsTemplates ts]) f

type FBuilder = ReaderT Env_ (State Context) BU.Builder
type FCoparser a = a -> FBuilder

instance Monoid FBuilder where
    mappend mb1 mb2 = do
        b1 <- mb1
        b2 <- mb2
        return (b1 `BU.append` b2)
    mempty = return BU.empty

_segment' :: FCoparser (NsName, Maybe Value)
_segment' (n, v) = do 
    tid <- _templateIdentifier (n, v)
    pmap <- _presenceMap ()
    return $ pmap `BU.append` tid

_segment :: FCoparser ()
_segment = _presenceMap

_templateIdentifier :: FCoparser (NsName, Maybe Value)
_templateIdentifier (n, v)  = 
    let tidname = NsName (NameAttr "templateId") Nothing Nothing 
        _p = field2Cop  (IntField (UInt32Field (FieldInstrContent 
                        tidname 
                        (Just Mandatory) 
                        (Just (Copy (OpContext (Just (DictionaryAttr "global")) (Just (NsKey(KeyAttr (Token "tid")) Nothing)) Nothing)))
                        )))
   in
   do
       env <- ask
       tid <- _p (tidname, (Just . UI32 . temp2tid env . fname2tname) n)
       msg <- template2Cop (templates env M.! (fname2tname n)) (n, v)
       return (tid `BU.append` msg)

_presenceMap :: FCoparser ()
_presenceMap () = do
    st <- get
    put $ Context [] (dict st)
    return $ _anySBEEntity (pmToBs $ pm st)

template2Cop :: Template -> FCoparser (NsName, Maybe Value)
template2Cop t = f
    where f (_, Just (Gr g)) = sequenceD (map instr2Cop (tInstructions t)) g
          f (fname, _) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
          -- TODO: Are there cases that shoudn't trigger an exception?

instr2Cop :: Instruction -> FCoparser (NsName, Maybe Value)
instr2Cop (Instruction f) = field2Cop f 
instr2Cop (TemplateReference (Just trc)) = \(n, v) -> do
    env <- ask
    template2Cop (templates env M.! (tempRefCont2TempNsName trc)) (n, v) 
instr2Cop (TemplateReference Nothing) = _segment'

field2Cop :: Field -> FCoparser (NsName, Maybe Value)
field2Cop (IntField f@(Int32Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Int32))
field2Cop (IntField f@(Int64Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Int64))
field2Cop (IntField f@(UInt32Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Word32))
field2Cop (IntField f@(UInt64Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Word64))
field2Cop (DecField f@(DecimalField fname _ _ )) = contramap (fmap fromValue . assertNameIs fname) (decF2Cop f)
field2Cop (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = contramap (fmap fromValue . assertNameIs fname) (asciiStrF2Cop f)
field2Cop (UnicodeStrField (UnicodeStringField f@(FieldInstrContent fname _ _ ) maybe_len )) = contramap (fmap fromValue . assertNameIs fname) (bytevecF2Cop f maybe_len :: FCoparser (Maybe UnicodeString))
field2Cop (ByteVecField (ByteVectorField f@(FieldInstrContent fname _ _ ) maybe_len )) = contramap (fmap fromValue . assertNameIs fname) (bytevecF2Cop f maybe_len :: FCoparser (Maybe B.ByteString))
field2Cop (Seq s) = contramap (assertNameIs (sFName s)) (seqF2Cop s)
field2Cop (Grp g) = contramap (assertNameIs (gFName g)) (groupF2Cop g)

intF2Cop :: (Primitive a, Num a, Ord a, Ord (Delta a), Num (Delta a)) => IntegerField -> FCoparser (Maybe a)
intF2Cop (Int32Field fic) = intF2Cop' fic 
intF2Cop (UInt32Field fic) = intF2Cop' fic 
intF2Cop (Int64Field fic) = intF2Cop' fic 
intF2Cop (UInt64Field fic) = intF2Cop' fic 

intF2Cop' :: (Primitive a, Num a, Ord a, Ord (Delta a), Num (Delta a)) => FieldInstrContent -> FCoparser (Maybe a)

-- if the presence attribute is not specified, it is mandatory.
intF2Cop' (FieldInstrContent fname Nothing maybe_op) = intF2Cop' (FieldInstrContent fname (Just Mandatory) maybe_op)

-- pm: No, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) Nothing) 
    = cp where cp (Just i) = lift $ return $ encodeP i
               cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: No, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) Nothing)
    = cp where cp (Just i) = lift $ return $ encodeP0 i
               cp (Nothing) = nulL

-- pm: No, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Constant _)))
    = \_ -> lift $ return $ BU.empty

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) (Just (Default (Just iv))))
    = cp where cp (Just i) =    if i == ivToPrimitive iv
                                then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP i)
               cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing)))
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) (Just (Copy oc)))
    = cp where cp (Just i) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if (assertType v) == i 
                                                    then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) = if (ivToPrimitive iv) == i 
                                                                                then (lift $ setPMap False) >> (lift $ return BU.empty) 
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)

               cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) (Just (Increment oc)))
    = cp where cp (Just i) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if (assertType v) == i - 1
                                                    then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) = if (ivToPrimitive iv) == i 
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return BU.empty) 
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)

               cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: "  ++ show fname

-- pm: -, Nullable: -
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 

-- pm: -, Nullable: -
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Optional) (Just (Constant iv)))
    = cp where cp (Just i) | i == ivToPrimitive iv = (lift $ setPMap True) >> (lift $ return BU.empty)
               cp (Just _) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
               cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Default (Just iv))))
    = cp where  cp (Just i) | i == ivToPrimitive iv = (lift $ setPMap False) >> (lift $ return BU.empty)
                cp (Just i) = (lift $ setPMap True) >> (lift $ return $ encodeP0 i)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Default Nothing)))
    = cp where  cp (Just i) = (lift $ setPMap True) >> (return $ encodeP0 i)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)                   

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent fname (Just Optional) (Just (Copy oc)))
    = cp where  cp (Just i) = do 
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if (assertType v == i) 
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP0 i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) | ivToPrimitive iv == i = (lift $ setPMap False) >> (lift $ return BU.empty)
                                                h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP0 i)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP0 i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP0 i)

                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> nulL
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return $ BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return $ BU.empty)


-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent fname (Just Optional) (Just (Increment oc)))
    = cp where  cp (Just i) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == i - 1 
                                                    then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP0 i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) | ivToPrimitive iv == i = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return BU.empty)
                                                h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP0 i) 
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP0 i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP0 i)
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL 
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: No, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) (Just (Delta oc)))
    = cp where      cp (Just i) = let   baseValue (Assigned p) = assertType p
                                        baseValue (Undefined) = h oc
                                            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                    h (OpContext _ _ Nothing) = defaultBaseValue
                                        baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                  in                               
                                      do 
                                        b <- lift $ baseValue <$> (prevValue fname oc)
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeD $ (delta_ i b))
                    cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: No, Nullable: Yes
intF2Cop' (FieldInstrContent fname (Just Optional) (Just (Delta oc)))
    = cp where      cp (Just i) = let   baseValue (Assigned p) = assertType p
                                        baseValue (Undefined) = h oc
                                            where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                    h (OpContext _ _ Nothing) = defaultBaseValue
                                        baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                  in                               
                                      do 
                                        b <- lift $ baseValue <$> (prevValue fname oc)
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeD0 (delta_ i b))
                    cp (Nothing) = nulL
                    
decF2Cop::DecimalField -> FCoparser (Maybe Decimal)

-- If the presence attribute is not specified, the field is considered mandatory.
decF2Cop (DecimalField fname Nothing maybe_either_op) 
    = decF2Cop (DecimalField fname (Just Mandatory) maybe_either_op)

-- pm: No, Nullable: No
decF2Cop (DecimalField fname (Just Mandatory) Nothing)
    = cp where  cp (Just d) = lift $ return $ encodeP d
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit template, in the field: " ++ show fname

-- om: No, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) Nothing)
    = cp where cp (Just d) = lift $ return $ encodeP0 d
               cp (Nothing) = nulL

-- pm: No, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Constant _)))) 
    = \_ -> lift $ return BU.empty

-- pm: Yes, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
decF2Cop (DecimalField fname (Just Mandatory) (Just (Left (Default (Just iv)))))
    = cp where  cp (Just d) = if (d == ivToPrimitive iv) 
                              then (lift $ setPMap False) >> (lift $ return BU.empty)
                              else (lift $ setPMap True) >> (lift $ return $ encodeP d)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
decF2Cop (DecimalField fname (Just Mandatory) (Just (Left (Copy oc)))) 
    = cp where  cp (Just d) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == d 
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == d 
                                                                                then lift $ return BU.empty
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname


-- pm: Yes, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
decF2Cop (DecimalField fname (Just Mandatory) (Just (Left (Delta oc)))) 
    = cp where  cp (Just d) =   let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."

                                in 
                                    do
                                        p <- lift $ prevValue fname oc
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeD $ delta_ d (baseValue p)) 
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- pm: Yes, Nullable: No
decF2Cop (DecimalField fname (Just Optional) (Just (Left (Constant iv)))) 
    = cp where  cp (Just d) =   if ivToPrimitive iv == d
                                then (lift $ setPMap True) >> (lift $ return $ BU.empty)
                                else throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Default Nothing)))) 
    = cp where  cp (Just d) = (lift $ setPMap True) >> (lift $ return $ encodeP0 d)
                cp (Nothing) = (lift $ setPMap False) >> (return $ BU.empty)
                -- TODO: why is this field nullable?
            
-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Default (Just iv))))) 
    = cp where  cp (Just d) =   if ivToPrimitive iv == d
                                then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP0 d)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField fname (Just Optional) (Just (Left (Copy oc)))) 
    = cp where  cp (Just d) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == d 
                                                    then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP0 d)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == d
                                                                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP0 d)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP0 d)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP0 d)
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty)  >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)


-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is applicable only to integer fields."

-- pm: No, Nullable: Yes
decF2Cop (DecimalField fname (Just Optional) (Just (Left (Delta oc)))) 
    = cp where  cp (Just d) =   let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                in
                                    do
                                        p <- lift $ prevValue fname oc
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeD0 $ delta_ d (baseValue p))
                cp (Nothing) = nulL

-- pm: No, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Tail _)))) 
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- Both operators are handled individually as mandatory operators.
decF2Cop (DecimalField fname (Just Mandatory) (Just (Right (DecFieldOp maybe_exOp maybe_maOp)))) 
    = cp where cp (Just (e, m)) =   let fname' = uniqueFName fname "e"
                                        fname'' = uniqueFName fname "m" 
                                    in
                                        do 
                                            be <- intF2Cop (Int32Field (FieldInstrContent fname' (Just Mandatory) maybe_exOp)) (Just e) 
                                            bm <- intF2Cop (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp)) (Just m)
                                            lift $ return $ be `BU.append` bm
               cp (Nothing) =   let fname' = uniqueFName fname "e"
                                    fname'' = uniqueFName fname "m" 
                                in
                                    do 
                                        be <- ((intF2Cop (Int32Field (FieldInstrContent fname' (Just Mandatory) maybe_exOp))) :: FCoparser (Maybe Int32))  Nothing 
                                        bm <- ((intF2Cop (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))) :: FCoparser (Maybe Int64)) Nothing 
                                        lift $ return $ be `BU.append` bm


-- The exponent field is considered as an optional field, the mantissa field as a mandatory field.
decF2Cop (DecimalField fname (Just Optional) (Just (Right (DecFieldOp maybe_exOp maybe_maOp))))
    = cp where cp (Just (e, m)) =   let fname' = uniqueFName fname "e"
                                        fname'' = uniqueFName fname "m" 
                                    in
                                        do 
                                            be <- intF2Cop (Int32Field (FieldInstrContent fname' (Just Optional) maybe_exOp)) (Just e) 
                                            bm <- intF2Cop (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp)) (Just m)
                                            lift $ return $ be `BU.append` bm
               cp (Nothing) =   let fname' = uniqueFName fname "e"
                                    fname'' = uniqueFName fname "m" 
                                in
                                    do 
                                        be <- ((intF2Cop (Int32Field (FieldInstrContent fname' (Just Optional) maybe_exOp))) :: FCoparser (Maybe Int32))  Nothing 
                                        bm <- ((intF2Cop (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))) :: FCoparser (Maybe Int64)) Nothing 
                                        lift $ return $ be `BU.append` bm

asciiStrF2Cop :: AsciiStringField -> FCoparser (Maybe AsciiString)

-- If the presence attribute is not specified, its a mandatory field.
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
-- pm: No, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) Nothing))
    = cp where  cp (Just s) = lift $ return $ encodeP s
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: No, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) Nothing))
    = cp where  cp (Just s) = lift $ return $ encodeP0 s
                cp (Nothing) = nulL

-- pm: No, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant _)))) 
    = \_ -> lift $ return $ BU.empty

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Default (Just iv)))))
    = cp where  cp (Just s) = if ivToPrimitive iv == s 
                              then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                              else (lift $ setPMap True) >> (lift $ return $ encodeP s)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Copy oc))))
    = cp where  cp (Just s) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == s
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == s
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = throw $ S2 "Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Delta oc))))
    = cp where  cp (Just s) = let   baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue 
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                              in
                                do
                                    p <- lift $ prevValue fname oc
                                    (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeD $ delta_ s (baseValue p))
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field " ++ show fname

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Mandatory) (Just (Tail oc))))
    = cp where  cp (Just s) =   let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue

                                    baseValue (Empty) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                in
                                    do
                                        p <- lift $ prevValue fname oc
                                        case p of
                                            (Assigned v) -> if assertType v == s
                                                            then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                            else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == s
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                        else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                            Empty -> (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ s (baseValue p))
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Constant iv)))) 
    = cp where  cp (Just s) =   if ivToPrimitive iv == s
                                then (lift $ setPMap True) >> (lift $ return BU.empty)
                                else throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default Nothing))))
    = cp where  cp (Just s) = (lift $ setPMap True) >> (lift $ return $ encodeP0 s)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty) 
-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))))
    = cp where  cp (Just s) =   if ivToPrimitive iv == s
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $  encodeP0 s)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Copy oc))))
    = cp where  cp (Just s) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == s
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP0 s)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == s
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP0 s)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP0 s)
                                    Empty -> (lift $ setPMap True) >>(lift $ setPMap True) >>  (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP0 s)
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Increment _ ))))
    = throw $ S2 "Increment operator is only applicable to integer fields." 


-- pm: No, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Delta oc))))
    = cp where  cp (Just s) =   let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                in
                                    do 
                                        p <- lift $ prevValue fname oc
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeD0 $ delta_ s (baseValue p))
                cp (Nothing) = nulL

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Tail oc))))
    = cp where  cp (Just s) =   let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                in
                                    do
                                        p <- lift $ prevValue fname oc
                                        case p of
                                            (Assigned v) -> if assertType v == s
                                                            then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                            else (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ s (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == s
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                        else (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ s (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ s (baseValue p))
                                            Empty -> (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ s (baseValue p))
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h oc
                                        where   h (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

bytevecF2Cop :: (Primitive a, Eq a) => FieldInstrContent -> Maybe ByteVectorLength -> FCoparser (Maybe a)

bytevecF2Cop (FieldInstrContent fname Nothing maybe_op) len 
    = bytevecF2Cop (FieldInstrContent fname (Just Mandatory) maybe_op) len

-- pm: No, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) Nothing ) _ 
    = cp where  cp (Just bv) = lift $ return $ encodeP bv
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: No, Nullable: Yes
bytevecF2Cop (FieldInstrContent _ (Just Optional) Nothing ) _ 
    = cp where  cp (Just bv) = lift $ return $ encodeP0 bv
                cp (Nothing) = nulL
-- pm: No, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) (Just (Constant iv))) _ 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv 
                                then lift $ return BU.empty
                                else throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Optional) (Just(Constant iv))) _ 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap True) >> (lift $ return BU.empty)
                                else throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _ 
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) (Just(Default (Just iv)))) _ 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP bv)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: Yes
bytevecF2Cop (FieldInstrContent _ (Just Optional) (Just(Default Nothing))) _ 
    = cp where  cp (Just bv) = (lift $ setPMap True) >> (lift $ return $ encodeP0 bv)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)
-- pm: Yes, Nullable: Yes
bytevecF2Cop (FieldInstrContent _ (Just Optional) (Just(Default (Just iv)))) _ 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP0 bv)
                cp (Nothing) = (lift $ setPMap True) >> nulL
-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _ 
    = cp where  cp (Just bv) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == bv
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                    Undefined ->  h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == bv
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
-- pm: Yes, Nullable: Yes
bytevecF2Cop (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _ 
    = cp where  cp (Just bv) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == bv
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP0 bv)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == bv
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP0 bv)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP0 bv)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP0 bv)
                cp (Nothing) = do 
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) ->  (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)
-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _ 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
-- pm: Yes, Nullable: Yes
bytevecF2Cop (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ 
    = throw $ S2 "Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) (Just(Delta oc))) _ 
    = cp where  cp (Just bv) = let  baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                               in
                                do
                                    p <- lift $ prevValue fname oc
                                    lift $ return $ encodeD $ delta_ bv (baseValue p)
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: No, Nullable: Yes
bytevecF2Cop (FieldInstrContent fname (Just Optional) (Just(Delta oc))) _ 
    = cp where  cp (Just bv) = let  baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                in
                                    do 
                                        p <- lift $ prevValue fname oc
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeD0 $ delta_ bv (baseValue p))
                cp (Nothing) = nulL


-- pm: Yes, Nullable: No
bytevecF2Cop (FieldInstrContent fname (Just Mandatory) (Just(Tail oc))) _ 
    = cp where  cp (Just bv) = let  baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue

                                    baseValue (Empty) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                in
                                    do
                                        p <- lift $ prevValue fname oc
                                        case p of
                                            (Assigned v) -> if assertType v == bv
                                                            then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                            else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == bv
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                        else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                            Empty -> (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

-- pm: Yes, Nullable: Yes
bytevecF2Cop  (FieldInstrContent fname (Just Optional) (Just(Tail oc))) _ 
    = cp where  cp (Just bv) =  let baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                in
                                    do
                                        p <- lift $ prevValue fname oc
                                        case p of
                                            (Assigned v) -> if assertType v == bv
                                                            then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                            else (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ bv (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == bv
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                        else (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ bv (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ bv (baseValue p))
                                            Empty -> (lift $ setPMap True) >> (lift $ return $ encodeT0 $ ftail_ bv (baseValue p))
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL 
                                    Undefined -> h oc
                                        where   h (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h (OpContext _ _ Nothing) = (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

seqF2Cop :: Sequence -> FCoparser (Maybe Value)
seqF2Cop (Sequence fname maybe_presence _ _ maybe_length instrs) 
    = cp where cp (Just (Sq w xs)) = let    lengthb = h maybe_presence maybe_length
                                            fname' = uniqueFName fname "l" 
                                            h p Nothing = (intF2Cop (UInt32Field (FieldInstrContent fname' p Nothing)) :: FCoparser (Maybe Word32)) (Just w)
                                            h p (Just (Length Nothing op)) = (intF2Cop (UInt32Field (FieldInstrContent fname' p op)) :: FCoparser (Maybe Word32)) (Just w)
                                            h p (Just (Length (Just fn) op)) = (intF2Cop (UInt32Field (FieldInstrContent fn p op)) :: FCoparser (Maybe Word32)) (Just w)

                                            segs = map (sequenceD $ map instr2Cop instrs) xs
                                            segs' = fmap mconcat $ sequence (map g segs) where g segb = do  
                                                                                                        env <- ask
                                                                                                        if needsSegment instrs (templates env)
                                                                                                        then do
                                                                                                            s <- get
                                                                                                            put $ Context []  (dict s)
                                                                                                            b1 <- segb
                                                                                                            b2 <- _segment ()
                                                                                                            s' <- get
                                                                                                            put $ Context (pm s) (dict s')
                                                                                                            lift $ return (b2 `BU.append` b1)
                                                                                                        else segb
                                    in
                                        do
                                            bu1 <- lengthb
                                            bu2 <- segs'
                                            lift $ return $ (bu1 `BU.append` bu2)

               cp (Just _) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
               -- we need this since we don't have a 'fromValue' function for sequences.
               cp (Nothing) =   lengthb where   lengthb = h maybe_presence maybe_length
                                                fname' = uniqueFName fname "l" 
                                                h p Nothing = (intF2Cop (UInt32Field (FieldInstrContent fname' p Nothing)) :: FCoparser (Maybe Word32)) Nothing 
                                                h p (Just (Length Nothing op)) = (intF2Cop (UInt32Field (FieldInstrContent fname' p op)) :: FCoparser (Maybe Word32)) Nothing 
                                                h p (Just (Length (Just fn) op)) = (intF2Cop (UInt32Field (FieldInstrContent fn p op)) :: FCoparser (Maybe Word32)) Nothing 

groupF2Cop :: Group -> FCoparser (Maybe Value)
groupF2Cop (Group fname Nothing maybe_dict maybe_typeref instrs)
    = groupF2Cop (Group fname (Just Mandatory) maybe_dict maybe_typeref instrs)

groupF2Cop (Group fname (Just Mandatory) _ _ instrs) 
    = cp where  cp  (Just (Gr xs)) = do
                                        env <- ask
                                        if any (needsPm (templates env)) instrs 
                                        then do 
                                                s <- get
                                                put $ Context [] (dict s)
                                                b1 <- (sequenceD (map instr2Cop instrs)) xs
                                                b2 <- _segment ()
                                                s' <- get
                                                put $ Context (pm s) (dict s')
                                                lift $ return (b2 `BU.append` b1)
                                        else (sequenceD (map instr2Cop instrs)) xs
                cp (Just _) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname

groupF2Cop (Group fname (Just Optional) _ _ instrs) 
    = cp where  cp  (Just (Gr xs)) = (lift $ setPMap True) >> (do
                                                                env <- ask
                                                                if any (needsPm (templates env)) instrs 
                                                                then do 
                                                                        s <- get
                                                                        put $ Context [] (dict s)
                                                                        b1 <- (sequenceD (map instr2Cop instrs)) xs
                                                                        b2 <- _segment ()
                                                                        s' <- get
                                                                        put $ Context (pm s) (dict s')
                                                                        lift $ return (b2 `BU.append` b1)
                                                                else (sequenceD (map instr2Cop instrs)) xs)
                cp (Just _) = throw $ EncoderException $ "Template doesn't fit message, in the field: " ++ show fname
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

nulL :: FBuilder 
nulL = lift $ return $ BU.singleton 0x80
