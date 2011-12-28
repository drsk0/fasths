{-#LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module Codec.Fast.Coparser 
(
_segment',
_initEnv
)
where 

import Codec.Fast.Data
import qualified Data.Map as M
import Data.ByteString.UTF8 (fromString)
import Data.Word
import Data.Int
import Data.Bits
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Binary.Builder as BU

data Env_ = Env_ {
    -- |All known templates.
    templates :: M.Map NsName Template,
    -- |The application needs to define how uint32 values are mapped to template names and vice versa.
    temp2tid :: NsName -> Word32
    }

_initEnv :: Templates -> (NsName -> Word32) -> Env_
_initEnv = undefined

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

_templateIdentifier :: FCoparser (NsName, Maybe Value)
_templateIdentifier (n, v)  = 
    let _p = field2Cop  (IntField (UInt32Field (FieldInstrContent 
                        (NsName (NameAttr "templateId") Nothing Nothing) 
                        (Just Mandatory) 
                        (Just (Copy (OpContext (Just (DictionaryAttr "global")) (Just (NsKey(KeyAttr (Token "tid")) Nothing)) Nothing)))
                        )))
   in
   do
       env <- ask
       tid <- _p (n, (Just . UI32 . temp2tid env) n)
       msg <- template2Cop (templates env M.! n) (n, v)
       return (tid `BU.append` msg)

_presenceMap :: FCoparser ()
_presenceMap () = do
    st <- get
    return $ _anySBEEntity (pmToBs $ pm st)

pmToBs :: [Bool] -> B.ByteString
pmToBs xs = B.pack (map h (sublistsOfLength 7 xs))
    where   h :: [Bool] -> Word8
            h = fst . foldl (\(r,n) y -> if y then (setBit r n, n-1) else (r, n-1)) (0, 6)

sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength _ [] = []
sublistsOfLength n xs = take n xs : sublistsOfLength n (drop n xs)

template2Cop :: Template -> FCoparser (NsName, Maybe Value)
template2Cop t = f
    where f (_, Just (Gr g)) = sequenceD (map instr2Cop (tInstructions t)) g
          f (_, _) = throw $ OtherException "Template doesn't fit message."
          -- TODO: Are there cases that shoudn't trigger an exception?
        

instr2Cop :: Instruction -> FCoparser (NsName, Maybe Value)
instr2Cop (Instruction f) = field2Cop f 
instr2Cop (TemplateReference (Just trc)) = \(n, v) -> do
    env <- ask
    template2Cop (templates env M.! NsName name Nothing Nothing) (n, v) where (TemplateReferenceContent name _) = trc
instr2Cop (TemplateReference Nothing) = _segment'

field2Cop :: Field -> FCoparser (NsName, Maybe Value)
field2Cop (IntField f@(Int32Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Int32))
field2Cop (IntField f@(Int64Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Int64))
field2Cop (IntField f@(UInt32Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Word32))
field2Cop (IntField f@(UInt64Field (FieldInstrContent fname _ _))) = contramap (fmap fromValue . assertNameIs fname) (intF2Cop f :: FCoparser (Maybe Word64))
field2Cop (DecField f@(DecimalField fname _ _ )) = contramap (fmap fromValue . assertNameIs fname) (decF2Cop f)
field2Cop (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = contramap (fmap fromValue . assertNameIs fname) (asciiStrF2Cop f)
field2Cop (UnicodeStrField f@(UnicodeStringField (FieldInstrContent fname _ _ ) _ )) = contramap (fmap fromValue . assertNameIs fname) (unicodeF2Cop f)
field2Cop (ByteVecField f@(ByteVectorField (FieldInstrContent fname _ _ ) _ )) = contramap (fmap fromValue . assertNameIs fname) (bytevecF2Cop f)
field2Cop (Seq s) = contramap (assertNameIs (sFName s)) (seqF2Cop s)
field2Cop (Grp g) = contramap (assertNameIs (gFName g)) (groupF2Cop g)

assertNameIs :: NsName -> (NsName, a) -> a
assertNameIs n1 (n2, x) = if n1 == n2 then x else throw $ OtherException "Template doesn't fit message."

intF2Cop :: (Primitive a, Num a, Ord a, Ord (Delta a), Num (Delta a)) => IntegerField -> FCoparser (Maybe a)
intF2Cop (Int32Field fic) = intF2Cop' fic 
intF2Cop (UInt32Field fic) = intF2Cop' fic 
intF2Cop (Int64Field fic) = intF2Cop' fic 
intF2Cop (UInt64Field fic) = intF2Cop' fic 

intF2Cop' :: (Primitive a, Num a, Ord a, Ord (Delta a), Num (Delta a)) => FieldInstrContent -> FCoparser (Maybe a)

-- if the presence attribute is not specified, it is mandatory.
intF2Cop' (FieldInstrContent fname Nothing maybe_op) = intF2Cop' (FieldInstrContent fname (Just Mandatory) maybe_op)

-- pm: No, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Mandatory) Nothing) 
    = cp where cp (Just i) = lift $ return $ encodeP i
               cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: No, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) Nothing)
    = cp where cp (Just i) = lift $ return $ encodeP $ plusOne i
               cp (Nothing) = nulL

-- pm: No, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Constant _)))
    = \_ -> lift $ return $ BU.empty

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _))))
    = cp where cp (Just i) = (lift $ setPMap True) >> (lift $ return $ encodeP i)
               cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

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
                                                h' (OpContext _ _ Nothing) = lift $ setPMap True >> updatePrevValue fname oc (Assigned (witnessType i)) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)

               cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent fname (Just Mandatory) (Just (Increment oc)))
    = cp where cp (Just i) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if (assertType v) == i - 1
                                                    then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) = if (ivToPrimitive iv) == i 
                                                                                then (lift $ setPMap False) >> (lift $ return BU.empty) 
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)

               cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: -, Nullable: -
intF2Cop' (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 

-- pm: -, Nullable: -
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 

-- pm: Yes, Nullable: No
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Constant iv)))
    = cp where cp (Just i) | i == ivToPrimitive iv = (lift $ setPMap True) >> (lift $ return BU.empty)
               cp (Just _) = throw $ OtherException "Template doesn't fit message."
               cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Default (Just iv))))
    = cp where  cp (Just i) | i == ivToPrimitive iv = (lift $ setPMap False) >> (lift $ return BU.empty)
                cp (Just i) = (lift $ setPMap True) >> (lift $ return $ encodeP $ plusOne i)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent _ (Just Optional) (Just (Default Nothing)))
    = cp where  cp (Just i) = (lift $ setPMap True) >> (return $ encodeP $ plusOne i)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)                   

-- pm: Yes, Nullable: Yes
intF2Cop' (FieldInstrContent fname (Just Optional) (Just (Copy oc)))
    = cp where  cp (Just i) = do 
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if (assertType v == i) 
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) | ivToPrimitive iv == i = (lift $ setPMap False) >> (lift $ return BU.empty)
                                                h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP i)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned $ witnessType i)) >> (lift $ return $ encodeP i)

                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> nulL
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
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) | ivToPrimitive iv == i = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return BU.empty)
                                                h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i) 
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeP i)
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> nulL
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
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeD $ plusOne $ (delta_ i b))
                    cp (Nothing) = throw $ OtherException "Template doesn't fit message." 

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
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType i))) >> (lift $ return $ encodeD $ plusOne $ (delta_ i b))
                    cp (Nothing) = nulL
                    
decF2Cop::DecimalField -> FCoparser (Maybe Decimal)

-- If the presence attribute is not specified, the field is considered mandatory.
decF2Cop (DecimalField fname Nothing maybe_either_op) 
    = decF2Cop (DecimalField fname (Just Mandatory) maybe_either_op)

-- pm: No, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) Nothing)
    = cp where  cp (Just d) = lift $ return $ encodeP d
                cp (Nothing) = throw $ OtherException "Template doesn't fit template."

-- om: No, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) Nothing)
    = cp where cp (Just (e, m)) = lift $ return $ encodeP $ (plusOne e, m)
               cp (Nothing) = nulL

-- pm: No, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Constant _)))) 
    = \_ -> lift $ return BU.empty

-- pm: Yes, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Default (Just iv)))))
    = cp where  cp (Just d) = if (d == ivToPrimitive iv) 
                              then (lift $ setPMap False) >> (lift $ return BU.empty)
                              else (lift $ setPMap True) >> (lift $ return $ encodeP d)
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."


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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

decF2Cop (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 

-- pm: Yes, Nullable: No
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Constant iv)))) 
    = cp where  cp (Just d) =   if ivToPrimitive iv == d
                                then (lift $ setPMap True) >> (lift $ return $ BU.empty)
                                else throw $ OtherException "Template doesn't fit message."
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return $ BU.empty)

-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Default Nothing)))) 
    = cp where  cp (Just d) = (lift $ setPMap True) >> (lift $ return $ encodeP d)
                cp (Nothing) = (lift $ setPMap False) >> (return $ BU.empty)
                -- TODO: why is this field nullable?
            
-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField _ (Just Optional) (Just (Left (Default (Just iv))))) 
    = cp where  cp (Just d) =   if ivToPrimitive iv == d
                                then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP d)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
decF2Cop (DecimalField fname (Just Optional) (Just (Left (Copy oc)))) 
    = cp where  cp (Just d) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == d 
                                                    then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == d
                                                                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeP d)
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
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType d))) >> (lift $ return $ encodeD $ delta_ d (baseValue p))
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
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing))
    = cp where  cp (Just s) = lift $ return $ encodeP s
                cp (Nothing) = throw $ OtherException "Template doesn't fit message." 

-- pm: No, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) Nothing))
    = cp where  cp (Just s) = lift $ return $ encodeP s
                cp (Nothing) = nulL

-- pm: No, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant _)))) 
    = \_ -> lift $ return $ BU.empty

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just iv)))))
    = cp where  cp (Just s) = if ivToPrimitive iv == s 
                              then (lift $ setPMap False) >> (lift $ return $ BU.empty)
                              else (lift $ setPMap True) >> (lift $ return $ encodeP s)
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: Yes, Nullable: No
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv)))) 
    = cp where  cp (Just s) =   if ivToPrimitive iv == s
                                then (lift $ setPMap True) >> (lift $ return BU.empty)
                                else throw $ OtherException "Template doesn't fit message."
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default Nothing))))
    = cp where  cp (Just s) = (lift $ setPMap True) >> (lift $ return $ encodeP s)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty) 
-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Default (Just iv)))))
    = cp where  cp (Just s) =   if ivToPrimitive iv == s
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $  encodeP s)
                cp (Nothing) = (lift $ setPMap True) >> nulL

-- pm: Yes, Nullable: Yes
asciiStrF2Cop (AsciiStringField(FieldInstrContent fname (Just Optional) (Just (Copy oc))))
    = cp where  cp (Just s) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == s
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == s
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                else (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                                h' (OpContext _ _ Nothing) = (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
                                    Empty -> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeP s)
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
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return $ encodeD $ delta_ s (baseValue p))
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
                                                            else (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == s
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType s))) >> (lift $ return BU.empty)
                                                                                        else (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ return $ encodeT $ ftail_ s (baseValue p))
                                            Empty -> (lift $ return $ encodeT $ ftail_ s (baseValue p))
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h oc
                                        where   h (OpContext _ _ (Just _)) = (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

bytevecF2Cop :: ByteVectorField -> FCoparser (Maybe B.ByteString)

bytevecF2Cop (ByteVectorField (FieldInstrContent fname Nothing maybe_op) len) 
    = bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybe_op) len)

-- pm: No, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing ) _ ) 
    = cp where  cp (Just bv) = lift $ return $ encodeP bv
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: No, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Optional) Nothing ) _ ) 
    = cp where  cp (Just bv) = lift $ return $ encodeP bv
                cp (Nothing) = nulL
-- pm: No, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _ ) 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv 
                                then lift $ return BU.empty
                                else throw $ OtherException "Template doesn't fit message."
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ ) 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap True) >> (lift $ return BU.empty)
                                else throw $ OtherException "Template doesn't fit message."
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)

-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _ ) 
    = throw $ S5 "No initial value given for mandatory default operator."

-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default (Just iv)))) _ ) 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP bv)
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: Yes, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default Nothing))) _ ) 
    = cp where  cp (Just bv) = (lift $ setPMap True) >> (lift $ return $ encodeP bv)
                cp (Nothing) = (lift $ setPMap False) >> (lift $ return BU.empty)
-- pm: Yes, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Default (Just iv)))) _ ) 
    = cp where  cp (Just bv) =  if ivToPrimitive iv == bv
                                then (lift $ setPMap False) >> (lift $ return BU.empty)
                                else (lift $ setPMap True) >> (lift $ return $ encodeP bv)
                cp (Nothing) = (lift $ setPMap True) >> nulL
-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Copy oc))) _ ) 
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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."
-- pm: Yes, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Copy oc))) _ ) 
    = cp where  cp (Just bv) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned v) -> if assertType v == bv
                                                    then (lift $ setPMap False) >> (lift $ return BU.empty)
                                                    else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just iv)) =  if ivToPrimitive iv == bv
                                                                                then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                else (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                                    Empty -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeP bv)
                cp (Nothing) = do 
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) ->  (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                    Undefined -> h' oc
                                        where   h' (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h' (OpContext _ _ Nothing) = (lift $ setPMap False) >> (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)
-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _ ) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
-- pm: Yes, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ ) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 

-- pm: No, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Delta oc))) _ ) 
    = cp where  cp (Just bv) = let  baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                               in
                                do
                                    p <- lift $ prevValue fname oc
                                    lift $ return $ encodeD $ delta_ bv (baseValue p)
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: No, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Delta oc))) _ ) 
    = cp where  cp (Just bv) = let  baseValue (Assigned p) = assertType p
                                    baseValue (Undefined) = h oc
                                        where   h (OpContext _ _ (Just iv)) = ivToPrimitive iv
                                                h (OpContext _ _ Nothing) = defaultBaseValue
                                    baseValue (Empty) = throw $ D6 "previous value in a delta operator can not be empty."
                                in
                                    do 
                                        p <- lift $ prevValue fname oc
                                        (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return $ encodeD $ delta_ bv (baseValue p))
                cp (Nothing) = nulL


-- pm: Yes, Nullable: No
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Mandatory) (Just(Tail oc))) _ ) 
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
                cp (Nothing) = throw $ OtherException "Template doesn't fit message."

-- pm: Yes, Nullable: Yes
bytevecF2Cop (ByteVectorField (FieldInstrContent fname (Just Optional) (Just(Tail oc))) _) 
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
                                                            else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                            Undefined -> h oc
                                                where   h (OpContext _ _ (Just iv)) =   if ivToPrimitive iv == bv
                                                                                        then (lift $ setPMap False) >> (lift $ updatePrevValue fname oc (Assigned (witnessType bv))) >> (lift $ return BU.empty)
                                                                                        else (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                                        h (OpContext _ _ Nothing) = (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                                            Empty -> (lift $ setPMap True) >> (lift $ return $ encodeT $ ftail_ bv (baseValue p))
                cp (Nothing) = do
                                p <- lift $ prevValue fname oc
                                case p of
                                    (Assigned _) -> (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL 
                                    Undefined -> h oc
                                        where   h (OpContext _ _ (Just _)) = (lift $ setPMap True) >> (lift $ updatePrevValue fname oc Empty) >> nulL
                                                h (OpContext _ _ Nothing) = (lift $ updatePrevValue fname oc Empty) >> (lift $ return BU.empty)
                                    Empty -> (lift $ setPMap False) >> (lift $ return BU.empty)

-- |Maps an unicode field to its parser.
unicodeF2Cop :: UnicodeStringField -> FCoparser (Maybe UnicodeString)
unicodeF2Cop (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
    = cp where cp maybe_us = bytevecF2Cop (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length) (fromString <$> maybe_us)

nulL :: FBuilder 
nulL = lift $ return $ BU.singleton 0x80

plusOne :: (Ord a, Num a) => a -> a
plusOne x | x >= 0 = x + 1 
plusOne x = x
                            

seqF2Cop :: Sequence -> FCoparser (Maybe Value)
seqF2Cop = undefined
groupF2Cop :: Group -> FCoparser (Maybe Value)
groupF2Cop = undefined
