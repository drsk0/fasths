{-#LANGUAGE TypeSynonymInstances, FlexibleContexts #-}

module Codec.Fast.Coparser 
(
_segment',
_initEnv
)
where 

import Codec.Fast.Data
import qualified Data.Map as M
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

nulL :: FBuilder 
nulL = lift $ return $ BU.singleton 0x80

plusOne :: (Ord a, Num a) => a -> a
plusOne x | x >= 0 = x + 1 
plusOne x = x
                            

decF2Cop :: DecimalField -> FCoparser (Maybe (Int32, Int64))
decF2Cop = undefined
asciiStrF2Cop :: AsciiStringField -> FCoparser (Maybe AsciiString)
asciiStrF2Cop = undefined
unicodeF2Cop :: UnicodeStringField -> FCoparser (Maybe UnicodeString)
unicodeF2Cop = undefined
bytevecF2Cop :: ByteVectorField -> FCoparser (Maybe B.ByteString)
bytevecF2Cop = undefined
seqF2Cop :: Sequence -> FCoparser (Maybe Value)
seqF2Cop = undefined
groupF2Cop :: Group -> FCoparser (Maybe Value)
groupF2Cop = undefined
