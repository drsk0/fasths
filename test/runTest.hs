{-# LANGUAGE FlexibleContexts, TupleSections, TypeFamilies #-}

import Test.QuickCheck
import Codec.Fast.Data
import Codec.Fast
import Data.ByteString.Char8 (unpack, pack) 
import Data.ByteString.UTF8 (toString)
import Data.Bits
import Data.Int
import Data.Word
import Control.Applicative 
import Control.Exception
import qualified Data.ByteString as B

newtype SimpleTemplate = ST Template

instance Arbitrary SimpleTemplate where
    arbitrary = do
                    n <- arbitrary
                    ns <- arbitrary
                    d <- arbitrary
                    t <- arbitrary
                    i <- arbitrary
                    return $ ST $ Template  n ns d t [i]

newtype SimpleTemplateMsgPair = STMP (Template, [(NsName, Maybe Value)]) deriving Show

instance Arbitrary SimpleTemplateMsgPair where
    arbitrary = do 
                    (ST t) <- arbitrary
                    msgs <- listOf $ arbitraryMsgForTemplate t
                    return $ STMP (t, msgs)

newtype Bit7String = B7S String deriving Show

instance Arbitrary Bit7String where
    arbitrary = fmap (B7S . unpack . B.map (\w -> clearBit w 7) . pack) (arbitrary :: Gen String)
    shrink = (map B7S) . (shrink :: String -> [String]) . (\(B7S s) -> s)

newtype NormalizedDecimal = ND (Int32, Int64) deriving Show

instance Arbitrary NormalizedDecimal where
    arbitrary = fmap (ND . normalize) (arbitrary :: Gen (Int32, Int64))
        where   normalize (_, 0) = (0, 0)
                normalize (e, m) | m `mod` 10 == 0 = normalize (e + 1, m `div` 10)
                normalize (e, m) = (e, m)
    shrink = (map ND) . (shrink :: (Int32, Int64) -> [(Int32, Int64)]) . (\(ND d) -> d)

newtype NonOverlongString = NOS AsciiString deriving Show

instance Arbitrary NonOverlongString where
    arbitrary = fmap NOS ((arbitrary :: Gen AsciiString) `suchThat` (not . overlong))
        where 
            overlong :: AsciiString -> Bool
            overlong [] = False
            overlong "\0" = False
            overlong ('\0':_) = True
            overlong _ = False
    shrink = (map NOS) . (shrink :: String -> [String]) . (\(NOS s) -> s)

arbitraryMsgForTemplate :: Template -> Gen (NsName, Maybe Value)
arbitraryMsgForTemplate t = do 
                                vs <- sequence $ map arbitraryValueForInstruction (tInstructions t)
                                return (tname2fname $ tName t, Just $ Gr vs)

arbitraryValueForInstruction :: Instruction -> Gen (NsName, Maybe Value)
arbitraryValueForInstruction (Instruction f) = arbitraryValueForField f
arbitraryValueForInstruction (TemplateReference _) = error "Can't handle template references."

arbitraryValueForField :: Field -> Gen (NsName, Maybe Value)
arbitraryValueForField (IntField f@(Int32Field (FieldInstrContent fname _ _))) = (fname,) <$> fmap toValue <$> ((arbitraryValueForIntField f) :: Gen (Maybe Int32))
arbitraryValueForField (IntField f@(Int64Field (FieldInstrContent fname _ _))) = (fname,) <$> fmap toValue <$> ((arbitraryValueForIntField f) :: Gen (Maybe Int64))
arbitraryValueForField (IntField f@(UInt32Field (FieldInstrContent fname _ _))) = (fname,) <$> fmap toValue <$> ((arbitraryValueForIntField f) :: Gen (Maybe Word32))
arbitraryValueForField (IntField f@(UInt64Field (FieldInstrContent fname _ _))) = (fname,) <$> fmap toValue <$> ((arbitraryValueForIntField f) :: Gen (Maybe Word64))
arbitraryValueForField (DecField f@(DecimalField fname _ _ )) = (fname, ) <$> fmap toValue <$> arbitraryValueForDecField f
arbitraryValueForField (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = (fname, ) <$> fmap toValue <$> arbitraryValueForAsciiField f
arbitraryValueForField (UnicodeStrField f@(UnicodeStringField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> fmap U <$> arbitraryValueForUnicodeField f
arbitraryValueForField (ByteVecField f@(ByteVectorField (FieldInstrContent fname _ _ ) _ )) = (fname, ) <$> fmap toValue <$> arbitraryValueForByteVectorField f
arbitraryValueForField (Seq s) = arbitraryValueForSequence s
arbitraryValueForField (Grp g) = arbitraryValueForGroup g

arbitraryValueForIntField :: (Primitive a, Arbitrary a) => IntegerField -> Gen (Maybe a)
arbitraryValueForIntField (Int32Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (UInt32Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (Int64Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (UInt64Field fic) = arbitraryValueForIntField' fic 

arbitraryValueForIntField' :: (Primitive a, Arbitrary a) => FieldInstrContent -> Gen (Maybe a)
arbitraryValueForIntField' (FieldInstrContent fname Nothing maybe_op) = arbitraryValueForIntField' (FieldInstrContent fname (Just Mandatory) maybe_op)
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) Nothing) = fmap Just arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) = return $ Just $ ivToPrimitive iv
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing)))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Copy _))) = fmap Just arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) = fmap Just arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' (FieldInstrContent _ (Just Optional) (Just (Constant iv))) = oneof [return $ Just (ivToPrimitive iv), return Nothing]
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Delta _))) = fmap Just arbitrary
arbitraryValueForIntField' _ = arbitrary


arbitraryValueForDecField :: DecimalField -> Gen (Maybe (Int32, Int64))
arbitraryValueForDecField (DecimalField fname Nothing maybe_either_op) 
    = arbitraryValueForDecField (DecimalField fname (Just Mandatory) maybe_either_op)
arbitraryValueForDecField (DecimalField _ (Just Mandatory) Nothing) = fmap Just arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Default (Just _))))) = fmap Just arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Copy _)))) = fmap Just arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Delta _)))) = fmap Just arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Constant iv)))) = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is applicable only to integer fields."
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Tail _)))) 
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField _ = arbitrary

arbitraryValueForAsciiField :: AsciiStringField -> Gen (Maybe AsciiString)
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing)) = fmap Just arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just _))))) = fmap Just arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Copy _)))) = fmap Just arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Delta _)))) = fmap Just arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Tail _)))) = fmap Just arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv))))  = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForAsciiField _ = arbitrary

arbitraryValueForUnicodeField :: UnicodeStringField -> Gen (Maybe UnicodeString)
arbitraryValueForUnicodeField (UnicodeStringField (FieldInstrContent fname maybe_presence maybe_op) maybe_length)
    = fmap (fmap toString) (arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent fname maybe_presence maybe_op) maybe_length))

arbitraryValueForByteVectorField :: ByteVectorField -> Gen (Maybe B.ByteString)
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent fname Nothing maybe_op) len) 
    = arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent fname (Just Mandatory) maybe_op) len)
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing ) _ ) = fmap Just arbitrary
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _ ) = return $ Just $ ivToPrimitive iv
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ ) = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _ ) 
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default (Just _)))) _ ) = fmap Just arbitrary
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Copy _ ))) _ ) = fmap Just arbitrary
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _ ) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ ) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Delta _ ))) _ ) = fmap Just arbitrary
arbitraryValueForByteVectorField (ByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Tail _ ))) _ ) = fmap Just arbitrary
arbitraryValueForByteVectorField _ = arbitrary

arbitraryValueForSequence :: Sequence -> Gen (NsName, Maybe Value)
arbitraryValueForSequence (Sequence fname _ _ _ _ instrs) = do
    l <- arbitrary :: Gen Word32
    sq <- vectorOf (fromIntegral l) (mapM arbitraryValueForInstruction instrs)
    oneof [return (fname, Just $ Sq l sq), return (fname, Nothing)]

arbitraryValueForGroup :: Group -> Gen (NsName, Maybe Value)
arbitraryValueForGroup (Group fname Nothing maybe_dict maybe_typeref instrs)
    = arbitraryValueForGroup (Group fname (Just Mandatory) maybe_dict maybe_typeref instrs)
arbitraryValueForGroup (Group fname (Just Mandatory) _ _ instrs) = (fname,) <$> Just . Gr <$> (mapM arbitraryValueForInstruction instrs)
arbitraryValueForGroup (Group fname (Just Optional) _ _ instrs) = (fname,) <$> oneof [Just . Gr <$> (mapM arbitraryValueForInstruction instrs), return Nothing]

main :: IO ()
main = do
    {-putStr "\n[*] Checking 'decodeP . encodeP = id'\n"-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int32 -> Bool)-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word32 -> Bool)-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int64 -> Bool)-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word64 -> Bool)-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: (Int32, Int64) -> Bool)-}
    {-quickCheck (forAll (fmap (addPreamble . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) ((prop_decodeP_dot_encodeP_is_ID) :: AsciiString -> Bool))-}
    {-quickCheck (forAll (fmap (addPreamble' . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) ((prop_decodeP_dot_encodeP_is_ID) :: AsciiString -> Bool))-}
    {-quickCheck (prop_decodeP_dot_encodeP_is_ID :: B.ByteString -> Bool)-}

    {-putStr "\n[*] Checking 'decodeD . encodeD = id'\n"-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int32) -> Bool)-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int64) -> Bool)-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word32) -> Bool)-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word64) -> Bool)-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta (Int32, Int64)) -> Bool)-}
    {-quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta B.ByteString) -> Bool)-}

    {-putStr "\n[*] Checking 'decodeT . encodeT = id'\n"-}
    {-quickCheck (forAll (fmap (addPreamble . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) (prop_decodeT_dot_encodeT_is_ID :: AsciiString -> Bool))-}
    {-quickCheck (prop_decodeT_dot_encodeT_is_ID :: B.ByteString -> Bool)-}

    {-putStr "\n[*] Checking 'fromValue . toValue = id'\n"-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: Int32 -> Bool)-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: Word32 -> Bool)-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: Int64 -> Bool)-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: Word64 -> Bool)-}
    {-quickCheck $ expectFailure (prop_fromValue_dot_toValue_is_ID :: (Int32, Int64) -> Bool)-}
    {--- fails for imprecision of floating point operations.-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: AsciiString -> Bool)-}
    {-quickCheck (prop_fromValue_dot_toValue_is_ID :: B.ByteString -> Bool)-}

    {-putStr "\n[*] Checking 'delta . delta_ = id'\n"-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (Int32, Int32) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (Word32, Word32) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (Int64, Int64) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (Word64, Word64) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: ((Int32, Int64), (Int32, Int64)) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (AsciiString, AsciiString) -> Bool)-}
    {-quickCheck (prop_delta_dot_delta__is_ID :: (B.ByteString, B.ByteString) -> Bool)-}

    {-putStr "\n[*] Checking 'ftail . ftail_ = id'\n"-}
    {-quickCheck (prop_ftail_dot_ftail__is_ID :: (AsciiString, AsciiString) -> Bool)-}
    {-quickCheck (prop_ftail_dot_ftail__is_ID :: (B.ByteString, B.ByteString) -> Bool)-}

    {-putStr "\n[*] Checking 'rmPreamble . addPreamble = id'\n"-}
    {-quickCheck (prop_rmPreamble_dot_addPreamble_is_ID . (\(NOS s) -> s))-}
    
    {-putStr "\n[*] Checking 'bsToPm . pmToBs = id'\n"-}
    {-quickCheck (prop_bsToPm_dot_pmToBs_is_ID)-}

    putStr "\n[*] Checking 'decoder . encoder = id'\n"
    quickCheck ((uncurry $ prop_decode_template_encode_template_is_ID) . (\(STMP p) -> p))
