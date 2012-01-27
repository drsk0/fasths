{-# LANGUAGE FlexibleContexts, TupleSections, TypeFamilies #-}

import Test.QuickCheck
import Codec.Fast.Data
import Codec.Fast
import Data.ByteString.Char8 (unpack, pack) 
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ListLike as LL
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
                    i <- arbitrary `suchThat` (not . (\x -> isSequence x || isGroup x))
                    return $ ST $ Template  n ns d t [i]

isSequence :: Instruction -> Bool
isSequence (Instruction (Seq _)) = True
isSequence _ = False

isGroup :: Instruction -> Bool
isGroup (Instruction (Grp _)) = True
isGroup _ = False

newtype SimpleTemplateMsgPair = STMP (Template, [(NsName, Maybe Value)]) deriving Show

instance Arbitrary SimpleTemplateMsgPair where
    arbitrary = do 
                    (ST t) <- arbitrary
                    msgs <- listOf $ arbitraryMsgForTemplate t
                    return $ STMP (t, msgs)

newtype Bit7String = B7S String deriving Show
unwrapB7S :: Bit7String -> String
unwrapB7S (B7S s) = s

instance Arbitrary Bit7String where
    arbitrary = fmap (B7S . unpack . B.map (\w -> clearBit w 7) . pack . dropWhile (== '\0')) (arbitrary :: Gen String)
    shrink = (map B7S) . (shrink :: String -> [String]) . unwrapB7S

newtype Bit7Char = B7C Char deriving Show
unwrapB7C :: Bit7Char -> Char
unwrapB7C (B7C c) = c

instance Arbitrary Bit7Char where
    arbitrary = (B7C . toEnum . (\w -> clearBit w 7) . fromEnum) <$> (arbitrary :: Gen Char)
    shrink = (map B7C) . (shrink :: Char -> [Char]) . unwrapB7C

newtype NormalizedDecimal = ND (Int32, Int64) deriving Show
unwrapND :: NormalizedDecimal -> (Int32 , Int64)
unwrapND (ND x) = x

instance Arbitrary NormalizedDecimal where
    arbitrary = fmap (ND . normalize) (arbitrary :: Gen (Int32, Int64))
        where   normalize (_, 0) = (0, 0)
                normalize (e, m) | m `mod` 10 == 0 = normalize (e + 1, m `div` 10)
                normalize (e, m) = (e, m)
    shrink = (map ND) . (shrink :: (Int32, Int64) -> [(Int32, Int64)]) . unwrapND

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
arbitraryValueForField (UnicodeStrField (UnicodeStringField f@(FieldInstrContent fname _ _ ) maybe_len )) = (fname, ) <$> fmap toValue <$> (arbitraryValueForByteVectorField f maybe_len :: Gen (Maybe UnicodeString))
arbitraryValueForField (ByteVecField (ByteVectorField f@(FieldInstrContent fname _ _ ) maybe_len )) = (fname, ) <$> fmap toValue <$> (arbitraryValueForByteVectorField f maybe_len :: Gen (Maybe B.ByteString))
arbitraryValueForField (Seq s) = arbitraryValueForSequence s
arbitraryValueForField (Grp g) = arbitraryValueForGroup g

arbitraryValueForIntField :: (Primitive a, Arbitrary a) => IntegerField -> Gen (Maybe a)
arbitraryValueForIntField (Int32Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (UInt32Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (Int64Field fic) = arbitraryValueForIntField' fic 
arbitraryValueForIntField (UInt64Field fic) = arbitraryValueForIntField' fic 

arbitraryValueForIntField' :: (Primitive a, Arbitrary a) => FieldInstrContent -> Gen (Maybe a)
arbitraryValueForIntField' (FieldInstrContent fname Nothing maybe_op) = arbitraryValueForIntField' (FieldInstrContent fname (Just Mandatory) maybe_op)
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) Nothing) = Just <$> arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) = return $ Just $ ivToPrimitive iv
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing)))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) = Just <$> arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Copy _))) = Just <$> arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) = fmap Just arbitrary
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' (FieldInstrContent _ (Just Optional) (Just (Constant iv))) = oneof [return $ Just (ivToPrimitive iv), return Nothing]
arbitraryValueForIntField' (FieldInstrContent _ (Just Mandatory) (Just (Delta _))) = Just <$> arbitrary
arbitraryValueForIntField' _ = arbitrary


arbitraryValueForDecField :: DecimalField -> Gen (Maybe (Int32, Int64))
arbitraryValueForDecField (DecimalField fname Nothing maybe_either_op) 
    = arbitraryValueForDecField (DecimalField fname (Just Mandatory) maybe_either_op)
arbitraryValueForDecField (DecimalField _ (Just Mandatory) Nothing) = Just <$> arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Default (Just _))))) = Just <$> arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Copy _)))) = Just <$> arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Delta _)))) = Just <$> arbitrary
arbitraryValueForDecField (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Constant iv)))) = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is applicable only to integer fields."
arbitraryValueForDecField (DecimalField _ (Just Optional) (Just (Left (Tail _)))) 
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField (DecimalField fname (Just Optional) (Just (Right (DecFieldOp maybe_exOp maybe_maOp)))) = 
    let fname' = uniqueFName fname "e"
        fname'' = uniqueFName fname "m"
    in
    do 
        e <- arbitraryValueForIntField (Int32Field (FieldInstrContent fname' (Just Optional) maybe_exOp))
        m <- arbitraryValueForIntField (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        case (e, m) of
            (Nothing, Nothing) -> return Nothing
            ((Just e'), (Just m')) -> return (Just (e', m'))
            ((Just e'), Nothing) -> do 
                m' <- arbitrary
                return (Just (e', m'))
            (Nothing, Just m') -> do
                e' <- arbitrary
                return (Just (e', m'))
arbitraryValueForDecField (DecimalField fname (Just Mandatory) (Just (Right (DecFieldOp maybe_exOp maybe_maOp)))) = 
    let fname' = uniqueFName fname "e"
        fname'' = uniqueFName fname "m"
    in
    do 
        e <- arbitraryValueForIntField (Int32Field (FieldInstrContent fname' (Just Mandatory) maybe_exOp))
        m <- arbitraryValueForIntField (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        case (e, m) of
            (Nothing, Nothing) -> return Nothing
            ((Just e'), (Just m')) -> return (Just (e', m'))
            ((Just e'), Nothing) -> do 
                m' <- arbitrary
                return (Just (e', m'))
            (Nothing, Just m') -> do
                e' <- arbitrary
                return (Just (e', m'))
arbitraryValueForDecField _ = arbitrary

arbitraryValueForAsciiField :: AsciiStringField -> Gen (Maybe AsciiString)
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing)) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just _))))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Copy _)))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Delta _)))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Tail _)))) = (Just . (dropWhile (=='\0')) . (map unwrapB7C)) <$> vectorOf 10 arbitrary
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv))))  = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForAsciiField (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Tail _)))) = oneof [return Nothing, (Just . (dropWhile (=='\0')) . (map unwrapB7C)) <$> vectorOf 10 arbitrary]
arbitraryValueForAsciiField _ = (fmap unwrapB7S) <$> arbitrary

arbitraryValueForByteVectorField :: (Primitive a, Arbitrary a, LL.ListLike a c, Arbitrary c) => FieldInstrContent -> Maybe ByteVectorLength -> Gen (Maybe a)
arbitraryValueForByteVectorField (FieldInstrContent fname Nothing maybe_op) len 
    = arbitraryValueForByteVectorField (FieldInstrContent fname (Just Mandatory) maybe_op) len
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) Nothing ) _ = Just <$> arbitrary
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _  = return $ Just $ ivToPrimitive iv
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _  
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Default (Just _)))) _ = Just <$> arbitrary
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Copy _ ))) _  = Just <$> arbitrary
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _  
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Delta _ ))) _ = Just <$> arbitrary
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Mandatory) (Just(Tail _ ))) _ = (Just . LL.fromList) <$> vectorOf 10 arbitrary
arbitraryValueForByteVectorField (FieldInstrContent _ (Just Optional) (Just(Tail _ ))) _ = oneof [return Nothing, (Just . LL.fromList) <$> vectorOf 10 arbitrary]
arbitraryValueForByteVectorField _ _ = arbitrary

arbitraryValueForSequence :: Sequence -> Gen (NsName, Maybe Value)
arbitraryValueForSequence (Sequence fname (Just Optional) _ _ _ instrs) = do
    l <- arbitrary :: Gen Word32
    sq <- vectorOf (fromIntegral l) (mapM arbitraryValueForInstruction instrs)
    oneof [return (fname, Just $ Sq l sq), return (fname, Nothing)]
arbitraryValueForSequence (Sequence fname _ _ _ _ instrs) = do
    l <- arbitrary :: Gen Word32
    sq <- vectorOf (fromIntegral l) (mapM arbitraryValueForInstruction instrs)
    return (fname, Just $ Sq l sq)

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
    {-quickCheck (prop_ftail_dot_ftail__is_ID :: (UnicodeString, UnicodeString) -> Bool)-}

    {-putStr "\n[*] Checking 'rmPreamble . addPreamble = id'\n"-}
    {-quickCheck (prop_rmPreamble_dot_addPreamble_is_ID . (\(NOS s) -> s))-}
    
    {-putStr "\n[*] Checking 'bsToPm . pmToBs = id'\n"-}
    {-quickCheck (prop_bsToPm_dot_pmToBs_is_ID)-}

    {-putStr "\n[*] ivToPrimitive . primitiveToIv  = id'\n"-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Int32 -> Bool)-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Word32 -> Bool)-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Int64 -> Bool)-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Word64 -> Bool)-}
    {--- fails when mantissa overflows.-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: (Int32, Int64) -> Bool)-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: AsciiString -> Bool)-}
    {-quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: B.ByteString -> Bool)-}

    putStr "\n[*] Checking 'decoder . encoder = id'\n"
    quickCheck (prop_decode_template_encode_template_is_ID . (\(STMP p) -> p))
