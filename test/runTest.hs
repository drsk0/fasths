{-#Language FlexibleInstances, TypeSynonymInstances #-} 

import Test.QuickCheck
import Codec.Fast.Data
import Codec.Fast
import Data.ByteString.Char8 (unpack, pack) 
import Data.Bits
import Data.Int
import Data.Word
import qualified Data.ByteString as B

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack (listOf (arbitrary :: Gen Word8))
    shrink = shrinkNothing

instance Arbitrary (Delta Int32) where
    arbitrary = fmap Di32 (arbitrary :: Gen Int32)
    shrink (Di32 i) = map Di32 $ (shrink :: (Int32 -> [Int32])) i

instance Arbitrary (Delta Int64) where
    arbitrary = fmap Di64 (arbitrary :: Gen Int64)
    shrink (Di64 i) = map Di64 $ (shrink :: (Int64 -> [Int64])) i

instance Arbitrary (Delta Word32) where
    arbitrary = fmap Dw32 (arbitrary :: Gen Int32)
    shrink (Dw32 i) = map Dw32 $ (shrink :: (Int32 -> [Int32])) i

instance Arbitrary (Delta Word64) where
    arbitrary = fmap Dw64 (arbitrary :: Gen Int64)
    shrink (Dw64 i) = map Dw64 $ (shrink :: (Int64 -> [Int64])) i

instance Arbitrary (Delta (Int32, Int64)) where
    arbitrary = fmap Ddec (arbitrary :: Gen (Int32, Int64))
    shrink (Ddec (e, m)) = map Ddec $ (shrink :: (Int32, Int64) -> [(Int32, Int64)]) (e, m)

instance Arbitrary (Delta B.ByteString) where
    arbitrary = fmap Dbs (arbitrary :: Gen (Int32, B.ByteString))
    shrink (Dbs (i, bs)) = map Dbs (zip (shrink i) (replicate (length $ shrink i) bs))

instance Arbitrary Template where
    arbitrary = undefined
    shrink = undefined

newtype TemplateMsgPair = TMP (Template, [(NsName, Maybe Value)]) deriving Show

instance Arbitrary TemplateMsgPair where
    arbitrary = do 
                    t <- arbitrary :: Gen Template
                    msgs <- listOf $ arbitraryMsgForTemplate t
                    return $ TMP (t, msgs)
    shrink = undefined

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
arbitraryMsgForTemplate = undefined

main :: IO ()
main = do
    putStr "\n[*] Checking 'decodeP . encodeP = id'\n"
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: (Int32, Int64) -> Bool)
    quickCheck (forAll (fmap (addPreamble . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) ((prop_decodeP_dot_encodeP_is_ID) :: AsciiString -> Bool))
    quickCheck (forAll (fmap (addPreamble' . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) ((prop_decodeP_dot_encodeP_is_ID) :: AsciiString -> Bool))
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeD . encodeD = id'\n"
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int32) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int64) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word32) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word64) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta (Int32, Int64)) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta B.ByteString) -> Bool)

    putStr "\n[*] Checking 'decodeT . encodeT = id'\n"
    quickCheck (forAll (fmap (addPreamble . (\(B7S s) -> s)) (arbitrary :: Gen Bit7String)) (prop_decodeT_dot_encodeT_is_ID :: AsciiString -> Bool))
    quickCheck (prop_decodeT_dot_encodeT_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'fromValue . toValue = id'\n"
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int64 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word64 -> Bool)
    quickCheck $ expectFailure (prop_fromValue_dot_toValue_is_ID :: (Int32, Int64) -> Bool)
    -- fails for imprecision of floating point operations.
    quickCheck (prop_fromValue_dot_toValue_is_ID :: AsciiString -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'delta . delta_ = id'\n"
    quickCheck (prop_delta_dot_delta__is_ID :: (Int32, Int32) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: (Word32, Word32) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: (Int64, Int64) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: (Word64, Word64) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: ((Int32, Int64), (Int32, Int64)) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: (AsciiString, AsciiString) -> Bool)
    quickCheck (prop_delta_dot_delta__is_ID :: (B.ByteString, B.ByteString) -> Bool)

    putStr "\n[*] Checking 'ftail . ftail_ = id'\n"
    quickCheck (prop_ftail_dot_ftail__is_ID :: (AsciiString, AsciiString) -> Bool)
    quickCheck (prop_ftail_dot_ftail__is_ID :: (B.ByteString, B.ByteString) -> Bool)

    putStr "\n[*] Checking 'rmPreamble . addPreamble = id'\n"
    quickCheck (prop_rmPreamble_dot_addPreamble_is_ID . (\(NOS s) -> s))
    
    {-quickCheck ((uncurry $ prop_decode_template_encode_template_is_ID) . (\(TMP p) -> p))-}
