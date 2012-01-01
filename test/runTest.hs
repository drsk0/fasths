{-#Language FlexibleInstances, TypeSynonymInstances #-} 

import Test.QuickCheck
import Codec.Fast.Data
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

instance Arbitrary Bit7String where
    arbitrary = fmap (Bit7String . unpack .  (B.filter (\w -> not (testBit w 7)) . pack)) (arbitrary :: Gen String)
    shrink (Bit7String s) = map Bit7String (shrink s)

main :: IO ()
main = do
    putStr "\n[*] Checking 'decodeP . encodeP = id'\n"
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: (Int32, Int64) -> Bool)
    quickCheck prop_decodeP_dot_encodeP_is_ID_AsciiString
    quickCheck prop_decodeP_dot_encodeP_is_ID_AsciiString'
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeD . encodeD = id'\n"
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int32) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Int64) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word32) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta Word64) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta (Int32, Int64)) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: (Delta B.ByteString) -> Bool)

    putStr "\n[*] Checking 'decodeT . encodeT = id'\n"
    quickCheck prop_decodeT_dot_encodeT_is_ID_AsciiString
    quickCheck (prop_decodeT_dot_encodeT_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'fromValue . toValue = id'\n"
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int64 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word64 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: (Int32, Int64) -> Bool)
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
