{-#Language FlexibleInstances, TypeSynonymInstances #-} 

import Test.QuickCheck
import Codec.Fast.Data
import Codec.Fast
import Data.ByteString.Char8 (unpack, pack) 
import Data.Bits
import Data.Int
import Data.Word
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
    
    quickCheck ((uncurry $ prop_decode_template_encode_template_is_ID) . (\(STMP p) -> p))
