{-# LANGUAGE FlexibleContexts, TupleSections, TypeFamilies #-}

import Test.QuickCheck
import Text.XML.HXT.Core (readDocument)
import Data.Word (Word32, Word64)
import Data.Int (Int32, Int64)
import Codec.Fast.Test.Data
import Codec.Fast.Data
import Codec.Fast (prop_decode_template_encode_template_is_ID)
import Codec.Fast.TemplateParser (parseTemplateXML)
import qualified Data.ByteString as B

main :: IO ()
main = do
    putStr "\n[*] Checking 'decodeP . encodeP = id'\n"
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: (Int32, Int64) -> Bool)
    quickCheck (forAll (fmap (\(B7S s) -> s) (arbitrary :: Gen Bit7String)) (prop_decodeP_dot_encodeP_is_ID :: AsciiString -> Bool))
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeP0 . encodeP0 = id'\n"
    quickCheck (prop_decodeP0_dot_encodeP0_is_ID :: Int32 -> Bool)
    quickCheck (prop_decodeP0_dot_encodeP0_is_ID :: Word32 -> Bool)
    quickCheck (prop_decodeP0_dot_encodeP0_is_ID :: Int64 -> Bool)
    quickCheck (prop_decodeP0_dot_encodeP0_is_ID :: Word64 -> Bool)
    quickCheck (prop_decodeP0_dot_encodeP0_is_ID :: (Int32, Int64) -> Bool)
    quickCheck (forAll (fmap (\(B7S s) -> s) (arbitrary :: Gen Bit7String)) (prop_decodeP0_dot_encodeP0_is_ID :: AsciiString -> Bool))

    putStr "\n[*] Checking 'decodeD . encodeD = id'\n"
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta Int32 -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta Int64 -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta Word32 -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta Word64 -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta (Int32, Int64) -> Bool)
    quickCheck (prop_decodeD_dot_encodeD_is_ID :: Delta B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeD0 . encodeD0 = id'\n"
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta Int32 -> Bool)
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta Int64 -> Bool)
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta Word32 -> Bool)
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta Word64 -> Bool)
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta (Int32, Int64) -> Bool)
    quickCheck (prop_decodeD0_dot_encodeD0_is_ID :: Delta B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeT . encodeT = id'\n"
    quickCheck (forAll (fmap (\(B7S s) -> s) (arbitrary :: Gen Bit7String)) (prop_decodeT_dot_encodeT_is_ID :: AsciiString -> Bool))
    quickCheck (prop_decodeT_dot_encodeT_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'decodeT0 . encodeT0 = id'\n"
    quickCheck (forAll (fmap (\(B7S s) -> s) (arbitrary :: Gen Bit7String)) (prop_decodeT0_dot_encodeT0_is_ID :: AsciiString -> Bool))
    quickCheck (prop_decodeT0_dot_encodeT0_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking 'fromValue . toValue = id'\n"
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word32 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Int64 -> Bool)
    quickCheck (prop_fromValue_dot_toValue_is_ID :: Word64 -> Bool)
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
    quickCheck (prop_ftail_dot_ftail__is_ID :: (UnicodeString, UnicodeString) -> Bool)

    putStr "\n[*] Checking 'rmPreamble . addPreamble = id'\n"
    quickCheck (prop_rmPreamble_dot_addPreamble_is_ID . (\(NOS s) -> s))

    putStr "\n[*] Checking 'bsToPm . pmToBs = id'\n"
    quickCheck prop_bsToPm_dot_pmToBs_is_ID

    putStr "\n[*] ivToPrimitive . primitiveToIv  = id'\n"
    quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Int32 -> Bool)
    quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Word32 -> Bool)
    quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: Int64 -> Bool)
    quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: AsciiString -> Bool)
    quickCheck (prop_ivToPrimitive_dot_primitiveToIv_is_ID :: B.ByteString -> Bool)

    putStr "\n[*] Checking \"templates.xml\" \n"
    ts <- parseTemplateXML (readDocument [] "test/templates.xml") 
    mapM_ (h (tsTemplates ts)) (tsTemplates ts) where 
                h ts t = 
                    let (TemplateNsName (NameAttr n) _ _) = tName t in
                    do 
                        putStr $ n ++ " "
                        quickCheck (forAll (resize 10 (listOf $ arbitraryMsgForTemplate ts t)) (prop_decode_template_encode_template_is_ID ts . (\ms -> (t, ms))))
