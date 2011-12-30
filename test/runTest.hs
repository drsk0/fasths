import Test.QuickCheck
import Codec.Fast.Data
import Data.Int
import Data.Word
import Data.ByteString as B


main :: IO ()
main = do
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word32 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Int64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: Word64 -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: (Int32, Int64) -> Bool)
    quickCheck (prop_decodeP_dot_encodeP_is_ID :: AsciiString -> Bool)
