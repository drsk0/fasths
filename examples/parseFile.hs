import System.Environment
import qualified Data.ByteString as B
import Codec.Fast
import Text.XML.HXT.Core (readDocument)
import qualified Data.Attoparsec as A
import Control.Monad.State
import Control.Applicative
import Data.Word (Word32)

-- |Example of a stream. This stream never resets its context.
stream0::Templates -> (Word32 -> String) -> A.Parser [(NsName, Maybe Value)]
stream0 ts tid2tem = evalStateT (A.many1 (message ts tid2tem)) (initState ts)

-- |Example of a stream. In this case, the stream consists of messages
-- and the parser is reset when a new message starts.
stream1::Templates -> (Word32 -> String) -> A.Parser [(NsName, Maybe Value)]
stream1 ts tid2tem = evalStateT (A.many1 (Codec.Fast.reset ts >> message ts tid2tem)) (initState ts)

-- |Example of a stream. This stream resets the state on every occurence of a message with
-- name "Reset".
stream2::Templates -> (Word32 -> String) -> A.Parser [(NsName, Maybe Value)]
stream2 ts tid2tem = evalStateT (A.many1 (resetMsg <|> msg)) (initState ts)
    where    msg = message ts tid2tem
             resetMsg =  do 
                    m@(NsName (NameAttr n) _ _, _) <- msg
                    if n == "Reset" then Codec.Fast.reset ts >> return m else fail "Not reset."

main::IO ()
main = do 
    args <- getArgs
    ts <- if not . null $ args then parseTemplateXML (readDocument [] (args!!0)) else error "No template file given."
    bs <- B.readFile (args!!1)
--    print ts
    print (A.feed (A.parse (stream0 ts Main.tid2temp) bs) B.empty)

tid2temp::Word32 -> String
tid2temp 30 = "MDIncRefresh_30"
tid2temp 31 = "MDIncRefresh_31"
tid2temp 32 = "MDIncRefresh_32"
tid2temp 33 = "MDIncRefresh_33"
tid2temp 34 = "MDIncRefresh_34"
tid2temp 35 = "MDIncRefresh_35"
tid2temp 36 = "MDIncRefresh_36"
tid2temp 37 = "MDIncRefresh_37"
tid2temp 38 = "MDIncRefresh_38"
tid2temp 39 = "MDIncRefresh_39"
tid2temp 40 = "MDIncRefresh_40"
tid2temp 41 = "MDIncRefresh_41"
tid2temp 42 = "MDIncRefresh_42"
tid2temp 43 = "MDIncRefresh_43"
tid2temp 44 = "MDIncRefresh_44"
tid2temp 45 = "MDIncRefresh_45"
tid2temp 46 = "MDSecurityDefinition"
tid2temp 47 = "MDQuoteRequest"
tid2temp 48 = "MDSecurityStatus"
tid2temp 49 = "MDNewsMessage"
tid2temp 50 = "MDHeartBeat"
tid2temp 51 = "MDSnapshoFullRefresh_51"
tid2temp 1 = "MDLogon"
tid2temp 2 = "MDLogout"
tid2temp i = error ("Template id unknown." ++ " " ++ show i)
