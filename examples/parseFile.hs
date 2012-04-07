import Prelude hiding (catch)
import System.Environment (getArgs)
import qualified Data.ByteString as B
import Codec.Fast
import Text.XML.HXT.Core (readDocument)
import qualified Data.Attoparsec as A
import Control.Monad.State
import Control.Applicative
import Control.Exception (IOException, catch)
import Data.Word (Word32)

-- |Example of a stream. This stream never resets its context.
stream0 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
stream0 ts tid2tem = evalStateT (A.many1 (message ts tid2tem)) (initState ts)

-- |Example of a stream. In this case, the stream consists of messages
-- and the parser is reset when a new message starts.
stream1 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
stream1 ts tid2tem = evalStateT (A.many1 (Codec.Fast.reset ts >> message ts tid2tem)) (initState ts)

-- |Example of a stream. This stream resets the state on every occurence of a message with
-- name "Reset".
stream2 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
stream2 ts tid2tem = evalStateT (A.many1 (resetMsg <|> msg)) (initState ts)
    where    msg = message ts tid2tem
             resetMsg =  do 
                    m@(NsName (NameAttr n) _ _, _) <- msg
                    if n == "Reset" then Codec.Fast.reset ts >> return m else fail "Not reset."

handler :: IOException -> IO ()
handler e = error (show e)

main :: IO ()
main = do 
    args <- getArgs
    ts <- if not . null $ args then parseTemplateXML (readDocument [] (head args)) else error "No template file given."
    bs <- B.readFile (args!!1)
    catch (print (A.feed (A.parse (fmap (map PMsg) (stream2 ts Main.tid2temp)) bs) B.empty)) handler


tid2temp :: Word32 -> TemplateNsName
{-tid2temp 22 = "DefaultIncrementalRefreshMessage"-}
{-tid2temp 40 = "DefaultSnapshotMessage"-}
{-tid2temp 36 = "SecurityDefinition"-}
{-tid2temp 61 = "SecurityDefinitionUpdateReport"-}
{-tid2temp 44 = "Heartbeat"-}
{-tid2temp 49 = "SequenceReset"-}
{-tid2temp 120 = "Reset"-}
{-tid2temp 47 = "TradingSessionStatus"-}
{-tid2temp 302 = "News"-}
{-tid2temp 1001 = "Logon"-}
{-tid2temp 1002 = "Logout"-}
{-tid2temp i = error ("Template id unknown." ++ " " ++ show i)-}

tid2temp 30 = TemplateNsName (NameAttr "MDIncRefresh_30") Nothing (Just (IdAttr (Token "30")))
tid2temp 31 = TemplateNsName (NameAttr "MDIncRefresh_31") Nothing (Just (IdAttr (Token "31")))
tid2temp 32 = TemplateNsName (NameAttr "MDIncRefresh_32") Nothing (Just (IdAttr (Token "32")))
tid2temp 33 = TemplateNsName (NameAttr "MDIncRefresh_33") Nothing (Just (IdAttr (Token "33")))
tid2temp 34 = TemplateNsName (NameAttr "MDIncRefresh_34") Nothing (Just (IdAttr (Token "34")))
tid2temp 35 = TemplateNsName (NameAttr "MDIncRefresh_35") Nothing (Just (IdAttr (Token "35")))
tid2temp 36 =TemplateNsName (NameAttr "MDIncRefresh_36") Nothing (Just (IdAttr (Token "36")))
tid2temp 37 =TemplateNsName (NameAttr "MDIncRefresh_37") Nothing (Just (IdAttr (Token "37")))
tid2temp 38 =TemplateNsName (NameAttr "MDIncRefresh_38") Nothing (Just (IdAttr (Token "38")))
tid2temp 39 =TemplateNsName (NameAttr "MDIncRefresh_39") Nothing (Just (IdAttr (Token "39")))
tid2temp 40 =TemplateNsName (NameAttr "MDIncRefresh_40") Nothing (Just (IdAttr (Token "40")))
tid2temp 41 =TemplateNsName (NameAttr "MDIncRefresh_41") Nothing (Just (IdAttr (Token "41")))
tid2temp 42 =TemplateNsName (NameAttr "MDIncRefresh_42") Nothing (Just (IdAttr (Token "42")))
tid2temp 43 =TemplateNsName (NameAttr "MDIncRefresh_43") Nothing (Just (IdAttr (Token "43")))
tid2temp 44 =TemplateNsName (NameAttr "MDIncRefresh_44") Nothing (Just (IdAttr (Token "44")))
tid2temp 45 =TemplateNsName (NameAttr "MDIncRefresh_45") Nothing (Just (IdAttr (Token "45")))
tid2temp 46 =TemplateNsName (NameAttr "MDSecurityDefinition") Nothing (Just (IdAttr (Token "46")))
tid2temp 47 =TemplateNsName (NameAttr "MDQuoteRequest") Nothing (Just (IdAttr (Token "47")))
tid2temp 48 =TemplateNsName (NameAttr "MDSecurityStatus") Nothing (Just (IdAttr (Token "48")))
tid2temp 49 =TemplateNsName (NameAttr "MDNewsMessage") Nothing (Just (IdAttr (Token "49")))
tid2temp 50 =TemplateNsName (NameAttr "MDHeartBeat") Nothing (Just (IdAttr (Token "50")))
tid2temp 51 =TemplateNsName (NameAttr "MDSnapshoFullRefresh_51") Nothing (Just (IdAttr (Token "51")))
tid2temp 1 =TemplateNsName (NameAttr "MDLogon") Nothing (Just (IdAttr (Token "1 ")))
tid2temp 2 =TemplateNsName (NameAttr "MDLogout") Nothing (Just (IdAttr (Token "2 ")))
tid2temp i = error ("Template id unknown." ++ " " ++ show i)
