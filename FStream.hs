module FStream where 

import qualified Data.Attoparsec.Char8 as A
import Control.Monad.State
import Control.Monad.Reader
import Data.Word (Word32)
import FAST
import FParser 

-- |Example of a stream. In this case, the stream consists of messages
-- and the parser is reset when a new message starts.
stream1::Templates -> (Word32 -> String) -> A.Parser [(NsName, Maybe FValue)]
stream1 ts tid2tem = evalStateT (A.many1 (FStream.reset ts >> (message ts tid2tem))) (initState ts)

-- |Example of a stream. This stream resets the state on every occurence of a message with
-- name "Reset".
stream2::Templates -> (Word32 -> String) -> A.Parser [(NsName, Maybe FValue)]
stream2 ts tid2tem = 
    let msg = message ts tid2tem 
        parser = do 
                    ((NsName (NameAttr n) _ _), _) <- msg
                    if n == "Reset" then (FStream.reset ts >> msg) else msg
    in evalStateT (A.many1 (parser)) (initState ts)

-- |Stateful parser for one message depending on templates and the tid2temp 
-- converter function.
message::Templates -> (Word32 -> String) -> StateT FState A.Parser (NsName, Maybe FValue)
message ts tid2tem = let   env = initEnv ts tid2tem 
    in runReaderT segment' env

-- |Resets the state of a parser to the initial state depending on the
-- templates.
reset:: Templates -> StateT FState A.Parser ()
reset ts = put (initState ts)

-- NOTE: Blocks are not supported at the time.
