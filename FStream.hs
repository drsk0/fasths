module FStream where 

import qualified Data.Attoparsec.Char8 as A
import Control.Monad.State
import Control.Monad.Reader
import FAST
import FParser 

-- |Example of a stream. In this case, the stream consists of messages
-- and the parser is reset when a new message starts.
stream::Templates -> (Int -> String) -> A.Parser [(NsName, Maybe FValue)]
stream ts tid2tem = evalStateT (FStream.reset ts >> A.many (message ts tid2tem)) (initState ts)

-- |Stateful parser for one message depending on templates and the tid2temp 
-- converter function.
message::Templates -> (Int -> String) -> StateT FState A.Parser (NsName, Maybe FValue)
message ts tid2tem = let   env = initEnv ts tid2tem 
    in runReaderT segment' env

-- |Resets the state of a parser to the initial state depending on the
-- templates.
reset:: Templates -> StateT FState A.Parser ()
reset ts = put (initState ts)

-- NOTE: Blocks are not supported at the time.
