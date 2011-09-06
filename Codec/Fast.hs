-- |
-- Module      :  Codec.Fast
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
module Codec.Fast 
(
message, 
Codec.Fast.reset,
Templates, 
NsName (..), 
NameAttr (..),
FValue (..),
parseTemplateXML,
initState
) 
where

import qualified Data.Attoparsec as A
import Control.Monad.State
import Control.Monad.Reader
import Data.Word (Word32)
import Codec.Fast.Data
import Codec.Fast.Parser 
import Codec.Fast.TemplateParser

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
