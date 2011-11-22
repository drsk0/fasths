-- |
-- Module      :  Codec.Fast
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--

{-#LANGUAGE FlexibleContexts #-}

module Codec.Fast 
(
message, 
Codec.Fast.reset,
Templates, 
NsName (..), 
NameAttr (..),
Value (..),
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
import Codec.Fast.Coparser
import Codec.Fast.TemplateParser
import qualified Data.Binary.Builder as BU

-- |Stateful parser for one message depending on templates and the tid2temp 
-- converter function.
message::Templates -> (Word32 -> String) -> StateT Context A.Parser (NsName, Maybe Value)
message ts tid2tem = let env = initEnv ts tid2tem 
    in runReaderT segment' env

_message :: Templates -> (NsName -> Word32) -> (NsName, Value) -> State Context BU.Builder
_message ts tem2tid msg = let env = _initEnv ts tem2tid
    in runReaderT (_segment' msg) env

-- |Resets the state of a parser to the initial state depending on the
-- templates.
reset :: (MonadState Context m) => Templates -> m ()
reset ts = put (initState ts)

-- NOTE: Blocks are not supported at the time.
