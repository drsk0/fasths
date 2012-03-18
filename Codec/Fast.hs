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
TemplateNsName (..),
NameAttr (..),
Token (..),
IdAttr (..),
Value (..),
parseTemplateXML,
initState,
-- quickcheck properties.
prop_decode_template_encode_template_is_ID
) 
where

import qualified Data.Attoparsec as A
import qualified Data.Monoid as M
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative (many)
import Data.Word (Word32)
import Data.Maybe (fromMaybe)
import Codec.Fast.Data
import Codec.Fast.Parser 
import Codec.Fast.Coparser
import Codec.Fast.TemplateParser
import Codec.Fast.Pretty
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Test.QuickCheck (printTestCase, Property)

-- |Stateful parser for one message depending on templates and the tid2temp 
-- converter function.
message::Templates -> (Word32 -> TemplateNsName) -> StateT Context A.Parser (NsName, Maybe Value)
message ts tid2tem = let env = initEnv ts tid2tem 
    in runReaderT segment' env

_message :: Templates -> (TemplateNsName -> Word32) -> (NsName, Maybe Value) -> State Context BU.Builder
_message ts tem2tid msg = let env = _initEnv ts tem2tid
    in runReaderT (_segment' msg) env

-- |Resets the state of a parser to the initial state depending on the
-- templates.
reset :: (MonadState Context m) => Templates -> m ()
reset ts = put (initState ts)

-- NOTE: Blocks are not supported at the time.

prop_decode_template_encode_template_is_ID :: [Template] -> (Template, [(NsName, Maybe Value)]) -> Property
prop_decode_template_encode_template_is_ID ts (_, msgs) = printTestCase (unlines [showDiff m1 m2 | (m1, m2) <- zip (fromMaybe [] l) msgs]) result 
    where
            result = l == r 
            l = A.maybeResult (A.feed (A.parse (evalStateT (many parser) (initState templates)) bs) B.empty) 
            r = Just msgs
            parser = message templates tid2tem
            bs = (B.concat . BL.toChunks . BU.toLazyByteString) (M.mconcat (evalState (mapM (_message templates tem2tid) msgs) (initState templates)))
            templates = Templates Nothing Nothing Nothing ts 
            tid2tem tid = fromMaybe (error ("could not find template " ++ show tid ++ ".")) $ 
                lookup tid [((read . (\(IdAttr(Token s)) -> s) . fromMaybe (error "no id attribute in template.")) m_id , n) | (Template n@(TemplateNsName _ _ m_id) _ _ _ _) <- ts]
            tem2tid (TemplateNsName _ _ m_id) = read $ (\(IdAttr(Token s)) -> s) $ fromMaybe (error "no id attribute in template.") m_id
