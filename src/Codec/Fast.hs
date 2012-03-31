-- |
-- Module      :  Codec.Fast
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown

{-#LANGUAGE FlexibleContexts #-}

module Codec.Fast
(
-- * Introduction 

-- |This library provides the means to decode or encode binary data encoded according to the FAST protocol. See
-- <http://fixprotocol.org/fast> for more information on FAST. A FAST message is a tuple ('NsName', Maybe 'Value'). Note that 'Value' is a recursive data structure. Here is how a typical decoded message will look like: 
-- 
-- @[Msg Name [\"MDIncRefresh_33\"] Ns [\"-\"] Id [\"33\"] -> Gr 
--        Name [\"ApplVerID\"] Ns [\"-\"] Id [\"1128\"] -> A \"8\"
--        Name [\"MessageType\"] Ns [\"-\"] Id [\"35\"] -> A \"X\"
--        Name [\"SenderCompID\"] Ns [\"-\"] Id [\"49\"] -> A \"CME\"
--        Name [\"MsgSeqNum\"] Ns [\"-\"] Id [\"34\"] -> UI32 286187
--        Name [\"SendingTime\"] Ns [\"-\"] Id [\"52\"] -> UI64 20071106192034435
--        Name [\"PosDupFlag\"] Ns [\"-\"] Id [\"43\"] ->  --- 
--        Name [\"TradeDate\"] Ns [\"-\"] Id [\"75\"] -> UI32 20071106
--        Name [\"MDEntries\"] Ns [\"-\"] Id [\"-\"] -> Sq [1] 
--        Name [\"MDUpdateAction\"] Ns [\"-\"] Id [\"279\"] -> UI32 0
--        Name [\"MDEntryType\"] Ns [\"-\"] Id [\"269\"] -> A \"2\"
--        Name [\"SecurityIDSource\"] Ns [\"-\"] Id [\"22\"] -> UI32 8
--        Name [\"SecurityID\"] Ns [\"-\"] Id [\"48\"] -> UI32 21033
--        Name [\"RptSeq\"] Ns [\"-\"] Id [\"83\"] -> UI32 101494
--        Name [\"MDEntryPx\"] Ns [\"-\"] Id [\"270\"] -> Dec 220225.0
--        Name [\"MDEntrySize\"] Ns [\"-\"] Id [\"271\"] -> I32 15
--        Name [\"NetChgPrevDay\"] Ns [\"-\"] Id [\"451\"] -> Dec (-5025.0)
--        Name [\"TradeVolume\"] Ns [\"-\"] Id [\"1020\"] -> UI32 124322
--        Name [\"TickDirection\"] Ns [\"-\"] Id [\"274\"] ->  --- 
--        Name [\"TradeCondition\"] Ns [\"-\"] Id [\"277\"] ->  --- 
--        Name [\"MDEntryTime\"] Ns [\"-\"] Id [\"273\"] -> UI32 192034000@

-- * Examples

-- | The following stream never resets its context.
--
-- @
-- stream0 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
-- stream0 ts tid2tem = evalStateT (A.many1 (message ts tid2tem)) (initState ts)
-- @

-- | In this case the parser is reset when a new message starts.
--
-- @
-- stream1 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
-- stream1 ts tid2tem = evalStateT (A.many1 (Codec.Fast.reset ts >> message ts tid2tem)) (initState ts)
-- @

-- | This stream resets the state on every occurence of a message with
-- name \"Reset\".
--
-- @
-- stream2 :: Templates -> (Word32 -> TemplateNsName) -> A.Parser [(NsName, Maybe Value)]
-- stream2 ts tid2tem = evalStateT (A.many1 (resetMsg \<|\> msg)) (initState ts)
--     where    msg = message ts tid2tem
--              resetMsg =  do 
--                              m\@(NsName (NameAttr n) _ _, _) <- msg
--                              if n == \"Reset\" 
--                                  then Codec.Fast.reset ts >> return m 
--                                  else fail \"Not reset message.\"
-- @
--
-- An example of the function 
-- @
-- tid2tem :: 'Word32' -> 'TemplateNsName' 
-- @
-- is
--
-- @
-- tid2tem 30 = TemplateNsName (NameAttr "MDIncRefresh_30") Nothing (Just (IdAttr (Token "30")))
-- tid2tem i = error ("Template id unknown." ++ " " ++ show i)
-- @
--
-- This small program reads a templates.xml and a file containing FAST messages in binary format from stdin and prints the decoded messages.
--
-- @
-- main :: IO ()
-- main = do 
--     args <- getArgs
--     ts <- if not . null $ args then parseTemplateXML (readDocument [] (head args)) else error "No template file given."
--     bs <- B.readFile (args!!1)
--     catch (print (A.feed (A.parse (fmap (map PMsg) (stream2 ts Main.tid2temp)) bs) B.empty)) handler
-- @

-- * Encoder/Decoder
message, 
_message,
Codec.Fast.reset,
Templates, 
parseTemplateXML,
initState,
NsName (..), 
TemplateNsName (..),
IdAttr (..),
Value (..),
Token (..),
NameAttr (..),
-- * Pretty printing
PValue (..),
PMsg (..),
showDiff,
-- * QuickCheck properties.
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
import Codec.Fast.Decoder
import Codec.Fast.Encoder
import Codec.Fast.TemplateParser
import Codec.Fast.Pretty
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Test.QuickCheck (printTestCase, Property)

-- | Decoder for one message.
message :: Templates
        -> (Word32 -> TemplateNsName) -- ^ Defines how to map an identifier to a template.
        -> StateT Context A.Parser (NsName, Maybe Value)
message ts tid2tem = let env = initEnv ts tid2tem 
    in runReaderT segment' env

-- | Encoder for one message.
_message :: Templates 
            -> (TemplateNsName -> Word32) -- ^ Defines how to map a template to an identifier.
            -> ((NsName, Maybe Value) -> State Context BU.Builder)
_message ts tem2tid msg = let env = _initEnv ts tem2tid
    in runReaderT (_segment' msg) env

-- | Resets the de/encoder to the initial state.
reset :: (MonadState Context m) => Templates -> m ()
reset ts = put (initState ts)

-- | Encode, then decode a message.
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
