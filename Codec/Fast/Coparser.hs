{-#LANGUAGE TypeSynonymInstances #-}

module Codec.Fast.Coparser 
(
_segment',
_initEnv
)
where 

import Codec.Fast.Data
import qualified Data.Map as M
import Data.Word
import Data.Bits
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Binary.Builder as BU

data Env_ = Env_ {
    -- |All known templates.
    templates :: M.Map NsName Template,
    -- |The application needs to define how uint32 values are mapped to template names and vice versa.
    temp2tid :: NsName -> Word32
    }

_initEnv :: Templates -> (NsName -> Word32) -> Env_
_initEnv = undefined

type FBuilder = ReaderT Env_ (State Context) BU.Builder
type FCoparser a = a -> FBuilder

instance Monoid FBuilder where
    mappend mb1 mb2 = do
        b1 <- mb1
        b2 <- mb2
        return (b1 `BU.append` b2)
    mempty = return BU.empty

_segment' :: FCoparser (NsName, Maybe Value)
_segment' (n, v) = do 
    tid <- _templateIdentifier (n, v)
    pmap <- _presenceMap ()
    return $ pmap `BU.append` tid

_templateIdentifier :: FCoparser (NsName, Maybe Value)
_templateIdentifier (n, v)  = 
    let _p = field2Cop  (IntField (UInt32Field (FieldInstrContent 
                        (NsName (NameAttr "templateId") Nothing Nothing) 
                        (Just Mandatory) 
                        (Just (Copy (OpContext (Just (DictionaryAttr "global")) (Just (NsKey(KeyAttr (Token "tid")) Nothing)) Nothing)))
                        )))
   in
   do
       env <- ask
       tid <- _p (n, (Just . UI32 . (temp2tid env)) n)
       msg <- template2Cop ((templates env) M.! n) (n, v)
       return (tid `BU.append` msg)

_presenceMap :: FCoparser ()
_presenceMap () = do
    st <- get
    return $ (_anySBEEntity (pmToBs $ pm st))

pmToBs :: [Bool] -> B.ByteString
pmToBs xs = B.pack (map h (sublistsOfLength 7 xs))
    where   h :: [Bool] -> Word8
            h = fst . (foldl (\(r,n) y -> if y then (setBit r n, n-1) else (r, n-1)) (0, 6))

sublistsOfLength :: Int -> [a] -> [[a]]
sublistsOfLength _ [] = []
sublistsOfLength n xs = (take n xs) : sublistsOfLength n (drop n xs)

template2Cop :: Template -> FCoparser (NsName, Maybe Value)
template2Cop t = f
    where f (n, Just (Gr g)) = (sequenceD (map instr2Cop (tInstructions t))) g
          f (n, _) = throw $ OtherException "Template doesn't fit message."
          -- TODO: Are there cases that shoudn't trigger an exception?
        

instr2Cop :: Instruction -> FCoparser (NsName, Maybe Value)
instr2Cop (Instruction f) = field2Cop f 
instr2Cop (TemplateReference (Just trc)) = \(n, v) -> do
    env <- ask
    template2Cop ((templates env) M.! (NsName name Nothing Nothing)) (n, v) where (TemplateReferenceContent name _) = trc
instr2Cop (TemplateReference Nothing) = _segment'

field2Cop :: Field -> FCoparser (NsName, Maybe Value)
field2Cop = undefined
