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
import Control.Monad.Reader
import Control.Monad.State
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

type FCoparser a = a -> ReaderT Env_ (State Context) BU.Builder

_segment' :: FCoparser (NsName, Value)
_segment' (n, v) = do 
    tid <- _templateIdentifier (n, v)
    pmap <- _presenceMap ()
    return $ pmap `BU.append` tid

_templateIdentifier :: FCoparser (NsName, Value)
_templateIdentifier (n, v)  = 
    let _p = field2Cop  (IntField (UInt32Field (FieldInstrContent 
                        (NsName (NameAttr "templateId") Nothing Nothing) 
                        (Just Mandatory) 
                        (Just (Copy (OpContext (Just (DictionaryAttr "global")) (Just (NsKey(KeyAttr (Token "tid")) Nothing)) Nothing)))
                        )))
   in
   do
       env <- ask
       tid <- _p (n, (UI32 . (temp2tid env)) n)
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
sublistsOfLength n [] = []
sublistsOfLength n xs = (take n xs) : sublistsOfLength n (drop n xs)

template2Cop :: Template -> FCoparser (NsName, Value)
template2Cop = undefined

instr2Cop :: Instruction -> FCoparser (NsName, Value)
instr2Cop (Instruction f) = field2Cop f 
instr2Cop (TemplateReference (Just trc)) = \(n, v) -> do
    env <- ask
    template2Cop ((templates env) M.! (NsName name Nothing Nothing)) (n, v) where (TemplateReferenceContent name _) = trc
instr2Cop (TemplateReference Nothing) = _segment'

field2Cop :: Field -> FCoparser (NsName, Value)
field2Cop = undefined
