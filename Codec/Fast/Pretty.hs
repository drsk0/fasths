-- |
-- Module      :  Codec.Fast.Data
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
module Codec.Fast.Pretty 
(
PValue (..),
PMsg (..),
showDiff
)
where

import Codec.Fast.Data

type Msg = (NsName, Maybe Value)
type Pair = (NsName, Maybe Value)

newtype PValue = PV Value

instance Show PValue where
    show (PV val) = show' 0 val

newtype PMsg = PMsg (NsName, Maybe Value)

instance Show PMsg where
    show (PMsg msg) = "Msg " ++ showPair 0 msg

newtype PName = PN NsName

instance Show PName where
    show (PN (NsName (NameAttr n) (Just (NsAttr ns)) (Just (IdAttr (Token i))))) = "Name " ++ br n ++ " Ns " ++ br ns ++ " Id " ++ br i
    show (PN (NsName (NameAttr n) (Just (NsAttr ns)) Nothing)) = "Name " ++ br n ++ " Ns " ++ br ns ++ " Id " ++ br "-"
    show (PN (NsName (NameAttr n) Nothing (Just (IdAttr (Token i))))) = "Name " ++ br n ++ " Ns " ++ br "-" ++ " Id " ++ br i
    show (PN (NsName (NameAttr n) Nothing Nothing)) = "Name " ++ br n ++ " Ns " ++ br "-" ++ " Id " ++ br "-"

notEqual :: String
notEqual = " <-!-> " 

shiftRight :: Int -> String
shiftRight i = replicate i '\t' 

br :: String -> String
br s = "[" ++ (show s) ++ "]"

show' :: Int -> Value -> String
show' _ (Gr []) = "Gr " ++ "EMPTY"
show' i (Gr vs) = "Gr \n" ++ unlines (map (showPair (i + 1)) vs)
show' _ (Sq l []) = "Sq [" ++ show l ++ "] " ++ "EMTPY"
show' i (Sq l xss) = "Sq [" ++ show l ++ "] \n" ++ unlines (map (\xs -> unlines (map (showPair (i +1)) xs)) xss)
show' _ v = show v

showPair :: Int -> (NsName, (Maybe Value)) -> String
showPair i (n, m_v) = shiftRight i ++ (show $ PN n) ++ " -> " ++ showMaybeValue i m_v

showMaybeValue :: Int -> Maybe Value -> String
showMaybeValue i (Just v) = show' i v
showMaybeValue _ Nothing = " --- "

showDiff :: Msg -> Msg -> String
showDiff msg1 msg2 = "Msg " ++ showDiffPairs 0 (msg1, msg2)

showDiffValues :: Int -> Value -> Value -> String
showDiffValues i v1 v2 | v1 == v2 = show' i v1
showDiffValues _ (Gr []) (Gr _) = "Gr " ++ "EMPTY" ++ notEqual ++ "NONEMPTY" 
showDiffValues _ (Gr _) (Gr []) = "Gr " ++ "NONEMPTY" ++ notEqual ++ " EMPTY" 
showDiffValues i (Gr vs1) (Gr vs2) = "Gr \n" ++ unlines (map (showDiffPairs (i + 1)) (zip vs1 vs2))
showDiffValues _ (Sq l1 _) (Sq l2 _) | l1 /= l2 = "Different sequence lengths: " ++ show l1 ++ notEqual ++ show l2
showDiffValues i (Sq l1 pss1) (Sq _ pss2) | otherwise = shiftRight i ++ "Sq \n" ++ br (show l1) ++ unlines (map (\(ps1, ps2) -> unlines (map (showDiffPairs (i + 1)) (zip ps1 ps2))) (zip pss1 pss2))
showDiffValues _ v1 v2 = show v1 ++ notEqual ++  show v2

showDiffPairs :: Int -> (Pair, Pair) -> String
showDiffPairs i ((n1, m_v1), (n2, m_v2)) | n1 == n2 = shiftRight i ++ (show $ PN n1) ++ " -> " ++ showDiffMaybeValues i m_v1 m_v2
showDiffPairs i ((n1, _), (n2, _)) | otherwise = shiftRight i ++ "Names don't match: " ++ show (PN n1) ++ notEqual ++ show (PN n2)

showDiffMaybeValues :: Int -> Maybe Value -> Maybe Value -> String
showDiffMaybeValues i (Just v1) Nothing = show' i v1 ++ " / --- " 
showDiffMaybeValues i Nothing (Just v2) = " --- / " ++ show' i v2 
showDiffMaybeValues _ Nothing Nothing =  " --- "
showDiffMaybeValues i (Just v1) (Just v2) = showDiffValues i v1 v2
