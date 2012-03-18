{-# LANGUAGE FlexibleContexts, TupleSections, TypeFamilies #-}

module Codec.Fast.Test.Data 
(
arbitraryMsgForTemplate,
Bit7String (..),
Bit7Char (..),
NonOverlongString (..)
) 
where 

import Codec.Fast.Data
import Test.QuickCheck
import Data.ByteString.Char8 (unpack, pack) 
import Data.Bits
import Data.Int (Int32, Int64)
import Data.Word (Word32, Word64)
import qualified Data.ListLike as LL
import Control.Applicative 
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.Map as M

newtype Bit7String = B7S String deriving Show
unwrapB7S :: Bit7String -> String
unwrapB7S (B7S s) = s

instance Arbitrary Bit7String where
    arbitrary = fmap (B7S . dropWhile (== '\0') . unpack  . B.map (`clearBit` 7) . pack) (arbitrary :: Gen String)
    shrink = map B7S . (shrink :: String -> [String]) . unwrapB7S

newtype Bit7Char = B7C Char deriving (Show, Eq)
unwrapB7C :: Bit7Char -> Char
unwrapB7C (B7C c) = c

instance Arbitrary Bit7Char where
    arbitrary = ((B7C . toEnum . (`clearBit` 7) . fromEnum) <$> (arbitrary :: Gen Char)) `suchThat` (/= B7C '\0')
    shrink = map B7C . (shrink :: Char -> String) . unwrapB7C

newtype NormalizedDecimal = ND (Int32, Int64) deriving Show
unwrapND :: NormalizedDecimal -> (Int32 , Int64)
unwrapND (ND x) = x

instance Arbitrary NormalizedDecimal where
    arbitrary = fmap (ND . normalize) (arbitrary :: Gen (Int32, Int64))
        where   normalize (_, 0) = (0, 0)
                normalize (e, m) | m `mod` 10 == 0 = normalize (e + 1, m `div` 10)
                normalize (e, m) = (e, m)
    shrink = map ND . (shrink :: (Int32, Int64) -> [(Int32, Int64)]) . unwrapND

newtype NonOverlongString = NOS AsciiString deriving Show

instance Arbitrary NonOverlongString where
    arbitrary = fmap NOS ((arbitrary :: Gen AsciiString) `suchThat` (not . overlong))
        where 
            overlong :: AsciiString -> Bool
            overlong [] = False
            overlong "\0" = False
            overlong ('\0':_) = True
            overlong _ = False
    shrink = map NOS . (shrink :: String -> [String]) . (\(NOS s) -> s)

arbitraryMsgForTemplate :: [Template] -> Template -> Gen (NsName, Maybe Value)
arbitraryMsgForTemplate ts t = do 
                                vs <- mapM (arbitraryValueForInstruction ts) (tInstructions t)
                                return (tname2fname $ tName t, Just $ Gr vs)

arbitraryValueForInstruction :: [Template] -> Instruction -> Gen (NsName, Maybe Value)
arbitraryValueForInstruction ts (Instruction f) = arbitraryValueForField ts f
arbitraryValueForInstruction ts (TemplateReference (Just trc)) = arbitraryMsgForTemplate ts (M.fromList [(TemplateNsName n m_ns Nothing, t) | t@(Template (TemplateNsName n m_ns _) _ _ _ _)  <- ts] M.! tempRefCont2TempNsName trc)
arbitraryValueForInstruction ts (TemplateReference Nothing) = do 
                                                               t <- elements ts
                                                               arbitraryMsgForTemplate ts t

arbitraryValueForField :: [Template] -> Field -> Gen (NsName, Maybe Value)
arbitraryValueForField ts (IntField f@(Int32Field (FieldInstrContent fname _ _ ))) = (fname,) <$> fmap toValue <$> (arbitraryValueForIntField ts f :: Gen (Maybe Int32))
arbitraryValueForField ts (IntField f@(Int64Field (FieldInstrContent fname _ _ ))) = (fname,) <$> fmap toValue <$> (arbitraryValueForIntField ts f :: Gen (Maybe Int64))
arbitraryValueForField ts (IntField f@(UInt32Field (FieldInstrContent fname _ _ ))) = (fname,) <$> fmap toValue <$> (arbitraryValueForIntField ts f :: Gen (Maybe Word32))
arbitraryValueForField ts (IntField f@(UInt64Field (FieldInstrContent fname _ _ ))) = (fname,) <$> fmap toValue <$> (arbitraryValueForIntField ts f :: Gen (Maybe Word64))
arbitraryValueForField ts (DecField f@(DecimalField fname _ _ )) = (fname, ) <$> fmap toValue <$> arbitraryValueForDecField ts f 
arbitraryValueForField ts (AsciiStrField f@(AsciiStringField(FieldInstrContent fname _ _ ))) = (fname, ) <$> fmap toValue <$> arbitraryValueForAsciiField ts f
arbitraryValueForField ts (UnicodeStrField (UnicodeStringField f@(FieldInstrContent fname _ _ ) maybe_len )) = (fname, ) <$> fmap toValue <$> (arbitraryValueForByteVectorField ts f maybe_len :: Gen (Maybe UnicodeString))
arbitraryValueForField ts (ByteVecField (ByteVectorField f@(FieldInstrContent fname _ _ ) maybe_len )) = (fname, ) <$> fmap toValue <$> (arbitraryValueForByteVectorField ts f maybe_len :: Gen (Maybe B.ByteString))
arbitraryValueForField ts (Seq s) = arbitraryValueForSequence ts s
arbitraryValueForField ts (Grp g) = arbitraryValueForGroup ts g

arbitraryValueForIntField :: (Primitive a, Arbitrary a) => [Template] -> IntegerField -> Gen (Maybe a)
arbitraryValueForIntField ts (Int32Field fic) = arbitraryValueForIntField' ts fic 
arbitraryValueForIntField ts (UInt32Field fic) = arbitraryValueForIntField' ts fic 
arbitraryValueForIntField ts (Int64Field fic) = arbitraryValueForIntField' ts fic 
arbitraryValueForIntField ts (UInt64Field fic) = arbitraryValueForIntField' ts fic 

arbitraryValueForIntField' :: (Primitive a, Arbitrary a) => [Template] -> FieldInstrContent -> Gen (Maybe a)
arbitraryValueForIntField' ts (FieldInstrContent fname Nothing maybe_op) = arbitraryValueForIntField' ts (FieldInstrContent fname (Just Mandatory) maybe_op)
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) Nothing) = Just <$> arbitrary
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) = return $ Just $ ivToPrimitive iv
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Default Nothing)))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Default (Just _)))) = Just <$> arbitrary
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Copy _))) = Just <$> arbitrary
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Increment _))) = fmap Just arbitrary
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Optional) (Just (Tail _)))
    = throw $ S2 "Tail operator can not be applied on an integer type field." 
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Optional) (Just (Constant iv))) = oneof [return $ Just (ivToPrimitive iv), return Nothing]
arbitraryValueForIntField' _ (FieldInstrContent _ (Just Mandatory) (Just (Delta _))) = Just <$> arbitrary
arbitraryValueForIntField' _ _ = arbitrary


arbitraryValueForDecField :: [Template] -> DecimalField -> Gen (Maybe (Int32, Int64))
arbitraryValueForDecField ts (DecimalField fname Nothing maybe_either_op) 
    = arbitraryValueForDecField ts (DecimalField fname (Just Mandatory) maybe_either_op)
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) Nothing) = Just <$> arbitrary
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Default (Just _))))) = Just <$> arbitrary
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Copy _)))) = Just <$> arbitrary
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Delta _)))) = Just <$> arbitrary
arbitraryValueForDecField _ (DecimalField _ (Just Mandatory) (Just (Left (Tail _))))
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField _ (DecimalField _ (Just Optional) (Just (Left (Constant iv)))) = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForDecField _ (DecimalField _ (Just Optional) (Just (Left (Increment _)))) 
    = throw $ S2 "Increment operator is applicable only to integer fields."
arbitraryValueForDecField _ (DecimalField _ (Just Optional) (Just (Left (Tail _)))) 
    = throw $ S2 "Tail operator is only applicable to ascii, unicode and bytevector fields." 
arbitraryValueForDecField ts (DecimalField fname (Just Optional) (Just (Right (DecFieldOp maybe_exOp maybe_maOp)))) = 
    let fname' = uniqueFName fname "e"
        fname'' = uniqueFName fname "m"
    in
    do 
        e <- arbitraryValueForIntField ts (Int32Field (FieldInstrContent fname' (Just Optional) maybe_exOp))
        m <- arbitraryValueForIntField ts (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        case (e, m) of
            (Nothing, Nothing) -> return Nothing
            (Just e', Just m') -> return (Just (e', m'))
            (Just e', Nothing) -> do 
                m' <- arbitrary
                return (Just (e', m'))
            (Nothing, Just m') -> do
                e' <- arbitrary
                return (Just (e', m'))
arbitraryValueForDecField ts (DecimalField fname (Just Mandatory) (Just (Right (DecFieldOp maybe_exOp maybe_maOp)))) = 
    let fname' = uniqueFName fname "e"
        fname'' = uniqueFName fname "m"
    in
    do 
        e <- arbitraryValueForIntField ts (Int32Field (FieldInstrContent fname' (Just Mandatory) maybe_exOp))
        m <- arbitraryValueForIntField ts (Int64Field (FieldInstrContent fname'' (Just Mandatory) maybe_maOp))
        case (e, m) of
            (Nothing, Nothing) -> return Nothing
            (Just e', Just m') -> return (Just (e', m'))
            (Just e', Nothing) -> do 
                m' <- arbitrary
                return (Just (e', m'))
            (Nothing, Just m') -> do
                e' <- arbitrary
                return (Just (e', m'))
arbitraryValueForDecField _ _ = arbitrary

arbitraryValueForAsciiField :: [Template] -> AsciiStringField -> Gen (Maybe AsciiString)
arbitraryValueForAsciiField ts (AsciiStringField(FieldInstrContent fname Nothing maybe_op))
    = arbitraryValueForAsciiField ts (AsciiStringField(FieldInstrContent fname (Just Mandatory) maybe_op))
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) Nothing)) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Constant iv)))) = return $ Just $ ivToPrimitive iv
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default Nothing))))
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Default (Just _))))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Copy _)))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Increment _))))
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Delta _)))) = (Just . unwrapB7S) <$> arbitrary
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Mandatory) (Just (Tail _)))) = (Just . dropWhile (=='\0') . map unwrapB7C) <$> vectorOf 10 arbitrary
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Constant iv))))  = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForAsciiField _ (AsciiStringField(FieldInstrContent _ (Just Optional) (Just (Tail _)))) = oneof [return Nothing, (Just . dropWhile (=='\0') . map unwrapB7C) <$> vectorOf 10 arbitrary]
arbitraryValueForAsciiField _ _ = fmap unwrapB7S <$> arbitrary

arbitraryValueForByteVectorField :: (Primitive a, Arbitrary a, LL.ListLike a c, Arbitrary c) => [Template] -> FieldInstrContent -> Maybe ByteVectorLength -> Gen (Maybe a)
arbitraryValueForByteVectorField ts (FieldInstrContent fname Nothing maybe_op) len 
    = arbitraryValueForByteVectorField ts (FieldInstrContent fname (Just Mandatory) maybe_op) len
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) Nothing ) _ = Just <$> arbitrary
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just (Constant iv))) _  = return $ Just $ ivToPrimitive iv
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Optional) (Just(Constant iv))) _ = oneof [return $ Just $ ivToPrimitive iv, return Nothing]
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Default Nothing))) _  
    = throw $ S5 "No initial value given for mandatory default operator."
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Default (Just _)))) _ = Just <$> arbitrary
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Copy _ ))) _  = Just <$> arbitrary
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Increment _ ))) _  
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Optional) (Just(Increment _ ))) _ 
    = throw $ S2 "Increment operator is only applicable to integer fields." 
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Delta _ ))) _ = Just <$> arbitrary
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Mandatory) (Just(Tail _ ))) _ = (Just . LL.fromList) <$> vectorOf 10 arbitrary
arbitraryValueForByteVectorField _ (FieldInstrContent _ (Just Optional) (Just(Tail _ ))) _ = oneof [return Nothing, (Just . LL.fromList) <$> vectorOf 10 arbitrary]
arbitraryValueForByteVectorField _ _ _ = arbitrary

arbitraryValueForSequence :: [Template] -> Sequence -> Gen (NsName, Maybe Value)
arbitraryValueForSequence ts (Sequence fname pr _ _ m_length instrs) = do
    l <- h pr m_length
    g l
    where  
        g Nothing = return (fname, Nothing)
        g (Just i') = do 
                         sq <- vectorOf (fromIntegral i') (mapM (arbitraryValueForInstruction ts) instrs)
                         return (fname, Just (Sq (fromIntegral i') sq))
        fname' = uniqueFName fname "l" 
        h p Nothing = arbitraryValueForIntField ts (UInt32Field (FieldInstrContent fname' p Nothing)) :: Gen (Maybe Word32)
        h p (Just (Length Nothing op)) = arbitraryValueForIntField ts (UInt32Field (FieldInstrContent fname' p op)) :: Gen (Maybe Word32)
        h p (Just (Length (Just fn) op)) = arbitraryValueForIntField ts (UInt32Field (FieldInstrContent fn p op)) :: Gen (Maybe Word32)

arbitraryValueForGroup :: [Template] -> Group -> Gen (NsName, Maybe Value)
arbitraryValueForGroup ts (Group fname Nothing maybe_dict maybe_typeref instrs)
    = arbitraryValueForGroup ts (Group fname (Just Mandatory) maybe_dict maybe_typeref instrs)
arbitraryValueForGroup ts (Group fname (Just Mandatory) _ _ instrs) = (fname,) <$> Just . Gr <$> mapM (arbitraryValueForInstruction ts) instrs
arbitraryValueForGroup ts (Group fname (Just Optional) _ _ instrs) = (fname,) <$> oneof [Just . Gr <$> mapM (arbitraryValueForInstruction ts) instrs, return Nothing]
