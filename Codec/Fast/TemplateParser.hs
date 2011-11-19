-- |
-- Module      :  Codec.Fast.TemplateParser
-- Copyright   :  Robin S. Krom 2011
-- License     :  BSD3
-- 
-- Maintainer  :  Robin S. Krom
-- Stability   :  experimental
-- Portability :  unknown
--
{-# LANGUAGE Arrows #-}

module Codec.Fast.TemplateParser  where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Data.Maybe (listToMaybe)
import Codec.Fast.Data
import Control.Exception (throw)

data TPState = TPState {
    ns          ::Maybe NsAttr,
    templateNs  ::Maybe TemplateNsAttr,
    dic        ::Maybe DictionaryAttr
    }

parseTemplateXML::IOStateArrow TPState XmlTree XmlTree -> IO Templates
parseTemplateXML ar = fmap safehead (runXIOState (initialState (TPState Nothing Nothing Nothing)) (ar >>> deep (isElem >>> hasName "templates") >>> getTemplates))

safehead::[a] -> a
safehead xs | null xs = throw $ OtherException "Template file could not be parsed."
safehead xs = head xs

getTemplates::IOStateArrow TPState XmlTree Templates
getTemplates = proc l -> do
		n  		<- getNs                                -< l
		tns 	<- getTempNs                            -< l
		d    	<- getDict                              -< l
		ts 		<- listA (childrenWithName "template" >>> getTemplate) -< l
		returnA -< (Templates n tns d ts)

getNs::IOStateArrow TPState XmlTree (Maybe NsAttr)
getNs = ifA (hasAttr "ns") (getAttrValue "ns" >>> changeUserState updateNs >>> arr (Just . NsAttr)) (getUserState >>> arr ns)
    where   updateNs::String -> TPState -> TPState
            updateNs n s = TPState (Just (NsAttr n)) (templateNs s) (dic s)
            

getTempNs::IOStateArrow TPState XmlTree (Maybe TemplateNsAttr)
getTempNs = ifA (hasAttr "templateNs") (getAttrValue "templateNs" >>> changeUserState updateTempNs >>> arr (Just . TemplateNsAttr)) (getUserState >>> arr templateNs)
    where   updateTempNs::String -> TPState -> TPState
            updateTempNs tempNs s = TPState (ns s) (Just (TemplateNsAttr tempNs)) (dic s)

getDict::IOStateArrow TPState XmlTree (Maybe DictionaryAttr)
getDict = ifA (hasAttr "dictionary") (getAttrValue "dictionary" >>> changeUserState updateDict >>> arr (Just . DictionaryAttr)) (getUserState >>> arr dic)
    where   updateDict::String -> TPState -> TPState
            updateDict d s = TPState (ns s) (templateNs s) (Just (DictionaryAttr d))

getTemplate::IOStateArrow TPState XmlTree Template
getTemplate = proc l -> do
  tnn   <- getTemplateNsName -< l
  n     <- getNs             -< l
  d     <- getDict           -< l
  tr    <- maybeA (childrenWithName "typeRef" >>> getTypeRef) -< l
  ins   <- listA (getChildren >>> getInstruction)   -< l
  returnA -< (Template tnn n d tr ins)

getTemplateNsName::IOStateArrow TPState XmlTree TemplateNsName 
getTemplateNsName = proc l -> do
       name <- getName'     -< l
       tns  <- getTempNs    -< l
       i    <- maybeA getId -< l
       returnA -< (TemplateNsName name tns i)

getId::IOStateArrow TPState XmlTree IdAttr
getId = getAttrValue' "id" >>> arr (IdAttr . Token) 

getTypeRef::IOStateArrow TPState XmlTree TypeRef
getTypeRef =  proc l -> do
        name  <- getName' -< l
        n     <- getNs    -< l
        returnA -< (TypeRef name n)
        
getName'::IOStateArrow TPState XmlTree NameAttr 
getName' = getAttrValue' "name" >>> arr NameAttr

getInstruction::IOStateArrow TPState XmlTree Instruction
getInstruction = (hasName "templateRef" >>> getTemplateRef) <+> (getField >>> arr Instruction)

getTemplateRef::IOStateArrow TPState XmlTree Instruction
getTemplateRef = (proc l -> do
    name    <- getName'     -< l
    tns     <- getTempNs    -< l
    returnA -< (TemplateReference $ Just $ TemplateReferenceContent name tns))
    <+> constA (TemplateReference Nothing)

getField::IOStateArrow TPState XmlTree Field
getField = (getIntegerField >>> arr IntField) <+> (getDecimalField >>> arr DecField) <+> (getAsciiStringField >>> arr AsciiStrField) 
    <+> (getUnicodeStringField >>> arr UnicodeStrField) <+> (getByteVector >>> arr ByteVecField ) <+> (getSequence >>> arr Seq) <+> (getGroup >>> arr Grp)

getIntegerField::IOStateArrow TPState XmlTree IntegerField
getIntegerField = (getIntField "int32" >>> arr Int32Field) <+> (getIntField "uInt32" >>> arr UInt32Field) <+> (getIntField "int64" >>> arr Int64Field) <+> (getIntField "uInt64" >>> arr UInt64Field)

getIntField::String -> IOStateArrow TPState XmlTree FieldInstrContent
getIntField s = hasName s >>> getFieldInstrContent

getFieldInstrContent::IOStateArrow TPState XmlTree FieldInstrContent 
getFieldInstrContent = proc l -> do
    op      <- maybeA getFieldOp  -< l
    name    <- getNsName          -< l
    presence<- maybeA getPresence -< l
    returnA -< (FieldInstrContent name presence op)
    
getFieldOp::IOStateArrow TPState XmlTree FieldOp
getFieldOp = getChildren >>> (getConstantOp <+> getDefaultOp <+> getCopyOp <+> getIncrementOp <+> getDeltaOp <+> getTailOp)

getConstantOp::IOStateArrow TPState XmlTree FieldOp
getConstantOp = hasName "constant" >>> getInitialValue >>> arr Constant

getInitialValue::IOStateArrow TPState XmlTree InitialValueAttr
getInitialValue = getAttrValue' "value" >>> arr InitialValueAttr 

getDefaultOp::IOStateArrow TPState XmlTree FieldOp
getDefaultOp = hasName "default" >>> ((getInitialValue >>> arr (Default . Just)) <+> constA (Default Nothing))

getCopyOp::IOStateArrow TPState XmlTree FieldOp
getCopyOp = getOp "copy" >>> arr Copy

getIncrementOp::IOStateArrow TPState XmlTree FieldOp
getIncrementOp = getOp "increment" >>> arr Increment

getDeltaOp::IOStateArrow TPState XmlTree FieldOp
getDeltaOp = getOp "delta" >>> arr Delta

getTailOp::IOStateArrow TPState XmlTree FieldOp
getTailOp = getOp "tail" >>> arr Tail

getOp::String -> IOStateArrow TPState XmlTree OpContext
getOp s = hasName s >>> getOpContext

getOpContext::IOStateArrow TPState XmlTree OpContext
getOpContext = proc l -> do 
    d   <- getDict                -< l
    nk  <- maybeA getNsKey        -< l
    iv  <- maybeA getInitialValue -< l
    returnA -< (OpContext d nk iv)

getNsKey::IOStateArrow TPState XmlTree NsKey
getNsKey = proc l -> do 
    k <- getKey -< l
    n <- getNs  -< l
    returnA -< (NsKey k n)

getKey::IOStateArrow TPState XmlTree KeyAttr
getKey = getAttrValue' "key" >>> arr (KeyAttr . Token)

getNsName::IOStateArrow TPState XmlTree NsName
getNsName = proc l -> do
    na <- getName'     -< l
    n  <- getNs        -< l
    i  <- maybeA getId -< l
    returnA -< (NsName na n i)

getPresence::IOStateArrow TPState XmlTree PresenceAttr
getPresence = getAttrValue' "presence" >>> ((isA ( == "mandatory") >>> constA Mandatory) <+> constA Optional)

getDecimalField::IOStateArrow TPState XmlTree DecimalField
getDecimalField = hasName "decimal" >>> proc l -> do
   n <- getNsName                                                   -< l
   p <- maybeA getPresence                                          -< l
   op<- maybeA (ifA (getChildren >>> isElem >>> (hasName "exponent" <+> hasName "mantissa")) (getDecFieldOp >>> arr Right) (getFieldOp >>> arr Left)) -< l
   returnA -< (DecimalField n p op)

getDecFieldOp::IOStateArrow TPState XmlTree DecFieldOp 
getDecFieldOp = proc l -> do
    e <- maybeA (getChildren >>> getExponent) -< l
    m <- maybeA (getChildren >>> getMantissa) -< l
    returnA -< (DecFieldOp e m) 

getExponent::IOStateArrow TPState XmlTree FieldOp
getExponent = hasName "exponent" >>> getFieldOp

getMantissa::IOStateArrow TPState XmlTree FieldOp
getMantissa = hasName "mantissa" >>> getFieldOp

getAsciiStringField::IOStateArrow TPState XmlTree AsciiStringField
getAsciiStringField = hasName "string" >>> (((getAttrValue' "charset" >>> isA ( == "ascii")) `guards` (getFieldInstrContent >>> arr AsciiStringField)) <+> (neg (getAttrValue0 "charset") `guards` (getFieldInstrContent >>> arr AsciiStringField)))

getUnicodeStringField::IOStateArrow TPState XmlTree UnicodeStringField
getUnicodeStringField = 
    hasName "string" >>> (getAttrValue' "charset" >>> isA ( == "unicode")) `guards`
    (proc l -> do 
        fic     <- getFieldInstrContent       -< l
        ln      <- maybeA (childrenWithName "length" >>> getByteVectorLength) -< l
        returnA -< (UnicodeStringField fic ln)) 

getByteVectorLength::IOStateArrow TPState XmlTree ByteVectorLength
getByteVectorLength = getNsName >>> arr ByteVectorLength

getByteVector::IOStateArrow TPState XmlTree ByteVectorField
getByteVector = hasName "byteVector" >>> 
    (proc l -> do 
    fic     <- getFieldInstrContent       -< l
    ln      <- maybeA getByteVectorLength -< l
    returnA -< (ByteVectorField fic ln))

getSequence::IOStateArrow TPState XmlTree Sequence
getSequence = hasName "sequence" >>> proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA (childrenWithName "typeRef" >>> getTypeRef)  -< l
    ln<- maybeA (childrenWithName "length" >>> getLength)   -< l
    is<- listA (getChildren >>> getInstruction)    -< l
    returnA -< (Sequence n p d tr ln is)

getLength::IOStateArrow TPState XmlTree Length
getLength = proc l -> do 
    n <- maybeA getNsName -< l
    op<- maybeA getFieldOp -< l
    returnA -< (Length n op)

getGroup::IOStateArrow TPState XmlTree Group
getGroup = hasName "group" >>> proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA (childrenWithName "typeRef" >>> getTypeRef)  -< l
    is<- listA (getChildren >>> getInstruction)    -< l
    returnA -< (Group n p d tr is) 

childrenWithName::ArrowXml a => String -> a XmlTree XmlTree
childrenWithName tag = getChildren >>> isElem >>> hasName tag

getAttrValue'::ArrowXml a => String -> a XmlTree String
getAttrValue' s = hasAttr s >>> getAttrValue s

maybeA::IOStateArrow TPState XmlTree a -> IOStateArrow TPState XmlTree (Maybe a)
maybeA ar = listA ar >>> arr listToMaybe 
