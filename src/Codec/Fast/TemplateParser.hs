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

-- | State of the template parser.
data TPState = TPState {
    ns         :: Maybe NsAttr,
    templateNs :: Maybe TemplateNsAttr,
    dic        :: Maybe DictionaryAttr
    }

-- | Parser for the template xml file. Normally you parse your template.xml file with
-- 
-- @
--  parseTemplateXML (readDocument [] fileName)
-- @
--
-- where readDocument commes from HXT.Core.
parseTemplateXML :: IOStateArrow TPState XmlTree XmlTree -> IO Templates
parseTemplateXML ar = fmap safehead (runXIOState (initialState (TPState Nothing Nothing Nothing)) (ar >>> deep (isElem >>> hasName "templates") >>> getTemplates))

-- | Throws an OtherException when applied on empty list.
safehead :: [a] -> a
safehead xs | null xs = throw $ OtherException "Template file could not be parsed."
safehead xs = head xs

-- | Parses all template definitions.
getTemplates :: IOStateArrow TPState XmlTree Templates
getTemplates = proc l -> do
		n  		<- getNs                                -< l
		tns 	<- getTempNs                            -< l
		d    	<- getDict                              -< l
		ts 		<- listA (childrenWithName "template" >>> getTemplate) -< l
		returnA -< (Templates n tns d ts)

-- | Parses the ns attribute.
getNs :: IOStateArrow TPState XmlTree (Maybe NsAttr)
getNs = ifA (hasAttr "ns") (getAttrValue "ns" >>> changeUserState updateNs >>> arr (Just . NsAttr)) (getUserState >>> arr ns)
    where   updateNs::String -> TPState -> TPState
            updateNs n s = TPState (Just (NsAttr n)) (templateNs s) (dic s)
            
-- | Parses the templateNs attribute.
getTempNs :: IOStateArrow TPState XmlTree (Maybe TemplateNsAttr)
getTempNs = ifA (hasAttr "templateNs") (getAttrValue "templateNs" >>> changeUserState updateTempNs >>> arr (Just . TemplateNsAttr)) (getUserState >>> arr templateNs)
    where   updateTempNs::String -> TPState -> TPState
            updateTempNs tempNs s = TPState (ns s) (Just (TemplateNsAttr tempNs)) (dic s)

-- | Parses the dictionary attribute.
getDict :: IOStateArrow TPState XmlTree (Maybe DictionaryAttr)
getDict = ifA (hasAttr "dictionary") (getAttrValue "dictionary" >>> changeUserState updateDict >>> arr (Just . DictionaryAttr)) (getUserState >>> arr dic)
    where   updateDict::String -> TPState -> TPState
            updateDict d s = TPState (ns s) (templateNs s) (Just (DictionaryAttr d))

-- | Parses q template element.
getTemplate :: IOStateArrow TPState XmlTree Template
getTemplate = proc l -> do
  tnn   <- getTemplateNsName -< l
  n     <- getNs             -< l
  d     <- getDict           -< l
  tr    <- maybeA (childrenWithName "typeRef" >>> getTypeRef) -< l
  ins   <- listA (getChildren >>> getInstruction)   -< l
  returnA -< (Template tnn n d tr ins)

-- | Parses a templateNsName element.
getTemplateNsName :: IOStateArrow TPState XmlTree TemplateNsName
getTemplateNsName = proc l -> do
       name <- getName'     -< l
       tns  <- getTempNs    -< l
       i    <- maybeA getId -< l
       returnA -< (TemplateNsName name tns i)

-- | Parses the id attribute.
getId :: IOStateArrow TPState XmlTree IdAttr
getId = getAttrValue' "id" >>> arr (IdAttr . Token) 

-- | Parses typeRef element.
getTypeRef :: IOStateArrow TPState XmlTree TypeRef
getTypeRef =  proc l -> do
        name  <- getName' -< l
        n     <- getNs    -< l
        returnA -< (TypeRef name n)
        
-- | Parses name attribute.
getName' :: IOStateArrow TPState XmlTree NameAttr
getName' = getAttrValue' "name" >>> arr NameAttr

-- | Parses an instruction element.
getInstruction :: IOStateArrow TPState XmlTree Instruction
getInstruction = (hasName "templateRef" >>> (getTemplateRef `orElse` constA (TemplateReference Nothing))) <+> (getField >>> arr Instruction)

-- | Parses templateRef element.
getTemplateRef :: IOStateArrow TPState XmlTree Instruction
getTemplateRef = proc l -> do
    name    <- getName'     -< l
    tns     <- getTempNs    -< l
    returnA -< (TemplateReference $ Just $ TemplateReferenceContent name tns)

-- | Parses field element.
getField::IOStateArrow TPState XmlTree Field
getField = (getIntegerField >>> arr IntField) <+> (getDecimalField >>> arr DecField) <+> (getAsciiStringField >>> arr AsciiStrField) 
    <+> (getUnicodeStringField >>> arr UnicodeStrField) <+> (getByteVector >>> arr ByteVecField ) <+> (getSequence >>> arr Seq) <+> (getGroup >>> arr Grp)

-- |Parses int field element.
getIntegerField :: IOStateArrow TPState XmlTree IntegerField
getIntegerField = (getIntField "int32" >>> arr Int32Field) <+> (getIntField "uInt32" >>> arr UInt32Field) <+> (getIntField "int64" >>> arr Int64Field) <+> (getIntField "uInt64" >>> arr UInt64Field)

-- | Helper for getIntegerField.
getIntField::String -> IOStateArrow TPState XmlTree FieldInstrContent
getIntField s = hasName s >>> getFieldInstrContent

-- | Parses fieldInstrContent element.
getFieldInstrContent :: IOStateArrow TPState XmlTree FieldInstrContent
getFieldInstrContent = proc l -> do
    op      <- maybeA getFieldOp  -< l
    name    <- getNsName          -< l
    presence<- maybeA getPresence -< l
    returnA -< (FieldInstrContent name presence op)
    
-- | Parses fieldOp element.
getFieldOp :: IOStateArrow TPState XmlTree FieldOp
getFieldOp = getChildren >>> (getConstantOp <+> getDefaultOp <+> getCopyOp <+> getIncrementOp <+> getDeltaOp <+> getTailOp)

-- | Parses constant operator.
getConstantOp :: IOStateArrow TPState XmlTree FieldOp
getConstantOp = hasName "constant" >>> getInitialValue >>> arr Constant

-- |Parses value attribute.
getInitialValue :: IOStateArrow TPState XmlTree InitialValueAttr
getInitialValue = getAttrValue' "value" >>> arr InitialValueAttr 

-- |Parses default operator.
getDefaultOp :: IOStateArrow TPState XmlTree FieldOp
getDefaultOp = hasName "default" >>> ((getInitialValue >>> arr (Default . Just)) <+> constA (Default Nothing))

-- |Parses copy operator.
getCopyOp :: IOStateArrow TPState XmlTree FieldOp
getCopyOp = getOp "copy" >>> arr Copy

-- |Parses increment operator.
getIncrementOp :: IOStateArrow TPState XmlTree FieldOp
getIncrementOp = getOp "increment" >>> arr Increment

-- |Parses delta operator.
getDeltaOp :: IOStateArrow TPState XmlTree FieldOp
getDeltaOp = getOp "delta" >>> arr Delta

-- |Parses tail operator.
getTailOp :: IOStateArrow TPState XmlTree FieldOp
getTailOp = getOp "tail" >>> arr Tail

-- |Parses operator.
getOp :: String -> IOStateArrow TPState XmlTree OpContext
getOp s = hasName s >>> getOpContext

-- | Parses operator context.
getOpContext :: IOStateArrow TPState XmlTree OpContext
getOpContext = proc l -> do 
    d   <- getDict                -< l
    nk  <- maybeA getNsKey        -< l
    iv  <- maybeA getInitialValue -< l
    returnA -< (OpContext d nk iv)

-- |Parses nsKey element.
getNsKey :: IOStateArrow TPState XmlTree NsKey
getNsKey = proc l -> do 
    k <- getKey -< l
    n <- getNs  -< l
    returnA -< (NsKey k n)

-- |Parses key attribute.
getKey :: IOStateArrow TPState XmlTree KeyAttr
getKey = getAttrValue' "key" >>> arr (KeyAttr . Token)

-- |Parses nsName element.
getNsName :: IOStateArrow TPState XmlTree NsName
getNsName = proc l -> do
    na <- getName'     -< l
    n  <- getNs        -< l
    i  <- maybeA getId -< l
    returnA -< (NsName na n i)

-- |Parses presence attribute.
getPresence :: IOStateArrow TPState XmlTree PresenceAttr
getPresence = getAttrValue' "presence" >>> ((isA ( == "mandatory") >>> constA Mandatory) <+> constA Optional)

-- |Parses decimal field.
getDecimalField :: IOStateArrow TPState XmlTree DecimalField
getDecimalField = hasName "decimal" >>> proc l -> do
   n <- getNsName                                                   -< l
   p <- maybeA getPresence                                          -< l
   op<- maybeA (ifA (getChildren >>> isElem >>> (hasName "exponent" <+> hasName "mantissa")) (getDecFieldOp >>> arr Right) (getFieldOp >>> arr Left)) -< l
   returnA -< (DecimalField n p op)

-- |Parses decimal field operator
getDecFieldOp :: IOStateArrow TPState XmlTree DecFieldOp
getDecFieldOp = proc l -> do
    e <- maybeA (getChildren >>> getExponent) -< l
    m <- maybeA (getChildren >>> getMantissa) -< l
    returnA -< (DecFieldOp e m) 

-- |Parses exponent operator.
getExponent :: IOStateArrow TPState XmlTree FieldOp
getExponent = hasName "exponent" >>> getFieldOp

-- |Parses mantissa operator.
getMantissa :: IOStateArrow TPState XmlTree FieldOp
getMantissa = hasName "mantissa" >>> getFieldOp

-- |Parses ascii string field.
getAsciiStringField :: IOStateArrow TPState XmlTree AsciiStringField
getAsciiStringField = hasName "string" >>> (((getAttrValue' "charset" >>> isA ( == "ascii")) `guards` (getFieldInstrContent >>> arr AsciiStringField)) <+> (neg (getAttrValue0 "charset") `guards` (getFieldInstrContent >>> arr AsciiStringField)))

-- |Parses unicode string field.
getUnicodeStringField :: IOStateArrow TPState XmlTree UnicodeStringField
getUnicodeStringField = 
    hasName "string" >>> (getAttrValue' "charset" >>> isA ( == "unicode")) `guards`
    (proc l -> do 
        fic     <- getFieldInstrContent       -< l
        ln      <- maybeA (childrenWithName "length" >>> getByteVectorLength) -< l
        returnA -< (UnicodeStringField fic ln)) 

-- |Parses bytevector length element.
getByteVectorLength :: IOStateArrow TPState XmlTree ByteVectorLength
getByteVectorLength = getNsName >>> arr ByteVectorLength

-- |Parses bytevector element.
getByteVector :: IOStateArrow TPState XmlTree ByteVectorField
getByteVector = hasName "byteVector" >>> 
    (proc l -> do 
    fic     <- getFieldInstrContent       -< l
    ln      <- maybeA getByteVectorLength -< l
    returnA -< (ByteVectorField fic ln))

-- |Parses sequence element.
getSequence :: IOStateArrow TPState XmlTree Sequence
getSequence = hasName "sequence" >>> proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA (childrenWithName "typeRef" >>> getTypeRef)  -< l
    ln<- maybeA (childrenWithName "length" >>> getLength)   -< l
    is<- listA (getChildren >>> getInstruction)    -< l
    returnA -< (Sequence n p d tr ln is)

-- |Parses sequence length element.
getLength :: IOStateArrow TPState XmlTree Length
getLength = proc l -> do 
    n <- maybeA getNsName -< l
    op<- maybeA getFieldOp -< l
    returnA -< (Length n op)

-- |Parses group element.
getGroup :: IOStateArrow TPState XmlTree Group
getGroup = hasName "group" >>> proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA (childrenWithName "typeRef" >>> getTypeRef)  -< l
    is<- listA (getChildren >>> getInstruction)    -< l
    returnA -< (Group n p d tr is) 

-- |Gets all children of a node with a certain name.
childrenWithName :: ArrowXml a => String -> a XmlTree XmlTree
childrenWithName tag = getChildren >>> isElem >>> hasName tag

-- |Get attribute value with a certain name.
getAttrValue' :: ArrowXml a => String -> a XmlTree String
getAttrValue' s = hasAttr s >>> getAttrValue s

-- |Returns nothing if result of first arrow is empty, else Just result.
maybeA :: IOStateArrow TPState XmlTree a -> IOStateArrow TPState XmlTree (Maybe a)
maybeA ar = listA ar >>> arr listToMaybe 
