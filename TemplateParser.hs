{-# LANGUAGE Arrows #-}

module TemplateParser (parseTemplateXML) where

import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.XmlState.RunIOStateArrow
import Data.Maybe()
import FAST


data TPState = TPState {
    ns          ::Maybe NsAttr,
    templateNs  ::Maybe TemplateNsAttr,
    dict        ::Maybe DictionaryAttr
    }

parseTemplateXML::IOStateArrow TPState XmlTree XmlTree -> IO [Templates]
parseTemplateXML ar = runXIOState    (initialState (TPState Nothing Nothing Nothing)) 
                                    (ar >>> getTemplates)

getTemplates::IOStateArrow TPState XmlTree Templates
getTemplates = atTag "templates" >>>
    proc l -> do
		n  		<- getNs 		-< l
		tns 	<- getTempNs 	-< l
		d    	<- getDict 		-< l
		ts 		<- getTemplateL -< l
		returnA -< (Templates n tns d ts)

getNs::IOStateArrow TPState XmlTree (Maybe NsAttr)
getNs = (getAttrValue "ns" >>> isNotEmpty  >>> changeUserState updateNs >>> arr (Just . NsAttr)) <+>  (getUserState >>> arr ns)
    where   updateNs::String -> TPState -> TPState
            updateNs n s = TPState (Just (NsAttr n)) (templateNs s) (dict s)
            

getTempNs::IOStateArrow TPState XmlTree (Maybe TemplateNsAttr)
getTempNs = (getAttrValue "templateNs" >>> isNotEmpty  >>> changeUserState updateTempNs>>> arr (Just . TemplateNsAttr)) <+> (getUserState >>> arr templateNs)
    where   updateTempNs::String -> TPState -> TPState
            updateTempNs tempNs s = TPState (ns s) (Just (TemplateNsAttr tempNs)) (dict s)

getDict::IOStateArrow TPState XmlTree (Maybe DictionaryAttr)
getDict = (getAttrValue "dictionary" >>> isNotEmpty >>> changeUserState updateDict >>> arr (Just . DictionaryAttr)) <+>  (getUserState >>> arr dict)
    where   updateDict::String -> TPState -> TPState
            updateDict d s = TPState (ns s) (templateNs s) (Just (DictionaryAttr d))

getTemplateL::IOStateArrow TPState XmlTree [Template]
getTemplateL = listA (atTag "template" >>> getTemplate) 

getTemplate::IOStateArrow TPState XmlTree Template
getTemplate = proc l -> do
  tnn   <- getTemplateNsName -< l
  n     <- getNs             -< l
  d     <- getDict           -< l
  tr    <- maybeA getTypeRef -< l
  ins   <- getInstructions   -< l
  returnA -< (Template tnn n d tr ins)

getTemplateNsName::IOStateArrow TPState XmlTree TemplateNsName 
getTemplateNsName = atTag "templateNsName" >>> 
    proc l -> do
       name <- getName'     -< l
       tns  <- getTempNs    -< l
       i    <- maybeA getId -< l
       returnA -< (TemplateNsName name tns i)

getId::IOStateArrow TPState XmlTree IdAttr
getId = getAttrValue "id" >>> arr (IdAttr . Token) 

getTypeRef::IOStateArrow TPState XmlTree TypeRef
getTypeRef = atTag "typeRef" >>> 
    (proc l -> do
        name  <- getName' -< l
        n     <- getNs    -< l
        returnA -< (TypeRef name n))
        
getName'::IOStateArrow TPState XmlTree NameAttr 
getName' = getAttrValue  "name" >>> arr NameAttr

getInstructions::IOStateArrow TPState XmlTree [Instruction]
getInstructions = listA ((atTag "templateRef" >>> getTemplateRef) <+> (atTag "field" >>> getField >>> arr Instruction))

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
getIntegerField = (getIntField "int32" >>> arr Int32Field) <+> (getIntField "uint32" >>> arr UInt32Field) <+> (getIntField "int64" >>> arr Int64Field) <+> (getIntField "uint64" >>> arr UInt64Field)

getIntField::String -> IOStateArrow TPState XmlTree FieldInstrContent
getIntField s = hasName s >>> getFieldInstrContent

getFieldInstrContent::IOStateArrow TPState XmlTree FieldInstrContent 
getFieldInstrContent = proc l -> do
    op      <- maybeA getFieldOp  -< l
    name    <- getNsName          -< l
    presence<- maybeA getPresence -< l
    returnA -< (FieldInstrContent name presence op)
    
getFieldOp::IOStateArrow TPState XmlTree FieldOp
getFieldOp = getConstantOp <+> getDefaultOp <+> getCopyOp <+> getIncrementOp <+> getDeltaOp <+> getTailOp

getConstantOp::IOStateArrow TPState XmlTree FieldOp
getConstantOp = atTag "constant" >>> getInitialValue >>> arr Constant

getInitialValue::IOStateArrow TPState XmlTree InitialValueAttr
getInitialValue = getAttrValue "value" >>> arr InitialValueAttr 

getDefaultOp::IOStateArrow TPState XmlTree FieldOp
getDefaultOp = (getInitialValue >>> arr (Default . Just)) <+> constA (Default Nothing)

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
getKey = getAttrValue "key" >>> arr (KeyAttr . Token)

getNsName::IOStateArrow TPState XmlTree NsName
getNsName = proc l -> do
    na <- getName'     -< l
    n  <- getNs        -< l
    i  <- maybeA getId -< l
    returnA -< (NsName na n i)

getPresence::IOStateArrow TPState XmlTree PresenceAttr
getPresence = getAttrValue "presence" >>> (isA ( == "mandatory") >>> constA Mandatory) <+> constA Optional

getDecimalField::IOStateArrow TPState XmlTree DecimalField
getDecimalField = atTag "decimal" >>> proc l -> do
   n <- getNsName                                                   -< l
   p <- maybeA getPresence                                          -< l
   op<- (getFieldOp >>> arr Left) <+> (getDecFieldOp >>> arr Right) -< l
   returnA -< (DecimalField n p op)

getDecFieldOp::IOStateArrow TPState XmlTree DecFieldOp 
getDecFieldOp = proc l -> do
    e <- getExponent -< l
    m <- getMantissa -< l
    returnA -< (DecFieldOp e m) 

getExponent::IOStateArrow TPState XmlTree FieldOp
getExponent = atTag "exponent" >>> getFieldOp

getMantissa::IOStateArrow TPState XmlTree FieldOp
getMantissa = atTag "mantissa" >>> getFieldOp

getAsciiStringField::IOStateArrow TPState XmlTree AsciiStringField
getAsciiStringField = ifA (atTag "string" >>> getAttrValue "charset" >>> isA ( == "ascii")) (getFieldInstrContent >>> arr AsciiStringField) zeroArrow

getUnicodeStringField::IOStateArrow TPState XmlTree UnicodeStringField
getUnicodeStringField = 
    ifA 
    (hasName "string" >>> getAttrValue "charset" >>> isA ( == "unicode"))
    (proc l -> do 
        fic     <- getFieldInstrContent       -< l
        ln      <- maybeA getByteVectorLength -< l
        returnA -< (UnicodeStringField fic ln)) 
    zeroArrow

getByteVectorLength::IOStateArrow TPState XmlTree ByteVectorLength
getByteVectorLength = atTag "length" >>> getNsName >>> arr ByteVectorLength

getByteVector::IOStateArrow TPState XmlTree ByteVectorField
getByteVector = hasName "byteVector" >>> 
    (proc l -> do 
    fic     <- getFieldInstrContent       -< l
    ln      <- maybeA getByteVectorLength -< l
    returnA -< (ByteVectorField fic ln))

getSequence::IOStateArrow TPState XmlTree Sequence
getSequence = atTag "sequence" >>> (proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA getTypeRef  -< l
    ln<- maybeA getLength   -< l
    is<- getInstructions    -< l
    returnA -< (Sequence n p d tr ln is))

getLength::IOStateArrow TPState XmlTree Length
getLength = atTag "length" >>> (proc l -> do 
    n <- maybeA getNsName -< l
    op<- maybeA getFieldOp -< l
    returnA -< (Length n op))

getGroup::IOStateArrow TPState XmlTree Group
getGroup = atTag "group" >>> (proc l -> do
    n <- getNsName          -< l
    p <- maybeA getPresence -< l
    d <- getDict            -< l
    tr<- maybeA getTypeRef  -< l
    is<- getInstructions    -< l
    returnA -< (Group n p d tr is))

atTag::ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

isNotEmpty::IOStateArrow TPState [a] [a]
isNotEmpty = isA (not . null)

maybeA::IOStateArrow TPState XmlTree a -> IOStateArrow TPState XmlTree (Maybe a)
maybeA ar = (ar >>> arr Just) <+> constA Nothing
