{-# LANGUAGE Arrows #-}

import Text.XML.HXT.Core
import Data.Maybe (listToMaybe)
import FAST


data TPState = TPState {
    ns          ::Maybe NsAttr,
    templateNs  ::Maybe TemplateNsAttr,
    dict        ::Maybe DictionaryAttr
    }

initTPState::TPState
initTPState = TPState Nothing Nothing Nothing

getTemplateXML::String -> IOStateArrow TPState () Templates
getTemplateXML s = parseXML s >>> atTag "templates" >>>
    proc l -> do
		ns 		<- getNs 		-< l
		tNs 	<- getTempNs 	-< l
		dict 	<- getDict 		-< l
		ts 		<- getTemplates -< l
		returnA -< (Templates ns tNs dict ts)

getNs:: IOStateArrow TPState XmlTree (Maybe NsAttr)
getNs = (getAttrValue "ns" >>> isNotEmpty   >>> arr (Just . NsAttr)) <+>  (getUserState >>> arr ns)

getTempNs::IOStateArrow TPState XmlTree (Maybe TemplateNsAttr)
getTempNs = (getAttrValue "templateNs" >>> isNotEmpty   >>> arr (Just . TemplateNsAttr)) <+> (getUserState >>> arr templateNs)

getDict::IOStateArrow TPState XmlTree (Maybe DictionaryAttr)
getDict = (getAttrValue "dictionary" >>> isNotEmpty >>> arr (Just . DictionaryAttr)) <+>  (getUserState >>> arr dict)

getTemplates::IOStateArrow TPState XmlTree [Template]
getTemplates = listA (atTag "template" >>> getTemplate) 

getTemplate::IOStateArrow TPState XmlTree Template
getTemplate = proc l -> do
  tnn   <- getTemplateNsName -< l
  ns    <- getNs             -< l
  dict  <- getDict           -< l
  tr    <- maybeA getTypeRef -< l
  ins   <- getInstructions   -< l
  returnA -< (Template tnn ns dict tr ins)

getTemplateNsName::IOStateArrow TPState XmlTree TemplateNsName 
getTemplateNsName = atTag "templateNsName" >>> 
    proc l -> do
       name <- getName'     -< l
       tNs  <- getTempNs    -< l
       id   <- maybeA getId -< l
       returnA -< (TemplateNsName name tNs id)

getId::IOStateArrow TPState XmlTree IdAttr
getId = getAttrValue "id" >>> arr (IdAttr . Token) 

getTypeRef::IOStateArrow TPState XmlTree TypeRef
getTypeRef = atTag "typeRef" >>> 
    (proc l -> do
        name  <- getName' -< l
        ns    <- getNs    -< l
        returnA -< (TypeRef name ns))
        
getName'::IOStateArrow TPState XmlTree NameAttr 
getName' = getAttrValue  "name" >>> arr NameAttr

getInstructions::IOStateArrow TPState XmlTree [Instruction]
getInstructions = listA ((atTag "templateRef" >>> getTemplateRef) <+> (atTag "field" >>> getField >>> (arr Instruction)))

getTemplateRef::IOStateArrow TPState XmlTree Instruction
getTemplateRef = (proc l -> do
    name    <- getName'     -< l
    tNs     <- getTempNs    -< l
    returnA -< (TemplateReference $ Just $ (TemplateReferenceContent name tNs))) 
    <+> constA (TemplateReference $ Nothing)

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
    ns<- getNs  -< l
    returnA -< (NsKey k ns)

getKey::IOStateArrow TPState XmlTree KeyAttr
getKey = getAttrValue "key" >>> arr (KeyAttr . Token)

getNsName::IOStateArrow TPState XmlTree NsName
getNsName = proc l -> do
    n  <- getName'     -< l
    ns <- getNs        -< l
    id <- maybeA getId -< l
    returnA -< (NsName n ns id)

getPresence::IOStateArrow TPState XmlTree PresenceAttr
getPresence = getAttrValue "presence" >>> (isA (\s -> s == "mandatory") >>> constA Mandatory) <+> constA Optional

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
getAsciiStringField = ifA (atTag "string" >>> getAttrValue "charset" >>> isA (\s -> s == "ascii")) (getFieldInstrContent >>> arr AsciiStringField) zeroArrow

getUnicodeStringField::IOStateArrow TPState XmlTree UnicodeStringField
getUnicodeStringField = 
    ifA 
    (hasName "string" >>> getAttrValue "charset" >>> isA (\s -> s == "unicode"))
    (proc l -> do 
        fic     <- getFieldInstrContent       -< l
        length  <- maybeA getByteVectorLength -< l
        returnA -< (UnicodeStringField fic length)) 
    zeroArrow

getByteVectorLength::IOStateArrow TPState XmlTree ByteVectorLength
getByteVectorLength = atTag "length" >>> getNsName >>> arr ByteVectorLength

getByteVector::IOStateArrow TPState XmlTree ByteVectorField
getByteVector = hasName "byteVector" >>> 
    (proc l -> do 
    fic     <- getFieldInstrContent       -< l
    length  <- maybeA getByteVectorLength -< l
    returnA -< (ByteVectorField fic length))

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

parseXML::String -> IOStateArrow TPState () XmlTree
parseXML s = readString [ withValidate yes
		     	, withRemoveWS yes  -- throw away formating WS
		     	] s

atTag::ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

isNotEmpty::IOStateArrow TPState [a] [a]
isNotEmpty = isA (not . null)

maybeA::IOStateArrow TPState XmlTree a -> IOStateArrow TPState XmlTree (Maybe a)
maybeA ar = (ar >>> arr Just) <+> constA Nothing
