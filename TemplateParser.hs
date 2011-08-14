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
  tnn   <- getTemplateNsName-< l
  ns    <- getNs            -< l
  dict  <- getDict          -< l
  tr    <- getTypeRef       -< l
  ins   <- getInstructions  -< l
  returnA -< (Template tnn ns dict tr ins)

getTemplateNsName::IOStateArrow TPState XmlTree TemplateNsName 
getTemplateNsName = atTag "templateNsName" >>> 
    proc l -> do
       name <- getName'     -< l
       tNs  <- getTempNs    -< l
       id   <- getId        -< l
       returnA -< (TemplateNsName name tNs id)

getId::IOStateArrow TPState XmlTree (Maybe IdAttr)
getId = (getAttrValue "id" >>> arr (Just . IdAttr . Token)) <+> constA Nothing 

getTypeRef::IOStateArrow TPState XmlTree (Maybe TypeRef)
getTypeRef = (atTag "typeRef" >>> 
    (proc l -> do
        name    <- getName' -< l
        ns      <- getNs    -< l
        returnA -< (TypeRef name ns))
    >>> arr Just) <+> constA Nothing
        
getName'::IOStateArrow TPState XmlTree NameAttr 
getName' = getAttrValue  "name" >>> arr NameAttr

getInstructions::IOStateArrow TPState XmlTree [Instruction]
getInstructions = listA ((hasName "templateRef" >>> getTemplateRef) <+> (hasName "field" >>> getField >>> (arr Instruction)))

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
    op      <- getFieldOp   -< l
    name    <- getNsName    -< l
    presence<- getPresence  -< l
    returnA -< (FieldInstrContent name presence op)
    
getFieldOp::IOStateArrow TPState XmlTree (Maybe FieldOp)
getFieldOp = undefined

getNsName::IOStateArrow TPState XmlTree NsName
getNsName = undefined

getPresence::IOStateArrow TPState XmlTree (Maybe PresenceAttr)
getPresence = undefined

getDecimalField::IOStateArrow TPState XmlTree DecimalField
getDecimalField = undefined

getAsciiStringField::IOStateArrow TPState XmlTree AsciiStringField
getAsciiStringField = undefined

getUnicodeStringField::IOStateArrow TPState XmlTree UnicodeStringField
getUnicodeStringField = undefined

getByteVector::IOStateArrow TPState XmlTree ByteVectorField
getByteVector = undefined

getSequence::IOStateArrow TPState XmlTree Sequence
getSequence = undefined

getGroup::IOStateArrow TPState XmlTree Group
getGroup = undefined

parseXML::String -> IOStateArrow TPState () XmlTree
parseXML s = readString [ withValidate yes
		     	, withRemoveWS yes  -- throw away formating WS
		     	] s

atTag::ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

isNotEmpty::IOStateArrow TPState [a] [a]
isNotEmpty = isA (not . null)

