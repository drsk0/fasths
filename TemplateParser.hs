{-# LANGUAGE Arrows #-}

import Text.XML.HXT.Core
import FAST

getTemplateXML::String -> IOStateArrow () () Templates
getTemplateXML s = parseXML s >>> atTag "templates" >>>
    proc l -> do
		ns 		<- getNs 		-< l
		tNs 	<- getTempNs 	-< l
		dict 	<- getDict 		-< l
		ts 		<- getTemplates -< l
		returnA -< (Templates ns tNs dict ts)

getNs:: IOStateArrow () XmlTree (Maybe NsAttr)
getNs = (getAttrValue "ns" >>> isNotEmpty >>> arr (Just . NsAttr)) `orElse` constA Nothing

getTempNs::IOStateArrow () XmlTree (Maybe TemplateNsAttr)
getTempNs = (getAttrValue "templateNs" >>> isNotEmpty >>> arr (Just . TemplateNsAttr)) `orElse` constA Nothing

getDict::IOStateArrow () XmlTree (Maybe DictionaryAttr)
getDict = (getAttrValue "dictionary" >>> isNotEmpty >>> arr (Just . DictionaryAttr)) `orElse` constA Nothing

getTemplates::IOStateArrow () XmlTree [Template]
getTemplates = undefined
	

parseXML::String -> IOStateArrow () () XmlTree
parseXML s = readString [ withValidate yes
		     	, withRemoveWS yes  -- throw away formating WS
		     	] s

atTag::ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

isNotEmpty::IOStateArrow () [a] [a]
isNotEmpty = isA (not . null)

