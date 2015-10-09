%% ``Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: asn1ct_parser.yrl,v 1.1 2008/12/17 09:53:30 mikpe Exp $
%%
Nonterminals
ModuleDefinition ModuleIdentifier DefinitiveIdentifier DefinitiveObjIdComponentList
DefinitiveObjIdComponent TagDefault ExtensionDefault
ModuleBody Exports SymbolsExported Imports SymbolsImported
SymbolsFromModuleList SymbolsFromModule GlobalModuleReference AssignedIdentifier SymbolList
Symbol Reference AssignmentList Assignment
ExtensionAndException
ComponentTypeLists
Externaltypereference Externalvaluereference DefinedType DefinedValue
AbsoluteReference ItemSpec ItemId ComponentId TypeAssignment
ValueAssignment
% ValueSetTypeAssignment
ValueSet
Type BuiltinType NamedType ReferencedType
Value ValueNotNull BuiltinValue ReferencedValue NamedValue
% BooleanType
BooleanValue IntegerType NamedNumberList NamedNumber SignedNumber
% inlined IntegerValue
EnumeratedType
% inlined Enumerations
Enumeration EnumerationItem
% inlined EnumeratedValue
% RealType
RealValue NumericRealValue SpecialRealValue BitStringType
% inlined BitStringValue
IdentifierList
% OctetStringType
% inlined OctetStringValue
% NullType NullValue
SequenceType ComponentTypeList ComponentType
% SequenceValue SequenceOfValue
ComponentValueList SequenceOfType
SAndSOfValue ValueList SetType
% SetValue SetOfValue
SetOfType
ChoiceType
% AlternativeTypeList made common with ComponentTypeList
ChoiceValue
AnyValue
AnyDefBy
SelectionType
TaggedType Tag ClassNumber Class
% redundant TaggedValue
% EmbeddedPDVType EmbeddedPDVValue ExternalType ExternalValue ObjectIdentifierType
ObjectIdentifierValue ObjIdComponentList ObjIdComponent
% NameForm NumberForm NameAndNumberForm
CharacterStringType
RestrictedCharacterStringValue CharacterStringList
% CharSyms CharsDefn
Quadruple
% Group Plane Row Cell
Tuple
% TableColumn TableRow
% UnrestrictedCharacterString
CharacterStringValue
% UnrestrictedCharacterStringValue
ConstrainedType Constraint ConstraintSpec TypeWithConstraint
ElementSetSpecs ElementSetSpec
%GeneralConstraint
UserDefinedConstraint UserDefinedConstraintParameter
UserDefinedConstraintParameters
ExceptionSpec
ExceptionIdentification
Unions
UnionMark
UElems
Intersections
IntersectionElements
IntersectionMark
IElems
Elements
Elems
SubTypeElements
Exclusions
LowerEndpoint
UpperEndpoint
LowerEndValue
UpperEndValue
TypeConstraints NamedConstraint PresenceConstraint

ParameterizedTypeAssignment
ParameterList
Parameters
Parameter
ParameterizedType

% X.681
ObjectClassAssignment ObjectClass ObjectClassDefn
FieldSpecs FieldSpec OptionalitySpec WithSyntaxSpec
TokenOrGroupSpecs TokenOrGroupSpec
SyntaxList OptionalGroup RequiredToken Word
TypeOptionalitySpec
ValueOrObjectOptSpec
VSetOrOSetOptSpec
ValueOptionalitySpec
ObjectOptionalitySpec
ValueSetOptionalitySpec
ObjectSetOptionalitySpec
% X.681 chapter 15
InformationFromObjects
ValueFromObject
%ValueSetFromObjects
TypeFromObject
%ObjectFromObject
%ObjectSetFromObjects
ReferencedObjects
FieldName
PrimitiveFieldName

ObjectAssignment
ObjectSetAssignment
ObjectSet
ObjectSetElements
Object
ObjectDefn
DefaultSyntax
DefinedSyntax
FieldSettings
FieldSetting
DefinedSyntaxTokens
DefinedSyntaxToken
Setting
DefinedObject
ObjectFromObject
ObjectSetFromObjects
ParameterizedObject
ExternalObjectReference
DefinedObjectSet
DefinedObjectClass
ExternalObjectClassReference

% X.682
TableConstraint
ComponentRelationConstraint
ComponentIdList

% X.683
ActualParameter
.

%UsefulType.

Terminals
'ABSENT' 'ABSTRACT-SYNTAX' 'ALL' 'ANY'
'APPLICATION' 'AUTOMATIC' 'BEGIN' 'BIT'
'BOOLEAN' 'BY' 'CHARACTER' 'CHOICE' 'CLASS' 'COMPONENT'
'COMPONENTS' 'CONSTRAINED' 'DEFAULT' 'DEFINED' 'DEFINITIONS'
'EMBEDDED' 'END' 'ENUMERATED' 'EXCEPT' 'EXPLICIT'
'EXPORTS' 'EXTENSIBILITY' 'EXTERNAL' 'FALSE' 'FROM' 'GeneralizedTime'
'TYPE-IDENTIFIER'
'IDENTIFIER' 'IMPLICIT' 'IMPLIED' 'IMPORTS'
'INCLUDES' 'INSTANCE' 'INTEGER' 'INTERSECTION'
'MAX' 'MIN' 'MINUS-INFINITY' 'NULL'
'OBJECT' 'ObjectDescriptor' 'OCTET' 'OF' 'OPTIONAL' 'PDV' 'PLUS-INFINITY'
'PRESENT' 'PRIVATE' 'REAL' 'SEQUENCE' 'SET' 'SIZE'
'STRING' 'SYNTAX' 'TAGS' 'TRUE' 'UNION'
'UNIQUE' 'UNIVERSAL' 'UTCTime' 'WITH'
'{' '}' '(' ')' '.' '::=' ';' ',' '@' '*' '-' '[' ']'
'!' '..' '...' '|' '<' ':' '^'
number identifier typereference restrictedcharacterstringtype
bstring hstring cstring typefieldreference valuefieldreference
objectclassreference word.

Rootsymbol ModuleDefinition.
Endsymbol '$end'.

Left 300 'EXCEPT'.
Left 200 '^'.
Left 200 'INTERSECTION'.
Left 100 '|'.
Left 100 'UNION'.


ModuleDefinition -> ModuleIdentifier
	'DEFINITIONS'
	TagDefault
	ExtensionDefault
	'::='
	'BEGIN'
	ModuleBody
	'END' :
		{'ModuleBody',Ex,Im,Types} = '$7',
		{{typereference,Pos,Name},Defid} = '$1',
		#module{
			pos= Pos,
			name= Name,
			defid= Defid,
			tagdefault='$3',
			extensiondefault='$4',
			exports=Ex,
			imports=Im,
			typeorval=Types}.
%	{module, '$1','$3','$6'}.
% Results always in a record of type module defined in asn_records.hlr

ModuleIdentifier -> typereference DefinitiveIdentifier :
	put(asn1_module,'$1'#typereference.val),
	{'$1','$2'}.

DefinitiveIdentifier -> '{' DefinitiveObjIdComponentList '}' : '$2' .
DefinitiveIdentifier -> '$empty': [].

DefinitiveObjIdComponentList -> DefinitiveObjIdComponent : ['$1'].
DefinitiveObjIdComponentList -> DefinitiveObjIdComponent DefinitiveObjIdComponentList : ['$1'|'$2'].

DefinitiveObjIdComponent -> identifier : '$1' . %expanded->
% DefinitiveObjIdComponent -> NameForm : '$1' .
DefinitiveObjIdComponent -> number : '$1' . %expanded->
% DefinitiveObjIdComponent -> DefinitiveNumberForm : 'fix' .
DefinitiveObjIdComponent -> identifier '(' number ')' : {'$1','$3'} . %expanded->
% DefinitiveObjIdComponent -> DefinitiveNameAndNumberForm  : {'$1','$3'} .

% DefinitiveNumberForm -> number : 'fix' .

% DefinitiveNameAndNumberForm -> identifier '(' DefinitiveNumberForm ')' : 'fix' .

TagDefault -> 'EXPLICIT' 'TAGS' : put(tagdefault,'EXPLICIT'),'EXPLICIT' .
TagDefault -> 'IMPLICIT' 'TAGS' : put(tagdefault,'IMPLICIT'),'IMPLICIT' .
TagDefault -> 'AUTOMATIC' 'TAGS' : put(tagdefault,'AUTOMATIC'),'AUTOMATIC' .
TagDefault -> '$empty': put(tagdefault,'EXPLICIT'),'EXPLICIT'. % because this is the default

ExtensionDefault -> 'EXTENSIBILITY' 'IMPLIED' : 'IMPLIED'.
ExtensionDefault -> '$empty' : 'false'. % because this is the default

ModuleBody -> Exports Imports AssignmentList : {'ModuleBody','$1','$2','$3'}.
ModuleBody -> '$empty' : {'ModuleBody',nil,nil,[]}.

Exports -> 'EXPORTS' SymbolList ';' : {exports,'$2'}.
Exports -> 'EXPORTS'  ';' : {exports,[]}.
Exports -> '$empty' : {exports,all} .

% inlined above SymbolsExported -> SymbolList : '$1'.
% inlined above SymbolsExported -> '$empty' : [].

Imports -> 'IMPORTS' SymbolsFromModuleList ';' : {imports,'$2'}.
Imports -> 'IMPORTS'  ';' : {imports,[]}.
Imports -> '$empty' : {imports,[]} .

% inlined above SymbolsImported -> SymbolsFromModuleList : '$1'.
% inlined above SymbolsImported -> '$empty' : [].

SymbolsFromModuleList -> SymbolsFromModule :['$1'].
% SymbolsFromModuleList -> SymbolsFromModuleList SymbolsFromModule :$1.%changed
SymbolsFromModuleList -> SymbolsFromModule SymbolsFromModuleList :['$1'|'$2'].

% expanded SymbolsFromModule -> SymbolList 'FROM' GlobalModuleReference : #'SymbolsFromModule'{symbols = '$1',module='$3'}.
SymbolsFromModule -> SymbolList 'FROM' typereference : #'SymbolsFromModule'{symbols = '$1',module='$3'}.
SymbolsFromModule -> SymbolList 'FROM' typereference '{' ValueList '}': #'SymbolsFromModule'{symbols = '$1',module='$3'}.
%SymbolsFromModule -> SymbolList 'FROM' typereference identifier: #'SymbolsFromModule'{symbols = '$1',module='$3'}.
%SymbolsFromModule -> SymbolList 'FROM' typereference Externalvaluereference: #'SymbolsFromModule'{symbols = '$1',module='$3'}.
%SymbolsFromModule -> SymbolList 'FROM' typereference DefinedValue: #'SymbolsFromModule'{symbols = '$1',module='$3'}.

% inlined GlobalModuleReference -> typereference AssignedIdentifier : {'$1','$2'} .

% inlined above AssignedIdentifier -> '{' ValueList '}' : '$2'.
% replaced AssignedIdentifier -> '{' DefinedValue ObjIdComponentList '}' :{'$2','$3'}.
% not necessary , replaced by SAndSOfValue AssignedIdentifier -> ObjectIdentifierValue :'$1'.
% AssignedIdentifier -> DefinedValue : '$1'.
% inlined AssignedIdentifier -> '$empty' : undefined.

SymbolList -> Symbol : ['$1'].
SymbolList -> Symbol ',' SymbolList :['$1'|'$3'].

Symbol -> Reference :'$1'.
% later Symbol -> ParameterizedReference :'$1'.

Reference -> typereference :'$1'.
Reference -> identifier:'$1'.
Reference -> typereference '{' '}':'$1'.
Reference -> Externaltypereference '{' '}':'$1'.

% later Reference -> objectclassreference :'$1'.
% later Reference -> objectreference :'$1'.
% later Reference -> objectsetreference :'$1'.

AssignmentList -> Assignment : ['$1'].
% modified AssignmentList -> AssignmentList Assignment : '$1'.
AssignmentList -> Assignment AssignmentList : ['$1'|'$2'].

Assignment -> TypeAssignment : '$1'.
Assignment -> ValueAssignment : '$1'.
% later Assignment -> ValueSetTypeAssignment : '$1'.
Assignment -> ObjectClassAssignment : '$1'.
% later Assignment -> ObjectAssignment : '$1'.
% combined with ValueAssignment Assignment -> ObjectAssignment : '$1'.
Assignment -> ObjectSetAssignment : '$1'.
Assignment -> ParameterizedTypeAssignment : '$1'.
%Assignment -> ParameterizedValueAssignment : '$1'.
%Assignment -> ParameterizedValueSetTypeAssignment : '$1'.
%Assignment -> ParameterizedObjectClassAssignment : '$1'.

ObjectClassAssignment -> typereference '::=' 'CLASS' '{' FieldSpecs '}' :
%ObjectClassAssignment -> objectclassreference '::=' 'CLASS' '{' FieldSpecs '}' :
	#typedef{pos=element(2,'$1'),name=element(3,'$1'),typespec={'CLASS','$5',[]}}.
ObjectClassAssignment -> typereference '::=' 'CLASS' '{' FieldSpecs '}' WithSyntaxSpec :
%ObjectClassAssignment -> objectclassreference '::=' 'CLASS' '{' FieldSpecs '}' WithSyntaxSpec :
	#typedef{pos=element(2,'$1'),name=element(3,'$1'),typespec={'CLASS','$5','$7'}}.

FieldSpecs -> FieldSpec : ['$1'].
FieldSpecs -> FieldSpec ',' FieldSpecs : ['$1'|'$3'].

FieldSpec -> typefieldreference TypeOptionalitySpec : {typefield,'$1','$2'}.

FieldSpec -> valuefieldreference Type 'UNIQUE' ValueOrObjectOptSpec :
	{fixedtypevaluefield,'$1','$2','UNIQUE','$4'}.
FieldSpec -> valuefieldreference Type ValueOrObjectOptSpec :
	{fixedtypevaluefield,'$1','$2',undefined,'$3'}.

FieldSpec -> valuefieldreference typefieldreference ValueOrObjectOptSpec :
        {variabletypevaluefield, '$1','$2','$3'}.

FieldSpec -> typefieldreference typefieldreference VSetOrOSetOptSpec :
        {variabletypevaluesetfield, '$1','$2','$3'}.

FieldSpec -> typefieldreference Type VSetOrOSetOptSpec :
        {fixedtypevaluesetfield, '$1','$2','$3'}.

TypeOptionalitySpec -> 'DEFAULT' Type : {'DEFAULT','$2'}.
TypeOptionalitySpec -> 'OPTIONAL' : 'OPTIONAL'.
TypeOptionalitySpec -> '$empty' : 'MANDATORY'.

ValueOrObjectOptSpec -> ValueOptionalitySpec : '$1'.
ValueOrObjectOptSpec -> ObjectOptionalitySpec : '$1'.
ValueOrObjectOptSpec -> 'OPTIONAL' : 'OPTIONAL'.
ValueOrObjectOptSpec -> '$empty' : 'MANDATORY'.

ValueOptionalitySpec -> 'DEFAULT' Value :
	case '$2' of
	{identifier,_,Id} -> {'DEFAULT',Id};
	_ -> {'DEFAULT','$2'}
	end.

%ObjectOptionalitySpec -> 'DEFAULT' Object :{'DEFAULT','$1'}.
ObjectOptionalitySpec -> 'DEFAULT'  '{' FieldSetting ',' FieldSettings '}' :
        {'DEFAULT',{object,['$2'|'$4']}}.
ObjectOptionalitySpec -> 'DEFAULT'  '{' FieldSetting '}' :
        {'DEFAULT',{object, ['$2']}}.
%ObjectOptionalitySpec -> 'DEFAULT'  '{' DefinedSyntaxTokens '}' :
%        {'DEFAULT',{object, '$2'}}.
ObjectOptionalitySpec -> 'DEFAULT'  ObjectFromObject :
        {'DEFAULT',{object, '$2'}}.


VSetOrOSetOptSpec -> ValueSetOptionalitySpec : '$1'.
%VSetOrOSetOptSpec -> ObjectSetOptionalitySpec : '$1'.
VSetOrOSetOptSpec -> 'OPTIONAL' : 'OPTIONAL'.
VSetOrOSetOptSpec -> '$empty' : 'MANDATORY'.

ValueSetOptionalitySpec -> 'DEFAULT' ValueSet : {'DEFAULT','$1'}.

%ObjectSetOptionalitySpec -> 'DEFAULT' ObjectSet : {'DEFAULT','$1'}.

OptionalitySpec -> 'DEFAULT' Type : {'DEFAULT','$2'}.
OptionalitySpec -> 'DEFAULT' ValueNotNull :
	case '$2' of
	{identifier,_,Id} -> {'DEFAULT',Id};
	_ -> {'DEFAULT','$2'}
	end.
OptionalitySpec -> 'OPTIONAL' : 'OPTIONAL'.
OptionalitySpec -> '$empty' : 'MANDATORY'.

WithSyntaxSpec -> 'WITH' 'SYNTAX' SyntaxList : {'WITH SYNTAX','$3'}.

SyntaxList -> '{' TokenOrGroupSpecs '}' : '$2'.
SyntaxList -> '{' '}' : [].

TokenOrGroupSpecs -> TokenOrGroupSpec : ['$1'].
TokenOrGroupSpecs -> TokenOrGroupSpec TokenOrGroupSpecs : ['$1'|'$2'].

TokenOrGroupSpec -> RequiredToken : '$1'.
TokenOrGroupSpec -> OptionalGroup : '$1'.

OptionalGroup -> '[' TokenOrGroupSpecs ']' : '$2'.

RequiredToken -> typereference : '$1'.
RequiredToken -> Word : '$1'.
RequiredToken -> ',' : '$1'.
RequiredToken -> PrimitiveFieldName : '$1'.

Word -> 'BY' : 'BY'.

ParameterizedTypeAssignment -> typereference ParameterList '::=' Type :
	#ptypedef{pos=element(2,'$1'),name=element(3,'$1'),
	args='$2', typespec='$4'}.

ParameterList -> '{' Parameters '}':'$2'.

Parameters -> Parameter: ['$1'].
Parameters -> Parameter ',' Parameters: ['$1'|'$3'].

Parameter -> typereference: '$1'.
Parameter -> Value: '$1'.
Parameter -> Type ':' typereference: {'$1','$3'}.
Parameter -> Type ':' Value: {'$1','$3'}.
Parameter -> '{' typereference '}': {objectset,'$2'}.


% Externaltypereference -> modulereference '.' typereference : {'$1','$3'} .
Externaltypereference -> typereference '.' typereference : #'Externaltypereference'{pos=element(2,'$1'),module=element(3,'$1'),type=element(3,'$3')}.

% Externalvaluereference -> modulereference '.' valuereference : {'$1','$3'} .
% inlined Externalvaluereference -> typereference '.' identifier : #'Externalvaluereference'{pos=element(2,'$1'),module=element(3,'$1'),value=element(3,'$3')}.


DefinedType -> Externaltypereference : '$1' .
DefinedType -> typereference :
	#'Externaltypereference'{pos='$1'#typereference.pos,
				module= get(asn1_module),
				type= '$1'#typereference.val} .
DefinedType -> typereference ParameterList : {pt,'$1','$2'}.
DefinedType -> Externaltypereference ParameterList : {pt,'$1','$2'}.

% ActualParameterList -> '{' ActualParameters '}' : '$1'.

% ActualParameters -> ActualParameter : ['$1'].
% ActualParameters -> ActualParameter ',' ActualParameters : ['$1'|'$3'].

ActualParameter -> Type : '$1'.
ActualParameter -> ValueNotNull : '$1'.
ActualParameter -> ValueSet : '$1'.
% later DefinedType -> ParameterizedType : '$1' .
% later DefinedType -> ParameterizedValueSetType : '$1' .

% inlined DefinedValue -> Externalvaluereference :'$1'.
% inlined DefinedValue -> identifier :'$1'.
% later DefinedValue -> ParameterizedValue :'$1'.

% not referenced yet AbsoluteReference -> '@' GlobalModuleReference '.' ItemSpec :{'$2','$4'}.

% not referenced yet ItemSpec -> typereference :'$1'.
% not referenced yet ItemSpec -> ItemId '.' ComponentId : {'$1','$3'}.

% not referenced yet ItemId -> ItemSpec : '$1'.

% not referenced yet ComponentId -> identifier :'$1'.
% not referenced yet ComponentId -> number :'$1'.
% not referenced yet ComponentId -> '*' :'$1'.

TypeAssignment -> typereference '::=' Type :
	#typedef{pos=element(2,'$1'),name=element(3,'$1'),typespec='$3'}.

ValueAssignment -> identifier Type '::=' Value :
	#valuedef{pos=element(2,'$1'),name=element(3,'$1'),type='$2',value='$4'}.

% later ValueSetTypeAssignment -> typereference Type '::=' ValueSet :{'ValueSetTypeAssignment','$1','$2','$4'}.


ValueSet -> '{' ElementSetSpec '}' : {valueset,'$2'}.

% record(type,{tag,def,constraint}).
Type -> BuiltinType :#type{def='$1'}.
Type -> 'NULL' :#type{def='NULL'}.
Type -> TaggedType:'$1'.
Type -> ReferencedType:#type{def='$1'}. % change notag later
Type -> ConstrainedType:'$1'.

%ANY is here for compatibility with the old ASN.1 standard from 1988
BuiltinType -> 'ANY' AnyDefBy:
	case '$2' of
		[] -> 'ANY';
	        _ -> {'ANY DEFINED BY','$2'}
        end.
BuiltinType -> BitStringType :'$1'.
BuiltinType -> 'BOOLEAN' :element(1,'$1').
BuiltinType -> CharacterStringType :'$1'.
BuiltinType -> ChoiceType :'$1'.
BuiltinType -> 'EMBEDDED' 'PDV' :'EMBEDDED PDV'.
BuiltinType -> EnumeratedType :'$1'.
BuiltinType -> 'EXTERNAL' :element(1,'$1').
% later BuiltinType -> InstanceOfType :'$1'.
BuiltinType -> IntegerType :'$1'.
% BuiltinType -> 'NULL' :element(1,'$1').
% later BuiltinType -> ObjectClassFieldType :'$1'.
BuiltinType -> 'OBJECT' 'IDENTIFIER' :'OBJECT IDENTIFIER'.
BuiltinType -> 'OCTET' 'STRING' :'OCTET STRING'.
BuiltinType -> 'REAL' :element(1,'$1').
BuiltinType -> SequenceType :'$1'.
BuiltinType -> SequenceOfType :'$1'.
BuiltinType -> SetType :'$1'.
BuiltinType -> SetOfType :'$1'.
% The so called Useful types
BuiltinType -> 'GeneralizedTime': 'GeneralizedTime'.
BuiltinType -> 'UTCTime' :'UTCTime'.
BuiltinType -> 'ObjectDescriptor' : 'ObjectDescriptor'.

% moved BuiltinType -> TaggedType :'$1'.


AnyDefBy -> 'DEFINED' 'BY' identifier: '$3'.
AnyDefBy -> '$empty': [].

NamedType -> identifier Type :
%{_,Pos,Val} = '$1',
%{'NamedType',Pos,{Val,'$2'}}.
V1 = '$1',
{'NamedType',V1#identifier.pos,{V1#identifier.val,'$2'}}.
NamedType -> SelectionType :'$1'.

ReferencedType -> DefinedType : '$1'.
% redundant ReferencedType -> UsefulType : 'fix'.
ReferencedType -> SelectionType : '$1'.
ReferencedType -> TypeFromObject : '$1'.
% later ReferencedType -> ValueSetFromObjects : 'fix'.

% to much conflicts Value -> AnyValue :'$1'.
Value -> ValueNotNull : '$1'.
Value -> 'NULL' :element(1,'$1').

ValueNotNull -> BuiltinValue :'$1'.
% inlined Value -> DefinedValue :'$1'. % DefinedValue , identifier
% inlined Externalvaluereference -> Externalvaluereference :'$1'.
ValueNotNull -> typereference '.' identifier :
	#'Externalvaluereference'{pos=element(2,'$1'),module=element(3,'$1'),
	value=element(3,'$3')}.
ValueNotNull -> identifier :'$1'.


%tmp Value -> NamedNumber: '$1'. % not a value but part of ObjIdC
% redundant BuiltinValue -> BitStringValue :'$1'.
BuiltinValue -> BooleanValue :'$1'.
BuiltinValue -> CharacterStringValue :'$1'.
BuiltinValue -> ChoiceValue :'$1'.
% BuiltinValue -> EmbeddedPDVValue :'$1'. ==SequenceValue
% BuiltinValue -> EnumeratedValue :'$1'. identifier
% BuiltinValue -> ExternalValue :'$1'. ==SequenceValue
% later BuiltinValue -> InstanceOfValue :'$1'.
BuiltinValue -> SignedNumber :'$1'.
% BuiltinValue -> 'NULL' :'$1'.
% later BuiltinValue -> ObjectClassFieldValue :'$1'.
% replaced by SAndSOfValue BuiltinValue -> ObjectIdentifierValue :'$1'.
BuiltinValue -> bstring :element(3,'$1').
BuiltinValue -> hstring :element(3,'$1').
% conflict BuiltinValue -> RealValue :'$1'.
BuiltinValue -> SAndSOfValue :'$1'.
% replaced BuiltinValue -> SequenceOfValue :'$1'.
% replaced BuiltinValue -> SequenceValue :'$1'.
% replaced BuiltinValue -> SetValue :'$1'.
% replaced BuiltinValue -> SetOfValue :'$1'.
% conflict redundant BuiltinValue -> TaggedValue :'$1'.

% inlined ReferencedValue -> DefinedValue:'$1'.
% ReferencedValue -> Externalvaluereference:'$1'.
% ReferencedValue -> identifier :'$1'.
% later ReferencedValue -> ValueFromObject:'$1'.

% inlined BooleanType -> BOOLEAN :'BOOLEAN'.

% to much conflicts AnyValue -> Type ':' Value : {'ANYVALUE',{'$1','$3'}}.

BooleanValue -> TRUE :true.
BooleanValue -> FALSE :false.

IntegerType -> 'INTEGER' : 'INTEGER'.
IntegerType -> 'INTEGER' '{' NamedNumberList '}' : {'INTEGER','$3'}.

NamedNumberList -> NamedNumber :['$1'].
% modified NamedNumberList -> NamedNumberList ',' NamedNumber :'fix'.
NamedNumberList -> NamedNumber ',' NamedNumberList :['$1'|'$3'].

NamedNumber -> identifier '(' SignedNumber ')' : {'NamedNumber',element(3,'$1'),'$3'}.
NamedNumber -> identifier '(' typereference '.' identifier ')' : {'NamedNumber',element(3,'$1'),{'ExternalValue',element(3,'$3'),element(3,'$5')}}.
NamedNumber -> identifier '(' identifier ')' : {'NamedNumber',element(3,'$1'),element(3,'$3')}.

%NamedValue -> identifier Value :
%	{'NamedValue',element(2,'$1'),element(3,'$1'),'$2'}.


SignedNumber -> number : element(3,'$1').
SignedNumber -> '-' number : - element(3,'$1').

% inlined IntegerValue -> SignedNumber :'$1'.
% conflict moved to Value IntegerValue -> identifier:'$1'.

EnumeratedType -> ENUMERATED '{' Enumeration '}' :{'ENUMERATED','$3'}.

% inlined Enumerations -> Enumeration :{'$1','false',[]}.
% inlined Enumerations -> Enumeration ',' '...' : {'$1','true',[]}.
% inlined Enumerations -> Enumeration ',' '...' ',' Enumeration : {'$1','true','$5'}.

Enumeration -> EnumerationItem :['$1'].
% modified Enumeration -> EnumerationItem ',' Enumeration :'fix'.
Enumeration -> EnumerationItem ',' Enumeration :['$1'|'$3'].

EnumerationItem -> identifier:element(3,'$1').
EnumerationItem -> NamedNumber :'$1'.
EnumerationItem -> '...' :'EXTENSIONMARK'.

% conflict moved to Value EnumeratedValue -> identifier:'$1'.

% inlined RealType -> REAL:'REAL'.

RealValue -> NumericRealValue :'$1'.
RealValue -> SpecialRealValue:'$1'.

% ?? NumericRealValue -> number:'$1'. % number MUST BE '0'
NumericRealValue -> SAndSOfValue : '$1'. % Value of the associated sequence type

SpecialRealValue -> 'PLUS-INFINITY' :'$1'.
SpecialRealValue -> 'MINUS-INFINITY' :'$1'.

BitStringType -> 'BIT' 'STRING' :{'BIT STRING',[]}.
BitStringType -> 'BIT' 'STRING' '{' NamedNumberList '}' :{'BIT STRING','$4'}.
% NamedBitList replaced by NamedNumberList to reduce the grammar
% Must check later that all "numbers" are positive

% inlined BitStringValue -> bstring:'$1'.
% inlined BitStringValue -> hstring:'$1'.
% redundant use SequenceValue BitStringValue -> '{' IdentifierList '}' :$2.
% redundant use SequenceValue BitStringValue -> '{' '}' :'fix'.

IdentifierList -> identifier :[element(3,'$1')].
% modified IdentifierList -> IdentifierList ',' identifier :'$1'.
IdentifierList -> identifier ',' IdentifierList :[element(3,'$1')|'$3'].

% inlined OctetStringType -> 'OCTET' 'STRING' :'OCTET STRING'.

% inlined OctetStringValue -> bstring:'$1'.
% inlined OctetStringValue -> hstring:'$1'.

% inlined NullType -> 'NULL':'NULL'.

% inlined NullValue -> NULL:'NULL'.

% result is {'SEQUENCE',Optionals,Extensionmark,Componenttypelist}.
SequenceType -> SEQUENCE '{' ComponentTypeList '}' :{'SEQUENCE','$3'}.
% SequenceType -> SEQUENCE '{' ComponentTypeLists '}' :{'SEQUENCE','$3'}.
% SequenceType -> SEQUENCE '{' ExtensionAndException '}' :{'SEQUENCE','$3'}.
SequenceType -> SEQUENCE '{' '}' :{'SEQUENCE',[]}.

% result is {RootComponentList,ExtensionAndException,AdditionalComponentTypeList}.
%ComponentTypeLists -> ComponentTypeList ',' ExtensionAndException :{'$1','$3',[]}.
%ComponentTypeLists -> ComponentTypeList :{'$1','false',[]}.
%ComponentTypeLists -> ComponentTypeList ',' ExtensionAndException
%	',' ComponentTypeList :{'$1','$3', '$5'}.
%ComponentTypeLists -> ExtensionAndException ',' ComponentTypeList :{[],'$1','$3'}.

ComponentTypeList -> ComponentType :['$1'].
% modified below ComponentTypeList -> ComponentTypeList ',' ComponentType :'$1'.
ComponentTypeList -> ComponentType ',' ComponentTypeList  :['$1'|'$3'].

% -record('ComponentType',{pos,name,type,attrib}).
ComponentType -> '...' ExceptionSpec :{'EXTENSIONMARK',element(2,'$1'),'$2'}.
ComponentType -> NamedType :
	{'NamedType',Pos,{Name,Type}} = '$1',
	#'ComponentType'{pos=Pos,name=Name,typespec=Type,prop=mandatory}.
ComponentType -> NamedType 'OPTIONAL' :
	{'NamedType',Pos,{Name,Type}} = '$1',
	#'ComponentType'{pos=Pos,name=Name,typespec=Type,prop='OPTIONAL'}.
ComponentType -> NamedType 'DEFAULT' Value:
	{'NamedType',Pos,{Name,Type}} = '$1',
	#'ComponentType'{pos=Pos,name=Name,typespec=Type,prop={'DEFAULT','$3'}}.
ComponentType -> 'COMPONENTS' 'OF' Type :{'COMPONENTS OF','$3'}.

% redundant ExtensionAndException -> '...' : extensionmark.
% ExtensionAndException -> '...' ExceptionSpec : {extensionmark,'$2'}.

% replaced SequenceValue -> '{' ComponentValueList '}':'$2'.
% replaced SequenceValue -> '{'  '}':[].

ValueList -> Value :['$1'].
ValueList -> NamedNumber :['$1'].
% modified ValueList -> ValueList ',' Value :'$1'.
ValueList -> Value ',' ValueList :['$1'|'$3'].
ValueList -> Value ',' '...' :['$1' |[]].
ValueList -> Value ValueList : ['$1',space|'$2'].
ValueList -> NamedNumber ValueList: ['$1',space|'$2'].

%ComponentValueList -> identifier ObjIdComponent:[{'NamedValue','$1','$2'}].
%ComponentValueList -> NamedValue :['$1'].
%ComponentValueList -> NamedValue ',' ComponentValueList:['$1'|'$3'].
%ComponentValueList -> identifier ObjIdComponent ',' ComponentValueList :[{'NamedValue', '$1','$2'}|'$4'].

SequenceOfType -> SEQUENCE OF Type : {'SEQUENCE OF','$3'}.

% replaced SequenceOfValue with SAndSOfValue

SAndSOfValue -> '{' ValueList '}' :'$2'.
%SAndSOfValue -> '{' ComponentValueList '}' :'$2'.
SAndSOfValue -> '{'  '}' :[].

% save for later SetType ->
% result is {'SET',Optionals,Extensionmark,Componenttypelist}.
SetType -> SET '{' ComponentTypeList '}' :{'SET','$3'}.
% SetType -> SET '{' ExtensionAndException '}' :{'SET','$3'}.
SetType -> SET '{' '}' :{'SET',[]}.

% replaced SetValue with SAndSOfValue

SetOfType -> SET OF Type : {'SET OF','$3'}.

% replaced SetOfValue with SAndSOfValue

ChoiceType -> 'CHOICE' '{' ComponentTypeList '}' :{'CHOICE','$3'}.
%  AlternativeTypeList is replaced by ComponentTypeList
ChoiceValue -> identifier ':' Value : {'ChoiceValue',element(3,'$1'),'$3'}.
% save for later SelectionType ->

TaggedType -> Tag Type : '$2'#type{tag=['$1'#tag{type={default,get(tagdefault)}}]}.
TaggedType -> Tag IMPLICIT Type :'$3'#type{tag=['$1'#tag{type='IMPLICIT'}]}.
TaggedType -> Tag EXPLICIT Type :'$3'#type{tag=['$1'#tag{type='EXPLICIT'}]}.

Tag -> '[' Class ClassNumber ']': #tag{class='$2',number='$3'}.
Tag -> '[' Class typereference '.' identifier ']':
	#tag{class='$2',number=#'Externalvaluereference'{pos=element(2,'$3'),module=element(3,'$3'),
	value=element(3,'$5')}}.
Tag -> '[' Class number ']': #tag{class='$2',number=element(3,'$3')}.
Tag -> '[' Class identifier ']': #tag{class='$2',number=element(3,'$3')}.

ClassNumber -> number :element(3,'$1').
% inlined above ClassNumber -> typereference '.' identifier :{'Externalvaluereference',element(3,'$1'),element(3,'$3')}.
ClassNumber -> identifier :element(3,'$1').

Class -> 'UNIVERSAL' :element(1,'$1').
Class -> 'APPLICATION' :element(1,'$1').
Class -> 'PRIVATE' :element(1,'$1').
Class -> '$empty' :'CONTEXT'.

% conflict redundant TaggedValue -> Value:'$1'.

% inlined EmbeddedPDVType -> 'EMBEDDED' 'PDV' :'EMBEDDED PDV'.

% inlined EmbeddedPDVValue -> SequenceValue:'$1'.

% inlined ExternalType -> 'EXTERNAL' :'EXTERNAL'.

% inlined ExternalValue -> SequenceValue :'$1'.

% inlined ObjectIdentifierType -> 'OBJECT' 'IDENTIFIER' :'OBJECT IDENTIFIER'.

ObjectIdentifierValue -> '{' ObjIdComponentList '}' :'$2'.
%  inlined ObjectIdentifierValue -> SequenceAndSequenceOfValue :'$1'.
% ObjectIdentifierValue -> '{' identifier ObjIdComponentList '}' :{'ObjectIdentifierValue','$2','$3'}.
% ObjectIdentifierValue -> '{' typereference '.' identifier ObjIdComponentList '}' :{'ObjectIdentifierValue',{'$2','$4'},'$5'}.

ObjIdComponentList -> Value:'$1'.
ObjIdComponentList -> Value ObjIdComponentList :['$1'|'$2'].
%ObjIdComponentList -> DefinedValue:'$1'.
%ObjIdComponentList -> number:'$1'.
%ObjIdComponentList -> DefinedValue ObjIdComponentList :['$1'|'$2'].
%ObjIdComponentList -> number ObjIdComponentList :['$1'|'$2'].
%ObjIdComponentList -> ObjIdComponent ObjIdComponentList :['$1'|'$2'].
%ObjIdComponentList -> ObjIdComponent ObjIdComponentList :['$1'|'$2'].

% redundant ObjIdComponent -> NameForm :'$1'. % expanded
% replaced by 2 ObjIdComponent -> NumberForm :'$1'.
% ObjIdComponent -> number :'$1'.
% ObjIdComponent -> DefinedValue :'$1'. % means DefinedValue
% ObjIdComponent -> NameAndNumberForm :'$1'.
% ObjIdComponent -> NamedNumber :'$1'.
% NamedBit replaced by NamedNumber to reduce grammar
% must check later that "number" is positive

% NameForm -> identifier:'$1'.

% inlined NumberForm -> number :'$1'.
% inlined NumberForm -> DefinedValue :'$1'.

% replaced by NamedBit  NameAndNumberForm -> identifier '(' NumberForm ')'.
% NameAndNumberForm -> NamedBit:'$1'.


CharacterStringType -> restrictedcharacterstringtype :element(3,'$1').
CharacterStringType -> 'CHARACTER' 'STRING' :'CHARACTER STRING'.

RestrictedCharacterStringValue -> cstring :element(3, '$1').
% modified below RestrictedCharacterStringValue -> CharacterStringList :'$1'.
% conflict vs BuiltinValue RestrictedCharacterStringValue -> SequenceAndSequenceOfValue :'$1'.
RestrictedCharacterStringValue -> Quadruple :'$1'.
RestrictedCharacterStringValue -> Tuple :'$1'.

% redundant CharacterStringList -> '{' ValueList '}' :'$2'. % modified

% redundant CharSyms -> CharsDefn :'$1'.
% redundant CharSyms -> CharSyms ',' CharsDefn :['$1'|'$3'].

% redundant CharsDefn -> cstring :'$1'.
% temporary replaced see below CharsDefn -> DefinedValue :'$1'.
% redundant CharsDefn -> Value :'$1'.

Quadruple -> '{' number ',' number ',' number ',' number '}' :{'Quadruple','$2','$4','$6','$8'}.
% {Group,Plane,Row,Cell}

Tuple -> '{' number ',' number '}' :{'Tuple', '$2','$4'}.
% {TableColumn,TableRow}

% inlined UnrestrictedCharacterString -> 'CHARACTER' 'STRING' :'CHARACTER STRING'.

CharacterStringValue -> RestrictedCharacterStringValue :'$1'.
% conflict vs BuiltinValue CharacterStringValue -> SequenceValue :'$1'. % UnrestrictedCharacterStringValue

% inlined UsefulType -> typereference :'$1'.

SelectionType -> identifier '<' Type : {'SelectionType',element(3,'$1'),'$3'}.

ConstrainedType -> Type Constraint :
	'$1'#type{constraint=merge_constraints(['$2'])}.
ConstrainedType -> Type Constraint Constraint :
	'$1'#type{constraint=merge_constraints(['$2','$3'])}.
ConstrainedType -> Type Constraint Constraint Constraint:
	'$1'#type{constraint=merge_constraints(['$2','$3','$4'])}.
ConstrainedType -> Type Constraint Constraint Constraint Constraint:
	'$1'#type{constraint=merge_constraints(['$2','$3','$4','$5'])}.
%ConstrainedType -> Type Constraint :'$1'#type{constraint='$2'}.
%ConstrainedType -> Type Constraint :'$1'#type{constraint='$2'}.
ConstrainedType -> TypeWithConstraint :'$1'.

TypeWithConstraint -> 'SET' Constraint 'OF' Type :
	#type{def = {'SET OF','$4'},constraint=merge_constraints(['$2'])}.
TypeWithConstraint -> 'SET' 'SIZE' Constraint 'OF' Type :
	#type{def = {'SET OF','$5'},constraint = merge_constraints([#constraint{c={'SizeConstraint','$3'#constraint.c}}])}.
TypeWithConstraint -> 'SEQUENCE' Constraint 'OF' Type :
	#type{def = {'SEQUENCE OF','$4'},constraint =
	merge_constraints(['$2'])}.
TypeWithConstraint -> 'SEQUENCE' 'SIZE' Constraint 'OF' Type :
	#type{def = {'SEQUENCE OF','$5'},constraint = merge_constraints([#constraint{c={'SizeConstraint','$3'#constraint.c}}])}.


Constraint -> '(' ConstraintSpec ExceptionSpec ')' :
		#constraint{c='$2',e='$3'}.

% inlined Constraint -> SubTypeConstraint :'$1'.
ConstraintSpec -> ElementSetSpecs :'$1'.
ConstraintSpec -> UserDefinedConstraint :'$1'.
ConstraintSpec -> TableConstraint :'$1'.

TableConstraint -> ComponentRelationConstraint : '$1'.
TableConstraint -> ObjectSet : '$1'.
%TableConstraint -> '{' typereference '}' :tableconstraint.

ComponentRelationConstraint -> '{' typereference '}' '{' '@' ComponentIdList '}' : componentrelation.
ComponentRelationConstraint -> '{' typereference '}' '{' '@' '.' ComponentIdList '}' : componentrelation.

ComponentIdList -> identifier: ['$1'].
ComponentIdList -> identifier '.' ComponentIdList: ['$1'| '$3'].


% later ConstraintSpec -> GeneralConstraint :'$1'.

% from X.682
UserDefinedConstraint -> 'CONSTRAINED' 'BY' '{' '}' : {constrained_by,[]}.
UserDefinedConstraint -> 'CONSTRAINED' 'BY'
	'{' UserDefinedConstraintParameters '}' : {constrained_by,'$4'}.

UserDefinedConstraintParameters -> UserDefinedConstraintParameter : ['$1'].
UserDefinedConstraintParameters ->
	UserDefinedConstraintParameter ','
	UserDefinedConstraintParameters: ['$1'|'$3'].

UserDefinedConstraintParameter -> Type '.' ActualParameter : {'$1','$3'}.
UserDefinedConstraintParameter -> ActualParameter : '$1'.



ExceptionSpec -> '!' ExceptionIdentification : '$1'.
ExceptionSpec -> '$empty' : undefined.

ExceptionIdentification -> SignedNumber : '$1'.
% inlined ExceptionIdentification -> DefinedValue : '$1'.
ExceptionIdentification -> typereference '.' identifier :
	#'Externalvaluereference'{pos=element(2,'$1'),module=element(3,'$1'),
	value=element(3,'$1')}.
ExceptionIdentification -> identifier :'$1'.
ExceptionIdentification -> Type ':' Value : {'$1','$3'}.

% inlined SubTypeConstraint -> ElementSetSpec

ElementSetSpecs -> ElementSetSpec : '$1'.
ElementSetSpecs -> ElementSetSpec ',' '...': {'$1',[]}.
ElementSetSpecs -> '...' ',' ElementSetSpec : {[],'$3'}.
ElementSetSpecs -> ElementSetSpec ',' '...' ',' ElementSetSpec : {'$1','$5'}.

ElementSetSpec -> Unions : '$1'.
ElementSetSpec -> 'ALL' Exclusions : {'ALL','$2'}.

Unions -> Intersections : '$1'.
Unions -> UElems UnionMark IntersectionElements :
	case {'$1','$3'} of
	{{'SingleValue',V1},{'SingleValue',V2}} ->
		{'SingleValue',ordsets:union(to_set(V1),to_set(V2))}
	end.

UElems -> Unions :'$1'.

Intersections -> IntersectionElements :'$1'.
Intersections -> IElems IntersectionMark IntersectionElements :
	case {'$1','$3'} of
	{{'SingleValue',V1},{'SingleValue',V2}} ->
		{'SingleValue',ordsets:intersection(to_set(V1),to_set(V2))};
	{V1,V2} when list(V1) ->
	        V1 ++ [V2];
	{V1,V2} ->
	        [V1,V2]
	end.
%Intersections -> IElems '^' IntersectionElements :{'INTERSECTION','$1','$3'}.
%Intersections -> IElems 'INTERSECTION' IntersectionElements :{'INTERSECTION','$1','$3'}.

IElems -> Intersections :'$1'.

IntersectionElements -> Elements :'$1'.
IntersectionElements -> Elems Exclusions :{'$1','$2'}.

Elems -> Elements :'$1'.

Exclusions -> 'EXCEPT' Elements :{'EXCEPT','$2'}.

IntersectionMark -> 'INTERSECTION':'$1'.
IntersectionMark -> '^':'$1'.
UnionMark -> 'UNION':'$1'.
UnionMark -> '|':'$1'.


Elements -> SubTypeElements : '$1'.
%Elements -> ObjectSetElements : '$1'.
Elements -> '(' ElementSetSpec ')' : '$2'.
Elements -> ReferencedType : '$1'.

SubTypeElements -> ValueList : {'SingleValue','$1'}. % NOTE it must be a Value
% The rule above modifyed only because of conflicts
SubTypeElements -> 'INCLUDES' Type : {'ContainedSubType','$2'}.
%not lalr1 if this is activated SubTypeElements -> Type : {'TypeConstraint','$1'}.
SubTypeElements -> LowerEndpoint '..' UpperEndpoint : {'ValueRange',{'$1','$3'}}.
SubTypeElements -> 'FROM' Constraint : {'PermittedAlphabet','$2'#constraint.c}.
SubTypeElements -> 'SIZE' Constraint: {'SizeConstraint','$2'#constraint.c}.
% later will introduce conflicts related to NULL SubTypeElements -> Type : {'TypeConstraint','$1'}.
SubTypeElements -> 'WITH' 'COMPONENT' Constraint:{'WITH COMPONENT','$3'}.
SubTypeElements -> 'WITH' 'COMPONENTS' '{' TypeConstraints '}':{'WITH COMPONENTS',{'FullSpecification','$4'}}.
SubTypeElements -> 'WITH' 'COMPONENTS' '{' '...' ',' TypeConstraints '}' :{'WITH COMPONENTS',{'PartialSpecification','$3'}}.

% inlined above InnerTypeConstraints ::=
% inlined above	SingleTypeConstraint::= Constraint
% inlined above MultipleTypeConstraints ::= FullSpecification | PartialSpecification
% inlined above FullSpecification   ::= "{" TypeConstraints "}"
% inlined above PartialSpecification ::= "{"  "..."   ","   TypeConstraints "}"
% TypeConstraints -> identifier : [{'NamedConstraint',element(3,'$1'),undefined,undefined}]. % is this really meaningful or allowed
TypeConstraints -> NamedConstraint : ['$1'].
TypeConstraints -> NamedConstraint ',' TypeConstraints : ['$1'|'$3'].
TypeConstraints -> identifier : ['$1'].
TypeConstraints -> identifier ',' TypeConstraints : ['$1'|'$3'].

NamedConstraint -> identifier Constraint PresenceConstraint :{'NamedConstraint',element(3,'$1'),'$2','$3'}.
NamedConstraint -> identifier Constraint  :{'NamedConstraint',element(3,'$1'),'$2',undefined}.
NamedConstraint -> identifier PresenceConstraint :{'NamedConstraint',element(3,'$1'),undefined,'$2'}.

PresenceConstraint -> 'PRESENT' : 'PRESENT'.
PresenceConstraint -> 'ABSENT' : 'ABSENT'.
PresenceConstraint -> 'OPTIONAL' : 'OPTIONAL'.



LowerEndpoint -> LowerEndValue :'$1'.
%LowerEndpoint -> LowerEndValue '<':{gt,'$1'}.
LowerEndpoint -> LowerEndValue '<':('$1'+1).

UpperEndpoint -> UpperEndValue :'$1'.
%UpperEndpoint -> '<' UpperEndValue :{lt,'$2'}.
UpperEndpoint -> '<' UpperEndValue :('$2'-1).

LowerEndValue -> Value :'$1'.
LowerEndValue -> 'MIN' :'MIN'.

UpperEndValue -> Value :'$1'.
UpperEndValue -> 'MAX' :'MAX'.


% X.681


% X.681 chap 15

%TypeFromObject -> ReferencedObjects '.' FieldName : {'$1','$3'}.
TypeFromObject -> typereference '.' FieldName : {'$1','$3'}.

ReferencedObjects -> typereference : '$1'.
%ReferencedObjects -> ParameterizedObject
%ReferencedObjects -> DefinedObjectSet
%ReferencedObjects -> ParameterizedObjectSet

FieldName -> typefieldreference : ['$1'].
FieldName -> valuefieldreference : ['$1'].
FieldName -> FieldName '.' FieldName : ['$1' | '$3'].

PrimitiveFieldName -> typefieldreference : '$1'.
PrimitiveFieldName -> valuefieldreference : '$1'.

%ObjectSetAssignment -> typereference DefinedObjectClass '::=' ObjectSet: null.
ObjectSetAssignment -> typereference typereference '::=' ObjectSet :
	#typedef{pos=element(2,'$1'),name=element(3,'$1'),typespec={'ObjectSet',element(3,'$2'), '$4'}}.
ObjectSetAssignment -> typereference typereference '.' typereference '::=' ObjectSet.

ObjectSet -> '{' ElementSetSpecs '}' : '$2'.
ObjectSet -> '{' '...' '}' : ['EXTENSIONMARK'].

%ObjectSetElements -> Object.
% ObjectSetElements -> identifier : '$1'.
%ObjectSetElements -> DefinedObjectSet.
%ObjectSetElements -> ObjectSetFromObjects.
%ObjectSetElements -> ParameterizedObjectSet.

%ObjectAssignment -> identifier DefinedObjectClass '::=' Object.
ObjectAssignment -> ValueAssignment.
%ObjectAssignment -> identifier typereference '::=' Object.
%ObjectAssignment -> identifier typereference '.' typereference '::=' Object.

%Object -> DefinedObject: '$1'.
%Object -> ExternalObjectReference: '$1'.%Object -> DefinedObject: '$1'.
Object -> typereference '.' identifier: '$1'.%Object -> DefinedObject: '$1'.
Object -> identifier: '$1'.%Object -> DefinedObject: '$1'.

%Object -> ObjectDefn -> DefaultSyntax: '$1'.
Object -> '{' FieldSetting ',' FieldSettings '}' : ['$2'|'$4'].
Object -> '{' FieldSetting '}' :['$2'].

%% For User-friendly notation
%% Object -> ObjectDefn -> DefinedSyntax
Object -> '{' '}'.
Object -> '{' DefinedSyntaxTokens '}'.

% later Object -> ParameterizedObject: '$1'. look in x.683

%DefinedObject -> ExternalObjectReference: '$1'.
%DefinedObject -> identifier: '$1'.

DefinedObjectClass -> typereference.
%DefinedObjectClass -> objectclassreference.
DefinedObjectClass -> ExternalObjectClassReference.
%DefinedObjectClass -> typereference '.' objectclassreference.
%%DefinedObjectClass -> UsefulObjectClassReference.

ExternalObjectReference -> typereference '.' identifier.
ExternalObjectClassReference -> typereference '.' typereference.
%%ExternalObjectClassReference -> typereference '.' objectclassreference.

ObjectDefn -> DefaultSyntax: '$1'.
%ObjectDefn -> DefinedSyntax: '$1'.

ObjectFromObject -> ReferencedObjects '.' FieldName : {'ObjectFromObject','$1','$3'}.

% later look in x.683 ParameterizedObject ->

%DefaultSyntax -> '{' '}'.
%DefaultSyntax -> '{' FieldSettings '}': '$2'.
DefaultSyntax -> '{' FieldSetting ',' FieldSettings '}': '$2'.
DefaultSyntax -> '{' FieldSetting '}': '$2'.

FieldSetting -> PrimitiveFieldName Setting: {'$1','$2'}.

FieldSettings -> FieldSetting ',' FieldSettings: ['$1'|'$3'].
FieldSettings -> FieldSetting ',' FieldSettings: ['$1'|'$3'].
FieldSettings -> FieldSetting: '$1'.

%DefinedSyntax -> '{' '}'.
DefinedSyntax -> '{' DefinedSyntaxTokens '}': '$2'.

DefinedSyntaxTokens -> DefinedSyntaxToken: '$1'.
DefinedSyntaxTokens -> DefinedSyntaxToken DefinedSyntaxTokens: ['$1'|'$2'].

% expanded DefinedSyntaxToken -> Literal: '$1'.
%DefinedSyntaxToken -> typereference: '$1'.
DefinedSyntaxToken -> word: '$1'.
DefinedSyntaxToken -> ',': '$1'.
DefinedSyntaxToken -> Setting: '$1'.
%DefinedSyntaxToken -> '$empty': nil .

% Setting ::= Type|Value|ValueSet|Object|ObjectSet
Setting -> Type: '$1'.
%Setting -> Value: '$1'.
%Setting -> ValueNotNull: '$1'.
Setting -> BuiltinValue: '$1'.
Setting -> ValueSet: '$1'.
%Setting -> Object: '$1'.
%Setting -> ExternalObjectReference.
Setting -> typereference '.' identifier.
Setting -> identifier.
Setting -> ObjectDefn.

Setting -> ObjectSet: '$1'.


Erlang code.
%%-author('kenneth@erix.ericsson.se').
-copyright('Copyright (c) 1991-99 Ericsson Telecom AB').
-vsn('$Revision: 1.1 $').
-include("asn1_records.hrl").

to_set(V) when list(V) ->
	ordsets:list_to_set(V);
to_set(V) ->
	ordsets:list_to_set([V]).

merge_constraints({Rlist,ExtList}) -> % extensionmarker in constraint
	{merge_constraints(Rlist,[],[]),
	 merge_constraints(ExtList,[],[])};

merge_constraints(Clist) ->
	merge_constraints(Clist, [], []).

merge_constraints([Ch|Ct],Cacc, Eacc) ->
	NewEacc = case Ch#constraint.e of
			undefined -> Eacc;
			E -> [E|Eacc]
		  end,
	merge_constraints(Ct,[fixup_constraint(Ch#constraint.c)|Cacc],NewEacc);

merge_constraints([],Cacc,[]) ->
	lists:flatten(Cacc);
merge_constraints([],Cacc,Eacc) ->
	lists:flatten(Cacc) ++ [{'Errors',Eacc}].

fixup_constraint(C) ->
	case C of
		{'SingleValue',V} when list(V) ->
			[C,
			{'ValueRange',{lists:min(V),lists:max(V)}}];
		{'PermittedAlphabet',{'SingleValue',V}} when list(V) ->
			V2 = {'SingleValue',
			       ordsets:list_to_set(lists:flatten(V))},
			{'PermittedAlphabet',V2};
		{'PermittedAlphabet',{'SingleValue',V}} ->
			V2 = {'SingleValue',[V]},
			{'PermittedAlphabet',V2};
		{'SizeConstraint',Sc} ->
			{'SizeConstraint',fixup_size_constraint(Sc)};

		List when list(List) ->
			[fixup_constraint(Xc)||Xc <- List];
		Other ->
			Other
	end.

fixup_size_constraint({'ValueRange',{Lb,Ub}}) ->
	{Lb,Ub};
fixup_size_constraint({{'ValueRange',R},[]}) ->
	{R,[]};
fixup_size_constraint({[],{'ValueRange',R}}) ->
	{[],R};
fixup_size_constraint({{'ValueRange',R1},{'ValueRange',R2}}) ->
	{R1,R2};
fixup_size_constraint({'SingleValue',[Sv]}) ->
	fixup_size_constraint({'SingleValue',Sv});
fixup_size_constraint({'SingleValue',L}) when list(L) ->
	ordsets:list_to_set(L);
fixup_size_constraint({'SingleValue',L}) ->
	{L,L};
fixup_size_constraint({C1,C2}) ->
	{fixup_size_constraint(C1), fixup_size_constraint(C2)}.
