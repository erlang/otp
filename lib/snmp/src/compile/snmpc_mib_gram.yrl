%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
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
%% %CopyrightEnd%
%%

%%----------------------------------------------------------------------
%% Number of expected shift/reduce warnings
%% This is ugly but...
%%----------------------------------------------------------------------

Expect 2.


%% ----------------------------------------------------------------------
Nonterminals 
%% ----------------------------------------------------------------------
accessv1
definition
defvalpart
description
descriptionfield
displaypart
entry
namedbits
fatherobjectname
fieldname
fields
implies
import
import_stuff
imports
imports_from_one_mib
index
indexpartv1
indextypev1
indextypesv1
parentintegers
listofdefinitions
listofimports
mib
mibname
nameassign
newtype
newtypename
objectidentifier
objectname
objecttypev1
prodrel
range_num
referpart
size
sizedescr
statusv1
syntax
tableentrydefinition
traptype
type
usertype
variables
varpart

%v2
moduleidentity
revisionpart
revisions
listofdefinitionsv2
mibid
last_updated
organization
contact_info
revision
revision_string
revision_desc
v1orv2
objectidentity
objecttypev2
unitspart
indexpartv2
indextypesv2
indextypev2
statusv2
accessv2
notification
objectspart
objects
definitionv2
textualconvention
objectgroup
notificationgroup
modulecompliance
mc_modulepart
mc_modules
mc_module
mc_modulenamepart
mc_mandatorypart
mc_compliancepart
mc_compliances
mc_compliance
mc_compliancegroup
mc_object
mc_accesspart
agentcapabilities
ac_status
ac_modulepart
ac_modules
ac_module
ac_modulenamepart
ac_variationpart
ac_variations
ac_variation
ac_accesspart
ac_access
ac_creationpart
syntaxpart
writesyntaxpart
fsyntax
defbitsvalue
defbitsnames
.
%% ----------------------------------------------------------------------
Terminals 
%% ----------------------------------------------------------------------
integer variable atom string quote '{' '}' '::=' ':' '=' ',' '.' '(' ')' ';' '|'
'ACCESS'
'BEGIN'
'BIT'
'Counter'
'DEFINITIONS'
'DEFVAL'
'DESCRIPTION'
'DISPLAY-HINT'
'END'
'ENTERPRISE'
'FROM'
'Gauge'
'IDENTIFIER'
'IMPORTS'
'INDEX'
'INTEGER'
'IpAddress'
'NetworkAddress'
'OBJECT'
'OBJECT-TYPE'
'OCTET'
'OF'
'Opaque'
'REFERENCE'
'SEQUENCE'
'SIZE'
'STATUS'
'STRING'
'SYNTAX'
'TRAP-TYPE'
'TimeTicks'
'VARIABLES'

%v2
'LAST-UPDATED'
'ORGANIZATION'
'CONTACT-INFO'
'MODULE-IDENTITY'
'NOTIFICATION-TYPE'
'PRODUCT-RELEASE'
'AGENT-CAPABILITIES'
'INCLUDES'
'SUPPORTS'
'VARIATION'
'CREATION-REQUIRES'
'MODULE-COMPLIANCE'
'OBJECT-GROUP'
'NOTIFICATION-GROUP'
'REVISION'
'OBJECT-IDENTITY'
'MAX-ACCESS'
'UNITS'
'AUGMENTS'
'IMPLIED'
'OBJECTS'
'TEXTUAL-CONVENTION'
'NOTIFICATIONS'
'MODULE'
'MANDATORY-GROUPS'
'GROUP'
'WRITE-SYNTAX'
'MIN-ACCESS'
'BITS'
'DisplayString' 
'PhysAddress' 
'MacAddress' 
'TruthValue' 
'TestAndIncr' 
'AutonomousType' 
'InstancePointer' 
'VariablePointer' 
'RowPointer' 
'RowStatus' 
'TimeStamp' 
'TimeInterval' 
'DateAndTime' 
'StorageType' 
'TDomain' 
'TAddress'
.


Rootsymbol mib.
Endsymbol '$end'.

% **********************************************************************

mib -> mibname 'DEFINITIONS' implies 'BEGIN'
       import v1orv2 'END' 
    : {Version, Defs} = '$6',
      #pdata{mib_version = Version, 
             mib_name    = '$1', 
             imports     = '$5', 
             defs        = Defs}.

v1orv2 -> moduleidentity listofdefinitionsv2 :
			  {v2_mib, ['$1'|lreverse(v1orv2_mod, '$2')]}.
v1orv2 -> listofdefinitions : {v1_mib, lreverse(v1orv2_list, '$1')}.

definition -> objectidentifier : '$1'.
definition -> objecttypev1 : '$1'.
definition -> newtype : '$1'.
definition -> tableentrydefinition : '$1'.
definition -> traptype : '$1'.

listofdefinitions -> definition : ['$1'] .
listofdefinitions -> listofdefinitions definition : ['$2' | '$1'].

import -> '$empty' : [].
import -> 'IMPORTS' imports ';' : 
%%           i("import ->"
%% 	    "~n   imports: ~p", ['$2']), 
          '$2'.

imports -> imports_from_one_mib : 
%%            i("imports ->"
%%              "~n   imports_from_one_mib: ~p", ['$1']), 
           ['$1'].
imports -> imports_from_one_mib imports : 
%%            i("imports ->"
%%              "~n   imports_from_one_mib: ~p"
%%              "~n   imports:              ~p", ['$1', '$2']), 
           ['$1' | '$2'].

imports_from_one_mib -> listofimports 'FROM' variable :
%%                         i("imports_from_one_mib ->"
%%                           "~n   listofimports: ~p"
%%                           "~n   variable:      ~p", ['$1', '$3']), 
                        {{val('$3'), lreverse(imports_from_one_mib, '$1')}, line_of('$2')}.

listofimports -> import_stuff : 
%%                  i("listofimports ->"
%%                    "~n   import_stuff: ~p", ['$1']), 
                 ['$1'].
listofimports -> listofimports ',' import_stuff : 
%%                  i("listofimports ->"
%%                    "~n   listofimports: ~p"
%%                    "~n   import_stuff:  ~p", ['$1', '$3']), 
                 ['$3' | '$1'].

import_stuff -> 'OBJECT-TYPE' : {builtin, 'OBJECT-TYPE'}.
import_stuff -> 'TRAP-TYPE' : {builtin, 'TRAP-TYPE'}.
import_stuff -> 'NetworkAddress' : {builtin, 'NetworkAddress'}.
import_stuff -> 'TimeTicks' : {builtin, 'TimeTicks'}.
import_stuff -> 'IpAddress' : {builtin, 'IpAddress'}.
import_stuff -> 'Counter' : {builtin, 'Counter'}.
import_stuff -> 'Gauge' : {builtin, 'Gauge'}.
import_stuff -> 'Opaque' : {builtin, 'Opaque'}.
import_stuff -> variable : filter_v2imports(get(snmp_version), val('$1')).
import_stuff -> atom : {node, val('$1')}.
%%v2
import_stuff -> 'MODULE-IDENTITY'
       : ensure_ver(2,'$1'), {builtin, 'MODULE-IDENTITY'}.
import_stuff -> 'NOTIFICATION-TYPE' 
       : ensure_ver(2,'$1'), {builtin, 'NOTIFICATION-TYPE'}.
import_stuff -> 'AGENT-CAPABILITIES' 
       : ensure_ver(2,'$1'), {builtin, 'AGENT-CAPABILITIES'}.
import_stuff -> 'MODULE-COMPLIANCE' 
       : ensure_ver(2,'$1'), {builtin, 'MODULE-COMPLIANCE'}.
import_stuff -> 'NOTIFICATION-GROUP' 
       : ensure_ver(2,'$1'), {builtin, 'NOTIFICATION-GROUP'}.
import_stuff -> 'OBJECT-GROUP' 
       : ensure_ver(2,'$1'), {builtin, 'OBJECT-GROUP'}.
import_stuff -> 'OBJECT-IDENTITY' 
       : ensure_ver(2,'$1'), {builtin, 'OBJECT-IDENTITY'}.
import_stuff -> 'TEXTUAL-CONVENTION' 
       : ensure_ver(2,'$1'), {builtin, 'TEXTUAL-CONVENTION'}.
import_stuff -> 'DisplayString' 
       : ensure_ver(2,'$1'), {builtin, 'DisplayString'}.
import_stuff -> 'PhysAddress' 
       : ensure_ver(2,'$1'), {builtin, 'PhysAddress'}.
import_stuff -> 'MacAddress' 
       : ensure_ver(2,'$1'), {builtin, 'MacAddress'}.
import_stuff -> 'TruthValue' 
       : ensure_ver(2,'$1'), {builtin, 'TruthValue'}.
import_stuff -> 'TestAndIncr' 
       : ensure_ver(2,'$1'), {builtin, 'TestAndIncr'}.
import_stuff -> 'AutonomousType' 
       : ensure_ver(2,'$1'), {builtin, 'AutonomousType'}.
import_stuff -> 'InstancePointer' 
       : ensure_ver(2,'$1'), {builtin, 'InstancePointer'}.
import_stuff -> 'VariablePointer' 
       : ensure_ver(2,'$1'), {builtin, 'VariablePointer'}.
import_stuff -> 'RowPointer' 
       : ensure_ver(2,'$1'), {builtin, 'RowPointer'}.
import_stuff -> 'RowStatus' 
       : ensure_ver(2,'$1'), {builtin, 'RowStatus'}.
import_stuff -> 'TimeStamp' 
       : ensure_ver(2,'$1'), {builtin, 'TimeStamp'}.
import_stuff -> 'TimeInterval' 
       : ensure_ver(2,'$1'), {builtin, 'TimeInterval'}.
import_stuff -> 'DateAndTime' 
       : ensure_ver(2,'$1'), {builtin, 'DateAndTime'}.
import_stuff -> 'StorageType' 
       : ensure_ver(2,'$1'), {builtin, 'StorageType'}.
import_stuff -> 'TDomain' 
       : ensure_ver(2,'$1'), {builtin, 'TDomain'}.
import_stuff -> 'TAddress' 
       : ensure_ver(2,'$1'), {builtin, 'TAddress'}.
import_stuff -> 'BITS' 
       : ensure_ver(2,'$1'), {builtin, 'BITS'}.

traptype -> objectname 'TRAP-TYPE' 'ENTERPRISE' objectname varpart
	    description referpart implies integer :
            Trap = make_trap('$1', '$4', lreverse(traptype, '$5'), 
                             '$6', '$7', val('$9')),
            {Trap, line_of('$2')}.

% defines a name to an internal node.
objectidentifier -> objectname 'OBJECT' 'IDENTIFIER' nameassign : 
		    {Parent, SubIndex} = '$4',
                    Int = make_internal('$1', dummy, Parent, SubIndex),
		    {Int, line_of('$2')}.

% defines name, access and type for a variable.
objecttypev1 ->	objectname 'OBJECT-TYPE' 
		'SYNTAX' syntax
               	'ACCESS' accessv1
		'STATUS' statusv1
                'DESCRIPTION' descriptionfield
		referpart indexpartv1 defvalpart
		nameassign : 
                Kind = kind('$13', '$12'),
                OT = make_object_type('$1', '$4', '$6', '$8', '$10', 
                                      '$11', Kind, '$14'),
                {OT, line_of('$2')}.

newtype -> newtypename implies syntax :
           NT = make_new_type('$1', dummy, '$3'),
           {NT, line_of('$2')}.

tableentrydefinition -> newtypename implies 'SEQUENCE' '{' fields '}' : 
                        Seq = make_sequence('$1', lreverse(tableentrydefinition, '$5')),
                        {Seq, line_of('$3')}.

% returns: list of {<fieldname>, <asn1_type>}
fields -> fieldname fsyntax : 
	[{val('$1'), '$2'}].

fields -> fields ',' fieldname fsyntax :  [{val('$3'), '$4'} | '$1'].

fsyntax -> 'BITS' : {{bits,[{dummy,0}]},line_of('$1')}.
fsyntax -> syntax : '$1'.

fieldname -> atom : '$1'.

syntax -> usertype : {{type, val('$1')}, line_of('$1')}.
syntax -> type : {{type, cat('$1')},line_of('$1')}.
syntax -> type size : {{type_with_size, cat('$1'), '$2'},line_of('$1')}.
syntax -> usertype size : {{type_with_size,val('$1'), '$2'},line_of('$1')}.
syntax -> 'INTEGER' '{' namedbits '}' : 
          {{type_with_enum, 'INTEGER', '$3'}, line_of('$1')}.
syntax -> 'BITS' '{' namedbits '}' : 
          ensure_ver(2,'$1'), 
          {{bits, '$3'}, line_of('$1')}.
syntax -> usertype '{' namedbits '}' :
          {{type_with_enum, 'INTEGER', '$3'}, line_of('$1')}.
syntax -> 'SEQUENCE' 'OF' usertype : 
          {{sequence_of,val('$3')},line_of('$1')}.

size -> '(' sizedescr ')' : make_range('$2').
size -> '(' 'SIZE' '(' sizedescr  ')' ')' : make_range('$4').

%% Returns a list of integers describing a range.
sizedescr -> range_num '.' '.' range_num : ['$1', '$4'].
sizedescr -> range_num '.' '.' range_num sizedescr :['$1', '$4' |'$5'].
sizedescr -> range_num : ['$1'].
sizedescr -> sizedescr '|' sizedescr : ['$1', '$3'].

range_num -> integer : val('$1') .
range_num -> quote atom  : make_range_integer(val('$1'), val('$2')) . 
range_num -> quote variable  : make_range_integer(val('$1'), val('$2')) .

namedbits -> atom '(' integer ')' : [{val('$1'), val('$3')}].
namedbits -> namedbits ',' atom '(' integer ')' :
		 [{val('$3'), val('$5')} | '$1'].

usertype -> variable : '$1'.

type -> 'OCTET' 'STRING' : {'OCTET STRING', line_of('$1')}.
type -> 'BIT' 'STRING' : {'BIT STRING', line_of('$1')}.
type -> 'OBJECT' 'IDENTIFIER' : {'OBJECT IDENTIFIER', line_of('$1')}.
type -> 'INTEGER' : '$1'.
type -> 'NetworkAddress' : '$1'.
type -> 'IpAddress' : '$1'.
type -> 'Counter' : ensure_ver(1,'$1'),'$1'.
type -> 'Gauge' : ensure_ver(1,'$1'),'$1'.
type -> 'TimeTicks' : '$1'.
type -> 'Opaque' : '$1'.
type -> 'DisplayString' : ensure_ver(2,'$1'), '$1'.
type -> 'PhysAddress' : ensure_ver(2,'$1'), '$1'.
type -> 'MacAddress' : ensure_ver(2,'$1'), '$1'.
type -> 'TruthValue' : ensure_ver(2,'$1'), '$1'.
type -> 'TestAndIncr' : ensure_ver(2,'$1'), '$1'.
type -> 'AutonomousType' : ensure_ver(2,'$1'), '$1'.
type -> 'InstancePointer' : ensure_ver(2,'$1'), '$1'.
type -> 'VariablePointer' : ensure_ver(2,'$1'), '$1'.
type -> 'RowPointer' : ensure_ver(2,'$1'), '$1'.
type -> 'RowStatus' : ensure_ver(2,'$1'), '$1'.
type -> 'TimeStamp' : ensure_ver(2,'$1'), '$1'.
type -> 'TimeInterval' : ensure_ver(2,'$1'), '$1'.
type -> 'DateAndTime' : ensure_ver(2,'$1'), '$1'.
type -> 'StorageType' : ensure_ver(2,'$1'), '$1'.
type -> 'TDomain' : ensure_ver(2,'$1'), '$1'.
type -> 'TAddress' : ensure_ver(2,'$1'), '$1'.

% Returns: {FatherName, SubIndex}   (the parent)
nameassign -> implies '{' fatherobjectname parentintegers '}' : {'$3', '$4' }.
nameassign -> implies '{' parentintegers '}' : { root, '$3'}.


varpart -> '$empty' : [].
varpart -> 'VARIABLES' '{' variables '}' : '$3'.
variables -> objectname : ['$1'].
variables -> variables ',' objectname : ['$3' | '$1'].

implies -> '::=' : '$1'.
implies -> ':' ':' '=' : w("Sloppy asignment on line ~p", [line_of('$1')]), '$1'.
descriptionfield -> string : lreverse(descriptionfield, val('$1')).
descriptionfield -> '$empty' : undefined.
description -> 'DESCRIPTION' string : lreverse(description, val('$2')).
description -> '$empty' : undefined.

displaypart -> 'DISPLAY-HINT' string : display_hint('$2') .
displaypart -> '$empty' : undefined .

% returns: {indexes, undefined} 
%        | {indexes, IndexList} where IndexList is a list of aliasnames.
indexpartv1 -> 'INDEX' '{' indextypesv1 '}' : {indexes, lreverse(indexpartv1, '$3')}.
indexpartv1 -> '$empty' : {indexes, undefined}.

indextypesv1 -> indextypev1 : ['$1'].
indextypesv1 -> indextypesv1 ',' indextypev1 : ['$3' | '$1'].

indextypev1 ->  index : '$1'.

index -> objectname : '$1'.

parentintegers -> integer : [val('$1')].
parentintegers -> atom '(' integer ')' : [val('$3')].
parentintegers -> integer parentintegers : [val('$1') | '$2'].
parentintegers -> atom '(' integer ')' parentintegers : [val('$3') | '$5'].

defvalpart -> 'DEFVAL' '{' integer '}' : {defval, val('$3')}.
defvalpart -> 'DEFVAL' '{' atom '}' : {defval, val('$3')}.
defvalpart -> 'DEFVAL' '{' '{' defbitsvalue '}' '}' : {defval, '$4'}.
defvalpart -> 'DEFVAL' '{' quote atom '}' : 
	      {defval, make_defval_for_string(line_of('$1'), 
					      lreverse(defvalpart_quote_atom, val('$3')),
					      val('$4'))}.
defvalpart -> 'DEFVAL' '{' quote variable '}' : 
	      {defval, make_defval_for_string(line_of('$1'), 
					      lreverse(defvalpart_quote_variable, val('$3')),
					      val('$4'))}.
defvalpart -> 'DEFVAL' '{' string '}' : 
	      {defval, lreverse(defvalpart_string, val('$3'))}.
defvalpart -> '$empty' : undefined.

defbitsvalue -> defbitsnames : '$1'.
defbitsvalue -> '$empty' : [].

defbitsnames -> atom  : [val('$1')].
defbitsnames -> defbitsnames ',' atom  : [val('$3') | '$1'].

objectname -> atom : val('$1').
mibname -> variable : val('$1').
fatherobjectname -> objectname : '$1'.
newtypename -> variable : val('$1').

accessv1 -> atom: accessv1('$1').

statusv1 -> atom : statusv1('$1').

referpart -> 'REFERENCE' string : lreverse(referpart, val('$2')).
referpart -> '$empty' : undefined.


%%----------------------------------------------------------------------
%% SNMPv2 grammatics
%%v2
%%----------------------------------------------------------------------
moduleidentity -> mibid 'MODULE-IDENTITY' 
                  'LAST-UPDATED' last_updated
	          'ORGANIZATION' organization
                  'CONTACT-INFO' contact_info
	          'DESCRIPTION' descriptionfield 
                  revisionpart nameassign : 
                  MI = make_module_identity('$1', '$4', '$6', '$8', 
                                            '$10', '$11', '$12'), 
                  {MI, line_of('$2')}.

mibid -> atom : val('$1').
last_updated -> string : lreverse(last_updated, val('$1')) .
organization -> string : lreverse(organization, val('$1')) .
contact_info -> string : lreverse(contact_info, val('$1')) .

revisionpart -> '$empty' : [] .
revisionpart -> revisions : lreverse(revisionpart, '$1') .

revisions -> revision : ['$1'] .
revisions -> revisions revision : ['$2' | '$1'] .
revision -> 'REVISION' revision_string 'DESCRIPTION' revision_desc : 
            make_revision('$2', '$4') .

revision_string -> string : lreverse(revision_string, val('$1')) .
revision_desc   -> string : lreverse(revision_desc, val('$1')) .

definitionv2 -> objectidentifier : '$1'.
definitionv2 -> objecttypev2 : '$1'.
definitionv2 -> textualconvention : '$1'.
definitionv2 -> objectidentity : '$1'.
definitionv2 -> newtype : '$1'.
definitionv2 -> tableentrydefinition : '$1'.
definitionv2 -> notification : '$1'.
definitionv2 -> objectgroup : '$1'.
definitionv2 -> notificationgroup : '$1'.
definitionv2 -> modulecompliance : '$1'.
definitionv2 -> agentcapabilities : '$1'.

listofdefinitionsv2 -> '$empty' : [] .
listofdefinitionsv2 -> listofdefinitionsv2 definitionv2 : ['$2' | '$1'].

textualconvention -> newtypename implies 'TEXTUAL-CONVENTION' displaypart
                     'STATUS' statusv2 description referpart 'SYNTAX' syntax :
                     NT = make_new_type('$1', 'TEXTUAL-CONVENTION', '$4', 
                                        '$6', '$7', '$8', '$10'),
                     {NT, line_of('$3')}.

objectidentity -> objectname 'OBJECT-IDENTITY' 'STATUS' statusv2
                  'DESCRIPTION' string referpart nameassign : 
                  {Parent, SubIndex} = '$8',
                  Int = make_internal('$1', 'OBJECT-IDENTITY', 
                                      Parent, SubIndex),
                  {Int, line_of('$2')}.

objectgroup -> objectname 'OBJECT-GROUP' objectspart 
               'STATUS' statusv2 description referpart nameassign :
               OG = make_object_group('$1', '$3', '$5', '$6', '$7', '$8'),
	       {OG, line_of('$2')}.

notificationgroup -> objectname 'NOTIFICATION-GROUP' 'NOTIFICATIONS' '{'
                     objects '}' 'STATUS' statusv2 description referpart 
                     nameassign :
                     NG = make_notification_group('$1', '$5', '$8', '$9',
                                                  '$10', '$11'),
                     {NG, line_of('$2')}.

modulecompliance -> objectname 'MODULE-COMPLIANCE' 'STATUS' statusv2
                    description referpart mc_modulepart nameassign : 
%% 			io:format("modulecompliance -> "
%% 				  "~n   '$1': ~p"
%% 				  "~n   '$4': ~p"
%% 				  "~n   '$5': ~p"
%% 				  "~n   '$6': ~p"
%% 				  "~n   '$7': ~p"
%% 				  "~n   '$8': ~p"
%% 				  "~n", ['$1', '$4', '$5', '$6', '$7', '$8']),
                    MC = make_module_compliance('$1', '$4', '$5', '$6', 
                                                '$7', '$8'),
%% 			io:format("modulecompliance -> "
%% 				  "~n   MC: ~p"
%% 				  "~n", [MC]),
                    {MC, line_of('$2')}.


agentcapabilities -> objectname 'AGENT-CAPABILITIES' 
                     'PRODUCT-RELEASE' prodrel 
                     'STATUS' ac_status
                     description referpart ac_modulepart nameassign : 
                     AC = make_agent_capabilities('$1', '$4', '$6', '$7', 
                                                  '$8', '$9', '$10'),
                     {AC, line_of('$2')}.

prodrel -> string : lreverse(prodrel, val('$1')).

ac_status -> atom : ac_status('$1').

ac_modulepart -> ac_modules : 
                 lreverse(ac_modulepart, '$1').
ac_modulepart -> '$empty' : 
                 [].

ac_modules -> ac_module : 
              ['$1'].
ac_modules -> ac_module ac_modules : 
              ['$1' | '$2'].

ac_module -> 'SUPPORTS' ac_modulenamepart 'INCLUDES' '{' objects '}' ac_variationpart : 
             make_ac_module('$2', '$5', '$7').

ac_modulenamepart -> mibname : '$1'.
ac_modulenamepart -> '$empty' : undefined.
    
ac_variationpart -> '$empty' : 
                    [].
ac_variationpart -> ac_variations : 
                    lreverse(ac_variationpart, '$1').

ac_variations -> ac_variation : 
                 ['$1'].
ac_variations -> ac_variation ac_variations : 
                 ['$1' | '$2'].

%% ac_variation -> ac_objectvariation.
%% ac_variation -> ac_notificationvariation.

ac_variation -> 'VARIATION' objectname syntaxpart writesyntaxpart ac_accesspart ac_creationpart defvalpart description : 
                 make_ac_variation('$2', '$3', '$4', '$5', '$6', '$7', '$8').

ac_accesspart -> 'ACCESS' ac_access : '$2'.
ac_accesspart -> '$empty' : undefined. 

ac_access -> atom: ac_access('$1').     

ac_creationpart -> 'CREATION-REQUIRES' '{' objects '}' : 
                   lreverse(ac_creationpart, '$3').
ac_creationpart -> '$empty'                            : 
                   []. 

mc_modulepart -> '$empty'   : 
                 [].
mc_modulepart -> mc_modules : 
                 lreverse(mc_modulepart, '$1').

mc_modules -> mc_module : 
              ['$1'].
mc_modules -> mc_module mc_modules : 
              ['$1' | '$2'].
    
mc_module -> 'MODULE' mc_modulenamepart mc_mandatorypart mc_compliancepart : 
             make_mc_module('$2', '$3', '$4').

mc_modulenamepart -> mibname : '$1'.
mc_modulenamepart -> '$empty' : undefined.

mc_mandatorypart -> 'MANDATORY-GROUPS' '{' objects '}' : 
                    lreverse(mc_mandatorypart, '$3').
mc_mandatorypart -> '$empty' : 
                    [].
    
mc_compliancepart -> mc_compliances : 
                     lreverse(mc_compliancepart, '$1').
mc_compliancepart -> '$empty'       : 
                     [].

mc_compliances -> mc_compliance : 
                  ['$1'].
mc_compliances -> mc_compliance mc_compliances : 
                  ['$1' | '$2'].

mc_compliance -> mc_compliancegroup : 
                 '$1'.
mc_compliance -> mc_object          : 
                 '$1'.

mc_compliancegroup -> 'GROUP' objectname description : 
                      make_mc_compliance_group('$2', '$3').

mc_object -> 'OBJECT' objectname syntaxpart writesyntaxpart mc_accesspart description : 
             make_mc_object('$2', '$3', '$4', '$5', '$6').

syntaxpart -> 'SYNTAX' syntax : '$2'.
syntaxpart -> '$empty'        : undefined.

writesyntaxpart -> 'WRITE-SYNTAX' syntax : '$2'.
writesyntaxpart -> '$empty'              : undefined.
    
mc_accesspart -> 'MIN-ACCESS' accessv2 : '$2'.
mc_accesspart -> '$empty'              : undefined.
    
objecttypev2 ->	objectname 'OBJECT-TYPE' 
		'SYNTAX' syntax
                unitspart
               	'MAX-ACCESS' accessv2
		'STATUS' statusv2
                'DESCRIPTION' descriptionfield
                referpart indexpartv2 defvalpart
		nameassign : 
                Kind = kind('$14', '$13'), 
                OT = make_object_type('$1', '$4', '$5', '$7', '$9',
                                      '$11', '$12', Kind, '$15'),
                {OT, line_of('$2')}.

indexpartv2 -> 'INDEX' '{' indextypesv2 '}' : {indexes, lreverse(indexpartv2, '$3')}.
indexpartv2 -> 'AUGMENTS' '{' entry  '}' : {augments, '$3'}.
indexpartv2 -> '$empty' : {indexes, undefined}.

indextypesv2 -> indextypev2 : ['$1'].
indextypesv2 -> indextypesv2 ',' indextypev2 : ['$3' | '$1'].

indextypev2 ->  'IMPLIED' index : {implied,'$2'}.
indextypev2 ->  index : '$1'.

entry -> objectname : '$1'.

unitspart -> '$empty' : undefined.
unitspart -> 'UNITS' string : units('$2') .

statusv2 -> atom : statusv2('$1').

accessv2 -> atom: accessv2('$1').

notification -> objectname 'NOTIFICATION-TYPE' objectspart
                'STATUS' statusv2 'DESCRIPTION' descriptionfield referpart 
                nameassign :
                Not = make_notification('$1','$3','$5', '$7', '$8', '$9'),
                {Not, line_of('$2')}.

objectspart -> 'OBJECTS' '{' objects '}' : lreverse(objectspart, '$3').
objectspart -> '$empty' : [].

objects -> objectname : ['$1'].
objects -> objects ',' objectname : ['$3'|'$1'].

%%----------------------------------------------------------------------
Erlang code.
%%----------------------------------------------------------------------

-include("snmp_types.hrl").
-include("snmpc_lib.hrl").
-include("snmpc.hrl").

% value
val(Token) -> element(3, Token).

line_of(Token) -> element(2, Token).

%% category
cat(Token) -> element(1, Token). 

statusv1(Tok) ->
    case val(Tok) of
        mandatory -> mandatory;
        optional -> optional;
        obsolete -> obsolete;
        deprecated -> deprecated;
        Else -> return_error(line_of(Tok),
                             "(statusv1) syntax error before: " ++ atom_to_list(Else))
    end.

statusv2(Tok) ->
    case val(Tok) of
        current -> current;
        deprecated -> deprecated;
        obsolete -> obsolete;
        Else -> return_error(line_of(Tok),
                             "(statusv2) syntax error before: " ++ atom_to_list(Else))
    end.

ac_status(Tok) ->
    case val(Tok) of
        current -> current;
        obsolete -> obsolete;
        Else -> return_error(line_of(Tok),
                             "(ac_status) syntax error before: " ++ atom_to_list(Else))
    end.

accessv1(Tok) ->
    case val(Tok) of
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'write-only' -> 'write-only';
        'not-accessible' -> 'not-accessible';
        Else -> return_error(line_of(Tok),
                             "(accessv1) syntax error before: " ++ atom_to_list(Else))
    end.

accessv2(Tok) ->
    case val(Tok) of
        'not-accessible' -> 'not-accessible';
        'accessible-for-notify' -> 'accessible-for-notify';
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'read-create' -> 'read-create';
        Else -> return_error(line_of(Tok),
                             "(accessv2) syntax error before: " ++ atom_to_list(Else))
    end.

ac_access(Tok) ->
    case val(Tok) of
        'not-implemented' -> 'not-implemented'; % only for notifications
        'accessible-for-notify' -> 'accessible-for-notify';
        'read-only' -> 'read-only';
        'read-write' -> 'read-write';
        'read-create' -> 'read-create';
        'write-only' -> 'write-only'; % for backward-compatibility only
        Else -> return_error(line_of(Tok),
                             "(ac_access) syntax error before: " ++ atom_to_list(Else))
    end.

%% ---------------------------------------------------------------------
%% Various basic record build functions
%% ---------------------------------------------------------------------

make_module_identity(Name, LU, Org, CI, Desc, Revs, NA) ->
    #mc_module_identity{name         = Name,
                        last_updated = LU,
	                organization = Org,
	                contact_info = CI,
	                description  = Desc,
	                revisions    = Revs, 
	                name_assign  = NA}.

make_revision(Rev, Desc) ->
    #mc_revision{revision    = Rev,
	         description = Desc}.

make_object_type(Name, Syntax, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax,
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_object_type(Name, Syntax, Units, MaxAcc, Status, Desc, Ref, Kind, NA) ->
    #mc_object_type{name        = Name,
                    syntax      = Syntax, 
                    units       = Units, 
	            max_access  = MaxAcc,
	            status      = Status,
	            description = Desc,
	            reference   = Ref,
	            kind        = Kind, 
	            name_assign = NA}.

make_new_type(Name, Macro, Syntax) ->
    #mc_new_type{name   = Name, 
	         macro  = Macro,
                 syntax = Syntax}.

make_new_type(Name, Macro, DisplayHint, Status, Desc, Ref, Syntax) ->
    #mc_new_type{name         = Name, 
	         macro        = Macro,
                 status       = Status,
                 description  = Desc,
                 reference    = Ref,
	         display_hint = DisplayHint,
                 syntax       = Syntax}.

make_trap(Name, Ent, Vars, Desc, Ref, Num) ->
    #mc_trap{name        = Name,
             enterprise  = Ent,
             vars        = Vars,
             description = Desc,
	     reference   = Ref,
	     num         = Num}.

make_notification(Name, Vars, Status, Desc, Ref, NA) ->
    #mc_notification{name        = Name,
                     vars        = Vars,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_agent_capabilities(Name, ProdRel, Status, Desc, Ref, Mods, NA) ->
    #mc_agent_capabilities{name            = Name,
                           product_release = ProdRel,
                           status          = Status,
                           description     = Desc,
	                   reference       = Ref,
                           modules         = Mods,
	                   name_assign     = NA}.

make_ac_variation(Name, 
		  undefined = _Syntax, 
		  undefined = _WriteSyntax, 
		  Access, 
		  undefined = _Creation, 
		  undefined = _DefVal, 
		  Desc) ->
%%     io:format("make_ac_variation -> entry with"
%% 	      "~n   Name:        ~p"
%% 	      "~n   Access:      ~p"
%% 	      "~n   Desc:        ~p"
%% 	      "~n", [Name, Access, Desc]),
    #mc_ac_notification_variation{name        = Name, 
 				  access      = Access,
 				  description = Desc};

make_ac_variation(Name, Syntax, WriteSyntax, Access, Creation, DefVal, Desc) ->
%%     io:format("make_ac_variation -> entry with"
%% 	      "~n   Name:        ~p"
%% 	      "~n   Syntax:      ~p"
%% 	      "~n   WriteSyntax: ~p"
%% 	      "~n   Access:      ~p"
%% 	      "~n   Creation:    ~p"
%% 	      "~n   DefVal:      ~p"
%% 	      "~n   Desc:        ~p"
%% 	      "~n", [Name, Syntax, WriteSyntax, Access, Creation, DefVal, Desc]),
    #mc_ac_object_variation{name          = Name, 
			    syntax        = Syntax, 
			    write_syntax  = WriteSyntax, 
			    access        = Access,
			    creation      = Creation,
			    default_value = DefVal,
			    description   = Desc}.

make_ac_module(Name, Grps, Var) ->
    #mc_ac_module{name      = Name, 
		  groups    = Grps,
		  variation = Var}.


make_module_compliance(Name, Status, Desc, Ref, Mods, NA) ->
    #mc_module_compliance{name        = Name,
                          status      = Status,
                          description = Desc,
	                  reference   = Ref,
                          modules     = Mods,
	                  name_assign = NA}.

make_mc_module(Name, Mand, Compl) ->
    #mc_mc_module{name       = Name, 
		  mandatory  = Mand,
		  compliance = Compl}.

make_mc_compliance_group(Name, Desc) ->
    #mc_mc_compliance_group{name        = Name,
			    description = Desc}.

make_mc_object(Name, Syntax, WriteSyntax, Access, Desc) ->
    #mc_mc_object{name         = Name,
		  syntax       = Syntax,
		  write_syntax = WriteSyntax,
		  access       = Access, 
		  description  = Desc}.

make_object_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_object_group{name        = Name,
                     objects     = Objs,
                     status      = Status,
                     description = Desc,
	             reference   = Ref,
	             name_assign = NA}.

make_notification_group(Name, Objs, Status, Desc, Ref, NA) ->
    #mc_notification_group{name        = Name,
                           objects     = Objs,
                           status      = Status,
                           description = Desc,
	                   reference   = Ref,
	                   name_assign = NA}.

make_sequence(Name, Fields) ->
    #mc_sequence{name   = Name, 
                 fields = Fields}.

make_internal(Name, Macro, Parent, SubIdx) ->
    #mc_internal{name      = Name, 
                 macro     = Macro, 
                 parent    = Parent, 
                 sub_index = SubIdx}.



%% ---------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Purpose: Find how much room needs to be allocated for the data type
%%          (when sending it in a PDU (the maximum difference will be 
%%           the size allocated)).
%%          This is applicable for OCTET STRINGs and OBJECT IDENTIFIERs.
%%
%%     Or : Find the range of integers in the integer list.
%%          This is applicable for INTEGERs
%%
%% Arg: A list of integers.
%%----------------------------------------------------------------------

make_range_integer(RevHexStr, h) ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevHexStr, 'H') ->
    erlang:list_to_integer(lists:reverse(RevHexStr), 16);
make_range_integer(RevBitStr, b) ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevBitStr, 'B') ->
    erlang:list_to_integer(lists:reverse(RevBitStr), 2);
make_range_integer(RevStr, Base) ->
    throw({error, {invalid_base, Base, lists:reverse(RevStr)}}).

make_range(XIntList) ->
    IntList = lists:flatten(XIntList),
    {range, lists:min(IntList), lists:max(IntList)}.

make_defval_for_string(Line, Str, Atom) ->
    case lists:member(Atom, [h, 'H', b, 'B']) of
	true ->
	    case catch make_defval_for_string2(Str, Atom) of
		Defval when is_list(Defval) ->
		    Defval;
		{error, ErrStr} ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p - ~s",
						 [Atom, Str, ErrStr],
						 Line),
		    "";
		_Else ->
		    snmpc_lib:print_error("Bad DEFVAL ~w string ~p",
						 [Atom, Str],
						 Line),
		    ""
	    end;
	false ->
	    snmpc_lib:print_error("Bad DEFVAL string type ~w for ~p",
					 [Atom, Str],
					 Line),
	    ""
    end.
	    

make_defval_for_string2([], h) -> [];
make_defval_for_string2([X16,X|HexString], h) ->
    lists:append(hex_to_bytes(snmpc_misc:to_upper([X16,X])),
		 make_defval_for_string2(HexString, h));
make_defval_for_string2([_Odd], h) ->
    throw({error, "odd number of bytes in hex string"});
make_defval_for_string2(HexString, 'H') ->
    make_defval_for_string2(HexString,h);

make_defval_for_string2(BitString, 'B') ->
    bits_to_bytes(BitString);
make_defval_for_string2(BitString, b) ->
    make_defval_for_string2(BitString, 'B').

bits_to_bytes(BitStr) ->
    lists:reverse(bits_to_bytes(lists:reverse(BitStr), 1, 0)).

bits_to_bytes([], 1, _Byte) ->   % empty bitstring
    [];
bits_to_bytes([], 256, _Byte) -> % correct; multiple of 8
    [];
% If we are to support arbitrary length of bitstrings.  This migth
% be needed in the new SMI.
%bits_to_bytes([], N, Byte) ->
%    [Byte];
bits_to_bytes([], _N, _Byte) ->
    throw({error, "not a multiple of eight bits in bitstring"});
bits_to_bytes(Rest, 256, Byte) ->
    [Byte | bits_to_bytes(Rest, 1, 0)];
bits_to_bytes([$1 | T], N, Byte) ->
    bits_to_bytes(T, N*2, N + Byte);
bits_to_bytes([$0 | T], N, Byte) ->
    bits_to_bytes(T, N*2, Byte);
bits_to_bytes([_BadChar | _T], _N, _Byte) ->
    throw({error, "bad character in bit string"}).

%%----------------------------------------------------------------------
%% These HEX conversion routines are stolen from module asn1_bits by 
%% klacke@erix.ericsson.se
%% I didn't want to ship the entire asn1-compiler so I used cut-and-paste.
%%----------------------------------------------------------------------

%% hex_to_bytes(HexNumber) when is_atom(HexNumber) ->
%%     hex_to_bytes(atom_to_list(HexNumber));

hex_to_bytes(HexNumber) ->
    case length(HexNumber) rem 2 of
	1 ->  %% Odd
	    hex_to_bytes(lists:append(HexNumber,[$0]),[]);
	0 ->  %% even
	    hex_to_bytes(HexNumber,[])
    end.

hex_to_bytes([],R) ->
    lists:reverse(R);
hex_to_bytes([Hi,Lo|Rest],Res) ->
    hex_to_bytes(Rest,[hex_to_byte(Hi,Lo)|Res]).

hex_to_four_bits(Hex) ->
    if
	Hex == $0 -> 0;
	Hex == $1 -> 1;
	Hex == $2 -> 2;
	Hex == $3 -> 3;
	Hex == $4 -> 4;
	Hex == $5 -> 5;
	Hex == $6 -> 6;
	Hex == $7 -> 7;
	Hex == $8 -> 8;
	Hex == $9 -> 9;
	Hex == $A -> 10;
	Hex == $B -> 11;
	Hex == $C -> 12;
	Hex == $D -> 13;
	Hex == $E -> 14;
	Hex == $F -> 15;
	true -> throw({error, "bad hex character"})
    end.

hex_to_byte(Hi,Lo) ->
    (hex_to_four_bits(Hi) bsl 4) bor hex_to_four_bits(Lo).

kind(DefValPart,IndexPart) ->
    case DefValPart of
	undefined ->
	    case IndexPart of
		{indexes, undefined} -> {variable, []};
		{indexes, Indexes}  ->
		    {table_entry, {indexes, Indexes}};
		{augments,Table} ->
		    {table_entry,{augments,Table}}
	    end;
	{defval, DefVal} -> {variable, [{defval, DefVal}]}
    end.    

display_hint(Val) ->
    case val(Val) of
        Str when is_list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_display_hint, Val}})
    end.

units(Val) ->
    case val(Val) of
        Str when is_list(Str) ->
            lists:reverse(Str);
        _ ->
            throw({error, {invalid_units, Val}})
    end.

ensure_ver(Ver, Line, What) ->
    case get(snmp_version) of
	Ver -> ok;
	_Other ->
	    snmpc_lib:print_error(
	      "~s is only allowed in SNMPv~p.",[What,Ver],Line)
    end.


ensure_ver(Ver,Token) ->
    ensure_ver(Ver,line_of(Token), atom_to_list(cat(Token))).

filter_v2imports(2,'Integer32')  -> {builtin, 'Integer32'};
filter_v2imports(2,'Counter32')  -> {builtin, 'Counter32'};
filter_v2imports(2,'Gauge32')    -> {builtin, 'Gauge32'};
filter_v2imports(2,'Unsigned32') -> {builtin, 'Unsigned32'};
filter_v2imports(2,'Counter64')  -> {builtin, 'Counter64'};
filter_v2imports(_,Type)         -> {type, Type}.
    
w(F, A) ->
    ?vwarning(F, A).

lreverse(_Tag, L) when is_list(L) ->
    lists:reverse(L);
lreverse(Tag, X) ->
    exit({bad_list, Tag, X}).


%% i(F, A) ->
%%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).

