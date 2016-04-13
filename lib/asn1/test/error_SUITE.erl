%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013-2016. All Rights Reserved.
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

-module(error_SUITE).
-export([suite/0,all/0,groups/0,
	 already_defined/1,bitstrings/1,
	 classes/1,constraints/1,constructed/1,enumerated/1,
	 imports_exports/1,instance_of/1,integers/1,objects/1,
	 object_field_extraction/1,oids/1,rel_oids/1,
	 object_sets/1,parameterization/1,
	 syntax/1,table_constraints/1,tags/1,values/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks, [ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,parallel(),
      [already_defined,
       bitstrings,
       classes,
       constraints,
       constructed,
       enumerated,
       imports_exports,
       instance_of,
       integers,
       objects,
       object_field_extraction,
       object_sets,
       oids,
       rel_oids,
       parameterization,
       syntax,
       table_constraints,
       tags,
       values]}].

parallel() ->
    case erlang:system_info(schedulers) > 1 of
        true  -> [parallel];
        false -> []
    end.

already_defined(Config) ->
    M = 'Already',
    P = {M,
	 <<"Already DEFINITIONS ::= BEGIN\n"
	   "  I ::= INTEGER\n"
	   "  i I ::= 42\n"
	   "  I ::= OCTET STRING\n"
	   "  I ::= CLASS { &Type }\n"
	   "  MYCLASS ::= CLASS { &Type }\n"
	   "  i MYCLASS ::= { &Type INTEGER }\n"
	   "  o MYCLASS ::= { &Type INTEGER }\n"
	   "  I MYCLASS ::= { o }\n"
	   "  I{T} ::= SEQUENCE OF T\n"
	   "  I{INTEGER:x} INTEGER ::= { 1 | 2 | x }\n"
	   "  i{T} MYCLASS ::= { &Type T }\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,4},asn1ct_check,{already_defined,'I',2}},
      {structured_error,{M,5},asn1ct_check,{already_defined,'I',2}},
      {structured_error,{M,7},asn1ct_check,{already_defined,'i',3}},
      {structured_error,{M,9},asn1ct_check,{already_defined,'I',2}},
      {structured_error,{M,10},asn1ct_check,{already_defined,'I',2}},
      {structured_error,{M,11},asn1ct_check,{already_defined,'I',2}},
      {structured_error,{M,12},asn1ct_check,{already_defined,'i',3}}
     ]
    } = run(P, Config),
    ok.

bitstrings(Config) ->
    M = 'Bitstrings',
    P = {M,
	 <<"Bitstrings DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  Bs1 ::= BIT STRING {a(1), a(1)}\n"
	   "  Bs2 ::= BIT STRING {a(1), b(2), a(3)}\n"
	   "  Bs3 ::= BIT STRING {x(1), y(1)}\n"
	   "  Bs4 ::= BIT STRING {x(-1), y(0)}\n"
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,3},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,4},asn1ct_check,{value_reused,1}},
      {structured_error,{M,5},asn1ct_check,{invalid_bit_number,-1}}
     ]} = run(P, Config),
    ok.

classes(Config) ->
    M = 'Classes',
    P = {M,
	 <<"Classes DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  LowerCase ::= CLASS { &id INTEGER UNIQUE }\n"
	   "  CL ::= CLASS { &id INTEGER UNIQUE DEFAULT 42}\n"
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,
       {illegal_class_name,'LowerCase'}},
      {structured_error,{M,3},asn1ct_check,
       {unique_and_default,id}}
     ]} = run(P, Config),
    ok.

constraints(Config) ->
    M = 'Constraints',
    P = {M,
	 <<"Constraints DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  II-1 ::= INTEGER (holder-1.&obj)\n"
	   "  II-2 ::= INTEGER ('1234'H<..20)\n"
	   "  II-3 ::= INTEGER (1..<\"abc\")\n"
	   "  II-4 ::= INTEGER (10..1)\n"

	   "  HOLDER ::= CLASS {\n"
	   "    &obj HOLDER OPTIONAL\n"
	   "  }\n"

	   "  holder-1 HOLDER ::= { &obj holder-2 }\n"
	   "  holder-2 HOLDER ::= { }\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,illegal_value},
      {structured_error,{M,3},asn1ct_check,illegal_integer_value},
      {structured_error,{M,4},asn1ct_check,illegal_integer_value},
      {structured_error,{M,5},asn1ct_check,reversed_range}
     ]} = run(P, Config),
    ok.

enumerated(Config) ->
    M = 'Enumerated',
    P = {M,
	 <<"Enumerated DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  Enum ::= ENUMERATED { a, b, c }\n"
	   "  e Enum ::= d\n"
	   "  EnumExt ::= ENUMERATED { x, ..., y }\n"
	   "  ext EnumExt ::= z\n"
	   "  S1 ::= SEQUENCE {\n"
           "    ge1 Enum DEFAULT a,\n"
           "    ge2 EnumExt DEFAULT x,\n"
           "    ge3 EnumExt DEFAULT y,\n"
	   "    e Enum DEFAULT aa\n"
           "  }\n"
	   "  S2 ::= SEQUENCE {\n"
	   "    e2 EnumExt DEFAULT xyz\n"
	   "  }\n"

	   "  BadEnum1 ::= ENUMERATED {a, b, c, b }\n"
	   "  BadEnum2 ::= ENUMERATED {a(1), b(2), b(3) }\n"
	   "  BadEnum3 ::= ENUMERATED {a(1), b(1) }\n"
	   "  BadEnum4 ::= ENUMERATED {a, b, ..., c(0) }\n"
	   "  BadEnum5 ::= ENUMERATED {a, b, ..., c(10), d(5) }\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,3},asn1ct_check,{undefined,d}},
      {structured_error,{M,5},asn1ct_check,{undefined,z}},
      {structured_error,{M,6},asn1ct_check,{undefined,aa}},
      {structured_error,{M,12},asn1ct_check,{undefined,xyz}},
      {structured_error,{M,15},asn1ct_check,
       {enum_illegal_redefinition,b}},
      {structured_error,{M,16},asn1ct_check,
       {enum_illegal_redefinition,b}},
      {structured_error,{M,17},asn1ct_check,
       {enum_reused_value,b,1}},
      {structured_error,{M,18},asn1ct_check,
       {enum_reused_value,c,0}},
      {structured_error,{M,19},asn1ct_check,
       {enum_not_ascending,d,5,10}}
     ]
    } = run(P, Config),
    ok.

imports_exports(Config) ->
    Ext = 'ExternalModule',
    ExtP = {Ext,
	    <<"ExternalModule DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	      "IMPORTS\n"
	      " Int, NonExistingImport FROM ImportsFrom;\n"

	      " Existing ::= INTEGER\n"
	      "END\n">>},
    {error,
     [{structured_error,
       {Ext,3},
       asn1ct_check,
       {undefined_import,'NonExistingImport',
	'ImportsFrom'}}]} = run(ExtP, Config),

    M = 'Imports',
    P = {M,
	 <<"Imports DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "EXPORTS\n"
	   " T, UndefinedType;\n"

	   "IMPORTS\n"
	   " NotDefined, Existing, Int, NonExistingImport\n"
	   "   FROM ExternalModule\n"
	   " X FROM UndefinedModule objid\n"
	   " Y, Z FROM UndefinedModule2;\n"

	   "objid OBJECT IDENTIFIER ::= {joint-iso-ccitt(2) remote-operations(4)\n"
	   "    notation(0)}\n"
	   "T ::= INTEGER\n"
	   "END\n">>},
    {error,[{structured_error,{M,3},asn1ct_check,
	     {undefined_export, 'UndefinedType'}},
	    {structured_error,{M,5},asn1ct_check,
	     {undefined_import,'NonExistingImport',Ext}},
	    {structured_error,{M,5},asn1ct_check,
	     {undefined_import,'NotDefined',Ext}},
	    {structured_error,{M,7},asn1ct_check,
	     {undefined_import,'X','UndefinedModule'}},
	    {structured_error,{M,8},asn1ct_check,
	     {undefined_import,'Y','UndefinedModule2'}},
	    {structured_error,{M,8},asn1ct_check,
	     {undefined_import,'Z','UndefinedModule2'}}
	   ]} = run(P, Config),
    ok.

instance_of(Config) ->
    M = 'InstanceOf',
    P = {M,
	 <<"InstanceOf DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "XX ::= INSTANCE OF CL ({TI})\n"
	   "CL ::= CLASS {\n"
           "&id INTEGER,\n"
           "&Type\n"
	   "}\n"
	   "o1 CL ::= {&id 1, &Type OCTET STRING}\n"
	   "TI CL ::= { o1 }\n"
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{illegal_instance_of,'CL'}}
     ]} = run(P, Config),
    ok.

integers(Config) ->
    M = 'Integers',
    P = {M,
	 <<"Integers DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  Int1 ::= INTEGER {a(1), a(1)}\n"
	   "  Int2 ::= INTEGER {a(1), b(2), a(3)}\n"
	   "  Int3 ::= INTEGER {x(1), y(1)}\n"
	   "  i0 INTEGER ::= 1\n"
	   "  Int4 ::= INTEGER {x(i0), y(undef) }\n"
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,3},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,4},asn1ct_check,{value_reused,1}},
      {structured_error,{M,6},asn1ct_check,{undefined,undef}}
     ]} = run(P, Config),
    ok.


objects(Config) ->
    M = 'Objects',
    P = {M,
	 <<"Objects DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  obj1 CL ::= { &wrong 42 }\n"
	   "  obj2 CL ::= { &wrong 1, &Wrong INTEGER }\n"
	   "  obj3 CL ::= { &Data OCTET STRING }\n"
	   "  obj4 SMALL ::= { &code 42 }\n"
	   "  InvalidSet CL ::= { obj1 }\n"
	   "  obj5 CL ::= {}\n"
	   "  ErrSet ::= PT{ {PT{inst}}}\n"
	   "  obj6 CL ::= 7\n"
	   "  obj7 CL ::= int\n"
	   "  obj8 NON-CLASS ::= { &id 1 }\n"

	   "  CL ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
	   "    &enum ENUMERATED { a, b, c},\n"
	   "    &Data,\n"
	   "    &object CL,\n"
	   "    &Set CL,\n"
	   "    &vartypevalue &Data,\n"
	   "    &VarTypeValue &Data\n"
	   "  }\n"

	   "  SMALL ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
           "    &i INTEGER\n"
           "  }\n"

	   "  PT{SMALL:Small} ::= SEQUENCE { a SMALL.&code ({Small}) }\n"
	   "  inst SMALL ::= {&code 42, &i 4711}\n"

	   "  int INTEGER ::= 42\n"
	   "  NON-CLASS ::= SEQUENCE { a BOOLEAN }\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,
       {invalid_fields,[wrong],obj1}},
      {structured_error,{M,3},asn1ct_check,
       {invalid_fields,['Wrong',wrong],obj2}},
      {structured_error,{M,4},asn1ct_check,
       {missing_mandatory_fields,['Set','VarTypeValue',code,
				  enum,object,vartypevalue],obj3}},
      {structured_error,{M,5},asn1ct_check,
       {missing_mandatory_fields,[i],obj4}},
      {structured_error,{M,6},asn1ct_check,
       {invalid_fields,[wrong],'InvalidSet'}},
      {structured_error,{M,7},asn1ct_check,
       {missing_mandatory_fields,
	['Data','Set','VarTypeValue',code,enum,object,
	 vartypevalue],obj5}},
      {structured_error,{M,8},asn1ct_check,invalid_objectset},
      {structured_error,{M,9},asn1ct_check,illegal_object},
      {structured_error,{M,10},asn1ct_check,illegal_object},
      {structured_error,{M,11},asn1ct_check,illegal_object}
     ]
    } = run(P, Config),
    ok.

object_field_extraction(Config) ->
    M = 'ObjectFieldExtraction',
    P = {M,
	 <<"ObjectFieldExtraction DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"

	   "  DataObjSet DATA-CLASS ::= {\n"
	   "    holder-object-1.&int,\n"
	   "    ...\n"
	   "  }\n"

	   "  DataObjSetNoExt DATA-CLASS ::= {\n"
	   "    holder-object-1.&int\n"
	   "  }\n"

	   "  holder-object-1 HOLDER-CLASS ::= {\n"
	   "    &int 42\n"
	   "  }\n"

	   "  HOLDER-CLASS ::= CLASS {\n"
           "    &int INTEGER\n"
           "  }\n"

	   "  DATA-CLASS ::= CLASS {\n"
           "    &id INTEGER\n"
           "  }\n"

	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,illegal_object},
      {structured_error,{M,6},asn1ct_check,illegal_object}
     ]
    } = run(P, Config),
    ok.

object_sets(Config) ->
    M = 'ObjectSets',
    P = {M, <<"ObjectSets DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	      "TEST-UNIQ ::= CLASS { &id INTEGER UNIQUE,  &test INTEGER }\n"
	      "UniqSet TEST-UNIQ ::= { { &id 1, &test 1 } | {&id 1, &test 2} }\n"

	      "DOUBLE-UNIQ ::= CLASS { &id1 INTEGER UNIQUE,"
              "           &id INTEGER UNIQUE }\n"
              "DoubleSet DOUBLE-UNIQ ::= { {&id1 1, &id2 2} }\n"
	      "END\n">>},
    {error,
     [{structured_error,{M,3},asn1ct_check,{non_unique_object,1}},
      {structured_error,{M,5},asn1ct_check,multiple_uniqs}
     ]
    } = run(P, Config),
    ok.

oids(Config) ->
    M = 'OIDS',
    P = {M,<<"OIDS DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	    "CONTAINER ::= CLASS { &id OBJECT IDENTIFIER UNIQUE,\n"
	    "                      &int INTEGER OPTIONAL,\n"
	    "                      &seq SEQUENCE { a INTEGER } OPTIONAL\n"
	    "}\n"

	    "-- This is line 6.\n"
	    "object-1 CONTAINER ::= { &id {1 2 3}, &int 42 }\n"
	    "object-2 CONTAINER ::= { &id {1 999}, &int 0 }\n"
	    "object-3 CONTAINER ::= { &id {1 2}, &seq { a 42 } }\n"
	    "oid-1 OBJECT IDENTIFIER ::= object-1.&int\n"
	    "oid-2 OBJECT IDENTIFIER ::= object-2.&id\n"
	    "oid-3 OBJECT IDENTIFIER ::= object-3.&seq\n"
	    "-- This is line 13.\n"

	    "oid-5 OBJECT IDENTIFIER ::= { a 42, b 19 }\n"

	    "oid-6 OBJECT IDENTIFIER ::= int\n"
	    "int INTEGER ::= 42\n"

	    "oid-7 OBJECT IDENTIFIER ::= seq\n"
	    "seq SEQUENCE { x INTEGER } ::= { x 11 }\n"

	    "oid-8 OBJECT IDENTIFIER ::= os\n"
	    "os OCTET STRING ::= '1234'H\n"

	    "oid-9 OBJECT IDENTIFIER ::= { 1 os }\n"

	    "oid-10 OBJECT IDENTIFIER ::= { 1 invalid }\n"

	    "-- This is line 23.\n"
	    "oid-11 OBJECT IDENTIFIER ::= { 0 legal-oid }\n"
	    "legal-oid OBJECT IDENTIFIER ::= {1 2 3}\n"

	    "bad-root-1 OBJECT IDENTIFIER ::= {99}\n"
	    "bad-root-2 OBJECT IDENTIFIER ::= {0 42}\n"

	    "oid-object-ref-1 OBJECT IDENTIFIER ::= object-1\n"
	    "oid-object-ref-2 OBJECT IDENTIFIER ::= { object-1 19 } \n"

	    "oid-int OBJECT IDENTIFIER ::= 42\n"
	    "oid-sequence OBJECT IDENTIFIER ::= {a 42, b 35}\n"

	     "END\n">>},
    {error,
     [
      {structured_error,{M,8},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,10},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,11},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,12},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,14},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,15},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,17},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,19},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,21},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,22},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,24},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,26},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,27},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,28},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,29},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,30},asn1ct_check,{illegal_oid,o_id}},
      {structured_error,{M,31},asn1ct_check,{illegal_oid,o_id}}
     ]
    } = run(P, Config),
    ok.

rel_oids(Config) ->
    M = 'REL-OIDS',
    P = {M,<<"REL-OIDS DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	     "legal-oid OBJECT IDENTIFIER ::= {1 2}\n"
	     "legal-roid RELATIVE-OID ::= {1 2}\n"
	     "CONTAINER ::= CLASS { &oid OBJECT IDENTIFIER OPTIONAL,\n"
	     "                      &int INTEGER OPTIONAL,\n"
	     "                      &seq SEQUENCE { a INTEGER } OPTIONAL\n"
	     "}\n"
	     "object-1 CONTAINER ::= { &oid {1 2 3},\n"
	     "                         &int 42,\n",
	     "                         &seq {a 42}\n"
	     "                       }\n"

	     "wrong-type-rel-oid-1 RELATIVE-OID ::= legal-oid\n"
	     "wrong-type-rel-oid-2 RELATIVE-OID ::= object-1.&oid\n"
	     "wrong-type-rel-oid-3 RELATIVE-OID ::= object-1.&int\n"
	     "wrong-type-rel-oid-4 RELATIVE-OID ::= object-1.&seq\n"
	     "wrong-type-rel-oid-5 RELATIVE-OID ::= object-1.&undef\n"

	     "oid-bad-first OBJECT IDENTIFIER ::= {legal-roid 3}\n"
	     "END\n">>},
    {error,
     [
      {structured_error,{M,12},asn1ct_check,{illegal_oid,rel_oid}},
      {structured_error,{M,13},asn1ct_check,{illegal_oid,rel_oid}},
      {structured_error,{M,14},asn1ct_check,{illegal_oid,rel_oid}},
      {structured_error,{M,15},asn1ct_check,{illegal_oid,rel_oid}},
      {structured_error,{M,16},asn1ct_check,{undefined_field,undef}},
      {structured_error,{M,17},asn1ct_check,{illegal_oid,o_id}}
     ]
    } = run(P, Config),
    ok.


parameterization(Config) ->
    M = 'Parameterization',
    P = {M,
	 <<"Parameterization DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  NotUppercase{lowercase} ::= INTEGER (lowercase)\n"

	   "  P{T1,T2} ::= SEQUENCE { a T1, b T2 }\n"
	   "  S ::= P{OCTET STRING}\n"

	   "  Seq ::= SEQUENCE { a INTEGER }\n"
	   "  Sbad ::= Seq{INTEGER}\n"

	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,
       {illegal_typereference,lowercase}},
      {structured_error,{M,4},asn1ct_check,
       param_wrong_number_of_arguments},
      {structured_error,{M,6},asn1ct_check,
       {param_bad_type, 'Seq'}}
     ]
    } = run(P, Config),
    ok.


constructed(Config) ->
    M = 'Const',
    P = {M,
	 <<"Const DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  Seq1 ::= SEQUENCE {a INTEGER, b BIT STRING, a BOOLEAN}\n"
	   "  Ch   ::= CHOICE {a INTEGER, b BIT STRING, a BOOLEAN}\n"
	   "  Seq2 ::= SEQUENCE {COMPONENTS OF Ch}\n"
	   "  CL   ::= CLASS { &id INTEGER UNIQUE, &Type }\n"
	   "  Seq3 ::= SEQUENCE { id CL.&id, d CL.&foo }\n"

	   "  Seq4 ::= SEQUENCE { a INTEGER, z INTEGER OPTIONAL, b Set1 }\n"
	   "  Set1 ::= SET { c BOOLEAN, d INTEGER }\n"
	   "  s1 Seq4 ::= {a 42, b {c TRUE, zz 4711}}\n"
	   "  s2 Seq4 ::= {a 42, b {c TRUE, d FALSE}}\n"
	   "  s3 Seq4 ::= {a 42, b {c TRUE}}\n"
	   "  s4 Seq4 ::= {a 42, b {c TRUE, d 4711}, zz 4712}\n"
	   "  s5 Seq4 ::= {a 42}\n"
	   "  s6 Seq4 ::= {a 42, zz 4712, b {c TRUE, d 4711}}\n"
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{duplicate_identifier,a}},
      {structured_error,{M,3},asn1ct_check,{duplicate_identifier,a}},
      {structured_error,{M,4},asn1ct_check,{illegal_COMPONENTS_OF,'Ch'}},
      {structured_error,{M,6},asn1ct_check,{illegal_object_field,foo}},

      {structured_error,{M,9},asn1ct_check,{illegal_id,zz}},
      {structured_error,{M,10},asn1ct_check,illegal_integer_value},
      {structured_error,{M,11},asn1ct_check,{missing_id,d}},
      {structured_error,{M,12},asn1ct_check,{illegal_id,zz}},
      {structured_error,{M,13},asn1ct_check,{missing_id,b}},
      {structured_error,{M,14},asn1ct_check,{illegal_id,zz}}
     ]
    } = run(P, Config),
    ok.

syntax(Config) ->
    M = 'Syntax',
    P = {M,
	 <<"Syntax DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  obj1  CL ::= { WRONG }\n"
	   "  obj2  CL ::= { CODE 42 AGAIN WRONG }\n"
	   "  obj3  CL ::= { INTEGER }\n"
	   "  obj4  CL ::= { BIT STRING }\n"
	   "  obj5  CL ::= { , }\n"
	   "  obj6  CL ::= { CODE , }\n"
	   "  obj7  CL ::= { CODE \"abc\" }\n"
	   "  obj8  CL ::= { CODE }\n"
	   "  obj9  CL ::= { CODE 42 ENUM}\n"
	   "  obj10 CL ::= { CODE 42 ENUM BIT STRING}\n"

	   "  obj11 CL ::= { CODE 42 TYPE 13}\n"
	   "  obj12 CL ::= { CODE 42 TYPE d}\n"
	   "  obj13 CL ::= { CODE 42 TYPE bs-value}\n"

	   "  bad-syntax-1 BAD-SYNTAX-1 ::= { BAD 42 }\n"

	   "  obj14 CL ::= { CODE 42 OBJ-SET integer }\n"
	   "  obj15 CL ::= { CODE 42 OBJ-SET { A B } }\n"
	   "  obj16 CL ::= { CODE 42 OBJ-SET SEQUENCE { an INTEGER } }\n"

	   "  obj17 CL ::= { CODE 42 OID {seqtag 42} }\n"
	   "  obj18 CL ::= { CODE 42 OID {seqtag 42, seqtag-again 43} }\n"
	   "  obj19 CL ::= { CODE 42 OID {one 1 two 2} }\n"

	   "  BAD-SYNTAX-1 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE\n"
	   "  } WITH SYNTAX {\n"
	   "    BAD &bad\n"
	   "  }\n"

	   "  BAD-SYNTAX-2 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE\n"
	   "  } WITH SYNTAX {\n"
	   "    BAD &Bad\n"
	   "  }\n"

	   "  BAD-SYNTAX-3 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE\n"
	   "  } WITH SYNTAX {\n"
	   "    [ID &code]\n"
	   "  }\n"

	   "  BAD-SYNTAX-4 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE\n"
	   "  } WITH SYNTAX {\n"
	   "    ID\n"
	   "  }\n"

	   "  BAD-SYNTAX-5 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
	   "    &Type\n"
	   "  } WITH SYNTAX {\n"
	   "    ID\n"
	   "  }\n"

	   "  BAD-SYNTAX-6 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE\n"
	   "  } WITH SYNTAX {\n"
	   "    ID &code, &code\n"
	   "  }\n"

	   "  BAD-SYNTAX-7 ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
	   "    &Type\n"
	   "  } WITH SYNTAX {\n"
	   "    ID &Type, &code, &code, &Type\n"
	   "  }\n"

	   "  CL ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
	   "    &enum ENUMERATED { a, b, c} OPTIONAL,\n"
	   "    &Type OPTIONAL,\n"
	   "    &ObjSet CL OPTIONAL,\n"
	   "    &oid OBJECT IDENTIFIER OPTIONAL\n"
	   "  } WITH SYNTAX {\n"
	   "    CODE &code [ENUM &enum] [TYPE &Type] [OBJ-SET &ObjSet]\n"
           "    [OID &oid]\n"
	   "  }\n"

	   "  bs-value BIT STRING ::= '1011'B\n"

	   "  integer INTEGER ::= 42\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,
       {syntax_nomatch,"WRONG"}},
      {structured_error,{M,3},asn1ct_check,
       {syntax_nomatch,"AGAIN"}},
      {structured_error,{M,4},asn1ct_check,
       {syntax_nomatch,"INTEGER"}},
      {structured_error,{M,5},asn1ct_check,
       {syntax_nomatch,"BIT STRING"}},
      {structured_error,{M,6},asn1ct_check,
       {syntax_nomatch,"\",\""}},
      {structured_error,{M,7},asn1ct_check,
       {syntax_nomatch,"\",\""}},
      {structured_error,{M,8},asn1ct_check,
       {syntax_nomatch,"\"abc\""}},
      {structured_error,{M,9},asn1ct_check,
       syntax_nomatch},
      {structured_error,{M,10},asn1ct_check,
       syntax_nomatch},
      {structured_error,{M,11},asn1ct_check,
       {syntax_nomatch,"BIT STRING"}},
      {structured_error,{M,12},asn1ct_check,
       {syntax_nomatch,"13"}},
      {structured_error,{M,13},asn1ct_check,
       {syntax_nomatch,"d"}},
      {structured_error,{M,14},asn1ct_check,
       {syntax_nomatch,"bs-value"}},
      {structured_error,{M,15},asn1ct_check,
       {syntax_undefined_field,bad}},
      {structured_error,{M,16},asn1ct_check,
       {syntax_nomatch,"integer"}},
      {structured_error,{M,17},asn1ct_check,
       {syntax_nomatch,"\"A B\""}},
      {structured_error,{M,18},asn1ct_check,
       {syntax_nomatch,"SEQUENCE"}},
      {structured_error,{M,19},asn1ct_check,
       {syntax_nomatch,"\"seqtag 42\""}},
      {structured_error,{M,20},asn1ct_check,
       {syntax_nomatch,"\"seqtag 42 seqtag-again 43\""}},
      {structured_error,{M,21},asn1ct_check,
       {syntax_nomatch,"\"one 1 two 2\""}},
      {structured_error,{M,22},asn1ct_check,
       {syntax_undefined_field,bad}},
      {structured_error,{M,27},asn1ct_check,
       {syntax_undefined_field,'Bad'}},
      {structured_error,{M,32},asn1ct_check,
       {syntax_mandatory_in_optional_group,code}},
      {structured_error,{M,37},asn1ct_check,
       {syntax_missing_mandatory_fields,[code]}},
      {structured_error,{M,42},asn1ct_check,
       {syntax_missing_mandatory_fields,['Type',code]}},
      {structured_error,{M,48},asn1ct_check,
       {syntax_duplicated_fields,[code]}},
      {structured_error,{M,53},asn1ct_check,
       {syntax_duplicated_fields,['Type',code]}}
     ]
    } = run(P, Config),
    ok.

table_constraints(Config) ->
    M = 'TableConstraints',
    P = {M,
	 <<"TableConstraints DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  Seq-1 ::= SEQUENCE {\n"
	   "    contentType CONTENTS.&id,\n"
	   "    content CONTENTS.&Type({Contents}{@contentType})\n"
	   "  }\n"

	   "  Seq-2 ::= SEQUENCE {\n"
	   "    contentType INTEGER,\n"
	   "    content CONTENTS.&Type({Contents}{@contentType})\n"
	   "  }\n"

	   "  Int ::= INTEGER ({1})\n"

	   "  Seq-3 ::= SEQUENCE {\n"
	   "    contentType CONTENTS.&id({1})\n"
	   "  }\n"

	   "Contents CONTENTS ::= {\n"
	   "  {OCTET STRING IDENTIFIED BY {2 1 1}}\n"
	   "}\n"

	   "CONTENTS ::= TYPE-IDENTIFIER\n"
	   "END\n">>},
    {error,
     [{structured_error,
       {M,2},asn1ct_check,
       {missing_table_constraint,contentType}},
      {structured_error,
       {M,6},asn1ct_check,
       {missing_ocft,contentType}},
      {structured_error,
       {M,10},asn1ct_check,
       illegal_table_constraint},
      {structured_error,
       {M,11},asn1ct_check,
       invalid_table_constraint}
     ]} = run(P, Config),
    ok.

tags(Config) ->
    M = 'Tags',
    P = {M,
	 <<"Tags DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "SeqOpt1 ::= SEQUENCE\n"
	   "{\n"
	   "bool1  BOOLEAN OPTIONAL,\n"
	   "int1  INTEGER,\n"
	   "seq1  SeqIn OPTIONAL\n"
	   "}\n"

	   "SeqOpt1Imp ::= SEQUENCE \n"
	   "{\n"
	   "bool1 [1] BOOLEAN OPTIONAL,\n"
	   "int1  INTEGER,\n"
	   "seq1  [2] SeqIn OPTIONAL,\n"
	   "seq2  [2] SeqIn OPTIONAL,\n"
	   "...,\n"
	   "int2  [3] SeqIn,\n"
	   "int3  [3] SeqIn\n"
	   "}\n"

	   "SeqIn ::= SEQUENCE \n"
	   "{\n"
	   "boolIn  BOOLEAN,\n"
	   "intIn  INTEGER\n"
	   "}\n"
	   "\n"


	   "Set1 ::= SET {\n"
	   " os [0] OCTET STRING,\n"
	   " bool [0] BOOLEAN\n"
	   "}\n"

	   "Seq1 ::= SEQUENCE {\n"
	   "a [0] IMPLICIT Choice OPTIONAL\n"
	   "}\n"
	   "Seq2 ::= SEQUENCE {\n"
	   "a [0] IMPLICIT ANY OPTIONAL\n"
	   "}\n"
	   "Choice ::=\n"
	   "CHOICE {\n"
	   "a [0] BOOLEAN,\n"
	   "b [1] INTEGER\n"
	   "}\n"

	   "END\n">>},
    {error,
     [{structured_error,
       {M,8},asn1ct_check,
       {duplicate_tags,[seq1,seq2]}},
      {structured_error,
       {M,24},asn1ct_check,
       {duplicate_tags,[bool,os]}},
      {structured_error,
       {M,28},asn1ct_check,
       {implicit_tag_before,choice}},
      {structured_error,
       {M,31},asn1ct_check,
       {implicit_tag_before,open_type}}
     ]} = run(P, Config),
    ok.


values(Config) ->
    M = 'Values',
    P = {M,
	 <<"Values DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  os1 OCTET STRING ::= \"abc\"\n"
	   "  os2 OCTET STRING ::= 42\n"
	   "  os3 OCTET STRING ::= { 1, 3 }\n"
	   "  os4 OCTET STRING ::= '1234'H\n"
	   "  Seq ::= SEQUENCE {\n"
	   "    an OCTET STRING\n"
	   "  }\n"
	   "  seq Seq ::= { an int }\n"
	   "  os5 OCTET STRING ::= holder-1.&str\n"
	   "  os6 OCTET STRING ::= int\n"

	   "  int1 INTEGER ::= \"string\"\n"
	   "  int2 INTEGER ::= os4\n"
	   "  int3 INTEGER ::= not-defined\n"
	   "  int4 INTEGER ::= holder-1.&str\n"
	   "  int5 INTEGER ::= holder-2.&obj\n"
	   "  int6 INTEGER ::= holder-2.&undefined-field\n"
	   "  int7 INTEGER ::= holder-2.&UndefinedField.&id\n"

	   "  bs1 BIT STRING ::= 42\n"
	   "  bs2 BIT STRING ::= {a,b}\n"
	   "  bs3 BIT STRING {a(0),z(25)} ::= {a,b}\n"
	   "  bs4 BIT STRING {a(0),z(25)} ::= int\n"
	   "  bs5 BIT STRING ::= holder-2.&str\n"
	   "  bs6 BIT STRING ::= holder-2.&obj\n"

	   "  b1 BOOLEAN ::= 42\n"
	   "  b2 BOOLEAN ::= {a,b}\n"

	   "  HOLDER ::= CLASS {\n"
	   "    &str IA5String,\n"
	   "    &obj HOLDER OPTIONAL\n"
	   "  }\n"

	   "  holder-1 HOLDER ::= { &str \"xyz\" }\n"
	   "  holder-2 HOLDER ::= { &str \"xyz\", &obj holder-1 }\n"

	   "  ext-1 EXTERNAL ::= {identification bad:{1 2 3}, data-value '123'H}\n"
	   "  ext-2 EXTERNAL ::= {identification syntax:{1 2 3}, data '123'H}\n"

	   "  CH ::= CHOICE { a INTEGER, b BOOLEAN }\n"
	   "  ch1 CH ::= 2344\n"
	   "  ch2 CH ::= zz:34\n"

	   "  st1 an < Seq ::= 42\n"
	   "  st2 zz < CH ::= 42\n"
	   "  st3 a < HOLDER ::= 42\n"
	   "  st4 a < INTEGER ::= 42\n"

	   "  int INTEGER ::= 42\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,3},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,4},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,9},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,10},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,11},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,12},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,13},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,14},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,15},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,16},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,17},asn1ct_check,
       {undefined_field,'undefined-field'}},
      {structured_error,{M,18},asn1ct_check,
       {undefined_field,'UndefinedField'}},
      {structured_error,{M,19},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,20},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,21},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,22},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,23},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,24},asn1ct_check,
       {illegal_value, "BIT STRING"}},
      {structured_error,{M,25},asn1ct_check,
       {illegal_value, "BOOLEAN"}},
      {structured_error,{M,26},asn1ct_check,
       {illegal_value, "BOOLEAN"}},
      {structured_error,{M,33},asn1ct_check,
       illegal_external_value},
      {structured_error,{M,34},asn1ct_check,
       illegal_external_value},
      {structured_error,{M,36},asn1ct_check,
       {illegal_id, 2344}},
      {structured_error,{M,37},asn1ct_check,
       {illegal_id, zz}},
      {structured_error,{M,38},asn1ct_check,
       {illegal_choice_type, 'Seq'}},
      {structured_error,{M,39},asn1ct_check,
       {illegal_id, zz}},
      {structured_error,{M,40},asn1ct_check,
       {illegal_choice_type, 'HOLDER'}},
      {structured_error,{M,41},asn1ct_check,
       {illegal_choice_type, 'INTEGER'}}
     ]
    } = run(P, Config),
    ok.


run({Mod,Spec}, Config) ->
    Base = atom_to_list(Mod) ++ ".asn1",
    File = filename:join(proplists:get_value(priv_dir, Config), Base),
    Include0 = filename:dirname(proplists:get_value(data_dir, Config)),
    Include = filename:join(filename:dirname(Include0), "asn1_SUITE_data"),
    ok = file:write_file(File, Spec),
    asn1ct:compile(File, [{i, Include}]).
