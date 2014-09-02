%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(error_SUITE).
-export([suite/0,all/0,groups/0,
	 already_defined/1,bitstrings/1,
	 classes/1,constraints/1,enumerated/1,
	 imports/1,instance_of/1,integers/1,objects/1,
	 object_field_extraction/1,parameterization/1,
	 syntax/1,values/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks, [ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,parallel(),
      [already_defined,
       bitstrings,
       classes,
       constraints,
       enumerated,
       imports,
       instance_of,
       integers,
       objects,
       object_field_extraction,
       parameterization,
       syntax,
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
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{illegal_class_name,
					    'LowerCase'}}
     ]} = run(P, Config),
    ok.

constraints(Config) ->
    M = 'Constraints',
    P = {M,
	 <<"Constraints DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  II-1 ::= INTEGER (holder-1.&obj)\n"
	   "  II-2 ::= INTEGER ('1234'H<..20)\n"
	   "  II-3 ::= INTEGER (1..<\"abc\")\n"

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
      {structured_error,{M,4},asn1ct_check,illegal_integer_value}
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
	   "END\n">>},
    {error,
     [
      {structured_error,{'Enumerated',3},asn1ct_check,{undefined,d}},
      {structured_error,{'Enumerated',5},asn1ct_check,{undefined,z}},
      {structured_error,{'Enumerated',10},asn1ct_check,{undefined,aa}},
      {structured_error,{'Enumerated',13},asn1ct_check,{undefined,xyz}}
     ]
    } = run(P, Config),
    ok.

imports(Config) ->
    Ext = 'ExternalModule',
    ExtP = {Ext,
	    <<"ExternalModule DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	      "END\n">>},
    ok = run(ExtP, Config),

    M = 'Imports',
    P = {M,
	 <<"Imports DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "IMPORTS NotDefined FROM ExternalModule\n"
	   "X FROM UndefinedModule objid\n"
	   "Y, Z FROM UndefinedModule2;\n"
	   "objid OBJECT IDENTIFIER ::= {joint-iso-ccitt(2) remote-operations(4)\n"
	   "    notation(0)}\n"
	   "END\n">>},
    {error,[{structured_error,{M,2},asn1ct_check,
	     {undefined_import,'NotDefined','ExternalModule'}},
	    {structured_error,{M,3},asn1ct_check,{undefined_import,'X','UndefinedModule'}},
	    {structured_error,{M,4},asn1ct_check,{undefined_import,'Y','UndefinedModule2'}},
	    {structured_error,{M,4},asn1ct_check,{undefined_import,'Z','UndefinedModule2'}}
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
	   "END\n">>},
    {error,
     [{structured_error,{M,2},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,3},asn1ct_check,{namelist_redefinition,a}},
      {structured_error,{M,4},asn1ct_check,{value_reused,1}}
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
	 vartypevalue],obj5}}
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
      {structured_error,{M,2},asn1ct_check,illegal_object}
     ]
    } = run(P, Config),
    ok.

parameterization(Config) ->
    M = 'Parameterization',
    P = {M,
	 <<"Parameterization DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  NotUppercase{lowercase} ::= INTEGER (lowercase)\n"
	   "END\n">>},
    {error,
     [{structured_error,{'Parameterization',2},asn1ct_check,
       {illegal_typereference,lowercase}}
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

	   "  CL ::= CLASS {\n"
	   "    &code INTEGER UNIQUE,\n"
	   "    &enum ENUMERATED { a, b, c} OPTIONAL,\n"
	   "    &Type OPTIONAL,\n"
	   "    &ObjSet CL OPTIONAL\n"
	   "  } WITH SYNTAX {\n"
	   "    CODE &code [ENUM &enum] [TYPE &Type] [OBJ-SET &ObjSet]\n"
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
       {syntax_undefined_field,bad}},
      {structured_error,{M,24},asn1ct_check,
       {syntax_undefined_field,'Bad'}}
    ]
    } = run(P, Config),
    ok.

values(Config) ->
    M = 'Values',
    P = {M,
	 <<"Values DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  os1 OCTET STRING ::= \"abc\"\n"
	   "  os2 OCTET STRING ::= 42\n"
	   "  os3 OCTET STRING ::= { 1, 3 }\n"
	   "  os4 OCTET STRING ::= '1234'H\n"

	   "  int1 INTEGER ::= \"string\"\n"
	   "  int2 INTEGER ::= os4\n"
	   "  int3 INTEGER ::= not-defined\n"
	   "  int4 INTEGER ::= holder-1.&str\n"
	   "  int5 INTEGER ::= holder-2.&obj\n"

	   "  HOLDER ::= CLASS {\n"
	   "    &str IA5String,\n"
	   "    &obj HOLDER OPTIONAL\n"
	   "  }\n"

	   "  holder-1 HOLDER ::= { &str \"xyz\" }\n"
	   "  holder-2 HOLDER ::= { &str \"xyz\", &obj holder-1 }\n"
	   "END\n">>},
    {error,
     [
      {structured_error,{M,2},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,3},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,4},asn1ct_check,
       illegal_octet_string_value},
      {structured_error,{M,6},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,7},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,8},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,9},asn1ct_check,
       illegal_integer_value},
      {structured_error,{M,10},asn1ct_check,
       illegal_integer_value}
     ]
    } = run(P, Config),
    ok.


run({Mod,Spec}, Config) ->
    Base = atom_to_list(Mod) ++ ".asn1",
    File = filename:join(?config(priv_dir, Config), Base),
    ok = file:write_file(File, Spec),
    asn1ct:compile(File).
