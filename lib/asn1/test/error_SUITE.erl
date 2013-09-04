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
	 already_defined/1,enumerated/1,objects/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks, [ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,parallel(),[already_defined,
		    enumerated,
		    objects]}].

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

objects(Config) ->
    M = 'Objects',
    P = {M,
	 <<"Objects DEFINITIONS AUTOMATIC TAGS ::= BEGIN\n"
	   "  obj1 CL ::= { &wrong 42 }\n"
	   "  obj2 CL ::= { &wrong 1, &Wrong INTEGER }\n"
	   "  obj3 CL ::= { &Data OCTET STRING }\n"
	   "  obj4 SMALL ::= { &code 42 }\n"
	   "  InvalidSet CL ::= { obj1 }\n"

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
       {invalid_fields,[wrong],'InvalidSet'}}
     ]
    } = run(P, Config),
    ok.



run({Mod,Spec}, Config) ->
    Base = atom_to_list(Mod) ++ ".asn1",
    File = filename:join(?config(priv_dir, Config), Base),
    ok = file:write_file(File, Spec),
    asn1ct:compile(File).
