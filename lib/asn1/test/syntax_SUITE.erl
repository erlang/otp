%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2016. All Rights Reserved.
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

-module(syntax_SUITE).
-export([suite/0,all/0,groups/0,
	 assignment/1,
	 class/1,
	 constraints/1,
	 exports/1,
	 header/1,
	 imports/1,
	 objects/1,
	 sequence/1,
	 syntax/1,
	 tokenizer/1,
	 types/1,
	 values/1]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks, [ts_install_cth]}].

all() ->
    [{group,p}].

groups() ->
    [{p,parallel(),
      [assignment,
       class,
       constraints,
       exports,
       header,
       imports,
       objects,
       sequence,
       syntax,
       tokenizer,
       types,
       values]}].

parallel() ->
    case erlang:system_info(schedulers) > 1 of
        true  -> [parallel];
        false -> []
    end.

assignment(Config) ->
    Head = "Assignment DEFINITIONS AUTOMATIC TAGS ::=\nBEGIN\n",
    End = "\nEND\n",
    L0 = [{"42",3,{syntax_error,42}},
	  {"i",4,{syntax_error,'END'}},
	  {"i ::=",3,{syntax_error,'::='}},
	  {"i type",4,{syntax_error,'END'}},
	  {"i type ::=",3,{syntax_error,'::='}},
	  {"i TYPE",4,{syntax_error,'END'}},
	  {"i TYPE ::= ",4,{syntax_error,'END'}},
	  {"i INTEGER ::= 42 garbage",4,{syntax_error,'END'}},
	  {"i{T} Type",4,{syntax_error,'END'}},
	  {"TYPE",4,{syntax_error,'END'}},
	  {"TYPE ::=",4,{syntax_error,'END'}},
	  {"TYPE{ ::=",3,{syntax_error,'::='}},
	  {"TYPE{P, ::=",3,{syntax_error,'::='}},
	  {"TYPE{P,} ::=",3,{syntax_error,'}'}},
	  {"TYPE{Gov:} ::=",3,{syntax_error,':'}},
	  {"TYPE{A} CL ",4,{syntax_error,'END'}},
	  {"ObjSet CL",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Assignment", Config),
    ok.

class(Config) ->
    Head = "Class DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	" CL ::= CLASS {",
    End = "\nEND\n",
    L0 = [{"id",3,{syntax_error,'id'}},
	  {"&id INTEGER",4,{syntax_error,'END'}},
	  {"&id INTEGER,",4,{syntax_error,'END'}},
	  {"&id,",3,{syntax_error,','}},
	  {"&id OPTIONAL",3,{syntax_error,'OPTIONAL'}},
	  {"&id INTEGER OPTIONAL",4,{syntax_error,'END'}},
	  {"&var &Field",4,{syntax_error,'END'}},
	  {"&Type,",4,{syntax_error,'END'}},
	  {"&Type OPTIONAL",4,{syntax_error,'END'}},
	  {"&ValueSet INTEGER OPTIONAL",4,{syntax_error,'END'}},
	  {"&ValueSet INTEGER DEFAULT",4,{syntax_error,'END'}},
	  {"&ValueSet INTEGER DEFAULT {",4,{syntax_error,'END'}},
	  {"&ValueSet INTEGER DEFAULT {a",4,{syntax_error,'END'}},
	  {"&Var &Field",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Class", Config),
    ok.

constraints(Config) ->
    Head = "Constraints DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	" Type ::= ",
    End = "\nEND\n",
    L0 = [{"INTEGER (",4,{syntax_error,'END'}},
	  {"INTEGER (10x",3,{syntax_error,x}},
	  {"INTEGER (10|(10y",3,{syntax_error,y}},
	  {"INTEGER (CONSTRAINED BY {}",4,{syntax_error,'END'}},
	  {"INTEGER (CONSTRAINED BY {INTEGER garbage",3,
	   {syntax_error,garbage}},
	  {"INTEGER ({ObjSet",4,{syntax_error,'END'}},
	  {"INTEGER ({ObjSet}{",3,{syntax_error,'{'}},
	  {"INTEGER ({ObjSet}{@",3,{syntax_error,'{'}},
	  {"INTEGER ({ObjSet}{@x",3,{syntax_error,'{'}},
	  {"INTEGER ({ObjSet}{@x}",4,{syntax_error,'END'}},
	  {"INTEGER (10 !BOOLEAN",4,{syntax_error,'END'}},
	  {"INTEGER (10 !BOOLEAN:",4,{syntax_error,'END'}},
	  {"INTEGER (10 !BOOLEAN:FALSE",4,{syntax_error,'END'}},
	  {"SEQUENCE {} (WITH COMPONENTS { Type })",
	   3,{syntax_error,'Type'}},
	  {"SEQUENCE {} (WITH COMPONENTS { x (10)",
	   4,{syntax_error,'END'}},
	  {"SEQUENCE {} (WITH COMPONENTS { ..., x (10)",
	   4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Constraints", Config),
    ok.

exports(Config) ->
    Head = "Exports DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	" EXPORTS ",
    End = "\nEND\n",
    L0 = [{"Type",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Exports", Config),
    ok.

header(Config) ->
    L = [{"lowercase",1,{syntax_error,lowercase}},
	 {"H ",2,{syntax_error,'END-OF-FILE'}},
	 {"H-",1,{syntax_error,'-'}},
	 {"42",1,{syntax_error,42}},
	 {"H definitions",1,{syntax_error,definitions}},
	 {"H DEFINITIONS STUPID TAGS",1,{syntax_error,'STUPID'}},
	 {"H DEFINITIONS WHATEVER",1,{syntax_error,'WHATEVER'}},
	 {"H DEFINITIONS ::= BEGIN",2,{syntax_error,'END-OF-FILE'}},
	 {"BOOLEAN",1,{syntax_error,'BOOLEAN'}}
	],
    run(L, "H", Config),
    ok.

imports(Config) ->
    Head = "Imports DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	" IMPORTS ",
    End = "\nEND\n",
    L0 = [{"Type FROM X",4,{syntax_error,'END'}},
	  {"Symbols TO Y",3,{syntax_error,'TO'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Imports", Config),
    ok.

objects(Config) ->
    Head = "Objects DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	"  object CLASS-NAME ::= ",
    End = "\nEND\n",
    L0 = [{"{",4,{syntax_error,'END'}},
	  {"{&min 1, max 10}",3,{syntax_error,max}},
	  {"{&min 1, Max 10}",3,{syntax_error,'Max'}},
	  {"{min 1, &max 10}",3,{syntax_error,'&max'}},
	  {"{min 1, &Max 10}",3,{syntax_error,'&Max'}},
	  {"{RESERVERD WORD BIT}",3,{syntax_error,'BIT'}},
	  {"{&min 1",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Objects", Config),
    ok.

sequence(Config) ->
    Head = "Sequence DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	"  Type ::= SEQUENCE {",
    End = "\nEND\n",
    L0 = [{"",4,{syntax_error,'END'}},
	  {" UpperCase",3,{syntax_error,'UpperCase'}},
	  {" a b",4,{syntax_error,'END'}},
	  {" i INTEGER",4,{syntax_error,'END'}},
	  {" ...",4,{syntax_error,'END'}},
	  {" ..., [[",4,{syntax_error,'END'}},
	  {" ..., [[ a INTEGER ]",3,{syntax_error,']'}},
	  {" ..., [[ a INTEGER,",3,{syntax_error,','}},
	  {" ..., [[ a INTEGER, ... ]]",3,{syntax_error,','}},
	  {" ... !42 xxx",3,{syntax_error,'xxx'}},
	  {" ... !42, a INTEGER,",3,{syntax_error,','}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Sequence", Config),
    ok.

syntax(Config) ->
    Head = "Syntax DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	" CL ::= CLASS { &id INTEGER UNIQUE } WITH SYNTAX ",
    End = "\nEND\n",
    L0 = [{"{}",3,{syntax_error,'}'}},
	  {"WORD",3,{syntax_error,'WORD'}},
	  {"{ Word }",3,{syntax_error,'Word'}},
	  {"{ [ Word ] }",3,{syntax_error,'Word'}},
	  {"{ [ WORD }",3,{syntax_error,'}'}},
	  {"{ WORD;",3,{syntax_error,';'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Syntax", Config),
    ok.

tokenizer(Config) ->
    Head = "Tokenize DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n",
    End = "\nEND\n",
    L0 = [{"'",3,eol_in_token},
	  {"'42'B",3,{invalid_binary_number,"42"}},
	  {"'ZZZ'H",3,{invalid_hex_number,"ZZZ"}},
	  {"\"abc",3,missing_quote_at_eof},
	  {"/*",3,eof_in_comment}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Tokenizer", Config, asn1ct_tok),
    ok.

types(Config) ->
    Head = "Types DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	"  Type ::= ",
    End = "\nEND\n",
    L0 = [{"BIT STRING garbage",4,{syntax_error,'END'}},
	  {"BIT STRING {",4,{syntax_error,'END'}},
	  {"BIT STRING { a(42",3,{syntax_error,42}},
	  {"BIT STRING { a(0)",4,{syntax_error,'END'}},
	  {"CHOICE {",4,{syntax_error,'END'}},
	  {"CHOICE { ..., a}",3,{syntax_error,'...'}},
	  {"CHOICE { UpperCase",3,{syntax_error,'UpperCase'}},
	  {"CHOICE { i INTEGER",4,{syntax_error,'END'}},
	  {"CHOICE { ..., i INTEGER }",3,{syntax_error,'...'}},
	  {"CHOICE { b BOOLEAN, ..., i INTEGER",
	   4,{syntax_error,'END'}},
	  {"CHOICE { b BOOLEAN, ..., [[ e BOOLEAN, ...]]}",
	   3,{syntax_error,','}},
	  {"CHOICE { b BOOLEAN, ..., i INTEGER, ..., x BIT STRING}",
	   3,{syntax_error,','}},
	  {"ENUMERATED {",4,{syntax_error,'END'}},
	  {"ENUMERATED { 42 }",3,{syntax_error,42}},
	  {"ENUMERATED { a, b",4,{syntax_error,'END'}},
	  {"ENUMERATED { a, }",3,{syntax_error,','}},
	  {"ENUMERATED { a, ...,\nb, ..., c }",4,{syntax_error,','}},
	  {"INTEGER {",4,{syntax_error,'END'}},
	  {"INTEGER { a(42)",4,{syntax_error,'END'}},
	  {"SEQUENCE",3,{syntax_error,'SEQUENCE'}},
	  %% More tests for SEQUENCE in sequence/1.
	  {"SEQUENCE SIZE (1..10)",4,{syntax_error,'END'}},
	  {"SEQUENCE (SIZE (1..10))",4,{syntax_error,'END'}},
	  {"SET { i INTEGER",4,{syntax_error,'END'}},
	  {"SET { ...",4,{syntax_error,'END'}},
	  {"SET SIZE (1..10)",4,{syntax_error,'END'}},
	  {"SET (SIZE (1..10))",4,{syntax_error,'END'}},
	  {"SET { ... !42 xxx",3,{syntax_error,'xxx'}},
	  {"SET { ... !42, a INTEGER,",3,{syntax_error,','}},
	  {"[",4,{syntax_error,'END'}},
	  {"[42",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Types", Config),
    ok.

values(Config) ->
    Head = "Values DEFINITIONS AUTOMATIC TAGS ::=\n"
	"BEGIN\n"
	"  value Type ::= ",
    End = "\nEND\n",
    L0 = [{"",4,{syntax_error,'END'}}
	 ],
    L = [{Head++S++End,Line,E} || {S,Line,E} <- L0],
    run(L, "Values", Config),
    ok.

run(List, File, Config) ->
    run(List, File, Config, asn1ct_parser2).

run(List, File0, Config, Module) ->
    Base = File0 ++ ".asn1",
    File = filename:join(proplists:get_value(priv_dir, Config), Base),
    case run_1(List, Base, File, Module, 0) of
	0 -> ok;
	Errors -> ?t:fail(Errors)
    end.

run_1([{Source,Line,Error}=Exp|T], Base, File, Module, N) ->
    ok = file:write_file(File, Source),
    io:format("~s", [Source]),
    case asn1ct:compile(File) of
	{error,[{structured_error,{Base,L},Module,E}]} ->
	    case {L,E} of
		{Line,Error} ->
		    run_1(T, Base, File, Module, N);
		{Line,OtherError} ->
		    io:format("*** Wrong error: ~p, expected ~p ***\n",
			      [OtherError,Error]),
		    run_1(T, Base, File, Module, N+1);
		{OtherLine,Error} ->
		    io:format("*** Wrong line: ~p, expected ~p ***\n",
			      [OtherLine,Line]),
		    run_1(T, Base, File, Module, N+1);
		{_,_} ->
		    io:format("*** Wrong line: ~p, expected ~p ***",
			      [L,Line]),
		    io:format("*** Wrong error: ~p, expected ~p ***\n",
			      [E,Error]),
		    run_1(T, Base, File, Module, N+1)
	    end;
	Other ->
	    io:format("~p\nGOT: ~p", [Exp,Other])
    end;
run_1([], _, _, _, N) ->
    N.
