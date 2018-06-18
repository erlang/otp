%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2014-2018. All Rights Reserved.
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
%%

-module(testUniqueObjectSets).
-export([main/3]).

%% Run-time function called by the generated code.
seq_roundtrip(I, D0) ->
    M = 'UniqueObjectSets',
    try
	{ok,Enc} = M:encode('Seq', {'Seq',I,D0}),
        asn1_test_lib:map_roundtrip(M, 'Seq', Enc),
	{ok,{'Seq',I,D}} = M:decode('Seq', Enc),
	D
    catch C:E:Stk ->
	    io:format("FAILED: ~p ~p\n", [I,D0]),
	    erlang:raise(C, E, Stk)
    end.

types() ->
    [{"CHOICE { a INTEGER, b BIT STRING }", {b,<<42:3>>}},
     {"INTEGER",42},
     {"SEQUENCE {a OCTET STRING}",{'_',<<"abc">>}},
     {"SEQUENCE {b BOOLEAN, ...}",{'_',true}},
     {"SEQUENCE {b BOOLEAN, ..., s IA5String, ..., e ENUMERATED { x, y, z}}",
      {'_',false,"string",y}},
     {"SET {a BIT STRING}",{'_',<<1:17>>}},
     {"SEQUENCE OF INTEGER",[-19,0,555,777]},
     {"SET OF BOOLEAN",[true,false,true]},
     {"SEQUENCE OF SEQUENCE {x INTEGER (0..7)}",[{'_',7},{'_',0}]},
     {"SET OF SEQUENCE {x INTEGER (0..7)}",[{'_',7},{'_',0}]}
    ].

main(CaseDir, Rule, Opts) ->
    D0 = types(),
    {D1,_} = lists:mapfoldl(fun({T,S}, I) ->
				    {{I,T,S},I+1}
			    end, 1, D0),
    Types = [gen_types(I, Type) || {I,Type,_} <- D1],
    Set = [gen_set_items(I, T) || {I,T,_} <- D1],
    Objs = [gen_obj(I) || {I,_,_} <- D1],
    DupObjs = [gen_dup_obj(I, T) || {I,T,_} <- D1],
    DupObjRefs0 = [gen_dup_obj_refs(I) || {I,_,_} <- D1],
    DupObjRefs = lists:join(" |\n", DupObjRefs0),
    Asn1Spec = 'UniqueObjectSets',
    A = ["UniqueObjectSets DEFINITIONS AUTOMATIC TAGS ::=\n",
	 "BEGIN\n\n",
	 "TEST-UNIQUE ::= CLASS {\n"
	 " &id   INTEGER UNIQUE,\n"
	 " &Type OPTIONAL\n"
	 "}\n"
	 "WITH SYNTAX {IDENTIFIED BY &id [TYPE &Type]}\n",
	 $\n,
	 "DUP-CONTAINER ::= CLASS {\n"
	 " &id   INTEGER UNIQUE,\n"
	 " &data TEST-UNIQUE\n"
	 "} WITH SYNTAX {\n"
	 " ID &id, &data\n"
	 "}\n",
	 $\n,
	 Types,$\n,
	 "UniqSet TEST-UNIQUE ::= {\n",
	 Set,
	 " DupSet-1 |\n",
	 " DupSet-2, ...\n",
	 "}\n\n",
	 Objs,$\n,
	 DupObjs,$\n,
	 "DupSet-1 TEST-UNIQUE ::= {\n",
	 DupObjRefs,$\n,
	 "}\n\n",
	 "DupSet-2 TEST-UNIQUE ::= {\n",
	 DupObjRefs,",...\n",
	 "}\n\n",
	 "Seq ::= SEQUENCE {\n"
	 "  id TEST-UNIQUE.&id ({UniqSet}),\n"
	 "  type TEST-UNIQUE.&Type ({UniqSet}{@id})\n"
	 "}\n"
	 "END\n"],
    Asn1File = filename:join(CaseDir, atom_to_list(Asn1Spec)++".asn1"),
    ok = file:write_file(Asn1File, A),

    TestModule = 'unique_object_sets',
    Test0 = [gen_test(I, Data) || {I,_,Data} <- D1],
    Test = ["-module(",atom_to_list(TestModule),").\n"
	    "-export([main/1]).\n"
	    "\n"
	    "main(SeqRoundtrip) ->\n",
	    "  ",atom_to_list(Rule)," = '",atom_to_list(Asn1Spec),
	    "':encoding_rule(),\n",
	    Test0,
	    "  ok.\n"
	    ],
    ErlFile = filename:join(CaseDir, atom_to_list(TestModule)++".erl"),
    ok = file:write_file(ErlFile, Test),

    io:format("~s\n~s\n", [Asn1File,ErlFile]),
    case Rule of
	per ->
	    io:put_chars([A,$\n,Test,$\n]);
	_ ->
	    ok
    end,

    ok = asn1ct:compile(Asn1File, [Rule,{outdir,CaseDir}|Opts]),
    {ok,TestModule} = c:c(ErlFile, [{outdir,CaseDir}]),
    TestModule:main(fun seq_roundtrip/2),
    ok.

gen_types(I, Type) ->
    io_lib:format("AType~p ::= ~s\n", [I,Type]).

gen_set_items(I, T) ->
    io_lib:format(" {IDENTIFIED BY ~p TYPE AType~p} |\n"
		  " {IDENTIFIED BY ~p TYPE AType~p} |\n"
		  " {IDENTIFIED BY ~p TYPE ~s} |\n"
		  " obj-~p |\n\n",
		  [I,I,I,I,I,T,I]).

gen_obj(I) ->
    io_lib:format("obj-~p TEST-UNIQUE ::= {IDENTIFIED BY ~p TYPE AType~p}\n",
		  [I,I,I]).

gen_dup_obj(I, T) ->
    io_lib:format("dup-obj-~p DUP-CONTAINER ::= "
		  "{ID ~p, {IDENTIFIED BY ~p TYPE ~s}}\n",
		  [I,I,I+1000,T]).

gen_dup_obj_refs(I) ->
    io_lib:format("dup-obj-~p.&data", [I]).

gen_test(I, Data) ->
    io_lib:format("  ~s = SeqRoundtrip(~p, ~p),\n",
		  [match_term(Data),I,Data]).

match_term('_') ->
    "_";
match_term([H|T]=L) ->
    case is_intlist(L) of
	true ->
	    io_lib:format("~p", [L]);
	false ->
	    ["[",match_term(H),"|",match_term(T),"]"]
    end;
match_term(Tuple) when is_tuple(Tuple) ->
    ["{",match_term_tuple(Tuple, 1),"}"];
match_term(Other) ->
    io_lib:format("~p", [Other]).

match_term_tuple(T, I) when I =< tuple_size(T) ->
    [match_term(element(I, T)),
     if I < tuple_size(T) -> ",";
	true -> "" end|match_term_tuple(T, I+1)];
match_term_tuple(_, _) ->
    [].

is_intlist(L) ->
    lists:all(fun is_integer/1, L).
