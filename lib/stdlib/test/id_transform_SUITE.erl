%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2018. All Rights Reserved.
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
-module(id_transform_SUITE).
-author('pan@erix.ericsson.se').

-include_lib("kernel/include/file.hrl").
          
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 id_transform/1]).

-export([check/2,check2/1,g/0,f/1,t/1,t1/1,t2/1,t3/1,t4/1,
	 t5/1,apa/1,new_fun/0]).

%% Serves as test...
-hej(hopp).
-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [id_transform].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% Test erl_id_trans.
id_transform(Config) when is_list(Config) ->
    File = filename:join([code:lib_dir(stdlib),"examples",
			"erl_id_trans.erl"]),
    {ok,erl_id_trans,Bin} = compile:file(File,[binary]),
    {module,erl_id_trans} = code:load_binary(erl_id_trans, File, Bin),
    case test_server:is_valgrind() of
	false ->
	    ct:timetrap({hours,1}),
	    run_in_test_suite();
	true ->
	    {skip,"Valgrind (too slow)"}
    end.

run_in_test_suite() ->
    SuperDir = filename:dirname(filename:dirname(code:which(?MODULE))),
    TestDirs = filelib:wildcard(filename:join([SuperDir,"*_test"])),
    AbsDirs = [filename:absname(X) || X <- code:get_path()],
    Dirs = ordsets:from_list(AbsDirs ++ TestDirs),
    run_list(Dirs).

run_list(PathL) ->
    io:format("Where to search for beam files:\n~p\n", [PathL]),
    io:format("Searching for beam files ...~n",[]),
    Beams = collect_beams(PathL, []),
    io:format("~p beam files\n", [length(Beams)]),
    io:format("Applying erl_id_trans to found beam files...~n",[]),
    Res = [do_trans(X) || X <- Beams],
    io:format("...done~n",[]),
    Successes = [X || {ok,X} <- Res],
    SevereFailures = [{F,E} || {failed,{F,{transform,E}}} <- Res],
    BeamLib = [{F,E} || {failed,{F,{beam_lib,E}}} <- Res],
    io:format("~p files processed", [length(Res)]),
    io:format("~p files successfully transformed", [length(Successes)]),
    case length(SevereFailures) of
	0 -> ok;
	SevLen ->
	    io:format("\n~p severe failures:\n~p",
		      [SevLen,SevereFailures])
    end,
    case BeamLib of
	[] -> ok;
	_ -> io:format("\nbeam_lib failures:\n~p", [BeamLib])
    end,
    case length(SevereFailures) of
	0 -> ok;
	Len -> {failed,integer_to_list(Len)++" failures"}
    end.
    

collect_beams([P0|Ps], Acc) ->
    Wc = filename:join(filename:absname(P0), "*.beam"),
    collect_beams(Ps, filelib:wildcard(Wc)++Acc);
collect_beams([], Acc) -> Acc.
    
do_trans(Beam) ->
    case beam_lib:chunks(Beam, [abstract_code]) of
	{ok,{_Mod,[{abstract_code,no_abstract_code}]}} ->
	    {failed,{Beam,{beam_lib,no_debug_info}}};
	{ok,{_Mod,[{abstract_code,{raw_abstract_v1,Abst}}]}} ->
	    do_trans_1(Beam, Abst);
	{ok,{_Mod,[{abstract_code,{Tag,_}}]}} ->
	    {failed,{Beam,{beam_lib,{wrong_type_of_debug_info,Tag}}}};
	{ok,{_Mod,[{abstract_code,_}]}} ->
	    {failed,{Beam,{beam_lib,unknown_type_of_debug_info}}};
	{error,beam_lib,{missing_chunk,_,_}} ->
	    {failed,{Beam,{beam_lib,no_debug_info}}};	    
	Error ->
	    {failed,{Beam,{beam_lib,Error}}}
    end.

do_trans_1(File, Tree0) ->
    case catch erl_id_trans:parse_transform(Tree0, []) of
	Tree0 when is_list(Tree0) ->
	    {ok,File};
	Tree when is_list(Tree) ->
	    {failed,{File,{transform,output_not_same_as_input}}};
	{'EXIT', Reason} ->
	    {failed,{File,{transform,{crash,Reason}}}};
	Else ->
	    {failed,{File,{transform,{unknown,Else}}}}
    end.

%% From here on there's only fake code to serve as test cases
%% for the id_transform.
%% They need to be exported.

check(X,_Y) when X ->   
    true;
check(A,_) when atom(A) ->
    atom;
check(A,_) when erlang:is_list(A) ->
    list;
check(A,B) when erlang:'+'(A,B) ->
    atom;
check(_,_) ->
    false.

check2(A) ->
    case A of
	"hej" ++ "hopp" ->
	    a;
	[$l,$e,$k] ++ "hopp" ->
	    a;
	[1] ++ [2] ->
	    b
    end.

-record(x,{x,y}).
-record(y,{x=1,y=0}).

g() ->
    #y.y.

f(#y.y) ->
    vansinne;

f(X) when X =:= #y.y ->
    {#y.y,_Y} = {X,hej};
f(#x{_='_'}) ->
    hopp;
f(#x{x=true,y=true}) ->
    babba;
f(A) when A == #x{x=true,y=true} ->
    true;
f(A) when A#x.x == 4 ->
    #x{x = 1, _ = 2};
f(X) ->
    if X#x.y ->
	    ok;
       element(3,X) ->
	    banan;
       true ->
	    nok
    end.

%% Stolen from erl_lint_SUITE.erl
-record(apa, {}).

t(A) when atom(A) ->
    atom;
t(A) when binary(A) ->
    binary;
t(A) when float(A) ->
    float;
t(A) when function(A) ->
    function;
t(A) when integer(A) ->
    integer;
t(A) when is_atom(A) ->
    is_atom;
t(A) when is_binary(A) ->
    is_binary;
t(A) when is_float(A) ->
    is_float;
t(A) when is_function(A) ->
    is_function;
t(A) when is_integer(A) ->
    is_integer;
t(A) when is_list(A) ->
    is_list;
t(A) when is_number(A) ->
    is_number;
t(A) when is_pid(A) ->
    is_pid;
t(A) when is_port(A) ->
    is_port;
t(A) when is_record(A, apa) ->
    is_record;
t(A) when is_reference(A) ->
    is_reference;
t(A) when is_tuple(A) ->
    is_tuple;
t(A) when list(A) ->
    list;
t(A) when number(A) ->
    number;
t(A) when pid(A) ->
    pid;
t(A) when port(A) ->
    port;
t(A) when record(A, apa) ->
    record;
t(A) when reference(A) ->
    reference;
t(A) when tuple(A) ->
    tuple.

t1(A) when atom(A), atom(A) ->
    atom;
t1(A) when binary(A), binary(A) ->
    binary;
t1(A) when float(A), float(A) ->
    float;
t1(A) when function(A), function(A) ->
    function;
t1(A) when integer(A), integer(A) ->
    integer;
t1(A) when is_atom(A), is_atom(A) ->
    is_atom;
t1(A) when is_binary(A), is_binary(A) ->
    is_binary;
t1(A) when is_float(A), is_float(A) ->
    is_float;
t1(A) when is_function(A), is_function(A) ->
    is_function;
t1(A) when is_integer(A), is_integer(A) ->
    is_integer;
t1(A) when is_list(A), is_list(A) ->
    is_list;
t1(A) when is_number(A), is_number(A) ->
    is_number;
t1(A) when is_pid(A), is_pid(A) ->
    is_pid;
t1(A) when is_port(A), is_port(A) ->
    is_port;
t1(A) when is_record(A, apa), is_record(A, apa) ->
    is_record;
t1(A) when is_reference(A), is_reference(A) ->
    is_reference;
t1(A) when is_tuple(A), is_tuple(A) ->
    is_tuple;
t1(A) when list(A), list(A) ->
    list;
t1(A) when number(A), number(A) ->
    number;
t1(A) when pid(A), pid(A) ->
    pid;
t1(A) when port(A), port(A) ->
    port;
t1(A) when record(A, apa), record(A, apa) ->
    record;
t1(A) when reference(A), reference(A) ->
    reference;
t1(A) when tuple(A), tuple(A) ->
    tuple.

t2(A) when atom(A); atom(A) ->
    atom;
t2(A) when binary(A); binary(A) ->
    binary;
t2(A) when float(A); float(A) ->
    float;
t2(A) when function(A); function(A) ->
    function;
t2(A) when integer(A); integer(A) ->
    integer;
t2(A) when is_atom(A); is_atom(A) ->
    is_atom;
t2(A) when is_binary(A); is_binary(A) ->
    is_binary;
t2(A) when is_float(A); is_float(A) ->
    is_float;
t2(A) when is_function(A); is_function(A) ->
    is_function;
t2(A) when is_integer(A); is_integer(A) ->
    is_integer;
t2(A) when is_list(A); is_list(A) ->
    is_list;
t2(A) when is_number(A); is_number(A) ->
    is_number;
t2(A) when is_pid(A); is_pid(A) ->
    is_pid;
t2(A) when is_port(A); is_port(A) ->
    is_port;
t2(A) when is_record(A, apa); is_record(A, apa) ->
    is_record;
t2(A) when is_reference(A); is_reference(A) ->
    is_reference;
t2(A) when is_tuple(A); is_tuple(A) ->
    is_tuple;
t2(A) when list(A); list(A) ->
    list;
t2(A) when number(A); number(A) ->
    number;
t2(A) when pid(A); pid(A) ->
    pid;
t2(A) when port(A); port(A) ->
    port;
t2(A) when record(A, apa); record(A, apa) ->
    record;
t2(A) when reference(A); reference(A) ->
    reference;
t2(A) when tuple(A); tuple(A) ->
    tuple.

t3(A) when is_atom(A) or is_atom(A) ->
    is_atom;
t3(A) when is_binary(A) or is_binary(A) ->
    is_binary;
t3(A) when is_float(A) or is_float(A) ->
    is_float;
t3(A) when is_function(A) or is_function(A) ->
    is_function;
t3(A) when is_integer(A) or is_integer(A) ->
    is_integer;
t3(A) when is_list(A) or is_list(A) ->
    is_list;
t3(A) when is_number(A) or is_number(A) ->
    is_number;
t3(A) when is_pid(A) or is_pid(A) ->
    is_pid;
t3(A) when is_port(A) or is_port(A) ->
    is_port;
t3(A) when is_record(A, apa) or is_record(A, apa) ->
   is_record;
t3(A) when is_reference(A) or is_reference(A) ->
   is_reference;
t3(A) when is_tuple(A) or is_tuple(A) ->
   is_tuple; 
t3(A) when record(A, apa) ->
   foo;
t3(A) when erlang:is_record(A, apa) ->
   foo;
t3(A) when is_record(A, apa) ->
   foo;
t3(A) when record({apa}, apa) ->
   {A,foo}.

t4(A) when erlang:is_record({apa}, apa) ->
   {A,foo}.

t5(A) when is_record({apa}, apa) ->
   {A,foo}.

-record(apa2,{a=a,b=foo:bar()}).
apa(1) ->
    [X || X <- [], #apa2{a = a} == {r,X,foo}];
apa(2) ->
    [X || X <- [], #apa2{b = b} == {r,X,foo}];
apa(3) ->
    [X || X <- [], 3 == X#apa2.a].

new_fun() ->
    lists:map(fun erlang:abs/1, [-1,3,4]).
