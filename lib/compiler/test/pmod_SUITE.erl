%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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
-module(pmod_SUITE).

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 basic/1, otp_8447/1]).

-include("test_server.hrl").

all(suite) ->
    test_lib:recompile(?MODULE),
    [basic, otp_8447].

init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = test_server:timetrap(?t:minutes(1)),
    [{watchdog,Dog}|Config].

fin_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

basic(Config) when is_list(Config) ->
    ?line basic_1(Config, []),
    ?line basic_1(Config, [inline]),
    ?line basic_1(Config, [{inline,500},inline]),
    ok.

basic_1(Config, Opts) ->
    io:format("Options: ~p\n", [Opts]),
    ?line ok = compile_load(pmod_basic, Config, Opts),

    ?line Prop1 = pmod_basic:new([{a,xb},{b,true},{c,false}]),
    ?line Prop2 = pmod_basic:new([{y,zz}]),
    ?line io:format("Prop1 = ~p\n", [Prop1]),
    ?line io:format("Prop2 = ~p\n", [Prop2]),

    ?line {a,xb} = Prop1:lookup(a),
    ?line none = Prop1:lookup(glurf),
    ?line false = Prop1:or_props([]),
    ?line true = Prop1:or_props([b,c]),
    ?line true = Prop1:or_props([b,d]),
    ?line false = Prop1:or_props([d]),

    ?line none = Prop2:lookup(kalle),
    ?line {y,zz} = Prop2:lookup(y),
    ?line {a,xb} = Prop1:lookup(a),

    ?line Prop3 = Prop1:prepend({blurf,true}),
    ?line io:format("Prop3 = ~p\n", [Prop3]),
    ?line {blurf,true} = Prop3:lookup(blurf),

    Prop4 = Prop3:append(42),
    ?line io:format("Prop4 = ~p\n", [Prop4]),
    ?line {42,5} = Prop4:stupid_sum(),

    %% Some record guards.
    ?line ok = Prop4:bar({s,0}),
    ?line ok = Prop4:bar_bar({s,blurf}),
    ?line error = Prop4:bar_bar({s,a,b}),
    ?line error = Prop4:bar_bar([]),

    ok.

otp_8447(Config) when is_list(Config) ->
    ?line P = pmod_basic:new(foo),
    ?line [0,0,1,1,1,0,0,1] = P:bc1(),
    ?line <<10:4>> = P:bc2(),
    ok.

compile_load(Module, Conf, Opts) ->
    ?line Dir = ?config(data_dir,Conf),
    ?line Src = filename:join(Dir, atom_to_list(Module)),
    ?line Out = ?config(priv_dir,Conf),
    ?line CompRc = compile:file(Src, [report,{outdir,Out}|Opts]),
    ?line {ok,Module} = CompRc,
    ?line code:purge(Module),
    ?line {module,Module} =
	code:load_abs(filename:join(Out, atom_to_list(Module))),
    ok.
