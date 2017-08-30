%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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

-module(multi_load_SUITE).
-export([all/0, suite/0, many/1, on_load/1, errors/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() ->
    [many,on_load,errors].

many(_Config) ->
    Ms = make_modules(100, fun many_module/1),

    io:put_chars("Light load\n"
		 "=========="),
    many_measure(Ms),

    _ = [spawn_link(fun many_worker/0) || _ <- lists:seq(1, 8)],
    erlang:yield(),
    io:put_chars("Heavy load\n"
		 "=========="),
    many_measure(Ms),
    ok.

many_module(M) ->
    ["-module("++M++").",
     "-compile(export_all).",
     "f1() -> ok.",
     "f2() -> ok.",
     "f3() -> ok.",
     "f4() -> ok."].

many_measure(Ms) ->
    many_purge(Ms),
    MsPrep1 = prepare_modules(Ms),
    Us1 = ms(fun() -> many_load_seq(MsPrep1) end),
    many_try_call(Ms),
    many_purge(Ms),
    MsPrep2 = prepare_modules(Ms),
    Us2 = ms(fun() -> many_load_par(MsPrep2) end),
    many_try_call(Ms),
    io:format("# modules:  ~9w\n"
	      "Sequential: ~9w µs\n"
	      "Parallel:   ~9w µs\n"
	      "Ratio:      ~9w\n",
	      [length(Ms),Us1,Us2,divide(Us1,Us2)]),
    ok.

divide(A,B) when B > 0 -> A div B;
divide(_,_) -> inf.

many_load_seq(Ms) ->
    [erlang:finish_loading([M]) || M <- Ms],
    ok.

many_load_par(Ms) ->
    erlang:finish_loading(Ms).

many_purge(Ms) ->
    _ = [catch erlang:purge_module(M) || {M,_} <- Ms],
    ok.

many_try_call(Ms) ->
    _ = [begin
	     ok = M:f1(),
	     ok = M:f2(),
	     ok = M:f3(),
	     ok = M:f4()
	 end || {M,_} <- Ms],
    ok.

many_worker() ->
    many_worker(lists:seq(1, 100)).

many_worker(L) ->
    N0 = length(L),
    N1 = N0 * N0 * N0,
    N2 = N1 div (N0 * N0),
    N3 = N2 + 10,
    _ = N3 - 10,
    many_worker(L).


on_load(_Config) ->
    On = make_modules(2, fun on_load_module/1),
    OnPrep = prepare_modules(On),
    {'EXIT',{system_limit,_}} = (catch erlang:finish_loading(OnPrep)),

    Normal = make_modules(1, fun on_load_normal/1),
    Mixed = Normal ++ tl(On),
    MixedPrep = prepare_modules(Mixed),
    {'EXIT',{system_limit,_}} = (catch erlang:finish_loading(MixedPrep)),

    [false,true] = [erlang:has_prepared_code_on_load(Code) ||
		       Code <- MixedPrep],
    {'EXIT',{badarg,_}} = (catch erlang:has_prepared_code_on_load(<<1,2,3>>)),
    Magic = ets:match_spec_compile([{'_',[true],['$_']}]),
    {'EXIT',{badarg,_}} = (catch erlang:has_prepared_code_on_load(Magic)),

    SingleOnPrep = tl(OnPrep),
    {on_load,[OnLoadMod]} = erlang:finish_loading(SingleOnPrep),
    ok = erlang:call_on_load_function(OnLoadMod),
    ok.

on_load_module(M) ->
    ["-module("++M++").",
     "-on_load(f/0).",
     "f() -> ok."].

on_load_normal(M) ->
    ["-module("++M++")."].


errors(_Config) ->
    finish_loading_badarg(x),
    finish_loading_badarg([x|y]),
    finish_loading_badarg([x]),
    finish_loading_badarg([<<>>]),

    Mods = make_modules(2, fun errors_module/1),
    Ms = lists:sort([M || {M,_} <- Mods]),
    Prep = prepare_modules(Mods),
    {duplicated,Dups} = erlang:finish_loading(Prep ++ Prep),
    Ms = lists:sort(Dups),
    ok.

finish_loading_badarg(Arg) ->
    {'EXIT',{badarg,[{erlang,finish_loading,[Arg],_}|_]}} =
	(catch erlang:finish_loading(Arg)).

errors_module(M) ->
    ["-module("++M++").",
     "-export([f/0]).",
     "f() -> ok."].

%%%
%%% Common utilities
%%%

ms(Fun) ->
    {Ms,ok} = timer:tc(Fun),
    Ms.

make_modules(0, _) ->
    [];
make_modules(N, Fun) ->
    U = erlang:unique_integer([positive]),
    M0 = "m__" ++ integer_to_list(N) ++ "_" ++ integer_to_list(U),
    Contents = Fun(M0),
    Forms = [make_form(S) || S <- Contents],
    {ok,M,Code} = compile:forms(Forms),
    [{M,Code}|make_modules(N-1, Fun)].

make_form(S) ->
    {ok,Toks,_} = erl_scan:string(S),
    {ok,Form} = erl_parse:parse_form(Toks),
    Form.

prepare_modules(Ms) ->
    [erlang:prepare_loading(M, Code) || {M,Code} <- Ms].
