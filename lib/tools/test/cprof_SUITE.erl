%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Define to run outside of test server
%%%
%%% -define(STANDALONE,1).
%%%
%%%
%%% Define for debug output
%%%
%%% -define(debug,1).

-module(cprof_SUITE).

%% Exported end user tests
-export([basic_test/0, on_load_test/1, modules_test/1]).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server related stuff
%%

-ifdef(STANDALONE).
-define(config(A,B),config(A,B)).
-export([config/2]).
-else.
-include_lib("common_test/include/ct.hrl").
-endif.

-ifdef(debug).
-ifdef(STANDALONE).
-define(line, erlang:display({?MODULE,?LINE}), ).
-endif.
-define(dbgformat(A,B),io:format(A,B)).
-else.
-ifdef(STANDALONE).
-define(line, noop, ).
-endif.
-define(dbgformat(A,B),noop).
-endif.

-ifdef(STANDALONE).
config(priv_dir, _) ->
    ".";
config(data_dir, _) ->
    "cprof_SUITE_data".
-else.
%% When run in test server.
-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
         not_run/1]).
-export([basic/1, on_load/1, modules/1]).

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_count]),
    erlang:trace(all, false, [all]),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{seconds,30}}].

all() -> 
    case test_server:is_native(cprof_SUITE) of
        true -> [not_run];
        false -> [basic, on_load, modules]
    end.


not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

%% Tests basic profiling
basic(Config) when is_list(Config) ->
    basic_test().

%% Tests profiling of unloaded module
on_load(Config) when is_list(Config) ->
    on_load_test(Config).

%% Tests profiling of several modules
modules(Config) when is_list(Config) ->
    modules_test(Config).

-endif. %-ifdef(STANDALONE). ... -else.

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The Tests
%%%

basic_test() ->
    M = 1000,
    %%
    M2 = M*2,
    M3 = M*3,
    M2__1 = M2 + 1,
    M3__1 = M3 + 1,
    N = cprof:stop(),
    %%
    2 = cprof:start(?MODULE, seq_r),
    1 = cprof:start(?MODULE, seq, 3),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    L = lists:reverse(Lr),
    %%
    io:format("~p~n~p~n~p~n",
              [erlang:trace_info({?MODULE,sec_r,3}, all),
               erlang:trace_info({?MODULE,sec_r,4}, all),
               erlang:trace_info({?MODULE,sec,3}, all)]),
    %%
    ModAna1 = {?MODULE,M2__1,[{{?MODULE,seq_r,4},M},
                              {{?MODULE,seq,3},M},
                              {{?MODULE,seq_r,3},1}]},
    ModAna1 = cprof:analyse(?MODULE,0),
    {M2__1, [ModAna1]} = cprof:analyse(),
    ModAna1 = cprof:analyse(?MODULE, 1),
    {M2__1, [ModAna1]} = cprof:analyse(1),
    %%
    ModAna2 = {?MODULE,M2__1,[{{?MODULE,seq_r,4},M},
                              {{?MODULE,seq,3},M}]},
    ModAna2 = cprof:analyse(?MODULE, 2),
    {M2__1, [ModAna2]} = cprof:analyse(2),
    %%
    2 = cprof:pause(?MODULE, seq_r),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    %%
    ModAna3 = {?MODULE,M3__1,[{{?MODULE,seq,3},M2},
                              {{?MODULE,seq_r,4},M},
                              {{?MODULE,seq_r,3},1}]},
    ModAna3 = cprof:analyse(?MODULE),
    %%
    N = cprof:pause(),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    %%
    {M3__1, [ModAna3]} = cprof:analyse(),
    %%
    N = cprof:restart(),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    %%
    ModAna1 = cprof:analyse(?MODULE),
    %%
    N = cprof:stop(),
    {?MODULE,0,[]} = cprof:analyse(?MODULE),
    {0,[]} = cprof:analyse(),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_load_test(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "cprof_SUITE_test"),
    Module = cprof_SUITE_test,
    M = 1000,
    %%
    M2 = M*2,
    M2__1 = M2 + 1,
    N1 = cprof:start(),

    {ok,Module} = c:c(File, [{outdir,Priv}]),

    %% If this system is hipe-enabled, the loader may have called module_info/1
    %% when Module was loaded above. Reset the call count to avoid seeing
    %% the call in the analysis below.

    1 = cprof:restart(Module, module_info, 1),

    L = Module:seq(1, M, fun succ/1),
    Lr = Module:seq_r(1, M, fun succ/1),
    Lr = lists:reverse(L),
    N2 = cprof:pause(),
    N3 = cprof:pause(Module),
    {Module,M2__1,[{{Module,seq_r,4},M},
                   {{Module,seq,3},M},
                   {{Module,seq_r,3},1}]} = cprof:analyse(Module),
    io:format("~p ~p ~p~n", [N1, N2, N3]),
    code:purge(Module),
    code:delete(Module),
    N4 = N2 - N3,
    %%
    N4 = cprof:restart(),
    {ok,Module} = c:c(File, [{outdir,Priv}]),
    L = Module:seq(1, M, fun succ/1),
    Lr = Module:seq_r(1, M, fun succ/1),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    N2 = cprof:pause(),
    {Module,0,[]} = cprof:analyse(Module),
    M_1 = M - 1,
    M4__4 = M*4 - 4,
    M10_7 = M*10 - 7,
    {?MODULE,M10_7,[{{?MODULE,succ,1},M4__4},
                    {{?MODULE,seq_r,4},M},
                    {{?MODULE,seq,3},M},
                    {{?MODULE,'-on_load_test/1-fun-5-',1},M_1},
                    {{?MODULE,'-on_load_test/1-fun-4-',1},M_1},
                    {{?MODULE,'-on_load_test/1-fun-3-',1},M_1},
                    {{?MODULE,'-on_load_test/1-fun-2-',1},M_1},
                    {{?MODULE,seq_r,3},1}]}
    = cprof:analyse(?MODULE),
    N2 = cprof:stop(),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modules_test(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    File = filename:join(Data, "cprof_SUITE_test"),
    Module = cprof_SUITE_test,
    {ok,Module} = c:c(File, [{outdir,Priv}]),
    M = 10,
    %%
    M2 = M*2,
    M2__1 = M2 + 1,
    erlang:yield(),
    N = cprof:start(),
    L = Module:seq(1, M, fun succ/1),
    Lr = Module:seq_r(1, M, fun succ/1),
    L = seq(1, M, fun succ/1),
    Lr = seq_r(1, M, fun succ/1),
    N = cprof:pause(),
    Lr = lists:reverse(L),
    M_1 = M - 1,
    M4_4 = M*4 - 4,
    M10_7 = M*10 - 7,
    M2__1 = M*2 + 1,
    {Tot,ModList} = cprof:analyse(),
    {value,{?MODULE,M10_7,[{{?MODULE,succ,1},M4_4},
                           {{?MODULE,seq_r,4},M},
                           {{?MODULE,seq,3},M},
                           {{?MODULE,'-modules_test/1-fun-3-',1},M_1},
                           {{?MODULE,'-modules_test/1-fun-2-',1},M_1},
                           {{?MODULE,'-modules_test/1-fun-1-',1},M_1},
                           {{?MODULE,'-modules_test/1-fun-0-',1},M_1},
                           {{?MODULE,seq_r,3},1}]}} =
      lists:keysearch(?MODULE, 1, ModList),
    {value,{Module,M2__1,[{{Module,seq_r,4},M},
                          {{Module,seq,3},M},
                          {{Module,seq_r,3},1}]}} =
      lists:keysearch(Module, 1, ModList),
    Tot = lists:foldl(fun ({_,C,_}, A) -> C+A end, 0, ModList),
    {cprof,_,Prof} = cprof:analyse(cprof),
    {value,{{cprof,pause,0},1}} = lists:keysearch({cprof,pause,0}, 1, Prof),
    N = cprof:stop(),
    ok.



%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local helpers


%% Stack recursive seq
seq(Stop, Stop, Succ) when is_function(Succ) ->
    [Stop];
seq(Start, Stop, Succ) when is_function(Succ) ->
    [Start | seq(Succ(Start), Stop, Succ)].



%% Tail recursive seq, result list is reversed
seq_r(Start, Stop, Succ) when is_function(Succ) ->
    seq_r(Start, Stop, Succ, []).

seq_r(Stop, Stop, _, R) ->
    [Stop | R];
seq_r(Start, Stop, Succ, R) ->
    seq_r(Succ(Start), Stop, Succ, [Start | R]).


%% Successor
succ(X) -> X+1.
