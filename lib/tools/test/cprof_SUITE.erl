%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2010. All Rights Reserved.
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
-include("test_server.hrl").
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
-export([all/1, init_per_testcase/2, end_per_testcase/2, not_run/1]).
-export([basic/1, on_load/1, modules/1]).
	 
init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(test_server:seconds(30)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    erlang:trace_pattern({'_','_','_'}, false, [local,meta,call_count]),
    erlang:trace_pattern(on_load, false, [local,meta,call_count]),
    erlang:trace(all, false, [all]),
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(doc) ->
    ["Test the cprof profiling tool."];
all(suite) ->
    case test_server:is_native(?MODULE) of
	true -> [not_run];
	false -> [basic, on_load, modules]
%, on_and_off, info, 
%		  pause_and_restart, combo]
    end.

not_run(Config) when is_list(Config) -> 
    {skipped,"Native code"}.

basic(suite) ->
    [];
basic(doc) ->
    ["Tests basic profiling"];
basic(Config) when is_list(Config) ->
    basic_test().

on_load(suite) ->
    [];
on_load(doc) ->
    ["Tests profiling of unloaded module"];
on_load(Config) when is_list(Config) ->
    on_load_test(Config).

modules(suite) ->
    [];
modules(doc) ->
    ["Tests profiling of several modules"];
modules(Config) when is_list(Config) ->
    modules_test(Config).

-endif. %-ifdef(STANDALONE). ... -else.

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% The Tests
%%%

basic_test() ->
    ?line M = 1000,
    %%
    ?line M2 = M*2,
    ?line M3 = M*3,
    ?line M2__1 = M2 + 1,
    ?line M3__1 = M3 + 1,
    ?line N = cprof:stop(),
    %%
    ?line 2 = cprof:start(?MODULE, seq_r),
    ?line 1 = cprof:start(?MODULE, seq, 3),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    ?line L = lists:reverse(Lr),
    %%
    ?line io:format("~p~n~p~n~p~n", 
		    [erlang:trace_info({?MODULE,sec_r,3}, all),
		     erlang:trace_info({?MODULE,sec_r,4}, all),
		     erlang:trace_info({?MODULE,sec,3}, all)]),
    %%
    ?line ModAna1 = {?MODULE,M2__1,[{{?MODULE,seq_r,4},M},
				   {{?MODULE,seq,3},M},
				   {{?MODULE,seq_r,3},1}]},
    ?line ModAna1 = cprof:analyse(?MODULE,0),
    ?line {M2__1, [ModAna1]} = cprof:analyse(),
    ?line ModAna1 = cprof:analyse(?MODULE, 1),
    ?line {M2__1, [ModAna1]} = cprof:analyse(1),
    %%
    ?line ModAna2 = {?MODULE,M2__1,[{{?MODULE,seq_r,4},M},
				   {{?MODULE,seq,3},M}]},
    ?line ModAna2 = cprof:analyse(?MODULE, 2),
    ?line {M2__1, [ModAna2]} = cprof:analyse(2),
    %%
    2 = cprof:pause(?MODULE, seq_r),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    %%
    ?line ModAna3 = {?MODULE,M3__1,[{{?MODULE,seq,3},M2},
				   {{?MODULE,seq_r,4},M},
				   {{?MODULE,seq_r,3},1}]},
    ?line ModAna3 = cprof:analyse(?MODULE),
    %%
    ?line N = cprof:pause(),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    %%
    ?line {M3__1, [ModAna3]} = cprof:analyse(),
    %%
    ?line N = cprof:restart(),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    %%
    ?line ModAna1 = cprof:analyse(?MODULE),
    %%
    ?line N = cprof:stop(),
    ?line {?MODULE,0,[]} = cprof:analyse(?MODULE),
    ?line {0,[]} = cprof:analyse(),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on_load_test(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "cprof_SUITE_test"),
    ?line Module = cprof_SUITE_test,
    ?line M = 1000,
    %%
    ?line M2 = M*2,
    ?line M2__1 = M2 + 1,
    ?line N1 = cprof:start(),

    ?line {ok,Module} = c:c(File, [{outdir,Priv}]),

    %% If this system is hipe-enabled, the loader may have called module_info/1
    %% when Module was loaded above. Reset the call count to avoid seeing
    %% the call in the analysis below.

    ?line 1 = cprof:restart(Module, module_info, 1),

    ?line L = Module:seq(1, M, fun succ/1),
    ?line Lr = Module:seq_r(1, M, fun succ/1),
    ?line Lr = lists:reverse(L),
    ?line N2 = cprof:pause(),
    ?line N3 = cprof:pause(Module),
    ?line {Module,M2__1,[{{Module,seq_r,4},M},
			 {{Module,seq,3},M},
			 {{Module,seq_r,3},1}]} = cprof:analyse(Module),
    ?line io:format("~p ~p ~p~n", [N1, N2, N3]),
    ?line code:purge(Module),
    ?line code:delete(Module),
    ?line N4 = N2 - N3,
    %%
    ?line N4 = cprof:restart(),
    ?line {ok,Module} = c:c(File, [{outdir,Priv}]),
    ?line L = Module:seq(1, M, fun succ/1),
    ?line Lr = Module:seq_r(1, M, fun succ/1),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    ?line N2 = cprof:pause(),
    ?line {Module,0,[]} = cprof:analyse(Module),
    ?line M_1 = M - 1,
    ?line M4__4 = M*4 - 4,
    ?line M10_7 = M*10 - 7,
    ?line {?MODULE,M10_7,[{{?MODULE,succ,1},M4__4},
			  {{?MODULE,seq_r,4},M},
			  {{?MODULE,seq,3},M},
			  {{?MODULE,'-on_load_test/1-fun-5-',1},M_1},
			  {{?MODULE,'-on_load_test/1-fun-4-',1},M_1},
			  {{?MODULE,'-on_load_test/1-fun-3-',1},M_1},
			  {{?MODULE,'-on_load_test/1-fun-2-',1},M_1},
			  {{?MODULE,seq_r,3},1}]} 
	= cprof:analyse(?MODULE),
    ?line N2 = cprof:stop(),
    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modules_test(Config) ->
    ?line Priv = ?config(priv_dir, Config),
    ?line Data = ?config(data_dir, Config),
    ?line File = filename:join(Data, "cprof_SUITE_test"),
    ?line Module = cprof_SUITE_test,
    ?line {ok,Module} = c:c(File, [{outdir,Priv}]),
    ?line M = 10,
    %%
    ?line M2 = M*2,
    ?line M2__1 = M2 + 1,
    ?line erlang:yield(),
    ?line N = cprof:start(),
    ?line L = Module:seq(1, M, fun succ/1),
    ?line Lr = Module:seq_r(1, M, fun succ/1),
    ?line L = seq(1, M, fun succ/1),
    ?line Lr = seq_r(1, M, fun succ/1),
    ?line N = cprof:pause(),
    ?line Lr = lists:reverse(L),
    ?line M_1 = M - 1,
    ?line M4_4 = M*4 - 4,
    ?line M10_7 = M*10 - 7,
    ?line M2__1 = M*2 + 1,
    ?line {Tot,ModList} = cprof:analyse(),
    ?line {value,{?MODULE,M10_7,[{{?MODULE,succ,1},M4_4},
				 {{?MODULE,seq_r,4},M},
				 {{?MODULE,seq,3},M},
				 {{?MODULE,'-modules_test/1-fun-3-',1},M_1},
				 {{?MODULE,'-modules_test/1-fun-2-',1},M_1},
				 {{?MODULE,'-modules_test/1-fun-1-',1},M_1},
				 {{?MODULE,'-modules_test/1-fun-0-',1},M_1},
				 {{?MODULE,seq_r,3},1}]}} =
	lists:keysearch(?MODULE, 1, ModList),
    ?line {value,{Module,M2__1,[{{Module,seq_r,4},M},
				{{Module,seq,3},M},
				{{Module,seq_r,3},1}]}} = 
	lists:keysearch(Module, 1, ModList),
    ?line Tot = lists:foldl(fun ({_,C,_}, A) -> C+A end, 0, ModList),
    ?line {cprof,_,Prof} = cprof:analyse(cprof),
    ?line {value,{{cprof,pause,0},1}} = 
	lists:keysearch({cprof,pause,0}, 1, Prof),
    ?line N = cprof:stop(),
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
