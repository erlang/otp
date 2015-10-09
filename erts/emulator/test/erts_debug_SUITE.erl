%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2012. All Rights Reserved.
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

-module(erts_debug_SUITE).
-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 test_size/1,flat_size_big/1,df/1,
	 instructions/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [test_size, flat_size_big, df, instructions].

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


init_per_testcase(Func, Config) when is_atom(Func), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Func, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

test_size(Config) when is_list(Config) ->
    ConsCell1 = id([a|b]),
    ConsCell2 = id(ConsCell1),
    ConsCellSz = 2,

    0 = do_test_size([]),
    0 = do_test_size(42),
    ConsCellSz = do_test_size(ConsCell1),
    1 = do_test_size({}),
    2 = do_test_size({[]}),
    3 = do_test_size({a,b}),
    7 = do_test_size({a,[b,c]}),
    8 = do_test_size(#{b => 2,c => 3}),
    4 = do_test_size(#{}),
    32 = do_test_size(#{b => 2,c => 3,txt => "hello world"}),

    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,256)])) >= map_size_lower_bound(256),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,4096)])) >= map_size_lower_bound(4096),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,254)])) >= map_size_lower_bound(254),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,239)])) >= map_size_lower_bound(239),

    %% Test internal consistency of sizes, but without testing
    %% exact sizes.
    Const = id(42),
    AnotherConst = id(7),

    %% Fun environment size = 0 (the smallest fun possible)
    SimplestFun = fun() -> ok end,
    FunSz0 = do_test_size(SimplestFun),

    %% Fun environment size = 1
    FunSz1 = do_test_size(fun() -> Const end),
    FunSz1 = FunSz0 + 1,

    %% Fun environment size = 2
    FunSz2 = do_test_size(fun() -> Const+AnotherConst end),
    FunSz2 = FunSz1 + 1,

    FunSz1 = do_test_size(fun() -> ConsCell1 end) - do_test_size(ConsCell1),

    %% Test shared data structures.
    do_test_size([ConsCell1|ConsCell1],
        	 3*ConsCellSz,
        	 2*ConsCellSz),
    do_test_size(fun() -> {ConsCell1,ConsCell2} end,
        	 FunSz2 + 2*ConsCellSz,
        	 FunSz2 + ConsCellSz),
    do_test_size({SimplestFun,SimplestFun},
        	 2*FunSz0+do_test_size({a,b}),
        	 FunSz0+do_test_size({a,b})),

    M = id(#{ "atom" => first, i => 0}),
    do_test_size([M,M#{ "atom" := other },M#{i := 42}],54,32),
    ok.

do_test_size(Term) ->
    Sz = erts_debug:flat_size(Term),
    Sz = erts_debug:size(Term).

do_test_size(Term, FlatSz, Sz) ->
    FlatSz = erts_debug:flat_size(Term),
    Sz = erts_debug:size(Term).

map_size_lower_bound(N) ->
    %% this est. is a bit lower that actual lower bound
    %% number of internal nodes
    T = (N - 1) div 15,
    %% total words
    2 + 17 * T + 2 * N.

flat_size_big(Config) when is_list(Config) ->
    %% Build a term whose external size only fits in a big num (on 32-bit CPU).
    flat_size_big_1(16#11111111111111117777777777777777888889999, 0, 16#FFFFFFF).

flat_size_big_1(Term, Size0, Limit) when Size0 < Limit ->
    case erts_debug:flat_size(Term) of
	Size when is_integer(Size), Size0 < Size ->
	    io:format("~p", [Size]),
	    flat_size_big_1([Term|Term], Size, Limit)
    end;
flat_size_big_1(_, _, _) -> ok.

df(Config) when is_list(Config) ->
    P0 = pps(),
    PrivDir = ?config(priv_dir, Config),
    ok = file:set_cwd(PrivDir),

    AllLoaded = [M || {M,_} <- code:all_loaded()],
    {Pid,Ref} = spawn_monitor(fun() -> df_smoke(AllLoaded) end),
    receive
	{'DOWN',Ref,process,Pid,Status} ->
	    normal = Status
    after 20*1000 ->
	    %% Not finished (i.e. a slow computer). Stop now.
	    Pid ! stop,
	    receive
		{'DOWN',Ref,process,Pid,Status} ->
		    normal = Status,
		    io:format("...")
	    end
    end,
    io:nl(),
    _ = [_ = file:delete(atom_to_list(M) ++ ".dis") ||
	    M <- AllLoaded],

    true = (P0 == pps()),
    ok.

df_smoke([M|Ms]) ->
    io:format("~p", [M]),
    erts_debug:df(M),
    receive
	stop ->
	    ok
    after 0 ->
	    df_smoke(Ms)
    end;
df_smoke([]) -> ok.

pps() ->
    {erlang:ports()}.

instructions(Config) when is_list(Config) ->
    Is = erts_debug:instructions(),
    _ = [list_to_atom(I) || I <- Is],
    ok.

id(I) ->
    I.
