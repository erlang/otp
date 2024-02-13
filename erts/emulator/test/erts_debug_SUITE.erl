%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2005-2024. All Rights Reserved.
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
-include_lib("common_test/include/ct.hrl").
-include_lib("common_test/include/ct_event.hrl").

-export([all/0, suite/0, groups/0,
         init_per_testcase/2, end_per_testcase/2,
         test_size/1,flat_size_big/1,df/1,term_type/1,
         instructions/1, stack_check/1, alloc_blocks_size/1,
         t_copy_shared/1,
         interpreter_size_bench/1]).

-export([do_alloc_blocks_size/0]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() -> 
    [test_size, flat_size_big, df, instructions, term_type,
     t_copy_shared,
     stack_check, alloc_blocks_size].

groups() -> 
    [{interpreter_size_bench, [], [interpreter_size_bench]}].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).

interpreter_size_bench(_Config) ->
    Size = erts_debug:interpreter_size(),
    ct_event:notify(#event{name=benchmark_data,
                           data=[{value,Size}]}),
    {comment,integer_to_list(Size)++" bytes"}.

%% White box testing of term heap sizes
test_size(Config) when is_list(Config) ->
    ConsCell1 = id([a|b]),
    ConsCell2 = id(ConsCell1),
    ConsCellSz = 2,

    0 = do_test_size([]),
    0 = do_test_size(42),
    ConsCellSz = do_test_size(ConsCell1),
    0 = do_test_size({}),
    2 = do_test_size({[]}),
    3 = do_test_size({a,b}),
    7 = do_test_size({a,[b,c]}),
    8 = do_test_size(#{b => 2,c => 3}),
    3 = do_test_size(#{}),
    32 = do_test_size(#{b => 2,c => 3,txt => "hello world"}),

    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,256)])) >= map_size_lower_bound(256),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,4096)])) >= map_size_lower_bound(4096),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,254)])) >= map_size_lower_bound(254),
    true = do_test_size(maps:from_list([{I,I}||I<-lists:seq(1,239)])) >= map_size_lower_bound(239),

    Const = id(42),
    AnotherConst = id(7),

    %% Fun environment size = 0 (the smallest fun possible)
    SimplestFun = fun() -> ok end,

    %% 2 words for the fun, 1 word to point at the off-heap reference, and
    %% 3 words for the off-heap reference itself. The actual on-heap size is 3.
    FunSz0 = 6,
    FunSz0 = do_test_size(SimplestFun),

    %% Fun environment size = 1
    FunSz1 = do_test_size(fun() -> Const end),
    FunSz1 = FunSz0 + 1,

    %% Fun environment size = 2
    FunSz2 = do_test_size(fun() -> Const+AnotherConst end),
    FunSz2 = FunSz1 + 1,

    FunSz1 = do_test_size(fun() -> ConsCell1 end) - do_test_size(ConsCell1),

    %% External funs are always 2 words (they're also always stored off-heap,
    %% so the effective size is zero).
    2 = do_test_size(fun lists:sort/1),

    Arch = 8 * erlang:system_info({wordsize, external}),
    case {Arch, do_test_size(mk_ext_pid({a@b, 1}, 17, 42))} of
	{32, 5} -> ok;
	{64, 4} -> ok
    end,
    case {Arch, do_test_size(mk_ext_port({a@b, 1}, 1742))} of
	{32, 5} -> ok;
	{64, 4} -> ok
    end,
    case {Arch, do_test_size(make_ref())} of
	{32, 4} -> ok;
	{64, 3} -> ok
    end,
    case {Arch, do_test_size(mk_ext_ref({a@b, 1}, [42,43,44]))} of
	{32, 6} -> ok;
	{64, 5} -> ok
    end,
    3 = do_test_size(atomics:new(1,[])), % Magic ref

    3 = do_test_size(<<1,2,3>>),       % ErlHeapBin
    case {Arch, do_test_size(<<0:(8*64)>>)} of   % ERL_ONHEAP_BIN_LIMIT
	{32, 18} -> ok;
	{64, 10} -> ok
    end,
    8 = do_test_size(<<0:(8*65)>>),    % ErlSubBits + BinRef
    3 = do_test_size(<<5:7>>),         % ErlHeapBits
    8 = do_test_size(<<0:(8*80+1)>>),  % ErlSubBits + BinRef

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


term_type(Config) when is_list(Config) ->
    Ts = [{fixnum, 1},
          {fixnum, -1},
          {bignum, 1 bsl 300},
          {bignum, -(1 bsl 300)},
          {hfloat, 0.0},
          {hfloat, 0.0/-1},
          {hfloat, 1.0/(1 bsl 302)},
          {hfloat, 1.0*(1 bsl 302)},
          {hfloat, -1.0/(1 bsl 302)},
          {hfloat, -1.0*(1 bsl 302)},
          {hfloat, 3.1416},
          {hfloat, 1.0e18},
          {hfloat, -3.1416},
          {hfloat, -1.0e18},

          {heap_binary, <<1,2,3>>},
          {sub_binary, <<0:(8*80)>>},
          {heap_binary, <<5:7>>},

          {flatmap, #{ a => 1}},
          {hashmap, maps:from_list([{I,I}||I <- lists:seq(1,76)])},

          {list, [1,2,3]},
          {nil, []},
          {tuple, {1,2,3}},
          {tuple, {}},

          {export, fun lists:sort/1},
          {'fun', fun() -> ok end},
          {pid, self()},
          {atom, atom}],
    lists:foreach(fun({E,Val}) ->
                          R = erts_internal:term_type(Val),
                          io:format("expecting term type ~w, got ~w (~p)~n", [E,R,Val]),
                          E = R
                  end, Ts),
    ok.


df(Config) when is_list(Config) ->
    P0 = pps(),
    PrivDir = proplists:get_value(priv_dir, Config),
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

stack_check(Config) when is_list(Config) ->
    erts_debug:set_internal_state(available_internal_state,true),
    %% Recurses on the C stack until stacklimit is reached. That
    %% is, tests that the stack limit functionality works (used
    %% by PCRE). VM will crash if it doesn't work...
    Size = erts_debug:get_internal_state(stack_check),
    erts_debug:set_internal_state(available_internal_state,false),
    true = (is_integer(Size) and (Size > 0)),
    {comment, "Stack size: "++integer_to_list(Size)++" bytes"}.

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

alloc_blocks_size(Config) when is_list(Config) ->
    F = fun(Args) ->
                {ok, Peer, Node} = ?CT_PEER(Args),
                ok = rpc:call(Node, ?MODULE, do_alloc_blocks_size, []),
                peer:stop(Peer)
        end,
    case test_server:is_asan() of
	false -> F(["+Meamax"]);
	true -> skip
    end,
    F(["+Meamin"]),
    F([]),
    ok.

do_alloc_blocks_size() ->
    _ = erts_debug:alloc_blocks_size(binary_alloc),
    ok.

id(I) ->
    I.

mk_ext_pid({NodeName, Creation}, Number, Serial) ->
    erts_test_utils:mk_ext_pid({NodeName, Creation}, Number, Serial).

mk_ext_port({NodeName, Creation}, Number) ->
    erts_test_utils:mk_ext_port({NodeName, Creation}, Number).

mk_ext_ref({NodeName, Creation}, Numbers) ->
    erts_test_utils:mk_ext_ref({NodeName, Creation}, Numbers).


t_copy_shared(_Config) ->
    rand:seed(default),
    io:format("*** SEED: ~p ***\n", [rand:export_seed()]),

    [copy_shared_term_1(N div 10, CL) || N <- lists:seq(1,100),
                                         CL <- [false, true]],
    ok.

copy_shared_term_1(Size, CopyLit) ->
    Term = rand_term(Size),

    %% Note: Printing Term may suppress test failure
    %%       as it sends a copy to io-server.
    %%io:format("rand_term(~p): ~p\n", [Size, printable(Term)]),

    Binary = term_to_binary(Term),
    Copy = erts_debug:copy_shared(Term, CopyLit),
    test_eq(Term, Copy),
    copy_shared_term_2(Copy, Binary).

copy_shared_term_2(Copy, Binary) ->
    erlang:garbage_collect(),
    BinCopy = binary_to_term(Binary),
    test_eq(Copy, BinCopy),
    ok.

test_eq(A, B) ->
    case A of
        B -> ok;
        _ -> test_eq_fail("FAILED MATCH", A, B)
    end,
    case A == B of
        true -> ok;
        false -> test_eq_fail("FAILED EQUALITY", A, B)
    end.

test_eq_fail(Error, A, B) ->
    io:format("~s:\n\nA = ~p\n\nB = ~p\n",
              [Error, printable(A), printable(B)]),
    ct:fail(Error).

rand_term(Size) ->
    F = rand:uniform(100), % to produce non-literals
    Big = 666_701_523_687_345_689_643 * F,
    MagicRef = atomics:new(10,[]),
    Leafs = {atom, 42, 42.17*F,
             Big, -Big,
             [], {}, #{},
             "literal cons",
             {"literal boxed"},
             fun lists:sort/1,
             fun() -> F end,
             self(),
             lists:last(erlang:ports()),
             make_ref(),
             MagicRef,
             <<F:(8*10)>>,    % HeapBin
             <<F:(8*65)>>,    % ProcBin
             <<F:7>>,         % SubBin + HeapBin
             <<F:(8*80+1)>>,  % SubBin + ProcBin
             mk_ext_pid({a@b, 17}, 17, 42),
             mk_ext_port({a@b, 21}, 13),
             mk_ext_ref({a@b, 42}, [42, 19, 11])},
    rand_term(Leafs, Size).

rand_term(Leafs, Arity) when Arity > 0 ->
    Length = rand:uniform(Arity),
    List = [rand_term(Leafs, Arity-Length) || _ <- lists:seq(1,Length)],
    case rand:uniform(6) of
        1 -> List;
        2 -> list_to_improper_list(List);
        3 -> list_to_tuple(List);
        4 -> list_to_flatmap(List);
        5 -> list_to_hashmap(List);
        6 -> list_to_fun(List)
    end;
rand_term(Leafs, 0) ->
    element(rand:uniform(size(Leafs)), Leafs).

list_to_improper_list([A,B|T]) ->
    T ++ [A|B];
list_to_improper_list([H]) ->
    [[]|H].

list_to_flatmap(List) ->
    list_to_map(List, #{}).

list_to_hashmap(List) ->
    HashMap = #{1=>1, 2=>2, 3=>3, 4=>4, 5=>5, 6=>6, 7=>7, 8=>8, 9=>9,10=>0,
                11=>1,12=>2,13=>3,14=>4,15=>5,16=>6,17=>7,18=>8,19=>9,20=>0,
                21=>1,22=>2,23=>3,24=>4,25=>5,26=>6,27=>7,28=>8,29=>9,30=>0,
                31=>1,32=>2,33=>3},
    list_to_map(List, HashMap).

list_to_map([], Map) ->
    Map;
list_to_map([K], Map) ->
    Map#{K => K};
list_to_map([K,V|T], Map) ->
    list_to_map(T, Map#{K => V}).

list_to_fun([X]) ->
    fun(A) -> A + X end;
list_to_fun([X, Y]) ->
    fun(A) -> A + X + Y end;
list_to_fun([X, Y | T]) ->
    fun(A) -> [A+X+Y | T] end.


%% Convert local funs to maps to show fun environment
printable(Fun) when is_function(Fun) ->
    case erlang:fun_info(Fun, type) of
        {type,local} ->
            {env, Env} = erlang:fun_info(Fun, env),
            #{'fun' => [printable(T) || T <- Env]};
        {type,external} ->
            Fun
    end;
printable([H|T]) ->
    [printable(H)|printable(T)];
printable(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(printable(tuple_to_list(Tuple)));
printable(Map) when is_map(Map) ->
    maps:from_list(printable(maps:to_list(Map)));
printable(Leaf) ->
    Leaf.
