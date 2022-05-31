%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2020. All Rights Reserved.
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
-module(ei_accept_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ei_accept_SUITE_data/ei_accept_test_cases.hrl").

-export([all/0, suite/0,
         init_per_testcase/2,
         ei_accept/1,
         hopeful_random/1,
         ei_threaded_accept/1,
         monitor_ei_process/1]).

%% Internals
-export([id/1]).

-import(runner, [get_term/1,send_term/2]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [ei_accept,
     hopeful_random,
     ei_threaded_accept,
     monitor_ei_process].

init_per_testcase(Case, Config) ->
    rand:uniform(), % Make sure rand is initialized and seeded.
    %%rand:seed({exsss, [61781477086241372|88832360391433009]}),
    io:format("** rand seed = ~p\n", [rand:export_seed()]),
    runner:init_per_testcase(?MODULE, Case, Config).

ei_accept(Config) when is_list(Config) ->

    [ei_accept_do(Config, CR, SI)
     || CR <- [0,21],
        SI <- [default, ussi]],
    ok.

ei_accept_do(Config, CompatRel, SockImpl) ->
    io:format("CompatRel=~p, SockImpl=~p\n", [CompatRel, SockImpl]),
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, CompatRel, SockImpl),

    Myname = hd(tl(string:tokens(atom_to_list(node()), "@"))),
    io:format("Myname ~p ~n",  [Myname]),
    EINode = list_to_atom("c42@"++Myname),
    io:format("EINode ~p ~n",  [EINode]),

    %% We take this opportunity to also test export-funs and bit-strings
    %% with (ugly) tuple fallbacks in OTP 21 and older.
    %% Test both toward pending connection and established connection.
    TermsAndFallbacks =
        [{<<1:1>>, {<<128>>,1}},
         {fun lists:map/2, {lists,map}},

         %% Also test funs with hopeful encoding in environment,
         %% which lead to incorrect fun size encoding (OTP-18104)
         %% toward pending connection.
         {fun_with_env(<<1:1>>), fun_with_env({<<128>>,1})},
         {fun_with_env(fun lists:map/2), fun_with_env({lists,map})}],
    {RealTerms, Fallbacks} = lists:unzip(TermsAndFallbacks),
    EncTerms = case CompatRel of
                   0 -> RealTerms;
                   21 -> Fallbacks
               end,

    Self = self(),
    Funny = fun() -> hello end,
    TermToSend = {call, Self, "Test", Funny, RealTerms},
    TermToGet  = {call, Self, "Test", Funny, EncTerms},
    Port = 6543,
    {ok, ListenFd} = ei_publish(P, Port),
    {any, EINode} ! TermToSend,

    {ok, Fd, Node} = ei_accept(P, ListenFd),
    Node = node(),
    Got1 = ei_receive(P, Fd),

    %% Send again, now without auto-connect
    {any, EINode} ! TermToSend,
    Got2 = ei_receive(P, Fd),

    io:format("Sent ~p~nExp. ~p~nGot1 ~p~nGot2 ~p~n", [TermToSend, TermToGet, Got1, Got2]),
    TermToGet = Got1,
    TermToGet = Got2,

    runner:finish(P),
    ok.

fun_with_env(Term) ->
    Env = ?MODULE:id(Term),
    fun() -> Env end.

id(X) -> X.


%% Send random hopeful encoded terms from emulator to c-node
%% and verify correct encoding with/without fallback.
hopeful_random(Config) when is_list(Config) ->
    [hopeful_random_do(Config, CR, SI)
     || CR <- [0, 21],
        SI <- [default, ussi]],
    ok.


hopeful_random_do(Config, CompatRel, SockImpl) ->
    io:format("CompatRel=~p, SockImpl=~p\n", [CompatRel, SockImpl]),
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, CompatRel, SockImpl),

    Myname = hd(tl(string:tokens(atom_to_list(node()), "@"))),
    io:format("Myname ~p ~n",  [Myname]),
    EINode = list_to_atom("c42@"++Myname),
    io:format("EINode ~p ~n",  [EINode]),

    Port = 6543,
    {ok, ListenFd} = ei_publish(P, Port),

    Terms = [rand_term(10) || _ <- lists:seq(1,10)],

    %% lists:foldl(fun(T,N) ->
    %%                     io:format("Term #~p = ~p\n", [N, printable(T)]),
    %%                     N+1
    %%             end,
    %%             1,
    %%             Terms),

    %% Send on pending connection (hopeful encoding)
    [{any, EINode} ! T || T <- Terms],
    {ok, Fd, Node} = ei_accept(P, ListenFd),
    Node = node(),
    [match(T, ei_receive(P, Fd), CompatRel) || T <- Terms],

    runner:finish(P),
    ok.


match(A, B, 0) ->
    match(A, B);
match(A, B, 21) ->
    match(fallback(printable(A)),
          printable(B)). %% B assumed to already be fallback'ed

match(A, A) -> ok;
match(A, B) ->
    io:format("match failed\nA = ~p\nB = ~p\n", [A, B]),
    ct:fail("match failed").


%% Convert to fallbacks to bitstrings and export funs.
%% Does not support local funs with environment terms.
fallback(Binary) when is_binary(Binary) ->
    Binary;
fallback(BitStr) when is_bitstring(BitStr) ->
    TailBits = bit_size(BitStr) rem 8,
    PadBits = 8 - TailBits,
    {<<BitStr/bits, 0:PadBits>>, TailBits};
fallback(Fun) when is_function(Fun) ->
    FI = erlang:fun_info(Fun),
    {type,external} = lists:keyfind(type, 1, FI),
    {module, Mod} = lists:keyfind(module, 1, FI),
    {name, Func} = lists:keyfind(name, 1, FI),
    {Mod, Func};
fallback([H|T]) ->
    [fallback(H)|fallback(T)];
fallback(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(fallback(tuple_to_list(Tuple)));
fallback(Map) when is_map(Map) ->
    maps:from_list(fallback(maps:to_list(Map)));
fallback(Leaf) ->
    Leaf.

rand_term(MaxSize) ->
    F = rand:uniform(100), % to produce non-literals
    Big = 666_701_523_687_345_689_643 * F,
    MagicRef = atomics:new(10,[]),
    Leafs = {atom, 42, 42.17*F,
             Big, -Big,
             [], {}, #{},
             fun lists:sort/1,
             fun() -> ok end,
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
    rand_term(Leafs, rand:uniform(MaxSize)).

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

mk_ext_pid({NodeName, Creation}, Number, Serial) ->
    erts_test_utils:mk_ext_pid({NodeName, Creation}, Number, Serial).

mk_ext_port({NodeName, Creation}, Number) ->
    erts_test_utils:mk_ext_port({NodeName, Creation}, Number).

mk_ext_ref({NodeName, Creation}, Numbers) ->
    erts_test_utils:mk_ext_ref({NodeName, Creation}, Numbers).

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


ei_threaded_accept(Config) when is_list(Config) ->
    Einode = filename:join(proplists:get_value(data_dir, Config), "eiaccnode"),
    ei_threaded_accept_do(Einode, default),
    ei_threaded_accept_do(Einode, ussi),
    ok.

ei_threaded_accept_do(Einode, SockImpl) ->
    N = 3,
    wait_unreg_nodename(["eiacc0", "eiacc1", "eiacc2"], 10),
    start_einode(Einode, N, SockImpl),
    io:format("started eiaccnode"),
    TestServerPid = self(),
    [spawn_link(fun() -> send_rec_einode(I, TestServerPid) end) || I <- lists:seq(0, N-1)],
    [receive I -> ok end || I <- lists:seq(0, N-1) ],
    ok.


%% Test erlang:monitor toward erl_interface "processes"
monitor_ei_process(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, 0, default),

    Myname = hd(tl(string:tokens(atom_to_list(node()), "@"))),
    io:format("Myname ~p ~n",  [Myname]),
    EINode = list_to_atom("c42@"++Myname),
    io:format("EINode ~p ~n",  [EINode]),

    Port = 6543,
    {ok, ListenFd} = ei_publish(P, Port),
    MRef1 = erlang:monitor(process, {any, EINode}),
    {any, EINode} ! hello,

    {ok, Fd, _Node} = ei_accept(P, ListenFd),
    hello = ei_receive(P, Fd),

    %% Again, now on an established connection.
    MRef2 = erlang:monitor(process, {any, EINode}),
    {any, EINode} ! hello,
    hello = ei_receive(P, Fd),

    ok = receive M -> M after 0 -> ok end,

    runner:finish(P),

    ok  =receive
             {'DOWN', MRef1, process, {any, EINode}, noconnection} ->
                 ok
         after 1000 ->
                 timeout
         end,
    ok = receive
             {'DOWN', MRef2, process, {any, EINode}, noconnection} ->
                 ok
         after 1000 ->
                 timeout
         end,
    [] = flush(0, 1000),
    ok.

wait_unreg_nodename([], _) ->
    ok;
wait_unreg_nodename(Names, 0) ->
    ct:fail({name_not_unregistered, Names});
wait_unreg_nodename(Names, N) ->
    Registered = [X || {X,_} <- element(2,erl_epmd:names())],
    case lists:foldl(fun (Name, Acc) ->
                             case lists:member(Name, Registered) of
                                 true -> [Name | Acc];
                                 false -> Acc
                             end
                     end,
                     [],
                     Names) of
        [] ->
            ok;
        NewNames ->
            timer:sleep(1000),
            waitfornode(NewNames,N-1)
    end.

waitfornode(String,0) ->
    io:format("~s never published itself.~n",[String]),
    false;
waitfornode(String,N) ->
    Registered = [X || {X,_} <- element(2,erl_epmd:names())],
    case lists:member(String,Registered) of
        true ->
            true;
        false ->
            timer:sleep(1000),
            waitfornode(String,N-1)
    end.

send_rec_einode(N, TestServerPid) ->
    Myname= hd(tl(string:tokens(atom_to_list(node()), "@"))),
    FirstPart = "eiacc" ++ integer_to_list(N),
    EINode= list_to_atom(FirstPart ++ "@" ++ Myname),
    io:format("EINode ~p ~n",  [EINode]),
    Self= self(),
    case waitfornode(FirstPart,20) of
        true -> ok;
        false -> ct:fail({never_published,EINode})
    end,
    {any, EINode} ! Self,
    receive
        {N,_}=X ->
            io:format("Received by ~s ~p~n", [EINode, X]),
            TestServerPid ! N,
            X
    after 10000 ->
              ct:fail(EINode)
    end.

start_einode(Einode, N, SockImpl) ->
    Einodecmd = Einode ++ " " ++ atom_to_list(erlang:get_cookie())
        ++ " " ++ integer_to_list(N)
        ++ " " ++ atom_to_list(SockImpl),
    io:format("Einodecmd  ~p ~n", [Einodecmd]),      
    open_port({spawn, Einodecmd}, []),
    ok.


%%% Interface functions for ei (erl_interface) functions.

ei_connect_init(P, Num, Cookie, Creation, Compat, SockImpl) ->
    send_command(P, ei_connect_init, [Num,Cookie,Creation,Compat,SockImpl]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

ei_publish(P, PortNo) ->
    send_command(P, ei_publish, [PortNo]),
    case get_term(P) of
        {term,{ListenFd, EpmdFd, _}} when ListenFd >= 0, EpmdFd >= 0 -> {ok, ListenFd};
        {term,{_, _, Errno}} -> {error,Errno}
    end.

ei_accept(P, ListenFd) ->
    send_command(P, ei_accept, [ListenFd]),
    case get_term(P) of
        {term,{Fd, _, Node}} when Fd >= 0 -> {ok, Fd, Node};
        {term,{_Fd, Errno, _Node}} -> {error,Errno}
    end.

ei_receive(P, Fd) ->
    send_command(P, ei_receive, [Fd]),
    {term, T} = get_term(P),
    T.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).

flush(0, Timeout) ->
    flush(1, Timeout div 10);
flush(Expected, Timeout) ->
    receive M ->
            [M | flush(Expected-1, Timeout)]
    after Timeout ->
            []
    end.
