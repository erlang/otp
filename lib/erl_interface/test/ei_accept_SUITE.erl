%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2018. All Rights Reserved.
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
         ei_accept/1, ei_threaded_accept/1,
         monitor_ei_process/1]).

-import(runner, [get_term/1,send_term/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [ei_accept, ei_threaded_accept,
     monitor_ei_process].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

ei_accept(Config) when is_list(Config) ->
    ei_accept_do(Config, 0),   % default
    ei_accept_do(Config, 21).  % ei_set_compat_rel

ei_accept_do(Config, CompatRel) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, CompatRel),

    Myname = hd(tl(string:tokens(atom_to_list(node()), "@"))),
    io:format("Myname ~p ~n",  [Myname]),
    EINode = list_to_atom("c42@"++Myname),
    io:format("EINode ~p ~n",  [EINode]),

    %% We take this opportunity to also test export-funs and bit-strings
    %% with (ugly) tuple fallbacks in OTP 21 and older.
    %% Test both toward pending connection and established connection.
    RealTerms = [<<1:1>>,     fun lists:map/2],
    EncTerms = case CompatRel of
                   0 -> RealTerms;
                   21 -> [{<<128>>,1}, {lists,map}]
                end,

    Self = self(),
    Funny = fun() -> hello end,
    TermToSend = {call, Self, "Test", Funny, RealTerms},
    TermToGet  = {call, Self, "Test", Funny, EncTerms},
    Port = 6543,
    {ok, ListenFd} = ei_publish(P, Port),
    {any, EINode} ! TermToSend,

    {ok, Fd, _Node} = ei_accept(P, ListenFd),
    Got1 = ei_receive(P, Fd),

    %% Send again, now without auto-connect
    {any, EINode} ! TermToSend,
    Got2 = ei_receive(P, Fd),

    io:format("Sent ~p~nExp. ~p~nGot1 ~p~nGot2 ~p~n", [TermToSend, TermToGet, Got1, Got2]),
    TermToGet = Got1,
    TermToGet = Got2,

    runner:finish(P),
    ok.

ei_threaded_accept(Config) when is_list(Config) ->
    Einode = filename:join(proplists:get_value(data_dir, Config), "eiaccnode"),
    N = 3,
    Host = atom_to_list(node()),
    start_einode(Einode, N, Host),
    io:format("started eiaccnode"),
    TestServerPid = self(),
    [spawn_link(fun() -> send_rec_einode(I, TestServerPid) end) || I <- lists:seq(0, N-1)],
    [receive I -> ok end || I <- lists:seq(0, N-1) ],
    ok.


%% Test erlang:monitor toward erl_interface "processes"
monitor_ei_process(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, 0),

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

    [{'DOWN', MRef1, process, {any, EINode}, noconnection},
     {'DOWN', MRef2, process, {any, EINode}, noconnection}
    ] = lists:sort(flush(2, 1000)),

    ok.

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

start_einode(Einode, N, Host) ->
    Einodecmd = Einode ++ " " ++ atom_to_list(erlang:get_cookie())
    ++ " " ++ integer_to_list(N) ++ " " ++ Host,
    io:format("Einodecmd  ~p ~n", [Einodecmd]),      
    open_port({spawn, Einodecmd}, []),
    ok.


%%% Interface functions for ei (erl_interface) functions.

ei_connect_init(P, Num, Cookie, Creation, Compat) ->
    send_command(P, ei_connect_init, [Num,Cookie,Creation,Compat]),
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
