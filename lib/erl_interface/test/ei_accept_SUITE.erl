%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
         ei_accept/1, ei_threaded_accept/1]).

-import(runner, [get_term/1,send_term/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [ei_accept, ei_threaded_accept].

ei_accept(Config) when is_list(Config) ->
    P = runner:start(?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),

    Myname = hd(tl(string:tokens(atom_to_list(node()), "@"))),
    io:format("Myname ~p ~n",  [Myname]),
    EINode = list_to_atom("c42@"++Myname),
    io:format("EINode ~p ~n",  [EINode]),
    Self = self(),
    TermToSend= {call, Self, "Test"},
    F= fun() ->
               case waitfornode("c42",20) of
                   true ->
                       {any, EINode} ! TermToSend,
                       Self ! sent_ok;
                   false ->
                       Self ! never_published
               end,
               ok
       end,

    spawn(F),
    Port = 6543,
    {ok, Fd, _Node} = ei_accept(P, Port),
    TermReceived= ei_receive(P, Fd),
    io:format("Sent ~p received ~p ~n", [TermToSend, TermReceived]),
    TermToSend= TermReceived,
    receive
        sent_ok ->
            ok;
        Unknown ->
            io:format("~p ~n", [Unknown])
    after 1000 ->
              io:format("timeout ~n")
    end,
    runner:finish(P),
    ok.

ei_threaded_accept(Config) when is_list(Config) ->
    Einode = filename:join(proplists:get_value(data_dir, Config), "eiaccnode"),
    N = 1, % 3,
    Host = atom_to_list(node()),
    Port = 6767,
    start_einode(Einode, N, Host, Port),
    io:format("started eiaccnode"),
    %%spawn_link(fun() -> start_einode(Einode, N, Host, Port) end),
    TestServerPid = self(),
    [spawn_link(fun() -> send_rec_einode(I, TestServerPid) end) || I <- lists:seq(0, N-1)],
    [receive I -> ok end || I <- lists:seq(0, N-1) ],
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

start_einode(Einode, N, Host, Port) ->
    Einodecmd = Einode ++ " " ++ atom_to_list(erlang:get_cookie())
    ++ " " ++ integer_to_list(N) ++ " " ++ Host ++ " "
    ++ integer_to_list(Port) ++ " nothreads",
    io:format("Einodecmd  ~p ~n", [Einodecmd]),      
    open_port({spawn, Einodecmd}, []),
    ok.


%%% Interface functions for ei (erl_interface) functions.

ei_connect_init(P, Num, Cookie, Creation) ->
    send_command(P, ei_connect_init, [Num,Cookie,Creation]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

ei_accept(P, PortNo) ->
    send_command(P, ei_accept, [PortNo]),
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
