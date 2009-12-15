%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2009. All Rights Reserved.
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

%%
-module(ei_accept_SUITE).

-include("test_server.hrl").
-include("ei_accept_SUITE_data/ei_accept_test_cases.hrl").

-export([all/1, init_per_testcase/2, fin_per_testcase/2,
	 ei_accept/1, ei_threaded_accept/1]).

-import(runner, [get_term/1,send_term/2]).

all(suite) -> [ei_accept, ei_threaded_accept].

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(0.25)),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

ei_accept(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),

%    ?line AMsg={a,[message, with], " strings in it!", [-12, -23], 1.001},
    %% shouldn't this be a bif or function or something?
    ?line Myname= hd(tl(string:tokens(atom_to_list(node()), "@"))),
    ?line io:format("Myname ~p ~n",  [Myname]),
    ?line EINode= list_to_atom("c42@"++Myname),
    ?line io:format("EINode ~p ~n",  [EINode]),
    ?line Self= self(),
    ?line TermToSend= {call, Self, "Test"},
    ?line F= fun() ->
		     timer:sleep(500),
		     {any, EINode} ! TermToSend,
		     Self ! sent_ok,
		     ok
	     end,

    ?line spawn(F),
    ?line Port  = 6543,
    ?line {ok, Fd, _Node} = ei_accept(P, Port),
    ?line TermReceived= ei_receive(P, Fd),
    ?line io:format("Sent ~p received ~p ~n", [TermToSend, TermReceived]),
    ?line TermToSend= TermReceived,
    ?line receive
	      sent_ok ->
		  ok;
	      Unknown ->
		  io:format("~p ~n", [Unknown])
	  after 1000 ->
		  io:format("timeout ~n")
	  end,
    ?line ok= ei_unpublish(P),
    ok.

ei_threaded_accept(Config) when is_list(Config) ->
    ?line Einode = filename:join(?config(data_dir, Config), "eiaccnode"),
    ?line N = 1, % 3,
    ?line Host = atom_to_list(node()),
    ?line Port = 6767,
    ?line start_einode(Einode, N, Host, Port),
    ?line io:format("started eiaccnode"),
    %%?line spawn_link(fun() -> start_einode(Einode, N, Host, Port) end),
    ?line TestServerPid = self(),
    ?line [ spawn_link(fun() -> send_rec_einode(I, TestServerPid) end)
	    || I <- lists:seq(0, N-1) ],
    ?line [ receive I -> ok end
	    || I <- lists:seq(0, N-1) ],
    ok.

send_rec_einode(N, TestServerPid) ->
    ?line Myname= hd(tl(string:tokens(atom_to_list(node()), "@"))),
    ?line EINode= list_to_atom("eiacc" ++ integer_to_list(N) ++ "@" ++ Myname),
    ?line io:format("EINode ~p ~n",  [EINode]),
    ?line Self= self(),
    ?line timer:sleep(10*1000),
    ?line {any, EINode} ! Self,
    ?line receive
	      {N,_}=X ->
		  ?line io:format("Received by ~s ~p~n", [EINode, X]),
		  ?line TestServerPid ! N,
		  ?line X
	  after 10000 ->
		  ?line test_server:fail(EINode)
	  end.

start_einode(Einode, N, Host, Port) ->
    Einodecmd = Einode ++ " " ++ atom_to_list(erlang:get_cookie())
	++ " " ++ integer_to_list(N) ++ " " ++ Host ++ " "
	++ integer_to_list(Port) ++ " nothreads",
    io:format("Einodecmd  ~p ~n", [Einodecmd]),      
    ?line open_port({spawn, Einodecmd}, []),
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
    {term, T}= get_term(P),
    T.

ei_unpublish(P) ->
    send_command(P, ei_unpublish, []),
    case get_term(P) of
	{term,{0, _}} -> ok;
	{term,{_X, Errno}} -> {error,Errno}
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).

    


