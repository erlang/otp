%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2010. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").
-include("ei_accept_SUITE_data/ei_accept_test_cases.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 ei_accept/1, ei_threaded_accept/1]).

-import(runner, [get_term/1,send_term/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [ei_accept, ei_threaded_accept].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:seconds(30)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

ei_accept(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 0 = ei_connect_init(P, 42, erlang:get_cookie(), 0),

    ?line Myname= hd(tl(string:tokens(atom_to_list(node()), "@"))),
    ?line io:format("Myname ~p ~n",  [Myname]),
    ?line EINode= list_to_atom("c42@"++Myname),
    ?line io:format("EINode ~p ~n",  [EINode]),
    ?line Self= self(),
    ?line TermToSend= {call, Self, "Test"},
    ?line F= fun() ->
		     case waitfornode("c42",20) of
			 true ->
			     {any, EINode} ! TermToSend,
			     Self ! sent_ok;
			 false ->
			     Self ! never_published
		     end,
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
    ?line runner:finish(P),
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
    ?line Myname= hd(tl(string:tokens(atom_to_list(node()), "@"))),
    ?line FirstPart = "eiacc" ++ integer_to_list(N),
    ?line EINode= list_to_atom(FirstPart ++ "@" ++ Myname),
    ?line io:format("EINode ~p ~n",  [EINode]),
    ?line Self= self(),
    ?line case waitfornode(FirstPart,20) of
	      true -> ok;
	      false -> test_server:fail({never_published,EINode})
	  end,
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

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).

    


