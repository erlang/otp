%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(gen_tcp_api_SUITE).

%% Tests the documented API for the gen_tcp functions.  The "normal" cases
%% are not tested here, because they are tested indirectly in this and
%% and other test suites.

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 t_connect_timeout/1, t_accept_timeout/1,
	 t_connect_bad/1,
	 t_recv_timeout/1, t_recv_eof/1, t_recv_delim/1,
	 t_shutdown_write/1, t_shutdown_both/1, t_shutdown_error/1,
	 t_shutdown_async/1,
	 t_fdopen/1, t_fdconnect/1, t_implicit_inet6/1]).

-export([getsockfd/0,closesockfd/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [{group, t_accept}, {group, t_connect}, {group, t_recv},
     t_shutdown_write, t_shutdown_both, t_shutdown_error,
     t_shutdown_async, t_fdopen, t_fdconnect, t_implicit_inet6].

groups() -> 
    [{t_accept, [], [t_accept_timeout]},
     {t_connect, [], [t_connect_timeout, t_connect_bad]},
     {t_recv, [], [t_recv_timeout, t_recv_eof, t_recv_delim]}].



init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_,_Config) ->
    ok.

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

%%% gen_tcp:accept/1,2


%% Test that gen_tcp:accept/2 (with timeout) works.
t_accept_timeout(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    timeout({gen_tcp, accept, [L, 200]}, 0.2, 1.0).

%%% gen_tcp:connect/X


%% Test that gen_tcp:connect/4 (with timeout) works.
t_connect_timeout(Config) when is_list(Config) ->
    %%BadAddr = {134,138,177,16},
    %%TcpPort = 80,
    {ok, BadAddr} =  unused_ip(),
    TcpPort = 45638,
    ok = io:format("Connecting to ~p, port ~p", [BadAddr, TcpPort]),
    connect_timeout({gen_tcp,connect,[BadAddr,TcpPort,[],200]}, 0.2, 5.0).

%% Test that gen_tcp:connect/3 handles non-existings hosts, and other
%% invalid things.
t_connect_bad(Config) when is_list(Config) ->
    NonExistingPort = 45638,		% Not in use, I hope.
    {error, Reason1} = gen_tcp:connect(localhost, NonExistingPort, []),
    io:format("Error for connection attempt to port not in use: ~p",
	      [Reason1]),

    {error, Reason2} = gen_tcp:connect("non-existing-host-xxx", 7, []),
    io:format("Error for connection attempt to non-existing host: ~p",
	      [Reason2]),
    ok.


%%% gen_tcp:recv/X


%% Test that gen_tcp:recv/3 (with timeout works).
t_recv_timeout(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, _A} = gen_tcp:accept(L),
    timeout({gen_tcp, recv, [Client, 0, 200]}, 0.2, 5.0).

%% Test that end of file on a socket is reported correctly.
t_recv_eof(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, A} = gen_tcp:accept(L),
    ok = gen_tcp:close(A),
    {error, closed} = gen_tcp:recv(Client, 0),
    ok.

%% Test using message delimiter $X.
t_recv_delim(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    Opts = [{active,false},{packet,line},{line_delimiter,$X}],
    {ok, Client} = gen_tcp:connect(localhost, Port, Opts),
    {ok, A} = gen_tcp:accept(L),
    ok = gen_tcp:send(A, "abcXefgX"),
    {ok, "abcX"} = gen_tcp:recv(Client, 0, 0),
    {ok, "efgX"} = gen_tcp:recv(Client, 0, 0),
    ok = gen_tcp:close(Client),
    ok = gen_tcp:close(A),
    ok.

%%% gen_tcp:shutdown/2

t_shutdown_write(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, A} = gen_tcp:accept(L),
    ok = gen_tcp:shutdown(A, write),
    {error, closed} = gen_tcp:recv(Client, 0),
    ok.

t_shutdown_both(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, A} = gen_tcp:accept(L),
    ok = gen_tcp:shutdown(A, read_write),
    {error, closed} = gen_tcp:recv(Client, 0),
    ok.

t_shutdown_error(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, []),
    {error, enotconn} = gen_tcp:shutdown(L, read_write),
    ok = gen_tcp:close(L),
    {error, closed} = gen_tcp:shutdown(L, read_write),
    ok.

t_shutdown_async(Config) when is_list(Config) ->
    {OS, _} = os:type(),
    {ok, L} = gen_tcp:listen(0, [{sndbuf, 4096}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port,
				   [{recbuf, 4096},
				    {active, false}]),
    {ok, S} = gen_tcp:accept(L),
    PayloadSize = 1024 * 1024,
    Payload = lists:duplicate(PayloadSize, $.),
    ok = gen_tcp:send(S, Payload),
    case erlang:port_info(S, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail({unexpected, T})
    end,

    ok = gen_tcp:shutdown(S, write),
    {ok, Buf} = gen_tcp:recv(Client, PayloadSize),
    {error, closed} = gen_tcp:recv(Client, 0),
    case length(Buf) of
	PayloadSize -> ok;
	Sz -> ct:fail({payload_size,
		       {expected, PayloadSize},
		       {received, Sz}})
    end.


%%% gen_tcp:fdopen/2

t_fdopen(Config) when is_list(Config) ->
    Question = "Aaaa... Long time ago in a small town in Germany,",
    Question1 = list_to_binary(Question),
    Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
		 ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    Question1 = iolist_to_binary(Question2),
    Answer = "there was a shoemaker, Schumacher was his name.",
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, A} = gen_tcp:accept(L),
    {ok, FD} = prim_inet:getfd(A),
    {ok, Server} = gen_tcp:fdopen(FD, []),
    ok = gen_tcp:send(Client, Question),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question1),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question2),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Server, Answer),
    {ok, Answer} = gen_tcp:recv(Client, length(Answer), 2000),
    ok = gen_tcp:close(Client),
    {error,closed} = gen_tcp:recv(A, 1, 2000),
    ok = gen_tcp:close(Server),
    ok = gen_tcp:close(A),
    ok = gen_tcp:close(L),
    ok.

t_fdconnect(Config) when is_list(Config) ->
    Question = "Aaaa... Long time ago in a small town in Germany,",
    Question1 = list_to_binary(Question),
    Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
		 ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    Question1 = iolist_to_binary(Question2),
    Answer = "there was a shoemaker, Schumacher was his name.",
    Path = proplists:get_value(data_dir, Config),
    Lib = "gen_tcp_api_SUITE",
    ok = erlang:load_nif(filename:join(Path,Lib), []),
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    FD = gen_tcp_api_SUITE:getsockfd(),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{fd,FD},{port,20002},
                                                     {active,false}]),
    {ok, Server} = gen_tcp:accept(L),
    ok = gen_tcp:send(Client, Question),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question1),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question2),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Server, Answer),
    {ok, Answer} = gen_tcp:recv(Client, length(Answer), 2000),
    ok = gen_tcp:close(Client),
    FD = gen_tcp_api_SUITE:closesockfd(FD),
    {error,closed} = gen_tcp:recv(Server, 1, 2000),
    ok = gen_tcp:close(Server),
    ok = gen_tcp:close(L),
    ok.


%%% implicit inet6 option to api functions

t_implicit_inet6(Config) when is_list(Config) ->
    Host = ok(inet:gethostname()),
    case inet:getaddr(Host, inet6) of
	{ok,Addr} ->
	    t_implicit_inet6(Host, Addr);
	{error,Reason} ->
	    {skip,
	     "Can not look up IPv6 address: "
	     ++atom_to_list(Reason)}
    end.

t_implicit_inet6(Host, Addr) ->
    case gen_tcp:listen(0, [inet6]) of
	{ok,S1} ->
	    Loopback = {0,0,0,0,0,0,0,1},
	    io:format("~s ~p~n", ["::1",Loopback]),
	    implicit_inet6(S1, Loopback),
	    ok = gen_tcp:close(S1),
	    %%
	    Localhost = "localhost",
	    Localaddr = ok(inet:getaddr(Localhost, inet6)),
	    io:format("~s ~p~n", [Localhost,Localaddr]),
	    S2 = ok(gen_tcp:listen(0, [{ip,Localaddr}])),
	    implicit_inet6(S2, Localaddr),
	    ok = gen_tcp:close(S2),
	    %%
	    io:format("~s ~p~n", [Host,Addr]),
	    S3 = ok(gen_tcp:listen(0, [{ifaddr,Addr}])),
	    implicit_inet6(S3, Addr),
	    ok = gen_tcp:close(S3);
	{error,_} ->
	    {skip,"IPv6 not supported"}
    end.

implicit_inet6(S, Addr) ->
    P = ok(inet:port(S)),
    S2 = ok(gen_tcp:connect(Addr, P, [])),
    P2 = ok(inet:port(S2)),
    S1 = ok(gen_tcp:accept(S)),
    P1 = P = ok(inet:port(S1)),
    {Addr,P2} = ok(inet:peername(S1)),
    {Addr,P1} = ok(inet:peername(S2)),
    {Addr,P1} = ok(inet:sockname(S1)),
    {Addr,P2} = ok(inet:sockname(S2)),
    ok = gen_tcp:close(S2),
    ok = gen_tcp:close(S1).


%%% Utilities

%% Calls M:F/length(A), which should return a timeout error, and complete
%% within the given time.

timeout({M,F,A}, Lower, Upper) ->
    case test_server:timecall(M, F, A) of
	{Time, Result} when Time < Lower ->
	    ct:fail({too_short_time, Time, Result});
	{Time, Result} when Time > Upper ->
	    ct:fail({too_long_time, Time, Result});
	{_, {error, timeout}} ->
	    ok;
	{_, Result} ->
	    ct:fail({unexpected_result, Result})
    end.

connect_timeout({M,F,A}, Lower, Upper) ->
    case test_server:timecall(M, F, A) of
	{Time, Result} when Time < Lower ->
	    case Result of
		{error,econnrefused=E} ->
		    {comment,"Not tested -- got error "++atom_to_list(E)};
		{error,enetunreach=E} ->
		    {comment,"Not tested -- got error "++atom_to_list(E)};
		{ok,Socket} -> % What the...
		    Pinfo = erlang:port_info(Socket),
		    Db = inet_db:lookup_socket(Socket),
		    Peer = inet:peername(Socket),
		    ct:fail({too_short_time, Time,
			     [Result,Pinfo,Db,Peer]});
		_ ->
		    ct:fail({too_short_time, Time, Result})
	    end;
	{Time, Result} when Time > Upper ->
	    ct:fail({too_long_time, Time, Result});
	{_, {error, timeout}} ->
	    ok;
	{_, Result} ->
	    ct:fail({unexpected_result, Result})
    end.

%% Try to obtain an unused IP address in the local network.

unused_ip() ->
    {ok, Host} = inet:gethostname(),
    {ok, Hent} = inet:gethostbyname(Host),
    #hostent{h_addr_list=[{A, B, C, _D}|_]} = Hent,
    %% Note: In our net, addresses below 16 are reserved for routers and
    %% other strange creatures.
    IP = unused_ip(A, B, C, 16),
    io:format("we = ~p, unused_ip = ~p~n", [Hent, IP]),
    IP.

unused_ip(_, _, _, 255) -> error;
unused_ip(A, B, C, D) ->
    case inet:gethostbyaddr({A, B, C, D}) of
	{ok, _} -> unused_ip(A, B, C, D+1);
	{error, _} -> {ok, {A, B, C, D}}
    end.

ok({ok,V}) -> V.


getsockfd() -> undefined.
closesockfd(_FD) -> undefined.
