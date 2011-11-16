%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2011. All Rights Reserved.
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
-module(gen_tcp_api_SUITE).

%% Tests the documented API for the gen_tcp functions.  The "normal" cases
%% are not tested here, because they are tested indirectly in this and
%% and other test suites.

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet.hrl").
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,
	 t_connect_timeout/1, t_accept_timeout/1,
	 t_connect_bad/1,
	 t_recv_timeout/1, t_recv_eof/1,
	 t_shutdown_write/1, t_shutdown_both/1, t_shutdown_error/1,
	 t_fdopen/1, t_implicit_inet6/1,
	 t_sendfile_small/1, t_sendfile_big/1,
	 t_sendfile_hdtl/1, t_sendfile_partial/1,
	 t_sendfile_offset/1]).

-export([sendfile_server/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group, t_accept}, {group, t_connect}, {group, t_recv},
     t_shutdown_write, t_shutdown_both, t_shutdown_error,
     t_fdopen, t_implicit_inet6,{group,t_sendfile}].

groups() -> 
    [{t_accept, [], [t_accept_timeout]},
     {t_connect, [], [t_connect_timeout, t_connect_bad]},
     {t_recv, [], [t_recv_timeout, t_recv_eof]},
     {t_sendfile, [], [{group, sendfile_all()}]}].

sendfile_all() ->
    [
     t_sendfile_small
     ,t_sendfile_big
%     ,t_sendfile_hdtl
     ,t_sendfile_partial
     ,t_sendfile_offset
    ].
%    [t_sendfile_big].
%    [t_sendfile_small].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(t_sendfile, Config) ->
    Priv = ?config(priv_dir, Config),
    SFilename = filename:join(Priv, "sendfile_small.html"),
    {ok, DS} = file:open(SFilename,[write,raw]),
    file:write(DS,"yo baby yo"),
    file:sync(DS),
    file:close(DS),
    BFilename = filename:join(Priv, "sendfile_big.html"),
    {ok, DB} = file:open(BFilename,[write,raw]),
    [file:write(DB,[<<0:(10*8*1024*1024)>>]) || _I <- lists:seq(1,51)],
    file:sync(DB),
    file:close(DB),
    [{small_file, SFilename},
     {file_opts,[raw,binary]},
     {big_file, BFilename}|Config];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(t_sendfile, Config) ->
    file:delete(proplists:get_value(big_file, Config));
end_per_group(_,_Config) ->
    ok.



init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:seconds(60)),
    [{watchdog, Dog}|Config].
end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).

%%% gen_tcp:accept/1,2


t_accept_timeout(doc) -> "Test that gen_tcp:accept/2 (with timeout) works.";
t_accept_timeout(suite) -> [];
t_accept_timeout(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line timeout({gen_tcp, accept, [L, 200]}, 0.2, 1.0).

%%% gen_tcp:connect/X


t_connect_timeout(doc) -> "Test that gen_tcp:connect/4 (with timeout) works.";
t_connect_timeout(Config) when is_list(Config) ->
    %%?line BadAddr = {134,138,177,16},
    %%?line TcpPort = 80,
    ?line {ok, BadAddr} =  unused_ip(),
    ?line TcpPort = 45638,
    ?line ok = io:format("Connecting to ~p, port ~p", [BadAddr, TcpPort]),
    ?line connect_timeout({gen_tcp,connect,[BadAddr,TcpPort,[],200]}, 0.2, 5.0).

t_connect_bad(doc) ->
    ["Test that gen_tcp:connect/3 handles non-existings hosts, and other ",
     "invalid things."];
t_connect_bad(suite) -> [];
t_connect_bad(Config) when is_list(Config) ->
    ?line NonExistingPort = 45638,		% Not in use, I hope.
    ?line {error, Reason1} = gen_tcp:connect(localhost, NonExistingPort, []),
    ?line io:format("Error for connection attempt to port not in use: ~p",
		    [Reason1]),

    ?line {error, Reason2} = gen_tcp:connect("non-existing-host-xxx", 7, []),
    ?line io:format("Error for connection attempt to non-existing host: ~p",
		    [Reason2]),
    ok.


%%% gen_tcp:recv/X


t_recv_timeout(doc) -> "Test that gen_tcp:recv/3 (with timeout works).";
t_recv_timeout(suite) -> [];
t_recv_timeout(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line {ok, Port} = inet:port(L),
    ?line {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    ?line {ok, _A} = gen_tcp:accept(L),
    ?line timeout({gen_tcp, recv, [Client, 0, 200]}, 0.2, 5.0).

t_recv_eof(doc) -> "Test that end of file on a socket is reported correctly.";
t_recv_eof(suite) -> [];
t_recv_eof(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line {ok, Port} = inet:port(L),
    ?line {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    ?line {ok, A} = gen_tcp:accept(L),
    ?line ok = gen_tcp:close(A),
    ?line {error, closed} = gen_tcp:recv(Client, 0),
    ok.

%%% gen_tcp:shutdown/2

t_shutdown_write(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line {ok, Port} = inet:port(L),
    ?line {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    ?line {ok, A} = gen_tcp:accept(L),
    ?line ok = gen_tcp:shutdown(A, write),
    ?line {error, closed} = gen_tcp:recv(Client, 0),
    ok.

t_shutdown_both(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line {ok, Port} = inet:port(L),
    ?line {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    ?line {ok, A} = gen_tcp:accept(L),
    ?line ok = gen_tcp:shutdown(A, read_write),
    ?line {error, closed} = gen_tcp:recv(Client, 0),
    ok.

t_shutdown_error(Config) when is_list(Config) ->
    ?line {ok, L} = gen_tcp:listen(0, []),
    ?line {error, enotconn} = gen_tcp:shutdown(L, read_write),
    ?line ok = gen_tcp:close(L),
    ?line {error, closed} = gen_tcp:shutdown(L, read_write),
    ok.
    

%%% gen_tcp:fdopen/2

t_fdopen(Config) when is_list(Config) ->
    ?line Question = "Aaaa... Long time ago in a small town in Germany,",
    ?line Question1 = list_to_binary(Question),
    ?line Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
                       ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    ?line Question1 = iolist_to_binary(Question2),
    ?line Answer = "there was a shoemaker, Schumacher was his name.",
    ?line {ok, L} = gen_tcp:listen(0, [{active, false}]),
    ?line {ok, Port} = inet:port(L),
    ?line {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    ?line {ok, A} = gen_tcp:accept(L),
    ?line {ok, FD} = prim_inet:getfd(A),
    ?line {ok, Server} = gen_tcp:fdopen(FD, []),
    ?line ok = gen_tcp:send(Client, Question),
    ?line {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ?line ok = gen_tcp:send(Client, Question1),
    ?line {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ?line ok = gen_tcp:send(Client, Question2),
    ?line {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ?line ok = gen_tcp:send(Server, Answer),
    ?line {ok, Answer} = gen_tcp:recv(Client, length(Answer), 2000),
    ?line ok = gen_tcp:close(Client),
    ?line {error,closed} = gen_tcp:recv(A, 1, 2000),
    ?line ok = gen_tcp:close(Server),
    ?line ok = gen_tcp:close(A),
    ?line ok = gen_tcp:close(L),
    ok.


%%% implicit inet6 option to api functions

t_implicit_inet6(Config) when is_list(Config) ->
    ?line Host = ok(inet:gethostname()),
    ?line
	case inet:getaddr(Host, inet6) of
	    {ok,Addr} ->
		?line t_implicit_inet6(Host, Addr);
	    {error,Reason} ->
		{skip,
		 "Can not look up IPv6 address: "
		 ++atom_to_list(Reason)}
	end.

t_implicit_inet6(Host, Addr) ->
    ?line
	case gen_tcp:listen(0, [inet6]) of
	    {ok,S1} ->
		?line Loopback = {0,0,0,0,0,0,0,1},
		?line io:format("~s ~p~n", ["::1",Loopback]),
		?line implicit_inet6(S1, Loopback),
		?line ok = gen_tcp:close(S1),
		%%
		?line Localhost = "localhost",
		?line Localaddr = ok(inet:getaddr(Localhost, inet6)),
		?line io:format("~s ~p~n", [Localhost,Localaddr]),
		?line S2 = ok(gen_tcp:listen(0, [{ip,Localaddr}])),
		?line implicit_inet6(S2, Localaddr),
		?line ok = gen_tcp:close(S2),
		%%
		?line io:format("~s ~p~n", [Host,Addr]),
		?line S3 = ok(gen_tcp:listen(0, [{ifaddr,Addr}])),
		?line implicit_inet6(S3, Addr),
		?line ok = gen_tcp:close(S3);
	    {error,_} ->
		{skip,"IPv6 not supported"}
	end.

implicit_inet6(S, Addr) ->
    ?line P = ok(inet:port(S)),
    ?line S2 = ok(gen_tcp:connect(Addr, P, [])),
    ?line P2 = ok(inet:port(S2)),
    ?line S1 = ok(gen_tcp:accept(S)),
    ?line P1 = P = ok(inet:port(S1)),
    ?line {Addr,P2} = ok(inet:peername(S1)),
    ?line {Addr,P1} = ok(inet:peername(S2)),
    ?line {Addr,P1} = ok(inet:sockname(S1)),
    ?line {Addr,P2} = ok(inet:sockname(S2)),
    ?line ok = gen_tcp:close(S2),
    ?line ok = gen_tcp:close(S1).

t_sendfile_small(Config) when is_list(Config) ->
    Filename = proplists:get_value(small_file, Config),

    Send = fun(Sock) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok, Size} = gen_tcp:sendfile(Filename, Sock),
		   Data
	   end,

    ok = sendfile_send(Send).

t_sendfile_big(Config) when is_list(Config) ->
    Filename = proplists:get_value(big_file, Config),

    Send = fun(Sock) ->
		   {ok, #file_info{size = Size}} =
		       file:read_file_info(Filename),
		   {ok, Size} = gen_tcp:sendfile(Filename, Sock),
		   Size
	   end,

    ok = sendfile_send("localhost", Send, 0).

t_sendfile_partial(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),

    SendSingle = fun(Sock) ->
			 {_Size, <<Data:5/binary,_/binary>>} =
			     sendfile_file_info(Filename),
			 {ok,D} = file:open(Filename,[read|FileOpts]),
			 {ok,5} = gen_tcp:sendfile(D,Sock,0,5,[]),
			 file:close(D),
			 Data
		 end,
    ok = sendfile_send(SendSingle),

    {_Size, <<FData:5/binary,SData:3/binary,_/binary>>} =
	sendfile_file_info(Filename),
    {ok,D} = file:open(Filename,[read|FileOpts]),
    {ok, <<FData/binary>>} = file:read(D,5),
    FSend = fun(Sock) ->
		    {ok,5} = gen_tcp:sendfile(D,Sock,0,5,[]),
		    FData
	    end,

    ok = sendfile_send(FSend),

    SSend = fun(Sock) ->
		    {ok,3} = gen_tcp:sendfile(D,Sock,5,3,[]),
		    SData
	    end,

    ok = sendfile_send(SSend),

    {ok, <<SData/binary>>} = file:read(D,3),

    file:close(D).

t_sendfile_offset(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),

    Send = fun(Sock) ->
		   {_Size, <<_:5/binary,Data:3/binary,_/binary>> = AllData} =
		       sendfile_file_info(Filename),
		   {ok,D} = file:open(Filename,[read|FileOpts]),
		   {ok,3} = gen_tcp:sendfile(D,Sock,5,3,[]),
		   {ok, AllData} = file:read(D,100),
		   file:close(D),
		   Data
	   end,
    ok = sendfile_send(Send).


t_sendfile_hdtl(Config) ->
    Filename = proplists:get_value(small_file, Config),
    FileOpts = proplists:get_value(file_opts, Config, []),

    Send = fun(Sock, Headers, Trailers, HdtlSize) ->
		   {Size, Data} = sendfile_file_info(Filename),
		   {ok,D} = file:open(Filename,[read|FileOpts]),
		   AllSize = Size+HdtlSize,
		   {ok, AllSize} = gen_tcp:sendfile(
				     D, Sock,0,0,
				     [{headers,Headers},
				      {trailers,Trailers}]),
		   file:close(D),
		   Data
	   end,

    SendHdTl = fun(Sock) ->
		       Headers = [<<"header1">>,"header2"],
		       Trailers = [<<"trailer1">>,"trailer2"],
		       D = Send(Sock,Headers,Trailers,
			    iolist_size([Headers,Trailers])),
		       iolist_to_binary([Headers,D,Trailers])
	       end,
    ok = sendfile_send(SendHdTl),

    SendHd = fun(Sock) ->
		       Headers = [<<"header1">>,"header2"],
		       D = Send(Sock,Headers,undefined,
			    iolist_size([Headers])),
		       iolist_to_binary([Headers,D])
	       end,
    ok = sendfile_send(SendHd),

    SendTl = fun(Sock) ->
		       Trailers = [<<"trailer1">>,"trailer2"],
		       D = Send(Sock,undefined,Trailers,
			    iolist_size([Trailers])),
		       iolist_to_binary([D,Trailers])
	       end,
    ok = sendfile_send(SendTl).


%% TODO: consolidate tests and reduce code
sendfile_send(Send) ->
    sendfile_send("localhost",Send).
sendfile_send(Host, Send) ->
    sendfile_send(Host, Send, []).
sendfile_send(Host, Send, Orig) ->
    spawn_link(?MODULE, sendfile_server, [self(), Orig]),
    receive
	{server, Port} ->
	    {ok, Sock} = gen_tcp:connect(Host, Port,
					       [binary,{packet,0}]),
	    Data = Send(Sock),
	    ok = gen_tcp:close(Sock),
	    receive
		{ok, Bin} ->
		    Data = Bin,
		    ok
	    end
    end.

sendfile_server(ClientPid, Orig) ->
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, 0},
					   {active, false},
					   {reuseaddr, true}]),
    {ok, Port} = inet:port(LSock),
    ClientPid ! {server, Port},
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = sendfile_do_recv(Sock, Orig),
    ok = gen_tcp:close(Sock),
    ClientPid ! {ok, Bin}.

-define(SENDFILE_TIMEOUT, 10000).
%% f(),{ok, S} = gen_tcp:connect("localhost",7890,[binary]),gen_tcp:sendfile("/ldisk/lukas/otp/sendfiletest.dat",S).
sendfile_do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0, ?SENDFILE_TIMEOUT) of
	{ok, B} when is_list(Bs) ->
	    sendfile_do_recv(Sock, [B|Bs]);
	{error, closed} when is_list(Bs) ->
	    {ok, iolist_to_binary(lists:reverse(Bs))};
	{ok, B} when is_integer(Bs) ->
	    sendfile_do_recv(Sock, byte_size(B) + Bs);
	{error, closed} when is_integer(Bs) ->
	    {ok, Bs}
    end.

sendfile_file_info(File) ->
    {ok, #file_info{size = Size}} = file:read_file_info(File),
    {ok, Data} = file:read_file(File),
    {Size, Data}.


%%% Utilities

%% Calls M:F/length(A), which should return a timeout error, and complete
%% within the given time.

timeout({M,F,A}, Lower, Upper) ->
    case test_server:timecall(M, F, A) of
	{Time, Result} when Time < Lower ->
	    test_server:fail({too_short_time, Time, Result});
	{Time, Result} when Time > Upper ->
	    test_server:fail({too_long_time, Time, Result});
	{_, {error, timeout}} ->
	    ok;
	{_, Result} ->
	    test_server:fail({unexpected_result, Result})
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
		    test_server:fail({too_short_time, Time, 
				      [Result,Pinfo,Db,Peer]});
		_ ->
		    test_server:fail({too_short_time, Time, Result})
	    end;
	{Time, Result} when Time > Upper ->
	    test_server:fail({too_long_time, Time, Result});
	{_, {error, timeout}} ->
	    ok;
	{_, Result} ->
	    test_server:fail({unexpected_result, Result})
    end.

%% Try to obtain an unused IP address in the local network.

unused_ip() ->
    ?line {ok, Host} = inet:gethostname(),
    ?line {ok, Hent} = inet:gethostbyname(Host),
    ?line #hostent{h_addr_list=[{A, B, C, _D}|_]} = Hent,
    %% Note: In our net, addresses below 16 are reserved for routers and
    %% other strange creatures.
    ?line IP = unused_ip(A, B, C, 16),
    io:format("we = ~p, unused_ip = ~p~n", [Hent, IP]),
    IP.

unused_ip(_, _, _, 255) -> error;
unused_ip(A, B, C, D) ->
    case inet:gethostbyaddr({A, B, C, D}) of
	{ok, _} -> unused_ip(A, B, C, D+1);
	{error, _} -> {ok, {A, B, C, D}}
    end.

ok({ok,V}) -> V.
