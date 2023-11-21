%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2023. All Rights Reserved.
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
%% Test the behavior of gen_udp. Testing udp is really a very unfunny task,
%% because udp is not deterministic.
%%
%% (cd /mnt/c/$LOCAL_TESTS/26/kernel_test/ && $ERL_TOP/bin/win32/erl.exe -sname kernel-26-tester -pa c:$LOCAL_TESTS/26/test_server)
%% application:set_env(kernel, test_inet_backends, true).
%% S = fun() -> ts:run(kernel, gen_udp_SUITE, [batch]) end.
%% S = fun(SUITE) -> ts:run(kernel, SUITE, [batch]) end.
%% S = fun() -> ct:run_test([{suite, gen_udp_SUITE}]) end.
%% S = fun(SUITE) -> ct:run_test([{suite, SUITE}]) end.
%% G = fun(GROUP) -> ts:run(kernel, gen_udp_SUITE, {group, GROUP}, [batch]) end.
%% G = fun(SUITE, GROUP) -> ts:run(kernel, SUITE, {group, GROUP}, [batch]) end.
%% G = fun(GROUP) -> ct:run_test([{suite, gen_udp_SUITE}, {group, GROUP}]) end.
%% G = fun(SUITE, GROUP) -> ct:run_test([{suite, SUITE}, {group, GROUP}]) end.
%% T = fun(TC) -> ts:run(kernel, gen_udp_SUITE, TC, [batch]) end.
%% T = fun(TC) -> ct:run_test([{suite, gen_udp_SUITE}, {testcase, TC}]) end.
%% T = fun(S, TC) -> ct:run_test([{suite, S}, {testcase, TC}]) end.
%% T = fun(S, G, TC) -> ct:run_test([{suite, S}, {group, G}, {testcase, TC}]) end.
%%

-module(gen_udp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("kernel_test_lib.hrl").


%% XXX - we should pick a port that we _know_ is closed. That's pretty hard.
-define(CLOSED_PORT, 6666).

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
	 send_to_closed/1, send_to_empty/1, active_n/1,
	 buffer_size/1, binary_passive_recv/1, max_buffer_size/1, bad_address/1,
	 read_packets/1, recv_poll_after_active_once/1,
         open_fd/1, connect/1, reconnect/1, implicit_inet6/1,
         recvtos/1, recvtosttl/1, recvttl/1, recvtclass/1,
         sendtos/1, sendtosttl/1, sendttl/1, sendtclass/1,
	 local_basic/1, local_basic_binary/1, local_unbound/1,
	 local_fdopen/1, local_fdopen_unbound/1, local_abstract/1,
         recv_close/1,
	 socket_monitor1/1,
	 socket_monitor1_manys/1,
	 socket_monitor1_manyc/1,
	 socket_monitor1_demon_after/1,
	 socket_monitor2/1,
	 socket_monitor2_manys/1,
	 socket_monitor2_manyc/1,
	 otp_17492/1,

         t_simple_local_sockaddr_in_send_recv/1,
         t_simple_link_local_sockaddr_in_send_recv/1,
         t_simple_local_sockaddr_in6_send_recv/1,
         t_simple_link_local_sockaddr_in6_send_recv/1,

         otp_18323_opts_processing/1,
         otp_18323_open/1

	]).

-include_lib("kernel/src/inet_int.hrl").

-define(TRY_TC(F), try_tc(F)).
               
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    %% This is a temporary measure to ensure that we can 
    %% test the socket backend without effecting *all*
    %% applications on *all* machines.
    %% This flag is set only for *one* host.
    case ?TEST_INET_BACKENDS() of
        true ->
            [
             {group, inet_backend_default},
             {group, inet_backend_inet},
             {group, inet_backend_socket}
            ];
        _ ->
            [
             {group, inet_backend_default}
            ]
    end.


groups() -> 
    [
     {inet_backend_default,   [], inet_backend_default_cases()},
     {inet_backend_inet,      [], inet_backend_inet_cases()},
     {inet_backend_socket,    [], inet_backend_socket_cases()},

     {recv_and_send_opts,     [], recv_and_send_opts_cases()},
     {local,                  [], local_cases()},
     {socket_monitor,         [], socket_monitor_cases()},

     {sockaddr,               [], sockaddr_cases()},
     {otp18323,               [], otp18323_cases()}
    ].

inet_backend_default_cases() ->
    all_cases().

inet_backend_inet_cases() ->
    all_cases().

inet_backend_socket_cases() ->
    all_cases().

all_cases() ->
    [
     send_to_closed,
     send_to_empty,
     buffer_size,
     binary_passive_recv,
     max_buffer_size,
     bad_address,
     read_packets,
     recv_poll_after_active_once,
     open_fd,
     connect,
     reconnect,
     implicit_inet6,
     active_n,
     {group, recv_and_send_opts},
     {group, local},
     recv_close,
     {group, socket_monitor},
     otp_17492,
     {group, sockaddr},
     {group, otp18323}
    ].

recv_and_send_opts_cases() ->
    [
     recvtos, recvtosttl, recvttl, recvtclass,
     sendtos, sendtosttl, sendttl, sendtclass
    ].

local_cases() ->
    [
     local_basic,
     local_basic_binary,
     local_unbound,
     local_fdopen,
     local_fdopen_unbound,
     local_abstract
    ].

socket_monitor_cases() ->
    [
     socket_monitor1,
     socket_monitor1_manys,
     socket_monitor1_manyc,
     socket_monitor1_demon_after,
     socket_monitor2,
     socket_monitor2_manys,
     socket_monitor2_manyc
    ].

sockaddr_cases() ->
    [
     t_simple_local_sockaddr_in_send_recv,
     t_simple_link_local_sockaddr_in_send_recv,
     t_simple_local_sockaddr_in6_send_recv,
     t_simple_link_local_sockaddr_in6_send_recv
    ].

otp18323_cases() ->
    [
     otp_18323_opts_processing,
     otp_18323_open
    ].


init_per_suite(Config0) ->

    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case ?LIB:init_per_suite(Config0) of
        {skip, _} = SKIP ->
            SKIP;

        Config1 when is_list(Config1) ->
            
            ?P("init_per_suite -> end when "
               "~n      Config: ~p", [Config1]),
            
            %% We need a monitor on this node also
            kernel_test_sys_monitor:start(),

            Config1
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    %% Stop the local monitor
    kernel_test_sys_monitor:stop(),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
       "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.

init_per_group(inet_backend_default = _GroupName, Config) ->
    [{socket_create_opts, []} | Config];
init_per_group(inet_backend_inet = _GroupName, Config) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, inet}]} | Config]
    end;
init_per_group(inet_backend_socket = _GroupName, Config) ->
    case ?EXPLICIT_INET_BACKEND() of
        true ->
            %% The environment trumps us,
            %% so only the default group should be run!
            {skip, "explicit inet backend"};
        false ->
            [{socket_create_opts, [{inet_backend, socket}]} | Config]
    end;
init_per_group(local, Config) -> 
    ?P("init_per_group(local) -> "
       "is socket supported and is inet-backend = socket ?"),
    case ?LIB:is_socket_supported() andalso ?IS_SOCKET_BACKEND(Config) of
        true ->
            ?P("init_per_group(local) -> [socket] do we support 'local'?"),
            case ?LIB:has_support_unix_domain_socket() of
                true ->
                    ?P("init_per_group(local) -> [socket] which platform?"),
                    case os:type() of
                        {win32, _} ->
                            %% AF_LOCAL (AF_UNIX) is *not* (yet) supported
                            %% for DGRAM (UDP) 
                            ?P("init_per_group(local) -> "
                               "[socket,win32] 'local' not (currently) "
                               "supported for DGRAM on windows"),
                            {skip, "AF_LOCAL not supported"};
                        {OSF, _} ->
                            ?P("init_per_group(local) -> "
                               "[socket,~w] we support (DGRAM) 'local'!",
                               [OSF]),
                            Config
                    end;
                false ->
                    {skip, "AF_LOCAL not supported"}
            end;
        false ->
            ?P("init_per_group(local) -> [inet] do we support 'local'?"),
            case ?OPEN(Config, 0, [local]) of
                {ok,S} ->
                    ?P("init_per_group(local) -> "
                       "[inet] we support (DGRAM) 'local'!"),
                    ok = gen_udp:close(S),
                    Config;
                {error, eafnosupport} ->
                    ?P("init_per_group(local) -> "
                       "[inet] we *do not* support 'local'"),
                    {skip, "AF_LOCAL not supported"};
                {error, {invalid, {domain, local}}} ->
                    ?P("init_per_group(local) -> "
                       "[inet] we *do not* support 'local'"),
                    {skip, "AF_LOCAL not supported"}
            end
    end;
init_per_group(sockaddr = _GroupName, Config) ->
    ?P("init_per_group(sockaddr) -> do we support 'socket'"),
    try socket:info() of
	_ ->
            ?P("init_per_group(sockaddr) -> we support 'socket'"),
            Config
    catch
        error : notsup ->
            ?P("init_per_group(sockaddr) -> we *do not* support 'socket'"),
            {skip, "esock not supported"};
        error : undef ->
            ?P("init_per_group(sockaddr) -> 'socket' not configured"),
            {skip, "esock not configured"}
    end;
init_per_group(otp18323 = _GroupName, Config) ->
    ?P("init_per_group(otp18323) -> inet-drv specific bug(s)"),
    case ?IS_SOCKET_BACKEND(Config) of
        true ->
            {skip, "Inet Drv specific bugs"};
        false ->
            ok
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(local, _Config) ->
    delete_local_filenames();
end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config0) ->
    ?P("init_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config0, erlang:nodes(), pi(links), pi(monitors)]),

    kernel_test_global_sys_monitor:reset_events(),

    Config1 = init_per_testcase2(Case, Config0),

    ?P("init_per_testcase -> done when"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), pi(links), pi(monitors)]),
    Config1.

init_per_testcase2(read_packets, Config) ->
    ct:timetrap({minutes, 2}),
    Config;
init_per_testcase2(_Case, Config) ->
    Config.


end_per_testcase(_Case, Config) ->
    ?P("end_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), pi(links), pi(monitors)]),

    ?P("system events during test: "
       "~n   ~p", [kernel_test_global_sys_monitor:events()]),

    ?P("end_per_testcase -> done with"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), pi(links), pi(monitors)]),
    ok.


%%-------------------------------------------------------------
%% Send two packets to a closed port (on some systems this causes the socket
%% to be closed).

%% Tests core functionality.
send_to_closed(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_send_to_closed(Config) end).

do_send_to_closed(Config) ->
    {ok, Sock} = ?OPEN(Config, 0),
    {ok, Addr} = ?LIB:which_local_addr(inet),
    ok = gen_udp:send(Sock, Addr, ?CLOSED_PORT, "foo"),
    timer:sleep(2),
    ok = gen_udp:send(Sock, Addr, ?CLOSED_PORT, "foo"),
    ok = gen_udp:close(Sock),
    ok.



%%-------------------------------------------------------------
%% Send to the empty host name

send_to_empty(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_send_to_empty(Config) end).

do_send_to_empty(Config) ->
    {ok, Sock} = ?OPEN(Config, 0),
    element(1, os:type()) =:= unix andalso
        begin
            {error, nxdomain} = gen_udp:send(Sock, "", ?CLOSED_PORT, "xXx"),
            {error, nxdomain} = gen_udp:send(Sock, '', ?CLOSED_PORT, "xXx")
        end,
    {error, nxdomain} = gen_udp:send(Sock, ".", ?CLOSED_PORT, "xXx"),
    {error, nxdomain} = gen_udp:send(Sock, '.', ?CLOSED_PORT, "xXx"),
    ok.

%%-------------------------------------------------------------
%% Test that the UDP socket buffer sizes are settable

%% Test UDP buffer size setting.
buffer_size(Config) when is_list(Config) ->
    Cond = fun() ->
                   case os:type() of
                       {win32, nt} ->
                           case ?IS_SOCKET_BACKEND(Config) of
                               true ->
                                   {skip, "TC not compliant with socket (yet)"};
                               false ->
                                   ok
                           end;
                       _ ->
                           ok
                   end
           end,
    TC   = fun() -> do_buffer_size(Config) end,
    ?TC_TRY(?FUNCTION_NAME, Cond, TC).

do_buffer_size(Config) when is_list(Config) ->
    {ok, Addr} = ?LIB:which_local_addr(inet),
    ClientIP = Addr,
    ServerIP = Addr,
    Len = 256,
    Bin = list_to_binary(lists:seq(0, Len-1)),
    M = 8192 div Len,
    SAFE = 3,
    LONG = 1,
    Spec0 =
	[
         {opt, M},   {safe, M   - SAFE}, {long, M   + LONG},
	 {opt, 2*M}, {safe, 2*M - SAFE}, {long, 2*M + LONG},
	 {opt, 4*M}, {safe, 4*M - SAFE}, {long, 4*M + LONG}
        ],
    Spec =
	[case Tag of
	     opt ->
		 [{recbuf,Val*Len},{sndbuf,(Val + 2)*Len}];
	     safe ->
		 {list_to_binary(lists:duplicate(Val, Bin)),
		  [correct]};
	     long ->
		 {list_to_binary(lists:duplicate(Val, Bin)),
		  [truncated,emsgsize,timeout]}
	 end || {Tag,Val} <- Spec0],
    %%
    {ok, ClientSocket} = ?OPEN(Config, 0, [binary, {ip, ClientIP}]),
    {ok, ClientPort}   = inet:port(ClientSocket),
    Client = self(),
    ?P("Client: {~p, ~p}, ~p", [ClientIP, ClientPort, ClientSocket]),
    Server =
	spawn_link(
	  fun () ->
                  ?P("buffer_size[server] -> starting"),
		  {ok, ServerSocket} =
                      ?OPEN(Config, 0, [binary, {ip, ServerIP}]),
		  {ok, ServerPort}   =
                      inet:port(ServerSocket),
		  Client ! {self(),port,ServerPort},
		  buffer_size_server(Client, ClientIP, ClientPort, 
				     ServerSocket, 1, Spec),
		  ok = gen_udp:close(ServerSocket)
	  end),
    Mref = erlang:monitor(process, Server),
    receive
	{Server, port, ServerPort} ->
            ?P("Server: {~p, ~p}, ~p", [ServerIP, ServerPort, Server]),
	    buffer_size_client(Server, ServerIP, ServerPort,
			       ClientSocket, 1, Spec)
    end,
    ok = gen_udp:close(ClientSocket),
    receive
	{'DOWN',Mref,_,_,normal} ->
	    ok
    end.

buffer_size_client(_, _, _, _, _, []) ->
    ok;
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    ?P("buffer_size_client(~w) -> entry with"
       "~n   Opts: ~p", [Cnt, Opts]),
    ok = inet:setopts(Socket, Opts),
    GOpts = [K || {K, _} <- Opts],
    ?P("buffer_size_client(~w) -> options after set: "
       "~n   ~p", [Cnt, inet:getopts(Socket, GOpts)]),
    Server ! {self(),setopts,Cnt},
    receive {Server,setopts,Cnt} -> ok end,
    buffer_size_client(Server, IP, Port, Socket, Cnt+1, T);
buffer_size_client(Server, IP, Port, 
		   Socket, Cnt, [{B,Replies}|T]=Opts) when is_binary(B) ->
    ?P("buffer_size_client(~w) -> entry with"
       "~n   send size:   ~w"
       "~n   expecting:   ~p"
       "~nwhen"
       "~n   Socket Info: ~p",
       [Cnt, byte_size(B), Replies, inet:info(Socket)]),
    case gen_udp:send(Socket, IP, Port, <<Cnt,B/binary>>) of
	ok ->
	    receive
		{Server, Cnt, Reply} ->
                    ?P("buffer_size_client(~w) -> "
                       "~n   Reply: ~p", [Cnt, Reply]),
		    Tag =
                        case Reply of
                            %% {completion_status, #{info := Tag0}}
                            %%   when is_atom(Tag0) ->
                            %%     Tag0;
                            %% {completion_status, Tag0} when is_atom(Tag0) ->
                            %%     Tag0;
                            {Tag0, _} ->
                                Tag0;
                            Tag0 when is_atom(Tag0) ->
                                Tag0
                        end,
		    case lists:member(Tag, Replies) of
			true -> ok;
			false ->
                            ?P("buffer_size_client(~w) -> "
                               "missing from expected replies: "
                               "~n   Tag:     ~p"
                               "~n   Replies: ~p", [Cnt, Tag, Replies]),
			    ct:fail({reply_mismatch,Cnt,Reply,Replies,
				     byte_size(B),
				     inet:getopts(Socket,
						  [sndbuf,recbuf])})
		    end,
		    buffer_size_client(Server, IP, Port, Socket, Cnt+1, T)
	    after 1313 ->
                    %% ?P("buffer_size_client(~w) -> timeout", [Cnt]),
		    buffer_size_client(Server, IP, Port, Socket, Cnt, Opts)
	    end;

	{error, enobufs = Reason} ->
	    ?P("<WARNING> send failed with '~w' - system overload => SKIP"),
	    ?SKIPE(Reason);

	{error, Reason} ->
	    ?P("<ERROR> Client failed sending ~w bytes of data: "
	       "~n   SndBuf: ~p"
	       "~n   Reason: ~p",
	       [byte_size(B), inet:getopts(Socket, [sndbuf]), Reason]),
	    ct:fail(Reason)
    end.

buffer_size_server(_, _, _, _, _, []) -> 
    ok;
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [Opts|T]) when is_list(Opts) ->
    ?P("buffer_size_server(~w) -> entry when await client setopts", [Cnt]),
    receive {Client, setopts, Cnt} -> ok end,
    ?P("buffer_size_server(~w) -> setopts with "
       "~n   Opts:        ~p"
       "~nwhen"
       "~n   Socket info: ~p", [Cnt, Opts, inet:info(Socket)]),
    ok = inet:setopts(Socket, Opts),
    GOpts = [K || {K, _} <- Opts],
    ?P("buffer_size_server(~w) -> options after set: "
       "~n                ~p"
       "~nwhen"
       "~n   Socket info: ~p",
       [Cnt, inet:getopts(Socket, GOpts), inet:info(Socket)]),
    Client ! {self(), setopts, Cnt},
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T);
buffer_size_server(Client, IP, Port, 
		   Socket, Cnt, [{B,_}|T]) when is_binary(B) ->
    ?P("buffer_size_server(~w) -> entry when"
       "~n   expect ~w bytes of data"
       "~nwhen"
       "~n   Socket info: ~p"
       "~n   MQ:          ~p", [Cnt, byte_size(B), inet:info(Socket), mq()]),
    Reply = case buffer_size_server_recv(Socket, IP, Port, Cnt) of
                D when is_binary(D) ->
                    SizeD = byte_size(D),
                    ?P("buffer_size_server(~w) -> received: ~w bytes of data"
                      "~nwhen"
                       "~n   Socket Info: ~p", [Cnt, SizeD, inet:info(Socket)]),
                    case B of
                        D ->
                            correct;
                        <<D:SizeD/binary,_/binary>> ->
                            truncated;
                        _ ->
                            {unexpected,D}
                    end;
                Error ->
                    ?P("buffer_size_server(~w) -> error:"
                       "~n                ~p"
                       "~nwhen"
                       "~n   Socket Info: ~p",
                       [Cnt, Error, inet:info(Socket)]),
                    Error
            end,
    ?P("buffer_size_server(~w) -> send reply '~p' when"
       "~n   Socket Info: ~p", [Cnt, Reply, inet:info(Socket)]),
    Client ! {self(), Cnt, Reply},
    ?SLEEP(?SECS(1)),
    buffer_size_server(Client, IP, Port, Socket, Cnt+1, T).

buffer_size_server_recv(Socket, IP, Port, Cnt) ->
    ?P("buffer_size_server(~w) -> await data: "
       "~n   Socket:      ~p"
       "~n   IP:          ~p"
       "~n   Port:        ~p"
       "~nwhen"
       "~n   Socket Info: ~p", [Cnt, Socket, IP, Port, inet:info(Socket)]),
    receive
	{udp, Socket, IP, Port, <<Cnt, B/binary>>} ->
            ?P("buffer_size_server(~w) -> received ~w bytes",
               [Cnt, byte_size(B)]),
	    B;
	{udp, Socket, IP, Port, <<_B/binary>>} ->
            ?P("buffer_size_server(~w) -> received unexpected ~w bytes",
               [Cnt, byte_size(_B)]),
	    buffer_size_server_recv(Socket, IP, Port, Cnt);

	{udp, Socket, IP, Port, _CRAP} ->
            ?P("buffer_size_server(~w) -> received unexpected crap", [Cnt]),
	    buffer_size_server_recv(Socket, IP, Port, Cnt);

	{udp, XSocket, XIP, XPort, _CRAP} ->
            ?P("buffer_size_server(~w) -> received unexpected udp message: "
               "~n   XSocket: ~p"
               "~n   Socket:  ~p"
               "~n   XIP:     ~p"
               "~n   IP:      ~p"
               "~n   XPort:   ~p"
               "~n   Port:    ~p",
               [Cnt, XSocket, Socket, XIP, IP, XPort, Port]),
	    buffer_size_server_recv(Socket, IP, Port, Cnt);

	{udp_closed, Socket} ->
            ?P("buffer_size_server(~w) -> received unexpected 'closed'", [Cnt]),
	    closed;

	{udp_error, Socket, Error} ->
            ?P("buffer_size_server(~w) -> error: "
               "~n   ~p", [Cnt, Error]),
            ok = inet:setopts(Socket, [{active, true}]),
	    Error

    after 5000 ->
            ?P("buffer_size_server(~w) -> timeout: "
               "~n   Socket:       ~p"
               "~n   Socket Info:  ~p"
               "~n   Process Info: ~p",
               [Cnt,
                Socket, inet:info(Socket), erlang:process_info(self())]),
	    {timeout, mq()}
    end.


%%-------------------------------------------------------------
%% OTP-15206: Keep buffer small for udp
%%-------------------------------------------------------------
max_buffer_size(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_max_buffer_size(Config) end).

do_max_buffer_size(Config) when is_list(Config) ->
    ?P("create socket"),
    {ok, Socket} = ?OPEN(Config, 0, [binary]),
    ?P("get buffers"),
    {ok, [{recbuf, RecBuf0}, {buffer, Buffer0}]} =
        inet:getopts(Socket, [recbuf, buffer]),
    ?P("buffers: "
       "~n   RecBuf: ~p"
       "~n   Buffer: ~p", [RecBuf0, Buffer0]),
    NewRecBuf = 1 bsl 20,
    ?P("set recbuf: ~w", [NewRecBuf]),
    ok = inet:setopts(Socket, [{recbuf, NewRecBuf}]),
    ?P("get buffers"),
    case inet:getopts(Socket, [recbuf, buffer]) of
        {ok, [{recbuf, RecBuf}, {buffer, 65536 = Buffer}]} ->
            ?P("buffers: expected"
               "~n   RecBuf: ~p"
               "~n   Buffer: ~p", [RecBuf, Buffer]),
            gen_udp:close(Socket),
            ok;
        {ok, [{recbuf, RecBuf}, {buffer, Buffer}]} ->
            ?P("buffers: unexpected"
               "~n   RecBuf: ~p"
               "~n   Buffer: ~p", [RecBuf, Buffer]),
            gen_udp:close(Socket),
            ct:fail({unexpected_buffer_size, Buffer});
        {error, Reason} ->
            ?P("failed extracting buffers"
               "~n   ~p", [Reason]),
            (catch gen_udp:close(Socket)),
            ct:fail({unexpected_getopts_error, Reason})
    end,
    ?P("done"),
    ok.
            



%%-------------------------------------------------------------
%% OTP-3823 gen_udp:recv does not return address in binary mode
%%

%% OTP-3823 gen_udp:recv does not return address in binary mode.
binary_passive_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_binary_passive_recv(Config) end).

do_binary_passive_recv(Config) when is_list(Config) ->
    D1       = "The quick brown fox jumps over a lazy dog",
    D2       = list_to_binary(D1),
    D3       = ["The quick", <<" brown ">>, "fox jumps ", <<"over ">>,
		<<>>, $a, [[], " lazy ", <<"dog">>]],
    D2       = iolist_to_binary(D3),
    B        = D2,
    {ok, Addr} = ?LIB:which_local_addr(inet),
    ?P("open receive socket"),
    {ok, R}  = ?OPEN(Config, 0, [binary, {active, false}, {debug, true}]),
    ok = inet:setopts(R, [{debug, false}]),
    {ok, RP} = inet:port(R),
    ?P("open send socket"),
    {ok, S}  = ?OPEN(Config, 0),
    {ok, SP} = inet:port(S),
    ?P("try send (to port ~w) ~w bytes (as a list)", [RP, length(D1)]),
    ok       = gen_udp:send(S, Addr, RP, D1),
    ?P("try recv (from port ~w) ~w bytes", [SP, byte_size(B)+1]),
    {ok, {Addr, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ?P("try send (to port ~w) ~w bytes (as a binary)", [RP, byte_size(D2)]),
    ok       = gen_udp:send(S, Addr, RP, D2),
    ?P("try recv (from port ~w) ~w bytes", [SP, byte_size(B)+1]),
    {ok, {Addr, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ?P("try send (to port ~w) ~w bytes (as a iolist)", [RP, iolist_size(D3)]),
    ok       = gen_udp:send(S, Addr, RP, D3),
    ?P("try recv (from port ~w) ~w bytes", [SP, byte_size(B)+1]),
    {ok, {Addr, SP, B}} = gen_udp:recv(R, byte_size(B)+1),
    ?P("cleanup"),
    ok       = gen_udp:close(S),
    ok       = gen_udp:close(R),
    ?P("done"),
    ok.


%%-------------------------------------------------------------
%% OTP-3836 inet_udp crashes when IP-address is larger than 255.

%% OTP-3836 inet_udp crashes when IP-address is larger than 255.
bad_address(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_bad_address(Config) end).

do_bad_address(Config) when is_list(Config) ->
    ?P("create sockets"),
    {ok, R}   = ?OPEN(Config, 0),
    {ok, RP}  = inet:port(R),
    {ok, S}   = ?OPEN(Config, 0),
    {ok, _SP} = inet:port(S),

    ?P("try send to invalid address 1 - expect failure"),
    case (catch gen_udp:send(S, {127,0,0,1,0}, RP, "void")) of
        {'EXIT', badarg} ->
            ok;
        Any1 ->
            ?P("<ERROR> unexpected result: "
               "~n   ~p", [Any1]),
            ct:fail({unexpected_result, 1, Any1})                
    end,

    ?P("try send to invalid address 2 - expect failure"),
    case (catch gen_udp:send(S, {127,0,0,256}, RP, "void")) of
        {'EXIT', badarg} ->
            ok;
        Any2 ->
            ?P("<ERROR> unexpected result: "
               "~n   ~p", [Any2]),
            ct:fail({unexpected_result, 2, Any2})
    end,

    ?P("cleanup"),
    ok       = gen_udp:close(S),
    ok       = gen_udp:close(R),

    ?P("done"),
    ok.


%%-------------------------------------------------------------
%% OTP-6249 UDP option for number of packet reads
%%
%% Starts a node that on command sends a bunch of messages
%% to our UDP port. The receiving process just receives and
%% ignores the incoming messages.
%% A tracing process traces the receiving port for
%% 'send' and scheduling events. From the trace,
%% how many messages are received per in/out scheduling,
%% which should never be more than the read_packet parameter.

%% OTP-6249 UDP option for number of packet reads.
read_packets(Config) when is_list(Config) ->
    Cond = fun() ->
		   case ?IS_SOCKET_BACKEND(Config) of
		       true ->
                           %% We have not (yet) implemented support for 
                           %% this option. We accept it but do not use it.
			   {skip, "Not compliant with socket"};
		       false ->
			   ok
		   end
	   end,
    TC   = fun() ->
                   ?P("~w:tc -> begin", [?FUNCTION_NAME]),
                   Res = do_read_packets(Config),
                   ?P("~w:tc -> done", [?FUNCTION_NAME]),
                   Res
           end,
    ?TC_TRY(?FUNCTION_NAME, Cond, TC).

do_read_packets(Config) when is_list(Config) ->
    N1   = 5,
    N2   = 1,
    Msgs = 30000,
    {ok, Addr} = ?LIB:which_local_addr(inet),
    ?P("open socket (with read-packets: ~p)", [N1]),
    {ok, R}   = ?OPEN(Config, 0, [{ip, Addr}, {read_packets,N1}]),
    {ok, {RA, RP}}  = inet:sockname(R),
    ?P("reader: "
       "~n   Addr: ~p"
       "~n   Port: ~p", [RA, RP]),

    ?P("create slave node"),
    {ok,Peer,Node} = ?CT_PEER(),
    %%
    ?P("perform read-packets test"),
    {V1, Trace1} = read_packets_test(Config, R, RA, RP, Msgs, Node),
    ?P("verify read-packets (to ~w)", [N1]),
    {ok,[{read_packets,N1}]} = inet:getopts(R, [read_packets]),
    %%
    ?P("set new read-packets: ~p", [N2]),
    ok = inet:setopts(R, [{read_packets, N2}]),
    ?P("perform read-packets test"),
    {V2, Trace2} = read_packets_test(Config, R, RA, RP, Msgs, Node),
    ?P("verify read-packets (to ~w)", [N2]),
    {ok, [{read_packets,N2}]} = inet:getopts(R, [read_packets]),
    %%
    ?P("stop slave node"),
    peer:stop(Peer),
    ?P("dump trace 1"),
    dump_terms(Config, "trace1.terms", Trace1),
    ?P("dump trace 2"),
    dump_terms(Config, "trace2.terms", Trace2),

    %% Because of the inherit racy-ness of the feature it is
    %% hard to test that it behaves correctly.
    %% Right now (OTP 21) a port task takes 5% of the
    %% allotted port task reductions to execute, so
    %% the max number of executions a port is allowed to
    %% do before being re-scheduled is N * 20

    ?P("read-packets test verification when: "
       "~n      N1: ~p"
       "~n      V1: ~p"
       "~n   vs"
       "~n      N2: ~p"
       "~n      V2: ~p", [N1, V1, N2, V2]),
    if
        V1 > (N1 * 20) ->
            ct:fail("Got ~p msgs, max was ~p", [V1, N1]);
        V2 > (N2 * 20) ->
            ct:fail("Got ~p msgs, max was ~p", [V2, N2]);
        true ->
            ok
    end,
    ?P("done"),
    ok.

dump_terms(Config, Name, Terms) ->
    FName = filename:join(proplists:get_value(priv_dir, Config),Name),
    file:write_file(FName, term_to_binary(Terms)),
    ct:log("Logged terms to ~s",[FName]).

read_packets_test(Config, R, RA, RP, Msgs, Node) ->
    Receiver = self(),
    Tracer =
	spawn_link(
	  fun () ->
		  receive
		      {Receiver,get_trace} ->
			  Receiver ! {self(),{trace,flush()}}
		  end
	  end),
    Sender =
	spawn_opt(
	  Node,
	  fun () ->
                  %% We run on the same "machine" as R,
                  %% so use the same addr
		  {ok, S}  = ?OPEN(Config, 0, [{ip, RA}]),
		  {ok, {SA, SP}} = inet:sockname(S),
		  Receiver ! {self(),{sockname, {SA, SP}}},
		  receive
		      {Receiver,go} ->
			  read_packets_send(S, RA, RP, Msgs)
		  end
	  end, 
	  [link,{priority,high}]),
    receive
	{Sender, {sockname, {SA, SP}}} ->
	    erlang:trace(R, true,
			 [running_ports,'send',{tracer,Tracer}]),
	    erlang:yield(),
	    Sender ! {Receiver,go},
	    read_packets_recv(Msgs),
	    erlang:trace(R, false, [all]),
	    Tracer ! {Receiver,get_trace},
	    receive
		{Tracer,{trace,Trace}} ->
		    {read_packets_verify(R, SA, SP, Trace), Trace}
	    end
    end.

read_packets_send(_S, _RA, _RP, 0) ->
    ok;
read_packets_send(S, RA, RP, Msgs) ->
    ok = gen_udp:send(S, RA, RP, "UDP FLOOOOOOD"),
    read_packets_send(S, RA, RP, Msgs - 1).

read_packets_recv(0) ->
    ok;
read_packets_recv(N) ->
    receive
	_ ->
	    read_packets_recv(N - 1)
    after 5000 ->
	    timeout
    end.

read_packets_verify(R, SA, SP, Trace) ->
    [Max | _] = Pkts =
        lists:reverse(lists:sort(read_packets_verify(R, SA, SP, Trace, 0))),
    ?P("read-packets verify: ~p", [lists:sublist(Pkts,10)]),
    Max.

read_packets_verify(R, SA, SP, [{trace,R,OutIn,_}|Trace], M) 
  when OutIn =:= out; OutIn =:= in ->
    push(M, read_packets_verify(R, SA, SP, Trace, 0));
read_packets_verify(R, SA, SP, [{trace, R,'receive',timeout}|Trace], M) ->
    push(M, read_packets_verify(R, SA, SP, Trace, 0));
read_packets_verify(R, SA, SP,
		    [{trace,R,'send',{udp, R, SA, SP,_Msg}, Self} | Trace], M)
  when Self =:= self() ->
    read_packets_verify(R, SA, SP, Trace, M+1);
read_packets_verify(_R, _SA, _SP, [], M) ->
    push(M, []);
read_packets_verify(R, SA, SP, [T | Trace], M) ->
    ct:fail(
      {read_packets_verify, mismatch, self(),
       {R, SA, SP, [T, length(Trace)], M}});
read_packets_verify(_R, _SA, _SP, Trace, M) ->
    ct:fail({read_packets_verify,mismatch,Trace,M}).

push(0, Vs) ->
    Vs;
push(V, Vs) ->
    [V|Vs].

flush() ->
    receive
	X ->
	    [X|flush()]
    after 200 ->
	    []
    end.


%% OTP-16059
%% UDP recv with timeout 0 corrupts internal state so that after a
%% recv under {active, once} the UDP recv poll wastes incoming data
recv_poll_after_active_once(Config) when is_list(Config) ->
    Msg1 = <<"Hej!">>,
    Msg2 = <<"Hej igen!">>,
    {ok, Addr} = ?LIB:which_local_addr(inet),
    %% Addr = {127,0,0,1},
    {ok,S1} = ?OPEN(Config, 0, [binary, {ip, Addr}, {active, once}]),
    {ok,P1} = inet:port(S1),
    {ok,S2} = ?OPEN(Config, 0, [binary, {ip, Addr}, {active, false}]),
    {ok,P2} = inet:port(S2),
    ok = gen_udp:send(S2, Addr, P1, Msg1),
    receive
        {udp, S1, Addr, P2, Msg1} ->
            {error, timeout} = gen_udp:recv(S1, 0, 0),
            ok = gen_udp:send(S2, Addr, P1, Msg2),
            receive after 500 -> ok end, % Give the kernel time to deliver
            {ok, {Addr, P2, Msg2}} = gen_udp:recv(S1, 0, 0),
            ok
    end.


%% Test that the 'fd' option works.
open_fd(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> is_not_windows() end,
            fun() -> do_open_fd(Config) end).

do_open_fd(Config) when is_list(Config) ->
    Msg = "Det gör ont när knoppar brista. Varför skulle annars våren tveka?",
    Addr = {127,0,0,1},
    ?P("try open first (default domain = inet) socket and get its fd"),
    {S1, FD} = case ?OPEN(Config, 0) of
                   {ok, Sock1} when is_port(Sock1) ->
                       {ok, FileDesc1} = prim_inet:getfd(Sock1),
                       {Sock1, FileDesc1};
                   {ok, Sock1} ->
                      {ok, [{fd, FileDesc1}]} =
                          gen_udp_socket:getopts(Sock1, [fd]),
                      {Sock1, FileDesc1}
               end,
    ?P("try get the socket port number"),
    {ok, P2} = inet:port(S1),

    ?P("try open second (domain = inet6) socket with FD = ~w "
       "and expect *failure*", [FD]),

    case ?OPEN(Config, 0, [inet6, {fd,FD}]) of
        {error, einval = Reason} ->
            ?P("expected failure reason ~w", [Reason]),
            ok;
        {error, eafnosupport = Reason} ->
            ?P("expected failure reason ~w (IPv6 not supported?)", [Reason]),
            ok;
        {error, Reason} ->
            ?P("unexpected failure: ~w", [Reason]),
            ct:fail({unexpected_failure, Reason});
        {ok, Socket} ->
            ?P("unexpected success: "
               "~n   ~p", [inet:info(Socket)]),
            (catch gen_udp:close(Socket)),
            (catch gen_udp:close(S1)),
            ct:fail(unexpected_succes)
    end,

    ?P("try open second socket with FD = ~w "
       "and expect success", [FD]),
    {ok, S2} = ?OPEN(Config, 0, [{fd, FD}]),
    {ok, P2} = inet:port(S2),

    ?P("try open third socket and expect success"),
    {ok, S3} = ?OPEN(Config, 0),
    {ok, P3} = inet:port(S3),

    ?P("try send message from socket 3 to socket 2 (~w)", [P2]),
    ok = gen_udp:send(S3, Addr, P2, Msg),
    receive
	{udp, S2, Addr, P3, Msg} ->
            ?P("expected recv - "
               "try send message from socket 2 to socket 3 (~w)", [P3]),
	    ok = gen_udp:send(S2, Addr, P3, Msg),
	    receive
		{udp, S3, Addr, P2, Msg} ->
                    ?P("expected recv - done"),
		    ok
	    after 1000 ->
                    ?P("unexpected timeout"),
		    ct:fail(io_lib:format("~w", [flush()]))
	    end
    after 1000 ->
            ?P("unexpected timeout"),
	    ct:fail(io_lib:format("~w", [flush()]))
    end,
    ?P("cleanup"),
    (catch gen_udp:close(S3)),
    (catch gen_udp:close(S2)),
    (catch gen_udp:close(S1)),
    ?P("done"),
    ok.

active_n(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_active_n(Config) end).

do_active_n(Config) when is_list(Config) ->
    N = 3,
    ?P("create socket (1) with active = ~w", [N]),
    {ok, Addr} = ?LIB:which_local_addr(inet),
    S1 = ok(?OPEN(Config, 0, [{ip, Addr}, {active,N}])),
    ?P("verify socket active = ~w (~p)", [N, which_info(S1)]),
    [{active,N}] = ok(inet:getopts(S1, [active])),

    ?P("set active = ~w and expect passive", [-N]),
    ok = inet:setopts(S1, [{active,-N}]),
    receive
        {udp_passive, S1} ->
            ?P("expected passive received"),
            ok
    after
        5000 ->
            ?P("ERROR: passive *not* received"),
            exit({error,udp_passive_failure})
    end,
    ?P("verify socket active passive (=false) (~p)", [which_info(S1)]),
    [{active,false}] = ok(inet:getopts(S1, [active])),

    ?P("set active = 0 and expect passive message"),
    ok = inet:setopts(S1, [{active, 0}]),
    receive
        {udp_passive, S1} ->
            ?P("expected passive received"),
            ok
    after
        5000 ->
            ?P("ERROR: passive *not* received"),
            exit({error,udp_passive_failure})
    end,

    ?P("set active = 32767 (=max) and expect success"),
    ok = inet:setopts(S1, [{active,32767}]),
    ?P("try set active = 1 (already at max) and expect failure (einval)"),
    {error, einval} = inet:setopts(S1, [{active,1}]),
    ?P("set active = -32769 (<min) and expect failure (einval)"),
    {error, einval} = inet:setopts(S1, [{active,-32769}]),
    ?P("set active = -32768 and expect passive message"),
    ok = inet:setopts(S1, [{active,-32768}]),
    receive
        {udp_passive, S1} ->
            ?P("expected passive received"),
            ok
    after
        5000 ->
            ?P("ERROR: passive *not* received"),
            exit({error,udp_passive_failure})
    end,

    ?P("verify socket active passive (=false), (~p)", [which_info(S1)]),
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ?P("set active = ~w and expect success", [N]),
    ok = inet:setopts(S1, [{active,N}]),
    ?P("set active = ~w and expect success", [true]),
    ok = inet:setopts(S1, [{active,true}]),
    ?P("verify socket active active (=true) (~p)", [which_info(S1)]),
    [{active,true}] = ok(inet:getopts(S1, [active])),
    ?P("verify nothing in the message queue"),
    receive
        Unexpected_1 ->
            ?P("ERROR: something in the message queue: "
               "~n   ~p", [Unexpected_1]),
            exit({error, active_n, Unexpected_1})
    after
        0 ->
            ?P("nothing in the message queue"),
            ok
    end,

    ?P("set active = ~w and expect success", [N]),
    ok = inet:setopts(S1, [{active,N}]),
    ?P("set active = ~w and expect success", [once]),
    ok = inet:setopts(S1, [{active,once}]),
    ?P("verify socket active active (=once) (~p)", [which_info(S1)]),
    [{active,once}] = ok(inet:getopts(S1, [active])),
    receive
        Unexpected_2 ->
            ?P("ERROR: something in the message queue: "
               "~n   ~p", [Unexpected_2]),
            exit({error,active_n, Unexpected_2})
    after
        0 ->
            ?P("nothing in the message queue"),
            ok
    end,

    ?P("set active = ~w and expect failure (einval)", [32768]),
    {error,einval} = inet:setopts(S1, [{active,32768}]),
    ?P("set active = ~w and expect success", [false]),
    ok = inet:setopts(S1, [{active,false}]),
    ?P("verify socket active passive (=false) (~p)", [which_info(S1)]),
    [{active,false}] = ok(inet:getopts(S1, [active])),

     %% S1Port = ok(inet:port(S1)),
    {S1Addr, S1Port} = ok(inet:sockname(S1)),
    ?P("S1 sockname: "
       "~n   Addr: ~p"
       "~n   Port: ~p", [S1Addr, S1Port]),

    ?P("create socket (2) with active = ~w", [N]),
    S2 = ok(?OPEN(Config, 0, [{ip, Addr}, {active,N}])),

    %% S2Port = ok(inet:port(S2)),
    {S2Addr, S2Port} = ok(inet:sockname(S2)),
    ?P("S2 sockname: "
       "~n   Addr: ~p"
       "~n   Port: ~p", [S2Addr, S2Port]),

    ?P("verify socket (2) active = ~w (~p)", [N, which_info(S2)]),
    [{active,N}] = ok(inet:getopts(S2, [active])),
    ?P("set socket (1) active = ~w and expect success", [N]),
    ok = inet:setopts(S1, [{active,N}]),
    ?P("verify socket (1) active = ~w (~p)", [N, which_info(S1)]),
    [{active,N}] = ok(inet:getopts(S1, [active])),
    ?P("generate ~w message(s) and send them: S1 -> S2 and S2 -> S1 when:"
       "~n   Socket 1 info: ~p"
       "~n   Socket 2 info: ~p", [N, inet:info(S1), inet:info(S2)]),
    lists:foreach(
      fun(I) ->
              Msg = "message "++integer_to_list(I),
              ?P("send message ~w from S2 to S1 (~p, ~w):"
                 "~n   S1 info: ~p"
                 "~n   S2 info: ~p",
                 [I, S1Addr, S1Port, inet:info(S1), inet:info(S2)]),
              case gen_udp:send(S2, S1Addr, S1Port, Msg) of
                  ok ->
                      ok;
                  {error, Reason} = ERROR ->
                      ?P("Failed sending message ~w: "
                         "~n   ~p", [N, Reason]),
                      exit(ERROR)
              end,
              ?P("expect message on S1"),
              receive
                  {udp, S1, _, S2Port, Msg} ->
                      ?P("expected message received on S1 - "
                         "send message back: from S1 to S2 (~p, ~w): "
                         "~n   ~p", [S2Addr, S2Port, inet:info(S1)]),
                      ok = gen_udp:send(S1, S2Addr, S2Port, Msg)
              after
                  5000 ->
                      ?P("ERROR: received nothing (->S1)"),
                      exit({error,timeout})
              end,
              ?P("expect message on S2"),
              receive
                  {udp, S2, _, S1Port, Msg} ->
                      ?P("expected message received on S2: "
                         "~n   ~p", [inet:info(S2)]),
                      ok
              after
                  5000 ->
                      ?P("ERROR: received nothing (->S2)"),
                      exit({error,timeout})
              end
      end, lists:seq(1,N)),
    ?P("expect passive message for socket (1)"),
    receive
        {udp_passive, S1} ->
            ?P("received passive message for socket (1) - "
               "verify socket active passive (false)"),
            [{active,false}] = ok(inet:getopts(S1, [active]))
    after
        5000 ->
            ?P("ERROR: received nothing (1)"),
            exit({error,udp_passive})
    end,
    ?P("expect passive message for socket (2)"),
    receive
        {udp_passive,S2} ->
            ?P("received passive message for socket (2) - "
               "verify socket active passive (false)"),
            [{active,false}] = ok(inet:getopts(S2, [active]))
    after
        5000 ->
            ?P("ERROR: received nothing (2)"),
            exit({error,udp_passive})
    end,

    ?P("create socket (3) with active = ~w and expect passive message", [0]),
    S3 = ok(?OPEN(Config, 0, [{ip, Addr}, {active,0}])),
    receive
        {udp_passive,S3} ->
            ?P("received passive message for socket (3) - "
               "verify socket active passive (false)"),
            [{active,false}] = ok(inet:getopts(S3, [active]))
    after
        5000 ->
            ?P("ERROR: received nothing (3)"),
            exit({error,udp_passive})
    end,

    ?P("cleanup"),
    ok = gen_udp:close(S3),
    ok = gen_udp:close(S2),
    ok = gen_udp:close(S1),

    ?P("done"),
    ok.



recvtos(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvtos,tos,96}], false,
      fun recvtos_ok/2).

recvtosttl(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvtos,tos,96},{recvttl,ttl,33}], false,
      fun (OSType, OSVer) ->
              recvtos_ok(OSType, OSVer) andalso recvttl_ok(OSType, OSVer)
      end).

recvttl(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvttl,ttl,33}], false,
      fun recvttl_ok/2).

recvtclass(Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_recv_opts(
              Config,
              inet6, [{recvtclass,tclass,224}], false,
              fun recvtclass_ok/2);
        [] ->
            {skip,ipv6_not_supported,IFs}
    end.


sendtos(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_sendtos(Config) end).

do_sendtos(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvtos,tos,96}], true,
      fun sendtos_ok/2).

sendtosttl(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_sendtosttl(Config) end).

do_sendtosttl(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvtos,tos,96},{recvttl,ttl,33}], true,
      fun (OSType, OSVer) ->
              sendtos_ok(OSType, OSVer) andalso sendttl_ok(OSType, OSVer)
      end).

sendttl(Config) ->
    ?TC_TRY(sendttl, fun() -> do_sendttl(Config) end).

do_sendttl(Config) ->
    test_recv_opts(
      Config,
      inet, [{recvttl,ttl,33}], true,
      fun sendttl_ok/2).

sendtclass(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_sendtclass(Config) end).

do_sendtclass(Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_recv_opts(
              Config,
              inet6, [{recvtclass,tclass,224}], true,
              fun sendtclass_ok/2);
        [] ->
            {skip, {ipv6_not_supported, IFs}}
    end.


%% These version numbers are just above the highest noted in daily tests
%% where the test fails for a plausible reason, that is the lowest
%% where we can expect that the test might succeed, so
%% skip on platforms lower than this.
%%
%% On newer versions it might be fixed, but we'll see about that
%% when machines with newer versions gets installed...
%% If the test still fails for a plausible reason these
%% version numbers simply should be increased.
%% Or maybe we should change to only test on known good platforms?

%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {17,6,0});
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,openbsd}, _OSVer) -> false; % not semver_lt(OSVer, {6,9,0});
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,netbsd}, _OSVer) -> false;
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvtos_ok({unix,_}, _) -> true;
recvtos_ok(_, _) -> false.

%% Option has no effect
recvttl_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvttl_ok({unix,_}, _) -> true;
recvttl_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
recvtclass_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {9,9,0});
recvtclass_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {2,6,11});
%% Option has no effect
recvtclass_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
recvtclass_ok({unix,_}, _) -> true;
recvtclass_ok(_, _) -> false.


%% To send ancillary data seems to require much higher version numbers
%% than receiving it...
%%

%% Using the option returns einval, so it is not implemented.
sendtos_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,0,0});
sendtos_ok({unix,netbsd}, _OSVer) -> false;
sendtos_ok({unix,openbsd}, _OSVer) -> false; % not semver_lt(OSVer, {6,9,0});
sendtos_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
sendtos_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {4,0,0});
sendtos_ok({unix,freebsd}, _OSVer) -> false; % not semver_lt(OSVer, {13,1,0});
%%
sendtos_ok({unix,_}, _) -> true;
sendtos_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
sendttl_ok({unix,darwin}, _OSVer) -> false; % not semver_lt(OSVer, {19,6,0});
sendttl_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {4,0,0});
%% Using the option returns enoprotoopt, so it is not implemented.
sendttl_ok({unix,freebsd}, _OSVer) -> false; % not semver_lt(OSVer, {13,1,0});
%% Option has no effect
sendttl_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
sendttl_ok({unix,openbsd}, _OSVer) -> false; % not semver_lt(OSVer, {6,9,0});
%%
sendttl_ok({unix,_}, _) -> true;
sendttl_ok(_, _) -> false.

%% Using the option returns einval, so it is not implemented.
sendtclass_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {9,9,0});
sendtclass_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {2,6,11});
%% Option has no effect
sendtclass_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%%
sendtclass_ok({unix,_}, _) -> true;
sendtclass_ok(_, _) -> false.


semver_lt({X1,Y1,Z1} = V1, {X2,Y2,Z2} = V2) ->
    ?P("semver_lt -> OS version check:"
       "~n   Version 1: ~p"
       "~n   Version 2: ~p", [V1, V2]),
    if
        X1 > X2 -> ?P("semver_lt -> X1 > X2: ~p > ~p", [X1, X2]), false;
        X1 < X2 -> ?P("semver_lt -> X1 < X2: ~p < ~p", [X1, X2]), true;
        Y1 > Y2 -> ?P("semver_lt -> Y1 > Y2: ~p > ~p", [Y1, Y2]), false;
        Y1 < Y2 -> ?P("semver_lt -> Y1 < Y2: ~p < ~p", [Y1, Y2]), true;
        Z1 > Z2 -> ?P("semver_lt -> Z1 > Z2: ~p > ~p", [Z1, Z2]), false;
        Z1 < Z2 -> ?P("semver_lt -> Z1 < Z2: ~p < ~p", [Z1, Z2]), true;
        true    -> ?P("semver_lt -> default"), false
    end;
semver_lt(V1, {_,_,_} = V2) ->
    ?P("semver_lt -> fallback OS version check when: "
       "~n   Version 1: ~p"
       "~n   Version 2: ~p", [V1, V2]),
    false.

test_recv_opts(Config, Family, Spec, TestSend, OSFilter) ->
    OSType = os:type(),
    OSVer  = os:version(),
    case OSFilter(OSType, OSVer) of
        true ->
            ?P("OS: ~p, ~p", [OSType, OSVer]),
            test_recv_opts(Config,
                           Family, Spec, TestSend, OSType, OSVer);
        false ->
            {skip,{not_supported_for_os_version,{OSType,OSVer}}}
    end.
%%
test_recv_opts(Config, Family, Spec, TestSend, _OSType, _OSVer) ->
    Timeout = 5000,
    RecvOpts = [RecvOpt || {RecvOpt,_,_} <- Spec],
    TrueRecvOpts = [{RecvOpt,true} || {RecvOpt,_,_} <- Spec],
    FalseRecvOpts = [{RecvOpt,false} || {RecvOpt,_,_} <- Spec],
    Opts = [Opt || {_,Opt,_} <- Spec],
    OptsVals = [{Opt,Val} || {_,Opt,Val} <- Spec],
    TrueRecvOpts_OptsVals = TrueRecvOpts ++ OptsVals,
    Addr =
        case Family of
            inet ->
                {127,0,0,1};
            inet6 ->
                {0,0,0,0,0,0,0,1}
        end,
    %%
    ?P("try open socket (1) with true opts"),
    {ok, S1} = ?OPEN(Config, 0, [Family, binary, {active,false}|TrueRecvOpts]),
    {ok, P1} = inet:port(S1),
    ?P("try get (true) socket (1) opts"),
    {ok, TrueRecvOpts} = inet:getopts(S1, RecvOpts),
    ?P("try set (false) socket (1) opts"),
    ok = inet:setopts(S1, FalseRecvOpts),
    ?P("verify (false) socket (1) opts"),
    {ok, FalseRecvOpts} = inet:getopts(S1, RecvOpts),
    ok = inet:setopts(S1, TrueRecvOpts_OptsVals),
    {ok,TrueRecvOpts_OptsVals} = inet:getopts(S1, RecvOpts ++ Opts),
    %%
    %% S1 now has true receive options and set option values
    %%
    ?P("try open socket (2) with false opts"),
    {ok, S2} =
        ?OPEN(Config, 0, [Family, binary, {active,true} | FalseRecvOpts]),
    {ok, P2} = inet:port(S2),
    ?P("try get (false) socket (2) opts"),
    {ok, FalseRecvOpts_OptsVals2} = inet:getopts(S2, RecvOpts ++ Opts),
    OptsVals2 = FalseRecvOpts_OptsVals2 -- FalseRecvOpts,
    ?P("info: "
       "~n   Socket 1:    ~p"
       "~n   Socket 2:    ~p"
       "~n   Opts Vals 2: ~p", [inet:info(S1), inet:info(S2), OptsVals2]),

    %%
    %% S2 now has false receive options and default option values,
    %% OptsVals2 contains the default option values
    %%
    ?P("send/3: S2 -> S1"),
    ok = gen_udp:send(S2, {Addr,P1}, <<"abcde">>),
    ?SLEEP(100),

    ?P("send/4: S1 -> S2"),
    ok = gen_udp:send(S1, Addr, P2, <<"fghij">>),
    TestSend andalso
        begin
            ?P("send/5: S2 -> S1"
               "~n   ~p", [OptsVals]),
            case gen_udp:send(S2, Addr, P1, OptsVals, <<"ABCDE">>) of
                ok ->
                    ?SLEEP(100),
                    ok;
                {error, enoprotoopt = Reason1} ->
                    ?SKIPT(?F("send (1) failed: ~p", [Reason1]))
            end,
            ?P("send/4: S2 -> S1"
               "~n   ~p", [OptsVals]),
            case gen_udp:send(S2, {Addr,P1}, OptsVals, <<"12345">>) of
                ok ->
                    ?SLEEP(100),
                    ok;
                {error, enoprotoopt = Reason2} ->
                    ?SKIPT(?F("send (2) failed: ~p", [Reason2]))
            end
        end,
    ?P("try S1 recv"),
    {ok,{_,P2,OptsVals3, <<"abcde">>}} = gen_udp:recv(S1, 0, Timeout),
    ?P("S1 recv: "
       "~n   OptsVals3: ~p", [OptsVals3]),
    ?SLEEP(100),

    verify_sets_eq(OptsVals3, OptsVals2),
    TestSend andalso
        begin
            ?P("try S1 recv"),
            {ok,{_,P2,OptsVals0,<<"ABCDE">>}} = gen_udp:recv(S1, 0, Timeout),
            ?P("S1 recv: "
               "~n   OptsVals0: ~p", [OptsVals0]),
            ?SLEEP(100),

            ?P("try S1 recv"),
            {ok,{_,P2,OptsVals1,<<"12345">>}} = gen_udp:recv(S1, 0, Timeout),
            ?P("S1 recv: "
               "~n   OptsVals1: ~p", [OptsVals1]),
            ?SLEEP(100),
            verify_sets_eq(OptsVals0, OptsVals),
            verify_sets_eq(OptsVals1, OptsVals)
        end,
    ?P("await message on S2"),
    receive
        {udp, S2, _, P1, <<"fghij">>} ->
            ?P("S2 received message"),
            ok;
        Other1 ->
            exit({unexpected,Other1})
    after Timeout ->
            exit(timeout)
    end,
    %%
    ?P("try set (false) socket (1) opts"),
    ok = inet:setopts(S1, FalseRecvOpts),
    ?P("verify (false) socket (1) opts"),
    {ok, FalseRecvOpts} = inet:getopts(S1, RecvOpts),
    ?P("try set (true) socket (1) opts"),
    ok = inet:setopts(S2, TrueRecvOpts),
    ?P("verify (true) socket (1) opts"),
    {ok,TrueRecvOpts} = inet:getopts(S2, RecvOpts),
    %%
    %% S1 now has false receive options and set option values
    %%
    %% S2 now has true receive options and default option values
    %%
    ?P("send/4: S2 -> S1"),
    ok = gen_udp:send(S2, {Addr,P1}, [], <<"klmno">>),
    ?SLEEP(100),

    ?P("send/3: S1 -> S2"),
    ok = gen_udp:send(S1, {Family,{loopback,P2}}, <<"pqrst">>),
    ?SLEEP(100),

    TestSend andalso
        begin
            ?P("send/4: S1 -> S2"
               "~n   ~p", [OptsVals]),
            ok = gen_udp:send(S1,
                              {Family,{loopback,P2}}, OptsVals2, <<"PQRST">>),
            ?SLEEP(100)
        end,

    ?P("try recv data on S1"),
    {ok,{_,P2,<<"klmno">>}} = gen_udp:recv(S1, 0, Timeout),
    ?P("await message on S2"),
    receive
        {udp,S2,_,P1,OptsVals4,<<"pqrst">>} ->
            ?P("S2 message received: "
               "~n   OptsVals4: ~p", [OptsVals4]),
            verify_sets_eq(OptsVals4, OptsVals);
        Other2 ->
            exit({unexpected,Other2})
    after Timeout ->
            exit(timeout)
    end,
    ?P("(maybe, ~p) await message on S2", [TestSend]),
    TestSend andalso
        receive
            {udp, S2, _, P1, OptsVals5, <<"PQRST">>} ->
                ?P("S2 message received: "
                   "~n   OptsVals5: ~p", [OptsVals5]),
                verify_sets_eq(OptsVals5, OptsVals2);
            Other3 ->
                exit({unexpected,Other3})
        after Timeout ->
                exit(timeout)
        end,

    ?P("cleanup"),
    ok = gen_udp:close(S1),
    ok = gen_udp:close(S2),
%%%    exit({{_OSType,_OSVer},success}), % In search for the truth

    ?P("done"),
    ok.

verify_sets_eq(L1, L2) ->
    L = lists:sort(L1),
    case lists:sort(L2) of
        L ->
            ok;
        _ ->
            exit({sets_neq, L1, L2})
    end.


local_basic(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_local_basic(Config, []) end).

local_basic_binary(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_local_basic(Config, [binary]) end).

do_local_basic(Config, Opts) ->
    ?P("begin"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),
    %%
    ?P("create server socket"),
    S = ok(?OPEN(Config, 0, [{ifaddr,{local,SFile}},{active,false}|Opts])),
    ?P("create client socket"),
    C = ok(?OPEN(Config, 0, [{ifaddr,{local,CFile}},{active,false}|Opts])),
    SAddr = ok(inet:sockname(S)),
    CAddr = ok(inet:sockname(C)),
    ?P("SockName(s):"
       "~n   Server: ~p"
       "~n   Client: ~p", [SAddr, CAddr]),
    local_handshake(S, SAddr, C, CAddr),

    ?P("cleanup"),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok = file:delete(CFile),
    ?P("end"),
    ok.

local_unbound(Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    %%
    S = ok(?OPEN(Config, 0, [{ifaddr,SAddr},{active,false}])),
    C = ok(?OPEN(Config, 0, [local,{active,false}])),
    SAddr = ok(inet:sockname(S)),
    local_handshake(S, SAddr, C, undefined),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok.

local_fdopen(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_local_fdopen(Config) end).


do_local_fdopen(Config) ->
    ?P("begin"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),

    %%
    ?P("try create \"dummy\" (server) socket"),
    {S0, FD} = case ?OPEN(Config, 0, [{ifaddr,SAddr},{active,false}]) of
                   {ok, Sock0} when is_port(Sock0) ->
                       ?P("(port) try extract FD"),
                       Fd = ok(prim_inet:getfd(Sock0)),
                       {Sock0, Fd};
                   {ok, Sock0} -> % socket
                       ?P("(socket) try extract FD"),
                       {ok, [{fd, Fd}]} =
                           gen_udp_socket:getopts(Sock0, [fd]),
                       {Sock0, Fd}
               end,
    ?P("try create (client) socket"),
    C  = ok(?OPEN(Config, 0, [{ifaddr,{local,CFile}},{active,false}])),
    SAddr = ok(inet:sockname(S0)),
    CAddr = ok(inet:sockname(C)),
    ?P("try create (server) socket using fd = ~w", [FD]),
    S  = ok(?OPEN(Config, 0, [{fd, FD}, local, {active,false}])),
    SAddr = ok(inet:sockname(S)),

    ?P("perform handshake"),
    local_handshake(S, SAddr, C, CAddr),

    ?P("cleanup"),
    ok = gen_udp:close(S),
    ok = gen_udp:close(S0),
    ok = gen_udp:close(C),
    %%
    ok = file:delete(SFile),
    ok = file:delete(CFile),
    ?P("done"),
    ok.

local_fdopen_unbound(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_local_fdopen_unbound(Config) end).

do_local_fdopen_unbound(Config) ->
    ?P("begin"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),

    %%
    ?P("try create (server) socket"),
    S  = ok(?OPEN(Config, 0, [{ifaddr,SAddr},{active,false}])),
    ?P("try create \"dummy\" (client) socket"),
    {C0, FD} = case ?OPEN(Config, 0, [local,{active,false}]) of
                   {ok, Sock0} when is_port(Sock0) ->
                       ?P("(port) try extract FD"),
                       Fd = ok(prim_inet:getfd(Sock0)),
                       {Sock0, Fd};
                   {ok, Sock0} -> % socket
                       ?P("(socket) try extract FD"),
                       {ok, [{fd, Fd}]} =
                           gen_udp_socket:getopts(Sock0, [fd]),
                       {Sock0, Fd}
               end,
    SAddr = ok(inet:sockname(S)),
    ?P("try create (client) socket using fd = ~w", [FD]),
    C  = ok(?OPEN(Config, 0, [{fd,Fd},local,{active,false}])),

    ?P("perform handshake"),
    local_handshake(S, SAddr, C, undefined),

    ?P("cleanup"),
    ok = gen_udp:close(S),
    ok = gen_udp:close(C),
    ok = gen_udp:close(C0),
    %%
    ok = file:delete(SFile),

    ?P("done"),
    ok.

local_abstract(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_local_abstract(Config) end).

do_local_abstract(Config) ->
    case os:type() of
	{unix,linux} ->
            ?P("create server socket"),
	    S = ok(?OPEN(Config, 0, [{ifaddr,{local,<<>>}},{active,false}])),
            ?P("create client socket"),
	    C = ok(?OPEN(Config, 0, [{ifaddr,{local,<<>>}},{active,false}])),
            ?P("verify sockname(s)"),
	    {local,_} = SAddr = ok(inet:sockname(S)),
	    {local,_} = CAddr = ok(inet:sockname(C)),
            ?P("perform handshake"),
	    local_handshake(S, SAddr, C, CAddr),
            ?P("cleanup"),
	    ok = gen_udp:close(S),
	    ok = gen_udp:close(C),
            ?P("done"),
	    ok;
	_ ->
	    {skip, "AF_LOCAL Abstract Addresses only supported on Linux"}
    end.


local_handshake(S, SAddr, C, CAddr) ->
    SData = "9876543210",
    CData = "0123456789",
    ?P("try (client) send"),
    ok = gen_udp:send(C, SAddr, 0, CData),
    ?P("try (server) recv"),
    CData1 = local_handshake_data(C, CData),
    case ok(gen_tcp:recv(S, 112)) of
	{{unspec,<<>>}, 0, CData1} when CAddr =:= undefined ->
	    ok;
	{{local,<<>>}, 0, CData1} when CAddr =:= undefined ->
	    ok;
	{CAddr, 0, CData1} when CAddr =/= undefined ->
	    ok = gen_udp:send(S, CAddr, 0, SData),
            SData1 = local_handshake_data(S, SData),
	    {SAddr, 0, SData1} = ok(gen_tcp:recv(C, 112)),
	    ok

    end.

local_handshake_data(S, Data) when is_list(Data) ->
    case inet:getopts(S, [mode]) of
        {ok,[{mode,binary}]} ->
            list_to_binary(Data);
        {ok,[{mode,list}]} ->
            Data
    end.


%%-------------------------------------------------------------
%% Open a passive socket. Create a socket that reads from it.
%% Then close the socket.
recv_close(Config) when is_list(Config) ->
    ?P("begin"),
    {ok, Sock} = ?OPEN(Config, 0, [{active, false}]),
    RECV = fun() ->
                   ?P("try recv"),
                   Res = gen_udp:recv(Sock, 0),
                   ?P("recv res: ~p", [Res]),
                   exit(Res)
           end,
    ?P("spawn reader"),
    {Pid, MRef} = spawn_monitor(RECV),
    receive
        {'DOWN', MRef, process, Pid, PreReason} ->
            %% Make sure id does not die for some other reason...
            ct:fail("Unexpected pre close from reader (~p): ~p",
                          [Pid, PreReason])
    after 5000 -> % Just in case...
            ok
    end,
    ?P("close socket"),
    ok = gen_udp:close(Sock),
    ?P("await reader termination"),
    receive
        {'DOWN', MRef, process, Pid, {error, closed}} ->
            ?P("expected reader termination result"),
            ok;
        {'DOWN', MRef, process, Pid, PostReason} ->
            ?P("unexpected reader termination: ~p", [PostReason]),
            ct:fail("Unexpected post close from reader (~p): ~p",
                          [Pid, PostReason])
    after 5000 ->
            ?P("unexpected reader termination timeout"),
            demonitor(MRef, [flush]),
            exit(Pid, kill),
            ct:fail("Reader (~p) termination timeout", [Pid])
    end,
    ?P("done"),
    ok.




%% Test that connect/3 has effect.
connect(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> do_connect(Config) end).

do_connect(Config) when is_list(Config) ->
    ?P("begin"),
    {ok, Addr} = ?LIB:which_local_addr(inet),
    ?P("try create first socket"),
    {ok, S1} = ?OPEN(Config, 0),
    {ok, P1} = inet:port(S1),
    ?P("try create second socket"),
    {ok, S2} = ?OPEN(Config, 0),
    ?P("try set second socket active: false: "
       "~n   ~p", [inet:info(S2)]),
    ok = inet:setopts(S2, [{active, false}]),
    ?P("try close first socket"),
    ok = gen_udp:close(S1),

    %% Test if this helps...
    ?P("sleep some"),
    ct:sleep({seconds, 5}),

    case os:type() of
	{win32, nt} ->
	    ok;
	_ ->
	    ?P("try some doomed connect targets: ~p", [P1]),
	    {error, nxdomain} = gen_udp:connect(S2, "", ?CLOSED_PORT),
	    {error, nxdomain} = gen_udp:connect(S2, '', ?CLOSED_PORT),
	    {error, nxdomain} = gen_udp:connect(S2, ".", ?CLOSED_PORT),
	    {error, nxdomain} = gen_udp:connect(S2, '.', ?CLOSED_PORT),
	    ok
    end,

    ?P("try connect second socket to: ~p, ~p", [Addr, P1]),
    ok = gen_udp:connect(S2, Addr, P1),
    ?P("try send on second socket"),
    ok = gen_udp:send(S2, <<16#deadbeef:32>>),
    ?P("try recv on second socket - expect failure when"
       "~n   Socket Info: ~p", [inet:info(S2)]),
    ok = case gen_udp:recv(S2, 0, 500) of
	     {error, econnrefused = R} -> ?P("expected failure: ~w", [R]), ok;
	     {error, econnreset   = R} -> ?P("expected failure: ~w", [R]), ok;
	     Other -> 
                 ?P("UNEXPECTED failure: ~p:"
                    "~n   ~p", [Other, inet:info(S2)]),
                 Other
	 end,
    ?P("done"),
    ok.



reconnect(Config) when is_list(Config) ->
    Cond = fun() -> is_not_windows() end,
    Pre  = fun() ->
                   Addr = {127, 0, 0, 1},
                   case open_port_0(Config,
                                    [{debug, true}]) of
                       {ok, {S, Port}} ->
                           ?P("[unix] Socket opened: "
                              "~n   Socket:      ~p"
                              "~n   socket info: ~p"
                              "~n   SockName:    ~s"
                              "~n   PeerName:    ~s",
                              [S, inet:info(S), sn(S), pn(S)]),
                           #{local_addr => Addr,
                             socket     => S,
                             port       => Port};
                       {error, Reason} ->
                           skip(?F("Failed open initial port: "
                                   "~p", [Reason]))
                   end
           end,
    TC   = fun(State) -> do_reconnect(State) end,
    Post = fun(#{socket := S}) -> ok = gen_udp:close(S) end,
    ?TC_TRY(?FUNCTION_NAME,
            Cond, Pre, TC, Post).

do_reconnect(#{local_addr := Addr,
               socket     := S,
               port       := Port}) ->
    XtrnAddr = {8,8,8,8},
    DestPort = 53,
    %% Connect to a local destination
    ok = gen_udp:connect(S, Addr, DestPort),
    ?P("Socket connected: "
       "~n   socket info: ~p"
       "~n   SockName:    ~s"
       "~n   PeerName:    ~s", [inet:info(S), sn(S), pn(S)]),
    {ok, {Addr,      DestPort}} = inet:peername(S),
    {ok, {LocalAddr, Port}}     = inet:sockname(S),

    %% Reconnect to external destination
    ?P("try (re-)connect to (external): ~s", [atos({XtrnAddr, DestPort})]),
    case gen_udp:connect(S, XtrnAddr, DestPort) of
        ok ->
            ok;
        {error, Reason} ->
            ?P("Failed (re-)connect to external destination:"
               "~n   Extern Dest: ~s"
               "~n   Reason:      ~p", [atos({XtrnAddr, DestPort}), Reason]),
            ct:fail({extern_reconnect, Reason})
    end,
    ?P("Socket (re-)connected: "
       "~n   socket info: ~p"
       "~n   SockName:    ~s"
       "~n   PeerName:    ~s", [inet:info(S), sn(S), pn(S)]),
    {ok, {XtrnAddr,     DestPort}} = inet:peername(S),
    {ok, {RoutableAddr, Port}}     = inet:sockname(S),

    ?P("verify socket (routable) addr: ~p", [RoutableAddr]),
    %% We should have a non-loopback address here
    true = RoutableAddr =/= LocalAddr,

    %% Reconnect to local addr
    ?P("(re-)connect to local address: ~s", [atos({Addr, DestPort})]),
    ok = gen_udp:connect(S, Addr, DestPort),
    ?P("Socket (re-)connected: "
       "~n   socket info: ~p"
       "~n   SockName:    ~s"
       "~n   PeerName:    ~s", [inet:info(S), sn(S), pn(S)]),
    {ok, {Addr,      DestPort}} = inet:peername(S),
    {ok, {LocalAddr, Port}}     = inet:sockname(S),

    ?P("done"),
    ok.

pn(S) ->
    case inet:peername(S) of
        {ok, Addr} ->
            atos(Addr);
        {error, _} ->
            "undefined"
    end.

sn(S) ->
    case inet:sockname(S) of
        {ok, Addr} ->
            atos(Addr);
        {error, _} ->
            "undefined"
    end.

atos({A, P}) when is_tuple(A) andalso is_integer(P) ->
    ?F("~s:~w", [inet_parse:ntoa(A), P]).
            
%% For Linux to keep the port when we reconnect;
%% we need to first bind to a specific port.
%% If we bind to port 0 and get an ephemeral port
%% it apparently can change when we reconnect to a different
%% destination depending on routing and interfaces.
%%
%% I consider this a workaround for a Linux bug,
%% ironically in a test case that tests
%% a workaround for another Linux bug (related)...
%%
open_port_0(Config, Opts) ->
    open_port_0(Config, 0, Opts, 10).
%%
open_port_0(Config, Port, Opts, N) ->
    case ?OPEN(Config, Port, Opts) of
        {ok, S} ->
            if
                Port =:= 0 ->
                    {ok, Port_1} = inet:port(S),
                    ok = gen_udp:close(S),
                    %% Speculate that we can open a socket with that port
                    open_port_0(Config, Port_1, Opts, N);
                true ->
                    ?P("Socket port: ~w", [Port]),
                    {ok, {S, Port}}
            end;
        {error, eaddrinuse} when Port =/= 0 ->
            open_port_0(Config, 0, Opts, N - 1);
        {error, _} = Error ->
            Error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

implicit_inet6(Config) when is_list(Config) ->
    ?TC_TRY(implicit_inet6, fun() -> do_implicit_inet6(Config) end).

do_implicit_inet6(Config) ->
    Host = ok(inet:gethostname()),
    case inet:getaddr(Host, inet6) of
	{ok, {16#fe80,0,0,0,_,_,_,_} = Addr} ->
	     ?SKIPT("Got link local IPv6 address: "
                    ++inet:ntoa(Addr));
	{ok, Addr} ->
	    implicit_inet6(Config, Host, Addr);
	{error, Reason} ->
	    ?SKIPT("Can not look up IPv6 address: "
                   ++atom_to_list(Reason))
    end.

implicit_inet6(Config, Host, Addr) ->
    Active   = {active,false},
    Loopback = {0,0,0,0,0,0,0,1},
    ?P("try 1 with explicit inet6 on loopback"),
    S1 = case ?OPEN(Config, 0, [inet6, Active, {ip, Loopback}]) of
             {ok, Sock1} ->
                 Sock1;
             {error, eaddrnotavail = Reason1} ->
                 ?SKIPT(open_failed_str(Reason1));
             _ ->
                 ?SKIPT("IPv6 not supported")
         end,
    implicit_inet6(Config, S1, Active, Loopback),
    ok = gen_udp:close(S1),

    %%
    Localaddr = ok(get_localaddr()),
    ?P("try 2 on local addr (~p)", [Localaddr]),
    S2 = case ?OPEN(Config, 0, [{ip, Localaddr}, Active]) of
             {ok, Sock2} ->
                 Sock2;
             {error, eaddrnotavail = Reason2} ->
                 ?SKIPT(open_failed_str(Reason2))
         end,
    implicit_inet6(Config, S2, Active, Localaddr),
    ok = gen_udp:close(S2),

    %%
    ?P("try 3 on addr ~p (~p)", [Addr, Host]),
    S3 = case ?OPEN(Config, 0, [{ifaddr, Addr}, Active]) of
             {ok, Sock3} ->
                 Sock3;
             {error, eaddrnotavail = Reason3} ->
                 ?SKIPT(open_failed_str(Reason3))
         end,
    implicit_inet6(Config, S3, Active, Addr),
    ok = gen_udp:close(S3),
    ok.

implicit_inet6(Config, S1, Active, Addr) ->
    ?P("get (\"local\") port number"),
    P1 = ok(inet:port(S1)),
    ?P("open \"remote\" socket"),
    S2 = case ?OPEN(Config, 0, [inet6, Active]) of
             {ok, Sock2} ->
                 Sock2;
             {error, eaddrnotavail = Reason3} ->
                 ?SKIPT(open_failed_str(Reason3))
         end,
    ?P("get (\"remote\") port number"),
    P2 = ok(inet:port(S2)),
    ?P("connect (\"remote\") socket (to ~p:~p)", [Addr, P1]),
    ok = gen_udp:connect(S2, Addr, P1),
    ?P("connect (\"local\") socket (to ~p:~p)", [Addr, P2]),
    ok = gen_udp:connect(S1, Addr, P2),
    ?P("peername of \"local\" socket"),
    {Addr,P2} = ok(inet:peername(S1)),
    ?P("peername of \"remote\" socket"),
    {Addr,P1} = ok(inet:peername(S2)),
    ?P("sockname of \"local\" socket"),
    {Addr,P1} = ok(inet:sockname(S1)),
    ?P("sockname of \"remote\" socket"),
    {Addr,P2} = ok(inet:sockname(S2)),
    ?P("send ping on \"local\" socket (to ~p:~p)", [Addr, P2]),
    %% On some platforms its allowed to specify address and port
    %% (that is; when useing sendto) *even* if the socket is connected
    %% (assuming the send destination is the same as connected destination).
    %% But on other platforms, e.g. FreeBSD, this is *not* allowed!
    %% Linux:
    %%   EISCONN
    %%      The connection-mode socket was connected already but a recipient
    %%      was specified. (Now either this error is returned, or the re-
    %%      cipient specification is ignored.)
    %% FreeBSD:
    %%   [EISCONN]    A destination address was specified and the socket is
    %%                already connected.
    case gen_udp:send(S1, Addr, P2, "ping") of
        ok ->
            ?P("recv ping on \"remote\" socket (from ~p:~p)", [Addr, P1]),
            {Addr,P1,"ping"} = ok(gen_udp:recv(S2, 1024, 1000)),
            ?P("send pong on \"remote\" socket (to ~p:~p)", [Addr, P1]),
            ok = gen_udp:send(S2, Addr, P1, "pong"),
            ?P("recv ping on \"local\" socket (from ~p:~p)", [Addr, P2]),
            {Addr,P2,"pong"} = ok(gen_udp:recv(S1, 1024)),
            ?P("close \"remote\" socket"),
            ok = gen_udp:close(S2),
            ok;
        {error, eisconn} ->
            ?P("socket is connect => *not* allowed to use sendto"),
            ok = gen_udp:send(S1, "ping"),
            %% Not allowed to specify address *at all* for a connected socket
            ?P("recv ping on \"remote\" socket (from ~p:~p)", [Addr, P1]),
            {Addr,P1,"ping"} = ok(gen_udp:recv(S2, 1024, 1000)),
            ?P("send pong on \"remote\" socket (to ~p:~p)", [Addr, P1]),
            ok = gen_udp:send(S2, "pong"),
            ?P("recv ping on \"local\" socket (from ~p:~p)", [Addr, P2]),
            {Addr,P2,"pong"} = ok(gen_udp:recv(S1, 1024)),
            ?P("close \"remote\" socket"),
            ok = gen_udp:close(S2),
            ok
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% We create a socket, then spawns processes that create
%% monitors to it...
socket_monitor1(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1,
            fun() -> do_socket_monitor1(Config) end).

do_socket_monitor1(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, Sock1} = ?OPEN(Config),
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Socket, Parent) when is_pid(Parent) ->
		 ?P("[client] create monitor"),
		 MRef = inet:monitor(Socket),
		 Parent ! {self(), ready},
		 sm_await_socket_down(MRef, Socket, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F(Sock1, F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket"),
    gen_udp:close(Sock1),
    ?P("await client termination"),
    sm_await_down(Pid1, Mon1, ok),
    ?P("done"),
    ok.

sm_await_socket_down(ExpMon, ExpSock, ExpType) ->
    sm_await_socket_down(ExpMon, ExpSock, ExpType, "client").

sm_await_socket_down(ExpMon, ExpSock, ExpType, Name) ->
    receive
	{'DOWN', Mon, Type, Sock, Info} when (Type =:= ExpType) andalso 
					     (Mon  =:= ExpMon)  andalso 
					     (Sock =:= ExpSock) ->
	    ?P("[~s] received expected (socket) down message: "
	       "~n   Mon:  ~p"
	       "~n   Type: ~p"
	       "~n   Sock: ~p"
	       "~n   Info: ~p", [Name, Mon, Type, Sock, Info]),
	    exit(ok);

	Any ->
	    ?P("[~s] received unexpected message: "
	       "~n   ~p", [Name, Any]),
	    exit({unexpected_message, Any})
    end.

sm_await_client_ready(Pid) ->
    sm_await_client_ready(Pid, "client").

sm_await_client_ready(Pid, Name) ->
    receive
	{Pid, ready} ->
	    ?P("received ~s ready", [Name])
    end.

sm_await_down(Pid, Mon, ExpRes) ->
    receive
	{'DOWN', Mon, process, Pid, ExpRes} ->
	    ?P("received expected process down message from ~p", [Pid]),
	    ok;
	{'DOWN', Mon, process, Pid, UnexpRes} ->
	    ?P("received unexpected process down message from ~p: "
	       "~n   ~p", [Pid, UnexpRes]),
	    ct:fail({unexpected_down, UnexpRes})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% We create "many" socket(s), then spawn processes that create
%% monitors to them...
socket_monitor1_manys(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1_manys,
            fun() -> do_socket_monitor1_manys(Config) end).

do_socket_monitor1_manys(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    ?P("[client] create socket(s)"),
    {ok, Sock1} = ?OPEN(Config),
    {ok, Sock2} = ?OPEN(Config),
    {ok, Sock3} = ?OPEN(Config),
    {ok, Sock4} = ?OPEN(Config),
    {ok, Sock5} = ?OPEN(Config),
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Sockets, Parent) when is_list(Sockets) andalso is_pid(Parent) ->
		 ?P("[client] create monitor(s)"),
		 Monitors = [{inet:monitor(Socket), Socket} ||
				Socket <- Sockets],
		 Parent ! {self(), ready},
		 sm_await_socket_down2(Monitors, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F([Sock1, Sock2, Sock3, Sock4, Sock5], F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket(s)"),
    gen_udp:close(Sock1),
    gen_udp:close(Sock2),
    gen_udp:close(Sock3),
    gen_udp:close(Sock4),
    gen_udp:close(Sock5),
    ?P("await client termination"),
    sm_await_down(Pid1, Mon1, ok),
    ?P("done"),
    ok.


sm_await_socket_down2(Monitors, ExpType) ->
    sm_await_socket_down2(Monitors, ExpType, "client").

sm_await_socket_down2([], _ExpType, Name) ->
    ?P("[~s] all sockets down", [Name]),
    exit(ok);
sm_await_socket_down2(Mons, ExpType, Name) when is_list(Mons) ->
    ?P("[~s] await socket down", [Name]),
    receive
	{'DOWN', Mon, Type, Sock, Info} when (Type =:= ExpType) ->
	    ?P("[~s] received expected (socket) down message: "
	       "~n   Mon:  ~p"
	       "~n   Type: ~p"
	       "~n   Sock: ~p"
	       "~n   Info: ~p", [Name, Mon, Type, Sock, Info]),
	    case lists:keysearch(Mon, 1, Mons) of
		{value, {Mon, Sock}} ->
		    Mons2 = lists:keydelete(Mon, 1, Mons),
		    sm_await_socket_down2(Mons2, ExpType, Name);
		{value, Value} ->
		    ?P("[~s] Unexpected socket down: "
		       "~n   Value: ~p", [Name, Value]),
		    ct:fail({unexpected_monitor, Mon, Value});
		false ->
		    ct:fail({unknown_monitor, Mon})
	    end
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% We create a socket, then spawn client process(es) that create
%% monitors to it...
socket_monitor1_manyc(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1_manyc,
            fun() -> do_socket_monitor1_manyc(Config) end).

do_socket_monitor1_manyc(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, Sock1}  = ?OPEN(Config),
    F  = fun(S, Fun, Name) ->
		 spawn_monitor(fun() -> Fun(S, Name, Self) end)
	 end,
    F1 = fun(Socket, Name, Parent) when is_list(Name) andalso is_pid(Parent) ->
		 ?P("[~s] monitor socket", [Name]),
		 MRef = inet:monitor(Socket),
		 Parent ! {self(), ready},
		 sm_await_socket_down(MRef, Socket, Type)
	 end,
    ?P("spawn client(s)"),
    {Pid1, Mon1} = F(Sock1, F1, "client1"),
    {Pid2, Mon2} = F(Sock1, F1, "client2"),
    {Pid3, Mon3} = F(Sock1, F1, "client3"),
    {Pid4, Mon4} = F(Sock1, F1, "client4"),
    {Pid5, Mon5} = F(Sock1, F1, "client5"),
    ?P("await client(s) ready"),
    sm_await_client_ready(Pid1, "client1"),
    sm_await_client_ready(Pid2, "client2"),
    sm_await_client_ready(Pid3, "client3"),
    sm_await_client_ready(Pid4, "client4"),
    sm_await_client_ready(Pid5, "client5"),
    ?P("close socket"),
    gen_udp:close(Sock1),
    ?P("await client(s) termination"),
    sm_await_down(Pid1, Mon1, ok),
    sm_await_down(Pid2, Mon2, ok),
    sm_await_down(Pid3, Mon3, ok),
    sm_await_down(Pid4, Mon4, ok),
    sm_await_down(Pid5, Mon5, ok),
    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% We create a socket, then spawns processes that create
%% monitors to it...
socket_monitor1_demon_after(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1_demon_after,
            fun() -> do_socket_monitor1_demon_after(Config) end).

do_socket_monitor1_demon_after(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, Sock1}  = ?OPEN(Config),
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Socket, Parent) when is_pid(Parent) ->
		 ?P("[client] create monitor"),
		 MRef = inet:monitor(Socket),
		 ?P("[client] sleep some"),
		 ?SLEEP(?SECS(1)),
		 ?P("[client] cancel (socket) monitor"),
		 inet:cancel_monitor(MRef),
		 ?P("[client] announce ready"),
		 Parent ! {self(), ready},
		 sm_await_no_socket_down(MRef, Socket, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F(Sock1, F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket"),
    gen_udp:close(Sock1),
    ?P("await client termination"),
    sm_await_down(Pid1, Mon1, ok),
    ?P("done"),
    ok.


sm_await_no_socket_down(ExpMon, ExpSock, ExpType) ->
    sm_await_no_socket_down(ExpMon, ExpSock, ExpType, "client").

sm_await_no_socket_down(ExpMon, ExpSock, ExpType, Name) ->
    receive
	{'DOWN', Mon, Type, Sock, Info} when (Type =:= ExpType) andalso 
					     (Mon  =:= ExpMon)  andalso 
					     (Sock =:= ExpSock) ->
	    ?P("[~s] received unexpected (socket) down message: "
	       "~n   Mon:  ~p"
	       "~n   Type: ~p"
	       "~n   Sock: ~p"
	       "~n   Info: ~p", [Name, Mon, Type, Sock, Info]),
	    exit({unexpected_down, Mon, Type, Sock, Info});

	Any ->
	    ?P("[~s] received unexpected message: "
	       "~n   ~p", [Name, Any]),
	    exit({unexpected_message, Any})

    after 1000 ->
	    ?P("[~s] expected message timeout", [Name]),
	    exit(ok)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% Spawn a process that creates a socket, then spawns processes
%% that create monitors to it...
socket_monitor2(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor2,
            fun() -> do_socket_monitor2(Config) end).

do_socket_monitor2(Config) ->
    ?P("begin"),
    Type         = ?SOCKET_TYPE(Config),
    Self         = self(),
    {OwnerPid, OwnerMon} =
	spawn_monitor(fun() ->
			      ?P("[owner] create (listen) socket"),
			      {ok, S} = ?OPEN(Config),
			      ?P("[owner] send socket to ctrl"),
			      Self ! {socket, S},
			      ?P("[owner] ready"),
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    Sock1 = receive
                {socket, S} ->
                    ?P("received socket from owner"),
                    S;
                {'DOWN', OwnerMon, process, OwnerPid, OwnerReason} ->
                    ?P("received unexpected owner termination: "
                       "~n   ~p", [OwnerReason]),
                    ct:fail({unexpected_owner_termination, OwnerReason})
	     end,
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Socket, Parent) when is_pid(Parent) ->
		 ?P("[client] create monitor"),
		 MRef = inet:monitor(Socket),
		 Parent ! {self(), ready},
		 sm_await_socket_down(MRef, Socket, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F(Sock1, F1),
    ?P("spawn client"),
    sm_await_client_ready(Pid1),
    ?P("kill owner"),
    exit(OwnerPid, kill),
    ?P("await owner termination"),
    sm_await_down(OwnerPid, OwnerMon, killed),
    ?P("await client termination"),
    sm_await_down(Pid1, Mon1, ok),
    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% Spawn a process that creates "many" socket(s), then spawns
%% a process that create monitors to them...

socket_monitor2_manys(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor2_manys,
            fun() -> do_socket_monitor2_manys(Config) end).

do_socket_monitor2_manys(Config) ->
    ?P("begin"),
    Type         = ?SOCKET_TYPE(Config),
    Self         = self(),
    ?P("spawn owner"),
    {OwnerPid, OwnerMon} =
	spawn_monitor(fun() ->
			      ?P("[owner] create (listen) socket(s)"),
			      {ok, S1} = ?OPEN(Config),
			      {ok, S2} = ?OPEN(Config),
			      {ok, S3} = ?OPEN(Config),
			      {ok, S4} = ?OPEN(Config),
			      {ok, S5} = ?OPEN(Config),
			      ?P("[owner] send (listen) socket(s) to ctrl"),
			      Self ! {socket, [S1, S2, S3, S4, S5]},
			      ?P("[owner] ready"),
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    ?P("await sockets (from owner)"),
    Socks = receive
                {socket, Ss} ->
                    ?P("received socket(s) from owner"),
                    Ss;
                {'DOWN', OwnerMon, process, OwnerPid, OwnerReason} ->
                    ?P("received unexpected owner termination: "
                       "~n   ~p", [OwnerReason]),
                    ct:fail({unexpected_owner_termination, OwnerReason})
            end,
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Sockets, Parent) when is_list(Sockets) andalso is_pid(Parent) ->
		 ?P("[client] create monitor(s)"),
		 Monitors = [{inet:monitor(Socket), Socket} ||
				Socket <- Sockets],
		 ?P("[client] announce ready"),
		 Parent ! {self(), ready},
		 sm_await_socket_down2(Monitors, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F(Socks, F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("kill owner"),
    exit(OwnerPid, kill),
    ?P("await owner (~p) termination", [OwnerPid]),
    sm_await_down(OwnerPid, OwnerMon, killed),
    ?P("await client termination"),
    sm_await_down(Pid1, Mon1, ok),
    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% Spawn a process that creates a socket, then spawns (client)
%% processes that create monitors to it...
socket_monitor2_manyc(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor2_manyc,
            fun() -> do_socket_monitor2_manyc(Config) end).

do_socket_monitor2_manyc(Config) ->
    ?P("begin"),
    Type         = ?SOCKET_TYPE(Config),
    Self         = self(),
    {OwnerPid, OwnerMon} =
	spawn_monitor(fun() ->
			      {ok, S} = ?OPEN(Config),
			      Self ! {socket, S},
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    Sock1 = receive
		 {socket, S} ->
		     ?P("received socket from owner"),
		     S;
		 {'DOWN', OwnerMon, process, OwnerPid, OwnerReason} ->
		     ?P("received unexpected owner termination: "
			"~n   ~p", [OwnerReason]),
		     ct:fail({unexpected_owner_termination, OwnerReason})
	     end,
    F  = fun(S, Fun, Name) ->
		 spawn_monitor(fun() -> Fun(S, Name, Self) end)
	 end,
    F1 = fun(Socket, Name, Parent) when is_list(Name) andalso is_pid(Parent) ->
		 ?P("[~s] create monitor", [Name]),
		 MRef = inet:monitor(Socket),
		 Parent ! {self(), ready},
		 sm_await_socket_down(MRef, Socket, Type, Name)
	 end,
    ?P("spawn client(s)"),
    {Pid1, Mon1} = F(Sock1, F1, "client1"),
    {Pid2, Mon2} = F(Sock1, F1, "client2"),
    {Pid3, Mon3} = F(Sock1, F1, "client3"),
    {Pid4, Mon4} = F(Sock1, F1, "client4"),
    {Pid5, Mon5} = F(Sock1, F1, "client5"),
    ?P("await client(s) ready"),
    sm_await_client_ready(Pid1, "client1"),
    sm_await_client_ready(Pid2, "client2"),
    sm_await_client_ready(Pid3, "client3"),
    sm_await_client_ready(Pid4, "client4"),
    sm_await_client_ready(Pid5, "client5"),
    ?P("kill owner"),
    exit(OwnerPid, kill),
    ?P("await owner termination"),
    sm_await_down(OwnerPid, OwnerMon, killed),
    ?P("await client(s) termination"),
    sm_await_down(Pid1, Mon1, ok),
    sm_await_down(Pid2, Mon2, ok),
    sm_await_down(Pid3, Mon3, ok),
    sm_await_down(Pid4, Mon4, ok),
    sm_await_down(Pid5, Mon5, ok),
    ?P("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% Spawn a process that creates a socket, then spawns (client)
%% processes that create monitors to it...
otp_17492(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(otp_17492, fun() -> do_otp_17492(Config) end).

do_otp_17492(Config) ->
    ?P("begin"),

    Self = self(),

    ?P("try create socket"),
    {ok, L} = ?OPEN(Config, 0, []),

    ?P("try get (created) socket info"),
    try inet:info(L) of
	#{owner := Owner} = Info when is_pid(Owner) andalso (Owner =:= Self) ->
	    ?P("(created) socket info: ~p", [Info]);
	OBadInfo ->
	    ?P("(created) socket info: ~p", [OBadInfo]),
	    (catch gen_udp:close(L)),
	    ct:fail({invalid_created_info, OBadInfo})
    catch
	OC:OE:OS ->
	    ?P("Failed get (created) Listen socket info: "
	       "~n   Class: ~p"
	       "~n   Error: ~p"
	       "~n   Stack: ~p", [OC, OE, OS]),
	    (catch gen_udp:close(L)),
	    ct:fail({unexpected_created_info_result, {OC, OE, OS}})
    end,

    ?P("try close socket"),
    ok = gen_udp:close(L),

    ?P("try get (closed) socket info"),
    try inet:info(L) of
	#{states := [closed]} = CInfo when is_port(L) ->
	    ?P("(closed) socket info: "
	       "~n   ~p", [CInfo]);
	#{rstates := [closed], wstates := [closed]} = CInfo ->
	    ?P("(closed) socket info: "
	       "~n   ~p", [CInfo]);
	CBadInfo ->
	    ?P("(closed) socket info: ~p", [CBadInfo]),
	    ct:fail({invalid_closed_info, CBadInfo})
    catch
	CC:CE:CS ->
	    ?P("Failed get (closed) socket info: "
	       "~n   Class: ~p"
	       "~n   Error: ~p"
	       "~n   Stack: ~p", [CC, CE, CS]),
	    (catch gen_udp:close(L)),
	    ct:fail({unexpected_closed_info_result, {CC, CE, CS}})
    end,

    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.

%% Here we use socket:sockaddr_in6() when creating and using the
%% socket(s).
%%
t_simple_local_sockaddr_in6_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> ?LIB:has_support_ipv6() end,
            fun() ->
                    Domain = inet6,
                    LocalAddr =
                        case ?LIB:which_local_addr(Domain) of
                            {ok, LA} ->
                                LA;
                        {error, _} ->
                            skip("No local address")
                    end,
                    SockAddr = #{family   => Domain,
                                 addr     => LocalAddr,
                                 port     => 0},
                    do_simple_sockaddr_send_recv(SockAddr, Config)
            end).


t_simple_link_local_sockaddr_in6_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() ->
                    ?LIB:has_support_ipv6(),
                    is_net_supported(),
                    is_not_darwin()
            end,
            fun() ->
                    Domain = inet6,
                    LinkLocalAddr =
                        case ?LIB:which_link_local_addr(Domain) of
                            {ok, LLA} ->
                                LLA;
                            {error, _} ->
                                skip("No link local address")
                        end,
                    Filter =
                        fun(#{addr := #{family := D,
                                        addr   := A}} = C) ->
                                if 
                                    (D =:= Domain) andalso
                                    (A =:= LinkLocalAddr) ->
                                        ?P("found link-local candidate: "
                                           "~n   ~p", [C]),
                                        true;
                                    true ->
                                        false
                                end;
                           (_) ->
                                false
                        end,
                    case net:getifaddrs(Filter) of
                        {ok, [#{addr := #{scope_id := ScopeID}}=H|T]} ->
                            ?P("found link-local candidate(s): "
                               "~n   Candidate:       ~p"
                               "~n   Rest Candidate:  ~p", [H, T]),
                            SockAddr = #{family   => Domain,
                                         addr     => LinkLocalAddr,
                                         port     => 0,
                                         scope_id => ScopeID},
                            do_simple_sockaddr_send_recv(SockAddr, Config);
                        {ok, _} ->
                            skip("Scope ID not found");
                        {error, R} ->
                            skip({failed_getifaddrs, R})
                    end
            end).

t_simple_local_sockaddr_in_send_recv(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> ok end,
            fun() ->
                    Domain = inet,
                    LocalAddr =
                        case ?LIB:which_local_addr(Domain) of
                            {ok, LA} ->
                                LA;
                        {error, _} ->
                            skip("No local address")
                    end,
                    SockAddr = #{family   => Domain,
                                 addr     => LocalAddr,
                                 port     => 0},
                    do_simple_sockaddr_send_recv(SockAddr, Config)
            end).

t_simple_link_local_sockaddr_in_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> ok end,
            fun() ->
                    Domain = inet,
                    LinkLocalAddr =
                        case ?LIB:which_link_local_addr(Domain) of
                            {ok, LLA} ->
                                LLA;
                            {error, _} ->
                                skip("No link local address")
                        end,
                    SockAddr = #{family => Domain,
                                 addr   => LinkLocalAddr,
                                 port   => 0},
                    do_simple_sockaddr_send_recv(SockAddr, Config)
            end).


do_simple_sockaddr_send_recv(#{family := _Fam} = SockAddr, _) ->
    %% Create the server
    Self   = self(),
    ?P("~n      SockAddr: ~p", [SockAddr]),
    ServerF = fun() ->
                      ?P("[server] try create socket"),
                      Sock =
                          try gen_udp:open(0, [{ifaddr, SockAddr},
                                               {active, true},
                                               binary]) of
                              {ok, S} ->
                                  S;
                              {error, OReason} ->
                                  ?P("[server] open error: "
                                     "~n      Reason: ~p", [OReason]),
                                  exit({open_error, OReason})
                          catch
                              OC:OE:OS ->
                                  ?P("[server] open failure: "
                                     "~n      Error Class: ~p"
                                     "~n      Error:       ~p"
                                     "~n      Call Stack:  ~p", [OC, OE, OS]),
                                  exit({open_failure, {OC, OE, OS}})
                          end,
                      ?P("[server] try get port"),
                      {ok, Port}  = inet:port(Sock),
                      ?P("[server] port: ~w", [Port]),
                      Self ! {{port, Port}, self()},


                      %% --- message sequance 1 ---

                      ?P("[server] await message 1"),
                      {CIP, CPort} =
                          receive
                              {udp, Sock, CIP1, CPort1, <<"hej">>} ->
                                  ?P("[server] received expected message 1 - "
                                     "connect to *this* client"),
                                  ok = gen_udp:connect(Sock, CIP1, CPort1),
                                  ?P("[server] send reply"),
                                  case gen_udp:send(Sock, "hopp") of
                                      ok -> 
                                          {CIP1, CPort1};
                                      {error, ehostunreach = Reason1} ->
                                          ?P("[server] send failed: ~p",
                                             [Reason1]),
                                          exit({skip, Reason1});
                                      {error, enetunreach = Reason1} ->
                                          ?P("[server] send failed: ~p",
                                             [Reason1]),
                                          exit({skip, Reason1});
                                      {error, Reason1} ->
                                          exit({send_failed, Reason1})
                                  end
                          after 5000 ->
                                  ?P("[server] receive (1) timeout:"
                                     "~n      ~p", [mq()]),
                                  exit(receive_timeout)
                          end,


                      %% --- message sequance 2 ---

                      ?P("[server] await message 2"),
                      receive
                          {udp, Sock, CIP2, CPort2, <<"hej">>}
                            when (CIP2 =:= CIP) andalso (CPort2 =:= CPort) ->
                              ?P("[server] received expected message 2 - "
                                 "send reply"),
                              case gen_udp:send(Sock, "hopp") of
                                  ok -> 
                                      ok;
                                  {error, ehostunreach = Reason2} ->
                                      ?P("[server] send failed: ~p",
                                         [Reason2]),
                                      exit({skip, Reason2});
                                  {error, enetunreach = Reason2} ->
                                      ?P("[server] send failed: ~p",
                                         [Reason2]),
                                      exit({skip, Reason2});
                                  {error, Reason2} ->
                                      exit({send_failed, Reason2})
                              end
                      after 5000 ->
                              ?P("[server] receive (2) timeout:"
                                 "~n      ~p", [mq()]),
                              exit(receive_timeout)
                      end,


                      %% --- message sequance 3 ---

                      ?P("[server] await message 3"),
                      receive
                          {udp, Sock, CIP3, CPort3, <<"hej">>}
                            when (CIP3 =:= CIP) andalso (CPort3 =:= CPort) ->
                              ?P("[server] received expected message 3 - "
                                 "send reply"),
                              ok = gen_udp:send(Sock, "hopp")
                      after 5000 ->
                              ?P("[server] receive (3) timeout:"
                                 "~n      ~p", [mq()]),
                              exit(receive_timeout)
                      end,


                      %% --- message sequance 4 ---

                      ?P("[server] await message 4 - should be none!"),
                      receive
                          {udp, Sock, CIP4, CPort4, <<"hej">>} ->
                              ?P("[server] received unexpected message 4:"
                                 "~n      Address: ~p"
                                 "~n      Port:    ~p", [CIP4, CPort4]),
                              exit({unexpected_message, CIP4, CPort4})
                      after
                          1000 ->
                              ?P("Received nothing - expected"),
                              Self ! {nothing, self()}
                      end,

                      ?P("[server] await termination command"),
                      receive
                          {die, Self} ->
                              ?P("[server] terminating"),
                              (catch gen_udp:close(Sock)),
                              exit(normal)
                      end
              end,
    ?P("try start server"),
    Server = spawn_link(ServerF),
    ?P("server started - await port "),
    ServerPort = receive
                     {{port, Port}, Server} ->
                         Port;
                     {'EXIT', Server, Reason} ->
                         ?P("server died unexpectedly: "
                            "~n      ~p", [Reason]),
                         exit({unexpected_server_failure, Reason})
                 end,
    ?P("server port received: ~p", [ServerPort]),
    
    ?P("try connect to server"),
    ServerSockAddr = SockAddr#{port => ServerPort},
    {ok, CSock1} = gen_udp:open(0,
                                [{ifaddr, SockAddr},
                                 {active, true},
                                 binary]),
    {ok, CSock2} = gen_udp:open(0,
                                [{ifaddr, SockAddr},
                                 {active, true},
                                 binary]),
    ?P("client socket: "
       "~n      CSock 1: ~p"
       "~n      CPort 1: ~p"
       "~n      CSock 2: ~p"
       "~n      CPort 2: ~p",
       [CSock1, inet:port(CSock1), CSock2, inet:port(CSock2)]),


    %% --- message sequance 1 ---

    ?P("[csock 1] try (trad = address and port) send message 1"),
    case gen_udp:send(CSock1, maps:get(addr, SockAddr), ServerPort, "hej") of
        ok ->
            ok;
        {error, ehostunreach = Reason1} ->
            ?SKIPT(?F("send (1,1) failed: ~p", [Reason1]));
        {error, Reason1} ->
            ct:fail({send_failed, Reason1})
    end,
                 

    ?P("[csock 1] await reply message 1"),
    receive
        {udp, CSock1, _, _, <<"hopp">>} ->
            ?P("[csock 1] received expected reply message 1"),
            ok;

        {'EXIT', Server, {skip, SReason1}} ->
            ?P("received unexpected server skip exit (1):"
               "~n      ~p", [SReason1]),
            ?SKIPT(?F("server send (1,1) failed: ~p", [SReason1]));

        {'EXIT', Server, SReason1} ->
            ?P("received unexpected server exit (1):"
               "~n      ~p", [SReason1]),
            ct:fail({unexpected_server_exit, 1, SReason1})

    after 5000 ->
            ?P("receive (1) timeout:"
               "~n      ~p", [mq()]),
            ct:fail(receive_timeout)
    end,


    %% --- message sequance 2 ---

    ?P("[csock 1] try (sockaddr) send message 2"),
    %% DstSockAddr = #{family => maps:get(family, SockAddr),
    %%                 addr   => maps:get(addr, SockAddr),
    %%                 port   => ServerPort},
    DstSockAddr = ServerSockAddr,
    case gen_udp:send(CSock1, DstSockAddr, "hej") of
        ok ->
            ok;
        {error, ehostunreach = Reason2} ->
            ?SKIPT(?F("send (1,2) failed: ~p", [Reason2]));
        {error, Reason2} ->
            ct:fail({send_failed, Reason2})
    end,
        

    ?P("[csock 1] await reply message 2"),
    receive
        {udp, CSock1, _, _, <<"hopp">>} ->
            ?P("[csock 1] received expected reply message 2"),
            ok;

        {'EXIT', Server, {skip, SReason2}} ->
            ?P("received unexpected server skip exit (2):"
               "~n      ~p", [SReason2]),
            ?SKIPT(?F("server send (1,2) failed: ~p", [SReason2]));

        {'EXIT', Server, SReason2} ->
            ?P("received unexpected server exit (2):"
               "~n.     ~p", [SReason2]),
            ct:fail({unexpected_server_exit, 2, SReason2})

    after 5000 ->
            ?P("[csock 1] receive (2) timeout:"
               "~n      ~p", [mq()]),
            ct:fail(receive_timeout)
    end,


    %% --- message sequance 3 ---

    ?P("[csock 1] try connect to: "
       "~n      ~p", [ServerSockAddr]),
    ok = gen_udp:connect(CSock1, DstSockAddr),

    ?P("[csock 1] try send message 3"),
    ok = gen_udp:send(CSock1, "hej"),

    ?P("[csock 1] await reply message 3"),
    receive
        {udp, CSock1, _, _, <<"hopp">>} ->
            ?P("received expected reply message 3"),
            ok;

        {'EXIT', Server, SReason3} ->
            ?P("received unexpected server exit (3):"
               "~n.     ~p", [SReason3]),
            ct:fail({unexpected_server_exit, 3, SReason3})

    after 5000 ->
            ?P("[csock 1] receive (2) timeout:"
               "~n      ~p", [mq()]),
            ct:fail(receive_timeout)

    end,


    %% --- message sequance 4 ---

    ?P("[csock 2] try (sockaddr) send message 4"),
    ok = gen_udp:send(CSock2, DstSockAddr, "hej"),

    ?P("[csock 2] await reply message 4 - expect failure"),
    receive
        {udp, CSock2, _, _, <<"hopp">>} ->
            ?P("[csock 2] received unexpected reply message 4"),
            exit(received_unexpected_message);
        {nothing, Server} ->
            ?P("[csock 2] server received nothing - expected"),
            ok
    end,


    ?P("terminate server"),
    Server ! {die, self()},

    ?P("await server termination"),
    receive
        {'EXIT', Server, normal} ->
            ok
    end,
    
    ?P("cleanup"),
    (catch gen_udp:close(CSock1)),
    (catch gen_udp:close(CSock2)),

    ?P("done"),
    ok.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Verify that the options [add|drop]_membership do not mess up
%% the options (including 'ip' which could not be added *after*).
%% This just attempts to very that the option processing is ok.
otp_18323_opts_processing(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_otp_18323_opts_processing(Config) end).

do_otp_18323_opts_processing(_Config) ->
    ?P("begin"),

    do_otp_18323_opts_processing_verify(
      {add_membership,  {{239,1,2,3},{0,0,0,0}}}),

    do_otp_18323_opts_processing_verify(
      {drop_membership, {{239,1,2,3},{0,0,0,0}}}),

    ?P("done"),
    ok.

do_otp_18323_opts_processing_verify(MembershipOpt) ->
    Port   = 4321,
    RecBuf = 123456,
    Active = 10,
    IP     = {1,2,3,4},
    Opts   = [binary, MembershipOpt, {ip, IP}, {active, Active}],

    case inet:udp_options([{port, Port}, {recbuf, RecBuf} | Opts], inet_udp) of
        {ok, #udp_opts{ifaddr = IP,
                       port   = Port,
                       opts   = SockOpts}} ->
            ?P("Processed Socket Options: "
               "~n   IfAddr:    ~p"
               "~n   Port:      ~p"
               "~n   Sock Opts: ~p", [IP, Port, SockOpts]),
            %% Check that the recbuf and mode options are as expected
            %% The option 'binary' is shorthand for {mode, binary}
            {value, {recbuf, RecBuf}} = lists:keysearch(recbuf, 1, SockOpts),
            {value, {mode,   binary}} = lists:keysearch(mode,   1, SockOpts),
            {value, {active, Active}} = lists:keysearch(active, 1, SockOpts),
            ok;
        {error, Reason} ->
            exit(?F("Failed processing options: ~p", [Reason]))
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Verify that the options [add|drop]_membership do not mess up
%% the options (including 'ip' which could not be added *after*).
otp_18323_open(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    Pre  = fun() ->
                   {ok, Addr} = ?LIB:which_local_addr(inet),
                   #{local_addr => Addr}
           end,
    Case = fun(State) -> do_otp_18323_open(State) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME, Pre, Case, Post).

do_otp_18323_open(#{local_addr := Addr}) ->
    ?P("begin"),

    ROpts = [binary,
             {add_membership, {Addr,{0,0,0,0}}},
             {ip, Addr},
             {active,false},
             {debug, true}],
    SOpts = [{reuseaddr, true}, binary],

    ?P("create received socket"),
    {ok, R}     = gen_udp:open(0, ROpts),
    ?P("extract received socket port"),
    {ok, RPort} = inet:port(R),

    ?P("create sender socket"),
    {ok, S} = gen_udp:open(0, SOpts),

    ?P("send to receiver (at port ~w)", [RPort]),
    ok = gen_udp:send(S, Addr, RPort, <<"aaaaa">>),

    ?P("attempt to receive data on specified format binary)"),
    {ok, {_,_,<<"aaaaa">>}} = gen_udp:recv(R, 0, 200),

    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ok({ok,V}) -> V;
ok(NotOk) ->
    try throw(not_ok)
    catch
	throw:not_ok:Stacktrace ->
	    raise_error({not_ok, NotOk}, tl(Stacktrace))
    end.

raise_error(Reason, Stacktrace) ->
    erlang:raise(error, Reason, Stacktrace).

local_filename(Tag) ->
    "/tmp/" ?MODULE_STRING "_" ++ os:getpid() ++ "_" ++ atom_to_list(Tag).

bin_filename(String) ->
    unicode:characters_to_binary(String, file:native_name_encoding()).

delete_local_filenames() ->
    _ =
	[file:delete(F) ||
	    F <-
		filelib:wildcard(
		  "/tmp/" ?MODULE_STRING "_" ++ os:getpid() ++ "_*")],
    ok.

get_localaddr() ->
    get_localaddr(["localhost", "localhost6", "ip6-localhost"]).

get_localaddr([]) ->
    {error, localaddr_not_found};
get_localaddr([Localhost|Ls]) ->
    case inet:getaddr(Localhost, inet6) of
       {ok, LocalAddr} ->
           ?P("found local address: ~s ~p", [Localhost, LocalAddr]),
           {ok, LocalAddr};
       _ ->
           get_localaddr(Ls)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_net_supported() ->
    try net:info() of
        #{} ->
            ok
    catch
        error : notsup ->
            not_supported(net)
    end.


is_not_darwin() ->
    is_not_platform(darwin, "Darwin").

is_not_windows() ->
    is_not_platform(win32, "Windows").

is_not_platform(Platform, PlatformStr)
  when is_atom(Platform) andalso is_list(PlatformStr) ->
      case os:type() of
        {unix, Platform} ->
            skip("This does not work on " ++ PlatformStr);
        {win32, nt} when (Platform =:= win32) ->
            skip("This does not work on " ++ PlatformStr);
        _ ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_supported(What) ->
    skip({not_supported, What}).

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

which_info(Sock) ->
    which_info([istate, active], inet:info(Sock), #{}).

which_info([], _Info, Acc) ->
    Acc;
which_info([Key|Keys], Info, Acc) ->
    case maps:find(Key, Info) of
        {ok, Value} ->
            which_info(Keys, Info, Acc#{Key => Value});
        error ->
            which_info(Keys, Info, Acc)
    end.


mq() ->
    pi(messages).

pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Utils
%%

open_failed_str(Reason) ->
    ?F("Open failed: ~w", [Reason]).
