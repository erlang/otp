%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2024. All Rights Reserved.
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
-include("kernel_test_lib.hrl").

-export([
	 all/0, suite/0, groups/0,
	 init_per_suite/1, end_per_suite/1, 
	 init_per_group/2, end_per_group/2, 
	 init_per_testcase/2, end_per_testcase/2,

	 t_connect_timeout/1, t_accept_timeout/1,
	 t_connect_src_port/1, t_connect_bad/1,
	 t_recv_timeout/1, t_recv_eof/1, t_recv_delim/1,
	 t_shutdown_write/1, t_shutdown_both/1, t_shutdown_error/1,
	 t_shutdown_async/1,
	 t_fdopen/1, t_fdconnect/1, t_implicit_inet6/1,
	 t_local_basic/1, t_local_unbound/1, t_local_fdopen/1,
	 t_local_fdopen_listen/1, t_local_fdopen_listen_unbound/1,
	 t_local_fdopen_connect/1, t_local_fdopen_connect_unbound/1,
	 t_local_abstract/1, t_accept_inet6_tclass/1,
	 s_accept_with_explicit_socket_backend/1,

         t_simple_local_sockaddr_in_send_recv/1,
         t_simple_link_local_sockaddr_in_send_recv/1,
         t_simple_local_sockaddr_in6_send_recv/1,
         t_simple_link_local_sockaddr_in6_send_recv/1
	]).

-export([getsockfd/0, closesockfd/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite() ->
    [
     {ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}
    ].

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
             {group, inet_backend_socket},
             {group, s_misc}
            ];
        _ ->
            [
             {group, inet_backend_default},
             {group, s_misc}
            ]
    end.

groups() -> 
    [
     {inet_backend_default, [], inet_backend_default_cases()},
     {inet_backend_inet,    [], inet_backend_inet_cases()},
     {inet_backend_socket,  [], inet_backend_socket_cases()},
     {t_accept,             [], t_accept_cases()},
     {t_connect,            [], t_connect_cases()},
     {t_recv,               [], t_recv_cases()},
     {t_shutdown,           [], t_shutdown_cases()},
     {t_misc,               [], t_misc_cases()},
     {sockaddr,             [], sockaddr_cases()},
     {t_local,              [], t_local_cases()},
     {s_misc,               [], s_misc_cases()}
    ].

inet_backend_default_cases() ->
    [
     {group, t_accept},
     {group, t_connect},
     {group, t_recv},
     {group, t_shutdown},
     {group, t_misc},
     {group, t_local}
    ].

inet_backend_inet_cases() ->
    inet_backend_default_cases().

inet_backend_socket_cases() ->
    inet_backend_default_cases().

t_accept_cases() ->
    [
     t_accept_timeout
    ].

t_connect_cases() ->
    [
     t_connect_timeout,
     t_connect_src_port,
     t_connect_bad
    ].

t_recv_cases() ->
    [
     t_recv_timeout,
     t_recv_eof,
     t_recv_delim
    ].

t_shutdown_cases() ->
    [
     t_shutdown_write,
     t_shutdown_both,
     t_shutdown_error,
     t_shutdown_async
    ].

t_misc_cases() ->
    [
     t_fdopen,
     t_fdconnect,
     t_implicit_inet6,
     t_accept_inet6_tclass,
     {group, sockaddr}
    ].

sockaddr_cases() ->
    [
     t_simple_local_sockaddr_in_send_recv,
     t_simple_link_local_sockaddr_in_send_recv,
     t_simple_local_sockaddr_in6_send_recv,
     t_simple_link_local_sockaddr_in6_send_recv
    ].

t_local_cases() ->
    [
     t_local_basic,
     t_local_unbound,
     t_local_fdopen,
     t_local_fdopen_listen,
     t_local_fdopen_listen_unbound,
     t_local_fdopen_connect,
     t_local_fdopen_connect_unbound,
     t_local_abstract
    ].

s_misc_cases() ->
    [
     s_accept_with_explicit_socket_backend
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
init_per_group(t_local = _GroupName, Config) ->
    case ?IS_SOCKET_BACKEND(Config) of
	true ->
	    case ?LIB:has_support_unix_domain_socket() of
		true ->
		    Config;
		false ->
		    {skip, "AF_LOCAL not supported"}
	    end;
	_ ->
	    try gen_tcp:connect({local,<<"/">>}, 0, []) of
		{error, eafnosupport} ->
		    {skip, "AF_LOCAL not supported"};
		{error,_} ->
		    Config
	    catch
		_C:_E:_S ->
		    {skip, "AF_LOCAL not supported"}
	    end
    end;
init_per_group(sockaddr = _GroupName, Config) ->
    case is_socket_supported() of
        ok ->
            Config;
        {skip, _} = SKIP ->
            SKIP
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(t_local, _Config) ->
    delete_local_filenames();
end_per_group(_, _Config) ->
    ok.


init_per_testcase(Func, Config)
  when Func =:= undefined -> % Insert your testcase name here
    dbg:tracer(),
    dbg:p(self(), c),
    dbg:tpl(prim_inet, cx),
    dbg:tpl(local_tcp, cx),
    dbg:tpl(inet, cx),
    dbg:tpl(gen_tcp, cx),
    Config;
init_per_testcase(_Func, Config) ->
    ?P("init_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), pi(links), pi(monitors)]),

    kernel_test_global_sys_monitor:reset_events(),

    ?P("init_per_testcase -> done when"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), pi(links), pi(monitors)]),
    Config.

end_per_testcase(Func, _Config)
  when Func =:= undefined -> % Insert your testcase name here
    dbg:stop();
end_per_testcase(_Func, Config) ->
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

%%% gen_tcp:accept/1,2


%% Test that gen_tcp:accept/2 (with timeout) works.
t_accept_timeout(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            throw({skip, Reason})
                    end
           end,
    TC   = fun(Addr) -> do_accept_timeout(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_accept_timeout(Config, Addr) ->
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}]),
    timeout({gen_tcp, accept, [L, 200]}, 0.2, 1.0).

%%% gen_tcp:connect/X


%% Test that gen_tcp:connect/4 (with timeout) works.
t_connect_timeout(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            throw({skip, Reason})
                    end
           end,
    TC   = fun(Addr) -> do_connect_timeout(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_connect_timeout(Config, Addr)->
    %%BadAddr = {134,138,177,16},
    %%TcpPort = 80,
    {ok, BadAddr} =  unused_ip(),
    TcpPort = 45638,
    ok = ?P("Connecting to ~p, port ~p", [BadAddr, TcpPort]),
    connect_timeout({gen_tcp, connect,
		     [BadAddr,TcpPort,
		      ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}], 200]},
		    0.2, 5.0).


%% Test that setting only the source port for a connection works.
t_connect_src_port(Config) when is_list(Config) ->
    Timeout = 1000,
    Loopback = {127,0,0,1},
    %% Allocate a port to later use as source port
    {ok, Tmp} = gen_tcp:listen(0, [{ip,Loopback}, {linger,{true,0}}]),
    {ok, SrcPort} = inet:port(Tmp),
    io:format("SrcPort = ~w~n", [SrcPort]),
    {ok, L} = gen_tcp:listen(0, [{ip,Loopback}]),
    ok = gen_tcp:close(Tmp),
    {ok, DstPort} = inet:port(L),
    io:format("DstPort = ~w~n", [DstPort]),
    ConnectOpts = [{port,SrcPort}, {linger,{true,0}}],
    {ok, C} = gen_tcp:connect(Loopback, DstPort, ConnectOpts, Timeout),
    {ok, A} = gen_tcp:accept(L, Timeout),
    {ok, {_, SrcPort}} = inet:peername(A),
    ok = gen_tcp:close(L),
    ok = gen_tcp:close(C),
    ok = gen_tcp:close(A).


%% Test that gen_tcp:connect/3 handles non-existings hosts, and other
%% invalid things.
t_connect_bad(Config) when is_list(Config) ->
    NonExistingPort = 45638,		% Not in use, I hope.
    t_connect_bad(Config, localhost, NonExistingPort, "port not in use"),
    t_connect_bad(Config, "non-existing-host-xxx", 7, "non-existing host"),
    element(1, os:type()) =:= unix andalso
        begin
            t_connect_bad(Config, "",              7, "empty host string"),
            t_connect_bad(Config, '',              7, "empty host atom")
        end,
    t_connect_bad(Config, ".",                     7, "root domain string"),
    t_connect_bad(Config, '.',                     7, "root domain atom").

t_connect_bad(Config, Host, Port, Descr) ->
    {error, Reason} =
        gen_tcp:connect(Host, Port,?INET_BACKEND_OPTS(Config)),
    io:format(
      "Error for connection attempt to " ++ Descr ++ ": ~p~n",
      [Reason]),
    ok.


%%% gen_tcp:recv/X


%% Test that gen_tcp:recv/3 (with timeout works).
t_recv_timeout(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_recv_timeout(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_recv_timeout(Config, Addr) ->
    {ok, L}      = gen_tcp:listen(0,
				  ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}]),
    {ok, Port}   = inet:port(L),
    {ok, Client} = gen_tcp:connect(Addr, Port,
                                   ?INET_BACKEND_OPTS(Config) ++
                                       [{ip, Addr}, {active, false}]),
    {ok, _A}     = gen_tcp:accept(L),
    timeout({gen_tcp, recv, [Client, 0, 200]}, 0.2, 5.0).

%% Test that end of file on a socket is reported correctly.
t_recv_eof(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_recv_eof(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_recv_eof(Config, Addr) ->
    {ok, L}      = gen_tcp:listen(0,
				  ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}]),
    {ok, Port}   = inet:port(L),
    {ok, Client} = gen_tcp:connect(Addr, Port,
                                   ?INET_BACKEND_OPTS(Config) ++
                                       [{ip, Addr}, {active, false}]),
    {ok, A}         = gen_tcp:accept(L),
    ok              = gen_tcp:close(A),
    {error, closed} = gen_tcp:recv(Client, 0),
    ok.

%% Test using message delimiter $X.
t_recv_delim(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_recv_delim(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_recv_delim(Config, Addr) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}]),
    {ok, Port} = inet:port(L),
    Opts = ?INET_BACKEND_OPTS(Config) ++
        [{ip, Addr}, {active,false}, {packet,line}, {line_delimiter,$X}],
    {ok, Client} = gen_tcp:connect(Addr, Port, Opts),
    {ok, A} = gen_tcp:accept(L),

    ?P("send the data"),
    ok = gen_tcp:send(A, "abcXefgX"),

    %% Why do we need a timeout?
    %% Sure, normally there would be no delay,
    %% but this testcase has nothing to do with timeouts?
    ?P("read the first chunk"),
    {ok, "abcX"} = gen_tcp:recv(Client, 0), % 200),
    ?P("read the second chunk"),
    {ok, "efgX"} = gen_tcp:recv(Client, 0), % 200),

    ?P("set active = 2"),
    ok = inet:setopts(Client, [{active,2}]),

    ?P("send the data again"),
    ok = gen_tcp:send(A, "abcXefgX"),

    ?P("await the first chunk"),
    receive {tcp, Client, "abcX"} -> ?P("received first chunk") end,
    ?P("await the second chunk"),
    receive {tcp, Client, "efgX"} -> ?P("received second chunk") end,

    ?P("cleanup"),
    ok = gen_tcp:close(Client),
    ok = gen_tcp:close(A),
    ?P("done"),
    ok.

%%% gen_tcp:shutdown/2

t_shutdown_write(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_shutdown_write(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_shutdown_write(Config, Addr) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++
				 [{ip, Addr}]),
    {ok, Port} = inet:port(L),
    ?P("create connect socket (C)"),
    {ok, C} = gen_tcp:connect(Addr, Port,
                              ?INET_BACKEND_OPTS(Config) ++
                                  [{ip, Addr}, {active, false}]),
    ?P("create accept socket (A)"),
    {ok, A} = gen_tcp:accept(L),
    ?P("send message A -> C"),
    ok = gen_tcp:send(A, "Hej Client"),
    ?P("socket A shutdown(write)"),
    ok = gen_tcp:shutdown(A, write),
    ?P("socket C recv - expect message"),
    {ok, "Hej Client"} = gen_tcp:recv(C, 0),
    ?P("socket C recv - expect closed"),
    {error, closed} = gen_tcp:recv(C, 0),
    ?P("done"),
    ok.

t_shutdown_both(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_shutdown_both(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_shutdown_both(Config, Addr) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++
				 [{ip, Addr}]),
    {ok, Port} = inet:port(L),
    ?P("create connect socket (C)"),
    {ok, C} = gen_tcp:connect(Addr, Port,
                              ?INET_BACKEND_OPTS(Config) ++
                                  [{ip, Addr}, {active, false}]),
    ?P("create accept socket (A)"),
    {ok, A} = gen_tcp:accept(L),
    ?P("send message A -> C"),
    ok = gen_tcp:send(A, "Hej Client"),
    ?P("socket A shutdown(read_write)"),
    ok = gen_tcp:shutdown(A, read_write),
    ?P("socket C recv - expect message"),
    {ok, "Hej Client"} = gen_tcp:recv(C, 0),
    ?P("socket C recv - expect closed"),
    {error, closed} = gen_tcp:recv(C, 0),
    ?P("done"),
    ok.

t_shutdown_error(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC = fun(Addr) -> do_shutdown_error(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	   Cond, Pre, TC, Post).

do_shutdown_error(Config, Addr) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++ [{ip, Addr}]),
    ?P("shutdown socket (with How = read_write)"),
    case gen_tcp:shutdown(L, read_write) of
        {error, enotconn} ->
            ok;
        ok -> % On some platforms this can happen...Linux...
            case os:type() of
                {unix, linux} ->
                    ?P("unexpected shutdown success - can happen on linux"),
                    ok;
                _ ->
                    exit(unexpected_success)
            end;
        {error, Reason} ->
            exit({unexpected_shutdown_error, Reason})
    end,
    ?P("close socket"),
    ok = gen_tcp:close(L),
    ?P("shutdown socket again (with How = read_write)"),
    {error, closed} = gen_tcp:shutdown(L, read_write),
    ?P("done"),
    ok.

t_shutdown_async(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC = fun(Addr) -> do_shutdown_async(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	   Cond, Pre, TC, Post).

do_shutdown_async(Config, Addr) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++
				 [{ip, Addr}, {sndbuf, 4096}]),
    if
        is_port(L) ->
            do_shutdown_async2(Config, Addr, L);
        true ->
            (catch gen_tcp:close(L)),
            exit({skip, "inet-only testcase"})
    end.

do_shutdown_async2(Config, Addr, L) ->
    {OS, _} = os:type(),
    {ok, Port} = inet:port(L),
    ?P("connect"),
    {ok, Client} = gen_tcp:connect(Addr, Port,
				   ?INET_BACKEND_OPTS(Config) ++
                                       [{ip, Addr},
					{recbuf, 4096},
                                        {active, false}]),
    ?P("accept connection"),
    {ok, S} = gen_tcp:accept(L),
    ?P("create payload"),
    PayloadSize = 1024 * 1024,
    Payload = lists:duplicate(PayloadSize, $.),
    ?P("send payload"),
    ok = gen_tcp:send(S, Payload),
    ?P("verify queue size"),
    case erlang:port_info(S, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail({unexpected, T})
    end,

    ?P("shutdown(write) accepted socket"),
    ok = gen_tcp:shutdown(S, write),
    ?P("recv from connected socket"),
    {ok, Buf} = gen_tcp:recv(Client, PayloadSize),
    ?P("recv(0) from connected socket (expect closed)"),
    {error, closed} = gen_tcp:recv(Client, 0),
    ?P("verify recv data"),
    case length(Buf) of
	PayloadSize -> ?P("done"), ok;
	Sz -> ?P("ERROR: "
                 "~n   extected: ~p"
                 "~n   received: ~p", [PayloadSize, Sz]),
              ct:fail({payload_size,
		       {expected, PayloadSize},
		       {received, Sz}})
    end.


%%% gen_tcp:fdopen/2

t_fdopen(Config) when is_list(Config) ->
    Cond = fun() -> ok end,
    Pre  = fun() -> case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
                        {error, Reason} ->
                            skip(Reason)
                    end
           end,
    TC   = fun(Addr) -> do_fdopen(Config, Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_fdopen(Config, Addr) ->
    Question  = "Aaaa... Long time ago in a small town in Germany,",
    Question1 = list_to_binary(Question),
    Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
		 ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    Question1 = iolist_to_binary(Question2),
    Answer    = "there was a shoemaker, Schumacher was his name.",
    {ok, L}      = gen_tcp:listen(0,
				  ?INET_BACKEND_OPTS(Config) ++
				      [{ip, Addr}, {active, false}]),
    {ok, Port}   = inet:port(L),
    {ok, Client} = gen_tcp:connect(Addr, Port,
                                   ?INET_BACKEND_OPTS(Config) ++
                                       [{ip, Addr}, {active, false}]),
    {A, FD} = case gen_tcp:accept(L) of
                  {ok, ASock} when is_port(ASock) ->
                      {ok, FileDesc} = prim_inet:getfd(ASock),
                      {ASock, FileDesc};
                  {ok, ASock} -> % socket
                      {ok, [{fd, FileDesc}]} =
                          gen_tcp_socket:getopts(ASock, [fd]),
                      {ASock, FileDesc}
              end,
    ?P("fdopen -> accepted: "
       "~n   A:  ~p"
       "~n   FD: ~p", [A, FD]),
    {ok, Server}    = try gen_tcp:fdopen(FD, ?INET_BACKEND_OPTS(Config)) of
			  {ok, _} = OK ->
			      OK;
			  {error, notsup = R} ->
			      %% This is not supported for 'socket on Windows'
			      skip({fdopen, R})
		      catch
			  throw:{error, notsup = R} ->
			      %% This is not supported for 'socket on Windows'
			      skip({fdopen, R})
		      end,
    ok              = gen_tcp:send(Client, Question),
    {ok, Question}  = gen_tcp:recv(Server, length(Question), 2000),
    ok              = gen_tcp:send(Client, Question1),
    {ok, Question}  = gen_tcp:recv(Server, length(Question), 2000),
    ok              = gen_tcp:send(Client, Question2),
    {ok, Question}  = gen_tcp:recv(Server, length(Question), 2000),
    ok              = gen_tcp:send(Server, Answer),
    {ok, Answer}    = gen_tcp:recv(Client, length(Answer), 2000),
    ok              = gen_tcp:close(Client),
    {error, closed} = gen_tcp:recv(A, 1, 2000),
    ok              = gen_tcp:close(Server),
    ok              = gen_tcp:close(A),
    ok              = gen_tcp:close(L),
    ok.


t_fdconnect(Config) when is_list(Config) ->
    Cond = fun() ->
                   ?P("Try verify if IPv4 is supported"),
                   ?HAS_SUPPORT_IPV4()
           end,
    Pre  = fun() ->
                   {ok, Addr} = ?WHICH_LOCAL_ADDR(inet),
                   ?P("Use (local) address: ~p", [Addr]),
                   #{local_addr => Addr}
           end,
    Case = fun(#{local_addr := Addr}) ->
                   do_t_fdconnect(Addr, Config)
           end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
            Cond, Pre, Case, Post).

do_t_fdconnect(Addr, Config) ->
    Question  = "Aaaa... Long time ago in a small town in Germany,",
    Question1 = list_to_binary(Question),
    Question2 = [<<"Aaaa">>, "... ", $L, <<>>, $o, "ng time ago ",
		 ["in ", [], <<"a small town">>, [" in Germany,", <<>>]]],
    Question1 = iolist_to_binary(Question2),
    Answer    = "there was a shoemaker, Schumacher was his name.",
    Path      = proplists:get_value(data_dir, Config),
    Lib       = "gen_tcp_api_SUITE",
    ?P("try load util nif lib"),
    case erlang:load_nif(filename:join(Path, Lib), []) of
        ok ->
            ok;
        {error, {reload, ReasonStr}} ->
            ?P("already loaded: "
               "~n   ~s", [ReasonStr]),
            ok;
        {error, Reason} ->
            ?P("UNEXPECTED - failed loading util nif lib: "
               "~n   ~p", [Reason]),
            ?SKIPT("failed loading util nif lib")
    end,
    ?P("try create listen socket"),
    LOpts = ?INET_BACKEND_OPTS(Config) ++ [{ifaddr, Addr}, {active, false}],
    L = try gen_tcp:listen(0, LOpts) of
            {ok, LSock} ->
                LSock;
            {error, eaddrnotavail = LReason} ->
                ?SKIPT(listen_failed_str(LReason));
            {error, LReason} ->
                ?P("UNEXPECTED ERROR - listen error: "
                   "~n      COpts:  ~p"
                   "~n      Reason: ~p", [LReason]),
                ct:fail({unexpected_listen_error, LReason, LOpts})
        catch
            LC : LE : LS ->
                ?P("UNEXPECTED ERROR - caught listen: "
                   "~n   LOpts: ~p"
                   "~n   C:     ~p"
                   "~n   E:     ~p"
                   "~n   S:     ~p", [LOpts, LC, LE, LS]),
                ct:fail({listen_failure, {LC, LE, LS}, LOpts})
        end,            
    {ok, LPort} = inet:port(L),
    ?P("try create file descriptor"),
    FD = gen_tcp_api_SUITE:getsockfd(),
    ?P("try connect (to port ~w) using file descriptor ~w", [LPort, FD]),
    COpts = ?INET_BACKEND_OPTS(Config) ++ [{fd,     FD},
                                           {ifaddr, Addr},
                                           {active, false}],
    %% The debug is just to "see" that it (debug) "works"...
    Client = try gen_tcp:connect(Addr, LPort, COpts ++ [{debug, true}]) of
                 {ok, CSock} ->
                     ok = inet:setopts(CSock, [{debug, false}]),
                     CSock;
                 {error, eaddrnotavail = CReason} ->
                     gen_tcp:close(L),
                     gen_tcp_api_SUITE:closesockfd(FD),
                     ?SKIPT(connect_failed_str(CReason));
                 {error, CReason} ->
                     ?P("UNEXPECTED ERROR - connect error: "
                        "~n      COpts:  ~p"
                        "~n      Reason: ~p", [COpts, CReason]),
                     ct:fail({unexpected_connect_error, CReason, COpts})
             catch
                 CC : CE : CS ->
                     ?P("UNEXPECTED ERROR - caught connect: "
                        "~n   COpts: ~p"
                        "~n   C:     ~p"
                        "~n   E:     ~p"
                        "~n   S:     ~p", [COpts, CC, CE, CS]),
                     ct:fail({connect_failure, {CC, CE, CS}, COpts})
             end,                             
    ?P("try accept connection"),
    Server = try gen_tcp:accept(L) of
                 {ok, ASock} ->
                     ASock;
                 {error, eaddrnotavail = AReason} ->
                     gen_tcp:close(Client),
                     gen_tcp:close(L),
                     gen_tcp_api_SUITE:closesockfd(FD),
                     ?SKIPT(accept_failed_str(AReason));
                 {error, AReason} ->
                     ?P("UNEXPECTED ERROR - accept error: "
                        "~n      Reason: ~p", [AReason]),
                     ct:fail({unexpected_accept_error, AReason})
             catch
                 AC : AE : AS ->
                     ?P("UNEXPECTED ERROR - caught accept: "
                        "~n   C: ~p"
                        "~n   E: ~p"
                        "~n   S: ~p", [AC, AE, AS]),
                     ct:fail({accept_failure, {AC, AE, AS}})
        end,                             
    ?P("begin validation"),
    ok = gen_tcp:send(Client, Question),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question1),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Client, Question2),
    {ok, Question} = gen_tcp:recv(Server, length(Question), 2000),
    ok = gen_tcp:send(Server, Answer),
    {ok, Answer} = gen_tcp:recv(Client, length(Answer), 2000),
    ?P("cleanup"),
    ok = gen_tcp:close(Client),
    FD = gen_tcp_api_SUITE:closesockfd(FD),
    {error, closed} = gen_tcp:recv(Server, 1, 2000),
    ok = gen_tcp:close(Server),
    ok = gen_tcp:close(L),
    ?P("done"),
    ok.


%%% implicit inet6 option to api functions

t_implicit_inet6(Config) when is_list(Config) ->
    ?TC_TRY(t_implicit_inet6, fun() -> do_t_implicit_inet6(Config) end).

do_t_implicit_inet6(Config) ->
    ?P("try get hostname"),
    Host = ok(inet:gethostname()),
    ?P("try get address for host ~p", [Host]),
    case inet:getaddr(Host, inet6) of
	{ok, Addr} ->
            ?P("address: ~p", [Addr]),
	    t_implicit_inet6(Config, Host, Addr);
	{error, Reason} ->
	    {skip,
	     "Can not look up IPv6 address: "
	     ++atom_to_list(Reason)}
    end.

t_implicit_inet6(Config, Host, Addr) ->
    Loopback = {0,0,0,0,0,0,0,1},
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    case gen_tcp:listen(0, InetBackendOpts ++ [inet6, {ip,Loopback}]) of
	{ok, S1} ->
	    ?P("try ~s ~p", ["::1", Loopback]),
	    implicit_inet6(Config, S1, Loopback),
	    ok = gen_tcp:close(S1),
	    %%
	    LocalAddr = ok(get_localaddr()),
	    S2 = case gen_tcp:listen(0, InetBackendOpts ++ [{ip, LocalAddr}]) of
                     {ok, LSock2} ->
                         LSock2;
                     {error, Reason2} ->
                         ?P("Listen failed (ip):"
                            "~n   Reason2: ~p", [Reason2]),
                         ?SKIPT(listen_failed_str(Reason2))
                 end,
	    implicit_inet6(Config, S2, LocalAddr),
	    ok = gen_tcp:close(S2),
	    %%
	    ?P("try ~s ~p", [Host, Addr]),
	    S3 = case gen_tcp:listen(0, InetBackendOpts ++ [{ifaddr,Addr}]) of
                     {ok, LSock3} ->
                         LSock3;
                     {error, Reason3} ->
                         ?P("Listen failed (ifaddr):"
                            "~n   Reason3: ~p", [Reason3]),
                         ?SKIPT(listen_failed_str(Reason3))
                 end,
	    implicit_inet6(Config, S3, Addr),
	    ok = gen_tcp:close(S3),
	    ?P("done"),
            ok;
        {error, Reason1} ->
            ?SKIPT(listen_failed_str(Reason1))
    end.

implicit_inet6(Config, S, Addr) ->
    P = ok(inet:port(S)),
    S2 = case gen_tcp:connect(Addr, P, ?INET_BACKEND_OPTS(Config)) of
             {ok, CSock} ->
                 CSock;
             {error, CReason} ->
                 ?SKIPT(connect_failed_str(CReason))
         end,
    P2 = ok(inet:port(S2)),
    S1 = case gen_tcp:accept(S) of
             {ok, ASock} ->
                 ASock;
             {error, AReason} ->
                 ?SKIPT(accept_failed_str(AReason))
         end,
    P1 = P = ok(inet:port(S1)),
    {Addr,P2} = ok(inet:peername(S1)),
    {Addr,P1} = ok(inet:peername(S2)),
    {Addr,P1} = ok(inet:sockname(S1)),
    {Addr,P2} = ok(inet:sockname(S2)),
    ok = gen_tcp:close(S2),
    ok = gen_tcp:close(S1).


t_local_basic(Config) ->
    SFile = local_filename(server),
    SAddr = {local, bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),
    %%
    ?P("try create listen socket"),
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    L =
	ok(
	  gen_tcp:listen(0, InetBackendOpts ++ 
                             [{ifaddr,{local,SFile}},{active,false}])),
    ?P("try connect"),
    C =
	ok(
	  gen_tcp:connect(
	    {local,SFile}, 0, InetBackendOpts ++
                [{ifaddr,{local,CFile}},{active,false}])),
    ?P("try accept connection"),
    S = ok(gen_tcp:accept(L)),
    ?P("try get sockname for listen socket"),
    %% SAddr = ok(inet:sockname(L)),
    case inet:sockname(L) of
        {ok, SAddr} ->
            ok;
        {ok, SAddr2} ->
            ?P("Invalid sockname: "
               "~n   Expected: ~p"
               "~n   Actual:   ~p", [SAddr, SAddr2]),
            exit({sockename, SAddr, SAddr2});
        {error, Reason} ->
            exit({sockname, Reason})
    end,
    ?P("try get peername for listen socket"),
    {error, enotconn} = inet:peername(L),
    ?P("try handshake"),
    local_handshake(S, SAddr, C, CAddr),
    ?P("try close listen socket"),
    ok = gen_tcp:close(L),
    ?P("try close accept socket"),
    ok = gen_tcp:close(S),
    ?P("try close connect socket"),
    ok = gen_tcp:close(C),
    %%
    ?P("try 'local' files"),
    ok = file:delete(SFile),
    ok = file:delete(CFile),
    ?P("done"),
    ok.


t_local_unbound(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_unbound(Config) end).

do_local_unbound(Config) ->
    ?P("create local (server) filename"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    %%
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    ?P("create listen socket with ifaddr ~p", [SAddr]),
    L = ok(gen_tcp:listen(0, InetBackendOpts ++
                              [{ifaddr,SAddr},{active,false}])),
    ?P("listen socket created: ~p"
       "~n   => try connect", [L]),
    C = ok(gen_tcp:connect(SAddr, 0,
                           InetBackendOpts ++ [{active,false}])),
    ?P("connected: ~p"
       "~n   => try accept", [C]),
    S = ok(gen_tcp:accept(L)),
    ?P("accepted: ~p"
       "~n   => sockname", [S]),
    SAddr = ok(inet:sockname(L)),
    ?P("sockname: ~p"
       "~n   => peername (expect enotconn)", [SAddr]),
    {error, enotconn} = inet:peername(L),
    ?P("try local handshake"),
    local_handshake(S, SAddr, C, {local,<<>>}),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    ?P("close accepted socket"),
    ok = gen_tcp:close(S),
    ?P("close connected socket"),
    ok = gen_tcp:close(C),
    ?P("delete (local) file"),
    ok = file:delete(SFile),
    ?P("done"),
    ok.


t_local_fdopen(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_fdopen(Config) end).

do_local_fdopen(Config) ->
    ?P("create local (server) filename"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    %%
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    ListenOpts = InetBackendOpts ++ [{ifaddr,SAddr},{active,false}],
    ?P("create listen socket with ListenOpts ~p", [ListenOpts]),
    L = ok(gen_tcp:listen(0, ListenOpts)),
    ConnectOpts = InetBackendOpts ++ [{active,false}],
    ?P("listen socket created: ~p"
       "~n   => try connect ~p", [L, ConnectOpts]),
    C0 = ok(gen_tcp:connect(SAddr, 0, ConnectOpts)),
    ok = inet:setopts(C0, [{debug, true}]),
    ?P("connected: ~p"
       "~n   => get fd", [C0]),
    Fd = if
             is_port(C0) ->
                 FD0 = ok(prim_inet:getfd(C0)),
                 ?P("FD: ~p"
                    "~n   => ignore fd", [FD0]),
                 %% Turn off C0, so it does not generate any events!
                 ok = prim_inet:ignorefd(C0, true),
                 FD0;
             true ->
                 [{fd, FD0}] = ok(inet:getopts(C0, [fd])),
                 ?P("FD: ~p", [FD0]),
                 FD0
         end,
    ?P("ignored fd:"
       "~n   => try fdopen (local)"),
    C = ok(gen_tcp:fdopen(Fd, ?INET_BACKEND_OPTS(Config) ++ [local])),
    ?P("fd open: ~p"
       "~n   => try accept", [C]),
    S = ok(gen_tcp:accept(L)),
    ?P("accepted: ~p"
       "~n   => get sockname", [S]),
    SAddr = ok(inet:sockname(L)),
    ?P("sockname: ~p"
       "~n   => try get peername (expect enotconn)", [SAddr]),
    {error,enotconn} = inet:peername(L),
    ?P("try local handshake"),
    local_handshake(S, SAddr, C, {local,<<>>}),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    ?P("close accepted socket"),
    ok = gen_tcp:close(S),
    ?P("close connected socket (final)"),
    ok = gen_tcp:close(C),
    ?P("close connected socket (pre)"),
    ok = gen_tcp:close(C0),
    ?P("delete (local) file"),
    ok = file:delete(SFile),
    ?P("done"),
    ok.

t_local_fdopen_listen(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_fdopen_listen(Config) end).

do_local_fdopen_listen(Config) ->
    ?P("create local (server) filename"),
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    ?P("create dummy listen socket with ifaddr ~p", [SAddr]),
    L0 = ok(gen_tcp:listen(0, InetBackendOpts ++
                               [{ifaddr,SAddr},{active,false}])),
    ?P("dummy listen socket created: ~p"
       "~n   => try get FD", [L0]),
    Fd = if
             is_port(L0) ->
                 ok(prim_inet:getfd(L0));
             true ->
                 [{fd, FD0}] = ok(inet:getopts(L0, [fd])),
                 FD0
         end,
    ?P("FD: ~p"
       "~n   => try create proper listen socket (using fd)", [Fd]),
    L = ok(gen_tcp:listen(0, InetBackendOpts ++
                              [{fd,Fd},local,{active,false}])),
    ?P("try connect"),
    C = ok(gen_tcp:connect(SAddr, 0, InetBackendOpts ++ [{active,false}])),
    ?P("try accept (connection)"),
    S = ok(gen_tcp:accept(L)),
    ?P("verify (proper) listen socket sockname"),
    SAddr = ok(inet:sockname(L)),
    ?P("verify (proper) listen socket peername (expect enotconn)"),
    {error, enotconn} = inet:peername(L),
    ?P("perform handshake"),
    local_handshake(S, SAddr, C, {local,<<>>}),
    ?P("close (proper) listen socket"),
    ok = gen_tcp:close(L),
    ?P("close (dummy) listen socket"),
    ok = gen_tcp:close(L0),
    ?P("close accepted socket"),
    ok = gen_tcp:close(S),
    ?P("close connected socket"),
    ok = gen_tcp:close(C),
    ?P("delete file (used for socket)"),
    ok = file:delete(SFile),
    ?P("done"),
    ok.

t_local_fdopen_listen_unbound(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_fdopen_listen_unbound(Config) end).

do_local_fdopen_listen_unbound(Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    P = ok(prim_inet:open(tcp, local, stream)),
    Fd = ok(prim_inet:getfd(P)),
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    L =
	ok(gen_tcp:listen(
	     0, InetBackendOpts ++ [{fd,Fd},{ifaddr,SAddr},{active,false}])),
    C = ok(gen_tcp:connect(SAddr, 0, InetBackendOpts ++ [{active,false}])),
    S = ok(gen_tcp:accept(L)),
    SAddr = ok(inet:sockname(L)),
    {error,enotconn} = inet:peername(L),
    local_handshake(S, SAddr, C, {local,<<>>}),
    ok = gen_tcp:close(L),
    ok = gen_tcp:close(P),
    ok = gen_tcp:close(S),
    ok = gen_tcp:close(C),
    ok = file:delete(SFile),
    ok.

t_local_fdopen_connect(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_fdopen_connect(Config) end).

do_local_fdopen_connect(Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    CFile = local_filename(client),
    CAddr = {local,bin_filename(CFile)},
    _ = file:delete(SFile),
    _ = file:delete(CFile),
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    L = ok(gen_tcp:listen(0, InetBackendOpts ++ [{ifaddr,SAddr},{active,false}])),
    P = ok(prim_inet:open(tcp, local, stream)),
    Fd = ok(prim_inet:getfd(P)),
    C =
	ok(gen_tcp:connect(
	     SAddr, 0, InetBackendOpts ++
                 [{fd,Fd},{ifaddr,CAddr},{active,false}])),
    S = ok(gen_tcp:accept(L)),
    SAddr = ok(inet:sockname(L)),
    {error,enotconn} = inet:peername(L),
    local_handshake(S, SAddr, C, CAddr),
    ok = gen_tcp:close(L),
    ok = gen_tcp:close(S),
    ok = gen_tcp:close(C),
    ok = gen_tcp:close(P),
    ok = file:delete(SFile),
    ok.

t_local_fdopen_connect_unbound(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
	    fun() -> is_not_windows() end,
	    fun() -> do_local_fdopen_connect_unbound(Config) end).

do_local_fdopen_connect_unbound(Config) ->
    SFile = local_filename(server),
    SAddr = {local,bin_filename(SFile)},
    _ = file:delete(SFile),
    InetBackendOpts = ?INET_BACKEND_OPTS(Config),
    L = ok(gen_tcp:listen(0, InetBackendOpts ++ [{ifaddr,SAddr},{active,false}])),
    P = ok(prim_inet:open(tcp, local, stream)),
    Fd = ok(prim_inet:getfd(P)),
    C =	ok(gen_tcp:connect(SAddr, 0, InetBackendOpts ++ [{fd,Fd},{active,false}])),
    S = ok(gen_tcp:accept(L)),
    SAddr = ok(inet:sockname(L)),
    {error,enotconn} = inet:peername(L),
    local_handshake(S, SAddr, C, {local,<<>>}),
    ok = gen_tcp:close(L),
    ok = gen_tcp:close(S),
    ok = gen_tcp:close(C),
    ok = gen_tcp:close(P),
    ok = file:delete(SFile),
    ok.

t_local_abstract(Config) ->
    ?TC_TRY(t_local_abstract, fun() -> do_local_abstract(Config) end).

do_local_abstract(Config) ->
    ?P("only run on linux"),
    case os:type() of
	{unix, linux} ->
	    AbstAddr = {local,<<>>},
            InetBackendOpts = ?INET_BACKEND_OPTS(Config),
            ?P("create listen socket"),
	    L =
		ok(gen_tcp:listen(
		     0, InetBackendOpts ++ [{ifaddr,AbstAddr},{active,false}])),
            ?P("listen socket created: ~p"
               "~n      => sockname", [L]),
	    {local, _} = SAddr = ok(inet:sockname(L)),
            ?P("(listen socket) sockname verified"
               "~n      => try connect"),
	    C =
		ok(gen_tcp:connect(
		     SAddr, 0,
                     InetBackendOpts ++ [{ifaddr,AbstAddr},{active,false}])),
            ?P("connected: ~p"
               "~n      => sockname", [C]),
	    {local,_} = CAddr = ok(inet:sockname(C)),
            ?P("(connected socket) sockname verified"
               "~n      => try accept"),
	    S = ok(gen_tcp:accept(L)),
            ?P("accepted: ~p"
               "~n   => peername (expect enotconn)", [S]),
	    {error,enotconn} = inet:peername(L),
            ?P("try local handshake"),
	    local_handshake(S, SAddr, C, CAddr),
            ?P("close listen socket"),
	    ok = gen_tcp:close(L),
            ?P("close accepted socket"),
	    ok = gen_tcp:close(S),
            ?P("close connected socket"),
	    ok = gen_tcp:close(C),
            ?P("done"),
	    ok;
	_ ->
            ?P("skip (unless linux)"),
	    {skip,"AF_LOCAL Abstract Addresses only supported on Linux"}
    end.


local_handshake(S, SAddr, C, CAddr) ->
    ?P("~w -> entry with"
       "~n   Server Sock:          ~p"
       "~n   Server Sock Info:     ~p"
       "~n   Server Addr:          ~p"
       "~n   Client Sock:          ~p"
       "~n   Client Sock Info:     ~p"
       "~n   Client Addr:          ~p"
       "~n", [?FUNCTION_NAME,
	      S, inet:info(S), SAddr,
	      C, inet:info(C), CAddr]),
    SData = "9876543210",
    CData = "0123456789",
    ?P("~w -> verify server sockname", [?FUNCTION_NAME]),
    SAddr = ok(inet:sockname(S)),
    ?P("~w -> verify client sockname", [?FUNCTION_NAME]),
    CAddr = ok(inet:sockname(C)),
    ?P("~w -> verify server peername", [?FUNCTION_NAME]),
    CAddr = ok(inet:peername(S)),
    ?P("~w -> verify client peername", [?FUNCTION_NAME]),
    SAddr = ok(inet:peername(C)),
    ?P("~w -> send data on client socket", [?FUNCTION_NAME]),
    ok = gen_tcp:send(C, CData),
    ?P("~w -> send data on server socket", [?FUNCTION_NAME]),
    ok = gen_tcp:send(S, SData),
    ?P("~w -> recv data on server socket", [?FUNCTION_NAME]),
    CData = ok(gen_tcp:recv(S, length(CData))),
    ?P("~w -> recv data on client socket", [?FUNCTION_NAME]),
    SData = ok(gen_tcp:recv(C, length(SData))),
    ?P("~w -> done", [?FUNCTION_NAME]),
    ok.

t_accept_inet6_tclass(Config) when is_list(Config) ->
    ?TC_TRY(t_accept_inet6_tclass, fun() -> do_accept_inet6_tclass(Config) end).

do_accept_inet6_tclass(Config) ->
    TClassOpt = {tclass, 8#56 bsl 2}, % Expedited forwarding
    Loopback  = {0,0,0,0,0,0,0,1},
    ?P("create listen socket with tclass: ~p", [TClassOpt]),
    case gen_tcp:listen(0, ?INET_BACKEND_OPTS(Config) ++
                            [inet6, {ip, Loopback}, TClassOpt]) of
	{ok, L} ->
            ?P("listen socket created: "
               "~n      ~p", [L]),
	    LPort = ok(inet:port(L)),
            ?P("try to connect to port ~p", [LPort]),
	    Sa = ok(gen_tcp:connect(Loopback, LPort,
                                    ?INET_BACKEND_OPTS(Config))),
            ?P("connected: ~p"
               "~n   => accept connection", [Sa]),
	    Sb = ok(gen_tcp:accept(L)),
            ?P("accepted: ~p"
               "~n   => getopts (tclass)", [Sb]),
	    [TClassOpt] = ok(inet:getopts(Sb, [tclass])),
            ?P("tclass verified => close accepted socket"),
	    ok = gen_tcp:close(Sb),
            ?P("close connected socket"),
	    ok = gen_tcp:close(Sa),
            ?P("close listen socket"),
	    ok = gen_tcp:close(L),
            ?P("done"),
	    ok;
	{error, _Reason} ->
            ?P("ERROR: Failed create listen socket"
               "~n   ~p", [_Reason]),
	    {skip,"IPv6 TCLASS not supported"}
    end.


%% Here we use socket:sockaddr_in6() when creating and using the
%% socket(s).
%%
t_simple_local_sockaddr_in6_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> ?LIB:has_support_ipv6() end,
            fun() ->
                    Domain = inet6,
                    {ok, LocalAddr} = ?LIB:which_local_addr(Domain),
                    SockAddr = #{family   => Domain,
                                 addr     => LocalAddr,
                                 port     => 0},
                    do_simple_sockaddr_send_recv(SockAddr, Config)
            end).


t_simple_link_local_sockaddr_in6_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() ->
                    ?LIB:has_support_ipv6(),
                    is_net_supported()
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
                                        addr   := A}}) ->
                                (D =:= Domain) andalso (A =:= LinkLocalAddr);
                           (_) ->
                                false
                        end,
                    case net:getifaddrs(Filter) of
                        {ok, [#{addr := #{scope_id := ScopeID}}|_]} ->
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


%% Here we use socket:sockaddr_in() when creating and using the
%% socket(s).
%%
t_simple_local_sockaddr_in_send_recv(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME,
            fun() -> ok end,
            fun() ->
                    Domain = inet,
                    {ok, LocalAddr} = ?LIB:which_local_addr(Domain),
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


do_simple_sockaddr_send_recv(SockAddr, _) ->
    %% Create the server
    Self   = self(),
    ?P("~n      SockAddr: ~p", [SockAddr]),
    ServerF = fun() ->
                      ?P("try create listen socket"),
                      LSock =
                          try gen_tcp:listen(0, [{ifaddr, SockAddr},
                                                 {active, true},
                                                 binary]) of
                              {ok, LS} ->
                                  LS;
                              {error, LReason} ->
                                  ?P("listen error: "
                                     "~n      Reason: ~p", [LReason]),
                                  exit({listen_error, LReason})
                          catch
                              LC:LE:LS ->
                                  ?P("listen failure: "
                                     "~n      Error Class: ~p"
                                     "~n      Error:       ~p"
                                     "~n      Call Stack:  ~p", [LC, LE, LS]),
                                  exit({listen_failure, {LC, LE, LS}})
                          end,
                      ?P("try get listen port"),
                      {ok, Port}  = inet:port(LSock),
                      ?P("listen port: ~w", [Port]),
                      Self ! {{port, Port}, self()},
                      ?P("try accept"),
                      {ok, ASock} = gen_tcp:accept(LSock),
                      ?P("accepted: "
                         "~n      ~p", [ASock]),
                      receive
                          {tcp, ASock, <<"hej">>} ->
                              ?P("received expected message - send reply"),
                              ok = gen_tcp:send(ASock, "hopp")
                      end,
                      ?P("await termination command"),
                      receive
                          {die, Self} ->
                              ?P("terminating"),
                              (catch gen_tcp:close(ASock)),
                              (catch gen_tcp:close(LSock)),
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
                         exit({unexpected_listen_failure, Reason})
                 end,
    ?P("server port received: ~p", [ServerPort]),
    
    ?P("try connect to server"),
    ServerSockAddr = SockAddr#{port => ServerPort},
    {ok, CSock} = gen_tcp:connect(ServerSockAddr,
                                  [{ifaddr, SockAddr},
                                   {active, true},
                                   binary]),
    ?P("connected to server: "
       "~n      CSock: ~p"
       "~n      CPort: ~p", [CSock, inet:port(CSock)]),

    ?P("try send message"),
    ok = gen_tcp:send(CSock, "hej"),
    
    ?P("await reply message"),
    receive
        {tcp, CSock, <<"hopp">>} ->
            ?P("received expected reply message")
    end,

    ?P("terminate server"),
    Server ! {die, self()},

    ?P("await server termination"),
    receive
        {'EXIT', Server, normal} ->
            ok
    end,
    
    ?P("cleanup"),
    (catch gen_tcp:close(CSock)),

    ?P("done"),
    ok.

    
%% On MacOS (maybe more), accepting a connection resulted in a crash.
%% Note that since 'socket' currently does not work on windows
%% we have to skip on that platform.
s_accept_with_explicit_socket_backend(Config) when is_list(Config) ->
    Cond = fun() ->
		   is_socket_supported()
	   end,
    Pre  = fun() ->
		   case ?WHICH_LOCAL_ADDR(inet) of
                        {ok, Addr} ->
                            Addr;
		       {error, Reason} ->
			   skip(Reason)
                    end
	   end,
    TC   = fun(Addr) -> do_s_accept_with_explicit_socket_backend(Addr) end,
    Post = fun(_) -> ok end,
    ?TC_TRY(?FUNCTION_NAME,
	    Cond, Pre, TC, Post).

do_s_accept_with_explicit_socket_backend(Addr) ->
    ?P("try create listen socket"),
    {ok, S}         = gen_tcp:listen(0, [{inet_backend, socket}, {ip, Addr}]),
    ?P("try get port number (via sockname)"),
    {ok, {_, Port}} = inet:sockname(S),
    ClientF = fun() ->
                      ?P("[client] try connect (tp ~p)", [Port]),
		      {ok, _} = gen_tcp:connect(Addr, Port, [{ip, Addr}]),
                      ?P("[client] connected - wait for termination command"),
		      receive
                          die ->
                              ?P("[client] termination command received"),
                              exit(normal)
                      after infinity -> ok
                      end
	      end,
    ?P("create client"),
    Client = spawn_link(ClientF),
    ?P("try accept"),
    {ok, _} = gen_tcp:accept(S),
    ?P("accepted - command client to terminate"),
    Client ! die,
    ?P("done"),
    ok.


%%% Utilities

is_socket_supported() ->
    try socket:info() of
        _ -> ok
    catch
        error : notsup ->
            {skip, "esock not supported"};
        error : undef ->
            {skip, "esock not configured"}
    end.


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
		{error, econnrefused = E} ->
		    {skip, "Not tested -- got error " ++ atom_to_list(E)};
		{error, enetunreach = E} ->
		    {skip, "Not tested -- got error " ++ atom_to_list(E)};
                {error, ehostunreach = E} ->
		    {skip, "Not tested -- got error " ++ atom_to_list(E)};
		{ok, Socket} -> % What the...
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
    if
        (IP =:= error) ->
            %% This is not supported on all platforms (yet), so...
            try net:getifaddrs() of
                {ok, IfAddrs} ->
                    ?P("~n   we        = ~p"
                       "~n   unused_ip = ~p"
                       "~n   ~p", [Hent, IP, IfAddrs]);
                {error, _} ->
                    ?P("~n   we:        ~p"
                       "~n   unused_ip: ~p", [Hent, IP])
            catch
                _:_:_ ->
                    ?P("~n   we:        ~p"
                       "~n   unused_ip: ~p", [Hent, IP])
            end;
        true ->
            ?P("~n   we:        ~p"
               "~n   unused_ip: ~p", [Hent, IP])
    end,
    IP.

unused_ip(255, 255, 255, 255) -> error;
unused_ip(255, B, C, D) -> unused_ip(1, B + 1, C, D);
unused_ip(A, 255, C, D) -> unused_ip(A, 1, C + 1, D);
unused_ip(A, B, 255, D) -> unused_ip(A, B, 1, D + 1);
unused_ip(A, B, C, D) ->
    case inet:gethostbyaddr({A, B, C, D}) of
	{ok, _} -> unused_ip(A + 1, B, C, D);
	{error, _} -> {ok, {A, B, C, D}}
    end.

ok({ok,V}) -> V;
ok(NotOk) ->
    try throw(not_ok)
    catch
	throw:Thrown:Stacktrace ->
	    erlang:raise(
	      error, {Thrown, NotOk}, tl(Stacktrace))
    end.

get_localaddr() ->
    get_localaddr(["localhost", "localhost6", "ip6-localhost"]).

get_localaddr([]) ->
    {error, localaddr_not_found};
get_localaddr([Localhost|Ls]) ->
    case inet:getaddr(Localhost, inet6) of
       {ok, LocalAddr} ->
           ?P("~s ~p", [Localhost, LocalAddr]),
           {ok, LocalAddr};
       _ ->
           get_localaddr(Ls)
    end.

getsockfd() -> undefined.
closesockfd(_FD) -> undefined.

-define(WIN32_LOCAL_PRE, "").
-define(UNIX_LOCAL_PRE,  "/tmp/").
 
local_filename(Tag) ->
    {OSF, _} = os:type(),
    local_filename(OSF, Tag).

local_filename(win32, Tag) ->
    local_filename(?WIN32_LOCAL_PRE, ".sock", Tag);
local_filename(_, Tag) ->
    local_filename(?UNIX_LOCAL_PRE, "", Tag).

local_filename(Pre, Post, Tag) ->
    ?F("~s~s_~s_~w_~w~s",
       [Pre,
	?MODULE_STRING,os:getpid(),erlang:system_time(millisecond),Tag,
	Post]).
    %% Pre ++
    %% 	?MODULE_STRING "_" ++ os:getpid() ++ "_" ++ atom_to_list(Tag) ++
    %% 	Post.

bin_filename(String) ->
    unicode:characters_to_binary(String, file:native_name_encoding()).

delete_local_filenames() ->
    {OSF, _} = os:type(),
    delete_local_filenames(OSF).

delete_local_filenames(win32) ->
    do_delete_local_filenames(?WIN32_LOCAL_PRE);
delete_local_filenames(_) ->
    do_delete_local_filenames(?UNIX_LOCAL_PRE).

do_delete_local_filenames(Pre) ->
    _ =
	[file:delete(F) ||
	    F <-
		filelib:wildcard(
		  Pre ++ ?MODULE_STRING "_" ++ os:getpid() ++ "_*")],
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_not_windows() ->
    case os:type() of
        {win32, nt} ->
            skip("This does not work on Windows");
        _ ->
            ok
    end.

is_net_supported() ->
    try net:info() of
        #{} ->
            ok
    catch
        error : notsup ->
            not_supported(net)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_supported(What) ->
    skip({not_supported, What}).

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_failed_str(Reason) ->
    ?F("Connect failed: ~w", [Reason]).

listen_failed_str(Reason) ->
    ?F("Listen failed: ~w", [Reason]).

accept_failed_str(Reason) ->
    ?F("Accept failed: ~w", [Reason]).


