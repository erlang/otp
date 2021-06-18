%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2021. All Rights Reserved.
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
-module(gen_tcp_misc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("kernel_test_lib.hrl").

-export([
	 all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 controlling_process/1, controlling_process_self/1,
	 no_accept/1, close_with_pending_output/1, active_n/1,
         active_n_closed/1,
	 data_before_close/1,
	 iter_max_socks/0, iter_max_socks/1,
	 get_status/1,
	 passive_sockets/1, accept_closed_by_other_process/1,
	 init_per_testcase/2, end_per_testcase/2,
	 otp_3924/1, closed_socket/1,
	 shutdown_active/1, shutdown_passive/1, shutdown_pending/1,
	 show_econnreset_active/1, show_econnreset_active_once/1,
	 show_econnreset_passive/1, econnreset_after_sync_send/1,
	 econnreset_after_async_send_active/1,
	 econnreset_after_async_send_active_once/1,
	 econnreset_after_async_send_passive/1,
         linger_zero/1, linger_zero_sndbuf/1,
	 default_options/1, http_bad_packet/1, 
	 busy_send/1, busy_disconnect_passive/1, busy_disconnect_active/1,
	 fill_sendq/1, partial_recv_and_close/1, 
	 partial_recv_and_close_2/1,partial_recv_and_close_3/1,so_priority/1,
         recvtos/1, recvttl/1, recvtosttl/1, recvtclass/1,
	 %% Accept tests
	 primitive_accept/1,multi_accept_close_listen/1,accept_timeout/1,
	 accept_timeouts_in_order/1,accept_timeouts_in_order2/1,
	 accept_timeouts_in_order3/1,accept_timeouts_in_order4/1,
	 accept_timeouts_in_order5/1,accept_timeouts_in_order6/1,
	 accept_timeouts_in_order7/1,accept_timeouts_mixed/1,
	 killing_acceptor/1,
         killing_multi_acceptors/1,
         killing_multi_acceptors2/1,
	 several_accepts_in_one_go/1, accept_system_limit/1,
	 active_once_closed/1, send_timeout/1, send_timeout_active/1,
         otp_7731/1, zombie_sockets/1, otp_7816/1, otp_8102/1,
         wrapping_oct/0, wrapping_oct/1, otp_9389/1, otp_13939/1,
         otp_12242/1, delay_send_error/1, bidirectional_traffic/1,
	 socket_monitor1/1,
	 socket_monitor1_manys/1,
	 socket_monitor1_manyc/1,
	 socket_monitor1_demon_after/1,
	 socket_monitor2/1,
	 socket_monitor2_manys/1,
	 socket_monitor2_manyc/1,
	 otp_17492/1
	]).

%% Internal exports.
-export([sender/4,
         not_owner/1,
         passive_sockets_server/3,
         priority_server/2, 
	 %% oct_acceptor/1,
         otp_3924_sender/5,
	 otp_7731_server/2,
         zombie_server/3,
         do_iter_max_socks/3]).

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

end_per_testcase(_Func, Config) ->
    ?P("end_per_testcase -> entry with"
       "~n   Config:   ~p"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p",
       [Config, erlang:nodes(), pi(links), pi(monitors)]),

    SysEvs = kernel_test_global_sys_monitor:events(),

    ?P("system events during test: "
       "~n   ~p", [SysEvs]),

    ?P("end_per_testcase -> done with"
       "~n   Nodes:    ~p"
       "~n   Links:    ~p"
       "~n   Monitors: ~p", [erlang:nodes(), pi(links), pi(monitors)]),
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() ->
    %% This is a temporary messure to ensure that we can 
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

     {ctrl_proc,              [], ctrl_proc_cases()},
     {close,                  [], close_cases()},
     {active,                 [], active_cases()},
     {shutdown,               [], shutdown_cases()},
     {econnreset,             [], econnreset_cases()},
     {linger_zero,            [], linger_zero_cases()},
     {busy_disconnect,        [], busy_disconnect_cases()},
     {partial_recv_and_close, [], partial_recv_and_close_cases()},
     {pktoptions,             [], pktoptions_cases()},
     {accept,                 [], accept_cases()},
     {send_timeout,           [], send_timeout_cases()},
     {socket_monitor,         [], socket_monitor_cases()}
    ].

inet_backend_default_cases() ->
    all_cases().

inet_backend_inet_cases() ->
    all_cases().

inet_backend_socket_cases() ->
    all_cases().

all_cases() ->
    [
     {group, ctrl_proc},
     iter_max_socks,
     {group, close},
     {group, active},
     {group, shutdown},
     {group, econnreset},
     {group, linger_zero},
     default_options, http_bad_packet, busy_send,
     {group, busy_disconnect},
     fill_sendq,
     {group, partial_recv_and_close},
     so_priority,
     {group, pktoptions},
     {group, accept},
     {group, send_timeout},
     otp_7731,
     wrapping_oct,
     zombie_sockets,
     otp_7816,
     otp_8102, otp_9389,
     otp_12242, delay_send_error,
     bidirectional_traffic,
     {group, socket_monitor},
     otp_17492
    ].

close_cases() ->
    [
     no_accept,
     close_with_pending_output,
     data_before_close,
     accept_closed_by_other_process,
     otp_3924,
     closed_socket
    ].

ctrl_proc_cases() ->
    [
     controlling_process,
     controlling_process_self
    ].

active_cases() ->
    [
     passive_sockets,
     active_n,
     active_n_closed,
     active_once_closed
    ].

shutdown_cases() ->
    [
     shutdown_active,
     shutdown_passive,
     shutdown_pending
    ].

econnreset_cases() ->
    [
     show_econnreset_active,
     show_econnreset_active_once,
     show_econnreset_passive,
     econnreset_after_sync_send,
     econnreset_after_async_send_active,
     econnreset_after_async_send_active_once,
     econnreset_after_async_send_passive
    ].

linger_zero_cases() ->
    [
     linger_zero,
     linger_zero_sndbuf
    ].

busy_disconnect_cases() ->
    [
     busy_disconnect_passive,
     busy_disconnect_active
    ].

partial_recv_and_close_cases() ->
    [
     partial_recv_and_close,
     partial_recv_and_close_2,
     partial_recv_and_close_3
    ].

pktoptions_cases() ->
    [
     recvtos,
     recvttl,
     recvtosttl,
     recvtclass
    ].

accept_cases() ->
    [
     primitive_accept,
     multi_accept_close_listen,
     accept_timeout,
     accept_timeouts_in_order,
     accept_timeouts_in_order2,
     accept_timeouts_in_order3,
     accept_timeouts_in_order4,
     accept_timeouts_in_order5,
     accept_timeouts_in_order6,
     accept_timeouts_in_order7,
     accept_timeouts_mixed,
     killing_acceptor,
     killing_multi_acceptors,
     killing_multi_acceptors2,
     several_accepts_in_one_go,
     accept_system_limit
    ].

send_timeout_cases() ->
    [
     send_timeout,
     send_timeout_active
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
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-define(UNIQ_NODE_NAME,
  list_to_atom(?MODULE_STRING ++ "__" ++
               atom_to_list(?FUNCTION_NAME) ++ "_" ++
	       integer_to_list(erlang:unique_integer([positive])))).

%% Tests kernel application variables inet_default_listen_options and
%% inet_default_connect_options.
default_options(Config) when is_list(Config) ->
    ?TC_TRY(default_options, fun() -> do_default_options(Config) end).

do_default_options(Config) ->
    %% First check the delay_send option
    {true,true,true}=do_delay_send_1(Config),
    {false,false,false}=do_delay_send_2(Config),
    {true,false,false}=do_delay_send_3(Config),
    {false,false,false}=do_delay_send_4(Config),
    {false,false,false}=do_delay_send_5(Config),
    {false,true,true}=do_delay_send_6(Config),
    %% Now lets start some nodes with different combinations of options:
    {true,true,true} =
        do_delay_on_other_node("", fun() -> do_delay_send_1(Config) end),
    {true,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_2(Config) end),
    
    {false,true,true} =
	do_delay_on_other_node("-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_2(Config) end),
    
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_3(Config) end),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_6(Config) end),
    {false,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_5(Config) end),
    {false,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_5(Config) end),
    {true,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun() -> do_delay_send_4(Config) end),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"{delay_send,true}\" "
			       "-kernel inet_default_listen_options "
			       "\"{delay_send,true}\"",
			       fun() -> do_delay_send_2(Config) end),
    %% Active is to dangerous and is supressed
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"{active,false}\" "
			       "-kernel inet_default_listen_options "
			       "\"{active,false}\"",
			       fun() -> do_delay_send_7(Config) end),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{active,false},{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{active,false},{delay_send,true}]\"",
			       fun() -> do_delay_send_7(Config) end),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{active,false},{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{active,false},{delay_send,true}]\"",
			       fun() -> do_delay_send_2(Config) end),
    ok.


do_delay_on_other_node(XArgs, Function) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Node} = ?START_SLAVE_NODE(?UNIQ_NODE_NAME,
                                   "-pa " ++ Dir ++ " " ++ XArgs),
    Res = rpc:call(Node,erlang,apply,[Function,[]]),
    ?STOP_NODE(Node),
    Res.

do_delay_send_1(Config) ->
    {ok,LS} = ?LISTEN(Config, 0, [{delay_send,true}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    S = case ?CONNECT(Config, "localhost", PortNum, [{delay_send,true}]) of
            {ok, Sock} ->
                Sock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.

do_delay_send_2(Config) ->
    {ok, LS} = ?LISTEN(Config),
    {ok, {{0,0,0,0},PortNum}} = inet:sockname(LS),
    {ok, S}  = ?CONNECT(Config, "localhost",PortNum,[]),
    {ok, S2} = gen_tcp:accept(LS),
    {ok, [{delay_send,B1}]} = inet:getopts(S,[delay_send]),
    {ok, [{delay_send,B2}]} = inet:getopts(LS,[delay_send]),
    {ok, [{delay_send,B3}]} = inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.

do_delay_send_3(Config) ->
    {ok, LS} = ?LISTEN(Config, 0, []),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S} = ?CONNECT(Config, "localhost", PortNum, [{delay_send,true}]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_4(Config) ->
    {ok,LS} = ?LISTEN(Config, 0, [{delay_send,false}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}  = ?CONNECT(Config, "localhost", PortNum, []),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_5(Config) ->
    {ok,LS} = ?LISTEN(Config, 0, []),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S} = ?CONNECT(Config, "localhost",PortNum,[{delay_send,false}]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_6(Config) ->
    {ok,LS} = ?LISTEN(Config, 0, [{delay_send,true}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S} = ?CONNECT(Config, "localhost", PortNum, []),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_7(Config) ->
    {ok,LS} = ?LISTEN(Config, 0, []),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}  = ?CONNECT(Config, "localhost", PortNum, []),
    {ok,S2} = gen_tcp:accept(LS),
    {ok,[{active,B1}]}=inet:getopts(S,[active]),
    {ok,[{active,B2}]}=inet:getopts(LS,[active]),
    {ok,[{active,B3}]}=inet:getopts(S2,[active]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.

%% Open a listen port and change controlling_process for it
%% The result should be ok of done by the owner process,
%% Otherwise is should return {error,not_owner} or similar.
controlling_process(Config) when is_list(Config) ->
    {ok, S} = ?LISTEN(Config, 0,[]),
    Pid2    = spawn(?MODULE, not_owner, [S]),
    Pid2 ! {self(),2,control},
    {error, E} = receive {2,_E} ->
			      _E
		      after 10000 -> timeout
		      end,
    io:format("received ~p~n",[E]),
    Pid = spawn(?MODULE,not_owner,[S]),
    ok = gen_tcp:controlling_process(S,Pid),
    Pid ! {self(),1,control},
    ok = receive {1,ok} ->
                     ok
         after 1000 -> timeout
         end,
    Pid ! close.

not_owner(S) ->
    receive
	{From,Tag,control} ->
	    From ! {Tag,gen_tcp:controlling_process(S,self())};
	close ->
	    gen_tcp:close(S)
    after 1000 ->
	    ok
    end.

%% Open a listen port and assign the controlling process to
%% it self, then exit and make sure the port is closed properly.
controlling_process_self(Config) when is_list(Config) ->
    S = self(),
    process_flag(trap_exit,true),
    spawn_link(fun() ->
		       {ok,Sock} = ?LISTEN(Config, 0, []),
		       S ! {socket, Sock},
		       ok = gen_tcp:controlling_process(Sock,self()),
		       S ! done
	       end),
    receive
	done ->
	    receive
		{socket,Sock} ->
		    process_flag(trap_exit,false),
		    %% Make sure the port is invalid after process crash
                    receive after 500 -> ok end,
                    case inet:port(Sock) of
                        {error,einval} -> ok;
                        {error,closed} -> ok % XXX gen_tcp_socket
                    end

	    end;
	Msg when element(1,Msg) /= socket ->
	    process_flag(trap_exit,false),
	    exit({unknown_msg,Msg})
    end.
    

%% Open a listen port and connect to it, then close the listen port
%% without doing any accept.  The connected socket should receive
%% a tcp_closed message.
no_accept(Config) when is_list(Config) ->
    {ok, L}         = ?LISTEN(Config, 0, []),
    {ok, {_, Port}} = inet:sockname(L),
    {ok, Client}    = ?CONNECT(Config, localhost, Port, []),
    ok = gen_tcp:close(L),
    receive
        {tcp_closed, Client} ->
            ok
    after 5000 ->
            ct:fail(never_closed)
    
    end.

%% Send several packets to a socket and close it.  All packets should
%% arrive to the other end.
close_with_pending_output(Config) when is_list(Config) ->
    {ok, L} = ?LISTEN(Config, 0, [binary, {active, false}]),
    {ok, {_, Port}} = inet:sockname(L),
    Packets = 16,
    Total = 2048*Packets,
    case start_remote(close_pending) of
	{ok, Node} ->
	    {ok, Host} = inet:gethostname(),
	    spawn_link(Node, ?MODULE, sender, [Config, Port, Packets, Host]),
	    {ok, A} = gen_tcp:accept(L),
	    case gen_tcp:recv(A, Total) of
		      {ok, Bin} when byte_size(Bin) == Total ->
			  gen_tcp:close(A),
			  gen_tcp:close(L);
		      {ok, Bin} ->
			  ct:fail({small_packet,
                                                  byte_size(Bin)});
		      Error ->
			  ct:fail({unexpected, Error})
		  end,
	    ok;
	{error, no_remote_hosts} ->
	    {skipped,"No remote hosts"};
	{error, Other} ->
	    ct:fail({failed_to_start_slave_node, Other})
    end.

sender(Config, Port, Packets, Host) ->
    X256 = lists:seq(0, 255),
    X512 = [X256|X256],
    X1K = [X512|X512],
    Bin = list_to_binary([X1K|X1K]),
    {ok, Sock} = ?CONNECT(Config, Host, Port, []),
    send_loop(Sock, Bin, Packets),
    ok = gen_tcp:close(Sock).

send_loop(_Sock, _Data, 0) -> ok;
send_loop(Sock, Data, Left) ->
    ok = gen_tcp:send(Sock, Data),
    send_loop(Sock, Data, Left-1).

%% Test {active,N} option
%% Verify operation of the {active,N} option.
active_n(Config) when is_list(Config) ->
    ?TC_TRY(active_n, fun() -> do_active_n(Config) end).

do_active_n(Config) ->
    N = 3,
    LS = ok(?LISTEN(Config, 0, [{active,N}])),
    [{active,N}] = ok(inet:getopts(LS, [active])),
    ok = inet:setopts(LS, [{active,-N}]),
    receive
        {tcp_passive, LS} -> ok
    after
        5000 ->
            exit({error,tcp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(LS, [active])),
    ok = inet:setopts(LS, [{active,0}]),
    receive
        {tcp_passive, LS} -> ok
    after
        5000 ->
            exit({error,tcp_passive_failure})
    end,
    ok = inet:setopts(LS, [{active,32767}]),
    {error,einval} = inet:setopts(LS, [{active,1}]),
    {error,einval} = inet:setopts(LS, [{active,-32769}]),
    ok = inet:setopts(LS, [{active,-32768}]),
    receive
        {tcp_passive, LS} -> ok
    after
        5000 ->
            exit({error,tcp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(LS, [active])),
    ok = inet:setopts(LS, [{active,N}]),
    ok = inet:setopts(LS, [{active,true}]),
    [{active,true}] = ok(inet:getopts(LS, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    ok = inet:setopts(LS, [{active,N}]),
    ok = inet:setopts(LS, [{active,once}]),
    [{active,once}] = ok(inet:getopts(LS, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    {error,einval} = inet:setopts(LS, [{active,32768}]),
    ok = inet:setopts(LS, [{active,false}]),
    [{active,false}] = ok(inet:getopts(LS, [active])),
    Port = ok(inet:port(LS)),
    C = case ?CONNECT(Config, "localhost", Port, [{active,N}]) of
            {ok, CS} ->
                CS;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    [{active,N}] = ok(inet:getopts(C, [active])),
    S = ok(gen_tcp:accept(LS)),
    ok = inet:setopts(S, [{active,N}]),
    [{active,N}] = ok(inet:getopts(S, [active])),
    repeat(3,
           fun(I) ->
                   Msg = "message "++integer_to_list(I),
                   ok = gen_tcp:send(C, Msg),
                   receive
                       {tcp,S,Msg} ->
                           ok = gen_tcp:send(S, Msg)
                   after
                       5000 ->
                           exit({error,timeout})
                   end,
                   receive
                       {tcp,C,Msg} ->
                           ok
                   after
                       5000 ->
                           exit({error,timeout})
                   end
           end),
    receive
        {tcp_passive,S} ->
            [{active,false}] = ok(inet:getopts(S, [active]))
    after
        5000 ->
            exit({error,tcp_passive})
    end,
    receive
        {tcp_passive,C} ->
            [{active,false}] = ok(inet:getopts(C, [active]))
    after
        5000 ->
            exit({error,tcp_passive})
    end,
    LS2 = ok(?LISTEN(Config, 0, [{active,0}])),
    receive
        {tcp_passive,LS2} ->
            [{active,false}] = ok(inet:getopts(LS2, [active]))
    after
        5000 ->
            exit({error,tcp_passive})
    end,
    ok = gen_tcp:close(LS2),
    ok = gen_tcp:close(C),
    ok = gen_tcp:close(S),
    ok = gen_tcp:close(LS),
    ok.

-define(OTP_3924_MAX_DELAY, 100).
%% Taken out of the blue, but on intra host connections
%% I expect propagation of a close to be quite fast
%% so 100 ms seems reasonable.

%% Tests that a socket can be closed fast enough.
otp_3924(Config) when is_list(Config) ->
    MaxDelay = (case has_superfluous_schedulers() of
	    true -> 4;
	    false -> 1
	end
	* case {erlang:system_info(debug_compiled),
		erlang:system_info(lock_checking)} of
	    {true, _} -> 6;
	    {_, true} -> 2;
	    _ -> 1
	end * ?OTP_3924_MAX_DELAY),
    otp_3924_1(Config, MaxDelay).

otp_3924_1(Config, MaxDelay) ->
    {ok, Node} = start_node(otp_3924),
    DataLen = 100*1024,
    Data = otp_3924_data(DataLen),
    %% Repeat the test a couple of times to prevent the test from passing
    %% by chance.
    repeat(10, fun(N) ->
                       ok = otp_3924(Config, MaxDelay, Node, Data, DataLen, N)
               end),
    ?STOP_NODE(Node),
    ok.

otp_3924(Config, MaxDelay, Node, Data, DataLen, N) ->
    {ok, L} = ?LISTEN(Config, 0, [list, {active, false}]),
    {ok, {_, Port}} = inet:sockname(L),
    {ok, Host} = inet:gethostname(),
    Sender = spawn_link(Node,
                        ?MODULE,
                        otp_3924_sender,
                        [Config, self(), Host, Port, Data]),
    Data = otp_3924_receive_data(L, Sender, MaxDelay, DataLen, N),
    ok = gen_tcp:close(L).

otp_3924_receive_data(LSock, Sender, MaxDelay, Len, N) ->
    OP = process_flag(priority, max),
    OTE = process_flag(trap_exit, true),
    TimeoutRef = make_ref(),
    Data = (catch begin
                      Sender ! start,
                      {ok, Sock} = gen_tcp:accept(LSock),
                      D = otp_3924_receive_data(Sock,
                                                TimeoutRef,
                                                MaxDelay,
                                                Len,
                                                [],
                                                0),
                      ok = gen_tcp:close(Sock),
                      D
                  end),
    unlink(Sender),
    process_flag(trap_exit, OTE),
    process_flag(priority, OP),
    receive
	{'EXIT', _, TimeoutRef} ->
	    ct:fail({close_not_fast_enough,MaxDelay,N});
	{'EXIT', Sender, Reason} ->
	    ct:fail({sender_exited, Reason});
	{'EXIT', _Other, Reason} ->
	    ct:fail({linked_process_exited, Reason})
    after 0 ->
	    case Data of
		{'EXIT', {A,B}} ->
		    ct:fail({A,B,N});
		{'EXIT', Failure} ->
		    ct:fail(Failure);
		_ ->
		    Data
	    end
    end.
    

otp_3924_receive_data(Sock, TimeoutRef, MaxDelay, Len, Acc, AccLen) ->
    case gen_tcp:recv(Sock, 0) of
	{ok, Data} ->
	    NewAccLen = AccLen + length(Data),
	    if
		NewAccLen == Len ->
		    {ok, TRef} = timer:exit_after(MaxDelay,
							self(),
							TimeoutRef),
		    {error, closed} = gen_tcp:recv(Sock, 0),
		    timer:cancel(TRef),
		    lists:flatten([Acc, Data]);
		NewAccLen > Len ->
		    exit({received_too_much, NewAccLen});
		true ->
		    otp_3924_receive_data(Sock,
					  TimeoutRef,
					  MaxDelay,
					  Len,
					  [Acc, Data],
					  NewAccLen)
	    end;
	{error, closed} ->
	    exit({premature_close, AccLen});
	Error ->
	    exit({unexpected_error, Error})
    end.
    
otp_3924_data(Size) ->
    Block =
	"This is a sequence of characters that will be repeated "
	"again and again and again and again and again and ...  ",
    L = length(Block), 
    otp_3924_data(Block, [], Size div L, Size rem L).

otp_3924_data(_, Acc, 0, 0) ->
    lists:flatten(Acc);
otp_3924_data(_, Acc, 0, SingleLeft) ->
    otp_3924_data(false, ["."|Acc], 0, SingleLeft-1);
otp_3924_data(Block, Acc, BlockLeft, SingleLeft) ->
    otp_3924_data(Block, [Block|Acc], BlockLeft-1, SingleLeft).

otp_3924_sender(Config, Receiver, Host, Port, Data) ->
    receive
	start ->
	    {ok, Sock} = ?CONNECT(Config, Host, Port, [list]),
	    gen_tcp:send(Sock, Data),
	    ok = gen_tcp:close(Sock),
	    unlink(Receiver)
    end.


%% Tests that a huge amount of data can be received before a close.
data_before_close(Config) when is_list(Config) ->
    {ok, L} = ?LISTEN(Config, 0, [binary]),
    {ok, {_, TcpPort}} = inet:sockname(L),
    Bytes = 256*1024,
    spawn_link(fun() -> huge_sender(Config, TcpPort, Bytes) end),
    {ok, A} = gen_tcp:accept(L),
    case count_bytes_recv(A, 0) of
        {Bytes, Result} ->
            io:format("Result: ~p", [Result]);
        {Wrong, Result} ->
            io:format("Result: ~p", [Result]),
            ct:fail({wrong_count, Wrong})
    end,
    ok.

count_bytes_recv(Sock, Total) ->
    receive
	{tcp, Sock, Bin} ->
	    count_bytes_recv(Sock, Total+byte_size(Bin));
	Other ->
	    {Total, Other}
    end.

huge_sender(Config, TcpPort, Bytes) ->
    {ok, Client} = ?CONNECT(Config, localhost, TcpPort, []),
    receive after 500 -> ok end,
    gen_tcp:send(Client, make_zero_packet(Bytes)),
    gen_tcp:close(Client).

make_zero_packet(0) -> [];
make_zero_packet(N) when N rem 2 == 0 ->
    P = make_zero_packet(N div 2),
    [P|P];
make_zero_packet(N) ->
    P = make_zero_packet(N div 2),
    [0, P|P].

%% OTP-2924. Test that the socket process does not crash when
%% sys:get_status(Pid) is called.
get_status(Config) when is_list(Config) ->
    {ok,{socket,Pid,_,_}} = ?LISTEN(Config, 5678,[]),
    {status,Pid,_,_} = sys:get_status(Pid).

-define(RECOVER_SLEEP, 60000).
-define(RETRY_SLEEP, 15000).

iter_max_socks() ->
    [{timetrap,{minutes,30}}].

%% Open as many sockets as possible. Do this several times and check
%% that we get the same number of sockets every time.
iter_max_socks(Config) when is_list(Config) ->
    %% This is not *nearly* enough
    %% We have some crap machines, which we need to "handle with care"...
    Tries =
        case os:type() of
            {win32, _} ->
                10;
            {unix, darwin} ->
                10;
            _ ->
                20
        end,
    %% Run on a different node in order to limit the effect if this test fails.
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, Node} = ?START_SLAVE_NODE(test_iter_max_socks, "+Q 2048 -pa " ++ Dir),
    %% L = rpc:call(Node,?MODULE,do_iter_max_socks,[N, initalize]),
    L = iter_max_socks_run(Node,
                           fun() ->
                                   exit(do_iter_max_socks(Config, Tries, initalize))
                           end),
    ?STOP_NODE(Node),

    io:format("Result: ~p", [L]),
    all_equal(L),
    {comment, "Max sockets: " ++ integer_to_list(hd(L))}.

iter_max_socks_run(Node, F) ->
    try erlang:spawn_opt(Node, F, [monitor]) of
        {Pid, MRef} when is_pid(Pid) andalso is_reference(MRef) ->
            receive
                {'DOWN', MRef, process, Pid, Res} ->
                    Res
            end;
        _Any ->
            ?P("Unexpected process start result: "
               "~n   ~p", [_Any]),
            {skip, "Failed starting iterator (slave) process"}
    catch
        C:E:S ->
            ?P("Failed starting iterator (slave) process: "
               "~n   Class: ~p"
               "~n   Error: ~p"
               "~n   Stack: ~p", [C, E, S]),
            {skip, "Failed starting iterator (slave) process"}
    end.
            
             
do_iter_max_socks(_Config, 0, _) ->
    ?P("do_iter_max_socks(0,-) -> done"),
    [];
do_iter_max_socks(Config, N, initalize = First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(Config),
    [MS|do_iter_max_socks(Config, N-1, MS)];
do_iter_max_socks(Config, N, failed = First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(Config),
    [MS|do_iter_max_socks(Config, N-1, failed)];
do_iter_max_socks(Config, N, First) when is_integer(First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(Config),
    if
        (MS =:= First) -> 
	    [MS|do_iter_max_socks(Config, N-1, First)];
       true ->
	    ?P("~w =/= ~w => sleeping for ~p seconds...",
               [MS, First, ?RETRY_SLEEP/1000]), 
	    ct:sleep(?RETRY_SLEEP),
	    ?P("Trying again...", []),
	    RetryMS = max_socks(Config),
	    if RetryMS == First ->
                    [RetryMS|do_iter_max_socks(Config, N-1, First)];
               true ->
                    [RetryMS|do_iter_max_socks(Config, N-1, failed)]
		  end
    end.

all_equal([]) ->
    ok;
all_equal([Rule | T]) ->
    all_equal(Rule, T).

all_equal(Rule, [Rule | T]) ->
    all_equal(Rule, T);
all_equal(_, [_ | _]) ->
    ct:sleep(?RECOVER_SLEEP), % Wait a while and *hope* that we'll
                              % recover so other tests won't be
                              % affected.
    ct:fail(max_socket_mismatch);
all_equal(_Rule, []) ->
    ok.

max_socks(Config) ->
    Socks = open_socks(Config),
    N = length(Socks),
    lists:foreach(fun(S) -> ok = gen_tcp:close(S) end, Socks),
    ?P("Got ~p sockets", [N]),
    N.

open_socks(Config) ->
    case ?LISTEN(Config, 0, []) of
	{ok, L} ->
	    {ok, {_, Port}} = inet:sockname(L),
	    [L| connect_accept(Config, L, Port)];
	_ ->
	    []
    end.

connect_accept(Config, L, Port) ->
    case ?CONNECT(Config, localhost, Port, []) of
	{ok, C} ->
	    [C| do_accept(Config, L, Port)];
	_ ->
	    []
    end.

do_accept(Config, L, Port) ->
    case gen_tcp:accept(L) of
	{ok, A} -> [A| connect_accept(Config, L, Port)];
	_ -> []
    end.

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    ?START_SLAVE_NODE(Name, "-pa " ++ Pa).

start_remote(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    ?START_SLAVE_NODE(Name, "-pa " ++ Pa, [{remote, true}]).

%% Tests that when 'the other side' on a passive socket closes, the
%% connecting side can still read until the end of data.
passive_sockets(Config) when is_list(Config) ->
    spawn_link(?MODULE, passive_sockets_server,
               [Config, [{active, false}], self()]),
    receive
        {socket,Port} -> ok
    end,
    ct:sleep(500),
    case ?CONNECT(Config, "localhost", Port, [{active, false}]) of
        {ok, Sock} ->
            passive_sockets_read(Sock);
        {error, eaddrnotavail = Reason} ->
            {skip, connect_failed_str(Reason)};
        Error ->
            ct:fail({"Could not connect to server", Error})
    end.

%%
%% Read until we get an {error, closed}. If we get another error, this test case
%% should fail.
%%
passive_sockets_read(Sock) ->
    case gen_tcp:recv(Sock, 0, 2000) of
	{ok, Data} ->
	    io:format("Received ~p bytes~n", [length(Data)]),
	    passive_sockets_read(Sock);
	{error, closed} ->
	    gen_tcp:close(Sock);
	Error ->
	    gen_tcp:close(Sock),
	    ct:fail({"Did not get {error, closed} before other error", Error})
    end.

passive_sockets_server(Config, Opts, Parent) ->
    case ?LISTEN(Config, 0, Opts) of
        {ok, LSock} ->
            {ok,{_,Port}} = inet:sockname(LSock),
            Parent ! {socket,Port},
            passive_sockets_server_accept(LSock);
        Error ->
            ct:fail({"Could not create listen socket", Error})
    end.

passive_sockets_server_accept(Sock) ->
    case gen_tcp:accept(Sock) of
        {ok, Socket} ->
            timer:sleep(500),			% Simulate latency
            passive_sockets_server_send(Socket, 5),
            passive_sockets_server_accept(Sock);
        Error ->
            ct:fail({"Could not accept connection", Error})
    end.

passive_sockets_server_send(Socket, 0) ->
    io:format("Closing other end..~n", []),
    gen_tcp:close(Socket);
passive_sockets_server_send(Socket, X) ->
    Data = lists:duplicate(1024*X, $a),
    case gen_tcp:send(Socket, Data) of
        ok ->
            ct:sleep(50),   % Simulate some processing.
            passive_sockets_server_send(Socket, X-1);
        {error, _Reason} ->
            ct:fail("Failed to send data")
    end.


%% Tests the return value from gen_tcp:accept when
%% the socket is closed from another process. (OTP-3817)
accept_closed_by_other_process(Config) when is_list(Config) ->
    Parent = self(),
    {ok, ListenSocket} = ?LISTEN(Config, 0, []),
    Child =
	spawn_link(
	  fun() ->
		  Parent ! {self(), gen_tcp:accept(ListenSocket)}
	  end),
    receive after 1000 -> ok end,
    ok = gen_tcp:close(ListenSocket),
    receive
        {Child, {error, closed}} ->
            ok;
        {Child, Other} ->
            ct:fail({"Wrong result of gen_tcp:accept", Other})
    end.

repeat(N, Fun) ->
    repeat(N, N, Fun).

repeat(N, T, Fun) when is_integer(N), N > 0 ->
    Fun(T-N),
    repeat(N-1, T, Fun);
repeat(_, _, _) ->
    ok.


%% Tests the response when using a closed socket as argument.
closed_socket(Config) when is_list(Config) ->
    {ok, LS1} = ?LISTEN(Config, 0, []),
    erlang:yield(),
    ok = gen_tcp:close(LS1),
    %% If the following delay is uncommented, the result error values
    %% below will change from {error, einval} to {error, closed} since
    %% inet_db then will have noticed that the socket is closed.
    %% This is a scheduling issue, i.e when the gen_server in 
    %% in inet_db processes the 'EXIT' message from the port,
    %% the socket is unregistered.
    %%
    %% ct:sleep({seconds,2})
    %%
    {error, R_send} = gen_tcp:send(LS1, "data"),
    {error, R_recv} = gen_tcp:recv(LS1, 17),
    {error, R_accept} = gen_tcp:accept(LS1),
    {error, R_controlling_process} =
	gen_tcp:controlling_process(LS1, self()),
    %%
    ok = ?P("R_send = ~p", [R_send]),
    ok = ?P("R_recv = ~p", [R_recv]),
    ok = ?P("R_accept = ~p", [R_accept]),
    ok = ?P("R_controlling_process = ~p", [R_controlling_process]),
    ok.

%%%
%%% Test using the gen_tcp:shutdown/2 function using a sort server.
%%% 

shutdown_active(Config) when is_list(Config) ->
    ?TC_TRY(shutdown_active, fun() -> shutdown_common(Config, true) end).

shutdown_passive(Config) when is_list(Config) ->
    ?TC_TRY(shutdown_passive, fun() -> shutdown_common(Config, false) end).

shutdown_common(Config, Active) ->
    ?P("start sort server"),
    P = sort_server(Config, Active),
    ?P("Sort server port: ~p", [P]),


    do_sort(Config, P, []),
    do_sort(Config, P, ["glurf"]),
    do_sort(Config, P, ["abc","nisse","dum"]),

    do_sort(Config, P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 255)]),
    do_sort(Config, P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(77, 999)]),
    do_sort(Config, P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 55)]),
    do_sort(Config, P, []),
    do_sort(Config, P, ["apa"]),
    do_sort(Config, P, ["kluns","gorilla"]),
    do_sort(Config, P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 1233)]),
    do_sort(Config, P, []),
    receive
	Any ->
	    ct:fail({unexpected_message,Any})
    after 0 -> ok
    end.

do_sort(Config, P, List0) ->
    ?P("Sort: "
       "~n   ~p", [List0]),
    List = [El++"\n" || El <- List0],
    S = case ?CONNECT(Config, localhost, P, [{packet,line}]) of
            {ok, Socket} ->
                Socket;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    send_lines(S, List),
    ok = gen_tcp:shutdown(S, write),
    Lines = collect_lines(S, true),
    ?P("Collected: "
       "~n   ~p", [Lines]),
    SortedLines = lists:sort(List),
    ?P("Sorted: "
       "~n   ~p", [SortedLines]),
    Lines = SortedLines,
    ok = gen_tcp:close(S).

sort_server(Config, Active) ->
    Opts = [{exit_on_close,false},{packet,line},{active,Active}],
    {ok,L} = ?LISTEN(Config, 0, Opts),
    Go = make_ref(),
    Pid = spawn_link(fun() ->
                             receive Go -> sort_server_1(L, Active) end
                     end),
    ok = gen_tcp:controlling_process(L, Pid),
    Pid ! Go,
    {ok,Port} = inet:port(L),
    Port.

sort_server_1(L, Active) ->
    {ok,S} = gen_tcp:accept(L),
    Go = make_ref(),
    Sorter = spawn(fun() -> receive Go -> sorter(S, Active) end end),
    ok = gen_tcp:controlling_process(S, Sorter),
    Sorter ! Go,
    sort_server_1(L, Active).

sorter(S, Active) ->
    Lines = collect_lines(S, Active),
    send_lines(S, lists:sort(Lines)),
    gen_tcp:shutdown(S, write),
    gen_tcp:close(S).

collect_lines(S, true) ->
    collect_lines_1(S, []);
collect_lines(S, false) ->
    passive_collect_lines_1(S, []).

collect_lines_1(S, Acc) ->
    receive
	{tcp,S,Line} ->
            ?P("collect_lines_1(~w): ~p", [S, Line]),
            collect_lines_1(S, [Line|Acc]);
	{tcp_closed,S} ->
            ?P("collect_lines_1(~w): tcp_closed", [S]),
            lists:reverse(Acc)
    end.

passive_collect_lines_1(S, Acc) ->
    case gen_tcp:recv(S, 0) of
	{ok,Line} -> passive_collect_lines_1(S, [Line|Acc]);
	{error,closed} -> lists:reverse(Acc)
    end.


send_lines(S, Lines) ->    
    lists:foreach(fun(Line) ->
                          ?P("send_line(~w): ~p", [S, Line]),
			  ok = gen_tcp:send(S, Line)
		  end, Lines).

%%%
%%% Shutdown pending.
%%%

shutdown_pending(Config) when is_list(Config) ->
    ?TC_TRY(shutdown_pending, fun() -> do_shutdown_pending(Config) end).

do_shutdown_pending(Config) ->
    N = 512*1024+17,
    ?P("N: ~p", [N]),
    Data = [<<N:32>>,ones(N),42],
    {Port, Pid} = a_server(Config),
    ?P("try connect to server (port: ~p)", [Port]),
    S = case ?CONNECT(Config, localhost, Port, []) of
            {ok, Socket} ->
                ?P("connected"),
                Socket;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    ?P("send"),
    gen_tcp:send(S, Data),
    ?P("shutdown(write)"),
    gen_tcp:shutdown(S, write),
    ?P("await data message"),
    case sp_await_data(Pid, S) of
        N ->
            ok;
        InvalidN ->
            ?P("Invalid message: "
               "~n   Expected: ~p"
               "~n   Received: ~p", [N, InvalidN]),
            ct:fail({unexpected_msg, N, InvalidN})
    end,
    ?P("done"),
    ok.

sp_await_data(Pid, Sock) ->
    receive
        {tcp, Sock, Msg} ->
            ?P("got tcp (data) message: ~p", [Msg]),
            list_to_integer(Msg) - 5;
        {'EXIT', Pid, normal} ->
            ?P("server exited normal"),
            sp_await_data(Pid, Sock);
        Other ->
            ?P("UNEXPECTED: "
               "~n   ~p", [Other]),
            ct:fail({unexpected, Other})
    end.

 ones(0) -> [];
 ones(1) -> [1];
 ones(N) ->
     Half = N div 2,
     Ones = ones(Half),
     case 2*Half of
	 N -> [Ones|Ones];
	 _ -> [1,Ones|Ones]
     end.

 a_server(Config) ->
     {ok, L}    = ?LISTEN(Config, 0, [{exit_on_close,false},{active,false}]),
     Pid        = spawn_link(fun() -> a_server2(L) end),
     ok         = gen_tcp:controlling_process(L, Pid),
     {ok, Port} = inet:port(L),
     {Port, Pid}.

a_server2(L) ->
     {ok,S} = gen_tcp:accept(L),
     do_recv(S, []).

 do_recv(S, Bs0) ->
     case gen_tcp:recv(S, 0) of
	 {ok,B} ->
	     do_recv(S, [Bs0,B]);
	 {error,closed} ->
	     Bs = list_to_binary(Bs0),
	     gen_tcp:send(S, integer_to_list(byte_size(Bs))),
	     gen_tcp:close(S)
     end.

%%
%% Test 'show_econnreset' option
%%

show_econnreset_active(Config) when is_list(Config) ->
    ?TC_TRY(show_econnreset_active,
            fun() -> do_show_econnreset_active(Config) end).

do_show_econnreset_active(Config) ->
    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L0} = ?LISTEN(Config, 0, []),
    {ok, Port0} = inet:port(L0),
    Client0 = case ?CONNECT(Config, localhost, Port0, [{active, false}]) of
                 {ok, CSock0} ->
                     CSock0;
                  {error, eaddrnotavail = Reason0} ->
                      ?SKIPT(connect_failed_str(Reason0))
              end,
    {ok, S0} = gen_tcp:accept(L0),
    ok = gen_tcp:close(L0),
    ok = inet:setopts(Client0, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client0),
    receive
       {tcp_closed, S0} ->
	   ok;
       Other0 ->
	   ct:fail({unexpected, off, closed, Other0})
    after 1000 ->
	ct:fail({timeout, {server, no_tcp_closed}})
    end,

    %% Now test with option switched on.
    %% Note: We are also testing that the show_econnreset option is
    %% inherited from the listening socket by the accepting socket.
    ?P("test with option explicitly switched on"),
    {ok, L1} = ?LISTEN(Config, 0, [{show_econnreset, true}]),
    {ok, Port1} = inet:port(L1),
    Client1 = case ?CONNECT(Config, localhost, Port1, [{active, false}]) of
                  {ok, CSock1} ->
                      CSock1;
                  {error, eaddrnotavail = Reason1} ->
                      ?SKIPT(connect_failed_str(Reason1))
              end,
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(Client1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client1),
    receive
	{tcp_error, S1, econnreset} ->
	    receive
		{tcp_closed, S1} ->
                    ?P("done"),
		    ok;
		Other1 ->
                    ?P("UNEXPECTED (expected closed):"
                       "~n   ~p", [Other1]),
		    ct:fail({unexpected, on, closed, Other1})
	    after 1 ->
                    ?P("UNEXPECTED timeout (expected closed)"),
                    ct:fail({timeout, {server, no_tcp_closed}})
	    end;
	Other2 ->
            ?P("UNEXPECTED (expected error:econnreset):"
               "~n   ~p", [Other2]),
	    ct:fail({unexpected, on, econnreset, Other2})
    after 1000 ->
            ?P("UNEXPECTED timeout (expected error:econnreset)"),
            ct:fail({timeout, {server, no_tcp_error}})
    end.

show_econnreset_active_once(Config) when is_list(Config) ->
    ?TC_TRY(show_econnreset_active_once,
            fun() -> do_show_econnreset_active_once(Config) end).

do_show_econnreset_active_once(Config) ->
    %% Now test using {active, once}
    {ok, L} = ?LISTEN(Config, 0,
			   [{active, false},
			    {show_econnreset, true}]),
    {ok, Port} = inet:port(L),
    Client = case ?CONNECT(Config, localhost, Port, [{active, false}]) of
                 {ok, CSock} ->
                     CSock;
                  {error, eaddrnotavail = Reason} ->
                      ?SKIPT(connect_failed_str(Reason))
              end,
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(Client, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client),
    ok = ct:sleep(20),
    ok = receive Msg -> {unexpected_msg, Msg} after 0 -> ok end,
    ok = inet:setopts(S, [{active, once}]),
    receive
	{tcp_error, S, econnreset} ->
	    receive
		{tcp_closed, S} ->
		    ok;
		Other1 ->
		    ct:fail({unexpected1, Other1})
	    after 1 ->
		ct:fail({timeout, {server, no_tcp_closed}})
	    end;
	Other2 ->
	    ct:fail({unexpected2, Other2})
    after 1000 ->
	ct:fail({timeout, {server, no_tcp_error}})
    end.

show_econnreset_passive(Config) when is_list(Config) ->
    ?TC_TRY(show_econnreset_passive,
            fun() -> do_show_econnreset_passive(Config) end).

do_show_econnreset_passive(Config) ->
    %% First confirm everything works with option turned off.
    {ok, L} = ?LISTEN(Config, 0, [{active, false}]),
    {ok, Port} = inet:port(L),
    Client = case ?CONNECT(Config, localhost, Port, [{active, false}]) of
                 {ok, CSock} ->
                     CSock;
                 {error, eaddrnotavail = Reason} ->
                     ?SKIPT(connect_failed_str(Reason))
             end,
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(1),
    {error, closed} = gen_tcp:recv(Client, 0),

    %% Now test with option switched on.
    {ok, L1} = ?LISTEN(Config, 0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    Client1 =
        case ?CONNECT(Config, localhost, Port1,
                             [{active, false}, {show_econnreset, true}]) of
            {ok, CSock1} ->
                CSock1;
            {error, eaddrnotavail = Reason1} ->
                ?SKIPT(connect_failed_str(Reason1))
        end,            
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(1),
    {error, econnreset} = gen_tcp:recv(Client1, 0).

econnreset_after_sync_send(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_sync_send,
            fun() -> do_econnreset_after_sync_send(Config) end).

do_econnreset_after_sync_send(Config) ->
    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L} = ?LISTEN(Config, 0, [{active, false}]),
    {ok, Port} = inet:port(L),
    Client = case ?CONNECT(Config, localhost, Port, [{active, false}]) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(20),
    {error, closed} = gen_tcp:send(Client, "Whatever"),

    %% Now test with option switched on.
    ?P("test with option explicitly switched on"),
    {ok, L1} = ?LISTEN(Config, 0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    Client1 =
        case ?CONNECT(Config, localhost, Port1,
                             [{active, false}, {show_econnreset, true}]) of
            {ok, CSock1} ->
                CSock1;
            {error, eaddrnotavail = Reason1} ->
                ?SKIPT(connect_failed_str(Reason1))
        end,            
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(20),
    {error, econnreset} = gen_tcp:send(Client1, "Whatever").

econnreset_after_async_send_active(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_async_send_active,
            fun() -> do_econnreset_after_async_send_active(Config) end).

do_econnreset_after_async_send_active(Config) ->
    {OS, _}  = os:type(),
    CPayload = craasa_mk_payload(),
    SPayload = "Whatever",

    %% First confirm everything works with option turned off.
    ?P("pre 1 (default)"),
    {Client1, Server1} = craasa_pre(Config, default),

    ?P("populate 1 (default)"),
    {ok, Sender1} = craasa_populate(OS, default,
                                    Client1, Server1, CPayload, SPayload),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("[server] set linger true:0"),
    ok = inet:setopts(Server1, [{linger, {true, 0}}]),
    ?P("[server] close socket"),
    ok = gen_tcp:close(Server1),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("verify 1 (default)"),
    craasa_verify(default, Client1, SPayload),

    ?P("cleanup 1 (default)"),
    craasa_cleanup(Client1, Sender1),

    %% Now test with option switched on.
    ?P("pre 2 (true)"),
    {Client2, Server2} = craasa_pre(Config, true),

    ?P("populate 2 (true)"),
    {ok, Sender2} = craasa_populate(OS, true,
                                    Client2, Server2, CPayload, SPayload),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("[server] set linger true:0"),
    ok = inet:setopts(Server2, [{linger, {true, 0}}]),
    ?P("[server] close socket"),
    ok = gen_tcp:close(Server2),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("verify 2 (true)"),
    craasa_verify(true, Client2, SPayload),

    ?P("cleanup 2 (true)"),
    craasa_cleanup(Client2, Sender2),

    ?P("done"),
    ok.

craasa_mk_payload() ->
    list_to_binary(lists:duplicate(1024 * 1024, $.)).

craasa_pre(Config, EConnReset)
  when is_boolean(EConnReset) orelse (EConnReset =:= default) ->
    ?P("create listen socket (server) with active = false"),
    {ok, L}    = ?LISTEN(Config, 0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    ?P("[client] create connect socket with show_econnreset: ~p", [EConnReset]),
    Opts = if (EConnReset =:= default) -> [];
              is_boolean(EConnReset)   -> [{show_econnreset, EConnReset}]
           end,
    Client = case ?CONNECT(Config, localhost, Port, [{sndbuf, 4096}] ++ Opts) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    ?P("create accept socket (server)"),
    {ok, S} = gen_tcp:accept(L),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    {Client, S}.

craasa_populate(OS, _EConnReset, Client, Server, CPayload, SPayload)
  when is_port(Client) andalso is_port(Server) ->
    ?P("send payload (~w bytes) from client to server", [byte_size(CPayload)]),
    ok = gen_tcp:send(Client, CPayload),
    ?P("verify client socket queue size"),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ?P("[server] send something"),
    ok = gen_tcp:send(Server, SPayload),
    {ok, undefined};
craasa_populate(_OS, EConnReset, Client, Server, CPayload, SPayload) ->
    ExpectedSendRes = if (EConnReset =:= default) -> closed;
                         (EConnReset =:= true)    -> econnreset
                      end,
    Sender = spawn_link(fun() ->
                                craasa_populate_sender(Client,
                                                       ExpectedSendRes,
                                                       CPayload)
                        end),
    receive
        {'EXIT', Sender, Reason} ->
            {error, {unexpected_exit, Sender, Reason}}
    after 2000 ->
            ?P("[server] send something"),
            ok = gen_tcp:send(Server, SPayload),
            {ok, Sender}
    end.

craasa_populate_sender(Client, ExpectedSendRes, Payload) ->
    ?P("[socket] send payload (expect failure)"), 
    case gen_tcp:send(Client, Payload) of
        {error, ExpectedSendRes} ->
            ?P("[socket] payload send failure (as expected)"), 
            exit(normal);
        Unexpected ->
            exit({unexpected, Unexpected})
    end.

craasa_cleanup(Client, Sender) ->
    (catch gen_tcp:close(Client)),
    craasa_cleanup(Sender).

craasa_cleanup(Sender) when is_pid(Sender) ->
    exit(Sender, kill),
    receive
        {'EXIT', Sender, _} ->
            ok
    after 0 ->
            ok
    end;
craasa_cleanup(_) ->
    ok.

craasa_verify(default, Client, Payload) ->
    ?P("[verify-default] client await server data"),
    receive
	{tcp, Client, Payload} ->
            ?P("[verify-default] "
               "client received expected server data - "
               "now await socket closed"),
	    receive
		{tcp_closed, Client} ->
                    ?P("[verify-default] "
                       "received client socket closed"),
		    ok;
		Other1 ->
                    ?P("[verify-default] "
                       "awaiting client socket closed - received upexpected: "
                       "~n      ~p", [Other1]),
		    ct:fail({unexpected, tcp_closed, Other1})
	    end;
	Other2 ->
            ?P("[verify-default] "
               "awaiting client socket data - received upexpected: "
               "~n      ~p", [Other2]),
	    ct:fail({unexpected, tcp, Other2})
    end;
craasa_verify(true, Client, Payload) ->
    ?P("[verify-true] client await server data"),
    receive
	{tcp, Client, Payload} ->
            ?P("[verify-true] "
               "client received expected server data - now await socket error"),
	    receive
		{tcp_error, Client, econnreset} ->
                    ?P("[verify-true] "
                       "client received expected socket error - "
                       "now await socket closed"),
		    receive
			{tcp_closed, Client} ->
                            ?P("[verify-true] "
                               "client received expected socket closed"),
			    ok;
			Other1 ->
                            ?P("[verify-true] "
                               "client awaiting socket closed - "
                               "received upexpected: "
                               "~n      ~p", [Other1]),
			    ct:fail({unexpected, tcp_closed, Other1})
		    end;
		Other2 ->
                    ?P("[verify-true] "
                       "client awaiting socket error - received upexpected: "
                       "~n      ~p", [Other2]),
		    ct:fail({unexpected, tcp_error, Other2})
	    end;
	Other3 ->
            ?P("[verify-true] "
               "client awaiting socket data - received upexpected: "
               "~n      ~p", [Other3]),
	    ct:fail({unexpected, tcp, Other3})
    end.


%% --------------------------------------------------------------------------

econnreset_after_async_send_active_once(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_async_send_active_once,
            fun() -> do_econnreset_after_async_send_active_once(Config) end).

do_econnreset_after_async_send_active_once(Config) ->
    ?P("pre"),
    {Client, Server, CPayload, SPayload} = craasao_pre(Config),

    ?P("populate"),
    {ok, Sender} = craasao_populate(Client, Server, CPayload, SPayload),

    ?P("set server socket option linger: {true, 0}"),
    ok = inet:setopts(Server, [{linger, {true, 0}}]),
    ?P("close server socket"),
    ok = gen_tcp:close(Server),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("verify"),
    craasao_verify(Client, Sender, SPayload),

    ?P("cleanup"),
    craasao_cleanup(Client),

    ?P("done"),
    ok.

craasao_pre(Config) ->
    ?P("[pre] create listen socket with active = false"),
    {ok, L}    = ?LISTEN(Config, 0, [{active, false}, {recbuf, 4096}]),
    ?P("[pre] listen socket: "
       "~n      ~p", [L]),
    {ok, Port} = inet:port(L),
    ?P("[pre] create connect socket (~w)", [Port]),
    Client     = case ?CONNECT(Config, localhost, Port,
                               [{active,          false},
                                {sndbuf,          4096},
                                {show_econnreset, true}]) of
                     {ok, CSock} ->
                         ?P("[pre] connect socket created:"
                              "~n      ~p", [CSock]),
                         CSock;
                     {error, eaddrnotavail = Reason} ->
                         ?SKIPT(connect_failed_str(Reason))
                 end,
    ?P("[pre] create accept socket"),
    {ok, Server} = gen_tcp:accept(L),
    ?P("[pre] close listen socket"),
    ok = gen_tcp:close(L),
    {Client, Server, craasao_mk_payload(), "Whatever"}.

craasao_populate(Client, Server, CPayload, SPayload)
  when is_port(Client) andalso is_port(Server) ->
    ?P("[populate,inet] client send data when"
       "~n      Client: ~p"
       "~n      Server: ~p", [Client, Server]),
    ok      = gen_tcp:send(Client, CPayload),
    ?P("[populate,inet] verify client socket queue size"),
    {OS, _} = os:type(),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ?P("[populate,inet] server send data"),
    ok = gen_tcp:send(Server, SPayload),
    ?P("[populate,inet] sleep some"),
    ok = ct:sleep(20),
    {ok, undefined};
craasao_populate(Client, Server, CPayload, SPayload) ->
    ?P("[populate,socket] entry when"
       "~n      Client: ~p"
       "~n      Server: ~p", [Client, Server]),
    Sender = spawn_link(fun() ->
                                craasao_populate_sender(Client, CPayload)
                        end),
    receive
        {'EXIT', Sender, Reason} ->
            {error, {unexpected_exit, Sender, Reason}}
    after 2000 ->
            ?P("[populate,socket] send something"),
            ok = gen_tcp:send(Server, SPayload),
            {ok, Sender}
    end.

craasao_populate_sender(Client, Payload) ->
    ?P("[populate,sender] send payload (expect failure)"), 
    {error, econnreset} = gen_tcp:send(Client, Payload),
    ?P("[populate,sender] payload send failure (as expected)"), 
    exit(normal).

craasao_mk_payload() ->
    list_to_binary(lists:duplicate(1024 * 1024, $.)).

craasao_verify(Client, _Sender, _Payload) when is_port(Client) ->
    ?P("[verify] ensure no 'unexpected messages' received"),
    ok = receive Msg -> {unexpected_msg, Msg} after 0 -> ok end,
    ?P("[verify] set client socket option active: once"),
    ok = inet:setopts(Client, [{active, once}]),
    ?P("[verify] expect client econnreset"),
    receive
	{tcp_error, Client, econnreset} ->
            ?P("[verify] received client econnreset -> "
               "expect client closed message"),
	    receive
		{tcp_closed, Client} ->
                    ?P("[verify] "
                       "received expected client closed message - done"),
		    ok;
		Other1 ->
                    ?P("[verify] client received unexpected message "
                         "(expected closed): "
                       "~n      ~p", [Other1]),
		    ct:fail({unexpected, tcp_closed, Other1})
	    end;
	Other2 ->
            ?P("[verify] client received unexpected message (expected error): "
               "~n      Unexpected: ~p"
               "~n      Flushed:    ~p", [Other2, craasao_flush()]),
	    ct:fail({unexpected, tcp_error, Other2})
    end;
craasao_verify(Client, Sender, Payload) when is_pid(Sender) ->
    ?P("[verify] begin with"
       "~n      Client: ~p"
       "~n      Sender: ~p", [Client, Sender]),
    craasao_verify_sender(Sender),
    ?P("[verify] ensure no 'unexpected messages' received"),
    ok = receive Msg -> {unexpected_msg, Msg} after 0 -> ok end,
    ?P("[verify] set client socket option active once (1)"),
    ok = inet:setopts(Client, [{active, once}]),
    ?P("[verify] client expect (server) data"),
    receive
        {tcp, Client, Payload} ->
            ?P("[verify] client received expected data"),
            ok
    end,
    ?P("[verify] set client socket option active once (2)"),
    ok = inet:setopts(Client, [{active, once}]),
    ?P("[verify] await client econnreset"),
    receive
	{tcp_error, Client, econnreset} ->
            ?P("[verify] client received expected econnreset -> "
               "expect socket close message"),
	    receive
		{tcp_closed, Client} ->
		    ?P("[verify] client received expected closed message"),
                    ok;
		Other1 ->
		    ?P("[verify] client received unexpected message "
                         "(expected closed): "
		       "~n      Unexpected: ~p"
		       "~n      Flushed:    ~p", [Other1, craasao_flush()]),
		    ct:fail({unexpected, tcp_closed, Other1})
	    end;
	Other2 ->
            ?P("[verify] client received unexpected message (expected error): "
               "~n      Unexpected: ~p"
               "~n      Flushed:    ~p", [Other2, craasao_flush()]),
	    ct:fail({unexpected, tcp_error, Other2})
    after 10000 ->
            ?P("[verify] client received unexpected timeout (expected error): "
               "~n      Flushed: ~p", [craasao_flush()]),
	    ct:fail({unexpected_timeout, tcp_error})            
    end.

craasao_flush() ->
    craasao_flush([]).

craasao_flush(Acc) ->
    erlang:yield(),
    receive Msg    -> craasao_flush([Msg|Acc])
    after     1000 -> lists:reverse(Acc)
    end.

craasao_verify_sender(Sender) when is_pid(Sender) ->
    ?P("await sender termination"),
    receive
        {'EXIT', Sender, normal} ->
	    ?P("expected normal sender termination received"),
            ok;
        {'EXIT', Sender, Reason} ->
            ?P("unexpected sender termination: "
               "~n      ~p", [Reason]),
	    ct:fail({unexpected_sender_termination, Sender, Reason})
    after infinity ->
            ok
    end;
craasao_verify_sender(_) ->
    ok.

craasao_cleanup(Client) ->
    (catch gen_tcp:close(Client)).


%% --------------------------------------------------------------------------

econnreset_after_async_send_passive(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_async_send_passive,
            fun() -> do_econnreset_after_async_send_passive(Config) end).

do_econnreset_after_async_send_passive(Config) ->
    CPayload = craasp_mk_payload(),
    SPayload = "Whatever",

    %% First confirm everything works with option turned off.
    ?P("pre 1 (default)"),
    {Client1, Server1} = craasp_pre(Config, default),

    ?P("populate 1 (default)"),
    {ok, Sender1} = craasp_populate(default,
                                    Client1, Server1, CPayload, SPayload),

    ?P("close server socket (1)"),
    ok = gen_tcp:close(Server1),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("verify 1 (default)"),
    ?line ok = craasp_verify(Client1, default, SPayload),

    ?P("cleanup 1 (default)"),
    craasp_cleanup(Client1, Sender1),

    %% Now test with option switched on.
    ?P("pre 2 (true)"),
    {Client2, Server2} = craasp_pre(Config, true),

    ?P("populate 2 (default)"),
    {ok, Sender2} = craasp_populate(true,
                                    Client2, Server2, CPayload, SPayload),


    ?P("close server socket 2"),
    ok = gen_tcp:close(Server2),
    ?P("sleep some"),
    ok = ct:sleep(20),

    ?P("verify 2 (default)"),
    ?line ok = craasp_verify(Client2, true, SPayload),

    ?P("cleanup 2 (default)"),
    craasp_cleanup(Client2, Sender2),

    ?P("done"),
    ok.


craasp_pre(Config, EConnReset)
  when is_boolean(EConnReset) orelse (EConnReset =:= default) ->
    ?P("create listen socket *** with option switched off (default)"),
    {ok, L}    = ?LISTEN(Config, 0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    ?P("[client] create connect socket with show_econnreset: ~p", [EConnReset]),
    COpts = [{active, false}, {sndbuf, 4096}] ++
        if (EConnReset =:= default) -> [];
           is_boolean(EConnReset)   -> [{show_econnreset, EConnReset}]
        end,
    Client = case ?CONNECT(Config, localhost, Port, COpts) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    ?P("accept connection"),
    {ok, Server} = gen_tcp:accept(L),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    {Client, Server}.

craasp_mk_payload() ->
    list_to_binary(lists:duplicate(1024 * 1024, $.)).

craasp_populate(_EConnReset, Client, Server, CPayload, SPayload)
  when is_port(Client) andalso is_port(Server) ->
    ?P("[server] set linger: true:0"),
    ok = inet:setopts(Server, [{linger, {true, 0}}]),
    ?P("[server] send some data to client"),
    ok = gen_tcp:send(Server, SPayload),
    ?P("[client] send some data to server"),
    ok = gen_tcp:send(Client, CPayload),
    {OS, _} = os:type(),
    ?P("[client] verify (port) queue-size"),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    {ok, undefined};
craasp_populate(EConnReset, Client, Server, CPayload, SPayload) ->
    ExpectedSendRes = if (EConnReset =:= default) -> closed;
                         (EConnReset =:= true)    -> econnreset
                      end,
    Sender = spawn_link(fun() ->
                                craasp_populate_sender(Client,
                                                       ExpectedSendRes,
                                                       CPayload)
                        end),
    receive
        {'EXIT', Sender, Reason} ->
            {error, {unexpected_exit, Sender, Reason}}
    after 2000 ->
            ?P("[server] send something"),
            ok = gen_tcp:send(Server, SPayload),
            {ok, Sender}
    end.
    
craasp_populate_sender(Client, ExpectedSendRes, Payload) ->
    ?P("[socket] send payload (expect failure)"), 
    {error, ExpectedSendRes} = gen_tcp:send(Client, Payload),
    ?P("[socket] payload send failure (as expected)"), 
    exit(normal).

craasp_verify(Client, EConnReset, _Payload)
  when is_port(Client) andalso
       ((EConnReset =:= default) orelse is_boolean(EConnReset)) ->
    ?P("[client,~w] attempt receive and expect error (closed): "
       "~n   Port Info:     ~p"
       "~n   Socket Status: ~s",
       [EConnReset,
        try erlang:port_info(Client)
        catch
            _:_:_ ->
                "-"
        end,
        try prim_inet:getstatus(Client) of
            {ok, CStatus} -> ?F("~p", [CStatus]);
            _             -> "-"
        catch
            _:_:_ ->
                "-"
        end]),
    case gen_tcp:recv(Client, 0) of
        {error, closed}     when (EConnReset =:= default) ->
            ok;
        {error, econnreset} when (EConnReset =:= true)    ->
            ok;
        {error, Reason} ->
            {error, {unexpected_error, Reason}};
        ok ->
            {error, unexpected_success}
    end;
craasp_verify(Client, EConnReset, Payload) 
  when (EConnReset =:= default) orelse is_boolean(EConnReset) ->
    ?P("[client] attempt first recv and expect success"),
    case gen_tcp:recv(Client, 0) of
        {ok, Payload} ->
            ?P("[client] attempt second recv and expect failure (~w)",
               [EConnReset]),
            case gen_tcp:recv(Client, 0) of
		{error, closed}     when (EConnReset =:= default) ->
                    ?P("[client] expected failure (closed)"),
		    ok;
		{error, econnreset} when (EConnReset =:= true)    ->
                    ?P("[client] expected failure (econnreset)"),
		    ok;
                {error, Reason2} ->
                    ?P("[client] unexpected failure:"
                       "~n      EConnReset: ~p"
                       "~n      Reason:     ~p", [EConnReset, Reason2]),
                    {error, {unexpected_error2, EConnReset, Reason2}};
                {ok, _} ->
                    ?P("[client] unexpected success"),
                    {error, unexpected_recv2}
            end;                    
        {ok, _} ->
            ?P("[client] unexpected first recv success (wrong payload)"),
            {error, unexpected_recv1};
        {error, Reason1} ->
            ?P("[client] unexpected first recv failure: "
               "~n      Reason: ~p", [Reason1]),
            {error, {unexpected_error1, Reason1}}
    end.

craasp_cleanup(Client, Sender) ->
    (catch gen_tcp:close(Client)),
    craasp_cleanup(Sender).

craasp_cleanup(Sender) when is_pid(Sender) ->
    exit(Sender, kill),
    receive
        {'EXIT', Sender, _} ->
            ok
    after 0 ->
            ok
    end;
craasp_cleanup(_) ->
    ok.



%%
%% Test {linger {true, 0}} aborts a connection
%%

linger_zero(Config) when is_list(Config) ->
    ?TC_TRY(linger_zero, fun() -> do_linger_zero(Config) end).

do_linger_zero(Config) ->
    %% All the econnreset tests will prove that {linger, {true, 0}} aborts
    %% a connection when the driver queue is empty. We will test here
    %% that it also works when the driver queue is not empty.
    {OS, Client, Server} = lz_pre(Config),

    PayloadSize  = 1024 * 1024,
    {ok, Sender} = lz_populate(Client, OS, PayloadSize),

    ?P("set linger: {true, 0}"),
    ok = inet:setopts(Client, [{linger, {true, 0}}]),
    ?P("close client socket"),
    ok = gen_tcp:close(Client),
    ?P("sleep some"),
    ok = ct:sleep(1),

    lz_verify(Client, Server, PayloadSize),

    ?P("cleanup"), % Just in case
    (catch gen_tcp:close(Server)),
    if is_pid(Sender) -> exit(Sender, kill);
       true           -> ok
    end,

    ?P("done"),
    ok.

lz_pre(Config) ->
    {OS, _} = os:type(),
    ?P("create listen socket"),
    {ok, L} = ?LISTEN(Config, 0, [{active, false},
                                  {recbuf, 4096},
                                  {show_econnreset, true}]),
    ?P("listen socket created"),
    %% inet_backend = inet
    {ok, Port} = inet:port(L),
    ?P("connect (create client socket)"),
    Client = case ?CONNECT(Config, localhost, Port,
                           [{nodelay, true},
                            {active,  false},
                            {sndbuf,  4096}]) of
                 {ok, CSock} ->
                     CSock;
                 {error, eaddrnotavail = Reason} ->
                     ?SKIPT(connect_failed_str(Reason))
             end,
    ?P("accept"),
    {ok, Server} = gen_tcp:accept(L),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    {OS, Client, Server}.

lz_populate(Client, OS, PayloadSize) when is_port(Client) ->
    ?P("[inet] create payload"),
    Payload = lz_make_payload(PayloadSize),
    ?P("[inet] ensure non-empty queue"),
    lz_ensure_non_empty_queue(Client, Payload, OS),
    {ok, undefined};
lz_populate(Client, _OS, PayloadSize) ->
    ?P("[socket] send payload"),
    Sender = spawn_link(fun() -> lz_populate_sender(Client, PayloadSize) end),
    receive
        {'EXIT', Sender, Reason} ->
            {error, {unexpected_exit, Sender, Reason}}
    after 2000 ->
            {ok, Sender}
    end.

lz_populate_sender(Client, PayloadSize) ->
    ?P("[socket] create payload"),
    Payload = lz_make_payload(PayloadSize),
    ?P("[socket] send payload (expect failure)"), 
    {error, closed} = gen_tcp:send(Client, Payload),
    ?P("[socket] payload send failed (as expected)"), 
    exit(normal).

lz_verify(Client, Server, PayloadSize)
  when is_port(Client) andalso is_port(Server) ->
    ?P("[inet] verify client socket (port) not connected"),
    undefined = erlang:port_info(Client, connected),
    ?P("[inet] try (and fail) recv (on accepted socket)"),
    {error, econnreset} = gen_tcp:recv(Server, PayloadSize),
    ok;
lz_verify(_Client, Server, PayloadSize) ->
    ?P("[socket] try (and fail) recv (on accepted socket)"),
    {error, econnreset} = gen_tcp:recv(Server, PayloadSize),
    ok.

lz_make_payload(PayloadSize) ->
    list_to_binary(lists:duplicate(PayloadSize, $.)).

%% THIS DOES NOT WORK FOR 'SOCKET'
lz_ensure_non_empty_queue(Sock, Payload, OS) when is_port(Sock) ->
    lz_ensure_non_empty_queue(Sock, Payload, OS, 1).

-define(LZ_MAX_SENDS, 3).

lz_ensure_non_empty_queue(Sock, _Payload, _OS, N) when (N > ?LZ_MAX_SENDS) ->
    ?P("queue size verification failed - port info: "
       "~n   Socket:      ~p"
       "~n   Socket info: ~p", [Sock, erlang:port_info(Sock)]),
    ct:fail("Queue size verification failed");
lz_ensure_non_empty_queue(Sock, Payload, OS, N) ->
    ?P("try send payload (~w bytes) ~w (on client socket) when port info:"
       "~n   ~p", [byte_size(Payload), N, erlang:port_info(Sock)]),
    ok = gen_tcp:send(Sock, Payload),
    ?P("try verify client socket queue size"),
    case erlang:port_info(Sock, queue_size) of
	{queue_size, QSz} when QSz > 0 ->
            ?P("queue size verification *successfull* (~p) - port info: "
               "~n   ~p", [QSz, erlang:port_info(Sock)]),
            ok;
	{queue_size, 0} when OS =:= win32 ->
            ?P("queue size verification *successfull* - port info: "
               "~n   ~p", [erlang:port_info(Sock)]),
            ok;
	{queue_size, 0} ->
            ?P("queue size verification failed - port info: "
               "~n   ~p", [erlang:port_info(Sock)]),
            lz_ensure_non_empty_queue(Sock, Payload, OS, N+1)
    end.


linger_zero_sndbuf(Config) when is_list(Config) ->
    ?TC_TRY(linger_zero_sndbuf, fun() -> do_linger_zero_sndbuf(Config) end).

do_linger_zero_sndbuf(Config) ->
    %% All the econnreset tests will prove that {linger, {true, 0}} aborts
    %% a connection when the driver queue is empty. We will test here
    %% that it also works when the driver queue is not empty
    %% and the linger zero option is set on the listen socket.
    {OS, Client, Server} = lzs_pre(Config),

    PayloadSize = 1024 * 1024,
    {ok, Sender} = lzs_populate(Client, OS, PayloadSize),

    ?P("verify linger: {true, 0}"),
    {ok, [{linger, {true, 0}}]} = inet:getopts(Server, [linger]),
    ?P("close client socket"),
    ok = gen_tcp:close(Server),
    ok = ct:sleep(1),

    lzs_verify(Client, Server, PayloadSize),

    ?P("cleanup"), % Just in case
    (catch gen_tcp:close(Server)),
    if is_pid(Sender) -> exit(Sender, kill);
       true           -> ok
    end,

    ?P("done"),
    ok.

lzs_pre(Config) ->
    {OS, _} = os:type(),
    ?P("create listen socket"),
    {ok, Listen} = ?LISTEN(Config, 0, [{active,          false},
                                       {recbuf,          4096},
                                       {show_econnreset, true},
                                       {linger,          {true, 0}}]),
    {ok, Port} = inet:port(Listen),
    ?P("connect (create client socket)"),
    Client = case ?CONNECT(Config, localhost, Port,
                           [{nodelay, true},
                            {active,  false},
                            {sndbuf,  4096}]) of
            {ok, CSock} ->
                CSock;
            {error, eaddrnotavail = CReason} ->
                ?SKIPT(connect_failed_str(CReason))
        end,
    ?P("accept"),
    {ok, Server} = gen_tcp:accept(Listen),
    %% On *some* platforms, the linger is inherited,
    %% but not all so do not assume...
    ?P("check if linger option was inherited"),
    case inet:getopts(Server, [linger]) of
	{ok, [{linger, {true, 0}}]} ->
	    ?P("linger option was inherited"),
	    ok;
	{ok, [{linger, {true, TO}}]} ->
	    ?P("linger option was *not* inherited: ~p", [TO]),
	    ok = inet:setopts(Server, [{linger, {true, 0}}]),
	    ok;
	{error, SReason} ->
	    ?P("FAILED reading linger option: ~p", [SReason]),
	    ok = inet:setopts(Server, [{linger, {true, 0}}]),
	    ok
    end,
    ?P("close listen socket"),
    ok = gen_tcp:close(Listen),
    {OS, Client, Server}.

lzs_populate(Client, OS, PayloadSize) when is_port(Client) ->
    ?P("[inet] create payload"),
    Payload = lzs_make_payload(PayloadSize),
    ?P("[inet] ensure non-empty queue"),
    lz_ensure_non_empty_queue(Client, Payload, OS),
    {ok, undefined};
lzs_populate(Client, _OS, PayloadSize) ->
    ?P("[socket] send payload ( = start payload sender)"),
    Sender = spawn(fun() -> lzs_populate_sender(Client, PayloadSize) end),
    receive
        {'EXIT', Sender, Reason} ->
            {error, {unexpected_exit, Sender, Reason}}
    after 2000 ->
            {ok, Sender}
    end.

lzs_populate_sender(Client, PayloadSize) ->
    ?P("[socket] create payload"),
    Payload = lzs_make_payload(PayloadSize),
    ?P("[socket] send payload (expect failure)"), 
    {error, closed} = gen_tcp:send(Client, Payload),
    ?P("[socket] payload send failed (as expected)"), 
    exit(normal).
    
lzs_make_payload(PayloadSize) ->
    Payload = binary:copy(<<"0123456789ABCDEF">>, 64 * 1024), % 1 MB
    if (PayloadSize =:= byte_size(Payload)) ->
            Payload;
       true ->
            exit({payload_size, PayloadSize, byte_size(Payload)})
    end.

lzs_verify(Client, Server, PayloadSize)
  when is_port(Client) andalso is_port(Server) ->
    ?P("[inet] verify client socket (port) not connected"),
    undefined = erlang:port_info(Server, connected),
    ?P("[inet] try (and fail) recv (on accepted socket)"),
    {error, closed} = gen_tcp:recv(Client, PayloadSize),
    ok;
lzs_verify(Client, _Server, PayloadSize) ->
    ?P("[socket] try (and fail) recv (on accepted socket)"),
    {error, closed} = gen_tcp:recv(Client, PayloadSize),
    ok.
    
%% Thanks to Luke Gorrie. Tests for a very specific problem with 
%% corrupt data. The testcase will be killed by the timetrap timeout
%% if the bug is present.
http_bad_packet(Config) when is_list(Config) ->
    {ok,L} = ?LISTEN(Config, 0, [{active, false},
                                binary,
                                {reuseaddr, true},
                                {packet, http}]),
    {ok,Port} = inet:port(L),
    spawn_link(fun() -> erlang:yield(), http_bad_client(Config, Port) end),
    case gen_tcp:accept(L) of
        {ok,S} ->
            http_worker(S);
        Err ->
            exit({accept,Err})
    end.

http_worker(S) ->
    case gen_tcp:recv(S, 0, 30000) of
	{ok,{http_error,Error}} ->
	     io:format("Http error: ~s\n", [Error]);
	{ok,Data} ->
	    io:format("Data: ~p\n", [Data]),
	    http_worker(S)
    end.

http_bad_client(Config, Port) ->
    {ok,S} = ?CONNECT(Config, "localhost", Port, [{active,false}, binary]),
    ok = gen_tcp:send(S, "\r\n"),
    ok = gen_tcp:close(S).


%% Fill send queue and then start receiving.
%%
busy_send(Config) when is_list(Config) ->
    ?TC_TRY(busy_send, fun() -> do_busy_send(Config) end).

do_busy_send(Config) ->
    OldFlag = process_flag(trap_exit, true),
    Master  = self(),
    Msg     = <<"the quick brown fox jumps over a lazy dog~n">>,
    ?P("[master] start server"),
    ServerF =
        fun() ->
                ?P("[server] create listen socket"),
                case ?LISTEN(Config, 0, [{active,false},binary,
                                         {reuseaddr,true},
                                         {packet,0},
                                         {recbuf,4096},
                                         {sndbuf,4096}]) of
                    {ok, L} ->
                        ?P("[server] listen socket created"),
                        {ok, Port} = inet:port(L),
                        Master ! {self(), listen_port, Port},
                        ?P("[server] await continue"),
                        receive
                            {Master, continue} ->
                                ?P("[server] received continue"),
                                busy_send_srv(L, Master, Msg)
                        end;
                    {error, Reason} ->
                        ?P("[server] UNEXPECTED: ~w", [Reason]),
                        ?SKIPE(listen_failed_str(Reason))
                end
        end,
    Server = spawn_link(ServerF),
    ?P("[master] server: ~p", [Server]),
    ListenPort =
        receive
            {'EXIT', Server, {skip, _} = SKIP} ->
                ?P("[master] server issued skip: "
                   "~n      ~p", [SKIP]),
                throw(SKIP);

            {'EXIT', Server, Reason} ->
                ?P("[master] server crashed: "
                   "~n      ~p", [Reason]),
                exit({server, Reason});

            {Server, listen_port, LP} ->
                ?P("listen port: ~p", [LP]),
                LP
        end,
    ?P("[master] start client"),
    ClientF = 
      fun () ->
              ?P("[client] await (connect) server port"),
              Port =
                  receive
                      {Master, connect, P} ->
                          P
                  end,
              ?P("[client] connect to ~w", [Port]),
	      case ?CONNECT(Config, "localhost", Port,
                                   [{active,false},
                                    binary,
                                    {packet,0},
                                    {recbuf,4096},
                                    {sndbuf,4096}]) of
                  {ok, Socket} ->
                      Master ! {self(), connected},
                      ?P("[client] connected - await recv"),
                      receive
                          {Master, recv, N} ->
                              ?P("[client] received recv:~w", [N]),
                              busy_send_client_loop(Socket, Master, Msg, N)
                      end;
                  {error, eaddrnotavail = CReason} ->
                      ?P("[client] UNEXPECTED: ~w", [CReason]),
                      ?SKIPE(connect_failed_str(CReason))
              end
      end,
    Client = spawn_link(ClientF),
    ?P("[master] client: ~p", [Client]),
    Server ! {self(), continue},
    Client ! {self(), connect, ListenPort},
    receive
        {Client, connected} ->
            ?P("[master] client connected"),
            ok
    end,
    busy_send_loop(Server, Client, 0),
    process_flag(trap_exit, OldFlag),
    ?P("[master] done"),
    ok.

busy_send_loop(Server, Client, N) ->
    %% Master
    %%
    receive
        {Server, send} ->
            busy_send_loop(Server, Client, N+1)

    after 2000 ->
            ?P("[master] send timeout: server send queue full (N = ~w+1)", [N]),
            %% Send queue full, sender blocked 
            %% -> stop sender and release client
            Server ! {self(), close},
            Client ! {self(), recv, N+1},
            ?P("[master] await server 'send'..."),
            receive
                {Server, send} ->
                    busy_send_2(Server, Client, N+1)
            after 10000 ->
                    %% If this happens, see busy_send_srv
                    ?P("[master] UNEXPECTED: server send timeout"),
                    ct:fail({timeout,{server,not_send,flush([])}})
            end
    end.

busy_send_2(Server, Client, _N) ->
    %% Master
    %%
    ?P("[master] await (server) closed"),
    receive
        {Server, [closed]} ->
            ?P("[master] received expected (server) closed - "
               "await client closed"),
            receive
                {Client, [0,{error,closed}]} ->
                    ?P("[master] received expected (client) closed"),
                    ok
            end
    after 10000 ->
            ?P("[master] UNEXPECTED: server closed timeout"),
            ct:fail({timeout, {server, not_closed, flush([])}})
    end.

busy_send_srv(L, Master, Msg) ->
    %% Server
    %% Sometimes this accept does not return, do not really know why
    %% but it causes the timeout error in busy_send_loop to be
    %% triggered. Only happens on OS X Leopard?!?
    ?P("[server] try accept"),
    case gen_tcp:accept(L) of
        {ok, Socket} ->
            ?P("[server] accepted"),
            busy_send_srv_loop(Socket, Master, Msg);
        {error, Reason} ->
            ?P("UNEXPECTED: server accept failure: ~p", [Reason]),
            ?SKIPE(accept_failed_str(Reason))
    end.

busy_send_srv_loop(Socket, Master, Msg) ->
    %% Server
    %%
    receive
	{Master, close} ->
            ?P("[server] received close"),
	    ok = gen_tcp:close(Socket),
	    Master ! {self(),flush([closed])}
    after 0 ->
	    ok = gen_tcp:send(Socket, Msg),
	    Master ! {self(), send},
	    busy_send_srv_loop(Socket, Master, Msg)
    end.

busy_send_client_loop(Socket, Master, Msg, N) ->
    %% Client
    %%
    Size = byte_size(Msg),
    case gen_tcp:recv(Socket, Size) of
	{ok, Msg} ->
	    busy_send_client_loop(Socket, Master, Msg, N-1);
	Other ->
            ?P("[client] recv response: "
               "~n      ~p", [Other]),
	    Master ! {self(), flush([Other,N])}
    end.

%%%
%%% Send to a socket whose other end does not read until the port gets busy.
%%% Then close the other end. The writer should get an {error,closed} error.
%%% (Passive mode.)
%%%

busy_disconnect_passive(Config) when is_list(Config) ->
    ?TC_TRY(busy_disconnect_passive,
            fun() -> do_busy_disconnect_passive(Config) end).

do_busy_disconnect_passive(Config) ->
    ?P("[passive] begin"),
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_passive2(Config, MuchoData, N) || N <- lists:seq(1, 10)],
    ?P("[passive] done"),
    ok.

do_busy_disconnect_passive2(Config, MuchoData, N) ->
    ?P("[passive,~w] *** prepare server *** ", [N]),
    {_Server, S} = busy_disconnect_prepare_server(Config, [{active,false}]),
    ?P("[passive,~w] server prepared - start sending", [N]),
    {_OSFam, OSName} = os:type(),
    busy_disconnect_passive_send(S, MuchoData, OSName).

busy_disconnect_passive_send(S, Data, OS) ->
    case gen_tcp:send(S, Data) of
	ok ->
            busy_disconnect_passive_send(S, Data, OS);
	{error, closed} ->
            ok;
	{error, eprototype = Reason} when (OS =:= darwin) ->
	    ?P("send failed with ~w", [Reason]),
            ok
    end.

%%%
%%% Send to a socket whose other end does not read until the port gets busy.
%%% Then close the other end. The writer should get an {error,closed} error and
%%% a {tcp_closed,Socket} message. (Active mode.)
%%%
busy_disconnect_active(Config) when is_list(Config) ->
    ?TC_TRY(busy_disconnect_active,
            fun() -> do_busy_disconnect_active(Config) end).

do_busy_disconnect_active(Config) ->
    ?P("[active] begin"),
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_active2(Config, MuchoData, N) || N <- lists:seq(1, 10)],
    ?P("[active] done"),
    ok.

do_busy_disconnect_active2(Config, MuchoData, N) ->
    ?P("[active,~w] *** prepare server *** ", [N]),
    {Server, S} = busy_disconnect_prepare_server(Config, [{active,true}]),
    ?P("[active,~w] server prepared - start sending", [N]),
    busy_disconnect_active_send(Server, S, MuchoData).

busy_disconnect_active_send(Server, S, Data) ->
    {_OSFam, OSName} = os:type(),
    busy_disconnect_active_send(Server, S, Data, 1, OSName).

busy_disconnect_active_send(Server, S, Data, Iter, OS) ->
    case gen_tcp:send(S, Data) of
	ok ->
            busy_disconnect_active_send(Server, S, Data, Iter+1, OS);
	{error, closed} ->
            ?P("[active-sender,~w] send failed with closed - await tcp-closed",
	       [Iter]),
            busy_disconnect_active_send_await_closed(Server, S);

	{error, eprototype = Reason} when (OS =:= darwin) ->
            ?P("[active-sender,~w] send failed with ~w - await tcp-closed",
	       [Iter, Reason]),
            busy_disconnect_active_send_await_closed(Server, S);

        {error, einval = Reason} ->
            ?P("[active-sender,~w] UNEXPECTED send failure:"
               "~n   ~p", [Iter, Reason]),
            ?SKIPT(send_failed_str(Reason));

        {error, Reason} ->
            ?P("[active-sender,~w] UNEXPECTED send failure:"
               "~n   ~p", [Iter, Reason]),
            ct:fail({unexpected_send_result, Reason, Iter})
    end.

busy_disconnect_active_send_await_closed(Server, S) ->
    busy_disconnect_active_send_await_closed(Server, S, false, false).
busy_disconnect_active_send_await_closed(Server, S, Closed, Stopped) ->
    receive
        {tcp_closed, S} when (Stopped =:= true) ->
            ?P("[active-sender] received tcp-closed - done"),
            ok;

        {tcp_closed, S} ->
            ?P("[active-sender] received tcp-closed"),
            busy_disconnect_active_send_await_closed(Server, S, true, Stopped);

        {'EXIT', Server, normal} when (Closed =:= true) ->
            ?P("[active-sender] received server (normal) exit - done"),
            ok;

        {'EXIT', Server, normal} ->
            ?P("[active-sender] received server (normal) exit"),
            busy_disconnect_active_send_await_closed(Server, S, Closed, true);

        Other ->
            ?P("[active-sender] received UNEXPECTED message:"
               "~n   Expected tcp-close of ~p"
               "~n   Server:               ~p"
               "~n   Unexpected message:   ~p", [S, Server, Other]),
            ct:fail({unexpected, Other, S, flush([])})
    end.
    

busy_disconnect_prepare_server(Config, ConnectOpts) ->
    Sender = self(),
    ?P("[prep-server] create server"),
    Server = spawn_link(fun() -> busy_disconnect_server(Config, Sender) end),
    ?P("[prep-server] await port (from server)"),
    receive {port, Server, Port} -> ok end,
    ?P("[prep-server] connect to ~w", [Port]),
    case ?CONNECT(Config, localhost, Port, ConnectOpts) of
        {ok, S} ->
            ?P("[prep-server] connected - order server start sending"),
            Server ! {Sender, sending},
            ?P("[prep-server] done"),
            {Server, S};
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end.

busy_disconnect_server(Config, Sender) ->
    ?P("[server] create listen socket"),
    {ok, L} = ?LISTEN(Config, 0,
                             [{active,false},
                              binary,
                              {reuseaddr,true},
                              {packet,0}]),
    ?P("[server] created - get port number"),
    {ok,Port} = inet:port(L),
    ?P("[server] send port ~w (to sender)", [Port]),
    Sender ! {port,self(),Port},
    ?P("[server] try accept"),
    {ok,S} = gen_tcp:accept(L),
    ?P("[server] connection accepted"),
    receive
	{Sender, sending} ->
            ?P("[server] received sending (from sender)"),
	    busy_disconnect_server_wait_for_busy(Sender, S)
    end.

%% Close the socket as soon as the Sender process can't send because of
%% a busy port.
busy_disconnect_server_wait_for_busy(Sender, S) ->
    case process_info(Sender, status) of
	{status, waiting = Status} ->
	    %% We KNOW that the sender will be in state 'waiting' only
	    %% if the port has become busy. (Fallback solution if the
	    %% implementation changes: Watch Sender's reduction count;
	    %% when it stops changing, wait 2 seconds and then close.)
	    ?P("[server] sender status ~p => close socket", [Status]),
	    gen_tcp:close(S);
	{status, Status} ->
	    ?P("[server] sender status ~p", [Status]),
	    timer:sleep(100),
	    busy_disconnect_server_wait_for_busy(Sender, S);
	Other ->
	    ?P("[server] sender status ~p", [Other]),
	    timer:sleep(100),
	    busy_disconnect_server_wait_for_busy(Sender, S)
    end.


%%%
%%% Fill send queue
%%%
fill_sendq(Config) when is_list(Config) ->
    ?TC_TRY(fill_sendq, fun() -> do_fill_sendq(Config) end).

do_fill_sendq(Config) ->
    OldFlag = process_flag(trap_exit, true),
    Master  = self(),
    ServerF =
        fun () ->
                ?P("[server] try listen"),
                case ?LISTEN(Config, 0, [{active,false},binary,
                                        {reuseaddr,true},{packet,0}]) of
                    {ok, L} ->
                        ?P("[server] try port"),
                        case inet:port(L) of
                            {ok, Port} ->
                                Master ! {self(), listen_port, Port},
                                fill_sendq_srv(L, Master);
                            {error, PReason} ->
                                ?SKIPE(port_failed_str(PReason))
                        end;
                    {error, LReason} ->
                        ?SKIPE(listen_failed_str(LReason))
                end
        end,
    Server  = spawn_link(ServerF),
    ?P("[master] server: ~p", [Server]),
    ClientF =
        fun () ->
                ?P("[client] await server port"),
                ServerPort =
                    receive
                        {Master, server_port, SP} ->
                            ?P("[client] server port: ~w", [SP]),
                            SP
                    end,
                %% Just close on order
                ?P("[client] try connect"),
                case ?CONNECT(Config, 
                       "localhost", ServerPort,
                       [{active,false},binary,{packet,0}]) of
                    {ok, S} ->
                        ?P("[client] connected"),
                        Master ! {self(), connected},
                        receive
                            {Master, close} ->
                                ?P("[client] received close"),
                                ok = gen_tcp:close(S)
                        end;
                    {error, eaddrnotavail = Reason} ->
                        ?SKIPE(connect_failed_str(Reason))
                end
        end,
    Client = spawn_link(ClientF),
    ?P("[master] client: ~p", [client]),
    ListenPort =
        receive
            {Server, listen_port, LP} ->
                ?P("[master] listen port: ~w", [LP]),
                LP
        end,
    Client ! {self(), server_port, ListenPort},
    ?P("[master] await client connected"),
    receive {Client, connected} -> ok end,
    ?P("[master] await reader"),
    Res = receive
              {Server, reader, Reader} ->
                  ?P("[master] reader: ~p", [Reader]),
                  fill_sendq_loop(Server, Client, Reader)
          end,
    process_flag(trap_exit, OldFlag),    
    ?P("[master] done"),
    Res.

fill_sendq_loop(Server, Client, Reader) ->
    %% Master
    %%
    receive
        {Server, send} ->
	    fill_sendq_loop(Server, Client, Reader)
    after 2000 ->
	    %% Send queue full, sender blocked -> close client.
	    ?P("[master] send timeout, closing client"),
	    Client ! {self(), close},
	    receive
                {Server, [{error,closed}] = SErrors} ->
                    ?P("[master] got expected server closed"),
                    receive
                        {Reader, [{error,closed}]} ->
                            ?P("[master] got expected reader closed"),
                            ok;
                        {Reader, RErrors} when is_list(RErrors) ->
                            ct:fail([{server, SErrors}, {reader, RErrors}])
                    after 3000 ->
                            ct:fail({timeout,{closed,reader}})
                    end;

                {Server, SErrors} when is_list(SErrors) ->
                    ?P("UNEXPECTED SERVER ERROR(S): "
                       "~n   ~p"
                       "~n   ~p", [SErrors, flush([])]),
                    ct:fail([{server, SErrors}, {reader, []}]);

                {Reader, [{error,closed}] = RErrors} ->
                    ?P("[master] got expected reader closed"),
                    receive
                        {Server, [{error,closed}]} ->
                            ?P("[master] got expected server closed"),
                            ok;
                        {Server, SErrors} when is_list(SErrors) ->
                            ct:fail([{server, SErrors}, {reader, RErrors}])
                    after 3000 ->
                            ct:fail({timeout,{closed,server}})
                    end;

                {Reader, RErrors} when is_list(RErrors) ->
                    ?P("UNEXPECTED READER ERROR(S): "
                       "~n   ~p"
                       "~n   ~p", [RErrors, flush([])]),
                    ct:fail([{server, []}, {reader, RErrors}])

            after 3000 ->
                    Msgs = flush([]),
                    ct:fail({timeout,{closed,[server,reader]}, Msgs})
            end
    end.

fill_sendq_srv(L, Master) ->
    %% Server
    %%
    ?P("[server] await accept"),
    case gen_tcp:accept(L) of
	{ok, S} ->
            ?P("[server] accepted ~p", [S]),
	    Master ! {self(), reader,
		      spawn_link(fun () -> fill_sendq_read(S, Master) end)},
	    Msg = "the quick brown fox jumps over a lazy dog~n",
	    fill_sendq_write(S, Master, [Msg,Msg,Msg,Msg,Msg,Msg,Msg,Msg]);
	E ->
            Error = flush([E]),
	    ?P("[server] accept error: ~p", [E]),
	    Master ! {self(), Error}
    end.

fill_sendq_write(S, Master, Msg) ->
    %% Server
    %%
    %% ?P("[server] sending..."),
    case gen_tcp:send(S, Msg) of
	ok ->
            Master ! {self(), send},
	    %% ?P("[server] ok."),
	    fill_sendq_write(S, Master, Msg);
	{error, _} = E ->
	    Error = flush([E]),
	    ?P("[server] send error: ~p", [Error]),
	    Master ! {self(), Error}
    end.

fill_sendq_read(S, Master) ->
    %% Reader
    %%
    ?P("[reader] read infinity..."),
    case gen_tcp:recv(S, 0, infinity) of
	{ok, Data} ->
	    ?P("[reader] recv: ~p", [Data]),
	    fill_sendq_read(S, Master);
	E ->
	    Error = flush([E]),
	    ?P("[reader] recv error: ~p", [Error]),
	    Master ! {self(), Error}
    end.


%%% Try to receive more than available number of bytes from 
%%% a closed socket.
%%%
partial_recv_and_close(Config) when is_list(Config) ->
    try do_partial_recv_and_close(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_partial_recv_and_close(Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = ?LISTEN(Config, 0, [{active,false}]),
    {ok,P} = inet:port(L),
    Sock =
        case ?CONNECT(Config, "localhost", P, [{active,false}]) of
            {ok, S} ->
                S;
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end,
    {ok, A} = gen_tcp:accept(L),
    ok = gen_tcp:send(Sock, Msg),
    ok = gen_tcp:close(Sock),
    {error, closed} = gen_tcp:recv(A, Len+1),
    ok.

%%% Try to receive more than available number of bytes from 
%%% a closed socket, this time waiting in the recv before closing.
%%%
partial_recv_and_close_2(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    Res = try do_partial_recv_and_close_2(Config)
          catch
              exit:{skip, _} = SKIP ->
                  SKIP
          end,
    process_flag(trap_exit, OldFlag),
    Res.

do_partial_recv_and_close_2(Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = ?LISTEN(Config, 0, [{active,false}]),
    {ok,P} = inet:port(L),
    Server = self(),
    Client =
	spawn_link(
	  fun () ->
		  receive after 2000 -> ok end,
		  case ?CONNECT(Config, "localhost", P, [{active,false}]) of
                      {ok, S} ->
                          ok = gen_tcp:send(S, Msg),
                          receive {Server,close} -> ok end,
                          receive after 2000 -> ok end,
                          ok = gen_tcp:close(S);
                      {error, eaddrnotavail = Reason} ->
                          ?SKIPE(connect_failed_str(Reason))
                  end
	  end),
    {ok, A} = gen_tcp:accept(L),
    Client ! {Server, close},
    {error, closed} = gen_tcp:recv(A, Len+1),
    ok.

%%% Here we tests that gen_tcp:recv/2 will return {error,closed} following
%%% a send operation of a huge amount data when the other end closed the socket.
%%%
partial_recv_and_close_3(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    Res = try do_partial_recv_and_close_3(Config)
          catch
              throw:{skip, _} = SKIP ->
                  SKIP
          end,
    process_flag(trap_exit, OldFlag),
    Res.

do_partial_recv_and_close_3(Config) ->
    [do_partial_recv_and_close_4(Config) || _ <- lists:seq(0, 20)],
    ok.

do_partial_recv_and_close_4(Config) ->
    Parent = self(),
    spawn_link(fun() ->
		       {ok,L} = ?LISTEN(Config, 0, [{active,false}]),
		       {ok,{_,Port}} = inet:sockname(L),
		       Parent ! {port,Port},
		       {ok,S} = gen_tcp:accept(L),
		       gen_tcp:recv(S, 1),
		       gen_tcp:close(S)
	       end),
    receive
	{port,Port} -> ok
    end,
    Much = ones(8*64*1024),
    S = case ?CONNECT(Config, localhost, Port, [{active, false}]) of
            {ok, Sock} ->
                Sock;
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end,

    %% Send a lot of data (most of it will be queued).
    %% The receiver will read one byte and close the connection.
    %% The write operation will fail.
    gen_tcp:send(S, Much),

    %% We should always get {error,closed} here.
    {error, closed} = gen_tcp:recv(S, 0).
    

test_prio_put_get(Config) ->
    Tos = 3 bsl 5,
    ?P("test_prio_put_get -> create listen socket"),
    {ok,L1} = ?LISTEN(Config, 0, [{active,false}]),
    ?P("test_prio_put_get -> set opts prio (= 3)"),
    ok = inet:setopts(L1,[{priority,3}]),
    ?P("test_prio_put_get -> set opts tos (= ~p)", [Tos]),
    ok = inet:setopts(L1,[{tos,Tos}]),
    ?P("test_prio_put_get -> verify opts prio and tos"),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    ?P("test_prio_put_get -> set opts prio (= 3)"),
    ok = inet:setopts(L1,[{priority,3}]), % Dont destroy each other
    ?P("test_prio_put_get -> verify opts prio and tos"),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    ?P("test_prio_put_get -> set opts reuseaddr (= true)"),
    ok = inet:setopts(L1,[{reuseaddr,true}]), % Dont let others destroy
    ?P("test_prio_put_get -> verify opts prio and tos"),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    ?P("test_prio_put_get -> close listen socket"),
    gen_tcp:close(L1),
    ?P("test_prio_put_get -> done"),
    ok.

test_prio_accept(Config) ->
    ?P("test_prio_accept -> create listen socket"),
    {ok, Sock} = ?LISTEN(Config, 0, [binary,{packet,0},{active,false},
                                     {reuseaddr,true},{priority,4}]),
    ?P("test_prio_accept -> get port number of listen socket"),
    {ok, Port} = inet:port(Sock),
    ?P("test_prio_accept -> connect to port ~p", [Port]),
    Sock2 = case ?CONNECT(Config, "localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4}]) of
                {ok, S2} -> 
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    ?P("test_prio_accept -> connected => accept connection"),
    {ok, Sock3} = gen_tcp:accept(Sock),
    ?P("test_prio_accept -> accepted => getopts prio for listen socket"),
    {ok, [{priority,4}]} = inet:getopts(Sock,  [priority]),
    ?P("test_prio_accept -> getopts prio for connected socket"),
    {ok, [{priority,4}]} = inet:getopts(Sock2, [priority]),
    ?P("test_prio_accept -> getopts prio for accepted socket"),
    {ok, [{priority,4}]} = inet:getopts(Sock3, [priority]),
    ?P("test_prio_accept -> close listen socket"),
    gen_tcp:close(Sock),
    ?P("test_prio_accept -> close connected socket"),
    gen_tcp:close(Sock2),
    ?P("test_prio_accept -> close accepted socket"),
    gen_tcp:close(Sock3),
    ?P("test_prio_accept -> done"),
    ok.

test_prio_accept2(Config) ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    ?P("test_prio_accept2 -> create listen socket"),
    {ok, Sock} = ?LISTEN(Config, 0,[binary,{packet,0},{active,false},
                                    {reuseaddr,true},{priority,4},
                                    {tos,Tos1}]),
    ?P("test_prio_accept2 -> get port number of listen socket"),
    {ok, Port} = inet:port(Sock),
    ?P("test_prio_accept2 -> connect to port ~p", [Port]),
    Sock2 = case ?CONNECT(Config, "localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    ?P("test_prio_accept2 -> connected => accept connection"),
    {ok, Sock3} = gen_tcp:accept(Sock),
    ?P("test_prio_accept2 -> accepted => getopts prio and tos for listen socket"),
    {ok,[{priority,4},{tos,Tos1}]} = inet:getopts(Sock,[priority,tos]),
    ?P("test_prio_accept2 -> getopts prio and tos for connected socket"),
    {ok,[{priority,4},{tos,Tos2}]} = inet:getopts(Sock2,[priority,tos]),
    ?P("test_prio_accept2 -> getopts prio and tos for accepted socket"),
    {ok,[{priority,4},{tos,Tos1}]} = inet:getopts(Sock3,[priority,tos]),
    ?P("test_prio_accept2 -> close listen socket"),
    gen_tcp:close(Sock),
    ?P("test_prio_accept2 -> close connected socket"),
    gen_tcp:close(Sock2),
    ?P("test_prio_accept2 -> close accepted socket"),
    gen_tcp:close(Sock3),
    ?P("test_prio_accept2 -> done"),
    ok.

test_prio_accept3(Config) ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    ?P("test_prio_accept3 -> create listen socket"),
    {ok, Sock} = ?LISTEN(Config, 0,[binary,{packet,0},{active,false},
                                    {reuseaddr,true},
                                    {tos,Tos1}]),
    ?P("test_prio_accept3 -> get port number of listen socket"),
    {ok,Port} = inet:port(Sock),
    ?P("test_prio_accept3 -> connect to port ~p", [Port]),
    Sock2 = case ?CONNECT(Config, "localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end,
    ?P("test_prio_accept3 -> connected => accept connection"),
    {ok, Sock3} = gen_tcp:accept(Sock),
    ?P("test_prio_accept3 -> "
       "accepted => getopts prio and tos for listen socket"),
    {ok, [{priority,0},{tos,Tos1}]} = inet:getopts(Sock,  [priority,tos]),
    ?P("test_prio_accept3 -> getopts prio and tos for connected socket"),
    {ok, [{priority,0},{tos,Tos2}]} = inet:getopts(Sock2, [priority,tos]),
    ?P("test_prio_accept3 -> getopts prio and tos for accepted socket"),
    {ok, [{priority,0},{tos,Tos1}]} = inet:getopts(Sock3, [priority,tos]),
    ?P("test_prio_accept3 -> close listen socket"),
    gen_tcp:close(Sock),
    ?P("test_prio_accept3 -> close connected socket"),
    gen_tcp:close(Sock2),
    ?P("test_prio_accept3 -> close accepted socket"),
    gen_tcp:close(Sock3),
    ?P("test_prio_accept3 -> done"),
    ok.
    
test_prio_accept_async(Config) ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    Ref = make_ref(),
    ?P("test_prio_accept_async -> create prio server"),
    spawn(?MODULE, priority_server, [Config, {self(),Ref}]),
    Port = receive
               {Ref,P} -> P
           after 5000 -> ct:fail({error,"helper process timeout"})
           end,
    receive
    after 3000 -> ok
    end,
    ?P("test_prio_accept_async -> connect to port ~p", [Port]),
    Sock2 = case ?CONNECT(Config, "localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    ?P("test_prio_accept_async -> "
       "connected => await prio and tos for listen socket"),
    receive
        {Ref,{ok,[{priority,4},{tos,Tos1}]}} ->
            ok;
        {Ref,Error} ->
            ct:fail({missmatch,Error})
    after 5000 -> ct:fail({error,"helper process timeout"})
    end,
    ?P("test_prio_accept_async -> await prio and tos for accepted socket"),
    receive
        {Ref,{ok,[{priority,4},{tos,Tos1}]}} ->
            ok;
        {Ref,Error2} ->
            ct:fail({missmatch,Error2})
    after 5000 -> ct:fail({error,"helper process timeout"})
    end,

    ?P("test_prio_accept_async -> getopts prio and tos for connected socket"),
    {ok,[{priority,4},{tos,Tos2}]} = inet:getopts(Sock2, [priority,tos]),
    ?P("test_prio_accept_async -> close connected socket"),
    catch gen_tcp:close(Sock2),
    ?P("test_prio_accept_async -> done"),
    ok.

priority_server(Config, {Parent,Ref}) ->
    Tos1 = 4 bsl 5,
    {ok,Sock}=?LISTEN(Config, 0,[binary,{packet,0},{active,false},
                                {reuseaddr,true},{priority,4},
                                {tos,Tos1}]),
    {ok,Port} = inet:port(Sock),
    Parent ! {Ref,Port},
    {ok,Sock3}=gen_tcp:accept(Sock),
    Parent ! {Ref, inet:getopts(Sock,[priority,tos])},
    Parent ! {Ref, inet:getopts(Sock3,[priority,tos])},
    ok.

test_prio_fail(Config) ->
    ?P("test_prio_fail -> create listen socket"),
    {ok,L} = ?LISTEN(Config, 0, [{active,false}]),
    ?P("test_prio_fail -> try set (and fail) opts prio (= 1000)"),
    {error,_} = inet:setopts(L,[{priority,1000}]),
    ?P("test_prio_fail -> close listen socket"),
    gen_tcp:close(L),
    ?P("test_prio_fail -> done"),
    ok.

test_prio_udp() ->
    Tos = 3 bsl 5,
    ?P("test_prio_udp -> create UDP socket (open)"),
    {ok,S} = gen_udp:open(0,[{active,false},binary,{tos, Tos},
                             {priority,3}]),
    ?P("test_prio_udp -> getopts prio and tos"),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(S,[priority,tos]),
    ?P("test_prio_fail -> close socket"),
    gen_udp:close(S),
    ?P("test_prio_fail -> done"),
    ok.

%% Tests the so_priority and ip_tos options on sockets when applicable.
so_priority(Config) when is_list(Config) ->
    ?TC_TRY(so_priority,
	    %% Normally we should have the condition funm here,
	    %% but since we are (currently) not platform independant,
	    %% we can't...check in the test case instead.
	    fun() -> do_so_priority(Config) end).

do_so_priority(Config) ->
    ?P("create listen socket"),
    L = case ?LISTEN(Config, 0, [{active,false}]) of
	    {ok, LSock} when not is_port(LSock) ->
		try socket:is_supported(options, socket, priority) of
		    true ->
			LSock;
		    false ->
			(catch gen_tcp:close(LSock)),
			?SKIPT("Option 'priority' not supported")
                catch
                    _:_:_ ->
			(catch gen_tcp:close(LSock)),
			?SKIPT("Option 'priority' not supported")
		end;
	    {ok, LSock} when is_port(LSock) ->
		LSock
	end,
    ?P("set opts on listen socket: prio to 1"),
    ok = inet:setopts(L,[{priority,1}]),
    ?P("verify prio"),
    case inet:getopts(L,[priority]) of
	{ok,[{priority,1}]} ->
            ?P("close listen socket"),
	    gen_tcp:close(L),
	    test_prio_put_get(Config),
	    test_prio_accept(Config),
	    test_prio_accept2(Config),
	    test_prio_accept3(Config),
	    test_prio_accept_async(Config),
	    test_prio_fail(Config),
	    test_prio_udp(),
            ?P("done"),
	    ok;
	_X ->
	    case os:type() of
		{unix,linux} ->
		    case os:version() of
			{X,Y,_} when (X > 2) or ((X =:= 2) and (Y >= 4)) ->
                            ?P("so prio should work on this version: "
                               "~n      ~p", [_X]),
			    ct:fail({error,
					   "so_priority should work on this "
					   "OS, but does not"});
			_ ->
			    {skip, "SO_PRIORITY not suppoorted"}
		    end;
		_ ->
		   {skip, "SO_PRIORITY not suppoorted"}
	    end
    end.



%% IP_RECVTOS and IP_RECVTCLASS for IP_PKTOPTIONS
%% does not seem to be implemented in Linux until kernel 3.1
%%
%% It seems pktoptions does not return valid values
%% for IPv4 connect sockets.  On the accept socket
%% we get valid values, but on the connect socket we get
%% the default values for TOS and TTL.
%%
%% Therefore the argument CheckConnect that enables
%% checking the returned values for the connect socket.
%% It is only used for recvtclass that is an IPv6 option
%% and there we get valid values from both socket ends.

has_support_ip_pktoptions() ->
    has_support_ip_option(pktoptions).

has_support_ip_recvtos() ->
    has_support_ip_option(recvtos).

has_support_ip_recvttl() ->
    has_support_ip_option(recvttl).

has_support_ipv6_pktoptions() ->
    has_support_ipv6_option(pktoptions).

has_support_ipv6_tclass() ->
    has_support_ipv6_option(tclass).

has_support_ip_option(Opt) ->
    has_support_option(ip, Opt).

has_support_ipv6_option(Opt) ->
    has_support_option(ipv6, Opt).

has_support_option(Level, Option) ->
    try socket:is_supported(options, Level, Option)
    catch
	_:_:_ -> false % Any platform that does not support socket!
    end.

has_os_support_recvtos() ->
    has_os_support_recvtos(os:type(), os:version()).

has_os_support_recvtos({unix, linux}, Version) ->
    not semver_lt(Version, {3,1,0});
has_os_support_recvtos(_, _) ->
    true.

has_os_support_recvttl() ->
    has_os_support_recvttl(os:type(), os:version()).

has_os_support_recvttl({unix, linux}, Version) ->
    not semver_lt(Version, {2,7,0});
has_os_support_recvttl(_, _) ->
    %% We should not even get here, but just in case...
    false.

has_os_support_recvtclass() ->
    has_os_support_recvtclass(os:type(), os:version()).

has_os_support_recvtclass({unix, linux}, Version) ->
    not semver_lt(Version, {3,1,0});
has_os_support_recvtclass(_, _) ->
    true.

semver_lt({X1,Y1,Z1} = V1, {X2,Y2,Z2} = V2) ->
    ?P("semver_lt -> OS version check:"
       "~n   (Actual)  Version 1: ~p"
       "~n   (Request) Version 2: ~p", [V1, V2]),
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

recvtos(Config) ->
    test_pktoptions(
      Config,
      inet, [{recvtos,tos,96}],
      fun() ->
              has_support_ip_recvtos() andalso
                  has_os_support_recvtos() 
     end,
      "recvtos",
      false).

recvtosttl(Config) ->
    test_pktoptions(
      Config,
      inet, [{recvtos,tos,96},{recvttl,ttl,33}],
      fun() ->
              has_support_ip_recvtos() andalso
		  has_support_ip_recvttl() andalso
                  has_os_support_recvtos() andalso
                  has_os_support_recvttl()
      end,
      "recvtos and/or recvttl",
      false).

recvttl(Config) ->
    test_pktoptions(
      Config,
      inet, [{recvttl,ttl,33}],
      fun() ->
              has_support_ip_recvttl() andalso
                  has_os_support_recvttl()
      end,
      "recvttl",
      false).

recvtclass(Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_pktoptions(
              Config,
              inet6, [{recvtclass,tclass,224}],
              fun() ->
                      has_support_ipv6_tclass() andalso
                          has_os_support_recvtclass()
              end,
              "tclass",
              true);
        [] ->
            {skip,{ipv6_not_supported,IFs}}
    end.

test_pktoptions(Config, Family, Spec, OptCond, OptStr, CheckConnect) ->
    ?P("test_pktoptions -> begin test with"
       "~n   Config:       ~p"
       "~n   Family:       ~p"
       "~n   Spec:         ~p"
       "~n   CheckConnect: ~p",
       [Config, Family, Spec, CheckConnect]),
    PktOptsCond = fun(inet)  -> has_support_ip_pktoptions();
		     (inet6) -> has_support_ipv6_pktoptions()
		  end,
    try PktOptsCond(Family) of
	true ->
	    ?P("pktoptions supported for family ~w", [Family]),
	    case OptCond() of
		true ->
		    ?P("options supported: "
		       "~n   ~p", [Spec]),
		    test_pktoptions(Config, Family, Spec, CheckConnect);
		false ->
		    {skip, ?F("Option(s) ~s not supported", [OptStr])}
	    end;
        false ->
            {skip,
             ?F("Option 'pktoptions' not supported for family ~w", [Family])}
    catch
        _:_:_ ->
            {skip,
             ?F("Option 'pktoptions' not supported for family ~w", [Family])}
    end.
%%
test_pktoptions(Config, Family, Spec, CheckConnect) ->
    ?P("test_pktoptions -> begin test with"
       "~n   Config:       ~p"
       "~n   Family:       ~p"
       "~n   Spec:         ~p"
       "~n   CheckConnect: ~p",
       [Config, Family, Spec, CheckConnect]),
    Timeout = 5000,
    RecvOpts = [RecvOpt || {RecvOpt,_,_} <- Spec],
    TrueRecvOpts = [{RecvOpt,true} || {RecvOpt,_,_} <- Spec],
    FalseRecvOpts = [{RecvOpt,false} || {RecvOpt,_,_} <- Spec],
    Opts = [Opt || {_,Opt,_} <- Spec],
    OptsVals = [{Opt,Val} || {_,Opt,Val} <- Spec],
    Address =
        case Family of
            inet ->
                {127,0,0,1};
            inet6 ->
                {0,0,0,0,0,0,0,1}
        end,
    %%
    %% Set RecvOpts on listen socket
    ?P("try create listen socket with: ~p", [TrueRecvOpts]),
    {ok, L} =
        ?LISTEN(Config, 
                0,
                [Family,binary,{active,false},{send_timeout,Timeout}
                 |TrueRecvOpts]),
    {ok, P} = inet:port(L),
    ?P("get (recv) options for listen socket: "
       "~n   ~p", [RecvOpts]),
    {ok, TrueRecvOpts} = inet:getopts(L, RecvOpts),
    ?P("get options for listen socket: "
       "~n   ~p", [Opts]),
    {ok, OptsValsDefault} = inet:getopts(L, Opts),

    %%
    %% Set RecvOpts and Option values on connect socket
    ?P("create connect socket with"
       "~n   ~p", [TrueRecvOpts ++ OptsVals]),
    {ok, S2} =
        ?CONNECT(Config,
                 Address, P,
                 [Family,binary,{active,false},{send_timeout,Timeout}
                  |TrueRecvOpts ++ OptsVals],
                 Timeout),
    ?P("get (recv) options for connect socket: "
       "~n   ~p", [RecvOpts]),
    {ok, TrueRecvOpts} = inet:getopts(S2, RecvOpts),
    ?P("get options for connect socket: "
       "~n   ~p", [Opts]),
    {ok, OptsVals} = inet:getopts(S2, Opts),

    %%
    %% Accept socket inherits the options from listen socket
    ?P("try create accept socket"),
    {ok, S1} = gen_tcp:accept(L, Timeout),
    ?P("get (recv) options for accept socket: "
       "~n   ~p", [RecvOpts]),
    {ok, TrueRecvOpts} = inet:getopts(S1, RecvOpts),

    ?P("get options for accept socket: "
       "~n   ~p", [Opts]),
    {ok, OptsValsDefault} = inet:getopts(S1, Opts),
    ?P("accept socket option values: "
       "~n   ~p", [OptsValsDefault]),

%%%    %%
%%%    %% Handshake
%%%    ok = gen_tcp:send(S1, <<"hello">>),
%%%    {ok,<<"hello">>} = gen_tcp:recv(S2, 5, Timeout),
%%%    ok = gen_tcp:send(S2, <<"hi">>),
%%%    {ok,<<"hi">>} = gen_tcp:recv(S1, 2, Timeout),

    %%
    %% Verify returned remote options
    VerifyRemOpts =
        fun(S, Role) ->
                case inet:getopts(S, [pktoptions]) of
                    {ok, []} ->
                        ?SKIPT("pktoptions not supported");
                    {ok, [{pktoptions, PktOpts1}]} ->
                        ?P("PktOptions (~w): "
                           "~n      ~p", [Role, PktOpts1]),
                        PktOpts1;
                    {ok, UnexpOK1} ->
                        ?P("Unexpected OK (~w): "
                           "~n   ~p"
                           "~n", [Role, UnexpOK1]),
                        exit({unexpected_getopts_ok,
                              Role,
                              Spec,
                              TrueRecvOpts,
                              OptsVals,
                              OptsValsDefault,
                              UnexpOK1});
                    {error, UnexpERR1} ->
                        ?P("Unexpected ERROR (~w): "
                           "~n   ~p"
                           "~n", [Role, UnexpERR1]),
                        exit({unexpected_getopts_failure,
                              Role,
                              Spec,
                              TrueRecvOpts,
                              OptsVals,
                              OptsValsDefault,
                              UnexpERR1})
                end
        end,
    ?P("verify dest (accept)"),
    OptsVals1 = VerifyRemOpts(S1, dest),
    ?P("verify orig (connect)"),
    OptsVals2 = VerifyRemOpts(S2, orig),
    %% {ok,[{pktoptions,OptsVals1}]} = inet:getopts(S1, [pktoptions]),
    %% {ok,[{pktoptions,OptsVals2}]} = inet:getopts(S2, [pktoptions]),
    (Result1 = sets_eq(OptsVals1, OptsVals))
        orelse ?P("Accept differs: ~p neq ~p", [OptsVals1,OptsVals]),
    (Result2 = sets_eq(OptsVals2, OptsValsDefault))
        orelse ?P("Connect differs: ~p neq ~p",
                  [OptsVals2, OptsValsDefault]),
    %%
    ?P("close connect socket"),
    ok = gen_tcp:close(S2),
    ?P("close accept socket"),
    ok = gen_tcp:close(S1),
    %%
    %%
    %% Clear RecvOpts on listen socket and set Option values
    ?P("clear (recv) options on listen socket"),
    ok = inet:setopts(L, FalseRecvOpts ++ OptsVals),
    {ok,FalseRecvOpts} = inet:getopts(L, RecvOpts),
    ?P("set  options on listen socket"),
    {ok,OptsVals} = inet:getopts(L, Opts),

    %%
    %% Set RecvOpts on connecting socket
    %%
    ?P("create connect socket with"
       "~n   ~p", [TrueRecvOpts]),
    {ok,S4} =
        ?CONNECT(Config, 
          Address, P,
          [Family,binary,{active,false},{send_timeout,Timeout}
          |TrueRecvOpts],
          Timeout),
    ?P("get (recv) options on connect socket"),
    {ok,TrueRecvOpts} = inet:getopts(S4, RecvOpts),
    ?P("get options on connect socket"),
    {ok,OptsValsDefault} = inet:getopts(S4, Opts),

    %%
    %% Accept socket inherits the options from listen socket
    ?P("create accept socket"),
    {ok,S3} = gen_tcp:accept(L, Timeout),
    ?P("get (recv) options on accept socket"),
    {ok,FalseRecvOpts} = inet:getopts(S3, RecvOpts),
    {ok,OptsVals} = inet:getopts(S3, Opts),
    %%
    %% Verify returned remote options
    ?P("verify pktoptions on accept socket"),
    {ok,[{pktoptions,[]}]} = inet:getopts(S3, [pktoptions]),
    ?P("verify pktoptions on connect socket"),
    {ok,[{pktoptions,OptsVals4}]} = inet:getopts(S4, [pktoptions]),
    ?P("verify options set"),
    (Result3 = sets_eq(OptsVals4, OptsVals))
        orelse ?P("Accept2 differs: ~p neq ~p", [OptsVals4, OptsVals]),
    %%
    ?P("close connect socket"),
    ok = gen_tcp:close(S4),
    ?P("close accept socket"),
    ok = gen_tcp:close(S3),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    ?P("verify final result"
       "~n   Result1:       ~p"
       "~n   Check Connect: ~p"
       "~n   Result2:       ~p"
       "~n   Result3:       ~p",
      [Result1, CheckConnect, Result2, Result3]),
    (Result1 and ((not CheckConnect) or (Result2 and Result3)))
        orelse
        exit({failed,
              [{OptsVals1,OptsVals4,OptsVals},
               {OptsVals2,OptsValsDefault}]}),
    ?P("done"),
    ok.

sets_eq(L1, L2) ->
    lists:sort(L1) == lists:sort(L2).



%% Accept test utilities (suites are below)

millis() ->
    erlang:monotonic_time(millisecond).
	
collect_accepts(0,_) -> [];
collect_accepts(N,Tmo) ->
    A = millis(),
    receive
	{accepted, P, {error, eaddrnotavail = Reason}} ->
            ?P("~p Failed accept: ~p", [P, Reason]),
            ?SKIPT(accept_failed_str(Reason));

        {accepted,P,Msg} ->
            ?P("received accepted from ~p: "
               "~n      ~p", [P, Msg]),
            NextN = if N =:= infinity -> N; true -> N - 1 end,
	    [{P,Msg}] ++ collect_accepts(NextN, Tmo - (millis()-A))

    after Tmo ->
            ?P("accept timeout (~w)", [Tmo]),
	    []
    end.
   
-define(EXPECT_ACCEPTS(Pattern,N,Timeout),
	(fun() ->
                 case collect_accepts((N), (Timeout)) of
		     Pattern ->
			 ok;
		     Other__ ->
			 {error,{unexpected,{Other__,process_info(self(),messages)}}}
		 end
	 end)()).
	
collect_connects(Tmo) ->
    A = millis(),
    receive
	{connected, P, {error, eaddrnotavail = Reason}} ->
            ?P("~p Failed connect: ~p", [P, Reason]),
            ?SKIPT(connect_failed_str(Reason));

	{connected,P,Msg} ->
            ?P("received connected from ~p: "
               "~n      ~p", [P, Msg]),
	    [{P,Msg}] ++ collect_connects(Tmo-(millis() - A))

    after Tmo ->
	    []
    end.
   
-define(EXPECT_CONNECTS(Pattern,Timeout),
	(fun() ->
		 case collect_connects(Timeout) of
		     Pattern ->
			 ok;
		     Other ->
			 {error,{unexpected,Other}}
		 end
	 end)()).

mktmofun(Tmo,Parent,LS) ->
    fun() -> Parent ! {accepted,self(), catch gen_tcp:accept(LS,Tmo)} end.

%% Accept tests
%% Test singular accept.
primitive_accept(Config) when is_list(Config) ->
    try do_primitive_accept(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_primitive_accept(Config) ->
    LSock =
        case ?LISTEN(Config, 0,[]) of
            {ok, LS} ->
                LS;
            {error, LReason} ->
                ?SKIPT(listen_failed_str(LReason))
        end,
    {ok, PortNo} = inet:port(LSock),
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LSock)} end,
    P = spawn(F),
    case ?CONNECT(Config, "localhost", PortNo, []) of
        {ok, _} ->
            ok;
        {error, eaddrnotavail = CReason} ->
            ?SKIPT(connect_failed_str(CReason))
    end,        
    receive
        {accepted,P,{ok,P0}} when is_port(P0) ->
            ok;
        {accepted,P,Other0} ->
            {error,Other0}
    after 500 ->
              {error,timeout}
    end.

	
%% Closing listen socket when multi-accepting.
multi_accept_close_listen(Config) when is_list(Config) ->
    ?TC_TRY(multi_accept_close_listen,
            fun() -> do_multi_accept_close_listen(Config) end).

do_multi_accept_close_listen(Config) ->
    ?P("try create listen socket"),
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    F = fun() ->
                ?P("started"),
                Accepted = gen_tcp:accept(LS),
                ?P("accept result: ~p", [Accepted]),
                Parent ! {accepted,self(),Accepted}
        end,
    ?P("create acceptor processes"),
    spawn(F),
    spawn(F),
    spawn(F),
    spawn(F),
    ?P("sleep some"),
    ct:sleep(?SECS(2)),
    ?P("close (listen) socket"),
    gen_tcp:close(LS),
    ?P("await accepts"),
    ok = ?EXPECT_ACCEPTS([{_,{error,closed}},{_,{error,closed}},
                          {_,{error,closed}},{_,{error,closed}}],4,500),
    ?P("done"),
    ok.
	
%% Single accept with timeout.
accept_timeout(Config) when is_list(Config) ->
    try do_accept_timeout(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeout(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS,1000)} end,
    P = spawn(F),
    ok = ?EXPECT_ACCEPTS([{P,{error,timeout}}],1,2000).

%% Check that multi-accept timeouts happen in the correct order.
accept_timeouts_in_order(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(1000,Parent,LS)),
    P2 = spawn(mktmofun(1200,Parent,LS)),
    P3 = spawn(mktmofun(1300,Parent,LS)),
    P4 = spawn(mktmofun(1400,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P1,{error,timeout}},{P2,{error,timeout}},
                          {P3,{error,timeout}},{P4,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order (more).
accept_timeouts_in_order2(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order2(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order2(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(1400,Parent,LS)),
    P2 = spawn(mktmofun(1300,Parent,LS)),
    P3 = spawn(mktmofun(1200,Parent,LS)),
    P4 = spawn(mktmofun(1000,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P4,{error,timeout}},{P3,{error,timeout}},
                          {P2,{error,timeout}},{P1,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order (even more).
accept_timeouts_in_order3(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order3(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order3(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(1200,Parent,LS)),
    P2 = spawn(mktmofun(1400,Parent,LS)),
    P3 = spawn(mktmofun(1300,Parent,LS)),
    P4 = spawn(mktmofun(1000,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P4,{error,timeout}},{P1,{error,timeout}},
                          {P3,{error,timeout}},{P2,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order after
%% mixing millsec and sec timeouts.
accept_timeouts_in_order4(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order4(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order4(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(200,Parent,LS)),
    P2 = spawn(mktmofun(400,Parent,LS)),
    P3 = spawn(mktmofun(1000,Parent,LS)),
    P4 = spawn(mktmofun(600,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P1,{error,timeout}},{P2,{error,timeout}},
			  {P4,{error,timeout}},{P3,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order after
%% mixing millsec and sec timeouts (more).
accept_timeouts_in_order5(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order5(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order5(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(400,Parent,LS)),
    P2 = spawn(mktmofun(1000,Parent,LS)),
    P3 = spawn(mktmofun(600,Parent,LS)),
    P4 = spawn(mktmofun(200,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P4,{error,timeout}},{P1,{error,timeout}},
			  {P3,{error,timeout}},{P2,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order after
%% mixing millsec and sec timeouts (even more).
accept_timeouts_in_order6(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order6(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order6(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(1000,Parent,LS)),
    P2 = spawn(mktmofun(400,Parent,LS)),
    P3 = spawn(mktmofun(600,Parent,LS)),
    P4 = spawn(mktmofun(200,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P4,{error,timeout}},{P2,{error,timeout}},
			  {P3,{error,timeout}},{P1,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order after
%% mixing millsec and sec timeouts (even more++).
accept_timeouts_in_order7(Config) when is_list(Config) ->
    try do_accept_timeouts_in_order7(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_in_order7(Config) ->
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    P1 = spawn(mktmofun(1000,Parent,LS)),
    P2 = spawn(mktmofun(200,Parent,LS)),
    P3 = spawn(mktmofun(1200,Parent,LS)),
    P4 = spawn(mktmofun(600,Parent,LS)),
    P5 = spawn(mktmofun(400,Parent,LS)),
    P6 = spawn(mktmofun(800,Parent,LS)),
    P7 = spawn(mktmofun(1600,Parent,LS)),
    P8 = spawn(mktmofun(1400,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P2,{error,timeout}},{P5,{error,timeout}},
			  {P4,{error,timeout}},{P6,{error,timeout}},
			  {P1,{error,timeout}},{P3,{error,timeout}},
			  {P8,{error,timeout}},{P7,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts behave correctly when
%% mixed with successful timeouts.
accept_timeouts_mixed(Config) when is_list(Config) ->
    ?TC_TRY(accept_timeouts_mixed,
	    fun() -> do_accept_timeouts_mixed(Config) end).

do_accept_timeouts_mixed(Config) ->
    ?P("create listen socket"),
    LS = case ?LISTEN(Config, 0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    {ok,PortNo}=inet:port(LS),

    ?P("create acceptor process 1 (with timeout 1000)"),
    P1 = spawn(mktmofun(1000,Parent,LS)),

    ?P("await ~p accepting", [P1]),
    wait_until_accepting(P1,500),

    ?P("create acceptor process 2 (with timeout 2000)"),
    P2 = spawn(mktmofun(2000,Parent,LS)),

    ?P("await ~p accepting", [P2]),
    wait_until_accepting(P2,500),

    ?P("create acceptor process 3 (with timeout 3000)"),
    P3 = spawn(mktmofun(3000,Parent,LS)),

    ?P("await ~p accepting", [P3]),
    wait_until_accepting(P3,500),

    ?P("create acceptor process 4 (with timeout 4000)"),
    P4 = spawn(mktmofun(4000,Parent,LS)),

    ?P("await ~p accepting", [P4]),
    wait_until_accepting(P4,500),

    ?P("expect accept from 1 (~p) with timeout", [P1]),
    ok = ?EXPECT_ACCEPTS([{P1,{error,timeout}}],infinity,1500),

    ?P("connect"),
    case ?CONNECT(Config, "localhost", PortNo, []) of
        {ok, _} ->
            ok;
        {error, eaddrnotavail = Reason1} ->
            ?SKIPT(connect_failed_str(Reason1))
    end,

    ?P("expect accept from 2 (~p) with success", [P2]),
    if is_port(LS) ->
            ok = ?EXPECT_ACCEPTS([{P2,{ok,Port0}}] when is_port(Port0),
                                                        infinity,100);
       true ->
            ok = ?EXPECT_ACCEPTS([{P2,{ok,_}}],infinity,100)
    end,

    ?P("expect accept from 3 (~p) with timeout", [P3]),
    ok = ?EXPECT_ACCEPTS([{P3,{error,timeout}}],infinity,2000),

    ?P("connect"),
    case ?CONNECT(Config, "localhost", PortNo, []) of
        {error, eaddrnotavail = Reason2} ->
            ?SKIPT(connect_failed_str(Reason2));
        _  ->
            ok
    end,

    ?P("expect accept from 4 (~p) with success", [P4]),
    if is_port(LS) ->
            ok = ?EXPECT_ACCEPTS([{P4,{ok,Port1}}] when is_port(Port1),
                                                        infinity,100);
       true ->
            ok = ?EXPECT_ACCEPTS([{P4,{ok,_Port1}}],infinity,100)
    end,

    ?P("done"),
    ok.

%% Check that single acceptor behaves as expected when killed.
killing_acceptor(Config) when is_list(Config) ->
    ?TC_TRY(killing_acceptor, fun() -> do_killing_acceptor(Config) end).

do_killing_acceptor(Config) ->
    ?P("create listen socket"),
    case ?LISTEN(Config, 0,[]) of
        {ok, LSocket} when is_port(LSocket) ->
            do_killing_acceptor_inet(LSocket);
        {ok, LSocket} ->
            do_killing_acceptor_socket(LSocket);
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(listen_failed_str(Reason))
    end,
    ?P("done"),
    ok.

do_killing_acceptor_inet(LS) ->
    ?P("validate state - listening"),
    validate_acceptor_state(LS, [listen], []),

    ?P("create acceptor process"),
    Pid = spawn(
            fun() ->
                    erlang:display({accepted,self(),gen_tcp:accept(LS)})
            end),
    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, [accepting], []),

    ?P("kill acceptor"),
    exit(Pid, kill),
    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - listening"),
    validate_acceptor_state(LS, [listen], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.

do_killing_acceptor_socket(LS) ->
    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], []),

    ?P("create acceptor process"),
    Pid = spawn(
            fun() ->
                    erlang:display({accepted,self(),gen_tcp:accept(LS)})
            end),
    ?P("sleep some"),
    ct:sleep(100),

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, 1, [accepting], []),

    ?P("kill acceptor"),
    exit(Pid, kill),
    ?P("sleep some"),
    ct:sleep(100),

    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.
    
validate_acceptor_state(LS, ExpStates, ExpNotStates) when is_port(LS) ->
    case inet:info(LS) of
        #{states := States} ->

            ?P("try validate state when: "
               "~n   Exp States:     ~p"
               "~n   Exp Not States: ~p"
               "~n   States:         ~p", [ExpStates, ExpNotStates, States]),

            %% State *shall* contain ExpStates => States1 =/= States
            States1 = States -- ExpStates,

            %% State shall *not* contain ExpNotStates => States2 =:= States
            States2 = States -- ExpNotStates,

            if
                (States1 =/= States) andalso
                (States2 =:= States) ->
                    ?P("validated: "
                       "~n    Expected States:     ~p"
                       "~n    Expected Not States: ~p",
                       [ExpStates, ExpNotStates]),
                    ok;
               true ->
                    ?P("invalid states: "
                       "~n   Expected States:     ~p"
                       "~n   Expected Not States: ~p"
                       "~n   States:              ~p",
                       [ExpStates, ExpNotStates, States]),
                    ct:fail("Invalid state(s)")
            end;

        InvalidInfo ->
            ?P("invalid info: "
               "~n   Expected States:     ~p"
               "~n   Expected Not States: ~p"
               "~n   Invalid Info:        ~p",
               [ExpStates, ExpNotStates, InvalidInfo]),
            ct:fail("Invalid state")
    end.

validate_acceptor_state(LS, ExpNumAcc, ExpState, ExpNotState) ->
    case inet:info(LS) of
        #{num_acceptors := ExpNumAcc, rstates := States} ->
            ?P("try validate state when: "
               "~n   Expected State:     ~p"
               "~n   Expected Not State: ~p"
               "~n   RStates:            ~p", [ExpState, ExpNotState, States]),

            %% States *shall* contain ExpState => States1 =/= States
            States1 = States -- ExpState,
            
            %% States shall *not* contain ExpNotState => States2 =:= States
            States2 = States -- ExpNotState,

            if
                (States1 =/= States) andalso
                (States2 =:= States) ->
                    ?P("validated: "
                       "~n    Expected States:     ~p"
                       "~n    Expected Not States: ~p",
                       [ExpState, ExpNotState]),
                    ok;
               true ->
                    ?P("invalid states: "
                       "~n   Expected State:     ~p"
                       "~n   Expected Not State: ~p"
                       "~n   States:             ~p",
                       [ExpState, ExpNotState, States]),
                    ct:fail("Invalid state(s)")
            end;

        #{num_acceptors := NumAcc, rstates := RStates, wstates := WStates} ->
            ?P("invalid state: "
               "~n   Expected Num Acceptors: ~w"
               "~n   Num Acceptors:          ~w"
               "~n   Expected State:         ~p"
               "~n   Expected Not State:     ~p"
               "~n   RStates:                ~p"
               "~n   WStates:                ~p",
               [ExpNumAcc, NumAcc, ExpState, RStates, WStates]),
            ct:fail("Invalid state");

        InvalidInfo ->
            ?P("invalid info: "
               "~n   Expected Num Acceptors: ~w"
               "~n   Expected State:         ~p"
               "~n   Expected Not State:     ~p"
               "~n   Invalid Info:           ~p",
               [ExpNumAcc, ExpNumAcc, ExpState, InvalidInfo]),
            ct:fail("Invalid state")
    end.
    

%% Check that multi acceptors behaves as expected when killed.
killing_multi_acceptors(Config) when is_list(Config) ->
    ?TC_TRY(killing_multi_acceptors,
            fun() -> do_killing_multi_acceptors(Config) end).

do_killing_multi_acceptors(Config) ->
    ?P("create listen socket"),
    case ?LISTEN(Config, 0,[]) of
        {ok, LSocket} when is_port(LSocket) ->
            do_killing_multi_acceptors_inet(LSocket);
        {ok, LSocket} ->
            do_killing_multi_acceptors_socket(LSocket);
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(listen_failed_str(Reason))
    end,
    ?P("done"),
    ok.

do_killing_multi_acceptors_inet(LS) ->
    ?P("validate state - listen"),
    validate_acceptor_state(LS, [listen], []),

    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),
    ?P("create first acceptor"),
    Pid = spawn(F),
    ?P("create second acceptor - with timeout"),
    Pid2 = spawn(F2),

    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, [accepting], []),

    ?P("kill first acceptor"),
    exit(Pid, kill),

    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - still accepting"),
    validate_acceptor_state(LS, [accepting], []),

    ?P("await second acceptor exit - timeout"),
    case ?EXPECT_ACCEPTS([{Pid2, {error,timeout}}], 1, 1000) of
	ok ->
	    ?P("second acceptor - expected result"),
	    ok;
	Any ->
	    ?P("second acceptor - failed:"
	       "~n      ~p", [Any]),
	    %% Check what Pid2 is doing
	    Pid2Info1 = process_info(Pid2),
	    Pid2Msgs1 = process_info(Pid2, messages),
	    %% Check if this is just a race...
	    ?SLEEP(?SECS(1)),
	    Pid2Info2 = process_info(Pid2),
	    Pid2Msgs2 = process_info(Pid2, messages),
	    ?P("second acceptor failed - check ~p"
	       "~n      Info 1: ~p"
	       "~n      Msgs 1: ~p"
	       "~n   After 1 sec sleep:"
	       "~n      Info 2: ~p"
	       "~n      Msgs 2: ~p",
	       [Pid2, Pid2Info1, Pid2Msgs1, Pid2Info2, Pid2Msgs2]),
	    ct:fail({unexpected_accepts, Any})
    end,

    ?P("validate state - *not* accepting"),
    validate_acceptor_state(LS, [listen], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.

do_killing_multi_acceptors_socket(LS) ->
    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], []),

    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),
    ?P("create first acceptor"),
    Pid = spawn(F),
    ?P("create second acceptor - with timeout"),
    Pid2 = spawn(F2),

    ?P("sleep some"),
    ct:sleep(100),

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, 2, [accepting], []),

    ?P("kill first acceptor"),
    exit(Pid, kill),
    ?P("sleep some"),
    ct:sleep(100),

    ?P("validate state - (still) accepting"),
    validate_acceptor_state(LS, 1, [accepting], []),

    ?P("await second acceptor exit - timeout"),
    ok = ?EXPECT_ACCEPTS([{Pid2,{error,timeout}}],1,1000),

    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.


%% Check that multi acceptors behaves as expected when killed (more).
killing_multi_acceptors2(Config) when is_list(Config) ->
    ?TC_TRY(killing_multi_acceptors2,
            fun() -> do_killing_multi_acceptors2(Config) end).

do_killing_multi_acceptors2(Config) ->
    ?P("create listen socket"),
    case ?LISTEN(Config, 0,[]) of
        {ok, LSocket} when is_port(LSocket) ->
            do_killing_multi_acceptors2_inet(Config, LSocket);
        {ok, LSocket} ->
            do_killing_multi_acceptors2_socket(Config, LSocket);
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(listen_failed_str(Reason))
    end,
    ?P("done"),
    ok.

do_killing_multi_acceptors2_inet(Config, LS) ->
    ?P("validate state - listen"),
    validate_acceptor_state(LS, [listen], []),

    Parent = self(),
    ?P("get port number for listen socket"),
    {ok, PortNo} = inet:port(LS),

    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),

    ?P("create acceptor process 1"),
    Pid = spawn(F),
    ?P("create acceptor process 2"),
    Pid2 = spawn(F),
    ?P("wait some"),
    receive after 100 -> ok
    end,

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, [accepting], []),

    ?P("kill acceptor 1"),
    exit(Pid,kill),

    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - (still) accepting"),
    {ok, L2} = prim_inet:getstatus(LS),
    true  = lists:member(accepting, L2),

    ?P("kill acceptor 2"),
    exit(Pid2, kill),

    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - listening"),
    validate_acceptor_state(LS, [listen], [accepting]),

    ?P("create acceptor 3"),
    Pid3 = spawn(F2),

    ?P("wait some"),
    receive after 100 -> ok
    end,

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, [accepting], []),

    ?P("connect to port ~p", [PortNo]),
    ?CONNECT(Config, "localhost", PortNo,[]),

    ?P("await accept"),
    ok = ?EXPECT_ACCEPTS([{Pid3,{ok,CSock}}] when is_port(CSock),1,100),

    ?P("validate state - listening"),
    validate_acceptor_state(LS, [listen], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.

do_killing_multi_acceptors2_socket(Config, LS) ->
    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], []),

    Parent = self(),
    ?P("get port number for listen socket"),
    {ok, PortNo} = inet:port(LS),

    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),

    ?P("create acceptor process 1"),
    Pid = spawn(F),
    ?P("create acceptor process 2"),
    Pid2 = spawn(F),

    ?P("wait some"),
    ct:sleep(100),

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, 2, [accepting], []),

    ?P("kill acceptor 1"),
    exit(Pid,kill),

    ?P("sleep some"),
    ct:sleep(100),

    ?P("validate state - (still) accepting"),
    validate_acceptor_state(LS, 1, [accepting], []),

    ?P("kill acceptor 2"),
    exit(Pid2, kill),

    ?P("sleep some"),
    receive after 100 -> ok
    end,

    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], [accepting]),

    ?P("create acceptor 3"),
    Pid3 = spawn(F2),

    ?P("wait some"),
    ct:sleep(100),

    ?P("validate state - accepting"),
    validate_acceptor_state(LS, 1, [accepting], []),

    ?P("connect to port ~p", [PortNo]),
    ?CONNECT(Config, "localhost", PortNo,[]),

    ?P("await accept"),
    ok = ?EXPECT_ACCEPTS([{Pid3,{ok, _CSock}}],1,100),

    ?P("validate state - listening"),
    validate_acceptor_state(LS, 0, [listening], [accepting]),

    ?P("cleanup"),
    (catch gen_tcp:close(LS)),
    ok.


%% Checks that multi-accept works when more than one accept can be
%% done at once (wb test of inet_driver).
several_accepts_in_one_go(Config) when is_list(Config) ->
    ?TC_TRY(several_accepts_in_one_go,
            fun() -> do_several_accepts_in_one_go(Config) end).

do_several_accepts_in_one_go(Config) ->
    ?P("create listen socket"),
    NumActors = 8,
    LS = case ?LISTEN(Config, 0, [{backlog, NumActors}]) of
             {ok, LSock} ->
                 LSock;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    {ok, PortNo} = inet:port(LS),
    F1 = fun() ->
		 ?P("acceptor starting"),
		 Parent ! {accepted,self(),gen_tcp:accept(LS)}
         end,
    F2 = fun() ->
		 ?P("connector starting"),
		 Parent ! {connected,self(),?CONNECT(Config, "localhost",PortNo,[])}
         end,
    Ns = lists:seq(1, NumActors),
    ?P("start acceptors"),
    _  = [spawn(F1) || _ <- Ns],
    ?P("await accept timeouts"),
    ok = ?EXPECT_ACCEPTS([],1,500), % wait for tmo
    ?P("start connectors"),
    _  = [spawn(F2) || _ <- Ns],
    ?P("await accepts"),
    ok = ?EXPECT_ACCEPTS([{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}}],NumActors,15000),
    ?P("await connects"),
    ok = ?EXPECT_CONNECTS([{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}}],1000),
    ?P("done"),
    ok.

flush(Msgs) ->
    erlang:yield(),
    receive Msg -> flush([Msg|Msgs])
    after     0 -> lists:reverse(Msgs)
    end.

wait_until_accepting(Proc,0) ->
    exit({timeout_waiting_for_accepting,Proc});
wait_until_accepting(Proc,N) ->
    case process_info(Proc,current_function) of
        {current_function, {prim_inet, accept0, 3}} ->
            case process_info(Proc, status) of
                {status,waiting} -> 
                    ok;
                _O1 ->
                    receive 
                    after 5 ->
                            wait_until_accepting(Proc, N-1)
                    end
            end;
        {current_function, {gen, do_call, 4}} ->
            case process_info(Proc, status) of
                {status,waiting} -> 
                    ok;
                _O1 ->
                    receive 
                    after 5 ->
                            wait_until_accepting(Proc, N-1)
                    end
            end;
        _O2 ->
            receive 
            after 5 ->
                    wait_until_accepting(Proc, N-1)
            end
    end.


%% Check that accept returns {error, system_limit}
%% (and not {error, enfile}) when running out of ports.
accept_system_limit(Config) when is_list(Config) ->
    Cond = fun() ->
		   case ?IS_SOCKET_BACKEND(Config) of
		       true ->
			   {skip, "Not complient with socket"};
		       false ->
			   ok
		   end
	   end,
    TC   = fun() -> do_accept_system_limit(Config) end,
    ?TC_TRY(accept_system_limit, Cond, TC).

do_accept_system_limit(Config) ->
    ?P("create listen socket"),
    LS = case ?LISTEN(Config, 0, []) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
        end,             
    {ok, {Addr, Port}} = inet:sockname(LS),
    ?P("listen socket \"bound\" to:"
       "~n      Address: ~p"
       "~n      Port:    ~p", [Addr, Port]),
    Me = self(),
    ?P("create connector"),
    Connector = spawn_link(fun() ->
                                   connector(Config, Port, Me)
                           end),
    ?P("sync with connector (~p)", [Connector]),
    receive {Connector, sync} -> Connector ! {self(), continue} end,
    ?P("begin accepting"),
    ok = acceptor(Connector, LS, false, []),
    ?P("stop connector (~p)", [Connector]),
    Connector ! stop,
    ?P("done"),
    ok.

acceptor(Connector, LS, GotSL, A) ->
    case gen_tcp:accept(LS, 1000) of
        {ok, S} ->
            acceptor(Connector, LS, GotSL, [S|A]);
        {error, eaddrnotavail = Reason} ->
            ?SKIPE(accept_failed_str(Reason));
        {error, system_limit} ->
            ?P("acceptor: "
               "system limit => *almost* done (~w)", [length(A)]),
            acceptor(Connector, LS, true, A);
        {error, timeout} when GotSL ->
            ?P("acceptor: timeout (with system limit) => done (~w)",
               [length(A)]),
            ok;
        {error, timeout} ->
            ?P("acceptor: timeout *without* system limit => failure"
               "~n     Number of Accepted: ~w",
               [length(A)]),
            error
    end.

connector(Config, AccPort, Tester) ->
    ?P("[connector] start"),
    ManyPorts = open_ports([]),
    ?P("[connector] length(ManyPorts): ~p", [length(ManyPorts)]),
    Tester ! {self(), sync},
    ?P("[connector] await continute from tester (~p)", [Tester]),
    receive {Tester, continue} -> timer:sleep(100) end,
    ?P("[connector] begin connecting"),
    ConnF =
        fun(Port) ->
                case (catch ?CONNECT(Config, {127,0,0,1}, AccPort)) of
                    {ok, Sock} ->
                        ?P("[connector] success: "
                           "~n      ~p", [Sock]),
                        Sock;
                    {error, eaddrnotavail = Reason} ->
                        ?SKIPE(connect_failed_str(Reason));
                    _Error ->
                        ?P("[connector] failure: "
                           "~n      ~p", [_Error]),
                        port_close(Port)
                end
        end,
    R = [ConnF(Port) || Port <- lists:sublist(ManyPorts, 10)],
    ?P("[connector] await stop"),
    receive stop -> ?P("[connector] stop (~w)", [length(R)]), R end.

open_ports(L) ->
    case catch open_port({spawn_driver, "ram_file_drv"}, []) of
	Port when is_port(Port) ->
	    open_ports([Port|L]);
	{'EXIT', {system_limit, _}} ->
	    {L1, L2} = lists:split(5, L),
	    [port_close(Port) || Port <- L1],
	    L2
    end.


%% Check that active once and tcp_close messages behave as expected.
active_once_closed(Config) when is_list(Config) ->
    ?TC_TRY(active_once_closed, fun() -> do_active_once_closed(Config) end).

do_active_once_closed(Config) ->
    ?P("stage 1"),
    (fun() ->
	     ?P("[stage1] begin setup"),
	     {Loop,A} = setup_closed_ao(Config),
	     ?P("[stage1] begin send"),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ?P("[stage1] try set active:once => expect success"),
	     ok = inet:setopts(A, [{active,once}]),
	     ?P("[stage1] await socket closed"),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     ?P("[stage1] try set active:once => expect failure (einval)"),
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ?P("[stage1] await socket closed - expect timeout"),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ?P("[stage1] done"),
	     ok
     end)(),
    ?P("stage 2"),
    (fun() ->
	     ?P("[stage2] begin setup"),
	     {Loop,A} = setup_closed_ao(Config),
	     ?P("[stage2] begin send"),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ?P("[stage2] try set active:true => expect success"),
	     ok = inet:setopts(A,[{active,true}]),
	     ?P("[stage2] await socket closed"),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     ?P("[stage2] try set active:true => expect failure (einval)"),
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ?P("[stage2] await socket closed - expect timeout"),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ?P("[stage2] done"),
	     ok
     end)(),
    ?P("stage 3"),
    (fun() ->
	     ?P("[stage3] begin setup"),
	     {Loop,A} = setup_closed_ao(Config),
	     ?P("[stage3] begin send"),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ?P("[stage3] try set active:true => expect success"),
	     ok = inet:setopts(A,[{active,true}]),
	     ?P("[stage3] await socket closed"),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     ?P("[stage3] try set active:once => expect failure (einval)"),
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ?P("[stage3] await socket closed - expect timeout"),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ?P("[stage3] done"),
	     ok
     end)(),
    ?P("stage 4"),
    (fun() ->
	     ?P("[stage4] begin setup"),
	     {Loop,A} = setup_closed_ao(Config),
	     ?P("[stage4] begin send"),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ?P("[stage1] try set active:once => expect success"),
	     ok = inet:setopts(A,[{active,once}]),
	     ?P("[stage4] await socket closed"),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     ?P("[stage4] try set active:true => expect failure (einval)"),
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ?P("[stage4] await socket closed - expect timeout"),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ?P("[stage4] done"),
	     ok
     end)(),
    ?P("stage 5"),
    (fun() ->
	     ?P("[stage5] begin setup"),
	     {Loop,A} = setup_closed_ao(Config),
	     ?P("[stage5] begin send"),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ?P("[stage5] try set active:false => expect success"),
	     ok = inet:setopts(A,[{active,false}]),
	     ?P("[stage5] await socket closed => expect timeout"),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ?P("[stage5] try set active:once => expect success"),
	     ok = inet:setopts(A,[{active,once}]),
	     ?P("[stage5] await socket closed"),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     ?P("[stage5] done"),
	     ok
     end)(),
    ?P("done"),
    ok.

%% Check that active n and tcp_close messages behave as expected.
active_n_closed(Config) when is_list(Config) ->
    ?TC_TRY(active_n_closed, fun() -> do_active_n_closed(Config) end).

do_active_n_closed(Config) ->
    ?P("create listen socket"),
    {ok, L} = ?LISTEN(Config, 0, [binary, {active, false}]),

    P = self(),

    {ok, Port} = inet:port(L),

    ClientF =
        fun() ->
                ?P("[client] started"),
                Payload = <<0:50000/unit:8>>,
                Cnt = 10000,
                ?P("[client] send size"),
                P ! {size, Cnt * byte_size(Payload)},
                ?P("[client] try connect"),
                S = case ?CONNECT(Config, "localhost", Port,
                                  [binary, {active, false}]) of
                        {ok, CS} ->
                            ?P("[client] connected"),
                            P ! {continue, self()},
                            CS;
                        {error, eaddrnotavail = Reason} ->
                            ?SKIPE(connect_failed_str(Reason))
                    end,
                ?P("[client] send payload"),
                _ = [gen_tcp:send(S, Payload) || _ <- lists:seq(1, Cnt)],
                ?P("[client] close socket"),
                gen_tcp:close(S),
                ?P("[client] done"),
                exit(ok)
        end,
    ?P("create client process"),
    {Pid, MRef} = spawn_monitor(ClientF),

    ?P("await size"),
    receive {size, SendSize} -> SendSize end,
    ?P("await continue or down"),
    receive
        {continue, Pid} ->
            ?P("got continue"),
            ok;
        {'DOWN', MRef, process, Pid, {skip, _} = SKIP} ->
            ?P("got *unexpected* skip"),
            gen_tcp:close(L),
            throw(SKIP);
        {'DOWN', MRef, process, Pid, ConnectRes} ->
            ?P("got *unexpected* crash: "
              "~n   ~p", [ConnectRes]),
            exit({unexpected, connect, ConnectRes})
    end,
    {ok, S} = gen_tcp:accept(L),
    inet:setopts(S, [{active, 10}]),
    ?P("start collecting data"),
    RecvSize = anc_await_closed_and_down(S, Pid, MRef),

    ?P("close listen socket"),
    gen_tcp:close(L),

    ?P("validate size"),
    if SendSize =:= RecvSize ->
            ?P("done"),
            ok;
       true ->
            ct:fail("Send and Recv size not equal: ~p ~p", [SendSize, RecvSize])
    end.


anc_await_closed_and_down(S, Pid, MRef) ->
    anc_await_closed_and_down(S, Pid, MRef, 0, false, false).

anc_await_closed_and_down(_S, _Pid, _MRef, Size, true, true) ->
    Size;
anc_await_closed_and_down(S, Pid, MRef, Size, Closed, Down) ->
    receive
        {tcp, S, Bin} ->
            %% ?P("got a chunk (~w) of data", [byte_size(Bin)]),
            anc_await_closed_and_down(S, Pid, MRef,
                                      byte_size(Bin) + Size, Closed, Down);
        {tcp_closed, S} ->
            ?P("got closed -> we are done: ~w", [Size]),
            anc_await_closed_and_down(S, Pid, MRef, Size, true, Down);
        {tcp_passive, S} ->
            %% ?P("got passive -> active"),
            inet:setopts(S, [{active, 10}]),
            anc_await_closed_and_down(S, Pid, MRef, Size, Closed, true);
        {'DOWN', MRef, process, Pid, ok} ->
            ?P("Received expected down message regarding client"),
            anc_await_closed_and_down(S, Pid, MRef, Size, Closed, true);

        {'DOWN', MRef, process, Pid, Reason} ->
            ?P("Received UNEXPECTED down message regarding client:"
               "~n   Reason:    ~p"
               "~n   Port Info: ~p",
               [Reason, (catch erlang:port_info(S))]),
            ct:fail({unexpected_client_down, Reason}); 

       Msg ->
            ?P("ignore: ~p", [Msg]),
            anc_await_closed_and_down(S, Pid, MRef, Size, Closed, Down)
    end.

%% Test the send_timeout socket option.
send_timeout(Config) when is_list(Config) ->
    ?TC_TRY(send_timeout, fun() -> do_send_timeout(Config) end).

do_send_timeout(Config) ->
    ?P("begin"),
    Dir = filename:dirname(code:which(?MODULE)),
    ?P("create (slave) node"),
    {ok, RNode} = ?START_SLAVE_NODE(?UNIQ_NODE_NAME, "-pa " ++ Dir),

    {TslTimeout, SndTimeout, BinData, SndBuf} = 
	case ?IS_SOCKET_BACKEND(Config) of
	    true ->
		{100, 3000, binary:copy(<<$a:8>>, 10*1024), 5*1024};
	    false ->
		{1,   1000, binary:copy(<<$a:8>>, 1*1024),  16*1024}
	end,

    %% Basic
    ?P("basic check wo autoclose"),
    send_timeout_basic(Config, BinData, SndBuf, TslTimeout, SndTimeout,
		       false, RNode),
    ?P("basic check w autoclose"),
    send_timeout_basic(Config, BinData, SndBuf, TslTimeout, SndTimeout,
		       true, RNode),

    %% Check timeout length.
    ?P("spawn sink process (check timeout length)"),
    Self = self(),
    {Pid, Mon} = spawn_monitor(
                   fun() ->
                           {A, _} = setup_timeout_sink(Config,
						       RNode, SndTimeout,
						       true, SndBuf),
                           Send = fun() ->
                                          Res = gen_tcp:send(A, BinData),
                                          Self ! Res,
                                          Res
                                  end,
                           {{error, timeout}, _} =
			       timeout_sink_loop(Send, TslTimeout)
                   end),
    Diff = get_max_diff(),
    ?P("Max time for send: ~p", [Diff]),
    true = (Diff > (SndTimeout - 500)) and (Diff < (SndTimeout + 500)),

    %% Wait for the process to die.
    ?P("await (timeout checker) process death"),
    receive {'DOWN', Mon, process, Pid, _} -> ok end,

    %% Check that parallell writers do not hang forever
    ?P("check parallell writers wo autoclose"),
    send_timeout_para(Config, BinData, SndBuf, TslTimeout, SndTimeout,
		      false, RNode),
    ?P("check parallell writers w autoclose"),
    send_timeout_para(Config, BinData, SndBuf, TslTimeout, SndTimeout,
		      true, RNode),

    ?P("stop (slave) node"),
    ?STOP_NODE(RNode),

    ?P("done"),
    ok.

send_timeout_basic(Config, BinData, SndBuf, TslTimeout, SndTimeout,
		   AutoClose, RNode) ->
    ?P("[basic] sink"),
    {A, Pid}              = setup_timeout_sink(Config, RNode, SndTimeout,
					       AutoClose, SndBuf),
    Send                  = fun() -> gen_tcp:send(A, BinData) end,
    {{error, timeout}, _} = timeout_sink_loop(Send, TslTimeout),

    %% Check that the socket is not busy/closed...
    ?P("[basic] verify socket not busy/closed"),
    case gen_tcp:send(A, BinData) of
	{error, Reason} ->
	    ?P("[basic] (expected) send failure"),
	    after_send_timeout(AutoClose, Reason),
	    (catch gen_tcp:close(A)),
	    exit(Pid, kill),
	    ok;
	ok ->
            %% Note that there is no active reader on the other end,
            %% so a 'channel' has been filled, should remain filled.
	    ?P("[basic] UNEXPECTED send success"),
	    (catch gen_tcp:close(A)),
	    exit(Pid, kill),
	    ct:fail("Unexpected send success")
    end.

send_timeout_para(Config, BinData, BufSz, TslTimeout, SndTimeout,
		  AutoClose, RNode) ->
    ?P("[para] sink -> entry with"
       "~n      size(BinData): ~p"
       "~n      BufSz:         ~p"
       "~n      SndTimeout:    ~p"
       "~n      AutoClose:     ~p",
       [byte_size(BinData), BufSz, SndTimeout, AutoClose]),
    {A, Pid} = setup_timeout_sink(Config, RNode, SndTimeout, AutoClose, BufSz),
    Self = self(),
    SenderFun = fun() ->
                        ?P("[para:sender] start"),
			Send = fun() -> gen_tcp:send(A, BinData) end,
			Self ! {self(), timeout_sink_loop(Send, TslTimeout)}
		end,
    ?P("[para] spawn process 1 with sender fun"),
    Snd1 = spawn_link(SenderFun),
    ?P("[para] spawn process 2 with sender fun"),
    Snd2 = spawn_link(SenderFun),

    SockInfo    = fun() -> (catch inet:info(A)) end,
    SockTimeout = fun() ->
                          try inet:getopts(A, [send_timeout]) of
                              {ok, [V2]} ->
                                  V2;
                              {error, R2} ->
                                  ?F("ERROR: ~p", [R2]);
                              X2 ->
                                  ?F("UNKNOWN: ~p", [X2])
                          catch
                              C2:E2:S2 ->
                                  ?F("CATCHED: ~p, ~p, ~p", [C2, E2, S2])
                          end
                  end,

    ?P("[para] await first sender timeout when"
       "~n   Sender 1: ~p"
       "~n   Sender 2: ~p", [Snd1, Snd2]),
    First =
        receive
            {Snd1, {{error, timeout}, N}} ->
                ?P("[para] timeout received from sender 1 (~p, ~p)", [Snd1, N]),
                1;
            {Snd2, {{error, timeout}, N}} ->
                ?P("[para] timeout received from sender 2 (~p, ~p)", [Snd2, N]),
                2;

            {'EXIT', _Pid, {timetrap_timeout, _Timeout, _Stack}} ->
                %% The test case (timetrap) has timed out, which either means
                %% we are running on very slow hw or some system functions
                %% are slowing us down (this test case should never normally
                %% time out like this).
                ?P("Test case timetrap timeout - check for system events"),
                case kernel_test_global_sys_monitor:events(?SECS(5)) of
                    SysEvs when (SysEvs =/= []) ->
                        ?P("timetrap timeout with system events: "
                           "~n   System Events: ~p"
                           "~n   Sender 1 Info: ~p"
                           "~n   Sender 2 Info: ~p"
                           "~n   Socket Info:   ~p",
                           [SysEvs,
                            process_info(Snd1), process_info(Snd2),
                            SockInfo()]),
                        ?SKIPT(?F("TC system ~w events", [length(SysEvs)]));
                    {error, Reason} ->
                        ?P("TC timetrap timeout but failed get system events: "
                           "~n   Reason:        ~p"
                           "~n   Sender 1 Info: ~p"
                           "~n   Sender 2 Info: ~p"
                           "~n   Socket Info:   ~p",
                           [Reason,
                            process_info(Snd1), process_info(Snd2),
                            SockInfo()]),
                        exit({timetrap, {failed_get_sys_evs, Reason}});
                    [] ->
                        ?P("TC timetrap *without* system events: "
                           "~n   Sender 1 Info: ~p"
                           "~n   Sender 2 Info: ~p"
                           "~n   Socket Info:   ~p",
                           [process_info(Snd1), process_info(Snd2),
                            SockInfo()]),
                        exit(timetrap)
                end

        after 20000 ->
                SockInfo1 = SockInfo(),
                SockTo1   = SockTimeout(),
                ?P("[para] UNEXPECTED timeout(1,~w) when:"
                   "~n   Sender 1 Info: ~p"
                   "~n   Sender 2 Info: ~p"
                   "~n   Socket Info:   ~p"
                   "~n   Send Timeout:  ~p"
                   "~n   Message Queue: ~p",
                   [AutoClose,
                    (catch process_info(Snd1)),
                    (catch process_info(Snd2)),
                    SockInfo1, SockTo1,
                    flush([])]),
                Snd1 ! {info_and_die, SockInfo, SockTimeout},
                Snd2 ! {info_and_die, SockInfo, SockTimeout},
                ct:sleep(?SECS(1)),
                exit({timeout, AutoClose})
        end,

    Second = if (First =:= 1) -> 2; true -> 1 end,
    ?P("[para] await second sender ~w error", [Second]),
    receive
	{Snd1, {{error, Error_1}, N_1}} ->
            ?P("[para] error (~p) received from sender 1 (~p, ~p)",
               [Error_1, Snd1, N_1]),
            after_send_timeout(AutoClose, Error_1);
	{Snd2, {{error, Error_1}, N_1}} ->
            ?P("[para] error (~p) received from sender 2 (~p, ~p)",
               [Error_1, Snd2, N_1]),
            after_send_timeout(AutoClose, Error_1)
    after 10000 ->
            if (Second =:= 1) ->
                    SockInfo21 = SockInfo(),
                    SockTo21   = SockTimeout(),
                    ?P("[para] UNEXPECTED timeout(2, ~w):"
                       "~n   Sender 1 Info: ~p"
                       "~n   Socket Info:   ~p"
                       "~n   Send Timeout:  ~p"
                       "~n   Message Queue: ~p",
                       [AutoClose,
                        (catch process_info(Snd1)),
                        SockInfo21, SockTo21,
                        flush([])]),
                Snd1 ! {info_and_die, SockInfo, SockTimeout};
               true ->
                    SockInfo22 = SockInfo(),
                    SockTo22   = SockTimeout(),
                    ?P("[para] UNEXPECTED timeout(2, ~w):"
                       "~n   Sender 2 Info: ~p"
                       "~n   Socket Info:   ~p"
                       "~n   Send Timeout:  ~p"
                       "~n   Message Queue: ~p",
                       [AutoClose,
                        (catch process_info(Snd2)),
                        SockInfo22, SockTo22,
                        flush([])]),
                    Snd2 ! {info_and_die, SockInfo, SockTimeout}
            end,
            ct:sleep(?SECS(1)),
	    exit({timeout, AutoClose, Second})
    end,

    ?P("[para] socket info: "
       "~n   ~p", [SockInfo()]),
    {error, Error_2} = gen_tcp:send(A, BinData),
    after_send_timeout(AutoClose, Error_2),
    ?P("[para] cleanup - await sender terminations"),
    st_await_sender_termination(Snd1, Snd2),
    ?P("[para] cleanup - close socket"),
    (catch gen_tcp:close(A)),
    ?P("[para] cleanup - kill sink"),
    exit(Pid, kill),
    ?P("[para] done"),
    ok.

st_await_sender_termination(undefined, undefined) ->
    ok;
st_await_sender_termination(Sender1, Sender2) ->
    receive
        {'EXIT', Pid, Reason} when (Pid =:= Sender1) ->
            ?P("sender 1 (~p) terminated: "
               "~n   ~p", [Pid, Reason]),
            st_await_sender_termination(undefined, Sender2);
        {'EXIT', Pid, Reason} when (Pid =:= Sender2) ->
            ?P("sender 2 (~p) terminated: "
               "~n   ~p", [Pid, Reason]),
            st_await_sender_termination(Sender1, undefined)
    end.
            
get_max_diff() ->
    receive
	ok ->
	    get_max_diff(0)
    after 10000 ->
	    exit(timeout)
    end.

get_max_diff(Max) ->
    T1 = millis(),
    receive
	ok ->
	    Diff = millis() - T1,
	    if
		Diff > Max ->
		    ?P("new max send time: ~w", [Diff]),
		    get_max_diff(Diff);
		true ->
		    get_max_diff(Max)
	    end;
	{error,timeout} ->
	    Diff = millis() - T1,
	    if
		Diff > Max ->
		    ?P("timeout diff (> prev max send to): ~w", [Diff]),
		    Diff;
		true ->
		    Max
	    end
    after 10000 ->
              exit(timeout)
    end.
	    
after_send_timeout(AutoClose, Reason) ->
    case Reason of
        timeout              when AutoClose =:= false -> ok;
        {timeout, _RestData} when AutoClose =:= false -> ok;
        enotconn             when AutoClose =:= true  -> ok;
	closed               when AutoClose           -> ok;
        _ ->
            ?P("after_send_timeout -> "
               "~n      AutoClose: ~w"
               "~n      Reason:    ~p", [AutoClose, Reason]),
            exit({after_send_timeout, AutoClose, Reason})
    end.


%% Test the send_timeout socket option for active sockets.
send_timeout_active(Config) when is_list(Config) ->
    ?TC_TRY(send_timeout_active, fun() -> do_send_timeout_active(Config) end).

do_send_timeout_active(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, RNode} = ?START_SLAVE_NODE(?UNIQ_NODE_NAME, "-pa " ++ Dir),
    do_send_timeout_active(Config, false, RNode),
    do_send_timeout_active(Config, true, RNode),
    ?STOP_NODE(RNode),
    ok.

do_send_timeout_active(Config, AutoClose, RNode) ->
    {A,C} = setup_active_timeout_sink(Config, RNode, 1, AutoClose),
    inet:setopts(A, [{active, once}]),
    Mad = spawn_link(RNode, fun() -> mad_sender(C) end),
    ListData = lists:duplicate(1000, $a),
    F = fun() ->
                ?P("[sink action] await data"),
		receive
		    {tcp, _Sock, _Data} ->
                        ?P("[sink action] active -> once"),
			inet:setopts(A, [{active, once}]),
                        ?P("[sink action] send payload"),
			Res = gen_tcp:send(A, ListData),
			Res;
		    Unexpected ->
			?P("[sink action] unexpected message: "
                           "~n      ~p", [Unexpected]),
			Unexpected
		end
	end,
    {{error, timeout}, _} = timeout_sink_loop(F, 1),
    unlink(Mad),
    exit(Mad, kill),
    flush(),
    ok.

mad_sender(S) ->
    put(action,  nothing),
    put(sent,    0),
    put(elapsed, 0),
    mad_sender(S, 0).

mad_sender(S, N) ->
    U = rand:uniform(1000000),
    put(action, send),
    Start   = erlang:monotonic_time(),
    Ret     = gen_tcp:send(S, integer_to_list(U)),
    Stop    = erlang:monotonic_time(),
    Elapsed = get(elapsed),
    put(elapsed, Elapsed + (Stop - Start)),
    put(action, sent),
    N2 = N + 1,
    put(sent,   N2),
    case Ret of
        ok ->
            mad_sender(S, N + 1);
        {error, timeout} = ERROR1 ->
            ?P("mad_sender -> send failed: timeout"
               "~n   Number of sends:     ~w"
               "~n   Elapsed (send) time: ~w msec",
               [N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond)]),
            ERROR1;
        {error, {timeout, RestData}} = ERROR2 ->
            ?P("mad_sender -> "
               "send failed: timeout with ~w bytes of rest data"
               "~n   Number of sends:     ~w"
               "~n   Elapsed (send) time: ~w msec",
               [byte_size(RestData),
                N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond)]),
            ERROR2;
        {error, Reason} = ERROR3 ->
            ?P("mad_sender -> send failed: "
               "~n   ~p"
               "~n   Number of sends:     ~w"
               "~n   Elapsed (send) time: ~w msec",
               [Reason,
                N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond)]),
            ERROR3;
        ERROR4 ->
            ?P("mad_sender -> send failed: "
               "~n   ~p"
               "~n   Number of sends:     ~w"
               "~n   Elapsed (send) time: ~w msec",
               [ERROR4,
                N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond)]),
            ERROR4
    end.

flush() ->
    receive
	_X ->
	    flush()
    after 0 ->
	    ok
    end.

setup_closed_ao(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    ?P("[setup] start slave node"),
    R = case ?START_SLAVE_NODE(?UNIQ_NODE_NAME, "-pa " ++ Dir) of
            {ok, Slave} ->
                Slave;
            {error, Reason} ->
                ?SKIPT(?F("failed starting slave node: ~p", [Reason]))
        end,
    Host = get_hostname(node()),
    ?P("[setup] create listen socket"),
    L = case ?LISTEN(Config, 0, [{active,false},{packet,2}]) of
            {ok, LSock} ->
                LSock;
            {error, eaddrnotavail = LReason} ->
                (catch ?STOP_NODE(R)),
                ?SKIPT(listen_failed_str(LReason))
        end,
    Fun = fun(F) ->
                  receive
                      {From,X} when is_function(X) ->
                          From ! {self(),X()}, F(F);
                      die -> ok
                  end
          end,
    ?P("[setup] create remote runner"),
    Pid = rpc:call(R,erlang,spawn,[fun() -> Fun(Fun) end]),
    {ok, Port} = inet:port(L),
    Remote = fun(Fu) ->
                     Pid ! {self(), Fu},
                     receive {Pid,X} -> X end
             end,
    Connect = fun() -> 
                      ?CONNECT(Config, Host, Port,
                                      [{active, false}, {packet, 2}]) 
              end,
    ?P("[setup] create (remote) connection"),
    C = case Remote(Connect) of
            {ok, CSock} ->
                CSock;
            {error, eaddrnotavail = CReason} ->
                (catch ?STOP_NODE(R)),
                ?SKIPT(connect_failed_str(CReason))
        end,
    ?P("[setup] accept (local) connection"),
    A = case gen_tcp:accept(L) of
            {ok, ASock} ->
                ASock;
            {error, eaddrnotavail = AReason} ->
                (catch ?STOP_NODE(R)),
                ?SKIPT(accept_failed_str(AReason))
        end,
    ?P("[setup] send (local) and receive (remote) message"),
    gen_tcp:send(A,"Hello"),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
    ?P("[setup] close (remote) connection"),
    ok =  Remote(fun() -> gen_tcp:close(C) end),
    Loop2 = fun(_,_,_,0) ->
		    {failure, timeout}; 
	       (L2,{MA,MB},F2,N) ->
		    case F2() of
			MA -> ?P("[setup] action result (MA): ~p", [MA] ), MA;
			MB -> ?P("[setup] action result (MB): ~p", [MB] ), MB;
			Other -> ?P("[setup] Loop2: ~p",[Other]),
				 receive after 1000 -> ok end,
				 L2(L2,{MA,MB},F2,N-1)
		    end
	    end,
    Loop = fun(Match2,F3) ->  Loop2(Loop2,Match2,F3,10) end,
    ?P("[setup] stop slave node"),
    ?STOP_NODE(R),
    ?P("[setup] done"),
    {Loop,A}.
    
setup_timeout_sink(Config, RNode, Timeout, AutoClose, BufSz) ->
    Host    = get_hostname(node()),
    ?P("[sink] create listen socket"),
    {ok, L} = ?LISTEN(Config, 0, [{active,             false},
				  {packet,             4},
				  {sndbuf,             BufSz},
				  {send_timeout,       Timeout},
				  {send_timeout_close, AutoClose}]),
    Fun = fun(F) ->
		  receive
		      {From,X} when is_function(X) ->
			  From ! {self(),X()}, F(F);
		      die -> ok
		  end
	  end,
    ?P("[sink] start remote runner process (on ~p)", [RNode]),
    Pid =  rpc:call(RNode, erlang, spawn, [fun() -> Fun(Fun) end]),
    {ok, Port} = inet:port(L),
    Remote = fun(Fu) ->
		     Pid ! {self(), Fu},
		     receive {Pid,X} -> X
		     end
	     end,
    ?P("[sink] connect from remote node (~p)", [RNode]),
    {ok, C} = Remote(fun() ->
			     ?CONNECT(Config, Host,Port,
				      [{active, false},
				       {packet, 4},
				       {sndbuf, BufSz div 2}])
		     end),
    ?P("[sink] accept"),
    {ok, A} = gen_tcp:accept(L),
    ?P("[sink] accepted - send test message"),
    gen_tcp:send(A, "Hello"),
    ?P("[sink] message sent - "
       "recv 'check' message on remote node (~p)", [RNode]),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
    ?P("[sink] cleanup - close listen socket"),
    (catch gen_tcp:close(L)),
    ?P("[sink] done with socket: "
       "~n   ~p", [A]),
    {A, Pid}.

setup_active_timeout_sink(Config, RNode, Timeout, AutoClose) ->
    Host = get_hostname(node()),
    ListenOpts =  [binary,
		   {active,             false},
		   {packet,             0},
		   {nodelay,            true},
		   {keepalive,          true},
		   {send_timeout,       Timeout},
		   {send_timeout_close, AutoClose}],
    {ok, L} = ?LISTEN(Config, 0, ListenOpts),
    Fun = fun(F) ->
		  receive
		      {From,X} when is_function(X) ->
			  From ! {self(),X()},
			  F(F);
		      die -> ok
		  end
	  end,
    Pid = rpc:call(RNode, erlang, spawn, [fun() -> Fun(Fun) end]),
    {ok, Port} = inet:port(L),
    Remote = fun(Fu) ->
		     Pid ! {self(), Fu},
		     receive {Pid,X} -> X
		     end
	     end,
    {ok, C} = Remote(fun() ->
			     ?CONNECT(Config, Host, Port, [{active,false}])
		     end),
    {ok, A} = gen_tcp:accept(L),
    gen_tcp:send(A, "Hello"),
    {ok, "H"++_} = Remote(fun() -> gen_tcp:recv(C, 0) end),
    {A, C}.

%% timeout_sink_loop(Action) ->
%%     timeout_sink_loop(Action, 1).

timeout_sink_loop(Action, To) ->
    put(action,  nothing),
    put(sent,    0),
    put(elapsed, 0),
    timeout_sink_loop(Action, To, 0).

timeout_sink_loop(Action, To, N) ->
    put(action, send),
    Start   = erlang:monotonic_time(),
    Ret     = Action(),
    Stop    = erlang:monotonic_time(),
    Elapsed = get(elapsed),
    put(elapsed, Elapsed + (Stop - Start)),
    put(action, sent),
    N2 = N + 1,
    put(sent,   N2),
    case Ret of
	ok ->
	    receive
                {info_and_die, SockInfo, SockTimeout} ->
                    ?P("[sink-loop] info and die: "
                       "~n   Socket Info:  ~p"
                       "~n   Send Timeout: ~p",
                       [SockInfo(), SockTimeout()]),
                    exit(normal)
            after To -> ok end,
	    timeout_sink_loop(Action, To, N+1);
	{error, {timeout, RestData}} ->
            ?P("[sink-loop] action result: "
               "~n   Number of actions: ~p"
               "~n   Elapsed time:      ~p msec"
               "~n   Result:            timeout with ~w of rest data",
               [N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond),
                byte_size(RestData)]),
	    {{error, timeout}, N2};
	Other ->
            ?P("[sink-loop] action result: "
               "~n   Number of actions: ~p"
               "~n   Elapsed time:      ~p msec"
               "~n   Result:            ~p",
               [N2,
                erlang:convert_time_unit(get(elapsed), native, millisecond),
                Other]),
	    {Other, N2}
    end.
     
has_superfluous_schedulers() ->
    case {erlang:system_info(schedulers),
	  erlang:system_info(logical_processors)} of
	{S, unknown} when S > 1 -> true;
	{S, P} when S > P -> true;
	_ -> false
    end.


%% Leaking message from inet_drv {inet_reply,P,ok}
%% when a socket sending resumes working after a send_timeout.
%% Should we even bother testing this if 'inet_backend = socket'?
otp_7731(Config) when is_list(Config) ->
    ?TC_TRY(otp_7731, fun() -> do_otp_7731(Config) end).

do_otp_7731(Config) when is_list(Config) ->
    ?P("[ctrl] create server"),
    ServerPid = spawn_link(?MODULE, otp_7731_server, [Config, self()]),
    ?P("[ctrl] await listening port (number) from server"),
    receive {ServerPid, ready, PortNum} -> ok end,

    ?P("[ctrl] connect to server on port ~w", [PortNum]),
    {ok, Socket} = ?CONNECT(Config, "localhost", PortNum,
                            [binary, {active, false}, {packet, raw},
                             {send_timeout, 1000}]),

    ?P("[ctrl] send data"),
    otp_7731_send(Socket),
    ?P("[ctrl] sending complete - order server to recv"),
    ServerPid ! {self(), recv},
    ?P("[ctrl] await 'recv complete' from server"),
    receive {ServerPid, ok} -> ?P("[ctrl] received 'recv complete'"), ok end,

    %% Now make sure inet_drv does not leak any internal messages.
    ?P("[ctrl] waiting for leaking messages..."),
    receive Msg ->
            ?P("[ctrl] got unexpected message: "
               "~n      ~p", [Msg]),
	    ct:fail({unexpected, Msg})
    after 1000 ->
	    ok
    end,
    ?P("[ctrl] no leaking messages - cleanup"),
    (catch gen_tcp:close(Socket)),
    ServerPid ! {self(), die},
    ?P("[ctrl] done."),
    ok.
    
otp_7731_send(Socket) ->
    Bin = <<1:10000>>,
    ?P("[client] sending ~p bytes...", [byte_size(Bin)]),
    case gen_tcp:send(Socket, Bin) of
        ok ->
            otp_7731_send(Socket);
        {error, {timeout, RestData}} ->
            ?P("[client] send timeout with ~w bytes of rest data",
               [byte_size(RestData)]),
            ok;
        {error, timeout} ->
            ?P("[client] send timeout"),
            ok
    end.

otp_7731_server(Config, Ctrl) ->
    ?P("[server] create listen socket"),
    {ok, LSocket} = ?LISTEN(Config, 0, [binary, {packet, raw},
                                       {active, false}]),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    ?P("[server] listening on port number ~p", [PortNum]),
    Ctrl ! {self(), ready, PortNum},

    ?P("[server] accept"),
    {ok, CSocket} = gen_tcp:accept(LSocket),
    ?P("[server] accepted - close listen socket"),
    gen_tcp:close(LSocket),

    ?P("[server] await recv order"),
    receive {Ctrl, recv} -> ok end,
    
    ?P("[server] start receiving..."),
    otp_7731_recv(CSocket),

    ?P("[server] announce recv done"),
    Ctrl ! {self(), ok},

    ?P("[server] finished, (connection) closing..."),
    gen_tcp:close(CSocket),
    receive {Ctrl, die} -> ok end,
    ?P("[server] done"),
    exit(normal).


otp_7731_recv(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
        {ok, Bin} ->
            ?P("[server] received ~p bytes", [size(Bin)]),
            otp_7731_recv(Socket);
        {error, timeout} ->
            ?P("[server] receive timeout - done recv"),
            ok
    end.


%% OTP-7615: TCP-ports hanging in CLOSING state when sending large
%% buffer followed by a recv() that returns error due to closed
%% connection.
%% OTP-7615 Leaking closed ports.
zombie_sockets(Config) when is_list(Config) ->
    register(zombie_collector,self()),
    Calls = 10,
    ?P("create zombie server"),
    Server = spawn_link(?MODULE, zombie_server, [Config, self(), Calls]),
    {Server, ready, PortNum} = receive Msg -> Msg  end,
    ?P("Ports before = ~p",[lists:sort(erlang:ports())]),
    zombie_client_loop(Config, Calls, PortNum),
    Ports = lists:sort(zombie_collector(Calls, [])),
    Server ! terminate,
    ?P("Collected ports = ~p", [Ports]),
    [] = zombies_alive(Ports, 10),
    timer:sleep(1000),
    ?P("done"),
    ok.

zombie_client_loop(_Config, 0, _) ->
    ?P("[zombie client] done"),
    ok;
zombie_client_loop(Config, N, PortNum) when is_integer(PortNum) ->
    ?P("[zombie client][~w] try connect", [N]),
    {ok, Socket} = ?CONNECT(Config, "localhost", PortNum,
                            [binary, {active, false}, {packet, raw}]),
    ?P("[zombie client] connected - now close ~p", [Socket]),
    gen_tcp:close(Socket), % to make server recv fail
    zombie_client_loop(Config, N-1, PortNum).


zombie_collector(0, Acc) ->
    ?P("[zombie collector] done: "
       "~n   ~p", [Acc]),
    Acc;
zombie_collector(N, Acc) ->
    receive 
	{closed, Socket} -> 
            ?P("[zombie collector] ~p closed", [Socket]),
	    zombie_collector(N-1, [Socket|Acc]);
	E -> 
	    {unexpected, E, Acc}
    end.
	    
zombies_alive(Ports, WaitSec) ->
    Alive = lists:sort(erlang:ports()),
    ?P("[zombies alive][~w] Alive: ~p", [WaitSec, Alive]),
    Zombies = lists:filter(fun(P) -> lists:member(P, Alive) end, Ports),
    case Zombies of
	[] -> [];
	_ ->
	    case WaitSec of
		0 -> Zombies;
		_ -> timer:sleep(1000), % Wait some more for zombies to die
		     zombies_alive(Zombies, WaitSec-1)
	    end
    end.

zombie_server(Config, Pid, Calls) ->
    ?P("[zombie server] try create listen socket with backlog: ~w", [Calls]),
    {ok, LSock} = ?LISTEN(Config, 0, [binary, {packet, raw},
                                      {active, false}, {backlog, Calls}]),
    {ok, {_, PortNum}} = inet:sockname(LSock),
    ?P("[zombie server] Listening on ~w with port number ~p", [LSock, PortNum]),
    BigBin = list_to_binary(lists:duplicate(100*1024, 77)),
    ?P("[zombie server] send ready"),
    Pid ! {self(), ready, PortNum},
    zombie_accept_loop(LSock, BigBin, Calls),
    ?P("[zombie server] await terminate"),
    terminate = receive Msg -> Msg end,
    ?P("[zombie server] terminating"),
    ok.

zombie_accept_loop(_, _, 0) ->
    ?P("[zombie server] accept loop done"),
    ok;
zombie_accept_loop(Socket, BigBin, Calls) ->
    ?P("[zombie server][~w] try accept", [Calls]),
    case gen_tcp:accept(Socket) of
	{ok, NewSocket} ->
            ?P("[zombie server][~w] accepted ~p - create handler",
               [Calls, NewSocket]),
	    spawn_link(fun() -> zombie_server_handler(NewSocket, BigBin) end),
	    zombie_accept_loop(Socket, BigBin, Calls-1);
	E ->
	    E
    end.

zombie_server_handler(Socket, Bin) ->
    ?P("[zombie server handler] got connection on ~p - attempt send", [Socket]),
    gen_tcp:send(Socket, Bin),
    ?P("[zombie server handler] Sent data, waiting for reply on ~p", [Socket]),
    case gen_tcp:recv(Socket, 4) of
        {error, closed} ->
            ?P("[zombie server handler] recv: closed (expected)"),
            ok;
        {error, econnaborted} -> % may be returned on Windows
            ?P("[zombie server handler] recv: econnaborted (expected)"),
            ok;
        {error, Reason} ->
            ?P("[zombie server handler] UNEXPECTED recv failure reason: ~p",
               [Reason]),
            exit({unexpected_recv_error_reason, Reason});
        {ok, _} ->
            ?P("[zombie server handler] UNEXPECTED recv success"),
            exit(unexpected_recv_success)
	  end,
    ?P("[zombie server handler] close socket ~p", [Socket]),
    gen_tcp:close(Socket),
    ?P("[zombie server handler] socket closed: inform collector"),
    zombie_collector ! {closed, Socket}.


%% Hanging send on windows when sending iolist with more than 16 binaries.
otp_7816(Config) when is_list(Config) ->
    ?TC_TRY(otp_7816, fun() -> do_otp_7816(Config) end).

do_otp_7816(Config) ->
    Ctrl = self(),
    ?P("[ctrl] create server process..."),
    Server = spawn_link(fun() -> otp_7816_server(Config, Ctrl) end),
    ?P("[ctrl] await server process ready..."),
    receive {Server, ready, PortNum} -> ok end,

    ?P("[ctrl] connect to server..."),
    {ok, Socket} = ?CONNECT(Config, "localhost", PortNum,
                            [binary, {active, false}, {packet, 4},
                             {send_timeout, 10}]),

    %% We use the undocumented feature that sending can be resumed after
    %% a send_timeout without any data loss if the peer starts to receive data.
    %% Unless of course the 7816-bug is in affect, in which case the write event
    %% for the socket is lost on windows and not all data is sent.

    ?P("[ctrl] begin sending..."),

    [otp_7816_ctrl(Socket, 18, BinSize, Server) ||
	BinSize <- lists:seq(1000, 2000, 123)],

    ?P("[ctrl] sending complete..."),

    ok = gen_tcp:close(Socket),
    Server ! {self(), closed},
    {Server, closed} = receive M -> M end.


otp_7816_ctrl(Socket, BinNr, BinSize, Server) ->
    ?P("[ctrl] create payload (BinSz: ~w)...", [BinSize]),
    Data = lists:duplicate(BinNr, <<1:(BinSize*8)>>),
    ?P("[ctrl] socket info prior to start sending: "
       "~n      ~p", [inet:info(Socket)]),
    Ctrl   = self(),
    Client = spawn_link(fun() -> otp_7816_send_data(Ctrl, Socket, Data) end),
    SentBytes =
	receive
	    {Client, continue, Loops} ->
		?P("[ctrl] received continue from client: ~p", [Loops]),
		SB = Loops * BinNr * BinSize,
		Server ! {self(), recv, SB},
		SB
	end,
    ct:sleep(1000),
    ?P("[ctrl] socket info after sending ~w bytes: "
       "~n      ~p", [SentBytes, inet:info(Socket)]),
    ?P("[ctrl] await server result..."),
    ok = receive
	     {Server, SR} ->
		 ?P("[ctrl] server result: ~p", [SR]),
		 SR
	 end,
    ?P("[ctrl] await client termination..."),
    ok = receive
	     {'EXIT', Client, normal} ->
		 ?P("[ctrl] client normal exit"),
		 ok;
	     {'EXIT', Client, CR} ->
		 ?P("[ctrl] client unexpected exit reason: ~p", [CR]),
		 CR
	 end,
    ?P("[ctrl] done with BinSz: ~p", [BinSize]),
    ok.
    

otp_7816_send_data(Ctrl, Socket, Data) ->
    otp_7816_send_data(Ctrl, Socket, Data, 0).

otp_7816_send_data(Ctrl, Socket, Data, Loops) ->
    ?P("[client] sending data (~w bytes, ~w)...", [iolist_size(Data), Loops]),
    case gen_tcp:send(Socket, Data) of
	ok ->
	    otp_7816_send_data(Ctrl, Socket, Data, Loops+1);

	{error, timeout} when is_port(Socket) ->
	    %% For the 'classic' sockets, when the OS buffers are
	    %% full, the rest data are put into the (inet driver)
	    %% internal, for later sending. SO, it can be counted
	    %% as sent.
	    ?P("[client] send timeout when Loops: ~p (+1)", [Loops]),
	    Ctrl ! {self(), continue, Loops + 1},
	    exit(normal);

	{error, timeout} ->
	    %% For inet_backend = 'socket' when we nothing of the
	    %% message was sent.
	    ?P("[client] send timeout when Loops: ~p", [Loops]),
	    Ctrl ! {self(), continue, Loops},
	    exit(normal);

	{error, {timeout, RestData}} ->
	    %% A timeout means that **some** of the data was not sent,
	    %% currently there is no way to know how much.
	    %% NOTE THAT THIS MEANS THAT WE HAVE A PARTIAL PACKAGE
	    %% WRITTEN, INCLUDING A HEADER THAT INDUCATES A DATA
	    %% SIZE THAT IS **NOT** PRESENT!!
	    %% So for this trest case to work, we need to write the rest.
	    %% But we cannot do that without first setting the package to raw.
	    %% 
	    ?P("[client] send timeout"
	       "~n      with ~w bytes of rest data"
	       "~n      when Loops: ~p", [byte_size(RestData), Loops]),
	    Ctrl ! {self(), continue, Loops + 1},
	    ?P("[client] packet to 'raw'..."),
	    ok = inet:setopts(Socket, [{packet, raw}, {send_timeout, 1000}]),
	    ?P("[client] send ~w bytes of rest data...",
	       [byte_size(RestData)]),
	    ok = gen_tcp:send(Socket, RestData),
	    ?P("[client] packet (back) to '4'..."),
	    ok = inet:setopts(Socket, [{packet, 4}, {send_timeout, 10}]),
	    ?P("[client] done"),
	    exit(normal)


    end.
    
    
otp_7816_server(Config, Ctrl) ->
    ?P("[server] create listening socket"),
    {ok, LSocket} = ?LISTEN(Config, 0, [binary, {packet, 4},
                                        {active, false}]),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    ?P("[server] listening on ~w with port number ~p", [LSocket, PortNum]),
    Ctrl ! {self(), ready, PortNum},

    ?P("[server] accept connection..."),
    {ok, ASocket} = gen_tcp:accept(LSocket),
    ?P("[server] got connection..."),
    gen_tcp:close(LSocket),

    otp_7816_server_loop(ASocket, Ctrl),

    ?P("[server] terminating").


otp_7816_server_loop(Socket, Ctrl) ->
    ?P("[server] waiting for order..."),
    receive 
	{Ctrl, recv, RecvBytes} -> 
	    ?P("[server] start receiving (~w bytes)...", [RecvBytes]),

	    ok = otp_7816_recv(Socket, RecvBytes),
	    
	    Ctrl ! {self(), ok},
	    otp_7816_server_loop(Socket, Ctrl);
	
	{Ctrl, closed} ->
	    {error, closed} = gen_tcp:recv(Socket, 0, 1000),
	    Ctrl ! {self(), closed}
    end.

otp_7816_recv(Socket, BytesLeft) ->
    otp_7816_recv(Socket, BytesLeft, 1).

otp_7816_recv(_, 0, _) ->
    ?P("[server] got all data"),
    ok;
otp_7816_recv(Socket, BytesLeft, N) ->
    ?P("[server] try recv ~w (~p bytes left)", [N, BytesLeft]),
    case gen_tcp:recv(Socket, 0, 1000) of
	{ok, Bin} when (byte_size(Bin) =< BytesLeft) andalso
		       (byte_size(Bin) > 0) -> 
	    ?P("[server] received ~p of ~p bytes",
	       [size(Bin), BytesLeft]),
	    otp_7816_recv(Socket, BytesLeft - byte_size(Bin), N+1);
	{ok, Bin} ->
	    ?P("[server] received unexpected data (~w): "
	       "~n   Expected:    ~p bytes"
	       "~n   Received:    ~p bytes"
	       "~n   Socket Info: ~p",
	       [N, BytesLeft, byte_size(Bin), inet:info(Socket)]),
	    {error, {unexpected_data, BytesLeft, byte_size(Bin)}};
	{error, timeout} ->
	    ?P("[server] got receive timeout when expecting more data:"
	       "~n      Socket Info: ~p", [inet:info(Socket)]),
	    {error, timeout}
    end.


%% ----------------------------------------------------------------------

%% Receive a packet with a faulty packet header.
otp_8102(Config) when is_list(Config) ->
    {ok, LSocket} = ?LISTEN(Config, 0, []),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    ?P("Listening on ~w with port number ~p", [LSocket, PortNum]),

    [otp_8102_do(Config, LSocket, PortNum, otp_8102_packet(Type,Size))
     || Size <- lists:seq(-10,-1), 
	Type <- [4, {cdr,big}, {cdr,little}]],
    
    gen_tcp:close(LSocket),
    ok.

otp_8102_packet(4, Size) ->
    {<<Size:32/big>>, 4};
otp_8102_packet({cdr,big}, Size) ->
    {<<"GIOP",0,0,0,0,Size:32/big>>, cdr};
otp_8102_packet({cdr,little}, Size) ->
    {<<"GIOP",0,0,1,0,Size:32/little>>, cdr}.

otp_8102_do(Config, LSocket, PortNum, {Bin,PType}) ->

    ?P("Connect with packet option ~p ...",[PType]),
    {ok, RSocket} = ?CONNECT(Config, "localhost", PortNum, [binary,
                                                            {packet,PType},
                                                            {active,true}]),
    {ok, SSocket} = gen_tcp:accept(LSocket),

    ?P("Got connection, sending ~p...",[Bin]),

    ok = gen_tcp:send(SSocket, Bin),

    ?P("Sending complete...",[]),

    {tcp_error,RSocket,emsgsize} = receive M -> M end,

    ?P("Got error msg, ok."),
    gen_tcp:close(SSocket),    
    gen_tcp:close(RSocket).

%% Verify packet_size handles long HTTP header lines.
otp_9389(Config) when is_list(Config) ->
    {ok, LS} = ?LISTEN(Config, 0, [{active,false}]),
    {ok, {_, PortNum}} = inet:sockname(LS),
    ?P("Listening on ~w with port number ~p", [LS, PortNum]),
    OrigLinkHdr = "/" ++ string:chars($S, 8192),
    _Server = spawn_link(
                fun() ->
                        {ok, S} = gen_tcp:accept(LS),
                        ok = inet:setopts(S, [{packet_size, 16384}]),
                        ok = otp_9389_loop(S, OrigLinkHdr),
                        ok = gen_tcp:close(S)
                end),
    {ok, S} = ?CONNECT(Config, "localhost", PortNum,
                                    [binary, {active, false}]),
    Req = "GET / HTTP/1.1\r\n"
        ++ "Host: localhost\r\n"
        ++ "Link: " ++ OrigLinkHdr ++ "\r\n\r\n",
    ok = gen_tcp:send(S, Req),
    ok = inet:setopts(S, [{packet, http}]),
    {ok, {http_response, {1,1}, 200, "OK"}} = gen_tcp:recv(S, 0),
    ok = inet:setopts(S, [{packet, httph}, {packet_size, 16384}]),
    {ok, {http_header, _, 'Content-Length', _, "0"}} = gen_tcp:recv(S, 0),
    {ok, {http_header, _, "Link", _, LinkHdr}} = gen_tcp:recv(S, 0),
    true = (LinkHdr == OrigLinkHdr),
    ok = gen_tcp:close(S),
    ok = gen_tcp:close(LS),
    ok.

otp_9389_loop(S, OrigLinkHdr) ->
    ok = inet:setopts(S, [{active,once},{packet,http}]),
    receive
        {http, S, {http_request, 'GET', _, _}} ->
            ok = otp_9389_loop(S, OrigLinkHdr, undefined)
    after
        3000 ->
            error({timeout,request_line})
    end.
otp_9389_loop(S, OrigLinkHdr, ok) ->
    Resp = "HTTP/1.1 200 OK\r\nContent-length: 0\r\n" ++
        "Link: " ++ OrigLinkHdr ++ "\r\n\r\n",
    ok = gen_tcp:send(S, Resp);
otp_9389_loop(S, OrigLinkHdr, State) ->
    ok = inet:setopts(S, [{active,once}, {packet,httph}]),
    receive
        {http, S, http_eoh} ->
            otp_9389_loop(S, OrigLinkHdr, ok);
        {http, S, {http_header, _, "Link", _, LinkHdr}} ->
            LinkHdr = OrigLinkHdr,
            otp_9389_loop(S, OrigLinkHdr, State);
        {http, S, {http_header, _, _Hdr, _, _Val}} ->
            otp_9389_loop(S, OrigLinkHdr, State);
        {http, S, {http_error, Err}} ->
            error({error, Err})
    after
        3000 ->
            error({timeout,header})
    end.

wrapping_oct() ->
    [{timetrap, {minutes,20}}].

%% Check that 64bit octet counters work.
wrapping_oct(Config) when is_list(Config) ->
    ?TC_TRY(wrapping_oct, fun() -> do_wrapping_oct(Config) end).

%% {recbuf, 8192}, {sndbuf, 8192}
do_wrapping_oct(Config) ->
    ?P("[ctrl] create listen socket"),
    Ctrl = self(),
    {ok, LSock} = ?LISTEN(Config, 0, [{active,false},{mode,binary}]),
    {ok, LPort} = inet:port(LSock),
    ?P("[ctrl] spawn acceptor"),
    Acceptor = spawn_link(fun() -> oct_acceptor(Ctrl, LSock) end),
    ?P("[ctrl] spawn pump"),
    Pump     = spawn_link(fun() -> 
                                  oct_datapump(Ctrl,
                                               Config, LPort, 16#10000FFFF)
                          end),
    ?P("[ctrl] await acceptor socket"),
    ASock = wc_await_socket("acceptor", Acceptor),
    ?P("[ctrl] await pump socket"),
    PSock = wc_await_socket("pump", Pump),
    ?P("[ctrl] await completion (from pump)"),
    Res = wc_await_completion(Acceptor, ASock, Pump, PSock),
    ?P("[ctrl] close listen socket"),
    gen_tcp:close(LSock),
    ?P("[ctrl] verify result"),
    ok = Res,
    ?P("[ctrl] done"),
    ok.

wc_await_socket(Tag, Pid) ->
    receive
        {Pid, socket, AS} ->
            ?P("received ~s socket: "
               "~n      ~p", [Tag, AS]),
            AS
    end.

wc_await_completion(Acceptor, ASock, Pump, PSock) ->
    wc_await_completion(Acceptor, ASock, Pump, PSock, 1, undefined).

wc_await_completion(undefined = _Acceptor, _ASock,
                    undefined = _Pump,     _PSock, 
                    Loops, Res) ->
    ?P("completed after ~w loops", [Loops]),
    Res;
wc_await_completion(Acceptor, ASock, Pump, PSock, Loops, CRes) ->
    receive
        {'EXIT', Pump, Res} ->
            ?P("[ctrl] Received pump exit: "
               "~n   Loops:  ~w"
               "~n   Reason: ~p", [Loops, Res]),
            wc_await_completion(Acceptor,  ASock,
                                undefined, PSock,
                                Loops + 1, Res);
        {'EXIT', Acceptor, Res} ->
            ?P("[ctrl] Received acceptor exit: "
               "~n   Loops:  ~w"
               "~n   Reason: ~p", [Loops, Res]),
            wc_await_completion(undefined, ASock,
                                Pump,      PSock,
                                Loops + 1, CRes)
                
    after 10000 ->
            ASockInfo = wc_sock_info(ASock),
            AccInfo   = wc_proc_info(Acceptor),
            PSockInfo = wc_sock_info(PSock),
            PumpInfo  = wc_proc_info(Pump),
            ?P("Info ~w while waiting for clompletion: "
               "~n   Acceptor Socket Info: ~p"
               "~n   Acceptor Info:        ~p"
               "~n   Pump Socket Info:     ~p"
               "~n   Pump Info:            ~p",
               [Loops, ASockInfo, AccInfo, PSockInfo, PumpInfo]),
            wc_await_completion(Acceptor,  ASock,
                                Pump,      PSock,
                                Loops + 1, CRes)
    end.
            
wc_sock_info(S) ->
    try inet:info(S)
    catch
        _:_:_ ->
            undefined
    end.

wc_proc_info(P) when is_pid(P) ->
    try erlang:process_info(P)
    catch
        _:_:_ ->
            undefined
    end;
wc_proc_info(_) ->
    undefined.

%% {recbuf, 16*1024}, {sndbuf, 16*1024}
oct_datapump(Ctrl, Config, Port, N) ->
    ?P("[pump] connect to listener"),
    {ok, CSock} = ?CONNECT(Config, "localhost", Port,
			   [{active, false}, {mode, binary}]),
    ?P("[pump] announce to ctrl"),
    Ctrl ! {self(), socket, CSock},
    {ok, [{sndbuf,SndBuf}]} = inet:getopts(CSock, [sndbuf]),
    ?P("[pump] connected - start 'pumping' with"
       "~n      SndBuf: ~w", [SndBuf]),
    put(action,  nothing),
    put(sent,    0),
    put(elapsed, 0),
    put(rem_bytes, N),
    oct_pump(CSock, N, binary:copy(<<$a:8>>,100000), 0, 0).

oct_pump(S, N, _, _, _Sent) when N =< 0 ->
    ?P("[pump] done"),
    (catch gen_tcp:close(S)),
    exit(ok);
oct_pump(S, N, Bin, Last, Sent) ->
    put(action, send),
    Start   = erlang:monotonic_time(nanosecond),
    Res     = gen_tcp:send(S, Bin),
    Stop    = erlang:monotonic_time(nanosecond),
    Elapsed = get(elapsed),
    put(elapsed, Elapsed + (Stop - Start)),
    put(action,  sent),
    put(sent,    Sent+1),
    case Res of
	ok ->
	    case inet:getstat(S) of
		{ok, Stat} ->
		    {_, R} = lists:keyfind(send_oct, 1, Stat),
		    case (R < Last) of
			true ->
			    ?P("[pump] send counter error ~p < ~p", [R, Last]),
                            (catch gen_tcp:close(S)),
			    exit({error, {output_counter, R, Last, N}});
			false ->
                            put(rem_bytes, N - byte_size(Bin)),
			    oct_pump(S, N-byte_size(Bin), Bin, R, Sent+1)
		    end;
		{error, StatReason} ->
		    ?P("[pump] get stat failed:"
		       "~n      Reason:    ~p"
		       "~n   when"
		       "~n      Remaining: ~p"
		       "~n      Last:      ~p", [StatReason, N, Last]),
		    (catch gen_tcp:close(S)),
		    exit({error, {stat_failure, StatReason, N, Last}})
	    end;
	{error, SendReason} ->
	    ?P("[pump] send failed:"
	       "~n      Reason:    ~p"
	       "~n   when"
	       "~n      Remaining: ~p"
	       "~n      Last:      ~p", [SendReason, N, Last]),
	    (catch gen_tcp:close(S)),
	    exit({error, {send_failure, SendReason, N, Last}})
    end.
    

oct_acceptor(Ctrl, LSock) ->
    ?P("[acceptor] accept connection"),
    {ok, ASock} = gen_tcp:accept(LSock),
    ?P("[acceptor] announce to ctrl"),
    Ctrl ! {self(), socket, ASock},
    {ok, [{recbuf,RecBuf}]} = inet:getopts(ASock, [recbuf]),
    ?P("[acceptor] connection accepted - start receiving with: "
       "~n      RecBuf: ~w", [RecBuf]),
    put(action,  nothing),
    put(recv,    0),
    put(elapsed, 0),
    oct_aloop(ASock, inet:info(ASock), 0, 0).

oct_aloop(S, LastInfo, Received, Times) ->
    put(action, recv),
    Start   = erlang:monotonic_time(),
    Res     = gen_tcp:recv(S, 0),
    Stop    = erlang:monotonic_time(),
    Elapsed = get(elapsed),
    put(elapsed, Elapsed + (Stop - Start)),
    put(action,  received),
    put(recv,    Times+1),
    case Res of
	{ok, _} ->
            #{counters := #{recv_oct := R} = _Stat} = Info = inet:info(S),
            case (R < Received) of
                true ->
                    ?P("[acceptor] recv counter error:"
                       "~n      Recv Cnt:  ~p"
                       "~n      Received:  ~p"
                       "~n      Times:     ~p"
                       "~n      Info:      ~p"
                       "~n      Last Info: ~p",
                       [R, Received, Times, Info, LastInfo]),
                    (catch gen_tcp:close(S)),
                    {error, {output_counter, R, Received, Times}};
                false ->
                    case Times rem 16#FFFFF of
                        0 ->
                            ?P("[acceptor] read: ~p"
                               "~n      Times: ~w"
                               "~n      Info:  ~p",
                               [R, Times, Info]);
                        _ ->
                            ok
                    end,
                    oct_aloop(S, Info, R, Times+1)
            end;

	{error, RecvReason} ->
	    ?P("[acceptor] receive failed:"
	       "~n      Reason:   ~p"
	       "~n   when"
	       "~n      Received: ~p"
	       "~n      Times:    ~p", [RecvReason, Received, Times]),
	    (catch gen_tcp:close(S)),
            ct:sleep(1000), % Just give the 'pump' a chance to get there first
	    exit(closed)
    end.

ok({ok,V}) -> V.

get_hostname(Name) ->
    "@"++Host = lists:dropwhile(fun(C) -> C =/= $@ end, atom_to_list(Name)),
    Host.

otp_13939(doc) ->
    ["Check that writing to a remotely closed socket doesn't block forever "
     "when exit_on_close is false."];
otp_13939(suite) ->
    [];
otp_13939(Config) when is_list(Config) ->
    {Pid, Ref} = spawn_opt(
        fun() ->
            {ok, Listener} = ?LISTEN(Config, 0, [{exit_on_close, false}]),
            {ok, Port} = inet:port(Listener),

            spawn_link(
                fun() ->
                    {ok, Client} = ?CONNECT(Config, "localhost", Port,
                        [{active, false}]),
                    ok = gen_tcp:close(Client)
                end),

            {ok, Accepted} = gen_tcp:accept(Listener),

            ok = gen_tcp:send(Accepted, <<0:(10*1024*1024*8)>>),

            %% The bug surfaces when there's a delay between the send
            %% operations; inet:getstat is a red herring.
            timer:sleep(100),

            {error, Code} = gen_tcp:send(Accepted, <<0:(10*1024*1024*8)>>),
            ct:pal("gen_tcp:send returned ~p~n", [Code])
        end, [link, monitor]),

    receive
        {'DOWN', Ref, process, Pid, normal} ->
            ok
    after 1000 ->
        demonitor(Ref, [flush]),
        exit(Pid, normal),
        ct:fail("Server process blocked on send.")
    end.


%% No point in running this as the test case with inet_backend = socket
%% as it tests a bug in the inet driver that cannot reproduced with
%% inet_backend = socket.
otp_12242(Config) when is_list(Config) ->
    Cond = fun() ->
		   case os:type() of
		       {win32,_} ->
			   %% Even if we set sndbuf and recbuf to small sizes
			   %% Windows either happily accepts to send GBytes of
			   %% data in no time, so the second send below that
			   %% is supposed to time out just succedes, or the 
			   %% first send that is supposed to fill the inet_drv
			   %% I/O queue and start waiting for when more data
			   %% can be sent instead sends all data but suffers
			   %% a send failure that closes the socket.
			   {skip, backpressure_broken_on_win32};
		       _ ->
			   case ?IS_SOCKET_BACKEND(Config) of
			       true ->
				   {skip, "Not complient with socket"};
			       false ->
				   ok
			   end
		   end
	   end,
    TC = fun() -> do_otp_12242(Config) end,
    ?TC_TRY(otp_12242, Cond, TC).

do_otp_12242(Config) when is_list(Config) ->
    %% Find the IPv4 address of an up and running interface
    %% that is not loopback nor pointtopoint
    {ok, IFList} = inet:getifaddrs(),
    ?P("IFList "
       "~n      ~p", [IFList]),
    case
	lists:flatten(
	  [lists:filtermap(
	     fun ({addr, Addr}) when (tuple_size(Addr) =:= 4) ->
		     {true, Addr};
		 (_) ->
		     false
	     end, Opts)
	   || {_,Opts} <- IFList,
	      case lists:keyfind(flags, 1, Opts) of
		  {_,Flags} ->
		      lists:member(up, Flags)
			  andalso
			  lists:member(running, Flags)
			  andalso
			  not lists:member(loopback, Flags)
			  andalso
			  not lists:member(pointtopoint, Flags);
		  false ->
		      false
	      end])
    of
	[Addr|_] ->
	    otp_12242(Config, Addr);
	Other ->
	    ?SKIPT({no_external_address, Other})
    end.

%%
otp_12242(Config, Addr) when (tuple_size(Addr) =:= 4) ->
    ct:timetrap(30000),
    ?P("Using address ~p", [Addr]),
    Bufsize  = 16 * 1024,
    Datasize = 128 * 1024 * 1024, % At least 1 s on GBit interface
    Blob     = binary:copy(<<$x>>, Datasize),
    LOpts =
        [{backlog,   4},
	 {reuseaddr, true},
	 {ip,        Addr},
         binary,
	 {active,    false},
         {recbuf,    Bufsize},
	 {sndbuf,    Bufsize},
	 {buffer,    Bufsize}],
    COpts =
        [binary,
	 {active,       false},
	 {ip,           Addr},
         {linger,       {true, 1}}, % 1 s
         {send_timeout, 500},
         {recbuf,       Bufsize},
	 {sndbuf,       Bufsize},
	 {buffer,       Bufsize}],
    Dir = filename:dirname(code:which(?MODULE)),
    {ok, ListenerNode} = ?START_SLAVE_NODE(?UNIQ_NODE_NAME, "-pa " ++ Dir),
    Tester = self(),
    ?P("create listener"),
    {Listener, ListenerMRef} =
        spawn_opt(
          ListenerNode,
          fun () ->
		  ?P("create listen socket"),
                  {ok,L}     = ?LISTEN(Config, 0, LOpts),
                  {ok,LPort} = inet:port(L),
		  ?P("inform tester about port number: ~w", [LPort]),
                  Tester ! {self(), port, LPort},
		  ?P("try accept"),
                  {ok, A}    = gen_tcp:accept(L),
		  ?P("accepted - close listen socket"),
                  ok         = gen_tcp:close(L),
		  ?P("await close order"),
                  receive
                      {Tester, stop} ->
			  ?P("close order received - close accepted socket"),
                          ok = gen_tcp:close(A)
                  end
          end, [monitor]),
    ?P("await listen port"),
    LPort = receive {Listener,port,P} -> P end,
    ?P("try connect to ~w", [LPort]),
    {ok,C} = ?CONNECT(Config, Addr, LPort, COpts, infinity),
    ?P("connected - get buffers"),
    {ok, ReadCOpts} = inet:getopts(C, [recbuf,sndbuf,buffer]),
    ?P("connected sockets buffers:"
       "~n      ~p", [ReadCOpts]),

    otp_12242_2(C, Blob, Datasize),

    ?P("send listener stop"),
    Listener ! {Tester, stop},
    ?P("await listener termination"),
    wait(ListenerMRef),
    ?P("stop listener node"),
    ?STOP_NODE(ListenerNode),
    ?P("done"),
    ok.
    

otp_12242_2(C, Blob, Datasize) when is_port(C) ->
    %% Fill the buffers
    ?P("sending ~p bytes", [Datasize]),
    ok = gen_tcp:send(C, Blob),
    ?P("sent ~p bytes", [Datasize]),

    %% Try to ensure that the close call is in progress
    %% before the owner proceeds with sending
    CloserMRef = otp_12242_closer(C),
    
    ?P("await tref"),
    receive
        {tref,Tref} ->
	    ?P("tref received - now await trigger timeout"),
            receive {timeout,Tref,_} -> ok end,
            ?P("trigger timeout received - try send ~p bytes again",
	       [Datasize]),
            %% Now should the close be in progress...
            %% All buffers are full, remote end is not reading,
            %% and the send timeout is 1 s so this will timeout:
            {error, timeout} = gen_tcp:send(C, Blob),
            ?P("expected timeout - update send_timeout (to 10000)"),
            ok = inet:setopts(C, [{send_timeout, 10000}]),
            %% There is a hidden timeout here.  Port close is sampled
            %% every 5 s by prim_inet:send_recv_reply.
            %% Linger is 3 s so the Closer will finish this send:
            ?P("try send ~p bytes - expect error (closed)", [Datasize]),
            {error, closed} = gen_tcp:send(C, Blob),
            ?P("await closer termination"),
            normal = wait(CloserMRef),
            ok
    end.

otp_12242_closer(C) ->
    Owner = self(),
    {_Closer, CloserMref} =
        spawn_opt(
          fun () ->
                  ?P("[closer] starting"),
                  Owner ! {tref, erlang:start_timer(50, Owner, closing)},
                  ?P("[closer] calling gen_tcp:close(C)"),
                  try gen_tcp:close(C) of
                      Result ->
                          ?P("[closer] gen_tcp:close(C) -> ~p", [Result]),
                          ok = Result
                  catch
                      Class:Reason:Stacktrace ->
                          ?P("[closer] catched gen_tcp:close(C):"
			     "~n      Error Class:  ~p"
			     "~n      Error:        ~p"
			     "~n      Stack trace:  ~p",
                            [Class, Reason, Stacktrace]),
                          erlang:raise(Class, Reason, Stacktrace)
                  end
          end, [link, monitor]),
    CloserMref.


wait(Mref) ->
    receive {'DOWN',Mref,_,_,Reason} -> Reason end.

%% OTP-15536
%% Test that send error works correctly for delay_send
delay_send_error(Config) ->
    ?P("create listen socket"),
    {ok, L} =
        ?LISTEN(Config,
          0, [{reuseaddr, true}, {packet, 1}, {active, false}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(L),
    ?P("try connect - with delay_send:true"),
    {ok, C} =
        ?CONNECT(Config,
          "localhost", PortNum,
          [{packet, 1}, {active, false}, {delay_send, true}]),
    ?P("try accept"),
    {ok, S} = gen_tcp:accept(L),
    %% Do a couple of sends first to see that it works
    ?P("send data"),
    ok = gen_tcp:send(C, "hello"),
    ?P("send data"),
    ok = gen_tcp:send(C, "hello"),
    ?P("send data"),
    ok = gen_tcp:send(C, "hello"),
    %% Close the receiver
    ?P("close receiver (accepted socket)"),
    ok = gen_tcp:close(S),
    %%
    ?P("send data"),
    case gen_tcp:send(C, "hello") of
        ok ->
            ?P("send data"),
            case gen_tcp:send(C, "hello") of
                ok ->
                    delay_send_error2(C);
                {error, closed} ->
                    ?P("closed (expected)"),
                    ok
            end;
        {error, closed} ->
                    ?P("closed (expected)"),
            ok
    end,
    ok = gen_tcp:close(C).


delay_send_error2(Sock) ->
    delay_send_error2(Sock, 3).

delay_send_error2(Sock, 0) ->
    gen_tcp:close(Sock),
    ct:fail("Unxpected send success");
delay_send_error2(Sock, N) ->
    %% Sleep in order for delay_send to have time to trigger
    %% This used to result in a double free
    timer:sleep(1000),
    case gen_tcp:send(Sock, "hello") of
        ok ->
            delay_send_error2(Sock, N-1);
        {error, closed} ->
            ?P("closed (expected, ~w)", [N]),
            ok;
        {error, Reason} ->
            ct:fail(?F("Unexpected send error: ~p", [Reason]))
    end.

    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-define(ACTIVE_N, 20).

%% 30-second test for gen_tcp in {active, N} mode,
%% ensuring it does not get stuck.
%% Verifies that erl_check_io properly handles extra EPOLLIN signals.
bidirectional_traffic(Config) when is_list(Config) ->
    ?TC_TRY(bidirectional_traffic,
            fun() -> do_bidirectional_traffic(Config) end).

do_bidirectional_traffic(Config) ->
    ?P("begin"),
    Workers = erlang:system_info(schedulers_online) * 2,
    ?P("Use ~w workers", [Workers]),
    Payload = crypto:strong_rand_bytes(32),
    {ok, LSock} = ?LISTEN(Config,
			  0,
			  [binary,
			   {packet, 0},
			   {active, false},
			   {reuseaddr, true}]),
    %% get all sockets to know failing ends
    ?P("listen socket created: "
       "~n      ~p", [LSock]),
    {ok, Port} = inet:port(LSock),
    ?P("listen socket port number ~w", [Port]),
    Control = self(),
    ?P("create ~w receivers", [Workers]),
    Receivers = [spawn_link(fun () -> 
				    exchange(Config, LSock, Port, Payload, Control)
			    end) || _ <- lists:seq(1, Workers)],
    ?P("await the result"),
    Result =
	%% If any of the receivers report, we have an error
        receive
            {timeout, Socket, Total} ->
                ?P("timeout msg for ~p: ~w", [Socket, Total]),
                {fail, {timeout, Socket, Total}};
	    {error, Socket, Reason} ->
                ?P("error msg for ~p: ~p", [Socket, Reason]),
                {fail, {error, Socket, Reason}}
        after 30000 ->
                %% if it does not fail in 30 seconds, it most likely works
                ?P("timeout => success"),
                ok
        end,
    ?P("ensure all receivers terminated"),
    [begin unlink(Rec), exit(Rec, kill) end || Rec <- Receivers],
    ?P("close listen socket"),
    (catch gen_tcp:close(LSock)),
    ?P("done"),
    Result.

    
exchange(Config, LSock, Port, Payload, Control) ->
    %% spin up client
    _ClntRcv = spawn_link(
        fun () ->
                ?P("connect"),
                {ok, Client} =
                    ?CONNECT(Config,
			     "localhost",
			     Port,
			     [binary, {packet, 0}, {active, ?ACTIVE_N}]),
                ?P("connected: ~p", [Client]),
                send_recv_loop(Client, Payload, Control)
        end),
    ?P("accept"),
    {ok, Socket} = gen_tcp:accept(LSock),
    ?P("accepted: ~p", [Socket]),
    %% sending process
    send_recv_loop(Socket, Payload, Control).

send_recv_loop(Socket, Payload, Control) ->
    %% {active, N} must be set to active > 12 to trigger the issue
    %% {active, 30} seems to trigger it quite often & reliably
    ?P("set (initial) active: ~p", [?ACTIVE_N]),
    inet:setopts(Socket, [{active, ?ACTIVE_N}]),
    ?P("spawn sender"),
    _Snd = spawn_link(
        fun Sender() ->
            case gen_tcp:send(Socket, Payload) of
                ok ->
                    Sender();
                {error, Reason} ->
                    ?P("Send failed: "
                       "~n      ~p", [Reason]),
                    exit({send_failed, Reason})
            end
        end),
    ?P("begin recv"),
    recv(Socket, 0, Control).

recv(Socket, Total, Control) ->
    receive
        {tcp, Socket, Data} ->
            recv(Socket, Total + byte_size(Data), Control);
        {tcp_passive, Socket} ->
            inet:setopts(Socket, [{active, ?ACTIVE_N}]),
            recv(Socket, Total, Control);
        {tcp_closed, Socket} ->
            ?P("[recv] closed when total received: ~w"
               "~n      Socket Info: ~p",
	       [Total, (catch inet:info(Socket))]),
	    ok;
        Other ->
            ?P("[recv] received unexpected when total received: ~w"
               "~n      ~p"
               "~n      Socket:      ~p"
               "~n      Socket Info: ~p",
               [Total, Other, Socket, (catch inet:info(Socket))]),
            Control ! {error, Socket, Other}
    after 2000 ->
            %% no data received in 2 seconds => test failed
            ?P("[recv] received nothing when total received: ~w"
               "~n      Socket:      ~p"
               "~n      Socket Info: ~p",
               [Total, Socket, (catch inet:info(Socket))]),
            Control ! {timeout, Socket, Total}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of tests.
%% We create a listen socket, then spawns processes that create
%% monitors to it...
socket_monitor1(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1,
            fun() -> do_socket_monitor1(Config) end).

do_socket_monitor1(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, LSock1} = ?LISTEN(Config),
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Socket, Parent) when is_pid(Parent) ->
		 ?P("[client] create monitor"),
		 MRef = inet:monitor(Socket),
		 Parent ! {self(), ready},
		 sm_await_socket_down(MRef, Socket, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F(LSock1, F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket"),
    gen_tcp:close(LSock1),
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
%% We create "many" listen socket(s), then spawns processes that create
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
    {ok, LSock1} = ?LISTEN(Config),
    {ok, LSock2} = ?LISTEN(Config),
    {ok, LSock3} = ?LISTEN(Config),
    {ok, LSock4} = ?LISTEN(Config),
    {ok, LSock5} = ?LISTEN(Config),
    F  = fun(S, Fun) -> spawn_monitor(fun() -> Fun(S, Self) end) end,
    F1 = fun(Sockets, Parent) when is_list(Sockets) andalso is_pid(Parent) ->
		 ?P("[client] create monitor(s)"),
		 Monitors = [{inet:monitor(Socket), Socket} ||
				Socket <- Sockets],
		 Parent ! {self(), ready},
		 sm_await_socket_down2(Monitors, Type)
	 end,
    ?P("spawn client"),
    {Pid1, Mon1} = F([LSock1, LSock2, LSock3, LSock4, LSock5], F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket(s)"),
    gen_tcp:close(LSock1),
    gen_tcp:close(LSock2),
    gen_tcp:close(LSock3),
    gen_tcp:close(LSock4),
    gen_tcp:close(LSock5),
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
%% We create a listen socket, then spawn client process(es) that create
%% monitors to it...
socket_monitor1_manyc(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1_manyc,
            fun() -> do_socket_monitor1_manyc(Config) end).

do_socket_monitor1_manyc(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, LSock1} = ?LISTEN(Config),
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
    {Pid1, Mon1} = F(LSock1, F1, "client1"),
    {Pid2, Mon2} = F(LSock1, F1, "client2"),
    {Pid3, Mon3} = F(LSock1, F1, "client3"),
    {Pid4, Mon4} = F(LSock1, F1, "client4"),
    {Pid5, Mon5} = F(LSock1, F1, "client5"),
    ?P("await client(s) ready"),
    sm_await_client_ready(Pid1, "client1"),
    sm_await_client_ready(Pid2, "client2"),
    sm_await_client_ready(Pid3, "client3"),
    sm_await_client_ready(Pid4, "client4"),
    sm_await_client_ready(Pid5, "client5"),
    ?P("close socket"),
    gen_tcp:close(LSock1),
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
%% We create a listen socket, then spawns processes that create
%% monitors to it...
socket_monitor1_demon_after(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(socket_monitor1_demon_after,
            fun() -> do_socket_monitor1_demon_after(Config) end).

do_socket_monitor1_demon_after(Config) ->
    ?P("begin"),
    Self         = self(),
    Type         = ?SOCKET_TYPE(Config),
    {ok, LSock1} = ?LISTEN(Config),
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
    {Pid1, Mon1} = F(LSock1, F1),
    ?P("await client ready"),
    sm_await_client_ready(Pid1),
    ?P("close socket"),
    gen_tcp:close(LSock1),
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
%% Spawn a process that creates a (listen) socket, then spawns processes
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
			      {ok, L} = ?LISTEN(Config),
			      ?P("[owner] send (listen) socket to ctrl"),
			      Self ! {socket, L},
			      ?P("[owner] ready"),
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    LSock1 = receive
		 {socket, L} ->
		     ?P("received socket from owner"),
		     L;
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
    {Pid1, Mon1} = F(LSock1, F1),
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
%% Spawn a process that creates "many" (listen) socket(s), then spawns
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
			      {ok, L1} = ?LISTEN(Config),
			      {ok, L2} = ?LISTEN(Config),
			      {ok, L3} = ?LISTEN(Config),
			      {ok, L4} = ?LISTEN(Config),
			      {ok, L5} = ?LISTEN(Config),
			      ?P("[owner] send (listen) socket(s) to ctrl"),
			      Self ! {socket, [L1, L2, L3, L4, L5]},
			      ?P("[owner] ready"),
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    ?P("await sockets (from owner)"),
    LSocks = receive
		 {socket, Socks} ->
		     ?P("received socket(s) from owner"),
		     Socks;
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
    {Pid1, Mon1} = F(LSocks, F1),
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
%% Spawn a process that creates a (listen) socket, then spawns (client)
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
			      {ok, L} = ?LISTEN(Config),
			      Self ! {socket, L},
			      receive
				  {Self, die} ->
				      exit(normal)
			      end
		      end),
    LSock1 = receive
		 {socket, L} ->
		     ?P("received socket from owner"),
		     L;
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
    {Pid1, Mon1} = F(LSock1, F1, "client1"),
    {Pid2, Mon2} = F(LSock1, F1, "client2"),
    {Pid3, Mon3} = F(LSock1, F1, "client3"),
    {Pid4, Mon4} = F(LSock1, F1, "client4"),
    {Pid5, Mon5} = F(LSock1, F1, "client5"),
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
%% Spawn a process that creates a (listen) socket, then spawns (client)
%% processes that create monitors to it...
otp_17492(Config) when is_list(Config) ->
    ct:timetrap(?MINS(1)),
    ?TC_TRY(otp_17492, fun() -> do_otp_17492(Config) end).

do_otp_17492(Config) ->
    ?P("begin"),

    Self = self(),

    ?P("try create listen socket"),
    {ok, L} = ?LISTEN(Config, 0, []),

    ?P("try get (created) listen socket info"),
    try inet:info(L) of
	#{owner := Owner} = Info when is_pid(Owner) andalso (Owner =:= Self) ->
	    ?P("(created) Listen socket info: ~p", [Info]);
	OBadInfo ->
	    ?P("(created) listen socket info: ~p", [OBadInfo]),
	    (catch gen_tcp:close(L)),
	    ct:fail({invalid_created_info, OBadInfo})
    catch
	OC:OE:OS ->
	    ?P("Failed get (created) listen socket info: "
	       "~n   Class: ~p"
	       "~n   Error: ~p"
	       "~n   Stack: ~p", [OC, OE, OS]),
	    (catch gen_tcp:close(L)),
	    ct:fail({unexpected_created_info_result, {OC, OE, OS}})
    end,

    ?P("try close (listen) socket"),
    ok = gen_tcp:close(L),

    ?P("try get (closed) listen socket info"),
    try inet:info(L) of
	#{states := [closed]} = CInfo when is_port(L) ->
	    ?P("(closed) listen socket info: "
	       "~n   ~p", [CInfo]);
	#{rstates := [closed], wstates := [closed]} = CInfo ->
	    ?P("(closed) listen socket info: "
	       "~n   ~p", [CInfo]);
	CBadInfo ->
	    ?P("(closed) listen socket info: ~p", [CBadInfo]),
	    ct:fail({invalid_closed_info, CBadInfo})
    catch
	CC:CE:CS ->
	    ?P("Failed get (closed) listen socket info: "
	       "~n   Class: ~p"
	       "~n   Error: ~p"
	       "~n   Stack: ~p", [CC, CE, CS]),
	    (catch gen_tcp:close(L)),
	    ct:fail({unexpected_closed_info_result, {CC, CE, CS}})
    end,

    ?P("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send_failed_str(Reason) ->
    ?F("Send failed: ~w", [Reason]).

connect_failed_str(Reason) ->
    ?F("Connect failed: ~w", [Reason]).

listen_failed_str(Reason) ->
    ?F("Listen failed: ~w", [Reason]).

accept_failed_str(Reason) ->
    ?F("Accept failed: ~w", [Reason]).

port_failed_str(Reason) ->
    ?F("Port failed: ~w", [Reason]).
