%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2020. All Rights Reserved.
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

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 controlling_process/1, controlling_process_self/1,
	 no_accept/1, close_with_pending_output/1, active_n/1,
         active_n_closed/1,
	 data_before_close/1,
	 iter_max_socks/0, iter_max_socks/1,
	 get_status/1,
	 passive_sockets/1, accept_closed_by_other_process/1,
	 init_per_testcase/2, end_per_testcase/2,
	 otp_3924/1, otp_3924_sender/4, closed_socket/1,
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
	 killing_acceptor/1,killing_multi_acceptors/1,killing_multi_acceptors2/1,
	 several_accepts_in_one_go/1, accept_system_limit/1,
	 active_once_closed/1, send_timeout/1, send_timeout_active/1,
         otp_7731/1, zombie_sockets/1, otp_7816/1, otp_8102/1,
         wrapping_oct/0, wrapping_oct/1, otp_9389/1, otp_13939/1,
         otp_12242/1, delay_send_error/1]).

%% Internal exports.
-export([sender/3, not_owner/1, passive_sockets_server/2, priority_server/1, 
	 oct_acceptor/1,
	 otp_7731_server/1, zombie_server/2, do_iter_max_socks/2]).

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,4}}].

all() -> 
    [controlling_process, controlling_process_self, no_accept,
     close_with_pending_output, data_before_close,
     iter_max_socks, passive_sockets, active_n, active_n_closed,
     accept_closed_by_other_process, otp_3924, closed_socket,
     shutdown_active, shutdown_passive, shutdown_pending,
     show_econnreset_active, show_econnreset_active_once,
     show_econnreset_passive, econnreset_after_sync_send,
     econnreset_after_async_send_active,
     econnreset_after_async_send_active_once,
     econnreset_after_async_send_passive,
     linger_zero, linger_zero_sndbuf,
     default_options, http_bad_packet, busy_send,
     busy_disconnect_passive, busy_disconnect_active,
     fill_sendq, partial_recv_and_close,
     partial_recv_and_close_2, partial_recv_and_close_3,
     so_priority, recvtos, recvttl, recvtosttl,
     recvtclass, primitive_accept,
     multi_accept_close_listen, accept_timeout,
     accept_timeouts_in_order, accept_timeouts_in_order2,
     accept_timeouts_in_order3, accept_timeouts_in_order4,
     accept_timeouts_in_order5, accept_timeouts_in_order6,
     accept_timeouts_in_order7, accept_timeouts_mixed,
     killing_acceptor, killing_multi_acceptors,
     killing_multi_acceptors2, several_accepts_in_one_go, accept_system_limit,
     active_once_closed, send_timeout, send_timeout_active, otp_7731,
     wrapping_oct,
     zombie_sockets, otp_7816, otp_8102, otp_9389,
     otp_12242, delay_send_error].

groups() -> 
    [].

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
            
            Config1
    end.

end_per_suite(Config0) ->

    ?P("end_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    Config1 = ?LIB:end_per_suite(Config0),

    ?P("end_per_suite -> "
            "~n      Nodes: ~p", [erlang:nodes()]),

    Config1.

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

do_default_options(_Config) ->
    %% First check the delay_send option
    {true,true,true}=do_delay_send_1(),
    {false,false,false}=do_delay_send_2(),
    {true,false,false}=do_delay_send_3(),
    {false,false,false}=do_delay_send_4(),
    {false,false,false}=do_delay_send_5(),
    {false,true,true}=do_delay_send_6(),
    %% Now lets start some nodes with different combinations of options:
    {true,true,true} = do_delay_on_other_node("", fun do_delay_send_1/0),
    {true,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_2/0),
    
    {false,true,true} =
	do_delay_on_other_node("-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_2/0),
    
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_3/0),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_6/0),
    {false,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_5/0),
    {false,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_5/0),
    {true,false,false} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{delay_send,true}]\"",
			       fun do_delay_send_4/0),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"{delay_send,true}\" "
			       "-kernel inet_default_listen_options "
			       "\"{delay_send,true}\"",
			       fun do_delay_send_2/0),
    %% Active is to dangerous and is supressed
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"{active,false}\" "
			       "-kernel inet_default_listen_options "
			       "\"{active,false}\"",
			       fun do_delay_send_7/0),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{active,false},{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{active,false},{delay_send,true}]\"",
			       fun do_delay_send_7/0),
    {true,true,true} =
	do_delay_on_other_node("-kernel inet_default_connect_options "
			       "\"[{active,false},{delay_send,true}]\" "
			       "-kernel inet_default_listen_options "
			       "\"[{active,false},{delay_send,true}]\"",
			       fun do_delay_send_2/0),
    ok.


do_delay_on_other_node(XArgs, Function) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(?UNIQ_NODE_NAME, slave,
                                       [{args,"-pa " ++ Dir ++ " " ++ XArgs}]),
    Res = rpc:call(Node,erlang,apply,[Function,[]]),
    test_server:stop_node(Node),
    Res.

do_delay_send_1() ->
    {ok,LS}=gen_tcp:listen(0,[{delay_send,true}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    S = case gen_tcp:connect("localhost",PortNum,[{delay_send,true}]) of
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

do_delay_send_2() ->
    {ok,LS}=gen_tcp:listen(0,[]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.

do_delay_send_3() ->
    {ok,LS}=gen_tcp:listen(0,[]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[{delay_send,true}]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_4() ->
    {ok,LS}=gen_tcp:listen(0,[{delay_send,false}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_5() ->
    {ok,LS}=gen_tcp:listen(0,[]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[{delay_send,false}]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_6() ->
    {ok,LS}=gen_tcp:listen(0,[{delay_send,true}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[]),
    {ok,S2}= gen_tcp:accept(LS),
    {ok,[{delay_send,B1}]}=inet:getopts(S,[delay_send]),
    {ok,[{delay_send,B2}]}=inet:getopts(LS,[delay_send]),
    {ok,[{delay_send,B3}]}=inet:getopts(S2,[delay_send]),
    gen_tcp:close(S2),
    gen_tcp:close(S),
    gen_tcp:close(LS),
    {B1,B2,B3}.
    
do_delay_send_7() ->
    {ok,LS}=gen_tcp:listen(0,[]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(LS),
    {ok,S}=gen_tcp:connect("localhost",PortNum,[]),
    {ok,S2}= gen_tcp:accept(LS),
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
    {ok,S} = gen_tcp:listen(0,[]),
    Pid2 = spawn(?MODULE,not_owner,[S]),
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
		       {ok,Sock} = gen_tcp:listen(0,[]),
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
    {ok, L} = gen_tcp:listen(0, []),
    {ok, {_, Port}} = inet:sockname(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, []),
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
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, {_, Port}} = inet:sockname(L),
    Packets = 16,
    Total = 2048*Packets,
    case start_remote(close_pending) of
	{ok, Node} ->
	    {ok, Host} = inet:gethostname(),
	    spawn_link(Node, ?MODULE, sender, [Port, Packets, Host]),
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

sender(Port, Packets, Host) ->
    X256 = lists:seq(0, 255),
    X512 = [X256|X256],
    X1K = [X512|X512],
    Bin = list_to_binary([X1K|X1K]),
    {ok, Sock} = gen_tcp:connect(Host, Port, []),
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

do_active_n(_Config) ->
    N = 3,
    LS = ok(gen_tcp:listen(0, [{active,N}])),
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
    C = case gen_tcp:connect("localhost", Port, [{active,N}]) of
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
    LS2 = ok(gen_tcp:listen(0, [{active,0}])),
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
    otp_3924_1(MaxDelay).

otp_3924_1(MaxDelay) ->
    {ok, Node} = start_node(otp_3924),
    DataLen = 100*1024,
    Data = otp_3924_data(DataLen),
    %% Repeat the test a couple of times to prevent the test from passing
    %% by chance.
    repeat(10, fun(N) ->
                       ok = otp_3924(MaxDelay, Node, Data, DataLen, N)
               end),
    test_server:stop_node(Node),
    ok.

otp_3924(MaxDelay, Node, Data, DataLen, N) ->
    {ok, L} = gen_tcp:listen(0, [list, {active, false}]),
    {ok, {_, Port}} = inet:sockname(L),
    {ok, Host} = inet:gethostname(),
    Sender = spawn_link(Node,
                        ?MODULE,
                        otp_3924_sender,
                        [self(), Host, Port, Data]),
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

otp_3924_sender(Receiver, Host, Port, Data) ->
    receive
	start ->
	    {ok, Sock} = gen_tcp:connect(Host, Port, [list]),
	    gen_tcp:send(Sock, Data),
	    ok = gen_tcp:close(Sock),
	    unlink(Receiver)
    end.


%% Tests that a huge amount of data can be received before a close.
data_before_close(Config) when is_list(Config) ->
    {ok, L} = gen_tcp:listen(0, [binary]),
    {ok, {_, TcpPort}} = inet:sockname(L),
    Bytes = 256*1024,
    spawn_link(fun() -> huge_sender(TcpPort, Bytes) end),
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

huge_sender(TcpPort, Bytes) ->
    {ok, Client} = gen_tcp:connect(localhost, TcpPort, []),
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
    {ok,{socket,Pid,_,_}} = gen_tcp:listen(5678,[]),
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
    {ok, Node} = test_server:start_node(test_iter_max_socks,slave,
                                        [{args,"+Q 2048 -pa " ++ Dir}]),
    %% L = rpc:call(Node,?MODULE,do_iter_max_socks,[N, initalize]),
    L = iter_max_socks_run(Node,
                           fun() ->
                                   exit(do_iter_max_socks(Tries, initalize))
                           end),
    test_server:stop_node(Node),

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
            
             
do_iter_max_socks(0, _) ->
    ?P("do_iter_max_socks(0,-) -> done"),
    [];
do_iter_max_socks(N, initalize = First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(),
    [MS|do_iter_max_socks(N-1, MS)];
do_iter_max_socks(N, failed = First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(),
    [MS|do_iter_max_socks(N-1, failed)];
do_iter_max_socks(N, First) when is_integer(First) ->
    ?P("do_iter_max_socks(~w,~w) -> entry", [N, First]),
    MS = max_socks(),
    if
        (MS =:= First) -> 
	    [MS|do_iter_max_socks(N-1, First)];
       true ->
	    ?P("~w =/= ~w => sleeping for ~p seconds...",
               [MS, First, ?RETRY_SLEEP/1000]), 
	    ct:sleep(?RETRY_SLEEP),
	    ?P("Trying again...", []),
	    RetryMS = max_socks(),
	    if RetryMS == First ->
			  [RetryMS|do_iter_max_socks(N-1, First)];
		     true ->
			  [RetryMS|do_iter_max_socks(N-1, failed)]
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
                                    %% recover so other tests won't be
                                    %% affected.
    ct:fail(max_socket_mismatch);
all_equal(_Rule, []) ->
    ok.

max_socks() ->
    Socks = open_socks(),
    N = length(Socks),
    lists:foreach(fun(S) -> ok = gen_tcp:close(S) end, Socks),
    ?P("Got ~p sockets", [N]),
    N.

open_socks() ->
    case gen_tcp:listen(0, []) of
	{ok, L} ->
	    {ok, {_, Port}} = inet:sockname(L),
	    [L| connect_accept(L, Port)];
	_ ->
	    []
    end.

connect_accept(L, Port) ->
    case gen_tcp:connect(localhost, Port, []) of
	{ok, C} ->
	    [C| do_accept(L, Port)];
	_ ->
	    []
    end.

do_accept(L, Port) ->
    case gen_tcp:accept(L) of
	{ok, A} -> [A| connect_accept(L, Port)];
	_ -> []
    end.

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{args, "-pa " ++ Pa}]).

start_remote(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    test_server:start_node(Name, slave, [{remote, true}, {args, "-pa " ++ Pa}]).

%% Tests that when 'the other side' on a passive socket closes, the
%% connecting side can still read until the end of data.
passive_sockets(Config) when is_list(Config) ->
    spawn_link(?MODULE, passive_sockets_server, [[{active, false}], self()]),
    receive
        {socket,Port} -> ok
    end,
    ct:sleep(500),
    case gen_tcp:connect("localhost", Port, [{active, false}]) of
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

passive_sockets_server(Opts, Parent) ->
    case gen_tcp:listen(0, Opts) of
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
    {ok, ListenSocket} = gen_tcp:listen(0, []),
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
    {ok, LS1} = gen_tcp:listen(0, []),
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
    ok = io:format("R_send = ~p~n", [R_send]),
    ok = io:format("R_recv = ~p~n", [R_recv]),
    ok = io:format("R_accept = ~p~n", [R_accept]),
    ok = io:format("R_controlling_process = ~p~n", [R_controlling_process]),
    ok.

%%%
%%% Test using the gen_tcp:shutdown/2 function using a sort server.
%%% 

shutdown_active(Config) when is_list(Config) ->
    ?TC_TRY(shutdown_active, fun() -> shutdown_common(true) end).

shutdown_passive(Config) when is_list(Config) ->
    ?TC_TRY(shutdown_passive, fun() -> shutdown_common(false) end).

shutdown_common(Active) ->
    ?P("start sort server"),
    P = sort_server(Active),
    ?P("Sort server port: ~p", [P]),


    do_sort(P, []),
    do_sort(P, ["glurf"]),
    do_sort(P, ["abc","nisse","dum"]),

    do_sort(P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 255)]),
    do_sort(P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(77, 999)]),
    do_sort(P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 55)]),
    do_sort(P, []),
    do_sort(P, ["apa"]),
    do_sort(P, ["kluns","gorilla"]),
    do_sort(P, [lists:reverse(integer_to_list(I)) || I <- lists:seq(25, 1233)]),
    do_sort(P, []),
    receive
	Any ->
	    ct:fail({unexpected_message,Any})
    after 0 -> ok
    end.

do_sort(P, List0) ->
    ?P("Sort: "
       "~n   ~p", [List0]),
    List = [El++"\n" || El <- List0],
    S = case gen_tcp:connect(localhost, P, [{packet,line}]) of
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

sort_server(Active) ->
    Opts = [{exit_on_close,false},{packet,line},{active,Active}],
    {ok,L} = gen_tcp:listen(0, Opts),
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

do_shutdown_pending(_Config) ->
    N = 512*1024+17,
    ?P("N: ~p", [N]),
    Data = [<<N:32>>,ones(N),42],
    {Port, Pid} = a_server(),
    ?P("try connect to server (port: ~p)", [Port]),
    S = case gen_tcp:connect(localhost, Port, []) of
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

 a_server() ->
     {ok, L}    = gen_tcp:listen(0, [{exit_on_close,false},{active,false}]),
     Pid        = spawn_link(fun() -> a_server(L) end),
     ok         = gen_tcp:controlling_process(L, Pid),
     {ok, Port} = inet:port(L),
     {Port, Pid}.

 a_server(L) ->
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

do_show_econnreset_active(_Config) ->
    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L0} = gen_tcp:listen(0, []),
    {ok, Port0} = inet:port(L0),
    Client0 = case gen_tcp:connect(localhost, Port0, [{active, false}]) of
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
    {ok, L1} = gen_tcp:listen(0, [{show_econnreset, true}]),
    {ok, Port1} = inet:port(L1),
    Client1 = case gen_tcp:connect(localhost, Port1, [{active, false}]) of
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

do_show_econnreset_active_once(_Config) ->
    %% Now test using {active, once}
    {ok, L} = gen_tcp:listen(0,
			   [{active, false},
			    {show_econnreset, true}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port, [{active, false}]) of
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

do_show_econnreset_passive(_Config) ->
    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port, [{active, false}]) of
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
    {ok, L1} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    Client1 =
        case gen_tcp:connect(localhost, Port1,
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

do_econnreset_after_sync_send(_Config) ->
    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port, [{active, false}]) of
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
    {ok, L1} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    Client1 =
        case gen_tcp:connect(localhost, Port1,
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

do_econnreset_after_async_send_active(_Config) ->
    {OS, _} = os:type(),
    Payload = lists:duplicate(1024 * 1024, $.),

    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port, [{sndbuf, 4096}]) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = gen_tcp:send(Client, Payload),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ok = gen_tcp:send(S, "Whatever"),
    ok = ct:sleep(20),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(20),
    receive
	{tcp, Client, "Whatever"} ->
	    receive
		{tcp_closed, Client} ->
		    ok;
		Other1 ->
		    ct:fail({unexpected1, Other1})
	    end;
	Other2 ->
	    ct:fail({unexpected2, Other2})
    end,

    %% Now test with option switched on.
    ?P("test with option explicitly switched on"),
    {ok, L1} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port1} = inet:port(L1),
    Client1 =
        case gen_tcp:connect(localhost, Port1,
                             [{sndbuf, 4096}, {show_econnreset, true}]) of
            {ok, CSock1} ->
                CSock1;
            {error, eaddrnotavail = Reason1} ->
                ?SKIPT(connect_failed_str(Reason1))
        end,
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = gen_tcp:send(Client1, Payload),
    case erlang:port_info(Client1, queue_size) of
	{queue_size, N1} when N1 > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T1 -> ct:fail(T1)
    end,
    ok = gen_tcp:send(S1, "Whatever"),
    ok = ct:sleep(20),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(20),
    receive
	{tcp, Client1, "Whatever"} ->
	    receive
		{tcp_error, Client1, econnreset} ->
		    receive
			{tcp_closed, Client1} ->
                            ?P("done"),
			    ok;
			Other3 ->
			    ct:fail({unexpected3, Other3})
		    end;
		Other4 ->
		    ct:fail({unexpected4, Other4})
	    end;
	Other5 ->
	    ct:fail({unexpected5, Other5})
    end.

econnreset_after_async_send_active_once(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_async_send_active_once,
            fun() -> do_econnreset_after_async_send_active_once(Config) end).

do_econnreset_after_async_send_active_once(_Config) ->
    {OS, _} = os:type(),
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port,
                                  [{active, false},
                                   {sndbuf, 4096},
                                   {show_econnreset, true}]) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,            
    {ok,S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    Payload = lists:duplicate(1024 * 1024, $.),
    ok = gen_tcp:send(Client, Payload),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ok = gen_tcp:send(S, "Whatever"),
    ok = ct:sleep(20),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(20),
    ok = receive Msg -> {unexpected_msg, Msg} after 0 -> ok end,
    ok = inet:setopts(Client, [{active, once}]),
    receive
	{tcp_error, Client, econnreset} ->
	    receive
		{tcp_closed, Client} ->
		    ok;
		Other ->
		    ct:fail({unexpected1, Other})
	    end;
	Other ->
	    ct:fail({unexpected2, Other})
    end.

econnreset_after_async_send_passive(Config) when is_list(Config) ->
    ?TC_TRY(econnreset_after_async_send_passive,
            fun() -> do_econnreset_after_async_send_passive(Config) end).

do_econnreset_after_async_send_passive(_Config) ->
    {OS, _} = os:type(),
    Payload = lists:duplicate(1024 * 1024, $.),

    %% First confirm everything works with option turned off.
    ?P("test with option switched off (default)"),
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    Client = case gen_tcp:connect(localhost, Port,
                                  [{active, false}, {sndbuf, 4096}]) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:send(S, "Whatever"),
    ok = gen_tcp:send(Client, Payload),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ok = gen_tcp:close(S),
    ok = ct:sleep(20),
    {error, closed} = gen_tcp:recv(Client, 0),

    %% Now test with option switched on.
    ?P("test with option explicitly switched on"),
    {ok, L1} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port1} = inet:port(L1),
    Client1 = case gen_tcp:connect(localhost, Port1,
				   [{active, false},
				    {sndbuf, 4096},
				    {show_econnreset, true}]) of
                  {ok, CSock1} ->
                      CSock1;
            {error, eaddrnotavail = Reason1} ->
                ?SKIPT(connect_failed_str(Reason1))
        end,
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:send(S1, "Whatever"),
    ok = gen_tcp:send(Client1, Payload),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(20),
    {error, econnreset} = gen_tcp:recv(Client1, 0),
    ?P("done"),
    ok.

%%
%% Test {linger {true, 0}} aborts a connection
%%

linger_zero(Config) when is_list(Config) ->
    ?TC_TRY(linger_zero, fun() -> do_linger_zero(Config) end).

do_linger_zero(_Config) ->
    %% All the econnreset tests will prove that {linger, {true, 0}} aborts
    %% a connection when the driver queue is empty. We will test here
    %% that it also works when the driver queue is not empty.
    {OS, _} = os:type(),
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, [{active, false},
				 {recbuf, 4096},
				 {show_econnreset, true}]),
    {ok, Port} = inet:port(L),
    ?P("connect (create client socket)"),
    Client = case gen_tcp:connect(localhost, Port,
                                  [{active, false}, {sndbuf, 4096}]) of
                 {ok, CSock} ->
                     CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    ?P("accept"),
    {ok, S} = gen_tcp:accept(L),
    ?P("close listen socket"),
    ok = gen_tcp:close(L),
    PayloadSize = 1024 * 1024,
    Payload = lists:duplicate(PayloadSize, $.),
    lz_ensure_non_empty_queue(Client, Payload, OS),
    ?P("linger: {true, 0}"),
    ok = inet:setopts(Client, [{linger, {true, 0}}]),
    ?P("close client socket"),
    ok = gen_tcp:close(Client),
    ok = ct:sleep(1),
    ?P("verify client socket (port) not connected"),
    undefined = erlang:port_info(Client, connected),
    ?P("try (and fail) recv (on accepted socket)"),
    {error, econnreset} = gen_tcp:recv(S, PayloadSize),
    ?P("done"),
    ok.

lz_ensure_non_empty_queue(Sock, Payload, OS) ->
    lz_ensure_non_empty_queue(Sock, Payload, OS, 1).

-define(LZ_MAX_SENDS, 3).

lz_ensure_non_empty_queue(Sock, _Payload, _OS, N) when (N > ?LZ_MAX_SENDS) ->
    ?P("queue size verification failed - port info: "
       "~n   Socket:      ~p"
       "~n   Socket info: ~p", [Sock, erlang:port_info(Sock)]),
    ct:fail("Queue size verification failed");
lz_ensure_non_empty_queue(Sock, Payload, OS, N) ->
    ?P("try send payload ~w (on client socket) when port info:"
       "~n   ~p", [N, erlang:port_info(Sock)]),
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

do_linger_zero_sndbuf(_Config) ->
    %% All the econnreset tests will prove that {linger, {true, 0}} aborts
    %% a connection when the driver queue is empty. We will test here
    %% that it also works when the driver queue is not empty
    %% and the linger zero option is set on the listen socket.
    {OS, _} = os:type(),
    ?P("create listen socket"),
    {ok, Listen} =
        gen_tcp:listen(0, [{active, false},
                           {recbuf, 4096},
                           {show_econnreset, true},
                           {linger, {true, 0}}]),
    {ok, Port} = inet:port(Listen),
    ?P("connect (create client socket)"),
    Client =
        case gen_tcp:connect(localhost, Port,
                             [{active, false}, {sndbuf, 4096}]) of
            {ok, CSock} ->
                CSock;
            {error, eaddrnotavail = Reason} ->
                ?SKIPT(connect_failed_str(Reason))
        end,
    ?P("accept"),
    {ok, Server} = gen_tcp:accept(Listen),
    ?P("close listen socket"),
    ok = gen_tcp:close(Listen),
    PayloadSize = 1024 * 1024,
    Payload = binary:copy(<<"0123456789ABCDEF">>, 256 * 1024), % 1 MB
    lz_ensure_non_empty_queue(Client, Payload, OS),
    ?P("verify linger: {true, 0}"),
    {ok, [{linger, {true, 0}}]} = inet:getopts(Server, [linger]),
    ?P("close client socket"),
    ok = gen_tcp:close(Server),
    ok = ct:sleep(1),
    ?P("verify client socket (port) not connected"),
    undefined = erlang:port_info(Server, connected),
    ?P("try (and fail) recv (on client socket)"),
    {error, closed} = gen_tcp:recv(Client, PayloadSize),
    ?P("done"),
    ok.


%% Thanks to Luke Gorrie. Tests for a very specific problem with 
%% corrupt data. The testcase will be killed by the timetrap timeout
%% if the bug is present.
http_bad_packet(Config) when is_list(Config) ->
    {ok,L} = gen_tcp:listen(0, [{active, false},
                                binary,
                                {reuseaddr, true},
                                {packet, http}]),
    {ok,Port} = inet:port(L),
    spawn_link(fun() -> erlang:yield(), http_bad_client(Port) end),
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

http_bad_client(Port) ->
    {ok,S} = gen_tcp:connect("localhost", Port, [{active,false}, binary]),
    ok = gen_tcp:send(S, "\r\n"),
    ok = gen_tcp:close(S).


%% Fill send queue and then start receiving.
%%
busy_send(Config) when is_list(Config) ->
    ?TC_TRY(busy_send, fun() -> do_busy_send(Config) end).

do_busy_send(_Config) ->
    OldFlag = process_flag(trap_exit, true),
    Master  = self(),
    Msg     = <<"the quick brown fox jumps over a lazy dog~n">>,
    ?P("[master] start server"),
    ServerF =
        fun() ->
                ?P("[server] create listen socket"),
                case gen_tcp:listen(0, [{active,false},binary,
                                        {reuseaddr,true},{packet,0}]) of
                    {ok, L} ->
                        ?P("[server] listen socket created"),
                        {ok, Port} = inet:port(L),
                        Master ! {self(), listen_port, Port},
                        ?P("[server] await continue"),
                        receive
                            {Master, continue} ->
                                ?P("[server] continue"),
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
                throw(SKIP);

            {'EXIT', Server, Reason} ->
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
	      case gen_tcp:connect("localhost", Port,
                                   [{active,false},binary,{packet,0}]) of
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
            ?P("Send timeout, time to receive..."),
            Server ! {self(), close},
            Client ! {self(), recv, N+1},
            receive
                {Server, send} ->
                    busy_send_2(Server, Client, N+1)
            after 10000 ->
                    %% If this happens, see busy_send_srv
                    ?P("UNEXPECTED: server send timeout"),
                    ct:fail({timeout,{server,not_send,flush([])}})
            end
    end.

busy_send_2(Server, Client, _N) ->
    %% Master
    %%
    receive
        {Server, [closed]} ->
            ?P("[master] received expected (server) closed"),
            receive
                {Client, [0,{error,closed}]} ->
                    ?P("[master] received expected (client) closed"),
                    ok
            end
    after 10000 ->
            ?P("UNEXPECTED: server closed timeout"),
            ct:fail({timeout, {server, not_closed, flush([])}})
    end.

busy_send_srv(L, Master, Msg) ->
    %% Server
    %% Sometimes this accept does not return, do not really know why
    %% but is causes the timeout error in busy_send_loop to be
    %% triggered. Only happens on OS X Leopard?!?
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

do_busy_disconnect_passive(_Config) ->
    ?P("[passive] begin"),
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_passive2(MuchoData, N) || N <- lists:seq(1, 10)],
    ?P("[passive] done"),
    ok.

do_busy_disconnect_passive2(MuchoData, N) ->
    ?P("[passive,~w] *** prepare server *** ", [N]),
    {_Server, S} = busy_disconnect_prepare_server([{active,false}]),
    ?P("[passive,~w] server prepared - start sending", [N]),
    busy_disconnect_passive_send(S, MuchoData).

busy_disconnect_passive_send(S, Data) ->
    case gen_tcp:send(S, Data) of
	ok ->
            busy_disconnect_passive_send(S, Data);
	{error, closed} ->
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

do_busy_disconnect_active(_Config) ->
    ?P("[active] begin"),
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_active2(MuchoData, N) || N <- lists:seq(1, 10)],
    ?P("[active] done"),
    ok.

do_busy_disconnect_active2(MuchoData, N) ->
    ?P("[active,~w] *** prepare server *** ", [N]),
    {Server, S} = busy_disconnect_prepare_server([{active,true}]),
    ?P("[active,~w] server prepared - start sending", [N]),
    busy_disconnect_active_send(Server, S, MuchoData).

busy_disconnect_active_send(Server, S, Data) ->
    case gen_tcp:send(S, Data) of
	ok ->
            busy_disconnect_active_send(Server, S, Data);
	{error, closed} ->
            ?P("[active-sender] send failed with closed - await tcp-closed"),
            busy_disconnect_active_send_await_closed(Server, S);
        {error, einval = Reason} ->
            ?P("[active-sender] UNEXPECTED send failure:"
               "~n   ~p", [Reason]),
            ?SKIPT(send_failed_str(Reason));

        {error, Reason} ->
            ?P("[active-sender] UNEXPECTED send failure:"
               "~n   ~p", [Reason]),
            ct:fail({unexpected_send_result, Reason})
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
    

busy_disconnect_prepare_server(ConnectOpts) ->
    Sender = self(),
    ?P("[prep-server] create server"),
    Server = spawn_link(fun() -> busy_disconnect_server(Sender) end),
    ?P("[prep-server] await port (from server)"),
    receive {port, Server, Port} -> ok end,
    ?P("[prep-server] connect to ~w", [Port]),
    case gen_tcp:connect(localhost, Port, ConnectOpts) of
        {ok, S} ->
            ?P("[prep-server] connected - order server start sending"),
            Server ! {Sender, sending},
            ?P("[prep-server] done"),
            {Server, S};
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end.

busy_disconnect_server(Sender) ->
    ?P("[server] create listen socket"),
    {ok, L} = gen_tcp:listen(0,
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

do_fill_sendq(_Config) ->
    OldFlag = process_flag(trap_exit, true),
    Master  = self(),
    ServerF =
        fun () ->
                ?P("[server] try listen"),
                case gen_tcp:listen(0, [{active,false},binary,
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
                case gen_tcp:connect(
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

do_partial_recv_and_close(_Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    {ok,P} = inet:port(L),
    Sock =
        case gen_tcp:connect("localhost", P, [{active,false}]) of
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

do_partial_recv_and_close_2(_Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    {ok,P} = inet:port(L),
    Server = self(),
    Client =
	spawn_link(
	  fun () ->
		  receive after 2000 -> ok end,
		  case gen_tcp:connect("localhost", P, [{active,false}]) of
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

do_partial_recv_and_close_3(_Config) ->
    [do_partial_recv_and_close_3() || _ <- lists:seq(0, 20)],
    ok.

do_partial_recv_and_close_3() ->
    Parent = self(),
    spawn_link(fun() ->
		       {ok,L} = gen_tcp:listen(0, [{active,false}]),
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
    S = case gen_tcp:connect(localhost, Port, [{active, false}]) of
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
    

test_prio_put_get() ->
    Tos = 3 bsl 5,
    {ok,L1} = gen_tcp:listen(0, [{active,false}]),
    ok = inet:setopts(L1,[{priority,3}]),
    ok = inet:setopts(L1,[{tos,Tos}]),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    ok = inet:setopts(L1,[{priority,3}]), % Dont destroy each other
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    ok = inet:setopts(L1,[{reuseaddr,true}]), % Dont let others destroy
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(L1,[priority,tos]),
    gen_tcp:close(L1),
    ok.

test_prio_accept() ->
    {ok,Sock}=gen_tcp:listen(0,[binary,{packet,0},{active,false},
                                {reuseaddr,true},{priority,4}]),
    {ok,Port} = inet:port(Sock),
    Sock2 = case gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4}]) of
                {ok, S2} -> 
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    {ok,Sock3}=gen_tcp:accept(Sock),
    {ok,[{priority,4}]} = inet:getopts(Sock,[priority]),
    {ok,[{priority,4}]} = inet:getopts(Sock2,[priority]),
    {ok,[{priority,4}]} = inet:getopts(Sock3,[priority]),
    gen_tcp:close(Sock),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    ok.

test_prio_accept2() ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    {ok,Sock}=gen_tcp:listen(0,[binary,{packet,0},{active,false},
                                {reuseaddr,true},{priority,4},
                                {tos,Tos1}]),
    {ok,Port} = inet:port(Sock),
    Sock2 = case gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    {ok,Sock3}=gen_tcp:accept(Sock),
    {ok,[{priority,4},{tos,Tos1}]} = inet:getopts(Sock,[priority,tos]),
    {ok,[{priority,4},{tos,Tos2}]} = inet:getopts(Sock2,[priority,tos]),
    {ok,[{priority,4},{tos,Tos1}]} = inet:getopts(Sock3,[priority,tos]),
    gen_tcp:close(Sock),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    ok.

test_prio_accept3() ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    {ok,Sock}=gen_tcp:listen(0,[binary,{packet,0},{active,false},
                                {reuseaddr,true},
                                {tos,Tos1}]),
    {ok,Port} = inet:port(Sock),
    Sock2 = case gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(connect_failed_str(Reason))
    end,
    {ok,Sock3}=gen_tcp:accept(Sock),
    {ok,[{priority,0},{tos,Tos1}]} = inet:getopts(Sock,[priority,tos]),
    {ok,[{priority,0},{tos,Tos2}]} = inet:getopts(Sock2,[priority,tos]),
    {ok,[{priority,0},{tos,Tos1}]} = inet:getopts(Sock3,[priority,tos]),
    gen_tcp:close(Sock),
    gen_tcp:close(Sock2),
    gen_tcp:close(Sock3),
    ok.
    
test_prio_accept_async() ->
    Tos1 = 4 bsl 5,
    Tos2 = 3 bsl 5,
    Ref = make_ref(),
    spawn(?MODULE,priority_server,[{self(),Ref}]),
    Port = receive
               {Ref,P} -> P
           after 5000 -> ct:fail({error,"helper process timeout"})
           end,
    receive
    after 3000 -> ok
    end,
    Sock2 = case gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                   {active,false},
                                                   {reuseaddr,true},
                                                   {priority,4},
                                                   {tos,Tos2}]) of
                {ok, S2} ->
                    S2;
                {error, eaddrnotavail = Reason} ->
                    ?SKIPT(connect_failed_str(Reason))
            end,
    receive
        {Ref,{ok,[{priority,4},{tos,Tos1}]}} ->
            ok;
        {Ref,Error} ->
            ct:fail({missmatch,Error})
    after 5000 -> ct:fail({error,"helper process timeout"})
    end,
    receive
        {Ref,{ok,[{priority,4},{tos,Tos1}]}} ->
            ok;
        {Ref,Error2} ->
            ct:fail({missmatch,Error2})
    after 5000 -> ct:fail({error,"helper process timeout"})
    end,

    {ok,[{priority,4},{tos,Tos2}]} = inet:getopts(Sock2,[priority,tos]),
    catch gen_tcp:close(Sock2),
    ok.

priority_server({Parent,Ref}) ->
    Tos1 = 4 bsl 5,
    {ok,Sock}=gen_tcp:listen(0,[binary,{packet,0},{active,false},
                                {reuseaddr,true},{priority,4},
                                {tos,Tos1}]),
    {ok,Port} = inet:port(Sock),
    Parent ! {Ref,Port},
    {ok,Sock3}=gen_tcp:accept(Sock),
    Parent ! {Ref, inet:getopts(Sock,[priority,tos])},
    Parent ! {Ref, inet:getopts(Sock3,[priority,tos])},
    ok.

test_prio_fail() ->
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    {error,_} = inet:setopts(L,[{priority,1000}]),
    gen_tcp:close(L),
    ok.

test_prio_udp() ->
    Tos = 3 bsl 5,
    {ok,S} = gen_udp:open(0,[{active,false},binary,{tos, Tos},
                             {priority,3}]),
    {ok,[{priority,3},{tos,Tos}]} = inet:getopts(S,[priority,tos]),
    gen_udp:close(S),
    ok.

%% Tests the so_priority and ip_tos options on sockets when applicable.
so_priority(Config) when is_list(Config) ->
    try do_so_priority(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_so_priority(_Config) ->
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    ok = inet:setopts(L,[{priority,1}]),
    case inet:getopts(L,[priority]) of
	{ok,[{priority,1}]} ->
	    gen_tcp:close(L),
	    test_prio_put_get(),
	    test_prio_accept(),
	    test_prio_accept2(),
	    test_prio_accept3(),
	    test_prio_accept_async(),
	    test_prio_fail(),
	    test_prio_udp(),
	    ok;
	_ ->
	    case os:type() of
		{unix,linux} ->
		    case os:version() of
			{X,Y,_} when (X > 2) or ((X =:= 2) and (Y >= 4)) ->
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

recvtos(_Config) ->
    test_pktoptions(
      inet, [{recvtos,tos,96}],
      fun recvtos_ok/2,
      false).

recvtosttl(_Config) ->
    test_pktoptions(
      inet, [{recvtos,tos,96},{recvttl,ttl,33}],
      fun (OSType, OSVer) ->
              recvtos_ok(OSType, OSVer) andalso recvttl_ok(OSType, OSVer)
      end,
      false).

recvttl(_Config) ->
    test_pktoptions(
      inet, [{recvttl,ttl,33}],
      fun recvttl_ok/2,
      false).

recvtclass(_Config) ->
    {ok,IFs} = inet:getifaddrs(),
    case
        [Name ||
            {Name,Opts} <- IFs,
            lists:member({addr,{0,0,0,0,0,0,0,1}}, Opts)]
    of
        [_] ->
            test_pktoptions(
              inet6, [{recvtclass,tclass,224}],
              fun recvtclass_ok/2,
              true);
        [] ->
            {skip,{ipv6_not_supported,IFs}}
    end.

%% These version numbers are above the highest noted
%% in daily tests where the test fails for a plausible reason,
%% so skip on platforms of lower version, i.e they are future
%% versions where it is possible that it might not fail.
%%
%% When machines with newer versions gets installed,
%% if the test still fails for a plausible reason these
%% version numbers simply should be increased.
%% Or maybe we should change to only test on known good
%% platforms - change {unix,_} to false?

%% pktoptions is not supported for IPv4
recvtos_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,8,0});
recvtos_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,4,0});
%% Using the option returns einval, so it is not implemented.
recvtos_ok({unix,freebsd}, OSVer) -> not semver_lt(OSVer, {12,2,0});
recvtos_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%% Does not return any value - not implemented for pktoptions
recvtos_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {3,1,0});
%%
recvtos_ok({unix,_}, _) -> true;
recvtos_ok(_, _) -> false.

%% pktoptions is not supported for IPv4
recvttl_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,8,0});
recvttl_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,4,0});
%% Using the option returns einval, so it is not implemented.
recvttl_ok({unix,freebsd}, OSVer) -> not semver_lt(OSVer, {12,2,0});
recvttl_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%% Does not return any value - not implemented for pktoptions
recvttl_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {2,7,0});
%%
recvttl_ok({unix,_}, _) -> true;
recvttl_ok(_, _) -> false.

%% pktoptions is not supported for IPv6
recvtclass_ok({unix,openbsd}, OSVer) -> not semver_lt(OSVer, {6,8,0});
recvtclass_ok({unix,darwin}, OSVer) -> not semver_lt(OSVer, {19,4,0});
recvtclass_ok({unix,sunos}, OSVer) -> not semver_lt(OSVer, {5,12,0});
%% Using the option returns einval, so it is not implemented.
recvtclass_ok({unix,freebsd}, OSVer) -> not semver_lt(OSVer, {12,2,0});
%% Does not return any value - not implemented for pktoptions
recvtclass_ok({unix,linux}, OSVer) -> not semver_lt(OSVer, {3,1,0});
%%
recvtclass_ok({unix,_}, _) -> true;
recvtclass_ok(_, _) -> false.

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

test_pktoptions(Family, Spec, OSFilter, CheckConnect) ->
    OSType = os:type(),
    OSVer  = os:version(),
    case OSFilter(OSType, OSVer) of
        true ->
            ?P("OS: ~p, ~p", [OSType, OSVer]),
            test_pktoptions(Family, Spec, CheckConnect, OSType, OSVer);
        false ->
            {skip,{not_supported_for_os_version,{OSType,OSVer}}}
    end.
%%
test_pktoptions(Family, Spec, CheckConnect, OSType, OSVer) ->
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
    {ok,L} =
        gen_tcp:listen(
          0,
          [Family,binary,{active,false},{send_timeout,Timeout}
           |TrueRecvOpts]),
    {ok,P} = inet:port(L),
    {ok,TrueRecvOpts} = inet:getopts(L, RecvOpts),
    {ok,OptsValsDefault} = inet:getopts(L, Opts),
    %%
    %% Set RecvOpts and Option values on connect socket
    {ok,S2} =
        gen_tcp:connect(
          Address, P,
          [Family,binary,{active,false},{send_timeout,Timeout}
           |TrueRecvOpts ++ OptsVals],
          Timeout),
    {ok,TrueRecvOpts} = inet:getopts(S2, RecvOpts),
    {ok,OptsVals} = inet:getopts(S2, Opts),
    %%
    %% Accept socket inherits the options from listen socket
    {ok,S1} = gen_tcp:accept(L, Timeout),
    {ok,TrueRecvOpts} = inet:getopts(S1, RecvOpts),
    {ok,OptsValsDefault} = inet:getopts(S1, Opts),
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
                    {ok, [{pktoptions, PktOpts1}]} ->
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
    OptsVals1 = VerifyRemOpts(S1, dest),
    OptsVals2 = VerifyRemOpts(S2, orig),
    %% {ok,[{pktoptions,OptsVals1}]} = inet:getopts(S1, [pktoptions]),
    %% {ok,[{pktoptions,OptsVals2}]} = inet:getopts(S2, [pktoptions]),
    (Result1 = sets_eq(OptsVals1, OptsVals))
        orelse ?P("Accept differs: ~p neq ~p", [OptsVals1,OptsVals]),
    (Result2 = sets_eq(OptsVals2, OptsValsDefault))
        orelse ?P("Connect differs: ~p neq ~p",
                  [OptsVals2, OptsValsDefault]),
    %%
    ok = gen_tcp:close(S2),
    ok = gen_tcp:close(S1),
    %%
    %%
    %% Clear RecvOpts on listen socket and set Option values
    ok = inet:setopts(L, FalseRecvOpts ++ OptsVals),
    {ok,FalseRecvOpts} = inet:getopts(L, RecvOpts),
    {ok,OptsVals} = inet:getopts(L, Opts),
    %%
    %% Set RecvOpts on connecting socket
    %%
    {ok,S4} =
        gen_tcp:connect(
          Address, P,
          [Family,binary,{active,false},{send_timeout,Timeout}
          |TrueRecvOpts],
          Timeout),
    {ok,TrueRecvOpts} = inet:getopts(S4, RecvOpts),
    {ok,OptsValsDefault} = inet:getopts(S4, Opts),
    %%
    %% Accept socket inherits the options from listen socket
    {ok,S3} = gen_tcp:accept(L, Timeout),
    {ok,FalseRecvOpts} = inet:getopts(S3, RecvOpts),
    {ok,OptsVals} = inet:getopts(S3, Opts),
    %%
    %% Verify returned remote options
    {ok,[{pktoptions,[]}]} = inet:getopts(S3, [pktoptions]),
    {ok,[{pktoptions,OptsVals4}]} = inet:getopts(S4, [pktoptions]),
    (Result3 = sets_eq(OptsVals4, OptsVals))
        orelse io:format(
                 "Accept2 differs: ~p neq ~p~n", [OptsVals4,OptsVals]),
    %%
    ok = gen_tcp:close(S4),
    ok = gen_tcp:close(S3),
    ok = gen_tcp:close(L),
    (Result1 and ((not CheckConnect) or (Result2 and Result3)))
        orelse
        exit({failed,
              [{OptsVals1,OptsVals4,OptsVals},
               {OptsVals2,OptsValsDefault}],
              {OSType,OSVer}}),
%%    exit({{OSType,OSVer},success}), % In search for the truth
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
            NextN = if N =:= infinity -> N; true -> N - 1 end,
	    [{P,Msg}] ++ collect_accepts(NextN, Tmo - (millis()-A))

    after Tmo ->
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

do_primitive_accept(_Config) ->
    LSock =
        case gen_tcp:listen(0,[]) of
            {ok, LS} ->
                LS;
            {error, LReason} ->
                ?SKIPT(listen_failed_str(LReason))
        end,
    {ok, PortNo} = inet:port(LSock),
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LSock)} end,
    P = spawn(F),
    case gen_tcp:connect("localhost", PortNo, []) of
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

do_multi_accept_close_listen(_Config) ->
    ?P("try create listen socket"),
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeout(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order2(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order3(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order4(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order5(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order6(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

do_accept_timeouts_in_order7(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
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

%% Check that multi-accept timeouts behave correctly when mixed with successful timeouts.
accept_timeouts_mixed(Config) when is_list(Config) ->
    try do_accept_timeouts_mixed(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_accept_timeouts_mixed(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    {ok,PortNo}=inet:port(LS),
    P1 = spawn(mktmofun(1000,Parent,LS)),
    wait_until_accepting(P1,500),
    P2 = spawn(mktmofun(2000,Parent,LS)),
    wait_until_accepting(P2,500),
    P3 = spawn(mktmofun(3000,Parent,LS)),
    wait_until_accepting(P3,500),
    P4 = spawn(mktmofun(4000,Parent,LS)),
    wait_until_accepting(P4,500),
    ok = ?EXPECT_ACCEPTS([{P1,{error,timeout}}],infinity,1500),
    case gen_tcp:connect("localhost", PortNo, []) of
        {ok, _} ->
            ok;
        {error, eaddrnotavail = Reason1} ->
            ?SKIPT(connect_failed_str(Reason1))
    end,
    ok = ?EXPECT_ACCEPTS([{P2,{ok,Port0}}] when is_port(Port0),infinity,100),
    ok = ?EXPECT_ACCEPTS([{P3,{error,timeout}}],infinity,2000),
    case gen_tcp:connect("localhost", PortNo, []) of
        {error, eaddrnotavail = Reason2} ->
            ?SKIPT(connect_failed_str(Reason2));
        _  ->
            ok
    end,
    ok = ?EXPECT_ACCEPTS([{P4,{ok,Port1}}] when is_port(Port1),infinity,100).

%% Check that single acceptor behaves as expected when killed.
killing_acceptor(Config) when is_list(Config) ->
    try do_killing_acceptor(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_killing_acceptor(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Pid = spawn(
            fun() ->
                    erlang:display({accepted,self(),gen_tcp:accept(LS)})
            end),
    receive after 100 -> ok
    end,
    {ok,L1} = prim_inet:getstatus(LS),
    true = lists:member(accepting, L1),
    exit(Pid,kill),
    receive after 100 -> ok
    end,
    {ok,L2} = prim_inet:getstatus(LS),
    false  = lists:member(accepting, L2),
    ok.

%% Check that multi acceptors behaves as expected when killed.
killing_multi_acceptors(Config) when is_list(Config) ->
    try do_killing_multi_acceptors(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_killing_multi_acceptors(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,             
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),
    Pid = spawn(F),
    Pid2 = spawn(F2),
    receive after 100 -> ok
    end,
    {ok,L1} = prim_inet:getstatus(LS),
    true = lists:member(accepting, L1),
    exit(Pid,kill),
    receive after 100 -> ok
    end,
    {ok,L2} = prim_inet:getstatus(LS),
    true  = lists:member(accepting, L2),
    ok = ?EXPECT_ACCEPTS([{Pid2,{error,timeout}}],1,1000),
    {ok,L3} = prim_inet:getstatus(LS),
    false  = lists:member(accepting, L3),
    ok.

%% Check that multi acceptors behaves as expected when killed (more).
killing_multi_acceptors2(Config) when is_list(Config) ->
    try do_killing_multi_acceptors2(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_killing_multi_acceptors2(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,             
    Parent = self(),
    {ok,PortNo}=inet:port(LS),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = mktmofun(1000,Parent,LS),
    Pid = spawn(F),
    Pid2 = spawn(F),
    receive after 100 -> ok
    end,
    {ok,L1} = prim_inet:getstatus(LS),
    true = lists:member(accepting, L1),
    exit(Pid,kill),
    receive after 100 -> ok
    end,
    {ok,L2} = prim_inet:getstatus(LS),
    true  = lists:member(accepting, L2),
    exit(Pid2,kill),
    receive after 100 -> ok
    end,
    {ok,L3} = prim_inet:getstatus(LS),
    false  = lists:member(accepting, L3),
    Pid3 = spawn(F2),
    receive after 100 -> ok
    end,
    {ok,L4} = prim_inet:getstatus(LS),
    true  = lists:member(accepting, L4),
    gen_tcp:connect("localhost",PortNo,[]),
    ok = ?EXPECT_ACCEPTS([{Pid3,{ok,Port}}] when is_port(Port),1,100),
    {ok,L5} = prim_inet:getstatus(LS),
    false  = lists:member(accepting, L5),
    ok.
    
%% Checks that multi-accept works when more than one accept can be
%% done at once (wb test of inet_driver).
several_accepts_in_one_go(Config) when is_list(Config) ->
    try do_several_accepts_in_one_go(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_several_accepts_in_one_go(_Config) ->
    LS = case gen_tcp:listen(0,[]) of
             {ok, LSock} ->
                 LSock;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
         end,
    Parent = self(),
    {ok,PortNo}=inet:port(LS),
    F1 = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    F2 = fun() -> Parent ! {connected,self(),gen_tcp:connect("localhost",PortNo,[])} end,
    Ns = lists:seq(1,8),
    _  = [spawn(F1) || _ <- Ns],
    ok = ?EXPECT_ACCEPTS([],1,500), % wait for tmo
    _  = [spawn(F2) || _ <- Ns],
    ok = ?EXPECT_ACCEPTS([{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}}],8,15000),
    ok = ?EXPECT_CONNECTS([{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}},{_,{ok,_}}],1000),
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
        {current_function,{prim_inet,accept0,3}} ->
            case process_info(Proc,status) of
                {status,waiting} -> 
                    ok;
                _O1 ->
                    receive 
                    after 5 ->
                        wait_until_accepting(Proc,N-1)
                    end
            end;
         _O2 ->
            receive 
            after 5 ->
                wait_until_accepting(Proc,N-1)
            end
    end.


%% Check that accept returns {error, system_limit}
%% (and not {error, enfile}) when running out of ports.
accept_system_limit(Config) when is_list(Config) ->
    OldFlag = process_flag(trap_exit, true),
    Res = try do_accept_system_limit(Config)
          catch
              exit:{skip, _} = SKIP ->
                  SKIP
          end,
    process_flag(trap_exit, OldFlag),
    Res.

do_accept_system_limit(_Config) ->
    ?P("create listen socket"),
    LS = case gen_tcp:listen(0, []) of
             {ok, LSocket} ->
                 LSocket;
             {error, eaddrnotavail = Reason} ->
                 ?SKIPT(listen_failed_str(Reason))
        end,             
    {ok, TcpPort} = inet:port(LS),
    Me = self(),
    ?P("create connector"),
    Connector = spawn_link(fun () -> connector(TcpPort, Me) end),
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
            ?P("acceptor: system limit => *almost* done (~w)", [length(A)]),
            acceptor(Connector, LS, true, A);
        {error, timeout} when GotSL ->
            ?P("acceptor: timeout (with system limit) => done (~w)",
              [length(A)]),
            ok;
        {error, timeout} ->
            error
    end.

connector(TcpPort, Tester) ->
    ?P("[connector] start"),
    ManyPorts = open_ports([]),
    Tester ! {self(), sync},
    ?P("[connector] sync with tester (~p)", [Tester]),
    receive {Tester, continue} -> timer:sleep(100) end,
    ?P("[connector] begin connecting"),
    ConnF = fun (Port) ->
                    case (catch gen_tcp:connect({127,0,0,1}, TcpPort, [])) of
                        {ok, Sock} ->
                            Sock;
                        {error, eaddrnotavail = Reason} ->
                            ?SKIPE(connect_failed_str(Reason));
                        _Error ->
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
    try do_active_once_closed(Config)
    catch
        throw:{skip, _} = SKIP ->
            SKIP
    end.

do_active_once_closed(_Config) ->
    ?P("stage 1"),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    ?P("stage 2"),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    ?P("stage 3"),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
                  fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    ?P("stage 4"),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    ?P("stage 5"),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,false}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end
     end)(),
    ?P("done"),
    ok.

%% Check that active n and tcp_close messages behave as expected.
active_n_closed(Config) when is_list(Config) ->
    ?TC_TRY(active_n_closed, fun() -> do_active_n_closed(Config) end).

do_active_n_closed(_Config) ->
    ?P("create listen socket"),
    {ok, L} = gen_tcp:listen(0, [binary, {active, false}]),

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
                S = case gen_tcp:connect("localhost", Port,
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
    %% {RecvSize, Down} =
    %%     (fun Server(Size) ->
    %%              receive
    %%                  {tcp, S, Bin} ->
    %%                      %% ?P("got a chunk (~w) of data", [byte_size(Bin)]),
    %%                      Server(byte_size(Bin) + Size);
    %%                  {tcp_closed, S} ->
    %%                      ?P("got closed -> we are done: ~w", [Size]),
    %%                      Size;
    %%                  {tcp_passive, S} ->
    %%                      %% ?P("got passive -> active"),
    %%                      inet:setopts(S, [{active, 10}]),
    %%                      Server(Size);
    %%                  {'DOWN', MRef, process, Pid, Reason} ->
    %%                      ?P("Received UNEXPECTED down message regarding client:"
    %%                         "~n   Reason: ~p"
    %%                         "~n   S:      ~p"
    %%                         "~n   Port Info: ~p",
    %%                         [Reason, ]),
    %%                      ct:fail({unexpected_client_down, Reason});
    %%                  Msg ->
    %%                      ?P("ignore: ~p", [Msg]),
    %%                      Server(Size)
    %%              end
    %%      end)(0),
    %% ?P("await client process termination"),
    %% receive
    %%     {'DOWN', MRef, process, Pid, ok} ->
    %%         ok;
    %%     {'DOWN', MRef, process, Pid, CloseRes} ->
    %%         exit({unexpected, close, CloseRes})
    %% end,

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
    ?P("begin"),
    Dir = filename:dirname(code:which(?MODULE)),
    ?P("create (slave) node"),
    {ok,RNode} = test_server:start_node(?UNIQ_NODE_NAME, slave,
					[{args,"-pa " ++ Dir}]),

    %% Basic
    ?P("basic check wo autoclose"),
    send_timeout_basic(false, RNode),
    ?P("basic check w autoclose"),
    send_timeout_basic(true, RNode),

    BinData = <<1:10000>>,

    %% Check timeout length.
    ?P("check timeout length"),
    Self = self(),
    {Pid, Mon} = spawn_monitor(
                   fun() ->
                           A = setup_timeout_sink(RNode, 1000, true),
                           Send = fun() ->
                                          Res = gen_tcp:send(A, BinData),
                                          Self ! Res,
                                          Res
                                  end,
                           {{error, timeout}, _} = timeout_sink_loop(Send)
                   end),
    Diff = get_max_diff(),
    ?P("Max time for send: ~p",[Diff]),
    true = (Diff > 500) and (Diff < 1500),

    %% Wait for the process to die.
    ?P("await (timeout checker) process death"),
    receive {'DOWN', Mon, process, Pid, _} -> ok end,

    %% Check that parallell writers do not hang forever
    ?P("check parallell writers wo autoclose"),
    send_timeout_para(false, RNode),
    ?P("check parallell writers w autoclose"),
    send_timeout_para(true, RNode),

    ?P("stop (slave) node"),
    test_server:stop_node(RNode),

    ?P("done"),
    ok.

send_timeout_basic(AutoClose, RNode) ->
    BinData = <<1:10000>>,

    A = setup_timeout_sink(RNode, 1000, AutoClose),
    Send = fun() -> gen_tcp:send(A, BinData) end,
    {{error, timeout}, _} = timeout_sink_loop(Send),

    %% Check that the socket is not busy/closed...
    {error,Error} = gen_tcp:send(A, <<"Hej">>),
    after_send_timeout(AutoClose, Error),
    ok.

send_timeout_para(AutoClose, RNode) ->
    BinData = <<1:10000>>,

    ?P("[para] sink"),
    A = setup_timeout_sink(RNode, 1000, AutoClose),
    Self = self(),
    SenderFun = fun() ->
                        ?P("[para:sender] start"),
			Send = fun() -> gen_tcp:send(A, BinData) end,
			Self ! {self(), timeout_sink_loop(Send)}
		end,
    Info = fun() ->
                   {(catch erlang:port_info(A)),
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
                    end,
                    try inet:getstat(A) of
                        {ok, S3} ->
                            S3;
                        {error, R3} ->
                            ?F("ERROR: ~p", [R3]);
                        X3 ->
                            ?F("UNKNOWN: ~p", [X3])
                    catch
                        C3:E3:S3 ->
                            ?F("CATCHED: ~p, ~p, ~p", [C3, E3, S3])
                    end,
                    try prim_inet:getstatus(A) of
                        {ok, S4} ->
                            S4;
                        {error, R4} ->
                            ?F("ERROR: ~p", [R4]);
                        X4 ->
                            ?F("UNKNOWN: ~p", [X4])
                    catch
                        C4:E4:S4 ->
                            ?F("CATCHED: ~p, ~p, ~p", [C4, E4, S4])
                    end}
           end,
    ?P("[para] spawn process 1 with sender fun"),
    Snd1 = spawn_link(SenderFun),
    ?P("[para] spawn process 2 with sender fun"),
    Snd2 = spawn_link(SenderFun),

    ?P("[para] await sender timeout when"
       "~n   Sender 1: ~p"
       "~n   Sender 2: ~p", [Snd1, Snd2]),
    First =
        receive
            {Snd1, {{error, timeout}, N}} ->
                ?P("[para] timeout received from sender 1 (~p, ~p)", [Snd1, N]),
                1;
            {Snd2, {{error, timeout}, N}} ->
                ?P("[para] timeout received from sender 2 (~p, ~p)", [Snd2, N]),
                2
        after 20000 ->
                {PortStatus1, SockOpts1, SockStat1, SockStatus1} = Info(),
                ?P("[para] UNEXPECTED timeout(1,~w) when:"
                   "~n   Sender 1 Info: ~p"
                   "~n   Sender 2 Info: ~p"
                   "~n   Port Status:   ~p"
                   "~n   Send Timeout:  ~p"
                   "~n   Socket Stats:  ~p"
                   "~n   Socket Status: ~p"
                   "~n   Message Queue: ~p",
                   [AutoClose,
                    (catch process_info(Snd1)),
                    (catch process_info(Snd2)),
                    PortStatus1, SockOpts1, SockStat1, SockStatus1,
                    flush([])]),
                Snd1 ! {info_and_die, Info},
                Snd2 ! {info_and_die, Info},
                ct:sleep(?SECS(1)),
                exit({timeout, AutoClose})
        end,
    
    Second = if (First =:= 1) -> 2; true -> 1 end,
    ?P("await sender ~w error", [Second]),
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
                    {PortStatus21, SockOpts21, SockStat21, SockStatus21} =
                        Info(),
                    ?P("[para] UNEXPECTED timeout(2,~w):"
                       "~n   Sender 1 Info: ~p"
                       "~n   Port Status:   ~p"
                       "~n   Send Timeout:  ~p"
                       "~n   Socket Stats:  ~p"
                       "~n   Socket Status: ~p"
                       "~n   Message Queue: ~p",
                       [AutoClose,
                        (catch process_info(Snd1)),
                        PortStatus21, SockOpts21, SockStat21, SockStatus21,
                        flush([])]),
                Snd1 ! {info_and_die, Info};
               true ->
                    {PortStatus22, SockOpts22, SockStat22, SockStatus22} =
                        Info(),
                    ?P("[para] UNEXPECTED timeout(2,~w):"
                       "~n   Sender 2 Info: ~p"
                       "~n   Port Status:   ~p"
                       "~n   Send Timeout:  ~p"
                       "~n   Socket Stats:  ~p"
                       "~n   Socket Status: ~p"
                       "~n   Message Queue: ~p",
                       [AutoClose,
                        (catch process_info(Snd2)),
                        PortStatus22, SockOpts22, SockStat22, SockStatus22,
                        flush([])]),
                    Snd2 ! {info_and_die, Info}
            end,
            ct:sleep(?SECS(1)),
	    exit({timeout, AutoClose, Second})
    end,
    {error,Error_2} = gen_tcp:send(A, <<"Hej">>),
    after_send_timeout(AutoClose, Error_2),
    ?P("[para] done"),    
    ok.


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
		    get_max_diff(Diff);
		true ->
		    get_max_diff(Max)
	    end;
	{error,timeout} ->
	    Diff = millis() - T1,
	    if
		Diff > Max ->
		    Diff;
		true ->
		    Max
	    end
    after 10000 ->
              exit(timeout)
    end.
	    
after_send_timeout(AutoClose, Reason) ->
    case Reason of
        timeout when AutoClose =:= false -> ok;
        enotconn when AutoClose =:= true -> ok
%%%        timeout -> ok;
%%%        enotconn when AutoClose -> ok;
%%%        closed when AutoClose -> ok
    end.


%% Test the send_timeout socket option for active sockets.
send_timeout_active(Config) when is_list(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,RNode} = test_server:start_node(?UNIQ_NODE_NAME, slave,
					[{args,"-pa " ++ Dir}]),
    do_send_timeout_active(false, RNode),
    do_send_timeout_active(true, RNode),
    test_server:stop_node(RNode),
    ok.

do_send_timeout_active(AutoClose, RNode) ->
    {A,C} = setup_active_timeout_sink(RNode, 1, AutoClose),
    inet:setopts(A, [{active, once}]),
    Mad = spawn_link(RNode, fun() -> mad_sender(C) end),
    ListData = lists:duplicate(1000, $a),
    F = fun() ->
		receive
		    {tcp, _Sock, _Data} ->
			inet:setopts(A, [{active, once}]),
			Res = gen_tcp:send(A, ListData),
			Res;
		    Err ->
			io:format("sock closed: ~p~n", [Err]),
			Err
		end
	end,
    {{error, timeout}, _} = timeout_sink_loop(F),
    unlink(Mad),
    exit(Mad, kill),
    flush(),
    ok.

mad_sender(S) ->
    U = rand:uniform(1000000),
    case gen_tcp:send(S, integer_to_list(U)) of
        ok ->
            mad_sender(S);
        Err ->
            Err
    end.

flush() ->
    receive
	_X ->
	    flush()
    after 0 ->
	    ok
    end.

setup_closed_ao() ->
    Dir = filename:dirname(code:which(?MODULE)),
    ?P("[setup] start slave node"),
    R = case test_server:start_node(?UNIQ_NODE_NAME, slave,
                                    [{args,"-pa " ++ Dir}]) of
            {ok, Slave} ->
                Slave;
            {error, Reason} ->
                ?SKIPT(?F("failed starting slave node: ~p", [Reason]))
        end,
    Host = get_hostname(node()),
    ?P("[setup] create listen socket"),
    L = case gen_tcp:listen(0, [{active,false},{packet,2}]) of
            {ok, LSock} ->
                LSock;
            {error, eaddrnotavail = LReason} ->
                (catch test_server:stop_node(R)),
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
                      gen_tcp:connect(Host, Port,
                                      [{active, false}, {packet, 2}]) 
              end,
    ?P("[setup] create (remote) connection"),
    C = case Remote(Connect) of
            {ok, CSock} ->
                CSock;
            {error, eaddrnotavail = CReason} ->
                (catch test_server:stop_node(R)),
                ?SKIPT(connect_failed_str(CReason))
        end,
    ?P("[setup] accept (local) connection"),
    A = case gen_tcp:accept(L) of
            {ok, ASock} ->
                ASock;
            {error, eaddrnotavail = AReason} ->
                (catch test_server:stop_node(R)),
                ?SKIPT(accept_failed_str(AReason))
        end,
    ?P("[setup] send message"),
    gen_tcp:send(A,"Hello"),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
    ?P("[setup] close (remote) connection"),
    ok =  Remote(fun() -> gen_tcp:close(C) end),
    Loop2 = fun(_,_,_,0) ->
		    {failure, timeout}; 
	       (L2,{MA,MB},F2,N) ->
		    case F2() of
			MA -> MA;
			MB -> MB;
			Other -> ?P("~p",[Other]),
				 receive after 1000 -> ok end,
				 L2(L2,{MA,MB},F2,N-1)
		    end
	    end,
    Loop = fun(Match2,F3) ->  Loop2(Loop2,Match2,F3,10) end,
    ?P("[setup] stop slave node"),
    test_server:stop_node(R),
    ?P("[setup] done"),
    {Loop,A}.
    
setup_timeout_sink(RNode, Timeout, AutoClose) ->
    Host = get_hostname(node()),
    ?P("[sink] create listen socket"),
    {ok, L} = gen_tcp:listen(0, [{active,             false},
                                 {packet,             2},
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
			     gen_tcp:connect(Host,Port,
					     [{active,false},{packet,2}])
		     end),
    ?P("[sink] accept"),
    {ok, A} = gen_tcp:accept(L),
    ?P("[sink] send message"),
    gen_tcp:send(A,"Hello"),
    ?P("[sink] recv message on remote node (~p)", [RNode]),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
    ?P("[sink] done"),
    A.

setup_active_timeout_sink(RNode, Timeout, AutoClose) ->
    Host = get_hostname(node()),
    ListenOpts =  [binary,{active,false},{packet,0},
		   {nodelay,true},{keepalive,true},
		   {send_timeout,Timeout},{send_timeout_close,AutoClose}],
    {ok, L} = gen_tcp:listen(0, ListenOpts),
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
			     gen_tcp:connect(Host, Port, [{active,false}])
		     end),
    {ok, A} = gen_tcp:accept(L),
    gen_tcp:send(A, "Hello"),
    {ok, "H"++_} = Remote(fun() -> gen_tcp:recv(C, 0) end),
    {A, C}.

timeout_sink_loop(Action) ->
    put(action,  nothing),
    put(sent,    0),
    put(elapsed, 0),
    timeout_sink_loop(Action, 0).

timeout_sink_loop(Action, N) ->
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
                {info_and_die, Info} ->
                    {PortStatus, SockOpts, SockStat, SockStatus} = Info(),
                    ?P("[sink-loop] info and die: "
                       "~n   Port Status:   ~p"
                       "~n   Send Timeout:  ~p"
                       "~n   Socket Stats:  ~p"
                       "~n   Socket Status: ~p",
                       [PortStatus, SockOpts, SockStat, SockStatus]),
                    exit(normal)
            after 1 -> ok
            end,
	    timeout_sink_loop(Action, N+1);
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
otp_7731(Config) when is_list(Config) ->
    ServerPid = spawn_link(?MODULE, otp_7731_server, [self()]),
    receive {ServerPid, ready, PortNum} -> ok end,

    {ok, Socket} = gen_tcp:connect("localhost", PortNum,
                                   [binary, {active, false}, {packet, raw},
                                    {send_timeout, 1000}]),
    otp_7731_send(Socket),
    io:format("Sending complete...\n",[]),
    ServerPid ! {self(), recv},
    receive {ServerPid, ok} -> ok end,

    io:format("Client waiting for leaking messages...\n",[]),

    %% Now make sure inet_drv does not leak any internal messages.
    receive Msg ->
	    ct:fail({unexpected, Msg})
    after 1000 ->
	    ok
    end,
    io:format("No leaking messages. Done.\n",[]),
    gen_tcp:close(Socket).
    
otp_7731_send(Socket) ->
    Bin = <<1:10000>>,
    io:format("Client sending ~p bytes...\n",[size(Bin)]),
    case gen_tcp:send(Socket, Bin) of
        ok -> otp_7731_send(Socket);
        {error,timeout} -> ok
    end.

otp_7731_server(ClientPid) ->
    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, raw},
                                       {active, false}]),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    io:format("Listening on ~w with port number ~p\n", [LSocket, PortNum]),
    ClientPid ! {self(), ready, PortNum},

    {ok, CSocket} = gen_tcp:accept(LSocket),
    gen_tcp:close(LSocket),

    io:format("Server got connection, wait for recv order...\n",[]),

    receive {ClientPid, recv} -> ok end,
    
    io:format("Server start receiving...\n",[]),

    otp_7731_recv(CSocket),

    ClientPid ! {self(), ok},

    io:format("Server finished, closing...\n",[]),
    gen_tcp:close(CSocket).


otp_7731_recv(Socket) ->
    case gen_tcp:recv(Socket, 0, 1000) of
	      {ok, Bin} -> 
		  io:format("Server received ~p bytes\n",[size(Bin)]),
		  otp_7731_recv(Socket);
	      {error,timeout} ->
		  io:format("Server got receive timeout\n",[]),
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
    Server = spawn_link(?MODULE, zombie_server,[self(), Calls]),
    {Server, ready, PortNum} = receive Msg -> Msg  end,
    ?P("Ports before = ~p",[lists:sort(erlang:ports())]),
    zombie_client_loop(Calls, PortNum),
    Ports = lists:sort(zombie_collector(Calls, [])),
    Server ! terminate,
    ?P("Collected ports = ~p", [Ports]),
    [] = zombies_alive(Ports, 10),
    timer:sleep(1000),
    ?P("done"),
    ok.

zombie_client_loop(0, _) ->
    ?P("[zombie client] done"),
    ok;
zombie_client_loop(N, PortNum) when is_integer(PortNum) ->
    ?P("[zombie client][~w] try connect", [N]),
    {ok, Socket} = gen_tcp:connect("localhost", PortNum,
                                   [binary, {active, false}, {packet, raw}]),
    ?P("[zombie client] connected - now close ~p", [Socket]),
    gen_tcp:close(Socket), % to make server recv fail
    zombie_client_loop(N-1, PortNum).


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

zombie_server(Pid, Calls) ->
    ?P("[zombie server] try create listen socket with backlog: ~w", [Calls]),
    {ok, LSock} = gen_tcp:listen(0, [binary, {packet, raw},
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
    Client = self(),
    Server = spawn_link(fun()-> otp_7816_server(Client) end),
    receive {Server, ready, PortNum} -> ok end,

    {ok, Socket} = gen_tcp:connect("localhost", PortNum,
                                   [binary, {active, false}, {packet, 4},
                                    {send_timeout, 10}]),
    %% We use the undocumented feature that sending can be resumed after
    %% a send_timeout without any data loss if the peer starts to receive data.
    %% Unless of course the 7816-bug is in affect, in which case the write event
    %% for the socket is lost on windows and not all data is sent.

    [otp_7816_send(Socket,18,BinSize,Server) || BinSize <- lists:seq(1000, 2000, 123)],

    io:format("Sending complete...\n",[]),

    ok = gen_tcp:close(Socket),
    Server ! {self(), closed},
    {Server, closed} = receive M -> M end.

    
otp_7816_send(Socket, BinNr, BinSize, Server) ->
    Data = lists:duplicate(BinNr, <<1:(BinSize*8)>>),
    SentBytes = otp_7816_send_data(Socket, Data, 0) * BinNr * BinSize,
    io:format("Client sent ~p bytes...\n",[SentBytes]),
    Server ! {self(),recv,SentBytes},
    {Server, ok} = receive M -> M end.

    

otp_7816_send_data(Socket, Data, Loops) ->
    io:format("Client sending data...\n",[]),
    case gen_tcp:send(Socket, Data) of
	ok ->
	    otp_7816_send_data(Socket,Data, Loops+1);
	{error,timeout} ->
	    Loops+1
    end.
    
    
otp_7816_server(Client) ->
    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, 4},
					     {active, false}]),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    io:format("Listening on ~w with port number ~p\n", [LSocket, PortNum]),
    Client ! {self(), ready, PortNum},

    {ok, CSocket} = gen_tcp:accept(LSocket),
    io:format("Server got connection...\n",[]),
    gen_tcp:close(LSocket),

    otp_7816_server_loop(CSocket),

    io:format("Server terminating.\n",[]).


otp_7816_server_loop(CSocket) ->
    io:format("Server waiting for order...\n",[]),

    receive 
	{Client, recv, RecvBytes} -> 
	    io:format("Server start receiving...\n",[]),

	    ok = otp_7816_recv(CSocket, RecvBytes),
	    
	    Client ! {self(), ok},
	    otp_7816_server_loop(CSocket);
	
	{Client, closed} ->
	    {error, closed} = gen_tcp:recv(CSocket, 0, 1000),
	    Client ! {self(), closed}
    end.


otp_7816_recv(_, 0) ->
    io:format("Server got all.\n",[]),
    ok;
otp_7816_recv(CSocket, BytesLeft) ->
    case gen_tcp:recv(CSocket, 0, 1000) of
	      {ok, Bin} when byte_size(Bin) =< BytesLeft -> 
		  io:format("Server received ~p of ~p bytes.\n",[size(Bin), BytesLeft]),
		  otp_7816_recv(CSocket, BytesLeft - byte_size(Bin));
	      {error,timeout} ->
		  io:format("Server got receive timeout when expecting more data\n",[]),
		  error
	  end.

%% Receive a packet with a faulty packet header.
otp_8102(Config) when is_list(Config) ->
    {ok, LSocket} = gen_tcp:listen(0, []),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    io:format("Listening on ~w with port number ~p\n", [LSocket, PortNum]),

    [otp_8102_do(LSocket, PortNum, otp_8102_packet(Type,Size))
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

otp_8102_do(LSocket, PortNum, {Bin,PType}) ->

    io:format("Connect with packet option ~p ...\n",[PType]),
    {ok, RSocket} = gen_tcp:connect("localhost", PortNum, [binary,
								 {packet,PType},
								 {active,true}]),
    {ok, SSocket} = gen_tcp:accept(LSocket),

    io:format("Got connection, sending ~p...\n",[Bin]),

    ok = gen_tcp:send(SSocket, Bin),

    io:format("Sending complete...\n",[]),

    {tcp_error,RSocket,emsgsize} = receive M -> M end,

    io:format("Got error msg, ok.\n",[]),
    gen_tcp:close(SSocket),    
    gen_tcp:close(RSocket).

%% Verify packet_size handles long HTTP header lines.
otp_9389(Config) when is_list(Config) ->
    {ok, LS} = gen_tcp:listen(0, [{active,false}]),
    {ok, {_, PortNum}} = inet:sockname(LS),
    io:format("Listening on ~w with port number ~p\n", [LS, PortNum]),
    OrigLinkHdr = "/" ++ string:chars($S, 8192),
    _Server = spawn_link(
                fun() ->
                        {ok, S} = gen_tcp:accept(LS),
                        ok = inet:setopts(S, [{packet_size, 16384}]),
                        ok = otp_9389_loop(S, OrigLinkHdr),
                        ok = gen_tcp:close(S)
                end),
    {ok, S} = gen_tcp:connect("localhost", PortNum,
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
    [{timetrap,{minutes,10}}].

%% Check that 64bit octet counters work.
wrapping_oct(Config) when is_list(Config) ->
    {ok,Sock} = gen_tcp:listen(0,[{active,false},{mode,binary}]),
    {ok,Port} = inet:port(Sock),
    spawn_link(?MODULE,oct_acceptor,[Sock]),
    Res = oct_datapump(Port,16#10000FFFF),
    gen_tcp:close(Sock),
    ok = Res,
    ok.

oct_datapump(Port,N) ->
    {ok,Sock} = gen_tcp:connect("localhost",Port,
				[{active,false},{mode,binary}]),
    oct_pump(Sock,N,binary:copy(<<$a:8>>,100000),0).

oct_pump(S,N,_,_) when N =< 0 ->
    gen_tcp:close(S),
    ok;
oct_pump(S,N,Bin,Last) ->
    case gen_tcp:send(S,Bin) of
	ok ->
	    {ok,Stat}=inet:getstat(S),
	    {_,R}=lists:keyfind(send_oct,1,Stat),
	    case (R < Last) of
		true ->
		    io:format("ERROR (output) ~p < ~p~n",[R,Last]),
		    output_counter_error;
		false ->
		    oct_pump(S,N-byte_size(Bin),Bin,R)
	    end;
	_ ->
	    input_counter_error
    end.
    

oct_acceptor(Sock) ->
    {ok,Data} = gen_tcp:accept(Sock),
    oct_aloop(Data,0,0).

oct_aloop(S,X,Times) ->
    case gen_tcp:recv(S,0) of
	{ok,_} ->
	    {ok,Stat}=inet:getstat(S),
	    {_,R}=lists:keyfind(recv_oct,1,Stat),
	    case (R < X) of
		true ->
		    io:format("ERROR ~p < ~p~n",[R,X]),
		    gen_tcp:close(S),
		    input_counter_error;
		false ->
		    case Times rem 16#FFFFF of
			0 ->
			    io:format("Read: ~p~n",[R]);
			_ ->
			    ok
		    end,
		    oct_aloop(S,R,Times+1)
	    end;
	_ ->
	    gen_tcp:close(S),
	    closed
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
            {ok, Listener} = gen_tcp:listen(0, [{exit_on_close, false}]),
            {ok, Port} = inet:port(Listener),

            spawn_link(
                fun() ->
                    {ok, Client} = gen_tcp:connect("localhost", Port,
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

otp_12242(Config) when is_list(Config) ->
    case os:type() of
        {win32,_} ->
            %% Even if we set sndbuf and recbuf to small sizes
            %% Windows either happily accepts to send GBytes of data
            %% in no time, so the second send below that is supposed
            %% to time out just succedes, or the first send that
            %% is supposed to fill the inet_drv I/O queue and
            %% start waiting for when more data can be sent
            %% instead sends all data but suffers a send
            %% failure that closes the socket
            {skipped,backpressure_broken_on_win32};
        _ ->
            %% Find the IPv4 address of an up and running interface
            %% that is not loopback nor pointtopoint
            {ok,IFList} = inet:getifaddrs(),
            ct:pal("IFList ~p~n", [IFList]),
            case
                lists:flatten(
                  [lists:filtermap(
                     fun ({addr,Addr}) when tuple_size(Addr) =:= 4 ->
                             {true,Addr};
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
                    otp_12242(Addr);
                Other ->
                    {skipped,{no_external_address,Other}}
            end
    end;
%%
otp_12242(Addr) when tuple_size(Addr) =:= 4 ->
    ct:timetrap(30000),
    ct:pal("Using address ~p~n", [Addr]),
    Bufsize = 16 * 1024,
    Datasize = 128 * 1024 * 1024, % At least 1 s on GBit interface
    Blob = binary:copy(<<$x>>, Datasize),
    LOpts =
        [{backlog,4},{reuseaddr,true},{ip,Addr},
         binary,{active,false},
         {recbuf,Bufsize},{sndbuf,Bufsize},{buffer,Bufsize}],
    COpts =
        [binary,{active,false},{ip,Addr},
         {linger,{true,1}}, % 1 s
         {send_timeout,500},
         {recbuf,Bufsize},{sndbuf,Bufsize},{buffer,Bufsize}],
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,ListenerNode} =
        test_server:start_node(
          ?UNIQ_NODE_NAME, slave, [{args,"-pa " ++ Dir}]),
    Tester = self(),
    Listener =
        spawn(
          ListenerNode,
          fun () ->
                  {ok,L} = gen_tcp:listen(0, LOpts),
                  {ok,LPort} = inet:port(L),
                  Tester ! {self(),port,LPort},
                  {ok,A} = gen_tcp:accept(L),
                  ok = gen_tcp:close(L),
                  receive
                      {Tester,stop} ->
                          ok = gen_tcp:close(A)
                  end
          end),
    ListenerMref = monitor(process, Listener),
    LPort = receive {Listener,port,P} -> P end,
    {ok,C} = gen_tcp:connect(Addr, LPort, COpts, infinity),
    {ok,ReadCOpts} = inet:getopts(C, [recbuf,sndbuf,buffer]),
    ct:pal("ReadCOpts ~p~n", [ReadCOpts]),
    %%
    %% Fill the buffers
    ct:pal("Sending ~p bytes~n", [Datasize]),
    ok = gen_tcp:send(C, Blob),
    ct:pal("Sent ~p bytes~n", [Datasize]),
    %% Spawn the Closer,
    %% try to ensure that the close call is in progress
    %% before the owner proceeds with sending
    Owner = self(),
    {_Closer,CloserMref} =
        spawn_opt(
          fun () ->
                  Owner ! {tref, erlang:start_timer(50, Owner, closing)},
                  ct:pal("Calling gen_tcp:close(C)~n"),
                  try gen_tcp:close(C) of
                      Result ->
                          ct:pal("gen_tcp:close(C) -> ~p~n", [Result]),
                          ok = Result
                  catch
                      Class:Reason:Stacktrace ->
                          ct:pal(
                            "gen_tcp:close(C) >< ~p:~p~n    ~p~n",
                            [Class,Reason,Stacktrace]),
                          erlang:raise(Class, Reason, Stacktrace)
                  end
          end, [link,monitor]),
    receive
        {tref,Tref} ->
            receive {timeout,Tref,_} -> ok end,
            ct:pal("Sending ~p bytes again~n", [Datasize]),
            %% Now should the close be in progress...
            %% All buffers are full, remote end is not reading,
            %% and the send timeout is 1 s so this will timeout:
            {error,timeout} = gen_tcp:send(C, Blob),
            ct:pal("Sending ~p bytes again timed out~n", [Datasize]),
            ok = inet:setopts(C, [{send_timeout,10000}]),
            %% There is a hidden timeout here.  Port close is sampled
            %% every 5 s by prim_inet:send_recv_reply.
            %% Linger is 3 s so the Closer will finish this send:
            ct:pal("Sending ~p bytes with 10 s timeout~n", [Datasize]),
            {error,closed} = gen_tcp:send(C, Blob),
            ct:pal("Sending ~p bytes with 10 s timeout was closed~n",
                   [Datasize]),
            normal = wait(CloserMref),
            ct:pal("The Closer has exited~n"),
            Listener ! {Tester,stop},
            receive {'DOWN',ListenerMref,_,_,_} -> ok end,
            ct:pal("The Listener has exited~n"),
            test_server:stop_node(ListenerNode),
            ok
    end.

wait(Mref) ->
    receive {'DOWN',Mref,_,_,Reason} -> Reason end.

%% OTP-15536
%% Test that send error works correctly for delay_send
delay_send_error(_Config) ->
    ?P("create listen socket"),
    {ok, L} =
        gen_tcp:listen(
          0, [{reuseaddr, true}, {packet, 1}, {active, false}]),
    {ok,{{0,0,0,0},PortNum}}=inet:sockname(L),
    ?P("try connect - with delay_send:true"),
    {ok, C} =
        gen_tcp:connect(
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

