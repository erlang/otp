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
-module(gen_tcp_misc_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 
	 controlling_process/1, controlling_process_self/1,
	 no_accept/1, close_with_pending_output/1, active_n/1,
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
	 econnreset_after_async_send_passive/1, linger_zero/1,
	 default_options/1, http_bad_packet/1, 
	 busy_send/1, busy_disconnect_passive/1, busy_disconnect_active/1,
	 fill_sendq/1, partial_recv_and_close/1, 
	 partial_recv_and_close_2/1,partial_recv_and_close_3/1,so_priority/1,
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
	 wrapping_oct/0, wrapping_oct/1,
         otp_9389/1]).

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
     iter_max_socks, passive_sockets, active_n,
     accept_closed_by_other_process, otp_3924, closed_socket,
     shutdown_active, shutdown_passive, shutdown_pending,
     show_econnreset_active, show_econnreset_active_once,
     show_econnreset_passive, econnreset_after_sync_send,
     econnreset_after_async_send_active,
     econnreset_after_async_send_active_once,
     econnreset_after_async_send_passive, linger_zero,
     default_options, http_bad_packet, busy_send,
     busy_disconnect_passive, busy_disconnect_active,
     fill_sendq, partial_recv_and_close,
     partial_recv_and_close_2, partial_recv_and_close_3,
     so_priority, primitive_accept,
     multi_accept_close_listen, accept_timeout,
     accept_timeouts_in_order, accept_timeouts_in_order2,
     accept_timeouts_in_order3, accept_timeouts_in_order4,
     accept_timeouts_in_order5, accept_timeouts_in_order6,
     accept_timeouts_in_order7, accept_timeouts_mixed,
     killing_acceptor, killing_multi_acceptors,
     killing_multi_acceptors2, several_accepts_in_one_go, accept_system_limit,
     active_once_closed, send_timeout, send_timeout_active, otp_7731,
     wrapping_oct,
     zombie_sockets, otp_7816, otp_8102, otp_9389].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

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
    {ok,S}=gen_tcp:connect("localhost",PortNum,[{delay_send,true}]),
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
		    {error,einval} = inet:port(Sock)
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
    C = ok(gen_tcp:connect("localhost", Port, [{active,N}])),
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
    N = case os:type() of {win32,_} -> 10; _ -> 20 end,
    %% Run on a different node in order to limit the effect if this test fails.
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(test_iter_max_socks,slave,
				       [{args,"+Q 2048 -pa " ++ Dir}]),
    L = rpc:call(Node,?MODULE,do_iter_max_socks,[N, initalize]),
    test_server:stop_node(Node),

    io:format("Result: ~p",[L]),
    all_equal(L),
    {comment, "Max sockets: " ++ integer_to_list(hd(L))}.

do_iter_max_socks(0, _) ->
    [];
do_iter_max_socks(N, initalize) ->
    MS = max_socks(),
    [MS|do_iter_max_socks(N-1, MS)];
do_iter_max_socks(N, failed) ->
    MS = max_socks(),
    [MS|do_iter_max_socks(N-1, failed)];
do_iter_max_socks(N, First) when is_integer(First) ->
    MS = max_socks(),
    if MS == First -> 
	    [MS|do_iter_max_socks(N-1, First)];
       true ->
	    io:format("Sleeping for ~p seconds...~n",
			    [?RETRY_SLEEP/1000]), 
	    ct:sleep(?RETRY_SLEEP),
	    io:format("Trying again...~n", []),
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
    io:format("Got ~p sockets", [N]),
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
%% connecting, side still can read until the end of data.
passive_sockets(Config) when is_list(Config) ->
    spawn_link(?MODULE, passive_sockets_server,
               [[{active,false}],self()]),
    receive
        {socket,Port} -> ok
    end,
    ct:sleep(500),
    case gen_tcp:connect("localhost", Port, [{active, false}]) of
        {ok, Sock} ->
            passive_sockets_read(Sock);
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
    shutdown_common(true).

shutdown_passive(Config) when is_list(Config) ->
    shutdown_common(false).

shutdown_common(Active) ->
    P = sort_server(Active),
    io:format("Sort server port: ~p\n", [P]),

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
    List = [El++"\n" || El <- List0],
    {ok,S} = gen_tcp:connect(localhost, P, [{packet,line}]),
    send_lines(S, List),
    gen_tcp:shutdown(S, write),
    Lines = collect_lines(S, true),
    io:format("~p\n", [Lines]),
    Lines = lists:sort(List),
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
	{tcp,S,Line} -> collect_lines_1(S, [Line|Acc]);
	{tcp_closed,S} -> lists:reverse(Acc)
    end.

passive_collect_lines_1(S, Acc) ->
    case gen_tcp:recv(S, 0) of
	{ok,Line} -> passive_collect_lines_1(S, [Line|Acc]);
	{error,closed} -> lists:reverse(Acc)
    end.


send_lines(S, Lines) ->    
    lists:foreach(fun(Line) ->
			  gen_tcp:send(S, Line)
		  end, Lines).

%%%
%%% Shutdown pending.
%%%

shutdown_pending(Config) when is_list(Config) ->
    N = 512*1024+17,
    io:format("~p\n", [N]),
    Data = [<<N:32>>,ones(N),42],
    P = a_server(),
    io:format("Server port: ~p\n", [P]),
    {ok,S} = gen_tcp:connect(localhost, P, []),
    gen_tcp:send(S, Data),
    gen_tcp:shutdown(S, write),
    receive
        {tcp,S,Msg} ->
            io:format("~p\n", [Msg]),
            N = list_to_integer(Msg) - 5;
        Other ->
            ct:fail({unexpected,Other})
    end,
    ok.

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
     {ok,L} = gen_tcp:listen(0, [{exit_on_close,false},{active,false}]),
     Pid = spawn_link(fun() -> a_server(L) end),
     ok = gen_tcp:controlling_process(L, Pid),
     {ok,Port} = inet:port(L),
     Port.

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
    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(Client, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client),
    receive
       {tcp_closed, S} ->
	   ok;
       Other ->
	   ct:fail({unexpected1, Other})
    after 1000 ->
	ct:fail({timeout, {server, no_tcp_closed}})
    end,

    %% Now test with option switched on.
    %% Note: We are also testing that the show_econnreset option is
    %% inherited from the listening socket by the accepting socket.
    {ok, L1} = gen_tcp:listen(0, [{show_econnreset, true}]),
    {ok, Port1} = inet:port(L1),
    {ok, Client1} = gen_tcp:connect(localhost, Port1, [{active, false}]),
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(Client1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client1),
    receive
	{tcp_error, S1, econnreset} ->
	    receive
		{tcp_closed, S1} ->
		    ok;
		Other1 ->
		    ct:fail({unexpected2, Other1})
	    after 1 ->
		ct:fail({timeout, {server, no_tcp_closed}})
	    end;
	Other2 ->
	    ct:fail({unexpected3, Other2})
    after 1000 ->
	ct:fail({timeout, {server, no_tcp_error}})
    end.

show_econnreset_active_once(Config) when is_list(Config) ->
    %% Now test using {active, once}
    {ok, L} = gen_tcp:listen(0,
			   [{active, false},
			    {show_econnreset, true}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
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
    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(1),
    {error, closed} = gen_tcp:recv(Client, 0),

    %% Now test with option switched on.
    {ok, L1} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    {ok, Client1} = gen_tcp:connect(localhost, Port1,
				 [{active, false},
				  {show_econnreset, true}]),
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(1),
    {error, econnreset} = gen_tcp:recv(Client1, 0).

econnreset_after_sync_send(Config) when is_list(Config) ->
    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{active, false}]),
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    ok = inet:setopts(S, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S),
    ok = ct:sleep(20),
    {error, closed} = gen_tcp:send(Client, "Whatever"),

    %% Now test with option switched on.
    {ok, L1} = gen_tcp:listen(0, [{active, false}]),
    {ok, Port1} = inet:port(L1),
    {ok, Client1} = gen_tcp:connect(localhost, Port1,
          			  [{active, false},
          			   {show_econnreset, true}]),
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(20),
    {error, econnreset} = gen_tcp:send(Client1, "Whatever").

econnreset_after_async_send_active(Config) when is_list(Config) ->
    {OS, _} = os:type(),
    Payload = lists:duplicate(1024 * 1024, $.),

    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port, [{sndbuf, 4096}]),
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
    {ok, L1} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port1} = inet:port(L1),
    {ok, Client1} = gen_tcp:connect(localhost, Port1,
				  [{sndbuf, 4096},
				   {show_econnreset, true}]),
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
    {OS, _} = os:type(),
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port,
				   [{active, false},
				    {sndbuf, 4096},
				    {show_econnreset, true}]),
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
    {OS, _} = os:type(),
    Payload = lists:duplicate(1024 * 1024, $.),

    %% First confirm everything works with option turned off.
    {ok, L} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port,
					 [{active, false},
					  {sndbuf, 4096}]),
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
    {ok, L1} = gen_tcp:listen(0, [{active, false}, {recbuf, 4096}]),
    {ok, Port1} = inet:port(L1),
    {ok, Client1} = gen_tcp:connect(localhost, Port1,
				   [{active, false},
				    {sndbuf, 4096},
				    {show_econnreset, true}]),
    {ok, S1} = gen_tcp:accept(L1),
    ok = gen_tcp:close(L1),
    ok = inet:setopts(S1, [{linger, {true, 0}}]),
    ok = gen_tcp:send(S1, "Whatever"),
    ok = gen_tcp:send(Client1, Payload),
    ok = gen_tcp:close(S1),
    ok = ct:sleep(20),
    {error, econnreset} = gen_tcp:recv(Client1, 0).

%%
%% Test {linger {true, 0}} aborts a connection
%%

linger_zero(Config) when is_list(Config) ->
    %% All the econnreset tests will prove that {linger, {true, 0}} aborts
    %% a connection when the driver queue is empty. We will test here
    %% that it also works when the driver queue is not empty.
    {OS, _} = os:type(),
    {ok, L} = gen_tcp:listen(0, [{active, false},
				 {recbuf, 4096},
				 {show_econnreset, true}]),
    {ok, Port} = inet:port(L),
    {ok, Client} = gen_tcp:connect(localhost, Port,
				   [{active, false},
				    {sndbuf, 4096}]),
    {ok, S} = gen_tcp:accept(L),
    ok = gen_tcp:close(L),
    PayloadSize = 1024 * 1024,
    Payload = lists:duplicate(PayloadSize, $.),
    ok = gen_tcp:send(Client, Payload),
    case erlang:port_info(Client, queue_size) of
	{queue_size, N} when N > 0 -> ok;
	{queue_size, 0} when OS =:= win32 -> ok;
	{queue_size, 0} = T -> ct:fail(T)
    end,
    ok = inet:setopts(Client, [{linger, {true, 0}}]),
    ok = gen_tcp:close(Client),
    ok = ct:sleep(1),
    undefined = erlang:port_info(Client, connected),
    {error, econnreset} = gen_tcp:recv(S, PayloadSize).


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
    Master = self(),
    Msg = <<"the quick brown fox jumps over a lazy dog~n">>,
    Server =
	spawn_link(fun () ->
			   {ok,L} = gen_tcp:listen
				      (0, [{active,false},binary,
					   {reuseaddr,true},{packet,0}]),
			   {ok,Port} = inet:port(L),
			   Master ! {self(),client,
				     busy_send_client(Port, Master, Msg)},
			   busy_send_srv(L, Master, Msg)
		   end),
    io:format("~p Server~n", [Server]),
    receive
        {Server,client,Client} ->
            io:format("~p Client~n", [Client]),
            busy_send_loop(Server, Client, 0)
    end.

busy_send_loop(Server, Client, N) ->
    %% Master
    %%
    receive {Server,send} ->
		  busy_send_loop(Server, Client, N+1)
	  after 2000 ->
		  %% Send queue full, sender blocked 
		  %% -> stop sender and release client
		  io:format("Send timeout, time to receive...~n", []),
		  Server ! {self(),close},
		  Client ! {self(),recv,N+1},
                  receive
                      {Server,send} ->
                          busy_send_2(Server, Client, N+1)
                  after 10000 ->
                            %% If this happens, see busy_send_srv
                            ct:fail({timeout,{server,not_send,flush([])}})
                  end
    end.

busy_send_2(Server, Client, _N) ->
    %% Master
    %%
    receive
        {Server,[closed]} ->
            receive {Client,[0,{error,closed}]} -> ok end
    after 10000 ->
              ct:fail({timeout,{server,not_closed,flush([])}})
    end.

busy_send_srv(L, Master, Msg) ->
    %% Server
    %% Sometimes this accept does not return, do not really know why
    %% but is causes the timeout error in busy_send_loop to be
    %% triggered. Only happens on OS X Leopard?!?
    {ok,Socket} = gen_tcp:accept(L),
    busy_send_srv_loop(Socket, Master, Msg).

busy_send_srv_loop(Socket, Master, Msg) ->
    %% Server
    %%
    receive
	{Master,close} ->
	    ok = gen_tcp:close(Socket),
	    Master ! {self(),flush([closed])}
    after 0 ->
	    ok = gen_tcp:send(Socket, Msg),
	    Master ! {self(),send},
	    busy_send_srv_loop(Socket, Master, Msg)
    end.

busy_send_client(Port, Master, Msg) ->    
    %% Client
    %%
    spawn_link(
      fun () ->
	      {ok,Socket} = gen_tcp:connect(
			 "localhost", Port,
			 [{active,false},binary,{packet,0}]),
	      receive
		  {Master,recv, N} ->
		      busy_send_client_loop(Socket, Master, Msg, N)
	      end
      end).

busy_send_client_loop(Socket, Master, Msg, N) ->
    %% Client
    %%
    Size = byte_size(Msg),
    case gen_tcp:recv(Socket, Size) of
	{ok,Msg} ->
	    busy_send_client_loop(Socket, Master, Msg, N-1);
	Other ->
	    Master ! {self(),flush([Other,N])}
    end.

%%%
%%% Send to a socket whose other end does not read until the port gets busy.
%%% Then close the other end. The writer should get an {error,closed} error.
%%% (Passive mode.)
%%%

busy_disconnect_passive(Config) when is_list(Config) ->
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_passive(MuchoData) || _ <- lists:seq(1, 10)],
    ok.

do_busy_disconnect_passive(MuchoData) ->
    S = busy_disconnect_prepare_server([{active,false}]),
    busy_disconnect_passive_send(S, MuchoData).

busy_disconnect_passive_send(S, Data) ->
    case gen_tcp:send(S, Data) of
	ok -> busy_disconnect_passive_send(S, Data);
	{error,closed} -> ok
    end.

%%%
%%% Send to a socket whose other end does not read until the port gets busy.
%%% Then close the other end. The writer should get an {error,closed} error and
%%% a {tcp_closed,Socket} message. (Active mode.)
%%%
busy_disconnect_active(Config) when is_list(Config) ->
    MuchoData = list_to_binary(ones(64*1024)),
    [do_busy_disconnect_active(MuchoData) || _ <- lists:seq(1, 10)],
    ok.

do_busy_disconnect_active(MuchoData) ->
    S = busy_disconnect_prepare_server([{active,true}]),
    busy_disconnect_active_send(S, MuchoData).

busy_disconnect_active_send(S, Data) ->
    case gen_tcp:send(S, Data) of
	ok -> busy_disconnect_active_send(S, Data);
	{error,closed} ->
	    receive
		{tcp_closed,S} -> ok;
		_Other -> ct:fail(failed)
	    end
    end.


busy_disconnect_prepare_server(ConnectOpts) ->
    Sender = self(),
    Server = spawn_link(fun() -> busy_disconnect_server(Sender) end),
    receive {port,Server,Port} -> ok end,
    {ok,S} = gen_tcp:connect(localhost, Port, ConnectOpts),
    Server ! {Sender,sending},
    S.

busy_disconnect_server(Sender) ->
    {ok,L} = gen_tcp:listen(0, [{active,false},binary,{reuseaddr,true},{packet,0}]),
    {ok,Port} = inet:port(L),
    Sender ! {port,self(),Port},
    {ok,S} = gen_tcp:accept(L),
    receive
	{Sender,sending} ->
	    busy_disconnect_server_wait_for_busy(Sender, S)
    end.

%% Close the socket as soon as the Sender process can't send because of
%% a busy port.
busy_disconnect_server_wait_for_busy(Sender, S) ->
    case process_info(Sender, status) of
	{status,waiting} ->
	    %% We KNOW that the sender will be in state 'waiting' only
	    %% if the port has become busy. (Fallback solution if the
	    %% implementation changes: Watch Sender's reduction count;
	    %% when it stops changing, wait 2 seconds and then close.)
	    gen_tcp:close(S);
	_Other ->
	    io:format("~p\n", [_Other]),
	    timer:sleep(100),
	    busy_disconnect_server_wait_for_busy(Sender, S)
    end.

%%%
%%% Fill send queue
%%%
fill_sendq(Config) when is_list(Config) ->
    Master = self(),
    Server =
	spawn_link(fun () ->
			   {ok,L} = gen_tcp:listen
				      (0, [{active,false},binary,
					   {reuseaddr,true},{packet,0}]),
			   {ok,Port} = inet:port(L),
			   Master ! {self(),client,
				     fill_sendq_client(Port, Master)},
			   fill_sendq_srv(L, Master)
		   end),
    io:format("~p Server~n", [Server]),
    receive {Server,client,Client} ->
		  io:format("~p Client~n", [Client]),
		  receive {Server,reader,Reader} ->
				io:format("~p Reader~n", [Reader]),
				fill_sendq_loop(Server, Client, Reader)
	    end
    end.

fill_sendq_loop(Server, Client, Reader) ->
    %% Master
    %%
    receive {Server,send} ->
	    fill_sendq_loop(Server, Client, Reader)
    after 2000 ->
	    %% Send queue full, sender blocked -> close client.
	    io:format("Send timeout, closing Client...~n", []),
	    Client ! {self(),close},
	    receive {Server,[{error,closed}]} ->
			  io:format("Got server closed.~n"),
			  receive {Reader,[{error,closed}]} ->
					io:format
						("Got reader closed.~n"),
					ok
				after 3000 ->
					ct:fail({timeout,{closed,reader}})
				end;
			  {Reader,[{error,closed}]} ->
			  io:format("Got reader closed.~n"),
			  receive {Server,[{error,closed}]} ->
					io:format("Got server closed~n"),
					ok
				after 3000 ->
					ct:fail({timeout,{closed,server}})
				end
		  after 3000 ->
			  ct:fail({timeout,{closed,[server,reader]}})
		  end
    end.

fill_sendq_srv(L, Master) ->
    %% Server
    %%
    case gen_tcp:accept(L) of
	{ok,S} ->
	    Master ! {self(),reader,
		      spawn_link(fun () -> fill_sendq_read(S, Master) end)},
	    Msg = "the quick brown fox jumps over a lazy dog~n",
	    fill_sendq_write(S, Master, [Msg,Msg,Msg,Msg,Msg,Msg,Msg,Msg]);
	Error ->
	    io:format("~p error: ~p.~n", [self(),Error]),
	    Master ! {self(),flush([Error])}
    end.

fill_sendq_write(S, Master, Msg) ->
    %% Server
    %%
    %%io:format("~p sending...~n", [self()]),
    Master ! {self(),send},
    case gen_tcp:send(S, Msg) of
	ok ->
	    %%io:format("~p ok.~n", [self()]),
	    fill_sendq_write(S, Master, Msg);
	E ->
	    Error = flush([E]),
	    io:format("~p error: ~p.~n", [self(),Error]),
	    Master ! {self(),Error}
    end.

fill_sendq_read(S, Master) ->
    %% Reader
    %%
    io:format("~p read infinity...~n", [self()]),
    case gen_tcp:recv(S, 0, infinity) of
	{ok,Data} ->
	    io:format("~p got: ~p.~n", [self(),Data]),
	    fill_sendq_read(S, Master);
	E ->
	    Error = flush([E]),
	    io:format("~p error: ~p.~n", [self(),Error]),
	    Master ! {self(),Error}
    end.

fill_sendq_client(Port, Master) ->
    %% Client
    %%
    spawn_link(fun () ->
		       %% Just close on order
		       {ok,S} = gen_tcp:connect(
				  "localhost", Port,
				  [{active,false},binary,{packet,0}]),
		       receive
			   {Master,close} ->
			       ok = gen_tcp:close(S)
		       end
	       end).

%%% Try to receive more than available number of bytes from 
%%% a closed socket.
%%%
partial_recv_and_close(Config) when is_list(Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    {ok,P} = inet:port(L),
    {ok,S} = gen_tcp:connect("localhost", P, [{active,false}]),
    {ok,A} = gen_tcp:accept(L),
    ok = gen_tcp:send(S, Msg),
    ok = gen_tcp:close(S),
    {error,closed} = gen_tcp:recv(A, Len+1),
    ok.

%%% Try to receive more than available number of bytes from 
%%% a closed socket, this time waiting in the recv before closing.
%%%
partial_recv_and_close_2(Config) when is_list(Config) ->
    Msg = "the quick brown fox jumps over a lazy dog 0123456789\n",
    Len = length(Msg),
    {ok,L} = gen_tcp:listen(0, [{active,false}]),
    {ok,P} = inet:port(L),
    Server = self(),
    Client =
	spawn_link(
	  fun () ->
		  receive after 2000 -> ok end,
		  {ok,S} = gen_tcp:connect("localhost", P, [{active,false}]),
		  ok = gen_tcp:send(S, Msg),
		  receive {Server,close} -> ok end,
		  receive after 2000 -> ok end,
		  ok = gen_tcp:close(S)
	  end),
    {ok,A} = gen_tcp:accept(L),
    Client ! {Server,close},
    {error,closed} = gen_tcp:recv(A, Len+1),
    ok.

%%% Here we tests that gen_tcp:recv/2 will return {error,closed} following
%%% a send operation of a huge amount data when the other end closed the socket.
%%%
partial_recv_and_close_3(Config) when is_list(Config) ->
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
    {ok,S} = gen_tcp:connect(localhost, Port, [{active,false}]),

    %% Send a lot of data (most of it will be queued). The receiver will read one byte
    %% and close the connection. The write operation will fail.
    gen_tcp:send(S, Much),

    %% We should always get {error,closed} here.
    {error,closed} = gen_tcp:recv(S, 0).
    

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
    {ok,Sock2}=gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                 {active,false},
                                                 {reuseaddr,true},
                                                 {priority,4}]),
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
    {ok,Sock2}=gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                 {active,false},
                                                 {reuseaddr,true},
                                                 {priority,4},
                                                 {tos,Tos2}]),
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
    {ok,Sock2}=gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                 {active,false},
                                                 {reuseaddr,true},
                                                 {tos,Tos2}]),
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
    {ok,Sock2}=gen_tcp:connect("localhost",Port,[binary,{packet,0},
                                                 {active,false},
                                                 {reuseaddr,true},
                                                 {priority,4},
                                                 {tos,Tos2}]),
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

%% Accept test utilities (suites are below)

millis() ->
    erlang:monotonic_time(milli_seconds).
	
collect_accepts(0,_) -> [];
collect_accepts(N,Tmo) ->
    A = millis(),
    receive
	{accepted,P,Msg} ->
	    [{P,Msg}] ++ collect_accepts(N-1,Tmo-(millis() - A))
    after Tmo ->
	    []
    end.
   
-define(EXPECT_ACCEPTS(Pattern,N,Timeout),
	(fun() ->
                 case collect_accepts(if N =:= infinity -> -1; true -> N end,Timeout) of
		     Pattern ->
			 ok;
		     Other ->
			 {error,{unexpected,{Other,process_info(self(),messages)}}}
		 end
	 end)()).
	
collect_connects(Tmo) ->
    A = millis(),
    receive
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
    {ok,LS}=gen_tcp:listen(0,[]),
    {ok,PortNo}=inet:port(LS),
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    P = spawn(F),
    gen_tcp:connect("localhost",PortNo,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS)} end,
    spawn(F),
    spawn(F),
    spawn(F),
    spawn(F),
    gen_tcp:close(LS),
    ok = ?EXPECT_ACCEPTS([{_,{error,closed}},{_,{error,closed}},
                          {_,{error,closed}},{_,{error,closed}}],4,500).
	
%% Single accept with timeout.
accept_timeout(Config) when is_list(Config) ->
    {ok,LS}=gen_tcp:listen(0,[]),
    Parent = self(),
    F = fun() -> Parent ! {accepted,self(),gen_tcp:accept(LS,1000)} end,
    P = spawn(F),
    ok = ?EXPECT_ACCEPTS([{P,{error,timeout}}],1,2000).

%% Check that multi-accept timeouts happen in the correct order.
accept_timeouts_in_order(Config) when is_list(Config) ->
    {ok,LS}=gen_tcp:listen(0,[]),
    Parent = self(),
    P1 = spawn(mktmofun(1000,Parent,LS)),
    P2 = spawn(mktmofun(1200,Parent,LS)),
    P3 = spawn(mktmofun(1300,Parent,LS)),
    P4 = spawn(mktmofun(1400,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P1,{error,timeout}},{P2,{error,timeout}},
                          {P3,{error,timeout}},{P4,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order (more).
accept_timeouts_in_order2(Config) when is_list(Config) ->
    {ok,LS}=gen_tcp:listen(0,[]),
    Parent = self(),
    P1 = spawn(mktmofun(1400,Parent,LS)),
    P2 = spawn(mktmofun(1300,Parent,LS)),
    P3 = spawn(mktmofun(1200,Parent,LS)),
    P4 = spawn(mktmofun(1000,Parent,LS)),
    ok = ?EXPECT_ACCEPTS([{P4,{error,timeout}},{P3,{error,timeout}},
                          {P2,{error,timeout}},{P1,{error,timeout}}],infinity,2000).

%% Check that multi-accept timeouts happen in the correct order (even more).
accept_timeouts_in_order3(Config) when is_list(Config) ->
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,_}=gen_tcp:connect("localhost",PortNo,[]),
    ok = ?EXPECT_ACCEPTS([{P2,{ok,Port0}}] when is_port(Port0),infinity,100),
    ok = ?EXPECT_ACCEPTS([{P3,{error,timeout}}],infinity,2000),
    gen_tcp:connect("localhost",PortNo,[]),
    ok = ?EXPECT_ACCEPTS([{P4,{ok,Port1}}] when is_port(Port1),infinity,100).

%% Check that single acceptor behaves as expected when killed.
killing_acceptor(Config) when is_list(Config) ->    
    {ok,LS}=gen_tcp:listen(0,[]),
    Pid = spawn(fun() -> erlang:display({accepted,self(),gen_tcp:accept(LS)}) end),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
    {ok,LS}=gen_tcp:listen(0,[]),
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
        {current_function,{prim_inet,accept0,2}} ->
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
    {ok, LS} = gen_tcp:listen(0, []),
    {ok, TcpPort} = inet:port(LS),
    Me = self(),
    Connector = spawn_link(fun () -> connector(TcpPort, Me) end),
    receive {Connector, sync} -> Connector ! {self(), continue} end,
    ok = acceptor(LS, false, []),
    Connector ! stop,
    ok.

acceptor(LS, GotSL, A) ->
    case gen_tcp:accept(LS, 1000) of
	{ok, S} ->
	    acceptor(LS, GotSL, [S|A]);
	{error, system_limit} ->
	    acceptor(LS, true, A);
	{error, timeout} when GotSL ->
	    ok;
	{error, timeout} ->
	    error
    end.

connector(TcpPort, Tester) ->
    ManyPorts = open_ports([]),
    Tester ! {self(), sync},
    receive {Tester, continue} -> timer:sleep(100) end,
    ConnF = fun (Port) ->
		    case catch gen_tcp:connect({127,0,0,1}, TcpPort, []) of
			{ok, Sock} ->
			    Sock;
			_Error ->
			    port_close(Port)
		    end
	    end,
    R = [ConnF(Port) || Port <- lists:sublist(ManyPorts, 10)],
    receive stop -> R end.

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
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
     (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end,
	     {error,einval} = inet:setopts(A,[{active,true}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end
     end)(),
    (fun() ->
	     {Loop,A} = setup_closed_ao(),
	     Loop({{error,closed},{error,econnaborted}},
			fun() -> gen_tcp:send(A,"Hello") end),
	     ok = inet:setopts(A,[{active,false}]),
	     ok = receive {tcp_closed, A} -> error after 1000 -> ok end,
	     ok = inet:setopts(A,[{active,once}]),
	     ok = receive {tcp_closed, A} -> ok after 1000 -> error end
     end)().
   
%% Test the send_timeout socket option.
send_timeout(Config) when is_list(Config) ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,RNode} = test_server:start_node(?UNIQ_NODE_NAME, slave,
					[{args,"-pa " ++ Dir}]),

    %% Basic
    send_timeout_basic(false, RNode),
    send_timeout_basic(true, RNode),

    BinData = <<1:10000>>,

    %% Check timeout length.
    Self = self(),
    Pid = spawn(fun() ->
                        A = setup_timeout_sink(RNode, 1000, true),
			Send = fun() ->
				       Res = gen_tcp:send(A, BinData),
				       Self ! Res,
				       Res
			       end,
			{error,timeout} = timeout_sink_loop(Send)
	      end),
    Diff = get_max_diff(),
    io:format("Max time for send: ~p~n",[Diff]),
    true = (Diff > 500) and (Diff < 1500),

    %% Wait for the process to die.
    Mon = erlang:monitor(process, Pid),
    receive {'DOWN',Mon,process,Pid,_} -> ok end,

    %% Check that parallell writers do not hang forever
    send_timeout_para(false, RNode),
    send_timeout_para(true, RNode),

    test_server:stop_node(RNode),

    ok.

send_timeout_basic(AutoClose, RNode) ->
    BinData = <<1:10000>>,

    A = setup_timeout_sink(RNode, 1000, AutoClose),
    Send = fun() -> gen_tcp:send(A, BinData) end,
    {error,timeout} = timeout_sink_loop(Send),

    %% Check that the socket is not busy/closed...
    Error = after_send_timeout(AutoClose),
    {error,Error} = gen_tcp:send(A, <<"Hej">>),
    ok.

send_timeout_para(AutoClose, RNode) ->
    BinData = <<1:10000>>,

    A = setup_timeout_sink(RNode, 1000, AutoClose),
    Self = self(),
    SenderFun = fun() ->
			Send = fun() -> gen_tcp:send(A, BinData) end,
			{error,Error} = timeout_sink_loop(Send),
			Self ! {error,Error}
		end,
    spawn_link(SenderFun),
    spawn_link(SenderFun),

    receive
	{error,timeout} -> ok
    after 10000 ->
	    exit(timeout)
    end,

    NextErr = after_send_timeout(AutoClose),
    receive
	{error,NextErr} -> ok
    after 10000 ->
	    exit(timeout)
    end,

    {error,NextErr} = gen_tcp:send(A, <<"Hej">>),
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
    {error,timeout} = timeout_sink_loop(F),
    unlink(Mad),
    exit(Mad, kill),
    flush(),
    ok.

after_send_timeout(AutoClose) ->
    case AutoClose of
	true -> enotconn;
	false -> timeout
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
	    
setup_closed_ao() ->
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,R} = test_server:start_node(?UNIQ_NODE_NAME, slave,
                                    [{args,"-pa " ++ Dir}]),
    Host = get_hostname(node()),
    {ok, L} = gen_tcp:listen(0, [{active,false},{packet,2}]),
    Fun = fun(F) ->
                  receive
                      {From,X} when is_function(X) ->
                          From ! {self(),X()}, F(F);
                      die -> ok
                  end
          end,
    Pid =  rpc:call(R,erlang,spawn,[fun() -> Fun(Fun) end]),
    {ok, Port} = inet:port(L),
    Remote = fun(Fu) ->
                     Pid ! {self(), Fu},
                     receive {Pid,X} -> X
                     end
             end,
    {ok, C} = Remote(fun() -> 
			     gen_tcp:connect(Host,Port,
					     [{active,false},{packet,2}]) 
		     end),
    {ok,A} = gen_tcp:accept(L),
    gen_tcp:send(A,"Hello"),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
    ok =  Remote(fun() -> gen_tcp:close(C) end),
    Loop2 = fun(_,_,_,0) ->
		    {failure, timeout}; 
	       (L2,{MA,MB},F2,N) ->
		    case F2() of
			MA -> MA;
			MB -> MB;
			Other -> io:format("~p~n",[Other]),
				 receive after 1000 -> ok end,
				 L2(L2,{MA,MB},F2,N-1)
		    end
	    end,
    Loop = fun(Match2,F3) ->  Loop2(Loop2,Match2,F3,10) end,
    test_server:stop_node(R),
    {Loop,A}.
    
setup_timeout_sink(RNode, Timeout, AutoClose) ->
    Host = get_hostname(node()),
    {ok, L} = gen_tcp:listen(0, [{active,false},{packet,2},
				 {send_timeout,Timeout},
				 {send_timeout_close,AutoClose}]),
    Fun = fun(F) ->
		  receive
		      {From,X} when is_function(X) ->
			  From ! {self(),X()}, F(F);
		      die -> ok
		  end
	  end,
    Pid =  rpc:call(RNode, erlang, spawn, [fun() -> Fun(Fun) end]),
    {ok, Port} = inet:port(L),
    Remote = fun(Fu) ->
		     Pid ! {self(), Fu},
		     receive {Pid,X} -> X
		     end
	     end,
    {ok, C} = Remote(fun() ->
			     gen_tcp:connect(Host,Port,
					     [{active,false},{packet,2}])
		     end),
    {ok,A} = gen_tcp:accept(L),
    gen_tcp:send(A,"Hello"),
    {ok, "Hello"} = Remote(fun() -> gen_tcp:recv(C,0) end),
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
    {ok,A} = gen_tcp:accept(L),
    gen_tcp:send(A, "Hello"),
    {ok, "H"++_} = Remote(fun() -> gen_tcp:recv(C, 0) end),
    {A,C}.

timeout_sink_loop(Action) ->
    Ret = Action(),
    case Ret of
	ok ->
	    receive after 1 -> ok end,
	    timeout_sink_loop(Action);
	Other ->
	    Other
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
    Server = spawn_link(?MODULE, zombie_server,[self(), Calls]),
    {Server, ready, PortNum} = receive Msg -> Msg  end,
    io:format("Ports before = ~p\n",[lists:sort(erlang:ports())]),
    zombie_client_loop(Calls, PortNum),
    Ports = lists:sort(zombie_collector(Calls,[])),
    Server ! terminate,
    io:format("Collected ports = ~p\n",[Ports]),
    [] = zombies_alive(Ports, 10),
    timer:sleep(1000),
    ok.

zombie_client_loop(0, _) -> ok;
zombie_client_loop(N, PortNum) when is_integer(PortNum) ->
    {ok, Socket} = gen_tcp:connect("localhost", PortNum,
                                   [binary, {active, false}, {packet, raw}]),
    gen_tcp:close(Socket), % to make server recv fail
    zombie_client_loop(N-1, PortNum).


zombie_collector(0,Acc) ->
    Acc;
zombie_collector(N,Acc) ->
    receive 
	{closed, Socket} -> 
	    zombie_collector(N-1,[Socket|Acc]);
	E -> 
	    {unexpected, E, Acc}
    end.
	    
zombies_alive(Ports, WaitSec) ->
    Alive = lists:sort(erlang:ports()),
    io:format("Alive = ~p\n",[Alive]),
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
    {ok, LSocket} = gen_tcp:listen(0, [binary, {packet, raw},
                                       {active, false}, {backlog, Calls}]),
    {ok, {_, PortNum}} = inet:sockname(LSocket),
    io:format("Listening on ~w with port number ~p\n", [LSocket, PortNum]),
    BigBin = list_to_binary(lists:duplicate(100*1024, 77)),
    Pid ! {self(), ready, PortNum},
    zombie_accept_loop(LSocket, BigBin, Calls),
    terminate = receive Msg -> Msg end.

zombie_accept_loop(_, _, 0) ->
    ok;
zombie_accept_loop(Socket, BigBin, Calls) ->
    case gen_tcp:accept(Socket) of
	{ok, NewSocket} ->
	    spawn_link(fun() -> zombie_serve_client(NewSocket, BigBin) end),
	    zombie_accept_loop(Socket, BigBin, Calls-1);
	E ->
	    E
    end.

zombie_serve_client(Socket, Bin) ->
    %%io:format("Got connection on ~p\n",[Socket]),
    gen_tcp:send(Socket, Bin),
    %%io:format("Sent data, waiting for reply on ~p\n",[Socket]),
    case gen_tcp:recv(Socket, 4) of
	      {error,closed} -> ok;
	      {error,econnaborted} -> ok % may be returned on Windows
	  end,
    %%io:format("Closing ~p\n",[Socket]),
    gen_tcp:close(Socket),
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
