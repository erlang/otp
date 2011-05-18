%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the tcp transport component of the Diameter application
%%----------------------------------------------------------------------
%% 
-module(diameter_tcp_test).

-export([
	 init_per_testcase/2, fin_per_testcase/2, 

	 all/0,
	 groups/0, 
	 init_per_suite/1, end_per_suite/1, 
	 suite_init/1, suite_fin/1, 
	 init_per_group/2, end_per_group/2, 

	 start_and_stop_transport_plain/1,
	 start_and_listen/1, 
	 simple_connect/1,
	 simple_send_and_recv/1

	]).

-export([t/0, t/1]).

%% diameter_peer (internal) callback API
-export([up/1, up/3, recv/2]).

-include("diameter_test_lib.hrl").
-include_lib("diameter/include/diameter.hrl").
%% -include_lib("diameter/src/tcp/diameter_tcp.hrl").


t()     -> diameter_test_server:t(?MODULE).
t(Case) -> diameter_test_server:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    diameter_test_server:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    diameter_test_server:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     {group, start}, 
     {group, simple}
    ].

groups() ->
    [
     {start,  [], [start_and_stop_transport_plain, start_and_listen]}, 
     {simple, [], [simple_connect, simple_send_and_recv]}
    ].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite_init(X) -> init_per_suite(X).

init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    Config.


suite_fin(X) -> end_per_suite(X).

end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Test case(s) 
%% 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Plain start and stop of TCP transport
%% 

start_and_stop_transport_plain(suite) ->
    [];
start_and_stop_transport_plain(doc) ->
    [];
start_and_stop_transport_plain(Config) when is_list(Config) ->

    ?SKIP(not_yet_implemented),

    %% This has been changed *a lot* since it was written...

    process_flag(trap_exit, true),
    Transport = ensure_transport_started(),
    TcpTransport = ensure_tcp_transport_started(),
    ensure_tcp_transport_stopped(TcpTransport),
    ensure_transport_stopped(Transport),
    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Start TCP transport and then create a listen socket
%% 

start_and_listen(suite) ->
    [];
start_and_listen(doc) ->
    [];
start_and_listen(Config) when is_list(Config) ->

    ?SKIP(not_yet_implemented),

    %% This has been changed *a lot* since it was written...

    process_flag(trap_exit, true),
    Transport = ensure_transport_started(),
    TcpTransport = ensure_tcp_transport_started(),

    case listen([{port, 0}]) of
	{ok, Acceptor} when is_pid(Acceptor) ->
	    Ref = erlang:monitor(process, Acceptor),
	    [{Acceptor, Info}] = diameter_tcp:which_listeners(),
	    case lists:keysearch(socket, 1, Info) of
		{value, {_, Listen}} ->
		    i("Listen socket: ~p"
		      "~n   Opts:  ~p"
		      "~n   Stats: ~p"
		      "~n   Name:  ~p", 
		      [Listen, 
		       ok(inet:getopts(Listen, [keepalive, delay_send])),
		       ok(inet:getstat(Listen)),
		       ok(inet:sockname(Listen))
		      ]),
		    ok;
		_ ->
		    ?FAIL({bad_listener_info, Acceptor, Info})
	    end,
	    Crash = simulate_crash, 
	    exit(Acceptor, Crash), 
	    receive
		{'DOWN', Ref, process, Acceptor, Crash} ->
		    ?SLEEP(1000),
		    case diameter_tcp:which_listeners() of
			[{NewAcceptor, _NewInfo}] ->
			    diameter_tcp_accept:stop(NewAcceptor),
			    ?SLEEP(1000),
			    case diameter_tcp:which_listeners() of
				[] ->
				    ok;
				UnexpectedListeners ->
				    ?FAIL({unexpected_listeners, empty, UnexpectedListeners})
			    end;
			UnexpectedListeners ->
			    ?FAIL({unexpected_listeners, non_empty, UnexpectedListeners})
		    end
	    after 5000 ->
		    ?FAIL({failed_killing, Acceptor})
	    end;
	Error ->
	    ?FAIL({failed_creating_acceptor, Error})
    end,
    ensure_tcp_transport_stopped(TcpTransport),
    ensure_transport_stopped(Transport),
    i("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% TCP transport connecting
%% 

simple_connect(suite) ->
    [];
simple_connect(doc) ->
    [];
simple_connect(Config) when is_list(Config) ->

    ?SKIP(not_yet_implemented),

    %% This has been changed *a lot* since it was written...

    process_flag(trap_exit, true),
    Transport = ensure_transport_started(),
    TcpTransport = ensure_tcp_transport_started(),
    {_Acceptor, Port} = ensure_tcp_listener(),
    
    {ok, Hostname} = inet:gethostname(),
    
    i("try connect"),
    Opts = [{host, Hostname}, {port, Port}, {module, ?MODULE}], 
    Conn = case connect(Opts) of
	       {ok, C} ->
		   C;
	       Error ->
		   ?FAIL({failed_connecting, Error})
	   end,
    i("connected: ~p", [Conn]),
    
    %% Up for connect
    receive
	{diameter, {up, Host, Port}} ->
	    i("Received expected connect up (~p:~p)", [Host, Port]),
	    ok
    after 5000 ->
	    ?FAIL(connect_up_confirmation_timeout)
    end,

    %% Up for accept
    receive
	{diameter, {up, _ConnPid}} ->
	    i("Received expected accept up"),
	    ok
    after 5000 ->
	    ?FAIL(acceptor_up_confirmation_timeout)
    end,

    i("try disconnect"),
    diameter_tcp:disconnect(Conn),
    ensure_tcp_transport_stopped(TcpTransport),
    ensure_transport_stopped(Transport),
    i("done"),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Plain start and stop of TCP transport
%% 

simple_send_and_recv(suite) ->
    [];
simple_send_and_recv(doc) ->
    [];
simple_send_and_recv(Config) when is_list(Config) ->

    ?SKIP(not_yet_implemented),

    %% This has been changed *a lot* since it was written...

    process_flag(trap_exit, true),
    %% --------------------------------------------------
    %% Start the TCP transport sub-system
    %% 

    Transport = ensure_transport_started(),
    TcpTransport = ensure_tcp_transport_started(),

    {_Acceptor, Port} = ensure_tcp_listener(),
    
    {ok, Hostname} = inet:gethostname(),
    
    i("try connect"),
    Opts = [{host, Hostname}, {port, Port}, {module, ?MODULE}], 
    Conn = case connect(Opts) of
	       {ok, C1} ->
		   C1;
	       Error ->
		   ?FAIL({failed_connecting, Error})
	   end,
    i("connected: ~p", [Conn]),
    
    %% Up for connect
    receive
	{diameter, {up, Host, Port}} ->
	    i("Received expected connect up (~p:~p)", [Host, Port]),
	    ok
    after 5000 ->
	    ?FAIL(connect_up_confirmation_timeout)
    end,

    %% Up for accept
    APid = 
	receive
	    {diameter, {up, C2}} ->
		i("Received expected accept up"),
		C2
	after 5000 ->
		?FAIL(acceptor_up_confirmation_timeout)
	end,
 
    %% --------------------------------------------------
    %% Start some stuff needed for the codec to run
    %% 

    i("start persistent table"),
    {ok, _Pers}    = diameter_persistent_table:start_link(),

    i("start session"),
    {ok, _Session} = diameter_session:start_link(),

    i("try decode a (DWR) message"),
    Base = diameter_gen_base_rfc3588,
    DWR  = ['DWR', 
	    {'Origin-Host',     Hostname}, 
	    {'Origin-Realm',    "whatever-realm"}, 
	    {'Origin-State-Id', [10]}],

    #diameter_packet{msg = Msg} = diameter_codec:encode(Base, DWR),


    %% --------------------------------------------------
    %% Now try to send the message
    %% 
    %% This is not the codec-test suite, so we dont really care what we 
    %% send, as long as it encoded/decodes correctly in the transport
    %% 

    i("try send from connect side"),
    ok = diameter_tcp:send_message(Conn, Msg),

    %% Wait for data on Accept side
    APkt = 
	receive
	    {diameter, {recv, A}} ->
		i("[accept] Received expected data message: ~p", [A]),
		A
	after 5000 ->
		?FAIL(acceptor_up_confirmation_timeout)
	end,
    
    %% Send the same message back, just to have something to send...
    i("try send (\"reply\") from accept side"),
    ok = diameter_tcp:send_message(APid, APkt),

    %% Wait for data on Connect side
    receive
	{diameter, {recv, B}} ->
	    i("[connect] Received expected data message: ~p", [B]),
	    ok
    after 5000 ->
	    ?FAIL(acceptor_up_confirmation_timeout)
    end,  

    i("we are done - now close shop"),
    diameter_session:stop(),
    diameter_persistent_table:stop(),

    ensure_tcp_transport_stopped(TcpTransport),
    ensure_transport_stopped(Transport),
    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ensure_transport_started() ->
%%     i("start diameter transport (top) supervisor"),
    case diameter_transport_sup:start_link() of
	{ok, TransportSup} ->
	    TransportSup;
	Error ->
	    ?FAIL({failed_starting_transport_sup, Error})
    end.

ensure_transport_stopped(Pid) when is_pid(Pid) ->
%%     i("stop diameter transport (top) supervisor"),
    Stop = fun(P) -> exit(P, kill) end,
    ensure_stopped(Pid, Stop, failed_stopping_transport_sup).

ensure_tcp_transport_started() ->
%%     i("start diameter TCP transport"),
    case diameter_tcp:start_transport() of
	{ok, TcpTransport} when is_pid(TcpTransport) ->
	    TcpTransport;
	Error ->
	    ?FAIL({failed_starting_transport, Error})
    end.

ensure_tcp_transport_stopped(Pid) when is_pid(Pid) ->
%%     i("stop diameter TCP transport supervisor"),
    Stop = fun(P) -> diameter_tcp:stop_transport(P) end,
    ensure_stopped(Pid, Stop, failed_stopping_tcp_transport).


ensure_tcp_listener() ->
%%     i("create diameter TCP transport listen socket"),
    case listen([{port, 0}]) of
	{ok, Acceptor} ->
	    [{Acceptor, Info}] = diameter_tcp:which_listeners(),
	    case lists:keysearch(socket, 1, Info) of
		{value, {_, Listen}} ->
		    {ok, Port} = inet:port(Listen),
		    {Acceptor, Port};
		_ ->
		    ?FAIL({failed_retrieving_listen_socket, Info})
	    end;
	Error ->
	    ?FAIL({failed_creating_listen_socket, Error})
    end.


ensure_stopped(Pid, Stop, ReasonTag) when is_pid(Pid) ->
%%     i("ensure_stopped -> create monitor to ~p", [Pid]),
    Ref = erlang:monitor(process, Pid),
%%     i("ensure_stopped -> try stop"),
    Stop(Pid), 
%%     i("ensure_stopped -> await DOWN message"),
    receive
	{'DOWN', Ref, process, Pid, _} ->
%% 	    i("ensure_stopped -> received DOWN message"),
	    ok
    after 5000 ->
%% 	    i("ensure_stopped -> timeout"),
	    ?FAIL({ReasonTag, Pid})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

listen(Opts) ->
    diameter_tcp:listen([{module, ?MODULE} | Opts]).

connect(Opts) ->
    diameter_tcp:connect([{module, ?MODULE} | Opts]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

up(Pid, Host, Port) ->
    Pid ! {diameter, {up, Host, Port}},
    ok.

up(Pid) ->
    Pid ! {diameter, {up, self()}},
    ok.

recv(Pid, Pkt) ->
    Pid ! {diameter, {recv, Pkt}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    io:format(F ++ "~n", A).


ok({ok, Whatever}) ->
    Whatever;
ok(Crap) ->
    Crap.


