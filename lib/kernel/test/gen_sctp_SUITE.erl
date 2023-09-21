%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2023. All Rights Reserved.
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
-module(gen_sctp_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/inet_sctp.hrl").
-include("kernel_test_lib.hrl").

%%-compile(export_all).

-export([all/0, suite/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export([
         skip_old_solaris/1,
         basic/1,
         api_open_close/1,api_listen/1,api_connect_init/1,api_connectx_init/1,api_opts/1,
         xfer_min/1,xfer_active/1,def_sndrcvinfo/1,implicit_inet6/1,
         open_multihoming_ipv4_socket/1,
         open_unihoming_ipv6_socket/1,
         open_multihoming_ipv6_socket/1,
         open_multihoming_ipv4_and_ipv6_socket/1,
         basic_stream/1, xfer_stream_min/1, active_n/1,
         peeloff_active_once/1, peeloff_active_true/1, peeloff_active_n/1,
         buffers/1,
         names_unihoming_ipv4/1, names_unihoming_ipv6/1,
         names_multihoming_ipv4/1, names_multihoming_ipv6/1,
         recv_close/1,

         t_simple_local_sockaddr_in_send_recv/1,
         t_simple_link_local_sockaddr_in_send_recv/1,
         t_simple_local_sockaddr_in6_send_recv/1,
         t_simple_link_local_sockaddr_in6_send_recv/1,
         t_simple_local_sockaddr_in_connectx_init/1
        ]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    G = case is_old_solaris() of
	    true -> old_solaris;
	    false -> extensive
	end,
    [
     {group, smoke},
     {group, G},
     {group, sockaddr}].

groups() -> 
    [
     {smoke,       [], smoke_cases()},
     {old_solaris, [], old_solaris_cases()},
     {extensive,   [], extensive_cases()},

     {sockaddr,    [], sockaddr_cases()}
    ].

smoke_cases() ->
    [
     basic,
     basic_stream
    ].

old_solaris_cases() ->
    [
     skip_old_solaris
    ].

extensive_cases() ->
    [
     api_open_close, api_listen, api_connect_init, api_connectx_init,
     api_opts, xfer_min, xfer_active, def_sndrcvinfo, implicit_inet6,
     open_multihoming_ipv4_socket,
     open_unihoming_ipv6_socket,
     open_multihoming_ipv6_socket,
     open_multihoming_ipv4_and_ipv6_socket, active_n,
     xfer_stream_min, peeloff_active_once,
     peeloff_active_true, peeloff_active_n, buffers,
     names_unihoming_ipv4, names_unihoming_ipv6,
     names_multihoming_ipv4, names_multihoming_ipv6,
     recv_close
    ].

sockaddr_cases() ->
    [
     t_simple_local_sockaddr_in_send_recv,
     t_simple_link_local_sockaddr_in_send_recv,
     t_simple_local_sockaddr_in6_send_recv,
     t_simple_link_local_sockaddr_in6_send_recv,
     t_simple_local_sockaddr_in_connectx_init
    ].


%% This (Config) was ignored before, why?
init_per_suite(Config0) ->

    ?P("init_per_suite -> entry with"
       "~n      Config: ~p"
       "~n      Nodes:  ~p", [Config0, erlang:nodes()]),

    case gen_sctp:open() of
	{ok, Socket} ->
	    gen_sctp:close(Socket),

            case ?LIB:init_per_suite(Config0) of
                {skip, _} = SKIP ->
                    SKIP;

                Config1 when is_list(Config1) ->

                    ?P("init_per_suite -> end when "
                       "~n      Config: ~p", [Config1]),

                    %% We need a monitor on this node also
                    kernel_test_sys_monitor:start(),

                    Config1
            end;

	{error, Error}
	  when Error =:= eprotonosupport;
	       Error =:= esocktnosupport ->
	    {skip,"SCTP not supported on this machine"}
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
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(api_connectx_init=Case, Config) ->
    check_sctp_connectx(Case, Config);
init_per_testcase(t_simple_local_sockaddr_in_connectx_init=Case, Config) ->
    check_sctp_connectx(Case, Config);
init_per_testcase(Case, Config) ->
    init_per_testcase_common(Case, Config).
    
init_per_testcase_common(_Case, Config) ->

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


-define(LOGVAR(Var), begin io:format(??Var" = ~p~n", [Var]) end).
-define(no_return(Expr), error({unexpected, Expr})).

check_sctp_connectx(Case, Config) ->
    {ok,S} = gen_sctp:open([]),
    try
        {ok,Pb} = inet:port(S),
        case gen_sctp:connectx_init(S, [{127,0,0,1}], Pb, []) of
            {error, enotsup} ->
                {skip, "sctp_connectx unsupported"};
            _ ->
                init_per_testcase_common(Case, Config)
        end
    after
        gen_sctp:close(S)
    end.

is_old_solaris() ->
    os:type() =:= {unix,sunos} andalso os:version() < {5,12,0}.

skip_old_solaris(_Config) ->
    {skip,"Unreliable test cases and/or implementation on old Solaris"}.

%% Hello world.
basic(Config) when is_list(Config) ->
    {ok,S} = gen_sctp:open(),
    ok = gen_sctp:close(S),
    ok.

%% Minimal data transfer.
xfer_min(Config) when is_list(Config) ->
    Stream = 0,
    Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    Loopback = {127,0,0,1},
    StatOpts =
	[recv_avg,recv_cnt,recv_max,recv_oct,
	 send_avg,send_cnt,send_max,send_oct],
    {ok,Sb} = gen_sctp:open([{type,seqpacket}]),
    {ok,SbStat1} = inet:getstat(Sb, StatOpts),
    {ok,Pb} = inet:port(Sb),
    ok = gen_sctp:listen(Sb, true),

    {ok,Sa} = gen_sctp:open(),
    {ok,Pa} = inet:port(Sa),
    {ok,#sctp_assoc_change{state=comm_up,
			   error=0,
			   outbound_streams=SaOutboundStreams,
			   inbound_streams=SaInboundStreams,
			   assoc_id=SaAssocId}=SaAssocChange} =
	gen_sctp:connect(Sa, Loopback, Pb, []),
    {SbAssocId,SaOutboundStreams,SaInboundStreams} =
	case recv_event(log_ok(gen_sctp:recv(Sb, infinity))) of
	    {Loopback,Pa,
	     #sctp_assoc_change{state=comm_up,
				error=0,
				outbound_streams=SbOutboundStreams,
				inbound_streams=SbInboundStreams,
				assoc_id=AssocId}} ->
		{AssocId,SbInboundStreams,SbOutboundStreams};
	    {Loopback,Pa,
	     #sctp_paddr_change{state=addr_confirmed,
				addr={Loopback,Pa},
				error=0,
				assoc_id=AssocId}} ->
		{Loopback,Pa,
		 #sctp_assoc_change{state=comm_up,
				    error=0,
				    outbound_streams=SbOutboundStreams,
				    inbound_streams=SbInboundStreams,
				    assoc_id=AssocId}} =
		    recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
		{AssocId,SbInboundStreams,SbOutboundStreams}
	end,

    ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    case log_ok(gen_sctp:recv(Sb, infinity)) of
	{Loopback,
	 Pa,
	 [#sctp_sndrcvinfo{stream   = Stream,
			   assoc_id = SbAssocId}],
	 Data} -> ok;
	Event1 ->
	    case recv_event(Event1) of
		{Loopback,Pa,
		 #sctp_paddr_change{addr = {Loopback,_},
				    state = State,
				    error = 0,
				    assoc_id = SbAssocId}}
		  when State =:= addr_available;
		       State =:= addr_confirmed ->
		    {Loopback,
		     Pa,
		     [#sctp_sndrcvinfo{stream=Stream,
				       assoc_id=SbAssocId}],
		     Data} = log_ok(gen_sctp:recv(Sb, infinity))
	    end
    end,
    ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    case log_ok(gen_sctp:recv(Sa, infinity)) of
	{Loopback,Pb,
	 [#sctp_sndrcvinfo{stream=Stream,
			   assoc_id=SaAssocId}],
	 Data} ->
	    ok;
	Event2 ->
	    {Loopback,Pb,
	     #sctp_paddr_change{addr={_,Pb},
				state=addr_confirmed,
				error=0,
				assoc_id=SaAssocId}} =
		recv_event(Event2),
	    {Loopback,
	     Pb,
	     [#sctp_sndrcvinfo{stream=Stream,
			       assoc_id=SaAssocId}],
	     Data} =
		log_ok(gen_sctp:recv(Sa, infinity))
    end,
    %%
    ok = gen_sctp:eof(Sa, SaAssocChange),
    {Loopback,Pa,#sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    {Loopback,Pb,
     #sctp_assoc_change{state=shutdown_comp,
			error=0,
			assoc_id=SaAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
    {Loopback,Pa,
     #sctp_assoc_change{state=shutdown_comp,
			error=0,
			assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ok = gen_sctp:close(Sa),
    {ok,SbStat2} = inet:getstat(Sb, StatOpts),
    [] = filter_stat_eq(SbStat1, SbStat2),
    ok = gen_sctp:close(Sb),

    receive
	Msg -> ct:fail({received,Msg})
    after 17 -> ok
    end,
    ok.

filter_stat_eq([], []) ->
    [];
filter_stat_eq([{Tag,Val1}=Stat|SbStat1], [{Tag,Val2}|SbStat2]) ->
    if
	Val1 == Val2 ->
	    [Stat|filter_stat_eq(SbStat1, SbStat2)];
	true ->
	    filter_stat_eq(SbStat1, SbStat2)
    end.



%% Minimal data transfer in active mode.
xfer_active(Config) when is_list(Config) ->
    Timeout = 2000,
    Stream = 0,
    Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    Loopback = {127,0,0,1},
    {ok,Sb} = gen_sctp:open([{active,true}]),
    {ok,Pb} = inet:port(Sb),
    ok = gen_sctp:listen(Sb, true),

    {ok,Sa} = gen_sctp:open([{active,true}]),
    {ok,Pa} = inet:port(Sa),
    ok = gen_sctp:connect_init(Sa, Loopback, Pb, []),
    #sctp_assoc_change{state=comm_up,
		       error=0,
		       outbound_streams=SaOutboundStreams,
		       inbound_streams=SaInboundStreams,
		       assoc_id=SaAssocId} = SaAssocChange =
	recv_assoc_change(Sa, Loopback, Pb, Timeout),
    io:format("Sa=~p, Pa=~p, Sb=~p, Pb=~p, SaAssocId=~p, "
	      "SaOutboundStreams=~p, SaInboundStreams=~p~n",
	      [Sa,Pa,Sb,Pb,SaAssocId,
	       SaOutboundStreams,SaInboundStreams]),
    #sctp_assoc_change{state=comm_up,
		       error=0,
		       outbound_streams=SbOutboundStreams,
		       inbound_streams=SbInboundStreams,
		       assoc_id=SbAssocId} =
	recv_assoc_change(Sb, Loopback, Pa, Timeout),
    SbOutboundStreams = SaInboundStreams,
    SbInboundStreams = SaOutboundStreams,
    io:format("SbAssocId=~p~n", [SbAssocId]),

    case recv_paddr_change(Sa, Loopback, Pb, 314) of
	#sctp_paddr_change{state=addr_confirmed,
			   addr={_,Pb},
			   error=0,
			   assoc_id=SaAssocId} -> ok;
	#sctp_paddr_change{state=addr_available,
			   addr={_,Pb},
			   error=0,
			   assoc_id=SaAssocId} -> ok;
	timeout -> ok
    end,
    case recv_paddr_change(Sb, Loopback, Pa, 314) of
	#sctp_paddr_change{state=addr_confirmed,
			   addr={Loopback,Pa},
			   error=0,
			   assoc_id=SbAssocId} -> ok;
	#sctp_paddr_change{state=addr_available,
			   addr={Loopback,P},
			   error=0,
			   assoc_id=SbAssocId} ->
	    match_unless_solaris(Pa, P);
	timeout -> ok
    end,
    [] = flush(),

    ok =
	do_from_other_process(
	  fun () -> gen_sctp:send(Sa, SaAssocId, 0, Data) end),
    receive
	{sctp,Sb,Loopback,Pa,
	 {[#sctp_sndrcvinfo{stream=Stream,
			    assoc_id=SbAssocId}],
	  Data}} -> ok
    after Timeout ->
	    ct:fail({timeout,flush()})
    end,
    ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    receive
	{sctp,Sa,Loopback,Pb,
	 {[#sctp_sndrcvinfo{stream=Stream,
			    assoc_id=SaAssocId}],
	  Data}} -> ok
    after Timeout ->
	    ct:fail({timeout,flush()})
    end,
    %%
    ok = gen_sctp:abort(Sa, SaAssocChange),
    case recv_assoc_change(Sb, Loopback, Pa, Timeout) of
	#sctp_assoc_change{state=comm_lost,
			   assoc_id=SbAssocId} -> ok;
	timeout ->
	    ct:fail({timeout,flush()})
    end,
    ok = gen_sctp:close(Sb),
    case recv_assoc_change(Sa, Loopback, Pb, Timeout) of
	#sctp_assoc_change{state=comm_lost,
			   assoc_id=SaAssocId} -> ok;
	timeout ->
	    io:format("timeout waiting for comm_lost on Sa~n"),
	    match_unless_solaris(ok, {timeout,flush()})
    end,
    receive
	{sctp_error,Sa,enotconn} -> ok % Solaris
    after 17 -> ok
    end,
    ok = gen_sctp:close(Sa),
    %%
    receive
	Msg -> ct:fail({unexpected,[Msg]++flush()})
    after 17 -> ok
    end,
    ok.

recv_assoc_change(S, Addr, Port, Timeout) ->
    receive
	{sctp,S,Addr,Port,{[], #sctp_assoc_change{}=AssocChange}} ->
	    AssocChange;
	{sctp,S,Addr,Port,
	 {[#sctp_sndrcvinfo{assoc_id=AssocId}],
	  #sctp_assoc_change{assoc_id=AssocId}=AssocChange}} ->
	    AssocChange
    after Timeout ->
	    timeout
    end.

recv_paddr_change(S, Addr, Port, Timeout) ->
    receive
	{sctp,S,Addr,Port,{[], #sctp_paddr_change{}=PaddrChange}} ->
	    PaddrChange;
	{sctp,S,Addr,Port,
	 {[#sctp_sndrcvinfo{assoc_id=AssocId}],
	  #sctp_paddr_change{assoc_id=AssocId}=PaddrChange}} ->
	    PaddrChange
    after Timeout ->
	    timeout
    end.

%% Test that #sctp_sndrcvinfo{} parameters set on a socket
%% are used by gen_sctp:send/4.
def_sndrcvinfo(Config) when is_list(Config) ->
    Loopback = {127,0,0,1},
    Data = <<"What goes up, must come down.">>,
    %%
    S1 =
	log_ok(gen_sctp:open(
		 0, [{sctp_default_send_param,#sctp_sndrcvinfo{ppid=17}}])),
    ?LOGVAR(S1),
    P1 =
	log_ok(inet:port(S1)),
    ?LOGVAR(P1),
    #sctp_sndrcvinfo{ppid=17, context=0, timetolive=0, assoc_id=0} =
	getopt(S1, sctp_default_send_param),
    ok =
	gen_sctp:listen(S1, true),
    %%
    S2 =
	log_ok(gen_sctp:open()),
    ?LOGVAR(S2),
    P2 =
	log_ok(inet:port(S2)),
    ?LOGVAR(P2),
    #sctp_sndrcvinfo{ppid=0, context=0, timetolive=0, assoc_id=0} =
	getopt(S2, sctp_default_send_param),
    %%
    #sctp_assoc_change{
       state=comm_up,
       error=0,
       assoc_id=S2AssocId} = S2AssocChange =
	log_ok(gen_sctp:connect(S2, Loopback, P1, [])),
    ?LOGVAR(S2AssocChange),
    S1AssocId =
	case recv_event(log_ok(gen_sctp:recv(S1))) of
	    {Loopback,P2,
	     #sctp_assoc_change{
		state=comm_up,
		error=0,
		assoc_id=AssocId}} ->
		AssocId;
	    {Loopback,P2,
	     #sctp_paddr_change{
		state=addr_confirmed,
		error=0,
		assoc_id=AssocId}} ->
		{Loopback,P2,
		 #sctp_assoc_change{
		    state=comm_up,
		    error=0,
		    assoc_id=AssocId}} =
		    recv_event(log_ok(gen_sctp:recv(S1))),
		AssocId
	end,
    ?LOGVAR(S1AssocId),

    #sctp_sndrcvinfo{
       ppid=17, context=0, timetolive=0} = %, assoc_id=S1AssocId} =
	getopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S1AssocId}),
    #sctp_sndrcvinfo{
       ppid=0, context=0, timetolive=0} = %, assoc_id=S2AssocId} =
	getopt(
	  S2, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S2AssocId}),
    %%
    ok =
	gen_sctp:send(S1, S1AssocId, 1, <<"1: ",Data/binary>>),
    case log_ok(gen_sctp:recv(S2)) of
	{Loopback,P1,
	 [#sctp_sndrcvinfo{
	     stream=1, ppid=17, context=0, assoc_id=S2AssocId}],
	 <<"1: ",Data/binary>>} -> ok;
	Event1 ->
	    {Loopback,P1,
	     #sctp_paddr_change{state=addr_confirmed,
				addr={_,P1},
				error=0,
				assoc_id=S2AssocId}} =
		recv_event(Event1),
	    {Loopback,P1,
	     [#sctp_sndrcvinfo{
		 stream=1, ppid=17, context=0, assoc_id=S2AssocId}],
	     <<"1: ",Data/binary>>} =
		log_ok(gen_sctp:recv(S2))
    end,
    %%
    ok =
	setopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{ppid=18}),
    ok =
	setopt(
	  S1, sctp_default_send_param,
	  #sctp_sndrcvinfo{ppid=19, assoc_id=S1AssocId}),
    #sctp_sndrcvinfo{
       ppid=18, context=0, timetolive=0, assoc_id=0} =
	getopt(S1, sctp_default_send_param),
    #sctp_sndrcvinfo{
       ppid=19, context=0, timetolive=0, assoc_id=S1AssocId} =
	getopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S1AssocId}),
    %%
    ok =
	gen_sctp:send(S1, S1AssocId, 0, <<"2: ",Data/binary>>),
    case log_ok(gen_sctp:recv(S2)) of
	{Loopback,P1,
	 [#sctp_sndrcvinfo{
	     stream=0, ppid=19, context=0, assoc_id=S2AssocId}],
	 <<"2: ",Data/binary>>} -> ok
    end,
    ok =
	gen_sctp:send(S2, S2AssocChange, 1, <<"3: ",Data/binary>>),
    case log_ok(gen_sctp:recv(S1)) of
	{Loopback,P2,
	 [#sctp_sndrcvinfo{
	     stream=1, ppid=0, context=0, assoc_id=S1AssocId}],
	 <<"3: ",Data/binary>>} -> ok;
	Event2 ->
	    case recv_event(Event2) of
		{Loopback,P2,
		 #sctp_paddr_change{
		    addr={Loopback,_},
		    state=State,
		    error=0, assoc_id=S1AssocId}}
		  when State =:= addr_available;
		       State =:= addr_confirmed ->
		    case log_ok(gen_sctp:recv(S1)) of
			{Loopback,P2,
			 [#sctp_sndrcvinfo{
			     stream=1, ppid=0, context=0,
			     assoc_id=S1AssocId}],
			 <<"3: ",Data/binary>>} -> ok
		    end
	    end
    end,
    ok =
	do_from_other_process(
	  fun () ->
		  gen_sctp:send(
		    S2,
		    #sctp_sndrcvinfo{stream=0, ppid=20, assoc_id=S2AssocId},
		    <<"4: ",Data/binary>>)
	  end),
    case log_ok(do_from_other_process(fun() -> gen_sctp:recv(S1) end)) of
	{Loopback,P2,
	 [#sctp_sndrcvinfo{
	     stream=0, ppid=20, context=0, assoc_id=S1AssocId}],
	 <<"4: ",Data/binary>>} -> ok
    end,
    %%
    ok =
	gen_sctp:close(S1),
    ok =
	gen_sctp:close(S2),
    receive
	Msg ->
	    ct:fail({received,Msg})
    after 17 -> ok
    end,
    ok.

getopt(S, Opt) ->
    {ok,[{Opt,Val}]} = inet:getopts(S, [Opt]),
    Val.

getopt(S, Opt, Param) ->
    {ok,[{Opt,Val}]} = inet:getopts(S, [{Opt,Param}]),
    Val.

setopt(S, Opt, Val) ->
    inet:setopts(S, [{Opt,Val}]).


flush() ->
    receive
	Msg ->
	    [Msg|flush()]
    after 17 ->
	    []
    end.

%% Test the API function open/1,2 and close/1.
api_open_close(Config) when is_list(Config) ->
    ?TC_TRY(?FUNCTION_NAME, fun() -> do_api_open_close() end).

do_api_open_close() ->
    {ok,S1} = gen_sctp:open(0),
    {ok,P}  = inet:port(S1),
    ok      = gen_sctp:close(S1),

    {ok,S2} = gen_sctp:open(P),
    {ok,P}  = inet:port(S2),
    ok      = gen_sctp:close(S2),

    {ok,S3} = gen_sctp:open([{port,P}]),
    {ok,P}  = inet:port(S3),
    ok      = gen_sctp:close(S3),

    {ok,S4} = gen_sctp:open(P, []),
    {ok,P}  = inet:port(S4),
    ok      = gen_sctp:close(S4),

    {ok,S5} = gen_sctp:open(P, [{debug, false}, {ifaddr,any}, {debug, true}]),
    {ok,P}  = inet:port(S5),
    ok      = gen_sctp:close(S5),

    ok      = gen_sctp:close(S5),

    try gen_sctp:close(0)
    catch error:badarg -> ok
    end,

    try gen_sctp:open({})
    catch error:badarg -> ok
    end,

    try gen_sctp:open(-1)
    catch error:badarg -> ok
    end,

    try gen_sctp:open(65536)
    catch error:badarg -> ok
    end,

    try gen_sctp:open(make_ref(), [])
    catch error:badarg -> ok
    end,

    try gen_sctp:open(0, {})
    catch error:badarg -> ok
    end,

    try gen_sctp:open(0, [make_ref()])
    catch error:badarg -> ok
    end,

    try gen_sctp:open([{invalid_option,0}])
    catch error:badarg -> ok
    end,

    try gen_sctp:open(0, [{mode,invalid_mode}])
    catch error:badarg -> ok
    end,
    ok.

%% Test the API function listen/2.
api_listen(Config) when is_list(Config) ->
    Localhost = {127,0,0,1},

    try gen_sctp:listen(0, true)
    catch error:badarg -> ok
    end,

    {ok,S} = gen_sctp:open(),
    {ok,Pb} = inet:port(S),
    try gen_sctp:listen(S, not_allowed_for_listen)
    catch error:badarg -> ok
    end,
    ok = gen_sctp:close(S),
    {error,closed} = gen_sctp:listen(S, true),

    {ok,Sb} = gen_sctp:open(Pb),
    {ok,Sa} = gen_sctp:open(),

    element(1, os:type()) =:= unix andalso
        begin
            {error, nxdomain} = gen_sctp:connect(Sa, "", 65535, []),
            {error, nxdomain} = gen_sctp:connect(Sa, '', 65535, [])
        end,
    {error, nxdomain} = gen_sctp:connect(Sa, ".", 65535, []),
    {error, nxdomain} = gen_sctp:connect(Sa, '.', 65535, []),

    case gen_sctp:connect(Sa, localhost, Pb, []) of
	{error,econnrefused} ->
	    {ok,{Localhost,
		 Pb,[],
		 #sctp_assoc_change{
		    state=comm_lost}}} =
		gen_sctp:recv(Sa, infinity);
	{error,#sctp_assoc_change{state=cant_assoc}} ->
	    ok%;
	    %% {error,{Localhost,Pb,_,#sctp_assoc_change{state=cant_assoc}}} ->
	    %% 	  ok
    end,
    ok = gen_sctp:listen(Sb, true),
    {ok,#sctp_assoc_change{state=comm_up,
			   error=0}} =
	gen_sctp:connect(Sa, localhost, Pb, []),
    ok = gen_sctp:close(Sa),
    ok = gen_sctp:close(Sb),
    ok.

%% Test the API function connect_init/4.
api_connect_init(Config) when is_list(Config) ->
    Localhost = {127,0,0,1},

    {ok,S} = gen_sctp:open(),
    {ok,Pb} = inet:port(S),
    try gen_sctp:connect_init(S, Localhost, not_allowed_for_port, [])
    catch error:badarg -> ok
    end,
    try gen_sctp:connect_init(S, Localhost, 12345, not_allowed_for_opts)
    catch error:badarg -> ok
    end,
    ok = gen_sctp:close(S),
    {error,closed} = gen_sctp:connect_init(S, Localhost, 12345, []),

    {ok,Sb} = gen_sctp:open(Pb),
    {ok,Sa} = gen_sctp:open(),
    case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	{error,econnrefused} ->
	    {Localhost,Pb,#sctp_assoc_change{state=comm_lost}} =
		recv_event(log_ok(gen_sctp:recv(Sa, infinity)));
	ok ->
	    {Localhost,Pb,#sctp_assoc_change{state=cant_assoc}} =
		recv_event(log_ok(gen_sctp:recv(Sa, infinity)))
    end,
    ok = gen_sctp:listen(Sb, true),
    case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	ok ->
	    {Localhost,Pb,#sctp_assoc_change{state=comm_up}} =
		recv_event(log_ok(gen_sctp:recv(Sa, infinity)))
    end,
    ok = gen_sctp:close(Sa),
    ok = gen_sctp:close(Sb),
    ok.

%% Test the API function connectx_init/4.
api_connectx_init(Config) when is_list(Config) ->
    Localhost = {127,0,0,1},

    {ok,S} = gen_sctp:open(),
    {ok,Pb} = inet:port(S),
    try ?no_return(gen_sctp:connectx_init(S, Localhost, 12345, []))
    catch error:badarg -> ok
    end,
    {error, einval} = gen_sctp:connectx_init(S, [Localhost], not_allowed_for_port, []),
    %% try ?no_return(gen_sctp:connectx_init(S, [Localhost], not_allowed_for_port, []))
    %% catch error:badarg -> ok
    %% end,
    try ?no_return(gen_sctp:connectx_init(S, [Localhost], 12345, not_allowed_for_opts))
    catch error:badarg -> ok
    end,
    ok = gen_sctp:close(S),
    {error,closed} = gen_sctp:connectx_init(S, [Localhost], 12345, []),

    {ok,Sb} = gen_sctp:open(Pb),
    {ok,Sa} = gen_sctp:open(),
    {ok, A1} = gen_sctp:connectx_init(Sa, [localhost], Pb, []),
    true = (A1 =/= 0),
    {Localhost,Pb,#sctp_assoc_change{state=cant_assoc, assoc_id = A1}} =
	recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
    ok = gen_sctp:listen(Sb, true),
    {ok, A2} = gen_sctp:connectx_init(Sa, [localhost], Pb, []),
    true = (A2 =/= 0),
    {Localhost,Pb,#sctp_assoc_change{state=comm_up, assoc_id = A2}} =
	recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
    ok = gen_sctp:close(Sa),
    ok = gen_sctp:close(Sb),
    ok.

recv_event({Addr, Port, [], #sctp_assoc_change{} = AssocChange}) ->
    {Addr, Port, AssocChange};
recv_event({Addr,Port,
	    [#sctp_sndrcvinfo{assoc_id  = Assoc}],
	    #sctp_assoc_change{assoc_id = Assoc} = AssocChange}) ->
    {Addr, Port, AssocChange};
recv_event({Addr, Port, [], #sctp_paddr_change{} = PaddrChange}) ->
    {Addr, Port, PaddrChange};
recv_event({Addr, Port,
	    [#sctp_sndrcvinfo{assoc_id  = Assoc}],
	    #sctp_paddr_change{assoc_id = Assoc} = PaddrChange}) ->
    {Addr, Port, PaddrChange};
recv_event({Addr, Port, [], #sctp_shutdown_event{} = ShutdownEvent}) ->
    {Addr, Port, ShutdownEvent};
recv_event({Addr, Port,
	    [#sctp_sndrcvinfo{assoc_id    = Assoc}],
	    #sctp_shutdown_event{assoc_id = Assoc} = ShutdownEvent}) ->
    {Addr, Port, ShutdownEvent}.

%% Test socket options.
api_opts(Config) when is_list(Config) ->
    Sndbuf = 32768,
    Recbuf = 65536,
    {ok,S} = gen_sctp:open(0),
    OSType = os:type(),
    case {inet:setopts(S, [{linger,{true,2}}]),OSType} of
	{ok,_} ->
	    ok;
	{{error,einval},{unix,sunos}} ->
	    ok
    end,
    ok = inet:setopts(S, [{sndbuf,Sndbuf}]),
    ok = inet:setopts(S, [{recbuf,Recbuf}]),
    case inet:getopts(S, [sndbuf]) of
	{ok, [{sndbuf,SB}]} when SB >= Sndbuf -> ok
    end,
    case inet:getopts(S, [recbuf]) of
	{ok, [{recbuf, RB}]} when (RB >= Recbuf) -> ok
    end.

%% What is this *actually* supposed to test?
implicit_inet6(Config) when is_list(Config) ->
    ?TC_TRY(implicit_inet6, fun() -> do_implicit_inet6(Config) end).

do_implicit_inet6(_Config) ->
    ?P("begin"),
    %% First
    ?P("try create server socket (1)"),
    case gen_sctp:open(0, [inet6]) of
	{ok, S1} ->
            Loopback = {0,0,0,0,0,0,0,1},
            ?P("*** ~s: ~p ***", ["Loopback", Loopback]),
            implicit_inet6(S1, Loopback),
            ok = gen_sctp:close(S1),
            
            %% Second
            ?P("try create server socket (2)"),
            Localhost =
                case inet:getaddr("localhost", inet6) of
                    {ok, LH} ->
                        LH;
                    {error, nxdomain = Reason_getaddr} ->
                        ?SKIPT(Reason_getaddr);
                    {error, Reason_getaddr} ->
                        ct:fail({unexpected, Reason_getaddr})
                end,
            S2 = case gen_sctp:open(0, [{ip, Localhost}]) of
                     {ok, S} ->
                         S;
                     {error, nxdomain = Reason_open} ->
                         ?SKIPT(Reason_open);
                     {error, Reason_open} ->
                         ct:fail({unexpected, Reason_open})
                 end,

            ?P("*** ~s: ~p ***", ["localhost", Localhost]),
            implicit_inet6(S2, Localhost),
            ok = gen_sctp:close(S2),
            
            %% Third
            ?P("try create server socket (3)"),
            Hostname = log_ok(inet:gethostname()),
            Addr     = case inet:getaddr(Hostname, inet6) of
                           {ok, A} ->
                               A;
                           {error, eafnosupport} ->
                               ok = gen_sctp:close(S1),
                               ?SKIPT("Can not look up IPv6 address")
                       end,
            S3 = log_ok(gen_sctp:open(0, [{ifaddr, Addr}])),
            ?P("*** ~s: ~p ***", [Hostname, Addr]),
            implicit_inet6(S3, Addr),
            ok = gen_sctp:close(S1),
            ?P("done"),
            ok;
        {error, eaddrnotavail = Reason} ->
            ?SKIPT(open_failed_str(Reason));
        _ ->
            {skip, "IPv6 not supported"}
    end.


implicit_inet6(S1, Addr) ->
    ?P("make (server) listen socket"),
    ok = gen_sctp:listen(S1, true),
    ServerPortNo = log_ok(inet:port(S1)),
    ?P("try create (client) socket"),
    S2 = case gen_sctp:open(0, [inet6, {ifaddr, Addr}]) of
             {ok, Sock} ->
                 ?P("client socket created: ~p", [Sock]),
                 Sock;
             {error, eaddrnotavail = Reason} ->
                 ?P("could not create (client) socket with ifaddr: "
                    "~n   ~p", [Addr]),
                 ?SKIPT(open_failed_str(Reason))
    end,
    {ClientAddr, ClientPortNo} = log_ok(inet:sockname(S2)),
    ?P("try connect"
       "~n   from (connector): ~p, ~p (~p)"
       "~n   to:               ~p, ~p",
       [ClientAddr, ClientPortNo, S2, Addr, ServerPortNo]),
    #sctp_assoc_change{state = comm_up} =
	log_ok(gen_sctp:connect(S2, Addr, ServerPortNo, [])),
    ?P("connect success: await events"),
    implicit_inet6_await_ac_comm_up(S1, ClientAddr, ClientPortNo),
    ?P("verify server sockname"),
    case log_ok(inet:sockname(S1)) of
	{Addr,              ServerPortNo} -> ok;
	{{0,0,0,0,0,0,0,0}, ServerPortNo} -> ok
    end,
    ?P("verify client sockname"),
    case log_ok(inet:sockname(S2)) of
	{Addr,              ClientPortNo} -> ok;
	{{0,0,0,0,0,0,0,0}, ClientPortNo} -> ok
    end,
    ?P("client client socket"),
    ok = gen_sctp:close(S2),
    ?P("verification complete"),
    ok.


implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo) ->
    {_OsFam, OsName} = os:type(),
    implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo, OsName).

implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo, OsName) ->
    case recv_event(log_ok(gen_sctp:recv(Sock))) of
	{Addr, PortNo, #sctp_assoc_change{state = comm_up}} ->
            ?P("received assoc-change:comm-up event => done"),
	    ok;
	{Addr, PortNo, #sctp_paddr_change{state = addr_confirmed,
                                          addr  = {Addr, PortNo},
                                          error = 0}} ->
            ?P("received paddr-change:addr-confirmed event - "
               "try recv assoc-change:comm-up"),
            implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo, OsName);

	{Addr2, PortNo2, #sctp_assoc_change{state = comm_up}}
          when (OsName =:= freebsd) ->
            ?P("Expected (assoc-change:comm-up) event from unexpected address: "
               "~n   Unexpected Address: ~p, ~p"
               "~n   Expected Address:   ~p, ~p"
               "~n   => RETRY"
               "~n", [Addr2, PortNo2, Addr, PortNo]),
            implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo, OsName);
	{Addr2, PortNo2, #sctp_paddr_change{state = addr_confirmed}}
          when (OsName =:= freebsd) ->
            ?P("Expected paddr-change:addr-confirmed event from "
               "UNEXPECTED ADDRESS: "
               "~n   UNEXPECTED Address: ~p, ~p"
               "~n   Expected Address:   ~p, ~p"
               "~n   => RETRY"
               "~n", [Addr2, PortNo2, Addr, PortNo]),
            implicit_inet6_await_ac_comm_up(Sock, Addr, PortNo, OsName);

	{Addr2, PortNo2, #sctp_assoc_change{state = comm_up} = CX} = UNEX ->
            ?P("Expected (assoc-change:comm-up) event from UNEXPECTED ADDRESS: "
               "~n   UNEXPECTED Address: ~p, ~p"
               "~n   Expected Address:   ~p, ~p"
               "~n   Assoc Change:       ~p"
               "~n", [Addr2, PortNo2, Addr, PortNo, CX]),
            exit({unexpected_event, UNEX});

	{AX, PX, #sctp_paddr_change{state = addr_confirmed} = CX} = UNEX ->
            ?P("Expected paddr-change:addr-confirmed event from "
               "UNEXPECTED ADDRESS: "
               "~n   UNEXPECTED Address: ~p, ~p"
               "~n   Expected Address:   ~p, ~p"
               "~n   PAddr Change:       ~p"
               "~n", [AX, PX, Addr, PortNo, CX]),
            exit({unexpected_event, UNEX});

        {AX, PX, CX} = UNEX ->
            ?P("UNEXPECTED EVENT: "
               "~n   ~p"
               "~n   UNEXPECTED ADDRESS: ~p, ~p"
               "~n   Expected Address:   ~p, ~p"
               "~n", [CX, AX, PX, Addr, PortNo]),
            exit({unexpected_event, UNEX})
    end.


%% Verify {active, N} socket management.
%% This is difficult to do since we do not just receive data messages.
%% Also, how do we know that sctp behaves the same way on all platforms?
active_n(Config) when is_list(Config) ->
    ?TC_TRY(active_n, fun() -> do_active_n(Config) end).

do_active_n(_Config) ->
    N = 3,
    S1 = ok(gen_sctp:open([{active,N}])),
    [{active,N}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,-N}]),
    receive
        {sctp_passive, S1} -> ok
    after
        5000 ->
            exit({error,sctp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,0}]),
    receive
        {sctp_passive, S1} -> ok
    after
        5000 ->
            exit({error,sctp_passive_failure})
    end,
    ok = inet:setopts(S1, [{active,32767}]),
    {error,einval} = inet:setopts(S1, [{active,1}]),
    {error,einval} = inet:setopts(S1, [{active,-32769}]),
    ok = inet:setopts(S1, [{active,-32768}]),
    receive
        {sctp_passive, S1} -> ok
    after
        5000 ->
            exit({error,sctp_passive_failure})
    end,
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ok = inet:setopts(S1, [{active,N}]),
    ok = inet:setopts(S1, [{active,true}]),
    [{active,true}] = ok(inet:getopts(S1, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    ok = inet:setopts(S1, [{active,N}]),
    ok = inet:setopts(S1, [{active,once}]),
    [{active,once}] = ok(inet:getopts(S1, [active])),
    receive
        _ -> exit({error,active_n})
    after
        0 ->
            ok
    end,
    {error,einval} = inet:setopts(S1, [{active,32768}]),
    ok = inet:setopts(S1, [{active,false}]),
    [{active,false}] = ok(inet:getopts(S1, [active])),
    ok = gen_sctp:listen(S1, true),
    S1Port = ok(inet:port(S1)),
    S2 = ok(gen_sctp:open(0, [{active,false}])),
    Assoc = ok(gen_sctp:connect(S2, "localhost", S1Port, [])),
    ok = inet:setopts(S1, [{active,N}]),
    active_n_flush_connect_msgs(S1),
    active_n_send_loop(N, S2, Assoc, S1),
    S3 = ok(gen_sctp:open([{active,0}])),
    receive
        {sctp_passive,S3} ->
            [{active,false}] = ok(inet:getopts(S3, [active]))
    after
        5000 ->
            exit({error,udp_passive})
    end,
    ok = gen_sctp:close(S3),
    ok = gen_sctp:close(S2),
    ok = gen_sctp:close(S1),
    ok.


%% There is no way to know how many addresses this host has,
%% and if there is "too many" (more then N = 3), then the
%% socket may already be passive. In this case the send-
%% loop will fail.
%% So, if we get a passive-message here, we just give up (=skip).
active_n_flush_connect_msgs(Sock) ->
    %% This seems only to be needed on certain platforms
    active_n_flush_connect_msgs(os:type(), Sock).

active_n_flush_connect_msgs(_, Sock) ->
    do_active_n_flush_connect_msgs(Sock).
%% active_n_flush_connect_msgs({unix, freebsd}, Sock) ->
%%     do_active_n_flush_connect_msgs(Sock);
%% active_n_flush_connect_msgs(_, _) ->
%%     ok.

do_active_n_flush_connect_msgs(Sock) ->
    receive
        {sctp_passive, Sock} ->
            ?P("connect-flush-loop -> premature passive"),
            ?SKIPT("Too many addresses (premature passive)");

        {sctp, Sock,
         _FromIP, _FromPort,
         {[], #sctp_assoc_change{state = comm_up}}} ->
            ?P("connect-flush-loop -> "
               "connect message discard - assoc change : comm-up"),
            ok = inet:setopts(Sock, [{active, 1}]),
            do_active_n_flush_connect_msgs(Sock);

        {sctp, Sock,
         _FromIP, _FromPort,
         {[], #sctp_paddr_change{state    = addr_confirmed,
                                 addr     = Addr,
                                 error    = Error,
                                 assoc_id = AID}}} ->
            ?P("connect-flush-loop -> "
               "connect message discard - paddr change : addr-confirmed:"
               "~n   Addr:    ~p"
               "~n   Error:   ~p"
               "~n   AssocID: ~p", [Addr, Error, AID]),
            ok = inet:setopts(Sock, [{active, 1}]),
            do_active_n_flush_connect_msgs(Sock)

    after 5000 ->
            ok
    end.
            
active_n_send_loop(Count, SrcSock, SndAssoc, DstSock) ->
    active_n_send_loop(0, Count, SrcSock, SndAssoc, DstSock).

active_n_send_loop(Count, Count, _SndSock, _SndAssoc, RcvSock) ->
    ?P("send-loop -> we are done - wait for passive"),
    receive
        {sctp_passive, RcvSock} ->
            ?P("received passive"),
            ok
    after
        5000 ->
            ?P("UNEXPECTED TIMEOUT: "
               "~n   Message Queue:    ~p"
               "~n   Active:           ~p",
               [process_info(self(), messages),
                inet:getopts(RcvSock, [active])]),
            exit({error, timeout})
    end;

active_n_send_loop(Sent, Count, SndSock, SndAssoc, RcvSock) ->
    Msg = list_to_binary("message " ++ integer_to_list(Sent+1)),
    ?P("send-loop(~w,~w) -> send message (on ~p)", [Sent, Count, SndSock]),
    ok = gen_sctp:send(SndSock, SndAssoc, 0, Msg),
    receive
        {sctp, RcvSock, FromIP, FromPort, {[SR], Msg}}
        when is_record(SR, sctp_sndrcvinfo) ->
            ?P("send-loop(~w,~w) -> "
               "recv (expected) data message (on ~p):"
               "~n   Msg:  ~p"
               "~n   From: ~p, ~p",
               [Sent, Count,
                RcvSock, Msg, FromIP, FromPort]),
            active_n_send_loop(Sent+1, Count, SndSock, SndAssoc, RcvSock);
        
        {sctp, RcvSock, _FromIP, _FromPort, {_AncData, _Data}} ->
            %% ignore non-data messages
            %% we should not get any here because of the flush loop,
            %% but just in case...
            ?P("send-loop(~w,~w) -> "
               "ignore non-data messages (on ~p):"
               "~n   From:    ~p:~p"
               "~n   AncData: ~p"
               "~n   Data:    ~p",
               [Sent, Count,
                RcvSock, _FromIP, _FromPort, _AncData, _Data]),
            
            %% It may be too late to update here,
            %% the socket may already have gone passive 
            %% and generated a passive message!
            
            ok = inet:setopts(RcvSock, [{active, 1}]),

            active_n_send_loop(Sent, Count, SndSock, SndAssoc, RcvSock);
        
        Other ->
            ?P("send-loop(~w,~w) -> "
               "UNEXPECTED: "
               "~n   Other:     ~p"
               "~n   Send Sock: ~p"
               "~n   Recv Sock: ~p", [Sent, Count,
                                      Other, SndSock, RcvSock]),
            exit({unexpected, Other})
    after
        5000 ->
            exit({error,timeout})
    end.
    
%% Hello world stream socket.
basic_stream(Config) when is_list(Config) ->
    {ok,S} = gen_sctp:open([{type,stream}]),
    ok = gen_sctp:listen(S, true),
    ok =
	do_from_other_process(
	  fun () -> gen_sctp:listen(S, 10) end),
    ok = gen_sctp:close(S),
    ok.

%% Minimal data transfer.
xfer_stream_min(Config) when is_list(Config) ->
    {_, OSName} = os:type(),
    Stream = 0,
    Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    Loopback = {127,0,0,1},
    {ok,Sb} = gen_sctp:open([{type,seqpacket}]),
    ?LOGVAR(Sb),
    {ok,Pb} = inet:port(Sb),
    ?LOGVAR(Pb),
    ok = gen_sctp:listen(Sb, true),

    {ok,Sa} = gen_sctp:open([{type,stream}]),
    ?LOGVAR(Sa),
    {ok,Pa} = inet:port(Sa),
    ?LOGVAR(Pa),
    #sctp_assoc_change{state=comm_up,
		       error=0,
		       outbound_streams=SaOutboundStreams,
		       inbound_streams=SaInboundStreams,
		       assoc_id=SaAssocId_X} =
	log_ok(gen_sctp:connect(Sa, Loopback, Pb, [])),
    ?LOGVAR(SaAssocId_X),
    [{_,#sctp_paddrinfo{assoc_id=SaAssocId,state=active}}] =
	log_ok(inet:getopts(Sa, [{sctp_get_peer_addr_info,
				  #sctp_paddrinfo{address={Loopback,Pb}}}])),
    ?LOGVAR(SaAssocId),
    match_unless_solaris(SaAssocId_X, SaAssocId),

    {SbOutboundStreams,SbInboundStreams,SbAssocId} =
	case recv_event(log_ok(gen_sctp:recv(Sb, infinity))) of
	    {Loopback,Pa,
	     #sctp_assoc_change{state=comm_up,
				error=0,
				outbound_streams=OS,
				inbound_streams=IS,
				assoc_id=AI}} ->
		{OS,IS,AI};
	    {Loopback,Pa,
	     #sctp_paddr_change{state=addr_confirmed,
				addr={Loopback,Pa},
				error=0,
				assoc_id=AI}} ->
		{Loopback,Pa,
		 #sctp_assoc_change{state=comm_up,
				    error=0,
				    outbound_streams=OS,
				    inbound_streams=IS,
				    assoc_id=AI}} =
		    recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
		{OS,IS,AI}
	end,
    ?LOGVAR(SbAssocId),
    SaOutboundStreams = SbInboundStreams,
    ?LOGVAR(SaOutboundStreams),
    SbOutboundStreams = SaInboundStreams,
    ?LOGVAR(SbOutboundStreams),
    ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    case log_ok(gen_sctp:recv(Sb, infinity)) of
	{Loopback,
	 Pa,
	 [#sctp_sndrcvinfo{stream   = Stream,
			   assoc_id = SbAssocId}],
	 Data} ->
            ?P("[1] received expected data with ancillary data => done"),
            ok;

	{Loopback,
	 Pa,
         [],
	 #sctp_paddr_change{addr     = {Loopback,_},
			    state    = addr_available,
			    error    = 0,
			    assoc_id = SbAssocId}} ->
            ?P("[2] received paddr change => recv again"),
            Res2 = log_ok(gen_sctp:recv(Sb, infinity)),
            ?P("[2] recv ok => "
               "~n   ~p", [Res2]),
	    {Loopback,
	     Pa,
	     [#sctp_sndrcvinfo{stream   = Stream,
			       assoc_id = SbAssocId}],
	     Data} = Res2,
            ?P("[2] received expected data with ancillary data => done"),
            Res2;

	{Loopback,
	 Pa,
	 [#sctp_sndrcvinfo{stream    = Stream,
			   assoc_id  = SbAssocId}],
	 #sctp_paddr_change{addr     = {Loopback,_},
			    state    = addr_confirmed,
			    error    = 0,
			    assoc_id = SbAssocId}} ->
            ?P("[3] received paddr change with ancillary data => recv again"),
            Res3 = log_ok(gen_sctp:recv(Sb, infinity)),
            ?P("[3] recv ok => "
               "~n   ~p", [Res3]),
	    {Loopback,
	     Pa,
	     [#sctp_sndrcvinfo{stream   = Stream,
			       assoc_id = SbAssocId}],
	     Data} = Res3,
            ?P("[3] received expected data with ancillary data => done"),
            Res3;

        %% It seems that on FreeBSD (for instance) we don't get any
        %% AncData with this.
	{Loopback,
	 Pa,
	 [],
	 #sctp_paddr_change{addr     = {Loopback,_},
			    state    = addr_confirmed,
			    error    = 0,
			    assoc_id = SbAssocId}} when (OSName =:= freebsd) ->
            ?P("[4] received paddr change without ancillary data => "
               "recv again"),
            Res4 = log_ok(gen_sctp:recv(Sb, infinity)),
            ?P("[4] recv ok => "
               "~n   ~p", [Res4]),
	    {Loopback,
	     Pa,
	     [#sctp_sndrcvinfo{stream   = Stream,
                               assoc_id = SbAssocId}],
	     Data} = Res4,
            ?P("[4] received expected data with ancillary data => done"),
            Res4;

        {FromIPX, FromPortX, AncDataX, DataX} = Other1 ->
            ?P("UNEXPECTED: "
               "~n   FromIP:   ~p"
               "~n   FromPort: ~p"
               "~n   AncData:  ~p"
               "~n   Data:     ~p"
               "~nwhen"
               "~n   Loopback: ~p"
               "~n   Pa:       ~p",
               [FromIPX, FromPortX, AncDataX, DataX, Loopback, Pa]),
            exit({unexpected, Other1});
        Other2 ->
            ?P("UNEXPECTED: "
               "~n   Other:    ~p"
               "~nwhen"
               "~n   Loopback: ~p"
               "~n   Pa:       ~p",
               [Other2, Loopback, Pa]),
            exit({unexpected, Other2})
    end,
    ok =
	do_from_other_process(
	  fun () -> gen_sctp:send(Sb, SbAssocId, 0, Data) end),
    case log_ok(gen_sctp:recv(Sa, infinity)) of
	{Loopback,Pb,
	 [#sctp_sndrcvinfo{stream=Stream,
			   assoc_id=SaAssocId}],
	 Data} -> ok;
	Event1 ->
	    {Loopback,Pb,
	     #sctp_paddr_change{state=addr_confirmed,
				addr={_,Pb},
				error=0,
				assoc_id=SaAssocId}} =
		recv_event(Event1),
	    {Loopback,Pb,
	     [#sctp_sndrcvinfo{stream=Stream,
			       assoc_id=SaAssocId}],
	     Data} =
		log_ok(gen_sctp:recv(Sa, infinity))
    end,
    ok = gen_sctp:close(Sa),
    {Loopback,Pa,
     #sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    {Loopback,Pa,
     #sctp_assoc_change{state=shutdown_comp,
			error=0,
			assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ok = gen_sctp:close(Sb),

    receive
	Msg -> ct:fail({received,Msg})
    after 17 -> ok
    end,
    ok.



do_from_other_process(Fun) ->
    Parent = self(),
    Ref = make_ref(),
    Child =
	spawn(fun () ->
		      try Fun() of
			  Result ->
			      Parent ! {Ref,Result}
		      catch
			  Class:Reason:Stacktrace ->
			      Parent ! {Ref,Class,Reason,Stacktrace}
		      end
	      end),
    Mref = erlang:monitor(process, Child),
    receive
	{Ref,Result} ->
	    receive {'DOWN',Mref,_,_,_} -> Result end;
	{Ref,Class,Reason,Stacktrace} ->
	    receive {'DOWN',Mref,_,_,_} ->
		    erlang:raise(Class, Reason, Stacktrace)
	    end;
	{'DOWN',Mref,_,_,Reason} ->
	    erlang:exit(Reason)
    end.


%% Peel off an SCTP stream socket ({active,once}).

peeloff_active_once(Config) ->
    peeloff(Config, [{active,once}]).

%% Peel off an SCTP stream socket ({active,true}).

peeloff_active_true(Config) ->
    peeloff(Config, [{active,true}]).

%% Peel off an SCTP stream socket ({active,N}).

peeloff_active_n(Config) ->
    peeloff(Config, [{active,1}]).

peeloff(Config, SockOpts) when is_list(Config) ->
    Addr = {127,0,0,1},
    Stream = 0,
    Timeout = 333,
    StartTime = timestamp(),
    S1 = socket_open([{ifaddr,Addr}|SockOpts], Timeout),
    ?LOGVAR(S1),
    P1 = socket_call(S1, get_port),
    ?LOGVAR(P1),
    Socket1 = socket_call(S1, get_socket),
    ?LOGVAR(Socket1),
    socket_call(S1, {listen,true}),
    S2 = socket_open([{ifaddr,Addr}|SockOpts], Timeout),
    ?LOGVAR(S2),
    P2 = socket_call(S2, get_port),
    ?LOGVAR(P2),
    Socket2 = socket_call(S2, get_socket),
    ?LOGVAR(Socket2),
    %%
    socket_call(S2, {connect_init,Addr,P1,[]}),
    S2Ai =
	receive
	    {S2,{Addr,P1,
		 #sctp_assoc_change{
		    state=comm_up,
		    assoc_id=AssocId2}}} -> AssocId2
	after Timeout ->
		socket_bailout([S1,S2], StartTime)
	end,
    ?LOGVAR(S2Ai),
    S1Ai =
	receive
	    {S1,{Addr,P2,
		 #sctp_assoc_change{
		    state=comm_up,
		    assoc_id=AssocId1}}} -> AssocId1
	after Timeout ->
		socket_bailout([S1,S2], StartTime)
	end,
    ?LOGVAR(S1Ai),
    %%
    socket_call(S2, {send,S2Ai,Stream,<<"Number one">>}),
    receive
	{S1,{Addr,P2,S1Ai,Stream,<<"Number one">>}} -> ok
    after Timeout ->
	    socket_bailout([S1,S2], StartTime)
    end,
    socket_call(S2, {send,Socket1,S1Ai,Stream,<<"Number two">>}),
    receive
	{S2,{Addr,P1,S2Ai,Stream,<<"Number two">>}} -> ok
    after Timeout ->
	    socket_bailout([S1,S2], StartTime)
    end,
    %%
    S3 = socket_peeloff(Socket1, S1Ai, SockOpts, Timeout),
    ?LOGVAR(S3),
    P3_X = socket_call(S3, get_port),
    ?LOGVAR(P3_X),
    P3 = case P3_X of 0 -> P1; _ -> P3_X end,
    [{_,#sctp_paddrinfo{assoc_id=S3Ai,state=active}}] =
	socket_call(S3,
		    {getopts,[{sctp_get_peer_addr_info,
			       #sctp_paddrinfo{address={Addr,P2}}}]}),
    %%S3Ai = S1Ai,
    ?LOGVAR(S3Ai),
    %%
    socket_call(S3, {send,S3Ai,Stream,<<"Number three">>}),
    receive
	{S2,{Addr,P3,S2Ai,Stream,<<"Number three">>}} -> ok
    after Timeout ->
	    socket_bailout([S1,S2,S3], StartTime)
    end,
    socket_call(S3, {send,Socket2,S2Ai,Stream,<<"Number four">>}),
    receive
	{S3,{Addr,P2,S3Ai,Stream,<<"Number four">>}} -> ok
    after Timeout ->
	    socket_bailout([S1,S2,S3], StartTime)
    end,
    %%
    inet:i(sctp),
    socket_close_verbose(S1, StartTime),
    socket_close_verbose(S2, StartTime),
    receive
	{S3,{Addr,P2,#sctp_shutdown_event{assoc_id=S3Ai_X}}} ->
	    match_unless_solaris(S3Ai, S3Ai_X)
    after Timeout ->
	    socket_bailout([S3], StartTime)
    end,
    receive
	{S3,{Addr,P2,#sctp_assoc_change{state=shutdown_comp,
					assoc_id=S3Ai}}} -> ok
    after Timeout ->
	    socket_bailout([S3], StartTime)
    end,
    socket_close_verbose(S3, StartTime),
    [] = flush(),
    ok.



%% Check sndbuf and recbuf behaviour.
buffers(Config) when is_list(Config) ->
    Limit = 4096,
    Addr = {127,0,0,1},
    Stream = 1,
    Timeout = 3333,
    StartTime = timestamp(),
    S1 = socket_open([{ip,Addr}], Timeout),
    ?LOGVAR(S1),
    P1 = socket_call(S1, get_port),
    ?LOGVAR(P1),
    ok = socket_call(S1, {listen,true}),
    S2 = socket_open([{ip,Addr}], Timeout),
    ?LOGVAR(S2),
    P2 = socket_call(S2, get_port),
    ?LOGVAR(P2),
    %%
    socket_call(S2, {connect_init,Addr,P1,[]}),
    S2Ai =
	receive
	    {S2,{Addr,P1,
		 #sctp_assoc_change{
		    state=comm_up,
		    assoc_id=AssocId2}}} -> AssocId2
	after Timeout ->
		socket_bailout([S1,S2], StartTime)
	end,
    S1Ai =
	receive
	    {S1,{Addr,P2,
		 #sctp_assoc_change{
		    state=comm_up,
		    assoc_id=AssocId1}}} -> AssocId1
	after Timeout ->
		socket_bailout([S1,S2], StartTime)
	end,
    %%
    socket_call(S1, {setopts,[{recbuf,Limit}]}),
    Recbuf =
	case socket_call(S1, {getopts,[recbuf]}) of
	    [{recbuf,RB1}] when RB1 >= Limit -> RB1
	end,
    Data = mk_data(Recbuf+Limit),
    socket_call(S2, {setopts,[{sndbuf,Recbuf+Limit}]}),
    socket_call(S2, {send,S2Ai,Stream,Data}),
    receive
	{S1,{Addr,P2,S1Ai,Stream,Data}} -> ok
    after Timeout ->
	    socket_bailout([S1,S2], StartTime)
    end,
    %%
    socket_close_verbose(S1, StartTime),
    receive
	{S2,{Addr,P1,#sctp_shutdown_event{assoc_id=S2Ai}}} -> ok
    after Timeout ->
	    socket_bailout([S2], StartTime)
    end,
    receive
	{S2,{Addr,P1,#sctp_assoc_change{state=shutdown_comp,
					assoc_id=S2Ai}}} -> ok
    after Timeout ->
	    socket_bailout([S2], StartTime)
    end,
    socket_close_verbose(S2, StartTime),
    [] = flush(),
    ok.

mk_data(Bytes) ->
    mk_data(0, Bytes, <<>>).
%%
mk_data(N, Bytes, Bin) when N < Bytes ->
    mk_data(N+4, Bytes, <<Bin/binary,N:32>>);
mk_data(_, _, Bin) ->
    Bin.



%% Test opening a multihoming ipv4 socket.
open_multihoming_ipv4_socket(Config) when is_list(Config) ->
    ?P("get addrs by family (inet)"),
    case get_addrs_by_family(inet, 2) of
	{ok, [Addr1, Addr2]} ->
            ?P("got addrs: "
               "~n      Addr1: ~p"
               "~n      Addr2: ~p", [Addr1, Addr2]),
	    do_open_and_connect([Addr1, Addr2], Addr1);
	{error, Reason} ->
            ?P("failed get addrs: "
               "~n      ~p", [Reason]),
	    {skip, Reason}
    end.

%% This test is mostly aimed to indicate whether host has a
%% non-working ipv6 setup.  Test opening a unihoming (non-multihoming)
%% ipv6 socket.
open_unihoming_ipv6_socket(Config) when is_list(Config) ->
    case get_addrs_by_family(inet6, 1) of
	{ok, [Addr]} ->
	    do_open_and_connect([Addr], Addr);
	{error, Reason} ->
	    {skip, Reason}
    end.


%% Test opening a multihoming ipv6 socket.
open_multihoming_ipv6_socket(Config) when is_list(Config) ->
    case get_addrs_by_family(inet6, 2) of
	{ok, [Addr1, Addr2]} ->
	    do_open_and_connect([Addr1, Addr2], Addr1);
	{error, Reason} ->
	    {skip, Reason}
    end.

%% Test opening a multihoming ipv6 socket with ipv4 and ipv6 addresses.
open_multihoming_ipv4_and_ipv6_socket(Config) when is_list(Config) ->
    case get_addrs_by_family(inet_and_inet6, 2) of
	{ok, [[InetAddr1, InetAddr2], [Inet6Addr1, Inet6Addr2]]} ->
	    %% Connect to the first address to test bind
	    do_open_and_connect([InetAddr1, Inet6Addr1, InetAddr2],
				InetAddr1),
	    do_open_and_connect([Inet6Addr1, InetAddr1],
				Inet6Addr1),

	    %% Connect an address, not the first,
	    %% to test sctp_bindx
	    do_open_and_connect([Inet6Addr1, Inet6Addr2, InetAddr1],
				Inet6Addr2),
	    do_open_and_connect([Inet6Addr1, Inet6Addr2, InetAddr1],
				InetAddr1);
	{error, Reason} ->
	    {skip, Reason}
    end.

%% Test inet:socknames/peernames on unihoming IPv4 sockets.
names_unihoming_ipv4(Config) when is_list(Config) ->
    do_names(Config, inet, 1).

%% Test inet:socknames/peernames on unihoming IPv6 sockets.
names_unihoming_ipv6(Config) when is_list(Config) ->
    do_names(Config, inet6, 1).

%% Test inet:socknames/peernames on multihoming IPv4 sockets.
names_multihoming_ipv4(Config) when is_list(Config) ->
    do_names(Config, inet, 2).

%% Test inet:socknames/peernames on multihoming IPv6 sockets.
names_multihoming_ipv6(Config) when is_list(Config) ->
    do_names(Config, inet6, 2).



do_names(_, FamilySpec, AddressCount) ->
    Fun =
	fun (ServerSocket, _, ServerAssoc, ClientSocket, _, ClientAssoc) ->
		ServerSocknamesNoassoc =
		    lists:sort(ok(inet:socknames(ServerSocket))),
		?LOGVAR(ServerSocknamesNoassoc),
		ServerSocknames =
		    lists:sort(ok(inet:socknames(ServerSocket, ServerAssoc))),
		?LOGVAR(ServerSocknames),
		[_|_] =
		    ordsets:intersection
		      (ServerSocknamesNoassoc, ServerSocknames),
		ClientSocknamesNoassoc =
		    lists:sort(ok(inet:socknames(ClientSocket))),
		?LOGVAR(ClientSocknamesNoassoc),
		ClientSocknames =
		    lists:sort(ok(inet:socknames(ClientSocket, ClientAssoc))),
		?LOGVAR(ClientSocknames),
		[_|_] =
		    ordsets:intersection
		      (ClientSocknamesNoassoc, ClientSocknames),
		err([einval,enotconn], inet:peernames(ServerSocket)),
		ServerPeernames =
		    lists:sort(ok(inet:peernames(ServerSocket, ServerAssoc))),
		?LOGVAR(ServerPeernames),
		err([einval,enotconn], inet:peernames(ClientSocket)),
		ClientPeernames =
		    lists:sort(ok(inet:peernames(ClientSocket, ClientAssoc))),
		?LOGVAR(ClientPeernames),
		ServerSocknames = ClientPeernames,
		ClientSocknames = ServerPeernames,
		{ok,Socket} =
		    gen_sctp:peeloff(ServerSocket, ServerAssoc),
		SocknamesNoassoc =
		    lists:sort(ok(inet:socknames(Socket))),
		?LOGVAR(SocknamesNoassoc),
		Socknames =
		    lists:sort(ok(inet:socknames(Socket, ServerAssoc))),
		?LOGVAR(Socknames),
		true =
		    ordsets:is_subset(SocknamesNoassoc, Socknames),
		Peernames =
		    lists:sort(ok(inet:peernames(Socket, ServerAssoc))),
		?LOGVAR(Peernames),
		ok = gen_sctp:close(Socket),
		Socknames = ClientPeernames,
		ClientSocknames = Peernames,
		ok
	end,
    case get_addrs_by_family(FamilySpec, AddressCount) of
	{ok, Addresses} when length(Addresses) =:= AddressCount ->
	    do_open_and_connect(Addresses, hd(Addresses), Fun);
	{error, Reason} ->
	    {skip, Reason}
    end.



get_addrs_by_family(Family, NumAddrs) ->
    case os:type() of
	{unix,linux} ->
	    get_addrs_by_family_aux(Family, NumAddrs);
	{unix,freebsd} ->
	    get_addrs_by_family_aux(Family, NumAddrs);
	{unix,sunos} ->
	    case get_addrs_by_family_aux(Family, NumAddrs) of
		{ok, [InetAddrs, Inet6Addrs]} when Family =:= inet_and_inet6 ->
		    %% Man page for sctp_bindx on Solaris says: "If sock is an
		    %% Internet Protocol Version 6 (IPv6) socket, addrs should
		    %% be an array of sockaddr_in6 structures containing IPv6
		    %% or IPv4-mapped IPv6 addresses."
		    {ok, [ipv4_map_addrs(InetAddrs), Inet6Addrs]};
		{ok, Addrs} ->
		    {ok, Addrs};
		{error, Reason} ->
		    {error, Reason}
	    end;
	Os ->
	    Reason = if Family =:= inet_and_inet6 ->
			     ?F("Mixing ipv4 and ipv6 addresses for "
			       " multihoming has not been verified on ~p",
                                [Os]);
			true ->
			     ?F("Multihoming for ~p has not been verified "
                                "on ~p", [Family, Os])
		     end,
	    {error, Reason}
    end.

get_addrs_by_family_aux(Family, NumAddrs) when Family =:= inet;
					       Family =:= inet6 ->
    case inet:getaddr(localhost, Family) of
	{error, eafnosupport = Reason} ->
            ?P("failed get (~w) addrs for localhost: ~p", [Family, Reason]),
	    {error, ?F("No support for ~p (~p)", [Family, Reason])};
        {error, nxdomain = Reason} ->
            ?P("failed get (~w) addrs for localhost: ~p", [Family, Reason]),
	    {error, ?F("No support for ~p (~p)", [Family, Reason])};
	{ok, _} ->
            ?P("got addr for localhost (ignored)"),
	    IfAddrs = ok(inet:getifaddrs()),
            ?P("IfAddrs: ~p", [IfAddrs]),
	    case filter_addrs_by_family(IfAddrs, Family) of
		Addrs when length(Addrs) >= NumAddrs ->
		    {ok, lists:sublist(Addrs, NumAddrs)};
		[] ->
		    {error, ?F("Need ~p ~p address(es) found none~n",
                               [NumAddrs, Family])};
		Addrs ->
		    {error,
		     ?F("Need ~p ~p address(es) found only ~p: ~p~n",
                        [NumAddrs, Family, length(Addrs), Addrs])}
	    end
    end;
get_addrs_by_family_aux(inet_and_inet6, NumAddrs) ->
    try {ok, [case get_addrs_by_family_aux(Family, NumAddrs) of
                  {ok, Addrs}     -> Addrs;
                  {error, Reason} -> throw({error, Reason})
              end || Family <- [inet, inet6]]}
    catch
        throw:{error, _} = ERROR ->
            ERROR
    end.

filter_addrs_by_family(IfAddrs, Family) ->
    lists:flatten([[Addr || {addr, Addr} <- Info,
			    is_good_addr(Addr, Family)]
		   || {_IfName, Info} <- IfAddrs]).

is_good_addr(Addr, inet) when tuple_size(Addr) =:= 4 ->
    true;
is_good_addr({0,0,0,0,0,16#ffff,_,_}, inet6) ->
    false; %% ipv4 mapped
is_good_addr({16#fe80,_,_,_,_,_,_,_}, inet6) ->
    false; %% link-local
is_good_addr(Addr, inet6) when tuple_size(Addr) =:= 8 ->
    true;
is_good_addr(_Addr, _Family) ->
    false.

ipv4_map_addrs(InetAddrs) ->
    [begin
	 <<AB:16>> = <<A,B>>,
	 <<CD:16>> = <<C,D>>,
	 {0, 0, 0, 0, 0, 16#ffff, AB, CD}
     end || {A,B,C,D} <- InetAddrs].

do_open_and_connect(ServerAddresses, AddressToConnectTo) ->
    Fun = fun (_, _, _, _, _, _) -> ok end,
    do_open_and_connect(ServerAddresses, AddressToConnectTo, Fun).
%%
do_open_and_connect(ServerAddresses, AddressToConnectTo, Fun) ->
    {ServerFamily, ServerOpts} = get_family_by_addrs(ServerAddresses),
    io:format("Serving ~p addresses: ~p~n",
	      [ServerFamily, ServerAddresses]),
    S1 = ok(gen_sctp:open(0, [{ip,Addr} || Addr <- ServerAddresses] ++
			      [ServerFamily|ServerOpts])),
    ok = gen_sctp:listen(S1, true),
    P1 = ok(inet:port(S1)),
    ClientFamily = get_family_by_addr(AddressToConnectTo),
    io:format("Connecting to ~p ~p~n",
	      [ClientFamily, AddressToConnectTo]),
    ClientOpts =
	[ClientFamily |
	 case ClientFamily of
	     inet6 ->
		 [{ipv6_v6only,true}];
	     _ ->
		 []
	 end],
    S2 = ok(gen_sctp:open(0, ClientOpts)),
    log(open),
    %% Verify client can connect
    #sctp_assoc_change{state=comm_up} = S2Assoc =
	ok(gen_sctp:connect(S2, AddressToConnectTo, P1, [])),
    log(comm_up),
    %% verify server side also receives comm_up from client
    S1Assoc = recv_comm_up_eventually(S1),
    Result = Fun(S1, ServerFamily, S1Assoc, S2, ClientFamily, S2Assoc),
    ok = gen_sctp:close(S2),
    ok = gen_sctp:close(S1),
    Result.

%% If at least one of the addresses is an ipv6 address, return inet6, else inet.
get_family_by_addrs(Addresses) ->
    case lists:usort([get_family_by_addr(Addr) || Addr <- Addresses]) of
	[inet, inet6] -> {inet6, [{ipv6_v6only, false}]};
	[inet]        -> {inet,  []};
	[inet6]       -> {inet6, []}
    end.

get_family_by_addr(Addr) when tuple_size(Addr) =:= 4 -> inet;
get_family_by_addr(Addr) when tuple_size(Addr) =:= 8 -> inet6.

recv_comm_up_eventually(S) ->
    case ok(gen_sctp:recv(S)) of
	{_Addr, _Port, _Info,
	 #sctp_assoc_change{state=comm_up} = Assoc} ->
	    Assoc;
	{_Addr, _Port, _Info, _OtherSctpMsg} = Msg ->
	    log({unexpected,Msg}),
	    recv_comm_up_eventually(S)
    end.


%% 
recv_close(Config) when is_list(Config) ->
    ?P("create server socket (and listen)"),
    {ok, S} = gen_sctp:open(),
    gen_sctp:listen(S, true),
    {ok, SPort} = inet:port(S),

    ?P("create client socket (and connect)"),
    {ok, C} = gen_sctp:open(),
    {ok, _} = gen_sctp:connect(C, localhost, SPort, []),

    TC = self(),
    RECV = fun() ->
                   ?P("try setup recv(s)"),
                   ok = recv_close_setup_recv(S),
                   ?P("announce ready"),
                   TC ! {self(), ready},
                   ?P("try data recv"),
                   Res = gen_sctp:recv(S),
                   ?P("recv res: "
                      "~n   ~p", [Res]),
                   exit(Res)
           end,
    ?P("spawn reader - then await reader ready"),
    {Pid, MRef} = spawn_monitor(RECV),
    receive
        {'DOWN', MRef, process, Pid, PreReason} ->
            %% Make sure it does not die for some other reason...
            ?P("unexpected reader termination:"
               "~n   ~p", [PreReason]),
            (catch gen_sctp:close(S)),
            (catch gen_sctp:close(C)),
            ct:fail("Unexpected pre close from reader (~p): ~p",
                          [Pid, PreReason]);
        {Pid, ready} ->
            ?P("reader ready"),
            ok
    after 30000 -> % Just in case...
            %% This is **extreme**, but there is no way to know
            %% how long it will take to iterate through all the
            %% addresses of a host...
            ?P("reader ready timeout"),
            (catch gen_sctp:close(S)),
            (catch gen_sctp:close(C)),
            ct:fail("Unexpected pre close timeout (~p)", [Pid])
    end,

    ?P("\"ensure\" reader reading..."),
    receive
        Any ->
            ?P("Received unexpected message: "
               "~n   ~p", [Any]),
            (catch gen_sctp:close(S)),
            (catch gen_sctp:close(C)),
            ct:fail("Unexpected message: ~p", [Any])
    after 5000 ->
            ok
    end,

    ?P("close server socket"),
    ok = gen_sctp:close(S),
    ?P("await reader termination"),
    receive
        {'DOWN', MRef, process, Pid, {error, closed}} ->
            ?P("expected reader termination result"),
            (catch gen_sctp:close(C)),
            ok;
        {'DOWN', MRef, process, Pid, PostReason} ->
            ?P("unexpected reader termination: "
               "~n   ~p", [PostReason]),
            (catch gen_sctp:close(C)),
            ct:fail("Unexpected post close from reader (~p): ~p",
                          [Pid, PostReason])
    after 5000 ->
            ?P("unexpected reader termination timeout"),
            demonitor(MRef, [flush]),
            (catch gen_sctp:close(C)),
            exit(Pid, kill),
            ct:fail("Reader (~p) termination timeout", [Pid])
    end,
    ?P("close client socket"),
    (catch gen_sctp:close(C)),
    ?P("done"),
    ok.


recv_close_setup_recv(S) ->
    recv_close_setup_recv(S, 1).

recv_close_setup_recv(S, N) ->
    ?P("try setup recv ~w", [N]),
    case gen_sctp:recv(S, 5000) of
        {ok, {Addr,
              Port,
              _AncData, 
              Data}} when is_tuple(Addr) andalso is_integer(Port) ->
            ?P("setup recv ~w: "
               "~n   ~p", [N, Data]),
            recv_close_setup_recv(S, N+1);
        {error, timeout} ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is the most basic of (sockaddr) tests.

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
                    is_not_freebsd()
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
                        {ok, [#{addr := #{scope_id := ScopeID}} = H|T]} ->
                            ?P("found link-local candidate(s): "
                               "~n   Candidate:         ~p"
                               "~n   Rest Candidate(s): ~p", [H, T]),
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

do_simple_sockaddr_send_recv(#{family := _Fam} = SockAddr, _) ->
    %% Create the server
    Self   = self(),
    ?P("~n      SockAddr: ~p", [SockAddr]),
    ServerF = fun() ->
                      ?P("[server] try create (open) listen socket"),
                      Sock =
                          try gen_sctp:open([{ifaddr, SockAddr},
                                             {active, true},
                                             binary]) of
                              {ok, S} ->
                                  S;
                              {error, OReason} ->
                                  ?P("open error: "
                                     "~n      Reason: ~p", [OReason]),
                                  exit({open_error, OReason})
                          catch
                              OC:OE:OS ->
                                  ?P("open failure: "
                                     "~n      Error Class: ~p"
                                     "~n      Error:       ~p"
                                     "~n      Call Stack:  ~p", [OC, OE, OS]),
                                  exit({open_failure, {OC, OE, OS}})
                          end,

                      ?P("[server] try make 'listen' socket"),
                      ok = gen_sctp:listen(Sock, true),

                      ?P("[server] try get port number"),
                      {ok, Port}  = inet:port(Sock),
                      ?P("[server] port: ~w", [Port]),
                      Self ! {{port, Port}, self()},


                      ?P("try accept (await 'accept' message)"),
                      AssocID1 =
                          receive
                              {sctp,
                               Sock,
                               FromIP11, FromPort11,
                               {ANC1, #sctp_assoc_change{state            = comm_up,
                                                         error            = 0,
                                                         outbound_streams = OutStreams1,
                                                         inbound_streams  = InStreams1,
                                                         assoc_id         = AID1}}} ->
                                  ?P("[server] received assoc - accepted: "
                                     "~n      From IP:     ~p"
                                     "~n      From Port:   ~p"
                                     "~n      ANC:         ~p"
                                     "~n      Out Streams: ~p"
                                     "~n      In Streams:  ~p"
                                     "~n      Assoc ID:    ~p",
                                     [FromIP11, FromPort11, ANC1,
                                      OutStreams1, InStreams1, AID1]),
                                  AID1
                          end,

                      ?P("[server] await message 2"),
                      receive
                          {sctp, Sock, FromIP12, FromPort12, {_, <<"hej">>}} ->
                              ?P("[server] received expected message 1: "
                                 "~n      FromIP2:   ~p"
                                 "~n      FromPort2: ~p"
                                 "~n      => send reply", [FromIP12, FromPort12]),
                              ok = gen_sctp:send(Sock, AssocID1, 1, "hopp")
                      end,


                      ?P("try accept (await 'accept' message)"),
                      AssocID2 =
                          receive
                              {sctp,
                               Sock,
                               FromIP21, FromPort21,
                               {ANC2, #sctp_assoc_change{state            = comm_up,
                                                         error            = 0,
                                                         outbound_streams = OutStreams2,
                                                         inbound_streams  = InStreams2,
                                                         assoc_id         = AID2}}} ->
                                  ?P("[server] received assoc - accepted: "
                                     "~n      FromIP:      ~p"
                                     "~n      FromPort:    ~p"
                                     "~n      ANC:         ~p"
                                     "~n      Out Streams: ~p"
                                     "~n      In Streams:  ~p"
                                     "~n      Assoc ID:    ~p",
                                     [FromIP21, FromPort21, ANC2,
                                      OutStreams2, InStreams2, AID2]),
                                  AID2
                          end,

                      ?P("[server] await message 1"),
                      receive
                          {sctp, Sock, FromIP22, FromPort22, {_, <<"hej">>}} ->
                              ?P("[server] received expected message 1: "
                                 "~n      From IP:   ~p"
                                 "~n      From Port: ~p"
                                 "~n      => send reply", [FromIP22, FromPort22]),
                              ok = gen_sctp:send(Sock, AssocID2, 1, "hopp")
                      end,


                      ?P("[server] await termination command"),
                      receive
                          {die, Self} ->
                              ?P("[server] terminating"),
                              (catch gen_tcp:close(Sock)),
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
    

    ?P("try create client socket 1"),
    {ok, CSock1} = gen_sctp:open([{ifaddr, SockAddr},
                                  {active, true},
                                  binary]),

    ?P("client socket 1: "
       "~n      CSock: ~p"
       "~n      CPort: ~p", [CSock1, inet:port(CSock1)]),

    ?P("try connect client socket 1"),
    ServerSockAddr = SockAddr#{port => ServerPort},
    {COutStreams1, CInStreams1, CAssocID1} =
        case gen_sctp:connect(CSock1, ServerSockAddr, [], ?SECS(1)) of
            {ok, #sctp_assoc_change{state            = comm_up,
                                    error            = 0,
                                    outbound_streams = COS1,
                                    inbound_streams  = CIS1,
                                    assoc_id         = CAID1}} ->
                {COS1, CIS1, CAID1};
            {ok, Unexpected1} ->
                ?P("<ERROR> unexpected connect (1) result: "
                   "~n      ~p", [Unexpected1]),
                ct:fail({unexpected_client_connect_result, 1, Unexpected1});
            {error, Reason1} ->
                ?P("<ERROR> failed client 1 connect: "
                   "~n      Reason: ~p", [Reason1]),
                ct:fail({client_connect_failed, 1, Reason1})
        end,

    ?P("client 1 connected: "
       "~n      Out Streams: ~p"
       "~n      In Streams:  ~p"
       "~n      Assoc ID:    ~p", [COutStreams1, CInStreams1, CAssocID1]),

    ?P("[client 1] try send message"),
    ok = gen_sctp:send(CSock1, CAssocID1, 1, "hej"),

    ?P("[client 1] await reply message"),
    receive
        {sctp, CSock1, _, _, {_, <<"hopp">>}} ->
            ?P("received expected reply message")
    end,


    ?P("[client 2] try create client socket"),
    {ok, CSock2} = gen_sctp:open([{ifaddr, SockAddr},
                                  {active, true},
                                  binary]),

    ?P("[client 2] client socket: "
       "~n      CSock: ~p"
       "~n      CPort: ~p", [CSock2, inet:port(CSock2)]),

    ?P("[client 2] try connect-init"),
    ServerSockAddr = SockAddr#{port => ServerPort},
    case gen_sctp:connect_init(CSock2, ServerSockAddr, []) of
        ok ->
            ok;
        {error, Reason2} ->
            ?P("<ERROR> failed client 2 connect: "
               "~n      Reason: ~p", [Reason2]),
            ct:fail({client_connect_failed, 2, Reason2})
    end,

    ?P("[client 2] await connect completion"),
    CAssocID2 =
        receive
            {sctp,
             CSock2,
             CFromIP2, CFromPort2,
             {CANC2, #sctp_assoc_change{state            = comm_up,
                                        error            = 0,
                                        outbound_streams = COutStreams2,
                                        inbound_streams  = CInStreams2,
                                        assoc_id         = CAID2}}} ->
                ?P("[client 2] connected: "
                   "~n      From IP:     ~p"
                   "~n      From Port:   ~p"
                   "~n      ANC:         ~p"
                   "~n      Out Streams: ~p"
                   "~n      In Streams:  ~p"
                   "~n      Assoc ID:    ~p",
                   [CFromIP2, CFromPort2, CANC2,
                    COutStreams2, CInStreams2, CAID2]),
                CAID2
        end,

    ?P("[client 2] try send message"),
    ok = gen_sctp:send(CSock2, CAssocID2, 1, "hej"),

    ?P("[client 2] await reply message"),
    receive
        {sctp, CSock2, _, _, {_, <<"hopp">>}} ->
            ?P("[client 2] received expected reply message")
    end,


    ?P("terminate server"),
    Server ! {die, self()},

    ?P("await server termination"),
    receive
        {'EXIT', Server, normal} ->
            ok
    end,
    
    ?P("cleanup"),
    (catch gen_sctp:close(CSock1)),
    (catch gen_sctp:close(CSock2)),

    ?P("done"),
    ok.

    
%% Test the sockaddr version of connectx_init/4.
t_simple_local_sockaddr_in_connectx_init(Config) when is_list(Config) ->
    Localhost = {127,0,0,1},
    {ok,Sb} = gen_sctp:open(),
    {ok,Pb} = inet:port(Sb),
    {ok,Sa} = gen_sctp:open(),
    SockAddr = #{family   => inet,
                 addr     => Localhost,
                 port     => Pb},
    case gen_sctp:connectx_init(Sa, [SockAddr], []) of
        {ok, A1} ->
	    true = (A1 =/= 0),
            {Localhost,Pb,#sctp_assoc_change{state=cant_assoc, assoc_id = A1}} =
                recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
            ok = gen_sctp:listen(Sb, true),
            {ok, A2} = gen_sctp:connectx_init(Sa, [SockAddr], []),
	    true = (A2 =/= 0),
            {Localhost,Pb,#sctp_assoc_change{state=comm_up, assoc_id = A2}} =
                recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
            ok = gen_sctp:close(Sa),
            ok = gen_sctp:close(Sb);
        {error, enotsup} ->
            ok
    end,
    ok.


%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% socket gen_server ultra light

socket_open(SockOpts0, Timeout) ->
    SockOpts =
	case lists:keyfind(active,1,SockOpts0) of
	    false ->
		[{active,once}|SockOpts0];
	    _ ->
		SockOpts0
	end,
    Opts = [{type,seqpacket},binary|SockOpts],
    Starter =
	fun () ->
		{ok,Socket} =
		    gen_sctp:open(Opts),
		Socket
	end,
    s_start(Starter, Timeout).

socket_peeloff(Socket, AssocId, SocketOpts, Timeout) ->
    Opts = [binary|SocketOpts],
    Starter =
	fun () ->
		{ok,NewSocket} =
		    gen_sctp:peeloff(Socket, AssocId),
		ok = inet:setopts(NewSocket, Opts),
		NewSocket
	end,
    s_start(Starter, Timeout).

socket_close_verbose(S, StartTime) ->
    History = socket_history(socket_close(S), StartTime),
    io:format("socket_close ~p:~n    ~p.~n", [S,History]),
    History.

socket_close(S) ->
    s_req(S, close).

socket_call(S, Request) ->
    s_req(S, {Request}).

%% socket_get(S, Key) ->
%%     s_req(S, {get,Key}).

socket_bailout([S|Ss], StartTime) ->
    History = socket_history(socket_close(S), StartTime),
    io:format("bailout ~p:~n    ~p.~n", [S,History]),
    socket_bailout(Ss, StartTime);
socket_bailout([], _) ->
    io:format("flush: ~p.~n", [flush()]),
    ct:fail(socket_bailout).

socket_history({State,Flush}, StartTime) ->
    {lists:keysort(
       2,
       lists:flatten(
	 [[{Key,{TS-StartTime,Val}} || {TS,Val} <- Vals]
	  || {Key,Vals} <- gb_trees:to_list(State)])),
     Flush}.

s_handler(Socket) ->
    fun ({listen,Listen}) ->
	    ok = gen_sctp:listen(Socket, Listen);
	(get_port) ->
	    ok(inet:port(Socket));
	(get_socket) ->
	    Socket;
	({connect_init,ConAddr,ConPort,ConOpts}) ->
	    ok = gen_sctp:connect_init(Socket, ConAddr, ConPort, ConOpts);
	({send,AssocId,Stream,Data}) ->
	    ok = gen_sctp:send(Socket, AssocId, Stream, Data);
	({send,OtherSocket,AssocId,Stream,Data}) ->
	    ok = gen_sctp:send(OtherSocket, AssocId, Stream, Data);
	({setopts,Opts}) ->
	    ok = inet:setopts(Socket, Opts);
	({getopts,Optnames}) ->
	    ok(inet:getopts(Socket, Optnames))
    end.

s_req(S, Req) ->
    Mref = erlang:monitor(process, S),
    S ! {self(),Mref,Req},
    receive
	{'DOWN',Mref,_,_,Error} ->
	    exit(Error);
	{S,Mref,Reply} ->
	    erlang:demonitor(Mref, [flush]),
	    Reply
    end.

s_start(Starter, Timeout) ->
    Parent = self(),
    Owner =
	spawn_link(
	  fun () ->
		  s_start(Starter(), Timeout, Parent)
	  end),
    Owner.

s_start(Socket, Timeout, Parent) ->
    Handler = s_handler(Socket),
    try
	s_loop(Socket, Timeout, Parent, Handler, gb_trees:empty())
    catch
	Class:Reason:Stacktrace ->
	    io:format(?MODULE_STRING":socket exception ~w:~w at~n"
		      "~p.~n", [Class,Reason,Stacktrace]),
	    erlang:raise(Class, Reason, Stacktrace)
    end.

s_loop(Socket, Timeout, Parent, Handler, State) ->
    receive
	{Parent,Ref,close} -> % socket_close()
	    erlang:send_after(Timeout, self(), {Parent,Ref,exit}),
	    s_loop(Socket, Timeout, Parent, Handler, State);
	{Parent,Ref,exit} ->
	    ok = gen_sctp:close(Socket),
	    Key = exit,
	    NewState = gb_push(Key, Socket, State),
	    Parent ! {self(),Ref,{NewState,flush()}};
	{Parent,Ref,{Msg}} ->
	    Result = Handler(Msg),
	    Key = req,
	    NewState = gb_push(Key, {Msg,Result}, State),
	    Parent ! {self(),Ref,Result},
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	%% {Parent,Ref,{get,Key}} ->
	%%     Parent ! {self(),Ref,gb_get(Key, State)},
	%%     s_loop(Socket, Timeout, Parent, Handler, State);
	{sctp,Socket,Addr,Port,
	 {[#sctp_sndrcvinfo{stream=Stream,assoc_id=AssocId}=SRI],Data}}
	  when not is_tuple(Data) ->
	    case gb_get({assoc_change,AssocId}, State) of
		[{Addr,Port,
		  #sctp_assoc_change{
		     state=comm_up,
		     inbound_streams=Is}}|_]
		  when 0 =< Stream, Stream < Is-> ok;
		[] -> ok
	    end,
	    Key = {msg,AssocId,Stream},
	    NewState = gb_push(Key, {Addr,Port,SRI,Data}, State),
	    Parent ! {self(),{Addr,Port,AssocId,Stream,Data}},
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	{sctp,Socket,Addr,Port,
	 {SRI,#sctp_assoc_change{assoc_id=AssocId,state=St}=SAC}} ->
	    case SRI of
		[#sctp_sndrcvinfo{assoc_id=AssocId,stream=0}] -> ok;
		[] -> ok
	    end,
	    Key = {assoc_change,AssocId},
	    case {gb_get(Key, State),St} of
		{[],_} -> ok;
		{[{Addr,Port,#sctp_assoc_change{state=comm_up}}|_],_}
		  when St =:= comm_lost; St =:= shutdown_comp -> ok
	    end,
	    NewState = gb_push(Key, {Addr,Port,SAC}, State),
	    Parent ! {self(),{Addr,Port,SAC}},
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	{sctp,Socket,Addr,Port,
	 {SRI,#sctp_paddr_change{assoc_id=AssocId,
				 addr={_,P},
				 state=St}=SPC}} ->
	    match_unless_solaris(Port, P),
	    case SRI of
		[#sctp_sndrcvinfo{assoc_id=AssocId,stream=0}] -> ok;
		[] -> ok
	    end,
	    case {gb_get({assoc_change,AssocId}, State),St} of
		{[{Addr,Port,#sctp_assoc_change{state=comm_up}}|_],_}
		  when St =:= addr_available;
		       St =:= addr_confirmed -> ok;
		{[],addr_confirmed} -> ok
	    end,
	    Key = {paddr_change,AssocId},
	    NewState = gb_push(Key, {Addr,Port,SPC}, State),
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	{sctp,Socket,Addr,Port,
	 {SRI,#sctp_shutdown_event{assoc_id=AssocId}=SSE}} ->
	    case SRI of
		[#sctp_sndrcvinfo{assoc_id=AssocId,stream=0}] -> ok;
		[] -> ok
	    end,
	    case gb_get({assoc_change,AssocId}, State) of
		[{Addr,Port,#sctp_assoc_change{state=comm_up}}|_] -> ok;
		[] -> ok
	    end,
	    Key = {shutdown_event,AssocId},
	    NewState = gb_push(Key, {Addr,Port}, State),
	    Parent ! {self(), {Addr,Port,SSE}},
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	Unexpected ->
	    erlang:error({unexpected,Unexpected})
    end.

again(Socket) ->
    receive
	{sctp_passive,Socket} ->
	    [{active, false}] = ok(inet:getopts(Socket, [active])),
	    ok = inet:setopts(Socket,[{active,1}])
    after 0 ->
	    ok = inet:setopts(Socket, [{active,once}])
    end.

gb_push(Key, Val, GBT) ->
    TS = timestamp(),
    case gb_trees:lookup(Key, GBT) of
	none ->
	    gb_trees:insert(Key, [{TS,Val}], GBT);
	{value,V} ->
	    gb_trees:update(Key, [{TS,Val}|V], GBT)
    end.

gb_get(Key, GBT) ->
    case gb_trees:lookup(Key, GBT) of
	none ->
	    [];
	{value,V} ->
	    [Val || {_TS,Val} <- V]
    end.

match_unless_solaris(A, B) ->
    case os:type() of
	{unix,sunos} -> B;
	_ -> A = B
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


is_not_freebsd() ->
    is_not_platform(freebsd, "FreeBSD").

is_not_platform(Platform, PlatformStr)
  when is_atom(Platform) andalso is_list(PlatformStr) ->
      case os:type() of
        {unix, Platform} ->
            skip("This does not work on " ++ PlatformStr);
        _ ->
            ok
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timestamp() ->
    erlang:monotonic_time().


pi(Item) ->
    {Item, Val} = process_info(self(), Item),
    Val.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not_supported(What) ->
    skip({not_supported, What}).

skip(Reason) ->
    throw({skip, Reason}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log_ok(X) ->
    log(ok(X)).

ok({ok, X}) ->
    X;
ok({error, X}) ->
    ?P("ERROR: ~p", [X]),
    ct:fail({unexpected_error, X});
ok(X) ->
    ?P("UNEXPECTED: ~p", [X]),
    ct:fail({unexpected, X}).
    

log(X) ->
    ?P("LOG: ~p", [X]),
    X.

err([], Result) ->
    erlang:error(Result);
err([Reason|_], {error,Reason}) ->
    ok;
err([_|Reasons], Result) ->
    err(Reasons, Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open_failed_str(Reason) ->
    ?F("Open failed: ~w", [Reason]).

