%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2016. All Rights Reserved.
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

%%-compile(export_all).

-export([all/0, suite/0,groups/0,
	 init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export(
   [skip_old_solaris/1,
    basic/1,
    api_open_close/1,api_listen/1,api_connect_init/1,api_opts/1,
    xfer_min/1,xfer_active/1,def_sndrcvinfo/1,implicit_inet6/1,
    open_multihoming_ipv4_socket/1,
    open_unihoming_ipv6_socket/1,
    open_multihoming_ipv6_socket/1,
    open_multihoming_ipv4_and_ipv6_socket/1,
    basic_stream/1, xfer_stream_min/1, active_n/1,
    peeloff_active_once/1, peeloff_active_true/1, peeloff_active_n/1,
    buffers/1,
    names_unihoming_ipv4/1, names_unihoming_ipv6/1,
    names_multihoming_ipv4/1, names_multihoming_ipv6/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    G = case is_old_solaris() of
	    true -> old_solaris;
	    false -> extensive
	end,
    [{group,smoke},
     {group,G}].

groups() -> 
    [{smoke,[],[basic,basic_stream]},
     {old_solaris,[],[skip_old_solaris]},
     {extensive,[],
      [api_open_close, api_listen, api_connect_init,
       api_opts, xfer_min, xfer_active, def_sndrcvinfo, implicit_inet6,
       open_multihoming_ipv4_socket,
       open_unihoming_ipv6_socket,
       open_multihoming_ipv6_socket,
       open_multihoming_ipv4_and_ipv6_socket, active_n,
       xfer_stream_min, peeloff_active_once,
       peeloff_active_true, peeloff_active_n, buffers,
       names_unihoming_ipv4, names_unihoming_ipv6,
       names_multihoming_ipv4, names_multihoming_ipv6]}].

init_per_suite(_Config) ->
    case gen_sctp:open() of
	{ok,Socket} ->
	    gen_sctp:close(Socket),
	    [];
	{error,Error}
	  when Error =:= eprotonosupport;
	       Error =:= esocktnosupport ->
	    {skip,"SCTP not supported on this machine"}
    end.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.


-define(LOGVAR(Var), begin io:format(??Var" = ~p~n", [Var]) end).

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
	 [#sctp_sndrcvinfo{stream=Stream,
			   assoc_id=SbAssocId}],
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

log_ok(X) -> log(ok(X)).

ok({ok,X}) -> X.

err([], Result) ->
    erlang:error(Result);
err([Reason|_], {error,Reason}) ->
    ok;
err([_|Reasons], Result) ->
    err(Reasons, Result).

log(X) ->
    io:format("LOG[~w]: ~p~n", [self(),X]),
    X.

flush() ->
    receive
	Msg ->
	    [Msg|flush()]
    after 17 ->
	    []
    end.

%% Test the API function open/1,2 and close/1.
api_open_close(Config) when is_list(Config) ->
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

    {ok,S5} = gen_sctp:open(P, [{ifaddr,any}]),
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

recv_event({Addr,Port,[],#sctp_assoc_change{}=AssocChange}) ->
    {Addr,Port,AssocChange};
recv_event({Addr,Port,
	    [#sctp_sndrcvinfo{assoc_id=Assoc}],
	    #sctp_assoc_change{assoc_id=Assoc}=AssocChange}) ->
    {Addr,Port,AssocChange};
recv_event({Addr,Port,[],#sctp_paddr_change{}=PaddrChange}) ->
    {Addr,Port,PaddrChange};
recv_event({Addr,Port,
	    [#sctp_sndrcvinfo{assoc_id=Assoc}],
	    #sctp_paddr_change{assoc_id=Assoc}=PaddrChange}) ->
    {Addr,Port,PaddrChange};
recv_event({Addr,Port,[],#sctp_shutdown_event{}=ShutdownEvent}) ->
    {Addr,Port,ShutdownEvent};
recv_event({Addr,Port,
	    [#sctp_sndrcvinfo{assoc_id=Assoc}],
	    #sctp_shutdown_event{assoc_id=Assoc}=ShutdownEvent}) ->
    {Addr,Port,ShutdownEvent}.

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
	{ok,[{sndbuf,SB}]} when SB >= Sndbuf -> ok
    end,
    case inet:getopts(S, [recbuf]) of
	{ok,[{recbuf,RB}]} when RB >= Recbuf -> ok
    end.

implicit_inet6(Config) when is_list(Config) ->
    Hostname = log_ok(inet:gethostname()),
    case gen_sctp:open(0, [inet6]) of
	{ok,S1} ->
	    case inet:getaddr(Hostname, inet6) of
		{ok,Host} ->
		    Loopback = {0,0,0,0,0,0,0,1},
		    io:format("~s ~p~n", ["Loopback",Loopback]),
		    implicit_inet6(S1, Loopback),
		    ok = gen_sctp:close(S1),
		    %%
		    Localhost =
			log_ok(inet:getaddr("localhost", inet6)),
		    io:format("~s ~p~n", ["localhost",Localhost]),
		    S2 =
			log_ok(gen_sctp:open(0, [{ip,Localhost}])),
		    implicit_inet6(S2, Localhost),
		    ok = gen_sctp:close(S2),
		    %%
		    io:format("~s ~p~n", [Hostname,Host]),
		    S3 =
			log_ok(gen_sctp:open(0, [{ifaddr,Host}])),
		    implicit_inet6(S3, Host),
		    ok = gen_sctp:close(S1);
		{error,eafnosupport} ->
		    ok = gen_sctp:close(S1),
		    {skip,"Can not look up IPv6 address"}
	    end;
	_ ->
	    {skip,"IPv6 not supported"}
    end.

implicit_inet6(S1, Addr) ->
    ok = gen_sctp:listen(S1, true),
    P1 = log_ok(inet:port(S1)),
    S2 = log_ok(gen_sctp:open(0, [inet6])),
    P2 = log_ok(inet:port(S2)),
    #sctp_assoc_change{state=comm_up} =
	log_ok(gen_sctp:connect(S2, Addr, P1, [])),
    case recv_event(log_ok(gen_sctp:recv(S1))) of
	{Addr,P2,#sctp_assoc_change{state=comm_up}} ->
	    ok;
	{Addr,P2,#sctp_paddr_change{state=addr_confirmed,
				    addr={Addr,P2},
				    error=0}} ->
	    {Addr,P2,#sctp_assoc_change{state=comm_up}} =
		recv_event(log_ok(gen_sctp:recv(S1)))
    end,
    case log_ok(inet:sockname(S1)) of
	{Addr,P1} -> ok;
	{{0,0,0,0,0,0,0,0},P1} -> ok
    end,
    case log_ok(inet:sockname(S2)) of
	{Addr,P2} -> ok;
	{{0,0,0,0,0,0,0,0},P2} -> ok
    end,
    ok = gen_sctp:close(S2).

%% Verify {active,N} socket management.
active_n(Config) when is_list(Config) ->
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
    [{active,N}] = ok(inet:getopts(S1, [active])),
    LoopFun = fun(Count, Count, _Fn) ->
		      receive
			  {sctp_passive,S1} ->
			      ok
		      after
			  5000 ->
			      exit({error,timeout})
		      end;
		 (I, Count, Fn) ->
		      Msg = list_to_binary("message "++integer_to_list(I)),
		      ok = gen_sctp:send(S2, Assoc, 0, Msg),
		      receive
			  {sctp,S1,_,_,{[SR],Msg}} when is_record(SR, sctp_sndrcvinfo) ->
			      Fn(I+1, Count, Fn);
			  {sctp,S1,_,_,_} ->
			      %% ignore non-data messages
			      ok = inet:setopts(S1, [{active,1}]),
			      Fn(I, Count, Fn);
			  Other ->
			      exit({unexpected, Other})
		      after
			  5000 ->
			      exit({error,timeout})
		      end
	      end,
    ok = LoopFun(1, N, LoopFun),
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
	 [#sctp_sndrcvinfo{stream=Stream,
			   assoc_id=SbAssocId}],
	 Data} -> ok;
	{Loopback,
	 Pa,[],
	 #sctp_paddr_change{addr = {Loopback,_},
			    state = addr_available,
			    error = 0,
			    assoc_id = SbAssocId}} ->
	    {Loopback,
	     Pa,
	     [#sctp_sndrcvinfo{stream=Stream,
			       assoc_id=SbAssocId}],
	     Data} = log_ok(gen_sctp:recv(Sb, infinity));
	{Loopback,
	 Pa,
	 [#sctp_sndrcvinfo{stream=Stream,
			   assoc_id=SbAssocId}],
	 #sctp_paddr_change{addr = {Loopback,_},
			    state = addr_confirmed,
			    error = 0,
			    assoc_id = SbAssocId}} ->
	    {Loopback,
	     Pa,
	     [#sctp_sndrcvinfo{stream=Stream,
			       assoc_id=SbAssocId}],
	     Data} = log_ok(gen_sctp:recv(Sb, infinity))
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
    case get_addrs_by_family(inet, 2) of
	{ok, [Addr1, Addr2]} ->
	    do_open_and_connect([Addr1, Addr2], Addr1);
	{error, Reason} ->
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
			     f("Mixing ipv4 and ipv6 addresses for multihoming "
			       " has not been verified on ~p", [Os]);
			true ->
			     f("Multihoming for ~p has not been verified on ~p",
			       [Family, Os])
		     end,
	    {error, Reason}
    end.

get_addrs_by_family_aux(Family, NumAddrs) when Family =:= inet;
					       Family =:= inet6 ->
    case inet:getaddr(localhost, Family) of
	{error,eafnosupport} ->
	    {skip, f("No support for ~p", Family)};
	{ok, _} ->
	    IfAddrs = ok(inet:getifaddrs()),
	    case filter_addrs_by_family(IfAddrs, Family) of
		Addrs when length(Addrs) >= NumAddrs ->
		    {ok, lists:sublist(Addrs, NumAddrs)};
		[] ->
		    {error, f("Need ~p ~p address(es) found none~n",
			      [NumAddrs, Family])};
		Addrs ->
		    {error,
		     f("Need ~p ~p address(es) found only ~p: ~p~n",
		       [NumAddrs, Family, length(Addrs), Addrs])}
	    end
    end;
get_addrs_by_family_aux(inet_and_inet6, NumAddrs) ->
    catch {ok, [case get_addrs_by_family_aux(Family, NumAddrs) of
		    {ok, Addrs}     -> Addrs;
		    {error, Reason} -> throw({error, Reason})
		end || Family <- [inet, inet6]]}.

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

f(F, A) ->
    lists:flatten(io_lib:format(F, A)).

do_open_and_connect(ServerAddresses, AddressToConnectTo) ->
    Fun = fun (_, _, _, _, _, _) -> ok end,
    do_open_and_connect(ServerAddresses, AddressToConnectTo, Fun).
%%
do_open_and_connect(ServerAddresses, AddressToConnectTo, Fun) ->
    ServerFamily = get_family_by_addrs(ServerAddresses),
    io:format("Serving ~p addresses: ~p~n",
	      [ServerFamily, ServerAddresses]),
    S1 = ok(gen_sctp:open(0, [{ip,Addr} || Addr <- ServerAddresses] ++
			      [ServerFamily])),
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
	[inet, inet6] -> inet6;
	[inet]        -> inet;
	[inet6]       -> inet6
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

timestamp() ->
    erlang:monotonic_time().
