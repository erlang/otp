%% 
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2011. All Rights Reserved.
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
-module(gen_sctp_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

%%-compile(export_all).

-export([all/0, suite/0,groups/0,
	 init_per_suite/1,end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).
-export(
   [basic/1,
    api_open_close/1,api_listen/1,api_connect_init/1,api_opts/1,
    xfer_min/1,xfer_active/1,def_sndrcvinfo/1,implicit_inet6/1,
    basic_stream/1, xfer_stream_min/1, peeloff/1, buffers/1,
    open_multihoming_ipv4_socket/1,
    open_unihoming_ipv6_socket/1,
    open_multihoming_ipv6_socket/1,
    open_multihoming_ipv4_and_ipv6_socket/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, api_open_close, api_listen, api_connect_init,
     api_opts, xfer_min, xfer_active, def_sndrcvinfo, implicit_inet6,
     basic_stream, xfer_stream_min, peeloff, buffers,
     open_multihoming_ipv4_socket,
     open_unihoming_ipv6_socket,
     open_multihoming_ipv6_socket,
     open_multihoming_ipv4_and_ipv6_socket].

groups() -> 
    [].

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
    Dog = test_server:timetrap(test_server:seconds(15)),
    [{watchdog, Dog}|Config].
end_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).



-define(LOGVAR(Var), begin io:format(??Var" = ~p~n", [Var]) end).



basic(doc) ->
    "Hello world";
basic(suite) ->
    [];
basic(Config) when is_list(Config) ->
    ?line {ok,S} = gen_sctp:open(),
    ?line ok = gen_sctp:close(S),
    ok.

xfer_min(doc) ->
    "Minimal data transfer";
xfer_min(suite) ->
    [];
xfer_min(Config) when is_list(Config) ->
    ?line Stream = 0,
    ?line Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    ?line Loopback = {127,0,0,1},
    ?line {ok,Sb} = gen_sctp:open([{type,seqpacket}]),
    ?line {ok,Pb} = inet:port(Sb),
    ?line ok = gen_sctp:listen(Sb, true),
    
    ?line {ok,Sa} = gen_sctp:open(),
    ?line {ok,Pa} = inet:port(Sa),
    ?line {ok,#sctp_assoc_change{state=comm_up,
				 error=0,
				 outbound_streams=SaOutboundStreams,
				 inbound_streams=SaInboundStreams,
				 assoc_id=SaAssocId}=SaAssocChange} =
	gen_sctp:connect(Sa, Loopback, Pb, []),
    ?line {SbAssocId,SaOutboundStreams,SaInboundStreams} =
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
		    ?line recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
		{AssocId,SbInboundStreams,SbOutboundStreams}
	end,

    ?line ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    ?line case log_ok(gen_sctp:recv(Sb, infinity)) of
	      {Loopback,
	       Pa,
	       [#sctp_sndrcvinfo{stream=Stream,
				 assoc_id=SbAssocId}],
	       Data} -> ok;
	      Event1 ->
		  {Loopback,Pa,
		   #sctp_paddr_change{addr = {Loopback,_},
				      state = addr_available,
				      error = 0,
				      assoc_id = SbAssocId}} =
		      recv_event(Event1),
		  {ok,{Loopback,
		       Pa,
		       [#sctp_sndrcvinfo{stream=Stream,
					 assoc_id=SbAssocId}],
		       Data}} =	gen_sctp:recv(Sb, infinity)
	  end,
    ?line ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    ?line case log_ok(gen_sctp:recv(Sa, infinity)) of
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
		      ?line recv_event(Event2),
		  ?line {Loopback,
			 Pb,
			 [#sctp_sndrcvinfo{stream=Stream,
					   assoc_id=SaAssocId}],
			 Data} =
		      log_ok(gen_sctp:recv(Sa, infinity))
	  end,
    %%
    ?line ok = gen_sctp:eof(Sa, SaAssocChange),
    ?line {Loopback,Pa,#sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ?line {Loopback,Pb,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SaAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sa, infinity))),
    ?line {Loopback,Pa,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ?line ok = gen_sctp:close(Sa),
    ?line ok = gen_sctp:close(Sb),
    
    ?line receive
	      Msg -> test_server:fail({received,Msg})
	  after 17 -> ok
	  end,
    ok.

xfer_active(doc) ->
    "Minimal data transfer in active mode";
xfer_active(suite) ->
    [];
xfer_active(Config) when is_list(Config) ->
    ?line Timeout = 2000,
    ?line Stream = 0,
    ?line Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    ?line Loopback = {127,0,0,1},
    ?line {ok,Sb} = gen_sctp:open([{active,true}]),
    ?line {ok,Pb} = inet:port(Sb),
    ?line ok = gen_sctp:listen(Sb, true),
    
    ?line {ok,Sa} = gen_sctp:open([{active,true}]),
    ?line {ok,Pa} = inet:port(Sa),
    ?line ok = gen_sctp:connect_init(Sa, Loopback, Pb, []),
    ?line #sctp_assoc_change{state=comm_up,
			     error=0,
			     outbound_streams=SaOutboundStreams,
			     inbound_streams=SaInboundStreams,
			     assoc_id=SaAssocId} = SaAssocChange =
	recv_assoc_change(Sa, Loopback, Pb, Timeout),
    ?line io:format("Sa=~p, Pa=~p, Sb=~p, Pb=~p, SaAssocId=~p, "
		    "SaOutboundStreams=~p, SaInboundStreams=~p~n",
		    [Sa,Pa,Sb,Pb,SaAssocId,
		     SaOutboundStreams,SaInboundStreams]),
    ?line #sctp_assoc_change{state=comm_up,
			     error=0,
			     outbound_streams=SbOutboundStreams,
			     inbound_streams=SbInboundStreams,
			     assoc_id=SbAssocId} =
	recv_assoc_change(Sb, Loopback, Pa, Timeout),
    ?line SbOutboundStreams = SaInboundStreams,
    ?line SbInboundStreams = SaOutboundStreams,
    ?line io:format("SbAssocId=~p~n", [SbAssocId]),

    ?line case recv_paddr_change(Sa, Loopback, Pb, 314) of
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
    ?line case recv_paddr_change(Sb, Loopback, Pa, 314) of
	      #sctp_paddr_change{state=addr_confirmed,
				 addr={Loopback,Pa},
				 error=0,
				 assoc_id=SbAssocId} -> ok;
	      #sctp_paddr_change{state=addr_available,
				 addr={Loopback,P},
				 error=0,
				 assoc_id=SbAssocId} ->
		  ?line match_unless_solaris(Pa, P);
	      timeout -> ok
	  end,
    ?line [] = flush(),

    ?line ok =
	do_from_other_process(
	  fun () -> gen_sctp:send(Sa, SaAssocId, 0, Data) end),
    ?line receive
	      {sctp,Sb,Loopback,Pa,
	       {[#sctp_sndrcvinfo{stream=Stream,
				  assoc_id=SbAssocId}],
		Data}} -> ok
	  after Timeout ->
		  ?line test_server:fail({timeout,flush()})
	  end,
    ?line ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    ?line receive
	      {sctp,Sa,Loopback,Pb,
	       {[#sctp_sndrcvinfo{stream=Stream,
				  assoc_id=SaAssocId}],
		Data}} -> ok
	  after Timeout ->
		  ?line test_server:fail({timeout,flush()})
	  end,
    %%
    ?line ok = gen_sctp:abort(Sa, SaAssocChange),
    ?line case recv_assoc_change(Sb, Loopback, Pa, Timeout) of
	      #sctp_assoc_change{state=comm_lost,
				 assoc_id=SbAssocId} -> ok;
	      timeout ->
		  ?line test_server:fail({timeout,flush()})
	  end,
    ?line ok = gen_sctp:close(Sb),
    ?line case recv_assoc_change(Sa, Loopback, Pb, Timeout) of
	      #sctp_assoc_change{state=comm_lost,
				 assoc_id=SaAssocId} -> ok;
	      timeout ->
		  ?line io:format("timeout waiting for comm_lost on Sa~n"),
		  ?line match_unless_solaris(ok, {timeout,flush()})
	  end,
    ?line receive
	      {sctp_error,Sa,enotconn} -> ok % Solaris
	  after 17 -> ok
	  end,
    ?line ok = gen_sctp:close(Sa),
    %%
    ?line receive
	      Msg -> test_server:fail({unexpected,[Msg]++flush()})
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

def_sndrcvinfo(doc) ->
    "Test that #sctp_sndrcvinfo{} parameters set on a socket "
	"are used by gen_sctp:send/4";
def_sndrcvinfo(suite) ->
    [];
def_sndrcvinfo(Config) when is_list(Config) ->
    ?line Loopback = {127,0,0,1},
    ?line Data = <<"What goes up, must come down.">>,
    %%
    ?line S1 =
	log_ok(gen_sctp:open(
	     0, [{sctp_default_send_param,#sctp_sndrcvinfo{ppid=17}}])),
    ?LOGVAR(S1),
    ?line P1 =
	log_ok(inet:port(S1)),
    ?LOGVAR(P1),
    ?line #sctp_sndrcvinfo{ppid=17, context=0, timetolive=0, assoc_id=0} =
	getopt(S1, sctp_default_send_param),
    ?line ok =
	gen_sctp:listen(S1, true),
    %%
    ?line S2 =
	log_ok(gen_sctp:open()),
    ?LOGVAR(S2),
    ?line P2 =
	log_ok(inet:port(S2)),
    ?LOGVAR(P2),
    ?line #sctp_sndrcvinfo{ppid=0, context=0, timetolive=0, assoc_id=0} =
	getopt(S2, sctp_default_send_param),
    %%
    ?line #sctp_assoc_change{
       state=comm_up,
       error=0,
       assoc_id=S2AssocId} = S2AssocChange =
	log_ok(gen_sctp:connect(S2, Loopback, P1, [])),
    ?LOGVAR(S2AssocChange),
    ?line case recv_event(log_ok(gen_sctp:recv(S1))) of
	      {Loopback,P2,
	       #sctp_assoc_change{
			  state=comm_up,
			  error=0,
			  assoc_id=S1AssocId}} ->
		  ?LOGVAR(S1AssocId);
	      {Loopback,P2,
	       #sctp_paddr_change{
			  state=addr_confirmed,
			  error=0,
			  assoc_id=S1AssocId}} ->
		  ?LOGVAR(S1AssocId),
		  {Loopback,P2,
		   #sctp_assoc_change{
			      state=comm_up,
			      error=0,
			      assoc_id=S1AssocId}} =
		      recv_event(log_ok(gen_sctp:recv(S1)))
	  end,

    ?line #sctp_sndrcvinfo{
       ppid=17, context=0, timetolive=0} = %, assoc_id=S1AssocId} =
	getopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S1AssocId}),
    ?line #sctp_sndrcvinfo{
       ppid=0, context=0, timetolive=0} = %, assoc_id=S2AssocId} =
	getopt(
	  S2, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S2AssocId}),
    %%
    ?line ok =
	gen_sctp:send(S1, S1AssocId, 1, <<"1: ",Data/binary>>),
    ?line case log_ok(gen_sctp:recv(S2)) of
	      {Loopback,P1,
	       [#sctp_sndrcvinfo{
		   stream=1, ppid=17, context=0, assoc_id=S2AssocId}],
	       <<"1: ",Data/binary>>} -> ok;
	      Event1 ->
		  ?line {Loopback,P1,
			 #sctp_paddr_change{state=addr_confirmed,
					    addr={_,P1},
					    error=0,
					    assoc_id=S2AssocId}} =
		      recv_event(Event1),
		  ?line {Loopback,P1,
			 [#sctp_sndrcvinfo{
			     stream=1, ppid=17, context=0, assoc_id=S2AssocId}],
			 <<"1: ",Data/binary>>} =
		      log_ok(gen_sctp:recv(S2))
	  end,
    %%
    ?line ok =
	setopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{ppid=18}),
    ?line ok =
	setopt(
	  S1, sctp_default_send_param,
	  #sctp_sndrcvinfo{ppid=19, assoc_id=S1AssocId}),
    ?line #sctp_sndrcvinfo{
       ppid=18, context=0, timetolive=0, assoc_id=0} =
	getopt(S1, sctp_default_send_param),
    ?line #sctp_sndrcvinfo{
       ppid=19, context=0, timetolive=0, assoc_id=S1AssocId} =
	getopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S1AssocId}),
    %%
    ?line ok =
	gen_sctp:send(S1, S1AssocId, 0, <<"2: ",Data/binary>>),
    ?line case log_ok(gen_sctp:recv(S2)) of
	      {Loopback,P1,
	       [#sctp_sndrcvinfo{
		   stream=0, ppid=19, context=0, assoc_id=S2AssocId}],
	       <<"2: ",Data/binary>>} -> ok
	  end,
    ?line ok =
	gen_sctp:send(S2, S2AssocChange, 1, <<"3: ",Data/binary>>),
    ?line case log_ok(gen_sctp:recv(S1)) of
	      {Loopback,P2,
	       [#sctp_sndrcvinfo{
		   stream=1, ppid=0, context=0, assoc_id=S1AssocId}],
	       <<"3: ",Data/binary>>} -> ok;
	      Event2 ->
		  {Loopback,P2,
		   #sctp_paddr_change{
			      addr={Loopback,_}, state=addr_available,
			      error=0, assoc_id=S1AssocId}} =
		      recv_event(Event2),
		  ?line case log_ok(gen_sctp:recv(S1)) of
			    {Loopback,P2,
			     [#sctp_sndrcvinfo{
				 stream=1, ppid=0, context=0,
				 assoc_id=S1AssocId}],
			     <<"3: ",Data/binary>>} -> ok
			end
	  end,
    ?line ok =
	do_from_other_process(
	  fun () ->
		  gen_sctp:send(
		    S2,
		    #sctp_sndrcvinfo{stream=0, ppid=20, assoc_id=S2AssocId},
		    <<"4: ",Data/binary>>)
	  end),
    ?line case log_ok(do_from_other_process(fun() -> gen_sctp:recv(S1) end)) of
	      {Loopback,P2,
	       [#sctp_sndrcvinfo{
		   stream=0, ppid=20, context=0, assoc_id=S1AssocId}],
	       <<"4: ",Data/binary>>} -> ok
	  end,
    %%
    ?line ok =
	gen_sctp:close(S1),
    ?line ok =
	gen_sctp:close(S2),
    ?line receive
	      Msg ->
		  test_server:fail({received,Msg})
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

api_open_close(doc) ->
    "Test the API function open/1,2 and close/1";
api_open_close(suite) ->
    [];
api_open_close(Config) when is_list(Config) ->
    ?line {ok,S1} = gen_sctp:open(0),
    ?line {ok,P}  = inet:port(S1),
    ?line ok      = gen_sctp:close(S1),
    
    ?line {ok,S2} = gen_sctp:open(P),
    ?line {ok,P}  = inet:port(S2),
    ?line ok      = gen_sctp:close(S2),
    
    ?line {ok,S3} = gen_sctp:open([{port,P}]),
    ?line {ok,P}  = inet:port(S3),
    ?line ok      = gen_sctp:close(S3),
    
    ?line {ok,S4} = gen_sctp:open(P, []),
    ?line {ok,P}  = inet:port(S4),
    ?line ok      = gen_sctp:close(S4),
    
    ?line {ok,S5} = gen_sctp:open(P, [{ifaddr,any}]),
    ?line {ok,P}  = inet:port(S5),
    ?line ok      = gen_sctp:close(S5),

    ?line ok      = gen_sctp:close(S5),
    
    ?line try gen_sctp:close(0)
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open({})
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(-1)
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(65536)
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(make_ref(), [])
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(0, {})
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(0, [make_ref()])
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open([{invalid_option,0}])
	  catch error:badarg -> ok
	  end,
    
    ?line try gen_sctp:open(0, [{mode,invalid_mode}])
	  catch error:badarg -> ok
	  end,
    ok.

api_listen(doc) ->
    "Test the API function listen/2";
api_listen(suite) ->
    [];
api_listen(Config) when is_list(Config) ->
    ?line Localhost = {127,0,0,1},
    
    ?line try gen_sctp:listen(0, true)
	  catch error:badarg -> ok
	  end,
    
    ?line {ok,S} = gen_sctp:open(),
    ?line {ok,Pb} = inet:port(S),
    ?line try gen_sctp:listen(S, not_allowed_for_listen)
	  catch error:badarg -> ok
	  end,
    ?line ok = gen_sctp:close(S),
    ?line {error,closed} = gen_sctp:listen(S, true),
    
    ?line {ok,Sb} = gen_sctp:open(Pb),
    ?line {ok,Sa} = gen_sctp:open(),
    ?line case gen_sctp:connect(Sa, localhost, Pb, []) of
	      {error,econnrefused} ->
		  ?line {ok,{Localhost,
			     Pb,[],
			     #sctp_assoc_change{
				  state=comm_lost}}} =
		      gen_sctp:recv(Sa, infinity);
	      {error,#sctp_assoc_change{state=cant_assoc}} ->
		  ok%;
	      %% {error,{Localhost,Pb,_,#sctp_assoc_change{state=cant_assoc}}} ->
	      %% 	  ok
	  end,
    ?line ok = gen_sctp:listen(Sb, true),
    ?line {ok,#sctp_assoc_change{state=comm_up,
				 error=0}} =
	gen_sctp:connect(Sa, localhost, Pb, []),
    ?line ok = gen_sctp:close(Sa),
    ?line ok = gen_sctp:close(Sb),
    ok.

api_connect_init(doc) ->
    "Test the API function connect_init/4";
api_connect_init(suite) ->
    [];
api_connect_init(Config) when is_list(Config) ->
    ?line Localhost = {127,0,0,1},

    ?line {ok,S} = gen_sctp:open(),
    ?line {ok,Pb} = inet:port(S),
    ?line try gen_sctp:connect_init(S, Localhost, not_allowed_for_port, [])
	  catch error:badarg -> ok
	  end,
    ?line try gen_sctp:connect_init(S, Localhost, 12345, not_allowed_for_opts)
	  catch error:badarg -> ok
	  end,
    ?line ok = gen_sctp:close(S),
    ?line {error,closed} = gen_sctp:connect_init(S, Localhost, 12345, []),

    ?line {ok,Sb} = gen_sctp:open(Pb),
    ?line {ok,Sa} = gen_sctp:open(),
    ?line case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	      {error,econnrefused} ->
		  ?line {Localhost,Pb,#sctp_assoc_change{state=comm_lost}} =
		      recv_event(log_ok(gen_sctp:recv(Sa, infinity)));
	      ok ->
		  ?line {Localhost,Pb,#sctp_assoc_change{state=cant_assoc}} =
		      recv_event(log_ok(gen_sctp:recv(Sa, infinity)))
	  end,
    ?line ok = gen_sctp:listen(Sb, true),
    ?line case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	      ok ->
		  ?line {Localhost,Pb,#sctp_assoc_change{state=comm_up}} =
		      recv_event(log_ok(gen_sctp:recv(Sa, infinity)))
	  end,
    ?line ok = gen_sctp:close(Sa),
    ?line ok = gen_sctp:close(Sb),
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

api_opts(doc) ->
    "Test socket options";
api_opts(suite) ->
    [];
api_opts(Config) when is_list(Config) ->
    ?line Sndbuf = 32768,
    ?line Recbuf = 65536,
    ?line {ok,S} = gen_sctp:open(0),
    ?line OSType = os:type(),
    ?line case {inet:setopts(S, [{linger,{true,2}}]),OSType} of
              {ok,_} ->
                  ok;
              {{error,einval},{unix,sunos}} ->
                  ok
          end,
    ?line ok = inet:setopts(S, [{sndbuf,Sndbuf}]),
    ?line ok = inet:setopts(S, [{recbuf,Recbuf}]),
    ?line case inet:getopts(S, [sndbuf]) of
	      {ok,[{sndbuf,SB}]} when SB >= Sndbuf -> ok
	  end,
    ?line case inet:getopts(S, [recbuf]) of
	      {ok,[{recbuf,RB}]} when RB >= Recbuf -> ok
	  end.

implicit_inet6(Config) when is_list(Config) ->
    ?line Hostname = log_ok(inet:gethostname()),
    ?line
	case gen_sctp:open(0, [inet6]) of
	    {ok,S1} ->
		?line
		    case inet:getaddr(Hostname, inet6) of
			{ok,Host} ->
			    ?line Loopback = {0,0,0,0,0,0,0,1},
			    ?line io:format("~s ~p~n", ["Loopback",Loopback]),
			    ?line implicit_inet6(S1, Loopback),
			    ?line ok = gen_sctp:close(S1),
			    %%
			    ?line Localhost =
				log_ok(inet:getaddr("localhost", inet6)),
			    ?line io:format("~s ~p~n", ["localhost",Localhost]),
			    ?line S2 =
				log_ok(gen_sctp:open(0, [{ip,Localhost}])),
			    ?line implicit_inet6(S2, Localhost),
			    ?line ok = gen_sctp:close(S2),
			    %%
			    ?line io:format("~s ~p~n", [Hostname,Host]),
			    ?line S3 =
				log_ok(gen_sctp:open(0, [{ifaddr,Host}])),
			    ?line implicit_inet6(S3, Host),
			    ?line ok = gen_sctp:close(S1);
			{error,eafnosupport} ->
			    ?line ok = gen_sctp:close(S1),
			    {skip,"Can not look up IPv6 address"}
		    end;
	    _ ->
		{skip,"IPv6 not supported"}
	end.

implicit_inet6(S1, Addr) ->
    ?line ok = gen_sctp:listen(S1, true),
    ?line P1 = log_ok(inet:port(S1)),
    ?line S2 = log_ok(gen_sctp:open(0, [inet6])),
    ?line P2 = log_ok(inet:port(S2)),
    ?line #sctp_assoc_change{state=comm_up} =
	log_ok(gen_sctp:connect(S2, Addr, P1, [])),
    ?line case recv_event(log_ok(gen_sctp:recv(S1))) of
	      {Addr,P2,#sctp_assoc_change{state=comm_up}} ->
		  ok;
	      {Addr,P2,#sctp_paddr_change{state=addr_confirmed,
					  addr={Addr,P2},
					  error=0}} ->
		  {Addr,P2,#sctp_assoc_change{state=comm_up}} =
		      recv_event(log_ok(gen_sctp:recv(S1)))
	  end,
    ?line case log_ok(inet:sockname(S1)) of
	      {Addr,P1} -> ok;
	      {{0,0,0,0,0,0,0,0},P1} -> ok
	  end,
    ?line case log_ok(inet:sockname(S2)) of
	      {Addr,P2} -> ok;
	      {{0,0,0,0,0,0,0,0},P2} -> ok
	  end,
    ?line ok = gen_sctp:close(S2).

basic_stream(doc) ->
    "Hello world stream socket";
basic_stream(suite) ->
    [];
basic_stream(Config) when is_list(Config) ->
    ?line {ok,S} = gen_sctp:open([{type,stream}]),
    ?line ok = gen_sctp:listen(S, true),
    ?line ok =
	do_from_other_process(
	  fun () -> gen_sctp:listen(S, 10) end),
    ?line ok = gen_sctp:close(S),
    ok.

xfer_stream_min(doc) ->
    "Minimal data transfer";
xfer_stream_min(suite) ->
    [];
xfer_stream_min(Config) when is_list(Config) ->
    ?line Stream = 0,
    ?line Data = <<"The quick brown fox jumps over a lazy dog 0123456789">>,
    ?line Loopback = {127,0,0,1},
    ?line {ok,Sb} = gen_sctp:open([{type,seqpacket}]),
    ?line ?LOGVAR(Sb),
    ?line {ok,Pb} = inet:port(Sb),
    ?line ?LOGVAR(Pb),
    ?line ok = gen_sctp:listen(Sb, true),

    ?line {ok,Sa} = gen_sctp:open([{type,stream}]),
    ?line ?LOGVAR(Sa),
    ?line {ok,Pa} = inet:port(Sa),
    ?line ?LOGVAR(Pa),
    ?line #sctp_assoc_change{state=comm_up,
			     error=0,
			     outbound_streams=SaOutboundStreams,
			     inbound_streams=SaInboundStreams,
			     assoc_id=SaAssocId_X} =
	log_ok(gen_sctp:connect(Sa, Loopback, Pb, [])),
    ?line ?LOGVAR(SaAssocId_X),
    ?line [{_,#sctp_paddrinfo{assoc_id=SaAssocId,state=active}}] =
	log_ok(inet:getopts(Sa, [{sctp_get_peer_addr_info,
				  #sctp_paddrinfo{address={Loopback,Pb}}}])),
    ?line ?LOGVAR(SaAssocId),
    ?line match_unless_solaris(SaAssocId_X, SaAssocId),

    ?line {SbOutboundStreams,SbInboundStreams,SbAssocId} =
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
		 ?line #sctp_assoc_change{state=comm_up,
					  error=0,
					  outbound_streams=OS,
					  inbound_streams=IS,
					  assoc_id=AI}} =
		    recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
		{OS,IS,AI}
	end,
    ?line ?LOGVAR(SbAssocId),
    ?line SaOutboundStreams = SbInboundStreams,
    ?line ?LOGVAR(SaOutboundStreams),
    ?line SbOutboundStreams = SaInboundStreams,
    ?line ?LOGVAR(SbOutboundStreams),
    ?line ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    ?line case gen_sctp:recv(Sb, infinity) of
	      {ok,{Loopback,
		   Pa,
		   [#sctp_sndrcvinfo{stream=Stream,
				     assoc_id=SbAssocId}],
		   Data}} -> ok;
	      {ok,{Loopback,
		   Pa,[],
		   #sctp_paddr_change{addr = {Loopback,_},
				      state = addr_available,
				      error = 0,
				      assoc_id = SbAssocId}}} ->
		  {ok,{Loopback,
		       Pa,
		       [#sctp_sndrcvinfo{stream=Stream,
					 assoc_id=SbAssocId}],
		       Data}} =	gen_sctp:recv(Sb, infinity)
	  end,
    ?line ok =
	do_from_other_process(
	  fun () -> gen_sctp:send(Sb, SbAssocId, 0, Data) end),
    ?line case log_ok(gen_sctp:recv(Sa, infinity)) of
	      {Loopback,Pb,
	       [#sctp_sndrcvinfo{stream=Stream,
				 assoc_id=SaAssocId}],
	       Data} -> ok;
	      Event1 ->
		  ?line {Loopback,Pb,
			 #sctp_paddr_change{state=addr_confirmed,
					    addr={_,Pb},
					    error=0,
					    assoc_id=SaAssocId}} =
		      recv_event(Event1),
		  ?line {Loopback,Pb,
			 [#sctp_sndrcvinfo{stream=Stream,
					   assoc_id=SaAssocId}],
			 Data} =
		      log_ok(gen_sctp:recv(Sa, infinity))
	  end,
    ?line ok = gen_sctp:close(Sa),
    ?line {Loopback,Pa,
	   #sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ?line {Loopback,Pa,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SbAssocId}} =
	recv_event(log_ok(gen_sctp:recv(Sb, infinity))),
    ?line ok = gen_sctp:close(Sb),

    ?line receive
	      Msg -> test_server:fail({received,Msg})
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
			  Class:Reason ->
			      Stacktrace = erlang:get_stacktrace(),
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



peeloff(doc) ->
    "Peel off an SCTP stream socket";
peeloff(suite) ->
    [];
peeloff(Config) when is_list(Config) ->
    ?line Addr = {127,0,0,1},
    ?line Stream = 0,
    ?line Timeout = 333,
    ?line S1 = socket_open([{ifaddr,Addr}], Timeout),
    ?line ?LOGVAR(S1),
    ?line P1 = socket_call(S1, get_port),
    ?line ?LOGVAR(P1),
    ?line Socket1 = socket_call(S1, get_socket),
    ?line ?LOGVAR(Socket1),
    ?line socket_call(S1, {listen,true}),
    ?line S2 = socket_open([{ifaddr,Addr}], Timeout),
    ?line ?LOGVAR(S2),
    ?line P2 = socket_call(S2, get_port),
    ?line ?LOGVAR(P2),
    ?line Socket2 = socket_call(S2, get_socket),
    ?line ?LOGVAR(Socket2),
    %%
    ?line socket_call(S2, {connect_init,Addr,P1,[]}),
    ?line S2Ai =
	receive
	    {S2,{Addr,P1,
		 #sctp_assoc_change{
			state=comm_up,
			assoc_id=AssocId2}}} -> AssocId2
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    ?line ?LOGVAR(S2Ai),
    ?line S1Ai =
	receive
	      {S1,{Addr,P2,
		   #sctp_assoc_change{
			  state=comm_up,
			  assoc_id=AssocId1}}} -> AssocId1
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    ?line ?LOGVAR(S1Ai),
    %%
    ?line socket_call(S2, {send,S2Ai,Stream,<<"Number one">>}),
    ?line
	receive
	    {S1,{Addr,P2,S1Ai,Stream,<<"Number one">>}} -> ok
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    ?line socket_call(S2, {send,Socket1,S1Ai,Stream,<<"Number two">>}),
    ?line
	receive
	    {S2,{Addr,P1,S2Ai,Stream,<<"Number two">>}} -> ok
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    %%
    ?line S3 = socket_peeloff(Socket1, S1Ai, Timeout),
    ?line ?LOGVAR(S3),
    ?line P3_X = socket_call(S3, get_port),
    ?line ?LOGVAR(P3_X),
    ?line P3 = case P3_X of 0 -> P1; _ -> P3_X end,
    ?line [{_,#sctp_paddrinfo{assoc_id=S3Ai,state=active}}] =
	socket_call(S3,
	    {getopts,[{sctp_get_peer_addr_info,
		       #sctp_paddrinfo{address={Addr,P2}}}]}),
    %%?line S3Ai = S1Ai,
    ?line ?LOGVAR(S3Ai),
    %%
    ?line socket_call(S3, {send,S3Ai,Stream,<<"Number three">>}),
    ?line
	receive
	    {S2,{Addr,P3,S2Ai,Stream,<<"Number three">>}} -> ok
	after Timeout ->
		socket_bailout([S1,S2,S3])
	end,
    ?line socket_call(S3, {send,Socket2,S2Ai,Stream,<<"Number four">>}),
    ?line
	receive
	    {S3,{Addr,P2,S3Ai,Stream,<<"Number four">>}} -> ok
	after Timeout ->
		socket_bailout([S1,S2,S3])
	end,
    %%
    ?line inet:i(sctp),
    ?line socket_close_verbose(S1),
    ?line socket_close_verbose(S2),
    ?line
	receive
	    {S3,{Addr,P2,#sctp_shutdown_event{assoc_id=S3Ai_X}}} ->
		?line match_unless_solaris(S3Ai, S3Ai_X)
	after Timeout ->
		socket_bailout([S3])
	end,
    ?line
	receive
	    {S3,{Addr,P2,#sctp_assoc_change{state=shutdown_comp,
					    assoc_id=S3Ai}}} -> ok
	after Timeout ->
		socket_bailout([S3])
	end,
    ?line socket_close_verbose(S3),
    ?line [] = flush(),
    ok.



buffers(doc) ->
    ["Check sndbuf and recbuf behaviour"];
buffers(suite) ->
    [];
buffers(Config) when is_list(Config) ->
    ?line Limit = 4096,
    ?line Addr = {127,0,0,1},
    ?line Stream = 1,
    ?line Timeout = 3333,
    ?line S1 = socket_open([{ip,Addr}], Timeout),
    ?line ?LOGVAR(S1),
    ?line P1 = socket_call(S1, get_port),
    ?line ?LOGVAR(P1),
    ?line ok = socket_call(S1, {listen,true}),
    ?line S2 = socket_open([{ip,Addr}], Timeout),
    ?line ?LOGVAR(S2),
    ?line P2 = socket_call(S2, get_port),
    ?line ?LOGVAR(P2),
    %%
    ?line socket_call(S2, {connect_init,Addr,P1,[]}),
    ?line S2Ai =
	receive
	    {S2,{Addr,P1,
		 #sctp_assoc_change{
			state=comm_up,
			assoc_id=AssocId2}}} -> AssocId2
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    ?line S1Ai =
	receive
	    {S1,{Addr,P2,
		 #sctp_assoc_change{
			state=comm_up,
			assoc_id=AssocId1}}} -> AssocId1
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    %%
    ?line socket_call(S1, {setopts,[{recbuf,Limit}]}),
    ?line Recbuf =
	    case socket_call(S1, {getopts,[recbuf]}) of
		[{recbuf,RB1}] when RB1 >= Limit -> RB1
	    end,
    ?line Data = mk_data(Recbuf+Limit),
    ?line socket_call(S2, {setopts,[{sndbuf,Recbuf+Limit}]}),
    ?line socket_call(S2, {send,S2Ai,Stream,Data}),
    ?line
	receive
	    {S1,{Addr,P2,S1Ai,Stream,Data}} -> ok
	after Timeout ->
		socket_bailout([S1,S2])
	end,
    %%
    ?line socket_close_verbose(S1),
    ?line
	receive
	    {S2,{Addr,P1,#sctp_shutdown_event{assoc_id=S2Ai}}} -> ok
	after Timeout ->
		socket_bailout([S2])
	end,
    ?line
	receive
	    {S2,{Addr,P1,#sctp_assoc_change{state=shutdown_comp,
					    assoc_id=S2Ai}}} -> ok
	after Timeout ->
		socket_bailout([S2])
	end,
    ?line socket_close_verbose(S2),
    ?line [] = flush(),
    ok.

mk_data(Bytes) ->
    mk_data(0, Bytes, <<>>).
%%
mk_data(N, Bytes, Bin) when N < Bytes ->
    mk_data(N+4, Bytes, <<Bin/binary,N:32>>);
mk_data(_, _, Bin) ->
    Bin.



open_multihoming_ipv4_socket(doc) ->
    "Test opening a multihoming ipv4 socket";
open_multihoming_ipv4_socket(suite) ->
    [];
open_multihoming_ipv4_socket(Config) when is_list(Config) ->
    ?line case get_addrs_by_family(inet, 2) of
	      {ok, [Addr1, Addr2]} ->
		  ?line do_open_and_connect([Addr1, Addr2], Addr1);
	      {error, Reason} ->
		  {skip, Reason}
	  end.

open_unihoming_ipv6_socket(doc) ->
    %% This test is mostly aimed to indicate
    %% whether host has a non-working ipv6 setup
    "Test opening a unihoming (non-multihoming) ipv6 socket";
open_unihoming_ipv6_socket(suite) ->
    [];
open_unihoming_ipv6_socket(Config) when is_list(Config) ->
    ?line case get_addrs_by_family(inet6, 1) of
	      {ok, [Addr]} ->
		  ?line do_open_and_connect([Addr], Addr);
	      {error, Reason} ->
		  {skip, Reason}
	  end.


open_multihoming_ipv6_socket(doc) ->
    "Test opening a multihoming ipv6 socket";
open_multihoming_ipv6_socket(suite) ->
    [];
open_multihoming_ipv6_socket(Config) when is_list(Config) ->
    ?line case get_addrs_by_family(inet6, 2) of
	      {ok, [Addr1, Addr2]} ->
		  ?line do_open_and_connect([Addr1, Addr2], Addr1);
	      {error, Reason} ->
		  {skip, Reason}
	  end.

open_multihoming_ipv4_and_ipv6_socket(doc) ->
    "Test opening a multihoming ipv6 socket with ipv4 and ipv6 addresses";
open_multihoming_ipv4_and_ipv6_socket(suite) ->
    [];
open_multihoming_ipv4_and_ipv6_socket(Config) when is_list(Config) ->
    ?line case get_addrs_by_family(inet_and_inet6, 2) of
	      {ok, [[InetAddr1, InetAddr2], [Inet6Addr1, Inet6Addr2]]} ->
		  %% Connect to the first address to test bind
		  ?line do_open_and_connect([InetAddr1, Inet6Addr1, InetAddr2],
					    InetAddr1),
		  ?line do_open_and_connect([Inet6Addr1, InetAddr1],
					    Inet6Addr1),

		  %% Connect an address, not the first,
		  %% to test sctp_bindx
		  ?line do_open_and_connect([Inet6Addr1, Inet6Addr2, InetAddr1],
					    Inet6Addr2),
		  ?line do_open_and_connect([Inet6Addr1, Inet6Addr2, InetAddr1],
					    InetAddr1);
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
    ?line
	case inet:getaddr(localhost, Family) of
	    {error,eafnosupport} ->
		{skip, f("No support for ~p", Family)};
	    {ok, _} ->
		?line IfAddrs = ok(inet:getifaddrs()),
		?line case filter_addrs_by_family(IfAddrs, Family) of
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
    ?line catch {ok, [case get_addrs_by_family_aux(Family, NumAddrs) of
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
    ?line ServerFamily = get_family_by_addrs(ServerAddresses),
    ?line io:format("Serving ~p addresses: ~p~n",
		    [ServerFamily, ServerAddresses]),
    ?line S1 = ok(gen_sctp:open(0, [{ip,Addr} || Addr <- ServerAddresses] ++
				    [ServerFamily])),
    ?line ok = gen_sctp:listen(S1, true),
    ?line P1 = ok(inet:port(S1)),
    ?line ClientFamily = get_family_by_addr(AddressToConnectTo),
    ?line io:format("Connecting to ~p ~p~n",
		    [ClientFamily, AddressToConnectTo]),
    ?line S2 = ok(gen_sctp:open(0, [ClientFamily])),
    %% Verify client can connect
    ?line #sctp_assoc_change{state=comm_up} =
	ok(gen_sctp:connect(S2, AddressToConnectTo, P1, [])),
    %% verify server side also receives comm_up from client
    ?line recv_comm_up_eventually(S1),
    ?line ok = gen_sctp:close(S2),
    ?line ok = gen_sctp:close(S1).

%% If at least one of the addresses is an ipv6 address, return inet6, else inet.
get_family_by_addrs(Addresses) ->
    ?line case lists:usort([get_family_by_addr(Addr) || Addr <- Addresses]) of
	      [inet, inet6] -> inet6;
	      [inet]        -> inet;
	      [inet6]       -> inet6
	  end.

get_family_by_addr(Addr) when tuple_size(Addr) =:= 4 -> inet;
get_family_by_addr(Addr) when tuple_size(Addr) =:= 8 -> inet6.

recv_comm_up_eventually(S) ->
    ?line case ok(gen_sctp:recv(S)) of
	      {_Addr, _Port, _Info, #sctp_assoc_change{state=comm_up}} ->
		  ok;
	      {_Addr, _Port, _Info, _OtherSctpMsg} ->
		  ?line recv_comm_up_eventually(S)
	  end.

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% socket gen_server ultra light

socket_open(SocketOpts, Timeout) ->
    Opts = [{type,seqpacket},{active,once},binary|SocketOpts],
    Starter =
	fun () ->
		{ok,Socket} =
		    gen_sctp:open(Opts),
		Socket
	end,
    s_start(Starter, Timeout).

socket_peeloff(Socket, AssocId, Timeout) ->
    Opts = [{active,once},binary],
    Starter =
	fun () ->
		{ok,NewSocket} =
		    gen_sctp:peeloff(Socket, AssocId),
		ok = inet:setopts(NewSocket, Opts),
		NewSocket
	end,
    s_start(Starter, Timeout).

socket_close_verbose(S) ->
    History = socket_history(socket_close(S)),
    io:format("socket_close ~p:~n    ~p.~n", [S,History]),
    History.

socket_close(S) ->
    s_req(S, close).

socket_call(S, Request) ->
    s_req(S, {Request}).

%% socket_get(S, Key) ->
%%     s_req(S, {get,Key}).

socket_bailout([S|Ss]) ->
    History = socket_history(socket_close(S)),
    io:format("bailout ~p:~n    ~p.~n", [S,History]),
    socket_bailout(Ss);
socket_bailout([]) ->
    io:format("flush: ~p.~n", [flush()]),
    test_server:fail(socket_bailout).

socket_history({State,Flush}) ->
    {lists:keysort(
      2,
      lists:flatten(
	[[{Key,Val} || Val <- Vals]
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
	    erlang:demonitor(Mref),
	    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end,
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
	Class:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
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
	    Val = {now(),Socket},
	    NewState = gb_push(Key, Val, State),
	    Parent ! {self(),Ref,{NewState,flush()}};
	{Parent,Ref,{Msg}} ->
	    Result = Handler(Msg),
	    Key = req,
	    Val = {now(),{Msg,Result}},
	    NewState = gb_push(Key, Val, State),
	    Parent ! {self(),Ref,Result},
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	%% {Parent,Ref,{get,Key}} ->
	%%     Parent ! {self(),Ref,gb_get(Key, State)},
	%%     s_loop(Socket, Timeout, Parent, Handler, State);
	{sctp,Socket,Addr,Port,
	 {[#sctp_sndrcvinfo{stream=Stream,assoc_id=AssocId}=SRI],Data}}
	  when not is_tuple(Data) ->
	    case gb_get({assoc_change,AssocId}, State) of
		[{_,{Addr,Port,
		     #sctp_assoc_change{
			    state=comm_up,
			    inbound_streams=Is}}}|_]
		 when 0 =< Stream, Stream < Is-> ok;
		[] -> ok
	    end,
	    Key = {msg,AssocId,Stream},
	    Val = {now(),{Addr,Port,SRI,Data}},
	    NewState = gb_push(Key, Val, State),
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
	    Val = {now(),{Addr,Port,SAC}},
	    case {gb_get(Key, State),St} of
		{[],_} -> ok;
		{[{_,{Addr,Port,#sctp_assoc_change{state=comm_up}}}|_],_}
		when St =:= comm_lost; St =:= shutdown_comp -> ok
	    end,
	    NewState = gb_push(Key, Val, State),
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
		{[{_,{Addr,Port,#sctp_assoc_change{state=comm_up}}}|_],
		 addr_available} -> ok;
		{[],addr_confirmed} -> ok
	    end,
	    Key = {paddr_change,AssocId},
	    Val = {now(),{Addr,Port,SPC}},
	    NewState = gb_push(Key, Val, State),
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	{sctp,Socket,Addr,Port,
	 {SRI,#sctp_shutdown_event{assoc_id=AssocId}=SSE}} ->
	    case SRI of
		[#sctp_sndrcvinfo{assoc_id=AssocId,stream=0}] -> ok;
		[] -> ok
	    end,
	    case gb_get({assoc_change,AssocId}, State) of
		[{_,{Addr,Port,#sctp_assoc_change{state=comm_up}}}|_] -> ok;
		[] -> ok
	    end,
	    Key = {shutdown_event,AssocId},
	    Val = {now(),{Addr,Port}},
	    NewState = gb_push(Key, Val, State),
	    Parent ! {self(), {Addr,Port,SSE}},
	    again(Socket),
	    s_loop(Socket, Timeout, Parent, Handler, NewState);
	Unexpected ->
	    erlang:error({unexpected,Unexpected})
    end.

again(Socket) ->
    inet:setopts(Socket, [{active,once}]).

gb_push(Key, Val, GBT) ->
    case gb_trees:lookup(Key, GBT) of
	none ->
	    gb_trees:insert(Key, [Val], GBT);
	{value,V} ->
	    gb_trees:update(Key, [Val|V], GBT)
    end.

gb_get(Key, GBT) ->
    case gb_trees:lookup(Key, GBT) of
	none ->
	    [];
	{value,V} ->
	    V
    end.

match_unless_solaris(A, B) ->
    case os:type() of
	{unix,sunos} -> B;
	_ -> A = B
    end.
