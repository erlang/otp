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
    basic_stream/1, xfer_stream_min/1, peeloff/1, buffers/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, api_open_close, api_listen, api_connect_init,
     api_opts, xfer_min, xfer_active, def_sndrcvinfo, implicit_inet6,
     basic_stream, xfer_stream_min, peeloff, buffers].

groups() -> 
    [].

init_per_suite(Config) ->
    try gen_sctp:open() of
	{ok,Socket} ->
	    gen_sctp:close(Socket),
	    [];
	_ ->
	    []
    catch
	error:badarg ->
	    {skip,"SCTP not supported on this machine"};
	_:_ ->
	    Config
    end.

end_per_suite(_Conifig) ->
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
	case recv_event(ok(gen_sctp:recv(Sb, infinity))) of
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
		    ?line recv_event(ok(gen_sctp:recv(Sb, infinity))),
		{AssocId,SbInboundStreams,SbOutboundStreams}
	end,

    ?line ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    ?line case ok(gen_sctp:recv(Sb, infinity)) of
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
    ?line case ok(gen_sctp:recv(Sa, infinity)) of
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
		      ok(gen_sctp:recv(Sa, infinity))
	  end,
    %%
    ?line ok = gen_sctp:eof(Sa, SaAssocChange),
    ?line {Loopback,Pa,#sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(ok(gen_sctp:recv(Sb, infinity))),
    ?line {Loopback,Pb,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SaAssocId}} =
 	recv_event(ok(gen_sctp:recv(Sa, infinity))),
    ?line {Loopback,Pa,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SbAssocId}} =
 	recv_event(ok(gen_sctp:recv(Sb, infinity))),
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
				 addr={Loopback,Pa},
				 error=0,
				 assoc_id=SbAssocId} -> ok;
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
		  ?line test_server:fail({unexpected,flush()})
	  end,
    ?line ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    ?line receive
	      {sctp,Sa,Loopback,Pb,
	       {[#sctp_sndrcvinfo{stream=Stream,
				  assoc_id=SaAssocId}],
		Data}} -> ok
	  after Timeout ->
		  ?line test_server:fail({unexpected,flush()})
	  end,
    %%
    ?line ok = gen_sctp:abort(Sa, SaAssocChange),
    ?line case recv_assoc_change(Sb, Loopback, Pa, Timeout) of
	      #sctp_assoc_change{state=comm_lost,
				 assoc_id=SbAssocId} -> ok;
	      timeout ->
		  ?line test_server:fail({unexpected,flush()})
	  end,
    ?line ok = gen_sctp:close(Sb),
    ?line case recv_assoc_change(Sa, Loopback, Pb, Timeout) of
	      #sctp_assoc_change{state=comm_lost,
				 assoc_id=SaAssocId} -> ok;
	      timeout ->
		  ?line test_server:fail({unexpected,flush()})
	  end,
    ?line receive
              {sctp_error,Sa,enotconn} -> ok % Solaris
          after 17 -> ok %% Only happens on Solaris
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
	ok(gen_sctp:open(
	     0, [{sctp_default_send_param,#sctp_sndrcvinfo{ppid=17}}])),
    ?LOGVAR(S1),
    ?line P1 =
	ok(inet:port(S1)),
    ?LOGVAR(P1),
    ?line #sctp_sndrcvinfo{ppid=17, context=0, timetolive=0, assoc_id=0} =
	getopt(S1, sctp_default_send_param),
    ?line ok =
	gen_sctp:listen(S1, true),
    %%
    ?line S2 =
	ok(gen_sctp:open()),
    ?LOGVAR(S2),
    ?line P2 =
	ok(inet:port(S2)),
    ?LOGVAR(P2),
    ?line #sctp_sndrcvinfo{ppid=0, context=0, timetolive=0, assoc_id=0} =
	getopt(S2, sctp_default_send_param),
    %%
    ?line #sctp_assoc_change{
       state=comm_up,
       error=0,
       assoc_id=S2AssocId} = S2AssocChange =
	ok(gen_sctp:connect(S2, Loopback, P1, [])),
    ?LOGVAR(S2AssocChange),
    ?line case recv_event(ok(gen_sctp:recv(S1))) of
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
		      recv_event(ok(gen_sctp:recv(S1)))
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
    ?line case ok(gen_sctp:recv(S2)) of
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
		      ok(gen_sctp:recv(S2))
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
    ?line case ok(gen_sctp:recv(S2)) of
	      {Loopback,P1,
	       [#sctp_sndrcvinfo{
		   stream=0, ppid=19, context=0, assoc_id=S2AssocId}],
	       <<"2: ",Data/binary>>} -> ok
	  end,
    ?line ok =
	gen_sctp:send(S2, S2AssocChange, 1, <<"3: ",Data/binary>>),
    ?line case ok(gen_sctp:recv(S1)) of
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
		  ?line case ok(gen_sctp:recv(S1)) of
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
    ?line case ok(do_from_other_process(fun() -> gen_sctp:recv(S1) end)) of
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

ok({ok,X}) ->
    io:format("OK[~w]: ~p~n", [self(),X]),
    X.

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
		      recv_event(ok(gen_sctp:recv(Sa, infinity)));
	      ok ->
		  ?line {Localhost,Pb,#sctp_assoc_change{state=cant_assoc}} =
		      recv_event(ok(gen_sctp:recv(Sa, infinity)))
	  end,
    ?line ok = gen_sctp:listen(Sb, true),
    ?line case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	      ok ->
		  ?line {Localhost,Pb,#sctp_assoc_change{state=comm_up}} =
		      recv_event(ok(gen_sctp:recv(Sa, infinity)))
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
    ?line Hostname = ok(inet:gethostname()),
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
				ok(inet:getaddr("localhost", inet6)),
			    ?line io:format("~s ~p~n", ["localhost",Localhost]),
			    ?line S2 =
				ok(gen_sctp:open(0, [{ip,Localhost}])),
			    ?line implicit_inet6(S2, Localhost),
			    ?line ok = gen_sctp:close(S2),
			    %%
			    ?line io:format("~s ~p~n", [Hostname,Host]),
			    ?line S3 =
				ok(gen_sctp:open(0, [{ifaddr,Host}])),
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
    ?line P1 = ok(inet:port(S1)),
    ?line S2 = ok(gen_sctp:open(0, [inet6])),
    ?line P2 = ok(inet:port(S2)),
    ?line #sctp_assoc_change{state=comm_up} =
	ok(gen_sctp:connect(S2, Addr, P1, [])),
    ?line case recv_event(ok(gen_sctp:recv(S1))) of
	      {Addr,P2,#sctp_assoc_change{state=comm_up}} ->
		  ok;
	      {Addr,P2,#sctp_paddr_change{state=addr_confirmed,
					  addr={Addr,P2},
					  error=0}} ->
		  {Addr,P2,#sctp_assoc_change{state=comm_up}} =
		      recv_event(ok(gen_sctp:recv(S1)))
	  end,
    ?line case ok(inet:sockname(S1)) of
	      {Addr,P1} -> ok;
	      {{0,0,0,0,0,0,0,0},P1} -> ok
	  end,
    ?line case ok(inet:sockname(S2)) of
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
    ?line {ok,Pb} = inet:port(Sb),
    ?line ok = gen_sctp:listen(Sb, true),

    ?line {ok,Sa} = gen_sctp:open([{type,stream}]),
    ?line {ok,Pa} = inet:port(Sa),
    ?line {ok,#sctp_assoc_change{state=comm_up,
				 error=0,
				 outbound_streams=SaOutboundStreams,
				 inbound_streams=SaInboundStreams,
				 assoc_id=SaAssocId}} =
	gen_sctp:connect(Sa, Loopback, Pb, []),
    ?line {SbOutboundStreams,SbInboundStreams,SbAssocId} =
	case recv_event(ok(gen_sctp:recv(Sb, infinity))) of
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
		    recv_event(ok(gen_sctp:recv(Sb, infinity))),
		{OS,IS,AI}
	end,
    ?line SaOutboundStreams = SbInboundStreams,
    ?line SbOutboundStreams = SaInboundStreams,
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
    ?line case ok(gen_sctp:recv(Sa, infinity)) of
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
		      ok(gen_sctp:recv(Sa, infinity))
	  end,
    ?line ok = gen_sctp:close(Sa),
    ?line {Loopback,Pa,
	   #sctp_shutdown_event{assoc_id=SbAssocId}} =
	recv_event(ok(gen_sctp:recv(Sb, infinity))),
    ?line {Loopback,Pa,
	   #sctp_assoc_change{state=shutdown_comp,
			      error=0,
			      assoc_id=SbAssocId}} =
	recv_event(ok(gen_sctp:recv(Sb, infinity))),
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
    ?line S1 = socket_start(Addr, Timeout),
    ?line P1 = socket_call(S1, port),
    ?line Socket1 = socket_call(S1, socket),
    ?line ok = socket_call(S1, {listen,true}),
    ?line S2 = socket_start(Addr, Timeout),
    ?line P2 = socket_call(S2, port),
    ?line Socket2 = socket_call(S2, socket),
    %%
    ?line H_a = socket_req(S1, recv_assoc),
    ?line {S2Ai,Sa,Sb} = socket_call(S2, {connect,Addr,P1,[]}),
    ?line {S1Ai,Sb,Sa,Addr,P2} = socket_resp(H_a),
    %%
    ?line H_b = socket_req(S1, recv),
    ?line ok = socket_call(S2, {send,S2Ai,Stream,<<"Data H_b">>}),
    ?line {Addr,P2,S1Ai,Stream,<<"Data H_b">>} = socket_resp(H_b),
    ?line H_c = socket_req(S1, {recv,Socket2}),
    ?line ok =
	socket_call(S2, {send,Socket1,S1Ai,Stream,<<"Data H_c">>}),
    ?line {Addr,P1,S2Ai,Stream,<<"Data H_c">>} = socket_resp(H_c),
    %%
    ?line S3 = socket_peeloff(Socket1, S1Ai, Timeout),
    ?line P3 = socket_call(S3, port),
    ?line Socket3 = socket_call(S3, socket),
    ?line S3Ai = S1Ai,
    %%
    ?line H_d = socket_req(S2, recv),
    ?line ok = socket_call(S3, {send,S3Ai,Stream,<<"Data H_d">>}),
    ?line {Addr,P3,S2Ai,Stream,<<"Data H_d">>} = socket_resp(H_d),
    ?line ok = socket_call(S3, {send,Socket2,S2Ai,Stream,<<"Data S2">>}),
    ?line {Addr,P2,S3Ai,Stream,<<"Data S2">>} = socket_call(S2, {recv,Socket3}),
    %%
    ?line inet:i(sctp),
    ?line ok = socket_stop(S1),
    ?line ok = socket_stop(S2),
    ?line {Addr,P2,#sctp_shutdown_event{assoc_id=S1Ai}} =
	recv_event(ok(socket_stop(S3))),
    ok.



buffers(doc) ->
    ["Check sndbuf and recbuf behaviour"];
buffers(suite) ->
    [];
buffers(Config) when is_list(Config) ->
    ?line Limit = 8192,
    ?line Data = mk_data(Limit),
    ?line Addr = {127,0,0,1},
    ?line Stream = 1,
    ?line Timeout = 333,
    ?line S1 = socket_start(Addr, Timeout),
    ?line P1 = socket_call(S1, port),
    ?line ok = socket_call(S1, {listen,true}),
    ?line S2 = socket_start(Addr, Timeout),
    ?line P2 = socket_call(S2, port),
    %%
    ?line H_a = socket_req(S1, recv_assoc),
    ?line {S2Ai,Sa,Sb} = socket_call(S2, {connect,Addr,P1,[]}),
    ?line {S1Ai,Sb,Sa,Addr,P2} = socket_resp(H_a),
    %%
    ?line ok = socket_call(S1, {setopts,[{recbuf,Limit}]}),
    ?line case socket_call(S1, {getopts,[recbuf]}) of
	      {ok,[{recbuf,RB1}]} when RB1 >= Limit -> ok
	  end,
    ?line H_b = socket_req(S1, recv),
    ?line ok = socket_call(S2, {send,S2Ai,Stream,Data}),
    ?line {Addr,P2,S1Ai,Stream,Data} = socket_resp(H_b),
    %%
    ?line ok = socket_stop(S1),
    ?line {Addr,P1,#sctp_shutdown_event{assoc_id=S2Ai}} =
	recv_event(ok(socket_stop(S2))),
    ok.

mk_data(Words) ->
    mk_data(0, Words, <<>>).
%%
mk_data(Words, Words, Bin) ->
    Bin;
mk_data(N, Words, Bin) ->
    mk_data(N+1, Words, <<Bin/binary,N:32>>).

%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% socket gen_server ultra light

socket_peeloff(Socket, AssocId, Timeout) ->
    Starter =
	fun () ->
		{ok,NewSocket} =
		    gen_sctp:peeloff(Socket, AssocId),
		NewSocket
	end,
    socket_starter(Starter, Timeout).

socket_start(Addr, Timeout) ->
    Starter =
	fun () ->
		{ok,Socket} =
		    gen_sctp:open([{type,seqpacket},{ifaddr,Addr}]),
		Socket
	end,
    socket_starter(Starter, Timeout).

socket_starter(Starter, Timeout) ->
    Parent = self(),
    Owner =
	spawn_link(
	  fun () ->
		  socket_starter(Starter(), Timeout, Parent)
	  end),
    io:format("Started socket ~w.~n", [Owner]),
    Owner.

socket_starter(Socket, Timeout, Parent) ->
    try
	Handler = socket_handler(Socket, Timeout),
	socket_loop(Socket, Timeout, Parent, Handler)
    catch
	Class:Reason ->
	    Stacktrace = erlang:get_stacktrace(),
	    io:format(?MODULE_STRING":socket exception ~w:~w at~n"
		      "~p.~n", [Class,Reason,Stacktrace]),
	    erlang:raise(Class, Reason, Stacktrace)
    end.

socket_loop(Socket, Timeout, Parent, Handler) ->
    receive
	{Parent,Ref} -> % socket_stop()
	    Result =
		case log(gen_sctp:recv(Socket, Timeout)) of
		    {error,timeout} -> ok;
		    R -> R
		end,
	    ok = gen_sctp:close(Socket),
	    Parent ! {self(),Ref, Result};
	{Parent,Ref,Msg} ->
	    Parent ! {self(),Ref,Handler(Msg)},
	    socket_loop(Socket, Timeout, Parent, Handler)
    end.

socket_handler(Socket, Timeout) ->
    fun ({listen,Listen}) ->
	    gen_sctp:listen(Socket, Listen);
	(port) ->
	    ok(inet:port(Socket));
	(socket) ->
	    Socket;
	(recv_assoc) ->
	    case recv_event(ok(gen_sctp:recv(Socket, Timeout))) of
		{AssocAddr,AssocPort,
		 #sctp_paddr_change{state=addr_confirmed,
				    addr={_,AssocPort},
				    error=0,
				    assoc_id=AssocId}} ->
		    {AssocAddr,AssocPort,
		     #sctp_assoc_change{state=comm_up,
					error=0,
					outbound_streams=Os,
					inbound_streams=Is,
					assoc_id=AssocId}} =
			recv_event(ok(gen_sctp:recv(Socket, infinity))),
		    {AssocId,Os,Is,AssocAddr,AssocPort};
		{AssocAddr,AssocPort,
		 #sctp_assoc_change{state=comm_up,
				    error=0,
				    outbound_streams=Os,
				    inbound_streams=Is,
				    assoc_id=AssocId}} ->
		    {AssocId,Os,Is,AssocAddr,AssocPort}
	    end;
	    %% {AssocAddr,AssocPort,[],
	    %%  #sctp_assoc_change{state=comm_up,
	    %% 			error=0,
	    %% 			outbound_streams=Os,
	    %% 			inbound_streams=Is,
	    %% 			assoc_id=AssocId}} =
	    %% 	ok(gen_sctp:recv(Socket, infinity)),
	    %% case log(gen_sctp:recv(Socket, Timeout)) of
	    %% 	{ok,AssocAddr,AssocPort,[],
	    %% 	 #sctp_paddr_change{addr = {AssocAddr,AssocPort},
	    %% 			    state = addr_available,
	    %% 			    error = 0,
	    %% 			    assoc_id = AssocId}} -> ok;
	    %% 	{error,timeout} -> ok
	    %% end,
	    %% {AssocId,Os,Is,AssocAddr,AssocPort};
	({connect,ConAddr,ConPort,ConOpts}) ->
	    ok = gen_sctp:connect_init(Socket, ConAddr, ConPort, ConOpts),
	    case recv_event(ok(gen_sctp:recv(Socket, Timeout))) of
		{ConAddr,ConPort,
		 #sctp_paddr_change{state=addr_confirmed,
				    addr={_,ConPort},
				    error=0,
				    assoc_id=AssocId}} ->
		    {ConAddr,ConPort,
		     #sctp_assoc_change{state=comm_up,
					error=0,
					outbound_streams=Os,
					inbound_streams=Is,
					assoc_id=AssocId}} =
			recv_event(ok(gen_sctp:recv(Socket, infinity))),
		    {AssocId,Os,Is};
		{ConAddr,ConPort,
		 #sctp_assoc_change{state=comm_up,
				    error=0,
				    outbound_streams=Os,
				    inbound_streams=Is,
				    assoc_id=AssocId}} ->
		    {AssocId,Os,Is}
	    end;
	    %% #sctp_assoc_change{state=comm_up,
	    %% 		       error=0,
	    %% 		       outbound_streams=Os,
	    %% 		       inbound_streams=Is,
	    %% 		       assoc_id=AssocId} =
	    %% 	ok(gen_sctp:connect(Socket, ConAddr, ConPort, ConOpts)),
	    %% case log(gen_sctp:recv(Socket, Timeout)) of
	    %% 	{ok,ConAddr,ConPort,[],
	    %% 	 #sctp_paddr_change{addr = {ConAddr,ConPort},
	    %% 			    state = addr_available,
	    %% 			    error = 0,
	    %% 			    assoc_id = AssocId}} -> ok;
	    %% 	{error,timeout} -> ok
	    %% end,
	    %% {AssocId,Os,Is};
	({send,AssocId,Stream,Data}) ->
	    gen_sctp:send(Socket, AssocId, Stream, Data);
	({send,S,AssocId,Stream,Data}) ->
	    gen_sctp:send(S, AssocId, Stream, Data);
	(recv) ->
	    {Addr,Port,
	     [#sctp_sndrcvinfo{stream=Stream,assoc_id=AssocId}],Data} =
		ok(gen_sctp:recv(Socket, infinity)),
	    {Addr,Port,AssocId,Stream,Data};
	({recv,S}) ->
	    {Addr,Port,
	     [#sctp_sndrcvinfo{stream=Stream,assoc_id=AssocId}],Data} =
		ok(gen_sctp:recv(S, infinity)),
	    {Addr,Port,AssocId,Stream,Data};
	({setopts,Opts}) ->
	    inet:setopts(Socket, Opts);
	({getopts,Optnames}) ->
	    inet:getopts(Socket, Optnames)
    end.

socket_stop(Handler) ->
    Mref = erlang:monitor(process, Handler),
    Handler ! {self(),Mref},
    receive
	{Handler,Mref,Result} ->
	    receive {'DOWN',Mref,_,_,_} -> Result end;
	{'DOWN',Mref,_,_,Error} ->
	    exit(Error)
    end.

socket_call(Handler, Request) ->
    socket_resp(socket_req(Handler, Request)).

socket_req(Handler, Request) ->
    Mref = erlang:monitor(process, Handler),
    Handler ! {self(),Mref,Request},
    {Handler,Mref}.

socket_resp({Handler,Mref}) ->
    receive
	{'DOWN',Mref,_,_,Error} ->
	    exit(Error);
	{Handler,Mref,Reply} ->
	    erlang:demonitor(Mref),
	    receive {'DOWN',Mref,_,_,_} -> ok after 0 -> ok end,
	    Reply
    end.
