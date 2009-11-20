%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2009. All Rights Reserved.
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

-include("test_server.hrl").
-include_lib("kernel/include/inet_sctp.hrl").

%%-compile(export_all).

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 basic/1,xfer_min/1,xfer_active/1,api_open_close/1,api_listen/1]).

all(suite) ->
    [basic,xfer_min,xfer_active,api_open_close,api_listen].

init_per_testcase(_Func, Config) ->
    Dog = test_server:timetrap(test_server:seconds(15)),
    [{watchdog, Dog}|Config].
fin_per_testcase(_Func, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog).



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
    ?line {ok,Sb} = gen_sctp:open(),
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
    ?line {ok,{Loopback,
	       Pa,[],
	       #sctp_assoc_change{state=comm_up,
				  error=0,
				  outbound_streams=SbOutboundStreams,
				  inbound_streams=SbInboundStreams,
				  assoc_id=SbAssocId}}} =
	gen_sctp:recv(Sb, infinity),
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
    ?line ok = gen_sctp:send(Sb, SbAssocId, 0, Data),
    ?line {ok,{Loopback,
	       Pb,
	       [#sctp_sndrcvinfo{stream=Stream,
				 assoc_id=SaAssocId}],
	       Data}} =
	gen_sctp:recv(Sa, infinity),
    %%
    ?line ok = gen_sctp:eof(Sa, SaAssocChange),
    ?line {ok,{Loopback,
 	       Pa,[],
 	       #sctp_shutdown_event{assoc_id=SbAssocId}}} =
 	gen_sctp:recv(Sb, infinity),
    ?line {ok,{Loopback,
 	       Pb,[],
 	       #sctp_assoc_change{state=shutdown_comp,
 				  error=0,
 				  assoc_id=SaAssocId}}} =
 	gen_sctp:recv(Sa, infinity),
    ?line {ok,{Loopback,
 	       Pa,[],
 	       #sctp_assoc_change{state=shutdown_comp,
 				  error=0,
 				  assoc_id=SbAssocId}}} =
 	gen_sctp:recv(Sb, infinity),
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
    ?line {ok,#sctp_assoc_change{state=comm_up,
				 error=0,
				 outbound_streams=SaOutboundStreams,
				 inbound_streams=SaInboundStreams,
				 assoc_id=SaAssocId}=SaAssocChange} =
	gen_sctp:connect(Sa, Loopback, Pb, []),
    ?line io:format("Sa=~p, Pa=~p, Sb=~p, Pb=~p, SaAssocId=~p, "
		    "SaOutboundStreams=~p, SaInboundStreams=~p~n",
		    [Sa,Pa,Sb,Pb,SaAssocId,
		     SaOutboundStreams,SaInboundStreams]),
    ?line SbAssocId = 
	receive
	    {sctp,Sb,Loopback,Pa,
	     {[],
	      #sctp_assoc_change{state=comm_up,
				 error=0,
				   outbound_streams=SbOutboundStreams,
				 inbound_streams=SbInboundStreams,
				 assoc_id=SBAI}}} ->
		?line SaOutboundStreams = SbInboundStreams,
		?line  SaInboundStreams = SbOutboundStreams,
		SBAI
	after Timeout ->
		?line test_server:fail({unexpected,flush()})
	end,
    ?line io:format("SbAssocId=~p~n", [SbAssocId]),
    ?line ok = gen_sctp:send(Sa, SaAssocId, 0, Data),
    ?line receive
	      {sctp,Sb,Loopback,Pa,
	       {[#sctp_sndrcvinfo{stream=Stream,
				  assoc_id=SbAssocId}],
		Data}} -> ok;
	      {sctp,Sb,Loopback,Pa,
	       {[],
		#sctp_paddr_change{addr = {Loopback,_},
				   state = addr_available,
				   error = 0,
				   assoc_id = SbAssocId}}} ->
		  ?line receive
			    {sctp,Sb,Loopback,Pa,
			     {[#sctp_sndrcvinfo{stream=Stream,
						assoc_id=SbAssocId}],
			      Data}} -> ok
			end
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
    ?line receive
	      {sctp,Sb,Loopback,Pa,
	       {[],
		#sctp_assoc_change{state=comm_lost,
				   assoc_id=SbAssocId}}} -> ok
	  after Timeout ->
		  ?line test_server:fail({unexpected,flush()})
	  end,
    ?line ok = gen_sctp:close(Sb),
    ?line receive
	      {sctp,Sa,Loopback,Pb,
	       {[],
		#sctp_assoc_change{state=comm_lost,
				   assoc_id=SaAssocId}}} -> ok
	  after 17 -> ok %% On Solaris this does not arrive
	  end,
    ?line ok = gen_sctp:close(Sa),
    %%
    ?line receive
	      Msg -> test_server:fail({unexpected,[Msg]++flush()})
	  after 17 -> ok
	  end,
    ok.

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
				  state = comm_lost}}} =
		      gen_sctp:recv(Sa, infinity);
	      {error,#sctp_assoc_change{state=cant_assoc}} -> ok
	  end,
    ?line ok = gen_sctp:listen(Sb, true),
    ?line {ok,#sctp_assoc_change{state=comm_up,
				 error=0}} =
	gen_sctp:connect(Sa, localhost, Pb, []),
    ?line ok = gen_sctp:close(Sa),
    ?line ok = gen_sctp:close(Sb),
    ok.
