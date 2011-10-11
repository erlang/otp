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
    xfer_min/1,xfer_active/1,def_sndrcvinfo/1,implicit_inet6/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [basic, api_open_close, api_listen, api_connect_init,
     api_opts, xfer_min, xfer_active, def_sndrcvinfo,
     implicit_inet6].

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
    ?line ok =
	do_from_other_process(
	  fun () -> gen_sctp:send(Sa, SaAssocId, 0, Data) end),
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
          after Timeout ->
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
    ?line case ok(gen_sctp:recv(S1)) of
	      {Loopback, P2,[],
	       #sctp_assoc_change{
			      state=comm_up,
			      error=0,
			      assoc_id=S1AssocId}} ->
		  ?LOGVAR(S1AssocId)
	  end,
    ?line #sctp_sndrcvinfo{
       ppid=17, context=0, timetolive=0, assoc_id=S1AssocId} =
	getopt(
	  S1, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S1AssocId}),
    ?line #sctp_sndrcvinfo{
       ppid=0, context=0, timetolive=0, assoc_id=S2AssocId} =
	getopt(
	  S2, sctp_default_send_param, #sctp_sndrcvinfo{assoc_id=S2AssocId}),
    %%
    ?line ok =
	gen_sctp:send(S1, S1AssocId, 1, <<"1: ",Data/binary>>),
    ?line case ok(gen_sctp:recv(S2)) of
	      {Loopback,P1,
	       [#sctp_sndrcvinfo{
		   stream=1, ppid=17, context=0, assoc_id=S2AssocId}],
	       <<"1: ",Data/binary>>} -> ok
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
	      {Loopback,P2,[],
	       #sctp_paddr_change{
			  addr={Loopback,_}, state=addr_available,
			  error=0, assoc_id=S1AssocId}} ->
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
    io:format("OK: ~p~n", [X]),
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
	      {error,#sctp_assoc_change{state=cant_assoc}} -> ok
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
		  ?line {ok,{Localhost,
			     Pb,[],
			     #sctp_assoc_change{state=comm_lost}}} =
		      gen_sctp:recv(Sa, infinity);
	      ok ->
		  ?line {ok,{Localhost,
			     Pb,[],
			     #sctp_assoc_change{state=cant_assoc}}} =
		      gen_sctp:recv(Sa, infinity)
	  end,
    ?line ok = gen_sctp:listen(Sb, true),
    ?line case gen_sctp:connect_init(Sa, localhost, Pb, []) of
	      ok ->
		  ?line {ok,{Localhost,
			     Pb,[],
			     #sctp_assoc_change{
				  state = comm_up}}} =
		      gen_sctp:recv(Sa, infinity)
	  end,
    ?line ok = gen_sctp:close(Sa),
    ?line ok = gen_sctp:close(Sb),
    ok.

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
    ?line case ok(gen_sctp:recv(S1)) of
	      {Addr,P2,[],#sctp_assoc_change{state=comm_up}} ->
		  ok
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
