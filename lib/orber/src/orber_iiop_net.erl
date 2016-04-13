%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
%%
%%-----------------------------------------------------------------
%% File: orber_iiop_net.erl
%% 
%% Description:
%%    This file contains the IIOP communication server
%%
%%-----------------------------------------------------------------
-module(orber_iiop_net).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, connect/5, connections/0, 
	 sockname2peername/2, peername2sockname/2,
	 add_connection/5,
	 add/3, remove/1, reconfigure/1, reconfigure/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3, 
	 handle_cast/2, handle_info/2, code_change/3]).

%%-----------------------------------------------------------------
%% Server state record and definitions
%%-----------------------------------------------------------------
-define(CONNECTION_DB, orber_iiop_net_db).

-record(state, {ports=[], max_connections, db, counter = 1, queue}).

-record(connection, {pid, socket, type, peerdata, localdata, ref = 0}).

-record(listen, {pid, socket, port, type, ref = 0, options, proxy_options = []}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/1
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_iiop_net}, orber_iiop_net, Opts, []).

add(IP, normal, Options) ->
    Port = orber_tb:keysearch(iiop_port, Options, orber_env:iiop_port()),
    gen_server:call(orber_iiop_net, {add, IP, normal, Port, Options}, infinity);
add(IP, ssl, Options) ->
    Port = orber_tb:keysearch(iiop_ssl_port, Options, orber_env:iiop_ssl_port()),
    gen_server:call(orber_iiop_net, {add, IP, ssl, Port, Options}, infinity).

remove(Ref) ->
    gen_server:call(orber_iiop_net, {remove, Ref}, infinity).

reconfigure(Options) ->
    lists:foreach(fun(P) -> 
			  P !  {reconfigure, Options}
		  end, 
		  do_select([{#connection{pid = '$1', _='_'}, 
			      [], ['$1']}])).

reconfigure(Options, Ref) ->
    case do_select([{#connection{ref = Ref, pid = '$1', _='_'}, 
		     [], ['$1']}]) of
	[Pid] when is_pid(Pid) ->
	    Pid !  {reconfigure, Options},
	    ok;
	_ ->
	    {error, "No proxy matched the supplied reference"}
    end.

connect(Type, S, AcceptPid, Ref, ProxyOptions) ->
    gen_server:call(orber_iiop_net, {connect, Type, S, AcceptPid, 
				     Ref, ProxyOptions}, infinity).

connections() ->
    do_select([{#connection{peerdata = '$1', _='_'}, [], ['$1']}]).

sockname2peername(SockHost, SockPort) ->
    do_select([{#connection{peerdata = '$1',
			    localdata = {match_type(SockHost), 
					 match_type(SockPort)}, 
			    _='_'}, [], ['$1']}]).


peername2sockname(PeerHost, PeerPort) ->
    do_select([{#connection{peerdata = {match_type(PeerHost), 
					match_type(PeerPort)},
			    localdata = '$1', 
			    _='_'}, [], ['$1']}]).

do_select(Pattern) ->   
    case catch ets:select(?CONNECTION_DB, Pattern) of
	{'EXIT', _What} ->
	    [];
	Result ->
	    Result
    end.

match_type(0) -> 
    %% Wildcard port number
    '_';
match_type("") -> 
    %% Wildcard host
    '_';
match_type(Key) -> 
    %% Wildcard not used.
    Key.
    
add_connection(Socket, Type, PeerData, LocalData, Ref) ->
    ets:insert(?CONNECTION_DB, #connection{pid = self(), socket = Socket, 
					   type = Type, peerdata = PeerData,
					   localdata = LocalData, ref = Ref}).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),
    {ok, parse_options(Options, 
		       #state{max_connections = orber:iiop_max_in_connections(),
			      db = ets:new(?CONNECTION_DB, [set, public, 
							    named_table,
							    {keypos, 2}]), 
			      queue = queue:new()})}.

%%-----------------------------------------------------------------
%% Func: terminate/1
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: get_options/2
%%-----------------------------------------------------------------
get_options(normal, _Options) ->
    [];
get_options(ssl, Options) ->    
    SSLOpts = 
	case orber_tb:keysearch(ssl_server_options, Options,
				orber_env:ssl_server_options()) of
	    [] ->
		Verify = orber_tb:keysearch(ssl_server_verify, Options, 
					    orber_env:ssl_server_verify()),
		Depth = orber_tb:keysearch(ssl_server_depth, Options, 
					   orber_env:ssl_server_depth()),
		Cert = orber_tb:keysearch(ssl_server_certfile, Options, 
					  orber_env:ssl_server_certfile()),
		CaCert = orber_tb:keysearch(ssl_server_cacertfile, Options, 
					    orber_env:ssl_server_cacertfile()),
		Pwd = orber_tb:keysearch(ssl_server_password, Options, 
					 orber_env:ssl_server_password()),
		Key = orber_tb:keysearch(ssl_server_keyfile, Options, 
					 orber_env:ssl_server_keyfile()),
		Ciphers = orber_tb:keysearch(ssl_server_ciphers, Options, 
					     orber_env:ssl_server_ciphers()),
		Timeout = orber_tb:keysearch(ssl_server_cachetimeout, Options, 
					     orber_env:ssl_server_cachetimeout()),
		KeepAlive = orber_tb:keysearch(ssl_server_cachetimeout, Options, 
					       orber_env:iiop_ssl_in_keepalive()),
		[{verify, Verify},
		 {depth, Depth},
		 {certfile, Cert},
		 {cacertfile, CaCert},
		 {password, Pwd},
		 {keyfile, Key},
		 {ciphers, Ciphers},
		 {cachetimeout, Timeout},
		 {keepalive, KeepAlive}];
	    Opts ->	
		case orber_tb:check_illegal_tcp_options(Opts) of
		    ok -> 
			check_old_ssl_server_options(Options),
			Opts;
		    {error, IllegalOpts} ->
			error_logger:error_report([{application, orber},
						   "TCP options not allowed to set on a connection", 
						   IllegalOpts]),
			error("Illegal TCP option")
		end
	end,
    ssl_server_extra_options(SSLOpts, []).

%%-----------------------------------------------------------------
%% Func: parse_options/2
%%-----------------------------------------------------------------
parse_options([{port, Type, Port} | Rest], State) ->
    Options = get_options(Type, []),
    Family = orber_env:ip_version(),
    IPFamilyOptions = 
	case Family of
	    inet -> [inet];
	    inet6 -> [inet6, {ipv6_v6only, true}]
	end,    
    Options2 = 
	case orber_env:ip_address_variable_defined() of
	    false ->
		IPFamilyOptions ++ Options;
	    Host ->
		{ok, IP} = inet:getaddr(Host, Family),
		IPFamilyOptions ++ [{ip, IP} |Options]
	end,

    {ok, Listen, NewPort} = orber_socket:listen(Type, Port, Options2, true),
    {ok, Pid} = orber_iiop_socketsup:start_accept(Type, Listen, 0),
    link(Pid),
    ets:insert(?CONNECTION_DB, #listen{pid = Pid, socket = Listen, 
				       port = NewPort, type = Type,
				       options = Options2}),
    parse_options(Rest, State);
parse_options([], State) ->
    State.

ssl_server_extra_options([], Acc) ->
    Acc;
ssl_server_extra_options([{_Type, []}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{_Type, infinity}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{Type, Value}|T], Acc) ->
    ssl_server_extra_options(T, [{Type, Value}|Acc]).

filter_options([], Acc) ->
    Acc;
filter_options([{verify, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{depth, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{certfile, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{cacertfile, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{password, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{keyfile, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{ciphers, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([{cachetimeout, _}|T], Acc) ->
    filter_options(T, Acc);
filter_options([H|T], Acc) ->
    filter_options(T, [H|Acc]).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({remove, Ref}, _From, State) ->
    case do_select([{#listen{ref = Ref, pid = '$1', socket = '$2',
			     type = '$3', _='_'}, [], [{{'$1', '$2', '$3'}}]}]) of
	[{Pid, Listen, Type}|_] when is_pid(Pid) ->
	    unlink(Pid),
	    ets:delete(?CONNECTION_DB, Pid),
	    %% Just close the listen socket. Will cause the accept processs 
	    %% to terminate.
	    orber_socket:close(Type, Listen),
	    stop_proxies(do_select([{#connection{ref = Ref, pid = '$1', _='_'}, 
				     [], ['$1']}])),
	    {reply, ok, 
	     State#state{queue = 
			 from_list(
			   lists:keydelete(Pid, 1, 
					   queue:to_list(State#state.queue)))}};
	_ ->
	    {reply, ok, State}
    end;
handle_call({add, IP, Type, Port, AllOptions}, _From, State) ->
    Family = orber_tb:keysearch(ip_family, AllOptions, orber_env:ip_version()),
    IPFamilyOptions = 
	case Family of
	    inet -> [inet];
	    inet6 -> [inet6, {ipv6_v6only, true}]
	end,   
    case inet:getaddr(IP, Family) of
	{ok, IPTuple} ->
	    try 
		Options = IPFamilyOptions ++ [{ip, IPTuple} |get_options(Type, AllOptions)],
	        Ref = make_ref(),
	        ProxyOptions = filter_options(AllOptions, []),
	        case orber_socket:listen(Type, Port, Options, false) of
		    {ok, Listen, NewPort} ->
			{ok, Pid} = orber_iiop_socketsup:start_accept(Type, Listen, Ref,
								      ProxyOptions),
			link(Pid),
			ets:insert(?CONNECTION_DB, #listen{pid = Pid, 
							   socket = Listen, 
							   port = NewPort, 
							   type = Type, ref = Ref,
							   options = Options,
							   proxy_options = ProxyOptions}),
			{reply, {ok, Ref}, State};
		    Error ->
			{reply, Error, State}
		end
            catch
		error:Reason ->
		    {reply, {error, Reason}, State}
	    end;
	Other ->
	    {reply, Other, State}
    end;
handle_call({connect, Type, Socket, _AcceptPid, AccepRef, ProxyOptions}, _From, State) 
  when State#state.max_connections == infinity;
       State#state.max_connections > State#state.counter ->
    case catch access_allowed(Type, Socket, Type) of
	true ->
	    case orber_iiop_insup:start_connection(Type, Socket, 
						   AccepRef, ProxyOptions) of
		{ok, Pid} when is_pid(Pid) ->
		    link(Pid),
		    {reply, {ok, Pid, true}, update_counter(State, 1)};
		Other ->
		    {reply, Other, State}
	    end;
	_ ->
	    {H, P} = orber_socket:peerdata(Type, Socket),
	    orber_tb:info("Blocked connect attempt from ~s - ~p", [H, P]),
	    {reply, denied, State}
    end;
handle_call({connect, Type, Socket, AcceptPid, AccepRef, ProxyOptions}, _From, 
	    #state{queue = Q} = State) ->
    case catch access_allowed(Type, Socket, Type) of
	true ->
	    case orber_iiop_insup:start_connection(Type, Socket, 
						   AccepRef, ProxyOptions) of
		{ok, Pid} when is_pid(Pid) ->
		    link(Pid),
		    Ref = erlang:make_ref(),
		    {reply, {ok, Pid, Ref}, 
		     update_counter(State#state{queue = 
						queue:in({AcceptPid, Ref}, Q)}, 1)};
		Other ->
		    {reply, Other, State}
	    end;
	_ ->
	    {H, P} = orber_socket:peerdata(Type, Socket),
	    orber_tb:info("Blocked connect attempt from ~s - ~p", [H, P]),
	    {reply, denied, State}
    end;
handle_call(_, _, State) ->
    {noreply, State}.

stop_proxies([H|T]) ->
    catch orber_iiop_inproxy:stop(H),
    stop_proxies(T);
stop_proxies([]) ->
    ok.

access_allowed(Type, Socket, Type) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_ACL_INCOMING) of
	false ->
	    true;
	true ->
	    SearchFor = 
		case Type of
		    normal ->
			tcp_in;
		    ssl ->
			ssl_in
		end,	    
	    {ok, {Host, Port}} = orber_socket:peername(Type, Socket),
	    case orber_acl:match(Host, SearchFor, true) of
		{true, [], 0} ->
		    true;
		{true, [], Port} ->
		    true;
		{true, [], {Min, Max}} when Port >= Min, Port =< Max ->
		    true;
		{true, Interfaces, 0} ->
		    get_sockethost(Type, Socket),
		    lists:member(get_sockethost(Type, Socket), Interfaces);
		{true, Interfaces, Port} ->
		    lists:member(get_sockethost(Type, Socket), Interfaces);
		{true, Interfaces, {Min, Max}} when Port >= Min, Port =< Max ->
		    lists:member(get_sockethost(Type, Socket), Interfaces);
		_ ->
		    false
	    end
    end.

get_sockethost(Type, Socket) ->
    case orber_socket:peername(Type, Socket) of
	{ok, {Addr, _Port}} ->
	    orber_env:addr2str(Addr);
	_ ->
	    false
    end.
   
%%------------------------------------------------------------
%% Standard gen_server cast handle
%%------------------------------------------------------------
handle_cast(_, State) ->
    {noreply,  State}.

%%------------------------------------------------------------
%% Standard gen_server handles
%%------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) when is_pid(Pid) ->
    case ets:lookup(?CONNECTION_DB, Pid) of
	[#listen{pid = Pid, socket = Listen, port = Port, type = Type, 
		 ref = Ref, options = Options, proxy_options = POpts}] ->
	    ets:delete(?CONNECTION_DB, Pid),
	    unlink(Pid),
	    {ok, NewPid} = orber_iiop_socketsup:start_accept(Type, Listen, 
							     Ref, POpts),
	    link(NewPid),
	    ets:insert(?CONNECTION_DB, #listen{pid = NewPid, socket = Listen, 
					       port = Port, type = Type, 
					       ref = Ref, options = Options, 
					       proxy_options = POpts}),
	    %% Remove the connection if it's in the queue.
	    {noreply, 
	     State#state{queue = 
			 from_list(
			   lists:keydelete(Pid, 1, 
					   queue:to_list(State#state.queue)))}};
	[#connection{pid = Pid}] ->
	    ets:delete(?CONNECTION_DB, Pid),
	    unlink(Pid),
	    case queue:out(State#state.queue) of
		{empty, _} ->
		    {noreply, update_counter(State, -1)};
		{{value, {AcceptPid, Ref}}, Q} ->
		    AcceptPid ! {Ref, ok},
		    {noreply, update_counter(State#state{queue = Q}, -1)}
	    end;
	[] ->
	    {noreply, State}
    end;
handle_info(_, State) ->
    {noreply,  State}.

from_list(List) ->
    from_list(List, queue:new()).

from_list([], Q) ->
    Q;
from_list([H|T], Q) ->
    NewQ = queue:in(H, Q),
    from_list(T, NewQ).


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
update_counter(#state{max_connections = infinity} = State, _) ->
    State;
update_counter(State, Value) ->
    State#state{counter = State#state.counter + Value}.


check_old_ssl_server_options(Options) ->
    try
	0 = orber_tb:keysearch(ssl_server_verify, Options, 
			       orber_env:ssl_server_verify()),
    	1 = orber_tb:keysearch(ssl_server_depth, Options, 
			       orber_env:ssl_server_depth()),
     	[] = orber_tb:keysearch(ssl_server_certfile, Options, 
				orber_env:ssl_server_certfile()),
	[] = orber_tb:keysearch(ssl_server_cacertfile, Options, 
				orber_env:ssl_server_cacertfile()),
	[] = orber_tb:keysearch(ssl_server_password, Options, 
				orber_env:ssl_server_password()),
	[] = orber_tb:keysearch(ssl_server_keyfile, Options, 
				orber_env:ssl_server_keyfile()),
	[] = orber_tb:keysearch(ssl_server_ciphers, Options, 
				orber_env:ssl_server_ciphers()),
	infinity = orber_tb:keysearch(ssl_server_cachetimeout, Options, 
				      orber_env:ssl_server_cachetimeout()),
	false = orber_tb:keysearch(iiop_ssl_in_keepalive, Options, 
				   orber_env:iiop_ssl_in_keepalive())
    catch
	_:_ ->
					      io:format("hej\n",[]),
	    error_logger:warning_report([{application, orber},
			 "Ignoring deprecated ssl server options used together with the ssl_server_options"])
    end.
   
