%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2018. All Rights Reserved.
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

-module(dtls_packet_demux).

-behaviour(gen_server).

-include("ssl_internal.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/5, active_once/3, accept/2, sockname/1, close/1,
         get_all_opts/1, get_sock_opts/2, set_sock_opts/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, 
	{active_n,
         port,
	 listener,
         transport,
	 dtls_options,
	 emulated_options,
	 dtls_msq_queues = kv_new(),
	 clients = set_new(),
	 dtls_processes = kv_new(),
	 accepters  = queue:new(),
	 first,
         close
	}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, TransportInfo, EmOpts, InetOptions, DTLSOptions) ->
    gen_server:start_link(?MODULE, [Port, TransportInfo, EmOpts, InetOptions, DTLSOptions], []).

active_once(PacketSocket, Client, Pid) ->
    gen_server:cast(PacketSocket, {active_once, Client, Pid}).

accept(PacketSocket, Accepter) ->
    call(PacketSocket, {accept, Accepter}).

sockname(PacketSocket) ->
    call(PacketSocket, sockname).
close(PacketSocket) ->
    call(PacketSocket, close).
get_sock_opts(PacketSocket, SplitSockOpts) ->
    call(PacketSocket,  {get_sock_opts, SplitSockOpts}).
get_all_opts(PacketSocket) ->
    call(PacketSocket, get_all_opts).
set_sock_opts(PacketSocket, Opts) ->
     call(PacketSocket, {set_sock_opts, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, {TransportModule, _,_,_,_} = TransportInfo, EmOpts, InetOptions, DTLSOptions]) ->
    try 
	{ok, Socket} = TransportModule:open(Port, InetOptions),
        InternalActiveN =  case application:get_env(ssl, internal_active_n) of
                               {ok, N} when is_integer(N) ->
                                   N;
                               _  ->
                                   ?INTERNAL_ACTIVE_N
                           end,

	{ok, #state{active_n = InternalActiveN,
                    port = Port,
		    first = true,
                    transport = TransportInfo,
		    dtls_options = DTLSOptions,
		    emulated_options = EmOpts,
		    listener = Socket,
                    close = false}}
    catch _:_ ->
	    {stop, {shutdown, {error, closed}}}
    end.
handle_call({accept, _}, _, #state{close = true} = State) ->
    {reply, {error, closed}, State};

handle_call({accept, Accepter}, From, #state{active_n = N,
                                             first = true,
					     accepters = Accepters,
					     listener = Socket} = State0) ->
    next_datagram(Socket, N),
    State = State0#state{first = false,
			 accepters = queue:in({Accepter, From}, Accepters)}, 		 
    {noreply, State};

handle_call({accept, Accepter}, From, #state{accepters = Accepters} = State0) ->
    State = State0#state{accepters = queue:in({Accepter, From}, Accepters)}, 		 
    {noreply, State};
handle_call(sockname, _, #state{listener = Socket} = State) ->
    Reply = inet:sockname(Socket),
    {reply, Reply, State};
handle_call(close, _, #state{dtls_processes = Processes,
                             accepters = Accepters} = State) ->
    case kv_empty(Processes) of
        true ->
            {stop, normal, ok, State#state{close=true}};
        false -> 
            lists:foreach(fun({_, From}) ->
                                  gen_server:reply(From, {error, closed})
                          end, queue:to_list(Accepters)),
            {reply, ok,  State#state{close = true, accepters = queue:new()}}
    end;
handle_call({get_sock_opts, {SocketOptNames, EmOptNames}}, _, #state{listener = Socket,
                                                               emulated_options = EmOpts} = State) ->
    case get_socket_opts(Socket, SocketOptNames) of
        {ok, Opts} ->
            {reply, {ok, emulated_opts_list(EmOpts, EmOptNames, []) ++ Opts}, State};
        {error, Reason} ->
            {reply,  {error, Reason}, State}
    end;
handle_call(get_all_opts, _, #state{dtls_options = DTLSOptions,
                                    emulated_options = EmOpts} = State) ->
    {reply, {ok, EmOpts, DTLSOptions}, State};
handle_call({set_sock_opts, {SocketOpts, NewEmOpts}}, _, #state{listener = Socket, emulated_options = EmOpts0} = State) ->
    set_socket_opts(Socket, SocketOpts),
    EmOpts = do_set_emulated_opts(NewEmOpts, EmOpts0),
    {reply, ok, State#state{emulated_options = EmOpts}}.

handle_cast({active_once, Client, Pid}, State0) ->
    State = handle_active_once(Client, Pid, State0),
    {noreply, State}.

handle_info({Transport, Socket, IP, InPortNo, _} = Msg, #state{listener = Socket, transport = {_,Transport,_,_,_}} = State0) ->
    State = handle_datagram({IP, InPortNo}, Msg, State0),
    {noreply, State};

handle_info({PassiveTag, Socket},
            #state{active_n = N,
                   listener = Socket,
                   transport = {_, _, _, _, PassiveTag}} = State) ->
    next_datagram(Socket, N),
    {noreply, State}; 
%% UDP socket does not have a connection and should not receive an econnreset
%% This does however happens on some windows versions. Just ignoring it
%% appears to make things work as expected! 
handle_info({udp_error, Socket, econnreset = Error}, #state{listener = Socket, transport = {_,_,_, udp_error,_}} = State) ->
    Report = io_lib:format("Ignore SSL UDP Listener: Socket error: ~p ~n", [Error]),
    ?LOG_NOTICE(Report),
    {noreply, State};
handle_info({ErrorTag, Socket, Error}, #state{listener = Socket, transport = {_,_,_, ErrorTag,_}} = State) ->
    Report = io_lib:format("SSL Packet muliplxer shutdown: Socket error: ~p ~n", [Error]),
    ?LOG_NOTICE(Report),
    {noreply, State#state{close=true}};

handle_info({'DOWN', _, process, Pid, _}, #state{clients = Clients,
						 dtls_processes = Processes0,
                                                 dtls_msq_queues = MsgQueues0,
                                                 close = ListenClosed} = State) ->
    Client = kv_get(Pid, Processes0),
    Processes = kv_delete(Pid, Processes0),
    MsgQueues = kv_delete(Client, MsgQueues0),
    case ListenClosed andalso kv_empty(Processes) of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State#state{clients = set_delete(Client, Clients),
                                  dtls_processes = Processes,
                                  dtls_msq_queues = MsgQueues}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_datagram(Client, Msg, #state{clients = Clients,
				    accepters = AcceptorsQueue0} = State) ->
    case set_is_member(Client, Clients) of
	false ->
	    case queue:out(AcceptorsQueue0) of
		{{value, {UserPid, From}}, AcceptorsQueue} ->	
		    setup_new_connection(UserPid, From, Client, Msg, 
					 State#state{accepters = AcceptorsQueue});
		{empty, _} ->
		    %% Drop packet client will resend
		    State
	    end;
	true -> 
	    dispatch(Client, Msg, State)
    end.

dispatch(Client, Msg, #state{dtls_msq_queues = MsgQueues} = State) ->
    case kv_lookup(Client, MsgQueues) of
	{value, Queue0} ->
	    case queue:out(Queue0) of
		{{value, Pid}, Queue} when is_pid(Pid) ->
		    Pid ! Msg,
		    State#state{dtls_msq_queues = 
				    kv_update(Client, Queue, MsgQueues)};
		{{value, _UDP}, _Queue} ->
		    State#state{dtls_msq_queues = 
				    kv_update(Client, queue:in(Msg, Queue0), MsgQueues)};
		{empty, Queue} ->
		    State#state{dtls_msq_queues = 
				    kv_update(Client, queue:in(Msg, Queue), MsgQueues)}
	    end
    end.
next_datagram(Socket, N) ->
    inet:setopts(Socket, [{active, N}]).

handle_active_once(Client, Pid, #state{dtls_msq_queues = MsgQueues} = State0) ->
    Queue0 = kv_get(Client, MsgQueues),
    case queue:out(Queue0) of
	{{value, Pid}, _} when is_pid(Pid) ->
	    State0;
	{{value, Msg}, Queue} ->	      
	    Pid ! Msg,
	    State0#state{dtls_msq_queues = kv_update(Client, Queue, MsgQueues)};
	{empty, Queue0} ->
	    State0#state{dtls_msq_queues = kv_update(Client, queue:in(Pid, Queue0), MsgQueues)}
    end.

setup_new_connection(User, From, Client, Msg, #state{dtls_processes = Processes,
						     clients = Clients,
						     dtls_msq_queues = MsgQueues,
						     dtls_options = DTLSOpts,
						     port = Port,
						     listener = Socket,
						     emulated_options = EmOpts} = State) ->
    ConnArgs = [server, "localhost", Port, {self(), {Client, Socket}},
		{DTLSOpts, EmOpts, dtls_listener}, User, dtls_socket:default_cb_info()],
    case dtls_connection_sup:start_child(ConnArgs) of
	{ok, Pid} ->
	    erlang:monitor(process, Pid),
	    gen_server:reply(From, {ok, Pid, {Client, Socket}}),
	    Pid ! Msg,
	    State#state{clients = set_insert(Client, Clients), 
			dtls_msq_queues = kv_insert(Client, queue:new(), MsgQueues),
			dtls_processes = kv_insert(Pid, Client, Processes)};
	{error, Reason} ->
	    gen_server:reply(From, {error, Reason}),
	    State
    end.

kv_update(Key, Value, Store) ->
    gb_trees:update(Key, Value, Store).
kv_lookup(Key, Store) ->
    gb_trees:lookup(Key, Store).
kv_insert(Key, Value, Store) ->
    gb_trees:insert(Key, Value, Store).
kv_get(Key, Store) -> 
    gb_trees:get(Key, Store).
kv_delete(Key, Store) ->
    gb_trees:delete(Key, Store).
kv_new() ->
    gb_trees:empty().
kv_empty(Store) ->
    gb_trees:is_empty(Store).

set_new() ->
    gb_sets:empty().
set_insert(Item, Set) ->
    gb_sets:insert(Item, Set).
set_delete(Item, Set) ->
    gb_sets:delete(Item, Set).
set_is_member(Item, Set) ->
    gb_sets:is_member(Item, Set).

call(Server, Msg) ->
    try
        gen_server:call(Server, Msg, infinity)
    catch
	exit:{noproc, _} ->
	    {error, closed};
        exit:{normal, _} ->
            {error, closed};
        exit:{{shutdown, _},_} ->
            {error, closed}
    end.

set_socket_opts(_, []) ->
    ok;
set_socket_opts(Socket, SocketOpts) ->
    inet:setopts(Socket, SocketOpts).

get_socket_opts(_, []) ->
     {ok, []};
get_socket_opts(Socket, SocketOpts) ->
    inet:getopts(Socket, SocketOpts).

do_set_emulated_opts([], Opts) ->
    Opts;
do_set_emulated_opts([{mode, Value} | Rest], Opts) ->
    do_set_emulated_opts(Rest,  Opts#socket_options{mode = Value}); 
do_set_emulated_opts([{active, N0} | Rest], Opts=#socket_options{active = Active}) when is_integer(N0) ->
    N = tls_socket:update_active_n(N0, Active),
    do_set_emulated_opts(Rest,  Opts#socket_options{active = N});
do_set_emulated_opts([{active, Value} | Rest], Opts) ->
    do_set_emulated_opts(Rest,  Opts#socket_options{active = Value}).

emulated_opts_list(_,[], Acc) ->
    Acc;
emulated_opts_list( Opts, [mode | Rest], Acc) ->
    emulated_opts_list(Opts, Rest, [{mode, Opts#socket_options.mode} | Acc]); 
emulated_opts_list(Opts, [active | Rest], Acc) ->
    emulated_opts_list(Opts, Rest, [{active, Opts#socket_options.active} | Acc]).

