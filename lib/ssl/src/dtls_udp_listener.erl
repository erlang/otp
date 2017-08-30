%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016-2017. All Rights Reserved.
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

-module(dtls_udp_listener).

-behaviour(gen_server).

-include("ssl_internal.hrl").

%% API
-export([start_link/4, active_once/3, accept/2, sockname/1, close/1,
        get_all_opts/1, get_sock_opts/2, set_sock_opts/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, 
	{port, 
	 listner,
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

start_link(Port, EmOpts, InetOptions, DTLSOptions) ->
    gen_server:start_link(?MODULE, [Port, EmOpts, InetOptions, DTLSOptions], []).

active_once(UDPConnection, Client, Pid) ->
    gen_server:cast(UDPConnection, {active_once, Client, Pid}).

accept(UDPConnection, Accepter) ->
    call(UDPConnection, {accept, Accepter}).

sockname(UDPConnection) ->
    call(UDPConnection, sockname).
close(UDPConnection) ->
    call(UDPConnection, close).
get_sock_opts(UDPConnection, SplitSockOpts) ->
    call(UDPConnection,  {get_sock_opts, SplitSockOpts}).
get_all_opts(UDPConnection) ->
    call(UDPConnection, get_all_opts).
set_sock_opts(UDPConnection, Opts) ->
     call(UDPConnection, {set_sock_opts, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, EmOpts, InetOptions, DTLSOptions]) ->
    try 
	{ok, Socket} = gen_udp:open(Port, InetOptions),
	{ok, #state{port = Port,
		    first = true,
		    dtls_options = DTLSOptions,
		    emulated_options = EmOpts,
		    listner = Socket,
                    close = false}}
    catch _:_ ->
	    {error, closed}
    end.
handle_call({accept, _}, _, #state{close = true} = State) ->
    {reply, {error, closed}, State};

handle_call({accept, Accepter}, From, #state{first = true,
					     accepters = Accepters,
					     listner = Socket} = State0) ->
    next_datagram(Socket),
    State = State0#state{first = false,
			 accepters = queue:in({Accepter, From}, Accepters)}, 		 
    {noreply, State};

handle_call({accept, Accepter}, From, #state{accepters = Accepters} = State0) ->
    State = State0#state{accepters = queue:in({Accepter, From}, Accepters)}, 		 
    {noreply, State};
handle_call(sockname, _, #state{listner = Socket} = State) ->
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
handle_call({get_sock_opts, {SocketOptNames, EmOptNames}}, _, #state{listner = Socket,
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
handle_call({set_sock_opts, {SocketOpts, NewEmOpts}}, _, #state{listner = Socket, emulated_options = EmOpts0} = State) ->
    set_socket_opts(Socket, SocketOpts),
    EmOpts = do_set_emulated_opts(NewEmOpts, EmOpts0),
    {reply, ok, State#state{emulated_options = EmOpts}}.

handle_cast({active_once, Client, Pid}, State0) ->
    State = handle_active_once(Client, Pid, State0),
    {noreply, State}.

handle_info({udp, Socket, IP, InPortNo, _} = Msg, #state{listner = Socket} = State0) ->
    State = handle_datagram({IP, InPortNo}, Msg, State0),
    next_datagram(Socket),
    {noreply, State};

%% UDP socket does not have a connection and should not receive an econnreset
%% This does however happens on on some windows versions. Just ignoring it
%% appears to make things work as expected! 
handle_info({udp_error, Socket, econnreset = Error}, #state{listner = Socket} = State) ->
    Report = io_lib:format("Ignore SSL UDP Listener: Socket error: ~p ~n", [Error]),
    error_logger:info_report(Report),
    {noreply, State};
handle_info({udp_error, Socket, Error}, #state{listner = Socket} = State) ->
    Report = io_lib:format("SSL UDP Listener shutdown: Socket error: ~p ~n", [Error]),
    error_logger:info_report(Report),
    {noreply, State#state{close=true}};

handle_info({'DOWN', _, process, Pid, _}, #state{clients = Clients,
						 dtls_processes = Processes0,
                                                 close = ListenClosed} = State) ->
    Client = kv_get(Pid, Processes0),
    Processes = kv_delete(Pid, Processes0),
    case ListenClosed andalso kv_empty(Processes) of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State#state{clients = set_delete(Client, Clients),
                                  dtls_processes = Processes}}
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
		{{value, _}, Queue} -> 
		    State#state{dtls_msq_queues = 
				    kv_update(Client, queue:in(Msg, Queue), MsgQueues)};
		{empty, Queue} ->
		    State#state{dtls_msq_queues = 
				    kv_update(Client, queue:in(Msg, Queue), MsgQueues)}
	    end
    end.
next_datagram(Socket) ->
    inet:setopts(Socket, [{active, once}]).

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
						     listner = Socket,
						     emulated_options = EmOpts} = State) ->
    ConnArgs = [server, "localhost", Port, {self(), {Client, Socket}},
		{DTLSOpts, EmOpts, udp_listner}, User, dtls_socket:default_cb_info()],
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
do_set_emulated_opts([{active, Value} | Rest], Opts) ->
    do_set_emulated_opts(Rest,  Opts#socket_options{active = Value}).

emulated_opts_list(_,[], Acc) ->
    Acc;
emulated_opts_list( Opts, [mode | Rest], Acc) ->
    emulated_opts_list(Opts, Rest, [{mode, Opts#socket_options.mode} | Acc]); 
emulated_opts_list(Opts, [active | Rest], Acc) ->
    emulated_opts_list(Opts, Rest, [{active, Opts#socket_options.active} | Acc]).

