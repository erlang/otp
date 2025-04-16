%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2016-2025. All Rights Reserved.
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

-module(dtls_packet_demux).
-moduledoc false.

-behaviour(gen_server).

-include("ssl_internal.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/6,
         active_once/3,
         accept/2,
         sockname/1,
         close/1,
         new_owner/2,
         new_connection/2,
         connection_setup/2,
         get_all_opts/1,
         set_all_opts/2,
         get_sock_opts/2,
         set_sock_opts/2,
         getstat/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
	 terminate/2,
         code_change/3]).

-record(state,
	{active_n,
         port,
	 listener,
         transport,
	 dtls_options,
	 emulated_options,
	 dtls_msq_queues = kv_new(),
	 dtls_processes = kv_new(),
	 accepters  = queue:new(),
         owner, %% Listen process owner
	 first,
         close,
         session_id_tracker
	}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Owner, Port, TransportInfo, EmOpts, InetOptions, DTLSOptions) ->
    gen_server:start_link(?MODULE, [Owner, Port, TransportInfo, EmOpts, InetOptions, DTLSOptions], []).

active_once(PacketSocket, Client, Pid) ->
    gen_server:cast(PacketSocket, {active_once, Client, Pid}).

accept(PacketSocket, Accepter) ->
    call(PacketSocket, {accept, Accepter}).

sockname(PacketSocket) ->
    call(PacketSocket, sockname).

close(PacketSocket) ->
    call(PacketSocket, close).

new_owner(PacketSocket, Owner) ->
    call(PacketSocket, {new_owner, Owner}).

new_connection(PacketSocket, Client) ->
    call(PacketSocket, {new_connection, Client, self()}).

connection_setup(PacketSocket, Client) ->
    gen_server:cast(PacketSocket, {connection_setup, Client}).

get_sock_opts(PacketSocket, SplitSockOpts) ->
    call(PacketSocket,  {get_sock_opts, SplitSockOpts}).
get_all_opts(PacketSocket) ->
    call(PacketSocket, get_all_opts).

set_sock_opts(PacketSocket, Opts) ->
    call(PacketSocket, {set_sock_opts, Opts}).
set_all_opts(PacketSocket, Opts) ->
    call(PacketSocket, {set_all_opts, Opts}).

getstat(PacketSocket, Opts) ->
    call(PacketSocket, {getstat, Opts}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Owner, Port0, TransportInfo, EmOpts, DTLSOptions, Socket]) ->
    InternalActiveN = get_internal_active_n(),
    erlang:monitor(process, Owner),
    {ok, SessionIdHandle} = session_id_tracker(Socket, DTLSOptions),
    proc_lib:set_label({dtls_server_packet_demultiplexer, Port0}),
    {ok, #state{active_n = InternalActiveN,
                port = Port0,
                first = true,
                transport = TransportInfo,
                dtls_options = DTLSOptions,
                emulated_options = EmOpts,
                listener = Socket,
                owner = Owner,
                close = false,
                session_id_tracker = SessionIdHandle}}.

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
handle_call(close, _, State0) ->
    case do_close(State0) of
        {stop, State} ->
            {stop, normal, stop, State};
        {wait, State} ->
            {reply, waiting, State}
    end;
handle_call({new_owner, Owner}, _, State) ->
    {reply, ok,  State#state{close = false, first = true, owner = Owner}};
handle_call({new_connection, Old, _Pid}, _,
            #state{accepters = Accepters, dtls_msq_queues = MsgQs0} = State) ->
    case queue:is_empty(Accepters) of
        false ->
            case kv_lookup(Old, MsgQs0) of
                {value, OldQueue} ->
                    MsgQs1 = kv_delete(Old, MsgQs0),
                    MsgQs = kv_insert({old,Old}, OldQueue, MsgQs1),
                    {reply, true, State#state{dtls_msq_queues = MsgQs}};
                none ->
                    %% Already set as old
                    {reply, true, State}
            end;
        true ->
            {reply, false, State}
    end;

handle_call({get_sock_opts, {SocketOptNames, EmOptNames}}, _, #state{listener = Socket,
                                                                     transport = TransportInfo,
                                                                     emulated_options = EmOpts} = State) ->
    case get_socket_opts(Socket, SocketOptNames,  element(1, TransportInfo)) of
        {ok, Opts} ->
            {reply, {ok, emulated_opts_list(EmOpts, EmOptNames, []) ++ Opts}, State};
        {error, Reason} ->
            {reply,  {error, Reason}, State}
    end;
handle_call(get_all_opts, _, #state{dtls_options = DTLSOptions,
                                    emulated_options = EmOpts} = State) ->
    {reply, {ok, EmOpts, DTLSOptions}, State};
handle_call({set_sock_opts, {SocketOpts, NewEmOpts}}, _, #state{listener = Socket, emulated_options = EmOpts0,
                                                                transport = TransportInfo} = State) ->
    set_socket_opts(Socket, SocketOpts, element(1, TransportInfo)),
    EmOpts = do_set_emulated_opts(NewEmOpts, EmOpts0),
    {reply, ok, State#state{emulated_options = EmOpts}};
handle_call({set_all_opts, {SocketOpts, NewEmOpts, SslOpts}}, _, #state{listener = Socket,
                                                                        transport = TransportInfo} = State) ->
    set_socket_opts(Socket, SocketOpts, element(1, TransportInfo)),
    {reply, ok, State#state{emulated_options = NewEmOpts, dtls_options = SslOpts}};
handle_call({getstat, Options}, _,  #state{listener = Socket, transport =  TransportInfo} = State) ->
    Stats = dtls_socket:getstat(element(1, TransportInfo), Socket, Options),
    {reply, Stats, State}.

handle_cast({active_once, Client, Pid}, State0) ->
    State = handle_active_once(Client, Pid, State0),
    {noreply, State};
handle_cast({connection_setup, Client}, #state{dtls_msq_queues = MsgQueues} = State) ->
    case kv_lookup({old, Client}, MsgQueues) of
        none ->
            {noreply, State};
        {value, {Pid, _}} ->
            Pid ! {socket_reused, Client},
            %% Will be deleted when handling DOWN message
            {noreply, State}
    end.

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
    Report = io_lib:format("SSL Packet muliplexer shutdown: Socket error: ~p ~n", [Error]),
    ?LOG_NOTICE(Report),
    {noreply, State#state{close=true}};

handle_info({'DOWN', _, process, Owner, _}, #state{owner = Owner} = State0) ->
    case do_close(State0) of
        {stop, State} ->
            {stop, normal, State};
        {wait, State} ->
            {noreply, State}
    end;
handle_info({'DOWN', _, process, Pid, _},
            #state{dtls_processes = Processes0,
                   dtls_msq_queues = MsgQueues0,
                   close = ListenClosed} = State0) ->
    Client = kv_get(Pid, Processes0),
    Processes = kv_delete(Pid, Processes0),
    State = case kv_lookup(Client, MsgQueues0) of
                none ->
                    MsgQueues1 = kv_delete({old, Client}, MsgQueues0),
                    State0#state{dtls_processes = Processes, dtls_msq_queues = MsgQueues1};
                {value, {Pid, _}} ->
                    MsgQueues1 = kv_delete(Client, MsgQueues0),
                    %% Restore old process if exists
                    case kv_lookup({old, Client}, MsgQueues1) of
                        none ->
                            State0#state{dtls_processes = Processes, dtls_msq_queues = MsgQueues1};
                        {value, Old} ->
                            MsgQueues2 = kv_delete({old, Client}, MsgQueues1),
                            MsgQueues = kv_insert(Client, Old, MsgQueues2),
                            State0#state{dtls_processes = Processes, dtls_msq_queues = MsgQueues}
                    end;
                {value, _} -> %% Old process died (just delete its queue)
                    MsgQueues1 = kv_delete({old, Client}, MsgQueues0),
                    State0#state{dtls_processes = Processes, dtls_msq_queues = MsgQueues1}
            end,
    case ListenClosed andalso kv_empty(Processes) of
        true ->
            {stop, normal, State};
        false ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_close(#state{dtls_processes = Processes, accepters = Accepters} = State) ->
    case kv_empty(Processes) of
        true ->
            {stop, State#state{close=true}};
        false ->
            lists:foreach(fun({_, From}) -> gen_server:reply(From, {error, closed}) end,
                          queue:to_list(Accepters)),
            {wait, State#state{close = true, accepters = queue:new()}}
    end.

handle_datagram(Client, Msg, #state{dtls_msq_queues = MsgQueues, accepters = AcceptorsQueue0} = State) ->
    case kv_lookup(Client, MsgQueues) of
	none ->
	    case queue:out(AcceptorsQueue0) of
		{{value, {UserPid, From}}, AcceptorsQueue} ->
		    setup_new_connection(UserPid, From, Client, Msg,
					 State#state{accepters = AcceptorsQueue});
		{empty, _} ->
		    %% Drop packet client will resend
		    State
	    end;
	{value, Queue} ->
	    dispatch(Queue, Client, Msg, State)
    end.

dispatch({Pid, Queue0}, Client, Msg, #state{dtls_msq_queues = MsgQueues} = State) ->
    case queue:out(Queue0) of
        {{value, Pid}, Queue} when is_pid(Pid) ->
            Pid ! Msg,
            State#state{dtls_msq_queues =
                            kv_update(Client, {Pid, Queue}, MsgQueues)};
        {{value, _UDP}, _Queue} ->
            State#state{dtls_msq_queues =
                            kv_update(Client, {Pid, queue:in(Msg, Queue0)}, MsgQueues)};
        {empty, Queue} ->
            State#state{dtls_msq_queues =
                            kv_update(Client, {Pid, queue:in(Msg, Queue)}, MsgQueues)}
    end.

next_datagram(Socket, N) ->
    inet:setopts(Socket, [{active, N}]).

handle_active_once(Client, Pid, #state{dtls_msq_queues = MsgQueues} = State0) ->
    {Key, Queue0} = case kv_lookup(Client, MsgQueues) of
                        {value, {Pid, Q0}} -> {Client, Q0};
                        _ ->
                            OldKey = {old, Client},
                            {Pid, Q0} = kv_get(OldKey, MsgQueues),
                            {OldKey, Q0}
                    end,
    case queue:out(Queue0) of
        {{value, Pid}, _} when is_pid(Pid) ->
            State0;
        {{value, Msg}, Queue} ->
            Pid ! Msg,
            State0#state{dtls_msq_queues = kv_update(Key, {Pid, Queue}, MsgQueues)};
        {empty, Queue0} ->
            State0#state{dtls_msq_queues = kv_update(Key, {Pid, queue:in(Pid, Queue0)}, MsgQueues)}
    end.

setup_new_connection(User, From, Client, Msg, #state{dtls_processes = Processes,
						     dtls_msq_queues = MsgQueues,
						     dtls_options = DTLSOpts,
						     port = Port,
						     listener = Socket,
                                                     session_id_tracker = Tracker,
						     emulated_options = EmOpts} = State) ->
    ConnArgs = [server, "localhost", Port, {self(), {Client, Socket}},
		{DTLSOpts, EmOpts, [{session_id_tracker, Tracker}]}, User, dtls_socket:default_cb_info()],
    case dtls_connection_sup:start_child(ConnArgs) of
	{ok, Pid} ->
	    erlang:monitor(process, Pid),
	    gen_server:reply(From, {ok, Pid}),
	    Pid ! Msg,
	    State#state{dtls_msq_queues = kv_insert(Client, {Pid, queue:new()}, MsgQueues),
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

set_socket_opts(_, [], _) ->
    ok;
set_socket_opts(Socket, SocketOpts, gen_udp) ->
    inet:setopts(Socket, SocketOpts);
set_socket_opts(Socket, SocketOpts, Cb) ->
    Cb:setopts(Socket, SocketOpts).

get_socket_opts(_, [], _) ->
     {ok, []};
get_socket_opts(Socket, SocketOpts, gen_udp) ->
    inet:getopts(Socket, SocketOpts);
get_socket_opts(Socket, SocketOpts, Cb) ->
    Cb:getopts(Socket, SocketOpts).

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

%% Regardless of the option reuse_sessions we need the session_id_tracker
%% to generate session ids, but no sessions will be stored unless
%% reuse_sessions = true.
session_id_tracker(Listener,_) ->
    dtls_server_session_cache_sup:start_child(Listener).

get_internal_active_n() ->
    case application:get_env(ssl, internal_active_n) of
        {ok, N} when is_integer(N) ->
            N;
        _  ->
            ?INTERNAL_ACTIVE_N
    end.

