%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2014. All Rights Reserved.
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
-module(ssl_socket).

-behaviour(gen_server).

-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export([socket/5, setopts/3, getopts/3, peername/2, sockname/2, port/2]).
-export([emulated_options/0, internal_inet_values/0, default_inet_values/0,
	 init/1, start_link/3, terminate/2, inherit_tracker/3, get_emulated_opts/1, 
	 set_emulated_opts/2, get_all_opts/1, handle_call/3, handle_cast/2,
	 handle_info/2, code_change/3]).

-record(state, {
	  emulated_opts,
	  port,
	  ssl_opts
	 }).

%%--------------------------------------------------------------------
%%% Internal API
%%--------------------------------------------------------------------
socket(Pid, Transport, Socket, ConnectionCb, Tracker) ->
    #sslsocket{pid = Pid, 
	       %% "The name "fd" is keept for backwards compatibility
	       fd = {Transport, Socket, ConnectionCb, Tracker}}.
setopts(gen_tcp, #sslsocket{pid = {ListenSocket, #config{emulated = Tracker}}}, Options) ->
    {SockOpts, EmulatedOpts} = split_options(Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    inet:setopts(ListenSocket, SockOpts);
setopts(_, #sslsocket{pid = {ListenSocket, #config{transport_info = {Transport,_,_,_},
						  emulated = Tracker}}}, Options) ->
    {SockOpts, EmulatedOpts} = split_options(Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    Transport:setopts(ListenSocket, SockOpts);
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
setopts(gen_tcp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

getopts(gen_tcp,  #sslsocket{pid = {ListenSocket, #config{emulated = Tracker}}}, Options) ->
    {SockOptNames, EmulatedOptNames} = split_options(Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, inet),
    {ok, EmulatedOpts ++ SocketOpts}; 
getopts(Transport,  #sslsocket{pid = {ListenSocket, #config{emulated = Tracker}}}, Options) ->
    {SockOptNames, EmulatedOptNames} = split_options(Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, Transport),
    {ok, EmulatedOpts ++ SocketOpts}; 
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
getopts(gen_tcp, Socket, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).

peername(gen_tcp, Socket) ->
    inet:peername(Socket);
peername(Transport, Socket) ->
    Transport:peername(Socket).

sockname(gen_tcp, Socket) ->
    inet:sockname(Socket);
sockname(Transport, Socket) ->
    Transport:sockname(Socket).

port(gen_tcp, Socket) ->
    inet:port(Socket);
port(Transport, Socket) ->
    Transport:port(Socket).

emulated_options() ->
    [mode, packet, active, header, packet_size].

internal_inet_values() ->
    [{packet_size,0}, {packet, 0}, {header, 0}, {active, false}, {mode,binary}].

default_inet_values() ->
    [{packet_size, 0}, {packet,0}, {header, 0}, {active, true}, {mode, list}].

inherit_tracker(ListenSocket, EmOpts, #ssl_options{erl_dist = false} = SslOpts) ->
    ssl_listen_tracker_sup:start_child([ListenSocket, EmOpts, SslOpts]);
inherit_tracker(ListenSocket, EmOpts, #ssl_options{erl_dist = true} = SslOpts) ->
    ssl_listen_tracker_sup:start_child_dist([ListenSocket, EmOpts, SslOpts]).

get_emulated_opts(TrackerPid) -> 
    call(TrackerPid, get_emulated_opts).
set_emulated_opts(TrackerPid, InetValues) -> 
    call(TrackerPid, {set_emulated_opts, InetValues}).
get_all_opts(TrackerPid) -> 
    call(TrackerPid, get_all_opts).

%%====================================================================
%% ssl_listen_tracker_sup API
%%====================================================================

start_link(Port, SockOpts, SslOpts) ->
    gen_server:start_link(?MODULE, [Port, SockOpts, SslOpts], []).

%%--------------------------------------------------------------------
-spec init(list()) -> {ok, #state{}}.
%% Possible return values not used now. 
%% |  {ok, #state{}, timeout()} | ignore | {stop, term()}.		  
%%
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Port, Opts, SslOpts]) ->
    process_flag(trap_exit, true),
    true = link(Port),
    {ok, #state{emulated_opts = Opts, port = Port, ssl_opts = SslOpts}}.

%%--------------------------------------------------------------------
-spec handle_call(msg(), from(), #state{}) -> {reply, reply(), #state{}}. 
%% Possible return values not used now.  
%%					      {reply, reply(), #state{}, timeout()} |
%%					      {noreply, #state{}} |
%%					      {noreply, #state{}, timeout()} |
%%					      {stop, reason(), reply(), #state{}} |
%%					      {stop, reason(), #state{}}.
%%
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({set_emulated_opts, Opts0}, _From,
	    #state{emulated_opts = Opts1} = State) ->
    Opts = do_set_emulated_opts(Opts0, Opts1),
    {reply, ok, State#state{emulated_opts = Opts}};
handle_call(get_emulated_opts, _From,
	    #state{emulated_opts = Opts} = State) ->
    {reply, {ok, Opts}, State};
handle_call(get_all_opts, _From,
	    #state{emulated_opts = EmOpts,
		   ssl_opts = SslOpts} = State) ->
    {reply, {ok, EmOpts, SslOpts}, State}.

%%--------------------------------------------------------------------
-spec  handle_cast(msg(), #state{}) -> {noreply, #state{}}.
%% Possible return values not used now.  
%%				      | {noreply, #state{}, timeout()} |
%%				       {stop, reason(), #state{}}.
%%
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_, State)-> 
    {noreply, State}.

%%--------------------------------------------------------------------
-spec handle_info(msg(), #state{}) ->  {stop, reason(), #state{}}. 
%% Possible return values not used now.
%%			              {noreply, #state{}}.
%%				      |{noreply, #state{}, timeout()} |
%%				     
%%
%% Description: Handling all non call/cast messages
%%-------------------------------------------------------------------
handle_info({'EXIT', Port, _}, #state{port = Port} = State) ->
    {stop, normal, State}.


%%--------------------------------------------------------------------
-spec terminate(reason(), #state{}) -> ok.
%%		       
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
-spec code_change(term(), #state{}, list()) -> {ok, #state{}}.			 
%%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Pid, Msg) ->
    gen_server:call(Pid, Msg, infinity).

split_options(Opts) ->
    split_options(Opts, emulated_options(), [], []).
split_options([], _, SocketOpts, EmuOpts) ->
    {SocketOpts, EmuOpts};
split_options([{Name, _} = Opt | Opts], Emu, SocketOpts, EmuOpts) ->
    case lists:member(Name, Emu) of
	true ->
	    split_options(Opts, Emu, SocketOpts, [Opt | EmuOpts]);
	false ->
	    split_options(Opts, Emu, [Opt | SocketOpts], EmuOpts)
    end;
split_options([Name | Opts], Emu, SocketOptNames, EmuOptNames) ->
    case lists:member(Name, Emu) of
	true ->
	    split_options(Opts, Emu, SocketOptNames, [Name | EmuOptNames]);
	false ->
	    split_options(Opts, Emu, [Name | SocketOptNames], EmuOptNames)
    end.

do_set_emulated_opts([], Opts) ->
    Opts;
do_set_emulated_opts([{Name,_} = Opt | Rest], Opts) ->
    do_set_emulated_opts(Rest, [Opt | proplists:delete(Name, Opts)]).

get_socket_opts(_, [], _) ->
    [];
get_socket_opts(ListenSocket, SockOptNames, Cb) ->
    {ok, Opts} = Cb:getopts(ListenSocket, SockOptNames),
    Opts.

get_emulated_opts(TrackerPid, EmOptNames) -> 
    {ok, EmOpts} = get_emulated_opts(TrackerPid),
    lists:map(fun(Name) -> {value, Value} = lists:keysearch(Name, 1, EmOpts),
			   Value end,
	      EmOptNames).
