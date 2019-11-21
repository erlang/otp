%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2018. All Rights Reserved.
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
-module(tls_socket).

-behaviour(gen_server).

-include("ssl_internal.hrl").
-include("ssl_api.hrl").

-export([send/3, 
         listen/3, 
         accept/3, 
         socket/5, 
         connect/4, 
         upgrade/3,
	 setopts/3, 
         getopts/3, 
         getstat/3, 
         peername/2, 
         sockname/2, 
         port/2]).

-export([split_options/1, 
         get_socket_opts/3]).

-export([emulated_options/0, 
         emulated_options/1, 
         internal_inet_values/0, 
         default_inet_values/0,
	 init/1, 
         start_link/3, 
         terminate/2, 
         inherit_tracker/3, 
	 emulated_socket_options/2, 
         get_emulated_opts/1, 
	 set_emulated_opts/2, 
         get_all_opts/1, 
         handle_call/3, 
         handle_cast/2,
	 handle_info/2, 
         code_change/3]).

-export([update_active_n/2]).

-record(state, {
	  emulated_opts,
	  port,
	  ssl_opts
	 }).

%%--------------------------------------------------------------------
%%% Internal API
%%--------------------------------------------------------------------
send(Transport, Socket, Data) ->
    Transport:send(Socket, Data).

listen(Transport, Port, #config{transport_info = {Transport, _, _, _, _}, 
				inet_user = Options, 
				ssl = SslOpts, emulated = EmOpts} = Config) ->
    case Transport:listen(Port, Options ++ internal_inet_values()) of
	{ok, ListenSocket} ->
	    {ok, Tracker} = inherit_tracker(ListenSocket, EmOpts, SslOpts),
            %% TODO not hard code
            {ok, SessionHandler} = session_tickets_tracker(7200, SslOpts),
            Trackers =  [{option_tracker, Tracker}, {session_tickets_tracker, SessionHandler}],
            Socket = #sslsocket{pid = {ListenSocket, Config#config{trackers = Trackers}}},
            check_active_n(EmOpts, Socket),
	    {ok, Socket};
	Err = {error, _} ->
	    Err
    end.

accept(ListenSocket, #config{transport_info = {Transport,_,_,_,_} = CbInfo,
			     connection_cb = ConnectionCb,
			     ssl = SslOpts,
			     trackers = Trackers}, Timeout) -> 
    case Transport:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
            Tracker = proplists:get_value(option_tracker, Trackers),
            {ok, EmOpts} = get_emulated_opts(Tracker),
	    {ok, Port} = tls_socket:port(Transport, Socket),
            {ok, Sender} = tls_sender:start(),
            ConnArgs = [server, Sender, "localhost", Port, Socket,
			{SslOpts, emulated_socket_options(EmOpts, #socket_options{}), Trackers}, self(), CbInfo],
	    case tls_connection_sup:start_child(ConnArgs) of
		{ok, Pid} ->
		    ssl_connection:socket_control(ConnectionCb, Socket, [Pid, Sender], Transport, Trackers);
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

upgrade(Socket, #config{transport_info = {Transport,_,_,_,_}= CbInfo,
			ssl = SslOptions,
			emulated = EmOpts, connection_cb = ConnectionCb}, Timeout) ->
    ok = setopts(Transport, Socket, tls_socket:internal_inet_values()),
    case peername(Transport, Socket) of
	{ok, {Address, Port}} ->
	    ssl_connection:connect(ConnectionCb, Address, Port, Socket,
				   {SslOptions, 
				    emulated_socket_options(EmOpts, #socket_options{}), undefined},
				   self(), CbInfo, Timeout);
	{error, Error} ->
	    {error, Error}
    end.

connect(Address, Port,
	#config{transport_info = CbInfo, inet_user = UserOpts, ssl = SslOpts,
		emulated = EmOpts, inet_ssl = SocketOpts, connection_cb = ConnetionCb},
	Timeout) ->
    {Transport, _, _, _, _} = CbInfo,
    try Transport:connect(Address, Port,  SocketOpts, Timeout) of
	{ok, Socket} ->
	    ssl_connection:connect(ConnetionCb, Address, Port, Socket, 
				   {SslOpts, 
				    emulated_socket_options(EmOpts, #socket_options{}), undefined},
				   self(), CbInfo, Timeout);
	{error, Reason} ->
	    {error, Reason}
    catch
	exit:{function_clause, _} ->
	    {error, {options, {cb_info, CbInfo}}};
	exit:badarg ->
	    {error, {options, {socket_options, UserOpts}}};
	exit:{badarg, _} ->
	    {error, {options, {socket_options, UserOpts}}}
    end.

socket(Pids, Transport, Socket, ConnectionCb, Trackers) ->
    #sslsocket{pid = Pids, 
	       %% "The name "fd" is keept for backwards compatibility
	       fd = {Transport, Socket, ConnectionCb, Trackers}}.
setopts(gen_tcp, Socket = #sslsocket{pid = {ListenSocket, #config{trackers = Trackers}}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOpts, EmulatedOpts} = split_options(Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    check_active_n(EmulatedOpts, Socket),
    inet:setopts(ListenSocket, SockOpts);
setopts(_, Socket = #sslsocket{pid = {ListenSocket, #config{transport_info = {Transport,_,_,_,_},
                                                            trackers = Trackers}}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOpts, EmulatedOpts} = split_options(Options),
    ok = set_emulated_opts(Tracker, EmulatedOpts),
    check_active_n(EmulatedOpts, Socket),
    Transport:setopts(ListenSocket, SockOpts);
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
setopts(gen_tcp, Socket, Options) ->
    inet:setopts(Socket, Options);
setopts(Transport, Socket, Options) ->
    Transport:setopts(Socket, Options).

check_active_n(EmulatedOpts, Socket = #sslsocket{pid = {_, #config{trackers = Trackers}}}) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    %% We check the resulting options to send an ssl_passive message if necessary.
    case proplists:lookup(active, EmulatedOpts) of
        %% The provided value is out of bound.
        {_, N} when is_integer(N), N < -32768 ->
            throw(einval);
        {_, N} when is_integer(N), N > 32767 ->
            throw(einval);
        {_, N} when is_integer(N) ->
            case get_emulated_opts(Tracker, [active]) of
                [{_, false}] ->
                    self() ! {ssl_passive, Socket},
                    ok;
                %% The result of the addition is out of bound.
                [{_, A}] when is_integer(A), A < -32768 ->
                    throw(einval);
                [{_, A}] when is_integer(A), A > 32767 ->
                    throw(einval);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

getopts(gen_tcp,  #sslsocket{pid = {ListenSocket, #config{trackers = Trackers}}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOptNames, EmulatedOptNames} = split_options(Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, inet),
    {ok, EmulatedOpts ++ SocketOpts}; 
getopts(Transport,  #sslsocket{pid = {ListenSocket, #config{trackers = Trackers}}}, Options) ->
    Tracker = proplists:get_value(option_tracker, Trackers),
    {SockOptNames, EmulatedOptNames} = split_options(Options),
    EmulatedOpts = get_emulated_opts(Tracker, EmulatedOptNames),
    SocketOpts = get_socket_opts(ListenSocket, SockOptNames, Transport),
    {ok, EmulatedOpts ++ SocketOpts}; 
%%% Following clauses will not be called for emulated options, they are  handled in the connection process
getopts(gen_tcp, Socket, Options) ->
    inet:getopts(Socket, Options);
getopts(Transport, Socket, Options) ->
    Transport:getopts(Socket, Options).

getstat(gen_tcp, Socket, Options) ->
	inet:getstat(Socket, Options);
getstat(Transport, Socket, Options) ->
	Transport:getstat(Socket, Options).

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

emulated_options(Opts) ->
      emulated_options(Opts, internal_inet_values(), default_inet_values()).

internal_inet_values() ->
    [{packet_size,0}, {packet, 0}, {header, 0}, {active, false}, {mode,binary}].

default_inet_values() ->
    [{packet_size, 0}, {packet,0}, {header, 0}, {active, true}, {mode, list}].

inherit_tracker(ListenSocket, EmOpts, #{erl_dist := false} = SslOpts) ->
    ssl_listen_tracker_sup:start_child([ListenSocket, EmOpts, SslOpts]);
inherit_tracker(ListenSocket, EmOpts, #{erl_dist := true} = SslOpts) ->
    ssl_listen_tracker_sup:start_child_dist([ListenSocket, EmOpts, SslOpts]).

session_tickets_tracker(_, #{erl_dist := false,
                             session_tickets := disabled}) ->
    {ok, disabled};
session_tickets_tracker(Lifetime, #{erl_dist := false,
                                    session_tickets := Mode,
                                    anti_replay := AntiReplay}) ->
    tls_server_session_ticket_sup:start_child([Mode, Lifetime, AntiReplay]);
session_tickets_tracker(Lifetime, #{erl_dist := true, session_tickets := Mode}) ->
    tls_server_session_ticket_sup:start_child_dist([Mode, Lifetime]).


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
    {ok, #state{emulated_opts = do_set_emulated_opts(Opts, []), port = Port, ssl_opts = SslOpts}}.

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
do_set_emulated_opts([{active, N0} | Rest], Opts) when is_integer(N0) ->
    N = update_active_n(N0, proplists:get_value(active, Opts, false)),
    do_set_emulated_opts(Rest, [{active, N} | proplists:delete(active, Opts)]);
do_set_emulated_opts([{Name,_} = Opt | Rest], Opts) ->
    do_set_emulated_opts(Rest, [Opt | proplists:delete(Name, Opts)]).

update_active_n(New, Current) ->
    if
        is_integer(Current), New + Current =< 0 ->
            false;
        is_integer(Current) ->
            New + Current;
        New =< 0 ->
            false;
        true ->
            New
    end.

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

emulated_socket_options(InetValues, #socket_options{
				       mode   = Mode,
				       header = Header,
				       active = Active,
				       packet = Packet,
				       packet_size = Size}) ->
    #socket_options{
       mode   = proplists:get_value(mode, InetValues, Mode),
       header = proplists:get_value(header, InetValues, Header),
       active = proplists:get_value(active, InetValues, Active),
       packet = proplists:get_value(packet, InetValues, Packet),
       packet_size = proplists:get_value(packet_size, InetValues, Size)
      }.

emulated_options([{mode, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(mode, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(mode, Emulated)]);
emulated_options([{header, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(header, Value),
    emulated_options(Opts, Inet,  [Opt | proplists:delete(header, Emulated)]);
emulated_options([{active, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(active, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(active, Emulated)]);
emulated_options([{packet, Value} = Opt |Opts], Inet, Emulated) ->
    validate_inet_option(packet, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet, Emulated)]);
emulated_options([{packet_size, Value} = Opt | Opts], Inet, Emulated) ->
    validate_inet_option(packet_size, Value),
    emulated_options(Opts, Inet, [Opt | proplists:delete(packet_size, Emulated)]);
emulated_options([Opt|Opts], Inet, Emulated) ->
    emulated_options(Opts, [Opt|Inet], Emulated);
emulated_options([], Inet,Emulated) ->
    {Inet, Emulated}.

validate_inet_option(mode, Value)
  when Value =/= list, Value =/= binary ->
    throw({error, {options, {mode,Value}}});
validate_inet_option(packet, Value)
  when not (is_atom(Value) orelse is_integer(Value)) ->
    throw({error, {options, {packet,Value}}});
validate_inet_option(packet_size, Value)
  when not is_integer(Value) ->
    throw({error, {options, {packet_size,Value}}});
validate_inet_option(header, Value)
  when not is_integer(Value) ->
    throw({error, {options, {header,Value}}});
validate_inet_option(active, Value)
  when Value >= -32768, Value =< 32767 ->
    ok;
validate_inet_option(active, Value)
  when Value =/= true, Value =/= false, Value =/= once ->
    throw({error, {options, {active,Value}}});
validate_inet_option(_, _) ->
    ok.
