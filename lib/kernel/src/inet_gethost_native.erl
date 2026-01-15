%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
-module(inet_gethost_native).
-moduledoc false.
-behaviour(supervisor_bridge).

-compile(nowarn_deprecated_catch).

%% Supervisor bridge exports
-export([start_link/0, init/1, terminate/2]).

%% Server export
-export([server_init/2, main_loop/1]).

%% API exports
-export([gethostbyname/1, gethostbyname/2, gethostbyaddr/1, control/1]).

%%% Exports for sys:handle_system_msg/6
-export([system_continue/3, system_terminate/4, system_code_change/4]).

-include_lib("kernel/include/inet.hrl").

-define(PROCNAME_SUP, inet_gethost_native_sup).

-define(OP_GETHOSTBYNAME,1).
-define(OP_GETHOSTBYADDR,2).
-define(OP_CANCEL_REQUEST,3).
-define(OP_CONTROL,4).

-define(PROTO_IPV4,1).
-define(PROTO_IPV6,2).

%% OP_CONTROL
-define(SETOPT_DEBUG_LEVEL, 0).

-define(UNIT_ERROR,0).
-define(UNIT_IPV4,4).
-define(UNIT_IPV6,16).

-define(PORT_PROGRAM, "inet_gethost").
-define(DEFAULT_POOLSIZE, 4).
-define(REQUEST_TIMEOUT, (inet_db:res_option(timeout)*4)).

-define(MAX_TIMEOUT, 16#7FFFFFF).
-define(INVALID_SERIAL, 16#FFFFFFFF).

%-define(DEBUG,1).
-ifdef(DEBUG).
-define(dbg(A,B), io:format(A,B)).
-else.
-define(dbg(A,B), noop).
-endif.

%% In erlang, IPV6 addresses are built as 8-tuples of 16bit values (not 16-tuples of octets).
%% This macro, meant to be used in guards checks one such 16bit value in the 8-tuple.
-define(VALID_V6(Part), is_integer(Part), Part < 65536).
%% The regular IPV4 addresses are represented as 4-tuples of octets, this macro,
%% meant to be used in guards, check one such octet.
-define(VALID_V4(Part), is_integer(Part), Part < 256).

% Statistics, not used yet.
-record(statistics, {
	  netdb_timeout = 0,
	  netdb_internal = 0,
	  port_crash = 0,
	  notsup = 0,
	  host_not_found = 0,
	  try_again = 0,
	  no_recovery = 0,
	  no_data = 0
}).

-record(request,
        {rid,
         req, % {Op,Proto,Data}
         timer_ref,
         %%
         %% Timestamp in monotonic time native units,
         %% for latest piggy backed client request, used to
         %% restart server time-out matching latest client time-out
         req_ts = undefined
        }).

% The main loopstate...
-record(
   state,
   {
    port = noport, % Port() connected to the port program
    timeout = 8000, % Timeout value from inet_db:res_option
    %%
    %% One per unique request to the PORT program.
    %% Clients are registered in req_clients, multiple per RID.
    %% ETS set of {RID,{Op,Proto,Data}=Req}
    requests,
    %%
    %% One per request as the above,
    %% but for reverse lookup to find duplicate requests.
    %% ETS set of {{Op,Proto,Data}=Req,RID}
    req_index,
    %%
    %% One per requesting client for RID.
    %% When the request succeeds we can take all clients with key RID.
    %% When a request times out we can remove just that object from the bag.
    %% ETS bag of {RID,ClientHandle,TimerRef}
    req_clients,
    %%
    parent,    % The supervisor bridge
    pool_size = 4, % Number of C processes in pool.
    statistics % Statistics record (records error causes).
   }).
-type state() :: #state{}.

%% The supervisor bridge code
-spec init([]) -> {'ok', pid(), pid()} | {'error', term()}.

init([]) -> % Called by supervisor_bridge:start_link
    Ref = make_ref(),
    SaveTE = process_flag(trap_exit,true),
    Pid = spawn_link(?MODULE,server_init,[self(),Ref]),
    receive
	Ref ->
	    process_flag(trap_exit,SaveTE),
	    {ok, Pid, Pid};
	{'EXIT', Pid, Message} ->
	    process_flag(trap_exit,SaveTE),
	    {error, Message}
    after 10000 ->
	    process_flag(trap_exit,SaveTE),
	    {error, {timeout, ?MODULE}}
    end.

start_link() ->
    supervisor_bridge:start_link({local, ?PROCNAME_SUP}, ?MODULE, []).


-spec terminate(term(), pid()) -> 'ok'.

terminate(_Reason, Pid) ->
    (catch exit(Pid, kill)),
    ok.

%%-----------------------------------------------------------------------
%% Only used in fallback situations, no supervisor, no bridge, serve only until
%% no requests present...

run_once() ->
    Port = do_open_port(get_poolsize(), get_extra_args()),
    Timeout = ?REQUEST_TIMEOUT,
    persistent_term:put({?MODULE,timeout}, Timeout),
    RID = 1,
    {ClientHandle, Request} =
	receive
	    {ReqH, {?OP_GETHOSTBYNAME, Proto0, Name0}}
              when is_reference(ReqH) ->
		{ReqH,
                 [<<RID:32, ?OP_GETHOSTBYNAME:8, Proto0:8>>,Name0,0]};
	    {ReqH, {?OP_GETHOSTBYADDR, Proto1, Data1}}
              when is_reference(ReqH) ->
		{ReqH,
                 <<RID:32, ?OP_GETHOSTBYADDR:8, Proto1:8, Data1/binary>>}
	after Timeout ->
		exit(normal)
	end,
    _ = catch port_command(Port, Request),
    receive
	{Port, {data, <<RID:32, BinReply/binary>>}} ->
	    ClientHandle ! {ClientHandle, {ok, BinReply}}
    after Timeout ->
	    ClientHandle ! {ClientHandle, {error, timeout}}
    end.

%%-----------------------------------------------------------------------
%% Server API
%%-----------------------------------------------------------------------
server_init(Starter, Ref) ->
    _ = process_flag(trap_exit,true),
    case whereis(?MODULE) of
	undefined ->
	    case (catch register(?MODULE,self())) of
		true ->
		    Starter ! Ref;
		_->
		    exit({already_started,whereis(?MODULE)})
	    end;
	Winner ->
	   exit({already_started,Winner})
    end,
    _ = process_flag(message_queue_data, off_heap),
    Poolsize = get_poolsize(),
    Port = do_open_port(Poolsize, get_extra_args()),
    Timeout = ?REQUEST_TIMEOUT,
    persistent_term:put({?MODULE,timeout}, Timeout),
    put(rid,0),
    put(num_requests,0),
    RequestTab =
        ets:new(ign_requests,[set,protected,{keypos,#request.rid}]),
    RequestIndex = ets:new(ign_req_index,[set,protected]),
    RequestClients = ets:new(ign_req_clients, [bag,protected]),
    State = #state{port = Port, timeout = Timeout,
                   requests = RequestTab,
		   req_index = RequestIndex,
                   req_clients = RequestClients,
		   pool_size = Poolsize,
		   statistics = #statistics{},
		   parent = Starter},
    main_loop(State).

main_loop(State) ->
    receive
	Any ->
	    handle_message(Any,State)
    end.

handle_message({ClientHandle, {?OP_GETHOSTBYNAME, Proto, Name} = Req}, State)
  when is_reference(ClientHandle) ->
    do_handle_call(
      ClientHandle, Req, [<<?OP_GETHOSTBYNAME:8, Proto:8>>, Name,0], State),
    main_loop(State);

handle_message({ClientHandle, {?OP_GETHOSTBYADDR, Proto, Data} = Req}, State)
  when is_reference(ClientHandle) ->
    do_handle_call(
      ClientHandle, Req, <<?OP_GETHOSTBYADDR:8, Proto:8, Data/binary>>, State),
    main_loop(State);

handle_message({ClientHandle, {?OP_CONTROL, Ctl, Data}}, State)
  when is_reference(ClientHandle) ->
    _ = catch port_command(
                State#state.port,
                <<?INVALID_SERIAL:32, ?OP_CONTROL:8, Ctl:8, Data/binary>>),
    ClientHandle ! {ClientHandle, ok},
    main_loop(State);

handle_message({ClientHandle, restart_port}, State)
  when is_reference(ClientHandle) ->
    NewPort=restart_port(State),
    ClientHandle ! {ClientHandle, ok},
    main_loop(State#state{port=NewPort});

handle_message({Port, {data, Data}}, State = #state{port = Port}) ->
    NewState =
        case Data of
            <<RID:32, BinReply/binary>> ->
                case BinReply of
                    <<Unit, _/binary>>
                      when Unit =:= ?UNIT_ERROR;
                           Unit =:= ?UNIT_IPV4;
                           Unit =:= ?UNIT_IPV6 ->
                        case ets:take(State#state.requests, RID) of
                            [] ->
                                %% We must have cancelled this request
                                State;
                            [#request{timer_ref = TimerRef, req = Req}] ->
                                _ = erlang:cancel_timer(
                                      TimerRef,
                                      [{async,true},{info,false}]),
                                %% Clean up the request and reply to clients
                                ets:delete(State#state.req_index, Req),
                                lists:foreach(
                                  fun ({_,ClientHandle}) ->
                                          ClientHandle !
                                              {ClientHandle, {ok,BinReply}}
                                  end,
                                  ets:take(State#state.req_clients, RID)),
                                put(num_requests,get(num_requests) - 1),
                                State
                        end;
                    _UnitError ->
                        %% Unexpected data, let's restart it,
                        %% it must be broken.
                        NewPort = restart_port(State),
                        State#state{port=NewPort}
                end;
            _BasicFormatError ->
                NewPort = restart_port(State),
                State#state{port=NewPort}
        end,
    main_loop(NewState);

handle_message({'EXIT',Port,_Reason}, State = #state{port = Port}) -> 
    ?dbg("Port died.~n",[]),
    NewPort=restart_port(State),
    main_loop(State#state{port=NewPort});

handle_message({Port,eof}, State = #state{port = Port}) ->
    ?dbg("Port eof'ed.~n",[]),
    NewPort=restart_port(State),
    main_loop(State#state{port=NewPort});

handle_message({timeout,TimerRef,RID}, State) ->
    case ets:lookup(State#state.requests, RID) of
        [] ->
            ok;
        [#request{timer_ref = TimerRef, req = Req, req_ts = undefined}] ->
            %% Cancel the request, let the clients do their own time-out
            ets:delete(State#state.requests, RID),
            ets:delete(State#state.req_index, Req),
            ets:delete(State#state.req_clients, RID),
            put(num_requests,get(num_requests) - 1),
            %% Also cancel the request to the port program...
            _ = catch port_command(
                        State#state.port,
                        <<RID:32,?OP_CANCEL_REQUEST>>),
            ok;
        [#request{timer_ref = TimerRef, req_ts = ReqTs}] ->
            %% We may have more clients, restart the timer
            TimeoutTime =
                erlang:convert_time_unit(ReqTs, native, millisecond)
                + State#state.timeout,
            NewTimerRef =
                erlang:start_timer(TimeoutTime, self(), RID, [{abs,true}]),
            true =
                ets:update_element(
                  State#state.requests, RID,
                  [{#request.timer_ref,NewTimerRef},
                   {#request.req_ts,undefined}]),
            ok;
        [#request{}] ->
            %% Stale timer_ref - ignore
            ok
    end,
    main_loop(State);

handle_message({system, From, Req}, State) ->
    sys:handle_system_msg(
      Req, From, State#state.parent, ?MODULE, [], State);

handle_message(_, State) -> % Stray messages from dying ports etc.
    main_loop(State).


do_handle_call(ClientHandle, Req, RData, State) ->
    case ets:lookup(State#state.req_index, Req) of
        [{_,RID}] ->
            true =
                ets:update_element(
                  State#state.requests, RID,
                  {#request.req_ts,erlang:monotonic_time()}),
            ok;
        [] ->
            RID = get_rid(),
            _ = catch port_command(State#state.port, [<<RID:32>>|RData]),
            Timeout = State#state.timeout,
            TimerRef = erlang:start_timer(Timeout, self(), RID),
            ets:insert(
              State#state.requests,
              #request{rid = RID, req = Req, timer_ref = TimerRef}),
            ets:insert(State#state.req_index, {Req,RID})
    end,
    ets:insert(State#state.req_clients, {RID,ClientHandle}),
    ok.


get_rid () ->
    New = (get(rid) + 1) rem 16#7FFFFFF,
    put(rid,New),
    New.


foreach(Fun,Table) ->
    foreach(Fun,Table,ets:first(Table)).

foreach(_Fun,_Table,'$end_of_table') ->
    ok;
foreach(Fun,Table,Key) ->
    [Object] = ets:lookup(Table,Key),
    Fun(Object),
    foreach(Fun,Table,ets:next(Table,Key)).

restart_port(#state{port = Port, requests = Requests}) ->
    _ = catch port_close(Port),
    NewPort = do_open_port(get_poolsize(), get_extra_args()),
    %%
    %% Redo all requests on the new port
    foreach(
      fun (#request{rid = RID, req = {Op,Proto,Rdata}}) ->
              case Op of
                  ?OP_GETHOSTBYNAME ->
                      port_command(
                        NewPort,
                        [<<RID:32,?OP_GETHOSTBYNAME:8,Proto:8>>, Rdata, 0]);
                  ?OP_GETHOSTBYADDR ->
                      port_command(
                        NewPort,
                        <<RID:32,?OP_GETHOSTBYADDR:8,Proto:8,Rdata/binary>>)
              end
      end,
      Requests),
    NewPort.

-dialyzer({no_improper_lists, do_open_port/2}).
do_open_port(Poolsize, ExtraArgs) ->
    Args = [integer_to_list(Poolsize)] ++ ExtraArgs,
    %% open_executable/2 below assumes overlapped_io is at the head
    Opts = [overlapped_io, {args, Args}, {packet,4}, eof, binary],
    {ok,[BinDir]} = init:get_argument(bindir),
    Prog = filename:join(BinDir, ?PORT_PROGRAM),
    open_executable(Prog, Opts).

open_executable(Prog, Opts) ->
    try open_port({spawn_executable, Prog}, Opts)
    catch
        error : badarg when hd(Opts) =:= overlapped_io ->
            open_executable(Prog, tl(Opts));
        error : Reason ->
            erlang:halt(
              "Can not execute "++Prog++" : "++term2string(Reason))
    end.

term2string(Term) ->
    unicode:characters_to_list(io_lib:format("~tw", [Term])).



get_extra_args() ->
    case application:get_env(kernel, gethost_prioritize) of
        {ok, false} ->
            ["-ng"];
        _ ->
            []
    end ++
        case application:get_env(kernel, gethost_extra_args) of
            {ok, L} when is_list(L) ->
                string:tokens(L, " ");
            _ ->
                []
        end.

get_poolsize() ->
    case application:get_env(kernel, gethost_poolsize) of
	{ok,I} when is_integer(I) ->
	    I;
	_ ->
	    ?DEFAULT_POOLSIZE
    end.

%%------------------------------------------------------------------
%% System messages callbacks
%%------------------------------------------------------------------

system_continue(_Parent, _, State) ->
    main_loop(State).

system_terminate(Reason, _Parent, _, _State) ->
    exit(Reason).

-spec system_code_change(state(), module(), term(), term()) -> {'ok', state()}.
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}. %% Nothing to do in this version.


%%-----------------------------------------------------------------------
%% Client API
%%-----------------------------------------------------------------------

gethostbyname(Name) ->
    gethostbyname(Name, inet).

gethostbyname(Name, inet) when is_list(Name) ->
    getit(?OP_GETHOSTBYNAME, ?PROTO_IPV4, Name, Name);
gethostbyname(Name, inet6) when is_list(Name) ->
    getit(?OP_GETHOSTBYNAME, ?PROTO_IPV6, Name, Name);
gethostbyname(Name, Type) when is_atom(Name) ->
    gethostbyname(atom_to_list(Name), Type);
gethostbyname(_, _)  ->
    {error, formerr}.

gethostbyaddr({A,B,C,D}=Addr)
  when ?VALID_V4(A), ?VALID_V4(B), ?VALID_V4(C), ?VALID_V4(D) ->
    getit(?OP_GETHOSTBYADDR, ?PROTO_IPV4, <<A,B,C,D>>, Addr);
gethostbyaddr({A,B,C,D,E,F,G,H}=Addr)
  when ?VALID_V6(A), ?VALID_V6(B), ?VALID_V6(C), ?VALID_V6(D),
       ?VALID_V6(E), ?VALID_V6(F), ?VALID_V6(G), ?VALID_V6(H) ->
    getit
      (?OP_GETHOSTBYADDR, ?PROTO_IPV6,
       <<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16>>, Addr);
gethostbyaddr(Addr) when is_list(Addr) ->
    case inet_parse:address(Addr) of
        {ok, IP} -> gethostbyaddr(IP);
        _Error -> {error, formerr}
    end;
gethostbyaddr(Addr) when is_atom(Addr) ->
    gethostbyaddr(atom_to_list(Addr));
gethostbyaddr(_) -> {error, formerr}.

control({debug_level, Level}) when is_integer(Level) ->
    getit(?OP_CONTROL, ?SETOPT_DEBUG_LEVEL, <<Level:32>>, undefined);
control(soft_restart) ->
    getit(restart_port, undefined);
control(_) -> {error, formerr}.


getit(Op, Proto, Data, DefaultName) ->
    getit({Op, Proto, Data}, DefaultName).

getit(Req, DefaultName) ->
    Pid = ensure_started(),
    DefaultTimeout = (#state{})#state.timeout,
    Timeout = persistent_term:get({?MODULE,timeout}, DefaultTimeout),
    case call(Pid, Req, Timeout) of
        {ok, BinHostent} ->
            parse_address(BinHostent, DefaultName);
        ok ->
            ok;
        {error, _} = Result->
            Result
    end.

call(Pid, Req, Timeout) ->
    ReqHandle = monitor(process, Pid, [{alias,reply_demonitor}]),
    Pid ! {ReqHandle, Req},
    wait_reply(ReqHandle, Timeout).

wait_reply(ReqHandle, Timeout) ->
    receive
        {ReqHandle, Result} ->
            Result;
        {'DOWN', ReqHandle, process, _, Reason} ->
            {error, Reason}
    after Timeout ->
           case unalias(ReqHandle) of
                true ->
                   erlang:demonitor(ReqHandle, [flush]),
                    {error, timeout};
                false ->
                   wait_reply(ReqHandle, infinity)
           end
    end.


ensure_started() ->
    case whereis(?MODULE) of
	undefined ->
	    ChildSpec =
                {?PROCNAME_SUP, {?MODULE, start_link, []}, temporary,
		 1000, worker, [?MODULE]},
            ensure_started([kernel_safe_sup, net_sup], ChildSpec);
	Pid ->
	    Pid
    end.

ensure_started([Supervisor|Supervisors], ChildSpec) ->
    case whereis(Supervisor) of
        undefined ->
            ensure_started(Supervisors, ChildSpec);
        _ ->
            do_start(Supervisor, ChildSpec),
            case whereis(?MODULE) of
                undefined ->
                    exit({could_not_start_server, ?MODULE});
                Pid ->
                    Pid
            end
    end;
ensure_started([], _ChildSpec) ->
    %% Icky fallback, run once without supervisor
    spawn(fun run_once/0).

do_start(Sup, C) ->
    {Child,_,_,_,_,_} = C,
    case supervisor:start_child(Sup,C) of
	{ok,_} ->
	    ok;
	{error, {already_started, Pid}} when is_pid(Pid) ->
	    ok;
	{error, {{already_started, Pid}, _Child}} when is_pid(Pid) ->
	    ok;
	{error, already_present} ->
	    _ = supervisor:delete_child(Sup, Child),
	    do_start(Sup, C)
    end.


parse_address(BinHostent, DefaultName) ->
    case catch 
	begin
	    case BinHostent of
		<<?UNIT_ERROR, Errstring/binary>> -> 
		    {error, list_to_atom(listify(Errstring))};
		<<Length, Naddr:32, T0/binary>>
                  when Length =:= ?UNIT_IPV4 ->
		    {T1, Addresses} = pick_addresses_v4(Naddr, T0),
		    {Name, Names} =
			expand_default_name(pick_names(T1), DefaultName),
                    return_hostent(Length, Addresses, Name, Names);
		<<Length, Naddr:32, T0/binary>>
                  when Length =:= ?UNIT_IPV6 ->
		    {T1, Addresses} = pick_addresses_v6(Naddr, T0),
		    {Name, Names} =
			expand_default_name(pick_names(T1), DefaultName),
                    return_hostent(Length, Addresses, Name, Names);
		_Else ->
		    {error, {internal_error, {malformed_response, BinHostent}}}
	    end
	end of
	{'EXIT', Reason} ->
	    Reason;
	Normal ->
	    Normal
    end.

return_hostent(Length, Addresses, Name, Aliases) ->
    case Addresses of
        [] ->
            {error, nxdomain};
        [_|_] ->
            Addrtype =
                case Length of
                    ?UNIT_IPV4 -> inet;
                    ?UNIT_IPV6 -> inet6
                end,
            Hostent =
                #hostent{
                   h_length = Length,       h_addrtype = Addrtype,
                   h_name = Name,           h_aliases = Aliases,
                   h_addr_list = Addresses},
            {ok, Hostent}
    end.

expand_default_name([], DefaultName) when is_list(DefaultName) ->
    {DefaultName, []};
expand_default_name([], DefaultName) when is_tuple(DefaultName) ->
    {inet_parse:ntoa(DefaultName), []};
expand_default_name([Name|Names], DefaultName)
  when is_list(DefaultName); is_tuple(DefaultName) ->
    {Name, Names}.

listify(Bin) ->
    N = byte_size(Bin) - 1,
    <<Bin2:N/binary, Ch>> = Bin,
    case Ch of
	0 ->
	    listify(Bin2);
	_ ->
	    binary_to_list(Bin)
    end.
    
pick_addresses_v4(0,Tail) ->
    {Tail,[]};
pick_addresses_v4(N,<<A,B,C,D,Tail/binary>>) ->
    {NTail, OList} = pick_addresses_v4(N-1,Tail),
    {NTail, [{A,B,C,D} | OList]}.

pick_addresses_v6(0,Tail) ->
    {Tail,[]};
pick_addresses_v6(Num,<<A:16,B:16,C:16,D:16,E:16,F:16,G:16,H:16,
		  Tail/binary>>) ->
    {NTail, OList} = pick_addresses_v6(Num-1,Tail),
    {NTail, [{A,B,C,D,E,F,G,H} | OList]}.

ndx(Ch,Bin) ->
    ndx(Ch,0,byte_size(Bin),Bin).

ndx(_,N,N,_) ->
    undefined;
ndx(Ch,I,N,Bin) ->
    case Bin of
	<<_:I/binary,Ch,_/binary>> ->
	    I;
	_ ->
	    ndx(Ch,I+1,N,Bin)
    end.

pick_names(<<Length:32,Namelist/binary>>) ->
    pick_names(Length,Namelist).

pick_names(0,<<>>) ->
    [];
pick_names(0,_) ->
    exit({error,format_error});
pick_names(_N,<<>>) ->
    exit({error,format_error});
pick_names(N,Bin) ->
    Ndx = ndx(0,Bin),
    <<Str:Ndx/binary,0,Rest/binary>> = Bin,
    [binary_to_list(Str)|pick_names(N-1,Rest)].

