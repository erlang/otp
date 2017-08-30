%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%
%% Purpose: Handles the Megaco/H.248 TCP connections.
%%
%%-----------------------------------------------------------------
-module(megaco_tcp_connection).

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------

-include_lib("megaco/src/tcp/megaco_tcp.hrl").
-include_lib("megaco/src/app/megaco_internal.hrl"). 


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1,
	 stop/1,

	 upgrade_receive_handle/2
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2,

	 handle_received_message/5
	]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the proces that keeps track of an TCP 
%%              connection.
%%-----------------------------------------------------------------

start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).


stop(Pid) ->
    call(Pid, stop).


upgrade_receive_handle(Pid, NewHandle) ->
    call(Pid, {upgrade_receive_handle, NewHandle}).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the generic server
%%-----------------------------------------------------------------
init(Arg) ->
    %% process_flag(trap_exit, true),
    ?tcp_debug(Arg, "tcp connection handler starting", [self()]),
%%     info_msg("starting with"
%% 	     "~n   Arg: ~p", [Arg]),
    {ok, Arg}.


%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_call(stop, _From, TcpRec) ->
    {stop, shutdown, ok, TcpRec};
handle_call({upgrade_receive_handle, NewHandle}, _From, TcpRec) ->
    {reply, ok, TcpRec#megaco_tcp{receive_handle = NewHandle}};
handle_call(Req, From, TcpRec) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w", [From, Req]),
    {reply, {error, {invalid_request, Req}}, TcpRec}.


%%-----------------------------------------------------------------
%% Func: handle_cast/2
%% Description: Handling cast messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_cast(stop, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_cast(Msg, TcpRec) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, TcpRec}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages. Incomming messages
%%              from the socket and exit messages.
%%-----------------------------------------------------------------
handle_info({tcp_closed, _Socket}, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_info({tcp_error, _Socket}, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_info({tcp, Socket, <<3:8, _X:8, Length:16, Msg/binary>>}, 
	    #megaco_tcp{socket = Socket, serialize = false} = TcpRec) 
  when Length < ?GC_MSG_LIMIT ->
    #megaco_tcp{module = Mod, receive_handle = RH} = TcpRec,
    incNumInMessages(Socket),
    incNumInOctets(Socket, 4+size(Msg)),
    apply(Mod, receive_message, [RH, self(), Socket, Msg]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, TcpRec};
handle_info({tcp, Socket, <<3:8, _X:8, Length:16, Msg/binary>>}, 
	    #megaco_tcp{socket = Socket, serialize = false} = TcpRec) ->
    #megaco_tcp{module = Mod, receive_handle = RH} = TcpRec,
    incNumInMessages(Socket),
    incNumInOctets(Socket, 4+size(Msg)),
    receive_message(Mod, RH, Socket, Length, Msg),
    inet:setopts(Socket, [{active, once}]),
    {noreply, TcpRec};
handle_info({tcp, Socket, <<3:8, _X:8, _Length:16, Msg/binary>>},
            #megaco_tcp{socket = Socket, serialize = true} = TcpRec) ->
    #megaco_tcp{module = Mod, receive_handle = RH} = TcpRec,
    incNumInMessages(Socket),
    incNumInOctets(Socket, 4+size(Msg)),
    process_received_message(Mod, RH, Socket, Msg),
    inet:setopts(Socket, [{active, once}]),
    {noreply, TcpRec};
handle_info({tcp, Socket, Msg}, TcpRec) ->
    incNumErrors(Socket),
    error_msg("received bad tpkt packet: "
	      "~n~w", [Msg]),
    {noreply, TcpRec};
handle_info(Info, TcpRec) ->
    warning_msg("received unexpected info: "
		"~n~p", [Info]),
    {noreply, TcpRec}.


process_received_message(Mod, RH, SH, Msg) ->
    case (catch Mod:process_received_message(RH, self(), SH, Msg)) of
	ok ->
	    ok;
	Error ->
	    error_msg("failed processing received message: "
		      "~n~p", [Error]),
	    ok
    end.


receive_message(Mod, RH, SendHandle, Length, Msg) ->
    Opts = [link , {min_heap_size, ?HEAP_SIZE(Length)}],
    spawn_opt(?MODULE, handle_received_message,
	      [Mod, RH, self(), SendHandle, Msg], Opts),
    ok.


handle_received_message(Mod, RH, Parent, SH, Msg) ->
    Mod:process_received_message(RH, Parent, SH, Msg),
    unlink(Parent),
    exit(normal).

    
%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
terminate(shutdown = _Reason, TcpRec) ->
    ?tcp_debug(TcpRec, "tcp connection handler terminating", [self(),_Reason]),
    ok;

terminate(Reason, TcpRec) ->
    ?tcp_debug(TcpRec, "tcp connection handler terminating", [self(), Reason]),
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot       = erlang:memory(total),
    error_msg("abormal termination: "
	      "~n   Scheduler id:                         ~p"
	      "~n   Num scheduler:                        ~p"
	      "~n   Process count:                        ~p"
	      "~n   Process limit:                        ~p"
	      "~n   Memory used by erlang processes:      ~p"
	      "~n   Memory allocated by erlang processes: ~p"
	      "~n   The total amount of memory allocated: ~p"
	      "~n~p", 
	      [SchedId, SchedNum, ProcCount, ProcLimit, 
	       ProcMemUsed, ProcMemAlloc, MemTot, Reason]),
    ok.


%%-----------------------------------------------------------------
%% Func: code_change/3
%% Descrition: Handles code change messages during upgrade.
%%-----------------------------------------------------------------
code_change(_OldVsn, S, _Extra) ->
    ?d("code_change -> entry with"
	"~n   OldVsn: ~p"
	"~n   S:      ~p"
	"~n   Extra:  ~p", [_OldVsn, S, _Extra]),
    {ok, S}.



%%-----------------------------------------------------------------
%% Func: incNumInMessages/1, incNumInOctets/2, incNumErrors/1
%% Description: SNMP counter increment functions
%%              
%%-----------------------------------------------------------------
incNumInMessages(Socket) ->
    incCounter({Socket, medGwyGatewayNumInMessages}, 1).

incNumInOctets(Socket, NumOctets) ->
    incCounter({Socket, medGwyGatewayNumInOctets}, NumOctets).

incNumErrors(Socket) ->
    incCounter({Socket, medGwyGatewayNumErrors}, 1).

incCounter(Key, Inc) ->
    ets:update_counter(megaco_tcp_stats, Key, Inc).


%% info_msg(F, A) ->
%%     ?megaco_info("TCP connection handler " ++ F, A).
  
warning_msg(F, A) ->
    ?megaco_warning("TCP connection handler: " ++ F, A).
  
error_msg(F, A) ->
    ?megaco_error("TCP connection handler: " ++ F, A).
  

call(Pid, Req) ->
    gen_server:call(Pid, Req, infinity).

