%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2013. All Rights Reserved.
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
%% As the module name imply, this module is here for ERTS internal
%% functionality. As an application programmer you should *never*
%% call anything in this module directly. Functions exported by
%% this module may change behaviour or be removed at any time
%% without any notice whatsoever. Everything in this module is
%% intentionally left undocumented, and should remain so.
%%

-module(erts_internal).

-export([await_port_send_result/3]).
-export([cmp_term/2]).
-export([map_to_tuple_keys/1, map_type/1, map_hashmap_children/1]).
-export([port_command/3, port_connect/2, port_close/1,
	 port_control/3, port_call/3, port_info/1, port_info/2]).

-export([request_system_task/3]).

-export([check_process_code/2]).

-export([flush_monitor_messages/3]).

-export([time_unit/0]).

-export([bif_timer_server/2]).

-export([get_bif_timer_servers/0, create_bif_timer/0, access_bif_timer/1]).

-export([monitor_process/2]).

-export([is_system_process/1]).

%%
%% Await result of send to port
%%

await_port_send_result(Ref, Busy, Ok) ->
    receive
	{Ref, false} -> Busy;
	{Ref, _} -> Ok
    end.

%%
%% Statically linked port NIFs
%%

-spec erts_internal:port_command(Port, Data, OptionList) -> Result when
      Port :: port() | atom(),
      Data :: iodata(),
      OptionList :: [Option],
      Option :: force | nosuspend,
      Result :: boolean() | reference() | badarg | notsup.
port_command(_Port, _Data, _OptionList) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_connect(Port, Pid) -> Result when
      Port :: port() | atom(),
      Pid :: pid(),
      Result :: true | reference() | badarg.
port_connect(_Port, _Pid) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_close(Port) -> Result when
      Port :: port() | atom(),
      Result :: true | reference() | badarg.
port_close(_Port) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_control(Port, Operation, Data) -> Result when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: iodata(),
      Result :: string() | binary() | reference() | badarg.
port_control(_Port, _Operation, _Data) ->
    erlang:nif_error(undefined).

-spec erts_internal:port_call(Port, Operation, Data) -> Result when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: term(),
      Result :: {ok, term()} | reference() | badarg.
port_call(_Port, _Operation, _Data) ->
    erlang:nif_error(undefined).

-type port_info_1_result_item() ::
      {registered_name, RegName :: atom()} |
      {id, Index :: non_neg_integer()} |
      {connected, Pid :: pid()} |
      {links, Pids :: [pid()]} |
      {name, String :: string()} |
      {input, Bytes :: non_neg_integer()} |
      {output, Bytes :: non_neg_integer()} |
      {os_pid, OsPid :: non_neg_integer() | 'undefined'}.

-spec erts_internal:port_info(Port) -> Result when
      Port :: port() | atom(),
      Result :: [port_info_1_result_item()] | undefined | reference() | badarg | [].
port_info(_Result) ->
    erlang:nif_error(undefined).

-type port_info_2_item() ::
      registered_name |
      id |
      connected |
      links |
      name |
      input |
      output |
      os_pid |
      monitors |
      memory |
      parallelism |
      queue_size |
      locking.

-type port_info_2_result_item() ::
      {registered_name, RegName :: atom()} |
      [] | % No registered name
      {id, Index :: non_neg_integer()} |
      {connected, Pid :: pid()} |
      {links, Pids :: [pid()]} |
      {name, String :: string()} |
      {input, Bytes :: non_neg_integer()} |
      {output, Bytes :: non_neg_integer()} |
      {os_pid, OsPid :: non_neg_integer() | 'undefined'} |
      {monitors, Monitors :: [{process, pid()}]} |
      {memory, MemSz :: non_neg_integer()} |
      {parallelism, Boolean :: boolean()} |
      {queue_size, QSz :: non_neg_integer()} |
      {locking, Locking :: 'false' | 'port_level' | 'driver_level'}.

-spec erts_internal:port_info(Port, Item) -> Result when
      Port :: port() | atom(),
      Item :: port_info_2_item(),
      Result :: port_info_2_result_item() | undefined | reference() | badarg.

port_info(_Result, _Item) ->
    erlang:nif_error(undefined).

-spec request_system_task(Pid, Prio, Request) -> 'ok' when
      Prio :: 'max' | 'high' | 'normal' | 'low',
      Request :: {'garbage_collect', term()}
	       | {'check_process_code', term(), module(), boolean()},
      Pid :: pid().

request_system_task(_Pid, _Prio, _Request) ->
    erlang:nif_error(undefined).

-spec check_process_code(Module, OptionList) -> boolean() when
      Module :: module(),
      Option :: {allow_gc, boolean()},
      OptionList :: [Option].
check_process_code(_Module, _OptionList) ->
    erlang:nif_error(undefined).

%% term compare where integer() < float() = true

-spec cmp_term(A,B) -> Result when
    A :: term(),
    B :: term(),
    Result :: -1 | 0 | 1.

cmp_term(_A,_B) ->
    erlang:nif_error(undefined).

%% return the internal key tuple for map keys
-spec map_to_tuple_keys(M) -> Keys when
    M :: map(),
    Keys :: tuple().

map_to_tuple_keys(_M) ->
    erlang:nif_error(undefined).

%% return the internal map type
-spec map_type(M) -> Type when
    M :: map(),
    Type :: 'flatmap' | 'hashmap' | 'hashmap_node'.

map_type(_M) ->
    erlang:nif_error(undefined).

%% return the internal hashmap sub-nodes from
%% a hashmap node
-spec map_hashmap_children(M) -> Children when
    M :: map(), %% hashmap node
    Children :: [map() | nonempty_improper_list(term(),term())].

map_hashmap_children(_M) ->
    erlang:nif_error(undefined).

-spec erts_internal:flush_monitor_messages(Ref, Multi, Res) -> term() when
      Ref :: reference(),
      Multi :: boolean(),
      Res :: term().

%% erlang:demonitor(Ref, [flush]) traps to
%% erts_internal:flush_monitor_messages(Ref, Res) when
%% it needs to flush monitor messages.
flush_monitor_messages(Ref, Multi, Res) when is_reference(Ref) ->
    receive
	{_, Ref, _, _, _} ->
	    case Multi of
		false ->
		    Res;
		_ ->
		    flush_monitor_messages(Ref, Multi, Res)
	    end
    after 0 ->
	    Res
    end.

-spec erts_internal:time_unit() -> pos_integer().

time_unit() ->
    erlang:nif_error(undefined).

-spec erts_internal:get_bif_timer_servers() -> Pids when
      Pid :: pid(),
      Pids :: [Pid].

get_bif_timer_servers() ->
    erlang:nif_error(undefined).

-spec erts_internal:create_bif_timer() -> Res when
      Res :: {reference(), pid(), reference()}.

create_bif_timer() ->
    erlang:nif_error(undefined).

-spec erts_internal:access_bif_timer(Ref) -> Res when
      Ref :: reference(),
      Res :: {reference(), pid(), reference()}.

access_bif_timer(_Ref) ->
    erlang:nif_error(undefined).

-spec erts_internal:monitor_process(Pid, Ref) -> boolean() when
      Pid :: pid(),
      Ref :: reference().

monitor_process(_Pid, _Ref) ->
    erlang:nif_error(undefined).

-spec erts_internal:is_system_process(Pid) -> boolean() when
      Pid :: pid().

is_system_process(_Pid) ->
    erlang:nif_error(undefined).

%%
%% BIF timer servers
%%

-record(tsrv_state, {rtab,
		     ttab,
		     btr,
		     unit,
		     next}).

bif_timer_server(N, BTR) ->
    try
	tsrv_loop(tsrv_init_static_state(N, BTR), infinity)
    catch
	Type:Reason ->
	    erlang:display({'BIF_timer_server',
			    {Type, Reason},
			    erlang:get_stacktrace()}),
	    exit(Reason)
    end.

tsrv_init_static_state(N, BTR) ->
    process_flag(trap_exit, true),
    NList = integer_to_list(N),
    RTabName = list_to_atom("BIF_timer_reference_table_" ++ NList),
    TTabName = list_to_atom("BIF_timer_time_table_" ++ NList),
    #tsrv_state{rtab = ets:new(RTabName,
			       [set, private, {keypos, 2}]),
		ttab = ets:new(TTabName,
			       [ordered_set, private, {keypos, 1}]),
		btr = BTR,
		unit = erts_internal:time_unit(),
		next = infinity}.
    

tsrv_loop(#tsrv_state{unit = Unit} = StaticState, Nxt) ->
    CallTime = erlang:monotonic_time(),
    %% 'infinity' is greater than all integers...
    NewNxt = case CallTime >= Nxt of
		 true ->
		     tsrv_handle_timeout(CallTime, StaticState);
		 false ->
		     TMO = try
			       (1000*(Nxt - CallTime - 1)) div Unit + 1
			   catch
			       error:badarith when Nxt == infinity -> infinity
			   end,
		     receive
			 Msg ->
			     tsrv_handle_msg(Msg, StaticState, Nxt)
		     after TMO ->
			     Nxt
		     end
	     end,
    tsrv_loop(StaticState, NewNxt).

tsrv_handle_msg({set_timeout, BTR, Proc, Time, TRef, Msg},
		#tsrv_state{rtab = RTab,
			    ttab = TTab,
			    btr = BTR},
		Nxt) when erlang:is_integer(Time) ->
    RcvTime = erlang:monotonic_time(),
    case Time =< RcvTime of
	true ->
	    try Proc ! Msg catch _:_ -> ok end,
	    Nxt;
	false ->
	    Ins = case erlang:is_atom(Proc) of
		      true ->
			  true;
		      false ->
			  try
			      erts_internal:monitor_process(Proc, TRef)
			  catch
			      _:_ -> false
			  end
		  end,
	    case Ins of
		false ->
		    Nxt;
		true ->
		    TKey = {Time, TRef},
		    true = ets:insert(RTab, TKey),
		    true = ets:insert(TTab, {TKey, Proc, Msg}),
		    case Time < Nxt of
			true -> Time;
			false -> Nxt
		    end
	    end
    end;
tsrv_handle_msg({cancel_timeout, BTR, From, Reply, Req, TRef},
		#tsrv_state{rtab = RTab,
			    ttab = TTab,
			    unit = Unit,
			    btr = BTR},
		Nxt) ->
    case ets:lookup(RTab, TRef) of
	[] ->
	    case Reply of
		false ->
		    ok;
		_ ->
		    try From ! {cancel_timer, Req, false} catch _:_ -> ok end
	    end,
	    Nxt;
	[{Time, TRef} = TKey] ->
	    ets:delete(RTab, TRef),
	    ets:delete(TTab, TKey),
	    erlang:demonitor(TRef),
	    case Reply of
		false ->
		    ok;
		_ ->
		    RcvTime = erlang:monotonic_time(),
		    RT = case Time =< RcvTime of
			     true ->
				 0;
			     false ->
				 ((1000*(Time - RcvTime)) div Unit)
			 end,
		    try From ! {cancel_timer, Req, RT} catch _:_ -> ok end
	    end,
	    case Time =:= Nxt of
		false ->
		    Nxt;
		true ->
		    case ets:first(TTab) of
			'$end_of_table' -> infinity;
			{NextTime, _TRef} -> NextTime
		    end
	    end
    end;
tsrv_handle_msg({read_timeout, BTR, From, Req, TRef},
		#tsrv_state{rtab = RTab,
			    unit = Unit,
			    btr = BTR},
		Nxt) ->
    case ets:lookup(RTab, TRef) of
	[] ->
	    try From ! {read_timer, Req, false} catch _:_ -> ok end;
	[{Time, TRef}] ->
	    RcvTime = erlang:monotonic_time(),
	    RT = case Time =< RcvTime of
		     true -> 0;
		     false -> (1000*(Time - RcvTime)) div Unit
		 end,
	    try From ! {read_timer, Req, RT} catch _:_ -> ok end
    end,
    Nxt;
tsrv_handle_msg({'DOWN', TRef, process, _, _},
		#tsrv_state{rtab = RTab,
			    ttab = TTab},
		Nxt) ->
    case ets:lookup(RTab, TRef) of
	[] ->
	    Nxt;
	[{Time, TRef} = TKey] ->
	    ets:delete(RTab, TRef),
	    ets:delete(TTab, TKey),
	    case Time =:= Nxt of
		false ->
		    Nxt;
		true ->
		    case ets:first(TTab) of
			'$end_of_table' -> infinity;
			{NextTime, _} -> NextTime
		    end
	    end
    end;
tsrv_handle_msg({cancel_all_timeouts, BTR, From, Ref},
		#tsrv_state{rtab = RTab,
			    ttab = TTab,
			    btr = BTR},
		_Nxt) ->
    tsrv_delete_monitor_objects(RTab),
    ets:delete_all_objects(TTab),
    try From ! {canceled_all_timeouts, Ref} catch _:_ -> ok end,
    infinity;
tsrv_handle_msg(_GarbageMsg, _StaticState, Nxt) ->
    Nxt.

tsrv_delete_monitor_objects(RTab) ->
    case ets:first(RTab) of
	'$end_of_table' ->
	    ok;
	TRef ->
	    erlang:demonitor(TRef),
	    ets:delete(RTab, TRef),
	    tsrv_delete_monitor_objects(RTab)
    end.

tsrv_handle_timeout(CallTime, #tsrv_state{rtab = RTab,
					  ttab = TTab} = S) ->
    case ets:first(TTab) of
	'$end_of_table' ->
	    infinity;
	{Time, _TRef} when Time > CallTime ->
	    Time;
	{_Time, TRef} = TKey ->
	    [{TKey, Proc, Msg}] = ets:lookup(TTab, TKey),
	    case erlang:is_pid(Proc) of
		false -> ok;
		_ -> erlang:demonitor(TRef)
	    end,
	    ets:delete(TTab, TKey),
	    ets:delete(RTab, TRef),
	    try Proc ! Msg catch _:_ -> ok end,
	    tsrv_handle_timeout(CallTime, S)
    end.
