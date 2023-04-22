%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2023. All Rights Reserved.
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
-export([map_to_tuple_keys/1, term_type/1, map_hashmap_children/1,
         map_next/3]).
-export([mc_iterator/1, mc_refill/1]).
-export([open_port/2, port_command/3, port_connect/2, port_close/1,
	 port_control/3, port_call/3, port_info/1, port_info/2]).

-export([system_check/1,
         gather_system_check_result/1]).

-export([request_system_task/3, request_system_task/4]).
-export([garbage_collect/1]).

-export([check_process_code/3]).
-export([check_dirty_process_code/2]).
-export([is_process_executing_dirty/1]).
-export([dirty_process_handle_signals/1]).

-export([wait_release_literal_area_switch/1]).

-export([purge_module/2]).

-export([flush_monitor_messages/3,
         '@flush_monitor_messages_refopt'/0]).

-export([await_result/1, gather_io_bytes/2]).

-export([time_unit/0, perf_counter_unit/0]).

-export([is_system_process/1,
         set_code_and_literal_cleaner_prio/1]).

-export([await_microstate_accounting_modifications/3,
	 gather_microstate_accounting_result/2]).

-export([trace/3, trace_pattern/3]).

-export([dist_ctrl_put_data/2]).

-export([get_dflags/0]).
-export([get_creation/0]).
-export([new_connection/1]).
-export([abort_pending_connection/2]).

-export([scheduler_wall_time/1, system_flag_scheduler_wall_time/1,
         gather_sched_wall_time_result/1,
	 await_sched_wall_time_modifications/2]).

-export([group_leader/2, group_leader/3]).

%% Auto import name clash
-export([check_process_code/1]).

-export([is_process_alive/1, is_process_alive/2]).

-export([gather_alloc_histograms/1, gather_carrier_info/1]).

-export([suspend_process/2]).

-export([process_display/2]).

-export([process_flag/3]).

-export([create_dist_channel/3]).

-export([erase_persistent_terms/0]).

-export([atomics_new/2]).

-export([counters_new/1, counters_get/2, counters_add/3,
         counters_put/3, counters_info/1]).

-export([spawn_system_process/3]).

-export([ets_lookup_binary_info/2, ets_super_user/1, ets_info_binary/1,
         ets_raw_first/1, ets_raw_next/2]).

-export([get_internal_state_blocked/1]).

-export([spawn_request/4, spawn_init/1, dist_spawn_request/4, dist_spawn_init/1]).

-export([crasher/6]).

-export([prepare_loading/2, beamfile_chunk/2, beamfile_module_md5/1]).

-export([no_aux_work_threads/0]).

-export([dynamic_node_name/0, dynamic_node_name/1]).

%%
%% Await result of send to port
%%

await_port_send_result(Ref, Busy, Ok) ->
    receive
	{Ref, false} -> Busy;
	{Ref, _} -> Ok
    end.

%%
%% Await result...
%%

await_result(Ref) when is_reference(Ref) ->
    receive
	{Ref, Result} ->
	    Result
    end.

%%
%% statistics(io) end up in gather_io_bytes/2
%%

gather_io_bytes(Ref, No) when is_reference(Ref),
			      is_integer(No),
			      No > 0 ->
    gather_io_bytes(Ref, No, 0, 0).

gather_io_bytes(_Ref, 0, InAcc, OutAcc) ->
    {{input, InAcc}, {output, OutAcc}};
gather_io_bytes(Ref, No, InAcc, OutAcc) ->
    receive
	{Ref, _SchedId, In, Out} ->
	    gather_io_bytes(Ref, No-1, InAcc + In, OutAcc + Out)
    end.

%%
%% Statically linked port NIFs
%%

-spec erts_internal:open_port(PortName, PortSettings) -> Result when
      PortName :: tuple(),
      PortSettings :: term(),
      Result :: port() | reference() | atom().
open_port(_PortName, _PortSettings) ->
    erlang:nif_error(undefined).

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
      Prio :: 'max' | 'high' | 'normal' | 'low' | 'inherit',
      Type :: 'major' | 'minor',
      Request :: {'garbage_collect', term(), Type}
	       | {'check_process_code', term(), module()}
	       | {'copy_literals', term(), boolean()},
      Pid :: pid().

request_system_task(_Pid, _Prio, _Request) ->
    erlang:nif_error(undefined).

-spec request_system_task(RequesterPid, TargetPid, Prio, Request) -> 'ok' | 'dirty_execution' when
      Prio :: 'max' | 'high' | 'normal' | 'low',
      Request :: {'garbage_collect', term()}
	       | {'check_process_code', term(), module()}
	       | {'copy_literals', term(), boolean()},
      RequesterPid :: pid(),
      TargetPid :: pid().

request_system_task(_RequesterPid, _TargetPid, _Prio, _Request) ->
    erlang:nif_error(undefined).

-spec garbage_collect(Mode) -> 'true' when Mode :: 'major' | 'minor'.

garbage_collect(_Mode) ->
    erlang:nif_error(undefined).

-spec check_process_code(Module) -> boolean() when
      Module :: module().
check_process_code(_Module) ->
    erlang:nif_error(undefined).

-spec check_process_code(Pid, Module, OptionList) -> CheckResult | async when
      Pid :: pid(),
      Module :: module(),
      RequestId :: term(),
      Option :: {async, RequestId} | {allow_gc, boolean()},
      OptionList :: [Option],
      CheckResult :: boolean() | aborted.
check_process_code(Pid, Module, OptionList)  ->
    Async = get_cpc_opts(OptionList, sync),
    case Async of
	{async, ReqId} ->
	    erts_internal:request_system_task(Pid,
					      inherit,
					      {check_process_code,
					       ReqId,
					       Module}),
	    async;
	sync ->
	    case Pid == erlang:self() of
		true ->
		    erts_internal:check_process_code(Module);
		false ->
		    ReqId = erlang:make_ref(),
		    erts_internal:request_system_task(Pid,
						      inherit,
						      {check_process_code,
						       ReqId,
						       Module}),
		    receive
			{check_process_code, ReqId, CheckResult} ->
			    CheckResult
		    end
	    end
    end.

%% gets async opt and verify valid option list
get_cpc_opts([{async, _ReqId} = AsyncTuple | Options], _OldAsync) ->
    get_cpc_opts(Options, AsyncTuple);
get_cpc_opts([{allow_gc, AllowGC} | Options], Async) when is_boolean(AllowGC) ->
    get_cpc_opts(Options, Async);
get_cpc_opts([], Async) ->
    Async;
get_cpc_opts(_, _) ->
    error(bad_option).

-spec check_dirty_process_code(Pid, Module) -> Result when
      Result :: boolean() | 'normal' | 'busy',
      Pid :: pid(),
      Module :: module().
check_dirty_process_code(_Pid,_Module) ->
    erlang:nif_error(undefined).

-spec is_process_executing_dirty(Pid) -> 'true' | 'false' when
      Pid :: pid().
is_process_executing_dirty(_Pid) ->
    erlang:nif_error(undefined).

-spec dirty_process_handle_signals(Pid) -> Res when
      Pid :: pid(),
      Res :: 'false' | 'true' | 'noproc' | 'normal' | 'more' | 'ok'.

dirty_process_handle_signals(_Pid) ->
    erlang:nif_error(undefined).

-spec wait_release_literal_area_switch(WaitMsg) -> 'true' | 'false' when
      WaitMsg :: term().

wait_release_literal_area_switch(WaitMsg) ->
    %% release_literal_area_switch() traps to here
    %% when it needs to wait
    receive WaitMsg -> ok end,
    erts_literal_area_collector:release_area_switch().

-spec purge_module(Module, Op) -> boolean() when
      Module :: module(),
      Op :: 'prepare' | 'prepare_on_load' | 'abort' | 'complete'.
purge_module(_Module, _Op) ->
    erlang:nif_error(undefined).

-spec system_check(Type) -> 'ok' when
      Type :: 'schedulers'.

system_check(_Type) ->
    erlang:nif_error(undefined).

gather_system_check_result(Ref) when is_reference(Ref) ->
    gather_system_check_result(Ref, erlang:system_info(schedulers)).

gather_system_check_result(_Ref, 0) ->
    ok;
gather_system_check_result(Ref, N) ->
    receive
        Ref ->
            gather_system_check_result(Ref, N - 1)
    end.

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

%% return the internal term type
-spec term_type(T) -> Type when
    T :: term(),
    Type :: 'flatmap' | 'hashmap' | 'hashmap_node'
          | 'fixnum'  | 'bignum'  | 'hfloat'
          | 'list' | 'tuple' | 'export' | 'fun'
          | 'refc_binary' | 'heap_binary' | 'sub_binary'
          | 'reference'   | 'external_reference'
          | 'pid' | 'external_pid' | 'port' | 'external_port'
          | 'atom' | 'catch' | 'nil'.

term_type(_T) ->
    erlang:nif_error(undefined).

%% return the internal hashmap sub-nodes from
%% a hashmap node
-spec map_hashmap_children(M) -> Children when
    M :: map(), %% hashmap node
    Children :: [map() | nonempty_improper_list(term(),term())].

map_hashmap_children(_M) ->
    erlang:nif_error(undefined).

%% return the next assoc in the iterator and a new iterator
-spec map_next(I, M, A) -> {K,V,NI} | list() when
      I :: non_neg_integer() | list(),
      M :: map(),
      K :: term(),
      V :: term(),
      A :: iterator | list(),
      NI :: maps:iterator().

map_next(_I, _M, _A) ->
    erlang:nif_error(undefined).

%% Introduced in Erlang/OTP 26. This function is a helper, called from
%% code generated by the compiler. It must be kept compatible as long
%% code calling this helper can still be loaded.
-spec mc_iterator(MapOrIter) -> NI when
      MapOrIter :: map() | maps:iterator(),
      NI :: term().

mc_iterator(Map) when is_map(Map) ->
    erts_internal:map_next(0, Map, iterator);
mc_iterator([Path | Map]) ->
    %% This is probably an iterator.
    try erts_internal:map_next(Path, Map, iterator) of
        Iter ->
            Iter
    catch
        error:badarg ->
            []
    end;
mc_iterator(MapIter) ->
    %% Possible "used" iterator. Must validate it.
    case is_map_iter(MapIter) of
        true -> MapIter;
        false -> []
    end.

is_map_iter({_, _, Iter}) ->
    is_map_iter(Iter);
is_map_iter(Iter) ->
    case Iter of
        [Path | Map] ->
            try erts_internal:map_next(Path, Map, iterator) of
                _ ->
                    true
            catch
                error:badarg ->
                    false
            end;
        none -> true;
        _ -> false
    end.

%% Introduced in Erlang/OTP 26. This function is a helper, called from
%% code generated by the compiler. It must be kept compatible as long
%% code calling this helper can still be loaded.
-spec mc_refill(IM) -> {K,V,NI} when
      IM :: nonempty_improper_list(I, M),
      I :: non_neg_integer(),
      M :: map(),
      K :: term(),
      V :: term(),
      NI :: term().

mc_refill([Path | Map]) ->
    erts_internal:map_next(Path, Map, iterator).

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

-spec '@flush_monitor_messages_refopt'() -> ok.
'@flush_monitor_messages_refopt'() ->
    %% Enables reference optimization in flush_monitor_messages/3. Note that we
    %% both body- and tail-call it to ensure that the reference isn't cleared,
    %% in case the caller to demonitor/2 wants to continue using it.
    %%
    %% This never actually runs and is only used to trigger the optimization,
    %% see the module comment in beam_ssa_recv for details.
    Ref = make_ref(),
    flush_monitor_messages(Ref, true, ok),
    flush_monitor_messages(Ref, true, ok).

-spec erts_internal:time_unit() -> pos_integer().

time_unit() ->
    erlang:nif_error(undefined).

-spec erts_internal:perf_counter_unit() -> pos_integer().

perf_counter_unit() ->
    erlang:nif_error(undefined).

-spec erts_internal:is_system_process(Pid) -> boolean() when
      Pid :: pid().

is_system_process(_Pid) ->
    erlang:nif_error(undefined).

set_code_and_literal_cleaner_prio(Prio) ->
    Ref1 = make_ref(),
    erts_code_purger ! {change_prio, self(), Ref1, Prio},
    Ref2 = make_ref(),
    LAC = find_lac(),
    LAC ! {change_prio, self(), Ref2, Prio},
    [{code_purger, receive {Ref1, OP1} -> OP1 end},
     {literal_area_collector, receive {Ref2, OP2} -> OP2 end}].

find_lac() ->
    find_lac(erlang:processes()).

find_lac([Pid|Pids]) ->
    case process_info(Pid, initial_call) of
        {initial_call, {erts_literal_area_collector, start, 0}} ->
            Pid;
        _ ->
            find_lac(Pids)
    end.

-spec await_microstate_accounting_modifications(Ref, Result, Threads) -> boolean() when
      Ref :: reference(),
      Result :: boolean(),
      Threads :: pos_integer().

await_microstate_accounting_modifications(Ref, Result, Threads) ->
    _ = microstate_accounting(Ref,Threads),
    Result.

-spec gather_microstate_accounting_result(Ref, Threads) -> [#{}] when
      Ref :: reference(),
      Threads :: pos_integer().

gather_microstate_accounting_result(Ref, Threads) ->
    microstate_accounting(Ref, Threads).

microstate_accounting(_Ref, 0) ->
    [];
microstate_accounting(Ref, Threads) ->
    receive
        Ref -> microstate_accounting(Ref, Threads - 1);
        {Ref, Res} ->
	    [Res | microstate_accounting(Ref, Threads - 1)]
    end.

-spec trace(PidPortSpec, How, FlagList) -> integer() when
      PidPortSpec :: pid() | port()
                   | all | processes | ports
                   | existing | existing_processes | existing_ports
                   | new | new_processes | new_ports,
      How :: boolean(),
      FlagList :: list().
trace(_PidSpec, _How, _FlagList) ->
    erlang:nif_error(undefined).

-type match_variable() :: atom(). % Approximation of '$1' | '$2' | ...
-type trace_pattern_mfa() ::
      {atom(),atom(),arity() | '_'} | on_load.
-type trace_match_spec() ::
      [{[term()] | '_' | match_variable() ,[term()],[term()]}].

-spec trace_pattern(MFA, MatchSpec, FlagList) -> non_neg_integer() when
      MFA :: trace_pattern_mfa(),
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause,
      FlagList :: list().
trace_pattern(_MFA, _MatchSpec, _FlagList) ->
    erlang:nif_error(undefined).

-spec dist_ctrl_put_data(DHandle, Data) -> 'ok' when
      DHandle :: erlang:dist_handle(),
      Data :: iolist().

dist_ctrl_put_data(DHandle, IoList) ->
    %%
    %% Helper for erlang:dist_ctrl_put_data/2
    %%
    %% erlang:dist_ctrl_put_data/2 traps to
    %% this function if second argument is
    %% a list...
    %%
    try
        Binary = erlang:iolist_to_binary(IoList),
        %% Restart erlang:dist_ctrl_put_data/2
        %% with the iolist converted to a binary...
        erlang:dist_ctrl_put_data(DHandle, Binary)
    catch
        Class : Reason ->
            %% Throw exception as if thrown from
            %% erlang:dist_ctrl_put_data/2 ...
            RootST = try erlang:error(Reason)
                     catch
                         error:Reason:ST ->
                             case ST of
                                 [] -> [];
                                 [_|T] -> T
                             end
                     end,
	    StackTrace = [{erlang, dist_ctrl_put_data,
                           [DHandle, IoList], []}
                          | RootST],
	    erlang:raise(Class, Reason, StackTrace)
    end.


-spec erts_internal:get_dflags() -> {erts_dflags, integer(), integer(),
                                     integer(), integer(), integer()}.
get_dflags() ->
    erlang:nif_error(undefined).

-spec erts_internal:get_creation() -> pos_integer().
get_creation() ->
    erlang:nif_error(undefined).

-spec erts_internal:new_connection(Node) -> ConnId when
      Node :: atom(),
      ConnId :: {integer(), erlang:dist_handle()}.
new_connection(_Node) ->
    erlang:nif_error(undefined).

-spec erts_internal:abort_pending_connection(Node, ConnId) -> boolean() when
      Node :: atom(),
      ConnId :: {integer(), erlang:dist_handle()}.
abort_pending_connection(_Node, _ConnId) ->
    erlang:nif_error(undefined).

%% Scheduler wall time

-spec erts_internal:system_flag_scheduler_wall_time(Enable) -> boolean() when
      Enable :: boolean().

system_flag_scheduler_wall_time(Bool) ->
    kernel_refc:scheduler_wall_time(Bool).


-spec erts_internal:await_sched_wall_time_modifications(Ref, Result) -> boolean() when
      Ref :: reference(),
      Result :: boolean().

-spec erts_internal:scheduler_wall_time(Enable) -> boolean() when
      Enable :: boolean().

scheduler_wall_time(_Enable) ->
    erlang:nif_error(undefined).

await_sched_wall_time_modifications(Ref, Result) ->
    sched_wall_time(Ref, erlang:system_info(schedulers)),
    Result.

-spec erts_internal:gather_sched_wall_time_result(Ref) -> [{pos_integer(),
						     non_neg_integer(),
						     non_neg_integer()}] when
      Ref :: reference().

gather_sched_wall_time_result(Ref) when erlang:is_reference(Ref) ->
    sched_wall_time(Ref, erlang:system_info(schedulers), []).

sched_wall_time(_Ref, 0) ->
    ok;
sched_wall_time(Ref, N) ->
    receive Ref -> sched_wall_time(Ref, N-1) end.

sched_wall_time(_Ref, 0, Acc) ->
    Acc;
sched_wall_time(Ref, N, undefined) ->
    receive {Ref, _} -> sched_wall_time(Ref, N-1, undefined) end;
sched_wall_time(Ref, N, Acc) ->
    receive
	{Ref, undefined} -> sched_wall_time(Ref, N-1, undefined);
	{Ref, SWTL} when erlang:is_list(SWTL) -> sched_wall_time(Ref, N-1, Acc ++ SWTL);
	{Ref, SWT} -> sched_wall_time(Ref, N-1, [SWT|Acc])
    end.

-spec erts_internal:group_leader(GL, Pid) -> true | false | badarg when
      GL :: pid(),
      Pid :: pid().

group_leader(_GL, _Pid) ->
    erlang:nif_error(undefined).

-spec erts_internal:group_leader(GL, Pid, Ref) -> ok when
      GL :: pid(),
      Pid :: pid(),
      Ref :: reference().

group_leader(_GL, _Pid, _Ref) ->
    erlang:nif_error(undefined).

-spec erts_internal:is_process_alive(Pid, Ref) -> 'ok' when
      Pid :: pid(),
      Ref :: reference().

is_process_alive(_Pid, _Ref) ->
    erlang:nif_error(undefined).    

-spec erts_internal:is_process_alive(Pid) -> boolean() when
      Pid :: pid().

is_process_alive(Pid) ->
    Ref = make_ref(),
    erts_internal:is_process_alive(Pid, Ref),
    receive
        {Ref, Res} ->
            Res
    end.

-spec gather_alloc_histograms({Type, SchedId, HistWidth, HistStart, Ref}) -> MsgCount when
    Type :: atom(),
    SchedId :: non_neg_integer(),
    HistWidth :: non_neg_integer(),
    HistStart :: non_neg_integer(),
    Ref :: reference(),
    MsgCount :: non_neg_integer().

gather_alloc_histograms(_) ->
    erlang:nif_error(undef).

-spec gather_carrier_info({Type, SchedId, HistWidth, HistStart, Ref}) -> MsgCount when
    Type :: atom(),
    SchedId :: non_neg_integer(),
    HistWidth :: non_neg_integer(),
    HistStart :: non_neg_integer(),
    Ref :: reference(),
    MsgCount :: non_neg_integer().

gather_carrier_info(_) ->
    erlang:nif_error(undef).

-spec suspend_process(Suspendee, OptList) -> Result when
      Result :: boolean() | 'badarg' | reference(),
      Suspendee :: pid(),
      OptList :: [Opt],
      Opt :: unless_suspending | asynchronous | {asynchronous, term()}.

suspend_process(_Suspendee, _OptList) ->
    erlang:nif_error(undefined).

%% process_display/2
-spec process_display(Pid, Type) -> 'true' | 'badarg' | reference() when
      Pid :: pid(),
      Type :: backtrace.
process_display(_Pid, _Type) ->
    erlang:nif_error(undefined).

%% process_flag/3
-spec process_flag(Pid, Flag, Value) -> OldValue | 'badarg' | reference() when
      Pid :: pid(),
      Flag :: save_calls,
      Value :: non_neg_integer(),
      OldValue :: non_neg_integer().
process_flag(_Pid, _Flag, _Value) ->
    erlang:nif_error(undefined).

-spec create_dist_channel(Node, DistCtrlr, {Flags, Cr}) -> Result when
      Node :: atom(),
      DistCtrlr :: port() | pid(),
      Flags :: integer(),
      Cr :: pos_integer(),
      Result :: {'ok', erlang:dist_handle()}
              | {'message', reference()}
              | 'badarg'
              | 'system_limit'.
                                 
create_dist_channel(_Node, _DistCtrlr, _Tpl) ->
    erlang:nif_error(undefined).

-spec erase_persistent_terms() -> 'ok'.
erase_persistent_terms() ->
    erlang:nif_error(undefined).

-spec atomics_new(pos_integer(), pos_integer()) -> reference().
atomics_new(_Arity, _EncOpts) ->
    erlang:nif_error(undef).

-spec counters_new(pos_integer()) -> reference().
counters_new(_Size) ->
    erlang:nif_error(undef).

-spec counters_get(reference(), pos_integer()) -> integer().
counters_get(_Ref, _Ix) ->
    erlang:nif_error(undef).

-spec counters_add(reference(), pos_integer(), integer()) -> ok.
counters_add(_Ref, _Ix, _Incr) ->
    erlang:nif_error(undef).

-spec counters_put(reference(), pos_integer(), integer()) -> ok.
counters_put(_Ref, _Ix, _Value) ->
    erlang:nif_error(undef).

-spec counters_info(reference()) -> #{}.
counters_info(_Ref) ->
    erlang:nif_error(undef).

-spec spawn_system_process(Mod, Func, Args) -> pid() when
    Mod :: atom(),
    Func :: atom(),
    Args :: list().
spawn_system_process(_Mod, _Func, _Args) ->
    erlang:nif_error(undefined).


%%
%% ETS info internals...
%%

-spec ets_lookup_binary_info(Tab, Key) -> BinInfo when
      Tab :: ets:table(),
      Key :: term(),
      BinInfo :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}].

ets_lookup_binary_info(_Tab, _Key) ->
    erlang:nif_error(undef).

-spec ets_super_user(Bool) -> 'ok' when
      Bool :: boolean().

ets_super_user(_Bool) ->
    erlang:nif_error(undef).

-spec ets_raw_first(Tab) -> term() when
      Tab :: ets:table().

ets_raw_first(_Tab) ->
    erlang:nif_error(undef).
    
-spec ets_raw_next(Tab, Key) -> term() when
      Tab :: ets:table(),
      Key :: term().

ets_raw_next(_Tab, _Key) ->
    erlang:nif_error(undef).

-spec ets_info_binary(Tab) -> BinInfo when
      Tab :: ets:table(),
      BinInfo :: [{non_neg_integer(), non_neg_integer(), non_neg_integer()}].

ets_info_binary(Tab) ->
    try
        erts_internal:ets_super_user(true),
        ets:safe_fixtable(Tab, true),
        ets_info_binary_iter(Tab, erts_internal:ets_raw_first(Tab), [])
    catch
        C:R:S ->
            ets_info_binary_error(Tab, C, R, S)
    after
        ets:safe_fixtable(Tab, false),
        erts_internal:ets_super_user(false)
    end.
    
ets_info_binary_error(Tab, C, R, []) ->
    erlang:raise(C, R, [{ets, info, [Tab, binary], []}]);
ets_info_binary_error(Tab, C, R, [SF|SFs]) when
      element(1, SF) == erts_internal,
      element(2, SF) == ets_info_binary ->
    erlang:raise(C, R, [{ets, info, [Tab, binary], []}|SFs]);
ets_info_binary_error(Tab, C, R, [_SF|SFs]) ->
    ets_info_binary_error(Tab, C, R, SFs).

ets_info_binary_iter(_Tab, '$end_of_table', Acc) ->
    Acc;
ets_info_binary_iter(Tab, Key, Acc) ->
    NewAcc = case erts_internal:ets_lookup_binary_info(Tab, Key) of
                 [] -> Acc;
                 [BI] -> [BI|Acc];
                 [_|_] = BIL -> BIL ++ Acc
             end,
    ets_info_binary_iter(Tab, erts_internal:ets_raw_next(Tab, Key), NewAcc).

-spec get_internal_state_blocked(Arg :: term()) -> term().

get_internal_state_blocked(Arg) ->
    erlang:system_flag(multi_scheduling, block),
    Result = try
                 erts_debug:get_internal_state({Arg,
                                                blocked})
             after
                 erlang:system_flag(multi_scheduling, unblock)
             end,
    Result.

-spec spawn_request(Module, Function, Args, Opts) -> Res when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Opts :: [term()],
      Res :: reference() | 'badarg' | 'badopt'.

spawn_request(_Module, _Function, _Args, _Opts) ->
    erlang:nif_error(undef).

-spec spawn_init({Module, Function, Args}) -> Res when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Res :: term().

spawn_init({M, F, A}) ->
    apply(M, F, A).

-spec dist_spawn_request(Node, MFA, Opts, spawn_request) -> Res when
      Node :: node(),
      MFA :: {Module, Function, Args},
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Opts :: [term()],
      Res :: reference() | 'badarg';
                        (Node, MFA, Opts, spawn_opt) -> Res when
      Node :: node(),
      MFA :: {Module, Function, Args},
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Opts :: [term()],
      Res :: {reference(), boolean()} | 'badarg'.

dist_spawn_request(_Node, _MFA, _Opts, _Type) ->
    erlang:nif_error(undef).

-spec dist_spawn_init(MFA) -> Res when
      MFA :: {Module, Function, non_neg_integer()},
      Module :: module(),
      Function :: atom(),
      Res :: term().

dist_spawn_init(MFA) ->
    %%
    %% The argument list is passed as a message
    %% to the newly created process. This since
    %% it might be large and require a substantial
    %% amount of work to decode. This way we put
    %% this work on the newly created process
    %% (which can execute in parallel with all
    %% other tasks) instead of on the distribution
    %% channel code which is a bottleneck in the
    %% system.
    %% 
    %% erl_create_process() ensures that the
    %% argument list to use in apply is
    %% guaranteed to be the first message in the
    %% message queue.
    %%
    {M, F, _NoA} = MFA,
    receive
        A ->
            erlang:apply(M, F, A)
    end.

%%
%% Failed distributed spawn(), spawn_link(), spawn_monitor(), spawn_opt()
%% spawns a dummy process executing the crasher/6 function...
%%

crasher(Node,Mod,Fun,Args,[],Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w on ~w **~n",
			     [Mod,Fun,Args,Node]),
    erlang:exit(Reason);
crasher(Node,Mod,Fun,Args,Opts,Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w (~w) on ~w **~n",
			     [Mod,Fun,Args,Opts,Node]),
    erlang:exit(Reason).

%%
%% Actual BIF for erlang:prepare_loading/2, which decompresses the module when
%% necessary to save us from having to do it in C code.
%%
-spec prepare_loading(Module, Code) -> PreparedCode | {error, Reason} when
      Module :: module(),
      Code :: binary(),
      PreparedCode :: erlang:prepared_code(),
      Reason :: badfile.
prepare_loading(_Module, _Code) ->
    erlang:nif_error(undefined).

-spec beamfile_chunk(Bin, Chunk) -> binary() | undefined when
      Bin :: binary(),
      Chunk :: string().
beamfile_chunk(_Bin, _Chunk) ->
    erlang:nif_error(undefined).

-spec beamfile_module_md5(binary()) -> binary() | undefined.
beamfile_module_md5(_Bin) ->
    erlang:nif_error(undefined).

-spec no_aux_work_threads() -> pos_integer().

no_aux_work_threads() ->
    erlang:nif_error(undefined).

%%
%% Is dynamic node name enabled?
%%
-spec dynamic_node_name() -> boolean().

dynamic_node_name() ->
    case persistent_term:get({?MODULE, dynamic_node_name}, false) of
        false -> false;
        _ -> true
    end.

%%
%% Save whether dynamic node name is enabled or not.
%%
-spec dynamic_node_name(boolean()) -> ok.

dynamic_node_name(true) ->
    persistent_term:put({?MODULE, dynamic_node_name}, true);
dynamic_node_name(false) ->
    case dynamic_node_name() of
        false -> ok;
        _ -> _ = persistent_term:erase({?MODULE, dynamic_node_name}), ok
    end.
