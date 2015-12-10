%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-export([map_to_tuple_keys/1, term_type/1, map_hashmap_children/1]).
-export([open_port/2, port_command/3, port_connect/2, port_close/1,
	 port_control/3, port_call/3, port_info/1, port_info/2]).

-export([system_check/1,
         gather_system_check_result/1]).

-export([request_system_task/3]).

-export([check_process_code/3]).
-export([copy_literals/2]).
-export([purge_module/1]).

-export([flush_monitor_messages/3]).

-export([await_result/1, gather_io_bytes/2]).

-export([time_unit/0, perf_counter_unit/0]).

-export([is_system_process/1]).

-export([await_microstate_accounting_modifications/3,
	 gather_microstate_accounting_result/2]).

-export([trace/3, trace_pattern/3]).

%% Auto import name clash
-export([check_process_code/2]).

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
      Prio :: 'max' | 'high' | 'normal' | 'low',
      Request :: {'garbage_collect', term()}
	       | {'check_process_code', term(), module(), non_neg_integer()},
      Pid :: pid().

request_system_task(_Pid, _Prio, _Request) ->
    erlang:nif_error(undefined).

-define(ERTS_CPC_ALLOW_GC, (1 bsl 0)).
-define(ERTS_CPC_COPY_LITERALS, (1 bsl 1)).

-spec check_process_code(Module, Flags) -> boolean() when
      Module :: module(),
      Flags :: non_neg_integer().
check_process_code(_Module, _Flags) ->
    erlang:nif_error(undefined).

-spec check_process_code(Pid, Module, OptionList) -> CheckResult | async when
      Pid :: pid(),
      Module :: module(),
      RequestId :: term(),
      Option :: {async, RequestId} | {allow_gc, boolean()} | {copy_literals, boolean()},
      OptionList :: [Option],
      CheckResult :: boolean() | aborted.
check_process_code(Pid, Module, OptionList)  ->
    {Async, Flags} = get_cpc_opts(OptionList, sync, ?ERTS_CPC_ALLOW_GC),
    case Async of
	{async, ReqId} ->
	    {priority, Prio} = erlang:process_info(erlang:self(),
						   priority),
	    erts_internal:request_system_task(Pid,
					      Prio,
					      {check_process_code,
					       ReqId,
					       Module,
					       Flags}),
	    async;
	sync ->
	    case Pid == erlang:self() of
		true ->
		    erts_internal:check_process_code(Module, Flags);
		false ->
		    {priority, Prio} = erlang:process_info(erlang:self(),
							   priority),
		    ReqId = erlang:make_ref(),
		    erts_internal:request_system_task(Pid,
						      Prio,
						      {check_process_code,
						       ReqId,
						       Module,
						       Flags}),
		    receive
			{check_process_code, ReqId, CheckResult} ->
			    CheckResult
		    end
	    end
    end.

% gets async and flag opts and verify valid option list
get_cpc_opts([{async, _ReqId} = AsyncTuple | Options], _OldAsync, Flags) ->
    get_cpc_opts(Options, AsyncTuple, Flags);
get_cpc_opts([{allow_gc, AllowGC} | Options], Async, Flags) ->
    get_cpc_opts(Options, Async, cpc_flags(Flags, ?ERTS_CPC_ALLOW_GC, AllowGC));
get_cpc_opts([{copy_literals, CopyLit} | Options], Async, Flags) ->
    get_cpc_opts(Options, Async, cpc_flags(Flags, ?ERTS_CPC_COPY_LITERALS, CopyLit));
get_cpc_opts([], Async, Flags) ->
    {Async, Flags}.

cpc_flags(OldFlags, Bit, true) ->
    OldFlags bor Bit;
cpc_flags(OldFlags, Bit, false) ->
    OldFlags band (bnot Bit).

-spec copy_literals(Module,Bool) -> 'true' | 'false' | 'aborted' when
      Module :: module(),
      Bool :: boolean().
copy_literals(_Mod, _Bool) ->
    erlang:nif_error(undefined).

-spec purge_module(Module) -> boolean() when
      Module :: module().
purge_module(_Module) ->
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

-spec erts_internal:perf_counter_unit() -> pos_integer().

perf_counter_unit() ->
    erlang:nif_error(undefined).

-spec erts_internal:is_system_process(Pid) -> boolean() when
      Pid :: pid().

is_system_process(_Pid) ->
    erlang:nif_error(undefined).

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
      FlagList :: [].
trace(_PidSpec, _How, _FlagList) ->
    erlang:nif_error(undefined).

-type trace_pattern_mfa() ::
      {atom(),atom(),arity() | '_'} | on_load.
-type trace_match_spec() ::
      [{[term()] | '_' ,[term()],[term()]}].

-spec trace_pattern(MFA, MatchSpec, FlagList) -> non_neg_integer() when
      MFA :: trace_pattern_mfa(),
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause,
      FlagList :: [ ].
trace_pattern(_MFA, _MatchSpec, _FlagList) ->
    erlang:nif_error(undefined).
