%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2023. All Rights Reserved.
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
-module(erlang).

-export([apply/2,apply/3,spawn/4,spawn_link/4,
	 spawn_monitor/1,spawn_monitor/2,
	 spawn_monitor/3,spawn_monitor/4,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5,
         spawn_request/1, spawn_request/2,
         spawn_request/3, spawn_request/4, spawn_request/5,
         spawn_request_abandon/1, disconnect_node/1]).
-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2]).
-export([yield/0]).
-export([fun_info/1]).
-export([send_nosuspend/2, send_nosuspend/3]).
-export([localtime_to_universaltime/1]).
-export([suspend_process/1]).
-export([min/2, max/2]).
-export([dmonitor_node/3]).
-export([delay_trap/2]).
-export([set_cookie/1, set_cookie/2, get_cookie/0, get_cookie/1]).
-export([nodes/0, nodes/1, nodes/2]).

-export([integer_to_list/2]).
-export([integer_to_binary/2]).
-export([set_cpu_topology/1, format_cpu_topology/1]).
-export([memory/0, memory/1]).
-export([alloc_info/1, alloc_sizes/1]).

-export([gather_gc_info_result/1]).

-export([dist_ctrl_input_handler/2,
         dist_ctrl_put_data/2,
         dist_ctrl_get_data/1,
         dist_ctrl_get_data_notification/1,
         dist_ctrl_get_opt/2,
         dist_ctrl_set_opt/3,
         dist_get_stat/1]).

-deprecated([{now,0,
              "see the \"Time and Time Correction in Erlang\" "
              "chapter of the ERTS User's Guide for more information"}]).
-deprecated([{phash,2, "use erlang:phash2/2 instead"}]).
-removed([{hash,2,"use erlang:phash2/2 instead"}]).
-removed([{get_stacktrace,0,
           "use the new try/catch syntax for retrieving the "
           "stack backtrace"}]).

%% Get rid of autoimports of spawn to avoid clashes with ourselves.
-compile({no_auto_import,[spawn_link/1]}).
-compile({no_auto_import,[spawn_link/4]}).
-compile({no_auto_import,[spawn_opt/2]}).
-compile({no_auto_import,[spawn_opt/4]}).
-compile({no_auto_import,[spawn_opt/5]}).

%% We must inline these functions so that the stacktrace points to
%% the correct function.
-compile({inline, [badarg_with_info/1,error_with_info/2,
                   error_with_inherited_info/3,badarg_with_cause/2]}).
-compile(no_auto_import_types).

%% Built-in datatypes
-type any() :: any().
-type arity() :: arity().
-type atom() :: atom().
-type binary() :: <<_:_*8>>.
-type bitstring() :: <<_:_*1>>.
-type bool() :: boolean().
-type boolean() :: true | false.
-type byte() :: 0..255.
-type char() :: 0..16#10FFFF.
-type dynamic() :: dynamic().
-type float() :: float().
-type function() :: fun().
-type identifier() :: pid() | port() | reference().
-type integer() :: integer().
-type iodata() :: iolist() | binary().
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).
-type list() :: [any()].
-type list(ContentType) :: [ContentType].
-type map() :: #{ any() => any() }.
-type maybe_improper_list() :: maybe_improper_list(any(), any()).
-type maybe_improper_list(ContentType, TerminationType) :: maybe_improper_list(ContentType, TerminationType).
-type mfa() :: {module(),atom(),arity()}.
-type module() :: atom().
-type neg_integer() :: neg_integer().
-type nil() :: [].
-type no_return() :: none().
-type node() :: atom().
-type non_neg_integer() :: non_neg_integer().
-type none() :: none().
-type nonempty_binary() :: <<_:8, _:_*8>>.
-type nonempty_bitstring() :: <<_:1, _:_*1>>.
-type nonempty_improper_list(ContentType, TerminationType) :: nonempty_improper_list(ContentType, TerminationType).
-type nonempty_list() :: nonempty_list(any()).
-type nonempty_list(ContentType) :: [ContentType, ...].
-type nonempty_maybe_improper_list() :: nonempty_maybe_improper_list(any(), any()).
-type nonempty_maybe_improper_list(ContentType, TerminationType) :: nonempty_maybe_improper_list(ContentType, TerminationType).
-type nonempty_string() :: nonempty_list(char()).
-type number() :: integer() | float().
-type pid() :: pid().
-type port() :: port().
-type pos_integer() :: pos_integer().
-type reference() :: reference().
-type string() :: [char()].
-type term() :: any().
-type timeout() :: 'infinity' | non_neg_integer().
-type tuple() :: tuple().
-export_type([any/0, arity/0, atom/0, binary/0, bitstring/0, bool/0, boolean/0, byte/0,
              char/0, dynamic/0, float/0, function/0, identifier/0, integer/0, iodata/0, iolist/0,
              list/0, list/1, map/0, maybe_improper_list/0, maybe_improper_list/2, mfa/0,
              module/0, neg_integer/0, nil/0, no_return/0, node/0, non_neg_integer/0,
              none/0, nonempty_binary/0, nonempty_bitstring/0, nonempty_improper_list/2,
              nonempty_list/0, nonempty_list/1, nonempty_maybe_improper_list/0,
              nonempty_maybe_improper_list/2, nonempty_string/0, number/0, pid/0,
              port/0, pos_integer/0, reference/0, string/0, term/0, timeout/0,
              tuple/0]).

%% Datatypes that need an erlang: prefix
-export_type([timestamp/0]).
-export_type([time_unit/0]).
-export_type([deprecated_time_unit/0]).
-export_type([spawn_opt_option/0]).
-export_type([priority_level/0]).
-export_type([max_heap_size/0]).
-export_type([message_queue_data/0]).
-export_type([monitor_option/0]).
-export_type([stacktrace/0]).

-type stacktrace_extrainfo() ::
        {line, pos_integer()} |
        {file, unicode:chardata()} |
        {error_info, #{ module => module(), function => atom(), cause => term() }} |
        {atom(), term()}.
-type stacktrace() :: [{module(), atom(), arity() | [term()],
                        [stacktrace_extrainfo()]} |
                       {function(), arity() | [term()], [stacktrace_extrainfo()]}].

-type ext_binary() :: binary().
-type ext_iovec() :: iovec().
-type timestamp() :: {MegaSecs :: non_neg_integer(),
                      Secs :: non_neg_integer(),
                      MicroSecs :: non_neg_integer()}.

-type time_unit() ::
	pos_integer()
      | 'second'
      | 'millisecond'
      | 'microsecond'
      | 'nanosecond'
      | 'native'
      | 'perf_counter'
      | deprecated_time_unit().

%% Deprecated symbolic units...
-type deprecated_time_unit() ::
      'seconds'
      | 'milli_seconds'
      | 'micro_seconds'
      | 'nano_seconds'.

-opaque prepared_code() :: reference().
-export_type([prepared_code/0]).

-opaque nif_resource() :: reference().
-export_type([nif_resource/0]).

-opaque dist_handle() :: atom().
-export_type([dist_handle/0]).

-type iovec() :: [binary()].
-export_type([iovec/0]).

%% Type for the destination of sends.
-export_type([send_destination/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Native code BIF stubs and their types
%% (BIF's actually implemented in this module goes last in the file)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports for all native code stubs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([adler32/1, adler32/2, adler32_combine/3, append_element/2]).
-export([atom_to_binary/1, atom_to_binary/2]).
-export([atom_to_list/1, binary_part/2, binary_part/3]).
-export([binary_to_atom/1, binary_to_atom/2]).
-export([binary_to_existing_atom/1, binary_to_existing_atom/2]).
-export([binary_to_float/1]).
-export([binary_to_integer/1,binary_to_integer/2]).
-export([binary_to_list/1]).
-export([binary_to_list/3, binary_to_term/1, binary_to_term/2]).
-export([bit_size/1, bitstring_to_list/1]).
-export([bump_reductions/1, byte_size/1, call_on_load_function/1]).
-export([cancel_timer/1, cancel_timer/2, ceil/1,
	 check_old_code/1, check_process_code/2,
	 check_process_code/3, crc32/1]).
-export([crc32/2, crc32_combine/3, date/0, decode_packet/3]).
-export([delete_element/2]).
-export([delete_module/1, demonitor/1, demonitor/2, display/1]).
-export([display_string/1, display_string/2, erase/0, erase/1]).
-export([error/1, error/2, error/3, exit/1, exit/2, exit_signal/2, external_size/1]).
-export([external_size/2, finish_after_on_load/2, finish_loading/1, float/1]).
-export([float_to_binary/1, float_to_binary/2,
	 float_to_list/1, float_to_list/2, floor/1]).
-export([fun_info/2, fun_info_mfa/1, fun_to_list/1, function_exported/3]).
-export([garbage_collect/0, garbage_collect/1, garbage_collect/2]).
-export([garbage_collect_message_area/0, get/0, get/1, get_keys/0, get_keys/1]).
-export([get_module_info/1, group_leader/0]).
-export([group_leader/2]).
-export([halt/0, halt/1, halt/2,
	 has_prepared_code_on_load/1, hibernate/3]).
-export([insert_element/3]).
-export([integer_to_binary/1, integer_to_list/1]).
-export([iolist_size/1, iolist_to_binary/1, iolist_to_iovec/1]).
-export([is_alive/0, is_builtin/3, is_map_key/2, is_process_alive/1, length/1, link/1]).
-export([list_to_atom/1, list_to_binary/1]).
-export([list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1]).
-export([list_to_integer/1, list_to_integer/2]).
-export([list_to_pid/1, list_to_port/1, list_to_ref/1, list_to_tuple/1, loaded/0]).
-export([localtime/0, make_ref/0]).
-export([map_size/1, map_get/2, match_spec_test/3, md5/1, md5_final/1]).
-export([md5_init/0, md5_update/2, module_loaded/1, monitor/2, monitor/3]).
-export([monitor_node/2, monitor_node/3, nif_error/1, nif_error/2]).
-export([node/0, node/1, now/0, phash/2, phash2/1, phash2/2]).
-export([pid_to_list/1, port_close/1, port_command/2, port_command/3]).
-export([port_connect/2, port_control/3, port_get_data/1]).
-export([port_set_data/2, port_to_list/1, ports/0]).
-export([posixtime_to_universaltime/1, pre_loaded/0, prepare_loading/2]).
-export([monotonic_time/0, monotonic_time/1]).
-export([system_time/0, system_time/1]).
-export([convert_time_unit/3]).
-export([unique_integer/0, unique_integer/1]).
-export([time_offset/0, time_offset/1, timestamp/0]).
-export([process_display/2]).
-export([process_flag/3, process_info/1, processes/0, purge_module/1]).
-export([put/2, raise/3, read_timer/1, read_timer/2, ref_to_list/1, register/2]).
-export([send_after/3, send_after/4, start_timer/3, start_timer/4]).
-export([registered/0, resume_process/1, round/1, self/0]).
-export([seq_trace/2, seq_trace_print/1, seq_trace_print/2, setnode/2]).
-export([setnode/3, size/1, spawn/3, spawn_link/3, split_binary/2]).
-export([suspend_process/2, system_monitor/0]).
-export([system_monitor/1, system_monitor/2, system_profile/0]).
-export([system_profile/2, throw/1, time/0, trace/3, trace_delivered/1]).
-export([trace_info/2, trunc/1, tuple_size/1, universaltime/0]).
-export([universaltime_to_posixtime/1, unlink/1, unregister/1, whereis/1]).

-export([abs/1, append/2, element/2, get_module_info/2, hd/1,
         is_atom/1, is_binary/1, is_bitstring/1, is_boolean/1,
         is_float/1, is_function/1, is_function/2, is_integer/1,
         is_list/1, is_map/1, is_number/1, is_pid/1, is_port/1, is_record/2,
         is_record/3, is_reference/1, is_tuple/1, load_module/2,
         load_nif/2, localtime_to_universaltime/2, make_fun/3,
         make_tuple/2, make_tuple/3, open_port/2,
         port_call/2, port_call/3, port_info/1, port_info/2, process_flag/2,
         process_info/2, send/2, send/3, seq_trace_info/1,
         setelement/3,
	 statistics/1, subtract/2, system_flag/2,
         term_to_binary/1, term_to_binary/2,
         term_to_iovec/1, term_to_iovec/2,
         tl/1, trace_pattern/2,
         trace_pattern/3, tuple_to_list/1, system_info/1,
         universaltime_to_localtime/1]).
-export([alias/0, alias/1, unalias/1]).
-export([dt_get_tag/0, dt_get_tag_data/0, dt_prepend_vm_tag_data/1, dt_append_vm_tag_data/1,
	 dt_put_tag/1, dt_restore_tag/1, dt_spread_tag/1]).

%% Operators

-export(['=='/2, '=:='/2,
         '/='/2, '=/='/2,
         '=<'/2, '>='/2,
         '<'/2, '>'/2]).

-export(['-'/1, '+'/1,
         '-'/2, '+'/2,
         '/'/2, '*'/2,
         'div'/2, 'rem'/2,
         'bsl'/2, 'bsr'/2,
         'bor'/2, 'band'/2,
         'bxor'/2, 'bnot'/1]).

-export(['and'/2, 'or'/2,
         'xor'/2, 'not'/1]).

-export(['--'/2, '++'/2]).

-export(['!'/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simple native code BIFs
%%% These are here for the types/specs, the real implementation is in the C code.
%%% The first chunk is originally auto-generated from
%%% $ERL_TOP/lib/hipe/cerl/erl_bif_types.erl as released in R15B.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% types

-type fun_info_item() ::
      arity |
      env |
      index |
      name |
      module |
      new_index |
      new_uniq |
      pid |
      type |
      uniq.

-type seq_trace_info_returns() ::
      { 'send' | 'receive' | 'print' | 'timestamp' | 'monotonic_timestamp' | 'strict_monotonic_timestamp', boolean() } |
      { 'label', term() } |
      { 'serial', { non_neg_integer(), non_neg_integer() } } |
      [].

-type system_profile_option() ::
      'exclusive' |
      'runnable_ports' |
      'runnable_procs' |
      'scheduler' |
      'timestamp' |
      'monotonic_timestamp' |
      'strict_monotonic_timestamp'.

-type system_monitor_option() ::
      'busy_port' |
      'busy_dist_port' |
      {'long_gc', non_neg_integer()} |
      {'long_schedule', non_neg_integer()} |
      {'large_heap', non_neg_integer()}.

-type raise_stacktrace() ::
      [{module(), atom(), arity() | [term()]} |
       {function(), arity() | [term()]}].

-type bitstring_list() ::
      maybe_improper_list(byte() | bitstring() | bitstring_list(), bitstring() | []).

-type trace_flag() ::
      all |
      send |
      'receive' |
      procs |
      ports |
      call |
      arity |
      return_to |
      silent |
      running |
      exiting |
      running_procs |
      running_ports |
      garbage_collection |
      timestamp |
      cpu_timestamp |
      monotonic_timestamp |
      strict_monotonic_timestamp |
      set_on_spawn |
      set_on_first_spawn |
      set_on_link |
      set_on_first_link |
      {tracer, pid() | port()} |
      {tracer, module(), term()}.

-type trace_info_item_result() ::
       {traced, global | local | false | undefined} |
       {match_spec, trace_match_spec() | false | undefined} |
       {meta, pid() | port() | false | undefined | []} |
       {meta, module(), term() } |
       {meta_match_spec, trace_match_spec() | false | undefined} |
       {call_count, non_neg_integer() | boolean() | undefined} |
       {call_time | call_memory, [{pid(), non_neg_integer(),
		     non_neg_integer(), non_neg_integer()}] | boolean() | undefined}.

-type trace_info_flag() ::
      send |
      'receive' |
      set_on_spawn |
      call |
      return_to |
      procs |
      set_on_first_spawn |
      set_on_link |
      running |
      garbage_collection |
      timestamp |
      monotonic_timestamp |
      strict_monotonic_timestamp |
      arity.

-type trace_info_return() ::
      undefined |
      {flags, [trace_info_flag()]} |
      {tracer, pid() | port() | []} |
      {tracer, module(), term()} |
      trace_info_item_result() |
      {all, [ trace_info_item_result() ] | false | undefined}.

%% Specs and stubs
%% adler32/1
-spec erlang:adler32(Data) -> non_neg_integer() when
      Data :: iodata().
adler32(_Data) ->
    erlang:nif_error(undefined).

%% adler32/2
-spec erlang:adler32(OldAdler, Data) -> non_neg_integer() when
      OldAdler :: non_neg_integer(),
      Data :: iodata().
adler32(_OldAdler, _Data) ->
    erlang:nif_error(undefined).

%% adler32_combine/3
-spec erlang:adler32_combine(FirstAdler, SecondAdler, SecondSize) -> non_neg_integer() when
      FirstAdler :: non_neg_integer(),
      SecondAdler :: non_neg_integer(),
      SecondSize :: non_neg_integer().
adler32_combine(_FirstAdler, _SecondAdler, _SecondSize) ->
    erlang:nif_error(undefined).

%% append_element/2
-spec erlang:append_element(Tuple1, Term) -> Tuple2 when
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Term :: term().
append_element(_Tuple1, _Term) ->
    erlang:nif_error(undefined).

%% atom_to_binary/1
-spec atom_to_binary(Atom) -> binary() when
      Atom :: atom().
atom_to_binary(Atom) ->
    try
        erlang:atom_to_binary(Atom, utf8)
    catch
        error:Error ->
            error_with_info(Error, [Atom])
    end.

%% atom_to_binary/2
-spec atom_to_binary(Atom, Encoding) -> binary() when
      Atom :: atom(),
      Encoding :: latin1 | unicode | utf8.
atom_to_binary(_Atom, _Encoding) ->
    erlang:nif_error(undefined).

%% atom_to_list/1
-spec atom_to_list(Atom) -> string() when
      Atom :: atom().
atom_to_list(_Atom) ->
    erlang:nif_error(undefined).

%% binary_part/2
%% Shadowed by erl_bif_types: erlang:binary_part/2
-spec binary_part(Subject, PosLen) -> binary() when
      Subject :: binary(),
      PosLen :: {Start :: non_neg_integer(), Length :: integer()}.
binary_part(_Subject, _PosLen) ->
    erlang:nif_error(undefined).

%% binary_part/3
%% Shadowed by erl_bif_types: erlang:binary_part/3
-spec binary_part(Subject, Start, Length) -> binary() when
      Subject :: binary(),
      Start :: non_neg_integer(),
      Length :: integer().
binary_part(_Subject, _Start, _Length) ->
    erlang:nif_error(undefined).

%% binary_to_atom/1
-spec binary_to_atom(Binary) -> atom() when
      Binary :: binary().
binary_to_atom(Binary) ->
    try
        erlang:binary_to_atom(Binary, utf8)
    catch
	error:Error -> error_with_info(Error, [Binary])
    end.

%% binary_to_atom/2
-spec binary_to_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%% binary_to_existing_atom/1
-spec binary_to_existing_atom(Binary) -> atom() when
      Binary :: binary().
binary_to_existing_atom(Binary) ->
    try
        erlang:binary_to_existing_atom(Binary, utf8)
    catch
	error:Error -> error_with_info(Error, [Binary])
    end.

%% binary_to_existing_atom/2
-spec binary_to_existing_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_existing_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%% binary_to_float/1
-spec binary_to_float(Binary) -> float() when
      Binary :: binary().
binary_to_float(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_integer/1
-spec binary_to_integer(Binary) -> integer() when
      Binary :: binary().
binary_to_integer(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_integer/2
-spec binary_to_integer(Binary,Base) -> integer() when
      Binary :: binary(),
      Base :: 2..36.
binary_to_integer(_Binary,_Base) ->
    erlang:nif_error(undefined).

%% binary_to_list/1
-spec binary_to_list(Binary) -> [byte()] when
      Binary :: binary().
binary_to_list(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_list/3
-spec binary_to_list(Binary, Start, Stop) -> [byte()] when
      Binary :: binary(),
      Start :: pos_integer(),
      Stop :: pos_integer().
binary_to_list(_Binary, _Start, _Stop) ->
    erlang:nif_error(undefined).

%% binary_to_term/1
-spec binary_to_term(Binary) -> term() when
      Binary :: ext_binary().
binary_to_term(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_term/2
-spec binary_to_term(Binary, Opts) -> term() | {term(), Used} when
      Binary :: ext_binary(),
      Opt :: safe | used,
      Opts :: [Opt],
      Used :: pos_integer().
binary_to_term(_Binary, _Opts) ->
    erlang:nif_error(undefined).

%% bit_size/1
%% Shadowed by erl_bif_types: erlang:bit_size/1
-spec bit_size(Bitstring) -> non_neg_integer() when
      Bitstring :: bitstring().
bit_size(_Bitstring) ->
    erlang:nif_error(undefined).

%% bitstring_to_list/1
-spec bitstring_to_list(Bitstring) -> [byte() | bitstring()] when
      Bitstring :: bitstring().
bitstring_to_list(_Bitstring) ->
    erlang:nif_error(undefined).

%% bump_reductions/1
-spec erlang:bump_reductions(Reductions) -> true when
      Reductions :: pos_integer().
bump_reductions(_Reductions) ->
    erlang:nif_error(undefined).

%% byte_size/1
%% Shadowed by erl_bif_types: erlang:byte_size/1
-spec byte_size(Bitstring) -> non_neg_integer() when
      Bitstring :: bitstring().
byte_size(_Bitstring) ->
    erlang:nif_error(undefined).

%% call_on_load_function/1
-spec erlang:call_on_load_function(P1) -> term() when
      P1 :: atom().
call_on_load_function(_P1) ->
    erlang:nif_error(undefined).

%% cancel_timer/1
-spec erlang:cancel_timer(TimerRef) -> Result when
      TimerRef :: reference(),
      Time :: non_neg_integer(),
      Result :: Time | false.

cancel_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% cancel_timer/2
-spec erlang:cancel_timer(TimerRef, Options) -> Result | ok when
      TimerRef :: reference(),
      Async :: boolean(),
      Info :: boolean(),
      Option :: {async, Async} | {info, Info},
      Options :: [Option],
      Time :: non_neg_integer(),
      Result :: Time | false.

cancel_timer(_TimerRef, _Options) ->
    erlang:nif_error(undefined).

%% ceil/1
%% Shadowed by erl_bif_types: erlang:ceil/1
-spec ceil(Number) -> integer() when
      Number :: number().
ceil(_) ->
    erlang:nif_error(undef).

%% check_old_code/1
-spec check_old_code(Module) -> boolean() when
      Module :: module().
check_old_code(_Module) ->
    erlang:nif_error(undefined).

%% check_process_code/2
-spec check_process_code(Pid, Module) -> CheckResult when
      Pid :: pid(),
      Module :: module(),
      CheckResult :: boolean().
check_process_code(Pid, Module) ->
    try
	erts_internal:check_process_code(Pid, Module, [{allow_gc, true}])
    catch
	error:Error -> error_with_info(Error, [Pid, Module])
    end.

%% check_process_code/3
-spec check_process_code(Pid, Module, OptionList) -> CheckResult | async when
      Pid :: pid(),
      Module :: module(),
      RequestId :: term(),
      Option :: {async, RequestId} | {allow_gc, boolean()},
      OptionList :: [Option],
      CheckResult :: boolean() | aborted.
check_process_code(Pid, Module, OptionList)  ->
    try
	erts_internal:check_process_code(Pid, Module, OptionList)
    catch
        error:bad_option ->
            badarg_with_cause([Pid, Module, OptionList], bad_option);
        error:_ ->
            badarg_with_info([Pid, Module, OptionList])
    end.

%% crc32/1
-spec erlang:crc32(Data) -> non_neg_integer() when
      Data :: iodata().
crc32(_Data) ->
    erlang:nif_error(undefined).

%% crc32/2
-spec erlang:crc32(OldCrc, Data) -> non_neg_integer() when
      OldCrc :: non_neg_integer(),
      Data :: iodata().
crc32(_OldCrc, _Data) ->
    erlang:nif_error(undefined).

%% crc32_combine/3
-spec erlang:crc32_combine(FirstCrc, SecondCrc, SecondSize) -> non_neg_integer() when
      FirstCrc :: non_neg_integer(),
      SecondCrc :: non_neg_integer(),
      SecondSize :: non_neg_integer().
crc32_combine(_FirstCrc, _SecondCrc, _SecondSize) ->
    erlang:nif_error(undefined).

%% date/0
-spec date() -> Date when
      Date :: calendar:date().
date() ->
    erlang:nif_error(undefined).

%% decode_packet/3
-spec erlang:decode_packet(Type, Bin, Options) ->
                                  {ok, Packet, Rest} |
                                  {more, Length} |
                                  {error, Reason} when
      Type :: 'raw' | 0 | 1 | 2 | 4 | 'asn1' | 'cdr' | 'sunrm' | 'fcgi'
            | 'tpkt' | 'line' | 'http' | 'http_bin' | 'httph' | 'httph_bin',
      Bin :: binary(),
      Options :: [Opt],
      Opt :: {packet_size, non_neg_integer()}
           | {line_length, non_neg_integer()},
      Packet :: binary() | HttpPacket,
      Rest :: binary(),
      Length :: non_neg_integer() | undefined,
      Reason :: term(),
      HttpPacket :: HttpRequest
                  | HttpResponse
                  | HttpHeader
                  | 'http_eoh'
                  | HttpError,
      HttpRequest :: {'http_request', HttpMethod, HttpUri, HttpVersion},
      HttpResponse :: {'http_response', HttpVersion, integer(), HttpString},
      HttpHeader :: {'http_header',
                     integer(),
                     HttpField,
                     UnmodifiedField :: HttpString,
                     Value :: HttpString},
      HttpError :: {'http_error', HttpString},
      HttpMethod :: 'OPTIONS' | 'GET' | 'HEAD' | 'POST' | 'PUT' | 'DELETE'
                  | 'TRACE' | HttpString,
      HttpUri :: '*'
               | { 'absoluteURI',
                   'http' | 'https',
                   Host :: HttpString,
                   Port :: inet:port_number() | 'undefined',
                   Path :: HttpString}
               | {'scheme', Scheme :: HttpString, HttpString}
               | {'abs_path', HttpString}
               | HttpString,
      HttpVersion :: {Major :: non_neg_integer(), Minor :: non_neg_integer()},
      HttpField :: 'Cache-Control'
                 | 'Connection'
                 | 'Date'
                 | 'Pragma'
                 | 'Transfer-Encoding'
                 | 'Upgrade'
                 | 'Via'
                 | 'Accept'
                 | 'Accept-Charset'
                 | 'Accept-Encoding'
                 | 'Accept-Language'
                 | 'Authorization'
                 | 'From'
                 | 'Host'
                 | 'If-Modified-Since'
                 | 'If-Match'
                 | 'If-None-Match'
                 | 'If-Range'
                 | 'If-Unmodified-Since'
                 | 'Max-Forwards'
                 | 'Proxy-Authorization'
                 | 'Range'
                 | 'Referer'
                 | 'User-Agent'
                 | 'Age'
                 | 'Location'
                 | 'Proxy-Authenticate'
                 | 'Public'
                 | 'Retry-After'
                 | 'Server'
                 | 'Vary'
                 | 'Warning'
                 |'Www-Authenticate'
                 | 'Allow'
                 | 'Content-Base'
                 | 'Content-Encoding'
                 | 'Content-Language'
                 | 'Content-Length'
                 | 'Content-Location'
                 | 'Content-Md5'
                 | 'Content-Range'
                 | 'Content-Type'
                 | 'Etag'
                 | 'Expires'
                 | 'Last-Modified'
                 | 'Accept-Ranges'
                 | 'Set-Cookie'
                 | 'Set-Cookie2'
                 | 'X-Forwarded-For'
                 | 'Cookie'
                 | 'Keep-Alive'
                 | 'Proxy-Connection'
                 | HttpString,
      HttpString :: string() | binary().
decode_packet(_Type, _Bin, _Options) ->
    erlang:nif_error(undefined).

%% delete_element/2
-spec erlang:delete_element(Index, Tuple1) -> Tuple2 when
      Index  :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple().
delete_element(_Index, _Tuple1) ->
    erlang:nif_error(undefined).

%% delete_module/1
-spec delete_module(Module) -> true | undefined when
      Module :: module().
delete_module(_Module) ->
    erlang:nif_error(undefined).

%% demonitor/1
-spec demonitor(MonitorRef) -> true when
      MonitorRef :: reference().
demonitor(_MonitorRef) ->
    erlang:nif_error(undefined).

%% demonitor/2
-spec demonitor(MonitorRef, OptionList) -> boolean() when
      MonitorRef :: reference(),
      OptionList :: [Option],
      Option :: flush | info.
demonitor(_MonitorRef, _OptionList) ->
    erlang:nif_error(undefined).

-spec alias() -> Alias when
      Alias :: reference().

alias() ->
    erlang:alias([]).

-spec alias(Opts) -> Alias when
      Alias :: reference(),
      Opts :: ['explicit_unalias' | 'reply'].

alias(_Opts) ->
    erlang:nif_error(undefined).

-spec unalias(Alias) -> boolean() when
      Alias :: reference().

unalias(_Alias) ->
    erlang:nif_error(undefined).

%% display/1
-spec erlang:display(Term) -> true when
      Term :: term().
display(_Term) ->
    erlang:nif_error(undefined).

%% display_string/1
-spec erlang:display_string(P1) -> true when
      P1 :: string() | binary().
display_string(String) ->
    try erlang:display_string(stderr, String)
    catch error:badarg:ST ->
            [{erlang, display_string, _, [ErrorInfo]}|_] = ST,
            erlang:error(badarg, [String], [ErrorInfo])
    end.

%% display_string/2
-spec erlang:display_string(Device, P1) -> true when
      Device :: stdin | stdout | stderr,
      P1 :: string().
display_string(_Stream,_P1) ->
    erlang:nif_error(undefined).

%% dt_append_vm_tag_data/1
-spec erlang:dt_append_vm_tag_data(IoData) -> IoDataRet when
      IoData :: iodata(),
      IoDataRet :: iodata().
dt_append_vm_tag_data(_IoData) ->
    erlang:nif_error(undefined).

%% dt_get_tag/0
-spec erlang:dt_get_tag() -> binary() | undefined.
dt_get_tag() ->
    erlang:nif_error(undefined).

%%  dt_get_tag_data/0
-spec erlang:dt_get_tag_data() -> binary() | undefined.
dt_get_tag_data() ->
    erlang:nif_error(undefined).

%% dt_prepend_vm_tag_data/1
-spec erlang:dt_prepend_vm_tag_data(IoData) -> IoDataRet when
      IoData :: iodata(),
      IoDataRet :: iodata().
dt_prepend_vm_tag_data(_IoData) ->
    erlang:nif_error(undefined).

%% dt_put_tag/1
-spec erlang:dt_put_tag(IoData) -> binary() | undefined when
      IoData :: iodata().
dt_put_tag(_IoData) ->
    erlang:nif_error(undefined).

%% dt_restore_tag/1
-spec erlang:dt_restore_tag(TagData) -> true when
      TagData :: term().
dt_restore_tag(_TagData) ->
    erlang:nif_error(undefined).
    
%% dt_spread_tag/1
-spec erlang:dt_spread_tag(boolean()) -> TagData when
      TagData :: term().
dt_spread_tag(_Bool) ->   
    erlang:nif_error(undefined).

%% erase/0
-spec erase() -> [{Key, Val}] when
      Key :: term(),
      Val :: term().
erase() ->
    erlang:nif_error(undefined).

%% erase/1
-spec erase(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
erase(_Key) ->
    erlang:nif_error(undefined).

%% error/1
%% Shadowed by erl_bif_types: erlang:error/1
-spec error(Reason) -> no_return() when
      Reason :: term().
error(_Reason) ->
    erlang:nif_error(undefined).

%% error/2
%% Shadowed by erl_bif_types: erlang:error/2
-spec error(Reason, Args) -> no_return() when
      Reason :: term(),
      Args :: [term()] | none.
error(_Reason, _Args) ->
    erlang:nif_error(undefined).

%% error/3
%% Shadowed by erl_bif_types: erlang:error/3
-spec error(Reason, Args, Options) -> no_return() when
      Reason :: term(),
      Args :: [term()] | none,
      Options :: [Option],
      Option :: {'error_info', ErrorInfoMap},
      ErrorInfoMap :: #{'cause' => term(),
                        'module' => module(),
                        'function' => atom()}.
error(_Reason, _Args, _Options) ->
    erlang:nif_error(undefined).

%% exit/1
%% Shadowed by erl_bif_types: erlang:exit/1
-spec exit(Reason) -> no_return() when
      Reason :: term().
exit(_Reason) ->
    erlang:nif_error(undefined).

%% exit/2
-spec exit(Pid, Reason) -> true when
      Pid :: pid() | port(),
      Reason :: term().
exit(_Pid, _Reason) ->
    erlang:nif_error(undefined).

%% exit_signal/2
-spec erlang:exit_signal(Pid, Reason) -> true when
      Pid :: pid() | port(),
      Reason :: term().
exit_signal(_Pid, _Reason) ->
    erlang:nif_error(undefined).

%% external_size/1
-spec erlang:external_size(Term) -> non_neg_integer() when
      Term :: term().
external_size(_Term) ->
    erlang:nif_error(undefined).

%% external_size/2
-spec erlang:external_size(Term, Options) -> non_neg_integer() when
      Term :: term(),
      Options :: [compressed |
         {compressed, Level :: 0..9} |
         deterministic |
         {minor_version, Version :: 0..2} |
         local ].
external_size(_Term, _Options) ->
    erlang:nif_error(undefined).

%% finish_loading/2
-spec erlang:finish_loading(PreparedCodeList) -> ok | Error when
      PreparedCodeList :: [PreparedCode],
      PreparedCode :: prepared_code(),
      ModuleList :: [module()],
      Error :: {not_purged,ModuleList} | {on_load,ModuleList}.
finish_loading(_List) ->
    erlang:nif_error(undefined).

%% finish_after_on_load/2
-spec erlang:finish_after_on_load(P1, P2) -> true when
      P1 :: atom(),
      P2 :: boolean().
finish_after_on_load(_P1, _P2) ->
    erlang:nif_error(undefined).

%% float/1
%% Shadowed by erl_bif_types: erlang:float/1
-spec float(Number) -> float() when
      Number :: number().
float(_Number) ->
    erlang:nif_error(undefined).

%% float_to_binary/1
-spec float_to_binary(Float) -> binary() when
      Float :: float().
float_to_binary(_Float) ->
    erlang:nif_error(undefined).

%% float_to_binary/2
-spec float_to_binary(Float, Options) -> binary() when
      Float :: float(),
      Options :: [Option],
      Option  :: {decimals, Decimals :: 0..253} |
                 {scientific, Decimals :: 0..249} |
                 compact |
                 short.
float_to_binary(_Float, _Options) ->
    erlang:nif_error(undefined).

%% float_to_list/1
-spec float_to_list(Float) -> string() when
      Float :: float().
float_to_list(_Float) ->
    erlang:nif_error(undefined).

%% float_to_list/2
-spec float_to_list(Float, Options) -> string() when
      Float :: float(),
      Options :: [Option],
      Option  :: {decimals, Decimals :: 0..253} |
                 {scientific, Decimals :: 0..249} |
                 compact |
                 short.
float_to_list(_Float, _Options) ->
    erlang:nif_error(undefined).

%% floor/1
%% Shadowed by erl_bif_types: erlang:floor/1
-spec floor(Number) -> integer() when
      Number :: number().
floor(_) ->
    erlang:nif_error(undef).

%% fun_info/2
-spec erlang:fun_info(Fun, Item) -> {Item, Info} when
      Fun :: function(),
      Item :: fun_info_item(),
      Info :: term().
fun_info(_Fun, _Item) ->
    erlang:nif_error(undefined).

%% fun_info_mfa/1
-spec erlang:fun_info_mfa(Fun) -> {Mod, Name, Arity} when
      Fun :: function(),
      Mod :: atom(),
      Name :: atom(),
      Arity :: non_neg_integer().
fun_info_mfa(_Fun) ->
    erlang:nif_error(undefined).

%% fun_to_list/1
-spec erlang:fun_to_list(Fun) -> String :: string() when
      Fun :: function().
fun_to_list(_Fun) ->
    erlang:nif_error(undefined).

%% function_exported/3
-spec erlang:function_exported(Module, Function, Arity) -> boolean() when
      Module :: module(),
      Function :: atom(),
      Arity :: arity().
function_exported(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

%% garbage_collect/0
-spec garbage_collect() -> true.
garbage_collect() ->
    erts_internal:garbage_collect(major).

%% garbage_collect/1
-spec garbage_collect(Pid) -> GCResult when
      Pid :: pid(),
      GCResult :: boolean().
garbage_collect(Pid) ->
    try
	erlang:garbage_collect(Pid, [])
    catch
	error:Error -> error_with_info(Error, [Pid])
    end.

-record(gcopt, {
    async = sync :: sync | {async, _},
    type = major % default major, can also be minor
    }).

%% garbage_collect/2
-spec garbage_collect(Pid, OptionList) -> GCResult | async when
      Pid :: pid(),
      RequestId :: term(),
      Option :: {async, RequestId} | {type, 'major' | 'minor'},
      OptionList :: [Option],
      GCResult :: boolean().
garbage_collect(Pid, OptionList)  ->
    try
	GcOpts = get_gc_opts(OptionList, #gcopt{}),
	case GcOpts#gcopt.async of
	    {async, ReqId} ->
		erts_internal:request_system_task(
                    Pid, inherit, {garbage_collect, ReqId, GcOpts#gcopt.type}),
		async;
	    sync ->
		case Pid == erlang:self() of
		    true ->
			erts_internal:garbage_collect(GcOpts#gcopt.type);
		    false ->
			ReqId = erlang:make_ref(),
			erts_internal:request_system_task(
                            Pid, inherit,
                            {garbage_collect, ReqId, GcOpts#gcopt.type}),
			receive
			    {garbage_collect, ReqId, GCResult} ->
				GCResult
			end
		end
	end
    catch
        throw:bad_option -> badarg_with_cause([Pid, OptionList], bad_option);
	error:_ -> badarg_with_info([Pid, OptionList])
    end.

%% gets async opt and verify valid option list
get_gc_opts([{async, _ReqId} = AsyncTuple | Options], GcOpt = #gcopt{}) ->
    get_gc_opts(Options, GcOpt#gcopt{ async = AsyncTuple });
get_gc_opts([{type, T} | Options], GcOpt = #gcopt{}) ->
    get_gc_opts(Options, GcOpt#gcopt{ type = T });
get_gc_opts([], GcOpt) ->
    GcOpt;
get_gc_opts(_, _) ->
    erlang:throw(bad_option).

%% garbage_collect_message_area/0
-spec erlang:garbage_collect_message_area() -> boolean().
garbage_collect_message_area() ->
    erlang:nif_error(undefined).

%% get/0
-spec get() -> [{Key, Val}] when
      Key :: term(),
      Val :: term().
get() ->
    erlang:nif_error(undefined).

%% get/1
-spec get(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
get(_Key) ->
    erlang:nif_error(undefined).

%% get_keys/0
-spec get_keys() -> [Key] when
      Key :: term().
get_keys() ->
    erlang:nif_error(undefined).

%% get_keys/1
-spec get_keys(Val) -> [Key] when
      Val :: term(),
      Key :: term().
get_keys(_Val) ->
    erlang:nif_error(undefined).

%% get_module_info/1
-spec erlang:get_module_info(Module) -> [{Item, term()}] when
      Item :: module | exports | attributes | compile | native | md5,
      Module :: atom().
get_module_info(_Module) ->
    erlang:nif_error(undefined).

%% group_leader/0
-spec group_leader() -> pid().
group_leader() ->
    erlang:nif_error(undefined).

%% group_leader/2
-spec group_leader(GroupLeader, Pid) -> true when
      GroupLeader :: pid(),
      Pid :: pid().
group_leader(GroupLeader, Pid) ->
    case case erts_internal:group_leader(GroupLeader, Pid) of
             false ->
                 Ref = erlang:make_ref(),
                 erts_internal:group_leader(GroupLeader,
                                            Pid,
                                            Ref),
                 receive {Ref, MsgRes} -> MsgRes end;
             Res ->
                 Res
         end of
        true -> true;
        Error -> error_with_info(Error, [GroupLeader, Pid])
    end.

%% halt/0
%% Shadowed by erl_bif_types: erlang:halt/0
-spec halt() -> no_return().
halt() ->
    erlang:halt(0, []).

%% halt/1
%% Shadowed by erl_bif_types: erlang:halt/1
-spec halt(Status :: non_neg_integer()) ->
          no_return();
          (Abort :: abort) ->
          no_return();
          (CrashDumpSlogan :: string()) ->
          no_return().

-dialyzer({no_return, halt/1}).
halt(Status) ->
    try
        erlang:halt(Status, [])
    catch
	error:Error -> error_with_info(Error, [Status])
    end.

%% halt/2
%% Shadowed by erl_bif_types: erlang:halt/2
-type halt_options() ::
        [{flush, boolean()}].

-spec halt(Status :: non_neg_integer(), Options :: halt_options()) ->
          no_return();
          (Abort :: abort, Options :: halt_options()) ->
          no_return();
          (CrashDumpSlogan :: string(), Options :: halt_options()) ->
          no_return().

-dialyzer({no_return, halt/2}).
halt(_, _) ->
    erlang:nif_error(undefined).

%% has_prepared_code_on_load/1
-spec erlang:has_prepared_code_on_load(PreparedCode) -> boolean() when
      PreparedCode :: prepared_code().
has_prepared_code_on_load(_PreparedCode) ->
    erlang:nif_error(undefined).

%% hibernate/3
-spec erlang:hibernate(Module, Function, Args) -> no_return() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
hibernate(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% insert_element/3
-spec erlang:insert_element(Index, Tuple1, Term) -> Tuple2 when
      Index  :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Term   :: term().
insert_element(_Index, _Tuple1, _Term) ->
    erlang:nif_error(undefined).

%% integer_to_binary/1
-spec integer_to_binary(Integer) -> binary() when
      Integer :: integer().
integer_to_binary(_Integer) ->
    erlang:nif_error(undefined).

%% integer_to_list/1
-spec integer_to_list(Integer) -> string() when
      Integer :: integer().
integer_to_list(_Integer) ->
    erlang:nif_error(undefined).

%% iolist_size/1
-spec iolist_size(Item) -> non_neg_integer() when
      Item :: iolist() | binary().
iolist_size(_Item) ->
    erlang:nif_error(undefined).

%% iolist_to_binary/1
-spec iolist_to_binary(IoListOrBinary) -> binary() when
      IoListOrBinary :: iolist() | binary().
iolist_to_binary(_IoListOrBinary) ->
    erlang:nif_error(undefined).

%% iolist_to_iovec/1
-spec erlang:iolist_to_iovec(IoListOrBinary) -> iovec() when
      IoListOrBinary :: iolist() | binary().
iolist_to_iovec(_IoListOrBinary) ->
    erlang:nif_error(undefined).

%% is_alive/0
-spec is_alive() -> boolean().
is_alive() ->
    erlang:node() =/= nonode@nohost orelse erts_internal:dynamic_node_name().

%% is_builtin/3
-spec erlang:is_builtin(Module, Function, Arity) -> boolean() when
      Module :: module(),
      Function :: atom(),
      Arity :: arity().
is_builtin(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_map_key/2
-spec is_map_key(Key, Map) -> boolean() when
    Key :: term(),
    Map :: map().
is_map_key(_,_) ->
    erlang:nif_error(undef).

%% is_process_alive/1
-spec is_process_alive(Pid) -> boolean() when
      Pid :: pid().
is_process_alive(_Pid) ->
    erlang:nif_error(undefined).

%% length/1
%% Shadowed by erl_bif_types: erlang:length/1
-spec length(List) -> non_neg_integer() when
      List :: [term()].
length(_List) ->
    erlang:nif_error(undefined).

%% link/1
-spec link(PidOrPort) -> true when
      PidOrPort :: pid() | port().
link(_PidOrPort) ->
    erlang:nif_error(undefined).

%% list_to_atom/1
-spec list_to_atom(String) -> atom() when
      String :: string().
list_to_atom(_String) ->
    erlang:nif_error(undefined).

%% list_to_binary/1
-spec list_to_binary(IoList) -> binary() when
      IoList :: iolist().
list_to_binary(_IoList) ->
    erlang:nif_error(undefined).

%% list_to_bitstring/1
-spec list_to_bitstring(BitstringList) -> bitstring() when
      BitstringList :: bitstring_list().
list_to_bitstring(_BitstringList) ->
    erlang:nif_error(undefined).

%% list_to_existing_atom/1
-spec list_to_existing_atom(String) -> atom() when
      String :: string().
list_to_existing_atom(_String) ->
    erlang:nif_error(undefined).

%% list_to_float/1
-spec list_to_float(String) -> float() when
      String :: string().
list_to_float(_String) ->
    erlang:nif_error(undefined).

%% list_to_integer/1
-spec list_to_integer(String) -> integer() when
      String :: string().
list_to_integer(_String) ->
    erlang:nif_error(undefined).

%% list_to_integer/2
-spec list_to_integer(String, Base) -> integer() when
      String :: string(),
      Base :: 2..36.
list_to_integer(_String,_Base) ->
    erlang:nif_error(undefined).

%% list_to_pid/1
-spec list_to_pid(String) -> pid() when
      String :: string().
list_to_pid(_String) ->
    erlang:nif_error(undefined).

%% list_to_port/1
-spec list_to_port(String) -> port() when
      String :: string().
list_to_port(_String) ->
    erlang:nif_error(undefined).
 
%% list_to_ref/1
-spec list_to_ref(String) -> reference() when
      String :: string().
list_to_ref(_String) ->
    erlang:nif_error(undefined).

%% list_to_tuple/1
-spec list_to_tuple(List) -> tuple() when
      List :: [term()].
list_to_tuple(_List) ->
    erlang:nif_error(undefined).

%% loaded/0
-spec erlang:loaded() -> [Module] when
      Module :: module().
loaded() ->
    erlang:nif_error(undefined).

%% localtime/0
-spec erlang:localtime() -> DateTime when
      DateTime :: calendar:datetime().
localtime() ->
    erlang:nif_error(undefined).

%% make_ref/0
-spec make_ref() -> reference().
make_ref() ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:map_size/1
-spec map_size(Map) -> non_neg_integer() when
      Map :: map().
map_size(_Map) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:map_get/2
-spec map_get(Key, Map) -> Value when
      Map :: map(),
      Key :: any(),
      Value :: any().
map_get(_Key, _Map) ->
    erlang:nif_error(undefined).

%% match_spec_test/3
-spec erlang:match_spec_test(MatchAgainst, MatchSpec, Type) -> TestResult when
      MatchAgainst :: [term()] | tuple(),
      MatchSpec :: term(),
      Type :: table | trace,
      TestResult :: {ok, term(), [return_trace], [ {error | warning, string()} ]} | {error, [ {error | warning, string()} ]}.
match_spec_test(_P1, _P2, _P3) ->
    erlang:nif_error(undefined).

%% md5/1
-spec erlang:md5(Data) -> Digest when
      Data :: iodata(),
      Digest :: binary().
md5(_Data) ->
    erlang:nif_error(undefined).

%% md5_final/1
-spec erlang:md5_final(Context) -> Digest when
      Context :: binary(),
      Digest :: binary().
md5_final(_Context) ->
    erlang:nif_error(undefined).

%% md5_init/0
-spec erlang:md5_init() -> Context when
      Context :: binary().
md5_init() ->
    erlang:nif_error(undefined).

%% md5_update/2
-spec erlang:md5_update(Context, Data) -> NewContext when
      Context :: binary(),
      Data :: iodata(),
      NewContext :: binary().
md5_update(_Context, _Data) ->
    erlang:nif_error(undefined).

%% module_loaded/1
-spec module_loaded(Module) -> boolean() when
      Module :: module().
module_loaded(_Module) ->
    erlang:nif_error(undefined).

-type registered_name() :: atom().
-type registered_process_identifier() :: registered_name() | {registered_name(), node()}.
-type monitor_process_identifier() :: pid() | registered_process_identifier().
-type monitor_port_identifier() :: port() | registered_name().
-type monitor_option() :: {'alias', 'explicit_unalias' | 'demonitor' | 'reply_demonitor'}
                        | {'tag', term()}.

%% monitor/2
-spec monitor
      (process, monitor_process_identifier()) -> MonitorRef
	  when MonitorRef :: reference();
      (port, monitor_port_identifier()) -> MonitorRef
	  when MonitorRef :: reference();
      (time_offset, clock_service) -> MonitorRef
	  when MonitorRef :: reference().

monitor(_Type, _Item) ->
    erlang:nif_error(undefined).

%% monitor/3
-spec monitor
      (process, monitor_process_identifier(), [monitor_option()]) -> MonitorRef
	  when MonitorRef :: reference();
      (port, monitor_port_identifier(), [monitor_option()]) -> MonitorRef
	  when MonitorRef :: reference();
      (time_offset, clock_service, [monitor_option()]) -> MonitorRef
	  when MonitorRef :: reference().

monitor(_Type, _Item, _Opts) ->
    erlang:nif_error(undefined).

%% monitor_node/2
-spec monitor_node(Node, Flag) -> true when
      Node :: node(),
      Flag :: boolean().
monitor_node(_Node, _Flag) ->
    erlang:nif_error(undefined).

%% monitor_node/3
-spec erlang:monitor_node(Node, Flag, Options) -> true when
      Node :: node(),
      Flag :: boolean(),
      Options :: [Option],
      Option :: allow_passive_connect.
monitor_node(_Node, _Flag, _Options) ->
    erlang:nif_error(undefined).

%% nif_error/1
%% Shadowed by erl_bif_types: erlang:nif_error/1
-spec erlang:nif_error(Reason) -> no_return() when
      Reason :: term().
nif_error(_Reason) ->
    erlang:nif_error(undefined).

%% nif_error/2
%% Shadowed by erl_bif_types: erlang:nif_error/2
-spec erlang:nif_error(Reason, Args) -> no_return() when
      Reason :: term(),
      Args :: [term()].
nif_error(_Reason, _Args) ->
    erlang:nif_error(undefined).

%% node/0
%% Shadowed by erl_bif_types: erlang:node/0
-spec node() -> Node when
      Node :: node().
node() ->
    erlang:nif_error(undefined).

%% node/1
%% Shadowed by erl_bif_types: erlang:node/1
-spec node(Arg) -> Node when
      Arg :: pid() | port() | reference(),
      Node :: node().
node(_Arg) ->
    erlang:nif_error(undefined).

%% now/0
-spec now() -> Timestamp when
      Timestamp :: timestamp().
now() ->
    erlang:nif_error(undefined).

%% phash/2
-spec erlang:phash(Term, Range) -> Hash when
      Term :: term(),
      Range :: pos_integer(),
      Hash :: pos_integer().
phash(_Term, _Range) ->
    erlang:nif_error(undefined).

%% phash2/1
-spec erlang:phash2(Term) -> Hash when
      Term :: term(),
      Hash :: non_neg_integer().
phash2(_Term) ->
    erlang:nif_error(undefined).

%% phash2/2
-spec erlang:phash2(Term, Range) -> Hash when
      Term :: term(),
      Range :: pos_integer(),
      Hash :: non_neg_integer().
phash2(_Term, _Range) ->
    erlang:nif_error(undefined).

%% pid_to_list/1
-spec pid_to_list(Pid) -> string() when
      Pid :: pid().
pid_to_list(_Pid) ->
    erlang:nif_error(undefined).

%% port_to_list/1
-spec port_to_list(Port) -> string() when
      Port :: port().
port_to_list(_Port) ->
    erlang:nif_error(undefined).

%% ports/0
-spec erlang:ports() -> [port()].
ports() ->
    erlang:nif_error(undefined).

%% posixtime_to_universaltime/1
-spec erlang:posixtime_to_universaltime(P1) -> {calendar:date(), calendar:time()} when
      P1 :: integer().
posixtime_to_universaltime(_P1) ->
    erlang:nif_error(undefined).

-spec erlang:unique_integer(ModifierList) -> integer() when
      ModifierList :: [Modifier],
      Modifier :: positive | monotonic.

unique_integer(_ModifierList) ->
    erlang:nif_error(undefined).

-spec erlang:unique_integer() -> integer().

unique_integer() ->
    erlang:nif_error(undefined).

-spec erlang:monotonic_time() -> integer().

monotonic_time() ->
    erlang:nif_error(undefined).

-spec erlang:monotonic_time(Unit) -> integer() when
      Unit :: time_unit().

monotonic_time(_Unit) ->
    erlang:nif_error(undefined).

-spec erlang:system_time() -> integer().

system_time() ->
    erlang:nif_error(undefined).

-spec erlang:system_time(Unit) -> integer() when
      Unit :: time_unit().

system_time(_Unit) ->
    erlang:nif_error(undefined).

-spec erlang:convert_time_unit(Time, FromUnit, ToUnit) -> ConvertedTime when
      Time :: integer(),
      ConvertedTime :: integer(),
      FromUnit :: time_unit(),
      ToUnit :: time_unit().

convert_time_unit(Time, FromUnit, ToUnit) ->
    try
	FU = case FromUnit of
		 native -> erts_internal:time_unit();
                 perf_counter -> erts_internal:perf_counter_unit();
		 nanosecond -> 1000*1000*1000;
		 microsecond -> 1000*1000;
		 millisecond -> 1000;
		 second -> 1;

		 %% Deprecated symbolic units...
		 nano_seconds -> 1000*1000*1000;
		 micro_seconds -> 1000*1000;
		 milli_seconds -> 1000;
		 seconds -> 1;

		 _ when FromUnit > 0 -> FromUnit
	     end,
	TU = case ToUnit of
		 native -> erts_internal:time_unit();
                 perf_counter -> erts_internal:perf_counter_unit();
		 nanosecond -> 1000*1000*1000;
		 microsecond -> 1000*1000;
		 millisecond -> 1000;
		 second -> 1;

		 %% Deprecated symbolic units...
		 nano_seconds -> 1000*1000*1000;
		 micro_seconds -> 1000*1000;
		 milli_seconds -> 1000;
		 seconds -> 1;

		 _ when ToUnit > 0 -> ToUnit
	     end,
	case Time < 0 of
	    true -> TU*Time - (FU - 1);
	    false -> TU*Time
	end div FU
    catch
	_ : _ ->
	    error_with_info(badarg, [Time, FromUnit, ToUnit])
    end.

-spec erlang:time_offset() -> integer().

time_offset() ->
    erlang:nif_error(undefined).

-spec erlang:time_offset(Unit) -> integer() when
      Unit :: time_unit().

time_offset(_Unit) ->
    erlang:nif_error(undefined).

-spec erlang:timestamp() -> Timestamp when
      Timestamp :: timestamp().

timestamp() ->
    erlang:nif_error(undefined).

%% prepare_loading/2
-spec erlang:prepare_loading(Module, Code) -> PreparedCode | {error, Reason} when
      Module :: module(),
      Code :: binary(),
      PreparedCode :: prepared_code(),
      Reason :: badfile.
prepare_loading(Module, <<"FOR1",_/bits>>=Code) ->
    prepare_loading_1(Module, Code);
prepare_loading(Module, Code0) ->
    %% Corrupt header or compressed module, attempt to decompress it before
    %% passing it to the loader and leave error signalling to the BIF.
    Code = try zlib:gunzip(Code0) of
               Decompressed -> Decompressed
           catch
               _:_ -> Code0
           end,

    prepare_loading_1(Module, Code).

prepare_loading_1(Module, Code) ->
    try erts_internal:prepare_loading(Module, Code) of
        Res -> Res
    catch
        error:Reason ->
            try
                erlang:error(new_stacktrace, [Module,Code])
            catch
                error:new_stacktrace:Stk0 ->
                    [{Mod,_,L,Loc0}|T] = Stk0,
                    Loc = [{error_info,#{module => erl_erts_errors}}|Loc0],
                    Stk = [{Mod,prepare_loading,L,Loc}|T],
                    erlang:raise(error, Reason, Stk)
            end
    end.

%% pre_loaded/0
-spec pre_loaded() -> [module()].
pre_loaded() ->
    erlang:nif_error(undefined).

%% process_display/2
-spec erlang:process_display(Pid, Type) -> true when
      Pid :: pid(),
      Type :: backtrace.
process_display(Pid, Type) ->
    case case erts_internal:process_display(Pid, Type) of
             Ref when erlang:is_reference(Ref) ->
                 receive
                     {Ref, Res} ->
                         Res
                 end;
             Res ->
                 Res
         end of
        badopt ->
            badarg_with_cause([Pid, Type], badopt);
        badarg ->
            badarg_with_info([Pid, Type]);
        Result ->
            Result
    end.

%% process_flag/3
-spec process_flag(Pid, Flag, Value) -> OldValue when
      Pid :: pid(),
      Flag :: save_calls,
      Value :: non_neg_integer(),
      OldValue :: non_neg_integer().
process_flag(Pid, Flag, Value) ->
    case case erts_internal:process_flag(Pid, Flag, Value) of
             Ref when erlang:is_reference(Ref) ->
                 receive {Ref, Res} -> Res end;
             Res -> Res
         end of
        badtype -> badarg_with_cause([Pid, Flag, Value], badtype);
        badarg -> badarg_with_info([Pid, Flag, Value]);
        Result -> Result
    end.

%% process_info/1
-spec process_info(Pid) -> Info when
      Pid :: pid(),
      Info :: [InfoTuple] | undefined,
      InfoTuple :: process_info_result_item().
process_info(_Pid) ->
    erlang:nif_error(undefined).

%% processes/0
-spec processes() -> [pid()].
processes() ->
    erlang:nif_error(undefined).

%% purge_module/1
-spec purge_module(Module) -> true when
      Module :: atom().
purge_module(Module) when erlang:is_atom(Module) ->
    case erts_code_purger:purge(Module) of
	{false, _} ->
	    badarg_with_info([Module]);
	{true, _} ->
	    true
    end;
purge_module(Arg) ->
    badarg_with_info([Arg]).


%% put/2
-spec put(Key, Val) -> term() when
      Key :: term(),
      Val :: term().
put(_Key, _Val) ->
    erlang:nif_error(undefined).

%% raise/3
%% Shadowed by erl_bif_types: erlang:raise/3
-spec erlang:raise(Class, Reason, Stacktrace) -> 'badarg' when
      Class :: 'error' | 'exit' | 'throw',
      Reason :: term(),
      Stacktrace :: raise_stacktrace() | stacktrace().
raise(_Class, _Reason, _Stacktrace) ->
    erlang:nif_error(undefined).

%% read_timer/1
-spec erlang:read_timer(TimerRef) -> Result when
      TimerRef :: reference(),
      Time :: non_neg_integer(),
      Result :: Time | false.

read_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% read_timer/2
-spec erlang:read_timer(TimerRef, Options) -> Result | ok when
      TimerRef :: reference(),
      Async :: boolean(),
      Option :: {async, Async},
      Options :: [Option],
      Time :: non_neg_integer(),
      Result :: Time | false.

read_timer(_TimerRef, _Options) ->
    erlang:nif_error(undefined).

%% ref_to_list/1
-spec ref_to_list(Ref) -> string() when
      Ref :: reference().
ref_to_list(_Ref) ->
    erlang:nif_error(undefined).

%% register/2
-spec register(RegName, PidOrPort) -> true when
      RegName :: atom(),
      PidOrPort :: port() | pid().
register(_RegName, _PidOrPort) ->
    erlang:nif_error(undefined).

%% registered/0
-spec registered() -> [RegName] when
      RegName :: atom().
registered() ->
    erlang:nif_error(undefined).

%% resume_process/1
-spec erlang:resume_process(Suspendee) -> true when
      Suspendee :: pid().
resume_process(_Suspendee) ->
    erlang:nif_error(undefined).

%% round/1
%% Shadowed by erl_bif_types: erlang:round/1
-spec round(Number) -> integer() when
      Number :: number().
round(_Number) ->
    erlang:nif_error(undefined).

%% self/0
%% Shadowed by erl_bif_types: erlang:self/0
-spec self() -> pid().
self() ->
    erlang:nif_error(undefined).

%% send_after/3
-spec erlang:send_after(Time, Dest, Msg) -> TimerRef when
      Time :: non_neg_integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      TimerRef :: reference().

send_after(_Time, _Dest, _Msg) ->
    erlang:nif_error(undefined).

%% send_after/4
-spec erlang:send_after(Time, Dest, Msg, Options) -> TimerRef when
      Time :: integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      Options :: [Option],
      Abs :: boolean(),
      Option :: {abs, Abs},
      TimerRef :: reference().

send_after(_Time, _Dest, _Msg, _Options) ->
    erlang:nif_error(undefined).

%% seq_trace/2
-spec erlang:seq_trace(P1, P2) -> seq_trace_info_returns() | {term(), term(), term(), term(), term()} when
      P1 :: atom(),
      P2 :: term().
seq_trace(_P1, _P2) ->
    erlang:nif_error(undefined).

%% seq_trace_print/1
-spec erlang:seq_trace_print(P1) -> boolean() when
      P1 :: term().
seq_trace_print(_P1) ->
    erlang:nif_error(undefined).

%% seq_trace_print/2
-spec erlang:seq_trace_print(P1, P2) -> boolean() when
      P1 :: atom() | integer(),
      P2 :: term().
seq_trace_print(_P1, _P2) ->
    erlang:nif_error(undefined).

%% setnode/2
-spec erlang:setnode(P1, P2) -> true when
      P1 :: atom(),
      P2 :: integer().
setnode(_P1, _P2) ->
    erlang:nif_error(undefined).

%% setnode/3
-spec erlang:setnode(Node, DistCtrlr, Opts) -> dist_handle() when
      Node :: atom(),
      DistCtrlr :: port() | pid(),
      Opts :: {integer(), pos_integer()}.
setnode(Node, DistCtrlr, {_Flags, _Creation} = Opts) ->
    case case erts_internal:create_dist_channel(Node, DistCtrlr, Opts) of
             {ok, DH} -> DH;
             {message, Ref} -> receive {Ref, Res} -> Res end;
             Err -> Err
         end of
        Error when erlang:is_atom(Error) ->
            erlang:error(Error, [Node, DistCtrlr, Opts]);
        DHandle ->
            DHandle
    end;
setnode(Node, DistCtrlr, Opts) ->
    badarg_with_info([Node, DistCtrlr, Opts]).


%% size/1
%% Shadowed by erl_bif_types: erlang:size/1
-spec size(Item) -> non_neg_integer() when
      Item :: tuple() | binary().
size(_Item) ->
    erlang:nif_error(undefined).

%% spawn/3
-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% spawn_link/3
-spec spawn_link(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% split_binary/2
-spec split_binary(Bin, Pos) -> {binary(), binary()} when
      Bin :: binary(),
      Pos :: non_neg_integer().
split_binary(_Bin, _Pos) ->
    erlang:nif_error(undefined).

%% start_timer/3
-spec erlang:start_timer(Time, Dest, Msg) -> TimerRef when
      Time :: non_neg_integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      TimerRef :: reference().

start_timer(_Time, _Dest, _Msg) ->
    erlang:nif_error(undefined).

%% start_timer/4
-spec erlang:start_timer(Time, Dest, Msg, Options) -> TimerRef when
      Time :: integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      Options :: [Option],
      Abs :: boolean(),
      Option :: {abs, Abs},
      TimerRef :: reference().

start_timer(_Time, _Dest, _Msg, _Options) ->
    erlang:nif_error(undefined).

%% suspend_process/2
-spec erlang:suspend_process(Suspendee, OptList) -> boolean() when
      Suspendee :: pid(),
      OptList :: [Opt],
      Opt :: unless_suspending | asynchronous | {asynchronous, term()}.
suspend_process(Suspendee, OptList) ->
    case case erts_internal:suspend_process(Suspendee, OptList) of
	     Ref when erlang:is_reference(Ref) ->
                 receive {Ref, Res} -> Res end;
             Res ->
                 Res
         end of
        badopt -> badarg_with_cause([Suspendee, OptList], badopt);
        Bool when erlang:is_boolean(Bool) -> Bool;
	Error -> error_with_info(Error, [Suspendee, OptList])
    end.

-spec erlang:suspend_process(Suspendee) -> 'true' when
      Suspendee :: pid().
suspend_process(Suspendee) ->
    case case erts_internal:suspend_process(Suspendee, []) of
	     Ref when erlang:is_reference(Ref) ->
                 receive {Ref, Res} -> Res end;
             Res ->
                 Res
	 end of
	true -> true;
        false -> erlang:error(internal_error, [Suspendee]);
	Error -> error_with_info(Error, [Suspendee])
    end.

%% system_monitor/0
-spec erlang:system_monitor() -> MonSettings when
      MonSettings :: undefined | { MonitorPid, Options },
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ].
system_monitor() ->
    erlang:nif_error(undefined).

%% system_monitor/1
-spec erlang:system_monitor(Arg) -> MonSettings when
      Arg :: undefined | { MonitorPid, Options },
      MonSettings :: undefined | { MonitorPid, Options },
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ].
system_monitor(_Arg) ->
    erlang:nif_error(undefined).

%% system_monitor/2
-spec erlang:system_monitor(MonitorPid, Options) -> MonSettings when
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ],
      MonSettings :: undefined | { OldMonitorPid, OldOptions },
      OldMonitorPid :: pid(),
      OldOptions :: [ system_monitor_option() ].
system_monitor(_MonitorPid, _Options) ->
    erlang:nif_error(undefined).

%% system_profile/0
-spec erlang:system_profile() -> ProfilerSettings when
      ProfilerSettings :: undefined | { ProfilerPid, Options},
      ProfilerPid :: pid() | port(),
      Options :: [ system_profile_option() ].
system_profile() ->
    erlang:nif_error(undefined).

%% system_profile/2
-spec erlang:system_profile(ProfilerPid, Options) -> ProfilerSettings when
      ProfilerPid :: pid() | port() | undefined,
      Options :: [ system_profile_option() ],
      ProfilerSettings :: undefined | { pid() | port(), [ system_profile_option() ]}.
system_profile(_ProfilerPid, _Options) ->
    erlang:nif_error(undefined).

%% throw/1
%% Shadowed by erl_bif_types: erlang:throw/1
-spec throw(Any) -> no_return() when
      Any :: term().
throw(_Any) ->
    erlang:nif_error(undefined).

%% time/0
-spec time() -> Time when
      Time :: calendar:time().
time() ->
    erlang:nif_error(undefined).

%% trace/3
-spec erlang:trace(PidPortSpec, How, FlagList) -> integer() when
      PidPortSpec :: pid() | port()
                   | all | processes | ports
                   | existing | existing_processes | existing_ports
                   | new | new_processes | new_ports,
      How :: boolean(),
      FlagList :: [trace_flag()].
trace(PidPortSpec, How, FlagList) ->
    ensure_tracer_module_loaded(tracer, FlagList),
    try erts_internal:trace(PidPortSpec, How, FlagList) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [PidPortSpec, How, FlagList], Stk)
    end.
%% trace_delivered/1
-spec erlang:trace_delivered(Tracee) -> Ref when
      Tracee :: pid() | all,
      Ref :: reference().
trace_delivered(_Tracee) ->
    erlang:nif_error(undefined).

%% trace_info/2
-spec erlang:trace_info(PidPortFuncEvent, Item) -> Res when
      PidPortFuncEvent :: pid() | port() | new | new_processes | new_ports
                     | {Module, Function, Arity} | on_load | send | 'receive',
      Module :: module(),
      Function :: atom(),
      Arity :: arity(),
      Item :: flags | tracer | traced | match_spec
            | meta | meta_match_spec | call_count | call_time | call_memory | all,
      Res :: trace_info_return().
trace_info(_PidPortFuncEvent, _Item) ->
    erlang:nif_error(undefined).

%% trunc/1
%% Shadowed by erl_bif_types: erlang:trunc/1
-spec trunc(Number) -> integer() when
      Number :: number().
trunc(_Number) ->
    erlang:nif_error(undefined).

%% tuple_size/1
%% Shadowed by erl_bif_types: erlang:tuple_size/1
-spec tuple_size(Tuple) -> non_neg_integer() when
      Tuple :: tuple().
tuple_size(_Tuple) ->
    erlang:nif_error(undefined).

%% universaltime/0
-spec erlang:universaltime() -> DateTime when
      DateTime :: calendar:datetime().
universaltime() ->
    erlang:nif_error(undefined).

%% universaltime_to_posixtime/1
-spec erlang:universaltime_to_posixtime(P1) -> integer() when
      P1 :: {calendar:date(), calendar:time()}.
universaltime_to_posixtime(_P1) ->
    erlang:nif_error(undefined).

%% unlink/1
-spec unlink(Id) -> true when
      Id :: pid() | port().
unlink(_Id) ->
    erlang:nif_error(undefined).

%% unregister/1
-spec unregister(RegName) -> true when
      RegName :: atom().
unregister(_RegName) ->
    erlang:nif_error(undefined).

%% whereis/1
-spec whereis(RegName) -> pid() | port() | undefined when
      RegName :: atom().
whereis(_RegName) ->
    erlang:nif_error(undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% More complicated native code BIFs
%%% These are here for the types/specs, the real implementation is in the C code.
%%% This chunk is handwritten, i.e. contains more complicated specs.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% types and specs

%% Shadowed by erl_bif_types: erlang:abs/1
-spec abs(Float) -> float() when
      Float :: float();
         (Int) -> non_neg_integer() when
      Int :: integer().
abs(_Number) ->
    erlang:nif_error(undefined).

%% Not documented
%% Shadowed by erl_bif_types: erlang:append/2
-spec erlang:append(List,Tail) -> maybe_improper_list() when
      List :: [term()],
      Tail :: term().
append(_List,_Tail) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:element/2
-spec element(N, Tuple) -> term() when
    N :: pos_integer(),
    Tuple :: tuple().
element(_N, _Tuple) ->
    erlang:nif_error(undefined).

%% Not documented
-type module_info_key() :: attributes | compile | exports | functions | md5
                         | module | native | native_addresses | nifs.
-spec erlang:get_module_info(Module, Item) -> ModuleInfo when
      Module :: atom(),
      Item :: module_info_key(),
      ModuleInfo :: term().
get_module_info(_Module, _Item) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:hd/1
-spec hd(List) -> Head when
      List :: nonempty_maybe_improper_list(),
      Head :: term().
hd(_List) ->
    erlang:nif_error(undefined).

%% erlang:info/1 no longer exists!

%% Shadowed by erl_bif_types: erlang:is_atom/1
-spec is_atom(Term) -> boolean() when
      Term :: term().
is_atom(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_binary/1
-spec is_binary(Term) -> boolean() when
      Term :: term().
is_binary(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_bitstring/1
-spec is_bitstring(Term) -> boolean() when
      Term :: term().
is_bitstring(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_boolean/1
-spec is_boolean(Term) -> boolean() when
      Term :: term().
is_boolean(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_float/1
-spec is_float(Term) -> boolean() when
      Term :: term().
is_float(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_function/1
-spec is_function(Term) -> boolean() when
      Term :: term().
is_function(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_function/2
-spec is_function(Term, Arity) -> boolean() when
      Term :: term(),
      Arity :: arity().
is_function(_Term, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_integer/1
-spec is_integer(Term) -> boolean() when
      Term :: term().
is_integer(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_list/1
-spec is_list(Term) -> boolean() when
      Term :: term().
is_list(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_number/1
-spec is_number(Term) -> boolean() when
      Term :: term().
is_number(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_pid/1
-spec is_pid(Term) -> boolean() when
      Term :: term().
is_pid(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_map/1
-spec is_map(Term) -> boolean() when
      Term :: term().
is_map(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_port/1
-spec is_port(Term) -> boolean() when
      Term :: term().
is_port(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_record/2
-spec is_record(Term,RecordTag) -> boolean() when
      Term :: term(),
      RecordTag :: atom().
is_record(_Term,_RecordTag) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_record/3
-spec is_record(Term,RecordTag,Size) -> boolean() when
      Term :: term(),
      RecordTag :: atom(),
      Size :: non_neg_integer().
is_record(_Term,_RecordTag,_Size) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_reference/1
-spec is_reference(Term) -> boolean() when
      Term :: term().
is_reference(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_tuple/1
-spec is_tuple(Term) -> boolean() when
      Term :: term().
is_tuple(_Term) ->
    erlang:nif_error(undefined).

-spec load_module(Module, Binary) -> {module, Module} | {error, Reason} when
      Module :: module(),
      Binary :: binary(),
      Reason :: badfile | not_purged | on_load
              | {features_not_allowed, [atom()]}.
load_module(Mod, Code) ->
    try
        case erlang:prepare_loading(Mod, Code) of
            {error,_}=Error ->
                Error;
            Prep when erlang:is_reference(Prep) ->
                case erlang:finish_loading([Prep]) of
                    ok ->
                        {module,Mod};
                    {Error,[Mod]} ->
                        {error,Error}
                end
        end
    catch
        error:Reason -> error_with_info(Reason, [Mod, Code])
    end.

-spec erlang:load_nif(Path, LoadInfo) ->  ok | Error when
      Path :: string(),
      LoadInfo :: term(),
      Error :: {error, {Reason, Text :: string()}},
      Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
load_nif(_Path, _LoadInfo) ->
    erlang:nif_error(undefined).

-spec erlang:localtime_to_universaltime(Localtime, IsDst) -> Universaltime when
      Localtime :: calendar:datetime(),
      Universaltime :: calendar:datetime(),
      IsDst :: true | false | undefined.
localtime_to_universaltime(_Localtime, _IsDst) ->
    erlang:nif_error(undefined).

%% CHECK! Why the strange very thorough specification of the error
%% condition with disallowed arity in erl_bif_types?
%% Not documented
%% Shadowed by erl_bif_types: erlang:make_fun/3
-spec erlang:make_fun(Module, Function, Arity) -> function() when
      Module :: atom(),
      Function :: atom(),
      Arity :: arity().
make_fun(_Module,_Function, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:make_tuple/2
-spec erlang:make_tuple(Arity, InitialValue) -> tuple() when
      Arity :: arity(),
      InitialValue :: term().
make_tuple(_Arity,_InitialValue) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:make_tuple/3
-spec erlang:make_tuple(Arity, DefaultValue, InitList) -> tuple() when
      Arity :: arity(),
      DefaultValue :: term(),
      InitList :: [{Position :: pos_integer(), term()}].
make_tuple(_Arity,_DefaultValue,_InitList) ->
    erlang:nif_error(undefined).

-spec open_port(PortName, PortSettings) -> port() when
      PortName :: {spawn, Command :: string() | binary()} |
                  {spawn_driver, Command :: string() | binary()} |
                  {spawn_executable, FileName :: file:name_all() } |
                  {fd, In :: non_neg_integer(), Out :: non_neg_integer()},
      PortSettings :: [Opt],
      Opt :: {packet, N :: 1 | 2 | 4}
           | stream
           | {line, L :: non_neg_integer()}
           | {cd, Dir :: string() | binary()}
           | {env, Env :: [{Name :: os:env_var_name(), Val :: os:env_var_value() | false}]}
           | {args, [string() | binary()]}
           | {arg0, string() | binary()}
           | exit_status
           | use_stdio
           | nouse_stdio
           | stderr_to_stdout
           | in
           | out
           | binary
           | eof
	   | {parallelism, Boolean :: boolean()}
	   | hide
           | {busy_limits_port, {non_neg_integer(), non_neg_integer()} | disabled}
           | {busy_limits_msgq, {non_neg_integer(), non_neg_integer()} | disabled}.
open_port(PortName, PortSettings) ->
    case case erts_internal:open_port(PortName, PortSettings) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	Port when erlang:is_port(Port) -> Port;
        badopt -> badarg_with_cause([PortName, PortSettings], badopt);
	Error -> error_with_info(Error, [PortName, PortSettings])
    end.

-type priority_level() ::
      low | normal | high | max.

-type message_queue_data() ::
	off_heap | on_heap.

-spec process_flag(async_dist, Boolean) -> OldBoolean when
      Boolean :: boolean(),
      OldBoolean :: boolean();
                  (trap_exit, Boolean) -> OldBoolean when
      Boolean :: boolean(),
      OldBoolean :: boolean();
                  (error_handler, Module) -> OldModule when
      Module :: atom(),
      OldModule :: atom();
                  (fullsweep_after, FullsweepAfter) -> OldFullsweepAfter when
      FullsweepAfter :: non_neg_integer(),
      OldFullsweepAfter :: non_neg_integer();
                  (min_heap_size, MinHeapSize) -> OldMinHeapSize when
      MinHeapSize :: non_neg_integer(),
      OldMinHeapSize :: non_neg_integer();
                  (min_bin_vheap_size, MinBinVHeapSize) -> OldMinBinVHeapSize when
      MinBinVHeapSize :: non_neg_integer(),
      OldMinBinVHeapSize :: non_neg_integer();
                  (max_heap_size, MaxHeapSize) -> OldMaxHeapSize when
      MaxHeapSize :: max_heap_size(),
      OldMaxHeapSize :: max_heap_size();
                  (message_queue_data, MQD) -> OldMQD when
      MQD :: message_queue_data(),
      OldMQD :: message_queue_data();
                  (priority, Level) -> OldLevel when
      Level :: priority_level(),
      OldLevel :: priority_level();
                  (save_calls, N) -> OldN when
      N :: 0..10000,
      OldN :: 0..10000;
                  (sensitive, Boolean) -> OldBoolean when
      Boolean :: boolean(),
      OldBoolean :: boolean();
                  %% Deliberately not documented.
                  ({monitor_nodes, term()}, term()) -> term();
                  (monitor_nodes, term()) -> term().

process_flag(_Flag, _Value) ->
    erlang:nif_error(undefined).

-type process_info_item() ::
      async_dist |
      backtrace |
      binary |
      catchlevel |
      current_function |
      current_location |
      current_stacktrace |
      dictionary |
      error_handler |
      garbage_collection |
      garbage_collection_info |
      group_leader |
      heap_size |
      initial_call |
      links |
      last_calls |
      memory |
      message_queue_len |
      messages |
      min_heap_size |
      min_bin_vheap_size |
      monitored_by |
      monitors |
      message_queue_data |
      parent |
      priority |
      reductions |
      registered_name |
      sequential_trace_token |
      stack_size |
      status |
      suspending |
      total_heap_size |
      trace |
      trap_exit.

-type process_info_result_item() ::
      {async_dist, Enabled :: boolean()} |
      {backtrace, Bin :: binary()} |
      {binary, BinInfo :: [{non_neg_integer(),
                            non_neg_integer(),
                            non_neg_integer()}]} |
      {catchlevel, CatchLevel :: non_neg_integer()} |
      {current_function,
       {Module :: module(), Function :: atom(), Arity :: arity()} | undefined} |
      {current_location,
       {Module :: module(), Function :: atom(), Arity :: arity(),
        Location :: [{file, Filename :: string()} | % not a stack_item()!
                     {line, Line :: pos_integer()}]}} |
      {current_stacktrace, Stack :: [stack_item()]} |
      {dictionary, Dictionary :: [{Key :: term(), Value :: term()}]} |
      {error_handler, Module :: module()} |
      {garbage_collection, GCInfo :: [{atom(),non_neg_integer()}]} |
      {garbage_collection_info, GCInfo :: [{atom(),non_neg_integer()}]} |
      {group_leader, GroupLeader :: pid()} |
      {heap_size, Size :: non_neg_integer()} |
      {initial_call, mfa()} |
      {links, PidsAndPorts :: [pid() | port()]} |
      {last_calls, false | (Calls :: [mfa()])} |
      {memory, Size :: non_neg_integer()} |
      {message_queue_len, MessageQueueLen :: non_neg_integer()} |
      {messages, MessageQueue :: [term()]} |
      {min_heap_size, MinHeapSize :: non_neg_integer()} |
      {min_bin_vheap_size, MinBinVHeapSize :: non_neg_integer()} |
      {max_heap_size, MaxHeapSize :: max_heap_size()} |
      {monitored_by, MonitoredBy :: [pid() | port() | nif_resource()]} |
      {monitors,
       Monitors :: [{process | port, Pid :: pid() | port() |
                                     {RegName :: atom(), Node :: node()}}]} |
      {message_queue_data, MQD :: message_queue_data()} |
      {parent, pid() | undefined} |
      {priority, Level :: priority_level()} |
      {reductions, Number :: non_neg_integer()} |
      {registered_name, [] | (Atom :: atom())} |
      {sequential_trace_token, [] | (SequentialTraceToken :: term())} |
      {stack_size, Size :: non_neg_integer()} |
      {status, Status :: exiting | garbage_collecting | waiting | running | runnable | suspended} |
      {suspending,
       SuspendeeList :: [{Suspendee :: pid(),
                          ActiveSuspendCount :: non_neg_integer(),
                          OutstandingSuspendCount ::non_neg_integer()}]} |
      {total_heap_size, Size :: non_neg_integer()} |
      {trace, InternalTraceFlags :: non_neg_integer()} |
      {trap_exit, Boolean :: boolean()}.

-type stack_item() ::
        {Module :: module(),
         Function :: atom(),
         Arity :: arity() | (Args :: [term()]),
         Location :: [{file, Filename :: string()} |
                      {line, Line :: pos_integer()}]}.

-spec process_info(Pid, Item) ->
                          InfoTuple | [] | undefined when
      Pid :: pid(),
      Item :: process_info_item(),
      InfoTuple :: process_info_result_item();
                  (Pid, ItemList) -> InfoTupleList | [] | undefined when
      Pid :: pid(),
      ItemList :: [Item],
      Item :: process_info_item(),
      InfoTupleList :: [InfoTuple],
      InfoTuple :: process_info_result_item().
process_info(_Pid,_ItemSpec) ->
    erlang:nif_error(undefined).

-spec erlang:send(Dest, Msg) -> Msg when
      Dest :: send_destination(),
      Msg :: term().
send(_Dest,_Msg) ->
    erlang:nif_error(undefined).

-spec erlang:send(Dest, Msg, Options) -> Res when
      Dest :: send_destination(),
      Msg :: term(),
      Options :: [nosuspend | noconnect],
      Res :: ok | nosuspend | noconnect.
send(_Dest,_Msg,_Options) ->
    erlang:nif_error(undefined).

%% Not documented
-spec erlang:seq_trace_info(send) -> {send, boolean()};
                    ('receive') -> {'receive', boolean()};
                    (print) -> {print, boolean()};
                    (timestamp) -> {timestamp, boolean()};
                    (monotonic_timestamp) -> {timestamp, boolean()};
                    (strict_monotonic_timestamp) -> {strict_monotonic_timestamp, boolean()};
                    (label) -> [] | {label, term()};
                    (serial) -> [] | {serial, {non_neg_integer(), non_neg_integer()}}.
seq_trace_info(_What) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:setelement/3
-spec setelement(Index, Tuple1, Value) -> Tuple2 when
      Index :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Value :: term().
setelement(_Index, _Tuple1, _Value) ->
   erlang:nif_error(undefined).

-spec statistics(active_tasks) -> [ActiveTasks] when
      ActiveTasks :: non_neg_integer();
		(active_tasks_all) -> [ActiveTasks] when
      ActiveTasks :: non_neg_integer();
		(context_switches) -> {ContextSwitches,0} when
      ContextSwitches :: non_neg_integer();
                (exact_reductions) -> {Total_Exact_Reductions,
                                       Exact_Reductions_Since_Last_Call} when
      Total_Exact_Reductions :: non_neg_integer(),
      Exact_Reductions_Since_Last_Call :: non_neg_integer();
                (garbage_collection) -> {Number_of_GCs, Words_Reclaimed, 0} when
      Number_of_GCs :: non_neg_integer(),
      Words_Reclaimed :: non_neg_integer();
                (io) -> {{input, Input}, {output, Output}} when
      Input :: non_neg_integer(),
      Output :: non_neg_integer();
                (microstate_accounting) -> [MSAcc_Thread] | undefined when
      MSAcc_Thread :: #{ type := MSAcc_Thread_Type,
                        id := MSAcc_Thread_Id,
                        counters := MSAcc_Counters},
      MSAcc_Thread_Type :: async | aux | dirty_io_scheduler
                         | dirty_cpu_scheduler | poll | scheduler,
      MSAcc_Thread_Id :: non_neg_integer(),
      MSAcc_Counters :: #{ MSAcc_Thread_State => non_neg_integer() },
      MSAcc_Thread_State :: alloc | aux | bif | busy_wait | check_io |
                            emulator | ets | gc | gc_fullsweep | nif |
                            other | port | send | sleep | timers;
                (reductions) -> {Total_Reductions,
                                 Reductions_Since_Last_Call} when
      Total_Reductions :: non_neg_integer(),
      Reductions_Since_Last_Call :: non_neg_integer();
                (run_queue) -> non_neg_integer();
                (run_queue_lengths) -> [RunQueueLength] when
      RunQueueLength :: non_neg_integer();
                (run_queue_lengths_all) -> [RunQueueLength] when
      RunQueueLength :: non_neg_integer();
                (runtime) -> {Total_Run_Time, Time_Since_Last_Call} when
      Total_Run_Time :: non_neg_integer(),
      Time_Since_Last_Call :: non_neg_integer();
                (scheduler_wall_time) -> [{SchedulerId, ActiveTime, TotalTime}] | undefined when
      SchedulerId :: pos_integer(),
      ActiveTime  :: non_neg_integer(),
      TotalTime   :: non_neg_integer();
                (scheduler_wall_time_all) -> [{SchedulerId, ActiveTime, TotalTime}] | undefined when
      SchedulerId :: pos_integer(),
      ActiveTime  :: non_neg_integer(),
      TotalTime   :: non_neg_integer();
		(total_active_tasks) -> ActiveTasks when
      ActiveTasks :: non_neg_integer(); 
		(total_active_tasks_all) -> ActiveTasks when
      ActiveTasks :: non_neg_integer();
                (total_run_queue_lengths) -> TotalRunQueueLengths when
      TotalRunQueueLengths :: non_neg_integer();
                (total_run_queue_lengths_all) -> TotalRunQueueLengths when
      TotalRunQueueLengths :: non_neg_integer();
                (wall_clock) -> {Total_Wallclock_Time,
                                 Wallclock_Time_Since_Last_Call} when
      Total_Wallclock_Time :: non_neg_integer(),
      Wallclock_Time_Since_Last_Call :: non_neg_integer().
statistics(_Item) ->
    erlang:nif_error(undefined).

%% Not documented
%% Shadowed by erl_bif_types: erlang:subtract/2
-spec erlang:subtract([term()], [term()]) -> [term()].
subtract(_,_) ->
    erlang:nif_error(undefined).

-type scheduler_bind_type() ::
      'no_node_processor_spread' |
      'no_node_thread_spread' |
      'no_spread' |
      'processor_spread' |
      'spread' |
      'thread_spread' |
      'thread_no_node_processor_spread' |
      'unbound'.

-spec erlang:system_flag(backtrace_depth, Depth) -> OldDepth when
      Depth :: non_neg_integer(),
      OldDepth :: non_neg_integer();
                        (cpu_topology, CpuTopology) -> OldCpuTopology when
      CpuTopology :: cpu_topology(),
      OldCpuTopology :: cpu_topology();
                        (dirty_cpu_schedulers_online, DirtyCPUSchedulersOnline) ->
                                OldDirtyCPUSchedulersOnline when
      DirtyCPUSchedulersOnline :: pos_integer(),
      OldDirtyCPUSchedulersOnline :: pos_integer();
                        (erts_alloc, {Alloc, F, V}) -> ok | notsup when
      Alloc :: atom(),
      F :: atom(),
      V :: integer();
                        (fullsweep_after, Number) -> OldNumber when
      Number :: non_neg_integer(),
      OldNumber :: non_neg_integer();
                        (microstate_accounting, Action) -> OldState when
      Action :: true | false | reset,
      OldState :: true | false;
                        (min_heap_size, MinHeapSize) -> OldMinHeapSize when
      MinHeapSize :: non_neg_integer(),
      OldMinHeapSize :: non_neg_integer();
                        (min_bin_vheap_size, MinBinVHeapSize) ->
                                OldMinBinVHeapSize when
      MinBinVHeapSize :: non_neg_integer(),
      OldMinBinVHeapSize :: non_neg_integer();
                        (max_heap_size, MaxHeapSize) -> OldMaxHeapSize when
      MaxHeapSize :: max_heap_size(),
      OldMaxHeapSize :: max_heap_size();
                        (multi_scheduling, BlockState) -> OldBlockState when
      BlockState :: block | unblock | block_normal | unblock_normal,
      OldBlockState :: blocked | disabled | enabled;
                        (outstanding_system_requests_limit, NewLimit) ->
          OldLimit when
      NewLimit :: 1..134217727,
      OldLimit :: 1..134217727;
                        (scheduler_bind_type, How) -> OldBindType when
      How :: scheduler_bind_type() | default_bind,
      OldBindType :: scheduler_bind_type();
                        (scheduler_wall_time, Boolean) ->  OldBoolean when
      Boolean :: boolean(),
      OldBoolean :: boolean();
                        (schedulers_online, SchedulersOnline) ->
                                OldSchedulersOnline when
      SchedulersOnline :: pos_integer(),
      OldSchedulersOnline :: pos_integer();
                        (system_logger, Logger) -> PrevLogger when
      Logger :: logger | undefined | pid(),
      PrevLogger :: logger | undefined | pid();
                        (trace_control_word, TCW) -> OldTCW when
      TCW :: non_neg_integer(),
      OldTCW :: non_neg_integer();
			(time_offset, finalize) -> OldState when
      OldState :: preliminary | final | volatile;
                        %% These are deliberately not documented
			(internal_cpu_topology, term()) -> term();
                        (sequential_tracer, Tracer) -> PrevTracer | false when
      Tracer :: pid() | port() | {module(), term()} | false,
      PrevTracer :: pid() | port() | {module(), term()} | false;
                        (reset_seq_trace,true) -> true.

system_flag(_Flag, _Value) ->
    erlang:nif_error(undefined).

-spec term_to_binary(Term) -> ext_binary() when
      Term :: term().
term_to_binary(_Term) ->
    erlang:nif_error(undefined).

-spec term_to_binary(Term, Options) -> ext_binary() when
      Term :: term(),
      Options :: [compressed |
         {compressed, Level :: 0..9} |
         deterministic |
         {minor_version, Version :: 0..2} |
         local ].
term_to_binary(_Term, _Options) ->
    erlang:nif_error(undefined).

-spec term_to_iovec(Term) -> ext_iovec() when
      Term :: term().
term_to_iovec(_Term) ->
    erlang:nif_error(undefined).

-spec term_to_iovec(Term, Options) -> ext_iovec() when
      Term :: term(),
      Options :: [compressed |
         {compressed, Level :: 0..9} |
         deterministic |
         {minor_version, Version :: 0..2} |
         local ].
term_to_iovec(_Term, _Options) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:tl/1
-spec tl(List) -> Tail when
      List :: nonempty_maybe_improper_list(),
      Tail :: term().
tl(_List) ->
    erlang:nif_error(undefined).

-type match_variable() :: atom(). % Approximation of '$1' | '$2' | ...
-type trace_pattern_mfa() ::
      {atom(),atom(),arity() | '_'} | on_load.
-type trace_match_spec() ::
      [{[term()] | '_' | match_variable() ,[term()],[term()]}].

-spec erlang:trace_pattern(MFA, MatchSpec) -> non_neg_integer() when
      MFA :: trace_pattern_mfa() | send | 'receive',
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause.
trace_pattern(MFA, MatchSpec) ->
    try erts_internal:trace_pattern(MFA, MatchSpec, []) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [MFA, MatchSpec], Stk)
    end.

-type trace_pattern_flag() ::
      global | local |
      meta | {meta, Pid :: pid()} |
      {meta, TracerModule :: module(), TracerState :: term()} |
      call_count |
      call_time |
      call_memory.

-spec erlang:trace_pattern(send, MatchSpec, []) -> non_neg_integer() when
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean();
			  ('receive', MatchSpec, []) -> non_neg_integer() when
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean();
			  (MFA, MatchSpec, FlagList) -> non_neg_integer() when
      MFA :: trace_pattern_mfa(),
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause,
      FlagList :: [ trace_pattern_flag() ].
trace_pattern(MFA, MatchSpec, FlagList) ->
    ensure_tracer_module_loaded(meta, FlagList),
    try erts_internal:trace_pattern(MFA, MatchSpec, FlagList) of
        Res -> Res
    catch error:R:Stk ->
            error_with_inherited_info(R, [MFA, MatchSpec, FlagList], Stk)
    end.

%% Shadowed by erl_bif_types: erlang:tuple_to_list/1
-spec tuple_to_list(Tuple) -> [term()] when
      Tuple :: tuple().
tuple_to_list(_Tuple) ->
    erlang:nif_error(undefined).

-type cpu_topology() ::
        [LevelEntry :: level_entry()] | undefined.
-type level_entry() ::
        {LevelTag :: level_tag(), SubLevel :: sub_level()}
      | {LevelTag :: level_tag(),
         InfoList :: info_list(),
         SubLevel :: sub_level()}.
-type level_tag() :: core | node | processor | thread.
-type sub_level() :: [LevelEntry :: level_entry()]
                   | (LogicalCpuId :: {logical, non_neg_integer()}).
-type info_list() :: [].

%% Note: changing the ordering number of a clause will change the docs!
%% Shadowed by erl_bif_types: erlang:system_info/1
-spec erlang:system_info
         (allocated_areas) -> [ tuple() ];
         (allocator) ->
                 {Allocator, Version, Features, Settings} when
      Allocator :: undefined | glibc,
      Version :: [non_neg_integer()],
      Features :: [atom()],
      Settings :: [{Subsystem :: atom(),
                    [{Parameter :: atom(),
                      Value :: term()}]}];
         ({allocator, Alloc}) -> [_] when %% More or less anything
      Alloc :: atom();
         (alloc_util_allocators) -> [Alloc] when
      Alloc :: atom();
         ({allocator_sizes, Alloc}) -> [_] when %% More or less anything
      Alloc :: atom();
         (atom_count) -> pos_integer();
         (atom_limit) -> pos_integer();
         (build_type) -> opt | debug |
                         gcov | valgrind | gprof | lcnt | frmptr;
         (c_compiler_used) -> {atom(), term()};
         (check_io) -> [_];
         (compat_rel) -> integer();
         (cpu_topology) ->  CpuTopology when
      CpuTopology :: cpu_topology();
         ({cpu_topology, defined | detected | used}) -> CpuTopology when
      CpuTopology :: cpu_topology();
         (creation) -> integer();
         (debug_compiled) -> boolean();
         (delayed_node_table_gc) -> infinity | non_neg_integer();
         (dirty_cpu_schedulers) -> non_neg_integer();
         (dirty_cpu_schedulers_online) -> non_neg_integer();
         (dirty_io_schedulers) -> non_neg_integer();
         (dist) -> binary();
         (dist_buf_busy_limit) -> non_neg_integer();
         (dist_ctrl) -> [{Node :: node(),
                          ControllingEntity :: port() | pid()}];
         (driver_version) -> string();
         (dynamic_trace) -> none | dtrace | systemtap;
         (dynamic_trace_probes) -> boolean();
         (eager_check_io) -> boolean();
         (emu_flavor) -> emu | jit;
         (emu_type) -> opt | debug | gcov | valgrind | gprof | lcnt | frmptr;
         (end_time) -> non_neg_integer();
         (ets_count) -> pos_integer();
         (ets_limit) -> pos_integer();
         (fullsweep_after) -> {fullsweep_after, non_neg_integer()};
         (garbage_collection) -> [{atom(), integer()}];
         (heap_sizes) -> [non_neg_integer()];
         (heap_type) -> private;
         (info) -> binary();
         (kernel_poll) -> boolean();
         (loaded) -> binary();
         (logical_processors |
          logical_processors_available |
          logical_processors_online) -> unknown | pos_integer();
         (machine) -> string();
         (max_heap_size) -> {max_heap_size, MaxHeapSize :: max_heap_size()};
         (message_queue_data) -> message_queue_data();
         (min_heap_size) -> {min_heap_size, MinHeapSize :: pos_integer()};
         (min_bin_vheap_size) -> {min_bin_vheap_size,
                                  MinBinVHeapSize :: pos_integer()};
         (modified_timing_level) -> integer() | undefined;
         (multi_scheduling) -> disabled | blocked | blocked_normal | enabled;
         (multi_scheduling_blockers) -> [Pid :: pid()];
         (nif_version) -> string();
         (normal_multi_scheduling_blockers) -> [Pid :: pid()];
         (otp_release) -> string();
         (os_monotonic_time_source) -> [{atom(),term()}];
         (os_system_time_source) -> [{atom(),term()}];
         (outstanding_system_requests_limit) -> 1..134217727;
         (port_parallelism) -> boolean();
         (port_count) -> non_neg_integer();
         (port_limit) -> pos_integer();
         (process_count) -> pos_integer();
         (process_limit) -> pos_integer();
         (procs) -> binary();
         (scheduler_bind_type) -> spread |
                                  processor_spread |
                                  thread_spread |
                                  thread_no_node_processor_spread |
                                  no_node_processor_spread |
                                  no_node_thread_spread |
                                  no_spread |
                                  unbound;
         (scheduler_bindings) ->  tuple();
         (scheduler_id) -> SchedulerId :: pos_integer();
         (schedulers | schedulers_online) -> pos_integer();
         (smp_support) -> boolean();
         (start_time) -> integer();
         (system_architecture) -> string();
         (system_logger) -> logger | undefined | pid();
         (system_version) -> string();
         (threads) -> boolean();
         (thread_pool_size) -> non_neg_integer();
         (time_correction) -> true | false;
         (time_offset) -> preliminary | final | volatile;
         (time_warp_mode) -> no_time_warp | single_time_warp | multi_time_warp;
         (tolerant_timeofday) -> enabled | disabled;
         (trace_control_word) -> non_neg_integer();
         (update_cpu_info) -> changed | unchanged;
         (version) -> string();
         (wordsize | {wordsize, internal} | {wordsize, external}) -> 4 | 8;
         (async_dist) -> boolean();
         (overview) -> boolean();
         %% Deliberately left undocumented
         (sequential_tracer) -> {sequential_tracer, pid() | port() | {module(),term()} | false}.
system_info(_Item) ->
    erlang:nif_error(undefined).

-spec erlang:universaltime_to_localtime(Universaltime) ->  Localtime when
      Localtime :: calendar:datetime(),
      Universaltime :: calendar:datetime().
universaltime_to_localtime(_Universaltime) ->
    erlang:nif_error(undefined).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% End of  native code BIFs
%%% Actual Erlang implementation of some BIF's follow
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------------

%% Shadowed by erl_bif_types: erlang:apply/2
-spec apply(Fun, Args) -> term() when
      Fun :: function(),
      Args :: [term()].
apply(Fun, Args) ->
    erlang:apply(Fun, Args).

%% Shadowed by erl_bif_types: erlang:apply/3
-spec apply(Module, Function, Args) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
apply(Mod, Name, Args) ->
    erlang:apply(Mod, Name, Args).

%% Spawns with a fun

-spec spawn(Fun) -> pid() when
      Fun :: function().
spawn(F) when erlang:is_function(F) ->
    erlang:spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn(erlang, apply, [MF, []]);
spawn(F) ->
    badarg_with_info([F]).

-spec spawn(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn(N, F) when N =:= erlang:node() ->
    erlang:spawn(F);
spawn(N, F) when erlang:is_function(F) ->
    erlang:spawn(N, erlang, apply, [F, []]);
spawn(N, {M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn(N, erlang, apply, [MF, []]);
spawn(N, F) ->
    badarg_with_info([N, F]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when erlang:is_function(F) ->
    erlang:spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    badarg_with_info([F]).

-spec spawn_link(Node, Fun) -> pid() when
      Node :: node(),
      Fun :: function().
spawn_link(N, F) when N =:= erlang:node() ->
    spawn_link(F);
spawn_link(N, F) when erlang:is_function(F) ->
    spawn_link(N, erlang, apply, [F, []]);
spawn_link(N, {M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    spawn_link(N, erlang, apply, [MF, []]);
spawn_link(N, F) ->
    badarg_with_info([N, F]).

%% Spawn and atomically set up a monitor.

-spec spawn_monitor(Fun) -> {pid(), reference()} when
      Fun :: function().
spawn_monitor(F) when erlang:is_function(F, 0) ->
    erlang:spawn_opt(erlang,apply,[F,[]],[monitor]);
spawn_monitor(F) ->
    badarg_with_info([F]).

-spec spawn_monitor(Node, Fun) -> {pid(), reference()} when
      Node :: node(),
      Fun :: function().

spawn_monitor(Node, F) when erlang:is_atom(Node), erlang:is_function(F, 0) ->
    try
        erlang:spawn_monitor(Node,erlang,apply,[F,[]])
    catch
        error:Err ->
            error_with_info(Err, [Node, F])
    end;
spawn_monitor(Node, F) ->
    badarg_with_info([Node, F]).

-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(M, F, A) when erlang:is_atom(M),
                            erlang:is_atom(F),
                            erlang:is_list(A) ->
    erlang:spawn_opt(M,F,A,[monitor]);
spawn_monitor(M, F, A) ->
    badarg_with_info([M,F,A]).


-type max_heap_size() ::
        Size :: non_neg_integer()
        %% TODO change size => to := when -type maps support is finalized
      | #{ size => non_neg_integer(),
           kill => boolean(),
           error_logger => boolean(),
           include_shared_binaries => boolean() }.

-type spawn_opt_option() ::
	link
      | monitor
      | {monitor, MonitorOpts :: [monitor_option()]}
      | {priority, Level :: priority_level()}
      | {fullsweep_after, Number :: non_neg_integer()}
      | {min_heap_size, Size :: non_neg_integer()}
      | {min_bin_vheap_size, VSize :: non_neg_integer()}
      | {max_heap_size, Size :: max_heap_size()}
      | {message_queue_data, MQD :: message_queue_data()}
      | {async_dist, Enabled :: boolean()}.

-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()} when
      Fun :: function(),
      Options :: [spawn_opt_option()].
spawn_opt(F, O) when erlang:is_function(F) ->
    try
        erlang:spawn_opt(erlang, apply, [F, []], O)
    catch
        error:Error:Stk ->
            error_with_inherited_info(Error, [F,O], Stk)
    end;
spawn_opt({M,F}=MF, O) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn_opt(erlang, apply, [MF, []], O);
spawn_opt(F, O) ->
    badarg_with_info([F, O]).

-spec spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()} when
      Node :: node(),
      Fun :: function(),
      Options :: [monitor |
                  {monitor, [monitor_option()]} |
                  link |
                  OtherOption],
      OtherOption :: term().
spawn_opt(N, F, O) when N =:= erlang:node() ->
    try
        erlang:spawn_opt(F, O)
    catch
        error:Error:Stk ->
            error_with_inherited_info(Error, [N, F, O], Stk)
    end;
spawn_opt(N, F, O) when erlang:is_function(F, 0) ->
    try
        erlang:spawn_opt(N, erlang, apply, [F, []], O)
    catch
        error:Error:Stk -> error_with_inherited_info(Error, [N, F,O], Stk)
    end;
spawn_opt(N, {M,F}=MF, O) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn_opt(N, erlang, apply, [MF, []], O);
spawn_opt(N, F, O) ->
    badarg_with_info([N, F, O]).

%% Spawns with MFA

-spec spawn(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(N,M,F,A) when N =:= erlang:node(),
                    erlang:is_atom(M),
                    erlang:is_atom(F),
                    erlang:is_list(A) ->
    erlang:spawn(M,F,A);
spawn(N,M,F,A) when erlang:is_atom(N),
                    erlang:is_atom(M),
                    erlang:is_atom(F) ->
    try
        erlang:spawn_opt(N, M, F, A, [])
    catch
        _:Reason ->
            error_with_info(Reason, [N, M, F, A])
    end;
spawn(N,M,F,A) ->
    badarg_with_info([N, M, F, A]).

-spec spawn_link(Node, Module, Function, Args) -> pid() when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(N,M,F,A) when N =:= erlang:node(),
                         erlang:is_atom(M),
                         erlang:is_atom(F),
                         erlang:is_list(A) ->
    erlang:spawn_link(M,F,A);
spawn_link(N,M,F,A) when erlang:is_atom(N),
                         erlang:is_atom(M),
                         erlang:is_atom(F) ->
    try
        erlang:spawn_opt(N, M, F, A, [link])
    catch
        _:Reason ->
            error_with_info(Reason, [N, M, F, A])
    end;
spawn_link(N,M,F,A) ->
    badarg_with_info([N, M, F, A]).

-spec spawn_monitor(Node, Module, Function, Args) -> {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(N,M,F,A) when N =:= erlang:node(),
                            erlang:is_atom(M),
                            erlang:is_atom(F),
                            erlang:is_list(A) ->
    try
        erlang:spawn_monitor(M,F,A)
    catch
        error:Err ->
            error_with_info(Err, [N, M, F, A])
    end;
spawn_monitor(N,M,F,A) when erlang:is_atom(N),
                            erlang:is_atom(M),
                            erlang:is_atom(F) ->
    Ref = try
              erlang:spawn_request(N, M, F, A, [monitor])
          catch
              error:Err0 ->
                  error_with_info(Err0, [N, M, F, A])
          end,
    receive
        {spawn_reply, Ref, ok, Pid} when erlang:is_pid(Pid) ->
            {Pid, Ref};
        {spawn_reply, Ref, error, badopt} ->
            badarg_with_info([N, M, F, A]);
        {spawn_reply, Ref, error, noconnection} ->
            try 
                erlang:spawn_opt(erts_internal,crasher,
                                 [N,M,F,A,[monitor],
                                  noconnection],
                                 [monitor])
            catch
                _:Err1 ->
                    error_with_info(Err1, [N, M, F, A])
            end;
        {spawn_reply, Ref, error, Err2} ->
            error_with_info(Err2, [N, M, F, A])
    end;
spawn_monitor(N,M,F,A) ->
    badarg_with_info([N, M, F, A]).

-spec spawn_opt(Module, Function, Args, Options) ->
          Pid | {Pid, MonitorRef} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [spawn_opt_option()],
      Pid :: pid(),
      MonitorRef :: reference().
spawn_opt(_Module, _Function, _Args, _Options) ->
   erlang:nif_error(undefined).


-spec spawn_opt(Node, Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [monitor |
                  {monitor, [monitor_option()]} |
                  link |
                  OtherOption],
      OtherOption :: term().

spawn_opt(N, M, F, A, O) when N =:= erlang:node(),
			      erlang:is_atom(M), erlang:is_atom(F),
                              erlang:is_list(A), erlang:is_list(O) ->
    try
        erlang:spawn_opt(M, F, A, O)
    catch
        error:Error:Stk ->
            error_with_inherited_info(Error, [N, M, F, A, O], Stk)
    end;
spawn_opt(N, M, F, A, O) when erlang:is_atom(N),
                              erlang:is_atom(M),
                              erlang:is_atom(F) ->
    {Ref, MonOpt} = case erts_internal:dist_spawn_request(N, {M, F, A}, O, spawn_opt) of
                        {R, MO} when erlang:is_reference(R) -> {R, MO};
                        badarg -> badarg_with_info([N, M, F, A, O])
                    end,
    receive
        {spawn_reply, Ref, ok, Pid} when erlang:is_pid(Pid) ->
            case MonOpt of
                true -> {Pid, Ref};
                false -> Pid
            end;
        {spawn_reply, Ref, error, badopt} ->
            badarg_with_cause([N, M, F, A, O], badopt);
        {spawn_reply, Ref, error, Err0} when Err0 == noconnection;
                                             Err0 == notsup ->
            try
                erlang:spawn_opt(erts_internal,crasher,
                                 [N,M,F,A,O,Err0], O)
            catch
                _:Err1 ->
                    error_with_info(Err1, [N, M, F, A, O])
            end;
        {spawn_reply, Ref, error, Err2} ->
            error_with_info(Err2, [N, M, F, A, O])
    end;
spawn_opt(N,M,F,A,O) ->
    badarg_with_info([N,M,F,A,O]).
    
%%
%% spawn_request/1
%%

-spec spawn_request(Fun) -> ReqId when
      Fun :: function(),
      ReqId :: reference().

spawn_request(F) when erlang:is_function(F, 0) ->
    try
        erlang:spawn_request(erlang, apply, [F, []], [])
    catch
        error:Err ->
            error_with_info(Err, [F])
    end;
spawn_request(F) ->
    badarg_with_info([F]).

%%
%% spawn_request/2
%%

-spec spawn_request(Fun, Options) -> ReqId when
      Fun :: function(),
      Option :: {reply_tag, ReplyTag}
              | {reply, Reply}
              | spawn_opt_option(),
      ReplyTag :: term(),
      Reply :: yes | no | error_only | success_only,
      Options :: [Option],
      ReqId :: reference();
                   (Node, Fun) -> ReqId when
      Node :: node(),
      Fun :: function(),
      ReqId :: reference().

spawn_request(F, O) when erlang:is_function(F, 0) ->
    try
        erlang:spawn_request(erlang, apply, [F, []], O)
    catch
        error:Err:Stk ->
            error_with_inherited_info(Err, [F, O], Stk)
    end;
spawn_request(N, F) when erlang:is_function(F, 0) ->
    try
        erlang:spawn_request(N, erlang, apply, [F, []], [])
    catch
        error:Err:Stk ->
            error_with_inherited_info(Err, [N, F], Stk)
    end;
spawn_request(A1, A2) ->
    badarg_with_info([A1, A2]).

%%
%% spawn_request/3
%%

-spec spawn_request(Node, Fun, Options) -> ReqId when
      Node :: node(),
      Fun :: function(),
      Options :: [Option],
      Option :: monitor
              | {monitor, [monitor_option()]}
              | link
              | {reply_tag, ReplyTag}
              | {reply, Reply}
              | OtherOption,
      ReplyTag :: term(),
      Reply :: yes | no | error_only | success_only,
      OtherOption :: term(),
      ReqId :: reference();
                   (Module, Function, Args) ->
                           ReqId when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      ReqId :: reference().

spawn_request(N, F, O) when erlang:is_function(F, 0) ->
    try
        erlang:spawn_request(N, erlang, apply, [F, []], O)
    catch
        error:Err ->
            error_with_info(Err, [N, F, O])
    end;
spawn_request(M, F, A) ->
    try
        erlang:spawn_request(M, F, A, [])
    catch
        error:Err ->
            error_with_info(Err, [M, F, A])
    end.

%%
%% spawn_request/4
%%

-spec spawn_request(Node, Module, Function, Args) ->
                           ReqId when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      ReqId :: reference();
                   (Module, Function, Args, Options) ->
                           ReqId when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Option :: {reply_tag, ReplyTag}
              | {reply, Reply}
              | spawn_opt_option(),
      ReplyTag :: term(),
      Reply :: yes | no | error_only | success_only,
      Options :: [Option],
      ReqId :: reference().

spawn_request(N, M, F, A) when erlang:is_atom(F) ->
    try
        erlang:spawn_request(N, M, F, A, [])
    catch
        error:Err ->
            error_with_info(Err, [N, M, F, A])
    end;
spawn_request(M, F, A, O) ->
    case erts_internal:spawn_request(M, F, A, O) of
        Ref when erlang:is_reference(Ref) ->
            Ref;
        badopt ->
            badarg_with_cause([M, F, A, O], badopt);
        badarg ->
            badarg_with_info([M, F, A, O])
    end.

%%
%% spawn_request/5
%%

-spec spawn_request(Node, Module, Function, Args, Options) ->
                           ReqId when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: monitor
              | {monitor, [monitor_option()]}
              | link
              | {reply_tag, ReplyTag}
              | {reply, Reply}
              | OtherOption,
      ReplyTag :: term(),
      Reply :: yes | no | error_only | success_only,
      OtherOption :: term(),
      ReqId :: reference().

spawn_request(N, M, F, A, O) when N =:= erlang:node() ->
    try
        erlang:spawn_request(M, F, A, O)
    catch
        error:Err:Stk ->
            error_with_inherited_info(Err, [N, M, F, A, O], Stk)
    end;
spawn_request(N, M, F, A, O) ->
    case erts_internal:dist_spawn_request(N, {M, F, A}, O, spawn_request) of
        Ref when erlang:is_reference(Ref) ->
            Ref;
        badarg ->
            badarg_with_info([N, M, F, A, O])
    end.

-spec spawn_request_abandon(ReqId :: reference()) -> boolean().

spawn_request_abandon(_ReqId) ->
    erlang:nif_error(undefined).

-spec erlang:yield() -> 'true'.
yield() ->
    % This is not an infinite loop because erlang:yield() is
    % translated to an instruction by the loader
    erlang:yield().

-spec nodes() -> Nodes when
      Nodes :: [node()].
nodes() ->
    erlang:nif_error(undefined).

-spec nodes(Arg) -> Nodes when
      Arg :: NodeType | [NodeType],
      NodeType :: visible | hidden | connected | this | known,
      Nodes :: [node()].
nodes(_Arg) ->
    erlang:nif_error(undefined).

-spec nodes(Arg, InfoOpts) -> [NodeInfo] when
      NodeType :: visible | hidden | connected | this | known,
      Arg :: NodeType | [NodeType],
      InfoOpts :: #{connection_id => boolean(),
                    node_type => boolean()},
      NodeTypeInfo :: visible | hidden | this | known,
      ConnectionId :: undefined | integer(),
      Info :: #{connection_id => ConnectionId,
                node_type => NodeTypeInfo},
      NodeInfo :: {node(), Info}.

nodes(_Args, _Opts) ->
    erlang:nif_error(undefined).

-spec disconnect_node(Node) -> boolean() | ignored when
      Node :: node().
disconnect_node(Node) ->
    net_kernel:disconnect(Node).

-spec erlang:fun_info(Fun) -> [{Item, Info}] when
      Fun :: function(),
      Item :: arity | env | index | name
            | module | new_index | new_uniq | pid | type | uniq,
      Info :: term().
fun_info(Fun) when erlang:is_function(Fun) ->
    Keys = [type,env,arity,name,uniq,index,new_uniq,new_index,module,pid],
    fun_info_1(Keys, Fun, []);
fun_info(Fun) ->
    badarg_with_info([Fun]).

fun_info_1([K|Ks], Fun, A) ->
    case erlang:fun_info(Fun, K) of
	{K,undefined} -> fun_info_1(Ks, Fun, A);
	{K,_}=P -> fun_info_1(Ks, Fun, [P|A])
    end;
fun_info_1([], _, A) -> A.

-type send_destination() :: pid()
                          | reference()
                          | port()
                          | (RegName :: atom())
                          | {RegName :: atom(), Node :: node()}.

-spec erlang:send_nosuspend(Dest, Msg) -> boolean() when
      Dest :: send_destination(),
      Msg :: term().
send_nosuspend(Pid, Msg) ->
    try
        send_nosuspend(Pid, Msg, [])
    catch
        error:Error -> error_with_info(Error, [Pid, Msg])
    end.

-spec erlang:send_nosuspend(Dest, Msg, Options) -> boolean() when
      Dest :: send_destination(),
      Msg :: term(),
      Options :: [noconnect].
send_nosuspend(Pid, Msg, Opts) ->
    try erlang:send(Pid, Msg, [nosuspend|Opts]) of
	ok -> true;
	_  -> false
    catch
        error:Error:Stk ->
            error_with_inherited_info(Error, [Pid, Msg, Opts], Stk)
    end.

-spec erlang:localtime_to_universaltime(Localtime) -> Universaltime when
      Localtime :: calendar:datetime(),
      Universaltime :: calendar:datetime().
localtime_to_universaltime(Localtime) ->
    try
        erlang:localtime_to_universaltime(Localtime, undefined)
    catch
        error:Error ->
            error_with_info(Error, [Localtime])
    end.

%%
%% Port BIFs
%%
%%       Currently all port BIFs calls the corresponding
%%       erts_internal:port_*() native function which perform
%%       most of the actual work. These native functions should
%%       *never* be called directly by other functionality. The
%%       native functions may be changed, or removed without any
%%       notice whatsoever!
%%
%% IMPORTANT NOTE:
%%       When the erts_internal:port_*() native functions return
%%       a reference, they have also internally prepared the
%%       message queue of the caller for a receive that will
%%       unconditionally wait for a message containing this
%%       reference. If the erlang code calling these native
%%       functions do not do this, subsequent receives will not
%%       work as expected! That is, it is of *vital importance*
%%       that the receive is performed as described above!
%%

-spec port_command(Port, Data) -> 'true' when
      Port :: port() | atom(),
      Data :: iodata().

port_command(Port, Data) ->
    case case erts_internal:port_command(Port, Data, []) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	true -> true;
	Error -> error_with_info(Error, [Port, Data])
    end.

-spec port_command(Port, Data, OptionList) -> boolean() when
      Port :: port() | atom(),
      Data :: iodata(),
      Option :: force | nosuspend,
      OptionList :: [Option].

port_command(Port, Data, Flags) ->
    case case erts_internal:port_command(Port, Data, Flags) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
        badopt -> badarg_with_cause([Port, Data, Flags], badopt);
	Bool when erlang:is_boolean(Bool) -> Bool;
	Error -> error_with_info(Error, [Port, Data, Flags])
    end.

-spec port_connect(Port, Pid) -> 'true' when
      Port :: port() | atom(),
      Pid :: pid().

port_connect(Port, Pid) ->
    case case erts_internal:port_connect(Port, Pid) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	true -> true;
	Error -> error_with_info(Error, [Port, Pid])
    end.

-spec port_close(Port) -> 'true' when
      Port :: port() | atom().

port_close(Port) ->
    case case erts_internal:port_close(Port) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	true -> true;
	Error -> error_with_info(Error, [Port])
    end.

-spec port_control(Port, Operation, Data) -> iodata() | binary() when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: iodata().

port_control(Port, Operation, Data) ->
    case case erts_internal:port_control(Port, Operation, Data) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	badarg -> badarg_with_info([Port, Operation, Data]);
	Result -> Result
    end.

-spec erlang:port_call(Port, Data) -> term() when
      Port :: port() | atom(),
      Data :: term().

port_call(Port, Data) ->
    case case erts_internal:port_call(Port, 0, Data) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	{ok, Result} -> Result;
	Error -> error_with_info(Error, [Port, Data])
    end.

-spec erlang:port_call(Port, Operation, Data) -> term() when
      Port :: port() | atom(),
      Operation :: integer(),
      Data :: term().

port_call(Port, Operation, Data) ->
    case case erts_internal:port_call(Port, Operation, Data) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	{ok, Result} -> Result;
	Error -> error_with_info(Error, [Port, Operation, Data])
    end.

-spec erlang:port_info(Port) -> Result when
      Port :: port() | atom(),
      ResultItem :: {registered_name, RegisteredName :: atom()}
		  | {id, Index :: non_neg_integer()}
		  | {connected, Pid :: pid()}
		  | {links, Pids :: [pid()]}
		  | {name, String :: string()}
		  | {input, Bytes :: non_neg_integer()}
		  | {output, Bytes :: non_neg_integer()}
		  | {os_pid, OsPid :: non_neg_integer() | 'undefined'},
      Result :: [ResultItem] | 'undefined'.

port_info(Port) ->
    case case erts_internal:port_info(Port) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	badarg -> badarg_with_info([Port]);
	Result -> Result
    end.

-spec erlang:port_info(Port, connected) -> {connected, Pid} | 'undefined' when
      Port :: port() | atom(),
      Pid :: pid();
		      (Port, id) -> {id, Index} | 'undefined' when
      Port :: port() | atom(),
      Index :: non_neg_integer();
		      (Port, input) -> {input, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, links) -> {links, Pids} | 'undefined' when
      Port :: port() | atom(),
      Pids :: [pid()];
		      (Port, locking) -> {locking, Locking} | 'undefined' when
      Port :: port() | atom(),
      Locking :: 'false' | 'port_level' | 'driver_level';
		      (Port, memory) -> {memory, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, monitors) -> {monitors, Monitors} | 'undefined' when
      Port :: port() | atom(),
      Monitors :: [{process, pid()}];
		      (Port, monitored_by) -> {monitored_by, MonitoredBy} | 'undefined' when
      Port :: port() | atom(),
      MonitoredBy :: [pid()];
		      (Port, name) -> {name, Name} | 'undefined' when
      Port :: port() | atom(),
      Name :: string();
		      (Port, os_pid) -> {os_pid, OsPid} | 'undefined' when
      Port :: port() | atom(),
      OsPid :: non_neg_integer() | 'undefined';
		      (Port, output) -> {output, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, parallelism) -> {parallelism, Boolean} | 'undefined' when
      Port :: port() | atom(),
      Boolean :: boolean();
		      (Port, queue_size) -> {queue_size, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, registered_name) -> {registered_name, RegisteredName} | [] | 'undefined' when
      Port :: port() | atom(),
      RegisteredName :: atom().

port_info(Port, Item) ->
    case case erts_internal:port_info(Port, Item) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
        badtype -> badarg_with_cause([Port, Item], badtype);
	badarg -> badarg_with_info([Port, Item]);
	Result -> Result
    end.

-spec erlang:port_set_data(Port, Data) -> 'true' when
      Port :: port() | atom(),
      Data :: term().
    
port_set_data(_Port, _Data) ->
    erlang:nif_error(undefined).

-spec erlang:port_get_data(Port) -> term() when
      Port :: port() | atom().

port_get_data(_Port) ->
    erlang:nif_error(undefined).

%%
%% Distribution channel management
%%

-spec erlang:dist_ctrl_input_handler(DHandle, InputHandler) -> 'ok' when
      DHandle :: dist_handle(),
      InputHandler :: pid().

dist_ctrl_input_handler(_DHandle, _InputHandler) ->
    erlang:nif_error(undefined).

-spec erlang:dist_ctrl_put_data(DHandle, Data) -> 'ok' when
      DHandle :: dist_handle(),
      Data :: iodata().

dist_ctrl_put_data(_DHandle, _Data) ->
    erlang:nif_error(undefined).

-spec erlang:dist_ctrl_get_data(DHandle) -> {Size, Data} | Data | 'none' when
      Size :: non_neg_integer(),
      DHandle :: dist_handle(),
      Data :: iovec().

dist_ctrl_get_data(_DHandle) ->
    erlang:nif_error(undefined).

-spec erlang:dist_ctrl_get_data_notification(DHandle) -> 'ok' when
      DHandle :: dist_handle().

dist_ctrl_get_data_notification(_DHandle) ->
    erlang:nif_error(undefined).

-spec erlang:dist_ctrl_set_opt(DHandle, 'get_size', Value) -> OldValue when
      DHandle :: dist_handle(),
      Value :: boolean(),
      OldValue :: boolean().

dist_ctrl_set_opt(_DHandle, _Opt, _Val) ->
    erlang:nif_error(undefined).

-spec erlang:dist_ctrl_get_opt(DHandle, 'get_size') -> Value when
      DHandle :: dist_handle(),
      Value :: boolean().

dist_ctrl_get_opt(_DHandle, _Opt) ->
    erlang:nif_error(undefined).

-spec erlang:dist_get_stat(DHandle) -> Res when
      DHandle :: dist_handle(),
      InputPackets :: non_neg_integer(),
      OutputPackets :: non_neg_integer(),
      PendingOutputPackets :: non_neg_integer(),
      Res :: {'ok', InputPackets, OutputPackets, PendingOutputPackets}.

dist_get_stat(_DHandle) ->
    erlang:nif_error(undefined).


dmonitor_node(Node, _Flag, []) ->
    %% Only called when auto-connect attempt failed early in VM
    erlang:self() ! {nodedown, Node},
    true;
dmonitor_node(Node, Flag, Opts) ->
    case lists:member(allow_passive_connect, Opts) of
	true ->
	    case net_kernel:passive_cnct(Node) of
		true -> erlang:monitor_node(Node, Flag, Opts);
		false -> erlang:self() ! {nodedown, Node}, true
	    end;
	_ ->
	    dmonitor_node(Node,Flag,[])
    end.

%%
%% Trap function used when modified timing has been enabled.
%%

-spec erlang:delay_trap(Result, timeout()) -> Result.
delay_trap(Result, 0) -> erlang:yield(), Result;
delay_trap(Result, Timeout) -> receive after Timeout -> Result end.


-spec erlang:set_cookie(Cookie) -> true when
      Cookie :: atom().
set_cookie(C) when erlang:is_atom(C) ->
    auth:set_cookie(C);
set_cookie(C) ->
    badarg_with_info([C]).

-spec erlang:set_cookie(Node, Cookie) -> true when
      Node :: node(),
      Cookie :: atom().
set_cookie(Node, C)
  when Node =/= nonode@nohost, erlang:is_atom(Node), erlang:is_atom(C) ->
    auth:set_cookie(Node, C);
set_cookie(Node, C) ->
    badarg_with_info([Node, C]).


-spec erlang:get_cookie() -> Cookie | nocookie when
      Cookie :: atom().
get_cookie() ->
    auth:get_cookie().

-spec erlang:get_cookie(Node) -> Cookie | nocookie when
      Node :: node(),
      Cookie :: atom().
get_cookie(Node) when erlang:is_atom(Node) ->
    auth:get_cookie(Node);
get_cookie(Node) ->
    badarg_with_info([Node]).


-spec integer_to_list(Integer, Base) -> string() when
      Integer :: integer(),
      Base :: 2..36.
integer_to_list(_I, _Base) ->
    erlang:nif_error(undefined).

-spec integer_to_binary(Integer, Base) -> binary() when
      Integer :: integer(),
      Base :: 2..36.
integer_to_binary(_I, _Base) ->
    erlang:nif_error(undefined).


-record(cpu, {node = -1,
	      processor = -1,
	      processor_node = -1,
	      core = -1,
	      thread = -1,
	      logical = -1}).

%% erlang:set_cpu_topology/1 is for internal use only!
%%
%% erlang:system_flag(cpu_topology, CpuTopology) traps to 
%% erlang:set_cpu_topology(CpuTopology).
set_cpu_topology(CpuTopology) ->
    try format_cpu_topology(erlang:system_flag(internal_cpu_topology,
					       cput_e2i(CpuTopology)))
    catch
	Class:Exception when Class =/= error; Exception =/= internal_error -> 
	    badarg_with_info([CpuTopology])
    end.

cput_e2i_clvl({logical, _}, _PLvl) ->
    #cpu.logical;
cput_e2i_clvl([E | _], PLvl) ->
    case erlang:element(1, E) of
	node -> case PLvl of
		    0 -> #cpu.node;
		    #cpu.processor -> #cpu.processor_node
		end;
	processor -> case PLvl of
			 0 -> #cpu.node;
			 #cpu.node -> #cpu.processor
		     end;
	core -> #cpu.core;
	thread -> #cpu.thread
    end.

cput_e2i(undefined) ->
    undefined;
cput_e2i(E) ->
    rvrs(cput_e2i(E, -1, -1, #cpu{}, 0, cput_e2i_clvl(E, 0), [])).

cput_e2i([], _NId, _PId, _IS, _PLvl, _Lvl, Res) -> 
    Res;
cput_e2i([E|Es], NId0, PId, IS, PLvl, Lvl, Res0) -> 
    case cput_e2i(E, NId0, PId, IS, PLvl, Lvl, Res0) of
	[] ->
	    cput_e2i(Es, NId0, PId, IS, PLvl, Lvl, Res0);
	[#cpu{node = N,
	      processor = P,
	      processor_node = PN} = CPU|_] = Res1 ->
	    NId1 = case N > PN of
			 true -> N;
			 false -> PN
		     end,
	    cput_e2i(Es, NId1, P, CPU, PLvl, Lvl, Res1)
    end;
cput_e2i({Tag, [], TagList}, Nid, PId, CPU, PLvl, Lvl, Res) ->
    %% Currently [] is the only valid InfoList
    cput_e2i({Tag, TagList}, Nid, PId, CPU, PLvl, Lvl, Res);
cput_e2i({node, NL}, Nid0, PId, _CPU, 0, #cpu.node, Res) ->
    Nid1 = Nid0+1,
    Lvl = cput_e2i_clvl(NL, #cpu.node),
    cput_e2i(NL, Nid1, PId, #cpu{node = Nid1}, #cpu.node, Lvl, Res);
cput_e2i({processor, PL}, Nid, PId0, _CPU, 0, #cpu.node, Res) ->
    PId1 = PId0+1,
    Lvl = cput_e2i_clvl(PL, #cpu.processor),
    cput_e2i(PL, Nid, PId1, #cpu{processor = PId1}, #cpu.processor, Lvl, Res);
cput_e2i({processor, PL}, Nid, PId0, CPU, PLvl, CLvl, Res)
  when PLvl < #cpu.processor, CLvl =< #cpu.processor ->
    PId1 = PId0+1,
    Lvl = cput_e2i_clvl(PL, #cpu.processor),
    cput_e2i(PL, Nid, PId1, CPU#cpu{processor = PId1,
				    processor_node = -1,
				    core = -1,
				    thread = -1}, #cpu.processor, Lvl, Res);
cput_e2i({node, NL}, Nid0, PId, CPU, #cpu.processor, #cpu.processor_node,
	 Res) ->
    Nid1 = Nid0+1,
    Lvl = cput_e2i_clvl(NL, #cpu.processor_node),
    cput_e2i(NL, Nid1, PId, CPU#cpu{processor_node = Nid1},
	     #cpu.processor_node, Lvl, Res);
cput_e2i({core, CL}, Nid, PId, #cpu{core = C0} = CPU, PLvl, #cpu.core, Res)
  when PLvl < #cpu.core ->
    Lvl = cput_e2i_clvl(CL, #cpu.core),
    cput_e2i(CL, Nid, PId, CPU#cpu{core = C0+1, thread = -1}, #cpu.core, Lvl,
	     Res);
cput_e2i({thread, TL}, Nid, PId, #cpu{thread = T0} = CPU, PLvl, #cpu.thread,
	 Res) when PLvl < #cpu.thread ->
    Lvl = cput_e2i_clvl(TL, #cpu.thread),
    cput_e2i(TL, Nid, PId, CPU#cpu{thread = T0+1}, #cpu.thread, Lvl, Res);
cput_e2i({logical, ID}, _Nid, PId, #cpu{processor=P, core=C, thread=T} = CPU,
	 PLvl, #cpu.logical, Res)
  when PLvl < #cpu.logical, erlang:is_integer(ID), 0 =< ID, ID < 65536 ->
    [CPU#cpu{processor = case P of -1 -> PId+1; _ -> P end,
	     core = case C of -1 -> 0; _ -> C end,
	     thread = case T of -1 -> 0; _ -> T end,
	     logical = ID} | Res].

%% erlang:format_cpu_topology/1 is for internal use only!
%%
%% erlang:system_info(cpu_topology),
%% and erlang:system_info({cpu_topology, Which}) traps to
%% erlang:format_cpu_topology(InternalCpuTopology).
format_cpu_topology(InternalCpuTopology) ->
    try cput_i2e(InternalCpuTopology)
    catch _ : _ -> erlang:error(internal_error, [InternalCpuTopology])
    end.


cput_i2e(undefined) -> undefined;
cput_i2e(Is) -> cput_i2e(Is, true, #cpu.node, cput_i2e_tag_map()).

cput_i2e([], _Frst, _Lvl, _TM) ->
    [];
cput_i2e([#cpu{logical = LID}| _], _Frst, Lvl, _TM) when Lvl == #cpu.logical ->
    {logical, LID};
cput_i2e([#cpu{} = I | Is], Frst, Lvl, TM) ->
    cput_i2e(erlang:element(Lvl, I), Frst, Is, [I], Lvl, TM).

cput_i2e(V, Frst, [I | Is], SameV, Lvl, TM) when V =:= erlang:element(Lvl, I) ->
    cput_i2e(V, Frst, Is, [I | SameV], Lvl, TM);
cput_i2e(-1, true, [], SameV, Lvl, TM) ->
    cput_i2e(rvrs(SameV), true, Lvl+1, TM);
cput_i2e(_V, true, [], SameV, Lvl, TM) when Lvl =/= #cpu.processor,
                                            Lvl =/= #cpu.processor_node ->
    cput_i2e(rvrs(SameV), true, Lvl+1, TM);
cput_i2e(-1, _Frst, Is, SameV, #cpu.node, TM) ->
    cput_i2e(rvrs(SameV), true, #cpu.processor, TM)
	++ cput_i2e(Is, false, #cpu.node, TM);
cput_i2e(_V, _Frst, Is, SameV, Lvl, TM) ->
    [{cput_i2e_tag(Lvl, TM), cput_i2e(rvrs(SameV), true, Lvl+1, TM)}
     | cput_i2e(Is, false, Lvl, TM)].

cput_i2e_tag_map() -> erlang:list_to_tuple([cpu | record_info(fields, cpu)]).

cput_i2e_tag(Lvl, TM) ->
    case erlang:element(Lvl, TM) of processor_node -> node; Other -> Other end.

rvrs([_] = L) -> L;
rvrs(Xs) -> rvrs(Xs, []).

rvrs([],Ys) -> Ys;
rvrs([X|Xs],Ys) -> rvrs(Xs, [X|Ys]).

%% Shadowed by erl_bif_types: erlang:min/2
-spec min(Term1, Term2) -> Minimum when
      Term1 :: term(),
      Term2 :: term(),
      Minimum :: term().
%% In Erlang/OTP 26, min/2 is a guard BIF. This implementation is kept
%% for backward compatibility with code compiled with an earlier version.
min(A, B) when A > B -> B;
min(A, _) -> A.

%% Shadowed by erl_bif_types: erlang:max/2
-spec max(Term1, Term2) -> Maximum when
      Term1 :: term(),
      Term2 :: term(),
      Maximum :: term().
%% In Erlang/OTP 26, max/2 is a guard BIF. This implementation is kept
%% for backward compatibility with code compiled with an earlier version.
max(A, B) when A < B -> B;
max(A, _) -> A.


%%
%% erlang:memory/[0,1]
%%
%% NOTE! When updating these functions, make sure to also update
%%       erts_memory() in $ERL_TOP/erts/emulator/beam/erl_alloc.c
%%

-type memory_type() :: 'total' | 'processes' | 'processes_used' | 'system'
                     | 'atom' | 'atom_used' | 'binary' | 'code' | 'ets'.

-define(CARRIER_ALLOCS, [mseg_alloc]).
-define(ALL_NEEDED_ALLOCS, (erlang:system_info(alloc_util_allocators)
			    -- ?CARRIER_ALLOCS)).

-record(memory, {total = 0,
		 processes = 0,
		 processes_used = 0,
		 system = 0,
		 atom = 0,
		 atom_used = 0,
		 binary = 0,
		 code = 0,
		 ets = 0}).

-spec erlang:memory() -> [{Type, Size}] when
      Type :: memory_type(),
      Size :: non_neg_integer().
memory() ->
    case aa_mem_data(au_mem_data(?ALL_NEEDED_ALLOCS)) of
	notsup ->
	    erlang:error(notsup);
	Mem ->
	    [{total, Mem#memory.total},
	     {processes, Mem#memory.processes},
	     {processes_used, Mem#memory.processes_used},
	     {system, Mem#memory.system},
	     {atom, Mem#memory.atom},
	     {atom_used, Mem#memory.atom_used},
	     {binary, Mem#memory.binary},
	     {code, Mem#memory.code},
	     {ets, Mem#memory.ets}]
    end.

-spec erlang:memory(Type :: memory_type()) -> non_neg_integer();
                   (TypeList :: [memory_type()]) -> [{memory_type(), non_neg_integer()}].
memory(Type) when erlang:is_atom(Type) ->
    try
        case aa_mem_data(au_mem_data(?ALL_NEEDED_ALLOCS)) of
            notsup -> error_with_info(notsup, [Type]);
            Mem -> get_memval(Type, Mem)
        end
    catch
        error:badarg -> badarg_with_info([Type])
    end;
memory(Types) when erlang:is_list(Types) ->
    try
        case aa_mem_data(au_mem_data(?ALL_NEEDED_ALLOCS)) of
            notsup -> error_with_info(notsup, [Types]);
            Mem -> memory_1(Types, Mem)
        end
    catch
        error:badarg -> badarg_with_info([Types])
    end;
memory(Arg) ->
    badarg_with_info([Arg]).

memory_1([Type | Types], Mem) ->
    [{Type, get_memval(Type, Mem)} | memory_1(Types, Mem)];
memory_1([], _Mem) ->
    [].

get_memval(total, #memory{total = V}) -> V;
get_memval(processes, #memory{processes = V}) -> V;
get_memval(processes_used, #memory{processes_used = V}) -> V;
get_memval(system, #memory{system = V}) -> V;
get_memval(atom, #memory{atom = V}) -> V;
get_memval(atom_used, #memory{atom_used = V}) -> V;
get_memval(binary, #memory{binary = V}) -> V;
get_memval(code, #memory{code = V}) -> V;
get_memval(ets, #memory{ets = V}) -> V;
get_memval(_, #memory{}) -> erlang:error(badarg).

get_fix_proc([{ProcType, A1, U1}| Rest], {A0, U0}) when ProcType == proc;
							ProcType == monitor;
							ProcType == link;
							ProcType == msg_ref;
							ProcType == ll_ptimer;
							ProcType == hl_ptimer;
							ProcType == bif_timer;
							ProcType == accessor_bif_timer ->
    get_fix_proc(Rest, {A0+A1, U0+U1});
get_fix_proc([_|Rest], Acc) ->
    get_fix_proc(Rest, Acc);
get_fix_proc([], Acc) ->
    Acc.

fix_proc([{fix_types, SizeList} | _Rest], Acc) ->
    get_fix_proc(SizeList, Acc);
fix_proc([{fix_types, Mask, SizeList} | _Rest], Acc) ->
    {A, U} = get_fix_proc(SizeList, Acc),
    {Mask, A, U};
fix_proc([_ | Rest], Acc) ->
    fix_proc(Rest, Acc);
fix_proc([], Acc) ->
    Acc.

au_mem_fix(#memory{ processes = Proc,
                    processes_used = ProcU } = Mem, Data) ->
    case fix_proc(Data, {0, 0}) of
        {A, U} ->
            Mem#memory{ processes = Proc+A,
                        processes_used = ProcU+U };
        {Mask, A, U} ->
            Mem#memory{ processes = Mask band (Proc+A),
                        processes_used = Mask band (ProcU+U) }
    end.

au_mem_acc(#memory{ total = Tot,
                    processes = Proc,
                    processes_used = ProcU } = Mem,
           eheap_alloc, Data) ->
    Sz = acc_blocks_size(Data, 0),
    Mem#memory{ total = Tot+Sz,
                processes = Proc+Sz,
                processes_used = ProcU+Sz};
au_mem_acc(#memory{ total = Tot,
                    ets = Ets } = Mem,
           ets_alloc, Data) ->
    Sz = acc_blocks_size(Data, 0),
    Mem#memory{ total = Tot+Sz,
                ets = Ets+Sz };
au_mem_acc(#memory{total = Tot,
		    binary = Bin } = Mem,
	    binary_alloc, Data) ->
    Sz = acc_blocks_size(Data, 0),
    Mem#memory{ total = Tot+Sz,
                binary = Bin+Sz};
au_mem_acc(#memory{ total = Tot } = Mem,
           _Type, Data) ->
    Sz = acc_blocks_size(Data, 0),
    Mem#memory{ total = Tot+Sz }.

acc_blocks_size([{size, Sz, _, _} | Rest], Acc) ->
    acc_blocks_size(Rest, Acc+Sz);
acc_blocks_size([{size, Sz} | Rest], Acc) ->
    acc_blocks_size(Rest, Acc+Sz);
acc_blocks_size([_ | Rest], Acc) ->
    acc_blocks_size(Rest, Acc);
acc_blocks_size([], Acc) ->
    Acc.

au_mem_blocks([{blocks, L} | Rest], Mem0) ->
    Mem = au_mem_blocks_1(L, Mem0),
    au_mem_blocks(Rest, Mem);
au_mem_blocks([_ | Rest], Mem) ->
    au_mem_blocks(Rest, Mem);
au_mem_blocks([], Mem) ->
    Mem.

au_mem_blocks_1([{Type, SizeList} | Rest], Mem) ->
    au_mem_blocks_1(Rest, au_mem_acc(Mem, Type, SizeList));
au_mem_blocks_1([], Mem) ->
    Mem.

au_mem_current(Mem, Type, [{mbcs_pool, Stats} | Rest]) ->
    au_mem_current(au_mem_blocks(Stats, Mem), Type, Rest);
au_mem_current(Mem, Type, [{mbcs, Stats} | Rest]) ->
    au_mem_current(au_mem_blocks(Stats, Mem), Type, Rest);
au_mem_current(Mem, Type, [{sbcs, Stats} | Rest]) ->
    au_mem_current(au_mem_blocks(Stats, Mem), Type, Rest);
au_mem_current(Mem, Type, [_ | Rest]) ->
    au_mem_current(Mem, Type, Rest);
au_mem_current(Mem, _Type, []) ->
    Mem.

au_mem_data(notsup, _) ->
    notsup;
au_mem_data(_, [{_, false} | _]) ->
    notsup;
au_mem_data(#memory{} = Mem0, [{fix_alloc, _, Data} | Rest]) ->
    Mem = au_mem_fix(Mem0, Data),
    au_mem_data(au_mem_current(Mem, fix_alloc, Data), Rest);
au_mem_data(#memory{} = Mem, [{Type, _, Data} | Rest]) ->
    au_mem_data(au_mem_current(Mem, Type, Data), Rest);
au_mem_data(EMD, []) ->
    EMD.

au_mem_data(Allocs) ->
    Ref = erlang:make_ref(),
    erlang:system_info({memory_internal, Ref, Allocs}),
    receive_emd(Ref).

receive_emd(_Ref, EMD, 0) ->
    EMD;
receive_emd(Ref, EMD, N) ->
    receive
	{Ref, _, Data} ->
	    receive_emd(Ref, au_mem_data(EMD, Data), N-1)
    end.

receive_emd(Ref) ->
    receive_emd(Ref, #memory{}, erts_internal:no_aux_work_threads()-1).

aa_mem_data(#memory{total = Tot} = Mem,
	    [{external_alloc, Sz} | Rest]) ->
    %% Externally allocated data, this is not a part of alloc_util so we must
    %% bump the total memory size.
    aa_mem_data(Mem#memory{total = Tot + Sz},
		Rest);
aa_mem_data(#memory{atom = Atom,
		    atom_used = AtomU} = Mem,
	    [{atom_space, Alloced, Used} | Rest]) ->
    aa_mem_data(Mem#memory{atom = Atom+Alloced,
			   atom_used = AtomU+Used},
		Rest);
aa_mem_data(#memory{atom = Atom,
		    atom_used = AtomU} = Mem,
	    [{atom_table, Sz} | Rest]) ->
    aa_mem_data(Mem#memory{atom = Atom+Sz,
			   atom_used = AtomU+Sz},
		Rest);
aa_mem_data(#memory{ets = Ets} = Mem,
	    [{ets_misc, Sz} | Rest]) ->
    aa_mem_data(Mem#memory{ets = Ets+Sz},
		Rest);
aa_mem_data(#memory{processes = Proc,
		    processes_used = ProcU } = Mem,
	    [{ProcData, Sz} | Rest]) when ProcData == bif_timer;
					  ProcData == process_table ->
    aa_mem_data(Mem#memory{processes = Proc+Sz,
			   processes_used = ProcU+Sz },
		Rest);
aa_mem_data(#memory{code = Code} = Mem,
	    [{CodeData, Sz} | Rest]) when CodeData == module_table;
					  CodeData == export_table;
					  CodeData == export_list;
					  CodeData == fun_table;
					  CodeData == module_refs;
					  CodeData == loaded_code ->
    aa_mem_data(Mem#memory{code = Code+Sz},
		Rest);
aa_mem_data(EMD, [{_, _} | Rest]) ->
    aa_mem_data(EMD, Rest);
aa_mem_data(#memory{ total = Tot,
                     processes = Proc } = Mem,
            []) ->
    Mem#memory{system = Tot - Proc}.

aa_mem_data(notsup) ->
    notsup;
aa_mem_data(EMD) ->
    aa_mem_data(EMD, erlang:system_info(allocated_areas)).

%%
%% alloc_info/1 and alloc_sizes/1 are for internal use only (used by
%% erlang:system_info({allocator|allocator_sizes, _})).
%%

alloc_info(Allocs) ->
    get_alloc_info(allocator, Allocs).

alloc_sizes(Allocs) ->
    get_alloc_info(allocator_sizes, Allocs).

get_alloc_info(Type, AAtom) when erlang:is_atom(AAtom) ->
    [{AAtom, Result}] = get_alloc_info(Type, [AAtom]),
    Result;
get_alloc_info(Type, AList) when erlang:is_list(AList) ->
    Ref = erlang:make_ref(),
    erlang:system_info({Type, Ref, AList}),
    receive_allocator(Ref,
		      erts_internal:no_aux_work_threads()-1,
		      mk_res_list(AList)).

mk_res_list([]) ->
    [];
mk_res_list([Alloc | Rest]) ->
    [{Alloc, []} | mk_res_list(Rest)].

insert_instance(I, N, Rest) when erlang:is_atom(N) ->
    [{N, I} | Rest];
insert_instance(I, N, []) ->
    [{instance, N, I}];
insert_instance(I, N, [{instance, M, _}|_] = Rest) when N < M ->
    [{instance, N, I} | Rest];
insert_instance(I, N, [Prev|Rest]) ->
    [Prev | insert_instance(I, N, Rest)].

insert_info([], Ys) ->
    Ys;
insert_info([{A, false}|Xs], [{A, _IList}|Ys]) ->
    insert_info(Xs, [{A, false}|Ys]);
insert_info([{A, N, I}|Xs], [{A, IList}|Ys]) ->
    insert_info(Xs, [{A, insert_instance(I, N, IList)}|Ys]);
insert_info([{A1, _}|_] = Xs, [{A2, _} = Y | Ys]) when A1 /= A2 ->
    [Y | insert_info(Xs, Ys)];
insert_info([{A1, _, _}|_] = Xs, [{A2, _} = Y | Ys]) when A1 /= A2 ->
    [Y | insert_info(Xs, Ys)].

receive_allocator(_Ref, 0, Acc) ->
    Acc;
receive_allocator(Ref, N, Acc) ->
    receive
	{Ref, _, InfoList} ->
	    receive_allocator(Ref, N-1, insert_info(InfoList, Acc))
    end.

-spec erlang:gather_gc_info_result(Ref) ->
   {number(),number(),0} when Ref :: reference().

gather_gc_info_result(Ref) when erlang:is_reference(Ref) ->
    gc_info(Ref, erlang:system_info(schedulers), {0,0}).

gc_info(_Ref, 0, {Colls,Recl}) ->
    {Colls,Recl,0};
gc_info(Ref, N, {OrigColls,OrigRecl}) ->
    receive
	{Ref, {_,Colls, Recl}} -> 
	    gc_info(Ref, N-1, {Colls+OrigColls,Recl+OrigRecl})
    end.

%% Operators

-spec erlang:'=='(term(), term()) -> boolean().
'=='(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'=:='(term(), term()) -> boolean().
'=:='(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'/='(term(), term()) -> boolean().
'/='(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'=/='(term(), term()) -> boolean().
'=/='(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'=<'(term(), term()) -> boolean().
'=<'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'>='(term(), term()) -> boolean().
'>='(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'<'(term(), term()) -> boolean().
'<'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'>'(term(), term()) -> boolean().
'>'(_A, _B) ->
    erlang:nif_error(undefined).

-spec erlang:'-'(number()) -> number().
'-'(_A) ->
    erlang:nif_error(undefined).
-spec erlang:'+'(number()) -> number().
'+'(_A) ->
    erlang:nif_error(undefined).
-spec erlang:'-'(number(), number()) -> number().
'-'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'+'(number(), number()) -> number().
'+'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'/'(number(), number()) -> float().
'/'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'*'(number(), number()) -> number().
'*'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'div'(integer(), integer()) -> integer().
'div'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'rem'(integer(), integer()) -> integer().
'rem'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'bsl'(integer(), integer()) -> integer().
'bsl'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'bsr'(integer(), integer()) -> integer().
'bsr'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'bor'(integer(), integer()) -> integer().
'bor'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'band'(integer(), integer()) -> integer().
'band'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'bxor'(integer(), integer()) -> integer().
'bxor'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'bnot'(integer()) -> integer().
'bnot'(_A) ->
    erlang:nif_error(undefined).

-spec erlang:'--'(list(), list()) -> list().
'--'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'++'(list(), term()) -> term().
'++'(_A, _B) ->
    erlang:nif_error(undefined).

-spec erlang:'and'(boolean(), boolean()) -> boolean().
'and'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'or'(boolean(), boolean()) -> boolean().
'or'(_A, _B) ->
    erlang:nif_error(undefined).

-spec erlang:'xor'(boolean(), boolean()) -> boolean().
'xor'(_A, _B) ->
    erlang:nif_error(undefined).
-spec erlang:'not'(boolean()) -> boolean().
'not'(_A) ->
    erlang:nif_error(undefined).

-spec erlang:'!'(send_destination(), term()) -> term().
'!'(_Dst, _Msg) ->
    erlang:nif_error(undefined).

%% Make sure that we have loaded the tracer module.
ensure_tracer_module_loaded(Flag, FlagList) ->
    try lists:keyfind(Flag, 1, FlagList) of
        {Flag, Module, State} when erlang:is_atom(Module) ->
            case erlang:module_loaded(Module) of
                false ->
                    Module:enabled(trace_status, erlang:self(), State);
                true ->
                    ok
            end;
        _ ->
            ok
    catch
        _:_ ->
            ok
    end.

error_with_inherited_info(Reason, Args, [{_,_,_,ExtraInfo}|_]) ->
    %% We KNOW that lists:keyfind/3 is a BIF and is therefore safe to call.
    case lists:keyfind(error_info, 1, ExtraInfo) of
        {error_info,_}=ErrorInfoTuple ->
            erlang:error(Reason, Args, [ErrorInfoTuple]);
        false ->
            erlang:error(Reason, Args, [{error_info, #{module => erl_erts_errors}}])
    end.

error_with_info(Reason, Args) ->
    erlang:error(Reason, Args, [{error_info, #{module => erl_erts_errors}}]).

badarg_with_info(Args) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_erts_errors}}]).

badarg_with_cause(Args, Cause) ->
    erlang:error(badarg, Args, [{error_info, #{module => erl_erts_errors,
                                              cause => Cause}}]).
