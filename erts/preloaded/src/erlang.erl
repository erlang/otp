%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2025. All Rights Reserved.
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
-moduledoc """
The Erlang BIFs and predefined types.

By convention, most [Built-In Functions](`e:system:ref_man_functions.md#built-in-functions-bifs`)
(BIFs) and all [predefined types](`e:system:typespec.md#predefined`) are included
in this module. Some of the BIFs and all of the predefined types are viewed more
or less as part of the Erlang programming language and are _auto-imported_.
Thus, it is not necessary to specify the module name. For example, the calls
[`atom_to_list(erlang)`](`atom_to_list/1`) and [`erlang:atom_to_list(erlang)`](`atom_to_list/1`)
are identical.

Auto-imported BIFs are annotated with `auto-imported` and predefined types are
annotated with `predefined`.

Some auto-imported BIFs are also allowed in [guard expression](`e:system:expressions.md#guard-expressions`).
Such BIFs are annoted with both `auto-imported` and `guard-bif`.

BIFs can fail for various reasons. All BIFs fail with reason `badarg` if they
are called with arguments of an incorrect type. The other reasons are described
in the description of each individual BIF.
""".

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
-doc "All possible Erlang terms. Synonym for `t:term/0`.".
-type any() :: any().
-doc "The arity of a function or type.".
-type arity() :: arity().
-doc "An Erlang [atom](`e:system:data_types.md#atom`).".
-type atom() :: atom().
-doc """
An Erlang [binary](`e:system:data_types.md#bit-strings-and-binaries`), that is,
a bitstring with a size divisible by 8.
""".
-type binary() :: <<_:_*8>>.
-doc "An Erlang [bitstring](`e:system:data_types.md#bit-strings-and-binaries`).".
-type bitstring() :: <<_:_*1>>.
-doc false.
-type bool() :: boolean().
-doc "A [boolean](`e:system:data_types.md#boolean`) value.".
-type boolean() :: true | false.
-doc "A byte of data represented by an integer.".
-type byte() :: 0..255.
-doc "An ASCII character or a `m:unicode` codepoint presented by an integer.".
-type char() :: 0..16#10FFFF.
-doc "The [dynamic](`e:system:typespec.md#dynamic`) type, which represents a statically unknown type".
-type dynamic() :: dynamic().
-doc "An Erlang [float](`e:system:data_types.md#number`).".
-type float() :: float().
-doc "An Erlang [fun](`e:system:data_types.md#fun`).".
-type function() :: fun().
-doc """
An unique identifier for some entity, for example a
[process](`e:system:ref_man_processes.md`), [port](`e:system:ports.md#ports`) or
[monitor](`monitor/2`).
""".
-type identifier() :: pid() | port() | reference().
-doc "An Erlang [integer](`e:system:data_types.md#number`).".
-type integer() :: integer().
-doc """
A binary or list containing bytes and/or iodata.

This datatype is used to represent data that is meant to be output using
any I/O module. For example: `file:write/2` or `gen_tcp:send/2`.

To convert an `t:iodata/0` term to `t:binary/0` you can use
[iolist_to_binary/2](`iolist_to_binary/1`). To transcode a `t:string/0` or
`t:unicode:chardata/0` to `t:iodata/0` you can use `unicode:characters_to_binary/1`.
""".
-type iodata() :: iolist() | binary().
-doc """
A list containing bytes and/or iodata.

This datatype is used to represent data that is meant to be output using any
I/O module. For example: `file:write/2` or `gen_tcp:send/2`.

In most use cases you want to use `t:iodata/0` instead of this type.
""".
-type iolist() :: maybe_improper_list(byte() | binary() | iolist(), binary() | []).
-doc "An Erlang [list](`e:system:data_types.md#list`) containing terms of any type.".
-type list() :: [any()].
-doc """
An Erlang [list](`e:system:data_types.md#list`) containing terms of the type
`ContentType`.
""".
-type list(ContentType) :: [ContentType].
-doc """
An Erlang [map](`e:system:data_types.md#map`) containing any number of key and
value associations.
""".
-type map() :: #{ any() => any() }.
-doc """
An Erlang [list](`e:system:data_types.md#list`) that is not guaranteed to end
with a [`[]`](`t:nil/0`), and where the list elements can be of any type.
""".
-type maybe_improper_list() :: maybe_improper_list(any(), any()).
-doc """
An Erlang [list](`e:system:data_types.md#list`), that is not guaranteed to end
with a [`[]`](`t:nil/0`), and where the list elements are of the type
`ContentType`.
""".
-type maybe_improper_list(ContentType, TerminationType) ::
        maybe_improper_list(ContentType, TerminationType).
-doc "A three-tuple representing a `Module:Function/Arity` function signature.".
-type mfa() :: {module(),atom(),arity()}.
-doc "An Erlang module represented by an atom.".
-type module() :: atom().
-doc "A negative integer.".
-type neg_integer() :: neg_integer().
-doc "The empty `t:list/0`.".
-type nil() :: [].
-doc """
The type used to show that a function will _never_ return a value, that is it
will _always_ throw an exception.
""".
-type no_return() :: none().
-doc "An Erlang [node](`e:system:distributed.md#nodes`) represented by an atom.".
-type node() :: atom().
-doc "A non-negative integer, that is any positive integer or 0.".
-type non_neg_integer() :: non_neg_integer().
-doc """
This type is used to show that a function will _never_ return a value; that is
it will _always_ throw an exception.

In a spec, use `t:no_return/0` for the sake of clarity.
""".
-type none() :: none().
-doc "A `t:binary/0` that contains some data.".
-type nonempty_binary() :: <<_:8, _:_*8>>.
-doc "A `t:bitstring/0` that contains some data.".
-type nonempty_bitstring() :: <<_:1, _:_*1>>.
-doc "A [maybe_improper_list/2](`t:maybe_improper_list/0`) that contains some items.".
-type nonempty_improper_list(ContentType, TerminationType) ::
        nonempty_improper_list(ContentType, TerminationType).
-doc "A `t:list/0` that contains some items.".
-type nonempty_list() :: nonempty_list(any()).
-doc "A [list(ContentType)](`t:list/0`) that contains some items.".
-type nonempty_list(ContentType) :: [ContentType, ...].
-doc "A `t:maybe_improper_list/0` that contains some items.".
-type nonempty_maybe_improper_list() :: nonempty_maybe_improper_list(any(), any()).
-doc """
A [maybe_improper_list(ContentType, TerminationType)](`t:maybe_improper_list/0`)
that contains some items.
""".
-type nonempty_maybe_improper_list(ContentType, TerminationType) :: nonempty_maybe_improper_list(ContentType, TerminationType).
-doc "A `t:string/0` that contains some characters.".
-type nonempty_string() :: nonempty_list(char()).
-doc "An Erlang [number](`e:system:data_types.md#number`).".
-type number() :: integer() | float().
-doc "An Erlang [process identifier](`e:system:data_types.md#pid`).".
-type pid() :: pid().
-doc "An Erlang [port identifier](`e:system:data_types.md#port-identifier`).".
-type port() :: port().
-doc "An integer greater than zero.".
-type pos_integer() :: pos_integer().
-doc "An Erlang [reference](`e:system:data_types.md#reference`).".
-type reference() :: reference().
-doc """
A character string represented by a list of ASCII characters or unicode
codepoints.
""".
-type string() :: [char()].
-doc "All possible Erlang terms. Synonym for `t:any/0`.".
-type term() :: any().
-doc """
A timeout value that can be passed to a
[receive expression](`e:system:expressions.md#receive`).
""".
-type timeout() :: 'infinity' | non_neg_integer().
-doc "An Erlang [tuple](`e:system:data_types.md#tuple`).".
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
-export_type([processes_iter_ref/0]).

-type stacktrace_extrainfo() ::
        {line, pos_integer()} |
        {file, unicode:chardata()} |
        {error_info, #{ module => module(), function => atom(), cause => term() }} |
        {atom(), term()}.
-doc """
An Erlang stacktrace as described by
[Errors and Error Handling](`e:system:errors.md#stacktrace`) section in the
Erlang Reference Manual.
""".
-type stacktrace() :: [{module(), atom(), arity() | [term()],
                        [stacktrace_extrainfo()]} |
                       {function(), arity() | [term()], [stacktrace_extrainfo()]}].

-doc "A binary data object, structured according to the Erlang external term format.".
-type ext_binary() :: binary().
-doc """
A term of type `t:iovec/0`, structured according to the Erlang external term
format.
""".
-type ext_iovec() :: iovec().
-doc "See [`erlang:timestamp/0`](`timestamp/0`).".
-type timestamp() :: {MegaSecs :: non_neg_integer(),
                      Secs :: non_neg_integer(),
                      MicroSecs :: non_neg_integer()}.

-doc """
The time unit used by erlang time APIs.

Supported time unit representations:

- **`PartsPerSecond :: integer() >= 1`** - Time unit expressed in parts per
  second. That is, the time unit equals `1/PartsPerSecond` second.

- **`second`** - Symbolic representation of the time unit represented by the
  integer `1`.

- **`millisecond`** - Symbolic representation of the time unit represented by
  the integer `1000`.

- **`microsecond`** - Symbolic representation of the time unit represented by
  the integer `1000_000`.

- **`nanosecond`** - Symbolic representation of the time unit represented by the
  integer `1000_000_000`.

- **`native`** - Symbolic representation of the native time unit used by the
  Erlang runtime system.

  The `native` time unit is determined at runtime system start, and remains the
  same until the runtime system terminates. If a runtime system is stopped and
  then started again (even on the same machine), the `native` time unit of the
  new runtime system instance can differ from the `native` time unit of the old
  runtime system instance.

  One can get an approximation of the `native` time unit by calling
  [`erlang:convert_time_unit(1, second, native)`](`convert_time_unit/3`). The
  result equals the number of whole `native` time units per second. If the
  number of `native` time units per second does not add up to a whole number,
  the result is rounded downwards.

  > #### Note {: .info }
  >
  > The value of the `native` time unit gives you more or less no information
  > about the quality of time values. It sets a limit for the
  > [resolution](time_correction.md#time-resolution) and for the
  > [precision](time_correction.md#time-precision) of time values, but it gives
  > no information about the [accuracy](time_correction.md#time-accuracy) of
  > time values. The resolution of the `native` time unit and the resolution of
  > time values can differ significantly.

- **`perf_counter`** - Symbolic representation of the performance counter time
  unit used by the Erlang runtime system.

  The `perf_counter` time unit behaves much in the same way as the `native` time
  unit. That is, it can differ between runtime restarts. To get values of this
  type, call `os:perf_counter/0`.

- **`t:deprecated_time_unit/0`** -
  Deprecated symbolic representations kept for backwards-compatibility.

The `t:time_unit/0` type can be extended. To convert time values between time
units, use [`erlang:convert_time_unit/3`](`convert_time_unit/3`).
""".
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
-doc """
The `t:time_unit/0` type also consist of the following _deprecated_ symbolic
time units:

- **`seconds`** - Same as [`second`](`t:time_unit/0`).

- **`milli_seconds`** - Same as [`millisecond`](`t:time_unit/0`).

- **`micro_seconds`** - Same as [`microsecond`](`t:time_unit/0`).

- **`nano_seconds`** - Same as [`nanosecond`](`t:time_unit/0`).
""".
-type deprecated_time_unit() ::
      'seconds'
      | 'milli_seconds'
      | 'micro_seconds'
      | 'nano_seconds'.

-opaque prepared_code() :: reference().
-export_type([prepared_code/0]).

-doc """
An opaque handle identifying a
[NIF resource object ](erl_nif.md#resource_objects).
""".
-opaque nif_resource() :: reference().
-export_type([nif_resource/0]).

-doc "An opaque handle identifying a distribution channel.".
-opaque dist_handle() :: atom().
-export_type([dist_handle/0]).

-doc """
A list of binaries. This datatype is useful to use together with
[`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec).
""".
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
-export([error/1, error/2, error/3, exit/1, exit/2, exit/3,
         exit_signal/2, external_size/1]).
-export([external_size/2, finish_after_on_load/2, finish_loading/1, float/1]).
-export([float_to_binary/1, float_to_binary/2,
	 float_to_list/1, float_to_list/2, floor/1]).
-export([fun_info/2, fun_info_mfa/1, fun_to_list/1, function_exported/3]).
-export([garbage_collect/0, garbage_collect/1, garbage_collect/2]).
-export([garbage_collect_message_area/0, get/0, get/1, get_keys/0, get_keys/1]).
-export([get_module_info/1, group_leader/0]).
-export([group_leader/2]).
-export([halt/0, halt/1, halt/2,
	 has_prepared_code_on_load/1, hibernate/0, hibernate/3]).
-export([insert_element/3]).
-export([integer_to_binary/1, integer_to_list/1]).
-export([iolist_size/1, iolist_to_binary/1, iolist_to_iovec/1]).
-export([is_alive/0, is_builtin/3, is_map_key/2, is_process_alive/1, length/1]).
-export([link/1, link/2, list_to_atom/1, list_to_binary/1]).
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
-export([processes_iterator/0, processes_next/1]).
-export([put/2, raise/3, read_timer/1, read_timer/2, ref_to_list/1, register/2]).
-export([send_after/3, send_after/4, start_timer/3, start_timer/4]).
-export([registered/0, resume_process/1, round/1, self/0]).
-export([seq_trace/2, seq_trace_print/1, seq_trace_print/2, setnode/2]).
-export([setnode/3, size/1, spawn/3, spawn_link/3, split_binary/2]).
-export([suspend_process/2, system_monitor/0]).
-export([system_monitor/1, system_monitor/2, system_profile/0]).
-export([system_profile/2, throw/1, time/0, trace/3, trace_delivered/1]).
-export([trace_info/2]).
-export([trunc/1, tuple_size/1, universaltime/0]).
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
         tl/1,
         trace_pattern/2, trace_pattern/3,
         tuple_to_list/1, system_info/1,
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
      {'long_message_queue', {Disable :: non_neg_integer(),
                              Enable :: pos_integer()}} |
      {'long_schedule', non_neg_integer()} |
      {'large_heap', non_neg_integer()}.

-doc """
A extended `t:stacktrace/0` that can be passed to `raise/3`.
""".
-type raise_stacktrace() ::
      [{module(), atom(), arity() | [term()]} |
       {function(), arity() | [term()]}]
      | stacktrace().
-export_type([raise_stacktrace/0]).

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
-doc "Computes and returns the adler32 checksum for `Data`.".
-doc #{ category => checksum }.
-spec adler32(Data) -> non_neg_integer() when
      Data :: iodata().
adler32(_Data) ->
    erlang:nif_error(undefined).

%% adler32/2
-doc """
Continues computing the adler32 checksum by combining the previous checksum,
`OldAdler`, with the checksum of `Data`.

The following code:

```erlang
X = erlang:adler32(Data1),
Y = erlang:adler32(X,Data2).
```

assigns the same value to `Y` as this:

```erlang
Y = erlang:adler32([Data1,Data2]).
```
""".
-doc #{ category => checksum }.
-spec adler32(OldAdler, Data) -> non_neg_integer() when
      OldAdler :: non_neg_integer(),
      Data :: iodata().
adler32(_OldAdler, _Data) ->
    erlang:nif_error(undefined).

%% adler32_combine/3
-doc """
Combines two previously computed adler32 checksums.

This computation requires the size of the data object for the second checksum
to be known.

The following code:

```erlang
Y = erlang:adler32(Data1),
Z = erlang:adler32(Y,Data2).
```

assigns the same value to `Z` as this:

```erlang
X = erlang:adler32(Data1),
Y = erlang:adler32(Data2),
Z = erlang:adler32_combine(X,Y,iolist_size(Data2)).
```
""".
-doc #{ category => checksum }.
-spec adler32_combine(FirstAdler, SecondAdler, SecondSize) -> non_neg_integer() when
      FirstAdler :: non_neg_integer(),
      SecondAdler :: non_neg_integer(),
      SecondSize :: non_neg_integer().
adler32_combine(_FirstAdler, _SecondAdler, _SecondSize) ->
    erlang:nif_error(undefined).

%% append_element/2
-doc """
Returns a new tuple that has one element more than `Tuple1`, and contains the
elements in `Tuple1` followed by `Term` as the last element.

Semantically equivalent to
[`list_to_tuple(tuple_to_list(Tuple1) ++ [Term])`](`list_to_tuple/1`), but much
faster.

For example:

```erlang
> erlang:append_element({one, two}, three).
{one,two,three}
```
""".
-doc #{ category => terms }.
-spec append_element(Tuple1, Term) -> Tuple2 when
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Term :: term().
append_element(_Tuple1, _Term) ->
    erlang:nif_error(undefined).

%% atom_to_binary/1
-doc(#{ equiv => atom_to_binary(Atom, utf8) }).
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => terms }.
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
-doc """
Returns a binary corresponding to the text representation of `Atom`.

If `Encoding` is `latin1`, one byte exists for each character in the text
representation. If `Encoding` is `utf8` or `unicode`, the characters are encoded
using UTF-8 where characters may require multiple bytes.

> #### Change {: .info }
>
> As from Erlang/OTP 20, atoms can contain any Unicode character and
> [`atom_to_binary(Atom, latin1)`](`atom_to_binary/2`) may fail if the text
> representation for `Atom` contains a Unicode character > 255.

Example:

```erlang
> atom_to_binary('Erlang', latin1).
<<"Erlang">>
```
""".
-doc #{ category => terms }.
-spec atom_to_binary(Atom, Encoding) -> binary() when
      Atom :: atom(),
      Encoding :: latin1 | unicode | utf8.
atom_to_binary(_Atom, _Encoding) ->
    erlang:nif_error(undefined).

%% atom_to_list/1
-doc """
Returns a list of unicode code points corresponding to the text representation
of `Atom`.

For example:

```erlang
> atom_to_list('Erlang').
"Erlang"
```

```erlang
> atom_to_list('你好').
[20320,22909]
```

See `m:unicode` for how to convert the resulting list to different formats.
""".
-doc #{ category => terms }.
-spec atom_to_list(Atom) -> string() when
      Atom :: atom().
atom_to_list(_Atom) ->
    erlang:nif_error(undefined).

%% binary_part/2
%% Shadowed by erl_bif_types: erlang:binary_part/2
-doc """
Extracts the part of the binary described by `PosLen`.

Negative length can be used to extract bytes at the end of a binary. 

For example:

```erlang
1> Bin = <<1,2,3,4,5,6,7,8,9,10>>.
2> binary_part(Bin,{byte_size(Bin), -5}).
<<6,7,8,9,10>>
```

Failure: `badarg` if `PosLen` in any way references outside the binary.

`Start` is zero-based, that is:

```erlang
1> Bin = <<1,2,3>>
2> binary_part(Bin,{0,2}).
<<1,2>>
```

For details about the `PosLen` semantics, see `m:binary`.
""".
-doc #{ category => terms }.
-doc(#{since => <<"OTP R14B">>}).
-spec binary_part(Subject, PosLen) -> binary() when
      Subject :: binary(),
      PosLen :: {Start :: non_neg_integer(), Length :: integer()}.
binary_part(_Subject, _PosLen) ->
    erlang:nif_error(undefined).

%% binary_part/3
%% Shadowed by erl_bif_types: erlang:binary_part/3
-doc( #{ equiv =>  binary_part(Subject, {Start, Length}) }).
-doc(#{since => <<"OTP R14B">>}).
-doc #{ category => terms }.
-spec binary_part(Subject, Start, Length) -> binary() when
      Subject :: binary(),
      Start :: non_neg_integer(),
      Length :: integer().
binary_part(_Subject, _Start, _Length) ->
    erlang:nif_error(undefined).

%% binary_to_atom/1
-doc(#{ equiv => binary_to_atom(Binary, utf8) }).
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => terms }.
-spec binary_to_atom(Binary) -> atom() when
      Binary :: binary().
binary_to_atom(Binary) ->
    try
        erlang:binary_to_atom(Binary, utf8)
    catch
	error:Error -> error_with_info(Error, [Binary])
    end.

%% binary_to_atom/2
-doc """
Returns the atom whose text representation is `Binary`. If `Encoding` is `utf8`
or `unicode`, the binary must contain valid UTF-8 sequences.

> #### Change {: .info }
>
> As from Erlang/OTP 20, [`binary_to_atom(Binary, utf8)`](`binary_to_atom/2`) is
> capable of decoding any Unicode character. Earlier versions would fail if the
> binary contained Unicode characters > 255.

> #### Note {: .info }
>
> The number of characters that are permitted in an atom name is limited. The
> default limits can be found in the
> [Efficiency Guide (section System Limits)](`e:system:system_limits.md`).

> #### Note {: .info }
>
> There is configurable limit on how many atoms that can exist and atoms are not
> garbage collected. Therefore, it is recommended to consider whether
> [`binary_to_existing_atom/2`](`binary_to_existing_atom/2`) is a better option
> than [`binary_to_atom/2`](`binary_to_atom/2`). The default limits can be found
> in [Efficiency Guide (section System Limits)](`e:system:system_limits.md#atoms`).

Examples:

```erlang
> binary_to_atom(<<"Erlang">>, latin1).
'Erlang'
```

```erlang
> binary_to_atom(<<1024/utf8>>, utf8).
'Ѐ'
```
""".
-doc #{ category => terms }.
-spec binary_to_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%% binary_to_existing_atom/1
-doc(#{ equiv => binary_to_existing_atom(Binary, utf8) }).
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => terms }.
-spec binary_to_existing_atom(Binary) -> atom() when
      Binary :: binary().
binary_to_existing_atom(Binary) ->
    try
        erlang:binary_to_existing_atom(Binary, utf8)
    catch
	error:Error -> error_with_info(Error, [Binary])
    end.

%% binary_to_existing_atom/2
-doc """
As `binary_to_atom/2`, but the atom must exist.

The Erlang system has a [configurable limit](`e:system:system_limits.md#atoms`) for the
total number of atoms that can exist, and atoms are not garbage collected.
Therefore, it is not safe to create many atoms from binaries that come from an
untrusted source (for example, a file fetched from the Internet), for example,
using `binary_to_atom/2`. This function is thus the appropriate option when the
input binary comes from an untrusted source.

An atom exists in an Erlang system when included in a loaded Erlang module or
when created programmatically (for example, by
[`binary_to_atom/2`](`binary_to_atom/2`)). See the next note for an example of
when an atom exists in the source code for an Erlang module but not in the
compiled version of the same module.

Failure: `badarg` if the atom does not exist.

> #### Note {: .info }
>
> Note that the compiler may optimize away atoms. For example, the compiler will
> rewrite [`atom_to_list(some_atom)`](`atom_to_list/1`) to `"some_atom"`. If
> that expression is the only mention of the atom `some_atom` in the containing
> module, the atom will not be created when the module is loaded, and a
> subsequent call to
> [`binary_to_existing_atom(<<"some_atom">>, utf8)`](`binary_to_existing_atom/2`)
> will fail.

> #### Note {: .info }
>
> The number of characters that are permitted in an atom name is limited. The
> default limits can be found in the
> [Efficiency Guide (section System Limits)](`e:system:system_limits.md`).
""".
-doc #{ category => terms }.
-spec binary_to_existing_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_existing_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%% binary_to_float/1
-doc """
Returns the float whose text representation is `Binary`.

For example:

```erlang
> binary_to_float(<<"2.2017764e+0">>).
2.2017764
```

The float string format is the same as the format for
[Erlang float literals](`e:system:data_types.md`) except for that underscores
are not permitted.

Failure: `badarg` if `Binary` contains a bad representation of a float.
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec binary_to_float(Binary) -> float() when
      Binary :: binary().
binary_to_float(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_integer/1
-doc """
Returns an integer whose text representation is `Binary`.

For example:

```erlang
> binary_to_integer(<<"123">>).
123
```

[`binary_to_integer/1`](`binary_to_integer/1`) accepts the same string formats
as `list_to_integer/1`.

Failure: `badarg` if `Binary` contains a bad representation of an integer.
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec binary_to_integer(Binary) -> integer() when
      Binary :: binary().
binary_to_integer(Binary) ->
    case erts_internal:binary_to_integer(Binary, 10) of
        N when erlang:is_integer(N) ->
            N;
        big ->
            case big_binary_to_int(Binary, 10) of
                N when erlang:is_integer(N) ->
                    N;
                Reason ->
                    error_with_info(Reason, [Binary])
            end;
        badarg ->
            badarg_with_info([Binary])
    end.

%% binary_to_integer/2
-doc """
Returns an integer whose text representation in base `Base` is `Binary`.

For example:

```erlang
> binary_to_integer(<<"3FF">>, 16).
1023
```

[`binary_to_integer/2`](`binary_to_integer/2`) accepts the same string formats
as `list_to_integer/2`.

Failure: `badarg` if `Binary` contains a bad representation of an integer.
""".
-doc #{ category => terms }.
-doc(#{since => <<"OTP R16B">>}).
-spec binary_to_integer(Binary, Base) -> integer() when
      Binary :: binary(),
      Base :: 2..36.
binary_to_integer(Binary, Base) ->
    case erts_internal:binary_to_integer(Binary, Base) of
        N when erlang:is_integer(N) ->
            N;
        big ->
            case big_binary_to_int(Binary, Base) of
                N when erlang:is_integer(N) ->
                    N;
                Reason ->
                    error_with_info(Reason, [Binary,Base])
            end;
        badarg ->
            badarg_with_info([Binary,Base])
    end.

big_binary_to_int(Bin0, Base)
  when erlang:is_binary(Bin0),
       erlang:is_integer(Base), 2 =< Base, Base =< 36 ->
    {Bin1,Sign} = get_sign(Bin0),
    Bin = trim_zeroes(Bin1),
    Size = erlang:byte_size(Bin),
    if
        Size > 4_194_304 ->
            %% Too large even for base 2.
            system_limit;
        Size > 1_262_611, Base >= 10 ->
            system_limit;
        true ->
            WordSize = erlang:system_info(wordsize),
            LogRadix = digits_per_small(WordSize, Base),
            case segmentize(Bin, Size, Base, LogRadix) of
                [_|_]=Segments ->
                    Radix = radix(WordSize, Base),
                    try Sign * combine(Segments, Radix) of
                        Result ->
                            Result
                    catch
                        error:Reason ->
                            Reason
                    end;
                badarg ->
                    badarg
            end
    end.

segmentize(Bin, Size, Base, LogRadix) ->
    case Size rem LogRadix of
        0 ->
            segmentize_1(Bin, LogRadix, Base, []);
        NumFirst ->
            <<First:NumFirst/binary,T/binary>> = Bin,
            case erts_internal:binary_to_integer(First, Base) of
                FirstInt when erlang:is_integer(FirstInt) ->
                    segmentize_1(T, LogRadix, Base, [FirstInt]);
                badarg ->
                    badarg
            end
    end.

segmentize_1(Bin, LogRadix, Base, Acc) ->
    case Bin of
        <<B:LogRadix/binary,T/binary>> ->
            case erts_internal:binary_to_integer(B, Base) of
                Int when erlang:is_integer(Int) ->
                    segmentize_1(T, LogRadix, Base, [Int|Acc]);
                badarg ->
                    badarg
            end;
        <<>> ->
            Acc
    end.

combine(L0, Radix) ->
    case combine_pairs(L0, Radix) of
        [Result] -> Result;
        [_|_]=L -> combine(L, Radix * Radix)
    end.

combine_pairs([A,B|Pairs], Radix) ->
    [B * Radix + A|combine_pairs(Pairs, Radix)];
combine_pairs(L, _Radix) ->
    L.

get_sign(<<$-:8,B/binary>>) ->
    {B,-1};
get_sign(<<$+:8,B/binary>>) ->
    {B,1};
get_sign(B) ->
    {B,1}.

trim_zeroes(<<$0:8,B/binary>>) ->
    trim_zeroes(B);
trim_zeroes(B) ->
    B.

digits_per_small(WordSize, Base) ->
    T = case WordSize of
            4 ->
                %% Wolfram Alpha formula:
                %% Table [Trunc[27 / log[2,n]]-1, {n, 2, 36}]
                {27, 17, 13, 11, 10, 9, 9, 8,
                 8, 7, 7, 7, 7, 6, 6, 6, 6,
                 6, 6, 6, 6, 5, 5, 5, 5, 5,
                 5, 5, 5, 5, 5, 5, 5, 5, 5};
            8 ->
                %% Wolfram Alpha formula:
                %% Table [Trunc[59 / log[2,n]]-1, {n, 2, 36}]
                {59, 37, 29, 25, 22, 21, 19, 18, 17,
                 17, 16, 15, 15, 15, 14, 14, 14, 13,
                 13, 13, 13, 13, 12, 12, 12, 12, 12,
                 12, 12, 11, 11, 11, 11, 11, 11}
        end,
    erlang:element(Base - 1, T).

radix(WordSize, Base) ->
    %% The tables are generated using the following function:
    %%
    %%    gen(WordSize) ->
    %%        IntPow = fun IP(_Base, 0, P) -> P;
    %%                     IP(Base, N, P) -> IP(Base, N - 1, Base * P)
    %%                 end,
    %%        L = [IntPow(Base, digits_per_small(WordSize, Base), 1) ||
    %%                Base <- lists:seq(2, 36)],
    %%        io:format("~50p\n", [list_to_tuple(L)]).
    T = case WordSize of
            4 ->
                %% gen(4)
                {134217728,129140163,67108864,48828125,60466176,
                 40353607,134217728,43046721,100000000,19487171,
                 35831808,62748517,105413504,11390625,16777216,
                 24137569,34012224,47045881,64000000,85766121,
                 113379904,6436343,7962624,9765625,11881376,
                 14348907,17210368,20511149,24300000,28629151,
                 33554432,39135393,45435424,52521875,60466176};
            8 ->
                %% gen(8)
                {576460752303423488,450283905890997363,
                 288230376151711744,298023223876953125,
                 131621703842267136,558545864083284007,
                 144115188075855872,150094635296999121,
                 100000000000000000,505447028499293771,
                 184884258895036416,51185893014090757,
                 155568095557812224,437893890380859375,
                 72057594037927936,168377826559400929,
                 374813367582081024,42052983462257059,
                 81920000000000000,154472377739119461,
                 282810057883082752,504036361936467383,
                 36520347436056576,59604644775390625,
                 95428956661682176,150094635296999121,
                 232218265089212416,353814783205469041,
                 531441000000000000,25408476896404831,
                 36028797018963968,50542106513726817,
                 70188843638032384,96549157373046875,
                 131621703842267136}
        end,
    erlang:element(Base - 1, T).

%% binary_to_list/1
-doc "Returns a list of integers corresponding to the bytes of `Binary`.".
-doc #{ category => terms }.
-spec binary_to_list(Binary) -> [byte()] when
      Binary :: binary().
binary_to_list(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_list/3
-doc """
As [`binary_to_list/1`](`binary_to_list/1`), but returns a list of integers
corresponding to the bytes from position `Start` to position `Stop` in `Binary`.
The positions in the binary are numbered starting from 1.

> #### Note {: .info }
>
> _The one-based indexing for binaries used by this function is deprecated._ New
> code is to use `binary:bin_to_list/3` in STDLIB instead. All functions in
> module `binary` consistently use zero-based indexing.
""".
-doc #{ category => terms }.
-spec binary_to_list(Binary, Start, Stop) -> [byte()] when
      Binary :: binary(),
      Start :: pos_integer(),
      Stop :: pos_integer().
binary_to_list(_Binary, _Start, _Stop) ->
    erlang:nif_error(undefined).

%% binary_to_term/1
-doc """
Returns an Erlang term that is the result of decoding binary object `Binary`,
which must be encoded according to the
[Erlang external term format](erl_ext_dist.md).

```erlang
> Bin = term_to_binary(hello).
<<131,100,0,5,104,101,108,108,111>>
> hello = binary_to_term(Bin).
hello
```

> #### Warning {: .warning }
>
> When decoding binaries from untrusted sources, the untrusted source may submit
> data in a way to create resources, such as atoms and remote references, that
> cannot be garbage collected and lead to Denial of Service attack. In such
> cases, consider using [`binary_to_term/2`](`binary_to_term/2`) with the `safe`
> option.

See also `term_to_binary/1` and `binary_to_term/2`.
""".
-doc #{ category => terms }.
-spec binary_to_term(Binary) -> term() when
      Binary :: ext_binary().
binary_to_term(_Binary) ->
    erlang:nif_error(undefined).

%% binary_to_term/2
-doc """
Equivalent to [`binary_to_term(Binary)`](`binary_to_term/1`), but can be configured to
fit special purposes.

The allowed options are:

- **`safe`** - Use this option when receiving binaries from an untrusted source.

  When enabled, it prevents decoding data that can be used to attack the Erlang
  runtime. In the event of receiving unsafe data, decoding fails with a `badarg`
  error.

  This prevents creation of new atoms directly, creation of new atoms indirectly
  (as they are embedded in certain structures, such as process identifiers,
  refs, and funs), and creation of new external function references. None of
  those resources are garbage collected, so unchecked creation of them can
  exhaust available memory.

  ```erlang
  > binary_to_term(<<131,100,0,5,"hello">>, [safe]).
  ** exception error: bad argument
  > hello.
  hello
  > binary_to_term(<<131,100,0,5,"hello">>, [safe]).
  hello
  ```

  > #### Warning {: .warning }
  >
  > The `safe` option ensures the data is safely processed by the Erlang runtime
  > but it does not guarantee the data is safe to your application. You must
  > always validate data from untrusted sources. If the binary is stored or
  > transits through untrusted sources, you should also consider
  > cryptographically signing it.

- **`used`** - Changes the return value to `{Term, Used}` where `Used` is the
  number of bytes actually read from `Binary`.

  ```erlang
  > Input = <<131,100,0,5,"hello","world">>.
  <<131,100,0,5,104,101,108,108,111,119,111,114,108,100>>
  > {Term, Used} = binary_to_term(Input, [used]).
  {hello, 9}
  > split_binary(Input, Used).
  {<<131,100,0,5,104,101,108,108,111>>, <<"world">>}
  ```

Failure: `badarg` if `safe` is specified and unsafe data is decoded.

See also `term_to_binary/1`, `binary_to_term/1`, and `list_to_existing_atom/1`.
""".
-doc(#{since => <<"OTP R13B04">>}).
-doc #{ category => terms }.
-spec binary_to_term(Binary, Opts) -> term() | {term(), Used} when
      Binary :: ext_binary(),
      Opt :: safe | used,
      Opts :: [Opt],
      Used :: pos_integer().
binary_to_term(_Binary, _Opts) ->
    erlang:nif_error(undefined).

%% bit_size/1
%% Shadowed by erl_bif_types: erlang:bit_size/1
-doc """
Returns an integer that is the size in bits of `Bitstring`.

For example:

```erlang
> bit_size(<<433:16,3:3>>).
19
> bit_size(<<1,2,3>>).
24
```
""".
-doc #{ category => terms }.
-spec bit_size(Bitstring) -> non_neg_integer() when
      Bitstring :: bitstring().
bit_size(_Bitstring) ->
    erlang:nif_error(undefined).

%% bitstring_to_list/1
-doc """
Returns a list of integers corresponding to the bytes of `Bitstring`.

If the number of bits in the binary is not divisible by 8, the last element of
the list is a bitstring containing the remaining 1-7 bits.

For example:

```erlang
> bitstring_to_list(<<433:16>>).
[1,177]
```

```erlang
> bitstring_to_list(<<433:16,3:3>>).
[1,177,<<3:3>>]
```
""".
-doc #{ category => terms }.
-spec bitstring_to_list(Bitstring) -> [byte() | bitstring()] when
      Bitstring :: bitstring().
bitstring_to_list(_Bitstring) ->
    erlang:nif_error(undefined).

%% bump_reductions/1
-doc """
This implementation-dependent function increments the reduction counter for the
calling process.

In the Beam emulator, the reduction counter is normally incremented by one for
each function and BIF call. A context switch is forced when the counter reaches
the maximum number of reductions for a process (4000 reductions in Erlang/OTP 19.2 and later).

> #### Warning {: .warning }
>
> This BIF can be removed in a future version of the Beam machine without prior
> warning. It is unlikely to be implemented in other Erlang implementations.
""".
-doc #{ category => processes }.
-spec bump_reductions(Reductions) -> true when
      Reductions :: pos_integer().
bump_reductions(_Reductions) ->
    erlang:nif_error(undefined).

%% byte_size/1
%% Shadowed by erl_bif_types: erlang:byte_size/1
-doc """
Returns an integer that is the number of bytes needed to contain `Bitstring`.
That is, if the number of bits in `Bitstring` is not divisible by 8, the
resulting number of bytes is rounded _up_.

For example:

```erlang
> byte_size(<<433:16,3:3>>).
3
> byte_size(<<1,2,3>>).
3
```
""".
-doc #{ category => terms }.
-spec byte_size(Bitstring) -> non_neg_integer() when
      Bitstring :: bitstring().
byte_size(_Bitstring) ->
    erlang:nif_error(undefined).

%% call_on_load_function/1
-doc false.
-spec call_on_load_function(P1) -> term() when
      P1 :: atom().
call_on_load_function(_P1) ->
    erlang:nif_error(undefined).

%% cancel_timer/1
-doc( #{ equiv =>  erlang:cancel_timer(TimerRef, []) }).
-doc #{ category => time }.
-spec cancel_timer(TimerRef) -> Result when
      TimerRef :: reference(),
      Time :: non_neg_integer(),
      Result :: Time | false.

cancel_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% cancel_timer/2
-doc """
Cancels a timer that has been created by [`erlang:start_timer`](`start_timer/4`)
or [`erlang:send_after`](`send_after/4`). `TimerRef` identifies the timer, and
was returned by the BIF that created the timer.

`Option`s:

- **`{async, Async}`** - Asynchronous request for cancellation. `Async` defaults
  to `false`, which causes the cancellation to be performed synchronously. When
  `Async` is set to `true`, the cancel operation is performed asynchronously.
  That is, `cancel_timer()` sends an asynchronous request for cancellation to
  the timer service that manages the timer, and then returns `ok`.

- **`{info, Info}`** - Requests information about the `Result` of the
  cancellation. `Info` defaults to `true`, which means the `Result` is given.
  When `Info` is set to `false`, no information about the result of the
  cancellation is given.

  - When `Async` is `false`: if `Info` is `true`, the `Result` is returned by
    `erlang:cancel_timer()`. otherwise `ok` is returned.
  - When `Async` is `true`: if `Info` is `true`, a message on the form
    `{cancel_timer, TimerRef, Result}` is sent to the caller of
    `erlang:cancel_timer()` when the cancellation operation has been performed,
    otherwise no message is sent.

More `Option`s may be added in the future.

If `Result` is an integer, it represents the time in milliseconds left until the
canceled timer would have expired.

If `Result` is `false`, a timer corresponding to `TimerRef` could not be found.
This can be either because the timer had expired, already had been canceled, or
because `TimerRef` never corresponded to a timer. Even if the timer had expired,
it does not tell you if the time-out message has arrived at its destination yet.

> #### Note {: .info }
>
> The timer service that manages the timer can be co-located with another
> scheduler than the scheduler that the calling process is executing on. If so,
> communication with the timer service takes much longer time than if it is
> located locally. If the calling process is in critical path, and can do other
> things while waiting for the result of this operation, or is not interested in
> the result of the operation, you want to use option `{async, true}`. If using
> option `{async, false}`, the calling process blocks until the operation has
> been performed.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:start_timer/4`](`start_timer/4`), and
[`erlang:read_timer/2`](`read_timer/2`).
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec cancel_timer(TimerRef, Options) -> Result | ok when
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
-doc """
Returns the smallest integer not less than `Number`.

For example:

```erlang
> ceil(5.5).
6
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-doc #{ category => terms }.
-spec ceil(Number) -> integer() when
      Number :: number().
ceil(_) ->
    erlang:nif_error(undef).

%% check_old_code/1
-doc """
Returns `true` if `Module` has
[old code](`e:system:code_loading.md#code-replacement`), otherwise `false`.

See also `m:code`.
""".
-doc(#{since => <<"OTP R14B04">>}).
-doc #{ category => code }.
-spec check_old_code(Module) -> boolean() when
      Module :: module().
check_old_code(_Module) ->
    erlang:nif_error(undefined).

%% check_process_code/2
-doc(#{ equiv => check_process_code(Pid, Module, []) }).
-doc #{ category => code }.
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
-doc """
Checks if the node local process identified by `Pid` executes old code for
`Module`.

`Option`s:

- **`{allow_gc, boolean()}`** - Determines if garbage collection is allowed when
  performing the operation. If `{allow_gc, false}` is passed, and a garbage
  collection is needed to determine the result of the operation, the operation
  is aborted (see information on `CheckResult` below). The default is to allow
  garbage collection, that is, `{allow_gc, true}`.

- **`{async, RequestId}`** - The function
  [`check_process_code/3`](`check_process_code/3`) returns the value `async`
  immediately after the request has been sent. When the request has been
  processed, the process that called this function is passed a message on the
  form `{check_process_code, RequestId, CheckResult}`.

If `Pid` equals `self/0`, and no `async` option has been passed, the operation
is performed at once. Otherwise a request for the operation is sent to the
process identified by `Pid`, and is handled when appropriate. If no `async`
option has been passed, the caller blocks until `CheckResult` is available and
can be returned.

`CheckResult` informs about the result of the request as follows:

- **`true`** - The process identified by `Pid` executes old code for `Module`.
  That is, the current call of the process executes old code for this module, or
  the process has references to old code for this module, or the process
  contains funs that references old code for this module.

- **`false`** - The process identified by `Pid` does not execute old code for
  `Module`.

- **`aborted`** - The operation was aborted, as the process needed to be garbage
  collected to determine the operation result, and the operation was requested
  by passing option `{allow_gc, false}`.

> #### Change {: .info }
>
> Up until ERTS version 8.\*, the check process code operation checks for all
> types of references to the old code. That is, direct references (e.g. return
> addresses on the process stack), indirect references (`fun`s in process
> context), and references to literals in the code.
>
> As of ERTS version 9.0, the check process code operation only checks for
> direct references to the code. Indirect references via `fun`s will be ignored.
> If such `fun`s exist and are used after a purge of the old code, an exception
> will be raised upon usage (same as the case when the `fun` is received by the
> process after the purge). Literals will be taken care of (copied) at a later
> stage. This behavior can as of ERTS version 8.1 be enabled when
> [building OTP](`e:system:install.md#advanced-configuration-and-build-of-erlang-otp_configuring`),
> and will automatically be enabled if dirty scheduler support is enabled.

See also `m:code`.

Failures:

- **`badarg`** - If `Pid` is not a node local process identifier.

- **`badarg`** - If `Module` is not an atom.

- **`badarg`** - If `OptionList` is an invalid list of options.
""".
-doc #{ category => code }.
-doc(#{since => <<"OTP 17.0">>}).
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
-doc "Computes and returns the crc32 (IEEE 802.3 style) checksum for `Data`.".
-doc #{ category => checksum }.
-spec crc32(Data) -> non_neg_integer() when
      Data :: iodata().
crc32(_Data) ->
    erlang:nif_error(undefined).

%% crc32/2
-doc """
Continues computing the crc32 checksum by combining the previous checksum,
`OldCrc`, with the checksum of `Data`.

The following code:

```erlang
X = erlang:crc32(Data1),
Y = erlang:crc32(X,Data2).
```

assigns the same value to `Y` as this:

```erlang
Y = erlang:crc32([Data1,Data2]).
```
""".
-doc #{ category => checksum }.
-spec crc32(OldCrc, Data) -> non_neg_integer() when
      OldCrc :: non_neg_integer(),
      Data :: iodata().
crc32(_OldCrc, _Data) ->
    erlang:nif_error(undefined).

%% crc32_combine/3
-doc """
Combines two previously computed crc32 checksums.

This computation requires the size of the data object for the second checksum
to be known.

The following code:

```erlang
Y = erlang:crc32(Data1),
Z = erlang:crc32(Y,Data2).
```

assigns the same value to `Z` as this:

```erlang
X = erlang:crc32(Data1),
Y = erlang:crc32(Data2),
Z = erlang:crc32_combine(X,Y,iolist_size(Data2)).
```
""".
-doc #{ category => checksum }.
-spec crc32_combine(FirstCrc, SecondCrc, SecondSize) -> non_neg_integer() when
      FirstCrc :: non_neg_integer(),
      SecondCrc :: non_neg_integer(),
      SecondSize :: non_neg_integer().
crc32_combine(_FirstCrc, _SecondCrc, _SecondSize) ->
    erlang:nif_error(undefined).

%% date/0
-doc """
Returns the current date as `{Year, Month, Day}`.

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> date().
{1995,2,19}
```
""".
-doc #{ category => time }.
-spec date() -> Date when
      Date :: calendar:date().
date() ->
    erlang:nif_error(undefined).

%% decode_packet/3
-doc """
Decodes the binary `Bin` according to the packet protocol specified by `Type`.
Similar to the packet handling done by sockets with option `{packet,Type}.`

If an entire packet is contained in `Bin`, it is returned together with the
remainder of the binary as `{ok,Packet,Rest}`.

If `Bin` does not contain the entire packet, `{more,Length}` is returned.
`Length` is either the expected _total size_ of the packet, or `undefined` if
the expected packet size is unknown. `decode_packet` can then be called again
with more data added.

If the packet does not conform to the protocol format, `{error,Reason}` is
returned.

`Type`s:

- **`raw | 0`** - No packet handling is done. The entire binary is returned
  unless it is empty.

- **`1 | 2 | 4`** - Packets consist of a header specifying the number of bytes
  in the packet, followed by that number of bytes. The length of the header can
  be one, two, or four bytes; the order of the bytes is big-endian. The header
  is stripped off when the packet is returned.

- **`line`** - A packet is a line-terminated by a delimiter byte, default is the
  latin-1 newline character. The delimiter byte is included in the returned
  packet unless the line was truncated according to option `line_length`.

- **`asn1 | cdr | sunrm | fcgi | tpkt`** - The header is _not_ stripped off.

  The meanings of the packet types are as follows:

  - **`asn1` \- ASN.1 BER**

  - **`sunrm` \- Sun's RPC encoding**

  - **`cdr` \- CORBA (GIOP 1.1)**

  - **`fcgi` \- Fast CGI**

  - **`tpkt` \- TPKT format \[RFC1006]**

- **`http | httph | http_bin | httph_bin`** - The Hypertext Transfer Protocol.
  The packets are returned with the format according to `HttpPacket` described
  earlier. A packet is either a request, a response, a header, or an end of
  header mark. Invalid lines are returned as `HttpError`.

  Recognized request methods and header fields are returned as atoms. Others are
  returned as strings. Strings of unrecognized header fields are formatted with
  only capital letters first and after hyphen characters, for example,
  `"Sec-Websocket-Key"`. Header field names are also returned in
  `UnmodifiedField` as strings, without any conversion or formatting.

  The protocol type `http` is only to be used for the first line when an
  `HttpRequest` or an `HttpResponse` is expected. The following calls are to use
  `httph` to get `HttpHeader`s until `http_eoh` is returned, which marks the end
  of the headers and the beginning of any following message body.

  The variants `http_bin` and `httph_bin` return strings (`HttpString`) as
  binaries instead of lists.

  Since OTP 26.0, `Host` may be an IPv6 address enclosed in `[]`, as defined in
  [RFC2732 ](https://www.ietf.org/rfc/rfc2732.txt).

Options:

- **`{packet_size, integer() >= 0}`** - Sets the maximum allowed size of the
  packet body. If the packet header indicates that the length of the packet is
  longer than the maximum allowed length, the packet is considered invalid.
  Defaults to 0, which means no size limit.

- **`{line_length, integer() >= 0}`** - For packet type `line`, lines longer
  than the indicated length are truncated.

  Option `line_length` also applies to `http*` packet types as an alias for
  option `packet_size` if `packet_size` itself is not set. This use is only
  intended for backward compatibility.

- **`{line_delimiter, 0 =< byte() =< 255}`** - For packet type `line`, sets the
  delimiting byte. Default is the latin-1 character `$\n`.

Examples:

```erlang
> erlang:decode_packet(1,<<3,"abcd">>,[]).
{ok,<<"abc">>,<<"d">>}
> erlang:decode_packet(1,<<5,"abcd">>,[]).
{more,6}
```
""".
-doc #{ category => terms }.
-spec decode_packet(Type, Bin, Options) ->
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
-doc """
Returns a new tuple with element at `Index` removed from tuple `Tuple1`.

For example:

```erlang
> erlang:delete_element(2, {one, two, three}).
{one,three}
```
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec delete_element(Index, Tuple1) -> Tuple2 when
      Index  :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple().
delete_element(_Index, _Tuple1) ->
    erlang:nif_error(undefined).

%% delete_module/1
-doc """
Makes the current code for `Module` become old code and deletes all references
for this module from the export table. Returns `undefined` if the module does
not exist, otherwise `true`.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.

Failure: `badarg` if there already is an old version of `Module`.
""".
-doc #{ category => code }.
-spec delete_module(Module) -> true | undefined when
      Module :: module().
delete_module(_Module) ->
    erlang:nif_error(undefined).

%% demonitor/1
-doc """
If `MonitorRef` is a reference that the calling process obtained by calling
`monitor/2`, this monitoring is turned off. If the monitoring is already turned
off, nothing happens.

Once [`demonitor(MonitorRef)`](`demonitor/1`) has returned, it is guaranteed
that no `{'DOWN', MonitorRef, _, _, _}` message, because of the monitor, will be
placed in the caller message queue in the future. However, a
`{'DOWN', MonitorRef, _, _, _}` message can have been placed in the caller
message queue before the call. It is therefore usually advisable to remove such
a `'DOWN'` message from the message queue after monitoring has been stopped.
[`demonitor(MonitorRef, [flush])`](`demonitor/2`) can be used instead of
[`demonitor(MonitorRef)`](`demonitor/1`) if this cleanup is wanted.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

> #### Change {: .info }
>
> Before Erlang/OTP R11B (ERTS 5.5) [`demonitor/1`](`demonitor/1`) behaved
> completely asynchronously, that is, the monitor was active until the
> "demonitor signal" reached the monitored entity. This had one undesirable
> effect. You could never know when you were guaranteed _not_ to receive a
> `DOWN` message because of the monitor.
>
> The current behavior can be viewed as two combined operations: asynchronously
> send a "demonitor signal" to the monitored entity and ignore any future
> results of the monitor.

Failure: It is an error if `MonitorRef` refers to a monitoring started by
another process. Not all such cases are cheap to check. If checking is cheap,
the call fails with `badarg`, for example if `MonitorRef` is a remote reference.
""".
-spec demonitor(MonitorRef) -> true when
      MonitorRef :: reference().
-doc #{ category => processes }.
demonitor(_MonitorRef) ->
    erlang:nif_error(undefined).

%% demonitor/2
-doc """
The returned value is `true` unless `info` is part of `OptionList`.

[`demonitor(MonitorRef, [])`](`demonitor/2`) is equivalent to
[`demonitor(MonitorRef)`](`demonitor/1`).

`Option`s:

- **`flush`** - Removes (one) `{_, MonitorRef, _, _, _}` message, if there is
  one, from the caller message queue after monitoring has been stopped.

  Calling [`demonitor(MonitorRef, [flush])`](`demonitor/2`) is equivalent to the
  following, but more efficient:

  ```erlang
  demonitor(MonitorRef),
  receive
      {_, MonitorRef, _, _, _} ->
          true
  after 0 ->
          true
  end
  ```

- **`info`** - The returned value is one of the following:

  - **`true`** - The monitor was found and removed. In this case, no `'DOWN'`
    message corresponding to this monitor has been delivered and will not be
    delivered.

  - **`false`** - The monitor was not found and could not be removed. This
    probably because someone already has placed a `'DOWN'` message corresponding
    to this monitor in the caller message queue.

  If option `info` is combined with option `flush`, `false` is returned if a
  flush was needed, otherwise `true`.

> #### Change {: .info }
>
> More options can be added in a future release.

Failures:

- **`badarg`** - If `OptionList` is not a list.

- **`badarg`** - If `Option` is an invalid option.

- **`badarg`** - The same failure as for `demonitor/1`.
""".
-doc #{ category => processes }.
-spec demonitor(MonitorRef, OptionList) -> boolean() when
      MonitorRef :: reference(),
      OptionList :: [Option],
      Option :: flush | info.
demonitor(_MonitorRef, _OptionList) ->
    erlang:nif_error(undefined).

-doc(#{equiv => alias([])}).
-doc(#{since => <<"OTP 24.0">>}).
-doc #{ category => processes }.
-spec alias() -> Alias when
      Alias :: reference().
alias() ->
    erlang:alias([]).

-doc """
Create an alias which can be used when sending messages to the process that
created the alias. When the alias has been deactivated, messages sent using the
alias will be dropped. An alias can be deactivated using `unalias/1`.

Currently available options for [`alias/1`](`alias/1`):

- **`explicit_unalias`** - The alias can only be deactivated via a call to
  [`unalias/1`](`unalias/1`). This is also the default behaviour if no options
  are passed or if `alias/0` is called.

- **`reply`** - The alias will be automatically deactivated when a reply message
  sent via the alias is received. The alias can also still be deactivated via a
  call to [`unalias/1`](`unalias/1`).

- **`priority`** - [](){: #priority_alias } Since OTP @OTP-19198@

  The alias can be used for sending
  [priority messages](`e:system:ref_man_processes.md#priority-messages`) to the
  process that created this alias. An alias created with this option is also
  known as a *priority process alias* or shorter *priority alias*.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  section of the _Erlang Reference Manual_.

Example:

```erlang
server() ->
    receive
        {request, AliasReqId, Request} ->
            Result = perform_request(Request),
            AliasReqId ! {reply, AliasReqId, Result}
    end,
    server().

client(ServerPid, Request) ->
    AliasReqId = alias([reply]),
    ServerPid ! {request, AliasReqId, Request},
    %% Alias will be automatically deactivated if we receive a reply
    %% since we used the 'reply' option...
    receive
        {reply, AliasReqId, Result} -> Result
    after 5000 ->
            unalias(AliasReqId),
            %% Flush message queue in case the reply arrived
            %% just before the alias was deactivated...
            receive {reply, AliasReqId, Result} -> Result
            after 0 -> exit(timeout)
            end
    end.
```

Note that both the server and the client in this example must be executing on at
least OTP 24 systems in order for this to work.

For more information on process aliases see the
[_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section of
the _Erlang Reference Manual_.
""".

-doc #{ category => processes }.
-doc(#{since => <<"OTP 24.0">>}).
-spec alias(Opts) -> Alias when
      Alias :: reference(),
      Opts :: ['explicit_unalias' | 'reply' | 'priority'].

alias(_Opts) ->
    erlang:nif_error(undefined).

-doc """
Deactivate the alias `Alias` previously created by the calling process.

An alias can, for example, be created via `alias/0` or `monitor/3`.
[`unalias/1`](`unalias/1`) will always deactivate the alias regardless of
options used when creating the alias.

Returns true if `Alias` was a currently active alias for current processes;
otherwise, false.

For more information on process aliases see the
[_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section of
the _Erlang Reference Manual_.
""".
-doc(#{since => <<"OTP 24.0">>}).
-doc #{ category => processes }.
-spec unalias(Alias) -> boolean() when
      Alias :: reference().

unalias(_Alias) ->
    erlang:nif_error(undefined).

%% display/1
-doc """
Prints a text representation of `Term` on the standard output.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only. The printed representation may
> contain internal details that do not match the high-level representation of
> the term in Erlang.
""".
-doc #{ category => terms }.
-spec display(Term) -> true when
      Term :: term().
display(_Term) ->
    erlang:nif_error(undefined).

%% display_string/1
-doc false.
-spec display_string(P1) -> true when
      P1 :: string() | binary().
display_string(String) ->
    try erlang:display_string(stderr, String)
    catch error:badarg:ST ->
            [{erlang, display_string, _, [ErrorInfo]}|_] = ST,
            erlang:error(badarg, [String], [ErrorInfo])
    end.

%% display_string/2
-doc false.
-spec display_string(Device, P1) -> true when
      Device :: stdin | stdout | stderr,
      P1 :: string().
display_string(_Stream,_P1) ->
    erlang:nif_error(undefined).

%% dt_append_vm_tag_data/1
-doc false.
-spec dt_append_vm_tag_data(IoData) -> IoDataRet when
      IoData :: iodata(),
      IoDataRet :: iodata().
dt_append_vm_tag_data(_IoData) ->
    erlang:nif_error(undefined).

%% dt_get_tag/0
-doc false.
-spec dt_get_tag() -> binary() | undefined.
dt_get_tag() ->
    erlang:nif_error(undefined).

%%  dt_get_tag_data/0
-doc false.
-spec dt_get_tag_data() -> binary() | undefined.
dt_get_tag_data() ->
    erlang:nif_error(undefined).

%% dt_prepend_vm_tag_data/1
-doc false.
-spec dt_prepend_vm_tag_data(IoData) -> IoDataRet when
      IoData :: iodata(),
      IoDataRet :: iodata().
dt_prepend_vm_tag_data(_IoData) ->
    erlang:nif_error(undefined).

%% dt_put_tag/1
-doc false.
-spec dt_put_tag(IoData) -> binary() | undefined when
      IoData :: iodata().
dt_put_tag(_IoData) ->
    erlang:nif_error(undefined).

%% dt_restore_tag/1
-doc false.
-spec dt_restore_tag(TagData) -> true when
      TagData :: term().
dt_restore_tag(_TagData) ->
    erlang:nif_error(undefined).
    
%% dt_spread_tag/1
-doc false.
-spec dt_spread_tag(boolean()) -> TagData when
      TagData :: term().
dt_spread_tag(_Bool) ->   
    erlang:nif_error(undefined).

%% erase/0
-doc """
Returns the process dictionary and deletes it.

For example:

```erlang
> put(key1, {1, 2, 3}),
put(key2, [a, b, c]),
erase().
[{key1,{1,2,3}},{key2,[a,b,c]}]
```
""".
-doc #{ category => processes }.
-spec erase() -> [{Key, Val}] when
      Key :: term(),
      Val :: term().
erase() ->
    erlang:nif_error(undefined).

%% erase/1
-doc """
Returns the value `Val` associated with `Key` and deletes it from the process
dictionary. Returns `undefined` if no value is associated with `Key`.

The average time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
> put(key1, {merry, lambs, are, playing}),
X = erase(key1),
{X, erase(key1)}.
{{merry,lambs,are,playing},undefined}
```
""".
-doc #{ category => processes }.
-spec erase(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
erase(_Key) ->
    erlang:nif_error(undefined).

%% error/1
%% Shadowed by erl_bif_types: erlang:error/1
-doc """
Raises an exception of class `error` with the reason `Reason`.

As evaluating this function causes an exception to be thrown, it has no return value.

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.
Example:

```erlang
> catch error(foobar).
{'EXIT',{foobar,[{shell,apply_fun,3,
                        [{file,"shell.erl"},{line,906}]},
                 {erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,677}]},
                 {erl_eval,expr,5,[{file,"erl_eval.erl"},{line,430}]},
                 {shell,exprs,7,[{file,"shell.erl"},{line,687}]},
                 {shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
                 {shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}
```
""".
-doc #{ category => processes }.
-spec error(Reason) -> no_return() when
      Reason :: term().
error(_Reason) ->
    erlang:nif_error(undefined).

%% error/2
%% Shadowed by erl_bif_types: erlang:error/2
-doc """
Raises an exception of class `error` with the reason `Reason`. `Args` is
expected to be the list of arguments for the current function or the atom
`none`.

If `Args` is a list, it is used to provide the arguments for the current
function in the stack back-trace. If it is `none`, the arity of the calling
function is used in the stacktrace. As evaluating this function causes an
exception to be raised, it has no return value.

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.
Example:

`test.erl`:

```erlang
-module(test).
-export([example_fun/2]).

example_fun(A1, A2) ->
    erlang:error(my_error, [A1, A2]).
```

Erlang shell:

```erlang
6> c(test).
{ok,test}
7> test:example_fun(arg1,"this is the second argument").
** exception error: my_error
     in function  test:example_fun/2
         called as test:example_fun(arg1,"this is the second argument")
```
""".
-spec error(Reason, Args) -> no_return() when
      Reason :: term(),
      Args :: [term()] | none.
-doc #{ category => processes }.
error(_Reason, _Args) ->
    erlang:nif_error(undefined).

%% error/3
%% Shadowed by erl_bif_types: erlang:error/3
-doc """
Raises an exception of class `error` with the reason `Reason`. `Args` is
expected to be the list of arguments for the current function or the atom
`none`.

If `Args` is a list, it is used to provide the arguments for the current
function in the stack back-trace. If it is `none`, the arity of the calling
function is used in the stacktrace. As evaluating this function causes an
exception to be raised, it has no return value.

If the `error_info` option is given, the `ErrorInfoMap` will be inserted into
the stacktrace. The information given in the `ErrorInfoMap` is to be used by
error formatters such as [`erl_error`](`erl_error:format_exception/4`) to
provide more context around an error.

The default `module` of the `ErrorInfoMap` is the module that the call to
`error/3` is made. The default `function` is `format_error`. See
[`format_error/2`](`c:erl_error:format_error/2`) for more details on how this
Module:Function/2 is to be used

The intent of the exception class `error` is to signal that an unexpected error
has happened (for example, a function is called with a parameter that has an
incorrect type). See the guide about
[errors and error handling](`e:system:errors.md`) for additional information.
""".
-doc(#{since => <<"OTP 24.0">>}).
-doc #{ category => processes }.
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
-doc """
Raises an exception of class `exit` with exit reason `Reason`.

As evaluating this function causes an exception to be raised, it has no return value.

The intent of the exception class `exit` is that the current process should be
stopped (for example when a message telling a process to stop is received).

This function differ from [`error/1,2,3`](`error/1`) by causing an exception of
a different class and by having a reason that does not include the list of
functions from the call stack.

See the guide about [errors and error handling](`e:system:errors.md`) for
additional information.

Example:

```erlang
> exit(foobar).
** exception exit: foobar
> catch exit(foobar).
{'EXIT',foobar}
```

> #### Note {: .info }
>
> If a process calls [`exit(kill)`](`exit/1`) and does not catch the exception,
> it will terminate with exit reason `kill` and also emit exit signals with exit
> reason `kill` (not `killed`) to all linked processes. Such exit signals with
> exit reason `kill` can be trapped by the linked processes. Note that this
> means that signals with exit reason `kill` behave differently depending on how
> they are sent because the signal will be untrappable if a process sends such a
> signal to another process with [`erlang:exit/2`](`exit/2`).
""".
-doc #{ category => processes }.
-spec exit(Reason) -> no_return() when
      Reason :: term().
exit(_Reason) ->
    erlang:nif_error(undefined).

%% exit/2
-doc """
Sends an exit signal with exit reason `Reason` to the process or port identified
by `Dest`. If `Dest` is a reference, the exit signal will *only* affect the
identified process if the reference is an active
[process alias](`e:system:ref_man_processes.md#process-aliases`) of a process
executing on an OTP @OTP-19198@ node or newer.

The following behavior applies if `Reason` is any term, except `normal` or
`kill`, and `P` is the process or port identified by `Dest`:

- If `P` is not [trapping exits](`process_flag/2`), `P` exits with exit reason
  `Reason`.
- If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
  into a message `{'EXIT', From, Reason}`, where `From` is the process
  identifier of the process that sent the exit signal, and delivered to the
  message queue of `P`.

The following behavior applies if `Reason` is the term `normal` and `Dest` is the
identifier of a process `P` which is not the same as the process that invoked
`erlang:exit(Dest, normal)` (the behavior when a process sends a signal with the
`normal` reason to itself is described in the warning):

- If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
  into a message `{'EXIT', From, normal}`, where `From` is the process
  identifier of the process that sent the exit signal, and delivered to `P`'s
  message queue.
- The signal has no effect if `P` is not trapping exits.

If `Reason` is the atom `kill`, that is, if [`exit(Dest, kill)`](`exit/2`) is
called, an untrappable exit signal is sent to the process that is identified by
`Dest`, which unconditionally exits with exit reason `killed`. The exit reason is
changed from `kill` to `killed` to hint to linked processes that the killed
process got killed by a call to [`exit(Dest, kill)`](`exit/2`).

> #### Note {: .info }
>
> The functions [`erlang:exit/1`](`exit/1`) and [`erlang:exit/2`](`exit/2`) are
> named similarly but provide very different functionalities. The
> `erlang:exit/1` function should be used when the intent is to stop the current
> process while `erlang:exit/2` should be used when the intent is to send an
> exit signal to another process. Note also that `erlang:exit/1` raises an
> exception that can be caught while `erlang:exit/2` does not cause any
> exception to be raised.

> #### Warning {: .warning }
>
> The only scenario that has not been covered by the description above is when a
> process `P` sends an exit signal with reason `normal` to itself, that is
> `erlang:exit(self(), normal)`. The behavior in this scenario is as follows:
>
> - If `P` is [trapping exits](`process_flag/2`), the exit signal is transformed
>   into a message `{'EXIT', From, normal}`, where `From` is `P`'s process
>   identifier, and delivered to `P`'s message queue.
> - `P` exits with reason `normal` if `P` is not trapping exits.
>
> Note that the behavior described above is different from when a process sends
> an exit signal with reason `normal` to another process. This is arguably
> strange but this behavior is kept for backward compatibility reasons.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
""".

-doc #{ category => processes }.
-spec exit(Dest, Reason) -> true when
      Dest :: pid() | port() | reference(),
      Reason :: term().
exit(_Dest, _Reason) ->
    erlang:nif_error(undefined).

%% exit/3
-doc """
Provides an option list for modification of the functionality provided by the
`exit/2` BIF. The `Dest` and `Reason` arguments has the same meaning as when
passed to the `exit/2` BIF.

Currently available options:

- **`priority`** -- Since OTP @OTP-19198@

  Send this exit signal as a priority exit signal. In order for
  the signal to be handled as a
  [priority `EXIT` message](`e:system:ref_man_processes.md#priority-messages`)
  by the receiver, this option *must* be passed, `Dest` *must* be an active
  [*priority alias*](#priority_alias) and the receiver *must* be
  [trapping exits](#process_flag_trap_exit).

  If `Dest` is an active priority alias, but this option is not passed, the exit
  signal will be handled as on ordinary exit signal. The same is true, if this
  option is passed, but `Dest` is not an active priority alias.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  and the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  sections of the _Erlang Reference Manual_.

""".

-doc #{ category => processes }.
-doc(#{since => <<"OTP @OTP-19198@">>}).
-spec exit(Dest, Reason, OptList) -> true when
      Dest :: pid() | port() | reference(),
      Reason :: term(),
      OptList :: [priority].
exit(_Pid, _Reason, _OptList) ->
    erlang:nif_error(undefined).

%% exit_signal/2
-doc false.
-spec exit_signal(Pid, Reason) -> true when
      Pid :: pid() | port() | reference(),
      Reason :: term().
exit_signal(_Pid, _Reason) ->
    erlang:nif_error(undefined).

%% external_size/1
-doc """
Calculates, without doing the encoding, the maximum byte size for a term encoded
in the Erlang external term format.

The following condition applies always:

```erlang
> Size1 = byte_size(term_to_binary(Term)),
> Size2 = erlang:external_size(Term),
> true = Size1 =< Size2.
true
```

This is equivalent to a call to:

```erlang
erlang:external_size(Term, [])
```
""".
-doc(#{since => <<"OTP R14B04">>}).
-doc #{ category => terms }.
-spec external_size(Term) -> non_neg_integer() when
      Term :: term().
external_size(_Term) ->
    erlang:nif_error(undefined).

%% external_size/2
-doc """
Calculates, without doing the encoding, the maximum byte size for a term encoded
in the Erlang external term format.

The following condition applies always:

```erlang
> Size1 = byte_size(term_to_binary(Term, Options)),
> Size2 = erlang:external_size(Term, Options),
> true = Size1 =< Size2.
true
```

Option `{minor_version, Version}` specifies how floats are encoded. For a
detailed description, see `term_to_binary/2`.
""".
-doc #{ category => terms }.
-doc(#{since => <<"OTP R14B04">>}).
-spec external_size(Term, Options) -> non_neg_integer() when
      Term :: term(),
      Options :: [compressed |
         {compressed, Level :: 0..9} |
         deterministic |
         {minor_version, Version :: 0..2} |
         local ].
external_size(_Term, _Options) ->
    erlang:nif_error(undefined).

%% finish_loading/2
-doc false.
-spec finish_loading(PreparedCodeList) -> ok | Error when
      PreparedCodeList :: [PreparedCode],
      PreparedCode :: prepared_code(),
      ModuleList :: [module()],
      Error :: {not_purged,ModuleList} | {on_load,ModuleList}.
finish_loading(_List) ->
    erlang:nif_error(undefined).

%% finish_after_on_load/2
-doc false.
-spec finish_after_on_load(P1, P2) -> true when
      P1 :: atom(),
      P2 :: boolean().
finish_after_on_load(_P1, _P2) ->
    erlang:nif_error(undefined).

%% float/1
%% Shadowed by erl_bif_types: erlang:float/1
-doc """
Returns a float by converting `Number` to a float.

For example:

```erlang
> float(55).
55.0
```

> #### Note {: .info }
>
> If used on the top level in a guard, it tests whether the argument is a
> floating point number; for clarity, use `is_float/1` instead.
>
> When [`float/1`](`float/1`) is used in an expression in a guard, such as
> '`float(A) == 4.0`', it converts a number as described earlier.
""".
-doc #{ category => terms }.
-spec float(Number) -> float() when
      Number :: number().
float(_Number) ->
    erlang:nif_error(undefined).

%% float_to_binary/1
-doc(#{ equiv => float_to_binary(Float,[{scientific,20}]) }).
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec float_to_binary(Float) -> binary() when
      Float :: float().
float_to_binary(_Float) ->
    erlang:nif_error(undefined).

%% float_to_binary/2
-doc """
Returns a binary corresponding to the text representation of `Float` using fixed
decimal point formatting.

`Options` behaves in the same way as `float_to_list/2`.

For example:

```erlang
> float_to_binary(7.12, [{decimals, 4}]).
<<"7.1200">>
> float_to_binary(7.12, [{decimals, 4}, compact]).
<<"7.12">>
> float_to_binary(7.12, [{scientific, 3}]).
<<"7.120e+00">>
> float_to_binary(7.12, [short]).
<<"7.12">>
> float_to_binary(0.1+0.2, [short]).
<<"0.30000000000000004">>
> float_to_binary(0.1+0.2)
<<"3.00000000000000044409e-01">>
```
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
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
-doc(#{ equiv => float_to_list(Float,[{scientific,20}]) }).
-doc #{ category => terms }.
-spec float_to_list(Float) -> string() when
      Float :: float().
float_to_list(_Float) ->
    erlang:nif_error(undefined).

%% float_to_list/2
-doc """
Returns a string corresponding to the text representation of `Float` using fixed
decimal point formatting.

Available options:

- If option `decimals` is specified, the returned value contains at most
  `Decimals` number of digits past the decimal point. If the number does not fit
  in the internal static buffer of 256 bytes, the function throws `badarg`.
- If option `compact` is specified, the trailing zeros at the end of the list
  are truncated. This option is only meaningful together with option `decimals`.
- If option `scientific` is specified, the float is formatted using scientific
  notation with `Decimals` digits of precision.
- If option `short` is specified, the float is formatted with the smallest
  number of digits that still guarantees that
  `F =:= list_to_float(float_to_list(F, [short]))`. When the float is inside the
  range (-2⁵³, 2⁵³), the notation that yields the smallest number of characters
  is used (scientific notation or normal decimal notation). Floats outside the
  range (-2⁵³, 2⁵³) are always formatted using scientific notation to avoid
  confusing results when doing arithmetic operations.
- If `Options` is `[]`, the function behaves as `float_to_list/1`.

Examples:

```erlang
> float_to_list(7.12, [{decimals, 4}]).
"7.1200"
> float_to_list(7.12, [{decimals, 4}, compact]).
"7.12"
> float_to_list(7.12, [{scientific, 3}]).
"7.120e+00"
> float_to_list(7.12, [short]).
"7.12"
> float_to_list(0.1+0.2, [short]).
"0.30000000000000004"
> float_to_list(0.1+0.2)
"3.00000000000000044409e-01"
```

In the last example, [`float_to_list(0.1+0.2)`](`float_to_list/1`) evaluates to
`"3.00000000000000044409e-01"`. The reason for this is explained in
[Representation of Floating Point Numbers](`e:system:data_types.md#float_representation_problem`).
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
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
-doc """
Returns the largest integer not greater than `Number`.

For example:

```erlang
> floor(-10.5).
-11
```
""".
-doc(#{since => <<"OTP 20.0">>}).
-doc #{ category => terms }.
-spec floor(Number) -> integer() when
      Number :: number().
floor(_) ->
    erlang:nif_error(undef).

%% fun_info/2
-doc """
Returns information about `Fun` as specified by `Item`, in the form
`{Item,Info}`.

For any fun, `Item` can be any of the atoms `module`, `name`, `arity`, `env`, or
`type`.

For a local fun, `Item` can also be any of the atoms `index`, `new_index`,
`new_uniq`, `uniq`, and `pid`. For an external fun, the value of any of these
items is always the atom `undefined`.

See [`erlang:fun_info/1`](`fun_info/1`).
""".
-doc #{ category => terms }.
-spec fun_info(Fun, Item) -> {Item, Info} when
      Fun :: function(),
      Item :: fun_info_item(),
      Info :: term().
fun_info(_Fun, _Item) ->
    erlang:nif_error(undefined).

%% fun_info_mfa/1
-doc false.
-spec fun_info_mfa(Fun) -> {Mod, Name, Arity} when
      Fun :: function(),
      Mod :: atom(),
      Name :: atom(),
      Arity :: non_neg_integer().
fun_info_mfa(_Fun) ->
    erlang:nif_error(undefined).

%% fun_to_list/1
-doc """
Returns `String` that represents the code that created `Fun`.

`String` has the following form, if `Fun` was created by a
[fun expression](`e:system:expressions.md#fun-expressions`) of the form
`fun ModuleName:FuncName/Arity`:

`"fun ModuleName:FuncName/Arity"`

The form of `String` when `Fun` is created from other types of
[fun expressions](`e:system:expressions.md#fun-expressions`) differs depending
on if the fun expression was executed while executing compiled code or if the
fun expression was executed while executing uncompiled code (uncompiled
escripts, the Erlang shell, and other code executed by the erl_eval module):

- **compiled code** - `"#Fun<M.I.U>"`, where M, I and U correspond to the values
  named `module`, `index` and `uniq` in the result of
  [`erlang:fun_info(Fun)`](`fun_info/1`).

- **uncompiled code** - All funs created from fun expressions in uncompiled code
  with the same arity are mapped to the same list by
  [`fun_to_list/1`](`fun_to_list/1`).

> #### Note {: .info }
>
> Generally, one can not use [`fun_to_list/1`](`fun_to_list/1`) to check if two
> funs are equal as [`fun_to_list/1`](`fun_to_list/1`) does not take the fun's
> environment into account. See [`erlang:fun_info/1`](`fun_info/1`) for how to
> get the environment of a fun.

> #### Change {: .info }
>
> The output of [`fun_to_list/1`](`fun_to_list/1`) can differ between Erlang
> implementations and may change in future versions.

Examples:

```erlang
-module(test).
-export([add/1, add2/0, fun_tuple/0]).
add(A) -> fun(B) -> A + B end.
add2() -> fun add/1.
fun_tuple() -> {fun() -> 1 end, fun() -> 1 end}.
```

```erlang
> {fun test:add/1, test:add2()}.
{fun test:add/1,#Fun<test.1.107738983>}
```

Explanation: `fun test:add/1` is upgradable but `test:add2()` is not upgradable.

```erlang
> {test:add(1), test:add(42)}.
{#Fun<test.0.107738983>,#Fun<test.0.107738983>}
```

Explanation: `test:add(1)` and `test:add(42)` has the same string representation
as the environment is not taken into account.

```erlang
>test:fun_tuple().
{#Fun<test.2.107738983>,#Fun<test.3.107738983>}
```

Explanation: The string representations differ because the funs come from
different fun expressions.

```erlang
> {fun() -> 1 end, fun() -> 1 end}. >
{#Fun<erl_eval.45.97283095>,#Fun<erl_eval.45.97283095>}
```

Explanation: All funs created from fun expressions of this form in uncompiled
code with the same arity are mapped to the same list by
[`fun_to_list/1`](`fun_to_list/1`).
""".
-doc #{ category => terms }.
-spec fun_to_list(Fun) -> String :: string() when
      Fun :: function().
fun_to_list(_Fun) ->
    erlang:nif_error(undefined).

%% function_exported/3
-doc """
Returns `true` if the module `Module` is
[current](`e:system:code_loading.md#code-replacement`) and contains an exported
function `Function/Arity`, or if there is a BIF (a built-in function implemented
in C) with the specified name, otherwise returns `false`.
""".
-doc #{ category => code }.
-spec function_exported(Module, Function, Arity) -> boolean() when
      Module :: module(),
      Function :: atom(),
      Arity :: arity().
function_exported(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

%% garbage_collect/0
-doc """
Forces an immediate garbage collection of the executing process.

The function is not to be used unless it has been noticed (or there are good
reasons to suspect) that the spontaneous garbage collection will occur too late
or not at all.

> #### Warning {: .warning }
>
> Improper use can seriously degrade system performance.
""".
-doc #{ category => processes }.
-spec garbage_collect() -> true.
garbage_collect() ->
    erts_internal:garbage_collect(major).

%% garbage_collect/1
-doc(#{ equiv => garbage_collect(Pid, []) }).
-doc #{ category => processes }.
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
-doc """
Garbage collects the node local process identified by `Pid`.

`Option`:

- **`{async, RequestId}`** - The function
  [`garbage_collect/2`](`garbage_collect/2`) returns the value `async`
  immediately after the request has been sent. When the request has been
  processed, the process that called this function is passed a message on the
  form `{garbage_collect, RequestId, GCResult}`.

- **`{type, 'major' | 'minor'}`** - Triggers garbage collection of requested
  type. Default value is `'major'`, which would trigger a fullsweep GC. The
  option `'minor'` is considered a hint and may lead to either minor or major GC
  run.

If `Pid` equals `self/0`, and no `async` option has been passed, the garbage
collection is performed at once, that is, the same as calling
`garbage_collect/0`. Otherwise a request for garbage collection is sent to the
process identified by `Pid`, and will be handled when appropriate. If no `async`
option has been passed, the caller blocks until `GCResult` is available and can
be returned.

`GCResult` informs about the result of the garbage collection request as
follows:

- **`true`** - The process identified by `Pid` has been garbage collected.

- **`false`** - No garbage collection was performed, as the process identified
  by `Pid` terminated before the request could be satisfied.

Notice that the same caveats apply as for `garbage_collect/0`.

Failures:

- **`badarg`** - If `Pid` is not a node local process identifier.

- **`badarg`** - If `OptionList` is an invalid list of options.
""".
-doc(#{since => <<"OTP 17.0">>}).
-doc #{ category => processes }.
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
-doc false.
-spec garbage_collect_message_area() -> boolean().
garbage_collect_message_area() ->
    erlang:nif_error(undefined).

%% get/0
-doc """
Returns the process dictionary as a list of `{Key, Val}` tuples. The items in
the returned list can be in any order.

For example:

```erlang
> put(key1, merry),
put(key2, lambs),
put(key3, {are, playing}),
get().
[{key1,merry},{key2,lambs},{key3,{are,playing}}]
```
""".
-doc #{ category => processes }.
-spec get() -> [{Key, Val}] when
      Key :: term(),
      Val :: term().
get() ->
    erlang:nif_error(undefined).

%% get/1
-doc """
Returns the value `Val` associated with `Key` in the process dictionary, or
`undefined` if `Key` does not exist.

The expected time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
> put(key1, merry),
put(key2, lambs),
put({any, [valid, term]}, {are, playing}),
get({any, [valid, term]}).
{are,playing}
```
""".
-doc #{ category => processes }.
-spec get(Key) -> Val | undefined when
      Key :: term(),
      Val :: term().
get(_Key) ->
    erlang:nif_error(undefined).

%% get_keys/0
-doc """
Returns a list of all keys present in the process dictionary. The items in the
returned list can be in any order.

For example:

```erlang
> put(dog, {animal,1}),
put(cow, {animal,2}),
put(lamb, {animal,3}),
get_keys().
[dog,cow,lamb]
```
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => processes }.
-spec get_keys() -> [Key] when
      Key :: term().
get_keys() ->
    erlang:nif_error(undefined).

%% get_keys/1
-doc """
Returns a list of keys that are associated with the value `Val` in the process
dictionary. The items in the returned list can be in any order.

For example:

```erlang
> put(mary, {1, 2}),
put(had, {1, 2}),
put(a, {1, 2}),
put(little, {1, 2}),
put(dog, {1, 3}),
put(lamb, {1, 2}),
get_keys({1, 2}).
[mary,had,a,little,lamb]
```
""".
-doc #{ category => processes }.
-spec get_keys(Val) -> [Key] when
      Val :: term(),
      Key :: term().
get_keys(_Val) ->
    erlang:nif_error(undefined).

%% get_module_info/1
-doc false.
-spec get_module_info(Module) -> [{Item, term()}] when
      Item :: module | exports | attributes | compile | native | md5,
      Module :: atom().
get_module_info(_Module) ->
    erlang:nif_error(undefined).

%% group_leader/0
-doc """
Returns the process identifier of the group leader for the process evaluating
the function.

Every process is a member of some process group and all groups have a _group
leader_. All I/O from the group is channeled to the group leader. When a new
process is spawned, it gets the same group leader as the spawning process.

Initially, at system startup, `init` is both its own group leader and the group
leader of all processes. During the boot of a system the group leader for
processes will be changed depending on the need of the system. Some examples
where this is done are:

- When an application is started, the top supervisor of that application will
  have its group leader set to the application master. See `application:start/2`
  for more details.
- When running tests, both [`common_test`](`e:common_test:index.html`) and
  `m:eunit` set the group leader in order to capture any I/O from the testcase.
- The [interactive shell](`m:shell`) sets the group leader to intercept I/O.
""".
-doc #{ category => processes }.
-spec group_leader() -> pid().
group_leader() ->
    erlang:nif_error(undefined).

%% group_leader/2
-doc """
Sets the group leader of `Pid` to `GroupLeader`. Typically, this is used when a
process started from a certain shell is to have another group leader than
`init`.

The group leader should be rarely changed in applications with a supervision
tree, because OTP assumes the group leader of their processes is their
application master.

Setting the group leader follows the signal ordering guarantees described in the
[Processes Chapter](`e:system:ref_man_processes.md#signals`) in the _Erlang
Reference Manual_.

See also `group_leader/0` and
[OTP design principles](`e:system:applications.md#stopping`) related to starting
and stopping applications.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
""".
-doc #{ category => processes }.
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
-doc """
Equivalent to calling [`halt(0, [])`](`halt/2`).

For example:

```erlang
> halt().
os_prompt%
```
""".
-doc #{ category => system }.
-spec halt() -> no_return().
halt() ->
    erlang:halt(0, []).

%% halt/1
%% Shadowed by erl_bif_types: erlang:halt/1
-doc """
Equivalent to calling [`halt(HaltType, [])`](`halt/2`).

For example:

```erlang
> halt(17).
os_prompt% echo $?
17
os_prompt%
```
""".
-doc #{ category => system }.
-spec halt(Status :: non_neg_integer()) ->
          no_return();
          (Abort :: abort) ->
          no_return();
          (CrashDumpSlogan :: string()) ->
          no_return().

-dialyzer({no_return, halt/1}).
halt(HaltType) ->
    try
        erlang:halt(HaltType, [])
    catch
	error:Error -> error_with_info(Error, [HaltType])
    end.

%% halt/2
%% Shadowed by erl_bif_types: erlang:halt/2
-type halt_options() ::
        [{flush, boolean()}
         | {flush_timeout, Timeout :: 0..2147483647 | infinity}].

-doc """
Halt the runtime system.

- ```erlang
  halt(Status :: non_neg_integer(), Options :: halt_options())
  ```
  {: #halt_status_2 }

  Halt the runtime system with status code `Status`.

  > #### Note {: .info }
  >
  > On many platforms, the OS supports only status codes 0-255. A too large
  > status code is truncated by clearing the high bits.

  Currently the following options are valid:

  - **`{flush, EnableFlushing}`{: #halt_flush }** - If `EnableFlushing` equals
    `true`, which also is the default behavior, the runtime system will perform
    the following operations before terminating:

    - Flush all outstanding output.
    - Send all Erlang ports exit signals and wait for them to exit.
    - Wait for all async threads to complete all outstanding async jobs.
    - Call all installed [NIF _on halt_ callbacks](erl_nif.md#on_halt).
    - Wait for all ongoing
      [NIF calls with the _delay halt_ setting](erl_nif.md#delay_halt) enabled
      to return.
    - Call all installed `atexit`/`on_exit` callbacks.

    If `EnableFlushing` equals `false`, the runtime system will terminate
    immediately without performing any of the above listed operations.

    > #### Change {: .info }
    >
    > Runtime systems prior to OTP 26.0 called all installed `atexit`/`on_exit`
    > callbacks also when `flush` was disabled, but as of OTP 26.0 this is no
    > longer the case.

  - **`{flush_timeout, Timeout :: 0..2147483647 | infinity}`{: #halt_flush_timeout }** -
    Sets a limit on the time allowed for [flushing](#halt_flush) prior to
    termination of the runtime system. `Timeout` is in milliseconds. The default
    value is determined by the the `erl` [`+zhft <Timeout>`](erl_cmd.md#+zhft)
    command line flag.

    If flushing has been ongoing for `Timeout` milliseconds, flushing operations
    will be interrupted and the runtime system will immediately be terminated
    with the exit code `255`. If flushing is not enabled, the timeout will have
    no effect on the system.

    See also the `erl` [`+zhft <Timeout>`](erl_cmd.md#+zhft) command line flag.
    Note that the shortest timeout set by the command line flag and the
    `flush_timeout` option will be the actual timeout value in effect.

    Since: OTP 27.0

- ```erlang
  halt(Abort :: abort, Options :: halt_options())
  ```
  {: #halt_abort_2 }

  Halt the Erlang runtime system by aborting and produce a core dump if core
  dumping has been enabled in the environment that the runtime system is
  executing in.

  > #### Note {: .info }
  >
  > The [`{flush, boolean()}`](#halt_flush) option will be ignored, and
  > flushing will be disabled.

- ```erlang
  halt(CrashDumpSlogan :: string(), Options :: halt_options())
  ```
  {: #halt_crash_dump_2 }

  Halt the Erlang runtime system and generate an
  [Erlang crash dump](crash_dump.md). The string `CrashDumpSlogan` will be used
  as slogan in the Erlang crash dump created. The slogan will be trunkated if
  `CrashDumpSlogan` is longer than 1023 characters.

  > #### Note {: .info }
  >
  > The [`{flush, boolean()}`](#halt_flush) option will be ignored, and
  > flushing will be disabled.

  > #### Change {: .info }
  >
  > Behavior changes compared to earlier versions:
  >
  > - Before OTP 24.2, the slogan was truncated if `CrashDumpSlogan` was longer
  >   than 200 characters. Now it will be truncated if longer than 1023
  >   characters.
  > - Before OTP 20.1, only code points in the range 0-255 were accepted in the
  >   slogan. Now any Unicode string is valid.
""".
-doc(#{since => <<"OTP R15B01">>}).
-doc #{ category => system }.
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
-doc false.
-spec has_prepared_code_on_load(PreparedCode) -> boolean() when
      PreparedCode :: prepared_code().
has_prepared_code_on_load(_PreparedCode) ->
    erlang:nif_error(undefined).

%% hibernate/0
-doc """
Puts the calling process into a wait state where its memory allocation has been
reduced as much as possible. This is useful if the process does not expect to
receive any messages soon.

The process is awakened when a message is sent to it, and control resumes
normally to the caller. Unlike `erlang:hibernate/3`, it does not discard the
call stack.
""".
-doc #{ since => <<"OTP @OTP-19503@">> }.
-doc #{ category => processes }.
-spec hibernate() -> ok.
hibernate() ->
    %% This function is a fallback used on apply/3; the loader turns this
    %% remote call of ourselves into a special instruction.
    erlang:hibernate().

%% hibernate/3
-doc """
Puts the calling process into a wait state where its memory allocation has been
reduced as much as possible. This is useful if the process does not expect to
receive any messages soon.

The process is awakened when a message is sent to it, and control resumes in
`Module:Function` with the arguments specified by `Args` with the call stack
emptied, meaning that the process terminates when that function returns. Thus
`erlang:hibernate/3` never returns to its caller. The resume function
`Module:Function/Arity` must be exported (`Arity` =:=
[`length(Args)`](`length/1`)).

If the process has any message in its message queue, the process is awakened
immediately in the same way as described earlier.

In more technical terms, `erlang:hibernate/3` discards the call stack for the
process, and then garbage collects the process. After this, all live data is in
one continuous heap. The heap is then shrunken to the exact same size as the
live data that it holds (even if that size is less than the minimum heap size
for the process).

If the size of the live data in the process is less than the minimum heap size,
the first garbage collection occurring after the process is awakened ensures
that the heap size is changed to a size not smaller than the minimum heap size.

Notice that emptying the call stack means that any surrounding `catch` is
removed and must be re-inserted after hibernation. One effect of this is that
processes started using `proc_lib` (also indirectly, such as `gen_server`
processes), are to use `proc_lib:hibernate/3` instead, to ensure that the
exception handler continues to work when the process wakes up.
""".
-doc #{ category => processes }.
-spec hibernate(Module, Function, Args) -> no_return() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
hibernate(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% insert_element/3
-doc """
Returns a new tuple with element `Term` inserted at position `Index` in tuple
`Tuple1`. All elements from position `Index` and upwards are pushed one step
higher in the new tuple `Tuple2`.

For example:

```erlang
> erlang:insert_element(2, {one, two, three}, new).
{one,new,two,three}
```
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec insert_element(Index, Tuple1, Term) -> Tuple2 when
      Index  :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Term   :: term().
insert_element(_Index, _Tuple1, _Term) ->
    erlang:nif_error(undefined).

%% integer_to_binary/1
-doc """
Returns a binary corresponding to the text representation of `Integer`.

For example:

```erlang
> integer_to_binary(77).
<<"77">>
```
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
-spec integer_to_binary(Integer) -> binary() when
      Integer :: integer().
integer_to_binary(_Integer) ->
    erlang:nif_error(undefined).

%% integer_to_list/1
-doc """
Returns a string corresponding to the text representation of `Integer`.

For example:

```erlang
> integer_to_list(77).
"77"
```
""".
-doc #{ category => terms }.
-spec integer_to_list(Integer) -> string() when
      Integer :: integer().
integer_to_list(_Integer) ->
    erlang:nif_error(undefined).

%% iolist_size/1
-doc """
Returns an integer, that is the size in bytes, of the binary that would be the
result of [`iolist_to_binary(Item)`](`iolist_to_binary/1`).

For example:

```erlang
> iolist_size([1,2|<<3,4>>]).
4
```
""".
-doc #{ category => terms }.
-spec iolist_size(Item) -> non_neg_integer() when
      Item :: iolist() | binary().
iolist_size(_Item) ->
    erlang:nif_error(undefined).

%% iolist_to_binary/1
-doc """
Returns a binary that is made from the integers and binaries in
`IoListOrBinary`.

For example:

```erlang
> Bin1 = <<1,2,3>>.
<<1,2,3>>
> Bin2 = <<4,5>>.
<<4,5>>
> Bin3 = <<6>>.
<<6>>
> iolist_to_binary([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6>>
```
""".
-doc #{ category => terms }.
-spec iolist_to_binary(IoListOrBinary) -> binary() when
      IoListOrBinary :: iolist() | binary().
iolist_to_binary(_IoListOrBinary) ->
    erlang:nif_error(undefined).

%% iolist_to_iovec/1
-doc """
Returns an [iovec](`t:iovec/0`) that is made from the integers and binaries in
`IoListOrBinary`. This function is useful when you want to flatten an iolist but
you do not need a single binary. This can be useful for passing the data to nif
functions such as [`enif_inspect_iovec`](erl_nif.md#enif_inspect_iovec) or do
more efficient message passing. The advantage of using this function over
`iolist_to_binary/1` is that it does not have to copy
[off-heap binaries](`e:system:binaryhandling.md#refc_binary`).

For example:

```erlang
> Bin1 = <<1,2,3>>.
<<1,2,3>>
> Bin2 = <<4,5>>.
<<4,5>>
> Bin3 = <<6>>.
<<6>>
%% If you pass small binaries and integers it works as iolist_to_binary
> erlang:iolist_to_iovec([Bin1,1,[2,3,Bin2],4|Bin3]).
[<<1,2,3,1,2,3,4,5,4,6>>]
%% If you pass larger binaries, they are split and returned in a form
%% optimized for calling the C function writev.
> erlang:iolist_to_iovec([<<1>>,<<2:8096>>,<<3:8096>>]).
[<<1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,...>>,
 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   ...>>,
 <<0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,...>>]
```
""".
-doc(#{since => <<"OTP 20.1">>}).
-doc #{ category => terms }.
-spec iolist_to_iovec(IoListOrBinary) -> iovec() when
      IoListOrBinary :: iolist() | binary().
iolist_to_iovec(_IoListOrBinary) ->
    erlang:nif_error(undefined).

%% is_alive/0
-doc """
Returns `true` if the local node is alive (that is, if the node can be part of a
distributed system), otherwise `false`. A node is alive if it is started with:

1. [`"erl -name LONGNAME"`](erl_cmd.md#name) or,
1. [`"erl -sname SHORTNAME"`](erl_cmd.md#sname).

A node can also be alive if it has got a name from a call to
`net_kernel:start/2` and has not been stopped by a call to `net_kernel:stop/0`.
""".
-doc #{ category => distribution }.
-spec is_alive() -> boolean().
is_alive() ->
    erlang:node() =/= nonode@nohost orelse erts_internal:dynamic_node_name().

%% is_builtin/3
-doc """
This BIF is useful for builders of cross-reference tools.

Returns `true` if `Module:Function/Arity` is a BIF implemented in C, otherwise
`false`.
""".
-doc #{ category => code }.
-spec is_builtin(Module, Function, Arity) -> boolean() when
      Module :: module(),
      Function :: atom(),
      Arity :: arity().
is_builtin(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_map_key/2
-doc """
Returns `true` if map `Map` contains `Key` and returns `false` if it does not
contain the `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map.

_Example:_

```erlang
> Map = #{"42" => value}.
#{"42" => value}
> is_map_key("42",Map).
true
> is_map_key(value,Map).
false
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => terms }.
-spec is_map_key(Key, Map) -> boolean() when
    Key :: term(),
    Map :: map().
is_map_key(_,_) ->
    erlang:nif_error(undef).

%% is_process_alive/1
-doc """
`Pid` must refer to a process at the local node.

Returns `true` if the process exists and is alive, that is, is not exiting and
has not exited. Otherwise returns `false`.

If process `P1` calls [`is_process_alive(P2Pid)`](`is_process_alive/1`) it is
guaranteed that all signals, sent from `P1` to `P2` (`P2` is the process with
identifier `P2Pid`) before the call, will be delivered to `P2` before the
aliveness of `P2` is checked. This guarantee means that one can use
[`is_process_alive/1`](`is_process_alive/1`) to let a process `P1` wait until a
process `P2`, which has got an exit signal with reason `kill` from P1, is
killed.

For example:

```erlang
exit(P2Pid, kill),
% P2 might not be killed
is_process_alive(P2Pid),
% P2 is not alive (the call above always return false)
```

See the documentation about [signals](`e:system:ref_man_processes.md#signals`)
and [erlang:exit/2](`exit/2`) for more information about signals and exit
signals.
""".
-doc #{ category => processes }.
-spec is_process_alive(Pid) -> boolean() when
      Pid :: pid().
is_process_alive(_Pid) ->
    erlang:nif_error(undefined).

%% length/1
%% Shadowed by erl_bif_types: erlang:length/1
-doc """
Returns the length of `List`.

For example:

```erlang
> length([1,2,3,4,5,6,7,8,9]).
9
```
""".
-doc #{ category => terms }.
-spec length(List) -> non_neg_integer() when
      List :: [term()].
length(_List) ->
    erlang:nif_error(undefined).

%% link/1
-doc """
Sets up and activates a link between the calling process and another process or
a port identified by `PidOrPort`.

We will from here on call the identified process or port linkee. If the linkee
is a port, it must reside on the same node as the caller.

If one of the participants of a link terminates, it will
[send an exit signal](`e:system:ref_man_processes.md#sending_exit_signals`) to
the other participant. The exit signal will contain the
[exit reason](`e:system:ref_man_processes.md#link_exit_signal_reason`) of the
terminated participant. Other cases when exit signals are triggered due to a
link are when no linkee exist (`noproc` exit reason) and when the connection
between linked processes on different nodes is lost or cannot be established
(`noconnection` exit reason).

An existing link can be removed by calling `unlink/1`. For more information on
links and exit signals due to links, see the _Processes_ chapter in the _Erlang
Reference Manual_:

- [Links](`e:system:ref_man_processes.md#links`)
- [Sending Exit Signals](`e:system:ref_man_processes.md#sending_exit_signals`)
- [Receiving Exit Signals](`e:system:ref_man_processes.md#receiving_exit_signals`)

For historical reasons, [`link/1`](`link/1`) has a strange semi-synchronous
behavior when it is "cheap" to check if the linkee exists or not, and the caller
does not [trap exits](#process_flag_trap_exit). If the above is true
and the linkee does not exist, [`link/1`](`link/1`) will raise a `noproc` error
_exception_. The expected behavior would instead have been that
[`link/1`](`link/1`) returned `true`, and the caller later was sent an exit
signal with `noproc` exit reason, but this is unfortunately not the case. The
`noproc` [exception](`e:system:errors.md#exceptions`) is not to be confused with
an [exit signal](`e:system:ref_man_processes.md#sending_exit_signals`) with exit
reason `noproc`. Currently it is "cheap" to check if the linkee exists when it
is supposed to reside on the same node as the calling process.

The link setup and activation is performed asynchronously. If the link already
exists, or if the caller attempts to create a link to itself, nothing is done. A
detailed description of the [link protocol](erl_dist_protocol.md#link_protocol)
can be found in the _Distribution Protocol_ chapter of the _ERTS User's Guide_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

Failure:

- `badarg` if `PidOrPort` does not identify a process or a node local port.
- `noproc` linkee does not exist and it is "cheap" to check if it exists as
  described above.
""".

-doc #{ category => processes }.
-spec link(PidOrPort) -> true when
      PidOrPort :: pid() | port().
link(_PidOrPort) ->
    erlang:nif_error(undefined).

%% link/2
-doc """
Provides an option list for modification of the link functionality provided by
`link/1`. The `PidOrPort` argument has the same meaning as when passed to
`link/1`.

Currently available options:

- **`priority`** - Since OTP @OTP-19198@

  [Enables priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  of `EXIT` messages due to the link for the calling process. If the link
  already exists without priority message reception enabled for the link,
  priority message reception will be enabled on the existing link. If the link
  already exists with priority message reception enabled and this option is not
  passed or `link/1` is called, priority message reception for this link will be
  disabled.

  Note that priority message reception due to the link is *only* enabled for the
  process that passed this option. If the linked process also wants to enable
  priority message reception, it needs to call `link/2` passing the `priority`
  option itself.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.

""".

-doc #{ category => processes }.
-doc(#{since => <<"OTP @OTP-19198@">>}).
-spec link(PidOrPort, [link_option()]) -> true when
      PidOrPort :: pid() | port().
link(_PidOrPort, _OptList) ->
    erlang:nif_error(undefined).

%% list_to_atom/1
-doc """
Returns the atom whose text representation is `String`.

As from Erlang/OTP 20, `String` may contain any Unicode character. Earlier
versions allowed only ISO-latin-1 characters as the implementation did not allow
Unicode characters above 255.

> #### Note {: .info }
>
> The number of characters that are permitted in an atom name is limited. The
> default limits can be found in the
> [efficiency guide (section System Limits)](`e:system:system_limits.md`).

> #### Note {: .info }
>
> There is a [configurable limit](`e:system:system_limits.md#atoms`)
> on how many atoms that can exist and atoms are not
> garbage collected. Therefore, it is recommended to consider if
> `list_to_existing_atom/1` is a better option than
> [`list_to_atom/1`](`list_to_atom/1`). The default limits can be found in the
> [Efficiency Guide (section System Limits)](`e:system:system_limits.md`).

Example:

```erlang
> list_to_atom("Erlang").
'Erlang'
```
""".
-doc #{ category => terms }.
-spec list_to_atom(String) -> atom() when
      String :: string().
list_to_atom(_String) ->
    erlang:nif_error(undefined).

%% list_to_binary/1
-doc """
Returns a binary that is made from the integers and binaries in `IoList`.

For example:

```erlang
> Bin1 = <<1,2,3>>.
<<1,2,3>>
> Bin2 = <<4,5>>.
<<4,5>>
> Bin3 = <<6>>.
<<6>>
> list_to_binary([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6>>
```
""".
-doc #{ category => terms }.
-spec list_to_binary(IoList) -> binary() when
      IoList :: iolist().
list_to_binary(_IoList) ->
    erlang:nif_error(undefined).

%% list_to_bitstring/1
-doc """
Returns a bitstring that is made from the integers and bitstrings in
`BitstringList`. (The last tail in `BitstringList` is allowed to be a
bitstring.)

For example:

```erlang
> Bin1 = <<1,2,3>>.
<<1,2,3>>
> Bin2 = <<4,5>>.
<<4,5>>
> Bin3 = <<6,7:4>>.
<<6,7:4>>
> list_to_bitstring([Bin1,1,[2,3,Bin2],4|Bin3]).
<<1,2,3,1,2,3,4,5,4,6,7:4>>
```
""".
-doc #{ category => terms }.
-spec list_to_bitstring(BitstringList) -> bitstring() when
      BitstringList :: bitstring_list().
list_to_bitstring(_BitstringList) ->
    erlang:nif_error(undefined).

%% list_to_existing_atom/1
-doc """
Returns the atom whose text representation is `String`, but only if there
already exists such atom. An atom exists if it has been created by the run-time
system by either loading code or creating a term in which the atom is part.

Failure: `badarg` if there does not already exist an atom whose text
representation is `String`.

> #### Note {: .info }
>
> Note that the compiler may optimize away atoms. For example, the compiler will
> rewrite [`atom_to_list(some_atom)`](`atom_to_list/1`) to `"some_atom"`. If
> that expression is the only mention of the atom `some_atom` in the containing
> module, the atom will not be created when the module is loaded, and a
> subsequent call to
> [`list_to_existing_atom("some_atom")`](`list_to_existing_atom/1`) will fail.
""".
-doc #{ category => terms }.
-spec list_to_existing_atom(String) -> atom() when
      String :: string().
list_to_existing_atom(_String) ->
    erlang:nif_error(undefined).

%% list_to_float/1
-doc """
Returns the float whose text representation is `String`. 

For example:

```erlang
> list_to_float("2.2017764e+0").
2.2017764
```

The float string format is the same as the format for
[Erlang float literals](`e:system:data_types.md`) except for that underscores
are not permitted.

Failure: `badarg` if `String` contains a bad representation of a float.
""".
-doc #{ category => terms }.
-spec list_to_float(String) -> float() when
      String :: string().
list_to_float(_String) ->
    erlang:nif_error(undefined).

%% list_to_integer/1
-doc """
Returns an integer whose text representation is `String`.

For example:

```erlang
> list_to_integer("123").
123
```

```erlang
> list_to_integer("-123").
-123
```

```erlang
> list_to_integer("+123234982304982309482093833234234").
123234982304982309482093833234234
```

`String` must contain at least one digit character and can have an optional
prefix consisting of a single "`+`" or "`-`" character (that is, `String` must
match the regular expression `"^[+-]?[0-9]+$"`).

Failure: `badarg` if `String` contains a bad representation of an integer.
""".
-doc #{ category => terms }.
-spec list_to_integer(String) -> integer() when
      String :: string().
list_to_integer(String) ->
    Base = 10,
    case erts_internal:list_to_integer(String, Base) of
        {Int,[]} ->
            Int;
        big ->
            try erlang:list_to_binary(String) of
                Binary ->
                    case big_binary_to_int(Binary, Base) of
                        N when erlang:is_integer(N) ->
                            N;
                        Reason ->
                            error_with_info(Reason, [String])
                    end
            catch
                error:Reason ->
                    error_with_info(Reason, [String])
            end;
        _ ->
            badarg_with_info([String])
    end.

%% list_to_integer/2
-doc """
Returns an integer whose text representation in base `Base` is `String`.

For example:

```erlang
> list_to_integer("3FF", 16).
1023
```

```erlang
> list_to_integer("+3FF", 16).
1023
```

```erlang
> list_to_integer("3ff", 16).
1023
```

```erlang
> list_to_integer("3fF", 16).
1023
```

```erlang
> list_to_integer("-3FF", 16).
-1023
```

For example, when `Base` is 16, `String` must match the regular expression
`"^[+-]?([0-9]|[A-F]|[a-f])+$"`.

Failure: `badarg` if `String` contains a bad representation of an integer.
""".
-doc #{ category => terms }.
-spec list_to_integer(String, Base) -> integer() when
      String :: string(),
      Base :: 2..36.
list_to_integer(String, Base) ->
    case erts_internal:list_to_integer(String, Base) of
        {Int,[]} ->
            Int;
        big ->
            try erlang:list_to_binary(String) of
                Binary ->
                    case big_binary_to_int(Binary, Base) of
                        N when erlang:is_integer(N) ->
                            N;
                        Reason ->
                            error_with_info(Reason, [String,Base])
                    end
            catch
                error:Reason ->
                    error_with_info(Reason, [String,Base])
            end;
        _ ->
            badarg_with_info([String,Base])
    end.

%% list_to_pid/1
-doc """
Returns a process identifier whose text representation is a `String`.

For example:

```erlang
> list_to_pid("<0.4.1>").
<0.4.1>
```

Failure: `badarg` if `String` contains a bad representation of a process
identifier.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.
""".
-doc #{ category => terms }.
-spec list_to_pid(String) -> pid() when
      String :: string().
list_to_pid(_String) ->
    erlang:nif_error(undefined).

%% list_to_port/1
-doc """
Returns a port identifier whose text representation is a `String`.

For example:

```erlang
> list_to_port("#Port<0.4>").
#Port<0.4>
```

Failure: `badarg` if `String` contains a bad representation of a port
identifier.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.
""".
-doc(#{since => <<"OTP 20.0">>}).
-doc #{ category => terms }.
-spec list_to_port(String) -> port() when
      String :: string().
list_to_port(_String) ->
    erlang:nif_error(undefined).
 
%% list_to_ref/1
-doc """
Returns a reference whose text representation is a `String`.

For example:

```erlang
> list_to_ref("#Ref<0.4192537678.4073193475.71181>").
#Ref<0.4192537678.4073193475.71181>
```

Failure: `badarg` if `String` contains a bad representation of a reference.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.
""".
-doc(#{since => <<"OTP 20.0">>}).
-doc #{ category => terms }.
-spec list_to_ref(String) -> reference() when
      String :: string().
list_to_ref(_String) ->
    erlang:nif_error(undefined).

%% list_to_tuple/1
-doc """
Returns a tuple corresponding to `List`, for example

```erlang
> list_to_tuple([share, ['Ericsson_B', 163]]).
{share, ['Ericsson_B', 163]}
```

`List` can contain any Erlang terms.
""".
-doc #{ category => terms }.
-spec list_to_tuple(List) -> tuple() when
      List :: [term()].
list_to_tuple(_List) ->
    erlang:nif_error(undefined).

%% loaded/0
-doc """
Returns a list of all loaded Erlang modules (current and old code), including
preloaded modules.

See also `m:code`.
""".
-doc #{ category => code }.
-spec loaded() -> [Module] when
      Module :: module().
loaded() ->
    erlang:nif_error(undefined).

%% localtime/0
-doc """
Returns the current local date and time,
`{{Year, Month, Day}, {Hour, Minute, Second}}`.

For example:

```erlang
> erlang:localtime().
{{1996,11,6},{14,45,17}}
```

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).
""".
-doc #{ category => time }.
-spec localtime() -> DateTime when
      DateTime :: calendar:datetime().
localtime() ->
    erlang:nif_error(undefined).

%% make_ref/0
-doc """
Returns a [unique reference](`e:system:system_limits.md#unique_references`). The
reference is unique among connected nodes.

> #### Warning {: .warning }
>
> Before OTP 23 when a node is restarted multiple times with the same node name,
> references created on a newer node can be mistaken for a reference created on
> an older node with the same node name.
""".
-doc #{ category => terms }.
-spec make_ref() -> reference().
make_ref() ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:map_size/1
-doc """
Returns an integer, which is the number of key-value pairs in `Map`.

For example:

```erlang
> map_size(#{a=>1, b=>2, c=>3}).
3
```
""".
-doc(#{since => <<"OTP 17.0">>}).
-doc #{ category => terms }.
-spec map_size(Map) -> non_neg_integer() when
      Map :: map().
map_size(_Map) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:map_get/2
-doc """
Returns value `Value` associated with `Key` if `Map` contains `Key`.

The call fails with a `{badmap,Map}` exception if `Map` is not a map, or with a
`{badkey,Key}` exception if no value is associated with `Key`.

_Example:_

```erlang
> Key = 1337,
  Map = #{42 => value_two,1337 => "value one","a" => 1},
  map_get(Key,Map).
"value one"
```
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => terms }.
-spec map_get(Key, Map) -> Value when
      Map :: map(),
      Key :: any(),
      Value :: any().
map_get(_Key, _Map) ->
    erlang:nif_error(undefined).

%% match_spec_test/3
-doc """
Tests a match specification used in calls to `ets:select/2` and
`trace:function/4`.

The function tests both a match specification for "syntactic" correctness and
runs the match specification against the object.
If the match specification contains errors, the tuple
`{error, Errors}` is returned, where `Errors` is a list of natural language
descriptions of what was wrong with the match specification.

If `Type` is `table`, the object to match against is to be a tuple. The function
then returns `{ok,Result,[],Warnings}`, where `Result` is what would have been
the result in a real `ets:select/2` call, or `false` if the match specification
does not match the object tuple.

If `Type` is `trace`, the object to match against is to be a list. The function
returns `{ok, Result, Flags, Warnings}`, where `Result` is one of the following:

- `true` if a trace message is to be emitted
- `false` if a trace message is not to be emitted
- The message term to be appended to the trace message

`Flags` is a list containing all the trace flags to be enabled, currently this
is only `return_trace`.

This is a useful debugging and test tool, especially when writing complicated
match specifications.

See also `ets:test_ms/2`.
""".
-doc(#{since => <<"OTP 19.0">>}).
-doc #{ category => terms }.
-spec match_spec_test(MatchAgainst, MatchSpec, Type) -> TestResult when
      MatchAgainst :: [term()] | tuple(),
      MatchSpec :: term(),
      Type :: table | trace,
      TestResult :: {ok, term(), [return_trace], [ {error | warning, string()} ]} | {error, [ {error | warning, string()} ]}.
match_spec_test(_P1, _P2, _P3) ->
    erlang:nif_error(undefined).

%% md5/1
-doc """
Computes an MD5 message digest from `Data`, where the length of the digest is
128 bits (16 bytes). `Data` is a binary or a list of small integers and
binaries.

For more information about MD5, see
[RFC 1321 - The MD5 Message-Digest Algorithm](https://www.ietf.org/rfc/rfc1321.txt).

> #### Warning {: .warning }
>
> The MD5 Message-Digest Algorithm is _not_ considered safe for code-signing or
> software-integrity purposes.
""".
-doc #{ category => checksum }.
-spec md5(Data) -> Digest when
      Data :: iodata(),
      Digest :: binary().
md5(_Data) ->
    erlang:nif_error(undefined).

%% md5_final/1
-doc """
Finishes the update of an MD5 `Context` and returns the computed `MD5` message
digest.
""".
-doc #{ category => checksum }.
-spec md5_final(Context) -> Digest when
      Context :: binary(),
      Digest :: binary().
md5_final(_Context) ->
    erlang:nif_error(undefined).

%% md5_init/0
-doc """
Creates an MD5 context, to be used in the following calls to
[`md5_update/2`](`md5_update/2`).
""".
-doc #{ category => checksum }.
-spec md5_init() -> Context when
      Context :: binary().
md5_init() ->
    erlang:nif_error(undefined).

%% md5_update/2
-doc "Update an MD5 `Context` with `Data` and returns a `NewContext`.".
-doc #{ category => checksum }.
-spec md5_update(Context, Data) -> NewContext when
      Context :: binary(),
      Data :: iodata(),
      NewContext :: binary().
md5_update(_Context, _Data) ->
    erlang:nif_error(undefined).

%% module_loaded/1
-doc """
Returns `true` if the module `Module` is loaded as
[_current code_](`e:system:code_loading.md#code-replacement`); otherwise,
`false`. It does not attempt to load the module.
""".
-doc #{ category => code }.
-spec module_loaded(Module) -> boolean() when
      Module :: module().
module_loaded(_Module) ->
    erlang:nif_error(undefined).

-type registered_name() :: atom().
-type registered_process_identifier() :: registered_name() | {registered_name(), node()}.
-type monitor_process_identifier() :: pid() | registered_process_identifier().
-type monitor_port_identifier() :: port() | registered_name().
-doc "See `monitor/3`.".
-type monitor_option() :: {'alias', 'explicit_unalias' | 'demonitor' | 'reply_demonitor'}
                        | {'tag', term()} | priority.
-doc """
See `link/2`.

Since OTP @OTP-19198@"
""".
-type link_option() :: priority.

%% monitor/2
-doc """
Sends a monitor request of type `Type` to the entity identified by `Item`.

If the monitored entity does not exist or it changes monitored state, the caller
of `monitor/2` is notified by a message on the following format:
{: #monitor_message}

```erlang
{Tag, MonitorRef, Type, Object, Info}
```

> #### Note {: .info }
>
> The monitor request is an asynchronous signal. That is, it takes time before
> the signal reaches its destination.

`Type` can be one of the following atoms: `process`, `port` or `time_offset`.

A `process` or `port` monitor is triggered only once, after that it is removed
from both monitoring process and the monitored entity. Monitors are fired when
the monitored process or port terminates, does not exist at the moment of
creation, or if the connection to it is lost. If the connection to it is lost,
we do not know if it still exists. The monitoring is also turned off when
`demonitor/1` is called.

A `process` or `port` monitor by name resolves the `RegisteredName` to `t:pid/0`
or `t:port/0` only once at the moment of monitor instantiation, later changes to
the name registration will not affect the existing monitor.

When a `process` or `port` monitor is triggered, a `'DOWN'` message is sent that
has the following pattern:

```erlang
{'DOWN', MonitorRef, Type, Object, Info}
```

In the monitor message `MonitorRef` and `Type` are the same as described
earlier, and:

- **`Object`** - The monitored entity, which triggered the event. When
  monitoring a process or a local port, `Object` will be equal to the `t:pid/0`
  or `t:port/0` that was being monitored. When monitoring process or port by
  name, `Object` will have format `{RegisteredName, Node}` where
  `RegisteredName` is the name which has been used with
  `monitor/2` call and `Node` is local or remote node name (for
  ports monitored by name, `Node` is always local node name).

- **`Info`** - Either the exit reason of the process, `noproc` (process or port
  did not exist at the time of monitor creation), or `noconnection` (no
  connection to the node where the monitored process resides).

- **Monitoring a `process`{: #monitor_process }** - Creates monitor between the
  current process and another process identified by `Item`, which can be a
  `t:pid/0` (local or remote), an atom `RegisteredName` or a tuple
  `{RegisteredName, Node}` for a registered process, located elsewhere.

  > #### Change {: .info }
  >
  > Before ERTS 10.0 (OTP 21.0), monitoring a process could fail with `badarg`
  > if the monitored process resided on a primitive node (such as erl_interface
  > or jinterface), where remote process monitoring is not implemented.
  >
  > Now, such a call to `monitor` will instead succeed and a monitor is created.
  > But the monitor will only supervise the connection. That is, a
  > `{'DOWN', _, process, _, noconnection}` is the only message that may be
  > received, as the primitive node has no way of reporting the status of the
  > monitored process.

- **Monitoring a `port`{: #monitor_port }** - Creates monitor between the
  current process and a port identified by `Item`, which can be a `t:port/0`
  (only local), an atom `RegisteredName` or a tuple `{RegisteredName, Node}` for
  a registered port, located on this node. Note, that attempt to monitor a
  remote port will result in `badarg`.

  Available since OTP 19.0.

- **Monitoring a `time_offset`{: #monitor_time_offset }** - Monitors changes in
  `time_offset/0` between
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
  [Erlang system time](time_correction.md#erlang-system-time). One valid `Item`
  exists in combination with the `time_offset Type`, namely the atom
  `clock_service`. Notice that the atom `clock_service` is _not_ the registered
  name of a process. In this case it serves as an identifier of the runtime
  system internal clock service at current runtime system instance.

  The monitor is triggered when the time offset is changed. This either if the
  time offset value is changed, or if the offset is changed from preliminary to
  final during
  [finalization of the time offset](#system_flag_time_offset) when the
  [single time warp mode](time_correction.md#single-time-warp-mode) is used.
  When a change from preliminary to final time offset is made, the monitor is
  triggered once regardless of whether the time offset value was changed or not.

  If the runtime system is in
  [multi time warp mode](time_correction.md#multi-time-warp-mode), the time
  offset is changed when the runtime system detects that the
  [OS system time](time_correction.md#os-system-time) has changed. The runtime
  system does, however, not detect this immediately when it occurs. A task
  checking the time offset is scheduled to execute at least once a minute, so
  under normal operation this is to be detected within a minute, but during
  heavy load it can take longer time.

  The monitor is _not_ automatically removed after it has been triggered. That
  is, repeated changes of the time offset trigger the monitor repeatedly.

  When the monitor is triggered a `'CHANGE'` message is sent to the monitoring
  process. A `'CHANGE'` message has the following pattern:

  ```erlang
  {'CHANGE', MonitorRef, Type, Item, NewTimeOffset}
  ```

  where `MonitorRef`, `Type`, and `Item` are the same as described above, and
  `NewTimeOffset` is the new time offset.

  When the `'CHANGE'` message has been received you are guaranteed not to
  retrieve the old time offset when calling
  `erlang:time_offset/0`. Notice that you can observe the
  change of the time offset when calling `erlang:time_offset/0` before you get
  the `'CHANGE'` message.

  Available since OTP 18.0.

Making several calls to `monitor/2` for the same `Item` and/or
`Type` is not an error; it results in as many independent monitoring instances.

The monitor functionality is expected to be extended. That is, other `Type`s and
`Item`s are expected to be supported in a future release.

> #### Note {: .info }
>
> If or when `monitor/2` is extended, other possible values for
> `Tag`, `Object`, and `Info` in the monitor message will be introduced.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
""".
-doc #{ category => processes }.
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
-doc """
Provides an option list for modification of monitoring functionality provided by
`monitor/2`. The `Type` and `Item` arguments have the same meaning as when
passed to [`monitor/2`](`monitor/2`).

Currently available options:

- **`{alias, UnaliasOpt}`** - The returned monitor reference will also become an
  alias for the calling process. That is, the returned reference can be used for
  sending messages to the calling process. See also `alias/0`. The `UnaliasOpt`
  determines how the alias should be deactivated.

  - **`explicit_unalias`** - Only an explicit call to `unalias/1` will
    deactivate the alias.

  - **`demonitor`** - The alias will be automatically deactivated when the
    monitor is removed. This either via an explicit call to `demonitor/1` or
    when it is automatically removed at the same time as a `'DOWN'` message is
    delivered due to the monitor. The alias can also still be deactivated via a
    call to [`unalias/1`](`unalias/1`).

  - **`reply_demonitor`** - The alias will be automatically deactivated when the
    monitor is removed (see `demonitor` option above) or a reply message sent
    via the alias is received. When a reply message is received via the alias
    the monitor will also be automatically removed. This is useful in
    client/server scenarios when a client monitors the server and will get the
    reply via the alias. Once the response is received both the alias and the
    monitor will be automatically removed regardless of whether the response is
    a reply or a `'DOWN'` message. The alias can also still be deactivated via a
    call to [`unalias/1`](`unalias/1`). Note that if the alias is removed using
    the [`unalias/1`](`unalias/1`) BIF, the monitor will still be left active.

  Example:

  ```erlang
  server() ->
      receive
          {request, AliasReqId, Request} ->
              Result = perform_request(Request),
              AliasReqId ! {reply, AliasReqId, Result}
      end,
      server().

  client(ServerPid, Request) ->
      AliasMonReqId = monitor(process, ServerPid, [{alias, reply_demonitor}]),
      ServerPid ! {request, AliasMonReqId, Request},
      %% Alias as well as monitor will be automatically deactivated if we
      %% receive a reply or a 'DOWN' message since we used 'reply_demonitor'
      %% as unalias option...
      receive
          {reply, AliasMonReqId, Result} ->
              Result;
          {'DOWN', AliasMonReqId, process, ServerPid, ExitReason} ->
              error(ExitReason)
      end.
  ```

  Note that both the server and the client in this example must be executing on
  at least OTP 24 systems in order for this to work.

  For more information on process aliases see the
  [_Process Aliases_](`e:system:ref_man_processes.md#process-aliases`) section
  of the _Erlang Reference Manual_.

- **`{tag, UserDefinedTag}`** - Replace the default `Tag` with `UserDefinedTag`
  in the [monitor message](#monitor_message) delivered when the
  monitor is triggered. For example, when monitoring a process, the `'DOWN'` tag
  in the down message will be replaced by `UserDefinedTag`.

  An example of how the `{tag, UserDefinedTag}` option can be used in order to
  enable the new
  [selective receive optimization](`e:system:eff_guide_processes.md#receiving-messages`),
  introduced in OTP 24, when making multiple requests to different servers:

  ```erlang
  server() ->
      receive
          {request, From, ReqId, Request} ->
              Result = perform_request(Request),
              From ! {reply, self(), ReqId, Result}
      end,
      server().

  client(ServerPids, Request) when is_list(ServerPids) ->
      ReqId = make_ref(),
      lists:foreach(fun (ServerPid) ->
                            _ = monitor(process, ServerPid,
                                        [{tag, {'DOWN', ReqId}}]),
                            ServerPid ! {request, self(), ReqId, Request}
                    end,
                    ServerPids),
      receive_replies(ReqId, length(ServerPids), []).

  receive_replies(_ReqId, 0, Acc) ->
      Acc;
  receive_replies(ReqId, N, Acc) ->
      %% The compiler will detect that we match on the 'ReqId'
      %% reference in all clauses, and will enable the selective
      %% receive optimization which makes the receive able to
      %% skip past all messages present in the message queue at
      %% the time when the 'ReqId' reference was created...
      Res = receive
                {reply, ServerPid, ReqId, Result} ->
                    %% Here we typically would have deactivated the
                    %% monitor by a call to demonitor(Mon, [flush]) but
                    %% we ignore this in this example for simplicity...
                    {ok, ServerPid, Result};
                {{'DOWN', ReqId}, _Mon, process, ServerPid, ExitReason} ->
                    {error, ServerPid, ExitReason}
            end,
      receive_replies(ReqId, N-1, [Res | Acc]).
  ```

  In order for this example to work as intended, the client must be executing on
  at least an OTP 24 system, but the servers may execute on older systems.

- **`priority`** - Since OTP @OTP-19198@

  [Enables priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  of the monitor message(s) sent when this monitor is triggered for the calling
  process.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.
""".

-doc #{ category => processes }.
-doc(#{since => <<"OTP 24.0">>}).
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
-doc """
Monitor the status of the node `Node`. If `Flag` is `true`, monitoring is turned
on. If `Flag` is `false`, monitoring is turned off.

Making several calls to [`monitor_node(Node, true)`](`monitor_node/2`) for the
same `Node` is not an error; it results in as many independent monitoring
instances.

If `Node` fails or does not exist, the message `{nodedown, Node}` is delivered
to the process. If a process has made two calls to
[`monitor_node(Node, true)`](`monitor_node/2`) and `Node` terminates, two
`nodedown` messages are delivered to the process. If there is no connection to
`Node`, an attempt is made to create one. If this fails, a `nodedown` message is
delivered.

The delivery of the `nodedown` signal is not ordered with respect to other link
or monitor signals from the node that goes down. If you need a guarantee that
all signals from the remote node has been delivered before the `nodedown` signal
is sent, you should use `net_kernel:monitor_nodes/1`.

Nodes connected through hidden connections can be monitored as any other nodes.

Failure: `notalive` if the local node is not alive.
""".
-doc #{ category => distribution }.
-spec monitor_node(Node, Flag) -> true when
      Node :: node(),
      Flag :: boolean().
monitor_node(_Node, _Flag) ->
    erlang:nif_error(undefined).

%% monitor_node/3
-doc """
Behaves as `monitor_node/2` except that it allows an extra option to be
specified, namely `allow_passive_connect`.

This option allows the BIF to wait the normal network connection time-out
for the _monitored node_ to connect itself, even if it cannot be actively
connected from this node (that is, it is blocked). The state where this can
be useful can only be achieved by using the Kernel option `dist_auto_connect once`.
If that option is not used, option `allow_passive_connect` has no effect.

> #### Note {: .info }
>
> Option `allow_passive_connect` is used internally and is seldom needed in
> applications where the network topology and the Kernel options in effect are
> known in advance.

Failure: `badarg` if the local node is not alive or the option list is
malformed.
""".
-doc #{ category => distribution }.
-spec monitor_node(Node, Flag, Options) -> true when
      Node :: node(),
      Flag :: boolean(),
      Options :: [Option],
      Option :: allow_passive_connect.
monitor_node(_Node, _Flag, _Options) ->
    erlang:nif_error(undefined).

%% nif_error/1
%% Shadowed by erl_bif_types: erlang:nif_error/1
-doc """
Works exactly like `error/1`, but Dialyzer thinks that this BIF will return an
arbitrary term. When used in a stub function for a NIF to generate an exception
when the NIF library is not loaded, Dialyzer does not generate false warnings.
""".
-doc(#{since => <<"OTP R14B">>}).
-doc #{ category => processes }.
-spec nif_error(Reason) -> no_return() when
      Reason :: term().
nif_error(_Reason) ->
    erlang:nif_error(undefined).

%% nif_error/2
%% Shadowed by erl_bif_types: erlang:nif_error/2
-doc """
Works exactly like `error/2`, but Dialyzer thinks that this BIF will return an
arbitrary term. When used in a stub function for a NIF to generate an exception
when the NIF library is not loaded, Dialyzer does not generate false warnings.
""".
-doc(#{since => <<"OTP R14B">>}).
-doc #{ category => processes }.
-spec nif_error(Reason, Args) -> no_return() when
      Reason :: term(),
      Args :: [term()].
nif_error(_Reason, _Args) ->
    erlang:nif_error(undefined).

%% node/0
%% Shadowed by erl_bif_types: erlang:node/0
-doc """
Returns the name of the local node. If the node is not alive, `nonode@nohost` is
returned instead.
""".
-doc #{ category => distribution }.
-spec node() -> Node when
      Node :: node().
node() ->
    erlang:nif_error(undefined).

%% node/1
%% Shadowed by erl_bif_types: erlang:node/1
-doc """
Returns the node where `Arg` originates. `Arg` can be a process identifier, a
reference, or a port. If `Arg` originates from the local node and the local node
is not alive, `nonode@nohost` is returned.
""".
-doc #{ category => terms }.
-spec node(Arg) -> Node when
      Arg :: pid() | port() | reference(),
      Node :: node().
node(_Arg) ->
    erlang:nif_error(undefined).

%% now/0
-doc """
> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>
> For more information, see section
> [Time and Time Correction](time_correction.md) in the User's Guide.
> Specifically, section [Dos and Dont's](time_correction.md#Dos_and_Donts)
> describes what to use instead of `erlang:now/0`.

Returns the tuple `{MegaSecs, Secs, MicroSecs}`, which is the elapsed time since
00:00 GMT, January 1, 1970 (zero hour), if provided by the underlying OS.
Otherwise some other point in time is chosen. It is also guaranteed that the
following calls to this BIF return continuously increasing values. Hence, the
return value from `erlang:now/0` can be used to generate unique time stamps. If
it is called in a tight loop on a fast machine, the time of the node can become
skewed.

Can only be used to check the local time of day if the time-zone information of
the underlying OS is properly configured.
""".
-doc #{ category => deprecated }.
-spec now() -> Timestamp when
      Timestamp :: timestamp().
now() ->
    erlang:nif_error(undefined).

%% phash/2
-doc """
> #### Warning {: .warning }
>
> This function is deprecated as [`erlang:phash2/2`](`phash2/2`) should be used
> for new code. Note that `erlang:phash(X,N)` is not necessary equal to
> `erlang:phash2(X,N)`

Portable hash function that gives the same hash for the same Erlang term
regardless of machine architecture and ERTS version (the BIF was introduced in
ERTS 4.9.1.1). The function returns a hash value for `Term` within the range
`1..Range`. The maximum value for `Range` is 2^32.
""".
-doc #{ category => deprecated }.
-spec phash(Term, Range) -> Hash when
      Term :: term(),
      Range :: pos_integer(),
      Hash :: pos_integer().
phash(_Term, _Range) ->
    erlang:nif_error(undefined).

%% phash2/1
-doc(#{equiv => phash2/2}).
-doc #{ category => terms }.
-spec phash2(Term) -> Hash when
      Term :: term(),
      Hash :: non_neg_integer().
phash2(_Term) ->
    erlang:nif_error(undefined).

%% phash2/2
-doc """
Portable hash function that gives the same hash for the same Erlang term
regardless of machine architecture and ERTS version.

The function returns a hash value for `Term` within the range
`0..Range-1`. The maximum value for `Range` is 2^32. When without argument
`Range`, a value in the range 0..2^27-1 is returned.

This BIF is always to be used for hashing terms. It distributes small integers
better than [`phash/2`](`phash/2`), and it is faster for bignums and binaries.

Notice that the range `0..Range-1` is different from the range of
[`phash/2`](`phash/2`), which is `1..Range`.
""".
-doc(#{ category => terms }).
-spec phash2(Term, Range) -> Hash when
      Term :: term(),
      Range :: pos_integer(),
      Hash :: non_neg_integer().
phash2(_Term, _Range) ->
    erlang:nif_error(undefined).

%% pid_to_list/1
-doc """
Returns a string corresponding to the text representation of `Pid`.

For example:

```erlang
> erlang:pid_to_list(self()).
"<0.85.0>"
```

> #### Note {: .info }
>
> The [creation](erl_dist_protocol.md) for the node is not included in the list
> representation of `Pid`. This means that processes in different incarnations
> of a node with a specific name can get the same list representation.
""".
-doc #{ category => terms }.
-spec pid_to_list(Pid) -> string() when
      Pid :: pid().
pid_to_list(_Pid) ->
    erlang:nif_error(undefined).

%% port_to_list/1
-doc """
Returns a string corresponding to the text representation of the port identifier
`Port`.
""".
-doc #{ category => terms }.
-spec port_to_list(Port) -> string() when
      Port :: port().
port_to_list(_Port) ->
    erlang:nif_error(undefined).

%% ports/0
-doc """
Returns a list of port identifiers corresponding to all the ports existing on
the local node.

Notice that an exiting port exists, but is not open.
""".
-doc #{ category => ports }.
-spec ports() -> [port()].
ports() ->
    erlang:nif_error(undefined).

%% posixtime_to_universaltime/1
-doc false.
-spec posixtime_to_universaltime(P1) -> {calendar:date(), calendar:time()} when
      P1 :: integer().
posixtime_to_universaltime(_P1) ->
    erlang:nif_error(undefined).

-doc """
Generates and returns an
[integer unique on current runtime system instance](`e:system:system_limits.md#unique_integers`).
The integer is unique in the sense that this BIF, using the same set of
modifiers, does not return the same integer more than once on the current
runtime system instance. Each integer value can of course be constructed by
other means.

By default, when `[]` is passed as `ModifierList`, both negative and positive
integers can be returned. This to use the range of integers that do not need
heap memory allocation as much as possible. By default the returned integers are
also only guaranteed to be unique, that is, any returned integer can be smaller
or larger than previously returned integers.

`Modifier`s:

- **positive** - Returns only positive integers.

  Notice that by passing the `positive` modifier you will get heap allocated
  integers (bignums) quicker.

- **monotonic** - Returns
  [strictly monotonically increasing](time_correction.md#strictly-monotonically-increasing)
  integers corresponding to creation time. That is, the integer returned is
  always larger than previously returned integers on the current runtime system
  instance.

  These values can be used to determine order between events on the runtime
  system instance. That is, if both `X = erlang:unique_integer([monotonic])` and
  `Y = erlang:unique_integer([monotonic])` are executed by different processes
  (or the same process) on the same runtime system instance and `X < Y`, we know
  that `X` was created before `Y`.

  > #### Warning {: .warning }
  >
  > Strictly monotonically increasing values are inherently quite expensive to
  > generate and scales poorly. This is because the values need to be
  > synchronized between CPU cores. That is, do not pass the `monotonic`
  > modifier unless you really need strictly monotonically increasing values.

All valid `Modifier`s can be combined. Repeated (valid) `Modifier`s in the
`ModifierList` are ignored.

> #### Note {: .info }
>
> The set of integers returned by `erlang:unique_integer/1` using different sets
> of `Modifier`s _will overlap_. For example, by calling
> [`unique_integer([monotonic])`](`unique_integer/1`), and
> [`unique_integer([positive, monotonic])`](`unique_integer/1`) repeatedly, you
> will eventually see some integers that are returned by both calls.

Failures:

- **`badarg`** - if `ModifierList` is not a proper list.

- **`badarg`** - if `Modifier` is not a valid modifier.
""".
-doc #{ category => terms }.
-doc(#{since => <<"OTP 18.0">>}).
-spec unique_integer(ModifierList) -> integer() when
      ModifierList :: [Modifier],
      Modifier :: positive | monotonic.

unique_integer(_ModifierList) ->
    erlang:nif_error(undefined).

-doc """
Generates and returns an
[integer unique on current runtime system instance](`e:system:system_limits.md#unique_integers`).
Equivalent to calling [`erlang:unique_integer([])`](`unique_integer/1`).
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => terms }.
-spec unique_integer() -> integer().

unique_integer() ->
    erlang:nif_error(undefined).

-doc """
Returns the current
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) in `native`
[time unit](`t:time_unit/0`). This is a monotonically increasing time
since some unspecified point in time.

> #### Note {: .info }
>
> This is a
> [monotonically increasing](time_correction.md#monotonically-increasing) time,
> but _not_ a
> [strictly monotonically increasing](time_correction.md#strictly-monotonically-increasing)
> time. That is, consecutive calls to `erlang:monotonic_time/0` can produce the
> same result.
>
> Different runtime system instances will use different unspecified points in
> time as base for their Erlang monotonic clocks. That is, it is _pointless_
> comparing monotonic times from different runtime system instances. Different
> runtime system instances can also place this unspecified point in time
> different relative runtime system start. It can be placed in the future (time
> at start is a negative value), the past (time at start is a positive value),
> or the runtime system start (time at start is zero). The monotonic time at
> runtime system start can be retrieved by calling
> [`erlang:system_info(start_time)`](#system_info_start_time).
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec monotonic_time() -> integer().

monotonic_time() ->
    erlang:nif_error(undefined).

-doc """
Returns the current
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) converted into
the `Unit` passed as argument.

Same as calling
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[ `erlang:monotonic_time()`](`monotonic_time/0`)`, native, Unit)`,
however optimized for commonly used `Unit`s.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec monotonic_time(Unit) -> integer() when
      Unit :: time_unit().

monotonic_time(_Unit) ->
    erlang:nif_error(undefined).

-doc """
Returns current [Erlang system time](time_correction.md#erlang-system-time) in
`native` [time unit](`t:time_unit/0`).

Calling `erlang:system_time()` is equivalent to
[`erlang:monotonic_time()`](`monotonic_time/0`)`+`[`erlang:time_offset()`](`time_offset/0`).

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec system_time() -> integer().

system_time() ->
    erlang:nif_error(undefined).

-doc """
Returns current [Erlang system time](time_correction.md#erlang-system-time)
converted into the `Unit` passed as argument.

Calling `erlang:system_time(Unit)` is equivalent to
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[`erlang:system_time()`](`system_time/0`)`, native, Unit)`.

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec system_time(Unit) -> integer() when
      Unit :: time_unit().

system_time(_Unit) ->
    erlang:nif_error(undefined).

-doc """
Converts the `Time` value of time unit `FromUnit` to the corresponding
`ConvertedTime` value of time unit `ToUnit`. The result is rounded using the
`floor/1` function.

> #### Warning {: .warning }
>
> You can lose accuracy and precision when converting between time units. To
> minimize such loss, collect all data at `native` time unit and do the
> conversion on the end result.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec convert_time_unit(Time, FromUnit, ToUnit) -> ConvertedTime when
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

-doc """
Returns the current time offset between
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
[Erlang system time](time_correction.md#erlang-system-time) in `native`
[time unit](`t:time_unit/0`). Current time offset added to an Erlang
monotonic time gives corresponding Erlang system time.

The time offset may or may not change during operation depending on the
[time warp mode](time_correction.md#time-warp-modes) used.

> #### Note {: .info }
>
> A change in time offset can be observed at slightly different points in time
> by different processes.
>
> If the runtime system is in
> [multi-time warp mode](time_correction.md#multi-time-warp-mode), the time
> offset is changed when the runtime system detects that the
> [OS system time](time_correction.md#os-system-time) has changed. The runtime
> system will, however, not detect this immediately when it occurs. A task
> checking the time offset is scheduled to execute at least once a minute; so,
> under normal operation this is to be detected within a minute, but during
> heavy load it can take longer time.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec time_offset() -> integer().

time_offset() ->
    erlang:nif_error(undefined).

-doc """
Returns the current time offset between
[Erlang monotonic time](time_correction.md#erlang-monotonic-time) and
[Erlang system time](time_correction.md#erlang-system-time) converted into the
`Unit` passed as argument.

Same as calling
[`erlang:convert_time_unit`](`convert_time_unit/3`)`(`[ `erlang:time_offset()`](`time_offset/0`)`, native, Unit)`
however optimized for commonly used `Unit`s.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec time_offset(Unit) -> integer() when
      Unit :: time_unit().

time_offset(_Unit) ->
    erlang:nif_error(undefined).

-doc """
Returns current [Erlang system time](time_correction.md#erlang-system-time) on
the format `{MegaSecs, Secs, MicroSecs}`.

This format is the same as `os:timestamp/0` and the deprecated [`erlang:now/0`](`now/0`) use.
The reason for the existence of `erlang:timestamp()` is purely to simplify use for existing
code that assumes this time stamp format. Current Erlang system time can more
efficiently be retrieved in the time unit of your choice using
[`erlang:system_time/1`](`system_time/1`).

The `erlang:timestamp()` BIF is equivalent to:

```c
timestamp() ->
    ErlangSystemTime = erlang:system_time(microsecond),
    MegaSecs = ErlangSystemTime div 1000_000_000_000,
    Secs = ErlangSystemTime div 1000_000 - MegaSecs*1000_000,
    MicroSecs = ErlangSystemTime rem 1000_000,
    {MegaSecs, Secs, MicroSecs}.
```

It, however, uses a native implementation that does not build garbage on the
heap and with slightly better performance.

> #### Note {: .info }
>
> This time is _not_ a monotonically increasing time in the general case. For
> more information, see the documentation of
> [time warp modes](time_correction.md#time-warp-modes) in the User's Guide.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => time }.
-spec timestamp() -> Timestamp when
      Timestamp :: timestamp().

timestamp() ->
    erlang:nif_error(undefined).

%% prepare_loading/2
-doc false.
-spec prepare_loading(Module, Code) -> PreparedCode | {error, Reason} when
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
-doc """
Returns a list of Erlang modules that are preloaded in the run-time system.

Pre-loaded modules are Erlang modules that are needed to bootstrap the system to
load the first Erlang modules from either disk or by using `m:erl_boot_server`.
""".
-spec pre_loaded() -> [module()].
-doc #{ category => code }.
pre_loaded() ->
    erlang:nif_error(undefined).

%% process_display/2
-doc """
Writes information about the local process `Pid` on [standard error](`t:io:standard_error/0`).

The only allowed value for the atom `Type` is `backtrace`, which shows the contents of
the call stack, including information about the call chain, with the current
function printed first. The format of the output is not further defined.
""".
-doc #{ category => processes }.
-spec process_display(Pid, Type) -> true when
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
-doc """
Sets certain flags for the process `Pid`, in the same manner as
`process_flag/2`. Returns the old value of the flag. The valid values for `Flag`
are only a subset of those allowed in [`process_flag/2`](`process_flag/2`),
namely `save_calls`.

Failure: `badarg` if `Pid` is not a local process.
""".
-doc #{ category => processes }.
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
-doc """
Returns a list containing `InfoTuple`s with miscellaneous information about the
process identified by `Pid`, or `undefined` if the process is not alive.

The order of the `InfoTuple`s is undefined and all `InfoTuple`s are not
mandatory. The `InfoTuple`s part of the result can be changed without prior
notice.

The `InfoTuple`s with the following items are part of the result:

- `current_function`
- `initial_call`
- `status`
- `message_queue_len`
- `links`
- `dictionary`
- `trap_exit`
- `error_handler`
- `priority`
- `group_leader`
- `total_heap_size`
- `heap_size`
- `stack_size`
- `reductions`
- `garbage_collection`

If the process identified by `Pid` has a registered name, also an `InfoTuple`
with item `registered_name` is included.

For information about specific `InfoTuple`s, see `process_info/2`.

> #### Warning {: .warning }
>
> This BIF is intended for _debugging only_. For all other purposes, use
> `process_info/2`.

Failure: `badarg` if `Pid` is not a local process.
""".
-doc #{ category => processes }.
-spec process_info(Pid) -> Info when
      Pid :: pid(),
      Info :: [InfoTuple] | undefined,
      InfoTuple :: process_info_result_item().
process_info(_Pid) ->
    erlang:nif_error(undefined).

%% processes/0
-doc """
Returns a list of process identifiers corresponding to all the processes
currently existing on the local node.

Notice that an exiting process exists, but is not alive. That is,
[`is_process_alive/1`](`is_process_alive/1`) returns `false` for an exiting
process, but its process identifier is part of the result returned from
`processes/0`.

Example:

```erlang
> processes().
[<0.0.0>,<0.2.0>,<0.4.0>,<0.5.0>,<0.7.0>,<0.8.0>]
```
""".
-doc #{ category => processes }.
-spec processes() -> [pid()].
processes() ->
    erlang:nif_error(undefined).

%% The process iterator is a 2-tuple, consisting of an index to the process
%% table and a list of process identifiers that existed when the last scan of
%% the process table took place. The index is the starting place for the next
%% scan of the process table.
-opaque processes_iter_ref() :: {integer(), [pid()]}.

%% processes_iterator/0
-doc """
Returns a processes iterator that can be used in
[`processes_next/1`](`processes_next/1`).
""".
-doc #{ category => processes, since => <<"OTP @OTP-19369@">> }.
-spec processes_iterator() -> processes_iter_ref().
processes_iterator() ->
    {0, []}.

%% processes_next/1
-doc """
Returns a 2-tuple, consisting of one process identifier and a new processes
iterator. If the process iterator has run out of processes in the process table,
`none` will be returned.

The two major benefits of using the `processes_iterator/0`/`processes_next/1`
BIFs instead of using the `processes/0` BIF are that they scale better since
no locking is needed, and you do not risk getting a huge list allocated on the
heap if there are a huge amount of processes alive in the system.

Example:

```erlang
> I0 = erlang:processes_iterator(), ok.
ok
> {Pid1, I1} = erlang:processes_next(I0), Pid1.
<0.0.0>,
> {Pid2, I2} = erlang:processes_next(I1), Pid2.
<0.1.0>
```

> #### Note {: .info }
>
> This BIF has less consistency guarantee than [`processes/0`](`processes/0`).
> Process identifiers returned from consecutive calls of this BIF may not be a
> consistent snapshot of all elements existing in the table during any of the
> calls. The process identifier of a process that is alive before
> `processes_iterator/0` is called and continues to be alive until
> `processes_next/1` returns `none` is guaranteed to be part of the result
> returned from one of the calls to `processes_next/1`.
""".
-doc #{ category => processes, since => <<"OTP @OTP-19369@">> }.
-spec processes_next(Iter) -> {Pid, NewIter} | 'none' when
      Iter :: processes_iter_ref(),
      NewIter :: processes_iter_ref(),
      Pid :: pid().
processes_next({IterRef, [Pid|Pids]}) ->
    {Pid, {IterRef, Pids}};
processes_next({IterRef0, []}=Arg) ->
    try erts_internal:processes_next(IterRef0) of
        none -> none;
        {IterRef, [Pid|Pids]} -> {Pid, {IterRef, Pids}};
        {IterRef, []} -> processes_next({IterRef, []})
    catch error:badarg ->
            badarg_with_info([Arg])
    end;
processes_next(Arg) ->
    badarg_with_info([Arg]).

%% purge_module/1
-doc """
Removes old code for `Module`. Before this BIF is used, `check_process_code/2`
is to be called to check that no processes execute old code in the module.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.

> #### Change {: .info }
>
> As from ERTS 8.0 (Erlang/OTP 19), any lingering processes that still execute
> the old code is killed by this function. In earlier versions, such incorrect
> use could cause much more fatal failures, like emulator crash.

Failure: `badarg` if there is no old code for `Module`.
""".
-doc #{ category => code }.
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
-doc """
Adds a new `Key` to the process dictionary, associated with the value `Val`, and
returns `undefined`. If `Key` exists, the old value is deleted and replaced by
`Val`, and the function returns the old value.

The average time complexity for the current implementation of this function is
O(`1`) and the worst case time complexity is O(`N`), where `N` is the number of
items in the process dictionary.

For example:

```erlang
> X = put(name, walrus), Y = put(name, carpenter),
Z = get(name),
{X, Y, Z}.
{undefined,walrus,carpenter}
```

> #### Note {: .info }
>
> The values stored when `put` is evaluated within the scope of a `catch` are
> not retracted if a `throw` is evaluated, or if an error occurs.
""".
-doc #{ category => processes }.
-spec put(Key, Val) -> term() when
      Key :: term(),
      Val :: term().
put(_Key, _Val) ->
    erlang:nif_error(undefined).

%% raise/3
%% Shadowed by erl_bif_types: erlang:raise/3
-doc """
Raises an exception of the specified class, reason, and call stack backtrace
(_stacktrace_).

`Class` is `error`, `exit`, or `throw`. So, if it were not for the stacktrace,
`erlang:raise(Class, Reason, Stacktrace)` is equivalent to
`erlang:Class(Reason)` (given that `Class` is a valid class).

`Reason` can be any term.

`Stacktrace` is a list as provided in a try-catch clause.

```erlang
try
    ...
catch Class:Reason:Stacktrace ->
    ...
end
```

That is, a list of four-tuples `{Module, Function, Arity | Args, ExtraInfo}`,
where `Module` and `Function` are atoms, and the third element is an integer
arity or an argument list. The stacktrace can also contain
`{Fun, Args, ExtraInfo}` tuples, where `Fun` is a local fun and `Args` is an
argument list.

Element `ExtraInfo` at the end is optional. Omitting it is equivalent to
specifying an empty list.

The stacktrace is used as the exception stacktrace for the calling process; it
is truncated to the current maximum stacktrace depth.

As evaluating this function causes the process to terminate, it has no return
value unless the arguments are invalid, in which case the function _returns the
error reason_ `badarg`. If you want to be sure not to return, you can call
[`error(erlang:raise(Class, Reason, Stacktrace))`](`error/1`) and hope to
distinguish exceptions later.

See the reference manual about [errors and error handling](`e:system:errors.md`)
for more information about exception classes and how to catch exceptions.
""".
-doc #{ category => processes }.
-spec raise(Class, Reason, Stacktrace) -> 'badarg' when
      Class :: 'error' | 'exit' | 'throw',
      Reason :: term(),
      Stacktrace :: raise_stacktrace().
raise(_Class, _Reason, _Stacktrace) ->
    erlang:nif_error(undefined).

%% read_timer/1
-doc( #{ equiv =>  erlang:read_timer(TimerRef, []) }).
-doc #{ category => timer }.
-spec read_timer(TimerRef) -> Result when
      TimerRef :: reference(),
      Time :: non_neg_integer(),
      Result :: Time | false.

read_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% read_timer/2
-doc """
Reads the state of a timer that has been created by either
[`erlang:start_timer`](`start_timer/4`) or
[`erlang:send_after`](`send_after/4`). `TimerRef` identifies the timer, and was
returned by the BIF that created the timer.

`Options`:

- **`{async, Async}`** - Asynchronous request for state information. `Async`
  defaults to `false`, which causes the operation to be performed synchronously.
  In this case, the `Result` is returned by `erlang:read_timer`. When `Async` is
  `true`, `erlang:read_timer` sends an asynchronous request for the state
  information to the timer service that manages the timer, and then returns
  `ok`. A message on the format `{read_timer, TimerRef, Result}` is sent to the
  caller of `erlang:read_timer` when the operation has been processed.

More `Option`s can be added in the future.

If `Result` is an integer, it represents the time in milliseconds left until the
timer expires.

If `Result` is `false`, a timer corresponding to `TimerRef` could not be found.
This because the timer had expired, or been canceled, or because `TimerRef`
never has corresponded to a timer. Even if the timer has expired, it does not
tell you whether or not the time-out message has arrived at its destination yet.

> #### Note {: .info }
>
> The timer service that manages the timer can be co-located with another
> scheduler than the scheduler that the calling process is executing on. If so,
> communication with the timer service takes much longer time than if it is
> located locally. If the calling process is in a critical path, and can do
> other things while waiting for the result of this operation, you want to use
> option `{async, true}`. If using option `{async, false}`, the calling process
> is blocked until the operation has been performed.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:start_timer/4`](`start_timer/4`), and
[`erlang:cancel_timer/2`](`cancel_timer/2`).
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => timer }.
-spec read_timer(TimerRef, Options) -> Result | ok when
      TimerRef :: reference(),
      Async :: boolean(),
      Option :: {async, Async},
      Options :: [Option],
      Time :: non_neg_integer(),
      Result :: Time | false.

read_timer(_TimerRef, _Options) ->
    erlang:nif_error(undefined).

%% ref_to_list/1
-doc """
Returns a string corresponding to the text representation of `Ref`.

> #### Warning {: .warning }
>
> This BIF is intended for debugging and is not to be used in application
> programs.
""".
-doc #{ category => terms }.
-spec ref_to_list(Ref) -> string() when
      Ref :: reference().
ref_to_list(_Ref) ->
    erlang:nif_error(undefined).

%% register/2
-doc """
Registers the name `RegName` with a process identifier (pid) or a port
identifier in the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`).
`RegName`, which must be an atom, can be used instead of the pid or port
identifier in send operator (`RegName ! Message`) and most other BIFs that take
a pid or port identifies as an argument.

For example:

```erlang
> register(db, Pid).
true
```

The registered name is considered a
[Directly Visible Erlang Resource](`e:system:ref_man_processes.md#visible-resources`)
and is automatically unregistered when the process terminates.

Failures:

- **`badarg`** - If `PidOrPort` is not an existing local process or port.

- **`badarg`** - If `RegName` is already in use.

- **`badarg`** - If the process or port is already registered (already has a
  name).

- **`badarg`** - If `RegName` is the atom `undefined`.
""".
-doc #{ category => processes }.
-spec register(RegName, PidOrPort) -> true when
      RegName :: atom(),
      PidOrPort :: port() | pid().
register(_RegName, _PidOrPort) ->
    erlang:nif_error(undefined).

%% registered/0
-doc """
Returns a list of names that have been registered using `register/2`.

For example:

```erlang
> registered().
[code_server, file_server, init, user, my_db]
```
""".
-doc #{ category => processes }.
-spec registered() -> [RegName] when
      RegName :: atom().
registered() ->
    erlang:nif_error(undefined).

%% resume_process/1
-doc """
Decreases the suspend count on the process identified by `Suspendee`.

`Suspendee` is previously to have been suspended through
[`erlang:suspend_process/2`](`suspend_process/2`) or
[`erlang:suspend_process/1`](`suspend_process/1`) by the process calling
`erlang:resume_process(Suspendee)`. When the suspend count on `Suspendee`
reaches zero, `Suspendee` is resumed, that is, its state is changed from
suspended into the state it had before it was suspended.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.

Failures:

- **`badarg`** - If `Suspendee` is not a process identifier.

- **`badarg`** - If the process calling `erlang:resume_process/1` had not
  previously increased the suspend count on the process identified by
  `Suspendee`.

- **`badarg`** - If the process identified by `Suspendee` is not alive.
""".
-doc #{ category => processes }.
-spec resume_process(Suspendee) -> true when
      Suspendee :: pid().
resume_process(_Suspendee) ->
    erlang:nif_error(undefined).

%% round/1
%% Shadowed by erl_bif_types: erlang:round/1
-doc """
Returns an integer by rounding `Number`.

For example:

```erlang
round(42.1).
42
```

```erlang
round(5.5).
6
```

```erlang
round(-5.5).
-6
```

```erlang
round(36028797018963969.0).
36028797018963968
```

In the last example, [`round(36028797018963969.0)`](`round/1`) evaluates to
`36028797018963968`. The reason for this is that the number
`36028797018963969.0` cannot be represented exactly as a float value. Instead,
the float literal is represented as `36028797018963968.0`, which is the closest
number that can be represented exactly as a float value. See
[Representation of Floating Point Numbers](`e:system:data_types.md#float_representation_problem`)
for additional information.
""".
-doc #{ category => terms }.
-spec round(Number) -> integer() when
      Number :: number().
round(_Number) ->
    erlang:nif_error(undefined).

%% self/0
%% Shadowed by erl_bif_types: erlang:self/0
-doc """
Returns the process identifier of the calling process.

For example:

```erlang
> self().
<0.26.0>
```
""".
-doc #{ category => processes }.
-spec self() -> pid().
self() ->
    erlang:nif_error(undefined).

%% send_after/3
-doc( #{ equiv => erlang:send_after(Time, Dest, Msg, [])} ).
-doc #{ category => timer }.
-spec send_after(Time, Dest, Msg) -> TimerRef when
      Time :: non_neg_integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      TimerRef :: reference().

send_after(_Time, _Dest, _Msg) ->
    erlang:nif_error(undefined).

%% send_after/4
-doc """
Starts a timer. When the timer expires, the message `Msg` is sent to the process
identified by `Dest`. Apart from the format of the time-out message, this
function works exactly as [`erlang:start_timer/4`](`start_timer/4`).
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => timer }.
-spec send_after(Time, Dest, Msg, Options) -> TimerRef when
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
-doc false.
-spec seq_trace(P1, P2) -> seq_trace_info_returns() | {term(), term(), term(), term(), term()} when
      P1 :: atom(),
      P2 :: term().
seq_trace(_P1, _P2) ->
    erlang:nif_error(undefined).

%% seq_trace_print/1
-doc false.
-spec seq_trace_print(P1) -> boolean() when
      P1 :: term().
seq_trace_print(_P1) ->
    erlang:nif_error(undefined).

%% seq_trace_print/2
-doc false.
-spec seq_trace_print(P1, P2) -> boolean() when
      P1 :: atom() | integer(),
      P2 :: term().
seq_trace_print(_P1, _P2) ->
    erlang:nif_error(undefined).

%% setnode/2
-doc false.
-spec setnode(P1, P2) -> true when
      P1 :: atom(),
      P2 :: integer().
setnode(_P1, _P2) ->
    erlang:nif_error(undefined).

%% setnode/3
-doc false.
-spec setnode(Node, DistCtrlr, Opts) -> dist_handle() when
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
-doc """
Returns the number of elements in a tuple or the number of bytes in a binary or
bitstring.

For example:

```erlang
> size({morni, mulle, bwange}).
3
> size(<<11, 22, 33>>).
3
```

For bitstrings, the number of whole bytes is returned. That is, if the number of
bits in the bitstring is not divisible by 8, the resulting number of bytes is
rounded _down_.

See also `tuple_size/1`, `byte_size/1`, and `bit_size/1`.
""".
-doc #{ category => terms }.
-spec size(Item) -> non_neg_integer() when
      Item :: tuple() | binary().
size(_Item) ->
    erlang:nif_error(undefined).

%% spawn/3
-doc """
Returns the process identifier of a new process started by the application of
`Module:Function` to `Args`.

[`error_handler:undefined_function(Module, Function, Args)`](`error_handler`) is
 evaluated by the new process if `Module:Function/Arity` does not exist
(where `Arity` is the length of `Args`). The error handler can be redefined
(see `process_flag/2`). If
`error_handler` is undefined, or the user has redefined the default
`error_handler` and its replacement is undefined, a failure with reason `undef`
occurs.

Example:

```erlang
> spawn(speed, regulator, [high_speed, thin_cut]).
<0.13.1>
```
""".
-doc #{ category => processes }.
-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% spawn_link/3
-doc """
Returns the process identifier of a new process started by the application of
`Module:Function` to `Args`. A link is created between the calling process and
the new process, atomically. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
-spec spawn_link(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% split_binary/2
-doc """
Returns a tuple containing the binaries that are the result of splitting `Bin`
into two parts at position `Pos`.

This is not a destructive operation. After the operation, there are three binaries altogether.

For example:

```erlang
> B = list_to_binary("0123456789").
<<"0123456789">>
> byte_size(B).
10
> {B1, B2} = split_binary(B,3).
{<<"012">>,<<"3456789">>}
> byte_size(B1).
3
> byte_size(B2).
7
```
""".
-doc #{ category => terms }.
-spec split_binary(Bin, Pos) -> {binary(), binary()} when
      Bin :: binary(),
      Pos :: non_neg_integer().
split_binary(_Bin, _Pos) ->
    erlang:nif_error(undefined).

%% start_timer/3
-doc( #{ equiv =>  erlang:start_timer(Time, Dest, Msg, []) }).
-doc #{ category => timer }.
-spec start_timer(Time, Dest, Msg) -> TimerRef when
      Time :: non_neg_integer(),
      Dest :: pid() | atom(),
      Msg :: term(),
      TimerRef :: reference().

start_timer(_Time, _Dest, _Msg) ->
    erlang:nif_error(undefined).

%% start_timer/4
-doc """
Starts a timer. When the timer expires, the message `{timeout, TimerRef, Msg}`
is sent to the process identified by `Dest`.

`Option`s:

- **`{abs, false}`** - This is the default. It means the `Time` value is
  interpreted as a time in milliseconds _relative_ current
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time).

- **`{abs, true}`** - Absolute `Time` value. The `Time` value is interpreted as
  an absolute Erlang monotonic time in milliseconds.

More `Option`s can be added in the future.

The absolute point in time, the timer is set to expire on, must be in the
interval
`[ `[erlang:convert_time_unit](`convert_time_unit/3`)`(`[erlang:system_info](#system_info_start_time)`(start_time), native, millisecond), `[erlang:convert_time_unit](`convert_time_unit/3`)`(`[erlang:system_info](#system_info_end_time)`(end_time), native, millisecond) ]`.
If a relative time is specified, the `Time` value is not allowed to be negative.

If `Dest` is a `t:pid/0`, it must be a `t:pid/0` of a process created on the
current runtime system instance. This process has either terminated or not. If
`Dest` is an `t:atom/0`, it is interpreted as the name of a locally registered
process. The process referred to by the name is looked up at the time of timer
expiration. No error is returned if the name does not refer to a process.

If `Dest` is a `t:pid/0`, the timer is automatically canceled if the process
referred to by the `t:pid/0` is not alive, or if the process exits. This feature
was introduced in ERTS 5.4.11. Notice that timers are not automatically canceled
when `Dest` is an `t:atom/0`.

See also [`erlang:send_after/4`](`send_after/4`),
[`erlang:cancel_timer/2`](`cancel_timer/2`), and
[`erlang:read_timer/2`](`read_timer/2`).

For more information on timers in Erlang in general, see the
[*Timers*](`e:erts:time_correction.md#timers`) section of the
[*Time and Time Correction in Erlang*](`e:erts:time_correction.md`)
ERTS User's guide.

Failure: `badarg` if the arguments do not satisfy the requirements specified
here.
""".
-doc(#{since => <<"OTP 18.0">>}).
-doc #{ category => timer }.
-spec start_timer(Time, Dest, Msg, Options) -> TimerRef when
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
-doc """
Increases the suspend count on the process identified by `Suspendee` and puts it
in the suspended state if it is not already in that state. A suspended process
is not scheduled for execution until the process has been resumed. If the
suspended process currently is waiting in a `receive ... after` expression, the
timer for the timeout will, as of OTP 28.0, also be suspended until the process
is resumed.

A process can be suspended by multiple processes and can be suspended multiple
times by a single process. A suspended process does not leave the suspended
state until its suspend count reaches zero. The suspend count of `Suspendee` is
decreased when [`erlang:resume_process(Suspendee)`](`resume_process/1`) is
called by the same process that called `erlang:suspend_process(Suspendee)`. All
increased suspend counts on other processes acquired by a process are
automatically decreased when the process terminates.

Options (`Opt`s):

- **`asynchronous`** - A suspend request is sent to the process identified by
  `Suspendee`. `Suspendee` eventually suspends unless it is resumed before it
  could suspend. The caller of `erlang:suspend_process/2` returns immediately,
  regardless of whether `Suspendee` has suspended yet or not. The point in time
  when `Suspendee` suspends cannot be deduced from other events in the system.
  It is only guaranteed that `Suspendee` _eventually_ suspends (unless it is
  resumed). If no `asynchronous` options has been passed, the caller of
  `erlang:suspend_process/2` is blocked until `Suspendee` has suspended.

- **`{asynchronous, ReplyTag}`** - A suspend request is sent to the process
  identified by `Suspendee`. When the suspend request has been processed, a
  reply message is sent to the caller of this function. The reply is on the form
  `{ReplyTag, State}` where `State` is either:

  - **`exited`** - `Suspendee` has exited.

  - **`suspended`** - `Suspendee` is now suspended.

  - **`not_suspended`** - `Suspendee` is not suspended. This can only happen
    when the process that issued this request, have called
    [`resume_process(Suspendee)`](`resume_process/1`) before getting the reply.

  Apart from the reply message, the `{asynchronous, ReplyTag}` option behaves
  exactly the same as the `asynchronous` option without reply tag.

- **`unless_suspending`** - The process identified by `Suspendee` is suspended
  unless the calling process already is suspending `Suspendee`. If
  `unless_suspending` is combined with option `asynchronous`, a suspend request
  is sent unless the calling process already is suspending `Suspendee` or if a
  suspend request already has been sent and is in transit. If the calling
  process already is suspending `Suspendee`, or if combined with option
  `asynchronous` and a send request already is in transit, `false` is returned
  and the suspend count on `Suspendee` remains unchanged.

If the suspend count on the process identified by `Suspendee` is increased,
`true` is returned, otherwise `false`.

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.

> #### Warning {: .warning }
>
> You can easily create deadlocks if processes suspends each other (directly or
> in circles). In ERTS versions prior to ERTS version 10.0, the runtime system
> prevented such deadlocks, but this prevention has now been removed due to
> performance reasons.

Failures:

- **`badarg`** - If `Suspendee` is not a process identifier.

- **`badarg`** - If the process identified by `Suspendee` is the same process as
  the process calling `erlang:suspend_process/2`.

- **`badarg`** - If the process identified by `Suspendee` is not alive.

- **`badarg`** - If the process identified by `Suspendee` resides on another
  node.

- **`badarg`** - If `OptList` is not a proper list of valid `Opt`s.

- **`system_limit`** - If the process identified by `Suspendee` has been
  suspended more times by the calling process than can be represented by the
  currently used internal data structures. The system limit is greater than
  2,000,000,000 suspends and will never be lower.
""".
-doc #{ category => processes }.
-spec suspend_process(Suspendee, OptList) -> boolean() when
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

-doc """
Suspends the process identified by `Suspendee`. Equivalent to calling
[`erlang:suspend_process(Suspendee, [])`](`suspend_process/2`).

> #### Warning {: .warning }
>
> This BIF is intended for debugging only.
""".
-doc #{ category => processes }.
-spec suspend_process(Suspendee) -> 'true' when
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
-doc """
Returns the current system monitoring settings set by
[`erlang:system_monitor/2`](`system_monitor/2`) as `{MonitorPid, Options}`, or
`undefined` if no settings exist.

The order of the options can be different from the one that was set.
""".
-doc #{ category => system }.
-spec system_monitor() -> MonSettings when
      MonSettings :: undefined | { MonitorPid, Options },
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ].
system_monitor() ->
    erts_internal:system_monitor(legacy).

%% system_monitor/1
-doc """
When called with argument `undefined`, all system performance monitoring
settings are cleared.

Calling the function with `{MonitorPid, Options}` as argument is the same as
calling [`erlang:system_monitor(MonitorPid, Options)`](`system_monitor/2`).

Returns the previous system monitor settings just like
[`erlang:system_monitor/0`](`system_monitor/0`).
""".
-doc #{ category => system }.
-spec system_monitor(Arg) -> MonSettings when
      Arg :: undefined | { MonitorPid, Options },
      MonSettings :: undefined | { MonitorPid, Options },
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ].
system_monitor(undefined) ->
    erts_internal:system_monitor(legacy, undefined, []);
system_monitor({MonitorPid, Options}=Arg) ->
    try
        erts_internal:system_monitor(legacy, MonitorPid, Options)
    catch
        error:Reason ->
            error_with_info(Reason, [Arg])
    end;
system_monitor(Arg) ->
    badarg_with_info([Arg]).


%% system_monitor/2
-doc """
Sets the system event monitoring options. `MonitorPid` is a local process
identifier (pid) receiving system monitor messages.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:system/3` that operate on
  > dynamic trace sessions.

The second argument is a list of monitoring options to enable:

- **`{long_gc, Time}`**
- **`{long_message_queue, {Disable, Enable}}`**
- **`{long_schedule, Time}`**
- **`{large_heap, Size}`**
- **`busy_port`**
- **`busy_dist_port`**

For more detailed descriptions about the monitoring options, see
`trace:system/3`.

Unlink `trace:system/3`, the arguments to
[`system_monitor/2`](`system_monitor/2`) specifies how all system monitoring
should be set, not how it should be changed. This means only one process
at a time (`MonitorPid`) can be the receiver of messages from system monitoring set
with this function. Also, the way to clear a specific monitor option is to not
include it in the list `Options`. All system monitoring will, however, be
cleared if the process identified by `MonitorPid` terminates.

There are no special option values (like zero) to clear an option. Some of the
options have a unspecified minimum value. Lower values will be adjusted to the
minimum value. For example, it is currently not possible to monitor all garbage
collections with `{long_gc, 0}`.

Returns the previous system monitor settings just like
[`erlang:system_monitor/0`](`system_monitor/0`).

> #### Note {: .info }
>
> If a monitoring process gets so large that it itself starts to cause system
> monitor messages when garbage collecting, the messages enlarge the process
> message queue and probably make the problem worse.
>
> Keep the monitoring process neat and do not set the system monitor limits too
> tight.

Failures:

- **`badarg`** - If `MonitorPid` does not exist.

- **`badarg`** - If `MonitorPid` is not a local process.
""".
-doc #{ category => system }.
-spec system_monitor(MonitorPid, Options) -> MonSettings when
      MonitorPid :: pid(),
      Options :: [ system_monitor_option() ],
      MonSettings :: undefined | { OldMonitorPid, OldOptions },
      OldMonitorPid :: pid(),
      OldOptions :: [ system_monitor_option() ].
system_monitor(MonitorPid, Options) ->
    try
        erts_internal:system_monitor(legacy, MonitorPid, Options)
    catch
        error:Reason ->
            error_with_info(Reason, [MonitorPid, Options])
    end.

%% system_profile/0
-doc """
Returns the current system profiling settings set by
[`erlang:system_profile/2`](`system_profile/2`) as `{ProfilerPid, Options}`, or
`undefined` if there are no settings. The order of the options can be different
from the one that was set.
""".
-doc #{ category => system }.
-spec system_profile() -> ProfilerSettings when
      ProfilerSettings :: undefined | { ProfilerPid, Options},
      ProfilerPid :: pid() | port(),
      Options :: [ system_profile_option() ].
system_profile() ->
    erlang:nif_error(undefined).

%% system_profile/2
-doc """
Sets system profiler options. `ProfilerPid` is a local process identifier (pid)
or port receiving profiling messages. The receiver is excluded from all
profiling. The second argument is a list of profiling options:

- **`exclusive`** - If a synchronous call to a port from a process is done, the
  calling process is considered not runnable during the call runtime to the
  port. The calling process is notified as `inactive`, and later `active` when
  the port callback returns.

- **`monotonic_timestamp`** - Time stamps in profile messages use
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time). The time
  stamp (Ts) has the same format and value as produced by
  `erlang:monotonic_time(nanosecond)`.

- **`runnable_procs`** - If a process is put into or removed from the run queue,
  a message, `{profile, Pid, State, Mfa, Ts}`, is sent to `ProfilerPid`. Running
  processes that are reinserted into the run queue after having been pre-empted
  do not trigger this message.

- **`runnable_ports`** - If a port is put into or removed from the run queue, a
  message, `{profile, Port, State, 0, Ts}`, is sent to `ProfilerPid`.

- **`scheduler`** - If a scheduler is put to sleep or awoken, a message,
  `{profile, scheduler, Id, State, NoScheds, Ts}`, is sent to `ProfilerPid`.

- **`strict_monotonic_timestamp`** - Time stamps in profile messages consist of
  [Erlang monotonic time](time_correction.md#erlang-monotonic-time) and a
  monotonically increasing integer. The time stamp (Ts) has the same format and
  value as produced by
  `{erlang:monotonic_time(nanosecond), erlang:unique_integer([monotonic])}`.

- **`timestamp`** - Time stamps in profile messages include a time stamp (Ts)
  that has the same form as returned by `erlang:now()`. This is also the default
  if no time stamp flag is specified. If `cpu_timestamp` has been enabled
  through `trace:process/4`, this also effects the time stamp
  produced in profiling messages when flag `timestamp` is enabled.

> #### Note {: .info }
>
> `erlang:system_profile` behavior can change in a future release.
""".
-doc #{ category => system }.
-spec system_profile(ProfilerPid, Options) -> ProfilerSettings when
      ProfilerPid :: pid() | port() | undefined,
      Options :: [ system_profile_option() ],
      ProfilerSettings :: undefined | { pid() | port(), [ system_profile_option() ]}.
system_profile(_ProfilerPid, _Options) ->
    erlang:nif_error(undefined).

%% throw/1
%% Shadowed by erl_bif_types: erlang:throw/1
-doc """
Raises an exception of class `throw`. Intended to be used to do non-local
returns from functions.

If evaluated within a [catch expression](`e:system:expressions.md#catch-and-throw`), the
catch expression returns value `Any`.

For example:

```erlang
> catch throw({hello, there}).
        {hello,there}
```

If evaluated within a `try`\-block of a
[try expression](`e:system:expressions.md#try`), the value `Any` can be caught
within the catch block.

For example:

```erlang
try
    throw({my_exception, "Something happened"})
catch
    throw:{my_exception, Desc} ->
        io:format(standard_error, "Error: ~s~n", [Desc])
end
```

Failure: `nocatch` if not caught by an exception handler.

See the guide about [errors and error handling](`e:system:errors.md`) for
additional information.
""".
-doc #{ category => processes }.
-spec throw(Any) -> no_return() when
      Any :: term().
throw(_Any) ->
    erlang:nif_error(undefined).

%% time/0
-doc """
Returns the current time as `{Hour, Minute, Second}`.

The time zone and Daylight Saving Time correction depend on the underlying OS.
The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> time().
{9,42,44}
```
""".
-doc #{ category => time }.
-spec time() -> Time when
      Time :: calendar:time().
time() ->
    erlang:nif_error(undefined).

%% trace/3
-doc """
Turn on or off trace flags on processes or ports for the static legacy trace session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:process/4` and `trace:port/4` that
  > operate on dynamic trace sessions.

Argument `FlagList` can contain two additional options:

- **`{tracer, Tracer}`** - Specifies where to send the trace messages. `Tracer`
  must be the process identifier of a local process or the port identifier of a
  local port.

- **`{tracer, TracerModule, TracerState}`** - Specifies that a tracer module is
  to be called instead of sending a trace message. The tracer module can then
  ignore or change the trace message. For more details on how to write a tracer
  module, see `m:erl_tracer`.

If no `tracer` is specified, the calling process receives all the trace
messages. The legacy trace session has no specified tracer.

For further documentation see `trace:process/4` and `trace:port/4`.
""".
-doc #{ category => trace }.
-spec trace(PidPortSpec, How, FlagList) -> integer() when
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
-doc """
Calling this function makes sure all trace messages have been delivered.

The delivery of trace messages (generated by [`erlang:trace/3`](`trace/3`),
`m:seq_trace`, or [`erlang:system_profile/2`](`system_profile/2`)) is dislocated
on the time-line compared to other events in the system. If you know that
`Tracee` has passed some specific point in its execution, and you want to know
when at least all trace messages corresponding to events up to this point have
reached the tracer, use `erlang:trace_delivered(Tracee)`.

When it is guaranteed that all trace messages are delivered to the tracer up to
the point that `Tracee` reached at the time of the call to
`erlang:trace_delivered(Tracee)`, then a `{trace_delivered, Tracee, Ref}`
message is sent to the caller of `erlang:trace_delivered(Tracee)` .

Notice that message `trace_delivered` does _not_ imply that trace messages have
been delivered. Instead it implies that all trace messages that _are to be
delivered_ have been delivered. It is not an error if `Tracee` is not, and has
not been traced by someone, but if this is the case, _no_ trace messages have
been delivered when the `trace_delivered` message arrives.

Notice that `Tracee` must refer to a process currently or previously existing on
the same node as the caller of `erlang:trace_delivered(Tracee)` resides on. The
special `Tracee` atom `all` denotes all processes that currently are traced in
the node.

When used together with a [Tracer Module](`m:erl_tracer`), any message sent in
the trace callback is guaranteed to have reached its recipient before the
`trace_delivered` message is sent.

Example: Process `A` is `Tracee`, port `B` is tracer, and process `C` is the
port owner of `B`. `C` wants to close `B` when `A` exits. To ensure that the
trace is not truncated, `C` can call `erlang:trace_delivered(A)` when `A` exits,
and wait for message `{trace_delivered, A, Ref}` before closing `B`.

Failure: `badarg` if `Tracee` does not refer to a process (dead or alive) on the
same node as the caller of `erlang:trace_delivered(Tracee)` resides on.
""".
-doc #{ category => trace }.
-spec trace_delivered(Tracee) -> Ref when
      Tracee :: pid() | all,
      Ref :: reference().
trace_delivered(_Tracee) ->
    erlang:nif_error(undefined).

%% trace_info/2
-doc """
Returns trace information about a port, process, function, or event for the
static legacy trace session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:info/3` that operates on dynamic trace
  > sessions.
""".
-doc #{ category => trace }.
-spec trace_info(PidPortFuncEvent, Item) -> Res when
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


%% Shadowed by erl_bif_types: erlang:trunc/1
-doc """
Truncates the decimals of `Number`.

For example:

```erlang
> trunc(5.7).
5
```

```erlang
> trunc(-5.7).
-5
```

```erlang
> trunc(5).
5
```

```erlang
> trunc(36028797018963969.0).
36028797018963968
```

In the last example, [`trunc(36028797018963969.0)`](`trunc/1`) evaluates to
`36028797018963968`. The reason for this is that the number
`36028797018963969.0` cannot be represented exactly as a float value. Instead,
the float literal is represented as `36028797018963968.0`, which is the closest
number that can be represented exactly as a float value. See
[Representation of Floating Point Numbers](`e:system:data_types.md#float_representation_problem`)
for additional information.
""".
-doc #{ category => terms }.
-spec trunc(Number) -> integer() when
      Number :: number().
trunc(_Number) ->
    erlang:nif_error(undefined).

%% tuple_size/1
%% Shadowed by erl_bif_types: erlang:tuple_size/1
-doc """
Returns an integer that is the number of elements in `Tuple`.

For example:

```erlang
> tuple_size({morni, mulle, bwange}).
3
```
""".
-doc #{ category => terms }.
-spec tuple_size(Tuple) -> non_neg_integer() when
      Tuple :: tuple().
tuple_size(_Tuple) ->
    erlang:nif_error(undefined).

%% universaltime/0
-doc """
Returns the current date and time according to Universal Time Coordinated (UTC)
in the form `{{Year, Month, Day}, {Hour, Minute, Second}}` if supported by the
underlying OS. Otherwise `erlang:universaltime()` is equivalent to
`erlang:localtime()`. The return value is based on the
[OS System Time](time_correction.md#os-system-time).

For example:

```erlang
> erlang:universaltime().
{{1996,11,6},{14,18,43}}
```
""".
-doc #{ category => time }.
-spec universaltime() -> DateTime when
      DateTime :: calendar:datetime().
universaltime() ->
    erlang:nif_error(undefined).

%% universaltime_to_posixtime/1
-doc false.
-spec universaltime_to_posixtime(P1) -> integer() when
      P1 :: {calendar:date(), calendar:time()}.
universaltime_to_posixtime(_P1) ->
    erlang:nif_error(undefined).

%% unlink/1
-doc """
Removes a link between the calling process and another process or a port
identified by `Id`.

We will from here on call the identified process or port unlinkee.

A link can be set up using the `link/1` BIF. For more information on links and
exit signals due to links, see the _Processes_ chapter in the _Erlang Reference
Manual_:

- [Links](`e:system:ref_man_processes.md#links`)
- [Sending Exit Signals](`e:system:ref_man_processes.md#sending_exit_signals`)
- [Receiving Exit Signals](`e:system:ref_man_processes.md#receiving_exit_signals`)

Once [`unlink(Id)`](`unlink/1`) has returned, it is guaranteed that the link
between the caller and the unlinkee has no effect on the caller in the future
(unless the link is setup again). Note that if the caller is
[trapping exits](#process_flag_trap_exit), an
`{'EXIT', Id, ExitReason}` message due to the link may have been placed in the
message queue of the caller before the [`unlink(Id)`](`unlink/1`) call
completed. Also note that the `{'EXIT', Id, ExitReason}` message may be the
result of the link, but may also be the result of the unlikee sending the caller
an exit signal by calling the `exit/2` BIF. Therefore, it may or may not be
appropriate to clean up the message queue after a call to
[`unlink(Id)`](`unlink/1`) as follows, when trapping exits:

```erlang
unlink(Id),
receive
    {'EXIT', Id, _} ->
        true
after 0 ->
        true
end
```

The link removal is performed asynchronously. If such a link does not exist,
nothing is done. A detailed description of the
[link protocol](erl_dist_protocol.md#link_protocol) can be found in the
_Distribution Protocol_ chapter of the _ERTS User's Guide_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

Failure: `badarg` if `Id` does not identify a process or a node local port.
""".
-doc #{ category => processes }.
-spec unlink(Id) -> true when
      Id :: pid() | port().
unlink(_Id) ->
    erlang:nif_error(undefined).

%% unregister/1
-doc """
Removes the [`registered name`](`register/2`) `RegName` associated with a
process identifier or a port identifier from the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`).

For example:

```erlang
> unregister(db).
true
```

Keep in mind that you can still receive signals associated with the registered
name after it has been unregistered as the sender may have looked up the name
before sending to it.

Users are advised not to unregister system processes.

Failure: `badarg` if `RegName` is not a registered name.
""".
-doc #{ category => processes }.
-spec unregister(RegName) -> true when
      RegName :: atom().
unregister(_RegName) ->
    erlang:nif_error(undefined).

%% whereis/1
-doc """
Returns the process identifier or port identifier with the
[`registered name`](`register/2`) `RegName` from the
[`name registry`](`e:system:ref_man_processes.md#runtime-service`). Returns
`undefined` if the name is not registered.

For example:

```erlang
> whereis(db).
<0.43.0>
```
""".
-doc #{ category => processes }.
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
-doc """
Returns an integer or float that is the arithmetical absolute value of `Float`
or `Int`.

For example:

```erlang
> abs(-3.33).
3.33
> abs(-3).
3
```
""".
-doc #{ category => terms }.
-spec abs(Float) -> float() when
      Float :: float();
         (Int) -> non_neg_integer() when
      Int :: integer().
abs(_Number) ->
    erlang:nif_error(undefined).

%% Not documented
%% Shadowed by erl_bif_types: erlang:append/2
-doc false.
-spec append(List,Tail) -> maybe_improper_list() when
      List :: [term()],
      Tail :: term().
append(_List,_Tail) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:element/2
-doc """
Returns the `N`th element (numbering from 1) of `Tuple`.

For example:

```erlang
> element(2, {a, b, c}).
b
```
""".
-doc #{ category => terms }.
-spec element(N, Tuple) -> term() when
    N :: pos_integer(),
    Tuple :: tuple().
element(_N, _Tuple) ->
    erlang:nif_error(undefined).

%% Not documented
-type module_info_key() :: attributes | compile | exports | functions | md5
                         | module | native | native_addresses | nifs.
-doc false.
-spec get_module_info(Module, Item) -> ModuleInfo when
      Module :: atom(),
      Item :: module_info_key(),
      ModuleInfo :: term().
get_module_info(_Module, _Item) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:hd/1
-doc """
Returns the head of `List`, that is, the first element.

It works with improper lists.

Examples:

```erlang
> hd([1,2,3,4,5]).
1
```

```erlang
> hd([first, second, third, so_on | improper_end]).
first
```

Failure: `badarg` if `List` is an empty list `[]`.
""".
-doc #{ category => terms }.
-spec hd(List) -> Head when
      List :: nonempty_maybe_improper_list(),
      Head :: term().
hd(_List) ->
    erlang:nif_error(undefined).

%% erlang:info/1 no longer exists!

%% Shadowed by erl_bif_types: erlang:is_atom/1
-doc """
Returns `true` if `Term` is an atom, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_atom(Term) -> boolean() when
      Term :: term().
is_atom(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_binary/1
-doc """
Returns `true` if `Term` is a binary, otherwise `false`.

A binary always contains a complete number of bytes.
""".
-doc #{ category => terms }.
-spec is_binary(Term) -> boolean() when
      Term :: term().
is_binary(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_bitstring/1
-doc """
Returns `true` if `Term` is a bitstring (including a binary), otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_bitstring(Term) -> boolean() when
      Term :: term().
is_bitstring(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_boolean/1
-doc """
Returns `true` if `Term` is the atom `true` or the atom `false` (that is, a
boolean). Otherwise returns `false`.
""".
-doc #{ category => terms }.
-spec is_boolean(Term) -> boolean() when
      Term :: term().
is_boolean(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_float/1
-doc """
Returns `true` if `Term` is a floating point number, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_float(Term) -> boolean() when
      Term :: term().
is_float(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_function/1
-doc """
Returns `true` if `Term` is a fun, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_function(Term) -> boolean() when
      Term :: term().
is_function(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_function/2
-doc """
Returns `true` if `Term` is a fun that can be applied with `Arity` number of
arguments, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_function(Term, Arity) -> boolean() when
      Term :: term(),
      Arity :: arity().
is_function(_Term, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_integer/1
-doc """
Returns `true` if `Term` is an integer, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_integer(Term) -> boolean() when
      Term :: term().
is_integer(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_list/1
-doc """
Returns `true` if `Term` is a list with zero or more elements, otherwise
`false`.
""".
-doc #{ category => terms }.
-spec is_list(Term) -> boolean() when
      Term :: term().
is_list(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_number/1
-doc """
Returns `true` if `Term` is an integer or a floating point number. Otherwise
returns `false`.
""".
-doc #{ category => terms }.
-spec is_number(Term) -> boolean() when
      Term :: term().
is_number(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_pid/1
-doc """
Returns `true` if `Term` is a process identifier, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_pid(Term) -> boolean() when
      Term :: term().
is_pid(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_map/1
-doc """
Returns `true` if `Term` is a map, otherwise `false`.
""".
-doc(#{since => <<"OTP 17.0">>}).
-doc #{ category => terms }.
-spec is_map(Term) -> boolean() when
      Term :: term().
is_map(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_port/1
-doc """
Returns `true` if `Term` is a port identifier, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_port(Term) -> boolean() when
      Term :: term().
is_port(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_record/2
-doc """
Returns `true` if `Term` is a tuple and its first element is `RecordTag`.
Otherwise returns `false`.

> #### Note {: .info }
>
> Normally the compiler treats calls to [`is_record/2`](`is_record/2`)
> especially. It emits code to verify that `Term` is a tuple, that its first
> element is `RecordTag`, and that the size is correct. However, if `RecordTag`
> is not a literal atom, the BIF [`is_record/2`](`is_record/2`) is called
> instead and the size of the tuple is not verified.

Allowed in guard tests, if `RecordTag` is a literal atom.
""".
-doc #{ category => terms }.
-spec is_record(Term,RecordTag) -> boolean() when
      Term :: term(),
      RecordTag :: atom().
is_record(_Term,_RecordTag) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_record/3
-doc """
`RecordTag` must be an atom.

Returns `true` if `Term` is a tuple, its first element is `RecordTag`, and its
size is `Size`. Otherwise returns `false`.

Allowed in guard tests if `RecordTag` is a literal atom and `Size` is a literal
integer.

> #### Note {: .info }
>
> This BIF is documented for completeness. Usually
> [`is_record/2`](`is_record/2`) is to be used.
""".
-doc #{ category => terms }.
-spec is_record(Term,RecordTag,Size) -> boolean() when
      Term :: term(),
      RecordTag :: atom(),
      Size :: non_neg_integer().
is_record(_Term,_RecordTag,_Size) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_reference/1
-doc """
Returns `true` if `Term` is a reference, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_reference(Term) -> boolean() when
      Term :: term().
is_reference(_Term) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:is_tuple/1
-doc """
Returns `true` if `Term` is a tuple, otherwise `false`.
""".
-doc #{ category => terms }.
-spec is_tuple(Term) -> boolean() when
      Term :: term().
is_tuple(_Term) ->
    erlang:nif_error(undefined).

-doc """
Loads `Module` described by the object code contained within `Binary`.

If the code for module `Module` already exists, all export
references are replaced so they point to the newly loaded code. The previously
loaded code is kept in the system as old code, as there can still be processes
executing that code.

Returns either `{module, Module}`, or `{error, Reason}` if loading fails.
`Reason` is one of the following:

- **`badfile`** - The object code in `Binary` has an incorrect format _or_ the
  object code contains code for another module than `Module`.

- **`not_purged`** - `Binary` contains a module that cannot be loaded because
  old code for this module already exists.

- **`on_load`** - The code in `Binary` contains an `on_load` declaration that
  must be executed before `Binary` can become the current code. Any previous
  current code for `Module` will remain until the `on_load` call has finished.

- **not_allowed** - The code in `Binary` has been compiled with features that
  are currently not enabled in the runtime system.

> #### Warning {: .warning }
>
> This BIF is intended for the code server (see `m:code`) and is not to be used
> elsewhere.
""".
-doc #{ category => code }.
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

-doc """
Loads and links a dynamic library containing native implemented functions (NIFs)
for a module.

`Path` is a file path to the shareable object/dynamic library file
minus the OS-dependent file extension (`.so` for Unix and `.dll` for Windows).
Notice that on most OSs the library has to have a different name on disc when an
upgrade of the nif is done. If the name is the same, but the contents differ,
the old library may be loaded instead. For information on how to implement a NIF
library, see [`erl_nif(3)`](erl_nif.md).

`LoadInfo` can be any term. It is passed on to the library as part of the
initialization. A good practice is to include a module version number to support
future code upgrade scenarios.

The call to [`load_nif/2`](`load_nif/2`) must be made _directly_ from the Erlang
code of the module that the NIF library belongs to. It returns either `ok`, or
`{error,{Reason,Text}}` if loading fails. `Reason` is one of the following atoms
while `Text` is a human readable string that can give more information about the
failure:

- **`load_failed`** - The OS failed to load the NIF library.

- **`bad_lib`** - The library did not fulfill the requirements as a NIF library
  of the calling module.

- **`load | upgrade`** - The corresponding library callback was unsuccessful.

- **`reload`** - A NIF library is already loaded for this module instance. The
  previously deprecated `reload` feature was removed in OTP 20.

- **`old_code`** - The call to [`load_nif/2`](`load_nif/2`) was made from the
  old code of a module that has been upgraded; this is not allowed.

If the [`-nifs()`](`e:system:modules.md#nifs_attribute`) attribute is used
(which is recommended), all NIFs in the dynamic library must be declared as such
for [`load_nif/2`](`load_nif/2`) to succeed. On the other hand, all functions
declared with the `-nifs()` attribute do not have to be implemented by the
dynamic library. This allows a target independent Erlang file to contain
fallback implementations for functions that may lack NIF support depending on
target OS/hardware platform.
""".
-doc #{ category => code }.
-spec load_nif(Path, LoadInfo) ->  ok | Error when
      Path :: string(),
      LoadInfo :: term(),
      Error :: {error, {Reason, Text :: string()}},
      Reason :: load_failed | bad_lib | load | reload | upgrade | old_code.
load_nif(_Path, _LoadInfo) ->
    erlang:nif_error(undefined).

-doc """
Converts local date and time to Universal Time Coordinated (UTC) as
`erlang:localtime_to_universaltime/1`, but the caller decides if Daylight Saving
Time is active.

If `IsDst == true`, `Localtime` is during Daylight Saving Time, if
`IsDst == false` it is not. If `IsDst == undefined`, the underlying OS can
guess, which is the same as calling
`erlang:localtime_to_universaltime(Localtime)`.

Examples:

```erlang
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, true).
{{1996,11,6},{12,45,17}}
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, false).
{{1996,11,6},{13,45,17}}
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}, undefined).
{{1996,11,6},{13,45,17}}
```

Failure: `badarg` if `Localtime` denotes an invalid date and time.
""".
-doc #{ category => time }.
-spec localtime_to_universaltime(Localtime, IsDst) -> Universaltime when
      Localtime :: calendar:datetime(),
      Universaltime :: calendar:datetime(),
      IsDst :: true | false | undefined.
localtime_to_universaltime(_Localtime, _IsDst) ->
    erlang:nif_error(undefined).

%% CHECK! Why the strange very thorough specification of the error
%% condition with disallowed arity in erl_bif_types?
%% Not documented
%% Shadowed by erl_bif_types: erlang:make_fun/3
-doc false.
-spec make_fun(Module, Function, Arity) -> function() when
      Module :: atom(),
      Function :: atom(),
      Arity :: arity().
make_fun(_Module,_Function, _Arity) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:make_tuple/2
-doc """
Creates a new tuple of the specified `Arity`, where all elements are
`InitialValue`.

For example:

```erlang
> erlang:make_tuple(4, []).
{[],[],[],[]}
```
""".
-doc #{ category => terms }.
-spec make_tuple(Arity, InitialValue) -> tuple() when
      Arity :: arity(),
      InitialValue :: term().
make_tuple(_Arity,_InitialValue) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:make_tuple/3
-doc """
Creates a tuple of size `Arity`, where each element has value `DefaultValue`,
and then fills in values from `InitList`.

Each list element in `InitList` must be a two-tuple, where the first element is
a position in the newly created tuple and the second element is any term. If a
position occurs more than once in the list, the term corresponding to the last
occurrence is used.

For example:

```erlang
> erlang:make_tuple(5, [], [{2,ignored},{5,zz},{2,aa}]).
{[],aa,[],[],zz}
```
""".
-doc #{ category => terms }.
-spec make_tuple(Arity, DefaultValue, InitList) -> tuple() when
      Arity :: arity(),
      DefaultValue :: term(),
      InitList :: [{Position :: pos_integer(), term()}].
make_tuple(_Arity,_DefaultValue,_InitList) ->
    erlang:nif_error(undefined).

-doc """
Returns a port identifier as the result of opening a new Erlang port. A port can
be seen as an external Erlang process.

The name of the executable as well as the arguments specified in `cd`, `env`,
`args`, and `arg0` are subject to Unicode filename translation if the system is
running in Unicode filename mode. To avoid translation or to force, for example
UTF-8, supply the executable and/or arguments as a binary in the correct
encoding. For details, see the module `m:file`, the function
`file:native_name_encoding/0` in Kernel, and the
[`Using Unicode in Erlang`](`e:stdlib:unicode_usage.md`) User's Guide.

> #### Note {: .info }
>
> The characters in the name (if specified as a list) can only be > 255 if the
> Erlang virtual machine is started in Unicode filename translation mode.
> Otherwise the name of the executable is limited to the ISO Latin-1 character
> set.

`PortName`s:

- **`{spawn, Command}`** - Starts an external program. `Command` is the name of
  the external program to be run. `Command` runs outside the Erlang work space
  unless an Erlang driver with the name `Command` is found. If found, that
  driver is started. A driver runs in the Erlang work space, which means that it
  is linked with the Erlang runtime system.

  For external programs, `PATH` is searched (or an equivalent method is used to
  find programs, depending on the OS). This is done by invoking the shell on
  certain platforms. The first space-separated token of the command is
  considered as the name of the executable (or driver). This (among other
  things) makes this option unsuitable for running programs with spaces in
  filenames or directory names. If spaces in executable filenames are desired,
  use `{spawn_executable, Command}` instead.

  > #### Warning {: .warning }
  >
  > On Unix systems, arguments are passed to a new operating system process as
  > an array of strings but on Windows it is up to the child process to parse
  > them and some Windows programs may apply their own rules, which are
  > inconsistent with the standard C runtime `argv` parsing.
  >
  > This is particularly troublesome when invoking `.bat`, `.cmd`, or `.com`
  > files as these run implicitly through `cmd.exe`, whose argument parsing is
  > vulnerable to malicious input and can be used to run arbitrary shell
  > commands.
  >
  > Therefore, if you are running on Windows and you execute batch files or
  > `.com` applications, you must not pass untrusted input as arguments to the
  > program. This affects both `spawn` and `spawn_executable`.

- **`{spawn_executable, FileName}`** - Works like `{spawn, FileName}`, but only
  runs external executables. `FileName` in its whole is used as the name of the
  executable, including any spaces. If arguments are to be passed, the
  `PortSettings` `args` and `arg0` can be used.

  The shell is usually not invoked to start the program, it is executed
  directly. `PATH` (or equivalent) is not searched. To find a program in `PATH`
  to execute, use `os:find_executable/1`.

  Only if a shell script or `.bat` file is executed, the appropriate command
  interpreter is invoked implicitly, but there is still no command-argument
  expansion or implicit `PATH` search.

  If `FileName` cannot be run, an error exception is raised, with the POSIX
  error code as the reason. The error reason can differ between OSs. Typically
  the error `enoent` is raised when an attempt is made to run a program that is
  not found and `eacces` is raised when the specified file is not executable.

- **`{spawn_driver, Command}`** - Works like `{spawn, Command}`, but demands the
  first (space-separated) token of the command to be the name of a loaded
  driver. If no driver with that name is loaded, a `badarg` error is raised.

- **`{fd, In, Out}`** - Allows an Erlang process to access any currently opened
  file descriptors used by Erlang. The file descriptor `In` can be used for
  standard input, and the file descriptor `Out` for standard output. It is only
  used for various servers in the Erlang OS (`shell` and `user`). Hence, its use
  is limited.

`PortSettings` is a list of settings for the port. The valid settings are as
follows:

- **`{packet, N}`** - Messages are preceded by their length, sent in `N` bytes,
  with the most significant byte first. The valid values for `N` are 1, 2,
  and 4.

- **`stream`** - Output messages are sent without packet lengths. A user-defined
  protocol must be used between the Erlang process and the external object.

- **`{line, L}`** - Messages are delivered on a per line basis. Each line
  (delimited by the OS-dependent newline sequence) is delivered in a single
  message. The message data format is `{Flag, Line}`, where `Flag` is `eol` or
  `noeol`, and `Line` is the data delivered (without the newline sequence).

  `L` specifies the maximum line length in bytes. Lines longer than this are
  delivered in more than one message, with `Flag` set to `noeol` for all but the
  last message. If end of file is encountered anywhere else than immediately
  following a newline sequence, the last line is also delivered with `Flag` set
  to `noeol`. Otherwise lines are delivered with `Flag` set to `eol`.

  The `{packet, N}` and `{line, L}` settings are mutually exclusive.

- **`{cd, Dir}`** - Only valid for `{spawn, Command}` and
  `{spawn_executable, FileName}`. The external program starts using `Dir` as its
  working directory. `Dir` must be a string.

- **`{env, Env}`** - Only valid for `{spawn, Command}`, and `{spawn_executable, FileName}`.
  The environment of the started process is extended using the environment
  specifications in `Env`.

  `Env` is to be a list of tuples `{Name, Val}`, where `Name` is a `t:os:env_var_name/0`
  representing the name of an environment variable, and `Val` is a `t:os:env_var_name/0`
  representing the value it is to have in the spawned port process. Both `Name` and `Val` must
  be strings.

  If `Val` is set to the atom `false` or the empty string (that is `""` or `[]`), open_port
  will consider those variables unset just as if `os:unsetenv/1` had been called.

  For information about encoding requirements, see documentation of the types
  for `Name` and `Val`.

- **`{args, [ string() | binary() ]}`** - Only valid for
  `{spawn_executable, FileName}` and specifies arguments to the executable. Each
  argument is specified as a separate string and (on Unix) eventually ends up as
  one element each in the argument vector. On other platforms, a similar
  behavior is mimicked.

  The arguments are not expanded by the shell before they are supplied to the
  executable. Most notably this means that file wildcard expansion does not
  occur. To expand wildcards for the arguments, use `filelib:wildcard/1`. Notice
  that even if the program is a Unix shell script, meaning that the shell
  ultimately is invoked, wildcard expansion does not occur, and the script is
  provided with the untouched arguments. On Windows, wildcard expansion is
  always up to the program itself, therefore this is not an issue.

  The executable name (also known as `argv[0]`) is not to be specified in this
  list. The proper executable name is automatically used as `argv[0]`, where
  applicable.

  If you explicitly want to set the program name in the argument vector, option
  `arg0` can be used.

- **`{arg0, string() | binary()}`** - Only valid for
  `{spawn_executable, FileName}` and explicitly specifies the program name
  argument when running an executable. This can in some circumstances, on some
  OSs, be desirable. How the program responds to this is highly system-dependent
  and no specific effect is guaranteed.

- **`exit_status`** - Only valid for `{spawn, Command}`, where `Command` refers
  to an external program, and for `{spawn_executable, FileName}`.

  When the external process connected to the port exits, a message of the form
  `{Port,{exit_status,Status}}` is sent to the connected process, where `Status`
  is the exit status of the external process. If the program aborts on Unix, the
  same convention is used as the shells do (that is, 128+signal).

  If option `eof` is specified also, the messages `eof` and `exit_status` appear
  in an unspecified order.

- **`use_stdio`** - Only valid for `{spawn, Command}` and
  `{spawn_executable, FileName}`. It allows the standard input and output (file
  descriptors 0 and 1) of the spawned (Unix) process for communication with
  Erlang.

- **`nouse_stdio`** - The opposite of `use_stdio`. It uses file descriptors 3
  and 4 for communication with Erlang.

- **`stderr_to_stdout`** - Affects ports to external programs. The executed
  program gets its standard error file redirected to its standard output file.
  `stderr_to_stdout` and `nouse_stdio` are mutually exclusive.

- **`overlapped_io`** - Affects ports to external programs on Windows only. The
  standard input and standard output handles of the port program are, if this
  option is supplied, opened with flag `FILE_FLAG_OVERLAPPED`, so that the port
  program can (and must) do overlapped I/O on its standard handles. This is not
  normally the case for simple port programs, but an option of value for the
  experienced Windows programmer. _On all other platforms, this option is
  silently discarded._

- **`in`** - The port can only be used for input.

- **`out`** - The port can only be used for output.

- **`binary`** - All I/O from the port is binary data objects as opposed to
  lists of bytes.

- **`eof`** - The port is not closed at the end of the file and does not produce
  an exit signal. Instead, it remains open and a `{Port, eof}` message is sent
  to the process holding the port.

- **`hide`** - When running on Windows, suppresses creation of a new console
  window when spawning the port program. (This option has no effect on other
  platforms.)

- **`{parallelism, Boolean}`** - [](){: #open_port_parallelism } Sets scheduler
  hint for port parallelism. If set to `true`, the virtual machine schedules
  port tasks; when doing so, it improves parallelism in the system. If set to
  `false`, the virtual machine tries to perform port tasks immediately,
  improving latency at the expense of parallelism. The default can be set at
  system startup by passing command-line argument [`+spp`](erl_cmd.md#%2Bspp) to
  [erl](erl_cmd.md).

- **`{busy_limits_port, {Low, High} | disabled}`** - Sets limits that will be
  used for controlling the busy state of the port.

  When the ports internal output queue size becomes larger than or equal to
  `High` bytes, it enters the busy state. When it becomes less than `Low` bytes
  it leaves the busy state. When the port is in the busy state, processes
  sending commands to it will be suspended until the port leaves the busy state.
  Commands are in this context either `Port ! {Owner, {command, Data}}` or
  `port_command/[2,3]`.

  The `Low` limit is automatically adjusted to the same as `High` if it is set
  larger then `High`. Valid range of values for `Low` and `High` is
  `[1, (1 bsl (8*erlang:system_info(wordsize)))-2]`. If the atom `disabled` is
  passed, the port will never enter the busy state.

  The defaults are `Low = 4096` and `High = 8192`.

  _Note_ that this option is only valid when spawning an executable (port
  program) by opening the spawn driver and when opening the `fd` driver. This
  option will cause a failure with a `badarg` exception when opening other
  drivers.

- **`{busy_limits_msgq, {Low, High} | disabled}`** - Sets limits that will be
  used for controlling the busy state of the port message queue.

  When the ports message queue size becomes larger than or equal to `High` bytes
  it enters the busy state. When it becomes less than `Low` bytes it leaves the
  busy state. When the port message queue is in the busy state, processes
  sending commands to it will be suspended until the port message queue leaves
  the busy state. Commands are in this context either
  `Port ! {Owner, {command, Data}}` or `port_command/[2,3]`.

  The `Low` limit is automatically adjusted to the same as `High` if it is set
  larger then `High`. Valid range of values for `Low` and `High` is
  `[1, (1 bsl (8*erlang:system_info(wordsize)))-2]`. If the atom `disabled` is
  passed, the port message queue will never enter the busy state.

  _Note_ that if the driver statically has disabled the use of this feature, a
  failure with a `badarg` exception will be raised unless this option also is
  set to `disable` or not passed at all.

  The defaults are `Low = 4096` and `High = 8192` unless the driver itself does
  modifications of these values.

  _Note_ that the driver might fail if it also adjust these limits by itself and
  you have disabled this feature.

  The spawn driver (used when spawning an executable) and the `fd` driver do not
  disable this feature and do not adjust these limits by themselves.

  For more information see the documentation
  [`erl_drv_busy_msgq_limits()`](erl_driver.md#erl_drv_busy_msgq_limits).

Default is `stream` for all port types and `use_stdio` for spawned ports.

Failure: if the port cannot be opened, the exit reason is `badarg`,
`system_limit`, or the POSIX error code that most closely describes the error,
or `einval` if no POSIX code is appropriate:

- **`badarg`** - Bad input arguments to `open_port`.

- **`system_limit`** - All available ports in the Erlang emulator are in use.

- **`enomem`** - Not enough memory to create the port.

- **`eagain`** - No more available OS processes.

- **`enametoolong`** - Too long external command.

- **`emfile`** - No more available file descriptors (for the OS process that the
  Erlang emulator runs in).

- **`enfile`** - Full file table (for the entire OS).

- **`eacces`** - `Command` specified in `{spawn_executable, Command}` does not
  point out an executable file.

- **`enoent`** - `FileName` specified in `{spawn_executable, FileName}` does not
  point out an existing file.

During use of a port opened using `{spawn, Name}`, `{spawn_driver, Name}`, or
`{spawn_executable, Name}`, errors arising when sending messages to it are
reported to the owning process using signals of the form
`{'EXIT', Port, PosixCode}`. For the possible values of `PosixCode`, see
`m:file`.

The maximum number of ports that can be open at the same time can be configured
by passing command-line flag [`+Q`](erl_cmd.md#max_ports) to [erl](erl_cmd.md).
""".
-doc #{ category => ports }.
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
           | {env, Env :: [{Name :: os:env_var_name(), Val :: os:env_var_value() | [] | false}]}
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

-doc """
Process priority level. For more info see
[`process_flag(priority, Level)`](#process_flag_priority)
""".
-type priority_level() ::
      low | normal | high | max.

-doc """
See
[`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

Process message queue data configuration. For more information, see
[`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data)
""".
-type message_queue_data() ::
	off_heap | on_heap.

-doc """
process_flag(Flag, Value)

Sets the process flag indicated to the specified value. Returns the previous value
of the flag.

`Flag` is one of the following:

- ```erlang
  process_flag(async_dist, boolean())
  ```
  {: #process_flag_async_dist }
  
  Enable or disable _fully asynchronous distributed signaling_ for the calling
  process. When disabled, which is the default, the process sending a distributed
  signal will block in the send operation if the buffer for the distribution
  channel reach the [distribution buffer busy limit](erl_cmd.md#%2Bzdbbl). The
  process will remain blocked until the buffer shrinks enough. This might in some
  cases take a substantial amount of time. When `async_dist` is enabled, send
  operations of distributed signals will always buffer the signal on the outgoing
  distribution channel and then immediately return. That is, these send operations
  will _never_ block the sending process.
  
  > #### Note {: .info }
  >
  > Since no flow control is enforced by the runtime system when `async_dist`
  > process flag is enabled, you need to make sure that flow control for such data
  > is implemented, or that the amount of such data is known to always be limited.
  > Unlimited signaling with `async_dist` enabled in the absence of flow control
  > will typically cause the sending runtime system to crash on an out of memory
  > condition.
  
  Blocking due to disabled `async_dist` can be monitored by
  [`trace:system()`](`trace:system/3`) using the
  [`busy_dist_port`](`m:trace#busy_dist_port`) option. Only data buffered by
  processes which (at the time of sending a signal) have disabled `async_dist`
  will be counted when determining whether or not an operation should block the
  caller.
  
  The `async_dist` flag can also be set on a new process when spawning it using
  the [`spawn_opt()`](`spawn_opt/4`) BIF with the option
  [`{async_dist, Enable}`](#spawn_opt_async_dist). The default
  `async_dist` flag to use on newly spawned processes can be set by passing the
  command line argument [`+pad <boolean>`](erl_cmd.md#%2Bpad) when starting the
  runtime system. If the `+pad <boolean>` command line argument is not passed, the
  default value of the `async_dist` flag will be `false`.
  
  You can inspect the state of the `async_dist` process flag of a process by
  calling [`process_info(Pid, async_dist)`](#process_info_async_dist).
  
- ```erlang
  process_flag(trap_exit, boolean())
  ```
  {: #process_flag_trap_exit }
  
  When `trap_exit` is set to `true`, exit signals arriving to a process are
  converted to `{'EXIT', From, Reason}` messages, which can be received as
  ordinary messages. If `trap_exit` is set to `false`, the process exits if it
  receives an exit signal other than `normal` and the exit signal is propagated to
  its linked processes. Application processes are normally not to trap exits.
  
  See also `exit/2`.
  
- ```erlang
  process_flag(error_handler, module())
  ```
  {: #process_flag_error_handler }
  
  Used by a process to redefine the `m:error_handler` for undefined function calls and
  undefined registered processes. Use this flag with substantial caution, as code
  auto-loading depends on the correct operation of the error handling module.
  
- ```erlang
  process_flag(fullsweep_after,  non_neg_integer())
  ```
  
  Changes the maximum number of generational collections before forcing a
  fullsweep for the calling process.
  
- ```erlang
  process_flag(min_heap_size, non_neg_integer())
  ```
  {: #process_flag_min_heap_size }
  
  Changes the minimum heap size for the calling process.
  
- ```erlang
  process_flag(min_bin_vheap_size, non_neg_integer())
  ```
  
  Changes the minimum binary virtual heap size for the calling process.
  
- ```erlang
  process_flag(max_heap_size, max_heap_size())
  ```
  {: #process_flag_max_heap_size }
  
  This flag sets the maximum heap size for the calling process. If `MaxHeapSize`
  is an integer, the system default values for `kill` and `error_logger` are used.
  
  For details on how the heap grows, see
  [Sizing the heap](GarbageCollection.md#sizing-the-heap) in the ERTS internal
  documentation.
  
  - **`size`** - The maximum size in words of the process. If set to zero, the
    heap size limit is disabled. `badarg` is be thrown if the value is smaller
    than [`min_heap_size`](#process_flag_min_heap_size). The size check
    is only done when a garbage collection is triggered.
  
    `size` is the entire heap of the process when garbage collection is triggered.
    This includes all generational heaps, the process stack, any
    [messages that are considered to be part of the heap](#process_flag_message_queue_data),
    and any extra memory that the garbage collector needs during collection.
  
    `size` is the same as can be retrieved using
    [`erlang:process_info(Pid, total_heap_size)`](#process_info_total_heap_size),
    or by adding `heap_block_size`, `old_heap_block_size` and `mbuf_size` from
    [`erlang:process_info(Pid, garbage_collection_info)`](#process_info_garbage_collection_info).
  
  - **`kill`** - When set to `true`, the runtime system sends an untrappable exit
    signal with reason `kill` to the process if the maximum heap size is reached.
    The garbage collection that triggered the `kill` is not completed, instead the
    process exits as soon as possible. When set to `false`, no exit signal is sent
    to the process, instead it continues executing.
  
    If `kill` is not defined in the map, the system default will be used. The
    default system default is `true`. It can be changed by either option
    [\+hmaxk](erl_cmd.md#%2Bhmaxk) in [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  - **`error_logger`** - When set to `true`, the runtime system logs an error
    event via `m:logger`, containing details about the process when the maximum
    heap size is reached. One log event is sent each time the limit is reached.
  
    If `error_logger` is not defined in the map, the system default is used. The
    default system default is `true`. It can be changed by either the option
    [\+hmaxel](erl_cmd.md#%2Bhmaxel) int [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  - **`include_shared_binaries`** - When set to `true`, off-heap binaries are
    included in the total sum compared against the `size` limit. Off-heap binaries
    are typically larger binaries that may be shared between processes. The size
    of a shared binary is included by all processes that are referring it. Also,
    the entire size of a large binary may be included even if only a smaller part
    of it is referred by the process.
  
    If `include_shared_binaries` is not defined in the map, the system default is
    used. The default system default is `false`. It can be changed by either the
    option [\+hmaxib](erl_cmd.md#%2Bhmaxib) in [erl](erl_cmd.md), or
    [`erlang:system_flag(max_heap_size, MaxHeapSize)`](#system_flag_max_heap_size).
  
  The heap size of a process is quite hard to predict, especially the amount of
  memory that is used during the garbage collection. When contemplating using this
  option, it is recommended to first run it in production with `kill` set to
  `false` and inspect the log events to see what the normal peak sizes of the
  processes in the system is and then tune the value accordingly.
  
- ```erlang
  process_flag(message_queue_data, message_queue_data())
  ```
  {: #process_flag_message_queue_data }
  
  Determines how messages in the message queue are stored, as follows:
  
  - **`off_heap`** - _All_ messages in the message queue will be stored outside
    the process heap. This implies that _no_ messages in the message queue will be
    part of a garbage collection of the process.
  
  - **`on_heap`** - All messages in the message queue will eventually be placed on
    the process heap. They can, however, be temporarily stored off the heap. This
    is how messages have always been stored up until ERTS 8.0.
  
  The default value of the `message_queue_data` process flag is determined by the
  command-line argument [`+hmqd`](erl_cmd.md#%2Bhmqd) in [erl](erl_cmd.md).
  
  If the process may potentially accumulate a large number of messages in its
  queue it is recommended to set the flag value to `off_heap`. This is due to the
  fact that the garbage collection of a process that has a large number of
  messages stored on the heap can become extremely expensive and the process can
  consume large amounts of memory. The performance of the actual message passing
  is, however, generally better when the flag value is `on_heap`.
  
  Changing the flag value causes any existing messages to be moved. The move
  operation is initiated, but not necessarily completed, by the time the function
  returns.
  
- ```erlang
  process_flag(priority, priority_level())
  ```
  {: #process_flag_priority }
  
  Sets the process priority. `Level` is an atom. Four priority levels exist:
  `low`, `normal`, `high`, and `max`. Default is `normal`.
  
  > #### Note {: .info }
  >
  > Priority level `max` is reserved for internal use in the Erlang runtime
  > system, and is _not_ to be used by others.
  
  Internally in each priority level, processes are scheduled in a round robin
  fashion.
  
  Execution of processes on priority `normal` and `low` are interleaved. Processes
  on priority `low` are selected for execution less frequently than processes on
  priority `normal`.
  
  When runnable processes on priority `high` exist, no processes on priority `low`
  or `normal` are selected for execution. Notice however that this does _not_ mean
  that no processes on priority `low` or `normal` can run when processes are
  running on priority `high`. When using multiple schedulers, more processes can
  be running in parallel than processes on priority `high`. That is, a `low` and a
  `high` priority process can execute at the same time.
  
  When runnable processes on priority `max` exist, no processes on priority `low`,
  `normal`, or `high` are selected for execution. As with priority `high`,
  processes on lower priorities can execute in parallel with processes on priority
  `max`.
  
  Scheduling is pre-emptive. Regardless of priority, a process is pre-empted when
  it has consumed more than a certain number of reductions since the last time it
  was selected for execution.
  
  > #### Note {: .info }
  >
  > Do not depend on the scheduling to remain exactly as it is today. Scheduling
  > is likely to be changed in a future release to use available processor cores
  > better.
  
  There is _no_ automatic mechanism for avoiding priority inversion, such as
  priority inheritance or priority ceilings. When using priorities, take this into
  account and handle such scenarios by yourself.
  
  Making calls from a `high` priority process into code that you has no control
  over can cause the `high` priority process to wait for a process with lower
  priority. That is, effectively decreasing the priority of the `high` priority
  process during the call. Even if this is not the case with one version of the
  code that you have no control over, it can be the case in a future version of
  it. This can, for example, occur if a `high` priority process triggers code
  loading, as the code server runs on priority `normal`.
  
  Other priorities than `normal` are normally not needed. When other priorities
  are used, use them with care, _especially_ priority `high`. A process on
  priority `high` is only to perform work for short periods. Busy looping for long
  periods in a `high` priority process causes most likely problems, as important
  OTP servers run on priority `normal`.
  
- ```erlang
  process_flag(save_calls, 0..10000)
  ```
  
  `N` must be an integer in the interval 0..10000. If `N` > 0, call saving is made
  active for the process. This means that information about the `N` most recent
  global function calls, BIF calls, sends, and receives made by the process are
  saved in a list, which can be retrieved with
  [`process_info(Pid, last_calls)`](`process_info/2`). A global function call is
  one in which the module of the function is explicitly mentioned. Only a fixed
  amount of information is saved, as follows:
  
  - A tuple `{Module, Function, Arity}` for function calls
  - The atoms `send`, `'receive'`, and `timeout` for sends and receives
    (`'receive'` when a message is received and `timeout` when a receive times
    out)
  
  If `N` = 0, call saving is disabled for the process, which is the default.
  Whenever the size of the call saving list is set, its contents are reset.
  
- ```erlang
  process_flag(sensitive, boolean())
  ```
  
  Sets or clears flag `sensitive` for the current process. When a process has been
  marked as sensitive by calling
  [`process_flag(sensitive, true)`](`process_flag/2`), features in the runtime
  system that can be used for examining the data or inner working of the process
  are silently disabled.
  
  Features that are disabled include (but are not limited to) the following:
  
  - Tracing. Trace flags can still be set for the process, but no trace messages
    of any kind are generated. (If flag `sensitive` is turned off, trace messages
    are again generated if any trace flags are set.)
  - Sequential tracing. The sequential trace token is propagated as usual, but no
    sequential trace messages are generated.
  
  `process_info/1,2` cannot be used to read out the message queue or the process
  dictionary (both are returned as empty lists).
  
  Stack back-traces cannot be displayed for the process.
  
  In crash dumps, the stack, messages, and the process dictionary are omitted.
  
  If `{save_calls,N}` has been set for the process, no function calls are saved to
  the call saving list. (The call saving list is not cleared. Also, send, receive,
  and time-out events are still added to the list.)
""".
-doc #{ category => processes }.
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
      {dictionary, Key :: term()} |
      error_handler |
      garbage_collection |
      garbage_collection_info |
      group_leader |
      heap_size |
      initial_call |
      links |
      label |
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
      priority_messages |
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
      {{dictionary, Key :: term()}, Value :: term()} |
      {error_handler, Module :: module()} |
      {garbage_collection, GCInfo :: [{atom(),non_neg_integer()}]} |
      {garbage_collection_info, GCInfo :: [{atom(),non_neg_integer()}]} |
      {group_leader, GroupLeader :: pid()} |
      {heap_size, Size :: non_neg_integer()} |
      {initial_call, mfa()} |
      {links, PidsAndPorts :: [pid() | port()]} |
      {label, term()} |
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
      {priority_messages, Enabled :: boolean()} |
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

-doc """
Returns information about the process identified by `Pid`, as specified by
`Item` or `ItemList`. Returns `undefined` if the process is not alive.

If the process is alive and a single `Item` is specified, the returned value is
the corresponding `InfoTuple`, unless `Item =:= registered_name` and the process
has no registered name. In this case, `[]` is returned. This strange behavior is
because of historical reasons, and is kept for backward compatibility.

If `ItemList` is specified, the result is `InfoTupleList`. The `InfoTuple`s in
`InfoTupleList` are included with the corresponding `Item`s in the same order as
the `Item`s were included in `ItemList`. Valid `Item`s can be included multiple
times in `ItemList`.

Getting process information follows the signal ordering guarantees described in
the [Processes Chapter](`e:system:ref_man_processes.md#signals`) in the _Erlang
Reference Manual_.

> #### Note {: .info }
>
> If `registered_name` is part of `ItemList` and the process has no name
> registered, a `{registered_name, []}`, `InfoTuple` _will_ be included in the
> resulting `InfoTupleList`. This behavior is different when a single
> `Item =:= registered_name` is specified, and when
> [`process_info/1`](`process_info/1`) is used.

Valid `InfoTuple`s with corresponding `Item`s:

- **`{async_dist, Enabled}`{: #process_info_async_dist }** - Current value of the
  [`async_dist`](#process_flag_async_dist) process flag.

  Since: OTP 25.3

- **`{backtrace, Bin}`** - Binary `Bin` contains the same information as the
  output from `erlang:process_display(Pid, backtrace)`. Use
  [`binary_to_list/1`](`binary_to_list/1`) to obtain the string of characters
  from the binary.

- **`{binary, BinInfo}`** - `BinInfo` is a list containing miscellaneous
  information about binaries on the heap of this process. This `InfoTuple` can
  be changed or removed without prior notice. In the current implementation
  `BinInfo` is a list of tuples. The tuples contain; `BinaryId`, `BinarySize`,
  `BinaryRefcCount`.

  Depending on the value of the
  [`message_queue_data`](#process_flag_message_queue_data) process
  flag the message queue may be stored on the heap.

- **`{catchlevel, CatchLevel}`** - `CatchLevel` is the number of currently
  active catches in this process. This `InfoTuple` can be changed or removed
  without prior notice.

- **`{current_function, {Module, Function, Arity} | undefined}`** - `Module`,
  `Function`, `Arity` is the current function call of the process. The value
  `undefined` can be returned if the process is currently executing native
  compiled code.

- **`{current_location, {Module, Function, Arity, Location}}`** - `Module`,
  `Function`, `Arity` is the current function call of the process. `Location` is
  a list of two-tuples describing the location in the source code.

- **`{current_stacktrace, Stack}`**{: #process_info_current_stacktrace } -
  Returns the current call stack back-trace
  (_stacktrace_) of the process. The stack has the same format as in the `catch`
  part of a `try`. See
  [The call-stack back trace (stacktrace)](`e:system:errors.md#stacktrace`). The
  depth of the stacktrace is truncated according to the `backtrace_depth` system
  flag setting.

- **`{dictionary, Dictionary}`** - `Dictionary` is the process dictionary.

- **`{{dictionary, Key}, Value}`** - `Value` associated with `Key` in the
  process dictionary.

- **`{error_handler, Module}`** - `Module` is the `m:error_handler` module used by
  the process (for undefined function calls, for example).

- **`{garbage_collection, GCInfo}`** - `GCInfo` is a list containing
  miscellaneous information about garbage collection for this process. The
  content of `GCInfo` can be changed without prior notice.

- **`{garbage_collection_info, GCInfo}`{: #process_info_garbage_collection_info }** -
  `GCInfo` is a list containing miscellaneous detailed information about
  garbage collection for this process. The content of `GCInfo` can be changed
  without prior notice. For details about the meaning of each item, see
  [`gc_minor_start`](`m:trace#gc_minor_start`) in `trace:process/4`.

- **`{group_leader, GroupLeader}`** - `GroupLeader` is the group leader for the
  I/O of the process.

- **`{heap_size, Size}`** - `Size` is the size in words of the youngest heap
  generation of the process. This generation includes the process stack. This
  information is highly implementation-dependent, and can change if the
  implementation changes.

- **`{initial_call, {Module, Function, Arity}}`** - `Module`, `Function`,
  `Arity` is the initial function call with which the process was spawned.

- **`{links, PidsAndPorts}`** - `PidsAndPorts` is a list of process identifiers
  and port identifiers, with processes or ports to which the process has a link.

- **`{label, Label}`** -
  `Label` is the label for the process. See `proc_lib:get_label/1`.

   Since: OTP 27.2

- **`{last_calls, false|Calls}`** - The value is `false` if call saving is not
  active for the process (see `process_flag/3`). If call saving is active, a
  list is returned, in which the last element is the most recent called.

- **`{memory, Size}`** - [](){: #process_info_memory } `Size` is the size in
  bytes of the process. This includes call stack, heap, and internal structures.

- **`{message_queue_len, MessageQueueLen}`** - `MessageQueueLen` is the number
  of messages currently in the message queue of the process. This is the length
  of the list `MessageQueue` returned as the information item `messages` (see
  below).

- **`{messages, MessageQueue}`** - `MessageQueue` is a list of the messages to
  the process, which have not yet been processed.

- **`{min_heap_size, MinHeapSize}`** - `MinHeapSize` is the minimum heap size
  for the process.

- **`{min_bin_vheap_size, MinBinVHeapSize}`** - `MinBinVHeapSize` is the minimum
  binary virtual heap size for the process.

- **`{monitored_by, MonitoredBy}`** - A list of identifiers for all the
  processes, ports and NIF resources, that are monitoring the process.

- **`{monitors, Monitors}`** - A list of monitors (started by
  [`monitor/2`](`monitor/2`)) that are active for the process. For a local
  process monitor or a remote process monitor by a process identifier, the list
  consists of:

  - **`{process, Pid}`** - Process is monitored by pid.

  - **`{process, {RegName, Node}}`** - Local or remote process is monitored by
    name.

  - **`{port, PortId}`** - Local port is monitored by port id.

  - **`{port, {RegName, Node}}`** - Local port is monitored by name. Please
    note, that remote port monitors are not supported, so `Node` will always be
    the local node name.

- **`{message_queue_data, MQD}`** - `MQD` is the current value of the
  `message_queue_data` process flag, which can be either `off_heap` or
  `on_heap`. For more information, see the documentation of
  [`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

- **`{parent, Pid}`** - `Pid` is the identifier of the parent process, the one
  that spawned current process. When the process does not have a parent
  `undefined` is returned. Only the initial process (`init`) on a node lacks a
  parent, though.

  Since: OTP 25.0

- **`{priority, Level}`** - `Level` is the current priority level for the
  process. For more information on priorities, see
  [`process_flag(priority, Level)`](#process_flag_priority).

- **`{priority_messages, Enabled}`** - Since OTP @OTP-19198@

  If `Enabled` equals `true`, the process has
  [enabled priority message reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  enabled priority message reception for at least one type of messages.

  For more information see the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  section of the _Erlang Reference Manual_.


- **`{reductions, Number}`** - `Number` is the number of reductions executed by
  the process.

- **`{registered_name, Atom}`** - `Atom` is the registered process name. If the
  process has no registered name, this tuple is not present in the list.

- **`{sequential_trace_token, [] | SequentialTraceToken}`** -
  `SequentialTraceToken` is the sequential trace token for the process. This
  `InfoTuple` can be changed or removed without prior notice.

- **`{stack_size, Size}`** - `Size` is the stack size, in words, of the process.

- **`{status, Status}`** - `Status` is the status of the process and is one of
  the following:

  - `exiting`
  - `garbage_collecting`
  - `waiting` (for a message)
  - `running`
  - `runnable` (ready to run, but another process is running)
  - `suspended` (suspended on a "busy" port or by the BIF
    `erlang:suspend_process/1,2`)

- **`{suspending, SuspendeeList}`** - `SuspendeeList` is a list of
  `{Suspendee, ActiveSuspendCount, OutstandingSuspendCount}` tuples. `Suspendee`
  is the process identifier of a process that has been, or is to be, suspended
  by the process identified by `Pid` through the BIF
  [`erlang:suspend_process/2`](`suspend_process/2`) or
  [`erlang:suspend_process/1`](`suspend_process/1`).

  `ActiveSuspendCount` is the number of times `Suspendee` has been suspended by
  `Pid`. `OutstandingSuspendCount` is the number of not yet completed suspend
  requests sent by `Pid`, that is:

  - If `ActiveSuspendCount =/= 0`, `Suspendee` is currently in the suspended
    state.
  - If `OutstandingSuspendCount =/= 0`, option `asynchronous` of
    `erlang:suspend_process/2` has been used and the suspendee has not yet been
    suspended by `Pid`.

  Notice that `ActiveSuspendCount` and `OutstandingSuspendCount` are not the
  total suspend count on `Suspendee`, only the parts contributed by `Pid`.

- **`{total_heap_size, Size}`{: #process_info_total_heap_size }** - `Size` is
  the total size, in words, of all heap fragments of the process. This includes
  the process stack and any unreceived messages that are considered to be part
  of the heap.

- **`{trace, InternalTraceFlags}`** - `InternalTraceFlags` is an integer
  representing the internal trace flag for this process. This `InfoTuple` can be
  changed or removed without prior notice.

- **`{trap_exit, Boolean}`** - `Boolean` is `true` if the process is trapping
  exits, otherwise `false`.

Notice that not all implementations support all these `Item`s.

Failures:

- **`badarg`** - If `Pid` is not a local process.

- **`badarg`** - If `Item` is an invalid item.
""".
-doc #{ category => processes }.
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

-doc """
Sends a message and returns `Msg`. This is the same as using the
[send operator](`e:system:expressions.md#send`): `Dest ! Msg`.

`Dest` can be a remote or local process identifier, an alias, a (local) port, a
locally registered name, or a tuple `{RegName, Node}` for a registered name at
another node.

The function fails with a `badarg` run-time error if `Dest` is an atom name, but
this name is not registered. This is the only case when `send` fails for an
unreachable destination `Dest` (of correct type).

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
""".
-doc #{ category => processes }.
-spec send(Dest, Msg) -> Msg when
      Dest :: send_destination(),
      Msg :: term().
send(_Dest,_Msg) ->
    erlang:nif_error(undefined).

%% send/3
-doc """
Either sends a message and returns `ok`, or does not send the message but
returns something else (see below). Otherwise the same as
[`erlang:send/2`](`send/2`).

For more detailed explanation and warnings, see [`erlang:send_nosuspend/2,3`](`send_nosuspend/2`).

Options:

- **`nosuspend`** - If the sender would have to be suspended to do the send,
  `nosuspend` is returned instead.

- **`noconnect`** - If the destination node would have to be auto-connected to
  do the send, `noconnect` is returned instead.

- **`priority`** - Since OTP @OTP-19198@

  Send this message as a priority message. In order for the message to be
  handled as a
  [priority message](`e:system:ref_man_processes.md#priority-messages`) by the
  receiver, this option *must* be passed, and `Dest` *must* be an active
  [*priority alias*](#priority_alias).

  If `Dest` is an active priority alias, but this option is not passed, the
  message will be handled as on ordinary message. The same is true, if this
  option is passed, but `Dest` is not an active priority alias.

  > #### Warning {: .warning }
  >
  > You *very seldom* need to resort to using priority messages and you may
  > [cause issues](`e:system:ref_man_processes.md#priority-message-warning`)
  > instead of solving issues if not used with care.

  For more information see, the
  [_Adding Messages to the Message Queue_](`e:system:ref_man_processes.md#message-queue-order`)
  and the
  [Enabling Priority Message Reception](`e:system:ref_man_processes.md#enable-prio-msg-recv`)
  sections of the _Erlang Reference Manual_.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.

> #### Warning {: .warning }
>
> As with `erlang:send_nosuspend/2,3`: use with extreme care.
""".
-doc #{ category => processes }.
-spec send(Dest, Msg, Options) -> Res when
      Dest :: send_destination(),
      Msg :: term(),
      Options :: [nosuspend | noconnect | priority],
      Res :: ok | nosuspend | noconnect.
send(_Dest,_Msg,_Options) ->
    erlang:nif_error(undefined).

%% Not documented
-doc false.
-spec seq_trace_info(send) -> {send, boolean()};
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
-doc """
Returns a tuple that is a copy of argument `Tuple1` with the element specified
by integer argument `Index` (the first element is the element with index 1)
replaced by argument `Value`.

For example:

```erlang
> setelement(2, {10, green, bottles}, red).
{10,red,bottles}
```
""".
-doc #{ category => terms }.
-spec setelement(Index, Tuple1, Value) -> Tuple2 when
      Index :: pos_integer(),
      Tuple1 :: tuple(),
      Tuple2 :: tuple(),
      Value :: term().
setelement(_Index, _Tuple1, _Value) ->
   erlang:nif_error(undefined).

-doc """
Returns statistics about the current system.

The possible flags are:

- ```erlang
  statistics(active_tasks) -> [non_neg_integer()]
  ```
  {: #statistics_active_tasks }

  Returns the same as
  [`statistics(active_tasks_all)`](#statistics_active_tasks_all) with
  the exception that no information about the dirty IO run queue and its
  associated schedulers is part of the result. That is, only tasks that are
  expected to be CPU bound are part of the result.

  Available since OTP 18.3

- ```erlang
  statistics(active_tasks_all) -> [non_neg_integer()]
  ```
  {: #statistics_active_tasks_all }

  Returns a list where each element represents the amount of active processes and
  ports on each run queue and its associated schedulers. That is, the number of
  processes and ports that are ready to run, or are currently running. Values for
  normal run queues and their associated schedulers are located first in the
  resulting list. The first element corresponds to scheduler number 1 and so on.
  If support for dirty schedulers exist, an element with the value for the dirty
  CPU run queue and its associated dirty CPU schedulers follow and then as last
  element the value for the dirty IO run queue and its associated dirty IO
  schedulers follow. The information is _not_ gathered atomically. That is, the
  result is not necessarily a consistent snapshot of the state, but instead quite
  efficiently gathered.

  > #### Note {: .info }
  >
  > Each normal scheduler has one run queue that it manages. If dirty schedulers
  > are supported, all dirty CPU schedulers share one run queue, and all dirty IO
  > schedulers share one run queue. That is, we have multiple normal run queues,
  > one dirty CPU run queue and one dirty IO run queue. Work can _not_ migrate
  > between the different types of run queues. Only work in normal run queues can
  > migrate to other normal run queues. This has to be taken into account when
  > evaluating the result.

  See also
  [`statistics(total_active_tasks)`](#statistics_total_active_tasks),
  [`statistics(run_queue_lengths)`](#statistics_run_queue_lengths),
  [`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all),
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  and
  [`statistics(total_run_queue_lengths_all)`](#statistics_total_run_queue_lengths_all).

  Available since OTP 20.0

- ```erlang
  statistics(context_switches) -> {non_neg_integer(), 0}
  ```
  {: #statistics_context_switches }

  Returns the total number of context switches since the system started.

- ```erlang
  statistics(exact_reductions) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_exact_reductions }

  Returns the number of exact reductions.

  > #### Note {: .info }
  >
  > [`statistics(exact_reductions)`](`statistics/1`) is a more expensive operation
  > than [statistics(reductions)](#statistics_reductions).

- ```erlang
  statistics(garbage_collection) ->
    { NumerOfGCs :: non_neg_integer(), WordsReclaimed :: non_neg_integer(), 0}
  ```

  Returns information about garbage collection, for example:

  ```erlang
  > statistics(garbage_collection).
  {85,23961,0}
  ```

  This information can be invalid for some implementations.

- ```erlang
  statistics(io) -> {{input, non_neg_integer()}, {output, non_neg_integer()}}
  ```

  Returns `Input`, which is the total number of bytes received through ports, and
  `Output`, which is the total number of bytes output to ports.

- ```erlang
  statistics(microstate_accounting) -> [MSAcc_Thread]
  ```
  {: #statistics_microstate_accounting }

  Microstate accounting can be used to measure how much time the Erlang runtime
  system spends doing various tasks. It is designed to be as lightweight as
  possible, but some overhead exists when this is enabled. Microstate accounting
  is meant to be a profiling tool to help finding performance bottlenecks. To
  `start`/`stop`/`reset` microstate accounting, use system flag
  [`microstate_accounting`](#system_flag_microstate_accounting).

  [`statistics(microstate_accounting)`](`statistics/1`) returns a list of maps
  representing some of the OS threads within ERTS. Each map contains `type` and
  `id` fields that can be used to identify what thread it is, and also a counters
  field that contains data about how much time has been spent in the various
  states.

  Example:

  ```erlang
  > erlang:statistics(microstate_accounting).
  [#{counters => #{aux => 1899182914,
                   check_io => 2605863602,
                   emulator => 45731880463,
                   gc => 1512206910,
                   other => 5421338456,
                   port => 221631,
                   sleep => 5150294100},
     id => 1,
     type => scheduler}|...]
  ```

  The time unit is the same as returned by `os:perf_counter/0`. So, to convert it
  to milliseconds, you can do something like this:

  ```erlang
  lists:map(
    fun(#{ counters := Cnt } = M) ->
           MsCnt = maps:map(fun(_K, PerfCount) ->
                                      erlang:convert_time_unit(PerfCount, perf_counter, 1000)
                             end, Cnt),
           M#{ counters := MsCnt }
    end, erlang:statistics(microstate_accounting)).
  ```

  Notice that these values are not guaranteed to be the exact time spent in each
  state. This is because of various optimisation done to keep the overhead as
  small as possible.

  `MSAcc_Thread_Type`s:

  - **`scheduler`** - The main execution threads that do most of the work. See
    [erl +S](erl_cmd.md#%2BS) for more details.

  - **`dirty_cpu_scheduler`** - The threads for long running cpu intensive work.
    See [erl +SDcpu](erl_cmd.md#%2BSDcpu) for more details.

  - **`dirty_io_scheduler`** - The threads for long running I/O work. See
    [erl +SDio](erl_cmd.md#%2BSDio) for more details.

  - **`async`** - Async threads are used by various linked-in drivers (mainly the
    file drivers) do offload non-CPU intensive work. See
    [erl +A](erl_cmd.md#async_thread_pool_size) for more details.

  - **`aux`** - Takes care of any work that is not specifically assigned to a
    scheduler.

  - **`poll`** - Does the IO polling for the emulator. See
    [erl +IOt](erl_cmd.md#%2BIOt) for more details.

  The following `MSAcc_Thread_State`s are available. All states are exclusive,
  meaning that a thread cannot be in two states at once. So, if you add the
  numbers of all counters in a thread, you get the total runtime for that thread.

  - **`aux`** - Time spent handling auxiliary jobs.

  - **`check_io`** - Time spent checking for new I/O events.

  - **`emulator`** - Time spent executing Erlang processes.

  - **`gc`** - Time spent doing garbage collection. When extra states are enabled
    this is the time spent doing non-fullsweep garbage collections.

  - **`other`** - Time spent doing unaccounted things.

  - **`port`** - Time spent executing ports.

  - **`sleep`** - Time spent sleeping.

  More fine-grained `MSAcc_Thread_State`s can be added through configure (such as
  `./configure --with-microstate-accounting=extra`). Enabling these states causes
  performance degradation when microstate accounting is turned off and increases
  the overhead when it is turned on.

  - **`alloc`** - Time spent managing memory. Without extra states this time is
    spread out over all other states.

  - **`bif`** - Time spent in BIFs. Without extra states this time is part of the
    `emulator` state.

  - **`busy_wait`** - Time spent busy waiting. This is also the state where a
    scheduler no longer reports that it is active when using
    [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time).
    So, if you add all other states but this and sleep, and then divide that by
    all time in the thread, you should get something very similar to the
    `scheduler_wall_time` fraction. Without extra states this time is part of the
    `other` state.

  - **`ets`** - Time spent executing ETS BIFs. Without extra states this time is
    part of the `emulator` state.

  - **`gc_full`** - Time spent doing fullsweep garbage collection. Without extra
    states this time is part of the `gc` state.

  - **`nif`** - Time spent in NIFs. Without extra states this time is part of the
    `emulator` state.

  - **`send`** - Time spent sending messages (processes only). Without extra
    states this time is part of the `emulator` state.

  - **`timers`** - Time spent managing timers. Without extra states this time is
    part of the `other` state.

  The utility module `m:msacc` can be used to more easily analyse these
  statistics.

  Returns `undefined` if system flag
  [`microstate_accounting`](#system_flag_microstate_accounting) is
  turned off.

  The list of thread information is unsorted and can appear in different order
  between calls.

  > #### Note {: .info }
  >
  > The threads and states are subject to change without any prior notice.

  Available since OTP 19.0

- ```erlang
  statistics(reductions) -> {Reductions :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_reductions }

  Returns information about reductions, for example:

  ```erlang
  > statistics(reductions).
  {2046,11}
  ```

  > #### Change {: .info }
  >
  > As from ERTS 5.5 (Erlang/OTP R11B), this value does not include reductions
  > performed in current time slices of currently scheduled processes. If an exact
  > value is wanted, use
  > [`statistics(exact_reductions)`](#statistics_exact_reductions).

- ```erlang
  statistics(run_queue) -> non_neg_integer()
  ```
  {: #statistics_run_queue }

  Returns the total length of all normal and dirty CPU run queues. That is, queued
  work that is expected to be CPU bound. The information is gathered atomically.
  That is, the result is a consistent snapshot of the state, but this operation is
  much more expensive compared to
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  especially when a large amount of schedulers is used.

- ```erlang
  statistics(run_queue_lengths) -> [non_neg_integer()]
  ```
  {: #statistics_run_queue_lengths }

  Returns the same as
  [`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all)
  with the exception that no information about the dirty IO run queue is part of
  the result. That is, only run queues with work that is expected to be CPU bound
  is part of the result.

  Available since OTP 18.3

- ```erlang
  statistics(run_queue_lengths_all) -> [non_neg_integer()]
  ```
  {: #statistics_run_queue_lengths_all }

  Returns a list where each element represents the amount of processes and ports
  ready to run for each run queue. Values for normal run queues are located first
  in the resulting list. The first element corresponds to the normal run queue of
  scheduler number 1 and so on. If support for dirty schedulers exist, values for
  the dirty CPU run queue and the dirty IO run queue follow (in that order) at the
  end. The information is _not_ gathered atomically. That is, the result is not
  necessarily a consistent snapshot of the state, but instead quite efficiently
  gathered.

  > #### Note {: .info }
  >
  > Each normal scheduler has one run queue that it manages. If dirty schedulers
  > are supported, all dirty CPU schedulers share one run queue, and all dirty IO
  > schedulers share one run queue. That is, we have multiple normal run queues,
  > one dirty CPU run queue and one dirty IO run queue. Work can _not_ migrate
  > between the different types of run queues. Only work in normal run queues can
  > migrate to other normal run queues. This has to be taken into account when
  > evaluating the result.

  See also
  [`statistics(run_queue_lengths)`](#statistics_run_queue_lengths),
  [`statistics(total_run_queue_lengths_all)`](#statistics_total_run_queue_lengths_all),
  [`statistics(total_run_queue_lengths)`](#statistics_total_run_queue_lengths),
  [`statistics(active_tasks)`](#statistics_active_tasks),
  [`statistics(active_tasks_all)`](#statistics_active_tasks_all), and
  [`statistics(total_active_tasks)`](#statistics_total_active_tasks),
  [`statistics(total_active_tasks_all)`](#statistics_total_active_tasks_all).

  Available since OTP 20.0

- ```erlang
  statistics(runtime) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```

  Returns information about runtime, in milliseconds.

  This is the sum of the runtime for all threads in the Erlang runtime system and
  can therefore be greater than the wall clock time.

  > #### Warning {: .warning }
  >
  > This value might wrap due to limitations in the underlying functionality
  > provided by the operating system that is used.

  Example:

  ```erlang
  > statistics(runtime).
  {1690,1620}
  ```

- ```erlang
  statistics(scheduler_wall_time) ->
    [{Id :: pos_integer,
      ActiveTime :: non_neg_integer(),
      TotalTime :: non_neg_integer()}] |
    undefined
  ```
  {: #statistics_scheduler_wall_time }

  Returns information describing how much time
  [normal](#system_info_schedulers) and
  [dirty CPU](#system_info_dirty_cpu_schedulers) schedulers in the
  system have been busy. This value is normally a better indicator of how much
  load an Erlang node is under instead of looking at the CPU utilization provided
  by tools such as `top` or `sysstat`. This is because `scheduler_wall_time` also
  includes time where the scheduler is waiting for some other reasource (such as
  an internal mutex) to be available but does not use the CPU. In order to better
  understand what a scheduler is busy doing you can use
  [microstate accounting](#statistics_microstate_accounting).

  The definition of a busy scheduler is when it is not idle and not
  [busy waiting](erl_cmd.md#%2Bsbwt) for new work, that is:

  - Executing process code
  - Executing linked-in driver or NIF code
  - Executing BIFs, or any other runtime handling
  - Garbage collecting
  - Handling any other memory management

  Notice that a scheduler can also be busy even if the OS has scheduled out the
  scheduler thread.

  > #### Note {: .info }
  >
  > It is recommended to use the module `m:scheduler` instead of this function
  > directly as it provides an easier way to get the information that you usually
  > want.

  If [enabled](#system_flag_scheduler_wall_time) this function returns a
  list of tuples with `{SchedulerId, ActiveTime, TotalTime}`, where `SchedulerId`
  is an integer ID of the scheduler, `ActiveTime` is the duration the scheduler
  has been busy, and `TotalTime` is the total time duration since
  [`scheduler_wall_time`](#system_flag_scheduler_wall_time) activation
  for the specific scheduler. The time unit returned is undefined and can be
  subject to change between releases, OSs, and system restarts.
  `scheduler_wall_time` is only to be used to calculate relative values for
  scheduler utilization. The `ActiveTime` can never exceed `TotalTime`. The list
  of scheduler information is unsorted and can appear in different order between
  calls.

  The [disabled](#system_flag_scheduler_wall_time) this function returns
  `undefined`.

  The activation time can differ significantly between schedulers. Currently dirty
  schedulers are activated at system start while normal schedulers are activated
  some time after the `scheduler_wall_time` functionality is enabled.

  Only information about schedulers that are expected to handle CPU bound work is
  included in the return values from this function. If you also want information
  about [dirty I/O schedulers](#system_info_dirty_io_schedulers), use
  [`statistics(scheduler_wall_time_all)`](#statistics_scheduler_wall_time_all)
  instead.

  Normal schedulers will have scheduler identifiers in the range
  `1 =< SchedulerId =< `[`erlang:system_info(schedulers)`](#system_info_schedulers).
  Dirty CPU schedulers will have scheduler identifiers in the range
  `erlang:system_info(schedulers) < SchedulerId =< erlang:system_info(schedulers) + `[`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers).

  > #### Note {: .info }
  >
  > The different types of schedulers handle specific types of jobs. Every job is
  > assigned to a specific scheduler type. Jobs can migrate between different
  > schedulers of the same type, but never between schedulers of different types.
  > This fact has to be taken under consideration when evaluating the result
  > returned.

  You can use `scheduler_wall_time` to calculate scheduler utilization. First you
  take a sample of the values returned by
  `erlang:statistics(scheduler_wall_time)`.

  ```erlang
  > erlang:system_flag(scheduler_wall_time, true).
  false
  > Ts0 = lists:sort(erlang:statistics(scheduler_wall_time)), ok.
  ok
  ```

  Some time later the user takes another snapshot and calculates scheduler
  utilization per scheduler, for example:

  ```erlang
  > Ts1 = lists:sort(erlang:statistics(scheduler_wall_time)), ok.
  ok
  > lists:map(fun({{I, A0, T0}, {I, A1, T1}}) ->
          {I, (A1 - A0)/(T1 - T0)} end, lists:zip(Ts0,Ts1)).
  [{1,0.9743474730177548},
   {2,0.9744843782751444},
   {3,0.9995902361669045},
   {4,0.9738012596572161},
   {5,0.9717956667018103},
   {6,0.9739235846420741},
   {7,0.973237033077876},
   {8,0.9741297293248656}]
  ```

  Using the same snapshots to calculate a total scheduler utilization:

  ```erlang
  > {A, T} = lists:foldl(fun({{_, A0, T0}, {_, A1, T1}}, {Ai,Ti}) ->
          {Ai + (A1 - A0), Ti + (T1 - T0)} end, {0, 0}, lists:zip(Ts0,Ts1)),
    TotalSchedulerUtilization = A/T.
  0.9769136803764825
  ```

  Total scheduler utilization will equal `1.0` when all schedulers have been
  active all the time between the two measurements.

  Another (probably more) useful value is to calculate total scheduler utilization
  weighted against maximum amount of available CPU time:

  ```erlang
  > WeightedSchedulerUtilization = (TotalSchedulerUtilization
                                    * (erlang:system_info(schedulers)
                                       + erlang:system_info(dirty_cpu_schedulers)))
                                   / erlang:system_info(logical_processors_available).
  0.9769136803764825
  ```

  This weighted scheduler utilization will reach `1.0` when schedulers are active
  the same amount of time as maximum available CPU time. If more schedulers exist
  than available logical processors, this value may be greater than `1.0`.

  As of ERTS version 9.0, the Erlang runtime system will as default have more
  schedulers than logical processors. This due to the dirty schedulers.

  > #### Note {: .info }
  >
  > `scheduler_wall_time` is by default disabled. To enable it, use
  > [`erlang:system_flag(scheduler_wall_time, true)`](#system_flag_scheduler_wall_time).

  Available since OTP R15B01

- ```erlang
  statistics(scheduler_wall_time_all) ->
    [{Id :: pos_integer,
      ActiveTime :: non_neg_integer(),
      TotalTime :: non_neg_integer()}] |
    undefined
  ```
  {: #statistics_scheduler_wall_time_all }

  Equivalent to
  [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time),
  except that it also include information about all dirty I/O schedulers.

  Dirty IO schedulers will have scheduler identifiers in the range
  [`erlang:system_info(schedulers)`](#system_info_schedulers)`+`[`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers)`< SchedulerId =< erlang:system_info(schedulers) + erlang:system_info(dirty_cpu_schedulers) +`[`erlang:system_info(dirty_io_schedulers)`](#system_info_dirty_io_schedulers).

  > #### Note {: .info }
  >
  > Note that work executing on dirty I/O schedulers are expected to mainly wait
  > for I/O. That is, when you get high scheduler utilization on dirty I/O
  > schedulers, CPU utilization is _not_ expected to be high due to this work.

  Available since OTP 20.0

- ```erlang
  statistics(total_active_tasks) -> non_neg_integer()
  ```
  {: #statistics_total_active_tasks }

  Equivalent to calling
  `lists:sum(`[`statistics(active_tasks)`](#statistics_active_tasks)`)`,
  but more efficient.

  Available since OTP 18.3

- ```erlang
  statistics(total_active_tasks_all) -> non_neg_integer()
  ```
  {: #statistics_total_active_tasks_all }

  Equivalent to calling
  `lists:sum(`[`statistics(active_tasks_all)`](#statistics_active_tasks_all)`)`,
  but more efficient.

  Available since OTP 20.0

- ```erlang
  statistics(total_run_queue_lengths) -> non_neg_integer()
  ```
  {: #statistics_total_run_queue_lengths }

  Equivalent to calling
  `lists:sum(`[`statistics(run_queue_lengths)`](#statistics_run_queue_lengths)`)`,
  but more efficient.

  Available since OTP 18.3

- ```erlang
  statistics(total_run_queue_lengths_all) -> non_neg_integer()
  ```
  {: #statistics_total_run_queue_lengths_all }

  Equivalent to calling
  `lists:sum(`[`statistics(run_queue_lengths_all)`](#statistics_run_queue_lengths_all)`)`,
  but more efficient.

  Available since OTP 20.0

- ```erlang
  statistics(wall_clock) -> {Total :: non_neg_integer(), SinceLastCall :: non_neg_integer()}
  ```
  {: #statistics_wall_clock }

  Returns information about wall clock. `wall_clock` can be used in the same
  manner as `runtime`, except that real time is measured as opposed to runtime or
  CPU time.
""".
-doc #{ category => system }.
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
-doc false.
-spec subtract([term()], [term()]) -> [term()].
subtract(_,_) ->
    erlang:nif_error(undefined).

-doc "The requested scheduler bind type.".
-type scheduler_bind_type() ::
      'no_node_processor_spread' |
      'no_node_thread_spread' |
      'no_spread' |
      'processor_spread' |
      'spread' |
      'thread_spread' |
      'thread_no_node_processor_spread' |
      'unbound'.

-doc """
Sets a system flag to the given value.

The possible flags to set are:

- ```erlang
  system_flag(backtrace_depths, non_neg_integer()) -> non_neg_integer()
  ```

   Sets the maximum depth of call stack back-traces in the exit reason element of
  `'EXIT'` tuples. The flag also limits the stacktrace depth returned by
  `process_info/2` item [`current_stacktrace`](#process_info_current_stacktrace).

  Returns the old value of the flag.

- ```erlang
  system_flag(cpu_topology, cpu_topology()) -> cpu_topology()
  ```
  {: #system_flag_cpu_topology }

  > #### Warning {: .warning }
  >
  > _This argument is deprecated._ Instead of using this argument, use
  > command-line argument [`+sct`](erl_cmd.md#%2Bsct) in [erl](erl_cmd.md).
  >
  > When this argument is removed, a final CPU topology to use is determined at
  > emulator boot time.

  Sets the user-defined `CpuTopology`. The user-defined CPU topology overrides any
  automatically detected CPU topology. By passing `undefined` as `CpuTopology`,
  the system reverts to the CPU topology automatically detected. The returned
  value equals the value returned from `erlang:system_info(cpu_topology)` before
  the change was made.

  Returns the old value of the flag.

  The CPU topology is used when binding schedulers to logical processors. If
  schedulers are already bound when the CPU topology is changed, the schedulers
  are sent a request to rebind according to the new CPU topology.

  The user-defined CPU topology can also be set by passing command-line argument
  [`+sct`](erl_cmd.md#%2Bsct) to [erl](erl_cmd.md).

  For information on type `CpuTopology` and more, see
  [`erlang:system_info(cpu_topology)`](#system_info_cpu_topology) as
  well as command-line flags [`+sct`](erl_cmd.md#%2Bsct) and
  [`+sbt`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

- ```erlang
  system_flag(dirty_cpu_schedulers_online, pos_integer()) -> pos_integer()
  ```
  {: #system_flag_dirty_cpu_schedulers_online }

  Sets the number of dirty CPU schedulers online. Range is
  `1 <= DirtyCPUSchedulersOnline <= N`, where `N` is the smallest of the return
  values of `erlang:system_info(dirty_cpu_schedulers)` and
  `erlang:system_info(schedulers_online)`.

  Returns the old value of the flag.

  The number of dirty CPU schedulers online can change if the number of schedulers
  online changes. For example, if 12 schedulers and 6 dirty CPU schedulers are
  online, and [`system_flag/2`](`system_flag/2`) is used to set the number of
  schedulers online to 6, then the number of dirty CPU schedulers online is
  automatically decreased by half as well, down to 3. Similarly, the number of
  dirty CPU schedulers online increases proportionally to increases in the number
  of schedulers online.

  For more information, see
  [`erlang:system_info(dirty_cpu_schedulers)`](#system_info_dirty_cpu_schedulers)
  and
  [`erlang:system_info(dirty_cpu_schedulers_online)`](#system_info_dirty_cpu_schedulers_online).

  Available since OTP 17.0

- ```erlang
  system_flag(erts_alloc, {Alloc :: atom(), F :: atom(), V :: integer()}) ->
    ok | notsup
  ```

  Sets system flags for [`erts_alloc(3)`](erts_alloc.md). `Alloc` is the allocator
  to affect, for example `binary_alloc`. `F` is the flag to change and `V` is the
  new value.

  Only a subset of all `erts_alloc` flags can be changed at run time. This subset
  is currently only the flag [`sbct`](erts_alloc.md#M_sbct).

  Returns `ok` if the flag was set or `notsup` if not supported by `erts_alloc`.

  Available since OTP 20.2.3

- ```erlang
  system_flag(fullsweep_after, non_neg_integer()) -> non_neg_integer()
  ```

  Sets system flag `fullsweep_after`. `Number` is a non-negative integer
  indicating how many times generational garbage collections can be done without
  forcing a fullsweep collection. The value applies to new processes, while
  processes already running are not affected.

  Returns the old value of the flag.

  In low-memory systems (especially without virtual memory), setting the value to
  `0` can help to conserve memory.

  This value can also be set through (OS) environment variable
  `ERL_FULLSWEEP_AFTER`.

- ```erlang
  system_flag(microstate_accounting, true | false | reset) -> boolean()
  ```
  {: #system_flag_microstate_accounting }

  Turns on/off microstate accounting measurements. When passing reset, all
  counters are reset to 0.

  For more information see
  [`statistics(microstate_accounting)`](#statistics_microstate_accounting).

  Available since OTP 19.0

- ```erlang
  system_flag(min_heap_size, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the default minimum heap size for processes. The size is specified in
  words. The new `min_heap_size` effects only processes spawned after the change
  of `min_heap_size` has been made. `min_heap_size` can be set for individual
  processes by using `spawn_opt/4` or `process_flag/2`.

  Returns the old value of the flag.

- ```erlang
  system_flag(min_bin_vheap_size, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the default minimum binary virtual heap size for processes. The size is
  specified in words. The new `min_bin_vhheap_size` effects only processes spawned
  after the change of `min_bin_vheap_size` has been made. `min_bin_vheap_size` can
  be set for individual processes by using [`spawn_opt/2,3,4`](`spawn_opt/4`) or
  `process_flag/2`.

  Returns the old value of the flag.

  Available since OTP R13B04

- ```erlang
  system_flag(max_heap_size, max_heap_size()) -> max_heap_size()
  ```
  {: #system_flag_max_heap_size }

  Sets the default maximum heap size settings for processes. The size is specified
  in words. The new `max_heap_size` effects only processes spawned after the
  change has been made. `max_heap_size` can be set for individual processes using
  [`spawn_opt/2,3,4`](`spawn_opt/4`) or
  [`process_flag/2`](#process_flag_max_heap_size).

  Returns the old value of the flag.

  For details on how the heap grows, see
  [Sizing the heap](GarbageCollection.md#sizing-the-heap) in the ERTS internal
  documentation.

  Available since OTP 19.0

- ```erlang
  system_flag(multi_scheduling, BlockState) -> OldBlockState when
    BlockState :: block | unblock | block_normal | unblock_normal,
    OldBlockState :: blocked | disabled | enabled
  ```
  {: #system_flag_multi_scheduling }

  If multi-scheduling is enabled, more than one scheduler thread is used by the
  emulator. Multi-scheduling can be blocked in two different ways. Either all
  schedulers but one is blocked, or all _normal_ schedulers but one is blocked.
  When only normal schedulers are blocked, dirty schedulers are free to continue
  to schedule processes.

  If `BlockState =:= block`, multi-scheduling is blocked. That is, one and only
  one scheduler thread will execute. If `BlockState =:= unblock` and no one else
  blocks multi-scheduling, and this process has blocked only once,
  multi-scheduling is unblocked.

  If `BlockState =:= block_normal`, normal multi-scheduling is blocked. That is,
  only one normal scheduler thread will execute, but multiple dirty schedulers can
  execute. If `BlockState =:= unblock_normal` and no one else blocks normal
  multi-scheduling, and this process has blocked only once, normal
  multi-scheduling is unblocked.

  One process can block multi-scheduling and normal multi-scheduling multiple
  times. If a process has blocked multiple times, it must unblock exactly as many
  times as it has blocked before it has released its multi-scheduling block. If a
  process that has blocked multi-scheduling or normal multi-scheduling exits, it
  automatically releases its blocking of multi-scheduling and normal
  multi-scheduling.

  The return values are `disabled`, `blocked`, `blocked_normal`, or `enabled`. The
  returned value describes the state just after the call to
  `erlang:system_flag(multi_scheduling, BlockState)` has been made. For
  information about the return values, see
  [`erlang:system_info(multi_scheduling)`](#system_info_multi_scheduling).

  > #### Note {: .info }
  >
  > Blocking of multi-scheduling and normal multi-scheduling is normally not
  > needed. If you feel that you need to use these features, consider it a few
  > more times again. Blocking multi-scheduling is only to be used as a last
  > resort, as it is most likely a _very inefficient_ way to solve the problem.

  See also
  [`erlang:system_info(multi_scheduling)`](#system_info_multi_scheduling),
  [`erlang:system_info(normal_multi_scheduling_blockers)`](#system_info_normal_multi_scheduling_blockers),
  [`erlang:system_info(multi_scheduling_blockers)`](#system_info_multi_scheduling_blockers),
  and [`erlang:system_info(schedulers)`](#system_info_schedulers).

- ```erlang
  system_flag(outstanding_system_requests_limit, 1..134217727) -> 1..134217727
  ```
  {: #system_flag_outstanding_system_requests_limit }

  Sets a limit on the amount of outstanding requests made by a system process
  orchestrating system wide changes. Currently there are two such processes:

  - **The Code Purger** - The code purger orchestrates checking of references to
    old code before old code is removed from the system.

  - **The Literal Area Collector** - The literal area collector orchestrates
    copying of references from old literal areas before removal of such areas from
    the system.

  Each of these processes are allowed to have as many outstanding requests as this
  limit is set to. By default this limit is set to twice the amount of
  [schedulers](#system_info_schedulers) on the system. This will ensure
  that schedulers will have enough work scheduled to perform these operations as
  quickly as possible at the same time as other work will be interleaved with this
  work. Currently used limit can be checked by calling
  [`erlang:system_info(outstanding_system_requests_limit)`](#system_info_outstanding_system_requests_limit).

  This limit can also be set by passing the command line argument
  [`+zosrl <Limit>`](erl_cmd.md#%2Bzosrl) to `erl`.

  Available since OTP 24.2

- ```erlang
  system_flag(scheduler_bind_type, scheduler_bind_type() | default_bind) ->
    scheduler_bind_type()
  ```
  {: #system_flag_scheduler_bind_type }

  > #### Warning {: .warning }
  >
  > _This argument is deprecated._ Instead of using this argument, use
  > command-line argument [`+sbt`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md). When
  > this argument is removed, a final scheduler bind type to use is determined at
  > emulator boot time.

  Controls if and how schedulers are bound to logical processors.

  When `erlang:system_flag(scheduler_bind_type, How)` is called, an asynchronous
  signal is sent to all schedulers online, causing them to try to bind or unbind
  as requested.

  > #### Note {: .info }
  >
  > If a scheduler fails to bind, this is often silently ignored, as it is not
  > always possible to verify valid logical processor identifiers. If an error is
  > reported, an error event is logged. To verify that the schedulers have bound
  > as requested, call
  > [`erlang:system_info(scheduler_bindings)`](#system_info_scheduler_bindings).

  Schedulers can be bound on newer Linux, Solaris, FreeBSD, and Windows systems,
  but more systems will be supported in future releases.

  In order for the runtime system to be able to bind schedulers, the CPU topology
  must be known. If the runtime system fails to detect the CPU topology
  automatically, it can be defined. For more information on how to define the CPU
  topology, see command-line flag [`+sct`](erl_cmd.md#%2Bsct) in
  [erl](erl_cmd.md).

  The runtime system does by default _not_ bind schedulers to logical processors.

  > #### Note {: .info }
  >
  > If the Erlang runtime system is the only OS process binding threads to logical
  > processors, this improves the performance of the runtime system. However, if
  > other OS processes (for example, another Erlang runtime system) also bind
  > threads to logical processors, there can be a performance penalty instead.
  > Sometimes this performance penalty can be severe. If so, it is recommended to
  > not bind the schedulers.

  Schedulers can be bound in different ways. Argument `How` determines how
  schedulers are bound and can be any of the following:

  - **`unbound`** - Same as command-line argument [`+sbt u`](erl_cmd.md#%2Bsbt) in
    [erl](erl_cmd.md).

  - **`no_spread`** - Same as command-line argument [`+sbt ns`](erl_cmd.md#%2Bsbt)
    in [erl](erl_cmd.md).

  - **`thread_spread`** - Same as command-line argument
    [`+sbt ts`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`processor_spread`** - Same as command-line argument
    [`+sbt ps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`spread`** - Same as command-line argument [`+sbt s`](erl_cmd.md#%2Bsbt) in
    [erl](erl_cmd.md).

  - **`no_node_thread_spread`** - Same as command-line argument
    [`+sbt nnts`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`no_node_processor_spread`** - Same as command-line argument
    [`+sbt nnps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`thread_no_node_processor_spread`** - Same as command-line argument
    [`+sbt tnnps`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  - **`default_bind`** - Same as command-line argument
    [`+sbt db`](erl_cmd.md#%2Bsbt) in [erl](erl_cmd.md).

  The returned value equals `How` before flag `scheduler_bind_type` was changed.

  Failures:

  - **`notsup`** - If binding of schedulers is not supported.

  - **`badarg`** - If `How` is not one of the documented alternatives.

  - **`badarg`** - If CPU topology information is unavailable.

  The scheduler bind type can also be set by passing command-line argument
  [`+sbt`](erl_cmd.md#%2Bsbt) to [erl](erl_cmd.md).

  For more information, see
  [`erlang:system_info(scheduler_bind_type)`](#system_info_scheduler_bind_type),
  [`erlang:system_info(scheduler_bindings)`](#system_info_scheduler_bindings),
  as well as command-line flags [`+sbt`](erl_cmd.md#%2Bsbt) and
  [`+sct`](erl_cmd.md#%2Bsct) in [erl](erl_cmd.md).

- ```erlang
  system_flag(scheduler_wall_time, boolean()) -> boolean()
  ```
  {: #system_flag_scheduler_wall_time }

  Try enable or disable scheduler wall time measurements by passing `Boolean` as
  either `true` or `false`.

  For more information about how to use scheduler wall time measurements, see
  [`statistics(scheduler_wall_time)`](#statistics_scheduler_wall_time).

  Scheduler wall time measurements has a node global state. It is either enabled
  for all processes on the node or disabled for all processes. Each process has a
  logical counter initialized as zero. A call with `Boolean` as `true` will
  increase that counter one step for the calling process. A call with `false` will
  decrease it one step unless it already is zero. The node global state for
  `scheduler_wall_time` will be enabled as long as there is at least one process
  alive with a counter value larger than zero. When a process terminates, its
  counter will also disappear. To ensure `scheduler_wall_time` is kept enabled,
  the process that enabled it must therefore be kept alive.

  Returns the old value of the node global state, `true` if scheduler wall time
  measurements were enabled, `false` if it were disabled.

  Scheduler wall time measurements do consume some cpu overhead and should not be
  left turned on unless used.

  Available since OTP R15B01

- ```erlang
  system_flag(schedulers_online, pos_integer()) -> pos_integer()
  ```
  {: #system_flag_schedulers_online }

  Sets the number of schedulers online. Range is
  `1 <= SchedulersOnline <= erlang:system_info(schedulers)`.

  Returns the old value of the flag.

  If the emulator was built with support for
  [dirty schedulers](#system_flag_dirty_cpu_schedulers_online), changing
  the number of schedulers online can also change the number of dirty CPU
  schedulers online. For example, if 12 schedulers and 6 dirty CPU schedulers are
  online, and [`system_flag/2`](`system_flag/2`) is used to set the number of
  schedulers online to 6, then the number of dirty CPU schedulers online is
  automatically decreased by half as well, down to 3. Similarly, the number of
  dirty CPU schedulers online increases proportionally to increases in the number
  of schedulers online.

  For more information, see
  [`erlang:system_info(schedulers)`](#system_info_schedulers) and
  [`erlang:system_info(schedulers_online)`](#system_info_schedulers_online).

- ```erlang
  system_flag(system_logger, logger | undefined | pid()) -> logger | undefined | pid()
  ```

  Sets the process that will receive the logging messages generated by ERTS. If
  set to `undefined`, all logging messages generated by ERTS will be dropped. The
  messages will be in the format:

  ```erlang
  {log,Level,Format,ArgList,Metadata} where

  Level = atom(),
  Format = string(),
  ArgList = list(term()),
  Metadata = #{ pid => pid(),
     group_leader => pid(),
     time := logger:timestamp(),
     error_logger := #{ emulator := true, tag := atom() }
  ```

  If the `system_logger` process dies, this flag will be reset to `logger`.

  The default is the process named `logger`.

  Returns the old value of the flag.

  > #### Note {: .info }
  >
  > This function is designed to be used by the KERNEL `m:logger`. Be careful if
  > you change it to something else as log messages may be lost. If you want to
  > intercept emulator log messages, do it by adding a specialized handler to the
  > KERNEL logger.

  Available since OTP 21.2

- ```erlang
  system_flag(trace_control_word, non_neg_integer()) -> non_neg_integer()
  ```

  Sets the value of the node trace control word to `TCW`, which is to be an
  unsigned integer. For more information, see function
  [`set_tcw`](match_spec.md#set_tcw) in section "Match Specifications in Erlang"
  in the User's Guide.

  Returns the old value of the flag.

- ```erlang
  system_flag(time_offset, finalize) -> preliminary | final | volatile
  ```
  {: #system_flag_time_offset }

  Finalizes the [time offset](`time_offset/0`) when
  [single time warp mode](time_correction.md#single-time-warp-mode) is used. If
  another time warp mode is used, the time offset state is left unchanged.

  Returns the old state identifier, that is:

  - If `preliminary` is returned, finalization was performed and the time offset
    is now final.
  - If `final` is returned, the time offset was already in the final state. This
    either because another `erlang:system_flag(time_offset, finalize)` call or
    because [no time warp mode](time_correction.md#no-time-warp-mode) is used.
  - If `volatile` is returned, the time offset cannot be finalized because
    [multi-time warp mode](time_correction.md#multi-time-warp-mode) is used.

  Available since OTP 18.0
""".
-doc #{ category => system }.
-spec system_flag(backtrace_depth, Depth) -> OldDepth when
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

-doc """
Returns a binary data object that is the result of encoding `Term` according to
the [Erlang external term format.](erl_ext_dist.md)

This can be used for various purposes, for example, writing a term to a file in
an efficient way, or sending an Erlang term to some type of communications
channel not supported by distributed Erlang.

```erlang
> Bin = term_to_binary(hello).
<<131,100,0,5,104,101,108,108,111>>
> hello = binary_to_term(Bin).
hello
```

See also `binary_to_term/1`.

> #### Note {: .info }
>
> There is no guarantee that this function will return the same encoded
> representation for the same term.
""".
-doc #{ category => terms }.
-spec term_to_binary(Term) -> ext_binary() when
      Term :: term().
term_to_binary(_Term) ->
    erlang:nif_error(undefined).

-doc """
Returns a binary data object that is the result of encoding `Term` according to
the Erlang external term format.

Currently supported options:

- **`compressed`** - Compress the external term format. The compressed format is
  automatically recognized by [`binary_to_term/1`](`binary_to_term/1`) as from
  Erlang/OTP R7B.

- **`{compressed, Level}`** - Compress the external term format to a given
  level. The compression level is specified by `Level` which is an integer in
  the range 0..9, where:

  - **`0`** - No compression is done (it is the same as giving no `compressed`
    option).

  - **`1`** - Takes least time but may not compress as well as the higher
    levels.

  - **`6`** - Default level when option `compressed` is provided.

  - **`9`** - Takes most time and tries to produce a smaller result. Notice
    "tries" in the preceding sentence; depending on the input term, level 9
    compression either does or does not produce a smaller result than level 1
    compression.

- **`{minor_version, Version}`**(Since R11B-4)  
  The option can be used to control some encoding details. Valid values for
  `Version` are:

  - **`0`** - Floats are encoded using a textual representation.

    Atoms that can be represented by a latin1 string are encoded using latin1
    while only atoms that cannot be represented by latin1 are encoded using
    utf8.

  - **`1`** - Floats are encoded in a more space-efficient and exact way (namely
    in the 64-bit IEEE format, rather than converted to a textual
    representation). As from Erlang/OTP R11B-4,
    [`binary_to_term/1`](`binary_to_term/1`) can decode this representation.

    Atoms that can be represented by a latin1 string are encoded using latin1
    while only atoms that cannot be represented by latin1 are encoded using
    utf8.

  - **`2`** - This is as of Erlang/OTP 26.0 the _default_. Atoms are
    unconditionally encoded using utf8. Erlang/OTP systems as of R16B can decode
    this representation.

- **`deterministic`**(Since OTP 24.1)  
  This option can be used to ensure that, within the same major release of
  Erlang/OTP, the same encoded representation is returned for the same term.
  There is still no guarantee that the encoded representation remains the same
  between major releases of Erlang/OTP.

  This option cannot be combined with the `local` option.

- **`local`[](){: #term_to_binary_local } **(Since OTP 26.0)  
   This option will cause encoding of `Term` to an alternative local version of the
  external term format which when decoded by the same runtime system instance will
  produce a term identical to the encoded term even when the node name and/or [creation](#system_info_creation)
  of the current runtime system instance have changed between encoding and decoding.
  When encoding without the `local` option, local identifiers such as [pids](`t:pid/0`),
  [ports](`t:port/0`) and [references](`t:reference/0`) will not be the same if node
  name and/or creation of the current runtime system instance changed between encoding
  and decoding. This since such identifiers refer to a specific node by node name
  and creation.

  Node name and creation of a runtime system instance change when the
  distribution is started or stopped. The distribution is started when the
  runtime system is started using the [`-name`](erl_cmd.md#name) or
  [`-sname`](erl_cmd.md#sname) command line arguments. Note that the actual
  start of the distribution happens after other code in the startup phase has
  begun executing. The distribution can also be started by calling
  `net_kernel:start/2` and stopped by calling
  [`net_kernel:stop/1`](`net_kernel:stop/0`) if it has not been started via the
  command line.

  The decoding of a term encoded with the `local` option, using for example
  [`binary_to_term()`](`term_to_binary/1`), will try to verify that the term
  actually was encoded by the same runtime system instance, and will in the vast
  majority of cases fail if the encoding was performed by another runtime system
  instance. You should however _not_ trust that this verification will work in
  all cases. You _should_ make sure to _only_ decode terms encoded with the
  `local` option on the same Erlang runtime system instance as the one that
  encoded the terms.

  Since it is only the runtime system that encoded a term using the `local`
  option that can decode it, the local encoding is typically pieced together
  with something else to produce a reply to where the `local` encoding
  originates from. If a term encoded using the `local` option is stripped of its
  leading version number, it can be added as part of a larger term (for example
  as an element in a tuple) when encoding on the external term format using, for
  example, [ei](`e:erl_interface:ei.md`). In the `ei` case, you would strip it
  of the version number using
  [`ei_decode_version()`](`e:erl_interface:ei.md#ei_decode_version`) and then
  add the remaining local encoding to what you are encoding using for example
  [`ei_x_append_buf()`](`e:erl_interface:ei.md#ei_x_append_buf`).

  A good example of when you want to use the `local` option, is when you want to
  make a request from a process to a port [driver](erl_driver.md) and utilize
  the
  [selective receive optimization](`e:system:eff_guide_processes.md#receiving-messages`)
  when receiving the reply. In this scenario you want to create a reference,
  serialize the reference on the external term format using the `local` option,
  pass this to the driver in the request, and then wait for the reply message in
  a selective receive matching on the reference. The driver should send the
  reply using either
  [`erl_drv_output_term()`](erl_driver.md#erl_drv_output_term) or
  [`erl_drv_send_term()`](erl_driver.md#erl_drv_send_term) using the term type
  [`ERL_DRV_EXT2TERM`](erl_driver.md#ERL_DRV_EXT2TERM) for the, in the request,
  previously received reference on the external term format. Note that you
  should not strip the leading version number from the local encoding when using
  the term type `ERL_DRV_EXT2TERM` of this functionality. If you in this example
  do not encode the reference using the `local` option, and the distribution is
  started or stopped while the request is ongoing, the process that made the
  request will hang indefinitely since the reference in the reply message will
  never match.

  This option cannot be combined with the `deterministic` option.

  For more information see the [`LOCAL_EXT`](erl_ext_dist.md#local_ext) tag in
  the documentation of the external term format.

See also `binary_to_term/1`.
""".
-doc #{ category => terms }.
-spec term_to_binary(Term, Options) -> ext_binary() when
      Term :: term(),
      Options :: [compressed |
         {compressed, Level :: 0..9} |
         deterministic |
         {minor_version, Version :: 0..2} |
         local ].
term_to_binary(_Term, _Options) ->
    erlang:nif_error(undefined).

-doc """
Returns the encoding of `Term` according to the Erlang external term format as
`t:ext_iovec/0`.

This function produce the same encoding as `term_to_binary/1`, but with another
return type. The call
[`iolist_to_binary(term_to_iovec(Term))`](`iolist_to_binary/1`) will produce
exactly the same result as the call
[`term_to_binary(Term)`](`term_to_binary/1`).

`term_to_iovec()` is a pure optimization of the functionality `term_to_binary()`
provide. `term_to_iovec()` can for example refer directly to off heap binaries
instead of copying the binary data into the result.

See also `term_to_binary/1`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => terms }.
-spec term_to_iovec(Term) -> ext_iovec() when
      Term :: term().
term_to_iovec(_Term) ->
    erlang:nif_error(undefined).

-doc """
Returns the encoding of `Term` according to the Erlang external term format as
`t:ext_iovec/0`.

This function produce the same encoding as `term_to_binary/2`, but with another
return type. The call
[`iolist_to_binary(term_to_iovec(Term, Opts))`](`iolist_to_binary/1`) will
produce exactly the same result as
[`term_to_binary(Term, Opts)`](`term_to_binary/2`).

Currently recognised options are all options recognised by `term_to_binary/2`.

`term_to_iovec()` is a pure optimization of the functionality `term_to_binary()`
provide. `term_to_iovec()` can for example refer directly to off heap binaries
instead of copying the binary data into the result.

See also `term_to_binary/2`.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => terms }.
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
-doc """
Returns the tail of `List`, that is, the list minus the first element

It works with improper lists.

Examples:

```erlang
> tl([geesties, guilies, beasties]).
[guilies, beasties]
```

```erlang
> tl([geesties]).
[]
```

```erlang
> tl([geesties, guilies, beasties | improper_end]).
[guilies, beasties | improper_end]
```

```erlang
> tl([geesties | improper_end]).
improper_end
```

Failure: `badarg` if `List` is an empty list `[]`.
""".
-doc #{ category => terms }.
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

-doc """
Equivalent to [`erlang:trace_pattern(Event, MatchSpec, [])`](`trace_pattern/3`),
retained for backward compatibility.
""".
-doc #{ category => trace }.
-spec trace_pattern(MFA, MatchSpec) -> non_neg_integer() when
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

-doc """
Set trace pattern for call, send and receive tracing on the static legacy trace
session.

  > #### Change {: .info }
  >
  > This function is superseded by `trace:function/4`, `trace:send/3` and
  > `trace:recv/3` that operate on dynamic trace sessions.

Argument `FlagList` can contain two additional options for call tracing:

- **`{meta, Pid} | {meta, TracerModule, TracerState}`** - Turns on or off
  meta-tracing for all types of function calls. Trace messages are sent to the
  tracer whenever any of the specified functions are called. If no tracer is
  specified, `self/0` is used as a default tracer process.

For further documentation see `trace:function/4` , `trace:send/3` and
`trace:recv/3`.
""".
-doc #{ category => trace }.
-spec trace_pattern(send, MatchSpec, []) -> non_neg_integer() when
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
-doc """
Returns a list corresponding to `Tuple`. `Tuple` can contain any Erlang terms.
Example:

```erlang
> tuple_to_list({share, {'Ericsson_B', 163}}).
[share,{'Ericsson_B',163}]
```
""".
-doc #{ category => terms }.
-spec tuple_to_list(Tuple) -> [term()] when
      Tuple :: tuple().
tuple_to_list(_Tuple) ->
    erlang:nif_error(undefined).

-doc "The current cpu topology.

`node` refers to Non-Uniform Memory Access (NUMA) nodes. `thread` refers
to hardware threads (for example, Intel hyper-threads).

A level in term `CpuTopology` can be omitted if only one entry exists and
`InfoList` is empty.

`thread` can only be a sublevel to `core`. `core` can be a sublevel to
`processor` or `node`. `processor` can be on the top level or a sublevel to
`node`. `node` can be on the top level or a sublevel to `processor`. That
is, NUMA nodes can be processor internal or processor external. A CPU
topology can consist of a mix of processor internal and external NUMA nodes,
as long as each logical CPU belongs to _one_ NUMA node. Cache hierarchy is
not part of the `CpuTopology` type, but will be in a future release. Other
things can also make it into the CPU topology in a future release. So, expect
the `CpuTopology` type to change.
".
-type cpu_topology() ::
        [LevelEntry :: level_entry()] | undefined.
-doc "".
-type level_entry() ::
        {LevelTag :: level_tag(), SubLevel :: sub_level()}
      | {LevelTag :: level_tag(),
         InfoList :: info_list(),
         SubLevel :: sub_level()}.
-doc "".
-type level_tag() :: core | node | processor | thread.
-doc "".
-type sub_level() :: [LevelEntry :: level_entry()]
                   | (LogicalCpuId :: {logical, non_neg_integer()}).
-doc "".
-type info_list() :: [].

-doc "A list with the system wide garbage collection defaults.".
-type garbage_collection_defaults() :: [{max_heap_size, non_neg_integer()} |
                                        {min_bin_vheap_size, non_neg_integer()} |
                                        {min_heap_size, non_neg_integer()} |
                                        {fullsweep_after, non_neg_integer()}].

%% Note: changing the ordering number of a clause will change the docs!
%% Shadowed by erl_bif_types: erlang:system_info/1
-doc #{ category => system }.
-spec system_info
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
         (cpu_topology) ->  CpuTopology when
      CpuTopology :: cpu_topology();
         ({cpu_topology, defined | detected | used}) -> CpuTopology when
      CpuTopology :: cpu_topology();
         (cpu_quota) -> pos_integer() | unknown;
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
         (garbage_collection) -> garbage_collection_defaults();
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
         (scheduler_bind_type) -> scheduler_bind_type();
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
         (halt_flush_timeout) -> non_neg_integer() | infinity.
-doc {file,"../../doc/src/erlang_system_info.md"}.
system_info(_Item) ->
    erlang:nif_error(undefined).

-doc """
Converts Universal Time Coordinated (UTC) date and time to local date and time
in the form `{{Year, Month, Day}, {Hour, Minute, Second}}` if supported by the
underlying OS. Otherwise no conversion is done, and `Universaltime` is returned.

For example:

```erlang
> erlang:universaltime_to_localtime({{1996,11,6},{14,18,43}}).
{{1996,11,7},{15,18,43}}
```

Failure: `badarg` if `Universaltime` denotes an invalid date and time.
""".
-doc #{ category => time }.
-spec universaltime_to_localtime(Universaltime) ->  Localtime when
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
-doc """
Calls a fun, passing the elements in `Args` as arguments.

If the number of elements in the arguments are known at compile time, the call
is better written as `Fun(Arg1, Arg2, ... ArgN)`.

> #### Warning {: .warning }
>
> Earlier, `Fun` could also be specified as `{Module, Function}`, equivalent to
> [`apply(Module, Function, Args)`](`apply/3`). _This use is deprecated and will
> stop working in a future release._
""".
-doc #{ category => processes }.
-spec apply(Fun, Args) -> term() when
      Fun :: function(),
      Args :: [term()].
apply(Fun, Args) ->
    erlang:apply(Fun, Args).

%% Shadowed by erl_bif_types: erlang:apply/3
-doc """
Returns the result of applying `Function` in `Module` to `Args`. The applied
function must be exported from `Module`. The arity of the function is the length
of `Args`.

For example:

```erlang
> apply(lists, reverse, [[a, b, c]]).
[c,b,a]
> apply(erlang, atom_to_list, ['Erlang']).
"Erlang"
```

If the number of arguments are known at compile time, the call is better written
as `Module:Function(Arg1, Arg2, ..., ArgN)`.

Failure: `error_handler:undefined_function/3` is called if the applied function
is not exported. The error handler can be redefined (see `process_flag/2`). If
`error_handler` is undefined, or if the user has redefined the default
`error_handler` so the replacement module is undefined, an error with reason
`undef` is generated.
""".
-doc #{ category => processes }.
-spec apply(Module, Function, Args) -> term() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
apply(Mod, Name, Args) ->
    erlang:apply(Mod, Name, Args).

%% Spawns with a fun

-doc """
Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]`. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
-spec spawn(Fun) -> pid() when
      Fun :: function().
spawn(F) when erlang:is_function(F) ->
    erlang:spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn(erlang, apply, [MF, []]);
spawn(F) ->
    badarg_with_info([F]).

-doc """
Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]` on `Node`. If `Node` does not exist, a useless pid
is returned. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
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

-doc """
Returns the process identifier of a new process started by the application of
`Fun` to the empty list `[]`. A link is created between the calling process and
the new process, atomically. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when erlang:is_function(F) ->
    erlang:spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    badarg_with_info([F]).

-doc """
Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]` on `Node`. A link is created between the calling
process and the new process, atomically. If `Node` does not exist, a useless pid
is returned and an exit signal with reason `noconnection` is sent to the calling
process. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
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

-doc """
Returns the process identifier of a new process, started by the application of
`Fun` to the empty list `[]`, and a reference for a monitor created to the new
process. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
-spec spawn_monitor(Fun) -> {pid(), reference()} when
      Fun :: function().
spawn_monitor(F) when erlang:is_function(F, 0) ->
    erlang:spawn_opt(erlang,apply,[F,[]],[monitor]);
spawn_monitor(F) ->
    badarg_with_info([F]).

-doc """
Returns the process identifier of a new process, started by the application of
`Fun` to the empty list `[]` on the node `Node`, and a reference for a monitor
created to the new process. Otherwise works like `spawn/3`.

If the node identified by `Node` does not support distributed `spawn_monitor()`,
the call will fail with a `notsup` exception.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
A new process is started by the application of `Module:Function` to `Args`. The
process is monitored at the same time. Returns the process identifier and a
reference for the monitor. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
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


-doc """
Process max heap size configuration. For more info see
[`process_flag(max_heap_size, MaxHeapSize)`](#process_flag_max_heap_size)
""".
-type max_heap_size() ::
        Size :: non_neg_integer()
        %% TODO change size => to := when -type maps support is finalized
      | #{ size => non_neg_integer(),
           kill => boolean(),
           error_logger => boolean(),
           include_shared_binaries => boolean() }.

-doc "Options for [`spawn_opt()`](`spawn_opt/4`).".
-type spawn_opt_option() ::
	link
      | {link, LinkOpts :: [link_option()]}
      | monitor
      | {monitor, MonitorOpts :: [monitor_option()]}
      | {priority, Level :: priority_level()}
      | {fullsweep_after, Number :: non_neg_integer()}
      | {min_heap_size, Size :: non_neg_integer()}
      | {min_bin_vheap_size, VSize :: non_neg_integer()}
      | {max_heap_size, Size :: max_heap_size()}
      | {message_queue_data, MQD :: message_queue_data()}
      | {async_dist, Enabled :: boolean()}.

-doc """
Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]`. Otherwise works like `spawn_opt/4`.

If option `monitor` is specified, the newly created process is monitored, and
both the pid and reference for the monitor are returned.
""".
-doc #{ category => processes }.
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

-doc """
Returns the process identifier (pid) of a new process started by the application
of `Fun` to the empty list `[]` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn_opt/4`.

Valid options depends on what options are supported by the node identified by
`Node`. A description of valid `Option`s for the local node of current OTP
version can be found in the documentation of `spawn_opt/4`.
""".
-doc #{ category => processes }.
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

-doc """
Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
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

-doc """
Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. A link is created between the calling
process and the new process, atomically. If `Node` does not exist, a useless pid
is returned and an exit signal with reason `noconnection` is sent to the calling
process. Otherwise works like `spawn/3`.
""".
-doc #{ category => processes }.
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

-doc """
A new process is started by the application of `Module:Function` to `Args` on
the node `Node`. The process is monitored at the same time. Returns the process
identifier and a reference for the monitor. Otherwise works like `spawn/3`.

If the node identified by `Node` does not support distributed `spawn_monitor()`,
the call will fail with a `notsup` exception.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
Works as `spawn/3`, except that an extra option list is specified when creating
the process.

If option `monitor` is specified, the newly created process is monitored, and
both the pid and reference for the monitor are returned.

Options:

- **`link`** - Sets a link to the parent process (like `spawn_link/3` does).

- **`monitor`** - Monitors the new process (like
  [`monitor(process, Pid)`](`monitor/2`) does). A `{Pid, MonitorRef}` tuple will
  be returned instead of just a `Pid`.

- **`{monitor, MonitorOpts}`** - Monitors the new process with options (like
  [`monitor(process, Pid, MonitorOpts)`](`monitor/3`) does). A
  `{Pid, MonitorRef}` tuple will be returned instead of just a `Pid`.

- **`{priority, Level}`** - Sets the priority of the new process. Equivalent to
  executing [`process_flag(priority, Level)`](#process_flag_priority)
  in the start function of the new process, except that the priority is set
  before the process is selected for execution for the first time. For more
  information on priorities, see
  [`process_flag(priority, Level)`](#process_flag_priority).

- **`{fullsweep_after, Number}`** - Useful only for performance tuning. Do not
  use this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  The Erlang runtime system uses a generational garbage collection scheme, using
  an "old heap" for data that has survived at least one garbage collection. When
  there is no more room on the old heap, a fullsweep garbage collection is done.

  Option `fullsweep_after` makes it possible to specify the maximum number of
  generational collections before forcing a fullsweep, even if there is room on
  the old heap. Setting the number to zero disables the general collection
  algorithm, that is, all live data is copied at every garbage collection.

  A few cases when it can be useful to change `fullsweep_after`:

  - If binaries that are no longer used are to be thrown away as soon as
    possible. (Set `Number` to zero.)
  - A process that mostly have short-lived data is fullsweeped seldom or never,
    that is, the old heap contains mostly garbage. To ensure a fullsweep
    occasionally, set `Number` to a suitable value, such as 10 or 20.
  - In embedded systems with a limited amount of RAM and no virtual memory, you
    might want to preserve memory by setting `Number` to zero. (The value can be
    set globally, see [`erlang:system_flag/2`](`system_flag/2`).)

- **`{min_heap_size, Size}`** - Useful only for performance tuning. Do not use
  this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  Gives a minimum heap size, in words. Setting this value higher than the system
  default can speed up some processes because less garbage collection is done.
  However, setting a too high value can waste memory and slow down the system
  because of worse data locality. Therefore, use this option only for
  fine-tuning an application and to measure the execution time with various
  `Size` values.

- **`{min_bin_vheap_size, VSize}`** - Useful only for performance tuning. Do not
  use this option unless you know that there is problem with execution times or
  memory consumption, and ensure that the option improves matters.

  Gives a minimum binary virtual heap size, in words. Setting this value higher
  than the system default can speed up some processes because less garbage
  collection is done. However, setting a too high value can waste memory.
  Therefore, use this option only for fine-tuning an application and to measure
  the execution time with various `VSize` values.

- **`{max_heap_size, Size}`** - Sets the `max_heap_size` process flag. The
  default `max_heap_size` is determined by command-line argument
  [`+hmax`](erl_cmd.md#%2Bhmax) in [erl](erl_cmd.md). For more information, see
  the documentation of
  [`process_flag(max_heap_size, Size)`](#process_flag_max_heap_size).

- **`{message_queue_data, MQD}`** - Sets the value of the `message_queue_data`
  process flag. `MQD` can be either `off_heap` or `on_heap`. The default value
  of the `message_queue_data` process flag is determined by the command-line
  argument [`+hmqd`](erl_cmd.md#%2Bhmqd) in [erl](erl_cmd.md). For more
  information, see the documentation of
  [`process_flag(message_queue_data, MQD)`](#process_flag_message_queue_data).

- **`{async_dist, Enabled}`{: #spawn_opt_async_dist }** - Sets the
  [`async_dist`](#process_flag_async_dist) process flag of the spawned process.
  This option will override the default value set by the command line argument
  [`+pad <boolean>`](erl_cmd.md#%2Bpad).

  Since: OTP 25.3
""".
-doc #{ category => processes }.
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


-doc """
Returns the process identifier (pid) of a new process started by the application
of `Module:Function` to `Args` on `Node`. If `Node` does not exist, a useless
pid is returned. Otherwise works like `spawn_opt/4`.

Valid options depends on what options are supported by the node identified by
`Node`. A description of valid `Option`s for the local node of current OTP
version can be found in the documentation of `spawn_opt/4`.
""".
-doc #{ category => processes }.
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

-doc """
Equivalent to the call [`spawn_request(node(),Fun,[])`](`spawn_request/3`). That
is, a spawn request on the local node with no options.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
spawn_request(FunOrNode, OptionsOrFun)

Equivalent to [`spawn_request(node(),Fun,Options)`](`spawn_request/3`) or
[`spawn_request(Node,Fun,[])`](`spawn_request/3`) depending on the arguments.

That is either:
- a spawn request on the local node.
- a spawn request with no options.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
spawn_request(NodeOrModule, FunOrFunction, OptionsOrArgs)

Equivalent to
[`spawn_request(Node,erlang,apply,[Fun,[]],Options)`](`spawn_request/5`) or
[`spawn_request(node(),Module,Function,Args,[])`](`spawn_request/5`) depending
on the arguments.

That is either:

- a spawn request using the fun `Fun` of arity zero as entry point
- a spawn request on the local node with no options.

This function will fail with a `badarg` exception if:

- `Node` is not an atom.
- `Fun` is not a fun of arity zero.
- `Options` is not a proper list of terms.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
spawn_request(NodeOrModule, ModuleOrFunction, FunctionOrArgs, ArgsOrOptions)

Equivalent to
[`spawn_request(Node,Module,Function,Args,[])`](`spawn_request/5`) or
[`spawn_request(node(),Module,Function,Args,Options)`](`spawn_request/5`)
depending on the arguments.

That is either:
- a spawn request with no options.
- a spawn request on the local node.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
Asynchronously send a spawn request. Returns a request identifier `ReqId`.

[](){: #spawn_request_success_message }

If the spawn operation succeeds, a new process is created on the node identified
by `Node`. When a spawn operation succeeds, the caller will by default be sent a
message of the form `{ReplyTag, ReqId, ok, Pid}` where `Pid` is the process
identifier of the newly created process. Such a message is referred to as a
_success message_ below in the text. `ReplyTag` is by default the atom
`spawn_reply` unless modified by the `{reply_tag, ReplyTag}` option. The new
process is started by the application of `Module:Function` to `Args`.

[](){: #spawn_request_error_message }

The spawn operation fails either if creation of a new process failed or if the
spawn operation was interrupted by a connection failure. When a spawn operation
fails, the caller will by default be sent a message on the form
`{ReplyTag, ReqId, error, Reason}` where `Reason` is the error reason. Such a
message is referred to as an _error message_ below in the text. Currently the
following spawn error `Reason`s are defined, but other reasons can appear at any
time without prior notice:

- **`badopt`** - An invalid `Option` was passed as argument. Note that different
  runtime systems may support different options.

- **`notsup`** - The node identified by `Node` does not support spawn operations
  issued by `spawn_request()`.

- **`noconnection`** - Failure to set up a connection to the node identified by
  `Node` or the connection to that node was lost during the spawn operation. In
  the case the connection was lost, a process may or may not have been created.

- **`system_limit`** - Could not create a new process due to that some system
  limit was reached. Typically the process table was full.

Valid `Option`s:

- **`monitor`** - In the absence of spawn operation failures, atomically sets up
  a monitor to the newly created process. That is, as if the calling process had
  called [`monitor(process, Pid)`](`monitor/2`) where `Pid` is the process
  identifier of the newly created process. The `ReqId` returned by
  `spawn_request()` is also used as monitor reference as if it was returned from
  [`monitor(process, Pid)`](`monitor/2`).

  The monitor will not be activated for the calling process until the spawn
  operation has succeeded. The monitor can not be [demonitored](`demonitor/1`)
  before the operation has succeeded. A `'DOWN'` message for the corresponding
  monitor is guaranteed not to be delivered before a
  [_success message_](#spawn_request_success_message) that corresponds
  to the spawn operation. If the spawn operation fails, no `'DOWN'` message will
  be delivered.

  If the connection between the nodes involved in the spawn operation is lost
  during the spawn operation, the spawn operation will fail with an error reason
  of `noconnection`. A new process may or may not have been created.

- **`{monitor, MonitorOpts}`** - In the absence of spawn operation failures,
  atomically sets up a monitor to the newly created process. That is, as if the
  calling process had called [`monitor(process, Pid, MonitorOpts)`](`monitor/2`)
  where `Pid` is the process identifier of the newly created process. See the
  `monitor` option above for more information.

  Note that the monitor will not be activated for the calling process until the
  spawn operation has succeeded. For example, in the case that an alias is
  created using the monitor option, the alias will not be active until the
  monitor is activated.

- **`link`** - In absence of spawn operation failures, atomically sets up a link
  between the calling process and the newly created process. That is, as if the
  calling process had called [`link(Pid)`](`link/1`) where `Pid` is the process
  identifier of the newly created process.

  The link will not be activated for the calling process until the spawn
  operation has succeeded. The link can not be removed before the operation has
  succeeded. An exit signal due to the link is guaranteed not to be delivered
  before a [_success message_](#spawn_request_success_message) that
  corresponds to the spawn operation. If the spawn operation fails, no exit
  signal due to the link will be delivered to the caller of `spawn_request()`.

  If the connection between the nodes involved in the spawn operation is lost
  during the spawn operation, the spawn operation will fail with an error reason
  of `noconnection`. A new process may or may not have been created. If it has
  been created, it will be delivered an exit signal with an exit reason of
  `noconnection`.

- **`{reply, Reply}`** - Valid `Reply` values:

  - **`yes`** - A spawn reply message will be sent to the caller regardless of
    whether the operation succeeds or not. If the call to `spawn_request()`
    returns without raising an exception and the `reply` option is set to `yes`,
    the caller is guaranteed to be delivered either a
    [_success message_](#spawn_request_success_message) or an
    [_error message_](#spawn_request_error_message). The `reply`
    option is by default set to `yes`.

  - **`no`** - No spawn reply message will be sent to the caller when the spawn
    operation completes. This regardless of whether the operation succeeds or
    not.

  - **`error_only`** - No spawn reply message will be sent to the caller if the
    spawn operation succeeds, but an
    [_error message_](#spawn_request_error_message) will be sent to
    the caller if the operation fails.

  - **`success_only`** - No spawn reply message will be sent to the caller if
    the spawn operation fails, but a
    [_success message_](#spawn_request_success_message) will be sent
    to the caller if the operation succeeds.

- **`{reply_tag, ReplyTag}`** - Sets the reply tag to `ReplyTag` in the reply
  message. That is, in the [_success_](#spawn_request_success_message)
  or [_error_](#spawn_request_error_message) message that is sent to
  the caller due to the spawn operation. The default reply tag is the atom
  `spawn_reply`.

- **`OtherOption`** - Other valid options depends on what options are supported
  by the node identified by `Node`. A description of other valid `Option`s for
  the local node of current OTP version can be found in the documentation of
  `spawn_opt/4`.

If a spawn reply message is delivered, it is guaranteed to be delivered before
any other signals from the newly spawned process are delivered to the process
issuing the spawn request.

This function will fail with a `badarg` exception if:

- `Node` is not an atom.
- `Module` is not an atom.
- `Function` is not an atom.
- `Args` is not a proper list of terms.
- `Options` is not a proper list of terms.

Note that not all individual `Option`s are checked when the spawn request is
sent. Some `Option`s can only be checked on reception of the request. Therefore
an invalid option does _not_ cause a `badarg` exception, but will cause the
spawn operation to fail with an error reason of `badopt`.

A spawn request can be abandoned by calling `spawn_request_abandon/1`.

> #### Note {: .info }
>
> For some important information about distributed signals, see the
> [_Blocking Signaling Over Distribution_](`e:system:ref_man_processes.md#blocking-signaling-over-distribution`)
> section in the _Processes_ chapter of the _Erlang Reference Manual_.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
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

-doc """
Abandon a previously issued spawn request. `ReqId` corresponds to a request
identifier previously returned by [`spawn_request()`](`spawn_request/5`) in a
call from current process. That is, only the process that has made the request
can abandon the request.

A spawn request can only be successfully abandoned until the spawn request has
completed. When a spawn request has been successfully abandoned, the caller will
not be effected by future direct effects of the spawn request itself. For
example, it will not receive a spawn reply message. The request is however not
withdrawn, so a new process may or may not be created due to the request. If a
new process is created after the spawn request was abandoned, no monitors nor
links will be set up to the caller of
[`spawn_request_abandon/1`](`spawn_request_abandon/1`) due to the spawn request.
If the spawn request included the `link` option, the process created due to this
request will be sent an exit signal from its parent with the exit reason
`abandoned` when it is detected that the spawn operation has succeeded.

> #### Note {: .info }
>
> A process created due to a spawn request that has been abandoned may
> communicate with its parent as any other process. It is _only_ the direct
> effects on the parent of the actual spawn request, that will be canceled by
> abandoning a spawn request.

Return values:

- **`true`** - The spawn request was successfully abandoned.

- **`false`** - No spawn request was abandoned. The `ReqId` request identifier
  did not correspond to an outstanding spawn request issued by the calling
  process. The reason for this is either:

  - `ReqId` corresponds to a spawn request previoulsy made by the calling
    process. The spawn operation has completed and a spawn reply has already
    been delivered to the calling process unless the spawn reply was disabled in
    the request.
  - `ReqId` does not correspond to a spawn request that has been made by the
    calling process.

This function fail with a `badarg` exception if `ReqId` is not a reference.
""".
-doc(#{since => <<"OTP 23.0">>}).
-doc #{ category => processes }.
-spec spawn_request_abandon(ReqId :: reference()) -> boolean().

spawn_request_abandon(_ReqId) ->
    erlang:nif_error(undefined).

-doc """
Tries to give other processes with the same or higher priority (if any) a chance
to execute before returning. There is no guarantee that any other process runs
between the invocation and return of `erlang:yield/0`.

See the documentation for
[`receive-after` expressions](`e:system:expressions.md#receive`) for how to make
the current process sleep for a specific number of milliseconds.

> #### Warning {: .warning }
>
> There is seldom or never any need to use this BIF. Using this BIF without a
> thorough grasp of how the scheduler works can cause performance degradation.
> The current implementation of this function puts the current process last in
> the current scheduler's queue for processes of the same priority as the
> current process.
""".
-doc #{ category => processes }.
-spec yield() -> 'true'.
yield() ->
    % This is not an infinite loop because erlang:yield() is
    % translated to an instruction by the loader
    erlang:yield().

-doc """
Returns a list of all nodes connected to this node through normal connections
(that is, [hidden nodes](`e:system:distributed.md#hidden-nodes`) are not
listed). Same as [nodes(visible)](#nodes_visible).
""".
-doc #{ category => distribution }.
-spec nodes() -> Nodes when
      Nodes :: [node()].
nodes() ->
    erlang:nif_error(undefined).

-doc """
Returns a list of nodes according to the argument specified. The returned
result, when the argument is a list, is the list of nodes satisfying the
disjunction(s) of the list elements.

`NodeType`s:

- **`visible`{: #nodes_visible }** - Nodes connected to this node through normal
  connections.

- **`hidden`** - Nodes connected to this node through hidden connections.

- **`connected`** - All nodes connected to this node.

- **`this`** - This node.

- **`known`** - Nodes that are known to this node. That is, connected nodes and
  nodes referred to by process identifiers, port identifiers, and references
  located on this node. The set of known nodes is garbage collected. Notice that
  this garbage collection can be delayed. For more information, see
  [`erlang:system_info(delayed_node_table_gc)`](#system_info_delayed_node_table_gc).

Some equalities: `[node()] = nodes(this)`,
`nodes(connected) = nodes([visible, hidden])`, and `nodes() = nodes(visible)`.
""".
-doc #{ category => distribution }.
-spec nodes(Arg) -> Nodes when
      Arg :: NodeType | [NodeType],
      NodeType :: visible | hidden | connected | this | known,
      Nodes :: [node()].
nodes(_Arg) ->
    erlang:nif_error(undefined).

-doc """
Returns a list of `NodeInfo` tuples.

The first element is the node name. Nodes to be included in the list are determined
by the first argument `Arg` in the same way as for [`nodes(Arg)`](`nodes/1`).
The second element of `NodeInfo` tuples is a map containing further information
about the node identified by the first element.
The information present in this map is determined by the
`InfoOpts` map passed as the second argument. Currently the following
associations are allowed in the `InfoOpts` map:

- **`connection_id => boolean()`** - If the value of the association equals
  `true`, the `Info` map in the returned result will contain the key
  `connection_id` associated with the value `ConnectionId`. If `ConnectionId`
  equals `undefined`, the node is not connected to the node which the caller is
  executing on, or is the node which the caller is executing on. If
  `ConnectionId` is an integer, the node is currently connected to the node
  which the caller is executing on.

  [](){: #connection_id } The integer connection identifier value together with
  a node name identifies a specific connection instance to the node with that
  node name. The connection identifier value is node local. That is, on the
  other node the connection identifier will _not_ be the same value. If a
  connection is taken down and then taken up again, the connection identifier
  value will change for the connection to that node. The amount of values for
  connection identifiers are limited, so it is possible to see the same value
  for different instances, but quite unlikely. It is undefined how the value
  change between two consecutive connection instances.

- **`node_type => boolean()`** - If the value of the association equals `true`,
  the `Info` map in the returned result will contain the key `node_type`
  associated with the value `NodeTypeInfo`. Currently the following node types
  exist:

  - **`visible`** - The node is connected to the node of the calling process
    through an ordinary visible connection. That is, the node name would appear
    in the result returned by `nodes/0`.

  - **`hidden`** - The node is connected to the node of the calling process
    through a hidden connection. That is, the node name would _not_ appear in
    the result returned by `nodes/0`.

  - **`this`** - This is the node of the calling process.

  - **`known`** - The node is not connected but known to the node of the calling
    process.

Example:

```erlang
(a@localhost)1> nodes([this, connected], #{connection_id=>true, node_type=>true}).
[{c@localhost,#{connection_id => 13892108,node_type => hidden}},
 {b@localhost,#{connection_id => 3067553,node_type => visible}},
 {a@localhost,#{connection_id => undefined,node_type => this}}]
(a@localhost)2>
```
""".
-doc(#{since => <<"OTP 25.1">>}).
-doc #{ category => distribution }.
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

-doc """
Forces the disconnection of a node.

Doing this makes it appears to the node `Node` as if the local node has crashed.
This BIF is mainly used in the Erlang network authentication protocols.

Returns `true` if disconnection succeeds, otherwise `false`. If the local node
is not alive, `ignored` is returned.

> #### Note {: .info }
>
> This function may return before [`nodedown` messages](`monitor_node/2`) have
> been delivered.
""".
-doc #{ category => distribution }.
-spec disconnect_node(Node) -> boolean() | ignored when
      Node :: node().
disconnect_node(Node) ->
    net_kernel:disconnect(Node).

-doc """
Returns a list with information about the fun `Fun`. Each list element is a
tuple. The order of the tuples is undefined, and more tuples can be added in a
future release.

> #### Warning {: .warning }
>
> This BIF is mainly intended for debugging, but it can sometimes be useful in
> library functions that need to verify, for example, the arity of a fun.

Two types of funs have slightly different semantics:

- A fun created by `fun M:F/A` is called an _external_ fun. Calling it will
  always call the function `F` with arity `A` in the latest code for module `M`.
  Notice that module `M` does not even need to be loaded when the fun
  `fun M:F/A` is created.
- All other funs are called _local_. When a local fun is called, the same
  version of the code that created the fun is called (even if a newer version of
  the module has been loaded).

The following elements are always present in the list for both local and
external funs:

- **`{type, Type}`** - `Type` is `local` or `external`.

- **`{module, Module}`** - `Module` (an atom) is the module name.

  If `Fun` is a local fun, `Module` is the module in which the fun is defined.

  If `Fun` is an external fun, `Module` is the module that the fun refers to.

- **`{name, Name}`** - `Name` (an atom) is a function name.

  If `Fun` is a local fun, `Name` is the name of the local function that
  implements the fun. (This name was generated by the compiler, and is only of
  informational use. As it is a local function, it cannot be called directly.)
  If no code is currently loaded for the fun, `[]` is returned instead of an
  atom.

  If `Fun` is an external fun, `Name` is the name of the exported function that
  the fun refers to.

- **`{arity, Arity}`** - `Arity` is the number of arguments that the fun is to
  be called with.

- **`{env, Env}`** - `Env` (a list) is the environment or free variables for the
  fun. For external funs, the returned list is always empty.

The following elements are only present in the list if `Fun` is local:

- **`{pid, Pid}`** - `Pid` is the process identifier of `init` process on
  the local node.

  > #### Change {: .info }
  >
  > Starting in Erlang/OTP 27, `Pid` always points to the local `init` process,
  > regardless of which process or node the fun was originally created on.
  >
  > See
  > [Upcoming Potential Incompatibilities ](`e:general_info:upcoming_incompatibilities.md#fun-creator-pid-will-always-be-local-init-process`).

- **`{index, Index}`** - `Index` (an integer) is an index into the module fun
  table.

- **`{new_index, Index}`** - `Index` (an integer) is an index into the module
  fun table.

- **`{new_uniq, Uniq}`** - `Uniq` (a binary) is a unique value for this fun. It
  is calculated from the compiled code for the entire module.

- **`{uniq, Uniq}`** - `Uniq` (an integer) is a unique value for this fun. As
  from Erlang/OTP R15, this integer is calculated from the compiled code for the
  entire module. Before Erlang/OTP R15, this integer was based on only the body
  of the fun.
""".
-doc #{ category => terms }.
-spec fun_info(Fun) -> [{Item, Info}] when
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

-doc """
The destination for a send operation.

This can be a remote or local process identifier, a (local) port, a reference
denoting a process alias, a locally registered name, or a tuple `{RegName, Node}`
for a registered name at another node.
""".
-type send_destination() :: pid()
                          | reference()
                          | port()
                          | (RegName :: atom())
                          | {RegName :: atom(), Node :: node()}.

-doc """
Send a message without suspending the caller.

Equivalent to [`erlang:send(Dest, Msg, [nosuspend])`](`send/3`), but returns
`true` if the message was sent and `false` if the message was not sent because
the sender would have had to be suspended.

This function is intended for send operations to an unreliable remote node
without ever blocking the sending (Erlang) process. If the connection to the
remote node (usually not a real Erlang node, but a node written in C or Java) is
overloaded, this function _does not send the message_ and returns `false`.

The same occurs if `Dest` refers to a local port that is busy. For all other
destinations (allowed for the ordinary send operator `'!'`), this function sends
the message and returns `true`.

This function is only to be used in rare circumstances where a process
communicates with Erlang nodes that can disappear without any trace, causing the
TCP buffers and the drivers queue to be over-full before the node is shut down
(because of tick time-outs) by `net_kernel`. The normal reaction to take when
this occurs is some kind of premature shutdown of the other node.

Notice that ignoring the return value from this function would result in an
_unreliable_ message passing, which is contradictory to the Erlang programming
model. The message is _not_ sent if this function returns `false`.

In many systems, transient states of overloaded queues are normal. Although this
function returns `false` does not mean that the other node is guaranteed to be
non-responsive, it could be a temporary overload. Also, a return value of `true`
does only mean that the message can be sent on the (TCP) channel without
blocking; the message is not guaranteed to arrive at the remote node. For a
disconnected non-responsive node, the return value is `true` (mimics the
behavior of operator `!`). The expected behavior and the actions to take when
the function returns `false` are application- and hardware-specific.

> #### Warning {: .warning }
>
> Use with extreme care.
""".
-doc #{ category => processes }.
-spec send_nosuspend(Dest, Msg) -> boolean() when
      Dest :: send_destination(),
      Msg :: term().
send_nosuspend(Pid, Msg) ->
    try
        send_nosuspend(Pid, Msg, [])
    catch
        error:Error -> error_with_info(Error, [Pid, Msg])
    end.

-doc """
Equivalent to [`erlang:send(Dest, Msg, [nosuspend | Options])`](`send/3`), but
with a Boolean return value.

This function behaves like [`erlang:send_nosuspend/2`](`send_nosuspend/2`), but
takes a third parameter, a list of options. The only option is `noconnect`,
which makes the function return `false` if the remote node is not currently
reachable by the local node. The normal behavior is to try to connect to the
node, which can stall the process during a short period. The use of option
`noconnect` makes it possible to be sure not to get the slightest delay when
sending to a remote process. This is especially useful when communicating with
nodes that expect to always be the connecting part (that is, nodes written in C
or Java).

Whenever the function returns `false` (either when a suspend would occur or when
`noconnect` was specified and the node was not already connected), the message
is guaranteed _not_ to have been sent.

> #### Warning {: .warning }
>
> Use with extreme care.
""".
-doc #{ category => processes }.
-spec send_nosuspend(Dest, Msg, Options) -> boolean() when
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

-doc """
Converts local date and time to Universal Time Coordinated (UTC), if supported
by the underlying OS. Otherwise no conversion is done and `Localtime` is
returned.

For example:

```erlang
> erlang:localtime_to_universaltime({{1996,11,6},{14,45,17}}).
{{1996,11,6},{13,45,17}}
```

Failure: `badarg` if `Localtime` denotes an invalid date and time.
""".
-doc #{ category => time }.
-spec localtime_to_universaltime(Localtime) -> Universaltime when
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

-doc """
Sends data to a port. Same as `Port ! {PortOwner, {command, Data}}` except for
the error behavior and being synchronous (see below).

Any process can send data to a port with [`port_command/2`](`port_command/2`),
not only the port owner (the connected process).

For comparison: `Port ! {PortOwner, {command, Data}}` only fails with `badarg`
if `Port` does not refer to a port or a process. If `Port` is a closed port, the
data message disappears without a sound. If `Port` is open and the calling
process is not the port owner, the _port owner_ fails with `badsig`. The port
owner fails with `badsig` also if `Data` is an invalid I/O list.

Notice that any process can send to a port using
`Port ! {PortOwner, {command, Data}}` as if it itself was the port owner.

If the port is busy, the calling process is suspended until the port is not busy
any more.

As from Erlang/OTP R16, `Port ! {PortOwner, {command, Data}}` is truly
asynchronous. Notice that this operation has always been documented as an
asynchronous operation, while the underlying implementation has been
synchronous. [`port_command/2`](`port_command/2`) is however still fully
synchronous because of its error behavior.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Data` is an invalid I/O list.

> #### Warning {: .warning }
>
> Do not send data to an unknown port. Any undefined behavior is possible
> (including node crash) depending on how the port driver interprets the data.
""".
-doc #{ category => ports }.
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

-doc """
Sends data to a port. [`port_command(Port, Data, [])`](`port_command/3`) equals
[`port_command(Port, Data)`](`port_command/2`).

If the port command is aborted, `false` is returned, otherwise `true`.

If the port is busy, the calling process is suspended until the port is not busy
anymore.

`Option`s:

- **`force`** - The calling process is not suspended if the port is busy,
  instead the port command is forced through. The call fails with a `notsup`
  exception if the driver of the port does not support this. For more
  information, see driver flag
  [`ERL_DRV_FLAG_SOFT_BUSY`](driver_entry.md#driver_flags).

- **`nosuspend`** - The calling process is not suspended if the port is busy,
  instead the port command is aborted and `false` is returned.

> #### Change {: .info }
>
> More options can be added in a future release.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Data` is an invalid I/O list.

- **`badarg`** - If `OptionList` is an invalid option list.

- **`notsup`** - If option `force` has been passed, but the driver of the port
  does not allow forcing through a busy port.

> #### Warning {: .warning }
>
> Do not send data to an unknown port. Any undefined behavior is possible
> (including node crash) depending on how the port driver interprets the data.
""".
-doc #{ category => ports }.
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

-doc """
Sets the port owner (the connected port) to `Pid`. Roughly the same as
`Port ! {Owner, {connect, Pid}}` except for the following:

- The error behavior differs, see below.
- The port does _not_ reply with `{Port,connected}`.
- `port_connect/1` is synchronous, see below.
- The new port owner gets linked to the port.

The old port owner stays linked to the port and must call
[`unlink(Port)`](`unlink/1`) if this is not desired. Any process can set the
port owner to be any process with [`port_connect/2`](`port_connect/2`).

For comparison: `Port ! {self(), {connect, Pid}}` only fails with `badarg` if
`Port` does not refer to a port or a process. If `Port` is a closed port,
nothing happens. If `Port` is an open port and the calling process is the port
owner, the port replies with `{Port, connected}` to the old port owner. Notice
that the old port owner is still linked to the port, while the new is not. If
`Port` is an open port and the calling process is not the port owner, the _port
owner_ fails with `badsig`. The port owner fails with `badsig` also if `Pid` is
not an existing local process identifier.

Notice that any process can set the port owner using
`Port ! {PortOwner, {connect, Pid}}` as if it itself was the port owner, but the
reply always goes to the port owner.

As from Erlang/OTP R16, `Port ! {PortOwner, {connect, Pid}}` is truly
asynchronous. Notice that this operation has always been documented as an
asynchronous operation, while the underlying implementation has been
synchronous. [`port_connect/2`](`port_connect/2`) is however still fully
synchronous because of its error behavior.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If the process identified by `Pid` is not an existing local
  process.
""".
-doc #{ category => ports }.
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

-doc """
Closes an open port. Roughly the same as `Port ! {self(), close}` except for the
error behavior (see below), being synchronous, and that the port does _not_
reply with `{Port, closed}`.

Any process can close a port with [`port_close/1`](`port_close/1`), not only the
port owner (the connected process). If the calling process is linked to the port
identified by `Port`, the exit signal from the port is guaranteed to be delivered before
[`port_close/1`](`port_close/1`) returns.

For comparison: `Port ! {self(), close}` only fails with `badarg` if `Port` does
not refer to a port or a process. If `Port` is a closed port, nothing happens.
If `Port` is an open port and the calling process is the port owner, the port
replies with `{Port, closed}` when all buffers have been flushed and the port
really closes. If the calling process is not the port owner, the _port owner_
fails with `badsig`.

Notice that any process can close a port using `Port ! {PortOwner, close}` as if
it itself was the port owner, but the reply always goes to the port owner.

As from Erlang/OTP R16, `Port ! {PortOwner, close}` is truly asynchronous.
Notice that this operation has always been documented as an asynchronous
operation, while the underlying implementation has been synchronous.
[`port_close/1`](`port_close/1`) is however still fully synchronous because of
its error behavior.

Failure: `badarg` if `Port` is not an identifier of an open port, or the
registered name of an open port. If the calling process was previously linked to
the closed port, identified by `Port`, the exit signal from the port is
guaranteed to be delivered before this `badarg` exception occurs.
""".
-doc #{ category => ports }.
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

-doc """
Performs a synchronous control operation on a port. The meaning of `Operation`
and `Data` depends on the port, that is, on the port driver. Not all port
drivers support this control feature.

Returns a list of integers in the range 0..255, or a binary, depending on the
port driver. The meaning of the returned data also depends on the port driver.

Failures:

- **`badarg`** - If `Port` is not an open port or the registered name of an open
  port.

- **`badarg`** - If `Operation` cannot fit in a 32-bit integer.

- **`badarg`** - If the port driver does not support synchronous control
  operations.

- **`badarg`** - If the port driver so decides for any reason (probably
  something wrong with `Operation` or `Data`).
  > #### Warning {: .warning }
  >
  > Do not call [`port_control/3`](`port_control/3`) with an unknown `Port`
  > identifier and expect `badarg` exception. Any undefined behavior is possible
  > (including node crash) depending on how the port driver interprets the
  > supplied arguments.
""".
-doc #{ category => ports }.
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

-doc false.
-spec port_call(Port, Data) -> term() when
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

-doc """
Performs a synchronous call to a port. The meaning of `Operation` and `Data`
depends on the port, that is, on the port driver. Not all port drivers support
this feature.

`Port` is a port identifier, referring to a driver.

`Operation` is an integer, which is passed on to the driver.

`Data` is any Erlang term. This data is converted to binary term format and sent
to the port.

Returns a term from the driver. The meaning of the returned data also depends on
the port driver.

Failures:

- **`badarg`** - If `Port` is not an identifier of an open port, or the
  registered name of an open port. If the calling process was previously linked
  to the closed port, identified by `Port`, the exit signal from the port is
  guaranteed to be delivered before this `badarg` exception occurs.

- **`badarg`** - If `Operation` does not fit in a 32-bit integer.

- **`badarg`** - If the port driver does not support synchronous control
  operations.

- **`badarg`** - If the port driver so decides for any reason (probably
  something wrong with `Operation` or `Data`).

  > #### Warning {: .warning }
  >
  > Do not call `port_call` with an unknown `Port` identifier and expect
  > `badarg` exception. Any undefined behavior is possible (including node
  > crash) depending on how the port driver interprets the supplied arguments.
""".
-doc #{ category => ports }.
-spec port_call(Port, Operation, Data) -> term() when
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

-doc """
Returns a list containing tuples with information about `Port`, or `undefined`
if the port is not open.

The order of the tuples is undefined, and all the tuples are not mandatory.
If the port is closed and the calling process was
previously linked to the port, the exit signal from the port is guaranteed to be
delivered before [`port_info/1`](`port_info/1`) returns `undefined`.

The result contains information about the following `Item`s:

- `registered_name` (if the port has a registered name)
- `id`
- `connected`
- `links`
- `name`
- `input`
- `output`

For more information about the different `Item`s, see `port_info/2`.

Failure: `badarg` if `Port` is not a local port identifier, or an atom.
""".
-doc #{ category => ports }.
-spec port_info(Port) -> Result when
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

-doc #{ category => ports }.
-spec port_info(Port, Item :: connected) -> {connected, Pid} | 'undefined' when
      Port :: port() | atom(),
      Pid :: pid();
		      (Port, Item :: id) -> {id, Index} | 'undefined' when
      Port :: port() | atom(),
      Index :: non_neg_integer();
		      (Port, Item :: input) -> {input, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, Item :: links) -> {links, Pids} | 'undefined' when
      Port :: port() | atom(),
      Pids :: [pid()];
		      (Port, Item :: locking) -> {locking, Locking} | 'undefined' when
      Port :: port() | atom(),
      Locking :: 'false' | 'port_level' | 'driver_level';
		      (Port, Item :: memory) -> {memory, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, Item :: monitors) -> {monitors, Monitors} | 'undefined' when
      Port :: port() | atom(),
      Monitors :: [{process, pid()}];
		      (Port, Item :: monitored_by) -> {monitored_by, MonitoredBy} | 'undefined' when
      Port :: port() | atom(),
      MonitoredBy :: [pid()];
		      (Port, Item :: name) -> {name, Name} | 'undefined' when
      Port :: port() | atom(),
      Name :: string();
		      (Port, Item :: os_pid) -> {os_pid, OsPid} | 'undefined' when
      Port :: port() | atom(),
      OsPid :: non_neg_integer() | 'undefined';
		      (Port, Item :: output) -> {output, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, Item :: parallelism) -> {parallelism, Boolean} | 'undefined' when
      Port :: port() | atom(),
      Boolean :: boolean();
		      (Port, Item :: queue_size) -> {queue_size, Bytes} | 'undefined' when
      Port :: port() | atom(),
      Bytes :: non_neg_integer();
		      (Port, Item :: registered_name) -> {registered_name, RegisteredName} | [] | 'undefined' when
      Port :: port() | atom(),
      RegisteredName :: atom().

-doc {file, "../../doc/src/erlang_port_info.md"}.
port_info(Port, Item) ->
    case case erts_internal:port_info(Port, Item) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
        badtype -> badarg_with_cause([Port, Item], badtype);
	badarg -> badarg_with_info([Port, Item]);
	Result -> Result
    end.

-doc false.
-spec port_set_data(Port, Data) -> 'true' when
      Port :: port() | atom(),
      Data :: term().
    
port_set_data(_Port, _Data) ->
    erlang:nif_error(undefined).

-doc false.
-spec port_get_data(Port) -> term() when
      Port :: port() | atom().

port_get_data(_Port) ->
    erlang:nif_error(undefined).

%%
%% Distribution channel management
%%

-doc """
Register an alternate input handler process for the distribution channel
identified by `DHandle`.

Once this function has been called, `InputHandler` is the only process allowed to call
[`erlang:dist_ctrl_put_data(DHandle, Data)`](`dist_ctrl_put_data/2`) with the
`DHandle` identifying this distribution channel.

> #### Note {: .info }
>
> When the distribution controller for the distribution channel identified by
> `DHandle` is a process, it is the only process allowed to call this function.
> This function is also allowed to be called when the distribution controller
> for the distribution channel identified by `DHandle` is a port. The data
> received by the port should in this case be delivered to the process
> identified by `InputHandler` which in turn should call
> [`erlang:dist_ctrl_put_data/2`](`dist_ctrl_put_data/2`).

This function is used when implementing an alternative distribution carrier.
`DHandle` is retrieved via the callback
[`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete). More
information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_input_handler(DHandle, InputHandler) -> 'ok' when
      DHandle :: dist_handle(),
      InputHandler :: pid().

dist_ctrl_input_handler(_DHandle, _InputHandler) ->
    erlang:nif_error(undefined).

-doc """
Deliver distribution channel data from a remote node to the local node.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function unless an
> alternate input handler process has been registered using
> [`erlang:dist_ctrl_input_handler(DHandle, InputHandler)`](`dist_ctrl_input_handler/2`).
> If an alternate input handler has been registered, only the registered input
> handler process is allowed to call this function.

This function is used when implementing an alternative distribution carrier.
`DHandle` is retrieved via the callback
[`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete). More
information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_put_data(DHandle, Data) -> 'ok' when
      DHandle :: dist_handle(),
      Data :: iodata().

dist_ctrl_put_data(_DHandle, _Data) ->
    erlang:nif_error(undefined).

-doc """
Get distribution channel data from the local node that is to be passed to the
remote node.

The distribution channel is identified by `DHandle`. If no data is
available, the atom `none` is returned. One can request to be informed by a
message when more data is available by calling
[`erlang:dist_ctrl_get_data_notification(DHandle)`](`dist_ctrl_get_data_notification/1`).

The returned value when there are data available depends on the value of the
`get_size` option configured on the distribution channel identified by
`DHandle`. For more information see the documentation of the `get_size` option
for the [`erlang:dist_ctrl_set_opt/3`](`dist_ctrl_set_opt/3`) function.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_get_data(DHandle) -> {Size, Data} | Data | 'none' when
      Size :: non_neg_integer(),
      DHandle :: dist_handle(),
      Data :: iovec().

dist_ctrl_get_data(_DHandle) ->
    erlang:nif_error(undefined).

-doc """
Request notification when more data is available to fetch using
[`erlang:dist_ctrl_get_data(DHandle)`](`dist_ctrl_get_data/1`) for the
distribution channel identified by `DHandle`.

When more data is present, the caller will be sent the message `dist_data`.
Once a `dist_data` messages has been sent, no more `dist_data` messages will
be sent until the [`dist_ctrl_get_data_notification/1`](`dist_ctrl_get_data_notification/1`)
function has been called again.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 21.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_get_data_notification(DHandle) -> 'ok' when
      DHandle :: dist_handle().

dist_ctrl_get_data_notification(_DHandle) ->
    erlang:nif_error(undefined).

-doc """
Sets the value of the `get_size` option on the distribution channel identified
by `DHandle`.

This option controls the return value of calls to
[erlang:dist_ctrl_get_data(DHandle)](`dist_ctrl_get_data/1`) where `DHandle`
equals `DHandle` used when setting this option. When the `get_size` option is:

- **`false`** - and there are distribution data available, a call to
  `erlang:dist_ctrl_get_data(DHandle)` will just return `Data` to pass over the
  channel. This is the default value of the `get_size` option.

- **`true`** - and there are distribution data available, a call to
  `erlang:dist_ctrl_get_data(DHandle)` will return `Data` to pass over the
  channel as well as the `Size` of `Data` in bytes. This is returned as a tuple
  of the form `{Size, Data}`.

All options are set to default when a channel is closed.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 22.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_set_opt(DHandle, 'get_size', Value) -> OldValue when
      DHandle :: dist_handle(),
      Value :: boolean(),
      OldValue :: boolean().

dist_ctrl_set_opt(_DHandle, _Opt, _Val) ->
    erlang:nif_error(undefined).

-doc """
Returns the value of the `get_size` option on the distribution channel
identified by `DHandle`. For more information see the documentation of the
`get_size` option for the [`erlang:dist_ctrl_set_opt/3`](`dist_ctrl_set_opt/3`)
function.

> #### Note {: .info }
>
> Only the process registered as distribution controller for the distribution
> channel identified by `DHandle` is allowed to call this function.

This function is used when implementing an alternative distribution carrier
using processes as distribution controllers. `DHandle` is retrieved via the
callback [`f_handshake_complete`](alt_dist.md#hs_data_f_handshake_complete).
More information can be found in the documentation of
[ERTS User's Guide ➜ How to implement an Alternative Carrier for the Erlang Distribution ➜ Distribution Module](alt_dist.md#distribution-module).
""".
-doc(#{since => <<"OTP 22.0">>}).
-doc #{ category => distribution }.
-spec dist_ctrl_get_opt(DHandle, 'get_size') -> Value when
      DHandle :: dist_handle(),
      Value :: boolean().

dist_ctrl_get_opt(_DHandle, _Opt) ->
    erlang:nif_error(undefined).

-doc false.
-spec dist_get_stat(DHandle) -> Res when
      DHandle :: dist_handle(),
      InputPackets :: non_neg_integer(),
      OutputPackets :: non_neg_integer(),
      PendingOutputPackets :: non_neg_integer(),
      Res :: {'ok', InputPackets, OutputPackets, PendingOutputPackets}.

dist_get_stat(_DHandle) ->
    erlang:nif_error(undefined).


-doc false.
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

-doc false.
-spec delay_trap(Result, timeout()) -> Result.
delay_trap(Result, 0) -> erlang:yield(), Result;
delay_trap(Result, Timeout) -> receive after Timeout -> Result end.


-doc """
Sets the magic cookie of the local node to the atom `Cookie`, which is also the
cookie for all nodes that have no explicit cookie set with `set_cookie/2`
`Cookie`.

See section [Distributed Erlang](`e:system:distributed.md`) in the
Erlang Reference Manual in System Documentation for more information.

You can get this value using `get_cookie/0`.

Failure: `function_clause` if the local node is not alive.
""".
-doc(#{since => <<"OTP 24.1">>}).
-doc #{ category => distribution }.
-spec set_cookie(Cookie) -> true when
      Cookie :: atom().
set_cookie(C) when erlang:is_atom(C) ->
    auth:set_cookie(C);
set_cookie(C) ->
    badarg_with_info([C]).

-doc """
Sets the magic cookie for `Node` to the atom `Cookie`. If `Node` is the local
node, the function sets the cookie of all other nodes (that have no explicit
cookie set with this function) to `Cookie`.

See section [Distributed Erlang](`e:system:distributed.md`) in the
Erlang Reference Manual in System Documentation for more information.

You can get this value using `get_cookie/1`.

Failure: `function_clause` if the local node is not alive.
""".
-doc #{ category => distribution }.
-spec set_cookie(Node, Cookie) -> true when
      Node :: node(),
      Cookie :: atom().
set_cookie(Node, C)
  when Node =/= nonode@nohost, erlang:is_atom(Node), erlang:is_atom(C) ->
    auth:set_cookie(Node, C);
set_cookie(Node, C) ->
    badarg_with_info([Node, C]).


-doc """
Returns the magic cookie of the local node if the node is alive, otherwise the
atom `nocookie`. This value is set by `set_cookie/1`.
""".
-doc #{ category => distribution }.
-spec get_cookie() -> Cookie | nocookie when
      Cookie :: atom().
get_cookie() ->
    auth:get_cookie().

-doc """
Returns the magic cookie for node `Node` if the local node is alive, otherwise
the atom `nocookie`. This value is set by `set_cookie/2`.
""".
-doc(#{since => <<"OTP 24.1">>}).
-doc #{ category => distribution }.
-spec get_cookie(Node) -> Cookie | nocookie when
      Node :: node(),
      Cookie :: atom().
get_cookie(Node) when erlang:is_atom(Node) ->
    auth:get_cookie(Node);
get_cookie(Node) ->
    badarg_with_info([Node]).


-doc """
Returns a string corresponding to the text representation of `Integer` in base
`Base`.

For example:

```erlang
> integer_to_list(1023, 16).
"3FF"
```
""".
-doc #{ category => terms }.
-spec integer_to_list(Integer, Base) -> string() when
      Integer :: integer(),
      Base :: 2..36.
integer_to_list(_I, _Base) ->
    erlang:nif_error(undefined).

-doc """
Returns a binary corresponding to the text representation of `Integer` in base
`Base`.

For example:

```erlang
> integer_to_binary(1023, 16).
<<"3FF">>
```
""".
-doc(#{since => <<"OTP R16B">>}).
-doc #{ category => terms }.
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
-doc false.
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
-doc false.
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
-doc """
Returns the smallest of `Term1` and `Term2`. If the terms compare equal with the
`==` operator, `Term1` is returned.

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of the `==` operator and how terms are ordered.

Examples:

```erlang
> min(1, 2).
1
```

```erlang
> min(1.0, 1).
1.0
```

```erlang
> min(1, 1.0).
1
```

```erlang
> min("abc", "b").
"abc"
```

> #### Change {: .info }
>
> Allowed in guards tests from Erlang/OTP 26.
""".
-doc #{ category => terms }.
-spec min(Term1, Term2) -> Minimum when
      Term1 :: term(),
      Term2 :: term(),
      Minimum :: term().
%% In Erlang/OTP 26, min/2 is a guard BIF. This implementation is kept
%% for backward compatibility with code compiled with an earlier version.
min(A, B) when A > B -> B;
min(A, _) -> A.

%% Shadowed by erl_bif_types: erlang:max/2
-doc """
Returns the largest of `Term1` and `Term2`. If the terms compare equal with the
`==` operator, `Term1` is returned.

The [Expressions section](`e:system:expressions.md#term-comparisons`) contains
descriptions of the `==` operator and how terms are ordered.

Examples:

```erlang
> max(1, 2).
2
```

```erlang
> max(1.0, 1).
1.0
```

```erlang
> max(1, 1.0).
1
```

```erlang
> max("abc", "b").
"b"
```

> #### Change {: .info }
>
> Allowed in guards tests from Erlang/OTP 26.
""".
-doc #{ category => terms }.
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

-doc """
Returns a list with information about memory dynamically allocated by the Erlang
emulator.

Each list element is a tuple `{Type, Size}`. The first element `Type`
is an atom describing memory type. The second element `Size` is the memory size
in bytes.

Memory types:

- **`total`** - The total amount of memory currently allocated. This is the same
  as the sum of the memory size for `processes` and `system`.

- **`processes`** - The total amount of memory currently allocated for the
  Erlang processes.

- **`processes_used`** - The total amount of memory currently used by the Erlang
  processes. This is part of the memory presented as `processes` memory.

- **`system`** - The total amount of memory currently allocated for the emulator
  that is not directly related to any Erlang process. Memory presented as
  `processes` is not included in this memory. `m:instrument` can be used to get
  a more detailed breakdown of what memory is part of this type.

- **`atom`** - The total amount of memory currently allocated for atoms. This
  memory is part of the memory presented as `system` memory.

- **`atom_used`** - The total amount of memory currently used for atoms. This
  memory is part of the memory presented as `atom` memory.

- **`binary`** - The total amount of memory currently allocated for binaries.
  This memory is part of the memory presented as `system` memory.

- **`code`** - The total amount of memory currently allocated for Erlang code.
  This memory is part of the memory presented as `system` memory.

- **`ets`** - The total amount of memory currently allocated for ETS tables.
  This memory is part of the memory presented as `system` memory.

- **`maximum`** - The maximum total amount of memory allocated since the
  emulator was started. This tuple is only present when the emulator is run with
  instrumentation.

  For information on how to run the emulator with instrumentation, see
  `m:instrument` and/or [`erl(1)`](erl_cmd.md).

> #### Note {: .info }
>
> The `system` value is not complete. Some allocated memory that is to be part
> of this value is not.
>
> When the emulator is run with instrumentation, the `system` value is more
> accurate, but memory directly allocated for `malloc` (and friends) is still
> not part of the `system` value. Direct calls to `malloc` are only done from
> OS-specific runtime libraries and perhaps from user-implemented Erlang drivers
> that do not use the memory allocation functions in the driver interface.
>
> As the `total` value is the sum of `processes` and `system`, the error in
> `system` propagates to the `total` value.
>
> The different amounts of memory that are summed are _not_ gathered atomically,
> which introduces an error in the result.

The different values have the following relation to each other. Values beginning
with an uppercase letter is not part of the result.

```text
total      = processes + system
processes  = processes_used + ProcessesNotUsed
system     = atom + binary + code + ets + OtherSystem
atom       = atom_used + AtomNotUsed
RealTotal  = processes + RealSystem
RealSystem = system + MissedSystem
```

More tuples in the returned list can be added in a future release.

> #### Note {: .info }
>
> The `total` value is supposed to be the total amount of memory dynamically
> allocated by the emulator. Shared libraries, the code of the emulator itself,
> and the emulator stacks are not supposed to be included. That is, the `total`
> value is _not_ supposed to be equal to the total size of all pages mapped to
> the emulator.
>
> Also, because of fragmentation and prereservation of memory areas, the size of
> the memory segments containing the dynamically allocated memory blocks can be
> much larger than the total size of the dynamically allocated memory blocks.

> #### Change {: .info }
>
> As from ERTS 5.6.4, `erlang:memory/0` requires that all
> [`erts_alloc(3)`](erts_alloc.md) allocators are enabled (default behavior).

Failure: `notsup` if an [`erts_alloc(3)`](erts_alloc.md) allocator has been
disabled.
""".
-doc #{ category => system }.
-spec memory() -> [{Type, Size}] when
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

-doc """
Returns the memory size in bytes allocated for memory of type `Type`. The
argument can also be specified as a list of `t:memory_type/0` atoms, in which case
a corresponding list of `{memory_type(), Size :: integer >= 0}` tuples is
returned.

> #### Change {: .info }
>
> As from ERTS 5.6.4, `erlang:memory/1` requires that all
> [`erts_alloc(3)`](erts_alloc.md) allocators are enabled (default behavior).

Failures:

- **`badarg`** - If `Type` is not one of the memory types listed in the
  description of [`erlang:memory/0`](`memory/0`).

- **`badarg`** - If `maximum` is passed as `Type` and the emulator is not run in
  instrumented mode.

- **`notsup`** - If an [`erts_alloc(3)`](erts_alloc.md) allocator has been
  disabled.

See also [`erlang:memory/0`](`memory/0`).
""".
-doc #{ category => system }.
-spec memory(Type :: memory_type()) -> non_neg_integer();
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
					  CodeData == fun_list;
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

-doc false.
alloc_info(Allocs) ->
    get_alloc_info(allocator, Allocs).

-doc false.
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

-doc false.
-spec gather_gc_info_result(Ref) ->
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

-doc false.
-spec '=='(term(), term()) -> boolean().
'=='(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '=:='(term(), term()) -> boolean().
'=:='(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '/='(term(), term()) -> boolean().
'/='(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '=/='(term(), term()) -> boolean().
'=/='(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '=<'(term(), term()) -> boolean().
'=<'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '>='(term(), term()) -> boolean().
'>='(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '<'(term(), term()) -> boolean().
'<'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '>'(term(), term()) -> boolean().
'>'(_A, _B) ->
    erlang:nif_error(undefined).

-doc false.
-spec '-'(number()) -> number().
'-'(_A) ->
    erlang:nif_error(undefined).
-doc false.
-spec '+'(number()) -> number().
'+'(_A) ->
    erlang:nif_error(undefined).
-doc false.
-spec '-'(number(), number()) -> number().
'-'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '+'(number(), number()) -> number().
'+'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '/'(number(), number()) -> float().
'/'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '*'(number(), number()) -> number().
'*'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'div'(integer(), integer()) -> integer().
'div'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'rem'(integer(), integer()) -> integer().
'rem'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'bsl'(integer(), integer()) -> integer().
'bsl'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'bsr'(integer(), integer()) -> integer().
'bsr'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'bor'(integer(), integer()) -> integer().
'bor'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'band'(integer(), integer()) -> integer().
'band'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'bxor'(integer(), integer()) -> integer().
'bxor'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'bnot'(integer()) -> integer().
'bnot'(_A) ->
    erlang:nif_error(undefined).

-doc false.
-spec '--'(list(), list()) -> list().
'--'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec '++'(list(), term()) -> term().
'++'(_A, _B) ->
    erlang:nif_error(undefined).

-doc false.
-spec 'and'(boolean(), boolean()) -> boolean().
'and'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'or'(boolean(), boolean()) -> boolean().
'or'(_A, _B) ->
    erlang:nif_error(undefined).

-doc false.
-spec 'xor'(boolean(), boolean()) -> boolean().
'xor'(_A, _B) ->
    erlang:nif_error(undefined).
-doc false.
-spec 'not'(boolean()) -> boolean().
'not'(_A) ->
    erlang:nif_error(undefined).

-doc false.
-spec '!'(send_destination(), term()) -> term().
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
