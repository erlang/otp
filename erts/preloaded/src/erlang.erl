%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2012. All Rights Reserved.
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
-module(erlang).

-export([apply/2,apply/3,spawn/4,spawn_link/4,
	 spawn_monitor/1,spawn_monitor/3,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5,
	 disconnect_node/1]).
-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2]).
-export([yield/0]).
-export([crasher/6]).
-export([fun_info/1]).
-export([send_nosuspend/2, send_nosuspend/3]).
-export([localtime_to_universaltime/1]).
-export([suspend_process/1]).
-export([min/2, max/2]).
-export([dlink/1, dunlink/1, dsend/2, dsend/3, dgroup_leader/2,
	 dexit/2, dmonitor_node/3, dmonitor_p/2]).
-export([delay_trap/2]).
-export([set_cookie/2, get_cookie/0]).
-export([nodes/0]).

-export([list_to_integer/2,integer_to_list/2]).
-export([flush_monitor_message/2]).
-export([set_cpu_topology/1, format_cpu_topology/1]).
-export([await_proc_exit/3]).
-export([memory/0, memory/1]).
-export([alloc_info/1, alloc_sizes/1]).

-export([gather_sched_wall_time_result/1,
	 await_sched_wall_time_modifications/2]).

-deprecated([hash/2]).

%% Get rid of autoimports of spawn to avoid clashes with ourselves.
-compile({no_auto_import,[spawn_link/1]}).
-compile({no_auto_import,[spawn_link/4]}).
-compile({no_auto_import,[spawn_opt/2]}).
-compile({no_auto_import,[spawn_opt/4]}).
-compile({no_auto_import,[spawn_opt/5]}).

-export_type([timestamp/0]).

-type ext_binary() :: binary().
-type timestamp() :: {MegaSecs :: non_neg_integer(),
                      Secs :: non_neg_integer(),
                      MicroSecs :: non_neg_integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Native code BIF stubs and their types
%% (BIF's actually implemented in this module goes last in the file)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports for all native code stubs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-export([adler32/1, adler32/2, adler32_combine/3, append_element/2]).
-export([atom_to_binary/2, atom_to_list/1, binary_part/2, binary_part/3]).
-export([binary_to_atom/2, binary_to_existing_atom/2, binary_to_list/1]).
-export([binary_to_list/3, binary_to_term/1, binary_to_term/2]).
-export([bit_size/1, bitsize/1, bitstr_to_list/1, bitstring_to_list/1]).
-export([bump_reductions/1, byte_size/1, call_on_load_function/1]).
-export([cancel_timer/1, check_old_code/1, check_process_code/2, crc32/1]).
-export([crc32/2, crc32_combine/3, date/0, decode_packet/3]).
-export([delete_element/2]).
-export([delete_module/1, demonitor/1, demonitor/2, display/1]).
-export([display_nl/0, display_string/1, dist_exit/3, erase/0, erase/1]).
-export([error/1, error/2, exit/1, exit/2, external_size/1]).
-export([external_size/2, finish_after_on_load/2, finish_loading/1, float/1]).
-export([float_to_list/1, float_to_list/2]).
-export([fun_info/2, fun_to_list/1, function_exported/3]).
-export([garbage_collect/0, garbage_collect/1]).
-export([garbage_collect_message_area/0, get/0, get/1, get_keys/1]).
-export([get_module_info/1, get_stacktrace/0, group_leader/0]).
-export([group_leader/2, halt/0, halt/1, halt/2, hash/2, hibernate/3]).
-export([insert_element/3]).
-export([integer_to_list/1, iolist_size/1, iolist_to_binary/1]).
-export([is_alive/0, is_builtin/3, is_process_alive/1, length/1, link/1]).
-export([list_to_atom/1, list_to_binary/1, list_to_bitstr/1]).
-export([list_to_bitstring/1, list_to_existing_atom/1, list_to_float/1]).
-export([list_to_integer/1, list_to_pid/1, list_to_tuple/1, loaded/0]).
-export([localtime/0, make_ref/0, match_spec_test/3, md5/1, md5_final/1]).
-export([md5_init/0, md5_update/2, module_loaded/1, monitor/2]).
-export([monitor_node/2, monitor_node/3, nif_error/1, nif_error/2
]).
-export([node/0, node/1, now/0, phash/2, phash2/1, phash2/2]).
-export([pid_to_list/1, port_close/1, port_command/2, port_command/3]).
-export([port_connect/2, port_control/3, port_get_data/1]).
-export([port_set_data/2, port_to_list/1, ports/0]).
-export([posixtime_to_universaltime/1, pre_loaded/0, prepare_loading/2]).
-export([process_display/2]).
-export([process_flag/3, process_info/1, processes/0, purge_module/1]).
-export([put/2, raise/3, read_timer/1, ref_to_list/1, register/2]).
-export([registered/0, resume_process/1, round/1, self/0, send_after/3]).
-export([seq_trace/2, seq_trace_print/1, seq_trace_print/2, setnode/2]).
-export([setnode/3, size/1, spawn/3, spawn_link/3, split_binary/2]).
-export([start_timer/3, suspend_process/2, system_monitor/0]).
-export([system_monitor/1, system_monitor/2, system_profile/0]).
-export([system_profile/2, throw/1, time/0, trace/3, trace_delivered/1]).
-export([trace_info/2, trunc/1, tuple_size/1, universaltime/0]).
-export([universaltime_to_posixtime/1, unlink/1, unregister/1, whereis/1]).

-export([abs/1, append/2, element/2, get_module_info/2, hd/1,
         is_atom/1, is_binary/1, is_bitstring/1, is_boolean/1,
         is_float/1, is_function/1, is_function/2, is_integer/1,
         is_list/1, is_number/1, is_pid/1, is_port/1, is_record/2,
         is_record/3, is_reference/1, is_tuple/1, load_module/2,
         load_nif/2, localtime_to_universaltime/2, make_fun/3,
         make_tuple/2, make_tuple/3, nodes/1, open_port/2,
         port_call/2, port_call/3, port_info/1, port_info/2, process_flag/2,
         process_info/2, send/2, send/3, seq_trace_info/1,
         setelement/3, spawn_opt/1,
	 statistics/1, subtract/2, system_flag/2,
         term_to_binary/1, term_to_binary/2, tl/1, trace_pattern/2,
         trace_pattern/3, tuple_to_list/1, system_info/1,
         universaltime_to_localtime/1]).
-export([dt_get_tag/0, dt_get_tag_data/0, dt_prepend_vm_tag_data/1, dt_append_vm_tag_data/1,
	 dt_put_tag/1, dt_restore_tag/1, dt_spread_tag/1]). 


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

-type seq_trace_info() ::
      'send' |
      'receive' |
      'print' |
      'timestamp' |
      'label' |
      'serial'.

-type seq_trace_info_returns() ::
      { seq_trace_info(), non_neg_integer() |
                          boolean() |
			  { non_neg_integer(), non_neg_integer() } } |
      [].

-type system_profile_option() ::
      'exclusive' |
      'runnable_ports' |
      'runnable_procs' |
      'scheduler'.

-type system_monitor_option() ::
      'busy_port' |
      'busy_dist_port' |
      {'long_gc', non_neg_integer()} |
      {'large_heap', non_neg_integer()}.


-type raise_stacktrace() ::
      [{module(), atom(), arity() | [term()]} |
       {function(), [term()]}] |
      [{module(), atom(), arity() | [term()], [{atom(),term()}]} |
       {function(), [term()], [{atom(),term()}]}].

-type bitstring_list() ::
      maybe_improper_list(byte() | bitstring() | bitstring_list(), bitstring() | []).

-type trace_flag() ::
      all |
      send |
      'receive' |
      procs |
      call |
      silent |
      return_to |
      running |
      exiting |
      garbage_collection |
      timestamp |
      cpu_timestamp |
      arity |
      set_on_spawn |
      set_on_first_spawn |
      set_on_link |
      set_on_first_link |
      {tracer, pid() | port()}.

-type trace_info_item_result() ::
       {traced, global | local | false | undefined} |
       {match_spec, trace_match_spec() | false | undefined} |
       {meta, pid() | port() | false | undefined | []} |
       {meta_match_spec, trace_match_spec() | false | undefined} |
       {call_count, non_neg_integer() | boolean() | undefined} |
       {call_time, [{pid(), non_neg_integer(),
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
      arity.

-type trace_info_return() ::
      undefined |
      {flags, [trace_info_flag()]} |
      {tracer, pid() | port() | []} |
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

%% binary_to_atom/2
-spec binary_to_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_atom(_Binary, _Encoding) ->
    erlang:nif_error(undefined).

%% binary_to_existing_atom/2
-spec binary_to_existing_atom(Binary, Encoding) -> atom() when
      Binary :: binary(),
      Encoding :: latin1 | unicode | utf8.
binary_to_existing_atom(_Binary, _Encoding) ->
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
-spec binary_to_term(Binary, Opts) -> term() when
      Binary :: ext_binary(),
      Opts :: [safe].
binary_to_term(_Binary, _Opts) ->
    erlang:nif_error(undefined).

%% bit_size/1
%% Shadowed by erl_bif_types: erlang:bit_size/1
-spec bit_size(Bitstring) -> non_neg_integer() when
      Bitstring :: bitstring().
bit_size(_Bitstring) ->
    erlang:nif_error(undefined).

%% bitsize/1
-spec bitsize(P1) -> non_neg_integer() when
      P1 :: bitstring().
bitsize(_P1) ->
    erlang:nif_error(undefined).

%% bitstr_to_list/1
-spec erlang:bitstr_to_list(P1) -> [byte() | bitstring()] when
      P1 :: bitstring().
bitstr_to_list(_P1) ->
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
-spec erlang:cancel_timer(TimerRef) -> Time | false when
      TimerRef :: reference(),
      Time :: non_neg_integer().
cancel_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% check_old_code/1
-spec check_old_code(Module) -> boolean() when
      Module :: module().
check_old_code(_Module) ->
    erlang:nif_error(undefined).

%% check_process_code/2
-spec check_process_code(Pid, Module) -> boolean() when
      Pid :: pid(),
      Module :: module().
check_process_code(_Pid, _Module) ->
    erlang:nif_error(undefined).

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
                     Reserved :: term(),
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

%% display/1
-spec erlang:display(Term) -> true when
      Term :: term().
display(_Term) ->
    erlang:nif_error(undefined).

%% display_nl/0
-spec erlang:display_nl() -> true.
display_nl() ->
    erlang:nif_error(undefined).

%% display_string/1
-spec erlang:display_string(P1) -> true when
      P1 :: string().
display_string(_P1) ->
    erlang:nif_error(undefined).

%% dist_exit/3
-spec erlang:dist_exit(P1, P2, P3) -> true when
      P1 :: pid(),
      P2 :: kill | noconnection | normal,
      P3 :: pid() | port().
dist_exit(_P1, _P2, _P3) ->
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
      Args :: [term()].
error(_Reason, _Args) ->
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

%% external_size/1
-spec erlang:external_size(Term) -> non_neg_integer() when
      Term :: term().
external_size(_Term) ->
    erlang:nif_error(undefined).

%% external_size/2
-spec erlang:external_size(Term, Options) -> non_neg_integer() when
      Term :: term(),
      Options :: [{minor_version, Version :: non_neg_integer()}].
external_size(_Term, _Options) ->
    erlang:nif_error(undefined).

%% finish_loading/2
-spec erlang:finish_loading(PreparedCodeBinaries) -> ok | Error when
      PreparedCodeBinaries :: [PreparedCodeBinary],
      PreparedCodeBinary :: binary(),
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

%% float_to_list/1
-spec float_to_list(Float) -> string() when
      Float :: float().
float_to_list(_Float) ->
    erlang:nif_error(undefined).

%% float_to_list/2
-spec float_to_list(Float, Options) -> string() when
      Float :: float(),
      Options :: [Option],
      Option  :: {decimals, non_neg_integer()} |
                 {scientific, non_neg_integer()} |
                 compact.
float_to_list(_Float, _Options) ->
    erlang:nif_error(undefined).

%% fun_info/2
-spec erlang:fun_info(Fun, Item) -> {Item, Info} when
      Fun :: function(),
      Item :: fun_info_item(),
      Info :: term().
fun_info(_Fun, _Item) ->
    erlang:nif_error(undefined).

%% fun_to_list/1
-spec erlang:fun_to_list(Fun) -> string() when
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
    erlang:nif_error(undefined).

%% garbage_collect/1
-spec garbage_collect(Pid) -> boolean() when
      Pid :: pid().
garbage_collect(_Pid) ->
    erlang:nif_error(undefined).

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

%% get_keys/1
-spec get_keys(Val) -> [Key] when
      Val :: term(),
      Key :: term().
get_keys(_Val) ->
    erlang:nif_error(undefined).

%% get_module_info/1
-spec erlang:get_module_info(P1) -> [{atom(), [{atom(), term()}]}] when
      P1 :: atom().
get_module_info(_P1) ->
    erlang:nif_error(undefined).

%% get_stacktrace/0
-spec erlang:get_stacktrace() -> [stack_item()].
get_stacktrace() ->
    erlang:nif_error(undefined).

%% group_leader/0
-spec group_leader() -> pid().
group_leader() ->
    erlang:nif_error(undefined).

%% group_leader/2
-spec group_leader(GroupLeader, Pid) -> true when
      GroupLeader :: pid(),
      Pid :: pid().
group_leader(_GroupLeader, _Pid) ->
    erlang:nif_error(undefined).

%% halt/0
%% Shadowed by erl_bif_types: erlang:halt/0
-spec halt() -> no_return().
halt() ->
    erlang:nif_error(undefined).

%% halt/1
%% Shadowed by erl_bif_types: erlang:halt/1
-spec halt(Status) -> no_return() when
      Status :: non_neg_integer() | 'abort' | string().
halt(_Status) ->
    erlang:nif_error(undefined).

%% halt/2
%% Shadowed by erl_bif_types: erlang:halt/2
-spec halt(Status, Options) -> no_return() when
      Status :: non_neg_integer() | 'abort' | string(),
      Options :: [Option],
      Option :: {flush, boolean()}.
halt(_Status, _Options) ->
    erlang:nif_error(undefined).

%% hash/2
-spec erlang:hash(Term, Range) -> pos_integer() when
      Term :: term(),
      Range :: pos_integer().
hash(_Term, _Range) ->
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

%% is_alive/0
-spec is_alive() -> boolean().
is_alive() ->
    erlang:nif_error(undefined).

%% is_builtin/3
-spec erlang:is_builtin(Module, Function, Arity) -> boolean() when
      Module :: module(),
      Function :: atom(),
      Arity :: arity().
is_builtin(_Module, _Function, _Arity) ->
    erlang:nif_error(undefined).

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

%% list_to_bitstr/1
-spec erlang:list_to_bitstr(P1) -> bitstring() when
      P1 :: bitstring_list().
list_to_bitstr(_P1) ->
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

%% list_to_pid/1
-spec list_to_pid(String) -> pid() when
      String :: string().
list_to_pid(_String) ->
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

%% match_spec_test/3
-spec erlang:match_spec_test(P1, P2, P3) -> TestResult when
      P1 :: [term()] | tuple(),
      P2 :: term(),
      P3 :: table | trace,
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

%% monitor/2
-spec monitor(Type, Item) -> MonitorRef when
      Type :: process,
      Item :: pid() | Module | {Module, Node},
      Module :: module(),
      Node :: node(),
      MonitorRef :: reference().
monitor(_Type, _Item) ->
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
-spec erlang:port_to_list(Port) -> string() when
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

%% prepare_loading/2
-spec erlang:prepare_loading(Module, Code) -> PreparedCode | {error, Reason} when
      Module :: module(),
      Code :: binary(),
      PreparedCode :: binary(),
      Reason :: bad_file.
prepare_loading(_Module, _Code) ->
    erlang:nif_error(undefined).

%% pre_loaded/0
-spec pre_loaded() -> [module()].
pre_loaded() ->
    erlang:nif_error(undefined).

%% process_display/2
-spec erlang:process_display(Pid, Type) -> true when
      Pid :: pid(),
      Type :: backtrace.
process_display(_Pid, _Type) ->
    erlang:nif_error(undefined).

%% process_flag/3
-spec process_flag(Pid, Flag, Value) -> OldValue when
      Pid :: pid(),
      Flag :: save_calls,
      Value :: non_neg_integer(),
      OldValue :: non_neg_integer().
process_flag(_Pid, _Flag, _Value) ->
    erlang:nif_error(undefined).

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
purge_module(_Module) ->
    erlang:nif_error(undefined).

%% put/2
-spec put(Key, Val) -> term() when
      Key :: term(),
      Val :: term().
put(_Key, _Val) ->
    erlang:nif_error(undefined).

%% raise/3
-spec erlang:raise(Class, Reason, Stacktrace) -> no_return() when
      Class :: error | exit | throw,
      Reason :: term(),
      Stacktrace :: raise_stacktrace().
raise(_Class, _Reason, _Stacktrace) ->
    erlang:nif_error(undefined).

%% read_timer/1
-spec erlang:read_timer(TimerRef) -> non_neg_integer() | false when
      TimerRef :: reference().
read_timer(_TimerRef) ->
    erlang:nif_error(undefined).

%% ref_to_list/1
-spec erlang:ref_to_list(Ref) -> string() when
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

%% seq_trace/2
-spec erlang:seq_trace(P1, P2) -> seq_trace_info_returns() | {term(), term(), term(), term(), term()} when
      P1 :: atom(),
      P2 :: boolean() | {integer(), integer()} | integer() | [].
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
-spec erlang:setnode(P1, P2, P3) -> true when
      P1 :: atom(),
      P2 :: port(),
      P3 :: {term(), term(), term(), term()}.
setnode(_P1, _P2, _P3) ->
    erlang:nif_error(undefined).

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

%% suspend_process/2
-spec erlang:suspend_process(Suspendee, OptList) -> boolean() when
      Suspendee :: pid(),
      OptList :: [Opt],
      Opt :: unless_suspending | asynchronous.
suspend_process(_Suspendee, _OptList) ->
    erlang:nif_error(undefined).

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
-spec erlang:trace(PidSpec, How, FlagList) -> integer() when
      PidSpec :: pid() | existing | new | all,
      How :: boolean(),
      FlagList :: [trace_flag()].
trace(_PidSpec, _How, _FlagList) ->
    erlang:nif_error(undefined).

%% trace_delivered/1
-spec erlang:trace_delivered(Tracee) -> Ref when
      Tracee :: pid() | all,
      Ref :: reference().
trace_delivered(_Tracee) ->
    erlang:nif_error(undefined).

%% trace_info/2
-spec erlang:trace_info(PidOrFunc, Item) -> Res when
      PidOrFunc :: pid() | new | {Module, Function, Arity} | on_load,
      Module :: module(),
      Function :: atom(),
      Arity :: arity(),
      Item :: flags | tracer | traced | match_spec | meta | meta_match_spec | call_count | call_time | all,
      Res :: trace_info_return().
trace_info(_PidOrFunc, _Item) ->
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
-spec erlang:get_module_info(Module, Item) -> ModuleInfo when
      Module :: atom(),
      Item :: module | imports | exports | functions | attributes | compile | native_addresses,
      ModuleInfo :: atom() | [] | [{atom(), arity()}] | [{atom(), term()}] | [{atom(), arity(), integer()}].
get_module_info(_Module, _Item) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:hd/1
-spec hd(List) -> term() when
      List :: [term(), ...].
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
      Reason :: badfile | not_purged | on_load.
load_module(Mod, Code) ->
    case erlang:prepare_loading(Mod, Code) of
	{error,_}=Error ->
	    Error;
	Bin when erlang:is_binary(Bin) ->
	    case erlang:finish_loading([Bin]) of
		ok ->
		    {module,Mod};
		{Error,[Mod]} ->
		    {error,Error}
	    end
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

-spec nodes(Arg) -> Nodes when
      Arg :: NodeType | [NodeType],
      NodeType :: visible | hidden | connected | this | known,
      Nodes :: [node()].
nodes(_Arg) ->
    erlang:nif_error(undefined).

-spec open_port(PortName, PortSettings) -> port() when
      PortName :: {spawn, Command :: string()} |
                  {spawn_driver, Command :: [byte()]} |
                  {spawn_executable, FileName :: file:name() } |
                  {fd, In :: non_neg_integer(), Out :: non_neg_integer()},
      PortSettings :: [Opt],
      Opt :: {packet, N :: 1 | 2 | 4}
           | stream
           | {line, L :: non_neg_integer()}
           | {cd, Dir :: string()}
           | {env, Env :: [{Name :: string(), Val :: string() | false}]}
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
	   | hide.
open_port(_PortName,_PortSettings) ->
    erlang:nif_error(undefined).

-type priority_level() ::
      low | normal | high | max.

-spec process_flag(trap_exit, Boolean) -> OldBoolean when
      Boolean :: boolean(),
      OldBoolean :: boolean();
                  (error_handler, Module) -> OldModule when
      Module :: atom(),
      OldModule :: atom();
                  (min_heap_size, MinHeapSize) -> OldMinHeapSize when
      MinHeapSize :: non_neg_integer(),
      OldMinHeapSize :: non_neg_integer();
                  (min_bin_vheap_size, MinBinVHeapSize) -> OldMinBinVHeapSize when
      MinBinVHeapSize :: non_neg_integer(),
      OldMinBinVHeapSize :: non_neg_integer();
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
      backtrace |
      binary |
      catchlevel |
      current_function |
      current_location |
      current_stacktrace |
      dictionary |
      error_handler |
      garbage_collection |
      group_leader |
      heap_size |
      initial_call |
      links |
      last_calls |
      memory |
      message_que_len |
      messages |
      min_heap_size |
      min_bin_vheap_size |
      monitored_by |
      monitors |
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
      {backtrace, Bin :: binary()} |
      {binary, BinInfo :: [{non_neg_integer(),
                            non_neg_integer(),
                            non_neg_integer()}]} |
      {catchlevel, CatchLevel :: non_neg_integer()} |
      {current_function,
       {Module :: module(), Function :: atom(), Arity :: arity()}} |
      {current_location,
       {Module :: module(), Function :: atom(), Arity :: arity(),
        Location :: [{file, Filename :: string()} | % not a stack_item()!
                     {line, Line :: pos_integer()}]}} |
      {current_stacktrace, Stack :: [stack_item()]} |
      {dictionary, Dictionary :: [{Key :: term(), Value :: term()}]} |
      {error_handler, Module :: module()} |
      {garbage_collection, GCInfo :: [{atom(),non_neg_integer()}]} |
      {group_leader, GroupLeader :: pid()} |
      {heap_size, Size :: non_neg_integer()} |
      {initial_call, mfa()} |
      {links, PidsAndPorts :: [pid() | port()]} |
      {last_calls, false | (Calls :: [mfa()])} |
      {memory, Size :: non_neg_integer()} |
      {message_que_len, MessageQueueLen :: non_neg_integer()} |
      {messages, MessageQueue :: [term()]} |
      {min_heap_size, MinHeapSize :: non_neg_integer()} |
      {min_bin_vheap_size, MinBinVHeapSize :: non_neg_integer()} |
      {monitored_by, Pids :: [pid()]} |
      {monitors,
       Monitors :: [{process, Pid :: pid() |
                                     {RegName :: atom(), Node :: node()}}]} |
      {priority, Level :: priority_level()} |
      {reductions, Number :: non_neg_integer()} |
      {registered_name, Atom :: atom()} |
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
      Dest :: dst(),
      Msg :: term().
send(_Dest,_Msg) ->
    erlang:nif_error(undefined).

-spec erlang:send(Dest, Msg, Options) -> Res when
      Dest :: dst(),
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
                    (label) -> [] | {label, non_neg_integer()};
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

-spec erlang:spawn_opt({Module, Function, Args, Options}) ->   pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(_Tuple) ->
   erlang:nif_error(undefined).

-spec statistics(context_switches) -> {ContextSwitches,0} when
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
                (reductions) -> {Total_Reductions,
                                 Reductions_Since_Last_Call} when
      Total_Reductions :: non_neg_integer(),
      Reductions_Since_Last_Call :: non_neg_integer();
                (run_queue) -> non_neg_integer();
                (runtime) -> {Total_Run_Time, Time_Since_Last_Call} when
      Total_Run_Time :: non_neg_integer(),
      Time_Since_Last_Call :: non_neg_integer();
                (scheduler_wall_time) -> [{SchedulerId, ActiveTime, TotalTime}] | undefined when
      SchedulerId :: pos_integer(),
      ActiveTime  :: non_neg_integer(),
      TotalTime   :: non_neg_integer();
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
                        (fullsweep_after, Number) -> OldNumber when
      Number :: non_neg_integer(),
      OldNumber :: non_neg_integer();
                        (min_heap_size, MinHeapSize) -> OldMinHeapSize when
      MinHeapSize :: non_neg_integer(),
      OldMinHeapSize :: non_neg_integer();
                        (min_bin_vheap_size, MinBinVHeapSize) ->
                                OldMinBinVHeapSize when
      MinBinVHeapSize :: non_neg_integer(),
      OldMinBinVHeapSize :: non_neg_integer();
                        (multi_scheduling, BlockState) -> OldBlockState when
      BlockState :: block | unblock,
      OldBlockState :: block | unblock | enabled;
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
                        (trace_control_word, TCW) -> OldTCW when
      TCW :: non_neg_integer(),
      OldTCW :: non_neg_integer();
                        %% These are deliberately not documented
			(internal_cpu_topology, term()) -> term();
                        (sequential_tracer, pid() | port() | false) -> pid() | port() | false;
                        (1,0) -> true.

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
                  {minor_version, Version :: 0..1} ].
term_to_binary(_Term, _Options) ->
    erlang:nif_error(undefined).

%% Shadowed by erl_bif_types: erlang:tl/1
-spec tl(List) -> term() when
      List :: [term(), ...].
tl(_List) ->
    erlang:nif_error(undefined).

-type trace_pattern_mfa() ::
      {atom(),atom(),arity() | '_'} | on_load.
-type trace_match_spec() ::
      [{[term()] | '_' ,[term()],[term()]}].

-spec erlang:trace_pattern(MFA, MatchSpec) -> non_neg_integer() when
      MFA :: trace_pattern_mfa(),
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause.
trace_pattern(_MFA, _MatchSpec) ->
    erlang:nif_error(undefined).

-type trace_pattern_flag() ::
      global | local |
      meta | {meta, Pid :: pid()} |
      call_count |
      call_time.

-spec erlang:trace_pattern(MFA, MatchSpec, FlagList) -> non_neg_integer() when
      MFA :: trace_pattern_mfa(),
      MatchSpec :: (MatchSpecList :: trace_match_spec())
                 | boolean()
                 | restart
                 | pause,
      FlagList :: [ trace_pattern_flag() ].
trace_pattern(_MFA, _MatchSpec, _FlagList) ->
    erlang:nif_error(undefined).

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
         (alloc_util_allocators) -> [Alloc] when
      Alloc :: atom();
         ({allocator, Alloc}) -> [_] when %% More or less anything
      Alloc :: atom();
         ({allocator_sizes, Alloc}) -> [_] when %% More or less anything
      Alloc :: atom();
         (build_type) -> opt | debug | purify | quantify | purecov |
                         gcov | valgrind | gprof | lcnt;
         (c_compiler_used) -> {atom(), term()};
         (check_io) -> [_];
         (compat_rel) -> integer();
         (cpu_topology) ->  CpuTopology when
      CpuTopology :: cpu_topology();
         ({cpu_topology, defined | detected | used}) -> CpuTopology when
      CpuTopology :: cpu_topology();
         (creation) -> integer();
         (debug_compiled) -> boolean();
         (dist) -> binary();
         (dist_ctrl) -> {Node :: node(),
                         ControllingEntity :: port() | pid()};
         (driver_version) -> string();
	 (dynamic_trace) -> none | dtrace | systemtap;
         (dynamic_trace_probes) -> boolean();
         (elib_malloc) -> false;
         (dist_buf_busy_limit) -> non_neg_integer();
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
         (min_heap_size) -> {min_heap_size, MinHeapSize :: pos_integer()};
         (min_bin_vheap_size) -> {min_bin_vheap_size,
                                  MinBinVHeapSize :: pos_integer()};
         (modified_timing_level) -> integer() | undefined;
         (multi_scheduling) -> disabled | blocked | enabled;
         (multi_scheduling_blockers) -> [PID :: pid()];
         (otp_release) -> string();
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
         (system_version) -> string();
         (system_architecture) -> string();
         (threads) -> boolean();
         (thread_pool_size) -> non_neg_integer();
         (trace_control_word) -> non_neg_integer();
         (update_cpu_info) -> changed | unchanged;
         (version) -> string();
         (wordsize | {wordsize, internal} | {wordsize, external}) -> 4 | 8.
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
    erlang:error(badarg, [F]).

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
    erlang:error(badarg, [N, F]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when erlang:is_function(F) ->
    erlang:spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when erlang:is_atom(M), erlang:is_atom(F) ->
    erlang:spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    erlang:error(badarg, [F]).

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
    erlang:error(badarg, [N, F]).

%% Spawn and atomically set up a monitor.

-spec spawn_monitor(Fun) -> {pid(), reference()} when
      Fun :: function().
spawn_monitor(F) when erlang:is_function(F, 0) ->
    erlang:spawn_opt({erlang,apply,[F,[]],[monitor]});
spawn_monitor(F) ->
    erlang:error(badarg, [F]).

-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(M, F, A) when erlang:is_atom(M),
                            erlang:is_atom(F),
                            erlang:is_list(A) ->
    erlang:spawn_opt({M,F,A,[monitor]});
spawn_monitor(M, F, A) ->
    erlang:error(badarg, [M,F,A]).

-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()} when
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(F, O) when erlang:is_function(F) ->
    spawn_opt(erlang, apply, [F, []], O);
spawn_opt({M,F}=MF, O) when erlang:is_atom(M), erlang:is_atom(F) ->
    spawn_opt(erlang, apply, [MF, []], O);
spawn_opt({M,F,A}, O) -> % For (undocumented) backward compatibility
    spawn_opt(M, F, A, O);
spawn_opt(F, O) ->
    erlang:error(badarg, [F, O]).

-spec spawn_opt(Node, Fun, Options) -> pid() | {pid(), reference()} when
      Node :: node(),
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, F, O) when N =:= erlang:node() ->
    spawn_opt(F, O);
spawn_opt(N, F, O) when erlang:is_function(F) ->
    spawn_opt(N, erlang, apply, [F, []], O);
spawn_opt(N, {M,F}=MF, O) when erlang:is_atom(M), erlang:is_atom(F) ->
    spawn_opt(N, erlang, apply, [MF, []], O);
spawn_opt(N, F, O) ->
    erlang:error(badarg, [N, F, O]).

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
    case is_well_formed_list(A) of
	true ->
	    ok;
	false ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn,M,F,A,erlang:group_leader()},
			       infinity) of
	Pid when erlang:is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {no_link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

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
    case is_well_formed_list(A) of
	true ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn_link,M,F,A,erlang:group_leader()},
			       infinity) of
	Pid when erlang:is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn_link(N,M,F,A) ->
    erlang:error(badarg, [N, M, F, A]).

-spec spawn_opt(Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(M, F, A, Opts) ->
    case catch erlang:spawn_opt({M,F,A,Opts}) of
	{'EXIT',{Reason,_}} ->
	    erlang:error(Reason, [M,F,A,Opts]);
	Res ->
	    Res
    end.

-spec spawn_opt(Node, Module, Function, Args, Options) ->
                       pid() | {pid(), reference()} when
      Node :: node(),
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor | {priority, Level}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()},
      Level :: low | normal | high.
spawn_opt(N, M, F, A, O) when N =:= erlang:node(),
			      erlang:is_atom(M), erlang:is_atom(F),
                              erlang:is_list(A), erlang:is_list(O) ->
    spawn_opt(M, F, A, O);
spawn_opt(N, M, F, A, O) when erlang:is_atom(N),
                              erlang:is_atom(M),
                              erlang:is_atom(F) ->
    case {is_well_formed_list(A), is_well_formed_list(O)} of
	{true, true} ->
	    ok;
	_ ->
	    erlang:error(badarg, [N, M, F, A, O])
    end,
    case lists:member(monitor, O) of
	false -> ok;
	true -> erlang:error(badarg, [N, M, F, A, O])
    end,
    {L,NO} = lists:foldl(fun (link, {_, NewOpts}) ->
				 {link, NewOpts};
			     (Opt, {LO, NewOpts}) ->
				 {LO, [Opt|NewOpts]}
			 end,
			 {no_link,[]},
			 O),
    case catch gen_server:call({net_kernel,N},
			       {spawn_opt,M,F,A,NO,L,erlang:group_leader()},
			       infinity) of
	Pid when erlang:is_pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {L, N, M, F, A, NO}) of
		{fault, Fault} ->
		    erlang:error(Fault, [N, M, F, A, O]);
		Pid ->
		    Pid
	    end
    end;
spawn_opt(N,M,F,A,O) ->
    erlang:error(badarg, [N,M,F,A,O]).

remote_spawn_error({'EXIT', {{nodedown,N}, _}}, {L, N, M, F, A, O}) ->
    {Opts, LL} = case L =:= link of
		     true ->
			 {[link|O], [link]};
		     false ->
			 {O, []}
		 end,
    spawn_opt(erlang,crasher,[N,M,F,A,Opts,noconnection], LL);
remote_spawn_error({'EXIT', {Reason, _}}, _) ->
    {fault, Reason};
remote_spawn_error({'EXIT', Reason}, _) ->
    {fault, Reason};
remote_spawn_error(Other, _) ->
    {fault, Other}.
    
is_well_formed_list([]) ->
    true;
is_well_formed_list([_|Rest]) ->
    is_well_formed_list(Rest);
is_well_formed_list(_) ->
    false.

crasher(Node,Mod,Fun,Args,[],Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w on ~w **~n",
			     [Mod,Fun,Args,Node]),
    erlang:exit(Reason);
crasher(Node,Mod,Fun,Args,Opts,Reason) ->
    error_logger:warning_msg("** Can not start ~w:~w,~w (~w) on ~w **~n",
			     [Mod,Fun,Args,Opts,Node]),
    erlang:exit(Reason).

-spec erlang:yield() -> 'true'.
yield() ->
    erlang:yield().

-spec nodes() -> Nodes when
      Nodes :: [node()].
nodes() ->
    erlang:nodes(visible).

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
    fun_info_1(Keys, Fun, []).

fun_info_1([K|Ks], Fun, A) ->
    case erlang:fun_info(Fun, K) of
	{K,undefined} -> fun_info_1(Ks, Fun, A);
	{K,_}=P -> fun_info_1(Ks, Fun, [P|A])
    end;
fun_info_1([], _, A) -> A.

-type dst() :: pid()
             | port()
             | (RegName :: atom())
             | {RegName :: atom(), Node :: node()}.

-spec erlang:send_nosuspend(Dest, Msg) -> boolean() when
      Dest :: dst(),
      Msg :: term().
send_nosuspend(Pid, Msg) ->
    send_nosuspend(Pid, Msg, []).

-spec erlang:send_nosuspend(Dest, Msg, Options) -> boolean() when
      Dest :: dst(),
      Msg :: term(),
      Options :: [noconnect].
send_nosuspend(Pid, Msg, Opts) ->
    case erlang:send(Pid, Msg, [nosuspend|Opts]) of
	ok -> true;
	_  -> false
    end.

-spec erlang:localtime_to_universaltime(Localtime) -> Universaltime when
      Localtime :: calendar:datetime(),
      Universaltime :: calendar:datetime().
localtime_to_universaltime(Localtime) ->
    erlang:localtime_to_universaltime(Localtime, undefined).

-spec erlang:suspend_process(Suspendee) -> 'true' when
      Suspendee :: pid().
suspend_process(P) ->
    case catch erlang:suspend_process(P, []) of
	{'EXIT', {Reason, _}} -> erlang:error(Reason, [P]);
	{'EXIT', Reason} -> erlang:error(Reason, [P]);
	Res -> Res
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
	Error -> erlang:error(Error, [Port, Data])
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
	Bool when Bool == true; Bool == false -> Bool;
	Error -> erlang:error(Error, [Port, Data, Flags])
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
	Error -> erlang:error(Error, [Port, Pid])
    end.

-spec port_close(Port) -> 'true' when
      Port :: port() | atom().

port_close(Port) ->
    case case erts_internal:port_close(Port) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	true -> true;
	Error -> erlang:error(Error, [Port])
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
	badarg -> erlang:error(badarg, [Port, Operation, Data]);
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
	Error -> erlang:error(Error, [Port, Data])
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
	Error -> erlang:error(Error, [Port, Operation, Data])
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
	badarg -> erlang:error(badarg, [Port]);
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
	badarg -> erlang:error(badarg, [Port, Item]);
	Result -> Result
    end.

-spec erlang:port_set_data(Port, Data) -> 'true' when
      Port :: port() | atom(),
      Data :: term().
    
port_set_data(Port, Data) ->
    case case erts_internal:port_set_data(Port, Data) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	badarg -> erlang:error(badarg, [Port, Data]);
	Result -> Result
    end.

-spec erlang:port_get_data(Port) -> term() when
      Port :: port() | atom().

port_get_data(Port) ->
    case case erts_internal:port_get_data(Port) of
	     Ref when erlang:is_reference(Ref) -> receive {Ref, Res} -> Res end;
	     Res -> Res
	 end of
	{ok, Data} -> Data;
	Error -> erlang:error(Error, [Port])
    end.

%%
%% If the emulator wants to perform a distributed command and
%% a connection is not established to the actual node the following 
%% functions are called in order to set up the connection and then
%% reactivate the command.
%%

-spec erlang:dlink(pid() | port()) -> 'true'.
dlink(Pid) ->
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:link(Pid);
	false -> erlang:dist_exit(erlang:self(), noconnection, Pid), true
    end.

%% Can this ever happen?
-spec erlang:dunlink(identifier()) -> 'true'.
dunlink(Pid) ->
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:unlink(Pid);
	false -> true
    end.

dmonitor_node(Node, Flag, []) ->
    case net_kernel:connect(Node) of
	true -> erlang:monitor_node(Node, Flag, []);
	false -> erlang:self() ! {nodedown, Node}, true
    end;

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

dgroup_leader(Leader, Pid) ->
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:group_leader(Leader, Pid);
	false -> true  %% bad arg ?
    end.

dexit(Pid, Reason) -> 
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:exit(Pid, Reason);
	false -> true
    end.

dsend(Pid, Msg) when erlang:is_pid(Pid) ->
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:send(Pid, Msg);
	false -> Msg
    end;
dsend(Port, Msg) when erlang:is_port(Port) ->
    case net_kernel:connect(erlang:node(Port)) of
	true -> erlang:send(Port, Msg);
	false -> Msg
    end;
dsend({Name, Node}, Msg) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg);
	false -> Msg;
	ignored -> Msg				% Not distributed.
    end.

dsend(Pid, Msg, Opts) when erlang:is_pid(Pid) ->
    case net_kernel:connect(erlang:node(Pid)) of
	true -> erlang:send(Pid, Msg, Opts);
	false -> ok
    end;
dsend(Port, Msg, Opts) when erlang:is_port(Port) ->
    case net_kernel:connect(erlang:node(Port)) of
	true -> erlang:send(Port, Msg, Opts);
	false -> ok
    end;
dsend({Name, Node}, Msg, Opts) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg, Opts);
	false -> ok;
	ignored -> ok				% Not distributed.
    end.

-spec erlang:dmonitor_p('process', pid() | {atom(),atom()}) -> reference().
dmonitor_p(process, ProcSpec) ->
    %% ProcSpec = pid() | {atom(),atom()}
    %% ProcSpec CANNOT be an atom because a locally registered process
    %% is never handled here.
    Node = case ProcSpec of
	       {S,N} when erlang:is_atom(S),
                          erlang:is_atom(N),
                          N =/= erlang:node() -> N;
	       _ when erlang:is_pid(ProcSpec) -> erlang:node(ProcSpec)
	   end,
    case net_kernel:connect(Node) of
	true ->
	    erlang:monitor(process, ProcSpec);
	false ->
	    Ref = erlang:make_ref(),
	    erlang:self() ! {'DOWN', Ref, process, ProcSpec, noconnection},
	    Ref
    end.

%%
%% Trap function used when modified timing has been enabled.
%%

-spec erlang:delay_trap(Result, timeout()) -> Result.
delay_trap(Result, 0) -> erlang:yield(), Result;
delay_trap(Result, Timeout) -> receive after Timeout -> Result end.

%%
%% The business with different in and out cookies represented
%% everywhere is discarded.
%% A node has a cookie, connections/messages to that node use that cookie.
%% Messages to us use our cookie. IF we change our cookie, other nodes 
%% have to reflect that, which we cannot forsee.
%%
-spec erlang:set_cookie(Node, Cookie) -> true when
      Node :: node(),
      Cookie :: atom().
set_cookie(Node, C) when Node =/= nonode@nohost, erlang:is_atom(Node) ->
    case erlang:is_atom(C) of
	true ->
	    auth:set_cookie(Node, C);
	false ->
	    erlang:error(badarg)
    end.

-spec erlang:get_cookie() -> Cookie | nocookie when
      Cookie :: atom().
get_cookie() ->
    auth:get_cookie().

-spec integer_to_list(Integer, Base) -> string() when
      Integer :: integer(),
      Base :: 2..36.
integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base) 
  when erlang:is_integer(I), erlang:is_integer(Base),
       Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:error(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 =:= 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.


-spec list_to_integer(String, Base) -> integer() when
      String :: string(),
      Base :: 2..36.
list_to_integer(L, 10) ->
    erlang:list_to_integer(L);
list_to_integer(L, Base)
  when erlang:is_list(L), erlang:is_integer(Base),
       Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when erlang:is_integer(I) ->
	    I;
	Fault ->
	    erlang:error(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:error(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when erlang:is_integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when erlang:is_integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when erlang:is_integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when erlang:is_integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.

%% erlang:flush_monitor_message/2 is for internal use only!
%%
%% erlang:demonitor(Ref, [flush]) traps to
%% erlang:flush_monitor_message(Ref, Res) when
%% it needs to flush a monitor message.
flush_monitor_message(Ref, Res) when erlang:is_reference(Ref),
                                     erlang:is_atom(Res) ->
    receive {_, Ref, _, _, _} -> ok after 0 -> ok end,
    Res.

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
	    erlang:error(badarg, [CpuTopology])
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

%% erlang:await_proc_exit/3 is for internal use only!
%%
%% BIFs that need to await a specific process exit before
%% returning traps to erlang:await_proc_exit/3.
%%
%% NOTE: This function is tightly coupled to
%%       the implementation of the
%%       erts_bif_prep_await_proc_exit_*()
%%       functions in bif.c. Do not make
%%       any changes to it without reading
%%       the comment about them in bif.c!
-spec erlang:await_proc_exit(dst(), 'apply' | 'data' | 'reason', term()) -> term().
await_proc_exit(Proc, Op, Data) ->
    Mon = erlang:monitor(process, Proc),
    receive
	{'DOWN', Mon, process, _Proc, Reason} ->
	    case Op of
		apply ->
		    {M, F, A} = Data,
		    erlang:apply(M, F, A);
		data ->
		    Data;
		reason ->
		    Reason
	    end
    end.

-spec min(Term1, Term2) -> Minimum when
      Term1 :: term(),
      Term2 :: term(),
      Minimum :: term().
min(A, B) when A > B -> B;
min(A, _) -> A.

-spec max(Term1, Term2) -> Maximum when
      Term1 :: term(),
      Term2 :: term(),
      Maximum :: term().
max(A, B) when A < B -> B;
max(A, _) -> A.


%%
%% erlang:memory/[0,1]
%%
%% NOTE! When updating these functions, make sure to also update
%%       erts_memory() in $ERL_TOP/erts/emulator/beam/erl_alloc.c
%%

-type memory_type() :: 'total' | 'processes' | 'processes_used' | 'system'
                     | 'atom' | 'atom_used' | 'binary' | 'code' | 'ets'
                     | 'low' | 'maximum'.

-define(CARRIER_ALLOCS, [mseg_alloc, sbmbc_alloc, sbmbc_low_alloc]).
-define(LOW_ALLOCS, [sbmbc_low_alloc, ll_low_alloc, std_low_alloc]).
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
		 ets = 0,
		 low = 0,
		 maximum = 0}).

-spec erlang:memory() -> [{Type, Size}] when
      Type :: memory_type(),
      Size :: non_neg_integer().
memory() ->
    case aa_mem_data(au_mem_data(?ALL_NEEDED_ALLOCS)) of
	notsup ->
	    erlang:error(notsup);
	Mem ->
	    InstrTail = case Mem#memory.maximum of
			    0 -> [];
			    _ -> [{maximum, Mem#memory.maximum}]
			end,
	    Tail = case Mem#memory.low of
		       0 -> InstrTail;
		       _ -> [{low, Mem#memory.low} | InstrTail]
		   end,
	    [{total, Mem#memory.total},
	     {processes, Mem#memory.processes},
	     {processes_used, Mem#memory.processes_used},
	     {system, Mem#memory.system},
	     {atom, Mem#memory.atom},
	     {atom_used, Mem#memory.atom_used},
	     {binary, Mem#memory.binary},
	     {code, Mem#memory.code},
	     {ets, Mem#memory.ets} | Tail]
    end.

-spec erlang:memory(Type :: memory_type()) -> non_neg_integer();
                   (TypeList :: [memory_type()]) -> [{memory_type(), non_neg_integer()}].
memory(Type) when erlang:is_atom(Type) ->
    {AA, ALCU, ChkSup, BadArgZero} = need_mem_info(Type),
    case get_mem_data(ChkSup, ALCU, AA) of
	notsup ->
	    erlang:error(notsup, [Type]);
	Mem ->
	    Value = get_memval(Type, Mem),
	    case {BadArgZero, Value} of
		{true, 0} -> erlang:error(badarg, [Type]);
		_ -> Value
	    end
    end;
memory(Types) when erlang:is_list(Types) ->
    {AA, ALCU, ChkSup, BadArgZeroList} = need_mem_info_list(Types),
    case get_mem_data(ChkSup, ALCU, AA) of
	notsup ->
	    erlang:error(notsup, [Types]);
	Mem ->
	    case memory_result_list(Types, BadArgZeroList, Mem) of
		badarg -> erlang:error(badarg, [Types]);
		Result -> Result
	    end
    end.

memory_result_list([], [], _Mem) ->
    [];
memory_result_list([T|Ts], [BAZ|BAZs], Mem) ->
    case memory_result_list(Ts, BAZs, Mem) of
	badarg -> badarg;
	TVs ->
	    V = get_memval(T, Mem),
	    case {BAZ, V} of
		{true, 0} -> badarg;
		_ -> [{T, V}| TVs]
	    end
    end.

get_mem_data(true, AlcUAllocs, NeedAllocatedAreas) ->
    case memory_is_supported() of
	false -> notsup;
	true -> get_mem_data(false, AlcUAllocs, NeedAllocatedAreas)
    end;
get_mem_data(false, AlcUAllocs, NeedAllocatedAreas) ->
    AlcUMem = case AlcUAllocs of
		  [] -> #memory{};
		  _ ->
		      au_mem_data(AlcUAllocs)
	      end,
    case NeedAllocatedAreas of
	true -> aa_mem_data(AlcUMem);
	false -> AlcUMem
    end.

need_mem_info_list([]) ->
    {false, [], false, []};
need_mem_info_list([T|Ts]) ->
    {MAA, MALCU, MChkSup, MBadArgZero} = need_mem_info_list(Ts),
    {AA, ALCU, ChkSup, BadArgZero} = need_mem_info(T),
    {case AA of
	 true -> true;
	 _ -> MAA
     end,
     ALCU ++ (MALCU -- ALCU),
     case ChkSup of
	 true -> true;
	 _ -> MChkSup
     end,
     [BadArgZero|MBadArgZero]}.

need_mem_info(Type) when Type == total;
			 Type == system ->
    {true, ?ALL_NEEDED_ALLOCS, false, false};
need_mem_info(Type) when Type == processes;
			 Type == processes_used ->
    {true, [eheap_alloc, fix_alloc], true, false};
need_mem_info(Type) when Type == atom;
			 Type == atom_used;
			 Type == code ->
    {true, [], true, false};
need_mem_info(binary) ->
    {false, [binary_alloc], true, false};
need_mem_info(ets) ->
    {true, [ets_alloc], true, false};
need_mem_info(low) ->
    LowAllocs = ?LOW_ALLOCS -- ?CARRIER_ALLOCS,
    {_, _, FeatureList, _} = erlang:system_info(allocator),
    AlcUAllocs = case LowAllocs -- FeatureList of
		     [] -> LowAllocs;
		     _ -> []
		 end,
    {false, AlcUAllocs, true, true};
need_mem_info(maximum) ->
    {true, [], true, true};
need_mem_info(_) ->
    {false, [], false, true}.

get_memval(total, #memory{total = V}) -> V;
get_memval(processes, #memory{processes = V}) -> V;
get_memval(processes_used, #memory{processes_used = V}) -> V;
get_memval(system, #memory{system = V}) -> V;
get_memval(atom, #memory{atom = V}) -> V;
get_memval(atom_used, #memory{atom_used = V}) -> V;
get_memval(binary, #memory{binary = V}) -> V;
get_memval(code, #memory{code = V}) -> V;
get_memval(ets, #memory{ets = V}) -> V;
get_memval(low, #memory{low = V}) -> V;
get_memval(maximum, #memory{maximum = V}) -> V;
get_memval(_, #memory{}) -> 0.

memory_is_supported() ->
    {_, _, FeatureList, _} = erlang:system_info(allocator),
    case ((erlang:system_info(alloc_util_allocators) 
	   -- ?CARRIER_ALLOCS)
	  -- FeatureList) of
	[] -> true;
	_ -> false
    end.

get_blocks_size([{blocks_size, Sz, _, _} | Rest], Acc) ->
    get_blocks_size(Rest, Acc+Sz);
get_blocks_size([{_, _, _, _} | Rest], Acc) ->
    get_blocks_size(Rest, Acc);
get_blocks_size([], Acc) ->
    Acc.

blocks_size([{Carriers, SizeList} | Rest], Acc) when Carriers == mbcs;
						     Carriers == sbcs;
						     Carriers == sbmbcs ->
    blocks_size(Rest, get_blocks_size(SizeList, Acc));
blocks_size([_ | Rest], Acc) ->
    blocks_size(Rest, Acc);
blocks_size([], Acc) ->
    Acc.

get_fix_proc([{ProcType, A1, U1}| Rest], {A0, U0}) when ProcType == proc;
							ProcType == monitor_sh;
							ProcType == nlink_sh;
							ProcType == msg_ref ->
    get_fix_proc(Rest, {A0+A1, U0+U1});
get_fix_proc([_|Rest], Acc) ->
    get_fix_proc(Rest, Acc);
get_fix_proc([], Acc) ->
    Acc.

fix_proc([{fix_types, SizeList} | _Rest], Acc) ->
    get_fix_proc(SizeList, Acc);
fix_proc([_ | Rest], Acc) ->
    fix_proc(Rest, Acc);
fix_proc([], Acc) ->
    Acc.

is_low_alloc(_A, []) ->
    false;
is_low_alloc(A, [A|_As]) ->
    true;
is_low_alloc(A, [_A|As]) ->
    is_low_alloc(A, As).

is_low_alloc(A) ->
    is_low_alloc(A, ?LOW_ALLOCS).

au_mem_data(notsup, _) ->
    notsup;
au_mem_data(_, [{_, false} | _]) ->
    notsup;
au_mem_data(#memory{total = Tot,
		    processes = Proc,
		    processes_used = ProcU} = Mem,
	    [{eheap_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   processes = Proc+Sz,
			   processes_used = ProcU+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    ets = Ets} = Mem,
	    [{ets_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   ets = Ets+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    binary = Bin} = Mem,
	    [{binary_alloc, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   binary = Bin+Sz},
		Rest);
au_mem_data(#memory{total = Tot,
		    processes = Proc,
		    processes_used = ProcU,
		    system = Sys} = Mem,
	    [{fix_alloc, _, Data} | Rest]) ->
    {A, U} = fix_proc(Data, {0, 0}),
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   processes = Proc+A,
			   processes_used = ProcU+U,
			   system = Sys+Sz-A},
		Rest);
au_mem_data(#memory{total = Tot,
		    system = Sys,
		    low = Low} = Mem,
	    [{A, _, Data} | Rest]) ->
    Sz = blocks_size(Data, 0),
    au_mem_data(Mem#memory{total = Tot+Sz,
			   system = Sys+Sz,
			   low = case is_low_alloc(A) of
				     true -> Low+Sz;
				     false -> Low
				 end},
		Rest);
au_mem_data(EMD, []) ->
    EMD.

au_mem_data(Allocs) ->
    Ref = erlang:make_ref(),
    erlang:system_info({allocator_sizes, Ref, Allocs}),
    receive_emd(Ref).

receive_emd(_Ref, EMD, 0) ->
    EMD;
receive_emd(Ref, EMD, N) ->
    receive
	{Ref, _, Data} ->
	    receive_emd(Ref, au_mem_data(EMD, Data), N-1)
    end.

receive_emd(Ref) ->
    receive_emd(Ref, #memory{}, erlang:system_info(schedulers)).

aa_mem_data(#memory{} = Mem,
	    [{maximum, Max} | Rest]) ->
    aa_mem_data(Mem#memory{maximum = Max},
		Rest);
aa_mem_data(#memory{} = Mem,
	    [{total, Tot} | Rest]) ->
    aa_mem_data(Mem#memory{total = Tot,
			   system = 0}, % system will be adjusted later
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
		    processes_used = ProcU,
		    system = Sys} = Mem,
	    [{ProcData, Sz} | Rest]) when ProcData == bif_timer;
					  ProcData == link_lh;
					  ProcData == process_table ->
    aa_mem_data(Mem#memory{processes = Proc+Sz,
			   processes_used = ProcU+Sz,
			   system = Sys-Sz},
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
aa_mem_data(#memory{total = Tot,
		    processes = Proc,
		    system = Sys} = Mem,
	    []) when Sys =< 0 ->
    %% Instrumented runtime system -> Sys = Tot - Proc
    Mem#memory{system = Tot - Proc};
aa_mem_data(EMD, []) ->
    EMD.

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
		      erlang:system_info(schedulers),
		      mk_res_list(AList)).

mk_res_list([]) ->
    [];
mk_res_list([Alloc | Rest]) ->
    [{Alloc, []} | mk_res_list(Rest)].

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

-spec erlang:await_sched_wall_time_modifications(Ref, Result) -> boolean() when
      Ref :: reference(),
      Result :: boolean().

await_sched_wall_time_modifications(Ref, Result) ->
    sched_wall_time(Ref, erlang:system_info(schedulers)),
    Result.

-spec erlang:gather_sched_wall_time_result(Ref) -> [{pos_integer(),
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
	{Ref, SWT} -> sched_wall_time(Ref, N-1, [SWT|Acc])
    end.
