%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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

-module(erl_erts_errors).
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: [term()],
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              erlang ->
                  format_erlang_error(F, As, Reason, Cause);
              atomics ->
                  format_atomics_error(F, As, Cause);
              counters ->
                  format_counters_error(F, As, Cause);
              persistent_term ->
                  format_pt_error(F, As, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

format_atomics_error(_, _, _) ->
    [].

format_counters_error(_, _, _) ->
    [].

format_pt_error(_, _, _) ->
    [].

format_erlang_error(_, _, system_limit, _) ->
    %% The explanation for system_limit is clear enough, so we don't
    %% need any detailed explanations for the arguments.
    [];
format_erlang_error(F, As, _, Cause) ->
    format_erlang_error(F, As, Cause).

format_erlang_error(abs, [_], _) ->
    [not_number];
format_erlang_error(adler32, [_], _) ->
    [not_iodata];
format_erlang_error(adler32, [Int,Data], _) ->
    [must_be_adler32(Int),must_be_iodata(Data)];
format_erlang_error(adler32_combine, [First,Second,Size], _) ->
    [must_be_adler32(First),must_be_adler32(Second),must_be_size(Size)];
format_erlang_error(append, [_,_], _) ->
    [not_list];
format_erlang_error(atom_to_binary, [_], _) ->
    [not_atom];
format_erlang_error(atom_to_binary, [Atom, Encoding], _) ->
    [if
         not is_atom(Atom) ->
             not_atom;
         Encoding =:= latin1 ->
             <<"contains a character not expressible in latin1">>;
         true ->
             []
     end,
     case lists:member(Encoding, [latin1,unicode,utf8]) of
         true -> [];
         false -> <<"is an invalid encoding option">>
     end
    ];
format_erlang_error(atom_to_list, [_], _) ->
    [not_atom];
format_erlang_error(append_element, [_,_], _) ->
    [not_tuple];
format_erlang_error(bit_size, [_], _) ->
    [not_bitstring];
format_erlang_error(binary_part, [Bin,PosLen], Cause) ->
    case PosLen of
        {Pos,Len} when is_integer(Pos), is_integer(Len) ->
            case format_erlang_error(binary_part, [Bin,Pos,Len], Cause) of
                [Arg1,[],[]] ->
                    [Arg1];
                [Arg1,_,_] ->
                    [Arg1,range]
            end;
        _ ->
            [must_be_binary(Bin),<<"not a valid {Pos,Length} tuple">>]
    end;
format_erlang_error(binary_part, [Bin,Pos,Len], _) ->
    case [must_be_binary(Bin),must_be_non_neg_int(Pos),must_be_int(Len)] of
        [[],[],[]] ->
            Arg2 = if
                       Pos > byte_size(Bin) -> range;
                       true -> []
                   end,
            case Arg2 of
                [] -> [[],[],range];
                range -> [[],Arg2]
            end;
        Errors ->
            Errors
    end;
format_erlang_error(binary_to_atom, [Bin,Enc], _) ->
    DefaultError = [],                          %Can't happen.
    do_binary_to_atom(Bin, Enc, DefaultError);
format_erlang_error(binary_to_existing_atom, [Bin,Enc], _) ->
    do_binary_to_atom(Bin, Enc, non_existing_atom);
format_erlang_error(binary_to_float, [Bin], _) ->
    [must_be_binary(Bin, {not_encodable,<<"a float">>})];
format_erlang_error(binary_to_integer, [Bin], _) ->
    [must_be_binary(Bin, {not_encodable,<<"an integer">>})];
format_erlang_error(binary_to_integer, [Bin,Base], _) ->
    case must_be_base(Base) of
        [] ->
            [must_be_binary(Bin, {not_encodable,<<"an integer">>})];
        BadBase ->
            [must_be_binary(Bin, []),BadBase]
    end;
format_erlang_error(binary_to_list, [_], _) ->
    [not_binary];
format_erlang_error(binary_to_list, [Bin,Start,Stop], _) ->
    case [must_be_binary(Bin),must_be_pos_int(Start),must_be_pos_int(Stop)] of
        [[],[],[]] ->
            if
                Start > Stop ->
                    [[],<<"start position greater than stop position">>];
                true ->
                    [[],
                     if Start > byte_size(Bin) -> range;
                        true -> []
                     end,
                     if Stop > byte_size(Bin) -> range;
                        true -> []
                     end]
            end;
        Errors ->
            Errors
    end;
format_erlang_error(binary_to_term, [Bin], _) ->
    [must_be_binary(Bin, bad_ext_term)];
format_erlang_error(binary_to_term, [Bin,Options], _) ->
    Arg1 = must_be_binary(Bin),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(bitstring_to_list, [_], _) ->
    [not_bitstring];
format_erlang_error(bump_reductions, [Int], _) ->
    [must_be_non_neg_int(Int)];
format_erlang_error(byte_size, [_], _) ->
    [not_bitstring];
format_erlang_error(cancel_timer, [_], _) ->
    [not_ref];
format_erlang_error(cancel_timer, [Ref,Options], _) ->
    Arg1 = must_be_ref(Ref),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(ceil, [_], _) ->
    [not_number];
format_erlang_error(check_old_code, [_], _) ->
    [not_atom];
format_erlang_error(crc32, Args, Cause) ->
    format_erlang_error(adler32, Args, Cause);
format_erlang_error(crc32_combine, Args, Cause) ->
    format_erlang_error(adler32_combine, Args, Cause);
format_erlang_error(decode_packet, [_,Bin,Options], Cause) ->
    Arg2 = must_be_binary(Bin),
    case Cause of
        badopt ->
            [<<"invalid packet type">>,Arg2];
        none ->
            case Arg2 of
                [] ->
                    [[],[],must_be_option_list(Options)];
                _ ->
                    [[],Arg2,must_be_list(Options)]
            end
    end;
format_erlang_error(delete_element, Args, Cause) ->
    format_erlang_error(element, Args, Cause);
format_erlang_error(delete_module, [_], _) ->
    [not_atom];
format_erlang_error(demonitor, [_], _) ->
    [not_ref];
format_erlang_error(demonitor, [Ref,Options], _) ->
    Arg1 = must_be_ref(Ref),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(display_string, [_], _) ->
    [not_string];
format_erlang_error(element, [Index, Tuple], _) ->
    [if
         not is_integer(Index) ->
             not_integer;
         Index =< 0; Index > tuple_size(Tuple) ->
             range;
         true ->
             []
     end,
     if
         not is_tuple(Tuple) -> not_tuple;
         true -> []
     end];
format_erlang_error(exit, [_,_], _) ->
    [not_pid];
format_erlang_error(exit_signal, [_,_], _) ->
    [not_pid];
format_erlang_error(external_size, [_Term,Options], _) ->
    [[],must_be_option_list(Options)];
format_erlang_error(float, [_], _) ->
    [not_number];
format_erlang_error(float_to_binary, [_], _) ->
    [not_float];
format_erlang_error(float_to_binary, [Float,Options], _) ->
    Arg1 = must_be_float(Float),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(float_to_list, [_], _) ->
    [not_float];
format_erlang_error(float_to_list, [Float,Options], _) ->
    Arg1 = must_be_float(Float),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(floor, [_], _) ->
    [not_number];
format_erlang_error(function_exported, [M,F,A], _) ->
    must_be_mfa(M, F, A);
format_erlang_error(fun_info, [_], _) ->
    [not_fun];
format_erlang_error(fun_info, [Fun,_], _) ->
    Arg1 = if
               is_function(Fun) -> [];
               true -> not_fun
           end,
    case Arg1 of
        [] ->
            [[],<<"invalid item">>];
        _ ->
            [Arg1]
    end;
format_erlang_error(fun_info_mfa, [_], _) ->
    [not_fun];
format_erlang_error(fun_to_list, [_], _) ->
    [not_fun];
format_erlang_error(halt, [_Status,Options], Cause) ->
    case Cause of
        badopt ->
            [[],must_be_list(Options, bad_option)];
        none ->
            [<<"invalid status">>]
    end;
format_erlang_error(hibernate, [M,F,A], _) ->
    must_be_mfa(M, F, A);
format_erlang_error(hd, [_], _) ->
    [not_cons];
format_erlang_error(insert_element, [Index,Tuple,_], Cause) ->
    format_erlang_error(element, [Index,Tuple], Cause);
format_erlang_error(integer_to_binary, [_], _) ->
    [not_integer];
format_erlang_error(integer_to_binary, Args, Cause) ->
    format_erlang_error(integer_to_list, Args, Cause);
format_erlang_error(integer_to_list, [_], _) ->
    [not_integer];
format_erlang_error(integer_to_list, [Int,Base], _) ->
    [if
         is_integer(Int) -> [];
         true -> not_integer
     end,
     must_be_base(Base)];
format_erlang_error(iolist_size, [_], _) ->
    [not_iodata];
format_erlang_error(iolist_to_binary, [_], _) ->
    [not_iodata];
format_erlang_error(iolist_to_iovec, [_], _) ->
    [not_iodata];
format_erlang_error(is_builtin, [M,F,A], _) ->
    must_be_mfa(M, F, A);
format_erlang_error(is_function, [_,Arity], _) ->
    [[],
     if
         is_integer(Arity) -> range;
         true -> not_integer
     end];
format_erlang_error(is_map_key, [_,_], _) ->
    [[],not_map];
format_erlang_error(is_process_alive, [_], _) ->
    [not_pid];
format_erlang_error(is_record, [_,_], _) ->
    [not_atom];
format_erlang_error(is_record, [_,Tag,Size], _) ->
    [[],must_be_atom(Tag),must_be_int(Size)];
format_erlang_error(length, [_], _) ->
    [not_list];
format_erlang_error(link, [Pid], _) ->
    if
        is_pid(Pid) -> [dead_process];
        true -> [not_pid]
    end;
format_erlang_error(list_to_atom, [List], _) ->
    [must_be_list(List, not_string)];
format_erlang_error(list_to_existing_atom, [List], _) ->
    case is_flat_char_list(List) of
        false ->
            [must_be_list(List, not_string)];
        true ->
            [non_existing_atom]
    end;
format_erlang_error(list_to_binary, [_], _) ->
    [not_iolist];
format_erlang_error(list_to_bitstring, [_], _) ->
    [<<"not a bitstring list">>];
format_erlang_error(list_to_float, [List], _) ->
    list_to_something(List, [{not_encodable,<<"a float">>}]);
format_erlang_error(list_to_integer, [List], _) ->
    list_to_something(List, [{not_encodable,<<"an integer">>}]);
format_erlang_error(list_to_integer, [List,Base], _) ->
    case must_be_base(Base) of
        [] ->
            [must_be_list(List, {not_encodable,<<"an integer">>})];
        BadBase ->
            [must_be_list(List, []),BadBase]
    end;
format_erlang_error(list_to_pid, [List], _) ->
    list_to_something(List, [{not_encodable,<<"a pid">>}]);
format_erlang_error(list_to_port, [List], _) ->
    list_to_something(List, [{not_encodable,<<"a port">>}]);
format_erlang_error(list_to_ref, [List], _) ->
    list_to_something(List, [{not_encodable,<<"a reference">>}]);
format_erlang_error(list_to_tuple, [_], _) ->
    [not_list];
format_erlang_error(localtime_to_universaltime, [Time,Bool], _) ->
    [must_be_localtime(Time),must_be_isdst(Bool)];
format_erlang_error(load_nif, [_,_], _) ->
    [bad_path];
format_erlang_error(make_fun, [Mod,Name,Arity], _) ->
    [must_be_atom(Mod),must_be_atom(Name),must_be_non_neg_int(Arity)];
format_erlang_error(make_tuple, [_,_], _) ->
    [range];
format_erlang_error(make_tuple, [Arity,_Value,InitList], _) ->
    Arg1 = must_be_non_neg_int(Arity),
    [Arg1,[],must_be_option_list(InitList, Arg1)];
format_erlang_error(map_size, [_], _) ->
    [not_map];
format_erlang_error(map_get, [_Key,Map], _) ->
    if
        is_map(Map) ->
            [<<"not present in map">>];
        true ->
            [[],not_map]
    end;
format_erlang_error(match_spec_test, [_Subject,_Ms,Type], _) ->
    case Type of
        trace ->
            [not_list];
        table ->
            %% Can't actually happen.
            [not_tuple];
        _ ->
            [[],[],<<"invalid type for match spec">>]
    end;
format_erlang_error(md5, [_], _) ->
    [not_iodata];
format_erlang_error(md5_final, [Context], _) ->
    [check_md5_context(Context)];
format_erlang_error(md5_update, [Context, Data], _) ->
    [check_md5_context(Context),
     try iolist_size(Data) of
         _ ->
             []
     catch
         error:badarg ->
             not_iodata
     end];
format_erlang_error(module_loaded, [_], _) ->
    [not_atom];
format_erlang_error(monitor, [_Type,_Item], Cause) ->
    case Cause of
        badtype ->
            [<<"invalid monitor type">>];
        none ->
            [[],<<"invalid item">>]
    end;
format_erlang_error(monitor, [_Type,_Item,Options], Cause) ->
    case Cause of
        badopt ->
            [[],[],must_be_list(Options, bad_option)];
        badtype ->
            [<<"invalid monitor type">>];
        none ->
            [[],<<"invalid item">>]
    end;
format_erlang_error(monitor_node, [Node,Flag], _) ->
    [must_be_atom(Node),must_be_boolean(Flag)];
format_erlang_error(monitor_node, [Node,Flag,Options], Cause) ->
    Arg3 = case Cause of
               badopt -> bad_option;
               true -> []
           end,
    case format_erlang_error(monitor_node, [Node,Flag], Cause) of
        [[],[]] ->
            [[],[],must_be_list(Options, Arg3)];
        Errors ->
            Errors ++ [must_be_list(Options, Arg3)]
    end;
format_erlang_error(monotonic_time, [_], _) ->
    [bad_time_unit];
format_erlang_error(node, [_], _) ->
    [not_pid];
format_erlang_error(nodes, [_], _) ->
    [<<"not a valid node type">>];
format_erlang_error(phash, [_,N], _) ->
    [must_be_pos_int(N)];
format_erlang_error(phash2, [_,N], _) ->
    [must_be_pos_int(N)];
format_erlang_error(posixtime_to_universaltime, [_], _) ->
    [not_integer];
format_erlang_error(pid_to_list, [_], _) ->
    [not_pid];
format_erlang_error(port_to_list, [_], _) ->
    [not_port];
format_erlang_error(process_flag, [_,_], Cause) ->
    case Cause of
        badopt ->
            [<<"invalid process flag">>];
        _ ->
            [[],<<"invalid value for this process flag">>]
    end;
format_erlang_error(process_info, [_], _) ->
    [not_pid];
format_erlang_error(process_info, [Pid,_What], _) ->
    Arg1 = must_be_pid(Pid),
    case Arg1 of
        [] ->
            [[],<<"invalid item or item list">>];
        _ ->
            [Arg1]
    end;
format_erlang_error(read_timer, [_], _) ->
    [not_ref];
format_erlang_error(read_timer, [Ref,Options], _) ->
    Arg1 = must_be_ref(Ref),
    [Arg1,must_be_option_list(Options, Arg1)];
format_erlang_error(ref_to_list, [_], _) ->
    [not_ref];
format_erlang_error(register, [Name,PidOrPort], Cause) ->
    case Cause of
        registered_name ->
            [[],<<"this process or port already has a name">>];
        notalive ->
            [[],dead_process];
        _ ->
            Errors =
                [if
                     Name =:= undefined -> <<"'undefined' is not a valid name">>;
                     is_atom(Name) -> [];
                     true -> not_atom
                 end,
                 if
                     is_pid(PidOrPort) -> [];
                     is_port(PidOrPort) -> [];
                     true -> <<"not a pid or a port">>
                 end],
            case Errors of
                [[],[]] ->
                    [<<"name is in use">>];
                [_,_] ->
                    Errors
            end
    end;
format_erlang_error(resume_process, [Pid], _) ->
    [must_be_pid(Pid, <<"process is not suspended or is not alive">>)];
format_erlang_error(round, [_], _) ->
    [not_number];
format_erlang_error(send, [_,_], _) ->
    [bad_destination];
format_erlang_error(send, [_,_,Options], Cause) ->
    case Cause of
        badopt ->
            [[],[],must_be_list(Options, bad_option)];
        true ->
            [bad_destination]
    end;
format_erlang_error(send_after, Args, Cause) ->
    format_erlang_error(start_timer, Args, Cause);
format_erlang_error(setelement, [Index,Tuple,_], Cause) ->
    format_erlang_error(element, [Index,Tuple], Cause);
format_erlang_error(size, [_], _) ->
    [<<"not tuple or binary">>];
format_erlang_error(spawn, [M,F,A], _) ->
    must_be_mfa(M, F, A);
format_erlang_error(spawn_link, [M,F,A], _) ->
    must_be_mfa(M, F, A);
format_erlang_error(spawn_opt, [M,F,A,Options], Cause) ->
    must_be_mfa(M, F, A) ++
        [case Cause of
             badopt ->
                 must_be_list(Options, <<"invalid spawn option">>);
             none ->
                 []
         end];
format_erlang_error(spawn_request_abandon, [_], _) ->
    [not_ref];
format_erlang_error(split_binary, [Bin,Pos], _) ->
    case [must_be_binary(Bin),must_be_non_neg_int(Pos)] of
        [[],[]] ->
            if
                Pos > byte_size(Bin) ->
                    [[],range];
                true ->
                    []
            end;
        Errors ->
            Errors
    end;
format_erlang_error(start_timer, [Time,Process,_], _) ->
    [must_be_non_neg_int(Time),
     if
         is_pid(Process); is_atom(Process) -> [];
         true -> [<<"not pid or atom">>]
     end];
format_erlang_error(start_timer, [A1,A2,A3,Options], Cause) ->
    case format_erlang_error(start_timer, [A1,A2,A3], Cause) of
        [[],[]] ->
            [[],[],must_be_list(Options, <<"invalid option list">>)];
        Errors ->
            Errors ++ [must_be_list(Options, [])]
    end;
format_erlang_error(subtract, [A,B], _) ->
    [must_be_list(A),must_be_list(B)];
format_erlang_error(system_flag, [_,_], Cause) ->
    case Cause of
        badopt ->
            [<<"invalid system flag">>];
        none ->
            [[],<<"invalid value for this system flag">>]
    end;
format_erlang_error(system_info, [_], _) ->
    [<<"invalid system info item">>];
format_erlang_error(system_monitor, [_], _) ->
    [<<"invalid system monitor item">>];
format_erlang_error(system_monitor, [Pid,Options], _) ->
    if
        is_pid(Pid) ->
            [[],must_be_list(Options, <<"invalid system monitor option">>)];
        true ->
            [not_pid,must_be_list(Options)]
    end;
format_erlang_error(system_profile, [_,_], _) ->
    [];
format_erlang_error(system_time, [_], _) ->
    [bad_time_unit];
format_erlang_error(statistics, [_], _) ->
    [<<"invalid statistics item">>];
format_erlang_error(term_to_binary, [_,Options], _) ->
    [[],must_be_option_list(Options)];
format_erlang_error(term_to_iovec, [_,Options], _) ->
    [[],must_be_option_list(Options)];
format_erlang_error(time_offset, [_], _) ->
    [bad_time_unit];
format_erlang_error(trace_delivered, [_], _) ->
    [<<"not a pid or 'all'">>];
format_erlang_error(tuple_size, [_], _) ->
    [not_tuple];
format_erlang_error(tl, [_], _) ->
    [not_cons];
format_erlang_error(trace_info, [_,_], Cause) ->
    case Cause of
        badopt ->
            [bad_option];
        none ->
            [[],<<"invalid trace item">>]
    end;
format_erlang_error(trunc, [_], _) ->
    [not_number];
format_erlang_error(tuple_to_list, [_], _) ->
    [not_tuple];
format_erlang_error(unalias, [_], _) ->
    [not_ref];
format_erlang_error(unlink, [_], _) ->
    [not_pid];
format_erlang_error(unique_integer, [Modifiers], _) ->
    [must_be_list(Modifiers, <<"invalid modifier">>)];
format_erlang_error(universaltime_to_localtime, [_], _) ->
    [bad_universaltime];
format_erlang_error(universaltime_to_posixtime, [_], _) ->
    [bad_universaltime];
format_erlang_error(unregister, [_], _) ->
    [not_pid];
format_erlang_error(whereis, [_], _) ->
    [not_atom];
format_erlang_error(_, _, _) ->
    [].

list_to_something(List, Error) ->
    try length(List) of
        _ ->
            Error
    catch
        error:badarg ->
            [not_list]
    end.

must_be_adler32(N) ->
    if
        is_integer(N) ->
            if 0 =< N, N < 1 bsl 32 -> [];
               true -> range
            end;
        true ->
            not_integer
    end.

must_be_atom(A) when is_atom(A) -> [];
must_be_atom(_) -> not_atom.

must_be_base(N) when is_integer(N), 2 =< N, N =< 36 -> [];
must_be_base(_) -> bad_base.

must_be_boolean(B) when is_boolean(B) -> [];
must_be_boolean(_) -> bad_boolean.

must_be_isdst(undefined) -> [];
must_be_isdst(B) when is_boolean(B) -> [];
must_be_isdst(_) -> bad_isdst.

must_be_binary(Bin) ->
    must_be_binary(Bin, []).

must_be_binary(Bin, Error) when is_binary(Bin) -> Error;
must_be_binary(_, _) -> not_binary.

must_be_float(Float) when is_float(Float) -> [];
must_be_float(_) -> not_float.

must_be_iodata(Data) ->
    try iolist_size(Data) of
        _ ->
            []
    catch
        error:badarg ->
            not_iodata
    end.

must_be_list(List) ->
    must_be_list(List, []).

must_be_list(List, Error) when is_list(List) ->
    try length(List) of
        _ ->
            Error
    catch
        error:badarg ->
            not_proper_list
    end;
must_be_list(_, _) ->
    not_list.

must_be_localtime(Time) ->
    try erlang:localtime_to_universaltime(Time) of
        _ -> []
    catch
        error:badarg ->
            bad_localtime
    end.

must_be_mfa(M, F, A) ->
    [must_be_atom(M),
     must_be_atom(F),
     must_be_non_neg_int(A)].

must_be_int(N) when is_integer(N) -> [];
must_be_int(_) -> not_integer.

must_be_non_neg_int(N) when is_integer(N) ->
    if
        N >= 0 -> [];
        true -> range
    end;
must_be_non_neg_int(_) -> not_integer.

must_be_pos_int(N) when is_integer(N) ->
    if
        N > 0 -> [];
        true -> range
    end;
must_be_pos_int(_) -> not_integer.

must_be_option_list(Options) ->
    must_be_option_list(Options, []).

must_be_option_list(Options, DefaultError) ->
    case {DefaultError,must_be_list(Options)} of
        {[],[]} ->
            bad_option;
        {_,Arg2} ->
            Arg2
     end.

must_be_pid(Pid) ->
    must_be_pid(Pid, []).

must_be_pid(Pid, Error) when is_pid(Pid) -> Error;
must_be_pid(_, _) -> not_pid.

must_be_ref(Ref) when is_reference(Ref) -> [];
must_be_ref(_) -> not_ref.

must_be_size(N) when is_integer(N) ->
    if
        N < 0 -> range;
        true -> []
    end;
must_be_size(_) -> not_integer.

check_md5_context(Context) when is_binary(Context) ->
    case byte_size(erlang:md5_init()) =:= byte_size(Context) of
        true ->
            [];
        false ->
            <<"invalid MD5 context">>
    end;
check_md5_context(_) ->
    not_binary.

do_binary_to_atom(Bin, Enc0, DefaultError) ->
    Enc = case Enc0 of
              latin1 -> latin1;
              unicode -> unicode;
              utf8 -> unicode;
              _ -> invalid
          end,
    case Enc of
        latin1 ->
            [must_be_binary(Bin, DefaultError)];
        unicode ->
            if
                is_binary(Bin) ->
                    [bad_unicode];
                true ->
                    [not_binary]
            end;
        invalid ->
            [must_be_binary(Bin),bad_encode_option]
    end.

is_flat_char_list([H|T]) ->
    try <<H/utf8>> of
        _ ->
            is_flat_char_list(T)
    catch
        error:badarg ->
            false
    end;
is_flat_char_list(_) -> true.

format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(bad_base) ->
    <<"not an integer in the range 2 through 36">>;
expand_error(bad_boolean) ->
    <<"not a boolean ('true' or 'false')">>;
expand_error(bad_destination) ->
    <<"invalid destination">>;
expand_error(bad_encode_option) ->
    <<"not one of the atoms: latin1, utf8, or unicode">>;
expand_error(bad_ext_term) ->
    <<"invalid external representation of a term">>;
expand_error(bad_isdst) ->
    <<"not 'true', 'false', or 'undefined'">>;
expand_error(bad_localtime) ->
    <<"not a valid local time">>;
expand_error(bad_option) ->
    <<"invalid option in list">>;
expand_error(bad_universaltime) ->
    <<"not a valid universal time">>;
expand_error(bad_path) ->
    <<"not a valid path name">>;
expand_error(bad_time_unit) ->
    <<"invalid time unit">>;
expand_error(bad_unicode) ->
    <<"invalid UTF8 encoding">>;
expand_error(dead_process) ->
    <<"the pid does not refer to an existing process">>;
expand_error({not_encodable,Type}) ->
    [<<"not a textual representation of ">>,Type];
expand_error(non_existing_atom) ->
    <<"not an already existing atom">>;
expand_error(not_atom) ->
    <<"not an atom">>;
expand_error(not_binary) ->
    <<"not a binary">>;
expand_error(not_bitstring) ->
    <<"not a bitstring">>;
expand_error(not_cons) ->
    <<"not a nonempty list">>;
expand_error(not_float) ->
    <<"not a float">>;
expand_error(not_fun) ->
    <<"not a fun">>;
expand_error(not_integer) ->
    <<"not an integer">>;
expand_error(not_iodata) ->
    <<"not an iodata term">>;
expand_error(not_iolist) ->
    <<"not an iolist term">>;
expand_error(not_list) ->
    <<"not a list">>;
expand_error(not_proper_list) ->
    <<"not a proper list">>;
expand_error(not_map) ->
    <<"not a map">>;
expand_error(not_number) ->
    <<"not a number">>;
expand_error(not_pid) ->
    <<"not a pid">>;
expand_error(not_port) ->
    <<"not a port">>;
expand_error(not_ref) ->
    <<"not a reference">>;
expand_error(not_string) ->
    <<"not a list of characters">>;
expand_error(not_tuple) ->
    <<"not a tuple">>;
expand_error(range) ->
    <<"out of range">>;
expand_error(E) -> E.
