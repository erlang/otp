%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2024. All Rights Reserved.
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
-moduledoc false.
-export([format_error/2, format_bs_fail/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              erlang ->
                  format_erlang_error(F, As, Reason, Cause);
              atomics ->
                  format_atomics_error(F, As, Reason, Cause);
              counters ->
                  format_counters_error(F, As, Reason, Cause);
              persistent_term ->
                  format_pt_error(F, As);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

-spec format_bs_fail(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ErrorMap :: #{'general' => unicode:chardata()}.

format_bs_fail(Reason, [{_,_,_,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    case ErrorInfoMap of
        #{cause := {Segment0,Type,Error,Value}} ->
            Segment1 = maps:get(override_segment_position, ErrorInfoMap, Segment0),
            PrettyPrinter = maps:get(pretty_printer, ErrorInfoMap, fun possibly_truncated/1),
            Str0 = do_format_bs_fail(Reason, Type, Error, Value, PrettyPrinter),
            Str1 = io_lib:format("segment ~p of type '~ts': ~ts",
                                 [Segment1,Type,Str0]),
            Str = iolist_to_binary(Str1),
            #{general => Str, reason => <<"construction of binary failed">>};
        #{} ->
            #{}
    end.

format_atomics_error(new, [Size,Options], Reason, Cause) ->
    case Reason of
        system_limit ->
            [<<"atomics array size reached a system limit">>,
             must_be_list(Options)];
        badarg ->
            case Cause of
                badopt ->
                    SizeError = must_be_pos_int(Size),
                    [SizeError, must_be_list(Options, bad_option)];
                _ ->
                    [must_be_pos_int(Size)]
            end
    end;
format_atomics_error(Name, Args, _, _) ->
    format_atomics_error(Name, Args).

format_atomics_error(add, Args) ->
    do_atomics_operation(Args);
format_atomics_error(add_get, Args) ->
    do_atomics_operation(Args);
format_atomics_error(compare_exchange, [Ref,Index,Expected,Desired]) ->
    try atomics:info(Ref) of
        #{min := Min, max := Max, size := MaxIndex} ->
            [[], must_be_int(Index, 1, MaxIndex),
             must_be_int(Expected, Min, Max),
             must_be_int(Desired, Min, Max)]
    catch
        error:badarg ->
            [bad_atomics_ref,
             must_be_pos_int(Index),
             must_be_int(Expected),
             must_be_int(Desired)]
    end;
format_atomics_error(exchange, Args) ->
    do_atomics_operation(Args);
format_atomics_error(get, [Ref,Index]) ->
    do_atomics_operation([Ref,Index,0]);
format_atomics_error(info, [_]) ->
    [bad_atomics_ref];
format_atomics_error(put, Args) ->
    do_atomics_operation(Args);
format_atomics_error(sub, Args) ->
    do_atomics_operation(Args);
format_atomics_error(sub_get, Args) ->
    do_atomics_operation(Args);
format_atomics_error(_, _) ->
    [].

do_atomics_operation([Ref,Index,Value]) ->
    try atomics:info(Ref) of
        #{min := Min, max := Max, size := MaxIndex} ->
            [[], must_be_int(Index, 1, MaxIndex),
             must_be_int(Value, Min, Max)]
    catch
        error:badarg ->
            [bad_atomics_ref,
             must_be_pos_int(Index),
             must_be_int(Value)]
    end.

format_counters_error(new, [Size,Options], Reason, Cause) ->
    case Reason of
        system_limit ->
            [<<"counters array size reached a system limit">>,
             must_be_list(Options)];
        badarg ->
            case Cause of
                badopt ->
                    SizeError = must_be_pos_int(Size),
                    [SizeError, must_be_list(Options, bad_option)];
                _ ->
                    [must_be_pos_int(Size)]
            end
    end;
format_counters_error(Name, Args, _, _) ->
    format_counters_error(Name, Args).

format_counters_error(add, Args) ->
    do_counters_operation(Args);
format_counters_error(get, [Ref,Index]) ->
    do_counters_operation([Ref,Index,0]);
format_counters_error(info, [_]) ->
    [bad_counters_ref];
format_counters_error(put, Args) ->
    do_counters_operation(Args);
format_counters_error(sub, Args) ->
    do_counters_operation(Args).

do_counters_operation([Ref,Index,Value]) ->
    try counters:info(Ref) of
        #{size := MaxIndex} ->
            case must_be_int(Index, 1, MaxIndex) of
                [] when is_integer(Value) ->
                    [[], [], range];
                [] ->
                    [[], [], not_integer];
                IndexError ->
                    [[], IndexError]
            end
    catch
        error:badarg ->
            [bad_counters_ref,
             must_be_pos_int(Index),
             must_be_int(Value)]
    end.

format_pt_error(get, [_]) ->
    [<<"no persistent term stored with this key">>].

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
format_erlang_error(alias, [Options], _) ->
    [must_be_list(Options, bad_option)];
format_erlang_error(append, [_,_], _) ->
    [not_list];
format_erlang_error(apply, [Mod,Name,Arity], _) ->
    must_be_mf_args(Mod, Name, Arity);
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
format_erlang_error(binary_to_atom=Bif, [Bin], Cause) ->
    format_erlang_error(Bif, [Bin,utf8], Cause);
format_erlang_error(binary_to_atom, [Bin,Enc], _) ->
    DefaultError = [],                          %Can't happen.
    do_binary_to_atom(Bin, Enc, DefaultError);
format_erlang_error(binary_to_existing_atom=Bif, [Bin], Cause) ->
    format_erlang_error(Bif, [Bin,utf8], Cause);
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
format_erlang_error(binary_to_term, [Bin,Options], Cause) ->
    Arg1 = must_be_binary(Bin),
    Arg2 = case Cause of
               badopt ->
                   must_be_list(Options, bad_option);
               _ ->
                   []
           end,
    case {Arg1,Arg2} of
        {[],[]} ->
            case lists:member(safe, Options) of
                true ->
                    [bad_or_unsafe_ext_term];
                false ->
                    [bad_ext_term]
            end;
        {_,_} ->
            [Arg1,Arg2]
    end;
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
    [Arg1,maybe_option_list_error(Options, Arg1)];
format_erlang_error(ceil, [_], _) ->
    [not_number];
format_erlang_error(check_old_code, [_], _) ->
    [not_atom];
format_erlang_error(check_process_code, [Pid,Module], _) ->
    [must_be_local_pid(Pid),must_be_atom(Module)];
format_erlang_error(check_process_code, [Pid,Module,_Options], Cause) ->
    format_erlang_error(check_process_code, [Pid,Module], Cause) ++
        [case Cause of
             bad_option -> bad_option;
             _ -> []
         end];
format_erlang_error(convert_time_unit, [Time,FromUnit,ToUnit], _) ->
    [must_be_int(Time),
     must_be_time_unit(FromUnit),
     must_be_time_unit(ToUnit)];
format_erlang_error(crc32, Args, Cause) ->
    format_erlang_error(adler32, Args, Cause);
format_erlang_error(crc32_combine, Args, Cause) ->
    format_erlang_error(adler32_combine, Args, Cause);
format_erlang_error(decode_packet, [_,Bin,Options], Cause) ->
    Arg2 = must_be_binary(Bin),
    Arg3 = maybe_option_list_error(Options, Arg2),
    case Cause of
        badopt ->
            [<<"invalid packet type">>, Arg2, Arg3];
        none ->
            [[], Arg2, Arg3]
    end;
format_erlang_error(delete_element, Args, Cause) ->
    format_erlang_error(element, Args, Cause);
format_erlang_error(delete_module, [_], _) ->
    [not_atom];
format_erlang_error(demonitor, [_], _) ->
    [not_ref];
format_erlang_error(demonitor, [Ref,Options], _) ->
    Arg1 = must_be_ref(Ref),
    [Arg1,maybe_option_list_error(Options, Arg1)];
format_erlang_error(display_string, [_], none) ->
    [not_string];
format_erlang_error(display_string, [_], Cause) ->
    maybe_posix_message(Cause, false);
format_erlang_error(display_string, [Device, _], none) ->
    case lists:member(Device,[stdin,stdout,stderr]) of
        true ->
            [[],not_string];
        false ->
            [not_device,[]]
    end;
format_erlang_error(display_string, [_, _], Cause) ->
    maybe_posix_message(Cause, true);
format_erlang_error(element, [Index, Tuple], _) ->
    [if
         not is_integer(Index) ->
             not_integer;
         Index =< 0; Index > tuple_size(Tuple) ->
             range;
         true ->
             []
     end,
     must_be_tuple(Tuple)];
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
    [Arg1,maybe_option_list_error(Options, Arg1)];
format_erlang_error(float_to_list, [_], _) ->
    [not_float];
format_erlang_error(float_to_list, [Float,Options], _) ->
    Arg1 = must_be_float(Float),
    [Arg1,maybe_option_list_error(Options, Arg1)];
format_erlang_error(floor, [_], _) ->
    [not_number];
format_erlang_error(function_exported, [M,F,A], _) ->
    [must_be_atom(M),must_be_atom(F),must_be_non_neg_int(A)];
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
format_erlang_error(garbage_collect, [Pid], _) ->
    [must_be_local_pid(Pid)];
format_erlang_error(garbage_collect, [Pid,_], Cause) ->
    [must_be_local_pid(Pid),
     case Cause of
         bad_option -> bad_option;
         _ -> []
     end];
format_erlang_error(get_cookie, [Node], _) ->
    [must_be_atom(Node)];
format_erlang_error(group_leader, [Pid1,Pid2], _) ->
    [must_be_pid(Pid1),must_be_pid(Pid2)];
format_erlang_error(halt, [_], _) ->
    [bad_status];
format_erlang_error(halt, [_Status,Options], Cause) ->
    case Cause of
        badopt ->
            [[],must_be_list(Options, bad_option)];
        none ->
            [bad_status]
    end;
format_erlang_error(hibernate, [M,F,A], _) ->
    must_be_mf_args(M, F, A);
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
    must_be_mf_arity(M, F, A);
format_erlang_error(is_function, [_,Arity], _) ->
    [[],
     if
         is_integer(Arity) -> range;
         true -> not_integer
     end];
format_erlang_error(is_map_key, [_,_], _) ->
    [[],not_map];
format_erlang_error(is_process_alive, [Arg], _) ->
    [must_be_local_pid(Arg)];
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
format_erlang_error(load_module, [Module,Code], _) ->
    [must_be_atom(Module),must_be_binary(Code)];
format_erlang_error(localtime_to_universaltime, [Time], _) ->
    [must_be_localtime(Time)];
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
    [Arg1,[],maybe_option_list_error(InitList, Arg1)];
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
format_erlang_error(memory, [Options], _) ->
    if
        length(Options) >= 0 ->
            [bad_option];
        is_atom(Options) ->
            [<<"invalid memory type option">>];
        true ->
            [<<"not an atom or a list of atoms">>]
    end;
format_erlang_error(module_loaded, [_], _) ->
    [not_atom];
format_erlang_error(monitor, [Type,Item], Cause) ->
    case Cause of
        badtype ->
            [<<"invalid monitor type">>];
        none ->
            case Type of
                port ->
                    [[],must_be_local_port(Item)];
                process ->
                    [[],must_be_pid(Item)]
            end
    end;
format_erlang_error(monitor, [Type,Item,Options], Cause) ->
    ItemError = case Type of
                    port ->
                        must_be_local_port(Item);
                    process ->
                        must_be_pid(Item);
                    _ ->
                        []
                end,
    case Cause of
        badopt ->
            [[],ItemError,must_be_list(Options, bad_option)];
        badtype ->
            [<<"invalid monitor type">>];
        none ->
            [[],ItemError]
    end;
format_erlang_error(monitor_node, [Node,Flag], _) ->
    [must_be_atom(Node),must_be_boolean(Flag)];
format_erlang_error(monitor_node, [Node,Flag,Options], Cause) ->
    Arg3 = case Cause of
               badopt -> bad_option;
               _ -> []
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
format_erlang_error(nodes, [NTVal], _) when is_atom(NTVal) ->
    [<<"not a valid node type">>];
format_erlang_error(nodes, [NTVal], _) when is_list(NTVal) ->
    [<<"not a list of valid node types">>];
format_erlang_error(nodes, [_NTVal], _) ->
    [<<"not a valid node type or list of valid node types">>];
format_erlang_error(nodes, [NTVal, Opts], _) ->
    ValidNodeTypes = [this, connected, visible, hidden, known],
    [if is_atom(NTVal) ->
             case lists:member(NTVal, ValidNodeTypes) of
                 true -> [];
                 false -> <<"not a valid node type">>
             end;
        is_list(NTVal) ->
             try
                 lists:foreach(
                   fun (NT) ->
                           case lists:member(NT, ValidNodeTypes) of
                               true -> [];
                               false -> throw(invalid)
                           end
                   end,
                   NTVal),
                 []
             catch
                 throw:invalid ->
                     <<"not a list of valid node types">>
             end;
        true ->
             <<"not a valid node type or list of valid node types">>
     end,
     if is_map(Opts) ->
             try
                 maps:foreach(
                   fun (connection_id, Bool) when is_boolean(Bool) ->
                           ok;
                       (node_type, Bool) when is_boolean(Bool) ->
                           ok;
                       (_, _) ->
                           throw(invalid)
                   end,
                   Opts),
                 []
             catch
                 throw:invalid ->
                     <<"invalid options in map">>
             end;
        true ->
             not_map
     end];
format_erlang_error(open_port, [Name, Settings], Cause) ->
    case Cause of
        badopt ->
            [must_be_tuple(Name),bad_option];
        _ when is_tuple(Name) ->
            case lists:keysearch(args, 1, Settings) of
                {value,_} when element(1,Name) =/= spawn_executable ->
                    [<<"must be spawn_executable">>];
                _ ->
                    [<<"invalid port name">>]
            end;
        _ ->
            must_be_tuple(Name)
    end;
format_erlang_error(phash, [_,N], _) ->
    [[], must_be_pos_int(N)];
format_erlang_error(phash2, [_,N], _) ->
    [[], must_be_pos_int(N)];
format_erlang_error(posixtime_to_universaltime, [_], _) ->
    [not_integer];
format_erlang_error(pid_to_list, [_], _) ->
    [not_pid];
format_erlang_error(port_call, [Port,Operation,_Data], _) ->
    [must_be_local_port(Port),
     must_be_operation(Operation)];
format_erlang_error(port_close, [Port], _) ->
    [must_be_local_port(Port)];
format_erlang_error(port_command, [Port,Command], _) ->
    [must_be_local_port(Port),must_be_iodata(Command)];
format_erlang_error(port_command, [Port,Command,Options], Cause) ->
    case Cause of
        badopt ->
            [must_be_local_port(Port),
             must_be_iodata(Command),
             must_be_list(Options, bad_option)];
        _ ->
            [must_be_local_port(Port),must_be_iodata(Command)]
    end;
format_erlang_error(port_connect, [Port,Pid], _) ->
    [must_be_local_port(Port),must_be_local_pid(Pid)];
format_erlang_error(port_control, [Port,Operation,Data], _) ->
    [must_be_local_port(Port),
     must_be_operation(Operation),
     must_be_iodata(Data)];
format_erlang_error(port_info, [Port], _) ->
    [must_be_local_port(Port)];
format_erlang_error(port_info, [Port,_], Cause) ->
    case Cause of
        badtype ->
            [must_be_local_port(Port)];
        _ ->
            [must_be_local_port(Port),bad_option]
    end;
format_erlang_error(port_to_list, [_], _) ->
    [not_port];
format_erlang_error(prepare_loading, [Module,Code], _) ->
    [must_be_atom(Module),must_be_binary(Code)];
format_erlang_error(process_display, [Pid,_], Cause) ->
    case Cause of
        badopt ->
            [must_be_local_pid(Pid),<<"invalid value">>];
        _ ->
            [must_be_local_pid(Pid, dead_process)]
    end;
format_erlang_error(process_flag, [_,_], Cause) ->
    case Cause of
        badopt ->
            [<<"invalid process flag">>];
        _ ->
            [[],<<"invalid value for this process flag">>]
    end;
format_erlang_error(process_flag, [Pid,Option,_], Cause) ->
    OptionError = case Option of
                      save_calls -> [];
                      _ -> <<"invalid process flag">>
                  end,
    case Cause of
        badtype ->
            [must_be_local_pid(Pid, dead_process),OptionError];
        _ ->
            case {must_be_local_pid(Pid),OptionError} of
                {[],[]} ->
                    [[],[],<<"invalid value for process flag 'save_calls'">>];
                {PidError,_} ->
                    [PidError,OptionError]
            end
    end;
format_erlang_error(process_info, [Pid], _) ->
    [must_be_local_pid(Pid)];
format_erlang_error(process_info, [Pid,_What], _) ->
    Arg1 = must_be_local_pid(Pid),
    case Arg1 of
        [] ->
            [[],<<"invalid item or item list">>];
        _ ->
            [Arg1]
    end;
format_erlang_error(purge_module, [Module], _) ->
    [must_be_atom(Module)];
format_erlang_error(read_timer, [_], _) ->
    [not_ref];
format_erlang_error(read_timer, [Ref,Options], _) ->
    Arg1 = must_be_ref(Ref),
    [Arg1,maybe_option_list_error(Options, Arg1)];
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
                     is_pid(PidOrPort), node(PidOrPort) =/= node() ->
                         not_local_pid;
                     is_port(PidOrPort), node(PidOrPort) =/= node() ->
                         not_local_port;
                     is_pid(PidOrPort) -> [];
                     is_port(PidOrPort) -> [];
                     true -> <<"not a pid or port">>
                 end],
            case Errors of
                [[],[]] ->
                    [<<"name is in use">>];
                [_,_] ->
                    Errors
            end
    end;
format_erlang_error(resume_process, [Pid], _) ->
    [must_be_local_pid(Pid, <<"process is not suspended or is not alive">>)];
format_erlang_error(round, [_], _) ->
    [not_number];
format_erlang_error(send, [_,_], _) ->
    [bad_destination];
format_erlang_error(send, [_,_,Options], Cause) ->
    case Cause of
        badopt ->
            [[],[],must_be_list(Options, bad_option)];
        _ ->
            [bad_destination]
    end;
format_erlang_error(send_after, Args, Cause) ->
    format_erlang_error(start_timer, Args, Cause);
format_erlang_error(send_nosuspend, [_,_], _) ->
    [bad_destination];
format_erlang_error(send_nosuspend, [_,_,Options], Cause) ->
    case Cause of
        badopt ->
            [[],[],must_be_list(Options, bad_option)];
        _ ->
            [bad_destination]
    end;
format_erlang_error(set_cookie, [Cookie], _) ->
    [must_be_atom(Cookie)];
format_erlang_error(set_cookie, [Node, Cookie], _) ->
    [must_be_live_node(Node), must_be_atom(Cookie)];
format_erlang_error(setelement, [Index,Tuple,_], Cause) ->
    format_erlang_error(element, [Index,Tuple], Cause);
format_erlang_error(size, [_], _) ->
    [<<"not tuple or binary">>];
format_erlang_error(spawn, [_], _) ->
    [not_fun];
format_erlang_error(spawn, [N,F], _) ->
    must_be_node_fun(N, F);
format_erlang_error(spawn, [M,F,A], _) ->
    must_be_mf_args(M, F, A);
format_erlang_error(spawn, [N,M,F,A], _) ->
    must_be_node_mf_args(N, M, F, A);
format_erlang_error(spawn_link, [_], _) ->
    [not_fun];
format_erlang_error(spawn_link, [N,F], _) ->
    must_be_node_fun(N, F);
format_erlang_error(spawn_link, [M,F,A], _) ->
    must_be_mf_args(M, F, A);
format_erlang_error(spawn_link, [N,M,F,A], _) ->
    must_be_node_mf_args(N, M, F, A);
format_erlang_error(spawn_monitor, [_], _) ->
    [not_fun];
format_erlang_error(spawn_monitor, [N,F], _) ->
    must_be_node_fun(N, F);
format_erlang_error(spawn_monitor, [M,F,A], _) ->
    must_be_mf_args(M, F, A);
format_erlang_error(spawn_monitor, [N,M,F,A], _) ->
    must_be_node_mf_args(N, M, F, A);
format_erlang_error(spawn_opt, [Fun,Options], Cause) ->
    [must_be_fun(Fun) |
     [case Cause of
          badopt ->
              must_be_list(Options, <<"invalid spawn option">>);
          none ->
              []
      end]];
format_erlang_error(spawn_opt, [Node,Fun,Options], Cause) ->
    [must_be_atom(Node),must_be_fun(Fun) |
     [case Cause of
          badopt ->
              must_be_list(Options, <<"invalid spawn option">>);
          none ->
              []
      end]];
format_erlang_error(spawn_opt, [M,F,A,Options], Cause) ->
    must_be_mf_args(M, F, A) ++
        [case Cause of
             badopt ->
                 must_be_list(Options, <<"invalid spawn option">>);
             none ->
                 []
         end];
format_erlang_error(spawn_opt, [N,M,F,A,Options], Cause) ->
    must_be_node_mf_args(N, M, F, A) ++
        [case Cause of
             badopt ->
                 must_be_list(Options, <<"invalid spawn option">>);
             none ->
                 []
         end];
format_erlang_error(spawn_request, [_], _) ->
    [not_fun];
format_erlang_error(spawn_request, [Fun,_Options], Cause) when is_function(Fun) ->
    %% spawn_request(Fun, Options)
    case Cause of
        badopt ->
            [[],bad_option];
        _ ->
            %% Should not happen.
            []
    end;
format_erlang_error(spawn_request, [_Node,Fun], _) when is_function(Fun) ->
    %% spawn_request(Node, Fun)
    [not_atom];
format_erlang_error(spawn_request, [Node,_BadFun], _) when is_atom(Node) ->
    %% Assume spawn_request(Node, BadFun).
    [[],not_fun];
format_erlang_error(spawn_request, [_,_], _) ->
    %% No idea what was meant.
    [<<"not a fun or an atom">>];
format_erlang_error(spawn_request, [N,F,O], Cause) when is_function(F) ->
    %% spawn_request(Node, Fun, Options)
    case Cause of
        badopt ->
            [must_be_atom(N),[],must_be_list(O, bad_option)];
        _ ->
            [must_be_atom(N),[],must_be_list(O, [])]
    end;
format_erlang_error(spawn_request, [N,F,O], Cause) when is_function(F) ->
    %% spawn_request(Node, Fun, Options)
    case Cause of
        badopt ->
            [must_be_atom(N),[],must_be_option_list(O)];
        _ ->
            NodeError = must_be_atom(N),
            [NodeError,[],maybe_option_list_error(O, NodeError)]
    end;
format_erlang_error(spawn_request, [M,F,A], _) ->
    %% spawn_request(Module, Function, Arguments)
    must_be_mf_args(M, F, A);
format_erlang_error(spawn_request, [N,M,F,A], _) when is_atom(F) ->
    %% spawn_request(Node, Module, Function, Arguments)
    must_be_node_mf_args(N, M, F, A);
format_erlang_error(spawn_request, [M,F,A,_Opts], Cause) ->
    %% spawn_request(Module, Function, Arguments, Options)
    case Cause of
        badopt ->
            must_be_mf_args(M, F, A) ++ [bad_option];
        _ ->
            must_be_mf_args(M, F, A)
    end;
format_erlang_error(spawn_request, [N,M,F,A,_Opts], Cause) ->
    case Cause of
        badopt ->
            must_be_node_mf_args(N, M, F, A) ++ [bad_option];
        _ ->
            must_be_node_mf_args(N, M, F, A)
    end;
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
format_erlang_error(start_timer, [Time,Process,_], Cause) ->
    [must_be_time(Time, Cause),
     if
         is_pid(Process), node(Process) =/= node() -> not_local_pid;
         is_pid(Process), node(Process) =:= node(); is_atom(Process) -> [];
         true -> <<"not a pid or an atom">>
     end];
format_erlang_error(start_timer, [A1,A2,A3,Options], Cause) ->
    format_erlang_error(start_timer, [A1,A2,A3], Cause) ++
        [case Cause of
             badopt ->
                 must_be_list(Options, bad_option);
             _ ->
                 must_be_list(Options, [])
         end];
format_erlang_error(subtract, [A,B], _) ->
    [must_be_list(A),must_be_list(B)];
format_erlang_error(suspend_process, [Pid], _) ->
    [if
         Pid =:= self() ->
             self_not_allowed;
         true ->
             must_be_local_pid(Pid, dead_process)
     end];
format_erlang_error(suspend_process, [Pid,Options], Cause) ->
    case Cause of
        badopt ->
            [must_be_local_pid(Pid, []),must_be_list(Options, bad_option)];
        _ ->
            [if
                 Pid =:= self() ->
                     self_not_allowed;
                 true ->
                     must_be_local_pid(Pid, dead_process)
             end]
    end;
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
        is_pid(Pid), node(Pid) =:= node() ->
            [[],must_be_list(Options, <<"invalid system monitor option">>)];
        is_pid(Pid) ->
            [not_local_pid,must_be_list(Options)];
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
format_erlang_error(trace, [_Session,PidOrPort,How,Options], Cause) ->
    case Cause of
        session ->
            [bad_session];
        _ ->
            [[] | format_erlang_error(trace, [PidOrPort,How,Options], Cause)]
    end;
format_erlang_error(trace, [PidOrPort,How,Options], Cause) ->
    PidOrPortError =
        if
            is_pid(PidOrPort), node(PidOrPort) =/= node() ->
                not_local_pid;
            is_port(PidOrPort), node(PidOrPort) =/= node() ->
                not_local_port;
            true ->
                []
        end,
    HowError = must_be_boolean(How),
    case Cause of
        badopt ->
            [PidOrPortError, HowError, must_be_option_list(Options)];
        _ ->
            case {HowError, PidOrPortError} of
                {[], []} ->
                    [<<"invalid spec for pid or port">>];
                _ ->
                    [PidOrPortError, HowError, []]
            end
    end;
format_erlang_error(trace_pattern, [_Session,MFA,MatchSpec,Options], Cause) ->
    case Cause of
        session ->
            [bad_session];
        _ ->
            [[] | format_erlang_error(trace_pattern, [MFA,MatchSpec,Options], Cause)]
    end;
format_erlang_error(trace_pattern=F, [_,_]=Args, Cause) ->
    [Err1,Err2|_] = format_erlang_error(F, Args ++ [[]], Cause),
    [Err1,Err2];
format_erlang_error(trace_pattern, [_,_,Options], Cause) ->
    case Cause of
        badopt ->
            [[], [], must_be_option_list(Options)];
        match_spec ->
            [[], bad_match_spec, maybe_option_list_error(Options, bad_match_spec)];
        call_count ->
            [[], [], <<"a match spec is not allowed in combination with these options">>];
        _ ->
            [<<"invalid MFA specification">>, [], []]
    end;
format_erlang_error(trace_delivered, [Pid], _) ->
    if
        is_pid(Pid), node(Pid) =/= node ->
            [not_local_pid];
        true ->
            [<<"not a pid or 'all'">>]
    end;
format_erlang_error(tuple_size, [_], _) ->
    [not_tuple];
format_erlang_error(tl, [_], _) ->
    [not_cons];
format_erlang_error(trace_info, [_Session,Tracee,Item], Cause) ->
    case Cause of
        session ->
            [bad_session];
        _ ->
            [[] | format_erlang_error(trace_info, [Tracee,Item], Cause)]
    end;
format_erlang_error(trace_info, [Tracee,_], Cause) ->
    case Cause of
        badopt ->
            if
                is_pid(Tracee), node(Tracee) =/= node() ->
                    [not_local_pid];
                is_port(Tracee), node(Tracee) =/= node() ->
                    [not_local_port];
                true ->
                    [<<"not a valid tracee specification">>]
            end;
        none ->
            [[],<<"invalid trace item">>]
    end;
format_erlang_error(trace_session_create, [Name,Tracer,Options], _) ->
    NameError = if
                    is_atom(Name) -> [];
                    true -> not_atom
                end,
    TracerError = case Tracer of
                      _ when is_pid(Tracer), node(Tracer) =:= node() -> [];
                      _ when is_port(Tracer), node(Tracer) =:= node() -> [];
                      {Mod,_} when is_atom(Mod) -> [];
                      _ -> bad_tracer
                  end,
    OptError = case Options of
                   [] -> [];
                   [_|_] -> bad_option;
                   _ -> not_list
               end,
    [NameError, TracerError, OptError];
format_erlang_error(trace_session_destroy, [_Session], _) ->
    [bad_session];
format_erlang_error(trace_session_info, [_PidPortFuncEvent], _) ->
    [<<"not a valid tracee specification">>];
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

do_format_bs_fail(system_limit, binary, binary, size, _PrettyPrinter) ->
    %% On a 32-bit system, the size of the binary is 256 MiB or
    %% more, which is not supported because the size in bits does not
    %% fit in a 32-bit signed integer. In practice, an application
    %% that uses any binaries of that size is likely to quickly run
    %% out of memory.
    io_lib:format(<<"the size of the binary/bitstring is too large (exceeding ~p bits)">>,
                  [(1 bsl 31) - 1]);
do_format_bs_fail(system_limit, _Type, size, Value, _PrettyPrinter) ->
    io_lib:format(<<"the size ~p is too large">>, [Value]);
do_format_bs_fail(badarg, Type, Info, Value, PrettyPrinter) ->
    do_format_bs_fail(Type, Info, Value, PrettyPrinter).

do_format_bs_fail(float, invalid, Value, _PrettyPrinter) ->
    io_lib:format(<<"expected one of the supported sizes 16, 32, or 64 but got: ~p">>,
                  [Value]);
do_format_bs_fail(float, no_float, Value, PrettyPrinter) ->
    io_lib:format(<<"the value ~ts is outside the range expressible as a float">>,
                  [PrettyPrinter(Value)]);
do_format_bs_fail(binary, unit, Value, PrettyPrinter) ->
    io_lib:format(<<"the size of the value ~ts is not a multiple of the unit for the segment">>,
                  [PrettyPrinter(Value)]);
do_format_bs_fail(_Type, short, Value, PrettyPrinter) ->
    io_lib:format(<<"the value ~ts is shorter than the size of the segment">>,
                  [PrettyPrinter(Value)]);
do_format_bs_fail(_Type, size, Value, PrettyPrinter) ->
    io_lib:format(<<"expected a non-negative integer as size but got: ~ts">>,
                  [PrettyPrinter(Value)]);
do_format_bs_fail(Type, type, Value, PrettyPrinter) ->
    F = <<"expected a",
          (case Type of
               binary ->
                   <<" binary">>;
               float ->
                   <<" float or an integer">>;
           integer ->
                   <<"n integer">>;
               _ ->
                   <<" non-negative integer encodable as ", (atom_to_binary(Type))/binary>>
           end)/binary, " but got: ~ts">>,
    io_lib:format(F, [PrettyPrinter(Value)]).

possibly_truncated(Int) when is_integer(Int) ->
    Bin = integer_to_binary(Int),
    case byte_size(Bin) of
        Size when Size < 48 ->
            Bin;
        Size ->
            <<Prefix:12/binary, _:(Size-24)/binary, Suffix/binary>> = Bin,
            [Prefix, <<"...">>, Suffix]
    end;
possibly_truncated(Bin) when is_bitstring(Bin) ->
    case byte_size(Bin) of
        Size when Size < 16 ->
            io_lib:format("~p", [Bin]);
        Size ->
            <<Prefix0:8/binary, _:(Size-10)/binary, Suffix0/bitstring>> = Bin,
            Prefix1 = iolist_to_binary(io_lib:format("~w", [Prefix0])),
            <<Prefix:(byte_size(Prefix1)-2)/binary,_/binary>> = Prefix1,
            <<_:2/unit:8,Suffix/binary>> = iolist_to_binary(io_lib:format("~w", [Suffix0])),
            [Prefix, <<"...,">>, Suffix]
    end;
%% possibly_truncated(Value) when is_bitstring(Value) ->
possibly_truncated(Value) ->
    io_lib:format("~P", [Value,20]).

%%%
%%% Utility functions.
%%%

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

must_be_live_node(nonode@nohost) -> not_live_node;
must_be_live_node(A) when is_atom(A) -> [];
must_be_live_node(_) -> not_atom.

must_be_base(N) when is_integer(N), 2 =< N, N =< 36 -> [];
must_be_base(_) -> bad_base.

must_be_boolean(B) when is_boolean(B) -> [];
must_be_boolean(_) -> bad_boolean.

must_be_fun(F) when is_function(F) -> [];
must_be_fun(_) -> not_fun.

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

must_be_mf_args(M, F, A) ->
    [must_be_atom(M),
     must_be_atom(F),
     must_be_list(A)].

must_be_mf_arity(M, F, A) ->
    [must_be_atom(M),
     must_be_atom(F),
     must_be_non_neg_int(A)].

must_be_node_mf_args(N, M, F, A) ->
    [must_be_atom(N)|must_be_mf_args(M, F, A)].

must_be_node_fun(N, F) ->
    [must_be_atom(N) |
     if
         is_function(F) -> [];
         true -> [not_fun]
     end].

must_be_int(N) when is_integer(N) -> [];
must_be_int(_) -> not_integer.

must_be_int(N, Min, Max) ->
    must_be_int(N, Min, Max, []).

must_be_int(N, Min, Max, Default) when is_integer(N) ->
    if
        Min =< N, N =< Max ->
            Default;
        true ->
            range
    end;
must_be_int(_, _, _, _) -> not_integer.

must_be_non_neg_int(N) ->
    must_be_int(N, 0, infinity).

must_be_pos_int(N) ->
    must_be_int(N, 1, infinity).

must_be_operation(Operation) ->
    must_be_int(Operation, 0, (1 bsl 32) - 1, []).

must_be_option_list(Options) ->
    case must_be_list(Options) of
        [] -> bad_option;
        Error -> Error
    end.

%% maybe_option_list_error(Options, PreviousError)
%%  Options is an option-list argument to be checked.
%%  PreviousError is an error term or [] for another argument.
%%
%%  If PreviousError is [], it means that there is an error in
%%  Options, and so this function will always return an error
%%  (not_list, not_proper_list, or bad_option).
%%
%%  If PreviousError is an error term, this function will return
%%  not_list or not_proper_list if Options is in error.
%%  Otherwise, it will return [].
%%
maybe_option_list_error(Options, PreviousError) ->
    case {PreviousError,must_be_list(Options)} of
        {[],[]} ->
            bad_option;
        {_,Arg2} ->
            Arg2
     end.

must_be_pid(Pid) ->
    must_be_pid(Pid, []).

must_be_pid(Pid, Error) when is_pid(Pid) -> Error;
must_be_pid(_, _) -> not_pid.

must_be_local_pid(Pid) ->
    must_be_local_pid(Pid, []).

must_be_local_pid(Pid, _Error) when is_pid(Pid), node(Pid) =/= node() ->
    not_local_pid;
must_be_local_pid(Pid, Error) when is_pid(Pid) ->
    Error;
must_be_local_pid(_Pid, _Error) ->
    not_pid.

must_be_local_port(Term) ->
    must_be_local_port(Term, []).

must_be_local_port(Port, _Error) when is_port(Port), node(Port) =/= node() ->
    not_local_port;
must_be_local_port(Port, Error) when is_port(Port); is_atom(Port) ->
    Error;
must_be_local_port(_, _) ->
    not_port.

must_be_ref(Ref) when is_reference(Ref) -> [];
must_be_ref(_) -> not_ref.

must_be_size(N) when is_integer(N) ->
    if
        N < 0 -> range;
        true -> []
    end;
must_be_size(_) -> not_integer.

must_be_time(Time, Cause) ->
    case must_be_non_neg_int(Time) of
        [] ->
            case Cause of
                time -> beyond_end_time;
                _ -> []
            end;
        Error ->
            Error
    end.

must_be_time_unit(Unit) ->
    try erlang:convert_time_unit(1, native, Unit) of
        _ ->
            []
    catch
        error:_ ->
            bad_time_unit
    end.

must_be_tuple(Term) ->
    must_be_tuple(Term, []).

must_be_tuple(Tuple, Error) when is_tuple(Tuple) -> Error;
must_be_tuple(_, _) -> not_tuple.

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
                    case unicode:characters_to_list(Bin, Enc) of
                        CharList when is_list(CharList) ->
                            [non_existing_atom];
                        _ ->
                            [bad_unicode]
                    end;
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
is_flat_char_list([]) -> true;
is_flat_char_list(_) -> false.

maybe_posix_message(Cause, HasDevice) ->
    case erl_posix_msg:message(Cause) of
        "unknown POSIX error" ++ _ ->
            unknown;
        PosixStr when HasDevice ->
            [unicode:characters_to_binary(
               io_lib:format("~ts (~tp)",[PosixStr, Cause]))];
        PosixStr when not HasDevice ->
            [{general,
              unicode:characters_to_binary(
                io_lib:format("~ts (~tp)",[PosixStr, Cause]))}]
    end.

format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([{general, E}|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum, Map#{ general => expand_error(E)});
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(bad_atomics_ref) ->
    <<"invalid atomics reference">>;
expand_error(bad_base) ->
    <<"not an integer in the range 2 through 36">>;
expand_error(bad_boolean) ->
    <<"not a boolean ('true' or 'false')">>;
expand_error(bad_counters_ref) ->
    <<"invalid atomics reference">>;
expand_error(bad_destination) ->
    <<"invalid destination">>;
expand_error(bad_encode_option) ->
    <<"not one of the atoms: latin1, utf8, or unicode">>;
expand_error(bad_ext_term) ->
    <<"invalid external representation of a term">>;
expand_error(bad_or_unsafe_ext_term) ->
    <<"invalid or unsafe external representation of a term">>;
expand_error(bad_isdst) ->
    <<"not 'true', 'false', or 'undefined'">>;
expand_error(bad_localtime) ->
    <<"not a valid local time">>;
expand_error(bad_match_spec) ->
    <<"invalid match specification">>;
expand_error(bad_option) ->
    <<"invalid option in list">>;
expand_error(bad_path) ->
    <<"not a valid path name">>;
expand_error(bad_session) ->
    <<"invalid trace session">>;
expand_error(bad_status) ->
    <<"invalid status">>;
expand_error(bad_time_unit) ->
    <<"invalid time unit">>;
expand_error(bad_tracer) ->
    <<"invalid tracer">>;
expand_error(bad_unicode) ->
    <<"invalid UTF8 encoding">>;
expand_error(bad_universaltime) ->
    <<"not a valid universal time">>;
expand_error(beyond_end_time) ->
    <<"exceeds the maximum supported time value">>;
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
expand_error(not_live_node) ->
    <<"the node name is not part of a distributed system">>;
expand_error(not_local_pid) ->
    <<"not a local pid">>;
expand_error(not_local_port) ->
    <<"not a local port">>;
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
expand_error(not_device) ->
    <<"not a valid device type">>;
expand_error(not_tuple) ->
    <<"not a tuple">>;
expand_error(range) ->
    <<"out of range">>;
expand_error(self_not_allowed) ->
    <<"the pid refers to the current process">>;
expand_error(E) when is_binary(E) ->
    E.
