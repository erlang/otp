%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

-module(erl_stdlib_errors).
-moduledoc false.
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              binary ->
                  format_binary_error(F, As, Cause);
              ets ->
                  format_ets_error(F, As, Cause);
              lists ->
                  format_lists_error(F, As);
              maps ->
                  format_maps_error(F, As);
              math ->
                  format_math_error(F, As);
              re ->
                  format_re_error(F, As, Cause);
              unicode ->
                  format_unicode_error(F, As);
              io ->
                  format_io_error(F, As, Cause);
              json ->
                  format_json_error(F, As, Reason, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

format_binary_error(at, [Subject,Pos], _) ->
    [must_be_binary(Subject), must_be_position(Pos)];
format_binary_error(bin_to_list, [Subject], _) ->
    [must_be_binary(Subject)];
format_binary_error(bin_to_list, Args, Cause) ->
    format_binary_error(part, Args, Cause);
format_binary_error(compile_pattern, [_], _) ->
    [<<"not a valid pattern">>];
format_binary_error(copy, [Subject], _) ->
    [must_be_binary(Subject)];
format_binary_error(copy, [Subject, N], _) ->
    [must_be_binary(Subject), must_be_non_neg_integer(N)];
format_binary_error(decode_unsigned, [Subject], _) ->
    [must_be_binary(Subject)];
format_binary_error(decode_unsigned, [Subject, Endianness], _) ->
    [must_be_binary(Subject), must_be_endianness(Endianness)];
format_binary_error(encode_unsigned, [Subject], _) ->
    [must_be_non_neg_integer(Subject)];
format_binary_error(encode_unsigned, [Subject, Endianness], _) ->
    [must_be_non_neg_integer(Subject), must_be_endianness(Endianness)];
format_binary_error(encode_hex, [Subject], _) ->
    [must_be_binary(Subject)];
format_binary_error(encode_hex, [Subject, Case], _) ->
    [must_be_binary(Subject), must_be_hex_case(Case)];
format_binary_error(decode_hex, [Subject], _) ->
    if
        is_binary(Subject) ->
            if
                byte_size(Subject) rem 2 =:= 1 ->
                    [<<"must contain an even number of bytes">>];
                true ->
                    [<<"must only contain hex digits 0-9, A-F, and a-f">>]
            end;
        true ->
            [must_be_binary(Subject)]
    end;
format_binary_error(unhex, [Subject], _) ->
    [<<(Subject)/binary, " is not a valid hex">>];
format_binary_error(first, [Subject], _) ->
    [case Subject of
         <<>> -> empty_binary;
        _ -> must_be_binary(Subject)
     end];
format_binary_error(last, [Subject], _) ->
    [case Subject of
         <<>> -> empty_binary;
        _ -> must_be_binary(Subject)
     end];
format_binary_error(join, [Binaries,Separator], _) ->
    case must_be_binary(Separator) of
        [] when is_list(Binaries) ->
            case must_be_list(Binaries) of
                [] ->
                    [~"not a list of binaries", []];
                Error ->
                    [Error, []]
            end;
        [] ->
            [must_be_list(Binaries), []];
        Error ->
            [[], Error]
    end;
format_binary_error(list_to_bin, [_], _) ->
    [not_iodata];
format_binary_error(longest_common_prefix, [_], _) ->
    [bad_binary_list];
format_binary_error(longest_common_suffix, [_], _) ->
    [bad_binary_list];
format_binary_error(match, [Subject, Pattern], _) ->
    [must_be_binary(Subject), must_be_pattern(Pattern)];
format_binary_error(match, [Subject, Pattern, Options], _) ->
    case [must_be_binary(Subject), must_be_pattern(Pattern)] of
        [[], []] ->
            case Options of
                [{scope,{Start,Len}}] when is_integer(Start),
                                           is_integer(Len) ->
                    [[], [], <<"specified part is not wholly inside binary">>];
                _ ->
                    [[], [], bad_options]
            end;
        Errors ->
            Errors
    end;
format_binary_error(matches, Args, Cause) ->
    format_binary_error(match, Args, Cause);
format_binary_error(part=Name, [Subject, PosLen], Cause) ->
    case PosLen of
        {Pos,Len} when is_integer(Pos), is_integer(Len) ->
            case format_binary_error(Name, [Subject,Pos,Len], Cause) of
                [Arg1,[],[]] ->
                    [Arg1];
                [Arg1,_,_] ->
                    [Arg1,range]
            end;
        _ ->
            [must_be_binary(Subject),<<"not a valid {Pos,Length} tuple">>]
    end;
format_binary_error(part, [Subject, Pos, Len], _) ->
    case [must_be_binary(Subject),must_be_position(Pos),must_be_integer(Len)] of
        [[],[],[]] ->
            Arg2 = if
                       Pos > byte_size(Subject) -> range;
                       true -> []
                   end,
            case Arg2 of
                [] -> [[],[],range];
                range -> [[],Arg2]
            end;
        Errors ->
            Errors
    end;
format_binary_error(referenced_byte_size, [Subject], _) ->
    [must_be_binary(Subject)];
format_binary_error(split, [Subject, Pattern], _) ->
    [must_be_binary(Subject), must_be_pattern(Pattern)];
format_binary_error(split, [Subject, Pattern, _Options], _) ->
    case [must_be_binary(Subject), must_be_pattern(Pattern)] of
        [[], []] ->
            [[], [], bad_options];
        Errors ->
            Errors
    end;
format_binary_error(replace, [Subject, Pattern, Replacement], _) ->
    [must_be_binary(Subject),
     must_be_pattern(Pattern),
     must_be_binary_replacement(Replacement)];
format_binary_error(replace, [Subject, Pattern, Replacement, _Options], Cause) ->
    Errors = format_binary_error(replace, [Subject, Pattern, Replacement], Cause),
    case Cause of
        badopt ->
            Errors ++ [bad_options];
        _ ->
            case Errors of
                [[], [], []] ->
                    %% Options are syntactically correct, but not semantically
                    %% (e.g. referencing outside the subject).
                    [[], [], [], bad_options];
                _ ->
                    Errors
            end
    end.

format_lists_error(keyfind, [_Key, Pos, List]) ->
    PosError = if
                   is_integer(Pos) ->
                       if Pos < 1 -> range;
                          true -> []
                       end;
                   true ->
                       not_integer
               end,
    [[], PosError, must_be_list(List)];
format_lists_error(keymember, Args) ->
    format_lists_error(keyfind, Args);
format_lists_error(keysearch, Args) ->
    format_lists_error(keyfind, Args);
format_lists_error(member, [_Key, List]) ->
    [[], must_be_list(List)];
format_lists_error(reverse, [List, _Acc]) ->
    [must_be_list(List)];
format_lists_error(seq, [First, Last, Inc]) ->
    case [must_be_integer(First), must_be_integer(Last), must_be_integer(Inc)] of
        [[], [], []] ->
            IncError = if
                (Inc =< 0 andalso First - Inc =< Last) ->
                    <<"not a positive increment">>;
                (Inc >= 0 andalso First - Inc >= Last) ->
                    <<"not a negative increment">>
            end,
            [[], [], IncError];
        Errors -> Errors
    end.


format_maps_error(filter, Args) ->
    format_maps_error(map, Args);
format_maps_error(filtermap, Args) ->
    format_maps_error(map, Args);
format_maps_error(foreach, Args) ->
    format_maps_error(map, Args);
format_maps_error(find, _Args) ->
    [[], not_map];
format_maps_error(fold, [Pred, _Init, Map]) ->
    [must_be_fun(Pred, 3), [], must_be_map_or_iter(Map)];
format_maps_error(from_keys, [List, _]) ->
    [must_be_list(List)];
format_maps_error(from_list, [List]) ->
    [must_be_list(List)];
format_maps_error(get, [_Key,Map]) ->
    if
        is_map(Map) ->
            [<<"not present in map">>];
        true ->
            [[],not_map]
    end;
format_maps_error(groups_from_list, [Fun, List]) ->
    [must_be_fun(Fun, 1), must_be_list(List)];
format_maps_error(groups_from_list, [Fun1, Fun2, List]) ->
    [must_be_fun(Fun1, 1), must_be_fun(Fun2, 1), must_be_list(List)];
format_maps_error(get, [_,_,_]) ->
    [[],not_map];
format_maps_error(intersect, [Map1, Map2]) ->
    [must_be_map(Map1), must_be_map(Map2)];
format_maps_error(intersect_with, [Combiner, Map1, Map2]) ->
    [must_be_fun(Combiner, 3), must_be_map(Map1), must_be_map(Map2)];
format_maps_error(is_key, _Args) ->
    [[], not_map];
format_maps_error(iterator, [Map]) ->
    [must_be_map(Map)];
format_maps_error(iterator, [Map, Order]) ->
    [must_be_map(Map), must_be_map_iterator_order(Order)];
format_maps_error(keys, _Args) ->
    [not_map];
format_maps_error(map, [Pred, Map]) ->
    [must_be_fun(Pred, 2), must_be_map_or_iter(Map)];
format_maps_error(merge, [Map1, Map2]) ->
    [must_be_map(Map1), must_be_map(Map2)];
format_maps_error(merge_with, [Combiner, Map1, Map2]) ->
    [must_be_fun(Combiner, 3), must_be_map(Map1), must_be_map(Map2)];
format_maps_error(next, _Args) ->
    [bad_iterator];
format_maps_error(put, _Args) ->
    [[], [], not_map];
format_maps_error(remove, _Args) ->
    [[], not_map];
format_maps_error(size, _Args) ->
    [not_map];
format_maps_error(take, _Args) ->
    [[], not_map];
format_maps_error(to_list, _Args) ->
    [not_map_or_iterator];
format_maps_error(update, [Key, _Value, Map]) when is_map(Map) ->
    false = is_map_key(Key, Map),               %Assertion.
    [<<"not present in map">>, [], []];
format_maps_error(update, _Args) ->
    [[], [], not_map];
format_maps_error(update_with, [_Key, Fun, Map]) ->
    [[], must_be_fun(Fun, 1), must_be_map(Map)];
format_maps_error(update_with, [_Key, Fun, _Init, Map]) ->
    [[], must_be_fun(Fun, 1), [], must_be_map(Map)];
format_maps_error(values, _Args) ->
    [not_map];
format_maps_error(with, [List, Map]) ->
    [must_be_list(List), must_be_map(Map)];
format_maps_error(without, [List, Map]) ->
    [must_be_list(List), must_be_map(Map)].

format_math_error(acos, Args) ->
    maybe_domain_error(Args);
format_math_error(acosh, Args) ->
    maybe_domain_error(Args);
format_math_error(asin, Args) ->
    maybe_domain_error(Args);
format_math_error(atanh, Args) ->
    maybe_domain_error(Args);
format_math_error(log, Args) ->
    maybe_domain_error(Args);
format_math_error(log2, Args) ->
    maybe_domain_error(Args);
format_math_error(log10, Args) ->
    maybe_domain_error(Args);
format_math_error(sqrt, Args) ->
    maybe_domain_error(Args);
format_math_error(fmod, [Arg1, Arg2]) ->
    case [must_be_number(Arg1), must_be_number(Arg2)] of
        [[], []] ->
            if
                Arg2 == 0 -> [[], domain_error];
                true -> []
            end;
        Error ->
            Error
    end;
format_math_error(_, [Arg]) ->
    [must_be_number(Arg)];
format_math_error(_, [Arg1, Arg2]) ->
    [must_be_number(Arg1), must_be_number(Arg2)].

maybe_domain_error([Arg]) ->
    case must_be_number(Arg) of
        [] -> [domain_error];
        Error -> [Error]
    end.

format_re_error(compile, [_], _) ->
    [not_iodata];
format_re_error(compile, [Re, _Options], Cause) ->
    ReError = try re:compile(Re) of
                  {ok, _} -> [];
                  {error, Reason} ->
                      {bad_regexp, Reason}
              catch
                  _:_ -> not_iodata
              end,
    case Cause of
        badopt ->
            [ReError, bad_options];
        _ ->
            [ReError]
    end;
format_re_error(inspect, [CompiledRE, Item], _) ->
    ReError = try re:inspect(CompiledRE, namelist) of
                  _ -> []
              catch
                  error:_ -> not_compiled_regexp
              end,
    if
        ReError =:= []; not is_atom(Item) ->
            [ReError, <<"not a valid item">>];
        true ->
            [ReError]
    end;
format_re_error(replace, [Subject, RE, Replacement], _) ->
    [must_be_iodata(Subject),
     must_be_regexp(RE),
     must_be_re_replacement(Replacement)];
format_re_error(replace, [Subject, RE, Replacement, _Options], Cause) ->
    Errors = [must_be_iodata(Subject),
              must_be_regexp(RE),
              must_be_re_replacement(Replacement)],
    case Cause of
        badopt ->
            Errors ++ [bad_options];
        _ ->
            Errors
    end;
format_re_error(run, [Subject, RE], _) ->
    [must_be_iodata(Subject), must_be_regexp(RE)];
format_re_error(run, [Subject, RE, _Options], Cause) ->
    Errors = [must_be_iodata(Subject), must_be_regexp(RE)],
    case Cause of
        badopt ->
            Errors ++ [bad_options];
        _ ->
            Errors
    end;
format_re_error(split, [Subject, RE], _) ->
    [must_be_iodata(Subject), must_be_regexp(RE)];
format_re_error(split, [Subject, RE, _Options], Cause) ->
    Errors = [must_be_iodata(Subject), must_be_regexp(RE)],
    case Cause of
        badopt ->
            Errors ++ [bad_options];
        _ ->
            Errors
    end.

format_unicode_error(characters_to_binary, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_binary, [Chars, InEnc]) ->
    [unicode_char_data(Chars), unicode_encoding(InEnc)];
format_unicode_error(characters_to_binary, [Chars, InEnc, OutEnc]) ->
    [unicode_char_data(Chars), unicode_encoding(InEnc), unicode_encoding(OutEnc)];
format_unicode_error(characters_to_list, Args) ->
    format_unicode_error(characters_to_binary, Args);
format_unicode_error(characters_to_nfc_binary, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfc_list, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfd_binary, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfd_list, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfkc_binary, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfkc_list, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfkd_binary, [_]) ->
    [bad_char_data];
format_unicode_error(characters_to_nfkd_list, [_]) ->
    [bad_char_data].

unicode_char_data(Chars) ->
    try unicode:characters_to_binary(Chars) of
        {error,_,_} ->
            bad_char_data;

        {incomplete,_,_} ->
            bad_char_data;

        _ ->
            []
    catch
        error:_ ->
            bad_char_data
    end.

unicode_encoding(Enc) ->
    try unicode:characters_to_binary(<<"a">>, Enc) of
        _ ->
            []
    catch
        error:_ ->
            bad_encoding
    end.


%% Rewrite fwrite to format and add info if call has an
%% explicit Io Device as first argument.
format_io_error(fwrite, Args, Cause) ->
    format_io_error(format, Args, Cause);
format_io_error(format = Fn, [_Io, _Fmt, _Args] = Args, Cause) ->
    format_io_error(Fn, Args, Cause, true);
format_io_error(put_chars = Fn, [_Io, _Chars] = Args, Cause) ->
    format_io_error(Fn, Args, Cause, true);
format_io_error(put_chars = Fn, [_Io, _Encoding, _Chars] = Args, Cause) ->
    format_io_error(Fn, Args, Cause, true);
format_io_error(nl = Fn, [_Io] = Args, Cause) ->
    format_io_error(Fn, Args, Cause, true);
format_io_error(write = Fn, [_Io, _Term] = Args, Cause) ->
    format_io_error(Fn, Args, Cause, true);
format_io_error(Fn, Args, Cause) ->
    format_io_error(Fn, Args, Cause, false).

%% The cause should always be wrapped in either `io` or
%% `device`. If it is wrapped in `io` we know that the
%% error originated in the io module. If it is wrapped
%% in `device`, the error came from the called io device.

%% arguments, whereis(Io) failed
format_io_error(_, _, {io, arguments}, true) ->
    [device_arguments];
format_io_error(_, _, {io, arguments}, false) ->
    [{general,device_arguments}];
%% calling_self, Io =:= self()
format_io_error(_, _, {io, calling_self}, true) ->
    [calling_self];
format_io_error(_, _, {io, calling_self}, false) ->
    [{general,calling_self}];
%% terminated, monitor(Io) failed
format_io_error(_, _, {io, terminated}, true) ->
    [device_terminated];
format_io_error(_, _, {io, terminated}, false) ->
    [{general,device_terminated}];
%% Strip the wrapping as we no longer need it
format_io_error(Fn, Args, {device, Cause}, HasDevice) ->
    format_io_error_cause(Fn, Args, Cause, HasDevice).

%% Could not translate from unicode to latin1
format_io_error_cause(_, _, {no_translation, In, Out}, true) ->
    [{no_translation, In, Out}];
format_io_error_cause(_, _, {no_translation, In, Out}, false) ->
    [{general,{no_translation, In, Out}}];
format_io_error_cause(format, Args, Cause, HasDevice) ->
    case maybe_posix_message(Cause, HasDevice) of
        unknown ->
            if
                HasDevice ->
                    [[]] ++ check_io_format(tl(Args), Cause);
                not HasDevice ->
                    check_io_format(Args, Cause)
            end;
        PosixError ->
            PosixError
    end;
format_io_error_cause(put_chars, Args, Cause, HasDevice) ->
    Data = if HasDevice -> hd(tl(Args)); not HasDevice -> hd(Args) end,
    case maybe_posix_message(Cause, HasDevice) of
        unknown ->
            [[] || HasDevice] ++
                case unicode_char_data(Data) of
                    [] -> [{general,{unknown_error,Cause}}];
                    InvalidData ->
                        [InvalidData]
                end;
        PosixError ->
            PosixError ++ [unicode_char_data(Data)]
    end;
format_io_error_cause(Fn, _Args, Cause, HasDevice)
  when Fn =:= write; Fn =:= nl ->
    case maybe_posix_message(Cause, HasDevice) of
        unknown ->
            [{general,{unknown_error,Cause}}];
        PosixError ->
            PosixError
    end;
format_io_error_cause(_, _, _, _HasDevice) ->
    [].

maybe_posix_message(Cause, HasDevice) ->
    case erl_posix_msg:message(Cause) of
        "unknown POSIX error" ++ _ ->
            unknown;
        PosixStr when HasDevice ->
            [io_lib:format("~ts (~tp)",[PosixStr, Cause])];
        PosixStr when not HasDevice ->
            [{general,io_lib:format("~ts (~tp)",[PosixStr, Cause])}]
    end.

check_io_format([Fmt], Cause) ->
    check_io_format([Fmt, []], Cause);
check_io_format([Fmt, Args], Cause) ->
    case is_io_format(Fmt) of
        false ->
            [invalid_format, must_be_list(Args)] ++
                case (is_pid(Fmt) or is_atom(Fmt)) and is_io_format(Args) of
                    true ->
                        %% The user seems to have called io:format(Dev,"string").
                        [{general,missing_argument_list}];
                    false ->
                        []
                end;
        _ when not is_list(Args) ->
                [[],must_be_list(Args)];
        true ->
            case erl_lint:check_format_string(Fmt, false) of
                {error,S} ->
                    [io_lib:format("format string invalid (~ts)",[S])];
                {ok,ArgTypes} when length(ArgTypes) =/= length(Args) ->
                    [<<"wrong number of arguments">>] ++
                        %% The user seems to have called io:format(user,"string")
                        [{general,missing_argument_list} || is_atom(Fmt)];
                {ok,ArgTypes} ->
                    case check_io_arguments(ArgTypes, Args) of
                        [] when Cause =:= format ->
                            [format_failed];
                        [] ->
                            try io_lib:format(Fmt, Args) of
                                _ ->
                                    [{general,{unknown_error,Cause}}]
                            catch _:_ ->
                                    [format_failed]
                            end;
                        ArgErrors ->
                            ArgErrors
                    end
            end
    end.

is_io_format(Fmt) when is_list(Fmt) ->
    try lists:all(fun erlang:is_integer/1, Fmt) of
        Res -> Res
    catch _:_ ->
            false
    end;
is_io_format(Fmt) when is_atom(Fmt); is_binary(Fmt) ->
    true;
is_io_format(_Fmt) ->
    false.

check_io_arguments(Types, Args) ->
    case check_io_arguments(Types, Args, 1) of
        [] ->
            [];
        Checks ->
            [[],lists:join("\n",Checks)]
    end.

check_io_arguments([], [], _No) ->
    [];
check_io_arguments([Type|TypeT], [Arg|ArgT], No) ->
    case Type of
	'fun' when Arg =:= undefined; Arg =:= ordered; Arg =:= reversed; is_function(Arg, 2) ->
	    check_io_arguments(TypeT, ArgT, No+1);
	'fun' ->
	    [io_lib:format("element ~B must be 'undefined', 'ordered', 'reversed', or a fun that takes two arguments", [No]) |
	     check_io_arguments(TypeT, ArgT, No+1)];
        float when is_float(Arg) ->
            check_io_arguments(TypeT, ArgT, No+1);
        int when is_integer(Arg) ->
            check_io_arguments(TypeT, ArgT, No+1);
        term ->
            check_io_arguments(TypeT, ArgT, No+1);
        string when is_atom(Arg); is_binary(Arg) ->
            check_io_arguments(TypeT, ArgT, No+1);
        string when is_list(Arg) ->
            try unicode:characters_to_binary(Arg) of
                _ ->
                    check_io_arguments(TypeT, ArgT, No+1)
            catch _:_ ->
                    [io_lib:format("element ~B must be of type ~p", [No, string]) |
                    check_io_arguments(TypeT, ArgT, No+1)]
            end;
        int ->
            [io_lib:format("element ~B must be of type ~p", [No, integer]) |
             check_io_arguments(TypeT, ArgT, No+1)];
        _ when Type =:= float; Type =:= string ->
            [io_lib:format("element ~B must be of type ~p", [No, Type]) |
             check_io_arguments(TypeT, ArgT, No+1)]
    end.

format_json_error(_F, _As, {invalid_byte, Int}, #{position := Position}) ->
    Str = if 32 =< Int, Int < 127 ->
                  io_lib:format("invalid byte 16#~2.16.0B '~c' at byte position ~w",
                                [Int, Int, Position]);
             true ->
                  io_lib:format("invalid byte 16#~2.16.0B at byte position ~w",
                                [Int, Position])
          end,
    [{general, Str}];
format_json_error(_, _, _, _) ->
    [""].

format_ets_error(delete_object, Args, Cause) ->
    format_object(Args, Cause);
format_ets_error(give_away, [_Tab,Pid,_Gift]=Args, Cause) ->
    TabCause = format_cause(Args, Cause),
    case Cause of
        owner ->
            [TabCause, already_owner];
        not_owner ->
            [TabCause, not_owner];
        _ ->
            [TabCause,
             case {is_pid(Pid),TabCause} of
                 {true,""} ->
                     dead_process;
                 {false,_} ->
                     not_pid;
                 _ ->
                     ""
             end]
    end;
format_ets_error(info, Args, Cause) ->
    format_default(bad_info_item, Args, Cause);
format_ets_error(insert, Args, Cause) ->
    format_objects(Args, Cause);
format_ets_error(insert_new, Args, Cause) ->
    format_objects(Args, Cause);
format_ets_error(lookup_element, [_,_,Pos]=Args, Cause) ->
    TabCause = format_cause(Args, Cause),
    PosCause = format_non_negative_integer(Pos),
    case Cause of
        badkey ->
            [TabCause, bad_key, PosCause];
        _ ->
            case {TabCause,PosCause} of
                {"",""} ->
                    ["", "", <<"position is greater than the size of the object">>];
                {_,_} ->
                    [TabCause, "", PosCause]
            end
    end;
format_ets_error(lookup_element, [Tab, Key, Pos, _Default], Cause) ->
    % The default argument cannot cause an error.
    format_ets_error(lookup_element, [Tab, Key, Pos], Cause);
format_ets_error(match, [_], _Cause) ->
    [bad_continuation];
format_ets_error(match, [_,_,_]=Args, Cause) ->
    format_limit(Args, Cause);
format_ets_error(match_object, [_], _Cause) ->
    [bad_continuation];
format_ets_error(match_object, [_,_,_]=Args, Cause) ->
    format_limit(Args, Cause);
format_ets_error(match_spec_compile, [_], _Cause) ->
    [bad_matchspec];
format_ets_error(next, Args, Cause) ->
    format_default(bad_key, Args, Cause);
format_ets_error(next_lookup, Args, Cause) ->
    format_default(bad_key, Args, Cause);
format_ets_error(new, [Name,Options], Cause) ->
    NameError = if
                    is_atom(Name) -> [];
                    true -> not_atom
                end,
    OptsError = must_be_list(Options),
    case {NameError, OptsError, Cause} of
        {[], [], already_exists} ->
            [name_already_exists, []];
        {[], [], _} ->
            [[], bad_options];
        {_, _, _} ->
            [NameError, OptsError]
    end;
format_ets_error(prev, Args, Cause) ->
    format_default(bad_key, Args, Cause);
format_ets_error(prev_lookup, Args, Cause) ->
    format_default(bad_key, Args, Cause);
format_ets_error(rename, [_,NewName]=Args, Cause) ->
    case [format_cause(Args, Cause),
          if
              is_atom(NewName) -> "";
              true -> bad_table_name
          end] of
        ["", ""] ->
            ["", name_already_exists];
        Result ->
            Result
    end;
format_ets_error(safe_fixtable, Args, Cause) ->
    format_default(bad_boolean, Args, Cause);
format_ets_error(select, [_], _Cause) ->
    [bad_continuation];
format_ets_error(select, [_,_]=Args, Cause) ->
    format_default(bad_matchspec, Args, Cause);
format_ets_error(select, [_,_,_]=Args, Cause) ->
    format_ms_limit(Args, Cause);
format_ets_error(select_count, [_,_]=Args, Cause) ->
    format_default(bad_matchspec, Args, Cause);
format_ets_error(select_count, [_,_,_]=Args, Cause) ->
    format_ms_limit(Args, Cause);
format_ets_error(internal_select_delete, Args, Cause) ->
    format_default(bad_matchspec, Args, Cause);
format_ets_error(select_replace, Args, Cause) ->
    format_default(bad_matchspec, Args, Cause);
format_ets_error(select_reverse, [_], _Cause) ->
    [bad_continuation];
format_ets_error(select_reverse, [_,_]=Args, Cause) ->
    format_default(bad_matchspec, Args, Cause);
format_ets_error(select_reverse, [_,_,_]=Args, Cause) ->
    format_ms_limit(Args, Cause);
format_ets_error(setopts, Args, Cause) ->
    format_default(bad_options, Args, Cause);
format_ets_error(slot, Args, Cause) ->
    format_default(range, Args, Cause);
format_ets_error(update_counter, [_,_,UpdateOp]=Args, Cause) ->
    TabCause = format_cause(Args, Cause),
    case Cause of
        badkey ->
            [TabCause, bad_key, format_update_op(UpdateOp)];
        keypos ->
            [TabCause, "", same_as_keypos];
        position ->
            [TabCause, "", update_op_range];
        none ->
            case is_update_op_top(UpdateOp) of
                false ->
                    [TabCause, "", bad_update_op];
                true ->
                    %% This is the only possible remaining error.
                    [TabCause, "", counter_not_integer]
            end;
        _ ->
            [TabCause, "", format_update_op(UpdateOp)]
    end;
format_ets_error(update_counter, [_,_,UpdateOp,Default]=Args, Cause) ->
    case format_cause(Args, Cause) of
        TabCause when TabCause =/= [] ->
            [TabCause];
        "" ->
            %% The table is OK. The error is in one or more of the
            %% other arguments.
            TupleCause = format_tuple(Default),
            case Cause of
                badkey ->
                    ["", bad_key, format_update_op(UpdateOp) | TupleCause];
                keypos ->
                    ["", "", same_as_keypos | TupleCause];
                position ->
                    ["", "", update_op_range];
                _ ->
                    case {format_update_op(UpdateOp),TupleCause} of
                        {"",[""]} ->
                            %% UpdateOp and Default are individually
                            %% OK. The only possible remaining
                            %% problem is that the value in the record
                            %% is not an integer.
                            ["", "", counter_not_integer];
                        {UpdateOpCause,_} ->
                            ["", "", UpdateOpCause | TupleCause]
                    end
            end
    end;
format_ets_error(update_element, [_,_,ElementSpec]=Args, Cause) ->
    TabCause = format_cause(Args, Cause),
    [TabCause, "" |
     case Cause of
         keypos ->
             [same_as_keypos];
	 position ->
	     [update_op_range];
         _ ->
             case is_element_spec_top(ElementSpec) of
                 true ->
                     case TabCause of
                         [] ->
                             [range];
                         _ ->
                             []
                     end;
                 false ->
                     [<<"is not a valid element specification">>]
             end
     end];
format_ets_error(update_element, [_, _, ElementSpec, Default]=Args, Cause) ->
    TabCause = format_cause(Args, Cause),
    ArgsCause = case Cause of
		    keypos ->
			 [same_as_keypos];
		    position ->
			[update_op_range];
		    _ ->
			case {is_element_spec_top(ElementSpec), format_tuple(Default)} of
			    {true, [""]} ->
				[range];
			    {true, TupleCause} ->
				["" | TupleCause];
			    {false, [""]} ->
				[<<"is not a valid element specification">>];
			    {false, TupleCause} ->
				["" | TupleCause]
			end
		end,
    [TabCause, "" | ArgsCause];
format_ets_error(whereis, _Args, _Cause) ->
    [bad_table_name];
format_ets_error(_, Args, Cause) ->
    [format_cause(Args, Cause)].

format_default(Default, Args, Cause) ->
    case format_cause(Args, Cause) of
        "" -> ["",Default];
        Error -> [Error]
    end.

is_element_spec_top(List) when is_list(List) ->
    lists:all(fun is_element_spec/1, List);
is_element_spec_top(Other) ->
    is_element_spec(Other).

is_element_spec({Pos, _Value}) when is_integer(Pos), Pos > 0 ->
    true;
is_element_spec(_) ->
    false.

format_ms_limit([_,Ms,_]=Args, Cause) ->
    [Tab, [], Limit] = format_limit(Args, Cause),
    case is_match_spec(Ms) of
        true ->
            [Tab, "", Limit];
        false ->
            [Tab, bad_matchspec, Limit]
    end.

format_limit([_,_,Limit]=Args, Cause) ->
    [format_cause(Args, Cause), "", format_non_negative_integer(Limit)].

format_non_negative_integer(N) ->
     if
         not is_integer(N) -> not_integer;
         N < 1 -> range;
         true -> ""
     end.

format_object([_,Object|_]=Args, Cause) ->
    [format_cause(Args, Cause) | format_tuple(Object)].

format_tuple(Term) ->
    if tuple_size(Term) > 0 -> [""];
       is_tuple(Term) -> [empty_tuple];
       true -> [not_tuple]
    end.

format_objects([_,Term|_]=Args, Cause) ->
    [format_cause(Args, Cause) |
     if tuple_size(Term) > 0 -> [];
        is_tuple(Term) -> [empty_tuple];
        is_list(Term) ->
             try lists:all(fun(T) -> tuple_size(T) > 0 end, Term) of
                 true -> [];
                 false -> [not_tuple_or_list]
             catch
                 _:_ ->
                     [not_tuple_or_list]
             end;
        true -> [not_tuple]
     end].

format_cause(Args, Cause) ->
    case Cause of
        none ->
            "";
        type ->
            case Args of
                [Ref|_] when is_reference(Ref) ->
                    <<"not a valid table identifier">>;
                _ ->
                    <<"not an atom or a table identifier">>
            end;
        id ->
            <<"the table identifier does not refer to an existing ETS table">>;
        access ->
            <<"the table identifier refers to an ETS table with insufficient access rights">>;
        table_type ->
            <<"the table identifier refers to an ETS table of a type not supported by this operation">>;
        %% The following error reasons don't have anything to do with
        %% the table argument, but with some of the other arguments.
        badkey ->
            "";
        keypos ->
            "";
        position ->
            "";
        owner ->
            "";
        not_owner ->
            ""
    end.

is_match_spec(Term) ->
    ets:is_compiled_ms(Term) orelse
        try ets:match_spec_compile(Term) of
            _ ->
                true
        catch
            error:badarg ->
                false
        end.

format_update_op(UpdateOp) ->
    case is_update_op_top(UpdateOp) of
        true -> "";
        false -> bad_update_op
    end.

is_update_op_top(List) when is_list(List) ->
    lists:all(fun is_update_op/1, List);
is_update_op_top(Op) ->
    is_update_op(Op).

is_update_op({Pos, Incr}) when is_integer(Pos), is_integer(Incr) ->
    true;
is_update_op({Pos, Incr, Threshold, SetValue})
  when is_integer(Pos), is_integer(Incr), is_integer(Threshold), is_integer(SetValue) ->
    true;
is_update_op(Incr) ->
    is_integer(Incr).

is_iodata(<<_/binary>>) -> true;
is_iodata(Term) when is_list(Term) ->
    try iolist_size(Term) of
        _ -> true
    catch
        error:_ -> false
    end;
is_iodata(_) -> false.

format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([{general, E}|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum, Map#{ general => expand_error(E)});
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

must_be_binary(Bin) ->
    must_be_binary(Bin, []).

must_be_binary(Bin, Error) when is_binary(Bin) -> Error;
must_be_binary(Bin, _Error) when is_bitstring(Bin) -> bitstring;
must_be_binary(_, _) -> not_binary.

must_be_hex_case(uppercase) -> [];
must_be_hex_case(lowercase) -> [];
must_be_hex_case(_) -> bad_hex_case.

must_be_endianness(little) -> [];
must_be_endianness(big) -> [];
must_be_endianness(_) -> bad_endianness.

must_be_fun(F, Arity) when is_function(F, Arity) -> [];
must_be_fun(_, Arity) -> {not_fun,Arity}.

must_be_integer(N) when is_integer(N) -> [];
must_be_integer(_) -> not_integer.

must_be_integer(N, Min, Max, Default) when is_integer(N) ->
    if
        Min =< N, N =< Max ->
            Default;
        true ->
            range
    end;
must_be_integer(_, _, _, _) -> not_integer.

must_be_integer(N, Min, Max) ->
    must_be_integer(N, Min, Max, []).

must_be_non_neg_integer(N) ->
    must_be_integer(N, 0, infinity).

must_be_iodata(Term) ->
    case is_iodata(Term) of
        true -> [];
        false -> not_iodata
    end.

must_be_list(List) when is_list(List) ->
    try length(List) of
        _ ->
            []
    catch
        error:badarg ->
            not_proper_list
    end;
must_be_list(_) ->
    not_list.

must_be_map(#{}) -> [];
must_be_map(_) -> not_map.

must_be_map_iterator_order(undefined) ->
    [];
must_be_map_iterator_order(ordered) ->
    [];
must_be_map_iterator_order(CmpFun) when is_function(CmpFun, 2) ->
    [];
must_be_map_iterator_order(_) ->
    not_map_iterator_order.

must_be_map_or_iter(Map) when is_map(Map) ->
    [];
must_be_map_or_iter(Iter) ->
    case maps:is_iterator_valid(Iter) of
        true -> [];
        false -> not_map_or_iterator
    end.

must_be_number(N) ->
    if
        is_number(N) -> [];
        true -> not_number
    end.

must_be_pattern(P) ->
    try binary:match(<<"a">>, P) of
        _ ->
            []
    catch
        error:badarg ->
            bad_binary_pattern
    end.

must_be_binary_replacement(R) when is_binary(R) -> [];
must_be_binary_replacement(R) when is_function(R, 1) -> [];
must_be_binary_replacement(_R) -> bad_replacement.

must_be_position(Pos) when is_integer(Pos), Pos >= 0 -> [];
must_be_position(Pos) when is_integer(Pos) -> range;
must_be_position(_) -> not_integer.

must_be_regexp(Term) ->
    %% First check if we can compile the regexp as this
    %% returns better error messages
    try re:compile(Term) of
        {ok, _} ->
            [];
        {error, Reason} ->
            {bad_regexp, Reason}
    catch error:_ ->
            %% Then check if we can run it as this also allows
            %% compiled reg exps
            try re:run("", Term) of
                _ -> []
            catch
                error:_ -> not_regexp
            end
    end.

must_be_re_replacement(R) when is_function(R, 1) -> [];
must_be_re_replacement(R) ->
    case is_iodata(R) of
        true -> [];
        false -> bad_replacement
    end.

expand_error(already_owner) ->
    <<"the process is already the owner of the table">>;
expand_error(bad_boolean) ->
    <<"not a boolean value">>;
expand_error(bad_binary_list) ->
    <<"not a flat list of binaries">>;
expand_error(bad_char_data) ->
    <<"not valid character data (an iodata term)">>;
expand_error(bad_binary_pattern) ->
    <<"not a valid pattern">>;
expand_error(bad_continuation) ->
    <<"invalid continuation">>;
expand_error(bad_encoding) ->
    <<"not a valid encoding">>;
expand_error(bad_endianness) ->
    <<"must be 'big' or 'little'">>;
expand_error(bad_info_item) ->
    <<"not a valid info item">>;
expand_error(bad_iterator) ->
    <<"not a valid iterator">>;
expand_error(bad_key) ->
    <<"not a key that exists in the table">>;
expand_error(bad_matchspec) ->
    <<"not a valid match specification">>;
expand_error(bad_options) ->
    <<"invalid options">>;
expand_error(bad_replacement) ->
    <<"not a valid replacement">>;
expand_error(bad_table_name) ->
    <<"invalid table name (must be an atom)">>;
expand_error(bad_update_op) ->
    <<"not a valid update operation">>;
expand_error(bitstring) ->
    <<"is a bitstring (expected a binary)">>;
expand_error(calling_self) ->
    <<"the device is not allowed to be the current process">>;
expand_error(counter_not_integer) ->
    <<"the value in the given position, in the object, is not an integer">>;
expand_error(dead_process) ->
    <<"the pid refers to a terminated process">>;
expand_error(device_arguments) ->
    <<"the device does not exist">>;
expand_error(device_terminated) ->
    <<"the device has terminated">>;
expand_error(domain_error) ->
    <<"is outside the domain for this function">>;
expand_error(empty_binary) ->
    <<"a zero-sized binary is not allowed">>;
expand_error(empty_tuple) ->
    <<"is an empty tuple">>;
expand_error(format_failed) ->
    <<"failed to format string">>;
expand_error(invalid_format) ->
    <<"not a valid format string">>;
expand_error(missing_argument_list) ->
    <<"possibly missing argument list">>;
expand_error(name_already_exists) ->
    <<"table name already exists">>;
expand_error({no_translation, In, Out}) ->
    unicode:characters_to_binary(
      io_lib:format("device failed to transcode string from ~p to ~p", [In, Out]));
expand_error(not_atom) ->
    <<"not an atom">>;
expand_error(not_binary) ->
    <<"not a binary">>;
expand_error(bad_hex_case) ->
    <<"not 'uppercase' or 'lowercase'">>;
expand_error(not_compiled_regexp) ->
    <<"not a compiled regular expression">>;
expand_error(not_iodata) ->
    <<"not an iodata term">>;
expand_error({not_fun,1}) ->
    <<"not a fun that takes one argument">>;
expand_error({not_fun,2}) ->
    <<"not a fun that takes two arguments">>;
expand_error({not_fun,3}) ->
    <<"not a fun that takes three arguments">>;
expand_error(not_integer) ->
    <<"not an integer">>;
expand_error(not_list) ->
    <<"not a list">>;
expand_error(not_map_iterator_order) ->
    <<"not 'undefined', 'ordered', 'reversed', or a fun that takes two arguments">>;
expand_error(not_map_or_iterator) ->
    <<"not a map or an iterator">>;
expand_error(not_number) ->
    <<"not a number">>;
expand_error(not_proper_list) ->
    <<"not a proper list">>;
expand_error(not_map) ->
    <<"not a map">>;
expand_error(not_owner) ->
    <<"the current process is not the owner">>;
expand_error(not_pid) ->
    <<"not a pid">>;
expand_error(not_regexp) ->
    <<"neither an iodata term nor a compiled regular expression">>;
expand_error({bad_regexp, {Reason,Column}}) ->
    unicode:characters_to_binary(
      io_lib:format("could not parse regular expression~n"
                    "~ts on character ~p",
                    [Reason, Column]));
expand_error(not_tuple) ->
    <<"not a tuple">>;
expand_error(not_tuple_or_list) ->
    <<"not a non-empty tuple or a list of non-empty tuples">>;
expand_error(range) ->
    <<"out of range">>;
expand_error(same_as_keypos) ->
    <<"the position is the same as the key position">>;
expand_error({unknown_error,Cause}) ->
    unicode:characters_to_binary(
      io_lib:format("unknown error: ~tp", [Cause]));
expand_error(update_op_range) ->
    <<"the position in the update operation is out of range">>;
expand_error(Other) ->
    Other.
