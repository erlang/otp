%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020-2023. All Rights Reserved.
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

-module(erl_kernel_errors).
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(_Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              erl_ddll ->
                  format_erl_ddll_error(F, As, Cause);
              os ->
                  format_os_error(F, As, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

format_erl_ddll_error(_, _, _) ->
    [].

format_os_error(cmd, _, {open_port, Reason}) ->
    [{general, maybe_posix_message(Reason)}];
format_os_error(cmd, [_], _) ->
    [not_charlist];
format_os_error(cmd, [_, _], Cause) ->
    case Cause of
        badopt ->
            [[], not_map];
        _ ->
            [not_charlist]
    end;
format_os_error(getenv, [Name|_], _) ->
    [must_be_env_var_name(Name)];
format_os_error(perf_counter, [_], _) ->
    [invalid_time_unit];
format_os_error(putenv, [Name, Value], _) ->
    [must_be_env_var_name(Name), must_be_env_var_value(Value)];
format_os_error(set_signal, [Signal, _Option], Cause) ->
    case Cause of
        badopt ->
            [must_be_atom(Signal, []), invalid_signal_option];
        _ ->
            [must_be_atom(Signal, invalid_signal_name)]
    end;
format_os_error(system_time, [_], _) ->
    [invalid_time_unit];
format_os_error(unsetenv, [Name], _) ->
    [must_be_env_var_name(Name)];
format_os_error(_, _, _) ->
    [].

maybe_posix_message(Reason) ->
    case erl_posix_msg:message(Reason) of
        "unknown POSIX error" ++ _ ->
            io_lib:format("open_port failed with reason: ~tp",[Reason]);
        PosixStr ->
            io_lib:format("~ts (~tp)",[PosixStr, Reason])
    end.

must_be_atom(Term, Default) when is_atom(Term) -> Default;
must_be_atom(_, _) -> not_atom.

must_be_env_var_name(Term) ->
    case must_be_env_charlist(Term) of
        {ok, FlatList0} ->
            FlatList = case {FlatList0, os:type()} of
                           {"=" ++ FlatList1, {win32, _}} ->
                               %% On Windows, the first character is
                               %% allowed to be `=`.
                               FlatList1;
                           {_, _} ->
                               FlatList0
                       end,
            case lists:member($=, FlatList) of
                true -> eq_in_list;
                false -> []
            end;
        Error ->
            Error
    end.

must_be_env_var_value(Term) ->
    case must_be_env_charlist(Term) of
        {ok, _} -> [];
        Error -> Error
    end.

must_be_env_charlist(Term) when is_list(Term) ->
    try lists:flatten(Term) of
        FlatList ->
            case lists:all(fun is_integer/1, FlatList) of
                true ->
                    Enc = file:native_name_encoding(),
                    case unicode:characters_to_list(FlatList, Enc) of
                        L when is_list(L) ->
                            case lists:member(0, FlatList) of
                                true ->
                                    zero_in_list;
                                false ->
                                    {ok, FlatList}
                            end;
                        {error, _, _} ->
                            invalid_characters
                    end;
                false ->
                    not_charlist
            end
    catch
        error:_ ->
            not_proper_list
    end;
must_be_env_charlist(_) ->
    not_list.


format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([{general, E}|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum, Map#{ general => expand_error(E)});
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(eq_in_list) ->
    <<"\"=\" characters is not allowed in environment variable names">>;
expand_error(zero_in_list) ->
    <<"\"\\0\" characters is not allowed in environment variable names or values">>;
expand_error(invalid_characters) ->
    <<"invalid characters">>;
expand_error(invalid_signal_name) ->
    <<"invalid signal name">>;
expand_error(invalid_signal_option) ->
    <<"invalid signal handling option">>;
expand_error(invalid_time_unit) ->
    <<"invalid time unit">>;
expand_error(not_atom) ->
    <<"not an atom">>;
expand_error(not_charlist) ->
    <<"not a list of characters">>;
expand_error(not_list) ->
    <<"not a list">>;
expand_error(not_map) ->
    <<"not a map">>;
expand_error(not_proper_list) ->
    <<"not a proper list">>;
expand_error(Other) ->
    Other.
