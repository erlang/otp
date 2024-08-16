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

-module(erl_kernel_errors).
-moduledoc false.
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: erlang:stacktrace(),
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(_Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              code ->
                  format_code_error(F, As);
              erl_ddll ->
                  format_erl_ddll_error(F, As, Cause);
              os ->
                  format_os_error(F, As, Cause);
              trace ->
                  format_trace_error(F, As, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

format_code_error(get_coverage, [What,Module]) ->
    coverage(
      fun() ->
              Error = case What of
                          function -> [];
                          line -> [];
                          cover_line_id -> [];
                          _ -> coverage_level
                      end,
              case Error of
                  [] ->
                      [[],if
                              not is_atom(Module) ->
                                  not_atom;
                              true ->
                                  case erlang:module_loaded(Module) of
                                      false -> module_not_loaded;
                                      true -> coverage_disabled
                                  end
                          end];
                  _ ->
                      [Error]
              end
      end);
format_code_error(get_coverage_mode, [Module]) ->
    coverage(
      fun() ->
              [if
                   not is_atom(Module) ->
                       not_atom;
                   true ->
                       case erlang:module_loaded(Module) of
                           false -> module_not_loaded;
                           true -> coverage_disabled
                       end
               end]
      end);
format_code_error(reset_coverage, [Module]) ->
    coverage(
      fun () ->
              [if
                   not is_atom(Module) ->
                       not_atom;
                   true ->
                       case erlang:module_loaded(Module) of
                           false -> module_not_loaded;
                           true -> coverage_disabled
                       end
               end]
      end);
format_code_error(set_coverage_mode, [Mode]) ->
    coverage(
      fun () ->
              [if
                   not is_atom(Mode) ->
                       not_atom;
                   true ->
                       <<"must be one of: none, function, function_counters, "
                         "line, or line_counters">>
               end]
      end);
format_code_error(_, _) ->
    [].

coverage(Fun) ->
    case code:coverage_support() of
        true ->
            Fun();
        false ->
            [<<"this runtime system does not support coverage">>]
    end.

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

format_trace_error(process, [_Session,Proc,How,Options], Cause) ->
    PidError = case Proc of
                   _ when is_pid(Proc), node(Proc) =/= node() ->
                       not_local_pid;
                   _ ->
                       []
               end,
    HowError = must_be_boolean(How),
    case Cause of
        session ->
            [bad_session];

        badopt ->
            [[], PidError, HowError, must_be_option_list(Options)];
        _ ->
            case {HowError, PidError} of
                {[], []} ->
                    [[], <<"invalid process spec">>];
                _ ->
                    [[], PidError, HowError, []]
            end
    end;
format_trace_error(port, [_Session,Port,How,Options], Cause) ->
    PrtError = case Port of
                   _ when is_port(Port), node(Port) =/= node() ->
                       not_local_port;
                   _ ->
                       []
                end,
    HowError = must_be_boolean(How),
    case Cause of
        session ->
            [bad_session];

        badopt ->
            [[], PrtError, HowError, must_be_option_list(Options)];
        _ ->
            case {HowError, PrtError} of
                {[], []} ->
                    [[], <<"invalid port spec">>];
                _ ->
                    [[], PrtError, HowError, []]
            end
    end;
format_trace_error(function, [_Session,_MFA,_MatchSpec,Options], Cause) ->
    case Cause of
        session ->
            [bad_session];
        badopt ->
            [[], [], [], must_be_option_list(Options)];
        match_spec ->
            [[], [], bad_match_spec, must_be_list(Options)];
        call_count ->
            [[], [], [], <<"a match spec is not allowed in combination with these options">>];
        _ ->
            [[], <<"invalid MFA specification">>, [], []]
    end;
format_trace_error(SendRecv, [_Session,_MatchSpec,Options], Cause)
  when SendRecv =:= send; SendRecv =:= recv ->
    case Cause of
        session ->
            [bad_session];
        badopt ->
            [[], [], must_be_option_list(Options)];
        match_spec ->
            [[], bad_match_spec, must_be_list(Options)];
        _ ->
            []
    end;
format_trace_error(info, [_Session,Tracee,_Item], Cause) ->
    case Cause of
        session ->
            [bad_session];
        badopt ->
            if
                is_pid(Tracee), node(Tracee) =/= node() ->
                    [[], not_local_pid];
                is_port(Tracee), node(Tracee) =/= node() ->
                    [[], not_local_port];
                true ->
                    [[], <<"not a valid tracee specification">>]
            end;
        none ->
            [[], <<"invalid trace item">>]
    end;
format_trace_error(session_create, [Name,Tracer,Options], _) ->
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
format_trace_error(session_destroy, [_Session], _) ->
    [bad_session];
format_trace_error(session_info, [_PidPortFuncEvent], _) ->
    [<<"not a valid tracee specification">>];
format_trace_error(_, _, _) ->
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


must_be_boolean(B) when is_boolean(B) -> [];
must_be_boolean(_) -> bad_boolean.

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

must_be_option_list(Options) ->
    case must_be_list(Options) of
        [] -> bad_option;
        Error -> Error
    end.

format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([{general, E}|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum, Map#{ general => expand_error(E)});
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(bad_boolean) ->
    <<"not a boolean ('true' or 'false')">>;
expand_error(bad_match_spec) ->
    <<"invalid match specification">>;
expand_error(bad_option) ->
    <<"invalid option in list">>;
expand_error(bad_session) ->
    <<"invalid trace session">>;
expand_error(bad_tracer) ->
    <<"invalid tracer">>;
expand_error(coverage_disabled) ->
    <<"not loaded with coverage enabled">>;
expand_error(coverage_level) ->
    <<"must be one of: function, line, or cover_line_id">>;
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
expand_error(module_not_loaded) ->
    <<"the atom does not refer to a loaded module">>;
expand_error(not_atom) ->
    <<"not an atom">>;
expand_error(not_charlist) ->
    <<"not a list of characters">>;
expand_error(not_list) ->
    <<"not a list">>;
expand_error(not_local_pid) ->
    <<"not a local pid">>;
expand_error(not_local_port) ->
    <<"not a local port">>;
expand_error(not_map) ->
    <<"not a map">>;
expand_error(not_proper_list) ->
    <<"not a proper list">>;
expand_error(Other) ->
    Other.
