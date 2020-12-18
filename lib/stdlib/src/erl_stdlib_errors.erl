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

-module(erl_stdlib_errors).
-export([format_error/2]).

-spec format_error(Reason, StackTrace) -> ErrorMap when
      Reason :: term(),
      StackTrace :: [term()],
      ErrorMap :: #{pos_integer() => unicode:chardata()}.

format_error(_Reason, [{M,F,As,Info}|_]) ->
    ErrorInfoMap = proplists:get_value(error_info, Info, #{}),
    Cause = maps:get(cause, ErrorInfoMap, none),
    Res = case M of
              ets ->
                  format_ets_error(F, As, Cause);
              _ ->
                  []
          end,
    format_error_map(Res, 1, #{}).

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
format_ets_error(match, [_], _Cause) ->
    [bad_continuation];
format_ets_error(match, [_,_,_]=Args, Cause) ->
    format_limit(Args, Cause);
format_ets_error(match_object, [_], _Cause) ->
    [bad_continuation];
format_ets_error(match_object, [_,_,_]=Args, Cause) ->
    format_limit(Args, Cause);
format_ets_error(next, Args, Cause) ->
    format_default(bad_key, Args, Cause);
format_ets_error(prev, Args, Cause) ->
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

format_error_map([""|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map);
format_error_map([E|Es], ArgNum, Map) ->
    format_error_map(Es, ArgNum + 1, Map#{ArgNum => expand_error(E)});
format_error_map([], _, Map) ->
    Map.

expand_error(already_owner) ->
    <<"the process is already the owner of the table">>;
expand_error(bad_boolean) ->
    <<"not a boolean value">>;
expand_error(bad_continuation) ->
    <<"invalid continuation">>;
expand_error(bad_info_item) ->
    <<"not a valid info item">>;
expand_error(bad_key) ->
    <<"not a key that exists in the table">>;
expand_error(bad_matchspec) ->
    <<"not a valid match specification">>;
expand_error(bad_options) ->
    <<"invalid options">>;
expand_error(bad_table_name) ->
    <<"invalid table name (must be an atom)">>;
expand_error(bad_update_op) ->
    <<"not a valid update operation">>;
expand_error(counter_not_integer) ->
    <<"the value in the given position, in the object, is not an integer">>;
expand_error(dead_process) ->
    <<"the pid refers to a terminated process">>;
expand_error(empty_tuple) ->
    <<"is an empty tuple">>;
expand_error(name_already_exists) ->
    <<"table name already exists">>;
expand_error(not_integer) ->
    <<"not an integer">>;
expand_error(not_owner) ->
    <<"the current process is not the owner">>;
expand_error(not_pid) ->
    <<"not a pid">>;
expand_error(not_tuple) ->
    <<"not a tuple">>;
expand_error(not_tuple_or_list) ->
    <<"not a non-empty tuple or a list of non-empty tuples">>;
expand_error(range) ->
    <<"out of range">>;
expand_error(same_as_keypos) ->
    <<"the position is the same as the key position">>;
expand_error(update_op_range) ->
    <<"the position in the update operation is out of range">>;
expand_error(Other) ->
    Other.
