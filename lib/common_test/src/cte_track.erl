%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2023-2025. All Rights Reserved.
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

-module(cte_track).
-moduledoc false.
%% module for tracking CT execution progress
%% test spec addition examples:
%% {event_handler, {cte_track, []}}.
%% {event_handler, {cte_track, [{file, "/tmp/log.txt"}]}}.

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
	 handle_info/2, terminate/2, code_change/3]).
-include_lib("common_test/include/ct_event.hrl").

%%====================================================================
%% gen_event callbacks
%%====================================================================
init(InitArgs) ->
    Device =
        case proplists:get_value(file, InitArgs) of
            undefined ->
                user;
            Path when is_list(Path) ->
                {ok, D} = file:open(Path, [write]),
                D
        end,
    [begin
         dbg:start(),
         dbg:tracer(),
         dbg:tp(?MODULE, handle_event, 2, []),
         dbg:p(all,c),
         print(Device, "DEBUG started ~n")
     end || lists:member(debug, InitArgs)],
    {ok, #{device => Device,
           suite => undefined, group => undefined,
           test_stats => {0, 0, {0, 0}},
           test => #{ok => [], skipped => [], failed => [], auto_skipped => []},
           conf => #{ok => [], skipped => [], failed => [], auto_skipped => []},
           start_time => get_seconds()}}.

handle_event(#event{name = test_stats, data = TestStats}, State) ->
    {ok, maps:put(test_stats, TestStats, State)};
handle_event(#event{name = tc_start,
                    data = {Suite, init_per_suite}},
             State0 = #{device := D,
                       test_stats := TestStats}) ->
    print(D, "~n~p: ", [Suite]),
    State1 = maps:put(suite, Suite, State0),
    State = maps:put(suite_stats0, TestStats, State1),
    {ok, State};
handle_event(#event{name = tc_start,
                    data = {_Suite, {init_per_group, GroupName, _}}},
             State) ->
    {ok, maps:put(group, GroupName, State)};
handle_event(#event{name = Name,
                    data = {Suite, end_per_suite, Result}},
             State0 = #{device := Device, group := Group,
                        suite_stats0 := {SOk, SFailed, {SUserSkip, SAutoSkip}},
                        test_stats := {Ok, Failed, {UserSkip, AutoSkip}}})
  when Name == tc_done; Name == tc_user_skip; Name == tc_auto_skip->
    State1 = maps:put(suite, undefined, State0),
    State2 = maps:put(group, undefined, State1),
    State =
        case Name of
            tc_done ->
                handle_result(Suite, Group, end_per_suite, Result, undefined,
                              State2);
            Skip when Skip == tc_user_skip; Skip == tc_auto_skip ->
                handle_result(Suite, Group, end_per_suite, skipped, Result,
                              State2)
        end,
    print(Device, " | ~p ok, ~p failed, ~pU/~pA skipped",
          [Ok - SOk, Failed - SFailed, UserSkip - SUserSkip, AutoSkip - SAutoSkip]),
    {ok, State};
handle_event(#event{name = tc_done,
                    data = {Suite, Case, ok}},
             State = #{group := Group})
  when is_atom(Case) ->
    {ok, handle_result(Suite, Group, Case, ok, undefined, State)};
handle_event(#event{name = tc_done,
                    data = {_Suite, Case, {SkipOrFailed, Reason}}},
             State = #{suite := Suite, group := Group})
  when is_atom(Case) ->
    {ok, handle_result(Suite, Group, Case, SkipOrFailed, Reason, State)};
handle_event(#event{name = tc_done,
                    data = {_Suite, {Case, _, _}, {skipped, Reason}}},
             State = #{suite := Suite, group := Group})
  when Case == init_per_group; Case == end_per_group ->
    {ok, handle_result(Suite, Group, Case, skipped, Reason, State)};
handle_event(#event{name = tc_done,
                    data = {_Suite, {Case, _, _}, ok}},
             State)
  when Case == init_per_group; Case == end_per_group->
    % FIXME - not storing conf function result
    {ok, State};
handle_event(#event{name = tc_done,
                    data = Data},
             State = #{device := D}) ->
    print(D, "~n~n[cte_track] Unhandled interesting event:~nName = tc_done~nData = ~p~n~n",
          [Data]),
    {ok, State};
handle_event(#event{name = Name, data = {Suite, {Case, Group}, Comment}},
             State) when Name == tc_user_skip; Name == tc_auto_skip ->
    {ok, handle_result(Suite, Group, Case, skipped, Comment, State)};
handle_event(#event{name = Name, data = {Suite, Case, Comment}},
             State) when Name == tc_user_skip; Name == tc_auto_skip ->
    {ok, handle_result(Suite, undefined, Case, skipped, Comment, State)};
handle_event(#event{name = test_done},
             State = #{test := Test,
                       conf := Conf,
                       start_time := StartTime,
                       device := D,
                       test_stats := {Ok, Failed, {UserSkip, AutoSkip}}}) ->
    print_line(D),
    PrintCategory =
        fun(Result, Results) ->
                List = maps:get(Result, Results),
                [begin
                     print_entry(D, Result, I)
                 end || I <- lists:reverse(List)]
        end,
    print(D, "# CONF SUMMARY~n"),
    PrintCategory(auto_skipped, Conf),
    PrintCategory(failed, Conf),
    print(D, "# TEST SUMMARY~n"),
    PrintCategory(auto_skipped, Test),
    PrintCategory(failed, Test),
    print(D, "~n"),
    GetCount = fun(Result) ->
                   length(maps:get(Result, Conf))
               end,
    print(D,
          "# CONF TOTALS ~p ok, ~p failed, ~pU/~pA skipped~n" ++
          "# TEST TOTALS ~p ok, ~p failed, ~pU/~pA skipped~n" ++
              "# TIME ELAPSED ~s~n",
          [GetCount(ok), GetCount(failed), GetCount(skipped),
           GetCount(auto_skipped)] ++
          [Ok, Failed, UserSkip, AutoSkip,
           get_nice_time(get_seconds() - StartTime)]),
    print_line(D),
    print(D, "~n"),
    {ok,State};
handle_event(#event{name = Name, data = Data}, State = #{device := D}) ->
    NotInteresting = [start_logging, start_write_file, finished_write_file,
                      test_stats, start_make, finished_make, tc_logfile,
                      test_start, start_info, tc_start, stop_logging,
                      benchmark_data],
    case lists:member(Name, NotInteresting) of
        false ->
            print(D, "~n~n[cte_track] Unhandled interesting event:~nName = ~p~nData = ~p~n~n",
                  [Name, Data]);
        _ ->
            ok
    end,
    {ok,State}.

handle_call(_Req, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(stop, #{device := user}) ->
    ok;
terminate(stop, #{device := Device}) ->
    file:close(Device),
    ok;
terminate(Reason, #{device := D}) ->
    print(D, "~n[cte_track]  > Interesting terminate reason = ~p~n", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
print(Device, Fmt) ->
    print(Device, Fmt, []).

print(Device, Fmt, Args) ->
    io:fwrite(Device, Fmt, Args).

print_line(Device) ->
    print(Device, "~n======================================================================~n").

% FIXME - split handle_result into: store_event and present_event
handle_result(Suite, _Group, all, skipped, _Reason,
              State0 = #{device := Device}) ->
    % FIXME store this conf fun result
    print(Device, "~n~p: suite skipped", [Suite]),
    State0;
handle_result(Suite, Group, Case, Result, Reason0,
              State0 = #{device := Device}) ->
    Type = type(Case),
    TypeResults = maps:get(Type, State0),
    TypeResultAcc = maps:get(Result, TypeResults, []),
    Reason =
        case lists:keyfind(Reason0, 4, TypeResultAcc) of
            false ->
                Reason0;
            _ ->
                saved
        end,
    Entry = {Suite, Group, Case, Reason},
    [print_entry(Device, Result, Entry) ||
        is_test(Case)],
    maps:put(Type,
             maps:put(Result, [Entry | TypeResultAcc], TypeResults),
             State0).

print_entry(Device, Result, _Entry) when Result == ok; Result == skipped ->
    print(Device, "~s", [result_short(Result)]);
print_entry(Device, Result, Entry) when is_atom(Result) ->
    print_entry(Device, result_long(Result), Entry);
print_entry(Device, Result, {Suite, undefined, Case, Reason}) ->
    print(Device, "~n~s ~p:~p ~s~n",
          [Result, Suite, Case, format_reason(Reason)]);
print_entry(Device, Result, {Suite, Group, Case, Reason}) ->
    print(Device, "~n~s ~p@~p:~p ~s~n",
          [Result, Suite, Group, Case, format_reason(Reason)]).

%result_short(failed) -> "F";
result_short(ok) -> ".";
result_short(R) when R == skipped; R == auto_skipped -> "S";
result_short(_R) -> "?".

result_long(R) when R == skipped; R == auto_skipped; R == ok; R == failed ->
    io_lib:format("[~s]", [atom_to_list(R)]);
result_long(_) -> "[?]".

format_reason(saved) ->
    "";
format_reason(Reason) when is_atom(Reason) ->
    io_lib:format("Reason: ~p", [Reason]);
format_reason(Reason) when is_list(Reason) ->
    io_lib:format("Reason: ~p", [lists:flatten(Reason)]);
format_reason({Reason, SubReason}) ->
    io_lib:format("Reason: ~p (~p)", [Reason, SubReason]);
format_reason({Reason, A, B, C}) ->
    io_lib:format("Reason: ~p (~p ~p ~p)", [Reason, A, B, C]);
format_reason(undefined) ->
    "";
format_reason(Reason) ->
    io_lib:format("~n > Not recognized reason FIXME! ~p", [Reason]).

get_nice_time(Seconds) when is_integer(Seconds) ->
    case Seconds < 60 of
        true ->
            io_lib:format("~ws", [Seconds]);
        _ ->
            io_lib:format("~wm", [round(Seconds/60)])
    end.

get_seconds() ->
    erlang:system_time(second).

%% get_stats(#{ok := Ok, skipped := Skipped, auto_skipped := AutoSkipped,
%%             failed := Failed}) ->
%%     OkLength = length(Ok),
%%     SkippedLength = length(Skipped) + length(AutoSkipped),
%%     FailedLength = length(Failed),
%%     Total = OkLength + SkippedLength + FailedLength,
%%     [OkLength, SkippedLength, FailedLength, Total].

type(Case) ->
    case is_test(Case) of
        true -> test;
        _ -> conf
    end.

is_test(Case) ->
    not is_config(Case).

is_config(Case) ->
    % FIXME add 'all'?
    lists:member(Case, [init_per_suite, end_per_suite,
                        init_per_group, end_per_group,
                        init_per_testcase, end_per_testcase]).
