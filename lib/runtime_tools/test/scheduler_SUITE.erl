%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2018. All Rights Reserved.
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
%%

-module(scheduler_SUITE).

-export([suite/0, all/0]).

%% Test cases
-export([basic/1]).

all() -> [basic].


suite() -> [{ct_hooks,[ts_install_cth]}].


basic(_Config) ->
    S1 = scheduler:sample(),
    S2 = scheduler:sample_all(),

    check(scheduler:utilization(1)),

    check(scheduler:utilization(S1)),
    check(scheduler:utilization(S2)),
    check(scheduler:utilization(S1, scheduler:sample())),
    check(scheduler:utilization(S2, scheduler:sample())),

    S3 = scheduler:sample_all(),
    U13 = scheduler:utilization(S1, S3),
    U13 = scheduler:utilization(S1, remove_io(S3)),
    check(U13),

    U23all = scheduler:utilization(S2, S3),
    check(U23all),
    U23 = scheduler:utilization(S2, remove_io(S3)),
    U23 = scheduler:utilization(remove_io(S2), S3),
    U23 = remove_io(U23all),
    check(U23),

    ok.


check([{total, Tf, Ts} | List]=U) ->
    io:format("\nU = ~p\n", [U]),
    check_values(Tf, Ts, true),

    SchdList = case hd(List) of
                   {weighted, Wf, Ws} ->
                       check_values(Wf, Ws, false),
                       tl(List);
                   _ ->
                       unknown = erlang:system_info(logical_processors_available),
                       List
               end,

    lists:foreach(fun({Type, Id, F, S}) when ((Type =:= normal) or (Type =:= cpu) or (Type =:= io)),
                                             is_integer(Id) ->
                          check_values(F, S, true)
                  end,
                  SchdList),
    ok.

check_values(F, S, Max100) ->
    true = is_float(F),
    true = F >= 0.0,

    $% = lists:last(S),
    Sf = list_to_float(lists:droplast(S)),
    true = Sf >= 0.0,
    true = case Max100 of
               true ->
                   true = F =< 1.0,
                   true = Sf =< 100.0;
               false ->
                   true
           end,
    MaxDiff = 0.055555555555555555,  %% change to 0.05 when float_to_list/2 is fixed
    true = abs(F*100 - Sf) =< MaxDiff,
    ok.


remove_io({scheduler_wall_time_all,Lst}) ->
    {scheduler_wall_time, remove_io(Lst)};
remove_io(Lst) ->
    lists:filter(fun({io,_,_,_}) -> false;
                    (_) -> true end,
                 Lst).
