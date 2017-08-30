%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
-module(erl_anno_SUITE).

%%-define(debug, true).

-ifdef(debug).
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), io:format(S, A)).
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-endif.

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([new/1, is_anno/1, generated/1, end_location/1, file/1,
         line/1, location/1, record/1, text/1, bad/1]).

-export([parse_abstract/1, mapfold_anno/1]).

all() ->
    [{group, anno}, {group, parse}].

groups() ->
    [{anno, [], [new, is_anno, generated, end_location, file,
                 line, location, record, text, bad]},
     {parse, [], [parse_abstract, mapfold_anno]}].

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

-define(INFO(T, V), {T, V}).

-dialyzer({no_fail_call, new/1}).
%% Test erl_anno:new/1.
new(_Config) ->
    {'EXIT', {badarg, _}} =
        (catch erl_anno:new([{location,1},{text, "text"}])), % badarg
    ok.

%% Test erl_anno:is_anno/1.
is_anno(_Config) ->
    false = erl_anno:is_anno(a),
    false = erl_anno:is_anno({a}),
    false = erl_anno:is_anno([]),
    false = erl_anno:is_anno([{location, 1}|{generated, true}]),
    false = erl_anno:is_anno([{generated,false}]),
    false = erl_anno:is_anno([{generated,true}]),
    false = erl_anno:is_anno([{location,1},{file,nofile}]),
    false = erl_anno:is_anno([{location,1},{text,notext}]),

    true = erl_anno:is_anno(erl_anno:new(1)),
    A0 = erl_anno:new({1, 17}),
    true = erl_anno:is_anno(A0),
    A1 = erl_anno:set_generated(true, A0),
    true = erl_anno:is_anno(A1),
    A2 = erl_anno:set_file("", A1),
    true = erl_anno:is_anno(A2),
    A3 = erl_anno:set_record(true, A2),
    true = erl_anno:is_anno(A3),
    A4 = erl_anno:set_text("text", A3),
    true = erl_anno:is_anno(A4),
    A5 = erl_anno:set_file(<<"filename">>, A4),
    true = erl_anno:is_anno(A5),
    ok.

%% Test 'generated'.
generated(_Config) ->
    test(1, [{generated, true}, {generated, false}]),
    test(1, [{generated, false}, {generated, true}, {generated, false}]),
    test({1, 17}, [{generated, false},
                   {generated, true},
                   {generated, false}]),
    test({1, 17}, [{text, "text", [{end_location, {1, 21}}, {length, 4}]},
                   {generated, false},
                   {generated, true},
                   {generated, false}]),
    test(1, [{generated, false},
             {generated, true},
             {generated, false}]),
    test(1, [{text, "text", [{end_location, 1}, {length, 4}]},
             {generated, false},
             {generated, true},
             {generated, false}]),
    ok.

%% Test 'end_location'.
end_location(_Config) ->
    test({1, 17}, [{text, "TEXT", [{end_location, {1, 21}}, {length, 4}]},
                   {text, "TEXT\n", [{end_location, {2, 1}}, {length, 5}]},
                   {text, "TEXT\ntxt", [{end_location, {2, 4}}, {length, 8}]}]),
    test(1, [{text, "TEXT", [{end_location, 1}, {length, 4}]},
             {text, "TEXT\n", [{end_location, 2}, {length, 5}]},
             {text, "TEXT\ntxt", [{end_location, 2}, {length, 8}]}]),
    ok.

%% Test 'file'.
file(_Config) ->
    test(1, [{file, "name"}, {file, ""}]),
    test({1, 17}, [{file, "name"}, {file, ""}]),
    ok.

%% Test 'line'.
line(_Config) ->
    test(1, [{line, 17, [{location, 17}]},
             {location, {9, 8}, [{line, 9}, {column, 8}]},
             {line, 14, [{location, {14, 8}}]}]),
    ok.

%% Test 'location'.
location(_Config) ->
    test(1, [{location, 2, [{line,2}]},
             {location, {1, 17}, [{line, 1}, {column, 17}]},
             {location, {9, 6}, [{line, 9}, {column, 6}]},
             {location, 9, [{column, undefined}]}]),
    test(1, [{generated, true},
             {location, 2, [{line,2}]},
             {location, {1, 17}, [{line, 1}, {column, 17}]},
             {location, {9, 6}, [{line, 9}, {column, 6}]},
             {location, 9, [{column, undefined}]}]),
    test(1, [{record, true},
             {location, 2, [{line,2}]},
             {location, {1, 17}, [{line, 1}, {column, 17}]},
             {location, {9, 6}, [{line, 9}, {column, 6}]},
             {location, 9, [{column, undefined}]}]),
    ok.

%% Test 'record'.
record(_Config) ->
    test({1, 17}, [{record, true}, {record, false}]),
    test(1, [{record, true}, {record, false}]),
    test({1, 17}, [{generated, false},
                   {generated, true},
                   {generated, false}]),
    test({1, 17}, [{text, "text", [{end_location, {1, 21}}, {length, 4}]},
                   {generated, false},
                   {generated, true},
                   {generated, false}]),
    test(1, [{generated, false},
             {generated, true},
             {generated, false}]),
    test(1, [{text, "text", [{end_location, 1}, {length, 4}]},
             {generated, false},
             {generated, true},
             {generated, false}]),
    ok.

%% Test 'text'.
text(_Config) ->
    test(1, [{text, "text", [{end_location, 1}, {length, 4}]},
             {text, "", [{end_location, 1}, {length, 0}]}]),
    test({1, 17}, [{text, "text", [{end_location, {1,21}}, {length, 4}]},
                   {text, "", [{end_location, {1,17}}, {length, 0}]}]),
    ok.

-dialyzer({[no_opaque, no_fail_call], bad/1}).
%% Test bad annotations.
bad(_Config) ->
    Line = erl_anno:new(1),
    LineColumn = erl_anno:new({1, 17}),
    {'EXIT', {badarg, _}} =
        (catch erl_anno:set_generated(true, bad)), % 3rd arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:set_generated(false, bad)), % 3rd arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:set_generated(19, Line)),
    {'EXIT', {badarg, _}} =
        (catch erl_anno:set_generated(19, LineColumn)),

    {'EXIT', {badarg, _}} =
        (catch erl_anno:generated(bad)), % 1st arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:end_location(bad)), % 1st arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:file(bad)), % 1st arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:text(bad)), % 1st arg not opaque
    {'EXIT', {badarg, _}} =
        (catch erl_anno:record(bad)), % 1st arg not opaque
    ok.

%% Test erl_parse:new_anno/1, erl_parse:anno_to_term/1,
%% and erl_parse:anno_from_term/1.
parse_abstract(_Config) ->
    T = sample_term(),
    A = erl_parse:abstract(T, [{line,17}]),
    T1 = erl_parse:anno_to_term(A),
    Abstr = erl_parse:new_anno(T1),
    T = erl_parse:normalise(Abstr),
    Abstr2 = erl_parse:anno_from_term(T1),
    T = erl_parse:normalise(Abstr2),
    ok.

%% Test erl_parse:{map_anno/2,fold_anno/3, and mapfold_anno/3}.
mapfold_anno(_Config) ->
    T = sample_term(),
    Abstr = erl_parse:abstract(T),
    CF = fun(Anno, {L, D}) ->
                 {erl_anno:new(L), {L+1, dict:store(L, Anno, D)}}
         end,
    {U, {N, D}} = erl_parse:mapfold_anno(CF, {1, dict:new()}, Abstr),
    SeqA = erl_parse:fold_anno(fun(Anno, Acc) -> [Anno|Acc] end, [], U),
    Seq = [erl_anno:location(A) || A <- SeqA],
    Seq = lists:seq(N-1, 1, -1),
    NF = fun(Anno) ->
                 L = erl_anno:location(Anno),
                 dict:fetch(L, D)
         end,
    Abstr = erl_parse:map_anno(NF, U),
    ok.

sample_term() ->
    %% This is just a sample.
    {3,a,4.0,"foo",<<"bar">>,#{a => <<19:64/unsigned-little>>},
     [1000,2000]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(StartLocation, Updates) ->
    S0 = init(StartLocation),
    A0 = erl_anno:new(StartLocation),
    chk(S0, A0, []),
    eval(Updates, S0, A0).

eval([], _S0, _A0) ->
    ok;
eval([{Item, Value}|Updates], S0, A0) ->
    {S, A} = set(Item, Value, A0, S0, []),
    eval(Updates, S, A);
eval([{Item, Value, Secondary}|Updates], S0, A0) ->
    {S, A} = set(Item, Value, A0, S0, Secondary),
    eval(Updates, S, A).

init({Line, Column}) ->
    lists:sort([{location, {Line, Column}} | default()]);
init(Line) when is_integer(Line) ->
    lists:sort([{location, Line} | default()]).

set(Item, Value, Anno0, State0, Secondary) ->
    true = lists:member(Item, primary_items()),
    ?format("Set '~w' to ~p\n", [Item, Value]),
    State = set_value(Item, Value, State0),
    Anno = anno_set(Item, Value, Anno0),
    ?format("State0 ~p\n", [State0]),
    ?format("State  ~p\n", [State]),
    ?format("Anno0  ~p\n", [anno_to_term(Anno0)]),
    ?format("Anno   ~p\n", [anno_to_term(Anno)]),
    chk(State, Anno, Secondary),
    ok = frame(Anno0, Anno, Secondary),
    {State, Anno}.

frame(OldAnno, NewAnno, Secondary) ->
    SecItems = [I || {I, _} <- Secondary],
    Frame = secondary_items() -- (SecItems ++ primary_items()),
    ?format("Frame items ~p\n", [Frame]),
    frame1(Frame, OldAnno, NewAnno).

frame1([], _OldAnno, _NewAnno) ->
    ok;
frame1([Item|Items], OldAnno, NewAnno) ->
    V1 = anno_info(OldAnno, Item),
    V2 = anno_info(NewAnno, Item),
    ok = check_value(Item, V1, V2),
    frame1(Items, OldAnno, NewAnno).

chk(State, Anno, Secondary) ->
    ok = check_simple(Anno),
    ok = chk_primary(State, Anno),
    ok = check_secondary(Secondary, State, Anno).

chk_primary(State, Anno) ->
    chk_primary(primary_items(), State, Anno).

chk_primary([], _State, _Anno) ->
    ok;
chk_primary([Item | Items], State, Anno) ->
    V1 = primary_value(Item, State),
    V2 = anno_info(Anno, Item),
    ok = check_value(Item, V1, V2),
    chk_primary(Items, State, Anno).

check_secondary([], _State, _Anno) ->
    ok;
check_secondary([{Item, _}=V1 | Secondary], State, Anno) ->
    V2 = anno_info(Anno, Item),
    case {V1, V2} of
        {{Item, undefined}, undefined} ->
            ok;
        _ ->
            ok = check_value(Item, V1, V2)
    end,
    check_secondary(Secondary, State, Anno).

check_value(Item, V1, V2) ->
    ?format("~w: V1 ~p\n", [Item, V1]),
    ?format("~w: V2 ~p\n", [Item, V2]),
    case V1 =:= V2 of
        true ->
            ok;
        false ->
            io:format("~w: expected ~p\n got     ~p\n", [Item, V1, V2]),
            exit({V1, V2})
    end.

check_simple(Anno) ->
    Term = anno_to_term(Anno),
    case find_defaults(Term) of
        [] ->
            ok;
        Ds ->
            io:format("found default values ~w in ~p\n", [Ds, Anno]),
            exit({defaults, Anno})
    end,
    case check_simple1(Term) of
        true ->
            ok;
        false ->
            io:format("not simple ~p\n", [Anno]),
            exit({not_simple, Anno})
    end.

check_simple1(L) when is_integer(L) ->
    true;
check_simple1({L, C}) when is_integer(L), is_integer(C) ->
    true;
check_simple1(List) ->
    case lists:sort(List) of
        [{location, _}] ->
            false;
        _ ->
            true
    end.

find_defaults(L) when is_list(L) ->
    [I ||
        I <- default_items(),
        {I1, Value} <- L,
        I =:= I1,
        Value =:= default_value(I)];
find_defaults(_) ->
    [].

anno_to_term(Anno) ->
    T = erl_anno:to_term(Anno),
    maybe_sort(T).

maybe_sort(L) when is_list(L) ->
    lists:sort(L);
maybe_sort(T) ->
    T.

anno_set(file, Value, Anno) ->
    erl_anno:set_file(Value, Anno);
anno_set(generated, Value, Anno) ->
    erl_anno:set_generated(Value, Anno);
anno_set(line, Value, Anno) ->
    erl_anno:set_line(Value, Anno);
anno_set(location, Value, Anno) ->
    erl_anno:set_location(Value, Anno);
anno_set(record, Value, Anno) ->
    erl_anno:set_record(Value, Anno);
anno_set(text, Value, Anno) ->
    erl_anno:set_text(Value, Anno).

anno_info(Anno, Item) ->
    Value =
        case Item of
            column ->
                erl_anno:column(Anno);
            generated ->
                erl_anno:generated(Anno);
            end_location ->
                erl_anno:end_location(Anno);
            file ->
                erl_anno:file(Anno);
            length ->
                case erl_anno:text(Anno) of
                    undefined ->
                        undefined;
                    Text ->
                        length(Text)
                end;
            line ->
                erl_anno:line(Anno);
            location ->
                erl_anno:location(Anno);
            record ->
                erl_anno:record(Anno);
            text ->
                erl_anno:text(Anno);
            _ ->
                erlang:error(badarg, [Anno, Item])
        end,
    if
        Value =:= undefined ->
            undefined;
        true ->
            {Item, Value}
    end.

%%% Originally 'location' was primary while 'line' and 'column' were
%%% secondary (their values are determined by 'location'). But since
%%% set_line() is used kind of frequently, 'line' is also primary,
%%% and 'location' secondary (depends on 'line'). 'line' need to be
%%% handled separately.

set_value(line, Line, State) ->
    {location, Location} = primary_value(location, State),
    NewLocation = case Location of
                      {_, Column} ->
                          {Line, Column};
                      _ ->
                          Line
                  end,
    set_value(location, NewLocation, State);
set_value(Item, Value, State) ->
    lists:ukeymerge(1, [{Item, Value}], State).

primary_value(line, State) ->
    {location, Location} = primary_value(location, State),
    {line, case Location of
               {Line, _} ->
                   Line;
               Line ->
                   Line
           end};
primary_value(Item, State) ->
    case lists:keyfind(Item, 1, State) of
        false ->
            undefined;
        Tuple ->
            Tuple
    end.

default() ->
    [{Tag, default_value(Tag)} || Tag <- default_items()].

primary_items() ->
    [file, generated, line, location, record, text].

secondary_items() ->
    %% 'length' has not been implemented
    [column, end_location, length, line, location].

default_items() ->
    [generated, record].

default_value(generated) -> false;
default_value(record) -> false.
