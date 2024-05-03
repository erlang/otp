%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2024. All Rights Reserved.
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
-module(ets_prop).

-include_lib("common_test/include/ct_property_test.hrl").

-type table_type() :: set | ordered_set | bag | duplicate_bag.

-define(ETS_TAB_DATA, proper_types:list({ct_proper_ext:safe_any(), ct_proper_ext:safe_any()})).

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% --- first/2 ----------------------------------------------------------
prop_first() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(
            DataList,
            ?ETS_TAB_DATA,
            compare_with_and_without_lookup_variants(
                Type, DataList,
                fun (T, _) -> ets:first(T) end,
                fun (T, _) -> ets:first_lookup(T) end)
        )).

%% --- next/2 ----------------------------------------------------------
prop_next() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(
            DataList,
            ?ETS_TAB_DATA,
            compare_with_and_without_lookup_variants(
                Type, DataList, fun ets:next/2, fun ets:next_lookup/2)
        )).

%% --- last/2 ----------------------------------------------------------
prop_last() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(
            DataList,
            ?ETS_TAB_DATA,
            compare_with_and_without_lookup_variants(
                Type, DataList,
                fun (T, _) -> ets:last(T) end,
                fun (T, _) -> ets:last_lookup(T) end)
        )).

%% --- prev/2 ----------------------------------------------------------
prop_prev() ->
    ?FORALL(Type, noshrink(table_type()),
        ?FORALL(
            DataList,
            ?ETS_TAB_DATA,
            compare_with_and_without_lookup_variants(
                Type, DataList, fun ets:prev/2, fun ets:prev_lookup/2)
        )).

%%%% helpers %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_key([]) ->
    '$end_of_table';
random_key([{Key, _}]) ->
    Key;
random_key(Data) ->
    RandomN = 1 + erlang:phash2(erlang:unique_integer(), length(Data)),
    {Key, _} = lists:nth(RandomN, Data),
    Key.

compare_with_and_without_lookup_variants(TableType, TableData, WithoutLookupFun, LookupFun) ->
    Tab = ets:new(test, [TableType]),
    ets:insert(Tab, TableData),
    Res = do_compare_with_and_without_lookup_variants(
        random_key(TableData), Tab, WithoutLookupFun, LookupFun),
    ets:delete(Tab),
    Res.

% compare variants of first/next/last/prev with and without _lookup to make sure they are consistent
% Key is the current position in the table, used for prev/next and ignored for first/last
% Key = '$end_of_table' means nothing to compare
do_compare_with_and_without_lookup_variants('$end_of_table', _Tab, _WithoutLookupFun, _LookupFun) ->
    true;
do_compare_with_and_without_lookup_variants(Key, Tab, WithoutLookupFun, LookupFun) ->
    Key2 = WithoutLookupFun(Tab, Key),
    case Key2 of
        '$end_of_table' ->
            '$end_of_table' =:= LookupFun(Tab, Key);
        _ ->
            Values2 = ets:lookup(Tab, Key2),
            {Key2, Values2} =:= LookupFun(Tab, Key)
    end.
