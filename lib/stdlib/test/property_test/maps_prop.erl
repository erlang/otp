%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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
-module(maps_prop).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct_property_test.hrl").

%%%%%%%%%%%%%%%%%%
%%% Properties %%%
%%%%%%%%%%%%%%%%%%

%% size/1
prop_size() ->
    ?FORALL(
        InKeys,
        ?CT_SAFE_LIST(),
        begin
            Map = lists:foldl(fun(K, Acc) ->
                                  Acc#{K => make_ref()}
                              end,
                              #{},
                              InKeys),
            length(lists:uniq(InKeys)) =:= maps:size(Map)
        end
    ).

%% keys/1
prop_keys() ->
    ?FORALL(
        InKeys,
        ?CT_SAFE_LIST(),
        begin
            Map = lists:foldl(fun(K, Acc) ->
                                  Acc#{K => make_ref()}
                              end,
                              #{},
                              InKeys),
            lists_same_contents(lists:uniq(InKeys), maps:keys(Map))
        end
    ).

%% values/1
prop_values() ->
    ?FORALL(
        InValues,
        ?CT_SAFE_LIST(),
        begin
            Map = lists:foldl(fun(V, Acc) ->
                                  Acc#{make_ref() => V}
                              end,
                              #{},
                              InValues),
            lists_same_contents(InValues, maps:values(Map))
        end
    ).

%% from_list/1
prop_from_list() ->
    ?FORALL(
        InKVs,
        gen_kv_list(),
        begin
            Map = maps:from_list(InKVs),
            BuiltMap = lists:foldl(fun({K, V}, Acc) ->
                                       Acc#{K => V}
                                   end,
                                   #{},
                                   InKVs),
            Map =:= BuiltMap
        end
    ).

%% from_keys/2
prop_from_keys() ->
    ?FORALL(
        InKeys,
        ?CT_SAFE_LIST(),
        begin
            Value = make_ref(),
            Map = maps:from_keys(InKeys, Value),
            BuiltMap = lists:foldl(fun(K, Acc) ->
                                       Acc#{K => Value}
                                   end,
                                   #{},
                                   InKeys),
            Map =:= BuiltMap
        end
    ).

%% to_list/1
prop_to_list() ->
    ?FORALL(
       {InKVs, IterOrderFun},
       {gen_kv_list(), gen_ordering_fun()},
       begin
           Map = lists:foldl(fun({K, V}, Acc) ->
                                 Acc#{K => V}
                             end,
                             #{},
                             InKVs),

           FromMap = maps:to_list(Map),
           Unspecified = maps:to_list(maps:iterator(Map)),
           Undefined = maps:to_list(maps:iterator(Map, undefined)),
           Ordered = maps:to_list(maps:iterator(Map, ordered)),
           Reversed = maps:to_list(maps:iterator(Map, reversed)),
           ViaOrderingFun = maps:to_list(maps:iterator(Map, IterOrderFun)),

           lists_same_kvs(FromMap, InKVs) andalso
           lists_same_kvs(Unspecified, InKVs) andalso
           lists_same_kvs(Undefined, InKVs) andalso
           lists_same_kvs(Ordered, InKVs) andalso
           lists_same_kvs(Reversed, InKVs) andalso
           lists_same_kvs(ViaOrderingFun, InKVs) andalso

           Ordered =:= lists:reverse(Reversed) andalso
           list_ordered_by(ViaOrderingFun, IterOrderFun)
       end
    ).

%% iterator/1, iterator/2, next/1
prop_iteration() ->
    ?FORALL(
        {InMap, IterOrderFun},
        {?CT_SAFE_MAP(), gen_ordering_fun()},
        begin
            Reference = maps:to_list(InMap),

            Unspecified = iterate_collect(maps:iterator(InMap)),
            Undefined = iterate_collect(maps:iterator(InMap, undefined)),
            Ordered = iterate_collect(maps:iterator(InMap, ordered)),
            Reversed = iterate_collect(maps:iterator(InMap, reversed)),
            ViaOrderingFun = iterate_collect(maps:iterator(InMap, IterOrderFun)),

            %% check contents
            lists_same_contents(Unspecified, Reference) andalso
            lists_same_contents(Undefined, Reference) andalso
            lists_same_contents(Ordered, Reference) andalso
            lists_same_contents(Reversed, Reference) andalso
            lists_same_contents(ViaOrderingFun, Reference) andalso

            %% check ordering (as far as possible)
            Ordered =:= lists:reverse(Reversed) andalso
            list_ordered_by(ViaOrderingFun, IterOrderFun)
        end
    ).

iterate_collect(I) ->
    iterate_collect1(maps:next(I)).

iterate_collect1(none) ->
    [];
iterate_collect1({K, V, NextI}) ->
    [{K, V}|iterate_collect1(maps:next(NextI))].

%% is_key/2
prop_is_key() ->
    ?FORALL(
        {InKeys, ExtraKeys},
        {?CT_SAFE_LIST(), non_empty(?CT_SAFE_LIST())},
        begin
            Map = maps:from_keys(InKeys, make_ref()),
            lists:all(fun(K) ->
                         maps:is_key(K, Map) =:= lists:member(K, InKeys)
                      end,
                      InKeys ++ ExtraKeys)
        end
    ).

%% get/2
prop_get_2() ->
    ?FORALL(
        {InMap, ExtraKeys},
        {?CT_SAFE_MAP(), non_empty(?CT_SAFE_LIST())},
        begin
            %% retrieves the correct values for all existing keys
            lists:all(fun({K, V}) ->
                          V =:= maps:get(K, InMap)
                      end,
                      maps:to_list(InMap)) andalso
            %% non-existing keys throw an error
            lists:all(fun
                         (K) when is_map_key(K, InMap) ->
                             true;
                         (K) ->
                             try
                                 maps:get(K, InMap)
                             of
                                 _ ->
                                     false
                             catch
                                 error:{badkey, K} ->
                                     true;
                                 _:_ ->
                                     false
                             end
                      end,
                      ExtraKeys)
        end
    ).

%% get/3
prop_get_3() ->
    ?FORALL(
        {InMap, ExtraKeys},
        {?CT_SAFE_MAP(), non_empty(?CT_SAFE_LIST())},
        begin
            DefaultValue = make_ref(),
            %% retrieves the correct values for all existing keys
            lists:all(fun({K, V}) ->
                          V =:= maps:get(K, InMap, DefaultValue)
                      end,
                      maps:to_list(InMap)) andalso
            %% non-existing keys return the default value
            lists:all(fun
                         (K) when is_map_key(K, InMap) ->
                             true;
                         (K) ->
                             is_map_key(K, InMap) orelse
                             DefaultValue =:= maps:get(K, InMap, DefaultValue)
                      end,
                      ExtraKeys)
        end
    ).

%% find/2
prop_find() ->
    ?FORALL(
        {InMap, ExtraKeys},
        {?CT_SAFE_MAP(), non_empty(?CT_SAFE_LIST())},
        begin
            %% retrieves the correct values for all existing keys
            lists:all(fun({K, V}) ->
                          {ok, V} =:= maps:find(K, InMap)
                      end,
                      maps:to_list(InMap)) andalso
            %% non-existing keys return `error'
            lists:all(fun
                         (K) when is_map_key(K, InMap) ->
                             true;
                         (K) ->
                             error =:= maps:find(K, InMap)
                      end,
                      ExtraKeys)
        end
    ).

%% put/3
prop_put() ->
    ?FORALL(
        {{InMap, InKey}, InValue},
        {gen_map_and_key(), ?CT_SAFE_ANY()},
        InMap#{InKey => InValue} =:= maps:put(InKey, InValue, InMap)
    ).

%% update/3
prop_update() ->
    ?FORALL(
        {{InMap, InKey}, InValue},
        {gen_map_and_key(), ?CT_SAFE_ANY()},
        try
            maps:update(InKey, InValue, InMap)
        of
            UpdatedMap when is_map_key(InKey, InMap) ->
                InMap#{InKey := InValue} =:= UpdatedMap;
            _ ->
                false
        catch
            error:{badkey, InKey} ->
                not is_map_key(InKey, InMap)
        end
    ).

%% update_with/3
prop_update_with_3() ->
    ?FORALL(
        {{InMap, InKey}, UpdateFun},
        {gen_map_and_key(), function1(?CT_SAFE_ANY())},
        try
            maps:update_with(InKey, UpdateFun, InMap)
        of
            UpdatedMap when is_map_key(InKey, InMap) ->
                #{InKey := V} = InMap,
                InMap#{InKey := UpdateFun(V)} =:= UpdatedMap;
            _ ->
                false
        catch
            error:{badkey, InKey} ->
                not is_map_key(InKey, InMap)
        end
    ).

%% update_with/4
prop_update_with_4() ->
    ?FORALL(
        {{InMap, InKey}, UpdateFun},
        {gen_map_and_key(), function1(?CT_SAFE_ANY())},
        begin
            DefaultValue = make_ref(),
            UpdatedMap = maps:update_with(InKey, UpdateFun, DefaultValue, InMap),
            case InMap of
                #{InKey := V} ->
                    InMap#{InKey := UpdateFun(V)} =:= UpdatedMap;
                #{} ->
                    InMap#{InKey => DefaultValue} =:= UpdatedMap
            end
        end
    ).

%% remove/2
prop_remove() ->
    ?FORALL(
        {InKVs, ExtraKeys},
        {gen_kv_list(), non_empty(?CT_SAFE_LIST())},
        begin
            Map = maps:from_list(InKVs),
            %% deletes entries for existing keys and ignores non-existing keys
            lists:all(fun(K) ->
                          map_from_list_without_key(K, InKVs) =:= maps:remove(K, Map)
                      end,
                      maps:keys(Map) ++ ExtraKeys)
        end
    ).

%% take/2
prop_take() ->
    ?FORALL(
        {InMap, ExtraKeys},
        {?CT_SAFE_MAP(), non_empty(?CT_SAFE_LIST())},
        begin
            KVs = maps:to_list(InMap),
            %% retrieves the value and deletes the entry for existing keys
            lists:all(fun({K, V}) ->
                          {V, map_from_list_without_key(K, KVs)} =:= maps:take(K, InMap)
                      end,
                      KVs) andalso
            %% non-existing keys return `error'
            lists:all(fun
                         (K) when is_map_key(K, InMap) ->
                             true;
                         (K) ->
                             error =:= maps:take(K, InMap)
                      end,
                      ExtraKeys)
        end
    ).

%% with/2
prop_with() ->
    ?FORALL(
        {InMap, InKeys},
        ?LET(
            {M, ExtraKs},
            {?CT_SAFE_MAP(), ?CT_SAFE_LIST()},
            {M, gen_sublist(maps:keys(M) ++ ExtraKs)}
        ),
        begin
            MapWith = maps:with(InKeys, InMap),
            %% all entries in the input map were kept/discarded correctly
            lists:all(fun({K, V}) ->
                          case MapWith of
                              #{K := V} ->
                                  %% entry exists in the output map with the same value
                                  lists:member(K, InKeys);
                              #{K := _} ->
                                  %% entry exists in the output map with a different value
                                  false;
                              #{} ->
                                  %% entry does not exist in the output map
                                  not lists:member(K, InKeys)
                          end
                      end,
                      maps:to_list(InMap)) andalso
            %% all entries in the output map exist in the input map and were kept correctly
            lists:all(fun({K, V}) ->
                          case InMap of
                              #{K := V} ->
                                  %% entry exists in the input map with the same value
                                  lists:member(K, InKeys);
                              #{} ->
                                  %% entry does not exist in the input map,
                                  %% or entry exists in the input map with a different value
                                  false
                          end
                      end,
                      maps:to_list(MapWith))
        end
    ).

%% without/2
prop_without() ->
    ?FORALL(
        {InMap, InKeys},
        ?LET(
            {M, ExtraKs},
            {?CT_SAFE_MAP(), ?CT_SAFE_LIST()},
            {M, gen_sublist(maps:keys(M) ++ ExtraKs)}
        ),
        begin
            MapWithout = maps:without(InKeys, InMap),
            %% all entries in the input map were discarded/kept correctly
            lists:all(fun({K, V}) ->
                          case MapWithout of
                              #{K := V} ->
                                  %% entry exists in the output map with the same value
                                  not lists:member(K, InKeys);
                              #{K := _} ->
                                  %% entry exists in the output map with a different value
                                  false;
                              #{} ->
                                  %% entry does not exist in the output map
                                  lists:member(K, InKeys)
                          end
                      end,
                      maps:to_list(InMap)) andalso
            %% all entries in the output map exist in the input map and were kept correctly
            lists:all(fun({K, V}) ->
                          case InMap of
                              #{K := V} ->
                                  %% entry exists in the input map with the same value
                                  not lists:member(K, InKeys);
                              #{} ->
                                  %% entry does not exist in the input map,
                                  %% or entry exists in the input map with a different value
                                  false
                          end
                      end,
                      maps:to_list(MapWithout))
        end
    ).

%% foreach/2
prop_foreach() ->
    ?FORALL(
        {InMap, IterOrderFun},
        {?CT_SAFE_MAP(), gen_ordering_fun()},
        %% only check that every map entry is traversed
        lists:all(fun(MapOrIter) ->
                      Tag = make_ref(),
                      maps:foreach(fun(K, V) ->
                                       self() ! {Tag, K, V}
                                   end,
                                   MapOrIter),
                      self() ! {Tag, stop},
                      lists_same_contents(collect_messages(Tag), maps:to_list(InMap))
                  end,
                  map_and_iterators(InMap, IterOrderFun))
    ).

collect_messages(Tag) ->
    receive
        {Tag, K, V} ->
            [{K, V}|collect_messages(Tag)];
        {Tag, stop} ->
            []
    after 100 ->
        error(timeout)
    end.

%% fold/3
prop_fold() ->
    ?FORALL(
        {InMap, IterOrderFun},
        {?CT_SAFE_MAP(), gen_ordering_fun()},
        lists:all(fun(MapOrIter) ->
                      Tag = make_ref(),
                      Result = maps:fold(fun(K, V, Acc) -> [{K, V}|Acc] end, [Tag], MapOrIter),
                      lists_same_contents(Result, [Tag|maps:to_list(InMap)])
                  end,
                  map_and_iterators(InMap, IterOrderFun))
    ).

%% map/2
prop_map() ->
    ?FORALL(
        {InMap, MapFun, IterOrderFun},
        {?CT_SAFE_MAP(), function2(?CT_SAFE_ANY()), gen_ordering_fun()},
        begin
            lists:all(fun(MapOrIter) ->
                          MappedMap = maps:map(MapFun, MapOrIter),
                          %% same keys before and after mapping
                          lists_same_contents(maps:keys(InMap), maps:keys(MappedMap)) andalso
                          %% all values were mapped correctly
                          lists:all(fun
                                       ({K, V}) when is_map_key(K, MappedMap) ->
                                           #{K := MappedV} = MappedMap,
                                           MappedV =:= MapFun(K, V);
                                       (_) ->
                                           false
                                    end,
                                    maps:to_list(InMap))
                      end,
                      map_and_iterators(InMap, IterOrderFun))
        end
    ).

%% filter/2
prop_filter() ->
    ?FORALL(
        {InMap, FilterFun, IterOrderFun},
        {?CT_SAFE_MAP(), function2(bool()), gen_ordering_fun()},
        begin
            lists:all(fun(MapOrIter) ->
                          FilteredMap = maps:filter(FilterFun, MapOrIter),
                          %% all entries in the input map were filtered correctly
                          lists:all(fun({K, V}) ->
                                        case FilteredMap of
                                            #{K := V} ->
                                                %% entry exists in the output map with the same value
                                                true =:= FilterFun(K, V);
                                            #{K := _} ->
                                                %% entry exists in the output map with a different value
                                                false;
                                            #{} ->
                                                %% entry does not exist in the output map
                                                false =:= FilterFun(K, V)
                                        end
                                    end,
                                    maps:to_list(InMap)) andalso
                          %% all entries in the output map are the result of correct filtering of the input map
                          lists:all(fun({K, V}) ->
                                        case InMap of
                                            #{K := V} ->
                                                %% entry exists in the input map with the same value
                                                true =:= FilterFun(K, V);
                                            #{} ->
                                                %% entry does not exist in the input map or has a different value
                                                false
                                        end
                                    end,
                                    maps:to_list(FilteredMap))
                      end,
                      map_and_iterators(InMap, IterOrderFun))
        end
    ).

%% filtermap/2
prop_filtermap() ->
    ?FORALL(
        {InMap, FilterMapFun, IterOrderFun},
        {?CT_SAFE_MAP(), function2(oneof([false, true, {true, ?CT_SAFE_ANY()}])), gen_ordering_fun()},
        begin
            lists:all(fun(MapOrIter) ->
                          FilterMappedMap = maps:filtermap(FilterMapFun, MapOrIter),
                          %% all entries in the input map were filtered/mapped correctly
                          lists:all(fun({K, V}) ->
                                        case FilterMappedMap of
                                            #{K := V} ->
                                                %% entry exists in the output map with the same value
                                                Res = FilterMapFun(K, V),
                                                true =:= Res orelse {true, V} =:= Res;
                                            #{K := V2} ->
                                                %% entry exists in the output map with a different value
                                                {true, V2} =:= FilterMapFun(K, V);
                                            #{} ->
                                                %% entry does not exist in the output map
                                                false =:= FilterMapFun(K, V)
                                        end
                                    end,
                                    maps:to_list(InMap)) andalso
                          %% all entries in the output map are the result of correct filtering/mapping of the input map
                          lists:all(fun({K, V}) ->
                                        case InMap of
                                            #{K := V} ->
                                                %% entry exists in the input map with the same value
                                                Res = FilterMapFun(K, V),
                                                true =:= Res orelse {true, V} =:= Res;
                                            #{K := V2} ->
                                                %% entry exists in the input map with a different value
                                                {true, V} =:= FilterMapFun(K, V2);
                                            #{} ->
                                                %% entry does not exist in the input map
                                                false
                                        end
                                    end,
                                    maps:to_list(FilterMappedMap))
                      end,
                      map_and_iterators(InMap, IterOrderFun))
        end
    ).

%% merge/2
prop_merge() ->
    ?FORALL(
        {InMap1, InMap2},
        gen_overlapping_maps(),
        begin
            MergedMap = maps:merge(InMap1, InMap2),
            %% every key existing in the first input map exists in the output map
            lists:all(fun(K) ->
                          is_map_key(K, MergedMap)
                      end,
                      maps:keys(InMap1)) andalso
            %% every key existing in the second input map exists in the output map
            lists:all(fun(K) ->
                          is_map_key(K, MergedMap)
                      end,
                      maps:keys(InMap2)) andalso
            %% every key existing in the output map exists in the first or the second input map, or in both
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) orelse is_map_key(K, InMap2)
                      end,
                      maps:keys(MergedMap)) andalso
            %% every entry in the output map is correct, ie it has the value from the second input map
            %% if it exists there, and if not the value from the first input map
            lists:all(fun({K, V}) ->
                          case {InMap1, InMap2} of
                              {#{}, #{K := V2}} ->
                                  %% entry exists in the second input map
                                  V =:= V2;
                              {#{K := V1}, #{}} ->
                                  %% entry exists in the first input map
                                  V =:= V1;
                              _ ->
                                  %% entry does not exist in the first and second input maps
                                  false
                          end
                      end,
                      maps:to_list(MergedMap))
        end
    ).

%% merge_with/3
prop_merge_with() ->
    ?FORALL(
        {{InMap1, InMap2}, MergeFun},
        {gen_overlapping_maps(), function3(?CT_SAFE_ANY())},
        begin
            MergedMap = maps:merge_with(MergeFun, InMap1, InMap2),
            %% every key existing in the first input map exists in the output map
            lists:all(fun(K) ->
                          is_map_key(K, MergedMap)
                      end,
                      maps:keys(InMap1)) andalso
            %% every key existing in the second input map exists in the output map
            lists:all(fun(K) ->
                          is_map_key(K, MergedMap)
                      end,
                      maps:keys(InMap2)) andalso
            %% every key existing in the output map exists in the first or the second input map, or in both
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) orelse is_map_key(K, InMap2)
                      end,
                      maps:keys(MergedMap)) andalso
            %% every entry in the output map is correct, ie it has the value combined from the first and
            %% second input maps iff the key existed in both, otherwise the one from the first or second
            %% input maps in which it existed
            lists:all(fun({K, V}) ->
                          case {InMap1, InMap2} of
                              {#{K := V1}, #{K := V2}} ->
                                  %% key exists in both input maps
                                  V =:= MergeFun(K, V1, V2);
                              {#{}, #{K := V2}} ->
                                  %% key exists in the second input map
                                  V =:= V2;
                              {#{K := V1}, #{}} ->
                                  %% key exists in the first input map
                                  V =:= V1;
                              _ ->
                                  %% key does not exist in either input map
                                  false
                          end
                      end,
                      maps:to_list(MergedMap))
        end
    ).

%% intersect/2
prop_intersect() ->
    ?FORALL(
        {InMap1, InMap2},
        gen_overlapping_maps(),
        begin
            IntersectedMap = maps:intersect(InMap1, InMap2),
            %% every key existing in the first input map exists in the output map iff it also exists in the second input map
            lists:all(fun(K) ->
                          is_map_key(K, InMap2) =:= is_map_key(K, IntersectedMap)
                      end,
                      maps:keys(InMap1)) andalso
            %% every key existing in the second input map exists in the output map iff it also exists in the first input map
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) =:= is_map_key(K, IntersectedMap)
                      end,
                      maps:keys(InMap2)) andalso
            %% every key existing in the output map exists in both the first as well as the second input maps
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) andalso is_map_key(K, InMap2)
                      end,
                      maps:keys(IntersectedMap)) andalso
            %% every entry in the output map is correct, ie it has the value from the second input map
            lists:all(fun({K, V}) ->
                          #{K := V2} = InMap2,
                          V =:= V2
                      end,
                      maps:to_list(IntersectedMap))
        end
    ).

%% intersect_with/3
prop_intersect_with() ->
    ?FORALL(
        {{InMap1, InMap2}, IntersectFun},
        {gen_overlapping_maps(), function3(?CT_SAFE_ANY())},
        begin
            IntersectedMap = maps:intersect_with(IntersectFun, InMap1, InMap2),
            %% every key existing in the first input map exists in the output map iff it also exists in the second input map
            lists:all(fun(K) ->
                          is_map_key(K, InMap2) =:= is_map_key(K, IntersectedMap)
                      end,
                      maps:keys(InMap1)) andalso
            %% every key existing in the second input map exists in the output map iff it also exists in the first input map
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) =:= is_map_key(K, IntersectedMap)
                      end,
                      maps:keys(InMap2)) andalso
            %% every key existing in the output map exists in both the first as well as the second input maps
            lists:all(fun(K) ->
                          is_map_key(K, InMap1) andalso is_map_key(K, InMap2)
                      end,
                      maps:keys(IntersectedMap)) andalso
            %% every entry in the output map is correct, ie it has the value combined via the intersect function
            lists:all(fun({K, V}) ->
                          #{K := V1} = InMap1,
                          #{K := V2} = InMap2,
                          V =:= IntersectFun(K, V1, V2)
                      end,
                      maps:to_list(IntersectedMap))
        end
    ).

%% groups_from_list/2
prop_groups_from_list_2() ->
    ?FORALL(
        {InList, KeyFun},
        {?CT_SAFE_LIST(),
         gen_restricted_fun(10)},
        begin
            GroupsMap = maps:groups_from_list(KeyFun, InList),
            %% all elements of the input list appear somewhere in the group lists of the output map
            lists_same_contents(InList, lists:append(maps:values(GroupsMap))) andalso
            %% all keys in the output map were generated by an element in the input list via the key function
            lists_same_contents(maps:keys(GroupsMap), lists:uniq([KeyFun(E) || E <- InList])) andalso
            %% each group list in the output map contains all and only those values from the input list
            %% for which the key function returned the respective key
            lists:all(fun({K, V}) ->
                          V =:= [E || E <- InList, K =:= KeyFun(E)]
                      end,
                      maps:to_list(GroupsMap))
        end
    ).

%% groups_from_list/3
prop_groups_from_list_3() ->
    ?FORALL(
        {InList, KeyFun, ValueFun},
        {?CT_SAFE_LIST(),
         gen_restricted_fun(10),
         function1(?CT_SAFE_ANY())},
        begin
            GroupsMap = maps:groups_from_list(KeyFun, ValueFun, InList),
            %% all elements of the input list appear somewhere in the group lists of the output map
            lists_same_contents([ValueFun(E) || E <- InList], lists:append(maps:values(GroupsMap))) andalso
            %% all keys in the output map were generated by an element in the input list via the key function
            lists_same_contents(maps:keys(GroupsMap), lists:uniq([KeyFun(E) || E <- InList])) andalso
            %% each group list in the output map contains all and only those values from the input list,
            %% mapped via the value function, for which the key function returned the respective key
            lists:all(fun({K, V}) ->
                          V =:= [ValueFun(E) || E <- InList, K =:= KeyFun(E)]
                      end,
                      maps:to_list(GroupsMap))
        end
    ).

%%%%%%%%%%%%%%%%%%
%%% Generators %%%
%%%%%%%%%%%%%%%%%%

%% generator for a function which can be used as a comparison function
gen_ordering_fun() ->
    ?LET(
        F,
        function1(choose(1, 3)),
        fun(T1, T2) ->
            F(T1) =< F(T2)
        end
    ).

%% generator for a random sublist of the given list
gen_sublist(L) ->
    ?LET(
        F,
        function1(bool()),
        [E || E <- L, F(E)]
    ).

%% generator for a pair of possibly overlapping maps
%%
%% The purpose of this generator is to generate maps with a
%% higher rate of overlapping keys and/or common key-value pairs.
%% Using the built-in maps generator would produce almost
%% no overlap.
gen_overlapping_maps() ->
    ?LET(
        {KVs1, KVs2, CommonKVs, CommonKeyKVs, F1, F2},
        {
            %% entries intended for the first map
            gen_kv_list(),
            %% entries intended for the second map
            gen_kv_list(),

            %% entries intended to be in both maps with same values
            oneof([[], gen_kv_list()]),

            %% entries intended to be in both maps but with different values
            oneof([[], gen_kv_list()]),
            %% helper to transform values for the first map
            function1(?CT_SAFE_ANY()),
            %% helper to transform values for the second map
            function1(?CT_SAFE_ANY())
        },
        {maps:from_list(CommonKVs ++ [{K, F1(V)} || {K, V} <- CommonKeyKVs] ++ KVs1),
         maps:from_list(CommonKVs ++ [{K, F2(V)} || {K, V} <- CommonKeyKVs] ++ KVs2)}
    ).

%% generator for a function with a limited number of possible outputs
gen_restricted_fun(N) ->
    ?LET(
        {F1, F2},
        {function1(choose(1, N)), function1(?CT_SAFE_ANY())},
        fun(T) ->
            F2(F1(T))
        end
    ).

%% generator for a list of random `{Key, Value}' tuples
gen_kv_list() ->
    list({?CT_SAFE_ANY(), ?CT_SAFE_ANY()}).

%% generator for a map and a term which may be a key of that map or not (roughly 50/50 chance)
gen_map_and_key() ->
    ?LET(
        {M, K},
        {?CT_SAFE_MAP(), ?CT_SAFE_ANY()},
        {M,
         if
             #{} =:= M ->
                 K;
             true ->
                 oneof([K, oneof(maps:keys(M))])
         end}
    ).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%

%% checks if the argument lists contain the same elements, regardless of order
lists_same_contents([E|L1], [_|_]=L2) ->
    lists:member(E, L2) andalso lists_same_contents(L1, lists:delete(E, L2));
lists_same_contents([], []) ->
    true;
lists_same_contents(_, _) ->
    false.

%% checks if all and only the key-value pairs in the first argument list
%% exist in the second argument list as the respective last elements
%% with the same key
lists_same_kvs([{K, V}|L1], [_|_]=L2) ->
    {Found, NewL2} = lists:foldl(fun
                                     ({K2, V2}, {_, AccL}) when K2 =:= K, V2 =:= V ->
                                         {true, AccL};
                                     ({K2, _}, {_, AccL}) when K2 =:= K ->
                                         {false, AccL};
                                     (KV, {AccFound, AccL}) ->
                                         {AccFound, [KV|AccL]}
                                 end,
                                 {false, []},
                                 L2),
    Found andalso lists_same_kvs(L1, lists:reverse(NewL2));
lists_same_kvs([], []) ->
    true;
lists_same_kvs(_, _) ->
    false.

%% checks if the given list is sorted according to the given ordering function
list_ordered_by([{K1, _}|[{K2, _}|_]=L], F) ->
    F(K1, K2) andalso list_ordered_by(L, F);
list_ordered_by([{_, _}], _) ->
    true;
list_ordered_by([], _) ->
    true.

%% creates a map from the given kv-list, excluding the given key
map_from_list_without_key(K, L) ->
    lists:foldl(fun
                    ({K1, _}, Acc) when K1 =:= K ->
                        Acc;
                    ({K1, V}, Acc) ->
                        Acc#{K1 => V}
                end,
                #{},
                L).

%% returns a list consisting of the given map and iterators
map_and_iterators(M, IterOrderFun) ->
    [M,
     maps:iterator(M),
     maps:iterator(M, undefined),
     maps:iterator(M, ordered),
     maps:iterator(M, reversed),
     maps:iterator(M, IterOrderFun)].
