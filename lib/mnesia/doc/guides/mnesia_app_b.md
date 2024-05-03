<!--
%CopyrightBegin%

Copyright Ericsson AB 2023-2024. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

%CopyrightEnd%
-->
# Appendix B: Activity Access Callback Interface

## mnesia_access Callback Behavior

```erlang


-module(mnesia_frag).

%% Callback functions when accessed within an activity
-export([
	 lock/4,
	 write/5, delete/5, delete_object/5,
	 read/5, match_object/5, all_keys/4,
	 select/5,select/6,select_cont/3,
	 index_match_object/6, index_read/6,
	 foldl/6, foldr/6, table_info/4,
	 first/3, next/4, prev/4, last/3,
	 clear_table/4
       ]).
```

```erlang


%% Callback functions which provides transparent
%% access of fragmented tables from any activity
%% access context.

lock(ActivityId, Opaque, {table , Tab}, LockKind) ->
    case frag_names(Tab) of
	[Tab] ->
	    mnesia:lock(ActivityId, Opaque, {table, Tab}, LockKind);
	Frags ->
	    DeepNs = [mnesia:lock(ActivityId, Opaque, {table, F}, LockKind) ||
			 F <- Frags],
	    mnesia_lib:uniq(lists:append(DeepNs))
    end;

lock(ActivityId, Opaque, LockItem, LockKind) ->
    mnesia:lock(ActivityId, Opaque, LockItem, LockKind).

write(ActivityId, Opaque, Tab, Rec, LockKind) ->
    Frag = record_to_frag_name(Tab, Rec),
    mnesia:write(ActivityId, Opaque, Frag, Rec, LockKind).

delete(ActivityId, Opaque, Tab, Key, LockKind) ->
    Frag = key_to_frag_name(Tab, Key),
    mnesia:delete(ActivityId, Opaque, Frag, Key, LockKind).

delete_object(ActivityId, Opaque, Tab, Rec, LockKind) ->
    Frag = record_to_frag_name(Tab, Rec),
    mnesia:delete_object(ActivityId, Opaque, Frag, Rec, LockKind).

read(ActivityId, Opaque, Tab, Key, LockKind) ->
    Frag = key_to_frag_name(Tab, Key),
    mnesia:read(ActivityId, Opaque, Frag, Key, LockKind).

match_object(ActivityId, Opaque, Tab, HeadPat, LockKind) ->
    MatchSpec = [{HeadPat, [], ['$_']}],
    select(ActivityId, Opaque, Tab, MatchSpec, LockKind).

select(ActivityId, Opaque, Tab, MatchSpec, LockKind) ->
    do_select(ActivityId, Opaque, Tab, MatchSpec, LockKind).


select(ActivityId, Opaque, Tab, MatchSpec, Limit, LockKind) ->
    init_select(ActivityId, Opaque, Tab, MatchSpec, Limit, LockKind).


all_keys(ActivityId, Opaque, Tab, LockKind) ->
    Match = [mnesia:all_keys(ActivityId, Opaque, Frag, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:append(Match).

clear_table(ActivityId, Opaque, Tab, Obj) ->
    [mnesia:clear_table(ActivityId, Opaque, Frag, Obj)  || Frag <- frag_names(Tab)],
    ok.

index_match_object(ActivityId, Opaque, Tab, Pat, Attr, LockKind) ->
    Match =
	[mnesia:index_match_object(ActivityId, Opaque, Frag, Pat, Attr, LockKind)
	 || Frag <- frag_names(Tab)],
    lists:append(Match).

index_read(ActivityId, Opaque, Tab, Key, Attr, LockKind) ->
    Match =
	[mnesia:index_read(ActivityId, Opaque, Frag, Key, Attr, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:append(Match).

foldl(ActivityId, Opaque, Fun, Acc, Tab, LockKind) ->
    Fun2 = fun(Frag, A) ->
		   mnesia:foldl(ActivityId, Opaque, Fun, A, Frag, LockKind)
	   end,
    lists:foldl(Fun2, Acc, frag_names(Tab)).

foldr(ActivityId, Opaque, Fun, Acc, Tab, LockKind) ->
    Fun2 = fun(Frag, A) ->
		   mnesia:foldr(ActivityId, Opaque, Fun, A, Frag, LockKind)
	   end,
    lists:foldr(Fun2, Acc, frag_names(Tab)).

table_info(ActivityId, Opaque, {Tab, Key}, Item) ->
    Frag = key_to_frag_name(Tab, Key),
    table_info2(ActivityId, Opaque, Tab, Frag, Item);
table_info(ActivityId, Opaque, Tab, Item) ->
    table_info2(ActivityId, Opaque, Tab, Tab, Item).

table_info2(ActivityId, Opaque, Tab, Frag, Item) ->
    case Item of
	size ->
	    SumFun = fun({_, Size}, Acc) -> Acc + Size end,
	    lists:foldl(SumFun, 0, frag_size(ActivityId, Opaque, Tab));
	memory ->
	    SumFun = fun({_, Size}, Acc) -> Acc + Size end,
	    lists:foldl(SumFun, 0, frag_memory(ActivityId, Opaque, Tab));
	base_table ->
	    lookup_prop(Tab, base_table);
	node_pool ->
	    lookup_prop(Tab, node_pool);
	n_fragments ->
	    FH = lookup_frag_hash(Tab),
	    FH#frag_state.n_fragments;
	foreign_key ->
	    FH = lookup_frag_hash(Tab),
	    FH#frag_state.foreign_key;
	foreigners ->
	    lookup_foreigners(Tab);
	n_ram_copies ->
	    length(val({Tab, ram_copies}));
	n_disc_copies ->
	    length(val({Tab, disc_copies}));
	n_disc_only_copies ->
	    length(val({Tab, disc_only_copies}));
	n_external_copies ->
	    length(val({Tab, external_copies}));

	frag_names ->
	    frag_names(Tab);
	frag_dist ->
	    frag_dist(Tab);
	frag_size ->
	    frag_size(ActivityId, Opaque, Tab);
	frag_memory ->
	    frag_memory(ActivityId, Opaque, Tab);
	_ ->
	    mnesia:table_info(ActivityId, Opaque, Frag, Item)
    end.

first(ActivityId, Opaque, Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    mnesia:first(ActivityId, Opaque, Tab);
	FH ->
	    FirstFrag = Tab,
	    case mnesia:first(ActivityId, Opaque, FirstFrag) of
		'$end_of_table' ->
		    search_first(ActivityId, Opaque, Tab, 1, FH);
		Next ->
		    Next
	    end
    end.

search_first(ActivityId, Opaque, Tab, N, FH) when N < FH#frag_state.n_fragments ->
    NextN = N + 1,
    NextFrag = n_to_frag_name(Tab, NextN),
    case mnesia:first(ActivityId, Opaque, NextFrag) of
	'$end_of_table' ->
	    search_first(ActivityId, Opaque, Tab, NextN, FH);
	Next ->
	    Next
    end;
search_first(_ActivityId, _Opaque, _Tab, _N, _FH) ->
    '$end_of_table'.

last(ActivityId, Opaque, Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    mnesia:last(ActivityId, Opaque, Tab);
	FH ->
	    LastN = FH#frag_state.n_fragments,
	    search_last(ActivityId, Opaque, Tab, LastN, FH)
    end.

search_last(ActivityId, Opaque, Tab, N, FH) when N >= 1 ->
    Frag = n_to_frag_name(Tab, N),
    case mnesia:last(ActivityId, Opaque, Frag) of
	'$end_of_table' ->
	    PrevN = N - 1,
	    search_last(ActivityId, Opaque, Tab, PrevN, FH);
	Prev ->
	    Prev
    end;
search_last(_ActivityId, _Opaque, _Tab, _N, _FH) ->
    '$end_of_table'.

prev(ActivityId, Opaque, Tab, Key) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    mnesia:prev(ActivityId, Opaque, Tab, Key);
	FH ->
	    N = key_to_n(FH, Key),
	    Frag = n_to_frag_name(Tab, N),
	    case mnesia:prev(ActivityId, Opaque, Frag, Key) of
		'$end_of_table' ->
		    search_prev(ActivityId, Opaque, Tab, N);
		Prev ->
		    Prev
	    end
    end.

search_prev(ActivityId, Opaque, Tab, N) when N > 1 ->
    PrevN = N - 1,
    PrevFrag = n_to_frag_name(Tab, PrevN),
    case mnesia:last(ActivityId, Opaque, PrevFrag) of
	'$end_of_table' ->
	    search_prev(ActivityId, Opaque, Tab, PrevN);
	Prev ->
	    Prev
    end;
search_prev(_ActivityId, _Opaque, _Tab, _N) ->
    '$end_of_table'.

next(ActivityId, Opaque, Tab, Key) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    mnesia:next(ActivityId, Opaque, Tab, Key);
	FH ->
	    N = key_to_n(FH, Key),
	    Frag = n_to_frag_name(Tab, N),
	    case mnesia:next(ActivityId, Opaque, Frag, Key) of
		'$end_of_table' ->
		    search_next(ActivityId, Opaque, Tab, N, FH);
		Prev ->
		    Prev
	    end
    end.

search_next(ActivityId, Opaque, Tab, N, FH) when N < FH#frag_state.n_fragments ->
    NextN = N + 1,
    NextFrag = n_to_frag_name(Tab, NextN),
    case mnesia:first(ActivityId, Opaque, NextFrag) of
	'$end_of_table' ->
	    search_next(ActivityId, Opaque, Tab, NextN, FH);
	Next ->
	    Next
    end;
search_next(_ActivityId, _Opaque, _Tab, _N, _FH) ->
    '$end_of_table'.
```
