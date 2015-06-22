%% ``Licensed under the Apache License, Version 2.0 (the "License");
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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
%%     $Id: mnesia_index.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%% Purpose: Handles index functionality in mnesia

-module(mnesia_index).
-export([read/5,
	 add_index/5,
	 delete_index/3,
	 del_object_index/5,
	 clear_index/4,
	 dirty_match_object/3,
	 dirty_select/3,
	 dirty_read/3,
	 dirty_read2/3,

	 db_put/2,
	 db_get/2,
	 db_match_erase/2,
	 get_index_table/2,
	 get_index_table/3,

	 tab2filename/2,
	 tab2tmp_filename/2,
	 init_index/2,
	 init_indecies/3,
	 del_transient/2,
	 del_transient/3,
	 del_index_table/3]).

-import(mnesia_lib, [verbose/2]).
-include("mnesia.hrl").

-record(index, {setorbag, pos_list}).

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_);
	_VaLuE_ -> _VaLuE_
    end.

%% read an object list throuh its index table
%% we assume that table Tab has index on attribute number Pos

read(Tid, Store, Tab, IxKey, Pos) ->
    ResList = mnesia_locker:ixrlock(Tid, Store, Tab, IxKey, Pos),
    %% Remove all tuples which don't include Ixkey, happens when Tab is a bag
    case val({Tab, setorbag}) of
	bag ->
	    mnesia_lib:key_search_all(IxKey, Pos, ResList);
	_ ->
	    ResList
    end.

add_index(Index, Tab, Key, Obj, Old) ->
    add_index2(Index#index.pos_list, Index#index.setorbag, Tab, Key, Obj, Old).

add_index2([{Pos, Ixt} |Tail], bag, Tab, K, Obj, OldRecs) ->
    db_put(Ixt, {element(Pos, Obj), K}),
    add_index2(Tail, bag, Tab, K, Obj, OldRecs);
add_index2([{Pos, Ixt} |Tail], Type, Tab, K, Obj, OldRecs) ->
    %% Remove old tuples in index if Tab is updated
    case OldRecs of
	undefined ->
	    Old = mnesia_lib:db_get(Tab, K),
	    del_ixes(Ixt, Old, Pos, K);
	Old ->
	    del_ixes(Ixt, Old, Pos, K)
    end,
    db_put(Ixt, {element(Pos, Obj), K}),
    add_index2(Tail, Type, Tab, K, Obj, OldRecs);
add_index2([], _, _Tab, _K, _Obj, _) -> ok.

delete_index(Index, Tab, K) ->
    delete_index2(Index#index.pos_list, Tab, K).

delete_index2([{Pos, Ixt} | Tail], Tab, K) ->
    DelObjs = mnesia_lib:db_get(Tab, K),
    del_ixes(Ixt, DelObjs, Pos, K),
    delete_index2(Tail, Tab, K);
delete_index2([], _Tab, _K) -> ok.


del_ixes(_Ixt, [], _Pos, _L) -> ok;
del_ixes(Ixt, [Obj | Tail], Pos, Key) ->
    db_match_erase(Ixt, {element(Pos, Obj), Key}),
    del_ixes(Ixt, Tail, Pos, Key).

del_object_index(Index, Tab, K, Obj, Old) ->
    del_object_index2(Index#index.pos_list, Index#index.setorbag, Tab, K, Obj, Old).

del_object_index2([], _, _Tab, _K, _Obj, _Old) -> ok;
del_object_index2([{Pos, Ixt} | Tail], SoB, Tab, K, Obj, Old) ->
    case SoB of
	bag ->
	    del_object_bag(Tab, K, Obj, Pos, Ixt, Old);
	_ -> %% If set remove the tuple in index table
	    del_ixes(Ixt, [Obj], Pos, K)
    end,
    del_object_index2(Tail, SoB, Tab, K, Obj, Old).

del_object_bag(Tab, Key, Obj, Pos, Ixt, undefined) ->
    Old = mnesia_lib:db_get(Tab, Key),
    del_object_bag(Tab, Key, Obj, Pos, Ixt, Old);
%% If Tab type is bag we need remove index identifier if Tab
%% contains less than 2 elements.
del_object_bag(_Tab, Key, Obj, Pos, Ixt, Old) when length(Old) < 2 ->
    del_ixes(Ixt, [Obj], Pos, Key);
del_object_bag(_Tab, _Key, _Obj, _Pos, _Ixt, _Old) -> ok.

clear_index(Index, Tab, K, Obj) ->
    clear_index2(Index#index.pos_list, Tab, K, Obj).

clear_index2([], _Tab, _K, _Obj) -> ok;
clear_index2([{_Pos, Ixt} | Tail], Tab, K, Obj) ->
    db_match_erase(Ixt, Obj),
    clear_index2(Tail, Tab, K, Obj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirty_match_object(Tab, Pat, Pos) ->
    %% Assume that we are on the node where the replica is
    case element(2, Pat) of
	'_' ->
	    IxKey = element(Pos, Pat),
	    RealKeys = realkeys(Tab, Pos, IxKey),
	    merge(RealKeys, Tab, Pat, []);
	_Else ->
	    mnesia_lib:db_match_object(Tab, Pat)
    end.

merge([{_IxKey, RealKey} | Tail], Tab, Pat, Ack) ->
    %% Assume that we are on the node where the replica is
    Pat2 = setelement(2, Pat, RealKey),
    Recs = mnesia_lib:db_match_object(Tab, Pat2),
    merge(Tail, Tab, Pat, Recs ++ Ack);
merge([], _, _, Ack) ->
    Ack.

realkeys(Tab, Pos, IxKey) ->
    Index = get_index_table(Tab, Pos),
    db_get(Index, IxKey). % a list on the form [{IxKey, RealKey1} , ....

dirty_select(Tab, Spec, Pos) ->
    %% Assume that we are on the node where the replica is
    %% Returns the records without applying the match spec
    %% The actual filtering is handled by the caller
    IxKey = element(Pos, Spec),
    RealKeys = realkeys(Tab, Pos, IxKey),
    StorageType = val({Tab, storage_type}),
    lists:append([mnesia_lib:db_get(StorageType, Tab, Key) || Key <- RealKeys]).

dirty_read(Tab, IxKey, Pos) ->
    ResList = mnesia:dirty_rpc(Tab, ?MODULE, dirty_read2,
			       [Tab, IxKey, Pos]),
    case val({Tab, setorbag}) of
	bag ->
	    %% Remove all tuples which don't include Ixkey
	    mnesia_lib:key_search_all(IxKey, Pos, ResList);
	_ ->
	    ResList
    end.

dirty_read2(Tab, IxKey, Pos) ->
    Ix = get_index_table(Tab, Pos),
    Keys = db_match(Ix, {IxKey, '$1'}),
    r_keys(Keys, Tab, []).

r_keys([[H]|T],Tab,Ack) ->
    V = mnesia_lib:db_get(Tab, H),
    r_keys(T, Tab, V ++ Ack);
r_keys([], _, Ack) ->
    Ack.


%%%%%%% Creation, Init and deletion routines for index tables
%% We can have several indexes on the same table
%% this can be a fairly costly operation if table is *very* large

tab2filename(Tab, Pos) ->
    mnesia_lib:dir(Tab) ++ "_" ++ integer_to_list(Pos) ++ ".DAT".

tab2tmp_filename(Tab, Pos) ->
    mnesia_lib:dir(Tab) ++ "_" ++ integer_to_list(Pos) ++ ".TMP".

init_index(Tab, Storage) ->
    PosList = val({Tab, index}),
    init_indecies(Tab, Storage, PosList).

init_indecies(Tab, Storage, PosList) ->
    case Storage of
	unknown ->
	    ignore;
	disc_only_copies ->
	    init_disc_index(Tab, PosList);
	ram_copies ->
	    make_ram_index(Tab, PosList);
	disc_copies ->
	    make_ram_index(Tab, PosList)
    end.

%% works for both ram and disc indexes

del_index_table(_, unknown, _) ->
    ignore;
del_index_table(Tab, Storage, Pos) ->
    delete_transient_index(Tab, Pos, Storage),
    mnesia_lib:del({Tab, index}, Pos).

del_transient(Tab, Storage) ->
    PosList = val({Tab, index}),
    del_transient(Tab, PosList, Storage).

del_transient(_, [], _) -> done;
del_transient(Tab, [Pos | Tail], Storage) ->
    delete_transient_index(Tab, Pos, Storage),
    del_transient(Tab, Tail, Storage).

delete_transient_index(Tab, Pos, disc_only_copies) ->
    Tag = {Tab, index, Pos},
    mnesia_monitor:unsafe_close_dets(Tag),
    file:delete(tab2filename(Tab, Pos)),
    del_index_info(Tab, Pos), %% Uses val(..)
    mnesia_lib:unset({Tab, {index, Pos}});

delete_transient_index(Tab, Pos, _Storage) ->
    Ixt = val({Tab, {index, Pos}}),
    ?ets_delete_table(Ixt),
    del_index_info(Tab, Pos),
    mnesia_lib:unset({Tab, {index, Pos}}).

%%%%% misc functions for the index create/init/delete functions above

%% assuming that the file exists.
init_disc_index(_Tab, []) ->
    done;
init_disc_index(Tab, [Pos | Tail]) when integer(Pos) ->
    Fn = tab2filename(Tab, Pos),
    IxTag = {Tab, index, Pos},
    file:delete(Fn),
    Args = [{file, Fn}, {keypos, 1}, {type, bag}],
    mnesia_monitor:open_dets(IxTag, Args),
    Storage = disc_only_copies,
    Key = mnesia_lib:db_first(Storage, Tab),
    Recs = mnesia_lib:db_get(Storage, Tab, Key),
    BinSize = size(term_to_binary(Recs)),
    KeysPerChunk = (4000 div BinSize) + 1,
    Init = {start, KeysPerChunk},
    mnesia_lib:db_fixtable(Storage, Tab, true),
    ok = dets:init_table(IxTag, create_fun(Init, Tab, Pos)),
    mnesia_lib:db_fixtable(Storage, Tab, false),
    mnesia_lib:set({Tab, {index, Pos}}, IxTag),
    add_index_info(Tab, val({Tab, setorbag}), {Pos, {dets, IxTag}}),
    init_disc_index(Tab, Tail).

create_fun(Cont, Tab, Pos) ->
    fun(read) ->
	    Data =
		case Cont of
		    {start, KeysPerChunk} ->
			mnesia_lib:db_init_chunk(disc_only_copies, Tab, KeysPerChunk);
		    '$end_of_table' ->
			'$end_of_table';
		    _Else ->
			mnesia_lib:db_chunk(disc_only_copies, Cont)
		end,
	    case Data of
		'$end_of_table' ->
		    end_of_input;
		{Recs, Next} ->
		    IdxElems = [{element(Pos, Obj), element(2, Obj)} || Obj <- Recs],
		    {IdxElems, create_fun(Next, Tab, Pos)}
	    end;
       (close) ->
	    ok
    end.

make_ram_index(_, []) ->
    done;
make_ram_index(Tab, [Pos | Tail]) ->
    add_ram_index(Tab, Pos),
    make_ram_index(Tab, Tail).

add_ram_index(Tab, Pos) when integer(Pos) ->
    verbose("Creating index for ~w ~n", [Tab]),
    Index = mnesia_monitor:mktab(mnesia_index, [bag, public]),
    Insert = fun(Rec, _Acc) ->
		     true = ?ets_insert(Index, {element(Pos, Rec), element(2, Rec)})
	     end,
    mnesia_lib:db_fixtable(ram_copies, Tab, true),
    true = ets:foldl(Insert, true, Tab),
    mnesia_lib:db_fixtable(ram_copies, Tab, false),
    mnesia_lib:set({Tab, {index, Pos}}, Index),
    add_index_info(Tab, val({Tab, setorbag}), {Pos, {ram, Index}});
add_ram_index(_Tab, snmp) ->
    ok.

add_index_info(Tab, Type, IxElem) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    Index = #index{setorbag = Type,
			   pos_list = [IxElem]},
	    %% Check later if mnesia_tm is sensative about the order
	    mnesia_lib:set({Tab, commit_work},
			   mnesia_lib:sort_commit([Index | Commit]));
	{value, Old} ->
	    %% We could check for consistency here
	    Index = Old#index{pos_list = [IxElem | Old#index.pos_list]},
	    NewC = lists:keyreplace(index, 1, Commit, Index),
	    mnesia_lib:set({Tab, commit_work},
			   mnesia_lib:sort_commit(NewC))
    end.

del_index_info(Tab, Pos) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    %% Something is wrong ignore
	    skip;
	{value, Old} ->
	    case lists:keydelete(Pos, 1, Old#index.pos_list) of
		[] ->
		    NewC = lists:keydelete(index, 1, Commit),
		    mnesia_lib:set({Tab, commit_work},
				   mnesia_lib:sort_commit(NewC));
		New ->
		    Index = Old#index{pos_list = New},
		    NewC = lists:keyreplace(index, 1, Commit, Index),
		    mnesia_lib:set({Tab, commit_work},
				   mnesia_lib:sort_commit(NewC))
	    end
    end.

db_put({ram, Ixt}, V) ->
    true = ?ets_insert(Ixt, V);
db_put({dets, Ixt}, V) ->
    ok = dets:insert(Ixt, V).

db_get({ram, Ixt}, K) ->
    ?ets_lookup(Ixt, K);
db_get({dets, Ixt}, K) ->
    dets:lookup(Ixt, K).

db_match_erase({ram, Ixt}, Pat) ->
    true = ?ets_match_delete(Ixt, Pat);
db_match_erase({dets, Ixt}, Pat) ->
    ok = dets:match_delete(Ixt, Pat).

db_match({ram, Ixt}, Pat) ->
    ?ets_match(Ixt, Pat);
db_match({dets, Ixt}, Pat) ->
    dets:match(Ixt, Pat).

get_index_table(Tab, Pos) ->
    get_index_table(Tab,  val({Tab, storage_type}), Pos).

get_index_table(Tab, ram_copies, Pos) ->
    {ram,  val({Tab, {index, Pos}})};
get_index_table(Tab, disc_copies, Pos) ->
    {ram,  val({Tab, {index, Pos}})};
get_index_table(Tab, disc_only_copies, Pos) ->
    {dets, val({Tab, {index, Pos}})};
get_index_table(_Tab, unknown, _Pos) ->
    unknown.
