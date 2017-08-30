%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% Purpose: Handles index functionality in mnesia

-module(mnesia_index).
-export([read/5,
	 add_index/6,
	 delete_index/4,
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
	 init_index/2,
	 init_indecies/3,
	 del_transient/2,
	 del_transient/3,
	 del_index_table/3,

         index_info/2,
	 ext_index_instances/1]).

-import(mnesia_lib, [val/1, verbose/2]).
-include("mnesia.hrl").

-record(index, {setorbag, pos_list}).

%% read an object list through its index table
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

ext_index_instances(Tab) ->
    #index{pos_list = PosL} = val({Tab, index_info}),
    lists:foldr(
      fun({_, {{ext,Alias,Mod}, Tag}}, Acc) ->
	      [{Alias, Mod, Tag}|Acc];
	 (_, Acc) ->
	      Acc
      end, [], PosL).


add_index(#index{pos_list = PosL, setorbag = SorB},
	  Storage, Tab, Key, Obj, Old) ->
    add_index2(PosL, SorB, Storage, Tab, Key, Obj, Old).

add_index2([{{Pos,Type}, Ixt} |Tail], bag, Storage, Tab, K, Obj, OldRecs) ->
    ValsF = index_vals_f(Storage, Tab, Pos),
    Vals = ValsF(Obj),
    put_index_vals(Type, Ixt, Vals, K),
    add_index2(Tail, bag, Storage, Tab, K, Obj, OldRecs);
add_index2([{{Pos, Type}, Ixt} |Tail], SorB, Storage, Tab, K, Obj, OldRecs0) ->
    %% Remove old tuples in index if Tab is updated
    ValsF = index_vals_f(Storage, Tab, Pos),    NewVals = ValsF(Obj),
    OldRecs1 = case OldRecs0 of
		   undefined -> mnesia_lib:db_get(Storage, Tab, K);
		   _ -> OldRecs0
	       end,
    IdxVal = ValsF(Obj),
    case [Old || Old <- OldRecs1, ValsF(Old) =/= IdxVal] of
	[] when OldRecs1 =:= [] -> % Write
	    put_index_vals(Type, Ixt, NewVals, K),
	    add_index2(Tail, SorB, Storage, Tab, K, Obj, OldRecs1);
	[] -> %% when OldRecs1 =/= [] Update without modifying index field
	    add_index2(Tail, SorB, Storage, Tab, K, Obj, OldRecs1);
	OldRecs -> %% Update
	    put_index_vals(Type, Ixt, NewVals, K),
	    [del_ixes(Type, Ixt, ValsF, OldObj, K) || OldObj <- OldRecs],
	    add_index2(Tail, SorB, Storage, Tab, K, Obj, OldRecs1)
    end;
add_index2([], _, _, _Tab, _K, _Obj, _) -> ok.

delete_index(Index, Storage, Tab, K) ->
    delete_index2(Index#index.pos_list, Storage, Tab, K).

delete_index2([{{Pos, Type}, Ixt} | Tail], Storage, Tab, K) ->
    DelObjs = mnesia_lib:db_get(Storage, Tab, K),
    ValsF = index_vals_f(Storage, Tab, Pos),
    [del_ixes(Type, Ixt, ValsF, Obj, K) || Obj <- DelObjs],
    delete_index2(Tail, Storage, Tab, K);
delete_index2([], _Storage, _Tab, _K) -> ok.

put_index_vals(ordered, Ixt, Vals, K) ->
    [db_put(Ixt, {{V, K}}) || V <- Vals];
put_index_vals(bag, Ixt, Vals, K) ->
    [db_put(Ixt, {V, K}) || V <- Vals].


del_ixes(bag, Ixt, ValsF, Obj, Key) ->
    Vals = ValsF(Obj),
    [db_match_erase(Ixt, {V, Key}) || V <- Vals];
del_ixes(ordered, Ixt, ValsF, Obj, Key) ->
    Vals = ValsF(Obj),
    [db_erase(Ixt, {V,Key}) || V <- Vals].

del_object_index(#index{pos_list = PosL, setorbag = SorB}, Storage, Tab, K, Obj) ->
    del_object_index2(PosL, SorB, Storage, Tab, K, Obj).

del_object_index2([], _, _Storage, _Tab, _K, _Obj) -> ok;
del_object_index2([{{Pos, Type}, Ixt} | Tail], SoB, Storage, Tab, K, Obj) ->
    ValsF = index_vals_f(Storage, Tab, Pos),
    case SoB of
	bag -> 
	    del_object_bag(Type, ValsF, Tab, K, Obj, Ixt);
	_ -> %% If set remove the tuple in index table
	    del_ixes(Type, Ixt, ValsF, Obj, K)
    end,
    del_object_index2(Tail, SoB, Storage, Tab, K, Obj).

del_object_bag(Type, ValsF, Tab, Key, Obj, Ixt) ->
    IxKeys = ValsF(Obj),
    Found = [{X, ValsF(X)} || X <- mnesia_lib:db_get(Tab, Key)],
    del_object_bag_(IxKeys, Found, Type, Tab, Key, Obj, Ixt).

del_object_bag_([IxK|IxKs], Found, Type, Tab, Key, Obj, Ixt) ->
    case [X || {X, Ixes} <- Found, lists:member(IxK, Ixes)] of
        [Old] when Old =:= Obj ->
	    case Type of
		bag ->
                    db_match_erase(Ixt, {IxK, Key});
		ordered ->
		    db_erase(Ixt, {{IxK, Key}})
	    end;
        _ ->
	    ok
    end,
    del_object_bag_(IxKs, Found, Type, Tab, Key, Obj, Ixt);
del_object_bag_([], _, _, _, _, _, _) ->
    ok.

clear_index(Index, Tab, K, Obj) ->
    clear_index2(Index#index.pos_list, Tab, K, Obj).

clear_index2([], _Tab, _K, _Obj) -> ok;
clear_index2([{_Pos, Ixt} | Tail], Tab, K, Obj) ->
    db_match_erase(Ixt, Obj),
    clear_index2(Tail, Tab, K, Obj).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dirty_match_object(Tab, Pat, Pos) when is_integer(Pos) ->
    %% Assume that we are on the node where the replica is
    %% Cannot use index plugins here, as they don't map to match patterns
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
    
dirty_select(Tab, Spec, Pos) when is_integer(Pos) ->
    %% Assume that we are on the node where the replica is
    %% Returns the records without applying the match spec
    %% The actual filtering is handled by the caller
    IxKey = element(Pos, Spec),
    RealKeys = realkeys(Tab, Pos, IxKey),
    StorageType = val({Tab, storage_type}),
    lists:append([mnesia_lib:db_get(StorageType, Tab, Key) || {_,Key} <- RealKeys]).

dirty_read(Tab, IxKey, Pos) ->
    mnesia:dirty_rpc(Tab, ?MODULE, dirty_read2,
		     [Tab, IxKey, Pos]).

dirty_read2(Tab, IxKey, Pos) ->
    #index{pos_list = PosL} = val({Tab, index_info}),
    Storage = val({Tab, storage_type}),
    {Type, Ixt} = pick_index(PosL, Tab, Pos),
    Pat = case Type of
	      ordered -> [{{{IxKey, '$1'}}, [], ['$1']}];
	      bag     -> [{{IxKey, '$1'}, [], ['$1']}]
	  end,
    Keys = db_select(Ixt, Pat),
    ValsF = index_vals_f(Storage, Tab, Pos),
    lists:reverse(
      lists:foldl(
	fun(K, Acc) ->
		lists:foldl(
		  fun(Obj, Acc1) ->
			  case lists:member(IxKey, ValsF(Obj)) of
			      true -> [Obj|Acc1];
			      false -> Acc1
			  end
		  end, Acc, mnesia_lib:db_get(Storage, Tab, K))
	end, [], Keys)).

pick_index([{{{Pfx,_},IxType}, Ixt}|_], _Tab, {_} = Pfx) ->
    {IxType, Ixt};
pick_index([{{Pos,IxType}, Ixt}|_], _Tab, Pos) ->
    {IxType, Ixt};
pick_index([_|T], Tab, Pos) ->
    pick_index(T, Tab, Pos);
pick_index([], Tab, Pos) ->
    mnesia:abort({no_exist, Tab, {index, Pos}}).

	    

%%%%%%% Creation, Init and deletion routines for index tables
%% We can have several indexes on the same table
%% this can be a fairly costly operation if table is *very* large

tab2filename(Tab, {A}) when is_atom(A) ->
    mnesia_lib:dir(Tab) ++ "_-" ++ atom_to_list(A) ++ "-.DAT";
tab2filename(Tab, T) when is_tuple(T) ->
    tab2filename(Tab, element(1, T));
tab2filename(Tab, Pos) when is_integer(Pos) ->
    mnesia_lib:dir(Tab) ++ "_" ++ integer_to_list(Pos) ++ ".DAT".

init_index(Tab, Storage) ->
    Cs = val({Tab, cstruct}),
    PosList = Cs#cstruct.index,
    init_indecies(Tab, Storage, PosList).

init_indecies(Tab, Storage, PosList) ->
    case Storage of
	unknown ->
	    ignore;
	{ext, Alias, Mod} ->
	    init_ext_index(Tab, Storage, Alias, Mod, PosList);
	disc_only_copies ->
	    init_disc_index(Tab, Storage, PosList);
	ram_copies ->
	    make_ram_index(Tab, Storage, PosList);
	disc_copies ->
	    make_ram_index(Tab, Storage, PosList)
    end.

%% works for both ram and disc indexes

del_index_table(_, unknown, _) ->
    ignore;
del_index_table(Tab, Storage, {_} = Pos) ->
    delete_transient_index(Tab, Pos, Storage),
    mnesia_lib:del({Tab, index}, Pos);
del_index_table(Tab, Storage, Pos) when is_integer(Pos) ->
    delete_transient_index(Tab, Pos, Storage),
    mnesia_lib:del({Tab, index}, Pos).

del_transient(Tab, Storage) ->
    PosList = val({Tab, index}),
    del_transient(Tab, PosList, Storage).

del_transient(_, [], _) -> ok;
del_transient(Tab, [Pos | Tail], Storage) ->
    delete_transient_index(Tab, Pos, Storage),
    del_transient(Tab, Tail, Storage).

delete_transient_index(Tab, Pos, {ext, Alias, Mod}) ->
    PosInfo = case Pos of
		  _ when is_integer(Pos) ->
		      Cs = val({Tab, cstruct}),
		      lists:keyfind(Pos, 1, Cs#cstruct.index);
		  {P, T} -> {P, T}
	      end,
    Tag = {Tab, index, PosInfo},
    Mod:close_table(Alias, Tag),
    Mod:delete_table(Alias, Tag),
    del_index_info(Tab, Pos),
    mnesia_lib:unset({Tab, {index, Pos}});
delete_transient_index(Tab, Pos, disc_only_copies) ->
    Tag = {Tab, index, Pos},
    mnesia_monitor:unsafe_close_dets(Tag),
    _ = file:delete(tab2filename(Tab, Pos)),
    del_index_info(Tab, Pos), %% Uses val(..)
    mnesia_lib:unset({Tab, {index, Pos}});
delete_transient_index(Tab, Pos, _Storage) ->
    Ixt = val({Tab, {index, Pos}}),
    ?ets_delete_table(Ixt),
    del_index_info(Tab, Pos),
    mnesia_lib:unset({Tab, {index, Pos}}).

%%%%% misc functions for the index create/init/delete functions above

%% assuming that the file exists.
init_disc_index(_Tab, _Storage, []) ->
    done;
init_disc_index(Tab, disc_only_copies, [{Pos,_Pref} | Tail]) ->
    PosInfo = {Pos, bag},
    Fn = tab2filename(Tab, Pos),
    IxTag = {Tab, index, PosInfo},
    _ = file:delete(Fn),
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
    mnesia_lib:set({Tab, {index, PosInfo}}, IxTag),
    add_index_info(Tab, val({Tab, setorbag}), {PosInfo, {dets, IxTag}}),
    init_disc_index(Tab, Storage, Tail).

init_ext_index(_, _, _, _, []) ->
    done;
init_ext_index(Tab, Storage, Alias, Mod, [{Pos,Type} | Tail]) ->
    PosInfo = {Pos, Type},
    IxTag = {Tab, index, PosInfo},
    CS = val({Tab, cstruct}),
    CsList = mnesia_schema:cs2list(CS),
    _Res = mnesia_monitor:unsafe_create_external(IxTag, Alias, Mod, CsList),
    Mod:load_table(Alias, IxTag, init_index, CsList),
    case Mod:is_index_consistent(Alias, IxTag) of
        false ->
	    Mod:index_is_consistent(Alias, IxTag, false),
	    Mod:match_delete(Alias, IxTag, '_'),
	    IxValsF = index_vals_f(Storage, Tab, Pos),
            IxObjF = case Type of
                         bag     -> fun(IxVal, Key) -> {IxVal, Key} end;
                         ordered -> fun(IxVal, Key) -> {{IxVal, Key}} end
                     end,
            mnesia_lib:db_fixtable(Storage, Tab, true),
            mnesia_lib:db_foldl(
	      Storage,
	      fun(Rec, Acc) ->
		      Key = element(2, Rec),
		      lists:foreach(
			fun(V) ->
				IxObj = IxObjF(V, Key),
				Mod:insert(Alias, IxTag, IxObj)
			end, IxValsF(Rec)),
		      Acc
	      end, ok, Tab,
	      [{'_', [], ['$_']}], 100),
	    Mod:index_is_consistent(Alias, IxTag, true);
	true ->
            ignore
    end,

    mnesia_lib:set({Tab, {index, PosInfo}}, IxTag),

    add_index_info(Tab, val({Tab, setorbag}), {PosInfo, {Storage, IxTag}}),
    init_ext_index(Tab, Storage, Alias, Mod, Tail).

create_fun(Cont, Tab, Pos) ->
    IxF = index_vals_f(disc_only_copies, Tab, Pos),
    fun(read) ->
	    Data = 
		case Cont of
		    {start, KeysPerChunk} ->
			mnesia_lib:db_init_chunk(
			  disc_only_copies, Tab, KeysPerChunk);
		    '$end_of_table' -> 
			'$end_of_table';
		    _Else ->
			mnesia_lib:db_chunk(disc_only_copies, Cont)
		end,
	    case Data of
		'$end_of_table' ->
		    end_of_input;
		{Recs, Next} ->
		    IdxElems = lists:flatmap(
				 fun(Obj) ->
					 PrimK = element(2, Obj),
					 [{V, PrimK} || V <- IxF(Obj)]
				 end, Recs),
		    {IdxElems, create_fun(Next, Tab, Pos)}
	    end;
       (close) ->
	    ok
    end.

make_ram_index(_, _, []) ->
    done;
make_ram_index(Tab, Storage, [Pos | Tail]) ->
    add_ram_index(Tab, Storage, Pos),
    make_ram_index(Tab, Storage, Tail).

add_ram_index(Tab, Storage, {Pos, _Pref}) ->
    Type = ordered,
    verbose("Creating index for ~w ~p ~p~n", [Tab, Pos, Type]),
    SetOrBag = val({Tab, setorbag}),
    IxValsF = index_vals_f(Storage, Tab, Pos),
    IxFun = fun(Val, Key) -> {{Val, Key}} end,
    Index = mnesia_monitor:mktab(mnesia_index, [ordered_set, public]),
    Insert = fun(Rec, _Acc) ->
		     PrimK = element(2, Rec),
		     true = ?ets_insert(
			       Index, [IxFun(V, PrimK)
				       || V <- IxValsF(Rec)])
	     end,
    mnesia_lib:db_fixtable(ram_copies, Tab, true),
    true = mnesia_lib:db_foldl(Storage, Insert, true, Tab),
    mnesia_lib:db_fixtable(ram_copies, Tab, false),
    mnesia_lib:set({Tab, {index, Pos}}, Index),
    add_index_info(Tab, SetOrBag, {{Pos, Type}, {ram, Index}});
add_ram_index(_Tab, _, snmp) ->
    ok.

index_info(SetOrBag, PosList) ->
    IxPlugins = mnesia_schema:index_plugins(),
    ExpPosList = lists:map(
		   fun({{P,Type},Ixt} = PI) ->
			   case P of
			       {_} = IxN ->
				   {_, M, F} =
				       lists:keyfind(IxN, 1, IxPlugins),
				   {{{IxN,M,F}, Type}, Ixt};
			       _ ->
				   PI
			   end
		   end, PosList),
    #index{setorbag = SetOrBag, pos_list = ExpPosList}.

add_index_info(Tab, SetOrBag, IxElem) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    IndexInfo = index_info(SetOrBag, [IxElem]),
	    %% Check later if mnesia_tm is sensitive about the order
	    mnesia_lib:set({Tab, index_info}, IndexInfo),
	    mnesia_lib:set({Tab, index}, index_positions(IndexInfo)),
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit([IndexInfo | Commit]));
	{value, Old} ->
	    %% We could check for consistency here
	    Index = Old#index{pos_list = [IxElem | Old#index.pos_list]},
	    mnesia_lib:set({Tab, index_info}, Index),
	    mnesia_lib:set({Tab, index}, index_positions(Index)),
	    NewC = lists:keyreplace(index, 1, Commit, Index),
	    mnesia_lib:set({Tab, commit_work}, 
			   mnesia_lib:sort_commit(NewC))
    end.

index_positions(#index{pos_list = PL}) ->
    [P || {{P,_},_} <- PL].

del_index_info(Tab, Pos) ->
    Commit = val({Tab, commit_work}),
    case lists:keysearch(index, 1, Commit) of
	false ->
	    %% Something is wrong ignore
	    skip;
	{value, Old} ->
	    case lists:filter(fun({P,_}) ->
                                      element(1,P)=/=Pos
                              end,
                              Old#index.pos_list) of
		[] -> 
                    IndexInfo = index_info(Old#index.setorbag,[]),
		    mnesia_lib:set({Tab, index_info}, IndexInfo),
		    mnesia_lib:set({Tab, index}, index_positions(IndexInfo)),
		    NewC = lists:keydelete(index, 1, Commit),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC));
		New ->
		    Index = Old#index{pos_list = New},
		    mnesia_lib:set({Tab, index_info}, Index),
		    mnesia_lib:set({Tab, index}, index_positions(Index)),
		    NewC = lists:keyreplace(index, 1, Commit, Index),
		    mnesia_lib:set({Tab, commit_work}, 
				   mnesia_lib:sort_commit(NewC))
	    end
    end.

db_put({ram, Ixt}, V) ->
    true = ?ets_insert(Ixt, V);
db_put({{ext, _, _} = Ext, Ixt}, V) ->
    mnesia_lib:db_put(Ext, Ixt, V);
db_put({dets, Ixt}, V) ->
    ok = dets:insert(Ixt, V).

db_get({ram, _}=Ixt, IxKey) ->
    Pat = [{{{IxKey, '$1'}}, [], [{element, 1, '$_'}]}],
    db_select(Ixt, Pat);
db_get({{ext,_,_} = _Storage, {_,_,{_,Type}}} = Ixt, IxKey) ->
    Pat = case Type of
	      ordered -> [{{{IxKey, '$1'}}, [], [{element, 1, '$_'}]}];
	      bag     -> [{{IxKey, '_'}, [], ['$_']}]
	  end,
    db_select(Ixt, Pat);
db_get({dets, Ixt}, K) ->
    dets:lookup(Ixt, K).

db_erase({ram, Ixt}, K) ->
    ?ets_delete(Ixt, K);
db_erase({{ext,_,_} = Ext, Ixt}, K) ->
    mnesia_lib:db_erase(Ext, Ixt, K);
db_erase({dets, Ixt}, K) ->
    dets:delete(Ixt, K).

db_match_erase({ram, Ixt}, Pat) ->
    true = ?ets_match_delete(Ixt, Pat);
db_match_erase({{ext,_,_} = Ext, Ixt}, Pat) ->
    mnesia_lib:db_match_erase(Ext, Ixt, Pat);
db_match_erase({dets, Ixt}, Pat) ->
    ok = dets:match_delete(Ixt, Pat).
    
db_select({ram, Ixt}, Pat) ->
    ets:select(Ixt, Pat);
db_select({{ext,_,_} = Ext, Ixt}, Pat) ->
    mnesia_lib:db_select(Ext, Ixt, Pat);
db_select({dets, Ixt}, Pat) ->
    dets:select(Ixt, Pat).

    
get_index_table(Tab, Pos) ->
    get_index_table(Tab,  val({Tab, storage_type}), Pos).

get_index_table(Tab, _Storage, Pos) ->
    #index{pos_list = PosL} = val({Tab, index_info}),
    {_IxType, Ixt} = pick_index(PosL, Tab, Pos),
    Ixt.

index_vals_f(Storage, Tab, {_} = Pos) ->
    index_vals_f(Storage, Tab,
		 lists:keyfind(Pos, 1, mnesia_schema:index_plugins()));
index_vals_f(_Storage, Tab, {Pos,M,F}) ->
    fun(Obj) ->
	    M:F(Tab, Pos, Obj)
    end;
index_vals_f(Storage, Tab, Pos) when is_integer(Pos) ->
    case mnesia_lib:semantics(Storage, index_fun) of
	undefined ->
	    fun(Obj) ->
		    [element(Pos, Obj)]
	    end;
	F when is_function(F, 4) ->
	    {ext, Alias, _Mod} = Storage,
	    fun(Obj) ->
		    F(Alias, Tab, Pos, Obj)
	    end
    end.
