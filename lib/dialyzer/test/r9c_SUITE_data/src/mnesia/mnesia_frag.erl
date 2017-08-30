%%% ``Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%%% AB. All Rights Reserved.''
%%%
%%%     $Id: mnesia_frag.erl,v 1.1 2008/12/17 09:53:38 mikpe Exp $
%%%----------------------------------------------------------------------
%%% Purpose : Support tables so large that they need
%%%           to be divided into several fragments.
%%%----------------------------------------------------------------------

%header_doc_include

-module(mnesia_frag).
-behaviour(mnesia_access).

%% Callback functions when accessed within an activity
-export([
	 lock/4,
	 write/5, delete/5, delete_object/5,
	 read/5, match_object/5, all_keys/4,
	 select/5,
	 index_match_object/6, index_read/6,
	 foldl/6, foldr/6,
	 table_info/4
       ]).

%header_doc_include

-export([
	 change_table_frag/2,
	 remove_node/2,
	 expand_cstruct/1,
	 lookup_frag_hash/1,
	 lookup_foreigners/1,
	 frag_names/1,
	 set_frag_hash/2,
	 local_select/4,
	 remote_select/4
	]).

-include("mnesia.hrl").

-define(OLD_HASH_MOD, mnesia_frag_old_hash).
-define(DEFAULT_HASH_MOD, mnesia_frag_hash).
%%-define(DEFAULT_HASH_MOD, ?OLD_HASH_MOD). %%  BUGBUG: New should be default

-record(frag_state,
	{foreign_key,
	 n_fragments,
	 hash_module,
	 hash_state}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access functions

%impl_doc_include

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

all_keys(ActivityId, Opaque, Tab, LockKind) ->
    Match = [mnesia:all_keys(ActivityId, Opaque, Frag, LockKind)
	     || Frag <- frag_names(Tab)],
    lists:append(Match).

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
%impl_doc_include

frag_size(ActivityId, Opaque, Tab) ->
    [{F, remote_table_info(ActivityId, Opaque, F, size)} || F <- frag_names(Tab)].

frag_memory(ActivityId, Opaque, Tab) ->
    [{F, remote_table_info(ActivityId, Opaque, F, memory)} || F <- frag_names(Tab)].



remote_table_info(ActivityId, Opaque, Tab, Item) ->
    N = val({Tab, where_to_read}),
    case rpc:call(N, mnesia, table_info, [ActivityId, Opaque, Tab, Item]) of
	{badrpc, _} ->
	    mnesia:abort({no_exists, Tab, Item});
	Info ->
	    Info
    end.

do_select(ActivityId, Opaque, Tab, MatchSpec, LockKind) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    mnesia:select(ActivityId, Opaque, Tab, MatchSpec, LockKind);
	FH ->
	    HashState = FH#frag_state.hash_state,
	    FragNumbers =
		case FH#frag_state.hash_module of
		    HashMod when HashMod == ?DEFAULT_HASH_MOD ->
			?DEFAULT_HASH_MOD:match_spec_to_frag_numbers(HashState, MatchSpec);
		    HashMod ->
			HashMod:match_spec_to_frag_numbers(HashState, MatchSpec)
		end,
	    N = FH#frag_state.n_fragments,
	    VerifyFun = fun(F) when integer(F), F >= 1, F =< N -> false;
			   (_F) -> true
			end,
	    case catch lists:filter(VerifyFun, FragNumbers) of
		[] ->
		    Fun = fun(Num) ->
				  Name = n_to_frag_name(Tab, Num),
				  Node = val({Name, where_to_read}),
				  mnesia:lock(ActivityId, Opaque, {table, Name}, LockKind),
				  {Name, Node}
			  end,
		    NameNodes = lists:map(Fun, FragNumbers),
		    SelectAllFun =
			fun(PatchedMatchSpec) ->
				Match = [mnesia:dirty_select(Name, PatchedMatchSpec)
					 || {Name, _Node} <- NameNodes],
				lists:append(Match)
			end,
		    case [{Name, Node} || {Name, Node} <- NameNodes, Node /= node()] of
			[] ->
			    %% All fragments are local
			    mnesia:fun_select(ActivityId, Opaque, Tab, MatchSpec, none, '_', SelectAllFun);
			RemoteNameNodes ->
			    SelectFun =
				fun(PatchedMatchSpec) ->
					Ref = make_ref(),
					Args = [self(), Ref, RemoteNameNodes, PatchedMatchSpec],
					Pid = spawn_link(?MODULE, local_select, Args),
					LocalMatch = [mnesia:dirty_select(Name, PatchedMatchSpec)
						      || {Name, Node} <- NameNodes, Node == node()],
					OldSelectFun = fun() -> SelectAllFun(PatchedMatchSpec) end,
					local_collect(Ref, Pid, lists:append(LocalMatch), OldSelectFun)
				end,
			    mnesia:fun_select(ActivityId, Opaque, Tab, MatchSpec, none, '_', SelectFun)
		    end;
		BadFrags ->
		    mnesia:abort({"match_spec_to_frag_numbers: Fragment numbers out of range",
				  BadFrags, {range, 1, N}})
	    end
    end.

local_select(ReplyTo, Ref, RemoteNameNodes, MatchSpec) ->
    RemoteNodes = mnesia_lib:uniq([Node || {_Name, Node} <- RemoteNameNodes]),
    Args = [ReplyTo, Ref, RemoteNameNodes, MatchSpec],
    {Replies, BadNodes} = rpc:multicall(RemoteNodes, ?MODULE, remote_select, Args),
    case mnesia_lib:uniq(Replies) -- [ok] of
	[] when BadNodes == [] ->
	    ReplyTo ! {local_select, Ref, ok};
	_ when BadNodes /= [] ->
	    ReplyTo ! {local_select, Ref, {error, {node_not_running, hd(BadNodes)}}};
	[{badrpc, {'EXIT', Reason}} | _] ->
	    ReplyTo ! {local_select, Ref, {error, Reason}};
	[Reason | _] ->
	    ReplyTo ! {local_select, Ref, {error, Reason}}
    end,
    unlink(ReplyTo),
    exit(normal).

remote_select(ReplyTo, Ref, NameNodes, MatchSpec) ->
    do_remote_select(ReplyTo, Ref, NameNodes, MatchSpec).

do_remote_select(ReplyTo, Ref, [{Name, Node} | NameNodes], MatchSpec) ->
    if
	Node == node() ->
	    Res = (catch {ok, mnesia:dirty_select(Name, MatchSpec)}),
	    ReplyTo ! {remote_select, Ref, Node, Res},
	    do_remote_select(ReplyTo, Ref, NameNodes, MatchSpec);
	true ->
	    do_remote_select(ReplyTo, Ref, NameNodes, MatchSpec)
    end;
do_remote_select(_ReplyTo, _Ref, [], _MatchSpec) ->
    ok.

local_collect(Ref, Pid, LocalMatch, OldSelectFun) ->
    receive
	{local_select, Ref, LocalRes} ->
	    remote_collect(Ref, LocalRes, LocalMatch, OldSelectFun);
	{'EXIT', Pid, Reason} ->
	    remote_collect(Ref, {error, Reason}, [], OldSelectFun)
    end.

remote_collect(Ref, LocalRes = ok, Acc, OldSelectFun) ->
    receive
	{remote_select, Ref, Node, RemoteRes} ->
	    case RemoteRes of
		{ok, RemoteMatch} ->
		    remote_collect(Ref, LocalRes, RemoteMatch ++ Acc, OldSelectFun);
		_ ->
		    remote_collect(Ref, {error, {node_not_running, Node}}, [], OldSelectFun)
	    end
    after 0 ->
	    Acc
    end;
remote_collect(Ref, LocalRes = {error, Reason}, _Acc, OldSelectFun) ->
    receive
	{remote_select, Ref, _Node, _RemoteRes} ->
	    remote_collect(Ref, LocalRes, [], OldSelectFun)
    after 0 ->
	    mnesia:abort(Reason)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Returns a list of cstructs

expand_cstruct(Cs) ->
    expand_cstruct(Cs, create).

expand_cstruct(Cs, Mode) ->
    Tab = Cs#cstruct.name,
    Props = Cs#cstruct.frag_properties,
    mnesia_schema:verify({alt, [nil, list]}, mnesia_lib:etype(Props),
			 {badarg, Tab, Props}),
    %% Verify keys
    ValidKeys = [foreign_key, n_fragments, node_pool,
		 n_ram_copies, n_disc_copies, n_disc_only_copies,
		 hash_module, hash_state],
    Keys = mnesia_schema:check_keys(Tab, Props, ValidKeys),
    mnesia_schema:check_duplicates(Tab, Keys),

    %% Pick fragmentation props
    ForeignKey = mnesia_schema:pick(Tab, foreign_key, Props, undefined),
    {ForeignKey2, N, Pool, DefaultNR, DefaultND, DefaultNDO} =
	pick_props(Tab, Cs, ForeignKey),

    %% Verify node_pool
    BadPool = {bad_type, Tab, {node_pool, Pool}},
    mnesia_schema:verify(list, mnesia_lib:etype(Pool), BadPool),
    NotAtom = fun(A) when atom(A) -> false;
		 (_A) -> true
	      end,
    mnesia_schema:verify([], [P || P <- Pool, NotAtom(P)], BadPool),

    NR  = mnesia_schema:pick(Tab, n_ram_copies, Props, 0),
    ND  = mnesia_schema:pick(Tab, n_disc_copies, Props, 0),
    NDO = mnesia_schema:pick(Tab, n_disc_only_copies, Props, 0),

    PosInt = fun(I) when integer(I), I >= 0 -> true;
		(_I) -> false
	     end,
    mnesia_schema:verify(true, PosInt(NR),
			 {bad_type, Tab, {n_ram_copies, NR}}),
    mnesia_schema:verify(true, PosInt(ND),
			 {bad_type, Tab, {n_disc_copies, ND}}),
    mnesia_schema:verify(true, PosInt(NDO),
			 {bad_type, Tab, {n_disc_only_copies, NDO}}),

    %% Verify n_fragments
    Cs2 = verify_n_fragments(N, Cs, Mode),

    %% Verify hash callback
    HashMod = mnesia_schema:pick(Tab, hash_module, Props, ?DEFAULT_HASH_MOD),
    HashState = mnesia_schema:pick(Tab, hash_state, Props, undefined),
    HashState2 = HashMod:init_state(Tab, HashState), %% BUGBUG: Catch?

    FH = #frag_state{foreign_key = ForeignKey2,
		     n_fragments = 1,
		     hash_module = HashMod,
		     hash_state  = HashState2},
    if
	NR == 0, ND == 0, NDO == 0 ->
	    do_expand_cstruct(Cs2, FH, N, Pool, DefaultNR, DefaultND, DefaultNDO, Mode);
	true ->
	    do_expand_cstruct(Cs2, FH, N, Pool, NR, ND, NDO, Mode)
    end.

do_expand_cstruct(Cs, FH, N, Pool, NR, ND, NDO, Mode) ->
    Tab = Cs#cstruct.name,

    LC = Cs#cstruct.local_content,
    mnesia_schema:verify(false, LC,
			 {combine_error, Tab, {local_content, LC}}),

    Snmp = Cs#cstruct.snmp,
    mnesia_schema:verify([], Snmp,
			 {combine_error, Tab, {snmp, Snmp}}),

    %% Add empty fragments
    CommonProps = [{base_table, Tab}],
    Cs2 = Cs#cstruct{frag_properties = lists:sort(CommonProps)},
    expand_frag_cstructs(N, NR, ND, NDO, Cs2, Pool, Pool, FH, Mode).

verify_n_fragments(N, Cs, Mode) when integer(N), N >= 1 ->
    case Mode of
	create ->
	    Cs#cstruct{ram_copies = [],
		       disc_copies = [],
		       disc_only_copies = []};
	activate  ->
	    Reason = {combine_error, Cs#cstruct.name, {n_fragments, N}},
	    mnesia_schema:verify(1, N, Reason),
	    Cs
    end;
verify_n_fragments(N, Cs, _Mode) ->
    mnesia:abort({bad_type, Cs#cstruct.name, {n_fragments, N}}).

pick_props(Tab, Cs, {ForeignTab, Attr}) ->
    mnesia_schema:verify(true, ForeignTab /= Tab,
			 {combine_error, Tab, {ForeignTab, Attr}}),
    Props = Cs#cstruct.frag_properties,
    Attrs = Cs#cstruct.attributes,

    ForeignKey  = lookup_prop(ForeignTab, foreign_key),
    ForeignN    = lookup_prop(ForeignTab, n_fragments),
    ForeignPool = lookup_prop(ForeignTab, node_pool),
    N           = mnesia_schema:pick(Tab, n_fragments, Props,  ForeignN),
    Pool        = mnesia_schema:pick(Tab, node_pool, Props, ForeignPool),

    mnesia_schema:verify(ForeignN, N,
			 {combine_error, Tab, {n_fragments, N},
			  ForeignTab, {n_fragments, ForeignN}}),

    mnesia_schema:verify(ForeignPool, Pool,
			 {combine_error, Tab, {node_pool, Pool},
			  ForeignTab, {node_pool, ForeignPool}}),

    mnesia_schema:verify(undefined, ForeignKey,
			 {combine_error, Tab,
			  "Multiple levels of foreign_key dependencies",
			  {ForeignTab, Attr}, ForeignKey}),

    Key = {ForeignTab, mnesia_schema:attr_to_pos(Attr, Attrs)},
    DefaultNR = length(val({ForeignTab, ram_copies})),
    DefaultND = length(val({ForeignTab, disc_copies})),
    DefaultNDO = length(val({ForeignTab, disc_only_copies})),
    {Key, N, Pool, DefaultNR, DefaultND, DefaultNDO};
pick_props(Tab, Cs, undefined) ->
    Props = Cs#cstruct.frag_properties,
    DefaultN = 1,
    DefaultPool = mnesia:system_info(db_nodes),
    N    = mnesia_schema:pick(Tab, n_fragments, Props,  DefaultN),
    Pool = mnesia_schema:pick(Tab, node_pool, Props, DefaultPool),
    DefaultNR = 1,
    DefaultND = 0,
    DefaultNDO = 0,
    {undefined, N, Pool, DefaultNR, DefaultND, DefaultNDO};
pick_props(Tab, _Cs, BadKey) ->
    mnesia:abort({bad_type, Tab, {foreign_key, BadKey}}).

expand_frag_cstructs(N, NR, ND, NDO, CommonCs, Dist, Pool, FH, Mode)
  when N > 1, Mode == create ->
    Frag = n_to_frag_name(CommonCs#cstruct.name, N),
    Cs = CommonCs#cstruct{name = Frag},
    {Cs2, RevModDist, RestDist} = set_frag_nodes(NR, ND, NDO, Cs, Dist, []),
    ModDist = lists:reverse(RevModDist),
    Dist2 = rearrange_dist(Cs, ModDist, RestDist, Pool),
    %% Adjusts backwards, but it doesn't matter.
    {FH2, _FromFrags, _AdditionalWriteFrags} = adjust_before_split(FH),
    CsList = expand_frag_cstructs(N - 1, NR, ND, NDO, CommonCs, Dist2, Pool, FH2, Mode),
    [Cs2 | CsList];
expand_frag_cstructs(1, NR, ND, NDO, CommonCs, Dist, Pool, FH, Mode) ->
    BaseProps = CommonCs#cstruct.frag_properties ++
	[{foreign_key, FH#frag_state.foreign_key},
	 {hash_module, FH#frag_state.hash_module},
	 {hash_state,  FH#frag_state.hash_state},
	 {n_fragments, FH#frag_state.n_fragments},
	 {node_pool, Pool}
	],
    BaseCs = CommonCs#cstruct{frag_properties = lists:sort(BaseProps)},
    case Mode of
	activate ->
	    [BaseCs];
	create ->
	    {BaseCs2, _, _} = set_frag_nodes(NR, ND, NDO, BaseCs, Dist, []),
	    [BaseCs2]
    end.

set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when NR > 0 ->
    Pos = #cstruct.ram_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR - 1, ND, NDO, Cs2, Tail, [Head2 | Acc]);
set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when ND > 0 ->
    Pos = #cstruct.disc_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR, ND - 1, NDO, Cs2, Tail, [Head2 | Acc]);
set_frag_nodes(NR, ND, NDO, Cs, [Head | Tail], Acc) when NDO > 0 ->
    Pos = #cstruct.disc_only_copies,
    {Cs2, Head2} = set_frag_node(Cs, Pos, Head),
    set_frag_nodes(NR, ND, NDO - 1, Cs2, Tail, [Head2 | Acc]);
set_frag_nodes(0, 0, 0, Cs, RestDist, ModDist) ->
    {Cs, ModDist, RestDist};
set_frag_nodes(_, _, _, Cs, [], _) ->
    mnesia:abort({combine_error,  Cs#cstruct.name, "Too few nodes in node_pool"}).

set_frag_node(Cs, Pos, Head) ->
    Ns = element(Pos, Cs),
    {Node, Count2} =
	case Head of
	    {N, Count} when atom(N), integer(Count), Count >= 0 ->
		{N, Count + 1};
	    N when atom(N) ->
		{N, 1};
	    BadNode ->
		mnesia:abort({bad_type, Cs#cstruct.name, BadNode})
	end,
    Cs2 = setelement(Pos, Cs, [Node | Ns]),
    {Cs2, {Node, Count2}}.

rearrange_dist(Cs, [{Node, Count} | ModDist], Dist, Pool) ->
    Dist2 = insert_dist(Cs, Node, Count, Dist, Pool),
    rearrange_dist(Cs, ModDist, Dist2, Pool);
rearrange_dist(_Cs, [], Dist, _) ->
    Dist.

insert_dist(Cs, Node, Count, [Head | Tail], Pool) ->
    case Head of
	{Node2, Count2} when atom(Node2), integer(Count2), Count2 >= 0 ->
	    case node_diff(Node, Count, Node2, Count2, Pool) of
		less ->
		    [{Node, Count}, Head | Tail];
		greater ->
		    [Head | insert_dist(Cs, Node, Count, Tail, Pool)]
	    end;
	Node2 when atom(Node2) ->
	    insert_dist(Cs, Node, Count, [{Node2, 0} | Tail], Pool);
	BadNode ->
	    mnesia:abort({bad_type, Cs#cstruct.name, BadNode})
    end;
insert_dist(_Cs, Node, Count, [], _Pool) ->
    [{Node, Count}];
insert_dist(_Cs, _Node, _Count, Dist, _Pool) ->
    mnesia:abort({bad_type, Dist}).

node_diff(_Node, Count, _Node2, Count2, _Pool) when Count < Count2 ->
    less;
node_diff(Node, Count, Node2, Count2, Pool) when Count == Count2 ->
    Pos = list_pos(Node, Pool, 1),
    Pos2 = list_pos(Node2, Pool, 1),
    if
	Pos < Pos2 ->
	    less;
	Pos > Pos2 ->
	    greater
    end;
node_diff(_Node, Count, _Node2, Count2, _Pool) when Count > Count2 ->
    greater.

%% Returns position of element in list
list_pos(H,  [H | _T], Pos) ->
    Pos;
list_pos(E,  [_H | T], Pos) ->
    list_pos(E,  T, Pos + 1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Switch function for changing of table fragmentation
%%
%% Returns a list of lists of schema ops

change_table_frag(Tab, {activate, FragProps}) ->
    make_activate(Tab, FragProps);
change_table_frag(Tab, deactivate) ->
    make_deactivate(Tab);
change_table_frag(Tab,  {add_frag, SortedNodes}) ->
    make_multi_add_frag(Tab, SortedNodes);
change_table_frag(Tab,  del_frag) ->
    make_multi_del_frag(Tab);
change_table_frag(Tab,  {add_node, Node}) ->
    make_multi_add_node(Tab, Node);
change_table_frag(Tab,  {del_node, Node}) ->
    make_multi_del_node(Tab, Node);
change_table_frag(Tab,  Change) ->
    mnesia:abort({bad_type, Tab, Change}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turn a normal table into a fragmented table
%%
%% The storage type must be the same on all nodes

make_activate(Tab, Props) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    case Cs#cstruct.frag_properties of
	[] ->
	    Cs2 = Cs#cstruct{frag_properties = Props},
	    [Cs3] = expand_cstruct(Cs2, activate),
	    TabDef = mnesia_schema:cs2list(Cs3),
	    Op = {op, change_table_frag, activate, TabDef},
	    [[Op]];
	BadProps ->
	    mnesia:abort({already_exists, Tab, {frag_properties, BadProps}})
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Turn a table into a normal defragmented table

make_deactivate(Tab) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    Foreigners = lookup_foreigners(Tab),
    BaseTab = lookup_prop(Tab, base_table),
    FH = lookup_frag_hash(Tab),
    if
	BaseTab /= Tab ->
	    mnesia:abort({combine_error, Tab, "Not a base table"});
	Foreigners /= [] ->
	    mnesia:abort({combine_error, Tab, "Too many foreigners", Foreigners});
	FH#frag_state.n_fragments > 1 ->
	    mnesia:abort({combine_error, Tab, "Too many fragments"});
	true ->
	    Cs2 = Cs#cstruct{frag_properties = []},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, deactivate, TabDef},
	    [[Op]]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add a fragment to a fragmented table  and fill it with half of
%% the records from one of the old fragments

make_multi_add_frag(Tab, SortedNs) when list(SortedNs) ->
    verify_multi(Tab),
    Ops = make_add_frag(Tab, SortedNs),

    %% Propagate to foreigners
    MoreOps = [make_add_frag(T, SortedNs) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps];
make_multi_add_frag(Tab, SortedNs) ->
    mnesia:abort({bad_type, Tab, SortedNs}).

verify_multi(Tab) ->
    FH = lookup_frag_hash(Tab),
    ForeignKey = FH#frag_state.foreign_key,
    mnesia_schema:verify(undefined, ForeignKey,
			 {combine_error, Tab,
			  "Op only allowed via foreign table",
			  {foreign_key, ForeignKey}}).

make_frag_names_and_acquire_locks(Tab, N, FragIndecies, DoNotLockN) ->
    mnesia_schema:get_tid_ts_and_lock(Tab, write),
    Fun = fun(Index, FN) ->
		  if
		      DoNotLockN == true, Index == N ->
			  Name = n_to_frag_name(Tab, Index),
			  setelement(Index, FN, Name);
		      true ->
			  Name = n_to_frag_name(Tab, Index),
			  mnesia_schema:get_tid_ts_and_lock(Name, write),
			  setelement(Index , FN, Name)
		  end
	  end,
    FragNames = erlang:make_tuple(N, undefined),
    lists:foldl(Fun, FragNames, FragIndecies).

make_add_frag(Tab, SortedNs) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    FH = lookup_frag_hash(Tab),
    {FH2, FromIndecies, WriteIndecies} = adjust_before_split(FH),
    N = FH2#frag_state.n_fragments,
    FragNames = make_frag_names_and_acquire_locks(Tab, N, WriteIndecies, true),
    NewFrag = element(N, FragNames),

    NR = length(Cs#cstruct.ram_copies),
    ND = length(Cs#cstruct.disc_copies),
    NDO = length(Cs#cstruct.disc_only_copies),
    NewCs = Cs#cstruct{name = NewFrag,
		       frag_properties = [{base_table, Tab}],
		       ram_copies = [],
		       disc_copies = [],
		       disc_only_copies = []},
    {NewCs2, _, _} = set_frag_nodes(NR, ND, NDO, NewCs, SortedNs, []),
    [NewOp] = mnesia_schema:make_create_table(NewCs2),

    SplitOps = split(Tab, FH2, FromIndecies, FragNames, []),

    Cs2 = replace_frag_hash(Cs, FH2),
    TabDef = mnesia_schema:cs2list(Cs2),
    BaseOp = {op, change_table_frag, {add_frag, SortedNs}, TabDef},

    [BaseOp, NewOp | SplitOps].

replace_frag_hash(Cs, FH) when record(FH, frag_state) ->
    Fun = fun(Prop) ->
		  case Prop of
		      {n_fragments, _} ->
			  {true, {n_fragments, FH#frag_state.n_fragments}};
		      {hash_module, _} ->
			  {true, {hash_module, FH#frag_state.hash_module}};
		      {hash_state, _} ->
			  {true, {hash_state, FH#frag_state.hash_state}};
		      {next_n_to_split, _} ->
			  false;
		      {n_doubles, _} ->
			  false;
		      _ ->
			  true
		  end
	  end,
    Props = lists:zf(Fun, Cs#cstruct.frag_properties),
    Cs#cstruct{frag_properties = Props}.

%% Adjust table info before split
adjust_before_split(FH) ->
    HashState = FH#frag_state.hash_state,
    {HashState2, FromFrags, AdditionalWriteFrags} =
	case FH#frag_state.hash_module of
	    HashMod when HashMod == ?DEFAULT_HASH_MOD ->
		?DEFAULT_HASH_MOD:add_frag(HashState);
	    HashMod ->
		HashMod:add_frag(HashState)
	end,
    N = FH#frag_state.n_fragments + 1,
    FromFrags2 = (catch lists:sort(FromFrags)),
    UnionFrags = (catch lists:merge(FromFrags2, lists:sort(AdditionalWriteFrags))),
    VerifyFun = fun(F) when integer(F), F >= 1, F =< N -> false;
		   (_F) -> true
		end,
    case catch lists:filter(VerifyFun, UnionFrags) of
	[] ->
	    FH2 = FH#frag_state{n_fragments = N,
				hash_state  = HashState2},
	    {FH2, FromFrags2, UnionFrags};
	BadFrags ->
	    mnesia:abort({"add_frag: Fragment numbers out of range",
			  BadFrags, {range, 1, N}})
    end.

split(Tab, FH, [SplitN | SplitNs], FragNames, Ops) ->
    SplitFrag = element(SplitN, FragNames),
    Pat = mnesia:table_info(SplitFrag, wild_pattern),
    {_Mod, Tid, Ts} = mnesia_schema:get_tid_ts_and_lock(Tab, none),
    Recs = mnesia:match_object(Tid, Ts, SplitFrag, Pat, read),
    Ops2 = do_split(FH, SplitN, FragNames, Recs, Ops),
    split(Tab, FH, SplitNs, FragNames, Ops2);
split(_Tab, _FH, [], _FragNames, Ops) ->
    Ops.

%% Perform the split of the table
do_split(FH, OldN, FragNames, [Rec | Recs], Ops) ->
    Pos = key_pos(FH),
    HashKey = element(Pos, Rec),
    case key_to_n(FH, HashKey) of
	NewN when NewN == OldN ->
	    %% Keep record in the same fragment. No need to move it.
	    do_split(FH, OldN, FragNames, Recs, Ops);
	NewN ->
	    case element(NewN, FragNames) of
		NewFrag when NewFrag /= undefined ->
		    OldFrag = element(OldN, FragNames),
		    Key = element(2, Rec),
		    NewOid = {NewFrag, Key},
		    OldOid = {OldFrag, Key},
		    Ops2 = [{op, rec, unknown, {NewOid, [Rec], write}},
			    {op, rec, unknown, {OldOid, [OldOid], delete}} | Ops],
		    do_split(FH, OldN, FragNames, Recs, Ops2);
		_NewFrag ->
		    %% Tried to move record to fragment that not is locked
		    mnesia:abort({"add_frag: Fragment not locked", NewN})
	    end
    end;
do_split(_FH, _OldN, _FragNames, [], Ops) ->
    Ops.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete a fragment from a fragmented table
%% and merge its records with an other fragment

make_multi_del_frag(Tab) ->
    verify_multi(Tab),
    Ops = make_del_frag(Tab),

    %% Propagate to foreigners
    MoreOps = [make_del_frag(T) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].

make_del_frag(Tab) ->
    FH = lookup_frag_hash(Tab),
    case FH#frag_state.n_fragments of
	N when N > 1 ->
	    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
	    mnesia_schema:ensure_active(Cs),
	    {FH2, FromIndecies, WriteIndecies} = adjust_before_merge(FH),
	    FragNames = make_frag_names_and_acquire_locks(Tab, N, WriteIndecies, false),

	    MergeOps = merge(Tab, FH2, FromIndecies, FragNames, []),
	    LastFrag = element(N, FragNames),
	    [LastOp] = mnesia_schema:make_delete_table(LastFrag, single_frag),
	    Cs2 = replace_frag_hash(Cs, FH2),
	    TabDef = mnesia_schema:cs2list(Cs2),
	    BaseOp = {op, change_table_frag, del_frag, TabDef},
	    [BaseOp, LastOp | MergeOps];
	_ ->
	    %% Cannot remove the last fragment
	    mnesia:abort({no_exists, Tab})
    end.

%% Adjust tab info before merge
adjust_before_merge(FH) ->
    HashState = FH#frag_state.hash_state,
    {HashState2, FromFrags, AdditionalWriteFrags} =
	case FH#frag_state.hash_module of
	    HashMod when HashMod == ?DEFAULT_HASH_MOD ->
		?DEFAULT_HASH_MOD:del_frag(HashState);
	    HashMod ->
		HashMod:del_frag(HashState)
	end,
    N = FH#frag_state.n_fragments,
    FromFrags2 = (catch lists:sort(FromFrags)),
    UnionFrags = (catch lists:merge(FromFrags2, lists:sort(AdditionalWriteFrags))),
    VerifyFun = fun(F) when integer(F), F >= 1, F =< N -> false;
		   (_F) -> true
		end,
    case catch lists:filter(VerifyFun, UnionFrags) of
	[] ->
	    case lists:member(N, FromFrags2) of
		true ->
		    FH2 = FH#frag_state{n_fragments = N - 1,
					hash_state  = HashState2},
		    {FH2, FromFrags2, UnionFrags};
		false ->
		    mnesia:abort({"del_frag: Last fragment number not included", N})
	    end;
	BadFrags ->
	    mnesia:abort({"del_frag: Fragment numbers out of range",
			  BadFrags, {range, 1, N}})
    end.

merge(Tab, FH, [FromN | FromNs], FragNames, Ops) ->
    FromFrag = element(FromN, FragNames),
    Pat = mnesia:table_info(FromFrag, wild_pattern),
    {_Mod, Tid, Ts} = mnesia_schema:get_tid_ts_and_lock(Tab, none),
    Recs = mnesia:match_object(Tid, Ts, FromFrag, Pat, read),
    Ops2 = do_merge(FH, FromN, FragNames, Recs, Ops),
    merge(Tab, FH, FromNs, FragNames, Ops2);
merge(_Tab, _FH, [], _FragNames, Ops) ->
    Ops.

%% Perform the merge of the table
do_merge(FH, OldN, FragNames, [Rec | Recs], Ops) ->
    Pos = key_pos(FH),
    LastN = FH#frag_state.n_fragments + 1,
    HashKey = element(Pos, Rec),
    case key_to_n(FH, HashKey) of
	NewN when NewN == LastN ->
	    %% Tried to leave a record in the fragment that is to be deleted
	    mnesia:abort({"del_frag: Fragment number out of range",
			  NewN, {range, 1, LastN}});
	NewN when NewN == OldN ->
	    %% Keep record in the same fragment. No need to move it.
	    do_merge(FH, OldN, FragNames, Recs, Ops);
	NewN when OldN == LastN ->
	    %% Move record from the fragment that is to be deleted
	    %% No need to create a delete op for each record.
	    case element(NewN, FragNames) of
		NewFrag when NewFrag /= undefined ->
		    Key = element(2, Rec),
		    NewOid = {NewFrag, Key},
		    Ops2 = [{op, rec, unknown, {NewOid, [Rec], write}} | Ops],
		    do_merge(FH, OldN, FragNames, Recs, Ops2);
		_NewFrag ->
		    %% Tried to move record to fragment that not is locked
		    mnesia:abort({"del_frag: Fragment not locked", NewN})
	    end;
	NewN ->
	    case element(NewN, FragNames) of
		NewFrag when NewFrag /= undefined ->
		    OldFrag = element(OldN, FragNames),
		    Key = element(2, Rec),
		    NewOid = {NewFrag, Key},
		    OldOid = {OldFrag, Key},
		    Ops2 = [{op, rec, unknown, {NewOid, [Rec], write}},
			    {op, rec, unknown, {OldOid, [OldOid], delete}} | Ops],
		    do_merge(FH, OldN, FragNames, Recs, Ops2);
		_NewFrag ->
		    %% Tried to move record to fragment that not is locked
		    mnesia:abort({"del_frag: Fragment not locked", NewN})
	    end
    end;
 do_merge(_FH, _OldN, _FragNames, [], Ops) ->
   Ops.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Add a node to the node pool of a fragmented table

make_multi_add_node(Tab, Node)  ->
    verify_multi(Tab),
    Ops = make_add_node(Tab, Node),

    %% Propagate to foreigners
    MoreOps = [make_add_node(T, Node) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].

make_add_node(Tab, Node) when atom(Node)  ->
    Pool = lookup_prop(Tab, node_pool),
    case lists:member(Node, Pool) of
	false ->
	    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
	    Pool2 = Pool ++ [Node],
	    Props = Cs#cstruct.frag_properties,
	    Props2 = lists:keyreplace(node_pool, 1, Props, {node_pool, Pool2}),
	    Cs2 = Cs#cstruct{frag_properties = Props2},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, {add_node, Node}, TabDef},
	    [Op];
	true ->
	    mnesia:abort({already_exists, Tab, Node})
    end;
make_add_node(Tab, Node) ->
    mnesia:abort({bad_type, Tab, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delet a node from the node pool of a fragmented table

make_multi_del_node(Tab, Node)  ->
    verify_multi(Tab),
    Ops = make_del_node(Tab, Node),

    %% Propagate to foreigners
    MoreOps = [make_del_node(T, Node) || T <- lookup_foreigners(Tab)],
    [Ops | MoreOps].

make_del_node(Tab, Node) when atom(Node) ->
    Cs = mnesia_schema:incr_version(val({Tab, cstruct})),
    mnesia_schema:ensure_active(Cs),
    Pool = lookup_prop(Tab, node_pool),
    case lists:member(Node, Pool) of
	true ->
	    Pool2 = Pool -- [Node],
	    Props = lists:keyreplace(node_pool, 1, Cs#cstruct.frag_properties, {node_pool, Pool2}),
	    Cs2 = Cs#cstruct{frag_properties = Props},
	    TabDef = mnesia_schema:cs2list(Cs2),
	    Op = {op, change_table_frag, {del_node, Node}, TabDef},
	    [Op];
	false ->
	    mnesia:abort({no_exists, Tab, Node})
    end;
make_del_node(Tab, Node) ->
    mnesia:abort({bad_type, Tab, Node}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special case used to remove all references to a node during
%% mnesia:del_table_copy(schema, Node)

remove_node(Node, Cs) ->
    Tab = Cs#cstruct.name,
    case is_top_frag(Tab) of
	false ->
	    {Cs, false};
	true ->
	    Pool = lookup_prop(Tab, node_pool),
	    case lists:member(Node, Pool) of
		true ->
		    Pool2 = Pool -- [Node],
		    Props = lists:keyreplace(node_pool, 1,
					     Cs#cstruct.frag_properties,
					     {node_pool, Pool2}),
		    {Cs#cstruct{frag_properties = Props}, true};
		false ->
		    {Cs, false}
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers

val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', Reason} -> mnesia_lib:other_val(Var, Reason);
	Value -> Value
    end.

set_frag_hash(Tab, Props) ->
    case props_to_frag_hash(Tab, Props) of
	FH when record(FH, frag_state) ->
	    mnesia_lib:set({Tab, frag_hash}, FH);
	no_hash ->
	    mnesia_lib:unset({Tab, frag_hash})
    end.

props_to_frag_hash(_Tab, []) ->
    no_hash;
props_to_frag_hash(Tab, Props) ->
    case mnesia_schema:pick(Tab, base_table, Props, undefined) of
	T when T == Tab ->
	    Foreign = mnesia_schema:pick(Tab, foreign_key, Props, must),
	    N = mnesia_schema:pick(Tab, n_fragments, Props, must),

	    case mnesia_schema:pick(Tab, hash_module, Props, undefined) of
		undefined ->
		    Split = mnesia_schema:pick(Tab, next_n_to_split, Props, must),
		    Doubles = mnesia_schema:pick(Tab, n_doubles, Props, must),
		    FH = {frag_hash, Foreign, N, Split, Doubles},
		    HashState = ?OLD_HASH_MOD:init_state(Tab, FH),
		    #frag_state{foreign_key = Foreign,
				n_fragments = N,
				hash_module = ?OLD_HASH_MOD,
				hash_state  = HashState};
		HashMod ->
		    HashState = mnesia_schema:pick(Tab, hash_state, Props, must),
		    #frag_state{foreign_key = Foreign,
				n_fragments = N,
				hash_module = HashMod,
				hash_state  = HashState}
		    %% Old style. Kept for backwards compatibility.
	    end;
	_ ->
	    no_hash
    end.

lookup_prop(Tab, Prop) ->
    Props = val({Tab, frag_properties}),
    case lists:keysearch(Prop, 1,  Props) of
	{value, {Prop, Val}} ->
	    Val;
	false ->
	    mnesia:abort({no_exists, Tab, Prop, {frag_properties, Props}})
    end.

lookup_frag_hash(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	FH when record(FH, frag_state) ->
	    FH;
	{frag_hash, K, N, _S, _D} = FH ->
	    %% Old style. Kept for backwards compatibility.
	    HashState = ?OLD_HASH_MOD:init_state(Tab, FH),
	    #frag_state{foreign_key = K,
			n_fragments = N,
			hash_module = ?OLD_HASH_MOD,
			hash_state  = HashState};
	{'EXIT', _} ->
	    mnesia:abort({no_exists, Tab, frag_properties, frag_hash})
    end.

is_top_frag(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    false;
	_ ->
	    [] == lookup_foreigners(Tab)
    end.

%% Returns a list of tables
lookup_foreigners(Tab) ->
    %% First field in HashPat is either frag_hash or frag_state
    HashPat = {'_', {Tab, '_'}, '_', '_', '_'},
    [T || [T] <- ?ets_match(mnesia_gvar, {{'$1', frag_hash}, HashPat})].

%% Returns name of fragment table
record_to_frag_name(Tab, Rec) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    Tab;
	FH ->
	    Pos = key_pos(FH),
	    Key = element(Pos, Rec),
	    N = key_to_n(FH, Key),
	    n_to_frag_name(Tab, N)
    end.

key_pos(FH) ->
    case FH#frag_state.foreign_key of
	undefined ->
	    2;
	{_ForeignTab, Pos} ->
	    Pos
    end.

%% Returns name of fragment table
key_to_frag_name({BaseTab, _} = Tab, Key) ->
    N = key_to_frag_number(Tab, Key),
    n_to_frag_name(BaseTab, N);
key_to_frag_name(Tab, Key) ->
    N = key_to_frag_number(Tab, Key),
    n_to_frag_name(Tab, N).

%% Returns name of fragment table
n_to_frag_name(Tab, 1) ->
    Tab;
n_to_frag_name(Tab, N) when atom(Tab), integer(N) ->
    list_to_atom(atom_to_list(Tab) ++ "_frag" ++ integer_to_list(N));
n_to_frag_name(Tab, N) ->
    mnesia:abort({bad_type, Tab, N}).

%% Returns name of fragment table
key_to_frag_number({Tab, ForeignKey}, _Key) ->
    FH = val({Tab, frag_hash}),
    case FH#frag_state.foreign_key of
	{_ForeignTab, _Pos} ->
	    key_to_n(FH, ForeignKey);
	undefined ->
	    mnesia:abort({combine_error, Tab, frag_properties,
			  {foreign_key, undefined}})
    end;
key_to_frag_number(Tab, Key) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    1;
	FH ->
	    key_to_n(FH, Key)
    end.

%% Returns fragment number
key_to_n(FH, Key) ->
    HashState = FH#frag_state.hash_state,
    N =
	case FH#frag_state.hash_module of
	    HashMod when HashMod == ?DEFAULT_HASH_MOD ->
		?DEFAULT_HASH_MOD:key_to_frag_number(HashState, Key);
	    HashMod ->
		HashMod:key_to_frag_number(HashState, Key)
	end,
    if
	integer(N), N >= 1, N =< FH#frag_state.n_fragments ->
	    N;
	true ->
	    mnesia:abort({"key_to_frag_number: Fragment number out of range",
			  N, {range, 1, FH#frag_state.n_fragments}})
    end.

%% Returns a list of frament table names
frag_names(Tab) ->
    case ?catch_val({Tab, frag_hash}) of
	{'EXIT', _} ->
	    [Tab];
	FH ->
	    N = FH#frag_state.n_fragments,
	    frag_names(Tab, N, [])
    end.

frag_names(Tab, 1, Acc) ->
    [Tab | Acc];
frag_names(Tab, N, Acc) ->
    Frag = n_to_frag_name(Tab, N),
    frag_names(Tab, N - 1, [Frag | Acc]).

%% Returns a list of {Node, FragCount} tuples
%% sorted on FragCounts
frag_dist(Tab) ->
    Pool = lookup_prop(Tab, node_pool),
    Dist = [{good, Node, 0} || Node <- Pool],
    Dist2 = count_frag(frag_names(Tab), Dist),
    sort_dist(Dist2).

count_frag([Frag | Frags], Dist) ->
    Dist2 =  incr_nodes(val({Frag, ram_copies}), Dist),
    Dist3 =  incr_nodes(val({Frag, disc_copies}), Dist2),
    Dist4 =  incr_nodes(val({Frag, disc_only_copies}), Dist3),
    count_frag(Frags, Dist4);
count_frag([], Dist) ->
    Dist.

incr_nodes([Node | Nodes], Dist) ->
    Dist2 = incr_node(Node, Dist),
    incr_nodes(Nodes, Dist2);
incr_nodes([], Dist) ->
    Dist.

incr_node(Node, [{Kind, Node, Count} | Tail]) ->
    [{Kind, Node, Count + 1} | Tail];
incr_node(Node, [Head | Tail]) ->
    [Head | incr_node(Node, Tail)];
incr_node(Node, []) ->
    [{bad, Node, 1}].

%% Sorts dist according in decreasing count order
sort_dist(Dist) ->
    Dist2 = deep_dist(Dist, []),
    Dist3 = lists:keysort(1, Dist2),
    shallow_dist(Dist3).

deep_dist([Head | Tail], Deep) ->
    {Kind, _Node, Count} = Head,
    {Tag, Same, Other} = pick_count(Kind, Count, [Head | Tail]),
    deep_dist(Other, [{Tag, Same} | Deep]);
deep_dist([], Deep) ->
    Deep.

pick_count(Kind, Count, [{Kind2, Node2, Count2} | Tail]) ->
    Head = {Node2, Count2},
    {_, Same, Other} = pick_count(Kind, Count, Tail),
    if
	Kind == bad ->
	    {bad, [Head | Same], Other};
	Kind2 == bad ->
	    {Count, Same, [{Kind2, Node2, Count2} | Other]};
	Count == Count2 ->
	    {Count, [Head | Same], Other};
	true ->
	    {Count, Same, [{Kind2, Node2, Count2} | Other]}
    end;
pick_count(_Kind, Count, []) ->
    {Count, [], []}.

shallow_dist([{_Tag, Shallow} | Deep]) ->
    Shallow ++ shallow_dist(Deep);
shallow_dist([]) ->
    [].
