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
-module(dets_utils).

%% Utility functions common to several dets file formats.
%% To be used from dets, dets_v8 and dets_v9 only.

-export([cmp/2, msort/1, mkeysort/2, mkeysearch/3, family/1]).

-export([rename/2, pread/2, pread/4, ipread/3, pwrite/2, write/2,
         truncate/2, position/2, sync/1, open/2, truncate/3, fwrite/3,
         write_file/2, position/3, position_close/3, pwrite/4,
         pwrite/3, pread_close/4, read_n/2, pread_n/3, read_4/2]).

-export([code_to_type/1, type_to_code/1]).

-export([corrupt_reason/2, corrupt/2, corrupt_file/2, 
         vformat/2, file_error/2]).

-export([debug_mode/0, bad_object/2]).

-export([cache_lookup/4, cache_size/1, new_cache/1,
	 reset_cache/1, is_empty_cache/1]).

-export([empty_free_lists/0, init_alloc/1, alloc_many/4, alloc/2,
         free/3, get_freelists/1, all_free/1, all_allocated/1,
         all_allocated_as_list/1, find_allocated/4, find_next_allocated/3,
         log2/1, make_zeros/1]).

-export([init_slots_from_old_file/2]).

-export([list_to_tree/1, tree_to_bin/5]).

-compile({inline, [{sz2pos,1}, {adjust_addr,3}]}).
-compile({inline, [{bplus_mk_leaf,1}, {bplus_get_size,1},
		   {bplus_get_tree,2}, {bplus_get_lkey,2},
		   {bplus_get_rkey,2}]}).

%% Debug
-export([init_disk_map/1, stop_disk_map/0, 
         disk_map_segment_p/2, disk_map_segment/2]).

-include("dets.hrl").

%%% A total ordering of all Erlang terms.

%% -> -1 | 0 | 1. T1 is (smaller than | equal | greater than) T2.
%% If is_integer(I), is_float(F), I == F then I is deemed smaller than F.
cmp(T, T) ->
    0;
cmp([E1 | T1], [E2 | T2]) ->
    case cmp(E1, E2) of
        0 -> cmp(T1, T2);
        R -> R
    end;
cmp(T1, T2) when tuple_size(T1) =:= tuple_size(T2) ->
    tcmp(T1, T2, 1, tuple_size(T1));
cmp(I, F) when is_integer(I), is_float(F) ->
    -1;
cmp(F, I) when is_float(F), is_integer(I) ->
    1;
cmp(T1, T2) when T1 < T2 ->
    -1;
cmp(_T1, _T2) -> % when _T1 > _T2
    1.

tcmp(T1, T2, I, I) ->
    cmp(element(I, T1), element(I,  T2));
tcmp(T1, T2, I, N) ->
    case cmp(element(I, T1), element(I, T2)) of
        0 -> tcmp(T1, T2, I + 1, N);
        R -> R
    end.

msort(L) ->
    %% sort is very much faster than msort, let it do most of the work.    
    F = fun(X, Y) -> cmp(X, Y) =< 0 end,
    lists:sort(F, lists:sort(L)).

mkeysort(I, L) ->
    F = fun(X, Y) -> cmp(element(I, X), element(I, Y)) =< 0 end,
    %% keysort is much faster than mkeysort, let it do most of the work.
    lists:sort(F, lists:keysort(I, L)).

mkeysearch(Key, I, L) ->
    case lists:keysearch(Key, I, L) of
        {value, Value}=Reply when element(I, Value) =:= Key ->
            Reply;
        false ->
            false;
        _ ->
            mkeysearch2(Key, I, L)
    end.

mkeysearch2(_Key, _I, []) ->
    false;
mkeysearch2(Key, I, [E | _L]) when element(I, E) =:= Key ->
    {value, E};
mkeysearch2(Key, I, [_ | L]) ->
    mkeysearch2(Key, I, L).

%% Be careful never to compare keys, but use matching instead.
%% Otherwise sofs could have been used:
%%    sofs:to_external(sofs:relation_to_family(sofs:relation(L, 2))).
family([]) ->
    [];
family(L) ->
    [{K,V}|KVL] = mkeysort(1, L),
    per_key(KVL, K, [V], []).

per_key([], K, Vs, KVs) ->
    lists:reverse(KVs, [{K,msort(Vs)}]);
per_key([{K,V}|L], K, Vs, KVs) -> % match
    per_key(L, K, [V|Vs], KVs);
per_key([{K1,V}|L], K, Vs, KVs) ->
    per_key(L, K1, [V], [{K,msort(Vs)}|KVs]).

rename(From, To) ->
    case file:rename(From, To) of
        ok ->
            ok;
        {error, Reason} ->
            {error, {file_error, {From, To}, Reason}}
    end.

%% -> {ok, Bins} | throw({NewHead, Error})
pread(Positions, Head) ->
    R = case file:pread(Head#head.fptr, Positions) of
	    {ok, Bins} ->
		%% file:pread/2 can return 'eof' as "data".
		case lists:member(eof, Bins) of
		    true ->
			{error, {premature_eof, Head#head.filename}};
		    false ->
			{ok, Bins}
		end;
	    {error, Reason} when enomem =:= Reason; einval =:= Reason ->
		{error, {bad_object_header, Head#head.filename}};
	    {error, Reason} ->
		{file_error, Head#head.filename, Reason}
	end,
    case R of
	{ok, _Bins} ->
	    R;
	Error ->
	    throw(corrupt(Head, Error))
    end.

%% -> {ok, binary()} | throw({NewHead, Error})
pread(Head, Pos, Min, Extra) ->
    R = case file:pread(Head#head.fptr, Pos, Min+Extra) of
	    {error, Reason} when enomem =:= Reason; einval =:= Reason ->
		{error, {bad_object_header, Head#head.filename}};
	    {error, Reason} ->
		{file_error, Head#head.filename, Reason};
	    {ok, Bin} when byte_size(Bin) < Min ->
		{error, {premature_eof, Head#head.filename}};
	    OK -> OK
	end,
    case R of
	{ok, _Bin} ->
	    R;
	Error ->
	    throw(corrupt(Head, Error))
    end.
	    
%% -> eof | [] | {ok, {Size, Pointer, binary()}}
ipread(Head, Pos1, MaxSize) ->
    try 
        disk_map_pread(Pos1)
    catch Bad ->
        throw(corrupt_reason(Head, {disk_map, Bad}))
    end,
    case file:ipread_s32bu_p32bu(Head#head.fptr, Pos1, MaxSize) of
	{ok, {0, 0, eof}} ->
	    [];
	{ok, Reply} ->
	    {ok, Reply};
	_Else ->
	    eof
    end.

%% -> {Head, ok} | throw({Head, Error})
pwrite(Head, []) ->
    {Head, ok};
pwrite(Head, Bins) ->
    try
        disk_map(Bins)
    catch Bad -> 
        throw(corrupt_reason(Head, {disk_map, Bad, Bins}))
    end,
    case file:pwrite(Head#head.fptr, Bins) of
	ok ->
	    {Head, ok};
	Error ->
	    corrupt_file(Head, Error)
    end.

%% -> ok | throw({Head, Error})
write(_Head, []) ->
    ok;
write(Head, Bins) ->
    case file:write(Head#head.fptr, Bins) of
	ok ->
	    ok;
	Error ->
	    corrupt_file(Head, Error)
    end.

%% -> ok | throw({Head, Error})
%% Same as file:write_file/2, but calls file:sync/1.
write_file(Head, Bin) ->
    R = case file:open(Head#head.filename, [binary, raw, write]) of
	    {ok, Fd} ->
		R1 = file:write(Fd, Bin),
		R2 = file:sync(Fd),
		R3 = file:close(Fd),
                case {R1, R2, R3} of
                    {ok, ok, R3} -> R3;
                    {ok, R2, _} -> R2;
                    {R1, _, _} -> R1
                end;
	    Else ->
		Else
	end,
    case R of
	ok ->
	    ok;
	Error ->
	    corrupt_file(Head, Error)
    end.

%% -> ok | throw({Head, Error})
truncate(Head, Pos) ->
    case catch truncate(Head#head.fptr, Head#head.filename, Pos) of
	ok ->
	    ok;
	Error ->
	    throw(corrupt(Head, Error))
    end.

%% -> {ok, Pos} | throw({Head, Error})
position(Head, Pos) ->
    case file:position(Head#head.fptr, Pos) of
	{error, _Reason} = Error -> 
	    corrupt_file(Head, Error);
	OK -> OK
    end.
	    
%% -> ok | throw({Head, Error})
sync(Head) ->
    case file:sync(Head#head.fptr) of
	ok ->
	    ok;
	Error ->
	    corrupt_file(Head, Error)
    end.

open(FileSpec, Args) ->
    case file:open(FileSpec, Args) of
	{ok, Fd} ->
	    {ok, Fd};
	Error ->
	    file_error(FileSpec, Error)
    end.

truncate(Fd, FileName, Pos) ->
    _ = [position(Fd, FileName, Pos) || Pos =/= cur],
    case file:truncate(Fd) of
	ok    -> 
	    ok;
	Error ->
	    file_error(FileName, {error, Error})
    end.
	    
fwrite(Fd, FileName, B) ->
    case file:write(Fd, B) of
	ok    -> ok;
	Error -> file_error_close(Fd, FileName, Error)
    end.

position(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error(FileName, {error, Error});
	OK -> OK
    end.
	    
position_close(Fd, FileName, Pos) ->
    case file:position(Fd, Pos) of
	{error, Error} -> file_error_close(Fd, FileName, {error, Error});
	OK -> OK
    end.
	    
pwrite(Fd, FileName, Position, B) ->
    case file:pwrite(Fd, Position, B) of
	ok -> ok;
	Error -> file_error(FileName, {error, Error})
    end.

pwrite(Fd, FileName, Bins) ->
    case file:pwrite(Fd, Bins) of
	ok ->
	    ok;
	{error, {_NoWrites, Reason}} ->
	    file_error(FileName, {error, Reason})
    end.

pread_close(Fd, FileName, Pos, Size) ->
    case file:pread(Fd, Pos, Size) of
	{error, Error} ->
	    file_error_close(Fd, FileName, {error, Error});
	{ok, Bin} when byte_size(Bin) < Size ->
	    _ = file:close(Fd),
	    throw({error, {tooshort, FileName}});
	eof ->
	    _ = file:close(Fd),
	    throw({error, {tooshort, FileName}});
	OK -> OK
    end.
	    
file_error(FileName, {error, Reason}) ->
    throw({error, {file_error, FileName, Reason}}).

file_error_close(Fd, FileName, {error, Reason}) ->
    _ = file:close(Fd),
    throw({error, {file_error, FileName, Reason}}).
	    
debug_mode() ->
    os:getenv("DETS_DEBUG") =:= "true".    

bad_object(Where, Extra) ->
    case debug_mode() of
        true ->
            {bad_object, Where, Extra};
        false ->
            %% Avoid showing possibly secret data on the error logger.
            {bad_object, Where}
    end.

read_n(Fd, Max) ->
    case file:read(Fd, Max) of
	{ok, Bin} ->
	    Bin;
	_Else ->
	    eof
    end.

pread_n(Fd, Position, Max) ->
    case file:pread(Fd, Position, Max) of
	{ok, Bin} ->
	    Bin;
	_ ->
	    eof
    end.

read_4(Fd, Position) ->
    {ok, _} = file:position(Fd, Position),
    <<Four:32>> = dets_utils:read_n(Fd, 4),
    Four.

corrupt_file(Head, {error, Reason}) ->
    Error = {error, {file_error, Head#head.filename, Reason}},
    throw(corrupt(Head, Error)).

%% -> {NewHead, Error}
corrupt_reason(Head, Reason0) ->
    Reason = case get_disk_map() of
                 no_disk_map -> 
                     Reason0;
                 DM ->
                    ST = erlang:get_stacktrace(),
                    PD = get(),
                    {Reason0, ST, PD, DM}
             end,
    Error = {error, {Reason, Head#head.filename}},    
    corrupt(Head, Error).

corrupt(Head, Error) ->
    case get(verbose) of
	yes -> 
	    error_logger:format("** dets: Corrupt table ~p: ~tp\n", 
				[Head#head.name, Error]);
	_ -> ok
    end,
    case Head#head.update_mode of
	{error, _} ->
	    {Head, Error};
	_ ->
	    {Head#head{update_mode = Error}, Error}
    end.

vformat(F, As) ->
    case get(verbose) of
	yes -> error_logger:format(F, As);
	_ -> ok
    end.

code_to_type(?SET) -> set;
code_to_type(?BAG) -> bag;
code_to_type(?DUPLICATE_BAG) -> duplicate_bag;
code_to_type(_Type) -> badtype.

type_to_code(set) -> ?SET;
type_to_code(bag) -> ?BAG;
type_to_code(duplicate_bag) -> ?DUPLICATE_BAG.

%%%
%%% Write Cache
%%% 

cache_size(C) ->
    {C#cache.delay, C#cache.tsize}.

%% -> [object()] | false
cache_lookup(Type, [Key | Keys], CL, LU) ->
    %% mkeysearch returns the _first_ tuple with a matching key.
    case mkeysearch(Key, 1, CL) of
	{value, {Key,{_Seq,{insert,Object}}}} when Type =:= set ->
	    cache_lookup(Type, Keys, CL, [Object | LU]);
	{value, {Key,{_Seq,delete_key}}} ->
	    cache_lookup(Type, Keys, CL, LU);
	_ ->
	    false
    end;
cache_lookup(_Type, [], _CL, LU) ->
    LU.

reset_cache(C) ->
    WrTime = C#cache.wrtime,
    NewWrTime = if 
		    WrTime =:= undefined ->
			WrTime;
		    true ->
			erlang:monotonic_time(1000000)
		end,
    PK = family(C#cache.cache),
    NewC = C#cache{cache = [], csize = 0, inserts = 0, wrtime = NewWrTime},
    {NewC, C#cache.inserts, PK}.

is_empty_cache(Cache) ->
    Cache#cache.cache =:= [].

new_cache({Delay, Size}) ->
    #cache{cache = [], csize = 0, inserts = 0, 
	   tsize = Size, wrtime = undefined, delay = Delay}.

%%%
%%% Buddy System
%%% 

%% Definitions for the buddy allocator.
-define(MAXBUD, 32).             % 2 GB is maximum file size
-define(MAXFREELISTS, 50000000). % Bytes reserved for the free lists (at end).

%%-define(DEBUG(X, Y), io:format(X, Y)).
-define(DEBUG(X, Y), true).

%%% Algorithm : We use a buddy system on each file. This is nicely described
%%%             in i.e. the last chapter of the first-grade text book 
%%%             Data structures and algorithms by Aho, Hopcroft and
%%%             Ullman. I think buddy systems were invented by Knuth, a long
%%%             time ago.

init_slots_from_old_file([{Slot,Addr} | T], Ftab) ->
    init_slot(Slot+1,[{Slot,Addr} | T], Ftab);
init_slots_from_old_file([], Ftab) ->
    Ftab.

init_slot(_Slot,[], Ftab) ->
    Ftab; % should never happen
init_slot(_Slot,[{_Addr,0}|T], Ftab) ->
    init_slots_from_old_file(T, Ftab);
init_slot(Slot,[{_Slot1,Addr}|T], Ftab) ->
    Stree = element(Slot, Ftab),
    %%    io:format("init_slot ~p:~p~n",[Slot, Addr]),
    init_slot(Slot,T,setelement(Slot, Ftab, bplus_insert(Stree, Addr))).

%%% The free lists are kept in RAM, and written to the end of the file
%%% from time to time. It is possible that a considerable amount of
%%% memory is used for a fragmented file.
%%%
%%% To make things (slightly) worse (from a memory usage point of
%%% view), each traversal of the file starts with making a "map" of
%%% the allocated areas; only the allocated areas will be
%%% traversed. Creating a map involves inspecting and sorting the free
%%% lists. Since the map is passed on between client and server, it
%%% has to be a binary (to avoid copying a possibly huge term).
%%%
%%% An active map should always be protected by fixing the table. This
%%% prevents insertion of objects into the mapped area (where some
%%% objects may have been deleted). The means for implementing this
%%% protection is a copy of the free lists (using even more memory, if
%%% objects are inserted). The position to write an inserted object is
%%% found by looking at the free lists from the time when the table
%%% was fixed; areas within the mapped area that have been freed are
%%% hidden from the allocator.

%% -> free_table()
%% A free table is a tuple of ?MAXBUD elements, element i handling
%% buddies of size 2^(i-1).
init_alloc(Base) ->
    Ftab = empty_free_lists(),
    Empty = bplus_empty_tree(),
    setelement(?MAXBUD, Ftab, bplus_insert(Empty, Base)). 

empty_free_lists() ->
    Empty = bplus_empty_tree(),
    %% initiate a tuple with ?MAXBUD "Empty" elements
    erlang:make_tuple(?MAXBUD, Empty).

%% Only used when repairing or initiating.
alloc_many(Head, _Sz, 0, _A0) ->
    Head;
alloc_many(Head, Sz, N, A0) ->
    Ftab = Head#head.freelists,
    Head#head{freelists = alloc_many1(Ftab, 1, Sz * N, A0, Head)}.

%% -> NewFtab | throw(Error)
alloc_many1(Ftab, Pos, Size, A0, H) ->
    {FPos, Addr} = find_first_free(Ftab, Pos, Pos, H),
    true = Addr >= A0, % assertion
    if 
	?POW(FPos - 1) >= Size ->
	    alloc_many2(Ftab, sz2pos(Size), Size, A0, H);
	true ->
	    NewFtab = reserve_buddy(Ftab, FPos, FPos, Addr),
	    NSize = Size - ?POW(FPos-1),
	    alloc_many1(NewFtab, FPos, NSize, Addr, H)
    end.

alloc_many2(Ftab, _Pos, 0, _A0, _H) ->
    Ftab;
alloc_many2(Ftab, Pos, Size, A0, H) when Size band ?POW(Pos-1) > 0 ->
    {FPos, Addr} = find_first_free(Ftab, Pos, Pos, H),
    true = Addr >= A0, % assertion
    NewFtab = reserve_buddy(Ftab, FPos, Pos, Addr),
    NSize = Size - ?POW(Pos - 1),
    alloc_many2(NewFtab, Pos-1, NSize, Addr, H);
alloc_many2(Ftab, Pos, Size, A0, H) ->
    alloc_many2(Ftab, Pos-1, Size, A0, H).

%% -> {NewHead, Addr, Log2} | throw(Error)
alloc(Head, Sz) when Head#head.fixed =/= false -> % when Sz > 0
    ?DEBUG("alloc of size ~p (fixed)", [Sz]),
    Pos = sz2pos(Sz),
    {Frozen, Ftab} = Head#head.freelists,
    {FPos, Addr} = find_first_free(Frozen, Pos, Pos, Head),
    NewFrozen = reserve_buddy(Frozen, FPos, Pos, Addr),
    Ftab1 = undo_free(Ftab, FPos, Addr, Head#head.base),
    NewFtab = move_down(Ftab1, FPos, Pos, Addr),
    NewFreelists = {NewFrozen, NewFtab},
    {Head#head{freelists = NewFreelists}, Addr, Pos};
alloc(Head, Sz) when Head#head.fixed =:= false -> % when Sz > 0
    ?DEBUG("alloc of size ~p", [Sz]),
    Pos = sz2pos(Sz),
    Ftab = Head#head.freelists,
    {FPos, Addr} = find_first_free(Ftab, Pos, Pos, Head),
    NewFtab = reserve_buddy(Ftab, FPos, Pos, Addr),
    {Head#head{freelists = NewFtab}, Addr, Pos}.

find_first_free(_Ftab, Pos, _Pos0, Head) when Pos > ?MAXBUD ->
    throw({error, {no_more_space_on_file, Head#head.filename}});
find_first_free(Ftab, Pos, Pos0, Head) ->
    PosTab = element(Pos, Ftab),
    case bplus_lookup_first(PosTab) of
	undefined -> 
	    find_first_free(Ftab, Pos+1, Pos0, Head);
	{ok, Addr} when Addr + ?POW(Pos0-1) > ?POW(?MAXBUD-1)-?MAXFREELISTS ->
	    %% We would occupy (some of) the area reserved for the free lists.
	    throw({error, {no_more_space_on_file, Head#head.filename}});
	{ok, Addr} ->
	    {Pos, Addr}
    end.

%% When the table is fixed, free/4 may have joined buddies so that the
%% requested block is now part of some larger block. We have to find
%% that block, and insert free buddies along the way.
undo_free(Ftab, Pos, Addr, Base) ->
    PosTab = element(Pos, Ftab),
    case bplus_lookup(PosTab, Addr) of
	undefined ->
	    {BuddyAddr, MoveUpAddr} = my_buddy(Addr, ?POW(Pos-1), Base),
	    NewFtab = setelement(Pos, Ftab, bplus_insert(PosTab, BuddyAddr)),
	    undo_free(NewFtab, Pos+1, MoveUpAddr, Base);
	{ok, Addr} ->
	    NewPosTab = bplus_delete(PosTab, Addr),
	    setelement(Pos, Ftab, NewPosTab)
    end.

reserve_buddy(Ftab, Pos, Pos0, Addr) ->
    PosTab = element(Pos, Ftab),
    NewPosTab = bplus_delete(PosTab, Addr),
    NewFtab = setelement(Pos, Ftab, NewPosTab),
    move_down(NewFtab, Pos, Pos0, Addr).

move_down(Ftab, Pos, Pos, _Addr) ->
    ?DEBUG(" to address ~p, table ~p (~p bytes)~n", 
	    [_Addr, Pos, ?POW(Pos-1)]),
    Ftab;
move_down(Ftab, Pos, Pos0, Addr) ->
    Pos_1 = Pos - 1,
    Size = ?POW(Pos_1),
    HighBuddy = (Addr + (Size bsr 1)),
    NewPosTab_1 = bplus_insert(element(Pos_1, Ftab), HighBuddy),
    NewFtab = setelement(Pos_1, Ftab, NewPosTab_1), 
    move_down(NewFtab, Pos_1, Pos0, Addr).

%% -> {Head, Log2}
free(Head, Addr, Sz) ->
    ?DEBUG("free of size ~p at address ~p~n", [Sz, Addr]),
    Ftab = get_freelists(Head),
    Pos = sz2pos(Sz),
    {set_freelists(Head, free_in_pos(Ftab, Addr, Pos, Head#head.base)), Pos}.

free_in_pos(Ftab, _Addr, Pos, _Base) when Pos > ?MAXBUD ->
    Ftab;
free_in_pos(Ftab, Addr, Pos, Base) ->
    PosTab = element(Pos, Ftab),
    {BuddyAddr, MoveUpAddr} = my_buddy(Addr, ?POW(Pos-1), Base),
    case bplus_lookup(PosTab, BuddyAddr) of
	undefined -> % no buddy found
	    ?DEBUG("  table ~p, no buddy~n", [Pos]),
	    setelement(Pos, Ftab, bplus_insert(PosTab, Addr));
	{ok, BuddyAddr} -> % buddy found
	    PosTab1 = bplus_delete(PosTab, Addr),
	    PosTab2 = bplus_delete(PosTab1, BuddyAddr),
	    ?DEBUG("  table ~p, with buddy ~p~n", [Pos, BuddyAddr]),
	    NewFtab = setelement(Pos, Ftab, PosTab2),
	    free_in_pos(NewFtab, MoveUpAddr, Pos+1, Base)
    end.

get_freelists(Head) when Head#head.fixed =:= false ->
    Head#head.freelists;
get_freelists(Head) when Head#head.fixed =/= false ->
    {_Frozen, Current} = Head#head.freelists,
    Current.

set_freelists(Head, Ftab) when Head#head.fixed =:= false ->
    Head#head{freelists = Ftab};
set_freelists(Head, Ftab) when Head#head.fixed =/= false ->
    {Frozen, _} = Head#head.freelists,
    Head#head{freelists = {Frozen,Ftab}}.

%% Bug: If Sz0 is equal to 2^k for some k, then 2^(k+1) bytes are
%% allocated (wasting 2^k bytes). Inlined.
sz2pos(N) when N > 0 ->
    1 + log2(N+1).

%% Returns the i such that 2^(i-1) < N =< 2^i.
log2(N) when is_integer(N), N >= 0 ->
    if N > ?POW(8) ->
	    if N > ?POW(10) ->
		    if N > ?POW(11) ->
			    if N > ?POW(12) ->
				    12 + if N band (?POW(12)-1) =:= 0 -> 
						 log2(N bsr 12);
					    true -> log2(1 + (N bsr 12))
					 end;
			       true -> 12
			    end;
		       true -> 11
		    end;
	       N > ?POW(9) -> 10;
	       true -> 9
	    end;
       N > ?POW(4) ->
	    if N > ?POW(6) ->
		    if N > ?POW(7) -> 8;
		       true -> 7
		    end;
	       N > ?POW(5) -> 6;
	       true -> 5
	    end;
       N > ?POW(2) ->
	    if
		N > ?POW(3) -> 4;
		true -> 3
	    end;
       N > ?POW(1) -> 2;
       N >= ?POW(0) -> 1;
       true -> 0
    end.

make_zeros(0) -> [];
make_zeros(N) when N rem 2 =:= 0 ->
    P = make_zeros(N div 2),
    [P|P];
make_zeros(N) ->
    P = make_zeros(N div 2),
    [0,P|P].

%% Calculate the buddy of Addr
my_buddy(Addr, Sz, Base) ->
    case (Addr - Base) band Sz of
	0 -> % even, buddy is higher addr
	    {Addr+Sz, Addr};
	_ -> % odd, buddy is lower addr
            T = Addr-Sz,
	    {T, T}
    end.

all_free(Head) ->
    Tab = get_freelists(Head),
    Base = Head#head.base,
    case all_free(all(Tab), Base, Base, []) of
	[{Base,Base} | L] -> L;
	L -> L
    end.
    
all_free([], X0, Y0, F) ->
    lists:reverse([{X0,Y0} | F]);
all_free([{X,Y} | L], X0, Y0, F) when Y0 =:= X ->
    all_free(L, X0, Y, F);
all_free([{X,Y} | L], X0, Y0, F) when Y0 < X ->
    all_free(L, X, Y, [{X0,Y0} | F]).

all_allocated(Head) ->
    all_allocated(all(get_freelists(Head)), 0, Head#head.base, []).

all_allocated([], _X0, _Y0, []) ->
    <<>>;
all_allocated([], _X0, _Y0, A0) ->
    [<<From:32, To:32>> | A] = lists:reverse(A0),
    {From, To, list_to_binary(A)};
all_allocated([{X,Y} | L], X0, Y0, A) when Y0 =:= X ->
    all_allocated(L, X0, Y, A);
all_allocated([{X,Y} | L], _X0, Y0, A) when Y0 < X ->
    all_allocated(L, X, Y, [<<Y0:32,X:32>> | A]).

all_allocated_as_list(Head) ->
    all_allocated_as_list(all(get_freelists(Head)), 0, Head#head.base, []).

-dialyzer({no_improper_lists, all_allocated_as_list/4}).

all_allocated_as_list([], _X0, _Y0, []) ->
    [];
all_allocated_as_list([], _X0, _Y0, A) ->
    lists:reverse(A);
all_allocated_as_list([{X,Y} | L], X0, Y0, A) when Y0 =:= X ->
    all_allocated_as_list(L, X0, Y, A);
all_allocated_as_list([{X,Y} | L], _X0, Y0, A) when Y0 < X ->
    all_allocated_as_list(L, X, Y, [[Y0 | X] | A]).

all(Tab) ->
    all(Tab, tuple_size(Tab), []).

all(_Tab, 0, L) ->
    %% This is not as bad as it looks. L contains less than 32 runs,
    %% so there will be only a small number of merges.
    lists:sort(L);
all(Tab, I, L) ->
    LL = collect_tree(element(I, Tab), I, L),
    all(Tab, I-1, LL).

%% Finds allocated areas between Addr (approx.) and Addr+Length.
find_allocated(Ftab, Addr, Length, Base) ->
    MaxAddr = Addr + Length,
    Ints = collect_all_interval(Ftab, Addr, MaxAddr, Base),
    allocated(Ints, Addr, MaxAddr, Ftab, Base).

allocated(Some, Addr, Max, Ftab, Base) ->
    case allocated1(Some, Addr, Max, []) of
        [] ->
            case find_next_allocated(Ftab, Addr, Base) of
                {From,_} -> 
                    find_allocated(Ftab, From, ?CHUNK_SIZE, Base);
                none ->
                    <<>>
            end;
        L -> 
            list_to_binary(lists:reverse(L))
    end.

allocated1([], Y0, Max, A) when Y0 < Max ->
    [<<Y0:32,Max:32>> | A];
allocated1([], _Y0, _Max, A) ->
    A;
allocated1([{X,Y} | L], Y0, Max, A) when Y0 >= X ->
    allocated1(L, Y, Max, A);
allocated1([{X,Y} | L], Y0, Max, A) -> % when Y0 < X
    allocated1(L, Y, Max, [<<Y0:32,X:32>> | A]).

%% Finds the first allocated area starting at Addr or later.
find_next_allocated(Ftab, Addr, Base) ->
    case find_next_free(Ftab, Addr, Base) of
        none ->
            none;
        {Addr1, Pos} when Addr1 =< Addr ->
            find_next_allocated(Ftab, Addr1 + ?POW(Pos-1), Base);
        {Next, _Pos} ->
            {Addr, Next}
    end.

%% Finds the first free address starting att Addr or later. 
%% -> none | {FirstFreeAddress, FtabPosition}
find_next_free(Ftab, Addr, Base) ->
    MaxBud = tuple_size(Ftab),
    find_next_free(Ftab, Addr, 1, MaxBud, -1, -1, Base).

find_next_free(Ftab, Addr0, Pos, MaxBud, Next, PosN, Base)  
                         when Pos =< MaxBud ->
    Addr = adjust_addr(Addr0, Pos, Base),
    PosTab = element(Pos, Ftab),
    case bplus_lookup_next(PosTab, Addr-1) of
        undefined ->
            find_next_free(Ftab, Addr0, Pos+1, MaxBud, Next, PosN, Base);
        {ok, Next1} when PosN =:= -1; Next1 < Next ->
            find_next_free(Ftab, Addr0, Pos+1, MaxBud, Next1, Pos, Base);
        {ok, _} ->
            find_next_free(Ftab, Addr0, Pos+1, MaxBud, Next, PosN, Base)
    end;
find_next_free(_Ftab, _Addr, _Pos, _MaxBud, -1, _PosN, _Base) ->
    none;
find_next_free(_Ftab, _Addr, _Pos, _MaxBud, Next, PosN, _Base) ->
    {Next, PosN}.

collect_all_interval(Ftab, Addr, MaxAddr, Base) ->
    MaxBud = tuple_size(Ftab),
    collect_all_interval(Ftab, Addr, MaxAddr, 1, MaxBud, Base, []).

collect_all_interval(Ftab, L0, U, Pos, MaxBud, Base, Acc0) when Pos =< MaxBud ->
    PosTab = element(Pos, Ftab),
    L = adjust_addr(L0, Pos, Base),
    Acc = collect_interval(PosTab, Pos, L, U, Acc0),
    collect_all_interval(Ftab, L0, U, Pos+1, MaxBud, Base, Acc);
collect_all_interval(_Ftab, _L, _U, _Pos, _MaxBud, _Base, Acc) ->
    lists:sort(Acc).

%% It could be that Addr is inside a free area. This function adjusts
%% the address so that is placed on a boundary in the Pos tree. Inlined.
adjust_addr(Addr, Pos, Base) ->
    Pow = ?POW(Pos - 1),
    Rem = (Addr - Base) rem Pow,
    if
        Rem =:= 0 ->
            Addr;
        Addr < Pow ->
            Addr;
        true ->
            Addr - Rem
    end.

%%%-----------------------------------------------------------------
%%% The Disk Map is used for debugging only.
%%% Very tightly coupled to the way dets_v9 works.
%%%-----------------------------------------------------------------

-define(DM, disk_map).

get_disk_map() ->
    case get(?DM) of
        undefined -> no_disk_map;
        T -> {disk_map, ets:tab2list(T)}
    end.

init_disk_map(Name) ->
    error_logger:info_msg("** dets: (debug) using disk map for ~p~n", [Name]),
    put(?DM, ets:new(any,[ordered_set])).

stop_disk_map() ->
    catch ets:delete(erase(?DM)).

disk_map_segment_p(Fd, P) ->
    case get(?DM) of
        undefined ->
            ok;
        _T ->
            disk_map_segment(P, pread_n(Fd, P, 8*256))
    end.

disk_map_segment(P, Segment) ->
    case get(?DM) of
        undefined ->
            ok;
        T ->
            Ps = segment_fragment_to_pointers(P, iolist_to_binary(Segment)),
            Ss = [{X,<<Sz:32,?ACTIVE:32>>} || 
                     {_P1,<<Sz:32,X:32>>} <- Ps,
                     X > 0], % optimization
            dm(Ps ++ Ss, T)
    end.

disk_map_pread(P) ->
    case get(?DM) of
        undefined ->
            ok;
        T ->
            case ets:lookup(T, P) of
                [] -> 
                    throw({pread, P, 8});
                [{P,{pointer,0,0}}] ->
                    ok;
                [{P,{pointer,Pointer,Sz}}] ->
                    case ets:lookup(T, Pointer) of
                        %% _P =/= P after re-hash...
                        [{Pointer,{slot,_P,Sz}}] ->
                            ok;
                        Got ->
                            throw({pread, P, Pointer, Got})
                    end;
                Got ->
                    throw({pread, P, Got})
            end
    end.

-define(STATUS_POS, 4).
-define(BASE, 1336).
disk_map(Bins) ->
    case get(?DM) of
        undefined -> 
            ok;
        T -> 
            Bs = [{P,iolist_to_binary(Io)} || {P,Io} <- Bins],
            dm(Bs, T)
    end.

dm([{P,_Header} | Bs], T) when P < ?BASE ->
    dm(Bs, T);
dm([{P0,<<?FREE:32>>} | Bs], T) ->
    P = P0 - ?STATUS_POS,
    case ets:lookup(T, P) of
        [] -> 
            throw({free, P0});
        [{P,_OldSz}] ->
            true = ets:delete(T, P)
    end,
    dm(Bs, T);
dm([{SlotP,<<Sz:32,?ACTIVE:32,_/binary>>} | Bs], T) ->
    Ptr = case ets:lookup(T, {pointer,SlotP}) of
              [{{pointer,SlotP}, Pointer}] ->
                  case ets:lookup(T, Pointer) of
                      [{Pointer,{pointer,SlotP,Sz2}}] ->
                          case log2(Sz) =:= log2(Sz2) of
                              true -> 
                                  Pointer;
                              false ->
                                  throw({active, SlotP, Sz, Pointer, Sz2})
                          end;
                      Got ->
                          throw({active, SlotP, Sz, Got})
                  end;
              [] ->
                  throw({active, SlotP, Sz})
          end,
    true = ets:insert(T, {SlotP,{slot,Ptr,Sz}}),
    dm(Bs, T);
dm([{P,<<Sz:32,X:32>>} | Bs], T) ->
    %% Look for slot object in Bs?
    case prev(P, T) of
        {Prev, PrevSz} ->
            throw({prev, P, Sz, X, Prev, PrevSz});
        ok ->
            ok
    end,
    case next(P, 8, T) of
        {next, Next} ->
            %% Can (should?) do more...
            throw({next, P, Sz, X, Next});
        ok ->
            ok
    end,
    true = ets:insert(T, {P,{pointer,X,Sz}}),
    if 
        Sz =:= 0 -> 
            X = 0,
            true;
        true -> 
            true = ets:insert(T, {{pointer,X}, P})
    end,
    dm(Bs, T);
dm([{P,<<X:32>>} | Bs], T) ->
    case ets:lookup(T, X) of
        [] -> throw({segment, P, X});
        [{X,{pointer,0,0}}] -> ok;
        [{X,{pointer,P,X}}] -> ok
    end,
    dm(Bs, T);
dm([{P,<<_Sz:32,B0/binary>>=B} | Bs], T) ->
    Overwrite = 
        case catch binary_to_term(B0) of % accepts garbage at end of binary
            {'EXIT', _} ->
                <<_Sz1:32,B1/binary>> = B0,
                case catch binary_to_term(B1) of
                    {'EXIT', _}  ->
                        false;
                    _ ->
                        true
                end;
            _ -> 
                true
        end,
    if 
        Overwrite ->
            %% overwrite same
            dm([{P-8,<<(byte_size(B) + 8):32,?ACTIVE:32,B/binary>>} | Bs], T);
        true -> 
            dm(segment_fragment_to_pointers(P, B)++Bs, T)
    end;
dm([], _T) ->
    ok.

segment_fragment_to_pointers(_P, <<>>) ->
    [];
segment_fragment_to_pointers(P, <<SzP:8/binary,B/binary>>) ->
    [{P,SzP} | segment_fragment_to_pointers(P+8, B)].

prev(P, T) ->
    case ets:prev(T, P) of
        '$end_of_table' -> ok;
        Prev -> 
            case ets:lookup(T, Prev) of
                [{Prev,{pointer,_Ptr,_}}] when Prev + 8 > P -> 
                    {Prev, 8};
                [{Prev,{slot,_,Sz}}] when Prev + Sz > P ->
                    {Prev, Sz};
                _ ->
                    ok
            end
    end.

next(P, PSz, T) ->
    case ets:next(T, P) of
        '$end_of_table' -> ok;
        Next when P + PSz > Next ->
            {next, Next};
        _ ->
            ok
    end.

%%%-----------------------------------------------------------------
%%% These functions implement a B+ tree.
%%%-----------------------------------------------------------------

-define(max_size, 16).
-define(min_size, 8).
%%-----------------------------------------------------------------
%% Finds out the type of the node: 'l' or 'n'.
%%-----------------------------------------------------------------
-define(NODE_TYPE(Tree), element(1, Tree)).
%% Finds out if a node/leaf is full or not.
-define(FULL(Tree), (bplus_get_size(Tree) >= ?max_size)).
%% Finds out if a node/leaf is filled up over its limit.
-define(OVER_FULL(Tree), (bplus_get_size(Tree) > ?max_size)).
%% Finds out if a node/leaf has less items than allowed.
-define(UNDER_FILLED(Tree), (bplus_get_size(Tree) < ?min_size)).
%% Finds out if a node/leaf has as few items as minimum allowed.
-define(LOW_FILLED(Tree), (bplus_get_size(Tree) =< ?min_size)).
%%Returns a key in a leaf at position Pos.
-define(GET_LEAF_KEY(Leaf, Pos), element(Pos+1, Leaf)).

%% Special for dets.
collect_tree(v, _TI, Acc) -> Acc;
collect_tree(T, TI, Acc) ->
    Pow = ?POW(TI-1),
    collect_tree2(T, Pow, Acc).

collect_tree2(Tree, Pow, Acc) ->
    S = bplus_get_size(Tree),
    case ?NODE_TYPE(Tree) of
	l ->
	    collect_leaf(Tree, S, Pow, Acc);
	n ->
	    collect_node(Tree, S, Pow, Acc)
    end.
    
collect_leaf(_Leaf, 0, _Pow, Acc) ->
    Acc;
collect_leaf(Leaf, I, Pow, Acc) ->
    Key = ?GET_LEAF_KEY(Leaf, I),
    V = {Key, Key+Pow},
    collect_leaf(Leaf, I-1, Pow, [V | Acc]).

collect_node(_Node, 0, _Pow, Acc) ->
    Acc;
collect_node(Node, I, Pow, Acc) ->
    Acc1 = collect_tree2(bplus_get_tree(Node, I), Pow, Acc),
    collect_node(Node, I-1, Pow, Acc1).

%% Special for dets.
tree_to_bin(v, _F, _Max, Ws, WsSz) -> {Ws, WsSz};
tree_to_bin(T, F, Max, Ws, WsSz) ->
    {N, L1, Ws1, WsSz1} = tree_to_bin2(T, F, Max, 0, [], Ws, WsSz),
    {N1, L2, Ws2, WsSz2} = F(N, lists:reverse(L1), Ws1, WsSz1),
    {0, [], NWs, NWsSz} = F(N1, L2, Ws2, WsSz2),
    {NWs, NWsSz}.

tree_to_bin2(Tree, F, Max, N, Acc, Ws, WsSz) when N >= Max ->
    {NN, NAcc, NWs, NWsSz} = F(N, lists:reverse(Acc), Ws, WsSz),
    tree_to_bin2(Tree, F, Max, NN, lists:reverse(NAcc), NWs, NWsSz);
tree_to_bin2(Tree, F, Max, N, Acc, Ws, WsSz) ->
    S = bplus_get_size(Tree),
    case ?NODE_TYPE(Tree) of
	l ->
	    {N+S, leaf_to_bin(bplus_leaf_to_list(Tree), Acc), Ws, WsSz};
	n ->
	    node_to_bin(Tree, F, Max, N, Acc, 1, S, Ws, WsSz)
    end.
    
node_to_bin(_Node, _F, _Max, N, Acc, I, S, Ws, WsSz) when I > S ->
    {N, Acc, Ws, WsSz};
node_to_bin(Node, F, Max, N, Acc, I, S, Ws, WsSz) ->
    {N1,Acc1,Ws1,WsSz1} = 
	tree_to_bin2(bplus_get_tree(Node, I), F, Max, N, Acc, Ws, WsSz),
    node_to_bin(Node, F, Max, N1, Acc1, I+1, S, Ws1, WsSz1).

leaf_to_bin([N | L], Acc) ->
    leaf_to_bin(L, [<<N:32>> | Acc]);
leaf_to_bin([], Acc) ->
    Acc.

%% Special for dets. 
list_to_tree(L) ->
    leafs_to_nodes(L, length(L), fun bplus_mk_leaf/1, []).

leafs_to_nodes([], 0, _F, [T]) ->
    T;
leafs_to_nodes([], 0, _F, L) ->
    leafs_to_nodes(lists:reverse(L), length(L), fun mk_node/1, []);
leafs_to_nodes(Ls, Sz, F, L) ->
    I = if 
	    Sz =< 16 -> Sz;
	    Sz =< 32 -> Sz div 2;
	    true -> 12
	end,
    {L1, R} = split_list(Ls, I, []),
    N = F(L1),
    Sz1 = Sz - I, 
    leafs_to_nodes(R, Sz1, F, [N | L]).

mk_node([E | Es]) ->
    NL = [E | lists:foldr(fun(X, A) -> [get_first_key(X), X | A] end, [], Es)],
    bplus_mk_node(NL).    

split_list(L, 0, SL) ->
    {SL, L};
split_list([E | Es], I, SL) ->
    split_list(Es, I-1, [E | SL]).

get_first_key(T) ->
    case ?NODE_TYPE(T) of
	l ->
	    ?GET_LEAF_KEY(T, 1);
	n ->
	    get_first_key(bplus_get_tree(T, 1))
    end.

%% Special for dets.
collect_interval(v, _TI, _L, _U, Acc) -> Acc;
collect_interval(T, TI, L, U, Acc) ->
    Pow = ?POW(TI-1),
    collect_interval2(T, Pow, L, U, Acc).

collect_interval2(Tree, Pow, L, U, Acc) ->
    S = bplus_get_size(Tree),
    case ?NODE_TYPE(Tree) of
	l ->
	    collect_leaf_interval(Tree, S, Pow, L, U, Acc);
	n ->
            {Max, _} = bplus_select_sub_tree(Tree, U),
            {Min, _} = bplus_select_sub_tree_2(Tree, L, Max),
	    collect_node_interval(Tree, Min, Max, Pow, L, U, Acc)
    end.
    
collect_leaf_interval(_Leaf, 0, _Pow, _L, _U, Acc) ->
    Acc;
collect_leaf_interval(Leaf, I, Pow, L, U, Acc) ->
    Key = ?GET_LEAF_KEY(Leaf, I),
    if
        Key < L -> 
            Acc;
        Key > U -> 
            collect_leaf_interval(Leaf, I-1, Pow, L, U, Acc);
        true -> 
            collect_leaf_interval(Leaf, I-1, Pow, L, U, [{Key,Key+Pow} | Acc])
    end.

collect_node_interval(_Node, I, UP, _Pow, _L, _U, Acc) when I > UP ->
    Acc;
collect_node_interval(Node, I, UP, Pow, L, U, Acc) ->
    Acc1 = collect_interval2(bplus_get_tree(Node, I), Pow, L, U, Acc),
    collect_node_interval(Node, I+1, UP, Pow, L, U, Acc1).

%%-----------------------------------------------------------------
%% Func: empty_tree/0
%% Purpose: Creates a new empty tree.
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_empty_tree() -> v.

%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Looks for Key in the Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
bplus_lookup(v, _Key) -> undefined;
bplus_lookup(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_lookup_leaf(Key, Tree);
	n ->
	    {_, SubTree} = bplus_select_sub_tree(Tree, Key),
	    bplus_lookup(SubTree, Key)
    end.

%%-----------------------------------------------------------------
%% Searches through a leaf until the Key is ok or
%% when it is determined that it does not exist.
%%-----------------------------------------------------------------
bplus_lookup_leaf(Key, Leaf) -> 
    bplus_lookup_leaf_2(Key, Leaf, bplus_get_size(Leaf)).

bplus_lookup_leaf_2(_, _, 0) -> undefined;
bplus_lookup_leaf_2(Key, Leaf, N) ->
    case ?GET_LEAF_KEY(Leaf, N) of
	Key -> {ok, Key};
	_ ->
	    bplus_lookup_leaf_2(Key, Leaf, N-1)
    end.

%%-----------------------------------------------------------------
%% Func: lookup_first/1
%% Purpose: Finds the smallest key in the entire Tree.
%% Returns: {ok, {Key, Val}} | 'undefined'.
%%-----------------------------------------------------------------
bplus_lookup_first(v) -> undefined;
bplus_lookup_first(Tree) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    % Then it is the leftmost key here.
	    {ok, ?GET_LEAF_KEY(Tree, 1)};         
	n ->
	    % Look in the leftmost subtree.
	    bplus_lookup_first(bplus_get_tree(Tree, 1))
    end.


%%-----------------------------------------------------------------
%% Func: lookup_next/2
%% Purpose: Finds the next key nearest after Key.
%% Returns: {ok, {Key, Val}} | 'undefined'. NIX!!!
%%-----------------------------------------------------------------
bplus_lookup_next(v, _) -> undefined;
bplus_lookup_next(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    lookup_next_leaf(Key, Tree);
	n ->
	    {Pos, SubTree} = bplus_select_sub_tree(Tree, Key),
	    case bplus_lookup_next(SubTree, Key) of
		undefined ->
		    S = bplus_get_size(Tree),
		    if
			% There is a right brother.
			S > Pos ->                  
			    bplus_lookup_first(bplus_get_tree(Tree, Pos+1));
			% No there is no right brother.
			true ->
			    undefined
		    end;
		% We ok a next item.
		Result ->                         
		    Result
	    end
    end.

%%-----------------------------------------------------------------
%% Returns {ok, NextKey} if there is a key in the leaf which is greater.
%% If there is no such key we return 'undefined' instead.
%% Key does not have to be a key in the structure, just a search value.
%%-----------------------------------------------------------------
lookup_next_leaf(Key, Leaf) -> 
    lookup_next_leaf_2(Key, Leaf, bplus_get_size(Leaf), 1).

lookup_next_leaf_2(Key, Leaf, Size, Size) -> 
    % This is the rightmost key.
    K = ?GET_LEAF_KEY(Leaf, Size),
    if
	K > Key ->
	    {ok, ?GET_LEAF_KEY(Leaf, Size)};
	true ->
	    undefined
    end;
lookup_next_leaf_2(Key, Leaf, Size, N) ->
    K = ?GET_LEAF_KEY(Leaf, N),
    if
	K < Key ->                         
	    % K is still smaller, try next in the leaf.
	    lookup_next_leaf_2(Key, Leaf, Size, N+1);
	Key == K ->
	    % Since this is exact Key it must be the next.
	    {ok, ?GET_LEAF_KEY(Leaf, N+1)};
        true ->
            % Key was not an exact specification.
	    % It must be K that is next greater.
	    {ok, ?GET_LEAF_KEY(Leaf, N)}
    end.

%%-----------------------------------------------------------------
%% Func: insert/3
%% Purpose: Inserts a new {Key, Value} into the tree.
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_insert(v, Key) -> bplus_mk_leaf([Key]);
bplus_insert(Tree, Key) ->
    NewTree = bplus_insert_in(Tree, Key),
    case ?OVER_FULL(NewTree) of
	false ->
	    NewTree;
	% If the node is over-full the tree will grow.
	true ->
	    {LTree, DKey, RTree} = 
		case ?NODE_TYPE(NewTree) of
		    l ->
			bplus_split_leaf(NewTree);
		    n ->
			bplus_split_node(NewTree)
		end,
	    bplus_mk_node([LTree, DKey, RTree])
    end.

%%-----------------------------------------------------------------
%% Func: delete/2
%% Purpose: Deletes a key from the tree (if present).
%% Returns: tree()
%%-----------------------------------------------------------------
bplus_delete(v, _Key) -> v;
bplus_delete(Tree, Key) ->
    NewTree = bplus_delete_in(Tree, Key),
    S = bplus_get_size(NewTree),
    case ?NODE_TYPE(NewTree) of
	l ->
	    if
		S =:= 0 ->
		    v;
		true ->
		    NewTree
	    end;
	n ->
	    if
		S =:= 1 ->
		    bplus_get_tree(NewTree, 1);
		true ->
		    NewTree
	    end
    end.


%%% -----------------------
%%% Help function to insert.
%%% -----------------------

bplus_insert_in(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_insert_in_leaf(Tree, Key);
	n ->
	    {Pos, SubTree} = bplus_select_sub_tree(Tree, Key),  
            % Pos = "the position of the subtree".
	    NewSubTree = bplus_insert_in(SubTree, Key),
	    case ?OVER_FULL(NewSubTree) of
		false ->
		    bplus_put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case bplus_reorganize_tree_ins(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    bplus_put_subtree(bplus_put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    bplus_put_subtree(bplus_put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{split, {LeftT, DKey, RightT}} ->
			    bplus_extend_tree(Tree, {LeftT, DKey, RightT}, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Inserts a key in correct position in a leaf.
%%-----------------------------------------------------------------
bplus_insert_in_leaf(Leaf, Key) ->
    bplus_insert_in_leaf_2(Leaf, Key, bplus_get_size(Leaf), []).

bplus_insert_in_leaf_2(Leaf, Key, 0, Accum) ->
    bplus_insert_in_leaf_3(Leaf, 0, [Key|Accum]);
bplus_insert_in_leaf_2(Leaf, Key, N, Accum) ->
    K = ?GET_LEAF_KEY(Leaf, N),
    if
	Key < K ->
	    % Not here!
	    bplus_insert_in_leaf_2(Leaf, Key, N-1, [K|Accum]);
	K < Key ->
	    % Insert here.
	    bplus_insert_in_leaf_3(Leaf, N-1, [K, Key|Accum]);
	K == Key ->
	    % Replace (?).
	    bplus_insert_in_leaf_3(Leaf, N-1, [ Key|Accum])
    end.

bplus_insert_in_leaf_3(_Leaf, 0, LeafList) ->
    bplus_mk_leaf(LeafList);
bplus_insert_in_leaf_3(Leaf, N, LeafList) ->
    bplus_insert_in_leaf_3(Leaf, N-1, [?GET_LEAF_KEY(Leaf, N)|LeafList]).


%%% -------------------------
%%% Help functions for delete.
%%% -------------------------

bplus_delete_in(Tree, Key) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    bplus_delete_in_leaf(Tree, Key);
	n ->
	    {Pos, SubTree} = bplus_select_sub_tree(Tree, Key),  
	    % Pos = "the position of the subtree".
	    NewSubTree = bplus_delete_in(SubTree, Key),
	    % Check if it has become to small now
	    case ?UNDER_FILLED(NewSubTree) of
		false ->
		    bplus_put_subtree(Tree, [NewSubTree, Pos]);
		true ->
		    case bplus_reorganize_tree_del(Tree, NewSubTree, Pos) of
			{left, {LeftT, DKey, MiddleT}} ->
			    bplus_put_subtree(bplus_put_lkey(Tree, DKey, Pos),
					[LeftT, Pos-1, MiddleT, Pos]);
			{right, {MiddleT, DKey, RightT}} ->
			    bplus_put_subtree(bplus_put_rkey(Tree, DKey, Pos),
					[MiddleT, Pos, RightT, Pos+1]);
			{join_left, JoinedTree} ->
			    bplus_joinleft_tree(Tree, JoinedTree, Pos);
			{join_right, JoinedTree} ->
			    bplus_joinright_tree(Tree, JoinedTree, Pos)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% Deletes a key from the leaf returning a new (smaller) leaf.
%%-----------------------------------------------------------------
bplus_delete_in_leaf(Leaf, Key) ->
    bplus_delete_in_leaf_2(Leaf, Key, bplus_get_size(Leaf), []).

bplus_delete_in_leaf_2(Leaf, _, 0, _) -> Leaf;
bplus_delete_in_leaf_2(Leaf, Key, N, Accum) ->
    K = ?GET_LEAF_KEY(Leaf, N),
    if
	Key == K ->
            % Remove this one!
	    bplus_delete_in_leaf_3(Leaf, N-1, Accum);
	true ->
	    bplus_delete_in_leaf_2(Leaf, Key, N-1, [K|Accum])
    end.

bplus_delete_in_leaf_3(_Leaf, 0, LeafList) ->
    bplus_mk_leaf(LeafList);
bplus_delete_in_leaf_3(Leaf, N, LeafList) ->
    bplus_delete_in_leaf_3(Leaf, N-1, [?GET_LEAF_KEY(Leaf, N)|LeafList]).



%%-----------------------------------------------------------------
%% Selects and returns which subtree the search should continue in.
%%-----------------------------------------------------------------
bplus_select_sub_tree(Tree, Key) ->
    bplus_select_sub_tree_2(Tree, Key, bplus_get_size(Tree)).

bplus_select_sub_tree_2(Tree, _Key, 1) -> {1, bplus_get_tree(Tree, 1)};
bplus_select_sub_tree_2(Tree, Key, N) ->
    K = bplus_get_lkey(Tree, N),
    if
	K > Key ->
	    bplus_select_sub_tree_2(Tree, Key, N-1);
	K =< Key ->
            % Here it is!
	    {N, bplus_get_tree(Tree, N)}
    end.

%%-----------------------------------------------------------------
%% Selects which brother that should take over some of our items.
%% Or if they are both full makes a split.
%%-----------------------------------------------------------------
bplus_reorganize_tree_ins(Tree, NewSubTree, 1) ->
    RTree = bplus_get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case ?FULL(RTree) of
	false ->
	    bplus_reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is full, we must split this one!
	    bplus_reorganize_tree_s(NewSubTree)
    end;
bplus_reorganize_tree_ins(Tree, NewSubTree, Pos) ->
    Size = bplus_get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = bplus_get_tree(Tree, Pos-1),
 	    case ?FULL(LTree) of
		false ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    % It is full, we must split this one!
		    bplus_reorganize_tree_s(NewSubTree)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    RTree = bplus_get_tree(Tree, Pos+1),
	    SL = bplus_get_size(LTree),
	    SR = bplus_get_size(RTree),
	    if
		SL > SR ->
		    bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		SL < SR ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
		    case ?FULL(LTree) of
			false ->
			    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    bplus_reorganize_tree_s(NewSubTree)
		    end
	    end
    end.

%%-----------------------------------------------------------------
%% This function fills over items from brothers to maintain the minimum
%% number of items per node/leaf.
%%-----------------------------------------------------------------
bplus_reorganize_tree_del(Tree, NewSubTree, 1) ->
    % The case when Pos is at leftmost position.
    RTree = bplus_get_tree(Tree, 2),  % 2 = Pos+1 = 1+1.
    case ?LOW_FILLED(RTree) of
	false ->
	    bplus_reorganize_tree_r(Tree, NewSubTree, 1, RTree);
	true ->
            % It is to small, we must join them!
	    bplus_reorganize_tree_jr(Tree, NewSubTree, 1, RTree)
    end;
bplus_reorganize_tree_del(Tree, NewSubTree, Pos) ->
    Size = bplus_get_size(Tree),
    if
	Pos == Size ->
            % Pos is the rightmost postion!.
            % Our only chance is the left one.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    case ?LOW_FILLED(LTree) of
		false ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		true ->
                    % It is to small, we must join this one!
		    bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
	    end;
	true ->
            % Pos is somewhere inside the node.
	    LTree = bplus_get_tree(Tree, Pos-1),
	    RTree = bplus_get_tree(Tree, Pos+1),
	    SL = bplus_get_size(LTree),
	    SR = bplus_get_size(RTree),
	    if
		SL>SR ->
		    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
		SL < SR ->
		    bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree);
		true ->
		    case ?LOW_FILLED(LTree) of
			false ->
			    bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree);
			true ->
			    bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree)
		    end
	    end
    end.


bplus_reorganize_tree_l(Tree, NewSubTree, Pos, LTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {left, bplus_split_leaf(
		     bplus_mk_leaf(
		       lists:append(bplus_leaf_to_list(LTree),
				    bplus_leaf_to_list(NewSubTree))))};
	n ->
	    {left, bplus_split_node(
		     bplus_mk_node(
		       lists:append([bplus_node_to_list(LTree),
				     [bplus_get_lkey(Tree, Pos)],
				     bplus_node_to_list(NewSubTree)])))}
    end.

bplus_reorganize_tree_r(Tree, NewSubTree, Pos, RTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {right, 
	     bplus_split_leaf(
	       bplus_mk_leaf(
		 lists:append([bplus_leaf_to_list(NewSubTree),
			       bplus_leaf_to_list(RTree)])))};
	n ->
	    {right, 
	     bplus_split_node(
	       bplus_mk_node(
		 lists:append([bplus_node_to_list(NewSubTree),
			       [bplus_get_rkey(Tree, Pos)],
			       bplus_node_to_list(RTree)])))}
    end.

bplus_reorganize_tree_s(NewSubTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {split, bplus_split_leaf(NewSubTree)};
	n ->
	    {split, bplus_split_node(NewSubTree)}
    end.

bplus_reorganize_tree_jl(Tree, NewSubTree, Pos, LTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {join_left, 
	     bplus_mk_leaf(lists:append([bplus_leaf_to_list(LTree),
					 bplus_leaf_to_list(NewSubTree)]))};
	n ->
	    {join_left, 
	     bplus_mk_node(lists:append([bplus_node_to_list(LTree),
					 [bplus_get_lkey(Tree, Pos)],
					 bplus_node_to_list(NewSubTree)]))}
    end.

bplus_reorganize_tree_jr(Tree, NewSubTree, Pos, RTree) ->
    case ?NODE_TYPE(NewSubTree) of
	l ->
	    {join_right, 
	     bplus_mk_leaf(lists:append([bplus_leaf_to_list(NewSubTree),
					 bplus_leaf_to_list(RTree)]))};
	n ->
	    {join_right, 
	     bplus_mk_node(lists:append([bplus_node_to_list(NewSubTree),
					 [bplus_get_rkey(Tree, Pos)],
					 bplus_node_to_list(RTree)]))}
    end.


%%-----------------------------------------------------------------
%% Takes a leaf and divides it into two equal big leaves.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
bplus_split_leaf(Leaf) ->
    S = bplus_get_size(Leaf),
    bplus_split_leaf_2(Leaf, S, S div 2, []).

bplus_split_leaf_2(Leaf, Pos, 1, Accum) -> 
    K = ?GET_LEAF_KEY(Leaf, Pos),
    bplus_split_leaf_3(Leaf, Pos-1, [], K, [K|Accum]);
bplus_split_leaf_2(Leaf, Pos, N, Accum) ->
    bplus_split_leaf_2(Leaf, Pos-1, N-1, [?GET_LEAF_KEY(Leaf, Pos)|Accum]).

bplus_split_leaf_3(_, 0, LeftAcc, DKey, RightAcc) ->
    {bplus_mk_leaf(LeftAcc), DKey, bplus_mk_leaf(RightAcc)};
bplus_split_leaf_3(Leaf, Pos, LeftAcc, DKey, RightAcc) ->
    bplus_split_leaf_3(Leaf, Pos-1, [?GET_LEAF_KEY(Leaf, Pos)|LeftAcc],
		       DKey, RightAcc).

%%-----------------------------------------------------------------
%% Takes a node and divides it into two equal big nodes.
%% The result is returned in a tuple. The dividing key is also returned.
%%-----------------------------------------------------------------
bplus_split_node(Node) ->
    S = bplus_get_size(Node),
    bplus_split_node_2(Node, S, S div 2, []).

bplus_split_node_2(Node, Pos, 1, Accum) ->
    bplus_split_node_3(Node, Pos-1, [], bplus_get_lkey(Node, Pos),
		 [bplus_get_tree(Node, Pos)|Accum]);
bplus_split_node_2(Node, Pos, N, Accum) ->
    bplus_split_node_2(Node, Pos-1, N-1, [bplus_get_lkey(Node, Pos),
				    bplus_get_tree(Node, Pos)|Accum]).

bplus_split_node_3(Node, 1, LeftAcc, DKey, RightAcc) ->
    {bplus_mk_node([bplus_get_tree(Node, 1)|LeftAcc]), DKey, 
     bplus_mk_node(RightAcc)};
bplus_split_node_3(Node, Pos, LeftAcc, DKey, RightAcc) ->
    bplus_split_node_3(Node, Pos-1,
		       [bplus_get_lkey(Node, Pos), 
			bplus_get_tree(Node, Pos)|LeftAcc],
		       DKey, RightAcc).

%%-----------------------------------------------------------------
%% Inserts a joined tree insted of the old one at position Pos and
%% the one nearest left/right brother.
%%-----------------------------------------------------------------
bplus_joinleft_tree(Tree, JoinedTree, Pos) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos, bplus_get_size(Tree), []).
bplus_joinright_tree(Tree, JoinedTree, Pos) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos+1, bplus_get_size(Tree), []).

bplus_join_tree_2(Tree, JoinedTree, Pos, Pos, Accum) ->
    bplus_join_tree_3(Tree, Pos-2, [JoinedTree|Accum]);
bplus_join_tree_2(Tree, JoinedTree, Pos, N, Accum) ->
    bplus_join_tree_2(Tree, JoinedTree, Pos, N-1,
		[bplus_get_lkey(Tree, N), bplus_get_tree(Tree, N)|Accum]).

bplus_join_tree_3(_Tree, 0, Accum) -> bplus_mk_node(Accum);
bplus_join_tree_3(Tree, Pos, Accum) ->
    bplus_join_tree_3(Tree, Pos-1, [bplus_get_tree(Tree, Pos), 
				    bplus_get_rkey(Tree, Pos)|Accum]).

%%% ---------------------------------
%%% Primitive datastructure functions.
%%% ---------------------------------

%%-----------------------------------------------------------------
%% Constructs a node out of list format.
%%-----------------------------------------------------------------
bplus_mk_node(NodeList) -> list_to_tuple([ n |NodeList]).

%%-----------------------------------------------------------------
%% Converts the node into list format.
%%-----------------------------------------------------------------
bplus_node_to_list(Node) ->
    [_|NodeList] = tuple_to_list(Node),
    NodeList.

%%-----------------------------------------------------------------
%% Constructs a leaf out of list format.
%%-----------------------------------------------------------------
bplus_mk_leaf(KeyList) -> list_to_tuple([l|KeyList]).

%%-----------------------------------------------------------------
%% Converts a leaf into list format.
%%-----------------------------------------------------------------
bplus_leaf_to_list(Leaf) ->
    [_|LeafList] = tuple_to_list(Leaf),
    LeafList.

%%-----------------------------------------------------------------
%% Changes subtree "pointers" in a node.
%%-----------------------------------------------------------------
bplus_put_subtree(Tree, []) -> Tree;
bplus_put_subtree(Tree, [NewSubTree, Pos|Rest]) ->
    bplus_put_subtree(setelement(Pos*2, Tree, NewSubTree), Rest).

%%-----------------------------------------------------------------
%% Replaces the tree at position Pos with two new trees.
%%-----------------------------------------------------------------
bplus_extend_tree(Tree, Inserts, Pos) ->
    bplus_extend_tree_2(Tree, Inserts, Pos, bplus_get_size(Tree), []).

bplus_extend_tree_2(Tree, {T1, DKey, T2}, Pos, Pos, Accum) ->
    bplus_extend_tree_3(Tree, Pos-1, [T1, DKey, T2|Accum]);
bplus_extend_tree_2(Tree, Inserts, Pos, N, Accum) ->
    bplus_extend_tree_2(Tree, Inserts, Pos, N-1,
		  [bplus_get_lkey(Tree, N), bplus_get_tree(Tree, N)|Accum]).

bplus_extend_tree_3(_, 0, Accum) -> bplus_mk_node(Accum);
bplus_extend_tree_3(Tree, N, Accum) ->
    bplus_extend_tree_3(Tree, N-1, [bplus_get_tree(Tree, N), 
				    bplus_get_rkey(Tree, N)|Accum]).

%%-----------------------------------------------------------------
%% Changes the dividing key between two trees.
%%-----------------------------------------------------------------
bplus_put_lkey(Tree, DKey, Pos) -> setelement(Pos*2-1, Tree, DKey).
bplus_put_rkey(Tree, DKey, Pos) -> setelement(Pos*2+1, Tree, DKey).


%%-----------------------------------------------------------------
%% Calculates the number of items in a node/leaf.
%%-----------------------------------------------------------------
bplus_get_size(Tree) ->
    case ?NODE_TYPE(Tree) of
	l ->
	    tuple_size(Tree)-1;
	n ->
	    tuple_size(Tree) div 2
    end.

%%-----------------------------------------------------------------
%% Returns a tree at position Pos from an internal node.
%%-----------------------------------------------------------------
bplus_get_tree(Tree, Pos) -> element(Pos*2, Tree).

%%-----------------------------------------------------------------
%% Returns dividing keys, left of or right of a tree.
%%-----------------------------------------------------------------
bplus_get_lkey(Tree, Pos) -> element(Pos*2-1, Tree).
bplus_get_rkey(Tree, Pos) -> element(Pos*2+1, Tree).

