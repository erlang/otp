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
-module(dets_v8).

%% Dets files, implementation part. This module handles versions up to
%% and including 8(c). To be called from dets.erl only.

-export([mark_dirty/1, read_file_header/2,
         check_file_header/2, do_perform_save/1, initiate_file/11,
         init_freelist/2, fsck_input/4,
         bulk_input/3, output_objs/4, write_cache/1, may_grow/3,
         find_object/2, re_hash/2, slot_objs/2, scan_objs/8,
         db_hash/2, no_slots/1, table_parameters/1]).

-export([file_info/1, v_segments/1]).

-export([cache_segps/3]).

%% For backward compatibility.
-export([sz2pos/1]).

-dialyzer(no_improper_lists).

-compile({inline, [{sz2pos,1},{scan_skip,7}]}).
-compile({inline, [{skip_bytes,5}, {get_segp,1}]}).
-compile({inline, [{wl_lookup,5}]}).
-compile({inline, [{actual_seg_size,0}]}).

-include("dets.hrl").

%%  The layout of the file is :
%%
%%   bytes   decsription
%%  ---------------------- File header
%%    4      FreelistsPointer
%%    4      Cookie
%%    4      ClosedProperly (pos=8)
%%    4      Type (pos=12)
%%    4      Version (pos=16)
%%    4      M
%%    4      Next
%%    4      KeyPos
%%    4      NoObjects
%%    4      N
%%  ------------------ end of file header
%%    4*8192 SegmentArray
%%  ------------------
%%    4*256  First segment
%%  ----------------------------- This is BASE.
%%    ???    Objects (free and alive)
%%    4*256  Second segment (2 kB now, due to a bug)
%%    ???    Objects (free and alive)
%%    ... more objects and segments ...
%%  -----------------------------
%%    ???    Free lists
%%  -----------------------------
%%    4      File size, in bytes. 

%%  The first slot (0) in the segment array always points to the
%%  pre-allocated first segment.
%%  Before we can find an object we must find the slot where the
%%  object resides. Each slot is a (possibly empty) list (or chain) of
%%  objects that hash to the same slot. If the value stored in the
%%  slot is zero, the slot chain is empty. If the slot value is
%%  non-zero, the value points to a position in the file where the
%%  chain starts. Each object in a chain has the following layout:
%%
%%   bytes  decsription
%%  --------------------
%%    4     Pointer to the next object of the chain.
%%    4     Size of the object in bytes (Sz).
%%    4     Status  (FREE or ACTIVE)
%%    Sz    Binary representing the object
%%
%%  The status field is used while repairing a file (but not next or size).
%%
%%|---------------|
%%|      head     |
%%|       	  |
%%|               |
%%|_______________|
%%|		  |------|
%%|___seg ptr1____|      |
%%|		  |      |
%%|__ seg ptr 2___|      |
%%|               |      |    segment 1
%%|	....	  |      V _____________
%%			 |		|
%%			 |		|
%%			 |___slot 0 ____|
%%                       |              |
%%                       |___slot 1 ____|-----|
%%			 |		|     |
%%			 |   .....	|     |  1:st obj in slot 1
%%					      V  segment 1
%%						|-----------|
%%						|  next     |
%%						|___________|
%%						|  size     |
%%						|___________|
%%						|  status   |
%%						|___________|
%%						|	    |
%%						|           |
%%						|   obj     |
%%						|           |

%%%
%%% File header
%%%

-define(HEADSZ, 40).          % The size of the file header, in bytes.
-define(SEGSZ, 256).          % Size of a segment, in words.
-define(SEGSZ_LOG2, 8).
-define(SEGARRSZ, 8192).      % Maximal number of segments.
-define(SEGADDR(SegN), (?HEADSZ + (4 * (SegN)))).
-define(BASE, ?SEGADDR((?SEGSZ + ?SEGARRSZ))).
-define(MAXOBJS, (?SEGSZ * ?SEGARRSZ)). % 2 M objects

-define(SLOT2SEG(S), ((S) bsr ?SEGSZ_LOG2)).

%% BIG is used for hashing. BIG must be greater than the maximum
%% number of slots, currently MAXOBJS.
-define(BIG, 16#ffffff).

%% Hard coded positions into the file header:
-define(FREELIST_POS, 0).
-define(CLOSED_PROPERLY_POS, 8).
-define(D_POS, 20).
-define(NO_OBJECTS_POS, (?D_POS + 12)).

%% The version of a dets file is indicated by the ClosedProperly
%% field. Version 6 was used in the R1A release, and version 7 in the
%% R1B release up to and including the R3B01 release. Both version 6
%% and version 7 indicate properly closed files by the value
%% CLOSED_PROPERLY.
%%
%% The current version, 8, has three sub-versions:
%%
%% - 8(a), indicated by the value CLOSED_PROPERLY (same as in versions 6 
%%         and 7), introduced in R3B02;
%% - 8(b), indicated by the value CLOSED_PROPERLY2(_NEED_COMPACTING),
%%         introduced in R5A and used up to and including R6A;
%% - 8(c), indicated by the value CLOSED_PROPERLY_NEW_HASH(_NEED_COMPACTING),
%%         in use since R6B.
%%
%% The difference between the 8(a) and the 8(b) versions is the format
%% used for free lists saved on dets files.
%% The 8(c) version uses a different hashing algorithm, erlang:phash
%% (former versions use erlang:hash).
%% Version 8(b) files are only converted to version 8(c) if repair is
%% done, so we need compatibility with 8(b) for a _long_ time.
%%
%% There are known bugs due to the fact that keys and objects are
%% sometimes compared (==) and sometimes matched (=:=). The version
%% used by default (9, see dets_v9.erl) does not have this problem.

-define(NOT_PROPERLY_CLOSED,0).
-define(CLOSED_PROPERLY,1).
-define(CLOSED_PROPERLY2,2).
-define(CLOSED_PROPERLY2_NEED_COMPACTING,3).
-define(CLOSED_PROPERLY_NEW_HASH,4).
-define(CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING,5).

-define(FILE_FORMAT_VERSION, 8).
-define(CAN_BUMP_BY_REPAIR, [6, 7]).
-define(CAN_CONVERT_FREELIST, [8]).

%%%
%%% Object header (next, size, status).
%%%

-define(OHDSZ, 12).         % The size of the object header, in bytes.
-define(STATUS_POS, 8).     % Position of the status field.

%% The size of each object is a multiple of 16.
%% BUMP is used when repairing files.
-define(BUMP, 16).

-define(ReadAhead, 512).

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%% -> ok | throw({NewHead,Error})
mark_dirty(Head) ->
    Dirty = [{?CLOSED_PROPERLY_POS, <<?NOT_PROPERLY_CLOSED:32>>}],
    {_NewHead, ok} = dets_utils:pwrite(Head, Dirty),
    ok = dets_utils:sync(Head),
    {ok, _Pos} = dets_utils:position(Head, Head#head.freelists_p),
    ok = dets_utils:truncate(Head, cur).

%% -> {ok, head()} | throw(Error)
initiate_file(Fd, Tab, Fname, Type, Kp, MinSlots, MaxSlots, 
		Ram, CacheSz, Auto, _DoInitSegments) ->
    Freelist = 0,
    Cookie = ?MAGIC,
    ClosedProperly = ?NOT_PROPERLY_CLOSED, % immediately overwritten
    Version = ?FILE_FORMAT_VERSION,
    Factor = est_no_segments(MinSlots),
    N = 0,
    M = Next = ?SEGSZ * Factor,
    NoObjects = 0,
    dets_utils:pwrite(Fd, Fname, 0, 
                      <<Freelist:32,
                      Cookie:32,
                      ClosedProperly:32,
                      (dets_utils:type_to_code(Type)):32,
                      Version:32,
                      M:32,
                      Next:32,
                      Kp:32,
                      NoObjects:32,
                      N:32,
		      0:(?SEGARRSZ*4)/unit:8, % Initialize SegmentArray
		      0:(?SEGSZ*4)/unit:8>>), % Initialize first segment
    %% We must set the first slot of the segment pointer array to
    %% point to the first segment
    Pos = ?SEGADDR(0),
    SegP = (?HEADSZ + (4 * ?SEGARRSZ)),
    dets_utils:pwrite(Fd, Fname, Pos, <<SegP:32>>),
    segp_cache(Pos, SegP),

    Ftab = dets_utils:init_alloc(?BASE),
    H0 = #head{freelists=Ftab, fptr = Fd, base = ?BASE},
    {H1, Ws} = init_more_segments(H0, 1, Factor, undefined, []),

    %% This is not optimal but simple: always initiate the segments.
    dets_utils:pwrite(Fd, Fname, Ws),

    %% Return a new nice head structure
    Head = #head{
      m  = M,
      m2 = M * 2,
      next = Next,
      fptr = Fd,
      no_objects = NoObjects,
      n = N,
      type = Type,
      update_mode = dirty,
      freelists = H1#head.freelists,
      auto_save = Auto,
      hash_bif = phash,
      keypos = Kp,
      min_no_slots = Factor * ?SEGSZ,
      max_no_slots = no_segs(MaxSlots) * ?SEGSZ,
      
      ram_file = Ram, 
      filename = Fname, 
      name = Tab,
      cache = dets_utils:new_cache(CacheSz),
      version = Version,
      bump = ?BUMP,
      base = ?BASE,
      mod = ?MODULE
     },
    {ok, Head}.

est_no_segments(MinSlots) when 1 + ?SLOT2SEG(MinSlots) > ?SEGARRSZ ->
    ?SEGARRSZ;
est_no_segments(MinSlots) ->
    1 + ?SLOT2SEG(MinSlots).

init_more_segments(Head, SegNo, Factor, undefined, Ws) when SegNo < Factor ->
    init_more_segments(Head, SegNo, Factor, seg_zero(), Ws);
init_more_segments(Head, SegNo, Factor, SegZero, Ws) when SegNo < Factor ->
    {NewHead, W} = allocate_segment(Head, SegZero, SegNo),
    init_more_segments(NewHead, SegNo+1, Factor, SegZero, W++Ws);
init_more_segments(Head, _SegNo, _Factor, _SegZero, Ws) ->
    {Head, Ws}.

allocate_segment(Head, SegZero, SegNo) ->
    %% may throw error:
    {NewHead, Segment, _} = dets_utils:alloc(Head, 4 * ?SEGSZ),
    InitSegment = {Segment, SegZero},
    Pos = ?SEGADDR(SegNo),
    segp_cache(Pos, Segment),
    SegPointer = {Pos, <<Segment:32>>},
    {NewHead, [InitSegment, SegPointer]}.

%% Read free lists (using a Buddy System) from file. 
init_freelist(Head, {convert_freelist,_Version}) ->
    %% This function converts the saved freelist of the form
    %% [{Slot1,Addr1},{Addr1,Addr2},...,{AddrN,0},{Slot2,Addr},...]
    %% i.e each slot is a linked list which ends with a 0.
    %% This is stored in a bplus_tree per Slot.
    %% Each Slot is a position in a tuple.

    Ftab = dets_utils:empty_free_lists(),
    Pos = Head#head.freelists_p,
    case catch prterm(Head, Pos, ?OHDSZ) of
	{0, _Sz, Term}  ->
	    FreeList1 = lists:reverse(Term),
            FreeList = dets_utils:init_slots_from_old_file(FreeList1, Ftab),
            Head#head{freelists = FreeList, base = ?BASE};
	_ ->
	    throw({error, {bad_freelists, Head#head.filename}})
    end;
init_freelist(Head, _) ->
    %% bplus_tree stored as is
    Pos = Head#head.freelists_p,
    case catch prterm(Head, Pos, ?OHDSZ) of
	{0, _Sz, Term}  ->
            Head#head{freelists = Term, base = ?BASE};
	_ ->
	    throw({error, {bad_freelists, Head#head.filename}})
    end.

%% -> {ok, Fd, fileheader()} | throw(Error)
read_file_header(Fd, FileName) ->
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, 0, ?HEADSZ),
    [Freelist, Cookie, CP, Type2, Version, M, Next, Kp, NoObjects, N] = 
	bin2ints(Bin),
    {ok, EOF} = dets_utils:position_close(Fd, FileName, eof),
    {ok, <<FileSize:32>>} = dets_utils:pread_close(Fd, FileName, EOF-4, 4),
    FH = #fileheader{freelist = Freelist,
                     fl_base = ?BASE,
		     cookie = Cookie,
		     closed_properly = CP,
		     type = dets_utils:code_to_type(Type2),
		     version = Version,
		     m = M,
		     next = Next,
		     keypos = Kp,
		     no_objects = NoObjects,
		     min_no_slots = ?DEFAULT_MIN_NO_SLOTS,
		     max_no_slots = ?DEFAULT_MAX_NO_SLOTS,
		     trailer = FileSize,
		     eof = EOF,
		     n = N,
		     mod = ?MODULE},
    {ok, Fd, FH}.

%% -> {ok, head(), ExtraInfo} | {error, Reason} (Reason lacking file name)
%% ExtraInfo = {convert_freelist, Version} | true | need_compacting 
check_file_header(FH, Fd) ->
    Test = 
	if
	    FH#fileheader.cookie =/= ?MAGIC ->
		{error, not_a_dets_file};
	    FH#fileheader.type =:= badtype ->
		{error, invalid_type_code};
	    FH#fileheader.version =/= ?FILE_FORMAT_VERSION -> 
		case lists:member(FH#fileheader.version,
                                  ?CAN_BUMP_BY_REPAIR) of
		    true ->
			{error, version_bump};
		    false ->
			{error, bad_version}
		end;
	    FH#fileheader.trailer =/= FH#fileheader.eof ->
		{error, not_closed};
	    FH#fileheader.closed_properly =:= ?CLOSED_PROPERLY ->
		case lists:member(FH#fileheader.version,
				  ?CAN_CONVERT_FREELIST) of
		    true ->
			{ok, {convert_freelist, FH#fileheader.version}, hash};
		    false ->
			{error, not_closed} % should not happen
		end;
	    FH#fileheader.closed_properly =:= ?CLOSED_PROPERLY2 ->
		{ok, true, hash};
	    FH#fileheader.closed_properly =:= 
	          ?CLOSED_PROPERLY2_NEED_COMPACTING  ->
		{ok, need_compacting, hash};
	    FH#fileheader.closed_properly =:= ?CLOSED_PROPERLY_NEW_HASH ->
		{ok, true, phash};
	    FH#fileheader.closed_properly =:= 
	         ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING  ->
		{ok, need_compacting, phash};
	    FH#fileheader.closed_properly =:= ?NOT_PROPERLY_CLOSED ->
		{error, not_closed};
	    FH#fileheader.closed_properly > 
	         ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING ->
		{error, not_closed};
	    true ->
		{error, not_a_dets_file}
	end,
    case Test of
	{ok, ExtraInfo, HashAlg} ->
	    H = #head{
	      m = FH#fileheader.m,
	      m2 = FH#fileheader.m * 2,
	      next = FH#fileheader.next,
	      fptr = Fd,
	      no_objects= FH#fileheader.no_objects,
	      n = FH#fileheader.n,
	      type = FH#fileheader.type,
	      update_mode = saved,
	      auto_save = infinity,             % not saved on file
	      fixed = false,			% not saved on file
	      freelists_p = FH#fileheader.freelist,
	      hash_bif = HashAlg,
	      keypos = FH#fileheader.keypos,
	      min_no_slots = FH#fileheader.min_no_slots,
	      max_no_slots = FH#fileheader.max_no_slots,
	      version = ?FILE_FORMAT_VERSION,
	      mod = ?MODULE,
	      bump = ?BUMP,
	      base = FH#fileheader.fl_base},
	    {ok, H, ExtraInfo};
	Error ->
	    Error
    end.

cache_segps(Fd, FileName, M) ->
    NSegs = no_segs(M),
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, ?HEADSZ, 4 * NSegs),
    Fun = fun(S, P) -> segp_cache(P, S), P+4 end,
    lists:foldl(Fun, ?HEADSZ, bin2ints(Bin)).

no_segs(NoSlots) ->
    ?SLOT2SEG(NoSlots - 1) + 1.

bin2ints(<<Int:32, B/binary>>) ->
    [Int | bin2ints(B)];
bin2ints(<<>>) ->
    [].

%%%
%%% Repair, conversion and initialization of a dets file.
%%%

bulk_input(Head, InitFun, Cntrs) ->
    bulk_input(Head, InitFun, Cntrs, make_ref()).

bulk_input(Head, InitFun, Cntrs, Ref) ->
    fun(close) ->
	    ok;
       (read) ->
	    case catch {Ref, InitFun(read)} of
		{Ref, end_of_input} ->
		    end_of_input;
		{Ref, {L0, NewInitFun}} when is_list(L0), 
                                             is_function(NewInitFun) ->
		    Kp = Head#head.keypos,
		    case catch bulk_objects(L0, Head, Cntrs, Kp, []) of
			{'EXIT', _Error} ->
			    _ = (catch NewInitFun(close)),
			    {error, invalid_objects_list};
			L ->
			    {L, bulk_input(Head, NewInitFun, Cntrs, Ref)}
		    end;
		{Ref, Value} ->
		    {error, {init_fun, Value}};
		Error ->
		    throw({thrown, Error})
	    end
    end.

bulk_objects([T | Ts], Head, Cntrs, Kp, L) ->
    BT = term_to_binary(T),
    Sz = byte_size(BT),
    LogSz = sz2pos(Sz+?OHDSZ),
    count_object(Cntrs, LogSz),
    Key = element(Kp, T),
    bulk_objects(Ts, Head, Cntrs, Kp, [make_object(Head, Key, LogSz, BT) | L]);
bulk_objects([], _Head, _Cntrs, _Kp, L) ->
    L.

-define(FSCK_SEGMENT, 10000).

-define(DCT(D, CT), [D | CT]).

-define(VNEW(N, E), erlang:make_tuple(N, E)).
-define(VSET(I, V, E), setelement(I, V, E)).
-define(VGET(I, V), element(I, V)).

%% OldVersion not used, assuming later versions have been converted already.
output_objs(OldVersion, Head, SlotNumbers, Cntrs) ->
    fun(close) ->
	    {ok, 0, Head};
       ([]) ->
	    output_objs(OldVersion, Head, SlotNumbers, Cntrs);
       (L) ->
	    %% Descending sizes.
	    Count = lists:sort(ets:tab2list(Cntrs)),
	    RCount = lists:reverse(Count),
	    NoObjects = lists:foldl(fun({_Sz,No}, A) -> A + No end, 0, Count),
	    {_, MinSlots, _} = SlotNumbers,
	    if
		%% Using number of objects for bags and duplicate bags
		%% is not ideal; number of (unique) keys should be
		%% used instead. The effect is that there will be more
		%% segments than "necessary".
		MinSlots =/= bulk_init,
		abs(?SLOT2SEG(NoObjects) - ?SLOT2SEG(MinSlots)) > 5,
		(NoObjects < ?MAXOBJS) ->
		    {try_again, NoObjects};
		true ->
                    Head1 = Head#head{no_objects = NoObjects},
		    SegSz = actual_seg_size(),
		    {_, End, _} = dets_utils:alloc(Head, SegSz-1),
		    %% Now {LogSize,NoObjects} in Cntrs is replaced by
		    %% {LogSize,Position,{FileName,FileDescriptor},NoObjects}.
		    {Head2, CT} = allocate_all_objects(Head1, RCount, Cntrs),
		    [E | Es] = bin2term(L, []),
		    {NE, Acc, DCT1} = 
			output_slots(E, Es, [E], Head2, ?DCT(0, CT)),
		    NDCT = write_all_sizes(DCT1, Cntrs),
		    Max = ets:info(Cntrs, size),
                    output_objs2(NE, Acc, Head2, Cntrs, NDCT, End, Max,Max)
	    end
    end.

output_objs2(E, Acc, Head, Cntrs, DCT, End, 0, MaxNoChunks) ->
    NDCT = write_all_sizes(DCT, Cntrs),
    output_objs2(E, Acc, Head, Cntrs, NDCT, End, MaxNoChunks, MaxNoChunks);
output_objs2(E, Acc, Head, Cntrs, DCT, End, ChunkI, MaxNoChunks) ->
    fun(close) ->
	    DCT1 = output_slot(Acc, Head, DCT),
	    NDCT = write_all_sizes(DCT1, Cntrs),
	    ?DCT(NoDups, CT) = NDCT,
	    [SegAddr | []] = ?VGET(tuple_size(CT), CT),
            FinalZ = End - SegAddr,
            [{?FSCK_SEGMENT, _, {FileName, Fd}, _}] = 
		ets:lookup(Cntrs, ?FSCK_SEGMENT),
	    ok = dets_utils:fwrite(Fd, FileName, 
				   dets_utils:make_zeros(FinalZ)),
            NewHead = Head#head{no_objects = Head#head.no_objects - NoDups},
	    {ok, NoDups, NewHead};
       (L) ->
	    Es = bin2term(L, []),
	    {NE, NAcc, NDCT} = output_slots(E, Es, Acc, Head, DCT),
	    output_objs2(NE, NAcc, Head, Cntrs, NDCT, End, 
			 ChunkI-1, MaxNoChunks)
    end.

%% By allocating bigger objects before smaller ones, holes in the
%% buddy system memory map are avoided. Unfortunately, the segments
%% are always allocated first, so if there are objects bigger than a
%% segment, there is a hole to handle. (Haven't considered placing the
%% segments among other objects of the same size.)
allocate_all_objects(Head, Count, Cntrs) ->
    SegSize = actual_seg_size(),
    {Head1, HSz, HN, HA} = alloc_hole(Count, Head, SegSize),
    {Max, _} = hd(Count),
    CT = ?VNEW(Max+1, not_used),
    {Head2, NCT} = allocate_all(Head1, Count, Cntrs, CT),
    Head3 = free_hole(Head2, HSz, HN, HA),
    {Head3, NCT}.

alloc_hole([{LSize,_} | _], Head, SegSz) when ?POW(LSize-1) > SegSz ->
    {_, SegAddr, _} = dets_utils:alloc(Head, SegSz-1),
    Size = ?POW(LSize-1)-1,
    {_, Addr, _} = dets_utils:alloc(Head, Size),
    N = (Addr - SegAddr) div SegSz,
    Head1 = dets_utils:alloc_many(Head, SegSz, N, SegAddr),
    {Head1, SegSz-1, N, SegAddr};
alloc_hole(_Count, Head, _SegSz) ->
    {Head, 0, 0, 0}.

free_hole(Head, _Size, 0, _Addr) ->
    Head;
free_hole(Head, Size, N, Addr) ->
    {Head1, _} = dets_utils:free(Head, Addr, Size),
    free_hole(Head1, Size, N-1, Addr+Size+1).

%% One (temporary) file for each buddy size, write all objects of that
%% size to the file.
allocate_all(Head, [{LSize,NoObjects} | Count], Cntrs, CT) ->
    Size = ?POW(LSize-1)-1,
    {_Head, Addr, _} = dets_utils:alloc(Head, Size),
    NewHead = dets_utils:alloc_many(Head, Size+1, NoObjects, Addr),
    {FileName, Fd} = temp_file(Head, LSize),
    true = ets:insert(Cntrs, {LSize, Addr, {FileName, Fd}, NoObjects}),
    NCT = ?VSET(LSize, CT, [Addr | []]),
    allocate_all(NewHead, Count, Cntrs, NCT);
allocate_all(Head, [], Cntrs, CT) ->
    %% Note that space for the segments has been allocated already.
    %% And one file for the segments...
    {FileName, Fd} = temp_file(Head, ?FSCK_SEGMENT),
    Addr = ?SEGADDR(?SEGARRSZ),
    true = ets:insert(Cntrs, {?FSCK_SEGMENT, Addr, {FileName, Fd}, 0}),
    NCT = ?VSET(tuple_size(CT), CT, [Addr | []]),
    {Head, NCT}.

temp_file(Head, N) ->
    TmpName = lists:concat([Head#head.filename, '.', N]),
    {ok, Fd} = dets_utils:open(TmpName, [raw, binary, write]),
    {TmpName, Fd}.

bin2term([<<Slot:32, LogSize:8, BinTerm/binary>> | BTs], L) ->
    bin2term(BTs, [{Slot, LogSize, BinTerm} | L]);
bin2term([], L) ->
    lists:reverse(L).

write_all_sizes(?DCT(D, CT), Cntrs) ->
    ?DCT(D, write_sizes(1, tuple_size(CT), CT, Cntrs)).

write_sizes(Sz, Sz, CT, Cntrs) ->
    write_size(Sz, ?FSCK_SEGMENT, CT, Cntrs);
write_sizes(Sz, MaxSz, CT, Cntrs) ->
    NCT = write_size(Sz, Sz, CT, Cntrs),
    write_sizes(Sz+1, MaxSz, NCT, Cntrs).

write_size(Sz, I, CT, Cntrs) ->
    case ?VGET(Sz, CT) of
	not_used ->
	    CT;
	[Addr | L] ->
	    {FileName, Fd} = ets:lookup_element(Cntrs, I, 3),
	    case file:write(Fd, lists:reverse(L)) of
		ok ->
		    ?VSET(Sz, CT, [Addr | []]);
		Error ->
		    dets_utils:file_error(FileName, Error)
	    end
    end.

output_slots(E, [E1 | Es], Acc, Head, DCT) 
                       when element(1, E) =:= element(1, E1) ->
    output_slots(E1, Es, [E1 | Acc], Head, DCT);
output_slots(_E, [E | L], Acc, Head, DCT) ->
    NDCT = output_slot(Acc, Head, DCT),
    output_slots(E, L, [E], Head, NDCT);
output_slots(E, [], Acc, _Head, DCT) ->
    {E, Acc, DCT}.

output_slot([E], _Head, ?DCT(D, CT)) ->
    ?DCT(D, output_slot([{foo, E}], 0, foo, CT));
output_slot(Es0, Head, ?DCT(D, CT)) ->
    Kp = Head#head.keypos,
    Fun = fun({_Slot, _LSize, BinTerm} = E) -> 
		  Key = element(Kp, binary_to_term(BinTerm)),
		  {Key, E}
	  end,
    Es = lists:map(Fun, Es0),
    NEs = case Head#head.type of
	      set ->
		  [{Key0,_} = E | L0] = lists:sort(Es),
		  choose_one(lists:sort(L0), Key0, [E]);
	      bag -> 
		  lists:usort(Es);
	      duplicate_bag -> 
		  lists:sort(Es)
	  end,
    Dups = D + length(Es) - length(NEs),
    ?DCT(Dups, output_slot(NEs, 0, foo, CT)).

choose_one([{Key,_} | Es], Key, L) ->
    choose_one(Es, Key, L);
choose_one([{Key,_} = E | Es], _Key, L) ->
    choose_one(Es, Key, [E | L]);
choose_one([], _Key, L) ->
    L.

output_slot([E | Es], Next, _Slot, CT) ->
    {_Key, {Slot, LSize, BinTerm}} = E,
    Size = byte_size(BinTerm),
    Size2 = ?POW(LSize-1),
    Pad = <<0:(Size2-Size-?OHDSZ)/unit:8>>,
    BinObject = [<<Next:32, Size:32, ?ACTIVE:32>>, BinTerm | Pad],
    [Addr | L] = ?VGET(LSize, CT),
    NCT = ?VSET(LSize, CT, [Addr+Size2 | [BinObject | L]]),
    output_slot(Es, Addr, Slot, NCT);
output_slot([], Next, Slot, CT) ->
    I = tuple_size(CT),
    [Addr | L] = ?VGET(I, CT),
    {Pos, _} = slot_position(Slot),
    NoZeros = Pos - Addr,
    BinObject = if 
		    NoZeros > 100 ->
			[dets_utils:make_zeros(NoZeros) | <<Next:32>>];
		    true ->
			<<0:NoZeros/unit:8,Next:32>>
   	        end,
    Size = NoZeros+4,
    ?VSET(I, CT, [Addr+Size | [BinObject | L]]).

%% Does not close Fd.
fsck_input(Head, Fd, Cntrs, _FileHeader) ->
    %% The file is not compressed, so the object size cannot exceed
    %% the filesize, for all objects.
    MaxSz = case file:position(Fd, eof) of
                {ok, Pos} ->
                    Pos;
                _ ->
                    (1 bsl 32) - 1
            end,
    State0 = fsck_read(?BASE, Fd, []),
    fsck_input1(Head, State0, Fd, MaxSz, Cntrs).

fsck_input1(Head, State, Fd, MaxSz, Cntrs) ->
    fun(close) ->
	    ok;
       (read) ->
	    case State of
		done ->
		    end_of_input;
		{done, L} ->
		    R = count_input(Cntrs, L, []),
		    {R, fsck_input1(Head, done, Fd, MaxSz, Cntrs)};
		{cont, L, Bin, Pos} ->
		    R = count_input(Cntrs, L, []),
                    FR = fsck_objs(Bin, Head#head.keypos, Head, []),
		    NewState = fsck_read(FR, Pos, Fd, MaxSz, Head),
		    {R, fsck_input1(Head, NewState, Fd, MaxSz, Cntrs)}
	    end
    end.

%% The ets table Cntrs is used for counting objects per size.
count_input(Cntrs, [[LogSz | B] | Ts], L) ->
    count_object(Cntrs, LogSz),
    count_input(Cntrs, Ts, [B | L]);
count_input(_Cntrs, [], L) ->
    L.

count_object(Cntrs, LogSz) ->
    case catch ets:update_counter(Cntrs, LogSz, 1) of
	N when is_integer(N) -> ok;
	_Badarg -> true = ets:insert(Cntrs, {LogSz, 1})
    end.

fsck_read(Pos, F, L) ->
    case file:position(F, Pos) of
	{ok, _} ->
	    read_more_bytes(<<>>, 0, Pos, F, L);
	_Error ->
	    {done, L}
    end.

fsck_read({more, Bin, Sz, L}, Pos, F, MaxSz, Head) when Sz > MaxSz ->
    FR = skip_bytes(Bin, ?BUMP, Head#head.keypos, Head, L),
    fsck_read(FR, Pos, F, MaxSz, Head);
fsck_read({more, Bin, Sz, L}, Pos, F, _MaxSz, _Head) ->
    read_more_bytes(Bin, Sz, Pos, F, L);
fsck_read({new, Skip, L}, Pos, F, _MaxSz, _Head) ->
    NewPos = Pos + Skip,
    fsck_read(NewPos, F, L).

read_more_bytes(B, Min, Pos, F, L) ->
    Max = if 
	      Min < ?CHUNK_SIZE -> ?CHUNK_SIZE; 
	      true -> Min 
	  end,
    case dets_utils:read_n(F, Max) of
	eof ->
	    {done, L};
	Bin ->
	    NewPos = Pos + byte_size(Bin),
	    {cont, L, list_to_binary([B, Bin]), NewPos}
    end.

fsck_objs(Bin = <<_N:32, Sz:32, Status:32, Tail/binary>>, Kp, Head, L) ->
    if 
	Status =:= ?ACTIVE ->
	    case Tail of
		<<BinTerm:Sz/binary, Tail2/binary>> ->
		    case catch element(Kp, binary_to_term(BinTerm)) of
			{'EXIT', _} ->
			    skip_bytes(Bin, ?BUMP, Kp, Head, L);
			Key ->
			    LogSz = sz2pos(Sz+?OHDSZ),
			    Obj = make_object(Head, Key, LogSz, BinTerm),
			    NL = [[LogSz | Obj] | L],
			    Skip = ?POW(LogSz-1) - Sz - ?OHDSZ,
			    skip_bytes(Tail2, Skip, Kp, Head, NL)
		    end;
		_ ->
                    {more, Bin, Sz, L}
	    end;
	true -> 
	    skip_bytes(Bin, ?BUMP, Kp, Head, L)
    end;
fsck_objs(Bin, _Kp, _Head, L) ->
    {more, Bin, 0, L}.
    
%% Version 8 has to know about version 9.
make_object(Head, Key, _LogSz, BT) when Head#head.version =:= 9 ->
    Slot = dets_v9:db_hash(Key, Head),
    <<Slot:32, BT/binary>>;
make_object(Head, Key, LogSz, BT) ->
    Slot = db_hash(Key, Head),
    <<Slot:32, LogSz:8, BT/binary>>.

%% Inlined.
skip_bytes(Bin, Skip, Kp, Head, L) ->
    case Bin of
	<<_:Skip/binary, Tail/binary>> ->
	    fsck_objs(Tail, Kp, Head, L);
	_ ->
            {new, Skip - byte_size(Bin), L}
    end.

%% -> {NewHead, ok} | throw({Head, Error})
do_perform_save(H) ->
    FL = dets_utils:get_freelists(H),
    B = term_to_binary(FL),
    Size = byte_size(B),
    ?DEBUGF("size of freelist = ~p~n", [Size]),
    ?DEBUGF("head.m = ~p~n", [H#head.m]),
    ?DEBUGF("head.no_objects = ~p~n", [H#head.no_objects]),

    {ok, Pos} = dets_utils:position(H, eof),
    H1 = H#head{freelists_p = Pos},
    W1 = {?FREELIST_POS, <<Pos:32>>},
    W2 = {Pos, [<<0:32, Size:32, ?FREE:32>>, B]},
    
    W3 = {?D_POS, <<(H1#head.m):32, 
	            (H1#head.next):32, 
	            (H1#head.keypos):32,
	            (H1#head.no_objects):32,
		    (H1#head.n):32>>},
    {ClosedProperly, ClosedProperlyNeedCompacitng} = 
	case H1#head.hash_bif of
	    hash ->
		{?CLOSED_PROPERLY2, ?CLOSED_PROPERLY2_NEED_COMPACTING};
	    phash ->
		{?CLOSED_PROPERLY_NEW_HASH, 
		 ?CLOSED_PROPERLY_NEW_HASH_NEED_COMPACTING}
	end,
    W4 = 
	if 
	    Size > 1000, Size > H1#head.no_objects ->
		{?CLOSED_PROPERLY_POS, 
		 <<ClosedProperlyNeedCompacitng:32>>};
	    true ->
		{?CLOSED_PROPERLY_POS, <<ClosedProperly:32>>}
	end,
    W5 = {?FILE_FORMAT_VERSION_POS, <<?FILE_FORMAT_VERSION:32>>},
    {H2, ok} = dets_utils:pwrite(H1, [W1,W2,W3,W4,W5]),
    {ok, Pos2} = dets_utils:position(H2, eof),
    ?DEBUGF("Writing file size ~p, eof at ~p~n", [Pos2+4, Pos2]),
    dets_utils:pwrite(H2, [{Pos2, <<(Pos2 + 4):32>>}]).

%% -> [term()] | throw({Head, Error})
slot_objs(H, Slot) when Slot >= H#head.next ->
    '$end_of_table';
slot_objs(H, Slot) ->
    {_Pos, Chain} = chain(H, Slot),
    collect_chain(H, Chain).

collect_chain(_H, 0) -> [];
collect_chain(H, Pos) ->
    {Next, _Sz, Term} = prterm(H, Pos, ?ReadAhead),
    [Term | collect_chain(H, Next)].

db_hash(Key, Head) ->
    H = h(Key, Head#head.hash_bif),
    Hash = H rem Head#head.m,
    if
	Hash < Head#head.n ->
	    H rem (Head#head.m2); % H rem (2 * m)
	true ->
	    Hash
    end.

h(I, phash) -> erlang:phash(I, ?BIG) - 1;
h(I, HF) -> erlang:HF(I, ?BIG) - 1. %% stupid BIF has 1 counts.

no_slots(_Head) ->
    undefined.

table_parameters(_Head) ->
    undefined.

%% Re-hashing a segment, starting with SlotStart.
%%
%% On the average, half of the objects of the chain are put into a new
%% chain. If the slot of the old chain is i, then the slot of the new
%% chain is i+m.
%% Note that the insertion of objects into the new chain is simplified
%% by the fact that the chains are not sorted on key, which means that
%% each moved object can be inserted first in the new chain.
%% (It is also a fact that the objects with the same key are not sorted.)
%%
%% -> {ok, Writes} | throw({Head, Error})
re_hash(Head, SlotStart) ->
    {SlotPos, _4} = slot_position(SlotStart),
    {ok, Bin} = dets_utils:pread(Head, SlotPos, 4*?SEGSZ, 0),
    {Read, Cs} = split_bin(SlotPos, Bin, [], []),
    re_hash_read(Head, [], Read, Cs).

split_bin(Pos, <<P:32, B/binary>>, R, Cs) ->
    if
	P =:= 0 ->
	    split_bin(Pos+4, B, R, Cs);
	true ->
	    split_bin(Pos+4, B, [{P,?ReadAhead} | R], [[Pos] | Cs])
    end;
split_bin(_Pos, <<>>, R, Cs) ->
    {R, Cs}.

re_hash_read(Head, Cs, R, RCs) ->
    {ok, Bins} = dets_utils:pread(R, Head),
    re_hash_read(Head, R, RCs, Bins, Cs, [], []).

re_hash_read(Head, [{Pos, Size} | Ps], [C | Cs], 
	     [<<Next:32, Sz:32, _Status:32, Bin0/binary>> | Bins], 
	     DoneCs, R, RCs) ->
    case byte_size(Bin0) of
	BinSz when BinSz >= Sz ->
	    case catch binary_to_term(Bin0) of
		{'EXIT', _Error} ->
		    throw(dets_utils:corrupt_reason(Head, bad_object));
		Term ->
		    Key = element(Head#head.keypos, Term),
		    New = h(Key, Head#head.hash_bif) rem Head#head.m2,
		    NC = case New >= Head#head.m of
			     true -> [{Pos,New} | C];
			     false -> [Pos | C]
			 end,
		    if
			Next =:= 0 ->
			    NDoneCs = [NC | DoneCs], 
			    re_hash_read(Head, Ps, Cs, Bins, NDoneCs, R, RCs);
			true ->
			    NR = [{Next,?ReadAhead} | R],
			    NRCs = [NC | RCs],
			    re_hash_read(Head, Ps, Cs, Bins, DoneCs, NR, NRCs)
		    end
	    end;
	BinSz when Size =:= BinSz+?OHDSZ ->
	    NR = [{Pos, Sz+?OHDSZ} | R],
	    re_hash_read(Head, Ps, Cs, Bins, DoneCs, NR, [C | RCs]);
	_BinSz ->
	    throw({Head, {error, {premature_eof, Head#head.filename}}})
    end;
re_hash_read(Head, [], [], [], Cs, [], []) ->
    re_hash_traverse_chains(Cs, Head, [], [], []);
re_hash_read(Head, [], [], [], Cs, R, RCs) ->
    re_hash_read(Head, Cs, R, RCs).

re_hash_traverse_chains([C | Cs], Head, Rs, Ns, Ws) ->
    case re_hash_find_new(C, Rs, start, start) of
	false ->
	    re_hash_traverse_chains(Cs, Head, Rs, Ns, Ws);
	{NRs, FirstNew, LastNew} -> 
	    LastInNew = case C of
			    [{_,_} | _] -> true;
			    _ -> false
			end,
	    N = {FirstNew, LastNew, LastInNew},
	    NWs = re_hash_link(C, start, start, start, Ws),
	    re_hash_traverse_chains(Cs, Head, NRs, [N | Ns], NWs)
    end;
re_hash_traverse_chains([], Head, Rs, Ns, Ws) ->
    {ok, Bins} = dets_utils:pread(Rs, Head),
    {ok, insert_new(Rs, Bins, Ns, Ws)}.

re_hash_find_new([{Pos,NewSlot} | C], R, start, start) ->
    {SPos, _4} = slot_position(NewSlot),
    re_hash_find_new(C, [{SPos,4} | R], Pos, Pos);
re_hash_find_new([{Pos,_SPos} | C], R, _FirstNew, LastNew) ->
    re_hash_find_new(C, R, Pos, LastNew);
re_hash_find_new([_Pos | C], R, FirstNew, LastNew) ->
    re_hash_find_new(C, R, FirstNew, LastNew);
re_hash_find_new([], _R, start, start) ->
    false;
re_hash_find_new([], R, FirstNew, LastNew) ->
    {R, FirstNew, LastNew}.

re_hash_link([{Pos,_SPos} | C], LastOld, start, _LastInNew, Ws) ->
    re_hash_link(C, LastOld, Pos, true, Ws);
re_hash_link([{Pos,_SPos} | C], LastOld, LastNew, false, Ws) ->
    re_hash_link(C, LastOld, Pos, true, [{Pos,<<LastNew:32>>} | Ws]);
re_hash_link([{Pos,_SPos} | C], LastOld, _LastNew, LastInNew, Ws) ->
    re_hash_link(C, LastOld, Pos, LastInNew, Ws);
re_hash_link([Pos | C], start, LastNew, true, Ws) ->
    re_hash_link(C, Pos, LastNew, false, [{Pos,<<0:32>>} | Ws]);
re_hash_link([Pos | C], LastOld, LastNew, true, Ws) ->
    re_hash_link(C, Pos, LastNew, false, [{Pos,<<LastOld:32>>} | Ws]);
re_hash_link([Pos | C], _LastOld, LastNew, LastInNew, Ws) ->
    re_hash_link(C, Pos, LastNew, LastInNew, Ws);
re_hash_link([], _LastOld, _LastNew, _LastInNew, Ws) ->
    Ws.

insert_new([{NewSlotPos,_4} | Rs], [<<P:32>> = PB | Bins], [N | Ns], Ws) ->
    {FirstNew, LastNew, LastInNew} = N,
    Ws1 = case P of
	      0 when LastInNew ->
		  Ws;
	      0 ->
		  [{LastNew, <<0:32>>} | Ws];
	      _ ->
		  [{LastNew, PB} | Ws]
	  end,
    NWs = [{NewSlotPos, <<FirstNew:32>>} | Ws1],
    insert_new(Rs, Bins, Ns, NWs);
insert_new([], [], [], Ws) ->
    Ws.

%% When writing the cache, a 'work list' is first created:
%%   WorkList = [{Key, {Delete,Lookup,[Inserted]}}]
%%   Delete = keep | delete
%%   Lookup = skip | lookup
%%   Inserted = {object(), No}
%%   No = integer()
%% If No =< 0 then there will be -No instances of object() on the file
%% when the cache has been written. If No > 0 then No instances of
%% object() will be added to the file.
%% If Delete has the value 'delete', then all objects with the key Key
%% have been deleted. (This could be viewed as a shorthand for {Object,0}
%% for each object Object on the file not mentioned in some Inserted.)
%% If Lookup has the value 'lookup', all objects with the key Key will
%% be returned.
%%

%% -> {NewHead, [LookedUpObject], pwrite_list()} | throw({NewHead, Error})
write_cache(Head) ->
    #head{cache = C, type = Type} = Head,
    case dets_utils:is_empty_cache(C) of
	true -> {Head, [], []};
	false ->
	    {NewC, _MaxInserts, PerKey} = dets_utils:reset_cache(C),
	    %% NoInsertedKeys is an upper limit on the number of new keys.
	    {WL, NoInsertedKeys} = make_wl(PerKey, Type),
	    Head1 = Head#head{cache = NewC},
	    case may_grow(Head1, NoInsertedKeys, once) of
		{Head2, ok} ->
		    eval_work_list(Head2, WL);
		HeadError ->
		    throw(HeadError)
	    end
    end.

make_wl(PerKey, Type) ->
    make_wl(PerKey, Type, [], 0).

make_wl([{Key,L} | PerKey], Type, WL, Ins) ->
    [Cs | I] = wl(L, Type),
    make_wl(PerKey, Type, [{Key,Cs} | WL], Ins+I);
make_wl([], _Type, WL, Ins) ->
    {WL, Ins}.

wl(L, Type) ->
    wl(L, Type, keep, skip, 0, []).

wl([{_Seq, delete_key} | Cs], Type, _Del, Lookup, _I, _Objs) ->
    wl(Cs, Type, delete, Lookup, 0, []);
wl([{_Seq, {delete_object, Object}} | Cs], Type, Del, Lookup, I, Objs) ->
    NObjs = lists:keydelete(Object, 1, Objs),
    wl(Cs, Type, Del, Lookup, I, [{Object,0} | NObjs]);
wl([{_Seq, {insert, Object}} | Cs], Type, _Del, Lookup, _I, _Objs) 
                    when Type =:= set ->
    wl(Cs, Type, delete, Lookup, 1, [{Object,-1}]);
wl([{_Seq, {insert, Object}} | Cs], Type, Del, Lookup, _I, Objs) ->
    NObjs = 
	case lists:keyfind(Object, 1, Objs) of
	    {_, 0} ->
		lists:keyreplace(Object, 1, Objs, {Object,-1});
	    {_, _C} when Type =:= bag -> % C =:= 1; C =:= -1
		Objs;
	    {_, C} when C < 0 -> % when Type =:= duplicate_bag
		lists:keyreplace(Object, 1, Objs, {Object,C-1});
	    {_, C} -> % when C > 0, Type =:= duplicate_bag
		lists:keyreplace(Object, 1, Objs, {Object,C+1});
	    false when Del =:= delete ->
		[{Object, -1} | Objs];
	    false ->
		[{Object, 1} | Objs]
	end,
    wl(Cs, Type, Del, Lookup, 1, NObjs);
wl([{_Seq, {lookup,_Pid}=Lookup} | Cs], Type, Del, _Lookup, I, Objs) ->
    wl(Cs, Type, Del, Lookup, I, Objs);
wl([], _Type, Del, Lookup, I, Objs) ->
    [{Del, Lookup, Objs} | I].

%% -> {NewHead, ok} | {NewHead, Error}
may_grow(Head, 0, once) ->
    {Head, ok};
may_grow(Head, _N, _How) when Head#head.fixed =/= false ->
    {Head, ok};
may_grow(#head{access = read}=Head, _N, _How) ->
    {Head, ok};
may_grow(Head, _N, _How) when Head#head.next >= ?MAXOBJS ->
    {Head, ok};
may_grow(Head, N, How) ->
    Extra = erlang:min(2*?SEGSZ, Head#head.no_objects + N - Head#head.next),
    case catch may_grow1(Head, Extra, How) of
	{error, Reason} -> % alloc may throw error
	    {Head, {error, Reason}};
	Reply ->
	    Reply
    end.

may_grow1(Head, Extra, many_times) when Extra > ?SEGSZ ->
    Reply = grow(Head, 1, undefined),
    self() ! ?DETS_CALL(self(), may_grow),
    Reply;
may_grow1(Head, Extra, _How) ->    
    grow(Head, Extra, undefined).

%% -> {Head, ok} | throw({Head, Error})
grow(Head, Extra, _SegZero) when Extra =< 0 ->
    {Head, ok};
grow(Head, Extra, undefined) ->
    grow(Head, Extra, seg_zero());
grow(Head, Extra, SegZero) ->
    #head{n = N, next = Next, m = M} = Head,
    SegNum = ?SLOT2SEG(Next),
    {Head0, Ws1} = allocate_segment(Head, SegZero, SegNum),
    {Head1, ok} = dets_utils:pwrite(Head0, Ws1),
    %% If re_hash fails, segp_cache has been called, but it does not matter.
    {ok, Ws2} = re_hash(Head1, N),
    {Head2, ok} = dets_utils:pwrite(Head1, Ws2),
    NewHead =
	if 
	    N + ?SEGSZ =:= M ->
		Head2#head{n = 0, next = Next + ?SEGSZ, m = 2 * M, m2 = 4 * M};
	    true ->
		Head2#head{n = N + ?SEGSZ, next = Next + ?SEGSZ}
	end,
    grow(NewHead, Extra - ?SEGSZ, SegZero).

seg_zero() ->
    <<0:(4*?SEGSZ)/unit:8>>.

find_object(Head, Object) ->
    Key = element(Head#head.keypos, Object),
    Slot = db_hash(Key, Head),
    find_object(Head, Object, Slot).    

find_object(H, _Obj, Slot) when Slot >= H#head.next ->
    false;
find_object(H, Obj, Slot) ->
    {_Pos, Chain} = chain(H, Slot),
    case catch find_obj(H, Obj, Chain) of
	{ok, Pos} ->
	    {ok, Pos};
	_Else ->
	    false
    end.

find_obj(H, Obj, Pos) when Pos > 0 ->
    {Next, _Sz, Term} = prterm(H, Pos, ?ReadAhead),
    if 
	Term == Obj ->
	    {ok, Pos};
	true ->
	    find_obj(H, Obj, Next)
    end.

%% Given, a slot, return the {Pos, Chain} in the file where the
%% objects hashed to this slot reside. Pos is the position in the
%% file where the chain pointer is written and Chain is the position
%% in the file where the first object resides.
chain(Head, Slot) ->
    Pos = ?SEGADDR(?SLOT2SEG(Slot)),
    Segment = get_segp(Pos),
    FinalPos = Segment + (4 * ?REM2(Slot, ?SEGSZ)),
    {ok, <<Chain:32>>} = dets_utils:pread(Head, FinalPos, 4, 0),
    {FinalPos, Chain}.

%%%
%%% Cache routines depending on the dets file format.
%%%

%% -> {Head, [LookedUpObject], pwrite_list()} | throw({Head, Error})
eval_work_list(Head, WorkLists) ->
    SWLs = tag_with_slot(WorkLists, Head, []),
    P1 = dets_utils:family(SWLs), 
    {PerSlot, SlotPositions} = remove_slot_tag(P1, [], []),
    {ok, Bins} = dets_utils:pread(SlotPositions, Head),
    first_object(PerSlot, SlotPositions, Bins, Head, [], [], [], []).

tag_with_slot([{K,_} = WL | WLs], Head, L) ->
    tag_with_slot(WLs, Head, [{db_hash(K, Head), WL} | L]);
tag_with_slot([], _Head, L) ->
    L.

remove_slot_tag([{S,SWLs} | SSWLs], Ls, SPs) ->
    remove_slot_tag(SSWLs, [SWLs | Ls], [slot_position(S) | SPs]);
remove_slot_tag([], Ls, SPs) ->
    {Ls, SPs}.

%% The initial chain pointers and the first object in each chain are
%% read "in parallel", that is, with one call to file:pread/2 (two
%% calls altogether). The following chain objects are read one by
%% one. This is a compromise: if the chains are long and threads are
%% active, it would be faster to keep a state for each chain and read
%% the objects of the chains in parallel, but the overhead would be
%% quite substantial.

first_object([WorkLists | SPs], [{P1,_4} | Ss], [<<P2:32>> | Bs], Head,
	      ObjsToRead, ToRead, Ls, LU) when P2 =:= 0 ->
    L0 = [{old,P1}],
    {L, NLU} = eval_slot(Head, ?ReadAhead, P2, WorkLists, L0, LU),
    first_object(SPs, Ss, Bs, Head, ObjsToRead, ToRead, [L | Ls], NLU);
first_object([WorkLists | SPs], [{P1,_4} | Ss], [<<P2:32>> | Bs], Head, 
	      ObjsToRead, ToRead, Ls, LU) ->
    E = {P1,P2,WorkLists},
    first_object(SPs, Ss, Bs, Head, 
		 [E | ObjsToRead], [{P2, ?ReadAhead} | ToRead], Ls, LU);
first_object([], [], [], Head, ObjsToRead, ToRead, Ls, LU) ->
    {ok, Bins} = dets_utils:pread(ToRead, Head),
    case catch eval_first(Bins, ObjsToRead, Head, Ls, LU) of
	{ok, NLs, NLU} -> 
	    case create_writes(NLs, Head, [], 0) of
		{Head1, [], 0} ->
		    {Head1, NLU, []};
		{Head1, Ws, No} ->
		    {NewHead, Ws2} = update_no_objects(Head1, Ws, No),
		    {NewHead, NLU, Ws2}
	    end;
	_Error -> 
	    throw(dets_utils:corrupt_reason(Head, bad_object))
    end.

%% Update no_objects on the file too, if the number of segments that
%% dets:fsck/6 use for estimate has changed.
update_no_objects(Head, Ws, 0) -> {Head, Ws};
update_no_objects(Head, Ws, Delta) ->
    No = Head#head.no_objects,
    NewNo = No + Delta,
    NWs = 
	if 
	    NewNo > ?MAXOBJS ->
		Ws;
	    ?SLOT2SEG(No) =:= ?SLOT2SEG(NewNo) ->
		Ws;
	    true ->
		[{?NO_OBJECTS_POS, <<NewNo:32>>} | Ws]
	end,
    {Head#head{no_objects = NewNo}, NWs}.

eval_first([<<Next:32, Sz:32, _Status:32, Bin/binary>> | Bins], 
	   [SP | SPs], Head, Ls, LU) ->
    {P1, P2, WLs} = SP,
    L0 = [{old,P1}],
    case byte_size(Bin) of
	BinSz when BinSz >= Sz ->
	    Term = binary_to_term(Bin),
	    Key = element(Head#head.keypos, Term),
	    {L, NLU} = find_key(Head, P2, Next, Sz, Term, Key, WLs, L0, LU),
	    eval_first(Bins, SPs, Head, [L | Ls], NLU);
	_BinSz ->
	    {L, NLU} = eval_slot(Head, Sz+?OHDSZ, P2, WLs, L0, LU),
	    eval_first(Bins, SPs, Head, [L | Ls], NLU)
    end;
eval_first([], [], _Head, Ls, LU) ->
    {ok, Ls, LU}.

eval_slot(_Head, _TrySize, _Pos=0, [], L, LU) ->
    {L, LU};
eval_slot(Head, _TrySize, Pos=0, [WL | WLs], L, LU) ->
    {_Key, {_Delete, LookUp, Objects}} = WL,
    {NL, NLU} = end_of_key(Objects, LookUp, L, []),
    eval_slot(Head, ?ReadAhead, Pos, WLs, NL, NLU++LU);
eval_slot(Head, TrySize, Pos, WLs, L, LU) ->
    {NextPos, Size, Term} = prterm(Head, Pos, TrySize),
    Key = element(Head#head.keypos, Term),
    find_key(Head, Pos, NextPos, Size, Term, Key, WLs, L, LU).

find_key(Head, Pos, NextPos, Size, Term, Key, WLs, L, LU) ->
    case lists:keyfind(Key, 1, WLs) of
	{_, {Delete, LookUp, Objects}} = WL ->
	    NWLs = lists:delete(WL, WLs),
	    {NewObjects, NL, LUK} = eval_object(Size, Term, Delete, LookUp, 
						Objects, Head, Pos, L, []),
	    eval_key(Key, Delete, LookUp, NewObjects, Head, NextPos, 
		     NWLs, NL, LU, LUK);
	false ->
	    L0 = [{old,Pos} | L],
	    eval_slot(Head, ?ReadAhead, NextPos, WLs, L0, LU)
    end.

eval_key(_Key, _Delete, Lookup, _Objects, Head, Pos, WLs, L, LU, LUK) 
                            when Head#head.type =:= set ->
    NLU = case Lookup of
	      {lookup, Pid} -> [{Pid,LUK} | LU];
	      skip -> LU
	  end,
    eval_slot(Head, ?ReadAhead, Pos, WLs, L, NLU);
eval_key(_Key, _Delete, LookUp, Objects, Head, Pos, WLs, L, LU, LUK) 
                                                          when Pos =:= 0 ->
    {NL, NLU} = end_of_key(Objects, LookUp, L, LUK),
    eval_slot(Head, ?ReadAhead, Pos, WLs, NL, NLU++LU);
eval_key(Key, Delete, LookUp, Objects, Head, Pos, WLs, L, LU, LUK) ->
    {NextPos, Size, Term} = prterm(Head, Pos, ?ReadAhead),
    case element(Head#head.keypos, Term) of
	Key ->
	    {NewObjects, NL, LUK1} = 
		eval_object(Size, Term, Delete, LookUp,Objects,Head,Pos,L,LUK),
	    eval_key(Key, Delete, LookUp, NewObjects, Head, NextPos, WLs, 
		     NL, LU, LUK1);
	Key2 ->
	    {L1, NLU} = end_of_key(Objects, LookUp, L, LUK),
	    find_key(Head, Pos, NextPos, Size, Term, Key2, WLs, L1, NLU++LU)
    end.

%% All objects in Objects have the key Key.
eval_object(Size, Term, Delete, LookUp, Objects, Head, Pos, L, LU) ->
    Type = Head#head.type,
    case lists:keyfind(Term, 1, Objects) of
	{_Object, N} when N =:= 0 ->
	    L1 = [{delete,Pos,Size} | L],
	    {Objects, L1, LU};
	{_Object, N} when N < 0, Type =:= set ->
	    L1 = [{old,Pos} | L],
	    wl_lookup(LookUp, Objects, Term, L1, LU);
	{Object, _N} when Type =:= bag -> % when N =:= 1; N =:= -1
	    L1 = [{old,Pos} | L],
	    Objects1 = lists:keydelete(Object, 1, Objects),
	    wl_lookup(LookUp, Objects1, Term, L1, LU);
	{Object, N} when N < 0, Type =:= duplicate_bag ->
	    L1 = [{old,Pos} | L],
	    Objects1 = lists:keyreplace(Object, 1, Objects, {Object,N+1}),
	    wl_lookup(LookUp, Objects1, Term, L1, LU);
	{_Object, N} when N > 0, Type =:= duplicate_bag ->
	    L1 = [{old,Pos} | L],
	    wl_lookup(LookUp, Objects, Term, L1, LU);
	false when Type =:= set, Delete =:= delete ->
	    case lists:keyfind(-1, 2, Objects) of
		false -> % no inserted object, perhaps deleted objects
		    L1 = [{delete,Pos,Size} | L],
		    {[], L1, LU};
		{Term2, -1} ->
		    Bin2 = term_to_binary(Term2),
		    NSize = byte_size(Bin2),
		    Overwrite = 
			if
			    NSize =:= Size ->
				true;
			    true ->
				SizePos = sz2pos(Size+?OHDSZ),
				NSizePos = sz2pos(NSize+?OHDSZ),
				SizePos =:= NSizePos
			end,
		    E = if 
			    Overwrite ->
				{overwrite,Bin2,Pos};
			    true ->
				{replace,Bin2,Pos,Size}
			end,
		    wl_lookup(LookUp, [], Term2, [E | L], LU)
	    end;
	false when Delete =:= delete ->
	    L1 = [{delete,Pos,Size} | L],
	    {Objects, L1, LU};
	false ->
	    L1 = [{old,Pos} | L],
	    wl_lookup(LookUp, Objects, Term, L1, LU)
    end.

%% Inlined.
wl_lookup({lookup,_}, Objects, Term, L, LU) ->
    {Objects, L, [Term | LU]};
wl_lookup(skip, Objects, _Term, L, LU) ->
    {Objects, L, LU}.

end_of_key([{Object,N0} | Objs], LookUp, L, LU) when N0 =/= 0 ->
    N = abs(N0),
    NL = [{insert,N,term_to_binary(Object)} | L],
    NLU = case LookUp of 
	      {lookup, _} ->
		  lists:duplicate(N, Object) ++ LU;
	      skip ->
		  LU
	  end,
    end_of_key(Objs, LookUp, NL, NLU);
end_of_key([_ | Objects], LookUp, L, LU) ->
    end_of_key(Objects, LookUp, L, LU);
end_of_key([], {lookup,Pid}, L, LU) ->
    {L, [{Pid,LU}]};
end_of_key([], skip, L, LU) ->
    {L, LU}.

create_writes([L | Ls], H, Ws, No) ->
    {NH, NWs, NNo} = create_writes(L, H, Ws, No, 0, true),
    create_writes(Ls, NH, NWs, NNo);
create_writes([], H, Ws, No) ->
    {H, lists:reverse(Ws), No}.

create_writes([{old,Pos} | L], H, Ws, No, _Next, true) ->
    create_writes(L, H, Ws, No, Pos, true);
create_writes([{old,Pos} | L], H, Ws, No, Next, false) ->
    W = {Pos, <<Next:32>>},
    create_writes(L, H, [W | Ws], No, Pos, true);
create_writes([{insert,N,Bin} | L], H, Ws, No, Next, _NextIsOld) ->
    {NH, NWs, Pos} = create_inserts(N, H, Ws, Next, byte_size(Bin), Bin),
    create_writes(L, NH, NWs, No+N, Pos, false);
create_writes([{overwrite,Bin,Pos} | L], H, Ws, No, Next, _) ->
    Size = byte_size(Bin),
    W = {Pos, [<<Next:32, Size:32, ?ACTIVE:32>>, Bin]},
    create_writes(L, H, [W | Ws], No, Pos, true);
create_writes([{replace,Bin,Pos,OSize} | L], H, Ws, No, Next, _) ->
    Size = byte_size(Bin),
    {H1, _} = dets_utils:free(H, Pos, OSize+?OHDSZ),
    {NH, NewPos, _} = dets_utils:alloc(H1, ?OHDSZ + Size),
    W1 = {NewPos, [<<Next:32, Size:32, ?ACTIVE:32>>, Bin]},
    NWs = if 
	      Pos =:= NewPos -> 
		  [W1 | Ws];
	      true -> 
		  W2 = {Pos+?STATUS_POS, <<?FREE:32>>},
		  [W1,W2 | Ws]
	  end,
    create_writes(L, NH, NWs, No, NewPos, false);
create_writes([{delete,Pos,Size} | L], H, Ws, No, Next, _) ->
    {NH, _} = dets_utils:free(H, Pos, Size+?OHDSZ),
    NWs = [{Pos+?STATUS_POS,<<?FREE:32>>} | Ws],
    create_writes(L, NH, NWs, No-1, Next, false);
create_writes([], H, Ws, No, _Next, _NextIsOld) ->
    {H, Ws, No}.

create_inserts(0, H, Ws, Next, _Size, _Bin) ->
    {H, Ws, Next};
create_inserts(N, H, Ws, Next, Size, Bin) ->
    {NH, Pos, _} = dets_utils:alloc(H, ?OHDSZ + Size),
    W = {Pos, [<<Next:32, Size:32, ?ACTIVE:32>>, Bin]},
    create_inserts(N-1, NH, [W | Ws], Pos, Size, Bin).

slot_position(S) ->
    Pos = ?SEGADDR(?SLOT2SEG(S)),
    Segment = get_segp(Pos),
    FinalPos = Segment + (4 * ?REM2(S, ?SEGSZ)),
    {FinalPos, 4}.

%% Twice the size of a segment due to the bug in sz2pos/1. Inlined.
actual_seg_size() ->
    ?POW(sz2pos(?SEGSZ*4)-1).

segp_cache(Pos, Segment) ->
    put(Pos, Segment).

%% Inlined.
get_segp(Pos) ->
    get(Pos).

%% Bug: If Sz0 is equal to 2**k for some k, then 2**(k+1) bytes are
%% allocated (wasting 2**k bytes).
sz2pos(N) ->
    1 + dets_utils:log2(N+1).

scan_objs(_Head, Bin, From, To, L, Ts, R, _Type) ->
    scan_objs(Bin, From, To, L, Ts, R).

scan_objs(Bin, From, To, L, Ts, -1) ->
    {stop, Bin, From, To, L, Ts};
scan_objs(B = <<_N:32, Sz:32, St:32, T/binary>>, From, To, L, Ts, R) ->
    if 
	St =:= ?ACTIVE;
	St =:= ?FREE -> % deleted after scanning started
	    case T of
		<<BinTerm:Sz/binary, T2/binary>> ->
		    NTs = [BinTerm | Ts],
		    OSz = Sz + ?OHDSZ,
		    Skip = ?POW(sz2pos(OSz)-1) - OSz,
		    F2 = From + OSz,
		    NR = if 
			     R < 0 ->
				 R + 1;
			     true ->
				 R + OSz + Skip
			 end,
		    scan_skip(T2, F2, To, Skip, L, NTs, NR);
		_ ->
                    {more, From, To, L, Ts, R, Sz+?OHDSZ}
	    end;
	true -> % a segment
	    scan_skip(B, From, To, actual_seg_size(), L, Ts, R)
    end;
scan_objs(_B, From, To, L, Ts, R) ->
    {more, From, To, L, Ts, R, 0}.

scan_skip(Bin, From, To, Skip, L, Ts, R) when From + Skip < To ->
    SkipPos = From + Skip,
    case Bin of
	<<_:Skip/binary, Tail/binary>> ->
	    scan_objs(Tail, SkipPos, To, L, Ts, R);
	_ ->
            {more, SkipPos, To, L, Ts, R, 0}
    end;
scan_skip(Bin, From, To, Skip, L, Ts, R) when From + Skip =:= To ->
    scan_next_allocated(Bin, From, To, L, Ts, R);
scan_skip(_Bin, From, _To, Skip, L, Ts, R) -> % when From + Skip > _To 
    From1 = From + Skip,
    {more, From1, From1, L, Ts, R, 0}.

scan_next_allocated(_Bin, _From, To, <<>>=L, Ts, R) ->
    {more, To, To, L, Ts, R, 0};
scan_next_allocated(Bin, From0, _To, <<From:32, To:32, L/binary>>, Ts, R) ->
    Skip = From - From0,
    scan_skip(Bin, From0, To, Skip, L, Ts, R).

%% Read term from file at position Pos
prterm(Head, Pos, ReadAhead) ->
    Res = dets_utils:pread(Head, Pos, ?OHDSZ, ReadAhead),
    ?DEBUGF("file:pread(~tp, ~p, ?) -> ~p~n", [Head#head.filename, Pos, Res]),
    {ok, <<Next:32, Sz:32, _Status:32, Bin0/binary>>} = Res,
    ?DEBUGF("{Next, Sz} = ~p~n", [{Next, Sz}]),
    Bin = case byte_size(Bin0) of
	      Actual when Actual >= Sz ->
		  Bin0;
	      _ ->
		  {ok, Bin1} = dets_utils:pread(Head, Pos +  ?OHDSZ, Sz, 0),
		  Bin1
	  end,
    Term = binary_to_term(Bin),
    {Next, Sz, Term}.

%%%%%%%%%%%%%%%%%  DEBUG functions %%%%%%%%%%%%%%%%

file_info(FH) ->
    #fileheader{closed_properly = CP, keypos = Kp,
                m = M, next = Next, n = N, version = Version,
                type = Type, no_objects = NoObjects} 
        = FH,
    if
        CP =:= 0 ->
            {error, not_closed};
        FH#fileheader.cookie =/= ?MAGIC ->
            {error, not_a_dets_file};
        FH#fileheader.version =/= ?FILE_FORMAT_VERSION ->
            {error, bad_version};
        true ->
            {ok, [{closed_properly,CP},{keypos,Kp},{m, M},
                  {n,N},{next,Next},{no_objects,NoObjects},
                  {type,Type},{version,Version}]}
    end.

v_segments(H) ->
    v_segments(H, 0).

v_segments(_H, ?SEGARRSZ) ->
    done;
v_segments(H, SegNo) ->
    Seg = dets_utils:read_4(H#head.fptr, ?SEGADDR(SegNo)),
    if
	Seg =:= 0 ->
	    done;
	true ->
	    io:format("SEGMENT ~w ", [SegNo]),
	    io:format("At position ~w~n", [Seg]),
	    v_segment(H, SegNo, Seg, 0),
	    v_segments(H, SegNo+1)
    end.

v_segment(_H, _, _SegPos, ?SEGSZ) ->
    done;
v_segment(H, SegNo, SegPos, SegSlot) ->
    Slot = SegSlot + (SegNo * ?SEGSZ),
    Chain = dets_utils:read_4(H#head.fptr, SegPos + (4 * SegSlot)),
    if 
	Chain =:= 0 ->  %% don't print empty chains
	    true;
	true ->
	    io:format("   <~p>~p: [",[SegPos + (4 * SegSlot), Slot]),
	    print_chain(H, Chain)
    end,
    v_segment(H, SegNo, SegPos, SegSlot+1).

print_chain(_H, 0) ->
    io:format("] \n", []);
print_chain(H, Pos) ->
    {ok, _} = file:position(H#head.fptr, Pos),
    case rterm(H#head.fptr) of
	{ok, 0, _Sz, Term} ->
	    io:format("<~p>~p] \n",[Pos, Term]);
	{ok, Next, _Sz, Term} ->
	    io:format("<~p>~p, ", [Pos, Term]),
	    print_chain(H, Next);
	Other ->
	    io:format("~nERROR ~p~n", [Other])
    end.

%% Can't be used at the bucket level!!!!
%% Only when we go down a chain
rterm(F) ->
    case catch rterm2(F) of
	{'EXIT', Reason} -> %% truncated DAT file 
	    dets_utils:vformat("** dets: Corrupt or truncated dets file~n", 
                               []), 
	    {error, Reason};
	Other -> 
	    Other
    end.

rterm2(F) ->
    {ok, <<Next:32, Sz:32, _:32>>} = file:read(F, ?OHDSZ),
    {ok, Bin} = file:read(F, Sz),
    Term = binary_to_term(Bin),
    {ok, Next, Sz, Term}.


