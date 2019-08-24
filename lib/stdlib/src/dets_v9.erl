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
-module(dets_v9).

%% Dets files, implementation part. This module handles version 9.
%% To be called from dets.erl only.

-export([mark_dirty/1, read_file_header/2,
         check_file_header/2, do_perform_save/1, initiate_file/11,
         prep_table_copy/9, init_freelist/1, fsck_input/4,
         bulk_input/3, output_objs/3, bchunk_init/2,
         try_bchunk_header/2, compact_init/3, read_bchunks/2,
         write_cache/1, may_grow/3, find_object/2, slot_objs/2,
         scan_objs/8, db_hash/2, no_slots/1, table_parameters/1]).

-export([file_info/1, v_segments/1]).

-export([cache_segps/3]).

-dialyzer(no_improper_lists).

-compile({inline, [{max_objsize,1},{maxobjsize,1}]}).
-compile({inline, [{write_segment_file,6}]}). 
-compile({inline, [{sz2pos,1},{adjsz,1}]}).
-compile({inline, [{skip_bytes,6},{make_object,4}]}).
-compile({inline, [{segp_cache,2},{get_segp,1},{get_arrpart,1}]}).
-compile({inline, [{h,2}]}).

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
%%    4      NoKeys
%%    4      MinNoSlots
%%    4      MaxNoSlots
%%    4      HashMethod
%%    4      N
%%  ---
%%    256    Version 9(a): Reserved for future versions. Initially zeros.
%%           Version 9(b) has instead:
%%    112    28 counters for the buddy system sizes 2^4 to 2^31.
%%    144    Reserved for future versions. Initially zeros.
%%           Version 9(c) has instead:
%%    112    28 counters for the buddy system sizes (as for 9(b)).
%%    16     MD5-sum for the 44 plus 112 bytes before the MD5-sum.
%%           (FreelistsPointer, Cookie and ClosedProperly are not digested.)
%%    128    Reserved for future versions. Initially zeros.
%%           Version 9(d), introduced in R15A, has instead:
%%    112    28 counters for the buddy system sizes (as for 9(b)).
%%    16     MD5-sum for the 44 plus 112 bytes before the MD5-sum.
%%           (FreelistsPointer, Cookie and ClosedProperly are not digested.)
%%    4      Base of the buddy system.
%%           0 (zero) if the base is equal to ?BASE. Compatible with R14B.
%%           File size at the end of the file is RealFileSize - Base.
%%           The reason for modifying file size is that when a file created
%%           by R15 is read by R14 a repair takes place immediately, which
%%           is acceptable when downgrading.
%%    124    Reserved for future versions. Initially zeros.
%%  ---
%%  ------------------ end of file header
%%    4*256  SegmentArray Pointers.
%%  ------------------ This is BASE.
%%    4*512  SegmentArray Part 1
%%    ...    More SegmentArray Parts
%%    8*256  First segment
%%    ???    Objects (free and alive)
%%    4*512  Further SegmentArray Part.
%%    ???    Objects (free and alive)
%%    8*256  Further segment.
%%    ???    Objects (free and alive)
%%    ... more objects, segment array parts, and segments ...
%%  -----------------------------
%%    ???    Free lists
%%  -----------------------------
%%    4      File size, in bytes. See 9(d) obove.

%%  Before we can find an object we must find the slot where the
%%  object resides. Each slot is a (possibly empty) list (or chain) of
%%  objects that hash to the same slot. If the value stored in the
%%  slot is zero, the slot chain is empty. If the slot value is
%%  non-zero, the value points to a position in the file where the
%%  collection of objects resides. Each collection has the following
%%  layout:
%%
%%   bytes  decsription
%%  --------------------
%%    4     Size of the area allocated for the collection (8+Sz)
%%    4     Status  (FREE or ACTIVE). These two are the Object Header.
%%    Sz    A binary containing the objects per key, sorted on key.
%% 
%%  When repairing or converting a file, the status field is used.
%%
%%  The binary containing the objects per key of a table of type 'set'
%%  has the following layout:
%%
%%   bytes  decsription
%%  --------------------
%%    4     Size of the object of the first key (4+OSz1)
%%    OSz1  The object of the first key
%%    ...
%%    4     Size of the object of the ith key (4+OSzi)
%%    OSzi  The object of the ith key
%% 
%%  The binary containing the objects per key of a table of type 'bag'
%%  or 'duplicate_bag' has the following layout:
%%
%%   bytes    decsription
%%  ----------------------
%%    4       Size of the objects of the first key (4 + OSz1_1+...+OSz1_j+...)
%%    4       Size of the first object of the first key (4+OSz1_1)
%%    OSz1_1  The first object of the first key
%%    ...
%%    4       Size of the jth object of the first key (4+OSz1_j)
%%    OSz1_j  The jth object of the first key
%%    ...
%%    4       Size of the objects of the ith key (4 + OSzi_1+...+OSzi_k+...)
%%    4       Size of the first object of the ith key (4+OSzi_1)
%%    OSzi_1  The first object of the ith key
%%    ...
%%    4       Size of the kth object of the ith key (4+OSzi_k)
%%    OSzi_k  The kth object of the ith key
%%    ...
%%
%%  The objects of a key are placed in time order, that is, the older
%%  objects come first. If a new object is inserted, it is inserted
%%  last.
%%
%%
%%
%%|---------------|
%%|      head     |
%%|       	  |
%%|               |
%%|_______________|
%%|               |--|
%%|___part ptr 1__|  |
%%|               |  | segarr part 1
%%|___part ptr 2__|  V______________|
%%|               |  |   p1         |
%%|               |  |______________|--|
%%|     ....      |  |   p2         |  |
%%     (256)         |______________|  |
%%                   |              |  |
%%                   |     ....     |  | segment 1
%%                   |    (512)     |  V __slot 0 ____|
%%                                     |   size       |
%%                                     |   pointer    |--|
%%                                     |___slot 1 ____|  |
%%                                     |              |  |
%%                                     |   ....       |  |  objects in slot 0
%%                                         (256)         V  segment 1
%%                                                       |___________|
%%                                                       |  size     |
%%                                                       |___________|
%%                                                       |  status   |
%%                                                       |___________|
%%                                                       |           |
%%                                                       |   object  |
%%                                                       |   collec. |
%%                                                       |___________|

%%%
%%% File header
%%%

-define(RESERVED, 124).        % Reserved for future use.

-define(COLL_CNTRS, (28*4)).     % Counters for the buddy system.
-define(MD5SZ, 16).
-define(FL_BASE, 4).

-define(HEADSZ, 56+?COLL_CNTRS  % The size of the file header, in bytes,
            +?MD5SZ+?FL_BASE).  % not including the reserved part.
-define(HEADEND, (?HEADSZ+?RESERVED)). 
                               % End of header and reserved area.
-define(SEGSZ, 512).           % Size of a segment, in words. SZOBJP*SEGSZP.
-define(SEGSZP, 256).          % Size of a segment, in number of pointers.
-define(SEGSZP_LOG2, 8).
-define(SEGOBJSZ, (4 * ?SZOBJP)).
-define(SEGPARTSZ, 512).       % Size of segment array part, in words.
-define(SEGPARTSZ_LOG2, 9).
-define(SEGARRSZ, 256).        % Maximal number of segment array parts..
-define(SEGARRADDR(PartN), (?HEADEND + (4 * (PartN)))).
-define(SEGPARTADDR(P,SegN), ((P) + (4 * ?REM2(SegN, ?SEGPARTSZ)))).
-define(BASE, ?SEGARRADDR(?SEGARRSZ)).
-define(MAXSLOTS, (?SEGARRSZ * ?SEGPARTSZ * ?SEGSZP)).

-define(SLOT2SEG(S), ((S) bsr ?SEGSZP_LOG2)).
-define(SEG2SEGARRPART(S), ((S) bsr ?SEGPARTSZ_LOG2)).

-define(PHASH, 0).
-define(PHASH2, 1).

%% BIG is used for hashing. BIG must be greater than the maximum
%% number of slots, currently 32 M (MAXSLOTS).
-define(BIG, 16#3ffffff). % 64 M

%% Hard coded positions into the file header:
-define(FREELIST_POS, 0).
-define(CLOSED_PROPERLY_POS, 8).
-define(D_POS, 20).

%%% This module handles Dets file format version 9, introduced in
%%% Erlang/OTP R8.
%%% 
%%% Version 9(a) tables have 256 reserved bytes in the file header,
%%% all initialized to zero.
%%% Version 9(b) tables use the first 112 of these bytes for storing
%%% number of objects for each size of the buddy system. An empty 9(b)
%%% table cannot be distinguished from an empty 9(a) table.
%%% 9(c) has an MD5-sum for the file header.

-define(FILE_FORMAT_VERSION, 9).

-define(NOT_PROPERLY_CLOSED,0).
-define(CLOSED_PROPERLY,1).

%% Size of object pointer, in words. SEGSZ = SZOBJP * SEGSZP.
-define(SZOBJP, 2).

-define(OHDSZ, 8).          % The size of the object header, in bytes.
-define(STATUS_POS, 4).     % Position of the status field.

%% The size of each object is a multiple of 16.
%% BUMP is used when repairing files.
-define(BUMP, 16).

%%% '$hash' is the value of HASH_PARMS in Erlang/OTP R8, '$hash2' is
%%% the value in Erlang/OTP R9.
%%%
%%% The fields of the ?HASH_PARMS records are the same, but having
%%% different tags makes bchunk_init on Erlang/OTP R8 nodes reject
%%% data from Erlang/OTP R9 nodes, and vice versa. This is overkill,
%%% and due to an oversight. What should have been done in Erlang/OTP
%%% R8 was to check the hash method, not only the type of the table
%%% and the key position. Erlang/OTP R8 nodes cannot handle the phash2
%%% method.
-define(HASH_PARMS, '$hash2').

-define(BCHUNK_FORMAT_VERSION, 1).

-record(?HASH_PARMS, {
	   file_format_version,
	   bchunk_format_version, 
	   file, type, keypos, hash_method,
	   n,m,next,
	   min,max,
	   no_objects,no_keys,
	   no_colls :: no_colls()
	  }).

-define(ACTUAL_SEG_SIZE, (?SEGSZ*4)).

-define(MAXBUD, 32).

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%% -> ok | throw({NewHead,Error})
mark_dirty(Head) ->
    Dirty = [{?CLOSED_PROPERLY_POS, <<?NOT_PROPERLY_CLOSED:32>>}],
    {_H, ok} = dets_utils:pwrite(Head, Dirty),
    ok = dets_utils:sync(Head),
    {ok, _Pos} = dets_utils:position(Head, Head#head.freelists_p),
    dets_utils:truncate(Head, cur).

%% -> {ok, head()} | throw(Error) | throw(badarg)
prep_table_copy(Fd, Tab, Fname, Type, Kp, Ram, CacheSz, Auto, Parms) ->
    case Parms of
	#?HASH_PARMS{file_format_version = ?FILE_FORMAT_VERSION, 
		     bchunk_format_version = ?BCHUNK_FORMAT_VERSION,
		     n = N, m = M, next = Next,
		     min = Min, max = Max,
		     hash_method = HashMethodCode,
		     no_objects = NoObjects, no_keys = NoKeys, 
		     no_colls = _NoColls} 
	        when is_integer(N), is_integer(M), is_integer(Next), 
		     is_integer(Min), is_integer(Max), 
		     is_integer(NoObjects), is_integer(NoKeys),
		     NoObjects >= NoKeys ->
            HashMethod = code_to_hash_method(HashMethodCode),
	    case hash_invars(N, M, Next, Min, Max) of
		false ->
		    throw(badarg);
		true ->
		    init_file(Fd, Tab, Fname, Type, Kp, Min, Max, Ram, 
			      CacheSz, Auto, false, M, N, Next, HashMethod,
			      NoObjects, NoKeys)
	    end;
	_ ->
	    throw(badarg)
    end.

%% -> {ok, head()} | throw(Error)
%% The File header and the SegmentArray Pointers are written here.
%% SegmentArray Parts are also written, but the segments are are not
%% initialized on file unless DoInitSegments is 'true'. (When
%% initializing a file by calling init_table, some time is saved by
%% not writing the segments twice.)
initiate_file(Fd, Tab, Fname, Type, Kp, MinSlots0, MaxSlots0, 
	      Ram, CacheSz, Auto, DoInitSegments) ->
    MaxSlots1 = erlang:min(MaxSlots0, ?MAXSLOTS),
    MinSlots1 = erlang:min(MinSlots0, MaxSlots1),
    MinSlots = slots2(MinSlots1),
    MaxSlots = slots2(MaxSlots1),
    M = Next = MinSlots,
    N = 0,
    init_file(Fd, Tab, Fname, Type, Kp, MinSlots, MaxSlots, Ram, CacheSz,
	      Auto, DoInitSegments, M, N, Next, phash2, 0, 0).

init_file(Fd, Tab, Fname, Type, Kp, MinSlots, MaxSlots, Ram, CacheSz,
	  Auto, DoInitSegments, M, N, Next, HashMethod, NoObjects, NoKeys) ->
    Ftab = dets_utils:init_alloc(?BASE),

    Head0 = #head{
      m  = M,
      m2 = M * 2,
      next = Next,
      fptr = Fd,
      no_objects = NoObjects,
      no_keys = NoKeys,
      maxobjsize = 0,
      n = N,
      type = Type,
      update_mode = dirty,
      freelists = Ftab,
      no_collections = orddict:new(),
      auto_save = Auto,
      hash_bif = HashMethod,
      has_md5 = true,
      keypos = Kp,
      min_no_slots = MinSlots,
      max_no_slots = MaxSlots,
      
      ram_file = Ram, 
      filename = Fname, 
      name = Tab,
      cache = dets_utils:new_cache(CacheSz),
      bump = ?BUMP,
      base = ?BASE % to be overwritten
     },

    FreeListsPointer = 0,
    NoColls = <<0:?COLL_CNTRS/unit:8>>, %% Buddy system counters.
    FileHeader = file_header(Head0, FreeListsPointer, 
                             ?NOT_PROPERLY_CLOSED, NoColls),
    W0 = {0, [FileHeader |
              <<0:(4*?SEGARRSZ)/unit:8>>]},  %% SegmentArray Pointers

    %% Remove cached pointers to segment array parts and segments:
    lists:foreach(fun({I1,I2}) when is_integer(I1), is_integer(I2) -> ok;
		     ({K,V}) -> put(K, V)
		  end, erase()),

    %% Initialize array parts. 
    %% All parts before segments, for the sake of repair and initialization.
    Zero = seg_zero(),
    {Head1, Ws1} = init_parts(Head0, 0, no_parts(Next), Zero, []),
    NoSegs = no_segs(Next),

    {Head2, WsI, WsP} = init_segments(Head1, 0, NoSegs, Zero, [], []),
    Ws2 = if
	      DoInitSegments -> WsP ++ WsI;
	      true -> WsP
	 end,
    dets_utils:pwrite(Fd, Fname, [W0 | lists:append(Ws1) ++ Ws2]),
    true = hash_invars(Head2),
    %% The allocations that have been made so far (parts, segments)
    %% are permanent; the table will never shrink. Therefore the base
    %% of the Buddy system can be set to the first free object.
    %% This is used in allocate_all(), see below.
    {_, Where, _} = dets_utils:alloc(Head2, ?BUMP),
    NewFtab = dets_utils:init_alloc(Where),
    Head = Head2#head{freelists = NewFtab, base = Where},
    {ok, Head}.

%% Returns a power of two not less than 256.
slots2(NoSlots) when NoSlots >= 256 ->
    ?POW(dets_utils:log2(NoSlots)).

init_parts(Head, PartNo, NoParts, Zero, Ws) when PartNo < NoParts ->
    PartPos = ?SEGARRADDR(PartNo),
    {NewHead, W, _Part} = alloc_part(Head, Zero, PartPos),
    init_parts(NewHead, PartNo+1, NoParts, Zero, [W | Ws]);
init_parts(Head, _PartNo, _NoParts, _Zero, Ws) ->
    {Head, Ws}.

%% -> {Head, SegInitList, OtherList}; 
%% SegPtrList = SegInitList = pwrite_list().
init_segments(Head, SegNo, NoSegs, SegZero, WsP, WsI) when SegNo < NoSegs ->
    {NewHead, WI, Ws} = allocate_segment(Head, SegZero, SegNo),
    init_segments(NewHead, SegNo+1, NoSegs, SegZero, Ws ++ WsP, [WI | WsI]);
init_segments(Head, _SegNo, _NoSegs, _SegZero, WsP, WsI) ->
    {Head, WsI, WsP}.

%% -> {NewHead, SegInit, [SegPtr | PartStuff]}
allocate_segment(Head, SegZero, SegNo) ->
    PartPos = ?SEGARRADDR(SegNo div ?SEGPARTSZ),
    case get_arrpart(PartPos) of
	undefined ->
	    %% may throw error:
	    {Head1, [InitArrPart, ArrPartPointer], Part} = 
		alloc_part(Head, SegZero, PartPos),
	    {NewHead, InitSegment, [SegPointer]} =
                alloc_seg(Head1, SegZero, SegNo, Part),
	    {NewHead, InitSegment, [InitArrPart, SegPointer, ArrPartPointer]};
	Part ->
            alloc_seg(Head, SegZero, SegNo, Part)
    end.

alloc_part(Head, PartZero, PartPos) ->
    %% may throw error:
    {NewHead, Part, _} = dets_utils:alloc(Head, adjsz(4 * ?SEGPARTSZ)),
    arrpart_cache(PartPos, Part),
    InitArrPart = {Part, PartZero}, % same size as segment
    ArrPartPointer = {PartPos, <<Part:32>>},
    {NewHead, [InitArrPart, ArrPartPointer], Part}.

alloc_seg(Head, SegZero, SegNo, Part) ->
    %% may throw error:
    {NewHead, Segment, _} = dets_utils:alloc(Head, adjsz(4 * ?SEGSZ)), 
    InitSegment = {Segment, SegZero},
    Pos = ?SEGPARTADDR(Part, SegNo),
    segp_cache(Pos, Segment),
    dets_utils:disk_map_segment(Segment, SegZero),
    SegPointer = {Pos, <<Segment:32>>},
    {NewHead, InitSegment, [SegPointer]}.

%% Read free lists (using a Buddy System) from file. 
init_freelist(Head) ->
    Pos = Head#head.freelists_p,
    free_lists_from_file(Head, Pos).

%% -> {ok, Fd, fileheader()} | throw(Error)
read_file_header(Fd, FileName) ->
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, 0, ?HEADSZ),
    <<FreeList:32,   Cookie:32,  CP:32,         Type2:32,
      Version:32,    M:32,       Next:32,       Kp:32,
      NoObjects:32,  NoKeys:32,  MinNoSlots:32, MaxNoSlots:32,
      HashMethod:32, N:32, NoCollsB:?COLL_CNTRS/binary, 
      MD5:?MD5SZ/binary, FlBase:32>> = Bin,
    <<_:12/binary,MD5DigestedPart:(?HEADSZ-?MD5SZ-?FL_BASE-12)/binary,
      _/binary>> = Bin,
    {ok, EOF} = dets_utils:position_close(Fd, FileName, eof),
    {ok, <<FileSize:32>>} = dets_utils:pread_close(Fd, FileName, EOF-4, 4),
    {CL, <<>>} = lists:foldl(fun(LSz, {Acc,<<NN:32,R/binary>>}) -> 
				     if 
					 NN =:= 0 -> {Acc, R};
					 true -> {[{LSz,NN} | Acc], R}
				     end
			     end, {[], NoCollsB}, lists:seq(4, ?MAXBUD-1)),
    NoColls = 
	if 
	    CL =:= [], NoObjects > 0 -> % Version 9(a)
		undefined;
	    true ->
		lists:reverse(CL)
	end,
    Base = case FlBase of
               0 -> ?BASE;
               _ -> FlBase
           end,
    FH = #fileheader{freelist = FreeList,
                     fl_base = Base,
		     cookie = Cookie,
		     closed_properly = CP,
		     type = dets_utils:code_to_type(Type2),
		     version = Version,
		     m = M,
		     next = Next,
		     keypos = Kp,
		     no_objects = NoObjects,
		     no_keys = NoKeys,
		     min_no_slots = MinNoSlots,
		     max_no_slots = MaxNoSlots,
		     no_colls = NoColls,
		     hash_method = HashMethod,
                     read_md5 = MD5,
                     has_md5 = <<0:?MD5SZ/unit:8>> =/= MD5,
                     md5 = erlang:md5(MD5DigestedPart),
		     trailer = FileSize + FlBase,
		     eof = EOF,
		     n = N},
    {ok, Fd, FH}.

%% -> {ok, head()} | {error, Reason} (Reason lacking file name)
check_file_header(FH, Fd) ->
    HashBif = code_to_hash_method(FH#fileheader.hash_method),
    Test = 
	if
	    FH#fileheader.cookie =/= ?MAGIC ->
		{error, not_a_dets_file};
	    FH#fileheader.type =:= badtype ->
		{error, invalid_type_code};
	    FH#fileheader.version =/= ?FILE_FORMAT_VERSION -> 
                {error, bad_version};
            FH#fileheader.has_md5, 
            FH#fileheader.read_md5 =/= FH#fileheader.md5 ->
                {error, not_a_dets_file}; % harsh but fair
	    FH#fileheader.trailer =/= FH#fileheader.eof ->
		{error, not_closed};
	    HashBif =:= undefined ->
		{error, bad_hash_bif};
	    FH#fileheader.closed_properly =:= ?CLOSED_PROPERLY ->
		ok;
	    FH#fileheader.closed_properly =:= ?NOT_PROPERLY_CLOSED ->
		{error, not_closed};
	    true ->
		{error, not_a_dets_file}
	end,
    case Test of
	ok ->
            MaxObjSize = max_objsize(FH#fileheader.no_colls),
	    H = #head{
	      m = FH#fileheader.m,
	      m2 = FH#fileheader.m * 2,
	      next = FH#fileheader.next,
	      fptr = Fd,
	      no_objects = FH#fileheader.no_objects,
	      no_keys = FH#fileheader.no_keys,
	      maxobjsize = MaxObjSize,
	      n = FH#fileheader.n,
	      type = FH#fileheader.type,
	      update_mode = saved,
	      auto_save = infinity,             % not saved on file
	      fixed = false,			% not saved on file
	      freelists_p = FH#fileheader.freelist,
	      hash_bif = HashBif,
              has_md5 = FH#fileheader.has_md5,
	      keypos = FH#fileheader.keypos,
	      min_no_slots = FH#fileheader.min_no_slots,
	      max_no_slots = FH#fileheader.max_no_slots,
	      no_collections = FH#fileheader.no_colls,
	      bump = ?BUMP,
	      base = FH#fileheader.fl_base},
	    {ok, H};
	Error ->
	    Error
    end.

%% Inlined.
max_objsize(NoColls = undefined) ->
    NoColls;
max_objsize(NoColls) ->
    max_objsize(NoColls, 0).

max_objsize([], Max) ->
    Max;
max_objsize([{_,0} | L], Max) ->
    max_objsize(L, Max);
max_objsize([{I,_} | L], _Max) ->
    max_objsize(L, I).

cache_segps(Fd, FileName, M) ->
    NoParts = no_parts(M),
    ArrStart = ?SEGARRADDR(0),
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, ArrStart, 4 * NoParts),
    cache_arrparts(Bin, ?HEADEND, Fd, FileName).

cache_arrparts(<<ArrPartPos:32, B/binary>>, Pos, Fd, FileName) ->
    arrpart_cache(Pos, ArrPartPos),
    {ok, ArrPartBin} = dets_utils:pread_close(Fd, FileName, 
                                              ArrPartPos, 
                                              ?SEGPARTSZ*4),
    cache_segps1(Fd, ArrPartBin, ArrPartPos),
    cache_arrparts(B, Pos+4, Fd, FileName);
cache_arrparts(<<>>, _Pos, _Fd, _FileName) ->
    ok.

cache_segps1(_Fd, <<0:32,_/binary>>, _P) ->
    ok;
cache_segps1(Fd, <<S:32,B/binary>>, P) ->
    dets_utils:disk_map_segment_p(Fd, S),
    segp_cache(P, S),
    cache_segps1(Fd, B, P+4);
cache_segps1(_Fd, <<>>, _P) ->
    ok.

no_parts(NoSlots) ->
    ((NoSlots - 1) div (?SEGSZP * ?SEGPARTSZ)) + 1.

no_segs(NoSlots) ->
    ((NoSlots - 1) div ?SEGSZP) + 1.

%%%
%%% Repair, conversion and initialization of a dets file.
%%%

%%% bulk_input/3. Initialization, the general case (any stream of objects).
%%% output_objs/3. Initialization (general case) and repair.
%%% bchunk_init/2. Initialization using bchunk.

bulk_input(Head, InitFun, _Cntrs) ->
    bulk_input(Head, InitFun, make_ref(), 0).

bulk_input(Head, InitFun, Ref, Seq) ->
    fun(close) ->
	    _ = (catch InitFun(close));
       (read) ->
	    case catch {Ref, InitFun(read)} of
		{Ref, end_of_input} ->
		    end_of_input;
		{Ref, {L0, NewInitFun}} when is_list(L0), 
                                             is_function(NewInitFun) ->
		    Kp = Head#head.keypos,
		    case catch bulk_objects(L0, Head, Kp, Seq, []) of
			{'EXIT', _Error} ->
			    _ = (catch NewInitFun(close)),
			    {error, invalid_objects_list};
			{L, NSeq} ->
			    {L, bulk_input(Head, NewInitFun, Ref, NSeq)}
		    end;
		{Ref, Value} ->
		    {error, {init_fun, Value}};
		Error ->
		    throw({thrown, Error})
	    end
    end.

bulk_objects([T | Ts], Head, Kp, Seq, L) ->
    BT = term_to_binary(T),
    Key = element(Kp, T),
    bulk_objects(Ts, Head, Kp, Seq+1, [make_object(Head, Key, Seq, BT) | L]);
bulk_objects([], _Head, Kp, Seq, L) when is_integer(Kp), is_integer(Seq) ->
    {L, Seq}.

-define(FSCK_SEGMENT, 1).
-define(FSCK_SEGMENT2, 10000).

-define(VEMPTY, {}).
-define(VSET(I, V, E), setelement(I, V, E)).
-define(VGET(I, V), element(I, V)).
-define(VEXT(S, V, T), 
     list_to_tuple(tuple_to_list(V) ++ lists:duplicate(S-tuple_size(V), T))).

%% Number of bytes that will be handled before the cache is written to
%% file. Used when compacting or writing chunks.
-define(CACHE_SIZE, (60*?CHUNK_SIZE)).

%% {LogSize,NoObjects} in Cntrs is replaced by
%% {LogSize,Position,{FileName,FileDescriptor},NoCollections}.
%% There is also an object {no, NoObjects, NoKeys}.
-define(COUNTERS, no).
-define(OBJ_COUNTER, 2).
-define(KEY_COUNTER, 3).

output_objs(Head, SlotNums, Cntrs) ->
    fun(close) ->
            %% Make sure that the segments are initialized in case
            %% init_table has been called.
	    Cache = ?VEMPTY,
            Acc = [], % This is the only way Acc can be empty.
            true = ets:insert(Cntrs, {?FSCK_SEGMENT,0,[],0}),
	    true = ets:insert(Cntrs, {?COUNTERS, 0, 0}),
            Fun = output_objs2(foo, Acc, Head, Cache, Cntrs,
			       SlotNums, bar),
            Fun(close);
       ([]) ->
	    output_objs(Head, SlotNums, Cntrs);
       (L) ->
	    %% Information about number of objects per size is not
	    %% relevant for version 9. It is the number of collections
	    %% that matters.
            true = ets:delete_all_objects(Cntrs),
	    true = ets:insert(Cntrs, {?COUNTERS, 0, 0}),
	    Es = bin2term(L, Head#head.keypos),
	    %% The cache is a tuple indexed by the (log) size. An element
	    %% is [BinaryObject].
	    Cache = ?VEMPTY,
	    {NE, NAcc, NCache} = output_slots(Es, Head, Cache, Cntrs, 0, 0),
	    output_objs2(NE, NAcc, Head, NCache, Cntrs, SlotNums, 1)
    end.

output_objs2(E, Acc, Head, Cache, SizeT, SlotNums, 0) ->
    NCache = write_all_sizes(Cache, SizeT, Head, more),
    %% Number of handled file_sorter chunks before writing:
    Max = erlang:max(1, erlang:min(tuple_size(NCache), 10)),
    output_objs2(E, Acc, Head, NCache, SizeT, SlotNums, Max);
output_objs2(E, Acc, Head, Cache, SizeT, SlotNums, ChunkI) ->
    fun(close) ->
            {_, [], Cache1} = 
                 if
		     Acc =:= [] -> {foo, [], Cache};
		     true -> output_slot(Acc, Head, Cache, [], SizeT, 0, 0)
		 end,
	    _NCache = write_all_sizes(Cache1, SizeT, Head, no_more),
	    SegSz = ?ACTUAL_SEG_SIZE,
	    {_, SegEnd, _} = dets_utils:alloc(Head, adjsz(SegSz)),
	    [{?COUNTERS,NoObjects,NoKeys}] = ets:lookup(SizeT, ?COUNTERS),
	    Head1 = Head#head{no_objects = NoObjects, no_keys = NoKeys},
	    true = ets:delete(SizeT, ?COUNTERS),
	    {NewHead, NL, _MaxSz, _End} = allocate_all_objects(Head1, SizeT),
	    %% It is not known until all objects have been collected
	    %% how many object collections there are per size. Now
	    %% that is known and the absolute positions of the object
	    %% collections can be calculated.
            segment_file(SizeT, NewHead, NL, SegEnd),
	    {MinSlots, EstNoSlots, MaxSlots} = SlotNums,
	    if 
		EstNoSlots =:= bulk_init ->
		    {ok, 0, NewHead};
		true ->
		    EstNoSegs = no_segs(EstNoSlots),
		    MinNoSegs = no_segs(MinSlots),
		    MaxNoSegs = no_segs(MaxSlots),
		    NoSegs = no_segs(NoKeys),
		    Diff = abs(NoSegs - EstNoSegs),
		    if 
			Diff > 5, NoSegs =< MaxNoSegs, NoSegs >= MinNoSegs  ->
			    {try_again, NoKeys};
			true ->
			    {ok, 0, NewHead}
		    end
	    end;
       (L) ->
	    Es = bin2term(L, Head#head.keypos),
	    {NE, NAcc, NCache} = 
		output_slots(E, Es, Acc, Head, Cache, SizeT, 0, 0),
	    output_objs2(NE, NAcc, Head, NCache, SizeT, SlotNums, ChunkI-1)
    end.

%%% Compaction. 

compact_init(ReadHead, WriteHead, TableParameters) ->
    SizeT = ets:new(dets_compact, []),
    #head{no_keys = NoKeys, no_objects = NoObjects} = ReadHead,

    NoObjsPerSize = TableParameters#?HASH_PARMS.no_colls,
    {NewWriteHead, Bases, SegAddr, SegEnd} = 
	prepare_file_init(NoObjects, NoKeys, NoObjsPerSize, SizeT, WriteHead),

    Input = compact_input(ReadHead, NewWriteHead, SizeT, tuple_size(Bases)),
    Output = fast_output(NewWriteHead, SizeT, Bases, SegAddr, SegEnd),
    TmpDir = filename:dirname(NewWriteHead#head.filename),
    Reply = (catch file_sorter:sort(Input, Output, 
				    [{format, binary},{tmpdir, TmpDir},
				     {header, 1}])), % compact_objs/9: 13 bytes
    ets:delete(SizeT),
    Reply.

compact_input(Head, WHead, SizeT, NoSizes) ->
    L = dets_utils:all_allocated_as_list(Head),
    Cache = ?VEXT(NoSizes, ?VEMPTY, [0 | []]),
    compact_input(Head, WHead, SizeT, Cache, L).

compact_input(Head, WHead, SizeT, Cache, L) ->
    fun(close) ->
	    ok;
       (read) ->
	    compact_read(Head, WHead, SizeT, Cache, L, 0, [], 0)
    end.

compact_read(_Head, WHead, SizeT, Cache, [], _Min, [], _ASz) ->
    _ = fast_write_all_sizes(Cache, SizeT, WHead),
    end_of_input;
compact_read(Head, WHead, SizeT, Cache, L, Min, SegBs, ASz) 
            when ASz + Min >= ?CACHE_SIZE, ASz > 0 ->
    NCache = fast_write_all_sizes(Cache, SizeT, WHead),
    {SegBs, compact_input(Head, WHead, SizeT, NCache, L)};
compact_read(Head, WHead, SizeT, Cache, [[From | To] | L], Min, SegBs, ASz) ->
    Max = erlang:max(?CHUNK_SIZE*3, Min),
    case check_pread_arg(Max, Head) of
        true ->
            case dets_utils:pread_n(Head#head.fptr, From, Max) of
                eof ->
                    %% Should never happen since compaction will not
                    %% be tried unless the file trailer is valid.
                    not_ok; % try a proper repair
                Bin1 when byte_size(Bin1) < Min ->
                    %% The last object may not be padded.
                    Pad = Min - byte_size(Bin1),
                    NewBin = <<Bin1/binary, 0:Pad/unit:8>>,
                        compact_objs(Head, WHead, SizeT, NewBin, L, 
                                     From, To, SegBs, Cache, ASz);
                NewBin ->
                    compact_objs(Head, WHead, SizeT, NewBin, L, 
                                 From, To, SegBs, Cache, ASz)
            end;
        false ->
            not_ok % try a proper repair
    end.
    
compact_objs(Head, WHead, SizeT, Bin, L, From, To, SegBs, Cache, ASz) 
                            when From =:= To ->
    case L of
	[] ->
	    {SegBs, compact_input(Head, WHead, SizeT, Cache, L)};
	[[From1 | To1] | L1] ->
	    Skip1 = From1 - From,
	    case Bin of
		<<_:Skip1/binary,NewBin/binary>> ->
		    compact_objs(Head, WHead, SizeT, NewBin, L1, From1, To1, 
				 SegBs, Cache, ASz);
		_ when byte_size(Bin) < Skip1 ->
		    compact_read(Head, WHead, SizeT, Cache, L, 0, SegBs, ASz)
	    end
    end;
compact_objs(Head, WHead, SizeT, <<Size:32, St:32, _Sz:32, KO/binary>> = Bin, 
	     L, From, To, SegBs, Cache, ASz) when St =:= ?ACTIVE ->
    LSize = sz2pos(Size),
    Size2 = ?POW(LSize-1),
    if
	byte_size(Bin) >= Size2 ->
	    NASz = ASz + Size2,
	    <<SlotObjs:Size2/binary, NewBin/binary>> = Bin,
	    Term = if
		       Head#head.type =:= set ->
			   binary_to_term(KO);
		       true ->
			   <<_KSz:32,B2/binary>> = KO,
			   binary_to_term(B2)
		   end,
	    Key = element(Head#head.keypos, Term),
	    Slot = db_hash(Key, Head),
	    From1 = From + Size2,
	    [Addr | AL] = ?VGET(LSize, Cache),
	    NCache = ?VSET(LSize, Cache, [Addr + Size2 | [SlotObjs | AL]]),
	    NSegBs = [<<Slot:32,Size:32,Addr:32,LSize:8>> | SegBs],
	    compact_objs(Head, WHead, SizeT, NewBin, L, From1,
			 To, NSegBs, NCache, NASz);
	true ->
	    compact_read(Head, WHead, SizeT, Cache, [[From|To] | L], 
			 Size2, SegBs, ASz)
    end;
compact_objs(Head, WHead, SizeT, <<_:32, _St:32, _:32, _/binary>> = Bin, 
	     L, From, To, SegBs, Cache, ASz)
              when byte_size(Bin) >= ?ACTUAL_SEG_SIZE -> % , _St =/= ?ACTIVE
    <<_:?ACTUAL_SEG_SIZE/binary, NewBin/binary>> = Bin,
    compact_objs(Head, WHead, SizeT, NewBin, L, From + ?ACTUAL_SEG_SIZE,
		 To, SegBs, Cache, ASz);
compact_objs(Head, WHead, SizeT, <<_:32, _St:32, _:32, _/binary>> = Bin, 
	     L, From, To, SegBs, Cache, ASz)
              when byte_size(Bin) < ?ACTUAL_SEG_SIZE -> % , _St =/= ?ACTIVE
    compact_read(Head, WHead, SizeT, Cache, [[From|To] | L], 
		 ?ACTUAL_SEG_SIZE, SegBs, ASz);
compact_objs(Head, WHead, SizeT, _Bin, L, From, To, SegBs, Cache, ASz) ->
    compact_read(Head, WHead, SizeT, Cache, [[From|To] | L], 0, SegBs, ASz).

%%% End compaction.

%%% Bchunk.

read_bchunks(Head, L) ->
    read_bchunks(Head, L, 0, [], 0).

read_bchunks(_Head, L, Min, Bs, ASz) when ASz + Min >= 4*?CHUNK_SIZE, 
					  Bs =/= [] ->
    {lists:reverse(Bs), L};
read_bchunks(Head, {From, To, L}, Min, Bs, ASz) ->
    Max = erlang:max(?CHUNK_SIZE*2, Min),
    case check_pread_arg(Max, Head) of
        true ->
            case dets_utils:pread_n(Head#head.fptr, From, Max) of
                eof ->
                    %% Should never happen.
                    {error, premature_eof};
                NewBin when byte_size(NewBin) >= Min ->
                    bchunks(Head, L, NewBin, Bs, ASz, From, To);
                Bin1 when To - From =:= Min, L =:= <<>> -> 
                    %% when byte_size(Bin1) < Min. 
                    %% The last object may not be padded.
                    Pad = Min - byte_size(Bin1),
                    NewBin = <<Bin1/binary, 0:Pad/unit:8>>,
                    bchunks(Head, L, NewBin, Bs, ASz, From, To);
                _ ->
                    {error, premature_eof}
            end;
        false ->
            {error, dets_utils:bad_object(bad_object, {read_bchunks, Max})}
    end.
    
bchunks(Head, L, Bin, Bs, ASz, From, To) when From =:= To ->
    if 
	L =:= <<>> ->
	    {finished, lists:reverse(Bs)};
	true -> 
	    <<From1:32, To1:32, L1/binary>> = L,
	    Skip1 = From1 - From,
	    case Bin of
		<<_:Skip1/binary,NewBin/binary>> ->
		    bchunks(Head, L1, NewBin, Bs, ASz, From1, To1);
		_ when byte_size(Bin) < Skip1 ->
		    read_bchunks(Head, {From1,To1,L1}, 0, Bs, ASz)
	    end
    end;
bchunks(Head, L, <<Size:32, St:32, _Sz:32, KO/binary>> = Bin, Bs, ASz, 
	  From, To) when St =:= ?ACTIVE; St =:= ?FREE ->
    LSize = sz2pos(Size),
    Size2 = ?POW(LSize-1),
    if
	byte_size(Bin) >= Size2 ->
	    <<B0:Size2/binary, NewBin/binary>> = Bin,
	    %% LSize and Slot are used in make_slots/6. The reason to
	    %% calculate Slot here is to reduce the CPU load in
	    %% make_slots/6.
	    Term = if
		       Head#head.type =:= set ->
			   binary_to_term(KO);
		       true ->
			   <<_KSz:32,B2/binary>> = KO,
			   binary_to_term(B2)
		   end,
	    Key = element(Head#head.keypos, Term),
	    Slot = db_hash(Key, Head),
	    B = {LSize,Slot,B0},
	    bchunks(Head, L, NewBin, [B | Bs], ASz + Size2, From+Size2, To);
	true ->
	    read_bchunks(Head, {From, To, L}, Size2, Bs, ASz)
    end;
bchunks(Head, L, <<_:32, _St:32, _:32, _/binary>> = Bin, Bs, ASz, From, To)
                  when byte_size(Bin) >= ?ACTUAL_SEG_SIZE ->
    <<_:?ACTUAL_SEG_SIZE/binary, NewBin/binary>> = Bin,
    bchunks(Head, L, NewBin, Bs, ASz, From + ?ACTUAL_SEG_SIZE, To);
bchunks(Head, L, <<_:32, _St:32, _:32, _/binary>> = Bin, Bs, ASz, From, To)
                  when byte_size(Bin) < ?ACTUAL_SEG_SIZE ->
    read_bchunks(Head, {From, To, L}, ?ACTUAL_SEG_SIZE, Bs, ASz);
bchunks(Head, L, _Bin, Bs, ASz, From, To) ->
    read_bchunks(Head, {From, To, L}, 0, Bs, ASz).

%%% End bchunk.

%% -> {ok, NewHead} | throw(Error) | Error
bchunk_init(Head, InitFun) ->
    Ref = make_ref(), 
    %% The non-empty list of data begins with the table parameters.
    case catch {Ref, InitFun(read)} of
	{Ref, end_of_input} ->
	    {error, {init_fun, end_of_input}};
	{Ref, {[], NInitFun}} when is_function(NInitFun) ->
	    bchunk_init(Head, NInitFun);
	{Ref, {[ParmsBin | L], NInitFun}} 
	                when is_list(L), is_function(NInitFun) ->
	    #head{fptr = Fd, type = Type, keypos = Kp, 
		  auto_save = Auto, cache = Cache, 
		  filename = Fname, ram_file = Ram,
		  name = Tab} = Head,
            case try_bchunk_header(ParmsBin, Head) of
                {ok, Parms} ->
		    #?HASH_PARMS{no_objects = NoObjects, 
				 no_keys = NoKeys, 
				 no_colls = NoObjsPerSize} = Parms,
		    CacheSz = dets_utils:cache_size(Cache),
		    {ok, Head1} = 
			prep_table_copy(Fd, Tab, Fname, Type, 
					Kp, Ram, CacheSz, 
					Auto, Parms),
		    SizeT = ets:new(dets_init, []),
		    {NewHead, Bases, SegAddr, SegEnd} = 
			prepare_file_init(NoObjects, NoKeys, 
					  NoObjsPerSize, SizeT, Head1),
		    ECache = ?VEXT(tuple_size(Bases), ?VEMPTY, [0 | []]),
		    Input = 
			fun(close) ->
				_ = (catch NInitFun(close));
			   (read) ->
				do_make_slots(L, ECache, SizeT, NewHead, Ref,
					      0, NInitFun)
			end,
		    Output = fast_output(NewHead, SizeT, Bases, SegAddr,SegEnd),
		    TmpDir = filename:dirname(Head#head.filename),
		    Reply = (catch file_sorter:sort(Input, Output, 
						    [{format, binary},
						     {tmpdir, TmpDir},
						     {header, 1}])),
		    ets:delete(SizeT),
		    Reply;
		not_ok ->
		    {error, {init_fun, ParmsBin}}
	    end;
	{Ref, Value} ->
	    {error, {init_fun, Value}};
	Error ->
	    {thrown, Error}
    end.

try_bchunk_header(ParmsBin, Head) ->
    #head{type = Type, keypos = Kp, hash_bif = HashBif} = Head,
    HashMethod = hash_method_to_code(HashBif),
    case catch binary_to_term(ParmsBin) of
        Parms when is_record(Parms, ?HASH_PARMS),
                   Parms#?HASH_PARMS.type =:= Type,
                   Parms#?HASH_PARMS.keypos =:= Kp,
                   Parms#?HASH_PARMS.hash_method =:= HashMethod,
                   Parms#?HASH_PARMS.bchunk_format_version =:=
                         ?BCHUNK_FORMAT_VERSION ->
            {ok, Parms};
        _ ->
            not_ok
    end.

bchunk_input(InitFun, SizeT, Head, Ref, Cache, ASz) ->
    fun(close) ->
            _ = (catch InitFun(close));
       (read) ->
	    case catch {Ref, InitFun(read)} of
		{Ref, end_of_input} ->
		    _ = fast_write_all_sizes(Cache, SizeT, Head),
		    end_of_input;
		{Ref, {L, NInitFun}} when is_list(L), is_function(NInitFun) ->
		    do_make_slots(L, Cache, SizeT, Head, Ref, ASz, 
				  NInitFun);
		{Ref, Value} ->
		    {error, {init_fun, Value}};
		Error ->
		    throw({thrown, Error})
	    end
    end.

do_make_slots(L, Cache, SizeT, Head, Ref, ASz, InitFun) ->
    case catch make_slots(L, Cache, [], ASz) of
	{'EXIT', _} ->
	    _ = (catch InitFun(close)),
	    {error, invalid_objects_list};
	{Cache1, SegBs, NASz} when NASz > ?CACHE_SIZE ->
	    NCache = fast_write_all_sizes(Cache1, SizeT, Head),
	    F = bchunk_input(InitFun, SizeT, Head, Ref, NCache, 0),
	    {SegBs, F};
	{NCache, SegBs, NASz} ->
	    F = bchunk_input(InitFun, SizeT, Head, Ref, NCache, NASz),
	    {SegBs, F}
    end.

make_slots([{LSize,Slot,<<Size:32, St:32, Sz:32, KO/binary>> = Bin0} | Bins], 
	   Cache, SegBs, ASz) ->
    Bin = if 
	      St =:= ?ACTIVE ->
		  Bin0;
	      St =:= ?FREE ->
		  <<Size:32,?ACTIVE:32,Sz:32,KO/binary>>
          end,
    BSz = byte_size(Bin0),
    true = (BSz =:= ?POW(LSize-1)),
    NASz = ASz + BSz,
    [Addr | L] = ?VGET(LSize, Cache),
    NSegBs = [<<Slot:32,Size:32,Addr:32,LSize:8>> | SegBs],
    NCache = ?VSET(LSize, Cache, [Addr + BSz | [Bin | L]]),
    make_slots(Bins, NCache, NSegBs, NASz);
make_slots([], Cache, SegBs, ASz) ->
    {Cache, SegBs, ASz}.

fast_output(Head, SizeT, Bases, SegAddr, SegEnd) ->
    fun(close) ->
	    fast_output_end(Head, SizeT);
       (L) ->
	    case file:position(Head#head.fptr, SegAddr) of
		{ok, SegAddr} ->
		    NewSegAddr = write_segment_file(L, Bases, Head, [], 
						    SegAddr, SegAddr),
		    fast_output2(Head, SizeT, Bases, NewSegAddr, 
				 SegAddr, SegEnd);
		Error ->
		    catch dets_utils:file_error(Error, Head#head.filename)
	    end
    end.

fast_output2(Head, SizeT, Bases, SegAddr, SS, SegEnd) ->
    fun(close) ->
	    FinalZ = SegEnd - SegAddr,
	    dets_utils:write(Head, dets_utils:make_zeros(FinalZ)),
	    fast_output_end(Head, SizeT);
       (L) ->
            NewSegAddr = write_segment_file(L, Bases, Head, [], SegAddr, SS),
            fast_output2(Head, SizeT, Bases, NewSegAddr, SS, SegEnd)
    end.

fast_output_end(Head, SizeT) ->
    case ets:foldl(fun({_Sz,_Pos,Cnt,NoC}, Acc) -> (Cnt =:= NoC) and Acc end, 
		   true, SizeT) of
	true -> {ok, Head};
	false -> {error, invalid_objects_list}
    end.
    
%% Inlined.
write_segment_file([<<Slot:32,BSize:32,AddrToBe:32,LSize:8>> | Bins], 
		   Bases, Head, Ws, SegAddr, SS) ->
    %% Should call slot_position/1, but since all segments are
    %% allocated in a sequence, the position of a slot can be
    %% calculated faster.
    Pos = SS + ?SZOBJP*4 * Slot, % Same as Pos = slot_position(Slot).
    write_segment_file(Bins, Bases, Head, Ws, SegAddr, SS, Pos, 
		       BSize, AddrToBe, LSize);
write_segment_file([], _Bases, Head, Ws, SegAddr, _SS) ->
    dets_utils:write(Head, Ws),
    SegAddr.

write_segment_file(Bins, Bases, Head, Ws, SegAddr, SS, Pos, BSize, 
		   AddrToBe, LSize) when Pos =:= SegAddr ->
    Addr = AddrToBe + element(LSize, Bases),
    NWs = [Ws | <<BSize:32,Addr:32>>],
    write_segment_file(Bins, Bases, Head, NWs, SegAddr + ?SZOBJP*4, SS);
write_segment_file(Bins, Bases, Head, Ws, SegAddr, SS, Pos, BSize, 
		   AddrToBe, LSize) when Pos - SegAddr < 100 ->
    Addr = AddrToBe + element(LSize, Bases),
    NoZeros = Pos - SegAddr,
    NWs = [Ws | <<0:NoZeros/unit:8,BSize:32,Addr:32>>],
    NSegAddr = SegAddr + NoZeros + ?SZOBJP*4,
    write_segment_file(Bins, Bases, Head, NWs, NSegAddr, SS);
write_segment_file(Bins, Bases, Head, Ws, SegAddr, SS, Pos, BSize, 
		   AddrToBe, LSize) ->
    Addr = AddrToBe + element(LSize, Bases),
    NoZeros = Pos - SegAddr,
    NWs = [Ws, dets_utils:make_zeros(NoZeros) | <<BSize:32,Addr:32>>],
    NSegAddr = SegAddr + NoZeros + ?SZOBJP*4,
    write_segment_file(Bins, Bases, Head, NWs, NSegAddr, SS).

fast_write_all_sizes(Cache, SizeT, Head) ->
    CacheL = lists:reverse(tuple_to_list(Cache)),
    fast_write_sizes(CacheL, tuple_size(Cache), SizeT, Head, [], []).

fast_write_sizes([], _Sz, _SizeT, Head, NCL, PwriteList) ->
    #head{filename = FileName, fptr = Fd} = Head,
    ok = dets_utils:pwrite(Fd, FileName, PwriteList),
    list_to_tuple(NCL);
fast_write_sizes([[_Addr] = C | CL], Sz, SizeT, Head, NCL, PwriteList) ->
    fast_write_sizes(CL, Sz-1, SizeT, Head, [C | NCL], PwriteList);
fast_write_sizes([[Addr | C] | CL], Sz, SizeT, Head, NCL, PwriteList) ->
    case ets:lookup(SizeT, Sz) of
	[] ->
	    throw({error, invalid_objects_list});
	[{Sz,Position,_ObjCounter,_NoCollections}] ->
	    %% Update ObjCounter:
	    NoColls = length(C),
	    _ = ets:update_counter(SizeT, Sz, {3, NoColls}),
	    Pos = Position + Addr - NoColls*?POW(Sz-1),
            fast_write_sizes(CL, Sz-1, SizeT, Head, [[Addr] | NCL],
			     [{Pos,lists:reverse(C)} | PwriteList])
    end.

prepare_file_init(NoObjects, NoKeys, NoObjsPerSize, SizeT, Head) ->
    SegSz = ?ACTUAL_SEG_SIZE,
    {_, SegEnd, _} = dets_utils:alloc(Head, adjsz(SegSz)),
    Head1 = Head#head{no_objects = NoObjects, no_keys = NoKeys},
    true = ets:insert(SizeT, {?FSCK_SEGMENT,0,[],0}),
    lists:foreach(fun({LogSz,NoColls}) -> 
			  true = ets:insert(SizeT, {LogSz+1,0,0,NoColls})
		  end, NoObjsPerSize),
    {NewHead, NL0, MaxSz, EndOfFile} = allocate_all_objects(Head1, SizeT),
    [{?FSCK_SEGMENT,SegAddr,[],0} | NL] = NL0,
    true = ets:delete_all_objects(SizeT),
    lists:foreach(fun(X) -> true = ets:insert(SizeT, X) end, NL),
    Bases = lists:foldl(fun({LSz,P,_D,_N}, A) -> setelement(LSz,A,P) end,
			erlang:make_tuple(MaxSz, 0), NL),
    Est = lists:foldl(fun({LSz,_,_,N}, A) -> A + ?POW(LSz-1)*N end, 0, NL),
    ok = write_bytes(NewHead, EndOfFile, Est),
    {NewHead, Bases, SegAddr, SegEnd}.

%% Writes "zeros" to the file. This ensures that the file blocks are
%% allocated more or less contiguously, which reduces the seek times
%% to a minimum when the file is later read serially from beginning to
%% end (as is done when calling select and the like). A well-formed
%% file will be created also if nothing is written (as is the case for
%% small files, for efficiency).
write_bytes(_Head, _EndOfFile, Est) when Est < ?CACHE_SIZE ->
    ok;
write_bytes(Head, EndOfFile, _Est) ->
    Fd = Head#head.fptr,
    {ok, Start} = file:position(Fd, eof),
    BytesToWrite = EndOfFile - Start,
    SizeInKB = 64,
    Bin = list_to_binary(lists:duplicate(SizeInKB * 4, lists:seq(0, 255))),
    write_loop(Head, BytesToWrite, Bin).

write_loop(Head, BytesToWrite, Bin) when BytesToWrite >= byte_size(Bin) ->
    case file:write(Head#head.fptr, Bin) of
	ok -> write_loop(Head, BytesToWrite - byte_size(Bin), Bin);
	Error -> dets_utils:file_error(Error, Head#head.filename)
    end;
write_loop(_Head, 0, _Bin) ->
    ok;
write_loop(Head, BytesToWrite, Bin) ->
    <<SmallBin:BytesToWrite/binary,_/binary>> = Bin,
    write_loop(Head, BytesToWrite, SmallBin).

%% By allocating bigger objects before smaller ones, holes in the
%% buddy system memory map are avoided.
allocate_all_objects(Head, SizeT) ->
    DTL = lists:reverse(lists:keysort(1, ets:tab2list(SizeT))),
    MaxSz = element(1, hd(DTL)),
    {Head1, NL} = allocate_all(Head, DTL, []),
    %% Find the position that will be the end of the file by allocating
    %% a minimal object.
    {_Head, EndOfFile, _} = dets_utils:alloc(Head1, ?BUMP),
    NewHead = Head1#head{maxobjsize = max_objsize(Head1#head.no_collections)},
    {NewHead, NL, MaxSz, EndOfFile}.

%% One (temporary) file for each buddy size, write all objects of that
%% size to the file.
%%
%% Before R15 a "hole" was needed before the first bucket if the size
%% of the biggest bucket was greater than the size of a segment. The
%% hole proved to be a problem with almost full tables with huge
%% buckets. Since R15 the hole is no longer needed due to the fact
%% that the base of the Buddy system is flexible.
allocate_all(Head, [{?FSCK_SEGMENT,_,Data,_}], L) ->
    %% And one file for the segments...
    %% Note that space for the array parts and the segments has
    %% already been allocated, but the segments have not been
    %% initialized on disk.
    NoParts = no_parts(Head#head.next),
    %% All parts first, ensured by init_segments/6.
    Addr = ?BASE + NoParts * 4 * ?SEGPARTSZ,
    {Head, [{?FSCK_SEGMENT,Addr,Data,0} | L]};
allocate_all(Head, [{LSize,_,Data,NoCollections} | DTL], L) ->
    Size = ?POW(LSize-1),
    {_Head, Addr, _} = dets_utils:alloc(Head, adjsz(Size)),
    Head1 = dets_utils:alloc_many(Head, Size, NoCollections, Addr),
    NoColls = Head1#head.no_collections,
    NewNoColls = orddict:update_counter(LSize-1, NoCollections, NoColls),
    NewHead = Head1#head{no_collections = NewNoColls},
    E = {LSize,Addr,Data,NoCollections},
    allocate_all(NewHead, DTL, [E | L]).

bin2term(Bin, Kp) ->
    bin2term1(Bin, Kp, []).

bin2term1([<<Slot:32, Seq:32, BinTerm/binary>> | BTs], Kp, L) ->
    Term = binary_to_term(BinTerm),
    Key = element(Kp, Term),
    bin2term1(BTs, Kp, [{Slot, Key, Seq, Term, BinTerm} | L]);
bin2term1([], _Kp, L) ->
    lists:reverse(L).

write_all_sizes({}=Cache, _SizeT, _Head, _More) ->
    Cache;
write_all_sizes(Cache, SizeT, Head, More) ->
    CacheL = lists:reverse(tuple_to_list(Cache)),
    Sz = length(CacheL),
    NCL = case ets:info(SizeT, size) of
	      1 when More =:= no_more -> % COUNTERS only...
		  all_sizes(CacheL, Sz, SizeT);
	      _ ->
		  write_sizes(CacheL, Sz, SizeT, Head)
	  end,
    list_to_tuple(NCL).    

all_sizes([]=CL, _Sz, _SizeT) ->
    CL;
all_sizes([[]=C | CL], Sz, SizeT) ->
    [C | all_sizes(CL, Sz-1, SizeT)];
all_sizes([C0 | CL], Sz, SizeT) ->
    C = lists:reverse(C0),
    NoCollections = length(C),
    true = ets:insert(SizeT, {Sz,0,C,NoCollections}),
    [[] | all_sizes(CL, Sz-1, SizeT)].

write_sizes([]=CL, _Sz, _SizeT, _Head) ->
    CL;
write_sizes([[]=C | CL], Sz, SizeT, Head) ->
    [C | write_sizes(CL, Sz-1, SizeT, Head)];
write_sizes([C | CL], Sz, SizeT, Head) ->
    {FileName, Fd} = 
	case ets:lookup(SizeT, Sz) of
	    [] ->
		temp_file(Head, SizeT, Sz);
	    [{_,_,{FN,F},_}] ->
		{FN, F}
	end,
    NoCollections = length(C),
    _ = ets:update_counter(SizeT, Sz, {4,NoCollections}),
    case file:write(Fd, lists:reverse(C)) of
	ok ->
	    [[] | write_sizes(CL, Sz-1, SizeT, Head)];
	Error ->
	    dets_utils:file_error(FileName, Error)
    end.

output_slots([E | Es], Head, Cache, SizeT, NoKeys, NoObjs) ->
    output_slots(E, Es, [E], Head, Cache, SizeT, NoKeys, NoObjs);
output_slots([], _Head, Cache, SizeT, NoKeys, NoObjs) ->
    _ = ets:update_counter(SizeT, ?COUNTERS, {?OBJ_COUNTER,NoObjs}),
    _ = ets:update_counter(SizeT, ?COUNTERS, {?KEY_COUNTER,NoKeys}),
    {not_a_tuple, [], Cache}.

output_slots(E, [E1 | Es], Acc, Head, Cache, SizeT, NoKeys, NoObjs) 
                       when element(1, E) =:= element(1, E1) ->
    output_slots(E1, Es, [E1 | Acc], Head, Cache, SizeT, NoKeys, NoObjs);
output_slots(E, [], Acc, _Head, Cache, SizeT, NoKeys, NoObjs) ->
    _ = ets:update_counter(SizeT, ?COUNTERS, {?OBJ_COUNTER,NoObjs}),
    _ = ets:update_counter(SizeT, ?COUNTERS, {?KEY_COUNTER,NoKeys}),
    {E, Acc, Cache};
output_slots(_E, L, Acc, Head, Cache, SizeT, NoKeys, NoObjs) ->
    output_slot(Acc, Head, Cache, L, SizeT, NoKeys, NoObjs).

output_slot(Es, Head, Cache, L, SizeT, NoKeys, NoObjs) ->
    Slot = element(1, hd(Es)),
    %% Plain lists:sort/1 will do.
    {Bins, Size, No, KNo} = prep_slot(lists:sort(Es), Head),
    NNoKeys = NoKeys + KNo,
    NNoObjs = NoObjs + No,

    %% First the object collection.
    BSize = Size + ?OHDSZ,
    LSize = sz2pos(BSize),
    Size2 = ?POW(LSize-1),
    Pad = <<0:(Size2-BSize)/unit:8>>,
    BinObject = [<<BSize:32, ?ACTIVE:32>>, Bins | Pad],
    Cache1 = 
	if
	    LSize > tuple_size(Cache) ->
                C1 = ?VEXT(LSize, Cache, []),
		?VSET(LSize, C1, [BinObject]);
	    true ->
		CL = ?VGET(LSize, Cache),
		?VSET(LSize, Cache, [BinObject | CL])
	end,

    %% Then the pointer to the object collection.
    %% Cannot yet determine the absolute pointers; segment_file/4 does that.
    PBin = <<Slot:32,BSize:32,LSize:8>>,
    PL = ?VGET(?FSCK_SEGMENT, Cache1),
    NCache = ?VSET(?FSCK_SEGMENT, Cache1, [PBin | PL]),
    output_slots(L, Head, NCache, SizeT, NNoKeys, NNoObjs).

prep_slot(L, Head) when Head#head.type =/= set ->
    prep_slot(L, Head, []);
prep_slot([{_Slot,Key,_Seq,_T,BT} | L], _Head) ->
    prep_set_slot(L, Key, BT, 0, 0, 0, []).

prep_slot([{_Slot, Key, Seq, T, _BT} | L], Head, W) ->
    prep_slot(L, Head, [{Key, {Seq, {insert,T}}} | W]);
prep_slot([], Head, W) ->
    WLs = dets_utils:family(W),
    {[], Bins, Size, No, KNo, _} = 
	eval_slot(WLs, [], Head#head.type, [], [], 0, 0, 0, false),
    {Bins, Size, No, KNo}.

%% Optimization, prep_slot/3 would work for set tables as well.
prep_set_slot([{_,K,_Seq,_T1,BT1} | L], K, _BT, Sz, NoKeys, NoObjs, Ws) ->
    prep_set_slot(L, K, BT1, Sz, NoKeys, NoObjs, Ws);
prep_set_slot([{_,K1,_Seq,_T1,BT1} | L], _K, BT, Sz, NoKeys, NoObjs, Ws) ->
    BSize = byte_size(BT) + 4,
    NWs = [Ws,<<BSize:32>>|BT],
    prep_set_slot(L, K1, BT1, Sz+BSize, NoKeys+1, NoObjs+1, NWs);
prep_set_slot([], _K, BT, Sz, NoKeys, NoObjs, Ws) ->
    BSize = byte_size(BT) + 4,
    {[Ws, <<BSize:32>> | BT], Sz + BSize, NoKeys+1, NoObjs+1}.

segment_file(SizeT, Head, FileData, SegEnd) ->
    I = 2,
    true = ets:delete_all_objects(SizeT),    
    lists:foreach(fun(X) -> true = ets:insert(SizeT, X) end, FileData),
    [{?FSCK_SEGMENT,SegAddr,Data,0} | FileData1] = FileData,
    NewData = 
	case Data of
	    {InFile,In0} ->
		{OutFile, Out} = temp_file(Head, SizeT, I),
		_ = file:close(In0),
		{ok, In} = dets_utils:open(InFile, [raw,binary,read]),
		{ok, 0} = dets_utils:position(In, InFile, bof),
		seg_file(SegAddr, SegAddr, In, InFile, Out, OutFile, SizeT, 
			 SegEnd),
		_ = file:close(In),
		_ = file:delete(InFile),
		{OutFile,Out};
	    Objects ->
		{LastAddr, B} = seg_file(Objects, SegAddr, SegAddr, SizeT, []),
                dets_utils:disk_map_segment(SegAddr, B),
		FinalZ = SegEnd - LastAddr,
		[B | dets_utils:make_zeros(FinalZ)]
	end,
    %% Restore the positions.
    true = ets:delete_all_objects(SizeT),
    %% To get the segments copied first by dets:fsck_copy/4, use a big
    %% number here, FSCK_SEGMENT2.
    lists:foreach(fun(X) -> true = ets:insert(SizeT, X) end, 
		  [{?FSCK_SEGMENT2,SegAddr,NewData,0} | FileData1]),
    ok.
    
seg_file(Addr, SS, In, InFile, Out, OutFile, SizeT, SegEnd) ->
    case dets_utils:read_n(In, 4500) of
	eof ->
	    FinalZ = SegEnd - Addr,
	    dets_utils:fwrite(Out, OutFile, dets_utils:make_zeros(FinalZ));
	Bin ->
	    {NewAddr, L} = seg_file(Bin, Addr, SS, SizeT, []),
            dets_utils:disk_map_segment(Addr, L),
	    ok = dets_utils:fwrite(Out, OutFile, L),
	    seg_file(NewAddr, SS, In, InFile, Out, OutFile, SizeT, SegEnd)
    end.

seg_file(<<Slot:32,BSize:32,LSize:8,T/binary>>, Addr, SS, SizeT, L) ->
    seg_file_item(T, Addr, SS, SizeT, L, Slot, BSize, LSize);
seg_file([<<Slot:32,BSize:32,LSize:8>> | T], Addr, SS, SizeT, L) ->
    seg_file_item(T, Addr, SS, SizeT, L, Slot, BSize, LSize);
seg_file([], Addr, _SS, _SizeT, L) ->
    {Addr, lists:reverse(L)};
seg_file(<<>>, Addr, _SS, _SizeT, L) ->
    {Addr, lists:reverse(L)}.

seg_file_item(T, Addr, SS, SizeT, L, Slot, BSize, LSize) ->
    %% Should call slot_position/1, but since all segments are
    %% allocated in a sequence, the position of a slot can be
    %% calculated faster.
    SlotPos = SS + ?SZOBJP*4 * Slot, % SlotPos = slot_position(Slot)
    NoZeros = SlotPos - Addr,
    PSize = NoZeros+?SZOBJP*4,
    Inc = ?POW(LSize-1),
    CollP = ets:update_counter(SizeT, LSize, Inc) - Inc,
    PointerBin = if 
		     NoZeros =:= 0 ->
			 <<BSize:32, CollP:32>>;
		     NoZeros > 100 ->
			 [dets_utils:make_zeros(NoZeros) | 
			  <<BSize:32, CollP:32>>];
		     true ->
			 <<0:NoZeros/unit:8, BSize:32, CollP:32>>
                 end,
    seg_file(T, Addr + PSize, SS, SizeT, [PointerBin | L]).

temp_file(Head, SizeT, N) ->
    TmpName = lists:concat([Head#head.filename, '.', N]),
    {ok, Fd} = dets_utils:open(TmpName, [raw, binary, write]),
    %% The file table is consulted when cleaning up.
    true = ets:insert(SizeT, {N,0,{TmpName,Fd},0}),
    {TmpName, Fd}.

%% Does not close Fd.
fsck_input(Head, Fd, Cntrs, FileHeader) ->
    MaxSz0 = case FileHeader#fileheader.has_md5 of
                 true when is_list(FileHeader#fileheader.no_colls) ->
                     ?POW(max_objsize(FileHeader#fileheader.no_colls));
                 _ ->
                     %% The file is not compressed, so the bucket size
                     %% cannot exceed the filesize, for all buckets.
                     case file:position(Fd, eof) of
                         {ok, Pos} ->
                             Pos;
                         _ ->
                             1 bsl 32
                     end
             end,
    MaxSz = erlang:max(MaxSz0, ?CHUNK_SIZE),
    State0 = fsck_read(?BASE, Fd, [], 0),
    fsck_input(Head, State0, Fd, MaxSz, Cntrs).

fsck_input(Head, State, Fd, MaxSz, Cntrs) ->
    fun(close) ->
	    ok;
       (read) ->
	    case State of
		done ->
		    end_of_input;
		{done, L, _Seq} ->
		    R = count_input(L),
		    {R, fsck_input(Head, done, Fd, MaxSz, Cntrs)};
		{cont, L, Bin, Pos, Seq} ->
		    R = count_input(L),
                    FR = fsck_objs(Bin, Head#head.keypos, Head, [], Seq),
                    NewState = fsck_read(FR, Pos, Fd, MaxSz, Head),
		    {R, fsck_input(Head, NewState, Fd, MaxSz, Cntrs)}
	    end
    end.

%% The ets table Cntrs is used for counting objects per size.
count_input(L) ->
    lists:reverse(L).

fsck_read(Pos, F, L, Seq) ->
    case file:position(F, Pos) of
	{ok, _} ->
	    read_more_bytes([], 0, Pos, F, L, Seq);
	_Error ->
	    {done, L, Seq}
    end.

fsck_read({more, Bin, Sz, L, Seq}, Pos, F, MaxSz, Head) when Sz > MaxSz ->
    FR = skip_bytes(Bin, ?BUMP, Head#head.keypos, Head, L, Seq),
    fsck_read(FR, Pos, F, MaxSz, Head);
fsck_read({more, Bin, Sz, L, Seq}, Pos, F, _MaxSz, _Head) ->
    read_more_bytes(Bin, Sz, Pos, F, L, Seq);
fsck_read({new, Skip, L, Seq}, Pos, F, _MaxSz, _Head) ->
    NewPos = Pos + Skip,
    fsck_read(NewPos, F, L, Seq).
    
read_more_bytes(B, Min, Pos, F, L, Seq) ->
    Max = if 
	      Min < ?CHUNK_SIZE -> ?CHUNK_SIZE; 
	      true -> Min 
	  end,
    case dets_utils:read_n(F, Max) of
	eof ->
	    {done, L, Seq};
	Bin ->
	    NewPos = Pos + byte_size(Bin),
	    {cont, L, list_to_binary([B, Bin]), NewPos, Seq}
    end.

fsck_objs(Bin = <<Sz:32, Status:32, Tail/binary>>, Kp, Head, L, Seq) ->
    if 
	Status =:= ?ACTIVE ->
	    Sz1 = Sz-?OHDSZ,
	    case Tail of
		<<BinTerm:Sz1/binary, Tail2/binary>> ->
		    case catch bin2keybins(BinTerm, Head) of
			{'EXIT', _Reason} ->
			    %% The whole collection of objects is skipped.
			    skip_bytes(Bin, ?BUMP, Kp, Head, L, Seq);
			BOs ->
			    {NL, NSeq} = make_objects(BOs, Seq, Kp, Head, L),
			    Skip = ?POW(sz2pos(Sz)-1) - Sz,
			    skip_bytes(Tail2, Skip, Kp, Head, NL, NSeq)
		    end;
		_ when byte_size(Tail) < Sz1 ->
                    {more, Bin, Sz, L, Seq}
	    end;
	true -> 
	    skip_bytes(Bin, ?BUMP, Kp, Head, L, Seq)
    end;
fsck_objs(Bin, _Kp, _Head, L, Seq) ->
    {more, Bin, 0, L, Seq}.
    
make_objects([{K,BT} | Os], Seq, Kp, Head, L) ->
    Obj = make_object(Head, K, Seq, BT),
    make_objects(Os, Seq+1, Kp, Head, [Obj | L]);
make_objects([], Seq, _Kp, _Head, L) ->
    {L, Seq}.

%% Inlined.
make_object(Head, Key, Seq, BT) ->
    Slot = db_hash(Key, Head),
    <<Slot:32, Seq:32, BT/binary>>.

%% Inlined.
skip_bytes(Bin, Skip, Kp, Head, L, Seq) ->
    case Bin of
	<<_:Skip/binary, Tail/binary>> ->
	    fsck_objs(Tail, Kp, Head, L, Seq);
	_ when byte_size(Bin) < Skip ->
            {new, Skip - byte_size(Bin), L, Seq}
    end.

%%%
%%% End of repair, conversion and initialization of a dets file.
%%%

%% -> {NewHead, ok} | throw({Head, Error})
do_perform_save(H) ->
    {ok, FreeListsPointer} = dets_utils:position(H, eof),
    H1 = H#head{freelists_p = FreeListsPointer},
    {FLW, FLSize} = free_lists_to_file(H1),
    FileSize = FreeListsPointer + FLSize + 4,
    AdjustedFileSize = case H#head.base of
                           ?BASE -> FileSize;
                           Base -> FileSize - Base
                       end,
    ok = dets_utils:write(H1, [FLW | <<AdjustedFileSize:32>>]),
    FileHeader = file_header(H1, FreeListsPointer, ?CLOSED_PROPERLY),
    case dets_utils:debug_mode() of
        true -> 
            TmpHead0 = init_freelist(H1#head{fixed = false}),
            TmpHead = TmpHead0#head{base = H1#head.base},
            case 
                catch dets_utils:all_allocated_as_list(TmpHead)
                      =:= dets_utils:all_allocated_as_list(H1)
            of
                true -> 
                    dets_utils:pwrite(H1, [{0, FileHeader}]);
                _ -> 
                    throw(
                    dets_utils:corrupt_reason(H1, {failed_to_save_free_lists,
                                                   FreeListsPointer,
                                                   TmpHead#head.freelists,
                                                   H1#head.freelists}))
            end;
        false ->
            dets_utils:pwrite(H1, [{0, FileHeader}])
    end.

file_header(Head, FreeListsPointer, ClosedProperly) ->
    NoColls = case Head#head.no_collections of
		  undefined -> [];
		  NC -> NC
	      end,
    L = orddict:merge(fun(_K, V1, V2) -> V1 + V2 end, 
		      NoColls, 
		      lists:map(fun(X) -> {X,0} end, lists:seq(4,?MAXBUD-1))),
    CW = lists:map(fun({_LSz,N}) -> <<N:32>> end, L),
    file_header(Head, FreeListsPointer, ClosedProperly, CW).

file_header(Head, FreeListsPointer, ClosedProperly, NoColls) ->
    Cookie = ?MAGIC,
    TypeCode = dets_utils:type_to_code(Head#head.type),
    Version = ?FILE_FORMAT_VERSION,
    HashMethod = hash_method_to_code(Head#head.hash_bif),
    H1 = <<FreeListsPointer:32, Cookie:32, ClosedProperly:32>>,
    H2 = <<TypeCode:32,
           Version:32,
           (Head#head.m):32, 
           (Head#head.next):32, 
           (Head#head.keypos):32,
           (Head#head.no_objects):32,
           (Head#head.no_keys):32,
           (Head#head.min_no_slots):32,
           (Head#head.max_no_slots):32,
           HashMethod:32,
           (Head#head.n):32>>,
    DigH = [H2 | NoColls],
    MD5 = case Head#head.has_md5 of 
              true -> erlang:md5(DigH);
              false -> <<0:?MD5SZ/unit:8>>
          end,
    Base = case Head#head.base of
               ?BASE -> <<0:32>>;
               FlBase -> <<FlBase:32>>
           end,
    [H1, DigH, MD5, Base | <<0:?RESERVED/unit:8>>].

%% Going through some trouble to avoid creating one single binary for
%% the free lists. If the free lists are huge, binary_to_term and
%% term_to_binary could otherwise stop the emulator for quite some time.

-define(MAXFREEOBJ, 4096).
-define(ENDFREE, 12345).

free_lists_to_file(H) ->
    FL = dets_utils:get_freelists(H),
    free_list_to_file(FL, H, 1, tuple_size(FL), [], 0).

free_list_to_file(_Ftab, _H, Pos, Sz, Ws, WsSz) when Pos > Sz ->
    {[Ws | <<(4+?OHDSZ):32, ?FREE:32, ?ENDFREE:32>>], WsSz+4+?OHDSZ};
free_list_to_file(Ftab, H, Pos, Sz, Ws, WsSz) ->
    Max = (?MAXFREEOBJ - 4 - ?OHDSZ) div 4,
    F = fun(N, L, W, S) when N =:= 0 -> {N, L, W, S};
	   (N, L, W, S) ->
		{L1, N1, More} =
		    if 
			N > Max ->
			    {lists:sublist(L, Max), Max,
			     {N-Max, lists:nthtail(Max, L)}};
			true ->
			    {L, N, no_more}
		    end,
                Size = N1*4 + 4 + ?OHDSZ,
		Header = <<Size:32, ?FREE:32, Pos:32>>,
		NW = [W, Header | L1],
		case More of
		    no_more ->
			{0, [], NW, S+Size};
		    {NN, NL} ->
			ok = dets_utils:write(H, NW),
			{NN, NL, [], S+Size}
		end
	end,
    {NWs,NWsSz} = dets_utils:tree_to_bin(element(Pos, Ftab), F, Max, Ws, WsSz),
    free_list_to_file(Ftab, H, Pos+1, Sz, NWs, NWsSz).

free_lists_from_file(H, Pos) ->
    {ok, Pos} = dets_utils:position(H#head.fptr, H#head.filename, Pos),
    FL = dets_utils:empty_free_lists(),
    case catch bin_to_tree([], H, start, FL, -1, []) of
	{'EXIT', _} ->
	    throw({error, {bad_freelists, H#head.filename}});
        Ftab ->
            H#head{freelists = Ftab, base = ?BASE}
    end.

bin_to_tree(Bin, H, LastPos, Ftab, A0, L) ->
    case Bin of 
        <<_Size:32,?FREE:32,?ENDFREE:32,_/binary>> when L =:= [] ->
            Ftab;
        <<_Size:32,?FREE:32,?ENDFREE:32,_/binary>> ->
            setelement(LastPos, Ftab, dets_utils:list_to_tree(L));
        <<Size:32,?FREE:32,Pos:32,T/binary>> 
                      when byte_size(T) >= Size-4-?OHDSZ ->
	    {NFtab, L1, A1} = 
		if
		    Pos =/= LastPos, LastPos =/= start ->
			Tree = dets_utils:list_to_tree(L),
			{setelement(LastPos, Ftab, Tree), [], -1};
		    true ->
			{Ftab, L, A0}
		    end,
	    {NL, B2, A2} = bin_to_tree1(T, Size-?OHDSZ-4, A1, L1),
	    bin_to_tree(B2, H, Pos, NFtab, A2, NL);
        _ ->
            Bin2 = dets_utils:read_n(H#head.fptr, ?MAXFREEOBJ),
            bin_to_tree(list_to_binary([Bin | Bin2]), H, LastPos, Ftab, A0, L)
    end.

bin_to_tree1(<<A1:32,A2:32,A3:32,A4:32,T/binary>>, Size, A, L) 
         when Size >= 16, A < A1, A1 < A2, A2 < A3, A3 < A4 ->
    bin_to_tree1(T, Size-16, A4, [A4, A3, A2, A1 | L]);
bin_to_tree1(<<A1:32,T/binary>>, Size, A, L) when Size >= 4, A < A1 ->
    bin_to_tree1(T, Size - 4, A1, [A1 | L]);
bin_to_tree1(B, 0, A, L) ->
    {L, B, A}.

%% -> [term()] | throw({Head, Error})
slot_objs(H, Slot) when Slot >= H#head.next ->
    '$end_of_table';
slot_objs(H, Slot) ->
    {ok, _Pointer, Objects} = slot_objects(H, Slot),
    Objects.

%% Inlined.
h(I, phash2) -> erlang:phash2(I); % -> [0..2^27-1]
h(I, phash) -> erlang:phash(I, ?BIG) - 1.

db_hash(Key, Head) when Head#head.hash_bif =:= phash2 ->
    H = erlang:phash2(Key),
    Hash = ?REM2(H, Head#head.m),
    if
	Hash < Head#head.n ->
	    ?REM2(H, Head#head.m2); % H rem (2 * m)
	true ->
	    Hash
    end;
db_hash(Key, Head) ->
    H = h(Key, Head#head.hash_bif),
    Hash = H rem Head#head.m,
    if
	Hash < Head#head.n ->
	    H rem (Head#head.m2); % H rem (2 * m)
	true ->
	    Hash
    end.

hash_method_to_code(phash2) -> ?PHASH2;
hash_method_to_code(phash) -> ?PHASH.

code_to_hash_method(?PHASH2) -> phash2;
code_to_hash_method(?PHASH) -> phash;
code_to_hash_method(_) -> undefined.

no_slots(Head) ->
    {Head#head.min_no_slots, Head#head.next, Head#head.max_no_slots}.

table_parameters(Head) ->
    case Head#head.no_collections of
	undefined -> 
	    undefined; % Version 9(a)
	CL -> 
	    NoColls0 = lists:foldl(fun({_,0}, A) -> A;
				      (E, A) -> [E | A]
				   end, [], CL),
	    NoColls = lists:reverse(NoColls0),
	    #?HASH_PARMS{file_format_version = ?FILE_FORMAT_VERSION,
			 bchunk_format_version = ?BCHUNK_FORMAT_VERSION,
			 file = filename:basename(Head#head.filename),
			 type = Head#head.type,
			 keypos = Head#head.keypos,
			 hash_method = hash_method_to_code(Head#head.hash_bif),
			 n = Head#head.n, m = Head#head.m, 
			 next = Head#head.next, 
			 min = Head#head.min_no_slots, 
			 max = Head#head.max_no_slots, 
			 no_objects = Head#head.no_objects, 
			 no_keys = Head#head.no_keys, no_colls = NoColls}
    end.

%% Allow quite a lot when reading object collections.
-define(MAXCOLL, (10 * ?CHUNK_SIZE)).

%% Re-hashing a segment, starting with SlotStart.
%%
%% On the average, half of the keys of the slot are put in a new slot.
%% If the old slot is i, then the new slot is i+m. The new slots
%% reside in a newly allocated segment.
%%
%% -> {NewHead, ok} | throw({Head, Error})
re_hash(Head, SlotStart) ->
    FromSlotPos = slot_position(SlotStart),
    ToSlotPos = slot_position(SlotStart + Head#head.m),
    RSpec = [{FromSlotPos, 4 * ?SEGSZ}],
    {ok, [FromBin]} = dets_utils:pread(RSpec, Head),
    split_bins(FromBin, Head, FromSlotPos, ToSlotPos, [], [], 0).

split_bins(<<>>, Head, _Pos1, _Pos2, _ToRead, _L, 0) ->
    {Head, ok};
split_bins(<<>>, Head, Pos1, Pos2, ToRead, L, _SoFar) ->
    re_hash_write(Head, ToRead, L, Pos1, Pos2);
split_bins(FB, Head, Pos1, Pos2, ToRead, L, SoFar) ->
    <<Sz1:32, P1:32, FT/binary>> = FB,
    <<B1:?OHDSZ/binary, _/binary>> = FB,
    NSoFar = SoFar + Sz1,
    NPos1 = Pos1 + ?SZOBJP*4,
    NPos2 = Pos2 + ?SZOBJP*4,
    if
	NSoFar > ?MAXCOLL, ToRead =/= [] ->
	    {NewHead, ok} = re_hash_write(Head, ToRead, L, Pos1, Pos2),
	    split_bins(FB, NewHead, Pos1, Pos2, [], [], 0);
	Sz1 =:= 0 ->
	    E = {skip,B1},
	    split_bins(FT, Head, NPos1, NPos2, ToRead, [E | L], NSoFar);
        true ->
	    E = {Sz1,P1,B1,Pos1,Pos2},
	    NewToRead = [{P1,Sz1} | ToRead],
	    split_bins(FT, Head, NPos1, NPos2, NewToRead, [E | L], NSoFar)
    end.

re_hash_write(Head, ToRead, L, Pos1, Pos2) ->
    check_pread2_arg(ToRead, Head),
    {ok, Bins} = dets_utils:pread(ToRead, Head),
    Z = <<0:32, 0:32>>,
    {Head1, BinFS, BinTS, WsB} = re_hash_slots(Bins, L, Head, Z, [],[],[]),
    WPos1 = Pos1 - ?SZOBJP*4*length(L),
    WPos2 = Pos2 - ?SZOBJP*4*length(L),
    ToWrite = [{WPos1,BinFS}, {WPos2, BinTS} | WsB],
    dets_utils:pwrite(Head1, ToWrite).

re_hash_slots(Bins, [{skip,B1} | L], Head, Z, BinFS, BinTS, WsB) ->
    re_hash_slots(Bins, L, Head, Z, [B1 | BinFS], [Z | BinTS], WsB);
re_hash_slots([FB | Bins], [E | L], Head, Z, BinFS, BinTS, WsB) ->
    {Sz1,P1,B1,Pos1,Pos2} = E,
    KeyObjs = case catch per_key(Head, FB) of
		  {'EXIT', _Error} ->
                      Bad = dets_utils:bad_object(re_hash_slots, {FB, E}),
		      throw(dets_utils:corrupt_reason(Head, Bad));
		  Else ->
		      Else
	      end,
    case re_hash_split(KeyObjs, Head, [], 0, [], 0) of
	{_KL, _KSz, [], 0} ->
            Sz1 = _KSz + ?OHDSZ,
	    re_hash_slots(Bins, L, Head, Z, [B1 | BinFS], [Z | BinTS], WsB);
	{[], 0, _ML, _MSz} -> %% Optimization.
            Sz1 = _MSz + ?OHDSZ,
	    re_hash_slots(Bins, L, Head, Z, [Z | BinFS], [B1 | BinTS], WsB);
	{KL, KSz, ML, MSz} when KL =/= [], KSz > 0, ML =/= [], MSz > 0 ->
	    {Head1, FS1, Ws1} = 
		updated(Head, P1, Sz1, KSz, Pos1, KL, true, foo, bar),
	    {NewHead, [{Pos2,Bin2}], Ws2} = 
		updated(Head1, 0, 0, MSz, Pos2, ML, true, foo, bar),
	    NewBinFS = case FS1 of
			   [{Pos1,Bin1}] -> [Bin1 | BinFS];
			   [] -> [B1 | BinFS] % cannot happen
		       end,
	    NewBinTS = [Bin2 | BinTS],
	    NewWsB = Ws2 ++ Ws1 ++ WsB,
	    re_hash_slots(Bins, L, NewHead, Z, NewBinFS, NewBinTS, NewWsB)
    end;
re_hash_slots([], [], Head, _Z, BinFS, BinTS, WsB) ->
    {Head, BinFS, BinTS, lists:reverse(WsB)}.

re_hash_split([E | KeyObjs], Head, KL, KSz, ML, MSz) ->
    {Key,Sz,Bin,_Item,_Objs} = E,
    New = h(Key, Head#head.hash_bif) rem Head#head.m2, % h(key) rem (m * 2)
    if
	New >= Head#head.m ->
	    re_hash_split(KeyObjs, Head, KL, KSz, [Bin | ML], MSz + Sz);
	true ->
	    re_hash_split(KeyObjs, Head, [Bin | KL], KSz + Sz, ML, MSz)
    end;
re_hash_split([], _Head, KL, KSz, ML, MSz) ->
    {lists:reverse(KL), KSz, lists:reverse(ML), MSz}.

%% -> {NewHead, [LookedUpObject], pwrite_list()} | throw({NewHead, Error})
write_cache(Head) ->
    C = Head#head.cache,
    case dets_utils:is_empty_cache(C) of
	true -> {Head, [], []};
	false ->
	    {NewC, MaxInserts, PerKey} = dets_utils:reset_cache(C),
	    %% MaxNoInsertedKeys is an upper limit on the number of new keys.
	    MaxNoInsertedKeys = erlang:min(MaxInserts, length(PerKey)),
	    Head1 = Head#head{cache = NewC},
	    case may_grow(Head1, MaxNoInsertedKeys, once) of
		{Head2, ok} ->
		    eval_work_list(Head2, PerKey);
		HeadError ->
		    throw(HeadError)
	    end
    end.

%% -> {NewHead, ok} | {NewHead, Error}
may_grow(Head, 0, once) ->
    %% Do not re-hash if there is a chance that the file is not dirty.
    {Head, ok};
may_grow(Head, _N, _How) when Head#head.fixed =/= false ->
    {Head, ok};
may_grow(#head{access = read}=Head, _N, _How) ->
    {Head, ok};
may_grow(Head, _N, _How) when Head#head.next >= Head#head.max_no_slots ->
    {Head, ok};
may_grow(Head, N, How) ->
    Extra = erlang:min(2*?SEGSZP, Head#head.no_keys + N - Head#head.next),
    case catch may_grow1(Head, Extra, How) of
	{error, _Reason} = Error -> % alloc may throw error
	    dets_utils:corrupt(Head, Error);
	{NewHead, Reply} when is_record(Head, head) ->
	    {NewHead, Reply}
    end.

may_grow1(Head, Extra, many_times) when Extra > ?SEGSZP ->
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
grow(Head, _Extra, _SegZero) when Head#head.next >= Head#head.max_no_slots ->
    {Head, ok};
grow(Head, Extra, SegZero) ->
    #head{n = N, next = Next, m = M} = Head,
    SegNum = Next div ?SEGSZP,    
    {Head0, W, Ws1} = allocate_segment(Head, SegZero, SegNum),
    %% re_hash/2 will overwrite the segment, but initialize it anyway...
    {Head1, ok} = dets_utils:pwrite(Head0, [W | Ws1]),
    %% If re_hash fails, segp_cache has been called, but it does not matter.
    {Head2, ok} = re_hash(Head1, N),
    NewHead =
	if 
	    N + ?SEGSZP =:= M ->
		Head2#head{n = 0, next = Next + ?SEGSZP, m = 2 * M, m2 = 4 * M};
	    true ->
		Head2#head{n = N + ?SEGSZP, next = Next + ?SEGSZP}
	end,
    true = hash_invars(NewHead),
    grow(NewHead, Extra - ?SEGSZP, SegZero).

hash_invars(H) ->
    hash_invars(H#head.n, H#head.m, H#head.next, H#head.min_no_slots, 
		H#head.max_no_slots).

-define(M8(X), (((X) band (?SEGSZP - 1)) =:= 0)).
hash_invars(N, M, Next, Min, Max) ->
        ?M8(N) and ?M8(M) and ?M8(Next) and ?M8(Min) and ?M8(Max)
    and (0 =< N) and (N =< M) and (N =< 2*Next) and (M =< Next)
    and (Next =< 2*M) and (0 =< Min) and (Min =< Next) and (Next =< Max) 
    and (Min =< M).

seg_zero() ->
    <<0:(4*?SEGSZ)/unit:8>>.

find_object(Head, Object) ->
    Key = element(Head#head.keypos, Object),
    Slot = db_hash(Key, Head),
    find_object(Head, Object, Slot).    

find_object(H, _Obj, Slot) when Slot >= H#head.next ->
    false;
find_object(H, Obj, Slot) ->
    case catch slot_objects(H, Slot) of
	{ok, Pointer, Objects} ->
	    case lists:member(Obj, Objects) of
		true -> {ok, Pointer};
		false -> false
	    end;
	_ -> false
    end.

%% -> {ok, BucketP, Objects} | throw({Head, Error})
slot_objects(Head, Slot) ->
    SlotPos = slot_position(Slot),    
    MaxSize = maxobjsize(Head),
    case dets_utils:ipread(Head, SlotPos, MaxSize) of 
	{ok, {BucketSz, Pointer, <<BucketSz:32, _St:32, KeysObjs/binary>>}} ->
	    case catch bin2objs(KeysObjs, Head#head.type, []) of
		{'EXIT', _Error} ->
                    Bad = dets_utils:bad_object(slot_objects, 
                                                {SlotPos, KeysObjs}),
		    throw(dets_utils:corrupt_reason(Head, Bad));
		Objs when is_list(Objs) ->
		    {ok, Pointer, lists:reverse(Objs)}
	    end;
        [] ->
	    {ok, 0, []};
	BadRead -> % eof or bad badly formed binary
            Bad = dets_utils:bad_object(slot_objects, {SlotPos, BadRead}),
	    throw(dets_utils:corrupt_reason(Head, Bad))
    end.

%%%
%%% Cache routines depending on the dets file format.
%%%

%% -> {Head, [LookedUpObject], pwrite_list()} | throw({Head, Error})
eval_work_list(Head, [{Key,[{_Seq,{lookup,Pid}}]}]) ->
    SlotPos = slot_position(db_hash(Key, Head)),
    MaxSize = maxobjsize(Head),
    Objs = case dets_utils:ipread(Head, SlotPos, MaxSize) of 
	       {ok, {_BucketSz, _Pointer, Bin}} ->
                   case catch per_key(Head, Bin) of
                       {'EXIT', _Error} ->
                           Bad = dets_utils:bad_object(eval_work_list,
                                                       {SlotPos, Bin}),
                           throw(dets_utils:corrupt_reason(Head, Bad));
                       KeyObjs when is_list(KeyObjs) ->
                           case dets_utils:mkeysearch(Key, 1, KeyObjs) of
                               false ->
                                   [];
                               {value, {Key,_KS,_KB,O,Os}} ->
                                   case catch binobjs2terms(Os) of
                                       {'EXIT', _Error} ->
                                           Bad = dets_utils:bad_object
                                                   (eval_work_list,
                                                    {SlotPos, Bin, KeyObjs}),
                                           throw(dets_utils:corrupt_reason
                                                      (Head, Bad));
                                       Terms when is_list(Terms) -> 
                                           get_objects([O | Terms])
                                   end
                           end
                   end;
	       [] ->
		   [];
	       BadRead -> % eof or bad badly formed binary
                   Bad = dets_utils:bad_object(eval_work_list, 
                                               {SlotPos, BadRead}),
		   throw(dets_utils:corrupt_reason(Head, Bad))
	   end,
    {Head, [{Pid,Objs}], []};
eval_work_list(Head, PerKey) ->
    SWLs = tag_with_slot(PerKey, Head, []),
    P1 = dets_utils:family(SWLs),
    {PerSlot, SlotPositions} = remove_slot_tag(P1, [], []),
    {ok, Bins} = dets_utils:pread(SlotPositions, Head),
    read_buckets(PerSlot, SlotPositions, Bins, Head, [], [], [], [], 0, 0, 0).

tag_with_slot([{K,_} = WL | WLs], Head, L) ->
    tag_with_slot(WLs, Head, [{db_hash(K, Head), WL} | L]);
tag_with_slot([], _Head, L) ->
    L.

remove_slot_tag([{S,SWLs} | SSWLs], Ls, SPs) ->
    remove_slot_tag(SSWLs, [SWLs | Ls], [{slot_position(S), ?SEGOBJSZ} | SPs]);
remove_slot_tag([], Ls, SPs) ->
    {Ls, SPs}.

read_buckets([WLs | SPs], [{P1,_8} | Ss], [<<_Zero:32,P2:32>> | Bs], Head,
	      PWLs, ToRead, LU, Ws, NoObjs, NoKeys, SoFar) when P2 =:= 0 ->
    {NewHead, NLU, NWs, No, KNo} = 
	eval_bucket_keys(WLs, P1, 0, 0, [], Head, Ws, LU),
    NewNoObjs = No + NoObjs,
    NewNoKeys = KNo + NoKeys,
    read_buckets(SPs, Ss, Bs, NewHead, PWLs, ToRead, NLU, NWs, 
		 NewNoObjs, NewNoKeys, SoFar);
read_buckets([WorkLists| SPs], [{P1,_8} | Ss], [<<Size:32,P2:32>> | Bs], Head,
	     PWLs, ToRead, LU, Ws, NoObjs, NoKeys, SoFar) 
                                 when SoFar + Size < ?MAXCOLL; ToRead =:= [] ->
    NewToRead = [{P2, Size} | ToRead],
    NewPWLs = [{P2,P1,WorkLists} | PWLs],
    NewSoFar = SoFar + Size,
    read_buckets(SPs, Ss, Bs, Head, NewPWLs, NewToRead, LU, Ws, 
		 NoObjs, NoKeys, NewSoFar);
read_buckets(SPs, Ss, Bs, Head, PWLs0, ToRead0, LU, Ws, NoObjs, NoKeys, SoFar) 
                                                             when SoFar > 0 ->
    %% It pays off to sort the positions. The seek times are reduced,
    %% at least if the file blocks are reasonably contiguous, as is
    %% often the case.
    PWLs = lists:keysort(1, PWLs0),
    ToRead = lists:keysort(1, ToRead0),
    check_pread2_arg(ToRead, Head),
    {ok, Bins} = dets_utils:pread(ToRead, Head),
    case catch eval_buckets(Bins, PWLs, Head, LU, Ws, 0, 0) of
	{ok, NewHead, NLU, [], 0, 0} -> 
            read_buckets(SPs, Ss, Bs, NewHead, [], [], NLU, [], 
			 NoObjs, NoKeys, 0);
	{ok, Head1, NLU, NWs, No, KNo} -> 
	    NewNoObjs = NoObjs + No,
	    NewNoKeys = NoKeys + KNo,
	    %% It does not seem to reduce seek times to sort positions
	    %% when writing (maybe because it takes several calls to
	    %% write_cache/1 to fill the file system's buffer cache).
            {NewHead, ok} = dets_utils:pwrite(Head1, lists:reverse(NWs)),
            read_buckets(SPs, Ss, Bs, NewHead, [], [], NLU, [], 
			 NewNoObjs, NewNoKeys, 0);
	Error  -> 
            Bad = dets_utils:bad_object(read_buckets, {Bins, Error}),
	    throw(dets_utils:corrupt_reason(Head, Bad))
    end;
read_buckets([], [], [], Head, [], [], LU, Ws, NoObjs, NoKeys, 0) ->
    {NewHead, NWs} = update_no_keys(Head, Ws, NoObjs, NoKeys),
    {NewHead, LU, lists:reverse(NWs)}.

eval_buckets([Bin | Bins], [SP | SPs], Head, LU, Ws, NoObjs, NoKeys) ->
    {Pos, P1, WLs} = SP,
    KeyObjs = per_key(Head, Bin),
    {NewHead, NLU, NWs, No, KNo} = 
	eval_bucket_keys(WLs, P1, Pos, byte_size(Bin), KeyObjs, Head,Ws,LU),
    eval_buckets(Bins, SPs, NewHead, NLU, NWs, NoObjs + No, NoKeys + KNo);
eval_buckets([], [], Head, LU, Ws, NoObjs, NoKeys) ->
    {ok, Head, LU, Ws, NoObjs, NoKeys}.

eval_bucket_keys(WLs, SlotPos, Pos, OldSize, KeyObjs, Head, Ws, LU) ->
    {NLU, Bins, BSize, No, KNo, Ch} = 
         eval_slot(WLs, KeyObjs, Head#head.type, LU, [], 0, 0, 0, false),
    {NewHead, W1, W2} = 
	updated(Head, Pos, OldSize, BSize, SlotPos, Bins, Ch, No, KNo),
    {NewHead, NLU, W2++W1++Ws, No, KNo}.

updated(Head, Pos, OldSize, BSize, SlotPos, Bins, Ch, DeltaNoOs, DeltaNoKs) ->
    BinsSize = BSize + ?OHDSZ,
    if 
	Pos =:= 0, BSize =:= 0 ->
	    {Head, [], []};
	Pos =:= 0, BSize > 0 ->
	    {Head1, NewPos, FPos} = dets_utils:alloc(Head, adjsz(BinsSize)),
            NewHead = one_bucket_added(Head1, FPos-1),
	    W1 = {NewPos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
	    W2 = {SlotPos, <<BinsSize:32, NewPos:32>>},
	    {NewHead, [W2], [W1]};
	Pos =/= 0, BSize =:= 0 ->
	    {Head1, FPos} = dets_utils:free(Head, Pos, adjsz(OldSize)),
            NewHead = one_bucket_removed(Head1, FPos-1),
	    W1 = {Pos+?STATUS_POS, <<?FREE:32>>},
	    W2 = {SlotPos, <<0:32, 0:32>>},
	    {NewHead, [W2], [W1]};
	Pos =/= 0, BSize > 0, Ch =:= false ->
	    {Head, [], []};
	Pos =/= 0, BSize > 0 ->
	    %% Doubtful. The scan function has to be careful since
	    %% partly scanned objects may be overwritten.
	    Overwrite0 = if
			     OldSize =:= BinsSize -> same;
			     true -> sz2pos(OldSize) =:= sz2pos(BinsSize)
			 end,
            Overwrite = if 
			    Head#head.fixed =/= false ->
				%% Make sure that if the table is
				%% fixed, nothing is overwritten,
				%% unless the number of objects and
				%% the number of keys remain the same.
				%% This is used by bchunk, which
				%% assumes that it traverses exactly
				%% the same number of objects and keys
				%% (and collections) as were present
				%% when chunking started (the table
				%% must have been fixed).
				(Overwrite0 =/= false) and 
				(DeltaNoOs =:= 0) and (DeltaNoKs =:= 0);
			    true ->
				Overwrite0
			end,
	    if 
		Overwrite =:= same ->
		    W1 = {Pos+?OHDSZ, Bins},
		    {Head, [], [W1]};
		Overwrite ->
		    W1 = {Pos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
		    %% Pos is already there, but return {SlotPos, <8 bytes>}.
		    W2 = {SlotPos, <<BinsSize:32, Pos:32>>},
		    {Head, [W2], [W1]};
		true ->
		    {Head1, FPosF} = dets_utils:free(Head, Pos, adjsz(OldSize)),
		    {Head2, NewPos, FPosA} = 
			dets_utils:alloc(Head1, adjsz(BinsSize)),
                    Head3 = one_bucket_added(Head2, FPosA-1),
                    NewHead = one_bucket_removed(Head3, FPosF-1),
		    W0 = {NewPos, [<<BinsSize:32, ?ACTIVE:32>> | Bins]},
		    W2 = {SlotPos, <<BinsSize:32, NewPos:32>>},
		    W1 = if 
			      Pos =/= NewPos ->
                                  %% W0 first.
				  [W0, {Pos+?STATUS_POS, <<?FREE:32>>}];
			      true -> 
				  [W0]
			  end,
		    {NewHead, [W2], W1}
	    end
    end.

one_bucket_added(H, _Log2) when H#head.no_collections =:= undefined ->
    H;
one_bucket_added(H, Log2) when H#head.maxobjsize >= Log2 ->
    NewNoColls = orddict:update_counter(Log2, 1, H#head.no_collections),
    H#head{no_collections = NewNoColls};
one_bucket_added(H, Log2) ->
    NewNoColls = orddict:update_counter(Log2, 1, H#head.no_collections),
    H#head{no_collections = NewNoColls, maxobjsize = Log2}.

one_bucket_removed(H, _FPos) when H#head.no_collections =:= undefined ->
    H;
one_bucket_removed(H, Log2) when H#head.maxobjsize > Log2 ->
    NewNoColls = orddict:update_counter(Log2, -1, H#head.no_collections),
    H#head{no_collections = NewNoColls};
one_bucket_removed(H, Log2) when H#head.maxobjsize =:= Log2 ->
    NewNoColls = orddict:update_counter(Log2, -1, H#head.no_collections),
    MaxObjSize = max_objsize(NewNoColls),
    H#head{no_collections = NewNoColls, maxobjsize = MaxObjSize}.    

eval_slot([{Key,Commands} | WLs] = WLs0, [{K,KS,KB,O,Os} | KOs1]=KOs,
          Type, LU, Ws, No, KNo,BSz, Ch) ->
    case dets_utils:cmp(K, Key) of
        0 ->
            Old = [O | binobjs2terms(Os)],
            {NLU, NWs, Sz, No1, KNo1, NCh} = 
		eval_key(Key, Commands, Old, Type, KB, KS, LU, Ws, Ch),
            eval_slot(WLs, KOs1, Type, NLU, NWs, No1 + No, 
		      KNo1 + KNo, Sz + BSz, NCh);
        -1 ->
            eval_slot(WLs0, KOs1, Type, LU, [Ws | KB], No, 
		      KNo, KS + BSz, Ch);
        1 ->
            {NLU, NWs, Sz, No1, KNo1, NCh} = 
		eval_key(Key, Commands, [], Type, [], 0, LU, Ws, Ch),
            eval_slot(WLs, KOs, Type, NLU, NWs, No1 + No, 
		      KNo1 + KNo, Sz + BSz, NCh)
    end;
eval_slot([{Key,Commands} | WLs], [], Type, LU, Ws, No, KNo,BSz, Ch) ->
    {NLU, NWs, Sz, No1, KNo1, NCh} = 
        eval_key(Key, Commands, [], Type, [], 0, LU, Ws, Ch),
    eval_slot(WLs, [], Type, NLU, NWs, No1 + No, KNo1 + KNo, Sz + BSz, NCh);
eval_slot([], [{_Key,Size,KeyBin,_,_} | KOs], Type, LU, Ws, No, KNo,BSz, Ch) ->
    eval_slot([], KOs, Type, LU, [Ws | KeyBin], No, KNo, Size + BSz, Ch);
eval_slot([], [], _Type, LU, Ws, No, KNo, BSz, Ch) ->
    {LU, Ws, BSz, No, KNo, Ch}.

eval_key(_K, [{_Seq,{lookup,Pid}}], [], _Type, _KeyBin, _KeySz, LU, Ws, Ch) ->
    NLU = [{Pid, []} | LU],
    {NLU, Ws, 0, 0, 0, Ch};    
eval_key(_K, [{_Seq,{lookup,Pid}}], Old0, _Type, KeyBin, KeySz, LU, Ws, Ch) ->
    Old = lists:keysort(2, Old0), % sort on sequence number
    Objs = get_objects(Old),
    NLU = [{Pid, Objs} | LU],
    {NLU, [Ws | KeyBin], KeySz, 0, 0, Ch};    
eval_key(K, Comms, Orig, Type, KeyBin, KeySz, LU, Ws, Ch) ->
    Old = dets_utils:msort(Orig),
    case eval_key1(Comms, [], Old, Type, K, LU, Ws, 0, Orig) of
	{ok, NLU} when Old =:= [] ->
	    {NLU, Ws, 0, 0, 0, Ch};
	{ok, NLU} ->
	    {NLU, [Ws | KeyBin], KeySz, 0, 0, Ch};
	{NLU, NWs, NSz, No} when Old =:= [], NSz > 0 ->
	    {NLU, NWs, NSz, No, 1, true};
	{NLU, NWs, NSz, No} when Old =/= [], NSz =:= 0 ->
	    {NLU, NWs, NSz, No, -1, true};
	{NLU, NWs, NSz, No} ->
	    {NLU, NWs, NSz, No, 0, true}
    end.

%% First find 'delete_key' and 'lookup' commands, and handle the 'set' type.
eval_key1([{_Seq,{insert,Term}} | L], Cs, [{Term,_,_}] = Old, Type=set, K,
	  LU, Ws, No, Orig) ->
    eval_key1(L, Cs, Old, Type, K, LU, Ws, No, Orig);
eval_key1([{Seq,{insert,Term}} | L], Cs, Old, Type=set, K, LU, Ws, No, Orig) 
                                         ->
    NNo = No + 1 - length(Old),
    eval_key1(L, Cs, [{Term,Seq,insert}], Type, K, LU, Ws, NNo, Orig);
eval_key1([{_Seq,{lookup,Pid}} | L], Cs, Old, Type, Key, LU, Ws, No, Orig) ->
    {ok, New0, NewNo} = eval_comms(Cs, Old, Type, No),
    New = lists:keysort(2, New0), % sort on sequence number
    Objs = get_objects(New),
    NLU = [{Pid, Objs} | LU],
    if 
	L =:= [] ->
	    eval_end(New, NLU, Type, Ws, NewNo, Orig);
	true ->
	    NewOld = dets_utils:msort(New),
	    eval_key1(L, [], NewOld, Type, Key, NLU, Ws, NewNo, Orig)
    end;
eval_key1([{_Seq,delete_key} | L], _Cs, Old, Type, K, LU, Ws, No, Orig) ->
    NewNo = No - length(Old),
    eval_key1(L, [], [], Type, K, LU, Ws, NewNo, Orig);
eval_key1([{_Seq,{delete_object,Term}} | L], Cs, [{Term,_,_}], Type=set, K,
	  LU, Ws, No, Orig) ->
    eval_key1(L, Cs, [], Type, K, LU, Ws, No-1, Orig);
eval_key1([{_Seq,{delete_object,_T}}| L], Cs, Old1, Type=set, K, LU, 
	      Ws, No, Orig) ->
    eval_key1(L, Cs, Old1, Type, K, LU, Ws, No, Orig);
eval_key1([{Seq,{Comm,Term}} | L], Cs, Old, Type, K, LU, Ws, No, Orig) 
                                         when Type =/= set ->
    eval_key1(L, [{Term,Seq,Comm} | Cs], Old, Type, K, LU, Ws, No, Orig);
eval_key1([], Cs, Old, Type=set, _Key, LU, Ws, No, Orig) ->
    [] = Cs,
    eval_end(Old, LU, Type, Ws, No, Orig);
eval_key1([], Cs, Old, Type, _Key, LU, Ws, No, Orig) ->
    {ok, New, NewNo} = eval_comms(Cs, Old, Type, No),
    eval_end(New, LU, Type, Ws, NewNo, Orig).

eval_comms([], L, _Type=set, No) ->
    {ok, L, No};
eval_comms(Cs, Old, Type, No) ->
    Commands = dets_utils:msort(Cs),
    case Type of
	bag -> eval_bag(Commands, Old, [], No);
	duplicate_bag -> eval_dupbag(Commands, Old, [], No)
    end.

eval_end(New0, LU, Type, Ws, NewNo, Orig) ->
    New = lists:keysort(2, New0), % sort on sequence number
    NoChange = if
		   length(New) =/= length(Orig) -> false;
		   true -> 
		       same_terms(Orig, New)
	       end,
    if 
        NoChange ->
	    %% The key's objects have not changed.
	    {ok, LU};
	New =:= [] ->
	    {LU, Ws, 0, NewNo};
	true -> 
	    {Ws1, Sz} = make_bins(New, [], 0),
	    if 
		Type =:= set ->
		    {LU, [Ws | Ws1], Sz, NewNo};
		true -> 
		    NSz = Sz + 4,
		    {LU, [Ws, <<NSz:32>> | Ws1], NSz, NewNo}
	    end
    end.

same_terms([E1 | L1], [E2 | L2]) when element(1, E1) =:= element(1, E2) ->
    same_terms(L1, L2);
same_terms([], []) ->
    true;
same_terms(_L1, _L2) ->
    false.

make_bins([{_Term,_Seq,B} | L], W, Sz) when is_binary(B) ->
    make_bins(L, [W | B], Sz + byte_size(B));
make_bins([{Term,_Seq,insert} | L], W, Sz) ->
    B = term_to_binary(Term),
    BSize = byte_size(B) + 4,
    make_bins(L, [W, [<<BSize:32>> | B]], Sz + BSize);
make_bins([], W, Sz) ->
    {W, Sz}.

get_objects([{T,_S,_BT} | L]) ->
    [T | get_objects(L)];
get_objects([]) ->
    [].

eval_bag([{Term1,_S1,Op}=N | L]=L0, [{Term2,_,_}=O | Old]=Old0, New, No) ->
    case {Op, dets_utils:cmp(Term1, Term2)} of
        {delete_object, -1} ->
            eval_bag(L, Old0, New, No);
        {insert, -1} ->
            bag_object(L, Old0, New, No, [N], Term1);
        {delete_object, 0} ->
            bag_object(L, Old, New, No-1, [], Term1);
        {insert, 0} ->
            bag_object(L, Old, New, No-1, [N], Term1);
        {_, 1} ->
            eval_bag(L0, Old, [O | New], No)
    end;
eval_bag([{_Term1,_Seq1,delete_object} | L], []=Old, New, No) ->
    eval_bag(L, Old, New, No);
eval_bag([{Term,_Seq1,insert} = N | L], []=Old, New, No) ->
    bag_object(L, Old, New, No, [N], Term);
eval_bag([]=L, [O | Old], New, No) ->
    eval_bag(L, Old, [O | New], No);
eval_bag([], [], New, No) ->
    {ok, New, No}.

bag_object([{Term,_,insert} = N | L], Old, New, No, _N, Term) ->
    bag_object(L, Old, New, No, [N], Term);
bag_object([{Term,_,delete_object} | L], Old, New, No, _N, Term) ->
    bag_object(L, Old, New, No, [], Term);
bag_object(L, Old, New, No, [], _Term) ->
    eval_bag(L, Old, New, No);
bag_object(L, Old, New, No, [N], _Term) ->
    eval_bag(L, Old, [N | New], No+1).

eval_dupbag([{Term1,_S1,Op}=N | L]=L0, [{Term2,_,_}=O | Old]=Old0, New, No) ->
    case {Op, dets_utils:cmp(Term1, Term2)} of
        {delete_object, -1} ->
            eval_dupbag(L, Old0, New, No);
        {insert, -1} ->
            dup_object(L, Old0, New, No+1, Term1, [N]);
        {_, 0} ->
            old_dup_object(L0, Old, New, No, Term1, [O]);
        {_, 1} ->
            eval_dupbag(L0, Old, [O | New], No)
    end;
eval_dupbag([{_Term1,_Seq1,delete_object} | L], []=Old, New, No) ->
    eval_dupbag(L, Old, New, No);
eval_dupbag([{Term,_Seq1,insert} = N | L], []=Old, New, No) ->
    dup_object(L, Old, New, No+1, Term, [N]);
eval_dupbag([]=L, [O | Old], New, No) ->
    eval_dupbag(L, Old, [O | New], No);
eval_dupbag([], [], New, No) ->
    {ok, New, No}.
    
old_dup_object(L, [{Term,_,_} = Obj | Old], New, No, Term, N) ->
    old_dup_object(L, Old, New, No, Term, [Obj | N]);
old_dup_object(L, Old, New, No, Term, N) ->
    dup_object(L, Old, New, No, Term, N).

dup_object([{Term,_,insert} = Obj | L], Old, New, No, Term, Q) ->
    dup_object(L, Old, New, No+1, Term, [Obj | Q]);
dup_object([{Term,_Seq,delete_object} | L], Old, New, No, Term, Q) ->
    %% All objects are deleted.
    NewNo = No - length(Q),
    dup_object(L, Old, New, NewNo, Term, []);
dup_object(L, Old, New, No, _Term, Q) ->
    eval_dupbag(L, Old, Q ++ New, No).

%% Update no_keys on the file too, if the number of segments that
%% dets:fsck/6 uses for estimate has changed.
update_no_keys(Head, Ws, 0, 0) -> {Head, Ws};
update_no_keys(Head, Ws, DeltaObjects, DeltaKeys) ->
    NoKeys = Head#head.no_keys,
    NewNoKeys = NoKeys + DeltaKeys,
    NewNoObject = Head#head.no_objects + DeltaObjects,
    NewHead = Head#head{no_objects = NewNoObject, no_keys = NewNoKeys},
    NWs = 
	if 
	    NewNoKeys > NewHead#head.max_no_slots ->
		Ws;
	    NoKeys div ?SEGSZP =:= NewNoKeys div ?SEGSZP ->
		Ws;
	    true ->
                [{0, file_header(NewHead, 0, ?NOT_PROPERLY_CLOSED)} | Ws]
	end,
    {NewHead, NWs}.

slot_position(S) ->
    SegNo = ?SLOT2SEG(S), % S div ?SEGSZP
    PartPos = ?SEGARRADDR(?SEG2SEGARRPART(SegNo)), % SegNo div ?SEGPARTSZ
    Part = get_arrpart(PartPos),
    Pos = ?SEGPARTADDR(Part, SegNo),
    get_segp(Pos) + (?SEGOBJSZ * ?REM2(S, ?SEGSZP)).

check_pread2_arg([{_Pos,Sz}], Head) when Sz > ?MAXCOLL ->
    case check_pread_arg(Sz, Head) of
        true -> 
            ok;
        false ->
            Bad = dets_utils:bad_object(check_pread2_arg, Sz),
            throw(dets_utils:corrupt_reason(Head, Bad))
    end;
check_pread2_arg(_ToRead, _Head) ->
    ok.

check_pread_arg(Sz, Head) when Sz > ?MAXCOLL ->
    maxobjsize(Head) >= Sz;
check_pread_arg(_Sz, _Head) ->
    true.

%% Inlined.
segp_cache(Pos, Segment) ->
    put(Pos, Segment).

%% Inlined.
get_segp(Pos) ->
    get(Pos).

arrpart_cache(Pos, ArrPart) ->
    put(Pos, ArrPart).

%% Inlined.
get_arrpart(Pos) ->
    get(Pos).

sz2pos(N) ->
    1 + dets_utils:log2(N).

%% Inlined. Compensates for the bug in dets_utils:sz2pos/1.
adjsz(N) ->
    N-1.

%% Inlined.
maxobjsize(Head) when Head#head.maxobjsize =:= undefined ->
    ?POW(32);
maxobjsize(Head) ->
    ?POW(Head#head.maxobjsize).

scan_objs(Head, Bin, From, To, L, Ts, R, Type) ->
    case catch scan_skip(Bin, From, To, L, Ts, R, Type, 0) of
	{'EXIT', _Reason} ->
	    bad_object;
        Reply = {more, _From1, _To, _L, _Ts, _R, Size} when Size > ?MAXCOLL ->
            case check_pread_arg(Size, Head) of
                true -> Reply;
                false -> bad_object
            end;
	Reply ->
	    Reply
    end.

scan_skip(Bin, From, To, L, Ts, R, Type, Skip) ->
    From1 = From + Skip,
    case Bin of
        _ when From1 >= To ->
            if
                From1 > To; L =:= <<>> ->
                    {more, From1, To, L, Ts, R, 0};
		true ->
		    <<From2:32, To1:32, L1/binary>> = L,
		    Skip1 = From2 - From,
		    scan_skip(Bin, From, To1, L1, Ts, R, Type, Skip1)
	    end;
	<<_:Skip/binary, _Size:32, St:32, _Sz:32, KO/binary>> 
	                 when St =/= ?ACTIVE, St =/= ?FREE ->
	    %% Neither ?ACTIVE nor ?FREE is a multiple of ?BUMP and
	    %% thus cannot be found in segments or segment array
	    %% parts.
	    scan_skip(KO, From1+12, To, L, Ts, R, Type, ?ACTUAL_SEG_SIZE-12);
	<<_:Skip/binary, Size:32, _St:32, Sz:32, KO/binary>>
	                 when Size-12 =< byte_size(KO) ->
	    %% St = ?FREE means that the object was deleted after
	    %% scanning started
	    bin2bins(KO, From1+12, To, L, Ts, R, Type, Size, Sz);
	<<_:Skip/binary, Size:32, _St:32, _Sz:32, _KO/binary>> ->
	    {more, From1, To, L, Ts, R, Size};
	_ when Skip >= 0 ->
	    {more, From1, To, L, Ts, R, 0}
    end.

%% Appends objects in reversed order. All objects of the slot are
%% extracted. Note that binary_to_term/1 ignores garbage at the end.
bin2bins(Bin, From, To, L, Ts, R, Type=set, Size, ObjSz0) ->
    ObjsSz1 = Size - ObjSz0,
    if
	ObjsSz1 =:= ?OHDSZ ->
	    slot_end(Bin, From, To, L, [Bin | Ts], R, Type, Size, 1);
	true ->
	    ObjSz = ObjSz0-4,
	    <<_:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
	    bins_set(T, From, To, L, [Bin | Ts], R, Type, Size, 2, 
		     NObjSz, ObjsSz1-NObjSz, Bin)
    end;
bin2bins(<<ObjSz:32, Bin/binary>> = KO, From, To, L, Ts, R, Type, Size, Sz) ->
    bins_bag(Bin, From, To, L, Ts, R, Type, Size, 1, 
	     Sz-ObjSz-4, ObjSz-4, Size-Sz, KO).

bins_set(Bin, From, To, L, Ts, R, Type, Size, NoObjs, _ObjSz0, ?OHDSZ, KO) ->
    slot_end(KO, From, To, L, [Bin | Ts], R, Type, Size, NoObjs);
bins_set(Bin, From, To, L, Ts, R, Type, Size, NoObjs, ObjSz0, ObjsSz, KO) ->
    ObjSz = ObjSz0 - 4,
    <<_:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    bins_set(T, From, To, L, [Bin | Ts], R, Type, Size, NoObjs + 1, 
	     NObjSz, ObjsSz-NObjSz, KO).

bins_bag(Bin, From, To, L, Ts, R, Type, Size, NoObjs, Sz, ObjSz, ObjsSz, KO) 
       when Sz > 0 ->
    <<_:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    bins_bag(T, From, To, L, [Bin | Ts], R, Type, Size, NoObjs + 1, 
	     Sz-NObjSz, NObjSz-4, ObjsSz, KO);
bins_bag(Bin, From, To, L, Ts, R, Type, Size, NoObjs, _Z, _ObjSz, ?OHDSZ, KO) ->
    slot_end(KO, From, To, L, [Bin | Ts], R, Type, Size, NoObjs);
bins_bag(Bin, From, To, L, Ts, R, Type, Size, NoObjs, _Z, ObjSz, ObjsSz, KO) ->
    <<_:ObjSz/binary, Sz:32, NObjSz:32, T/binary>> = Bin,
    bins_bag(T, From, To, L, [Bin | Ts], R, Type, Size, NoObjs + 1, 
	     Sz-NObjSz-4, NObjSz-4, ObjsSz-Sz, KO).

slot_end(KO, From, To, L, Ts, R, Type, Size, NoObjs) ->
    Skip = ?POW(dets_utils:log2(Size)) - 12, % expensive...
    if
	R >= 0 ->
	    scan_skip(KO, From, To, L, Ts, R+Size, Type, Skip);
	true ->
	    %% Should check this at the end of every key.
	    case R + NoObjs of
		R1 when R1 >= -1 ->
		    From1 = From + Skip,
                    Bin1 = case KO of
                               <<_:Skip/binary, B/binary>> -> B;
                               _ -> <<>>
                           end,
                    {stop, Bin1, From1, To, L, Ts};
		R1 ->
		    scan_skip(KO, From, To, L, Ts, R1, Type, Skip)
	    end
    end.

%%%%%%%%%%%%%%%%%  DEBUG functions %%%%%%%%%%%%%%%%

file_info(FH) ->
    #fileheader{closed_properly = CP, keypos = Kp,
                m = M, next = Next, n = N, version = Version,
                type = Type, no_objects = NoObjects, no_keys = NoKeys} 
        = FH,
    if
        CP =:= 0 ->
            {error, not_closed};
        FH#fileheader.cookie =/= ?MAGIC ->
            {error, not_a_dets_file};
        FH#fileheader.version =/= ?FILE_FORMAT_VERSION ->
            {error, bad_version};
        true ->
            {ok, [{closed_properly,CP},{keypos,Kp},{m, M},{n,N},
		  {next,Next},{no_objects,NoObjects},{no_keys,NoKeys},
                  {type,Type},{version,Version}]}
    end.

v_segments(#head{}=H) ->
    v_parts(H, 0, 0).

v_parts(_H, ?SEGARRSZ, _SegNo) ->
    done;
v_parts(H, PartNo, SegNo) ->
    Fd = H#head.fptr,
    PartPos = dets_utils:read_4(Fd, ?SEGARRADDR(PartNo)),
    if
	PartPos =:= 0 ->
	    done;
	true ->
	    PartBin = dets_utils:pread_n(Fd, PartPos, ?SEGPARTSZ*4),
	    v_segments(H, PartBin, PartNo+1, SegNo)
    end.

v_segments(H, <<>>, PartNo, SegNo) ->
    v_parts(H, PartNo, SegNo);
v_segments(_H, <<0:32,_/binary>>, _PartNo, _SegNo) ->
    done;
v_segments(H, <<Seg:32,T/binary>>, PartNo, SegNo) ->
    io:format("<~w>SEGMENT ~w~n", [Seg, SegNo]),
    v_segment(H, SegNo, Seg, 0),
    v_segments(H, T, PartNo, SegNo+1).

v_segment(_H, _, _SegPos, ?SEGSZP) ->
    done;
v_segment(H, SegNo, SegPos, SegSlot) ->
    Slot = SegSlot + (SegNo * ?SEGSZP),
    BucketP = SegPos + (4 * ?SZOBJP * SegSlot),
    case catch read_bucket(H, BucketP, H#head.type) of
	{'EXIT', Reason} -> 
	    dets_utils:vformat("** dets: Corrupt or truncated dets file~n", 
			       []), 
	    io:format("~nERROR ~tp~n", [Reason]);
	[] ->  %% don't print empty buckets
	    true;
	{Size, CollP, Objects} ->
	    io:format("   <~w>~w: <~w:~p>~w~n", 
		      [BucketP, Slot, CollP, Size, Objects])
    end,
    v_segment(H, SegNo, SegPos, SegSlot+1).

%% -> [] | {Pointer, [object()]} | throw(EXIT)
read_bucket(Head, Position, Type) ->
    MaxSize = maxobjsize(Head),
    case dets_utils:ipread(Head, Position, MaxSize) of
	{ok, {Size, Pointer, <<Size:32, _Status:32, KeysObjs/binary>>}} ->
	    Objs = bin2objs(KeysObjs, Type, []),
	    {Size, Pointer, lists:reverse(Objs)};
	[] ->
	    []
    end.

-define(SEQSTART, -(1 bsl 26)).

%% -> [{Key,SizeOfWholeKey,WholeKeyBin,FirstObject,OtherObjects}] |throw(EXIT)
%% FirstObject = {Term, Seq, Binary}
%% Seq < 0 (and ascending).
per_key(Head, <<BinSize:32, ?ACTIVE:32, Bin/binary>> = B) ->
    true = (byte_size(B) =:= BinSize),
    if 
	Head#head.type =:= set ->
	    per_set_key(Bin, Head#head.keypos, []);
	true ->
	    per_bag_key(Bin, Head#head.keypos, [])
    end.

per_set_key(<<Size:32, T/binary>> = B, KeyPos, L) ->
    <<KeyBin:Size/binary, R/binary>> = B,
    Term = binary_to_term(T),
    Key = element(KeyPos, Term),
    Item = {Term, ?SEQSTART, KeyBin},
    per_set_key(R, KeyPos, [{Key,Size,KeyBin,Item,[]} | L]);
per_set_key(<<>>, KeyPos, L) when is_integer(KeyPos) ->
    lists:reverse(L).

per_bag_key(<<Size:32, ObjSz:32, T/binary>> = B, KeyPos, L) ->
    <<KeyBin:Size/binary, R/binary>> = B,
    ObjSz1 = ObjSz - 4, 
    Size1 = Size - ObjSz - 4,
    <<_:ObjSz1/binary, KeyObjs:Size1/binary, _/binary>> = T,
    <<_Size:32, Bin:ObjSz/binary, _/binary>> = B,
    Term = binary_to_term(T),
    Key = element(KeyPos, Term),
    Item = {Term, ?SEQSTART, Bin},
    per_bag_key(R, KeyPos, [{Key,Size,KeyBin,Item,KeyObjs} | L]);
per_bag_key(<<>>, KeyPos, L) when is_integer(KeyPos) ->
    lists:reverse(L).


binobjs2terms(<<ObjSz:32, T/binary>> = B) ->
    binobjs2terms(B, T, ObjSz, byte_size(B)-ObjSz, ?SEQSTART+1, []);
binobjs2terms([] = B) ->
    B;
binobjs2terms(<<>>) ->
    [].

binobjs2terms(Bin, Obj, _ObjSz, _Size=0, N, L) ->
    lists:reverse(L, [{binary_to_term(Obj), N, Bin}]);
binobjs2terms(Bin, Bin1, ObjSz, Size, N, L) ->
    <<B:ObjSz/binary, T/binary>> = Bin,
    <<NObjSz:32, T1/binary>> = T,
    Item = {binary_to_term(Bin1), N, B},
    binobjs2terms(T, T1, NObjSz, Size-NObjSz, N+1, [Item | L]).


%% Appends objects in reversed order.
bin2objs(KeysObjs, set, Ts) ->
    <<ObjSz:32, T/binary>> = KeysObjs,
    bin2objs(T, ObjSz-4, byte_size(KeysObjs)-ObjSz, Ts);
bin2objs(KeysObjs, _Type, Ts) ->
    bin2objs2(KeysObjs, Ts).

bin2objs2(<<Size:32, ObjSz:32, T/binary>>, Ts) ->
    bin2objs(T, ObjSz-4, Size-ObjSz-4, Ts);
bin2objs2(<<>>, Ts) ->
    Ts.

bin2objs(Bin, ObjSz, _Size=0, Ts) ->
    <<_:ObjSz/binary, T/binary>> = Bin,
    bin2objs2(T, [binary_to_term(Bin) | Ts]);
bin2objs(Bin, ObjSz, Size, Ts) ->
    <<_:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    bin2objs(T, NObjSz-4, Size-NObjSz, [binary_to_term(Bin) | Ts]).


bin2keybins(KeysObjs, Head) when Head#head.type =:= set ->
    <<ObjSz:32, T/binary>> = KeysObjs,
    bin2keybins(T, Head#head.keypos, ObjSz-4, byte_size(KeysObjs)-ObjSz,[]);
bin2keybins(KeysObjs, Head) ->
    bin2keybins2(KeysObjs, Head#head.keypos, []).

bin2keybins2(<<Size:32, ObjSz:32, T/binary>>, Kp, L) ->
    bin2keybins(T, Kp, ObjSz-4, Size-ObjSz-4, L);
bin2keybins2(<<>>, Kp, L) when is_integer(Kp) ->
    lists:reverse(L).

bin2keybins(Bin, Kp, ObjSz, _Size=0, L) ->
    <<Obj:ObjSz/binary, T/binary>> = Bin,
    Term = binary_to_term(Obj),
    bin2keybins2(T, Kp, [{element(Kp, Term),Obj} | L]);
bin2keybins(Bin, Kp, ObjSz, Size, L) ->
    <<Obj:ObjSz/binary, NObjSz:32, T/binary>> = Bin,
    Term = binary_to_term(Obj),
    bin2keybins(T, Kp, NObjSz-4, Size-NObjSz, [{element(Kp,Term),Obj} | L]).
