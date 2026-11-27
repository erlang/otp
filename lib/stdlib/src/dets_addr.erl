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
-module(dets_addr).
-moduledoc false.

-export([width/1, pointer_position/2, slot_position/2, slot_size/1,
         index_size/2, new_index/2, read_pointer/4, resolve_pointer/4,
         decode_slot/3, decode_slots/3, write_pointer/3, slot_write/4,
         physical_size/1, allocation_class/2, allocated_size/3,
         alloc_object/2, free_object/3, object_writes/4, object_write/5,
         object_overwrite/5, object_counts/1, free_write/1,
         read_object/3, native_read/3, ipread/3, header/1, read_header/3,
         native_bytes/2, read_native_block/2, read_native_block/3,
         init_wide/1, is_wide_header/2, header_writes/2, progress_writes/1,
         read_file_header/2, check_file_header/2, init_freelist/1,
         do_perform_save/1, mark_dirty/1, normalize_for_disk_map/3]).

-include("dets.hrl").

-define(MAIN_SIZE, 312).
-define(LOWER_BASE, (?MAIN_SIZE + 256*4)).
-define(FRAME_SIZE, 2032).
-define(FRAME_A, (?DETS64_BASE + 32)).
-define(FRAME_B, (?FRAME_A + ?FRAME_SIZE)).
-define(FREE_PAGE_SIZE, 4096).
-define(FREE_COUNT, 497).
-define(FOOTER_MAGIC, ((?DETS64_META bsl 32) bor 7)).
-define(MAX_LOGICAL, (1 bsl 30)).
-define(MAX_POSITION, ((1 bsl 63)-1)).

width(Position) when Position < ?DETS64_BASE -> 32;
width(_Position) -> 64.

pointer_position(Page, I) when Page < ?DETS64_BASE -> Page + 4*I;
pointer_position(Page, I) -> Page + ?DETS64_INDEX_HEADER_SIZE + 8*I.

slot_position(Page, I) when Page < ?DETS64_BASE -> Page + 8*I;
slot_position(Page, I) -> Page + ?DETS64_INDEX_HEADER_SIZE + 16*I.

slot_size(Position) when Position < ?DETS64_BASE -> 8;
slot_size(_Position) -> 16.

index_size(_Kind, Position) when Position < ?DETS64_BASE -> 2048;
index_size(_Kind, _Position) -> 8192.

new_index(Kind, Position) when Position < ?DETS64_BASE ->
    <<0:(index_size(Kind, Position))/unit:8>>;
new_index(Kind, Position) ->
    Size = index_size(Kind, Position),
    iolist_to_binary([native_header(Size, kind_atom(Kind), 4096, 0),
                      <<0:(Size-?DETS64_INDEX_HEADER_SIZE)/unit:8>>]).

read_pointer(Fd, FileName, Position, 32) ->
    <<Raw:32>> = pread(Fd, FileName, Position, 4),
    resolve_pointer(Fd, FileName, Position, Raw);
read_pointer(Fd, FileName, Position, 64) ->
    dets_wire:decode_uint(pread(Fd, FileName, Position, 8)).

resolve_pointer(_Fd, _FileName, _Owner, Raw) when Raw < ?DETS64_REF_BASE ->
    Raw;
resolve_pointer(Fd, FileName, Owner, Raw)
  when Raw < ?DETS64_REF_END,
       (Raw-?DETS64_REF_BASE) rem ?DETS64_REF_SIZE =:= 0 ->
    decode_reference(Owner, pread(Fd, FileName, Raw, ?DETS64_REF_SIZE));
resolve_pointer(_Fd, _FileName, _Owner, _Raw) ->
    erlang:error(badarg).

decode_reference(Owner, <<OwnerWord:8/binary, TargetWord:8/binary>>) ->
    EncodedOwner = dets_wire:decode_uint(OwnerWord),
    Target = dets_wire:decode_uint(TargetWord),
    Owner = EncodedOwner bsr 32,
    CRC = EncodedOwner band 16#ffffffff,
    CRC = erlang:crc32(<<Owner:32, Target:64>>),
    Target.

decode_slot(H, Position, <<Size:32, Raw:32>>) when Position < ?DETS64_BASE ->
    Pointer = resolve_pointer(H#head.fptr, H#head.filename, Position+4, Raw),
    {Size, Pointer};
decode_slot(_H, _Position, <<SizeWord:8/binary, PointerWord:8/binary>>) ->
    {dets_wire:decode_uint(SizeWord), dets_wire:decode_uint(PointerWord)}.

decode_slots(H, [Position | Positions], [Bin | Bins]) ->
    {Size, Pointer} = decode_slot(H, position(Position), Bin),
    [<<Size:32, Pointer:64>> | decode_slots(H, Positions, Bins)];
decode_slots(_H, [], []) ->
    [].

position({Position, _Size}) -> Position;
position(Position) -> Position.

write_pointer(H, Position, Target) when Position >= ?DETS64_BASE ->
    {H, [{Position, dets_wire:encode_uint(Target)}]};
write_pointer(H, Position, Target) ->
    {H1, Raw, SideWrites} = encode_lower(H, Position, Target),
    {H1, SideWrites ++ [{Position, <<Raw:32>>}]}.

slot_write(H, Position, Size, Target) when Position >= ?DETS64_BASE ->
    Bin = iolist_to_binary([dets_wire:encode_uint(Size),
                            dets_wire:encode_uint(Target)]),
    {H, [{Position, Bin}]};
slot_write(H, Position, Size, Target) ->
    {H1, Raw, SideWrites} = encode_lower(H, Position+4, Target),
    {H1, SideWrites ++ [{Position, <<Size:32, Raw:32>>}]}.

encode_lower(H = #head{wide = undefined}, _Owner, Target)
  when Target < ?DETS64_BASE ->
    {H, Target, []};
encode_lower(H = #head{wide = undefined}, Owner, Target) ->
    encode_lower(init_wide(H), Owner, Target);
encode_lower(H, Owner, Target) ->
    Raw = case file:pread(H#head.fptr, Owner, 4) of
              {ok, <<P:32>>} -> P;
              eof -> 0;
              {error, Reason} -> dets_utils:corrupt_file(H, {error, Reason})
          end,
    if
        Raw >= ?DETS64_REF_BASE ->
            %% The reference belongs to this field, including when its target is zero.
            Previous = resolve_pointer(H#head.fptr, H#head.filename, Owner, Raw),
            Writes = case Previous =:= Target of
                         true -> [];
                         false -> [{Raw, reference(Owner, Target)}]
                     end,
            {H, Raw, Writes};
        Target < ?DETS64_REF_BASE ->
            {H, Target, []};
        true ->
            Wide = H#head.wide,
            Ref = Wide#dets64.ref_next,
            case Ref + ?DETS64_REF_SIZE =< ?DETS64_REF_END of
                true ->
                    Wide1 = Wide#dets64{ref_next = Ref + ?DETS64_REF_SIZE},
                    {H#head{wide = Wide1}, Ref,
                     [{Ref, reference(Owner, Target)}]};
                false ->
                    throw({error, {no_more_space_on_file, H#head.filename}})
            end
    end.

reference(Owner, Target) ->
    CRC = erlang:crc32(<<Owner:32, Target:64>>),
    iolist_to_binary([dets_wire:encode_uint((Owner bsl 32) bor CRC),
                      dets_wire:encode_uint(Target)]).

physical_size(Logical) when Logical >= 8, Logical =< ?MAX_LOGICAL ->
    ?DETS64_OBJECT_HEADER_SIZE + dets_wire:encoded_size(Logical-8).

allocation_class(Position, Logical) when Position < ?DETS64_BASE ->
    dets_utils:log2(Logical);
allocation_class(_Position, Logical) ->
    dets_utils:log2(physical_size(Logical)).

allocated_size(_H, Position, Logical) when Position < ?DETS64_BASE ->
    1 bsl dets_utils:log2(Logical);
allocated_size(H, Position, _Logical) ->
    {Physical, _Kind, _BodyBytes, _Aux} =
        read_header(H#head.fptr, H#head.filename, Position),
    Physical.

alloc_object(H, Logical) when Logical > ?MAX_LOGICAL ->
    no_more_space(H);
alloc_object(H, Logical) ->
    {H1, Position, _PhysicalClass} =
        dets_utils:alloc_sizes(H, Logical, physical_size(Logical)),
    {H1, Position, dets_utils:log2(Logical)}.

free_object(H, Position, Logical) ->
    Physical = allocated_size(H, Position, Logical),
    {H1, _PhysicalClass} = dets_utils:free(H, Position, Physical-1),
    {H1, dets_utils:log2(Logical)}.

object_writes(_Position, Logical, _Bins, _Physical) when Logical > ?MAX_LOGICAL ->
    erlang:error(badarg);
object_writes(Position, Logical, Bins, Physical) ->
    object_image(Position, Logical, Bins, Physical, object_counts(Bins)).

object_write(H, _Position, Logical, _Bins, _Counts) when Logical > ?MAX_LOGICAL ->
    no_more_space(H);
object_write(_H, Position, Logical, Bins, Counts) ->
    Physical = 1 bsl allocation_class(Position, Logical),
    object_image(Position, Logical, Bins, Physical, Counts).

object_overwrite(H, _Position, Logical, _Bins, _Counts)
  when Logical > ?MAX_LOGICAL ->
    no_more_space(H);
object_overwrite(H, Position, Logical, Bins, Counts) ->
    Physical = allocated_size(H, Position, Logical),
    object_image(Position, Logical, Bins, Physical, Counts).

object_image(Position, Logical, Bins, _Physical, _Counts)
  when Position < ?DETS64_BASE ->
    [{Position, [<<Logical:32, ?ACTIVE:32>> | Bins]}];
object_image(Position, Logical, Bins, Physical, {Objects, Keys}) ->
    Body = iolist_to_binary(Bins),
    true = byte_size(Body) =:= Logical-8,
    Packed = dets_wire:encode(Body),
    Padding = Physical - ?DETS64_OBJECT_HEADER_SIZE - byte_size(Packed),
    true = Padding >= 0,
    Aux = (Keys bsl 32) bor Objects,
    [{Position, [native_header(Physical, ?ACTIVE, byte_size(Body), Aux),
                 Packed, dets_utils:make_zeros(Padding)]}].

object_counts(Bins) ->
    %% Object lengths are below 2 GiB; only a set starts its first term here.
    case iolist_to_binary(Bins) of
        <<>> -> {0, 0};
        <<_Size:32, 131, _/binary>> = Body -> count_set(Body, 0);
        Body -> count_bag(Body, 0, 0)
    end.

count_set(<<>>, N) -> {N, N};
count_set(<<Size:32, Rest/binary>>, N) when Size >= 4 ->
    <<_:(Size-4)/binary, More/binary>> = Rest,
    count_set(More, N+1).

count_bag(<<>>, Objects, Keys) -> {Objects, Keys};
count_bag(<<Size:32, Rest/binary>>, Objects, Keys) when Size >= 4 ->
    <<Group:(Size-4)/binary, More/binary>> = Rest,
    {Count, Count} = count_set(Group, 0),
    count_bag(More, Objects+Count, Keys+1).

free_write(Position) when Position < ?DETS64_BASE ->
    {Position+4, <<?FREE:32>>};
free_write(Position) ->
    {Position+8, dets_wire:encode_uint(kind_word(?FREE))}.

read_object(H, Position, Logical) when Position < ?DETS64_BASE ->
    Bin = pread(H#head.fptr, H#head.filename, Position, Logical),
    <<Logical:32, ?ACTIVE:32, _/binary>> = Bin,
    {ok, Bin};
read_object(H, Position, Logical) ->
    native_read(H, Position, Logical).

native_read(H, Position, Logical) ->
    {bucket, Bin, _Physical} =
        read_native_block(H#head.fptr, H#head.filename, Position),
    <<Logical:32, ?ACTIVE:32, _/binary>> = Bin,
    {ok, Bin}.

ipread(H, Position, MaxSize) ->
    Raw = pread(H#head.fptr, H#head.filename, Position, slot_size(Position)),
    case decode_slot(H, Position, Raw) of
        {0, 0} -> {ok, {0, 0, eof}};
        {Logical, Pointer} when Logical >= 8, Logical =< MaxSize ->
            {ok, Bin} = read_object(H, Pointer, Logical),
            {ok, {Logical, Pointer, Bin}};
        _ -> erlang:error(badarg)
    end.

native_header(Physical, Kind, BodyBytes, Aux) ->
    [dets_wire:encode_uint(Physical), dets_wire:encode_uint(kind_word(Kind)),
     dets_wire:encode_uint(BodyBytes), dets_wire:encode_uint(Aux)].

header(<<PhysicalWord:8/binary, KindWord:8/binary,
         BodyWord:8/binary, AuxWord:8/binary>>) ->
    Physical = dets_wire:decode_uint(PhysicalWord),
    Word = dets_wire:decode_uint(KindWord),
    ?DETS64_META = Word bsr 32,
    true = Physical >= 32 andalso Physical band (Physical-1) =:= 0,
    {Physical, kind_atom(Word band 16#ffffffff),
     dets_wire:decode_uint(BodyWord), dets_wire:decode_uint(AuxWord)}.

read_header(Fd, FileName, Position) ->
    {Physical, _, _, _} = Header =
        header(pread(Fd, FileName, Position, ?DETS64_OBJECT_HEADER_SIZE)),
    true = Position >= ?DETS64_BASE andalso
        Position + Physical =< ?MAX_POSITION,
    Header.

native_bytes(Bin, _MaxLogical) when byte_size(Bin) < 32 ->
    {more, 32};
native_bytes(<<Header:32/binary, Rest/binary>>, MaxLogical) ->
    {Physical, Kind, BodyBytes, _Aux} = header(Header),
    case Kind of
        Status when Status =:= ?ACTIVE; Status =:= ?FREE ->
            Logical = BodyBytes + 8,
            true = Logical >= 12 andalso Logical =< MaxLogical andalso
                Logical =< ?MAX_LOGICAL,
            Encoded = dets_wire:encoded_size(BodyBytes),
            true = Encoded + 32 =< Physical,
            case byte_size(Rest) >= Encoded of
                false -> {more, Encoded + 32};
                true ->
                    <<Wire:Encoded/binary, _/binary>> = Rest,
                    Body = dets_wire:decode(Wire, BodyBytes),
                    {bucket, <<Logical:32, Status:32, Body/binary>>, Physical}
            end;
        _ ->
            {meta, Physical}
    end.

read_native_block(H, Position) ->
    read_native_block(H#head.fptr, H#head.filename, Position).

read_native_block(Fd, FileName, Position) ->
    Header = pread(Fd, FileName, Position, 32),
    {Physical, Kind, BodyBytes, _Aux} = header(Header),
    true = Position >= ?DETS64_BASE andalso
        Position + Physical =< ?MAX_POSITION,
    case Kind of
        Status when Status =:= ?ACTIVE; Status =:= ?FREE ->
            true = BodyBytes >= 4 andalso BodyBytes + 8 =< ?MAX_LOGICAL,
            Encoded = dets_wire:encoded_size(BodyBytes),
            true = Encoded + 32 =< Physical,
            Wire = pread(Fd, FileName, Position+32, Encoded),
            native_bytes(<<Header/binary, Wire/binary>>, ?MAX_LOGICAL);
        _ -> {meta, Physical}
    end.

kind_word(Kind) ->
    (?DETS64_META bsl 32) bor kind_id(Kind).

kind_id(?ACTIVE) -> 1;
kind_id(?FREE) -> 2;
kind_id(part) -> 3;
kind_id(segment) -> 4;
kind_id(root) -> 5;
kind_id(freelist) -> 6.

kind_atom(1) -> ?ACTIVE;
kind_atom(2) -> ?FREE;
kind_atom(3) -> part;
kind_atom(4) -> segment;
kind_atom(5) -> root;
kind_atom(6) -> freelist;
kind_atom(part) -> part;
kind_atom(segment) -> segment.

init_wide(H = #head{wide = #dets64{}}) -> H;
init_wide(H) ->
    H1 = dets_utils:init_wide_alloc(H),
    erase({dets_addr, H#head.fptr, generation}),
    Main = main_header(H1),
    Body = meta_body(H1, 0, 0, 0, main_digest(Main)),
    Frames = [meta_frame(1, Body), <<0:?FRAME_SIZE/unit:8>>],
    ok = write_at(H1, [{?FRAME_A, Frames}]),
    ok = dets_utils:sync(H1),
    Root = native_header(?DETS64_HEADER_SIZE, root, 0, 0),
    ok = write_at(H1, [{?DETS64_BASE, Root}, {0, Main}]),
    ok = dets_utils:sync(H1),
    H1.

is_wide_header(Fd, FileName) ->
    {ok, EOF} = dets_utils:position(Fd, FileName, eof),
    case EOF >= ?DETS64_BASE + ?DETS64_HEADER_SIZE of
        false -> false;
        true -> probe_wide_header(Fd, FileName)
    end.

probe_wide_header(Fd, FileName) ->
    case file:pread(Fd, ?DETS64_BASE, 32) of
        {ok, Bin} when byte_size(Bin) =:= 32 ->
            case protected_header(Bin) of
                {ok, {?DETS64_HEADER_SIZE, root, 0, 0}} -> true;
                _ -> false
            end;
        {error, Reason} -> dets_utils:file_error(FileName, {error, Reason});
        _ -> false
    end.

header_writes(H, ClosedProperly) ->
    Main = main_header(H),
    {FreeList, FileSize} = case ClosedProperly of
                             0 -> {0, 0};
                             1 ->
                                 {ok, EOF} = dets_utils:position(H, eof),
                                 {H#head.freelists_p, EOF}
                         end,
    Body = meta_body(H, ClosedProperly, FreeList, FileSize, main_digest(Main)),
    {Generation, Previous, Offset} = latest_meta(H#head.fptr, H#head.filename),
    Writes = case Body =:= Previous of
                 true -> [];
                 false ->
                     ok = dets_utils:sync(H),
                     Next = other_frame(Offset),
                     [{Next, meta_frame(Generation+1, Body)}]
             end,
    Writes ++ [{0, Main}].

progress_writes(H) ->
    Main = main_header(H),
    Body = meta_body(H, 0, 0, 0, main_digest(Main)),
    A = read_meta(H#head.fptr, H#head.filename, ?FRAME_A),
    B = read_meta(H#head.fptr, H#head.filename, ?FRAME_B),
    {Generation, Previous, _Offset} = Latest = latest_meta(A, B, H#head.filename),
    true = dirty_meta(Latest),
    Writes = case Body =:= Previous of
                 true -> [];
                 false ->
                     %% Keep the older dirty frame intact until the next save.
                     Dirty = [M || M <- [A, B], dirty_meta(M)],
                     {_, _, Anchor} = lists:min(Dirty),
                     [{other_frame(Anchor), meta_frame(Generation+1, Body)}]
             end,
    Writes ++ [{0, Main}].

dirty_meta(invalid) -> false;
dirty_meta({_Generation, Body, _Offset}) ->
    element(2, binary_to_term(Body, [safe])) =:= 0.

other_frame(?FRAME_A) -> ?FRAME_B;
other_frame(?FRAME_B) -> ?FRAME_A.

main_header(H) ->
    Wide = H#head.wide,
    Objects = H#head.no_objects - Wide#dets64.no_objects,
    Keys = H#head.no_keys - Wide#dets64.no_keys,
    true = Objects >= Keys andalso Keys >= 0,
    Counts = lower_collections(H#head.no_collections, Wide#dets64.no_collections),
    CounterBin = iolist_to_binary([<<(counter(I, Counts)):32>> ||
                                      I <- lists:seq(4,31)]),
    Data = <<(dets_utils:type_to_code(H#head.type)):32, 9:32,
             (H#head.m):32, (H#head.next):32, (H#head.keypos):32,
             Objects:32, Keys:32, (H#head.min_no_slots):32,
             (H#head.max_no_slots):32, (hash_code(H#head.hash_bif)):32,
             (H#head.n):32, CounterBin/binary>>,
    [<<0:32, ?MAGIC:32, 0:32>>, Data, erlang:md5(Data),
     <<(H#head.base):32, 0:124/unit:8>>].

lower_collections(undefined, _Upper) -> undefined;
lower_collections(Global, Upper) ->
    [{K, V-proplists:get_value(K, Upper, 0)} || {K,V} <- Global].

counter(_I, undefined) -> 0;
counter(I, Counts) ->
    N = proplists:get_value(I, Counts, 0),
    true = N >= 0 andalso N < (1 bsl 32),
    N.

main_digest(Main) ->
    <<_:12/binary, _Data:156/binary, Digest:16/binary, _/binary>> =
        iolist_to_binary(Main),
    Digest.

meta_body(H, CP, FreeList, FileSize, MainDigest) ->
    W = H#head.wide,
    term_to_binary({dets_addr, CP, FreeList, FileSize,
                    H#head.no_objects, H#head.no_keys, H#head.no_collections,
                    W#dets64.no_objects, W#dets64.no_keys,
                    W#dets64.no_collections, W#dets64.levels,
                    W#dets64.ref_next, MainDigest}).

meta_frame(Generation, Body) ->
    CRC = erlang:crc32([<<Generation:64>>, Body]),
    Packed = dets_wire:encode(Body),
    Padding = ?FRAME_SIZE-24-byte_size(Packed),
    true = Padding >= 0,
    [dets_wire:encode_uint(Generation), dets_wire:encode_uint(byte_size(Body)),
     dets_wire:encode_uint(CRC), Packed, <<0:Padding/unit:8>>].

latest_meta(Fd, FileName) ->
    A = read_meta(Fd, FileName, ?FRAME_A),
    B = read_meta(Fd, FileName, ?FRAME_B),
    latest_meta(A, B, FileName).

latest_meta(A, B, FileName) ->
    case {A, B} of
        {invalid, invalid} -> throw({error, {bad_wide_header, FileName}});
        {invalid, Valid} -> Valid;
        {Valid, invalid} -> Valid;
        {{GA, _, _}, {GB, _, _}} when GA >= GB -> A;
        _ -> B
    end.

read_meta(Fd, FileName, Position) ->
    case file:pread(Fd, Position, ?FRAME_SIZE) of
        {ok, Bin} when byte_size(Bin) =:= ?FRAME_SIZE ->
            try
                <<G:8/binary, L:8/binary, C:8/binary, Rest/binary>> = Bin,
                Generation = dets_wire:decode_uint(G),
                Length = dets_wire:decode_uint(L),
                CRC = dets_wire:decode_uint(C),
                true = Generation > 0,
                Encoded = dets_wire:encoded_size(Length),
                <<Packed:Encoded/binary, _/binary>> = Rest,
                Body = dets_wire:decode(Packed, Length),
                CRC = erlang:crc32([<<Generation:64>>, Body]),
                {Generation, Body, Position}
            catch error:_ -> invalid end;
        {error, Reason} -> dets_utils:file_error(FileName, {error, Reason});
        _ -> invalid
    end.

read_file_header(Fd, FileName) ->
    Main = pread(Fd, FileName, 0, ?MAIN_SIZE),
    <<_Free32:32, Cookie:32, _MainCP:32, Type:32, Version:32,
      M:32, Next:32, KeyPos:32, _LowerO:32, _LowerK:32,
      Min:32, Max:32, Hash:32, N:32, _Counters:112/binary,
      ReadMD5:16/binary, FlBase:32, _/binary>> = Main,
    <<_:12/binary, Digested:156/binary, _/binary>> = Main,
    {_Generation, Body, _Offset} = latest_meta(Fd, FileName),
    {dets_addr, CP0, FreeList, FileSize, Objects, Keys, Counts,
     UpperObjects, UpperKeys, UpperCounts, Levels, RefNext, ExpectedMD5} =
        binary_to_term(Body, [safe]),
    {ok, EOF} = dets_utils:position(Fd, FileName, eof),
    Trailer = read_footer(Fd, FileName, EOF),
    CP = case ExpectedMD5 =:= ReadMD5 andalso FileSize =:= EOF of
             true -> CP0;
             false -> 0
         end,
    Base = case FlBase of 0 -> ?LOWER_BASE; _ -> FlBase end,
    Wide = #dets64{levels = Levels, ref_next = RefNext,
                   no_objects = UpperObjects, no_keys = UpperKeys,
                   no_collections = UpperCounts},
    {ok, Fd, #fileheader{freelist = FreeList, fl_base = Base, cookie = Cookie,
                         closed_properly = CP, type = dets_utils:code_to_type(Type),
                         version = Version, m = M, next = Next, keypos = KeyPos,
                         no_objects = Objects, no_keys = Keys,
                         min_no_slots = Min, max_no_slots = Max,
                         no_colls = Counts, hash_method = Hash,
                         read_md5 = ReadMD5, has_md5 = true,
                         md5 = erlang:md5(Digested), trailer = Trailer, eof = EOF,
                         n = N, wide = Wide}}.

read_footer(Fd, FileName, EOF) when EOF >= 32 ->
    case file:pread(Fd, EOF-32, 32) of
        {ok, <<A:8/binary, B:8/binary, C:8/binary, Z:8/binary>>} ->
            try
                true = ?FOOTER_MAGIC =:= dets_wire:decode_uint(A),
                FileSize = dets_wire:decode_uint(B),
                CRC = dets_wire:decode_uint(C),
                0 = dets_wire:decode_uint(Z),
                CRC = erlang:crc32(<<FileSize:64>>),
                FileSize
            catch error:_ -> 0 end;
        {error, Reason} -> dets_utils:file_error(FileName, {error, Reason});
        _ -> 0
    end;
read_footer(_Fd, _FileName, _EOF) -> 0.

check_file_header(FH, Fd) ->
    Hash = hash_method(FH#fileheader.hash_method),
    W = FH#fileheader.wide,
    if
        FH#fileheader.cookie =/= ?MAGIC -> {error, not_a_dets_file};
        FH#fileheader.type =:= badtype -> {error, invalid_type_code};
        FH#fileheader.version =/= 9 -> {error, bad_version};
        FH#fileheader.read_md5 =/= FH#fileheader.md5 -> {error, not_a_dets_file};
        Hash =:= undefined -> {error, bad_hash_bif};
        W#dets64.levels < 32; W#dets64.levels > 64 -> {error, bad_freelists};
        W#dets64.ref_next < ?DETS64_REF_BASE;
        W#dets64.ref_next > ?DETS64_REF_END -> {error, bad_freelists};
        FH#fileheader.closed_properly =/= 1;
        FH#fileheader.trailer =/= FH#fileheader.eof -> {error, not_closed};
        true ->
            Counts = FH#fileheader.no_colls,
            MaxSize = case Counts of undefined -> undefined;
                                     [] -> 0;
                                     _ -> element(1, lists:last(Counts)) end,
            {ok, #head{m = FH#fileheader.m, m2 = FH#fileheader.m*2,
                        next = FH#fileheader.next, n = FH#fileheader.n,
                        fptr = Fd, no_objects = FH#fileheader.no_objects,
                        no_keys = FH#fileheader.no_keys, maxobjsize = MaxSize,
                        type = FH#fileheader.type, keypos = FH#fileheader.keypos,
                        freelists_p = FH#fileheader.freelist,
                        no_collections = Counts, auto_save = infinity,
                        update_mode = saved, hash_bif = Hash, has_md5 = true,
                        min_no_slots = FH#fileheader.min_no_slots,
                        max_no_slots = FH#fileheader.max_no_slots,
                        bump = 16, base = FH#fileheader.fl_base, wide = W}}
    end.

init_freelist(H) ->
    Wide = H#head.wide,
    Lower = dets_utils:empty_free_lists(32),
    Upper = dets_utils:empty_free_lists(Wide#dets64.levels),
    try read_free_pages(H, H#head.freelists_p, Lower, Upper, none, []) of
        {FL, FU} -> H#head{freelists = FL, wide = Wide#dets64{freelists = FU}}
    catch error:_ -> throw({error, {bad_freelists, H#head.filename}})
    end.

read_free_pages(H, Position, Lower, Upper, Last, Addresses) ->
    {Physical, freelist, Length, CRC} =
        read_header(H#head.fptr, H#head.filename, Position),
    ?FREE_PAGE_SIZE = Physical,
    true = Length >= 24 andalso
        dets_wire:encoded_size(Length) + 32 =< ?FREE_PAGE_SIZE,
    Wire = pread(H#head.fptr, H#head.filename, Position+32,
                 dets_wire:encoded_size(Length)),
    Body = dets_wire:decode(Wire, Length),
    CRC = erlang:crc32(Body),
    <<Pool:32, Class:32, Count:32, 0:32, Next:64, Tail/binary>> = Body,
    case {Pool, Class, Count, Next} of
        {2, 0, 0, 0} ->
            finish_pool(Lower, Upper, Last, Addresses);
        _ when Pool < 2, Class > 0, Count > 0,
               byte_size(Tail) =:= Count*8 ->
            {L1, U1, Acc} = case Last =:= {Pool, Class} of
                               true -> {Lower, Upper, Addresses};
                               false ->
                                   {L0, U0} = finish_pool(Lower, Upper,
                                                          Last, Addresses),
                                   {L0, U0, []}
                           end,
            Values = [V || <<V:64>> <= Tail],
            read_free_pages(H, Next, L1, U1, {Pool, Class},
                            lists:reverse(Values, Acc))
    end.

finish_pool(Lower, Upper, none, []) -> {Lower, Upper};
finish_pool(Lower, Upper, {0, Class}, Addresses) ->
    {setelement(Class, Lower, dets_utils:list_to_tree(Addresses)), Upper};
finish_pool(Lower, Upper, {1, Class}, Addresses) ->
    {Lower, setelement(Class, Upper, dets_utils:list_to_tree(Addresses))}.

do_perform_save(H) ->
    {ok, EOF} = dets_utils:position(H, eof),
    First = free_position(EOF),
    Position1 = save_pool(H, 0, dets_utils:get_freelists(H), 1, First),
    Position2 = save_pool(H, 1, dets_utils:get_upper_freelists(H), 1, Position1),
    Terminator = <<2:32, 0:32, 0:32, 0:32, 0:64>>,
    ok = write_at(H, [{Position2, meta_block(freelist, Terminator)}]),
    FileSize = Position2 + ?FREE_PAGE_SIZE + 32,
    Footer = [dets_wire:encode_uint(?FOOTER_MAGIC),
              dets_wire:encode_uint(FileSize),
              dets_wire:encode_uint(erlang:crc32(<<FileSize:64>>)),
              dets_wire:encode_uint(0)],
    ok = write_at(H, [{Position2+?FREE_PAGE_SIZE, Footer}]),
    H1 = H#head{freelists_p = First},
    Writes = header_writes(H1, 1),
    ok = write_at(H1, Writes),
    {H1, ok}.

save_pool(H, Pool, FL, Class, Position) when Class =< tuple_size(FL) ->
    F = fun(0, [], At, Bytes) -> {0, [], At, Bytes};
           (N, Values, At, Bytes) ->
                {Count, Chunk, Rest} = if N > ?FREE_COUNT ->
                                            {A, B} = lists:split(?FREE_COUNT, Values),
                                            {?FREE_COUNT, A, B};
                                        true -> {N, Values, []}
                                     end,
                Next = free_position(At + ?FREE_PAGE_SIZE),
                Body = iolist_to_binary([<<Pool:32, Class:32, Count:32, 0:32,
                                          Next:64>> | Chunk]),
                ok = write_at(H, [{At, meta_block(freelist, Body)}]),
                {N-Count, Rest, Next, Bytes+?FREE_PAGE_SIZE}
        end,
    {NextPosition, _Bytes} = dets_utils:tree_to_bin(element(Class, FL), F,
                                                  ?FREE_COUNT, Position, 0, 64),
    save_pool(H, Pool, FL, Class+1, NextPosition);
save_pool(_H, _Pool, _FL, _Class, Position) -> Position.

meta_block(Kind, Body) ->
    Packed = dets_wire:encode(Body),
    Padding = ?FREE_PAGE_SIZE - 32 - byte_size(Packed),
    true = Padding >= 0,
    [native_header(?FREE_PAGE_SIZE, Kind, byte_size(Body), erlang:crc32(Body)),
     Packed, <<0:Padding/unit:8>>].

free_position(Position) ->
    Aligned = (Position+?FREE_PAGE_SIZE-1) band bnot (?FREE_PAGE_SIZE-1),
    if
        Aligned >= ?DETS64_REF_BASE, Aligned < ?DETS64_REF_END ->
            ?DETS64_REF_END;
        true -> Aligned
    end.

mark_dirty(H) ->
    Writes = header_writes(H, 0),
    ok = write_at(H, Writes),
    ok = dets_utils:sync(H),
    {ok, _} = dets_utils:position(H, H#head.freelists_p),
    dets_utils:truncate(H, cur).

normalize_for_disk_map(Fd, FileName, Writes) ->
    Planned = maps:from_list([{P, iolist_to_binary(B)} || {P, B} <- Writes,
                              P >= ?DETS64_REF_BASE, P < ?DETS64_REF_END]),
    lists:flatmap(fun({P, B}) -> normalize_write(Fd, FileName, P, B, Planned) end,
                  Writes).

normalize_write(_Fd, _FileName, P, _Io, _Planned)
  when P >= ?DETS64_REF_BASE, P < ?DETS64_REF_END -> [];
normalize_write(Fd, FileName, P, Io, Planned) when P < ?DETS64_BASE ->
    Bin = iolist_to_binary(Io),
    case Bin of
        <<Size:32, Raw:32>> ->
            Target = planned_pointer(Fd, FileName, P+4, Raw, Planned),
            [{P, <<(debug_size(Target, Size)):32, Target:64>>}];
        <<_:32>> when Bin =/= <<?FREE:32>> -> [];
        _ -> [{P, Bin}]
    end;
normalize_write(_Fd, _FileName, P, Io, _Planned) ->
    Bin = iolist_to_binary(Io),
    case byte_size(Bin) of
        8 ->
            case dets_wire:decode_uint(Bin) of
                Word when Word =:= ((?DETS64_META bsl 32) bor 2) ->
                    [{P-4, <<?FREE:32>>}];
                _ -> []
            end;
        16 ->
            <<S:8/binary, T:8/binary>> = Bin,
            Size = dets_wire:decode_uint(S),
            Target = dets_wire:decode_uint(T),
            [{P, <<(debug_size(Target, Size)):32, Target:64>>}];
        N when N >= 32 ->
            <<Header:32/binary, _/binary>> = Bin,
            case protected_header(Header) of
                {ok, {_Physical, Status, Body, _Aux}}
                  when Status =:= ?ACTIVE; Status =:= ?FREE ->
                    [{P, <<(physical_size(Body+8)):32, Status:32>>}];
                {ok, {_Physical, segment, 4096, _Aux}} ->
                    <<_:32/binary, Entries:4096/binary, _/binary>> = Bin,
                    normalize_segment(P+32, Entries);
                _ -> []
            end;
        _ -> []
    end.

normalize_segment(Position, <<S:8/binary, T:8/binary, Rest/binary>>) ->
    Size = dets_wire:decode_uint(S),
    Target = dets_wire:decode_uint(T),
    [{Position, <<(debug_size(Target, Size)):32, Target:64>>} |
     normalize_segment(Position+16, Rest)];
normalize_segment(_Position, <<>>) -> [].

planned_pointer(_Fd, _FileName, _Owner, Raw, _Planned)
  when Raw < ?DETS64_REF_BASE -> Raw;
planned_pointer(Fd, FileName, Owner, Raw, Planned) ->
    case maps:find(Raw, Planned) of
        {ok, Bin} -> decode_reference(Owner, Bin);
        error -> resolve_pointer(Fd, FileName, Owner, Raw)
    end.

debug_size(_Pointer, 0) -> 0;
debug_size(Pointer, Logical) when Pointer < ?DETS64_BASE -> Logical;
debug_size(_Pointer, Logical) -> physical_size(Logical).

hash_code(phash) -> 0;
hash_code(phash2) -> 1.

hash_method(0) -> phash;
hash_method(1) -> phash2;
hash_method(_) -> undefined.

no_more_space(H) ->
    throw({error, {no_more_space_on_file, H#head.filename}}).

protected_header(Bin) ->
    try {ok, header(Bin)}
    catch error:_ -> invalid end.

pread(Fd, FileName, Position, Size) ->
    {ok, Bin} = dets_utils:pread_close(Fd, FileName, Position, Size),
    Bin.

write_at(H, Writes) ->
    case file:pwrite(H#head.fptr, Writes) of
        ok -> ok;
        {error, {_Count, Reason}} -> dets_utils:corrupt_file(H, {error, Reason})
    end.
