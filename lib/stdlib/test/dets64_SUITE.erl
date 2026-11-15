%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1996-2026. All Rights Reserved.
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
-module(dets64_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include("dets.hrl").

-export([suite/0, all/0, lower_only/1, init_interruption/1, inplace_reference/1,
         logical_class/1, collection_types/1, index_growth/1,
         native_links/1, continuations/1, repair_compact/1, bulk_native/1,
         wire_guard/1, sync_lifecycle/1, progress_recovery/1, saved_sync/1]).

-export([pwrite/2, sync/1]).

suite() ->
    [{ct_hooks, [ts_install_cth]}, {timetrap, {minutes, 15}}].

all() ->
    [lower_only, init_interruption, inplace_reference, logical_class,
     collection_types, index_growth, native_links, continuations, repair_compact,
     bulk_native, wire_guard, sync_lifecycle, progress_recovery, saved_sync].

lower_only(Config) ->
    with_table(Config, lower_only, set,
      fun(Tab, File) ->
              ok = dets:insert(Tab, [{lower, aaaa}, {stay, 3}]),
              ok = dets:sync(Tab),
              undefined = (head(Tab))#head.wide,
              {ok, Position} = dets:where(Tab, {lower, aaaa}),
              Entry = entry(Tab, lower, File),
              ok = dets:insert(Tab, {lower, bbbb}),
              ok = dets:sync(Tab),
              {ok, Position} = dets:where(Tab, {lower, bbbb}),
              Entry = entry(Tab, lower, File),
              undefined = (head(Tab))#head.wide,
              {ok, Fd} = file:open(File, [read, raw, binary]),
              try
                  {ok, <<9:32>>} = file:pread(Fd, 16, 4),
                  eof = file:pread(Fd, ?DETS64_BASE, 32)
              after file:close(Fd) end,
              check_counts(Tab, File)
      end).

init_interruption(Config) ->
    with_table(Config, init_interruption, set,
      fun(Tab, File) ->
              Objects = [{lower, aaaa}, {stay, 3}],
              ok = dets:insert(Tab, Objects),
              ok = dets:close(Tab),
              Stops = [{?DETS64_BASE, all, true},
                       {?DETS64_BASE+32, 24, false},
                       {?DETS64_BASE+32, all, false},
                       {?DETS64_BASE, 15, false},
                       {?DETS64_BASE, 16, true},
                       {complete, all, true}],
              lists:foreach(
                fun({Position, Limit, Wide}) ->
                        CrashFile = File ++ ".crash",
                        {ok, _} = file:copy(File, CrashFile),
                        try
                            interrupted_init(CrashFile, Position, Limit, Wide),
                            {ok, Tab} = dets:open_file(Tab, [{file, CrashFile}]),
                            Objects = snapshot(Tab),
                            check_counts(Tab, CrashFile),
                            undefined = (head(Tab))#head.wide
                        after close_table(Tab), file:delete(CrashFile) end
                end, Stops)
      end).

interrupted_init(File, Position, Limit, Wide) ->
    {ok, Fd} = file:open(File, [read, write, raw, binary]),
    try
        {ok, Fd, FH} = dets_v9:read_file_header(Fd, File),
        {ok, H0} = dets_v9:check_file_header(FH, Fd),
        H = dets_v9:init_freelist(H0#head{filename = File}),
        ok = dets_v9:mark_dirty(H),
        {ok, LowerEOF} = file:position(Fd, eof),
        {ok, Lower} = file:pread(Fd, 312, LowerEOF-312),
        FaultFd = #file_descriptor{module = ?MODULE,
                                   data = {Fd, Position, Limit}},
        put({?MODULE, Fd}, []),
        try dets_addr:init_wide(H#head{fptr = FaultFd}) of
            #head{} when Position =:= complete -> ok;
            _ -> ct:fail(missing_interruption)
        catch throw:interrupted -> ok end,
        {ok, Lower} = file:pread(Fd, 312, LowerEOF-312),
        Wide = dets_addr:is_wide_header(Fd, File),
        {ok, Fd, InterruptedFH} = dets_v9:read_file_header(Fd, File),
        {error, not_closed} = dets_v9:check_file_header(InterruptedFH, Fd),
        case Position of
            ?DETS64_BASE ->
                [{write, ?DETS64_BASE}, sync, {write, Frame}] = get({?MODULE, Fd}),
                Frame = ?DETS64_BASE+32;
            complete ->
                2 = length([ok || sync <- get({?MODULE, Fd})]);
            _ -> ok
        end
    after erase({?MODULE, Fd}), file:close(Fd) end.

pwrite(#file_descriptor{data = {Fd, Position, Limit}}, Writes) ->
    lists:foreach(
      fun({At, Io}) when At =:= Position ->
              Bin = iolist_to_binary(Io),
              Length = case Limit of all -> byte_size(Bin); _ -> Limit end,
              ok = file:pwrite(Fd, At, binary:part(Bin, 0, Length)),
              put({?MODULE, Fd}, [{write, At} | get({?MODULE, Fd})]),
              throw(interrupted);
         ({At, _} = Write) ->
              ok = file:pwrite(Fd, [Write]),
              put({?MODULE, Fd}, [{write, At} | get({?MODULE, Fd})])
      end, Writes).

sync(#file_descriptor{data = {_Fd, sync_error}}) ->
    {error, eio};
sync(#file_descriptor{data = {Fd, _Position, _Limit}}) ->
    ok = file:sync(Fd),
    put({?MODULE, Fd}, [sync | get({?MODULE, Fd})]),
    ok.

sync_lifecycle(Config) ->
    with_mixed(Config, sync_lifecycle, set,
      fun(Tab, _File, _Native, Far) ->
              Worker = dets:info(Tab, owner),
              #head{} = sys:replace_state(Worker,
                                          fun(H) -> H#head{auto_save = infinity} end),
              ok = expect_syncs(Worker, 1, fun() -> dets:sync(Tab) end),
              ok = expect_syncs(Worker, 2,
                fun() ->
                        ok = dets:insert(Tab, {Far, bbbb}),
                        [{Far, bbbb}] = dets:lookup(Tab, Far),
                        ok
                end),
              Before = dets:info(Tab, no_keys),
              Added = [{{progress, I}, I} || I <- lists:seq(1, 256)],
              After = expect_syncs(Worker, 0,
                fun() ->
                        ok = dets:insert(Tab, Added),
                        [{{progress, 256}, 256}] = dets:lookup(Tab, {progress, 256}),
                        dets:info(Tab, no_keys)
                end),
              256 = After-Before,
              ok = expect_syncs(Worker, 2, fun() -> dets:sync(Tab) end),
              ok = dets:insert(Tab, {Far, cccc}),
              [{Far, cccc}] = dets:lookup(Tab, Far),
              ok = expect_syncs(Worker, 2, fun() -> dets:close(Tab) end)
      end).

progress_recovery(Config) ->
    lists:foreach(
      fun({Tag, Damage}) ->
              with_mixed(Config, Tag, set,
                fun(Tab, File, _Native, _Far) ->
                        Expected = snapshot(Tab),
                        ok = dets:close(Tab),
                        interrupted_progress(File, Damage),
                        {ok, Tab} = dets:open_file(Tab, [{file, File}]),
                        Expected = snapshot(Tab),
                        check_counts(Tab, File)
                end)
      end, [{progress_lost, lost}, {progress_torn, torn}]).

interrupted_progress(File, Damage) ->
    Parent = self(),
    {Worker, Monitor} = spawn_monitor(fun() -> progress_file(File, Damage, Parent) end),
    receive
        {progress_ready, Worker} -> ok;
        {'DOWN', Monitor, process, Worker, SetupReason} ->
            ct:fail({progress_setup, SetupReason})
    after 10000 -> ct:fail(progress_setup_timeout)
    end,
    ok = expect_syncs(Worker, 0,
      fun() ->
              Worker ! continue,
              receive
                  {progress_done, Worker} -> ok;
                  {'DOWN', Monitor, process, Worker, WriteReason} ->
                      ct:fail({progress_write, WriteReason})
              after 10000 -> ct:fail(progress_write_timeout)
              end
      end),
    receive
        {'DOWN', Monitor, process, Worker, normal} -> ok;
        {'DOWN', Monitor, process, Worker, CloseReason} ->
            ct:fail({progress_close, CloseReason})
    after 10000 -> ct:fail(progress_close_timeout)
    end.

progress_file(File, Damage, Parent) ->
    {ok, Fd} = file:open(File, [read, write, raw, binary]),
    try
        {ok, Fd, FH} = dets_v9:read_file_header(Fd, File),
        {ok, H0} = dets_v9:check_file_header(FH, Fd),
        H = dets_v9:init_freelist(H0#head{filename = File}),
        Before = progress_frames(Fd),
        ok = dets_addr:mark_dirty(H),
        Frames = progress_frames(Fd),
        [{Anchor, AnchorBytes}] = [{P, B} || {P, B} <- Frames,
                                            proplists:get_value(P, Before) =/= B],
        [Scratch] = [P || {P, _} <- Frames, P =/= Anchor],
        Parent ! {progress_ready, self()},
        receive continue -> ok end,
        lists:foldl(
          fun(_, Previous) ->
                  Wide = Previous#head.wide,
                  Next = Previous#head{wide = Wide#dets64{
                      ref_next = Wide#dets64.ref_next + ?DETS64_REF_SIZE}},
                  Writes = dets_addr:progress_writes(Next),
                  [Scratch] = [P || {P, _} <- Writes, P =/= 0],
                  ok = file:pwrite(Fd, Writes),
                  {ok, AnchorBytes} = file:pread(Fd, Anchor, byte_size(AnchorBytes)),
                  Next
          end, H#head{update_mode = dirty}, lists:seq(1, 3)),
        FrameSize = (?DETS64_HEADER_SIZE-32) div 2,
        case Damage of
            lost ->
                ok = file:pwrite(Fd, Scratch, <<0:FrameSize/unit:8>>);
            torn ->
                {ok, <<Byte>>} = file:pread(Fd, Scratch+16, 1),
                ok = file:pwrite(Fd, Scratch+16, <<(Byte bxor 1)>>)
        end,
        {ok, AnchorBytes} = file:pread(Fd, Anchor, byte_size(AnchorBytes)),
        {ok, Fd, Damaged} = dets_v9:read_file_header(Fd, File),
        {error, not_closed} = dets_v9:check_file_header(Damaged, Fd),
        Parent ! {progress_done, self()}
    after file:close(Fd) end.

progress_frames(Fd) ->
    Bytes = (?DETS64_HEADER_SIZE-32) div 2,
    [{P, begin {ok, B} = file:pread(Fd, P, Bytes), B end} ||
        P <- [?DETS64_BASE+32, ?DETS64_BASE+32+Bytes]].

saved_sync(Config) ->
    with_table(Config, saved_sync_error, set,
      fun(Tab, File) ->
              ok = dets:insert(Tab, {saved, value}),
              ok = dets:sync(Tab),
              Worker = dets:info(Tab, owner),
              Fd = (head(Tab))#head.fptr,
              #head{} = sys:replace_state(Worker,
                fun(H) -> H#head{fptr = #file_descriptor{
                    module = ?MODULE, data = {Fd, sync_error}}} end),
              try
                  {error, {file_error, File, eio}} = dets:sync(Tab)
              after
                  sys:replace_state(Worker, fun(H) -> H#head{fptr = Fd} end)
              end
      end),
    File = filename:join(proplists:get_value(priv_dir, Config), "saved_sync_ram"),
    Tab = {?MODULE, saved_sync_ram},
    Options = [{file, File}, {ram_file, true}, {auto_save, infinity}],
    {ok, Tab} = dets:open_file(Tab, Options),
    try
        ok = dets:insert(Tab, {saved, value}),
        ok = dets:sync(Tab),
        Worker = dets:info(Tab, owner),
        RamFd = (head(Tab))#head.fptr,
        {ok, Bytes} = file:read_file(File),
        {ok, Calls} = trace_file_calls(Worker, sync_file_functions(),
                                       fun() -> dets:sync(Tab) end),
        [DiskFd] = [F || {file, sync, [F]} <- Calls],
        true = DiskFd =/= RamFd,
        #file_descriptor{module = Module} = DiskFd,
        true = Module =/= ram_file,
        no_file_writes(Calls),
        {ok, Bytes} = file:read_file(File),
        ok = dets:close(Tab),
        {ok, Tab} = dets:open_file(Tab, [{access, read} | Options]),
        ReadWorker = dets:info(Tab, owner),
        {ok, ReadCalls} = trace_file_calls(ReadWorker, sync_file_functions(),
          fun() ->
                  [{saved, value}] = dets:lookup(Tab, saved),
                  ok = dets:sync(Tab),
                  dets:close(Tab)
          end),
        [] = [F || {file, sync, [F]} <- ReadCalls],
        no_file_writes(ReadCalls),
        true = lists:all(fun({file, open, [_Name, Modes]}) ->
                                not lists:member(write, Modes) andalso
                                    not lists:member(append, Modes);
                           (_) -> true
                        end, ReadCalls),
        {ok, Bytes} = file:read_file(File)
    after close_table(Tab), file:delete(File) end.

sync_file_functions() ->
    [{file, sync, 1}, {file, open, 2}, {file, write, 2},
     {file, pwrite, 2}, {file, pwrite, 3},
     {file, write_file, 2}, {file, write_file, 3}].

no_file_writes(Calls) ->
    [] = [Call || {file, Function, _} = Call <- Calls,
                  Function =:= write orelse Function =:= pwrite orelse
                      Function =:= write_file],
    ok.

expect_syncs(Worker, Expected, Fun) ->
    {Result, Calls} = trace_file_calls(Worker, [{file, sync, 1}], Fun),
    case length(Calls) of
        Expected -> Result;
        _ -> ct:fail({sync_calls, Expected, Calls})
    end.

trace_file_calls(Worker, Functions, Fun) ->
    Session = trace:session_create(?MODULE, self(), []),
    try
        lists:foreach(fun(MFA) -> 1 = trace:function(Session, MFA, true, []) end,
                      Functions),
        1 = trace:process(Session, Worker, true, [call]),
        Result = Fun(),
        Fence = trace:delivered(Session, all),
        {Result, collect_file_calls(Worker, Fence, [])}
    after trace:session_destroy(Session) end.

collect_file_calls(Worker, Fence, Calls) ->
    receive
        {trace, Worker, call, Call} ->
            collect_file_calls(Worker, Fence, [Call | Calls]);
        {trace_delivered, all, Fence} ->
            lists:reverse(Calls)
    after 10000 -> ct:fail(trace_delivery_timeout)
    end.

inplace_reference(Config) ->
    with_mixed(Config, inplace_reference, set,
      fun(Tab, File, Native, Far) ->
              {ok, LowerPosition} = dets:where(Tab, {lower, aaaa}),
              LowerEntry = entry(Tab, lower, File),
              {ok, NativePosition} = dets:where(Tab, {Native, aaaa}),
              {ok, FarPosition} = dets:where(Tab, {Far, aaaa}),
              true = FarPosition >= ?DETS64_REF_END,
              FarEntry = {SlotPosition, LowerPage, <<_:32, Reference:32>>} =
                  entry(Tab, Far, File),
              true = LowerPage < ?DETS64_BASE,
              true = Reference >= ?DETS64_REF_BASE,
              true = Reference < ?DETS64_REF_END,
              RefNext = ((head(Tab))#head.wide)#dets64.ref_next,
              ok = dets:insert(Tab, [{lower, bbbb}, {Native, bbbb}, {Far, bbbb}]),
              ok = dets:sync(Tab),
              {ok, LowerPosition} = dets:where(Tab, {lower, bbbb}),
              {ok, NativePosition} = dets:where(Tab, {Native, bbbb}),
              {ok, FarPosition} = dets:where(Tab, {Far, bbbb}),
              LowerEntry = entry(Tab, lower, File),
              FarEntry = entry(Tab, Far, File),
              ok = dets:delete(Tab, Far),
              ok = dets:sync(Tab),
              [] = dets:lookup(Tab, Far),
              {SlotPosition, LowerPage, <<0:32, Reference:32>>} =
                  entry(Tab, Far, File),
              0 = resolve(File, SlotPosition+4, Reference),
              ok = dets:insert(Tab, {Far, cccc}),
              ok = dets:sync(Tab),
              {ok, NewFar} = dets:where(Tab, {Far, cccc}),
              true = NewFar >= ?DETS64_REF_END,
              FarEntry = entry(Tab, Far, File),
              NewFar = resolve(File, SlotPosition+4, Reference),
              RefNext = ((head(Tab))#head.wide)#dets64.ref_next,
              check_counts(Tab, File)
      end).

logical_class(Config) ->
    with_mixed(Config, logical_class, set,
      fun(Tab, File, _Native, Far) ->
              Overhead = 12 + byte_size(term_to_binary({Far, <<>>})),
              Old = binary:copy(<<1>>, 1008-Overhead),
              New = binary:copy(<<2>>, 1040-Overhead),
              ok = dets:insert(Tab, {Far, Old}),
              ok = dets:sync(Tab),
              {ok, Position} = dets:where(Tab, {Far, Old}),
              Before = head(Tab),
              1 = proplists:get_value(10, Before#head.no_collections),
              ok = dets:insert(Tab, {Far, New}),
              ok = dets:sync(Tab),
              {ok, Position} = dets:where(Tab, {Far, New}),
              After = head(Tab),
              0 = proplists:get_value(10, After#head.no_collections),
              1 = proplists:get_value(11, After#head.no_collections),
              1 = proplists:get_value(11,
                                      (After#head.wide)#dets64.no_collections),
              [{Far, New}] = dets:lookup(Tab, Far),
              check_counts(Tab, File),
              copy_bchunks(Tab, File, set)
      end).

collection_types(Config) ->
    lists:foreach(
      fun(Type) ->
              with_mixed(Config, collection_types, Type,
                fun(Tab, File, Native, Far) ->
                        Oracle = ets:new(dets64_expected, [Type]),
                        try
                            ets:insert(Oracle, [{lower, aaaa}, {stay, 3},
                                                {Native, aaaa}, {Far, aaaa}]),
                            Added = [{Far, bbbb}, {Far, bbbb},
                                     {1, integer}, {1.0, float}],
                            ets:insert(Oracle, Added),
                            ok = dets:insert(Tab, Added),
                            false = dets:insert_new(Tab, [{fresh, 1}, {Far, 9}]),
                            [] = dets:lookup(Tab, fresh),
                            ok = dets:sync(Tab),
                            check_oracle(Tab, Oracle),
                            check_counts(Tab, File),
                            copy_bchunks(Tab, File, Type),
                            ok = dets:delete_object(Tab, {Far, bbbb}),
                            ets:delete_object(Oracle, {Far, bbbb}),
                            ok = dets:delete(Tab, lower),
                            ets:delete(Oracle, lower),
                            check_oracle(Tab, Oracle),
                            check_counts(Tab, File),
                            Expected = lists:sort(ets:tab2list(Oracle)),
                            ok = dets:safe_fixtable(Tab, false),
                            ok = dets:close(Tab),
                            {ok, Tab} = dets:open_file(Tab, [{file, File},
                                                           {type, Type}]),
                            Expected = snapshot(Tab)
                        after ets:delete(Oracle) end
                end)
      end, [set, bag, duplicate_bag]).

index_growth(Config) ->
    with_table(Config, index_growth, set,
      fun(Tab, File) ->
              Saved = prepare_native_bulk(Tab),
              Objects = [{I, I} || I <- lists:seq(1, 1024)],
              try
                  ok = dets:insert(Tab, Objects),
                  ok = dets:sync(Tab)
              after restore_lower(Tab, Saved) end,
              ok = dets:sync(Tab),
              Expected = lists:sort(Objects),
              ok = dets:safe_fixtable(Tab, true),
              try
                  {_, Used, _} = dets:info(Tab, no_slots),
                  true = Used > 256,
                  Expected = snapshot(Tab),
                  Entries = [entry(Tab, I, File) || I <- lists:seq(1, 1024)],
                  true = lists:any(fun({_P, Page, _}) -> Page >= ?DETS64_BASE end,
                                   Entries),
                  Expected = lists:sort(lists:append([dets:slot(Tab, S) ||
                                                       S <- lists:seq(0, Used-1)])),
                  Keys = maps:from_list([{I, true} || {I, _} <- Objects]),
                  Keys = walk_keys(Tab, dets:first(Tab), #{}),
                  check_counts(Tab, File)
              after dets:safe_fixtable(Tab, false) end,
              ok = dets:close(Tab),
              {ok, Tab} = dets:open_file(Tab, [{file, File}]),
              Expected = snapshot(Tab)
      end).

native_links(Config) ->
    with_mixed(Config, native_links, set,
      fun(Tab, File, _Native, _Far) ->
              Worker = dets:info(Tab, owner),
              Parent = self(),
              Ref = make_ref(),
              force_upper_full(Worker),
              #head{} = sys:replace_state(Worker,
                fun(H) ->
                        H0 = dirty(H),
                        {H1, Part, _} = dets_utils:alloc_index(H0, 2048, 8192),
                        {H2, Segment, _} = dets_utils:alloc_index(H1, 2048, 8192),
                        {H3, ok} = dets_utils:pwrite(H2,
                          [{Part, dets_addr:new_index(part, Part)},
                           {Segment, dets_addr:new_index(segment, Segment)}]),
                        Child = dets_addr:pointer_position(Part, 0),
                        {H4, ChildWrites} = dets_addr:write_pointer(H3, Child,
                                                                  Segment),
                        {H5, ParentWrites} = dets_addr:write_pointer(H4, 316, Part),
                        {H6, ok} = dets_utils:pwrite(H5, ChildWrites ++ ParentWrites),
                        Parent ! {Ref, Part, Segment},
                        H6
                end),
              receive
                  {Ref, Part, Segment} ->
                      true = Part >= ?DETS64_REF_END,
                      true = Segment >= ?DETS64_REF_END,
                      {ok, Fd} = file:open(File, [read, raw, binary]),
                      try
                          Part = dets_addr:read_pointer(Fd, File, 316, 32),
                          Segment = dets_addr:read_pointer(Fd, File,
                                     dets_addr:pointer_position(Part, 0), 64),
                          {ok, Cells} = file:pread(Fd, Segment, 8192),
                          safe_words(Cells)
                      after file:close(Fd) end
              after 5000 -> ct:fail(no_index_allocation) end,
              check_counts(Tab, File)
      end).

continuations(Config) ->
    with_table(Config, continuations, set,
      fun(Tab, File) ->
              Original = [{I, I} || I <- lists:seq(1, 64)],
              ok = dets:insert(Tab, Original),
              ok = dets:sync(Tab),
              ok = dets:safe_fixtable(Tab, true),
              {First, SelectState} = dets:select(Tab, [{'_', [], ['$_']}], 1),
              {ChunkState, Header} = dets:bchunk(Tab, start),
              Native = empty_key(Tab, head(Tab), 0),
              force_lower_full(dets:info(Tab, owner)),
              Added = {Native, aaaa},
              ok = dets:insert(Tab, Added),
              ok = dets:sync(Tab),
              Seen = First ++ finish_select(SelectState),
              true = lists:all(fun(O) -> lists:member(O, Seen) end, Original),
              true = lists:all(fun(O) -> lists:member(O, [Added | Original]) end,
                               Seen),
              true = length(Seen) =:= length(lists:usort(Seen)),
              Bins = export_chunks(Tab, ChunkState, [Header]),
              Copy = {continuation_copy, Tab},
              CopyFile = File ++ ".copy",
              {ok, Copy} = dets:open_file(Copy, [{file, CopyFile}]),
              try
                  ok = dets:init_table(Copy, chunk_input(Bins), [{format, bchunk}]),
                  Original = snapshot(Copy)
              after close_table(Copy), file:delete(CopyFile) end,
              Expected = lists:sort([Added | Original]),
              Expected = snapshot(Tab),
              ok = dets:safe_fixtable(Tab, false)
      end).

finish_select('$end_of_table') ->
    [];
finish_select(State) ->
    case dets:select(State) of
        '$end_of_table' -> [];
        {Objects, Next} -> Objects ++ finish_select(Next)
    end.

repair_compact(Config) ->
    lists:foreach(
      fun(Operation) ->
              with_mixed(Config, Operation, duplicate_bag,
                fun(Tab, File, Native, Far) ->
                        ok = dets:insert(Tab, [{Native, bbbb}, {Native, bbbb},
                                               {Far, adversarial()}]),
                        ok = dets:sync(Tab),
                        Expected = snapshot(Tab),
                        ok = dets:safe_fixtable(Tab, false),
                        ok = dets:close(Tab),
                        Options = case Operation of
                                      repair -> mark_dirty(File), [];
                                      compact -> [{repair, force}]
                                  end,
                        {ok, Tab} = dets:open_file(Tab,
                                      [{file, File}, {type, duplicate_bag} | Options]),
                        Expected = snapshot(Tab),
                        check_counts(Tab, File)
                end)
      end, [repair, compact]).

bulk_native(Config) ->
    lists:foreach(
      fun(Type) ->
              with_table(Config, bulk_native, Type,
                fun(Tab, File) ->
                        Saved = prepare_native_bulk(Tab),
                        Objects = [{I, I} || I <- lists:seq(1, 64)] ++
                                  [{1, extra}, {1, extra}],
                        Oracle = ets:new(dets64_bulk_expected, [Type]),
                        try
                            ets:insert(Oracle, Objects),
                            try
                                ok = dets:init_table(Tab, term_input(Objects))
                            after restore_lower(Tab, Saved) end,
                            check_oracle(Tab, Oracle),
                            check_counts(Tab, File),
                            true = lists:all(fun(O) ->
                                {ok, P} = dets:where(Tab, O), P >= ?DETS64_BASE
                            end, ets:tab2list(Oracle)),
                            copy_bchunks(Tab, File, Type)
                        after ets:delete(Oracle) end
                end)
      end, [set, bag, duplicate_bag]).

wire_guard(_Config) ->
    Values = [0, ?ACTIVE, (1 bsl 32) bor ?ACTIVE,
              (1 bsl 63)-1, ?DETS64_REF_END],
    lists:foreach(fun(V) ->
                          Encoded = dets_wire:encode_uint(V),
                          safe_words(Encoded),
                          V = dets_wire:decode_uint(Encoded)
                  end, Values),
    Payload = adversarial(),
    Packed = dets_wire:encode(Payload),
    safe_words(Packed),
    Payload = dets_wire:decode(Packed, byte_size(Payload)),
    ok.

with_table(Config, Tag, Type, Fun) ->
    File = filename:join(proplists:get_value(priv_dir, Config),
                         atom_to_list(Tag) ++ "_" ++ atom_to_list(Type)),
    Tab = {?MODULE, Tag, Type},
    {ok, Tab} = dets:open_file(Tab, [{file, File}, {type, Type},
                                   {max_no_slots, 2048}]),
    try Fun(Tab, File)
    after close_table(Tab), file:delete(File) end.

with_mixed(Config, Tag, Type, Fun) ->
    with_table(Config, Tag, Type,
      fun(Tab, File) ->
              ok = dets:insert(Tab, [{lower, aaaa}, {stay, 3}]),
              ok = dets:sync(Tab),
              LowerEntry = entry(Tab, lower, File),
              ok = dets:safe_fixtable(Tab, true),
              Native = empty_key(Tab, head(Tab), 0),
              force_lower_full(dets:info(Tab, owner)),
              ok = dets:insert(Tab, {Native, aaaa}),
              ok = dets:sync(Tab),
              {ok, NativePosition} = dets:where(Tab, {Native, aaaa}),
              true = NativePosition >= ?DETS64_BASE,
              LowerEntry = entry(Tab, lower, File),
              Far = empty_key(Tab, head(Tab), 1000),
              force_upper_full(dets:info(Tab, owner)),
              ok = dets:insert(Tab, {Far, aaaa}),
              ok = dets:sync(Tab),
              {ok, FarPosition} = dets:where(Tab, {Far, aaaa}),
              true = FarPosition >= ?DETS64_REF_END,
              LowerEntry = entry(Tab, lower, File),
              Fun(Tab, File, Native, Far)
      end).

head(Tab) -> sys:get_state(dets:info(Tab, owner)).

force_lower_full(Worker) ->
    #head{} = sys:replace_state(Worker,
      fun(H = #head{freelists = {_Frozen, Current}}) ->
              H#head{freelists = {dets_utils:empty_free_lists(), Current}}
      end),
    ok.

force_upper_full(Worker) ->
    #head{} = sys:replace_state(Worker,
      fun(H = #head{wide = Wide}) ->
              {_Frozen, Current} = Wide#dets64.freelists,
              Empty = dets_utils:empty_free_lists(Wide#dets64.levels),
              H#head{wide = Wide#dets64{freelists = {Empty, Current}}}
      end),
    ok.

prepare_native_bulk(Tab) ->
    Worker = dets:info(Tab, owner),
    H0 = head(Tab),
    0 = H0#head.no_objects,
    Saved = H0#head.freelists,
    #head{} = sys:replace_state(Worker,
      fun(H) ->
              H1 = dets_addr:init_wide(H),
              H1#head{freelists = dets_utils:empty_free_lists()}
      end),
    Saved.

restore_lower(Tab, Saved) ->
    #head{} = sys:replace_state(dets:info(Tab, owner),
                               fun(H) ->
                                       H1 = dirty(H),
                                       H1#head{freelists = Saved}
                               end),
    ok.

dirty(#head{update_mode = saved} = Head) ->
    ok = dets_v9:mark_dirty(Head),
    Head#head{update_mode = dirty};
dirty(Head) ->
    Head.

empty_key(Tab, Head, I) ->
    Key = {new, I},
    case dets:slot(Tab, dets_v9:db_hash(Key, Head)) of
        [] -> Key;
        _ -> empty_key(Tab, Head, I+1)
    end.

entry(Tab, Key, File) ->
    H = head(Tab),
    Slot = dets_v9:db_hash(Key, H),
    Segment = Slot div 256,
    {ok, Fd} = file:open(File, [read, raw, binary]),
    try
        Part = dets_addr:read_pointer(Fd, File, 312+4*(Segment div 512), 32),
        Field = dets_addr:pointer_position(Part, Segment rem 512),
        Page = dets_addr:read_pointer(Fd, File, Field, dets_addr:width(Field)),
        Position = dets_addr:slot_position(Page, Slot rem 256),
        {ok, Bytes} = file:pread(Fd, Position, dets_addr:slot_size(Position)),
        {Position, Page, Bytes}
    after file:close(Fd) end.

resolve(File, Owner, Raw) ->
    {ok, Fd} = file:open(File, [read, raw, binary]),
    try dets_addr:resolve_pointer(Fd, File, Owner, Raw)
    after file:close(Fd) end.

snapshot(Tab) ->
    lists:sort(dets:foldl(fun(O, Acc) -> [O | Acc] end, [], Tab)).

check_oracle(Tab, Oracle) ->
    Expected = lists:sort(ets:tab2list(Oracle)),
    Expected = snapshot(Tab),
    Expected = lists:sort(dets:select(Tab, [{'_', [], ['$_']}])),
    Expected = lists:sort(dets:match_object(Tab, '_')),
    ok.

check_counts(Tab, File) ->
    ok = dets:sync(Tab),
    Objects = snapshot(Tab),
    Count = length(Objects),
    Count = dets:info(Tab, size),
    Keys = maps:size(maps:from_list([{element(1, O), true} || O <- Objects])),
    Keys = dets:info(Tab, no_keys),
    Upper = [O || O <- Objects,
                  begin {ok, P} = dets:where(Tab, O), P >= ?DETS64_BASE end],
    UpperCount = length(Upper),
    UpperKeys = maps:size(maps:from_list([{element(1, O), true} || O <- Upper])),
    case (head(Tab))#head.wide of
        undefined -> 0 = UpperCount;
        W -> UpperCount = W#dets64.no_objects, UpperKeys = W#dets64.no_keys
    end,
    {ok, Fd} = file:open(File, [read, raw, binary]),
    try
        {ok, <<9:32>>} = file:pread(Fd, 16, 4),
        {ok, <<LowerCount:32, LowerKeys:32>>} = file:pread(Fd, 32, 8),
        LowerCount = Count-UpperCount,
        LowerKeys = Keys-UpperKeys
    after file:close(Fd) end,
    ok.

copy_bchunks(Tab, File, Type) ->
    ok = dets:safe_fixtable(Tab, true),
    try
        Expected = snapshot(Tab),
        Bins = export_chunks(Tab, start, []),
        [Parameters | Records] = Bins,
        '$hash2' = element(1, binary_to_term(Parameters)),
        9 = element(2, binary_to_term(Parameters)),
        true = lists:all(fun({C, S, B}) ->
                            is_integer(C) andalso is_integer(S) andalso is_binary(B)
                        end, Records),
        Copy = {copy, Tab},
        CopyFile = File ++ ".copy",
        {ok, Copy} = dets:open_file(Copy, [{file, CopyFile}, {type, Type}]),
        try
            ok = dets:init_table(Copy, chunk_input(Bins), [{format, bchunk}]),
            Expected = snapshot(Copy)
        after close_table(Copy), file:delete(CopyFile) end
    after dets:safe_fixtable(Tab, false) end.

export_chunks(Tab, Continuation, Acc) ->
    case dets:bchunk(Tab, Continuation) of
        '$end_of_table' -> lists:append(lists:reverse(Acc));
        {Next, Bins} -> export_chunks(Tab, Next, [Bins | Acc])
    end.

chunk_input([]) ->
    fun(read) -> end_of_input; (close) -> ok end;
chunk_input([Bin | Bins]) ->
    fun(read) -> {[Bin], chunk_input(Bins)}; (close) -> ok end.

term_input([]) ->
    fun(read) -> end_of_input; (close) -> ok end;
term_input(Objects) ->
    fun(read) -> {Objects, term_input([])}; (close) -> ok end.

walk_keys(_Tab, '$end_of_table', Keys) -> Keys;
walk_keys(Tab, Key, Keys) ->
    false = maps:is_key(Key, Keys),
    walk_keys(Tab, dets:next(Tab, Key), Keys#{Key => true}).

mark_dirty(File) ->
    {ok, Fd} = file:open(File, [read, write, raw, binary]),
    try
        {ok, Fd, FH} = dets_v9:read_file_header(Fd, File),
        {ok, H} = dets_v9:check_file_header(FH, Fd),
        ok = dets_addr:mark_dirty(H#head{filename = File})
    after file:close(Fd) end.

adversarial() ->
    Term = term_to_binary({phantom, should_not_appear}),
    Size = 12 + byte_size(Term),
    Block = 1 bsl dets_utils:log2(Size),
    Record = <<Size:32, ?ACTIVE:32, (byte_size(Term)+4):32,
               Term/binary, 0:(Block-Size)/unit:8>>,
    iolist_to_binary([<<0:I/unit:8, Record/binary>> || I <- lists:seq(0, 15)]).

safe_words(Bin) ->
    true = lists:all(fun(Word) -> Word band 16#ffffffff =/= ?ACTIVE end,
                     [Word || <<Word:64>> <= Bin]),
    ok.

close_table(Tab) ->
    try dets:close(Tab) catch _:_ -> ok end.
