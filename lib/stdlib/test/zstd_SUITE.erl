%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
-module(zstd_SUITE).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).

-export([bulk/1, bulk_with_dict/1,
         pledge/1,
         cstream/1, cstream_with_dict/1,
         dstream/1, dstream_with_dict/1,
         parameters/1, dict_api/1,
         doc_tests/1
        ]).

-export([generate_dict/0]).

-include_lib("stdlib/include/assert.hrl").

suite() -> [].

all() ->
    [{group, zstd}].

groups() ->
    [{zstd, [parallel],
      [bulk,
       cstream,
       pledge,
       dstream,
       parameters,
       {group, dict} ]},
     {dict, [parallel],
      [ bulk_with_dict,
        cstream_with_dict,
        dstream_with_dict,
        dict_api,
        doc_tests ]} ].

init_per_suite(Config) ->
    Config.

generate_dict() ->
    DataDir = filename:join([os:getenv("ERL_TOP"), "lib", "stdlib",
                             "test", "zstd_SUITE_data"]),
    ok = filelib:ensure_path(DataDir),
    TrainingSetDir = filename:join(DataDir,"training"),
    Dict = filename:join(DataDir, "dict"),
    ok = file:make_dir(TrainingSetDir),
    generate_training_set(TrainingSetDir, 1_000),
    io:format("~ts~n",[os:cmd("zstd --train " ++
                                  TrainingSetDir ++ "/* -o " ++ Dict)]),
    file:del_dir_r(TrainingSetDir),
    ok.

generate_training_set(_Dir, 0) -> [];
generate_training_set(Dir, N) ->
    ok = file:write_file(filename:join(Dir, integer_to_list(N)),
                         lists:duplicate(rand:uniform(10000), integer_to_list(N))),
    generate_training_set(Dir, N - 1).

init_per_group(dict, Config) ->
    Dict = filename:join(proplists:get_value(data_dir, Config),
                         "dict"),

    [{dict, Dict} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

bulk(_Config) ->

    Data = ~"abc",
    Compressed = zstd:compress(Data),
    ?assertEqual(Data, iob(zstd:decompress(Compressed))),

    Compressed = zstd:compress(binary_to_list(Data)),
    ?assertEqual(Data, iob(zstd:decompress(Compressed))),

    Compressed = zstd:compress([Data]),
    ?assertEqual(Data, iob(zstd:decompress(Compressed))),

    %% Test decompressing of multiple frames
    DoubleCompressed = [zstd:compress([Data]), zstd:compress([Data])],
    ?assertEqual(<<Data/binary,Data/binary>>,
                 iob(zstd:decompress(DoubleCompressed))),

    %% Test compression of a large amount of data
    Iter = lists:seq(0,large_data_size()),
    IOV = lists:duplicate(10, iob([rand:uniform(255) || _ <- Iter])),
    IOVCompressed = zstd:compress(IOV, #{ compressionLevel => -20 }),
    ?assertEqual(iob(IOV), iob(zstd:decompress(IOVCompressed))),

    %% bulk with context
    {ok, CCxt} = zstd:context(compress, #{ compressionLevel => 15 }),
    CCtxCompressed = zstd:compress(Data, CCxt),
    {ok, DCxt} = zstd:context(decompress),
    ?assertEqual(iob(Data), iob(zstd:decompress(CCtxCompressed, DCxt))),
    zstd:close(CCxt),
    zstd:close(DCxt),

    P = self(),
    spawn(fun() -> {ok, Ctx} = zstd:context(compress), P ! {ctx, Ctx} end),
    OtherCCtx = receive {ctx, Ctx} -> Ctx end,

    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:compress(Data, OtherCCtx),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:stream(OtherCCtx, Data),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:finish(OtherCCtx, Data),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:set_parameter(OtherCCtx, compressionLevel, 14),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:set_parameter(OtherCCtx, dictionary, ~"abc"),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:set_parameter(OtherCCtx, setPledgeSrcSize, 0),
    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:get_parameter(OtherCCtx, compressionLevel),

    ok.

bulk_with_dict(Config) ->

    {ok, Dict} = file:read_file(proplists:get_value(dict, Config)),
    Data = ~"abc",

    Compressed = zstd:compress(Data, #{ dictionary => Dict }),
    ?assertEqual(Data, iob(zstd:decompress(Compressed, #{ dictionary => Dict }))),

    {ok, CDict} = zstd:dict(compress, Dict),
    {ok, DDict} = zstd:dict(decompress, Dict),
    Compressed = zstd:compress(Data, #{ dictionary => CDict }),
    ?assertEqual(Data, iob(zstd:decompress(Compressed, #{ dictionary => DDict }))),

    {'EXIT',{{zstd_error, ~"Dictionary mismatch"},_}} =
        catch zstd:decompress(Compressed),

    P = self(),
    spawn(fun() -> {ok, Ctx} = zstd:context(compress), P ! {ctx, Ctx} end),
    OtherCCtx = receive {ctx, Ctx} -> Ctx end,

    {'EXIT',{not_on_controlling_process, _}} =
        catch zstd:set_parameter(OtherCCtx, dictionary, CDict),

    ok.

cstream(_Config) ->

    {ok, CCtx} = zstd:context(compress),

    Data = ~"aaa",

    {done, C1} = zstd:finish(CCtx, Data),
    ?assertEqual(Data, iob(zstd:decompress(C1))),

    {continue, C2_1} = zstd:stream(CCtx, ~"a"),
    {continue, C2_2} = zstd:stream(CCtx, ~"a"),
    {done, C2_3} = zstd:finish(CCtx, ~"a"),
    ?assertEqual(Data, iob(zstd:decompress([C2_1, C2_2, C2_3]))),

    {continue, C3_1} = zstd:stream(CCtx, ~"aaa"),
    ok = zstd:reset(CCtx),
    {continue, C3_1} = zstd:stream(CCtx, [~"aaa"]),
    {done, C3_2} = zstd:finish(CCtx, <<>>),
    ?assertEqual(Data, iob(zstd:decompress([C3_1, C3_2]))),

    %% Test compression of a large amount of data
    LargeData = iob([rand:uniform(255) || _ <- lists:seq(0,large_data_size())]),

    %% On debug emulator we set some parameters to make the threshold for when
    %% zstd fills it internal buffers lower so that we get {continue, Remain, Data}
    [ begin
          ok = zstd:set_parameter(CCtx, targetCBlockSize, 1340),
          ok = zstd:set_parameter(CCtx, windowLog, 10),
          ok = zstd:set_parameter(CCtx, chainLog, 6),
          ok = zstd:set_parameter(CCtx, hashLog, 6)
      end || erlang:system_info(emu_type) =:= debug],
    {continue, Remain, C4_1} = zstd:stream(CCtx, LargeData),
    {done, C4_2} = zstd:finish(CCtx, [Remain]),
    ?assertEqual(iob([LargeData]), iob(zstd:decompress([C4_1,C4_2]))),

    {done, C5_1} = zstd:finish(CCtx, [LargeData, LargeData]),
    ?assertEqual(iob([LargeData, LargeData]), iob(zstd:decompress([C5_1]))),

    ok = zstd:close(CCtx),
    {'EXIT',{badarg,_}} = catch zstd:finish(CCtx, Data),

    ok.

pledge(_Config) ->

    AAAAs = iob(lists:duplicate(10_000, $a)),
    BBBBs = iob(lists:duplicate(10_000, $b)),

    {ok, CCtx} = zstd:context(compress),
    {continue, C1_1} = zstd:stream(CCtx, AAAAs),
    {done, C1_2} = zstd:finish(CCtx, BBBBs),

    {ok, #{ frameContentSize := undefined } } = zstd:get_frame_header([C1_1, C1_2]),
    ?assertEqual(<<AAAAs/binary,BBBBs/binary>>,
                 iob(zstd:decompress([<<Byte>> || <<Byte>> <= iob([C1_1,C1_2])]))),
    ?assertEqual(<<AAAAs/binary,BBBBs/binary>>, iob(zstd:decompress(iob([C1_1,C1_2])))),

    ok = zstd:set_parameter(CCtx, pledgedSrcSize, 20_000 ),
    {continue, C2_1} = zstd:stream(CCtx, AAAAs),
    {done, C2_2} = zstd:finish(CCtx, BBBBs),

    {ok, #{ frameContentSize := 20_000 } } = zstd:get_frame_header([C2_1, C2_2]),
    ?assertEqual(<<AAAAs/binary,BBBBs/binary>>,
                 iob(zstd:decompress([<<Byte>> || <<Byte>> <= iob([C2_1, C2_2])]))),
    ?assertEqual(<<AAAAs/binary,BBBBs/binary>>, iob(zstd:decompress(iob([C2_1, C2_2])))),

    ok = zstd:set_parameter(CCtx, pledgedSrcSize, 20 ),
    {'EXIT',{{zstd_error, ~"Src size is incorrect"},_}} = catch zstd:stream(CCtx, AAAAs),

    ok = zstd:set_parameter(CCtx, pledgedSrcSize, 20_001 ),
    {continue, C2_1} = zstd:stream(CCtx, AAAAs),
    {'EXIT',{{zstd_error, ~"Src size is incorrect"},_}} = catch zstd:finish(CCtx, BBBBs),

    ok.

cstream_with_dict(Config) ->

    {ok, Dict} = file:read_file(proplists:get_value(dict, Config)),
    DictID = zstd:get_dict_id(Dict),
    {ok, CCtx} = zstd:context(compress),

    zstd:set_parameter(CCtx, dictionary, Dict),

    Data = ~"aaa",
    {done, C1} = zstd:finish(CCtx, Data),
    ?assertEqual(Data, iob(zstd:decompress(C1, #{ dictionary => Dict }))),
    DictID = zstd:get_dict_id(C1),

    {continue, C2_1} = zstd:stream(CCtx, ~"a"),
    {continue, C2_2} = zstd:stream(CCtx, ~"a"),
    {done, C2_3} = zstd:finish(CCtx, ~"a"),
    ?assertEqual(Data, iob(zstd:decompress([C2_1, C2_2, C2_3], #{ dictionary => Dict }))),

    {continue, C3_1} = zstd:stream(CCtx, ~"aaa"),
    ok = zstd:reset(CCtx),
    {continue, C3_1} = zstd:stream(CCtx, ~"aaa"),
    {done, C3_2} = zstd:finish(CCtx, <<>>),
    ?assertEqual(Data, iob(zstd:decompress([C3_1, C3_2], #{ dictionary => Dict }))),

    zstd:set_parameter(CCtx, dictionary, ""),

    {done, C4} = zstd:finish(CCtx, Data),
    ?assertEqual(Data, iob(zstd:decompress(C4))),
    0 = zstd:get_dict_id(C4),

    {continue, C4_1} = zstd:stream(CCtx, ~"a"),
    {continue, C4_2} = zstd:stream(CCtx, ~"a"),
    {done, C4_3} = zstd:finish(CCtx, ~"a"),
    ?assertEqual(Data, iob(zstd:decompress([C4_1, C4_2, C4_3]))),

    {continue, C5_1} = zstd:stream(CCtx, ~"aaa"),
    ok = zstd:reset(CCtx),
    {continue, C5_1} = zstd:stream(CCtx, ~"aaa"),
    {done, C5_2} = zstd:finish(CCtx, <<>>),
    ?assertEqual(Data, iob(zstd:decompress([C5_1, C5_2]))),

    {'EXIT',{{zstd_error, ~"Dictionary mismatch"}, _}} =
        catch iob(zstd:decompress(iob([C1, C4]))),

    ok.

dstream(_Config) ->

    {ok, DCtx} = zstd:context(decompress),

    Data = ~"aaa",
    CompressedData = zstd:compress(Data),
    {First, Last} = lists:split(4, binary_to_list(iob(CompressedData))),

    {done, D1} = zstd:finish(DCtx, CompressedData),
    ?assertEqual(Data, iob(D1)),

    {continue, D2_1} = zstd:stream(DCtx, First),
    {done, D2_2} = zstd:finish(DCtx, Last),
    ?assertEqual(Data, iob([D2_1, D2_2])),

    {continue, D3_1} = zstd:stream(DCtx, First),
    ok = zstd:reset(DCtx),
    {continue, D3_1} = zstd:stream(DCtx, [First]),
    {continue, D3_2} = zstd:stream(DCtx, Last),
    {done, D3_3} = zstd:finish(DCtx, <<>>),
    ?assertEqual(Data, iob([D3_1, D3_2, D3_3])),

    ok = zstd:close(DCtx),
    {'EXIT',{badarg,_}} = catch zstd:finish(DCtx, Data),

    ok.

dstream_with_dict(Config) ->

    {ok, Dict} = file:read_file(proplists:get_value(dict, Config)),
    {ok, DCtx} = zstd:context(decompress),

    Data = ~"aaa",
    CompressedData = zstd:compress(Data, #{ dictionary => Dict }),
    {First, Last} = lists:split(4, binary_to_list(iob(CompressedData))),

    ok = zstd:set_parameter(DCtx, dictionary, Dict),

    {done, D1} = zstd:finish(DCtx, CompressedData),
    Data = iob(D1),

    {continue, D2_1} = zstd:stream(DCtx, First),
    {done, D2_2} = zstd:finish(DCtx, Last),
    Data = iob([D2_1, D2_2]),

    {continue, D3_1} = zstd:stream(DCtx, First),
    ok = zstd:reset(DCtx),
    {continue, D3_1} = zstd:stream(DCtx, [First]),
    {continue, D3_2} = zstd:stream(DCtx, Last),
    {done, D3_3} = zstd:finish(DCtx, <<>>),
    Data =  iob([D3_1, D3_2, D3_3]),

    ok = zstd:set_parameter(DCtx, dictionary, <<>>),
    {'EXIT',{{zstd_error, ~"Dictionary mismatch"}, _}} =
        catch zstd:finish(DCtx, CompressedData),

    ok.

parameters(_Config) ->

    {ok, CCxt} = zstd:context(compress),

    CParams = [{compressionLevel, 5},
               {windowLog, 10},
               {hashLog, 6},
               {chainLog, 6},
               {searchLog, 1},
               {minMatch, 5},
               {targetLength, 5},
               {strategy, fast},
               {targetCBlockSize, 5},
               {enableLongDistanceMatching, true},
               {ldmHashLog, 6},
               {ldmMinMatch, 6},
               {ldmBucketSizeLog, 5},
               {ldmHashRateLog, 5},
               {contentSizeFlag, false},
               {checksumFlag, true},
               {dictIDFlag, false}],

    %% Check that we can get default parameters
    [case {Key, zstd:get_parameter(CCxt, Key)} of
         {Key, V} when is_integer(V), is_integer(Value) ->
             ok;
         {Key, V} when is_boolean(V), is_boolean(Value)  ->
             ok;
         {Key, V} when is_atom(V), is_atom(Value) ->
             ok
     end || {Key, Value} <- CParams],

    %% Set all available parameters
    [ok = zstd:set_parameter(CCxt, Key, Value) || {Key, Value} <- CParams],

    %% Check that we can get the non-default parameters
    %% We cannot check that the actual values are correct as zstd
    %% changes some of them when set.
    [case {Key, zstd:get_parameter(CCxt, Key)} of
         {Key, V} when is_integer(V), is_integer(Value) ->
             ok;
         {Key, V} when is_boolean(V), is_boolean(Value)  ->
             ok;
         {Key, V} when is_atom(V), is_atom(Value) ->
             ok
     end || {Key, Value} <- CParams],

    zstd:close(CCxt),

    {ok, DCxt} = zstd:context(decompress),

    DParams = [{windowLogMax, 10}],

    %% Check that we can get default parameters
    [case zstd:get_parameter(DCxt, Key) of
         V when is_integer(V), is_integer(Value) ->
             ok
     end || {Key, Value} <- DParams],

    %% Set all available parameters
    [ok = zstd:set_parameter(DCxt, Key, Value) || {Key, Value} <- DParams],

    %% Check that we can get the non-default parameters
    %% We cannot check that the actual values are correct as zstd
    %% changes some of them when set.
    [case zstd:get_parameter(DCxt, Key) of
         V when is_integer(V), is_integer(Value) ->
             ok
     end || {Key, Value} <- DParams],

    zstd:close(DCxt),

    ok.

dict_api(Config) ->

    {ok, Dict} = file:read_file(proplists:get_value(dict, Config)),
    DictId = zstd:get_dict_id(Dict),

    {ok, CDict} = zstd:dict(compress, Dict, #{ compressionLevel => -5 }),
    DictId = zstd:get_dict_id(CDict),

    {ok, DDict} = zstd:dict(decompress, Dict),
    DictId = zstd:get_dict_id(DDict),

    {ok, CCtx} = zstd:context(compress, #{ dictionary => CDict }),
    {'EXIT', _} = catch zstd:set_parameter(CCtx, dictionary, DDict),

    {ok, DCtx} = zstd:context(decompress, #{ dictionary => DDict }),
    {'EXIT', _} = catch zstd:set_parameter(DCtx, dictionary, CDict),

    {'EXIT', _} = catch zstd:dict(compress, [1,2,3]),
    {'EXIT', _} = catch zstd:dict(decompress, [1,2,3]),

    ok.


doc_tests(Config) ->
    case erlang:system_info(emu_type) of
        debug ->
            %% As return values from decompress are split into an iovec with
            %% multiple element on debug emulators, we skip running these test
            {skip, "Don't run in debug emulator"};
        _ ->
            {ok, Dict} = file:read_file(proplists:get_value(dict, Config)),
            DictBinding = erl_eval:add_binding('Dict', Dict, erl_eval:new_bindings()),
            File = filename:join(proplists:get_value(priv_dir, Config), "example"),
            ok = file:write_file(File, ~"lorem ipsum"),
            shell_docs:test(
              zstd,
              [
               {module_doc, erl_eval:add_binding('File', File, erl_eval:new_bindings())},
               {{function, get_dict_id, 1}, DictBinding},
               {{function, dict, 3}, DictBinding}
              ]
             )
    end.

iob(IOList) ->
    erlang:iolist_to_binary(IOList).

%% Doing enif_inspect on a large binary is very slow in debug
%% build as it does a checksum of entire binary every time.
%% So we limit the size of the input data in debug builds.
large_data_size() ->
    case erlang:system_info(emu_type) =:= debug of
        true -> 2_000;
        false -> 1_000_000
    end.
