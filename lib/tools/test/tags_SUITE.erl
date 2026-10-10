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

-module(tags_SUITE).

%% Test server framework exports
-export([all/0, init_per_suite/1, end_per_suite/1]).

-export([single_file/1, multiple_files/1, bad_file/1, valid_file/1]).

all() ->
    [single_file, multiple_files, bad_file, valid_file].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

single_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "m1.erl"),
    TAGS = filename:join(DataDir, "TAGS"),

    ok = tags:file(File, [{outdir, DataDir}]),
    {ok, Bin} = file:read_file(TAGS),

    %% TAGS format contains function name
    {_, _} = binary:match(Bin, ~"m1"),
    {_, _} = binary:match(Bin, ~"f").

multiple_files(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File1 = filename:join(DataDir, "m1.erl"),
    File2 = filename:join(DataDir, "m2.erl"),
    TAGS = filename:join(DataDir, "TAGS"),

    ok = tags:files([File1, File2], [{outdir, DataDir}]),
    {ok, Bin} = file:read_file(TAGS),

    %% TAGS format contains function name
    %% TAGS format contains function name
    {_, _} = binary:match(Bin, ~"m1"),
    {_, _} = binary:match(Bin, ~"f"),
    {_, _} = binary:match(Bin, ~"m2"),
    {_, _} = binary:match(Bin, ~"g1").

bad_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "bad.erl"),
    TAGS = filename:join(DataDir, "TAGS"),

    ok = tags:file(File, [{outdir, DataDir}]),
    {ok, Bin} = file:read_file(TAGS),

    %% TAGS format contains function name
    {_, _} = binary:match(Bin, ~"bad"),
    {_, _} = binary:match(Bin, ~"foo").

valid_file(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "valid.erl"),
    TAGS = filename:join(DataDir, "TAGS"),

    ok = tags:file(File, [{outdir, DataDir}]),
    {ok, Bin} = file:read_file(TAGS),

    %% TAGS format contains function name
    {_, _} = binary:match(Bin, ~"valid"),
    {_, _} = binary:match(Bin, ~"bar").
