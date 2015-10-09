%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2012. All Rights Reserved.
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
-module(beam_disasm_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).

-export([stripped/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [stripped].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

stripped(doc) ->
    ["Check that stripped beam files can be disassembled"];
stripped(Config) when is_list(Config) ->
    ?line PrivDir = ?config(priv_dir, Config),
    ?line SrcName = filename:join(PrivDir, "tmp.erl"),
    ?line BeamName = filename:join(PrivDir, "tmp.beam"),
    Prog = <<"-module(tmp).\n-export([tmp/0]).\ntmp()->ok.\n">>,
    ?line ok = file:write_file(SrcName, Prog),
    ?line {ok, tmp} =
	compile:file(SrcName, [{outdir, PrivDir}]),
    ?line {beam_file, tmp, _, Attr, CompileInfo, [_|_]} =
	beam_disasm:file(BeamName),
    ?line true = is_list(Attr),
    ?line true = is_list(CompileInfo),
    ?line {ok, {tmp, _}} = beam_lib:strip(BeamName),
    ?line {beam_file, tmp, _, [], [], [_|_]} =
	beam_disasm:file(BeamName),
    ok.
