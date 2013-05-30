%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%
%% Test encode/decode of dictionary-related modules. Each test case
%% runs multiple tests in parallel since many of the tests are just
%% the same code with different in-data: implementing each test as a
%% single testcase would make for much duplication with ct's current
%% requirement of one function per testcase.
%%

-module(diameter_codec_SUITE).

-export([suite/0,
         all/0,
         init_per_testcase/2,
         end_per_testcase/2]).

%% testcases
-export([base/1,
         gen/1,
         lib/1,
         unknown/1]).

-include("diameter_ct.hrl").

-define(L, atom_to_list).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [base, gen, lib, unknown].

init_per_testcase(gen, Config) ->
    [{application, ?APP, App}] = diameter_util:consult(?APP, app),
    {modules, Ms} = lists:keyfind(modules, 1, App),
    [_|_] = Gs = lists:filter(fun(M) ->
                                      lists:prefix("diameter_gen_", ?L(M))
                              end,
                              Ms),
    [{dicts, Gs} | Config];

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_, _) ->
    ok.

%% ===========================================================================

base(_Config) ->
    diameter_codec_test:base().

gen([{dicts, Ms} | _]) ->
    lists:foreach(fun diameter_codec_test:gen/1, Ms).

lib(_Config) ->
    diameter_codec_test:lib().

%% Have a separate AVP dictionary just to exercise more code.
unknown(Config) ->
    Priv = proplists:get_value(priv_dir, Config),
    Data = proplists:get_value(data_dir, Config),
    ok = make(Data, "recv.dia"),
    ok = make(Data, "avps.dia"),
    {ok, _, _} = compile("diameter_test_avps.erl"),
    ok = make(Data, "send.dia"),
    {ok, _, _} = compile("diameter_test_send.erl"),
    {ok, _, _} = compile("diameter_test_recv.erl"),
    {ok, _, _} = compile(filename:join([Data, "diameter_test_unknown.erl"]),
                         [{i, Priv}]),
    diameter_test_unknown:run().

make(Dir, File) ->
    diameter_make:codec(filename:join([Dir, File])).

compile(File) ->
    compile(File, []).

compile(File, Opts) ->
    compile:file(File, [return | Opts]).
