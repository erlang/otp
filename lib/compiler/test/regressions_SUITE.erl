%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

%% Test specific code snippets that has crashed the compiler in the past.
-module(regressions_SUITE).
-include_lib("test_server/include/test_server.hrl").

-export([all/0, groups/0, init_per_testcase/2,end_per_testcase/2]).

-export([maps/1]).

groups() -> 
    [{p,test_lib:parallel(),
      [maps]}].

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(2)).

init_per_testcase(_Case, Config) ->
    ?line Dog = ?t:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

%%% test cases

maps(Config) when is_list(Config) ->
    Ts = [{beam_bool_get_elements,
           <<"century(#{ron := operator}, _century) ->
                  if 0.0; _century, _century, _century -> _century end.
           ">>}],
    ok = run(Config, Ts),
    ok.

%% aux

run(Config, Tests) ->
    F = fun({N,P}) ->
                io:format("Compiling test for: ~w~n", [N]),
                case catch run_test(Config, P) of
                    {'EXIT', Reason} -> 
                        ?t:format("~nTest ~p failed.~nReason: ~p~n", [N, Reason]),
                        fail();
                    _ -> ok
                end
        end,
    lists:foreach(F, Tests).


run_test(Conf, Test0) ->
    Module = "regressions_"++test_lib:uniq(),
    Filename = Module ++ ".erl",
    DataDir = ?config(priv_dir, Conf),
    Test = ["-module(", Module, "). ", Test0],
    File = filename:join(DataDir, Filename),
    Def = [binary,export_all,return],
    Opts = [ Opt ++ Def ||
             Opt <- [ [no_postopt],
                      [no_copt],
                      [no_postopt,no_copt],
                      [inline],
                      [inline,no_postopt],
                      []
                    ]],
    ok = file:write_file(File, Test),
    lists:foreach(fun(Opt) ->
                          io:format("  - compiling with ~p~n", [Opt]),
                          {ok,_M,_Bin,_} = compile:file(File,Opt)
                  end, Opts),
    file:delete(File),
    ok.

fail() ->
    io:format("failed~n"),
    ?t:fail().
