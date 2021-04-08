%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021-2021. All Rights Reserved.
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

-module(perf_SUITE).

-export([all/0, suite/0, init_per_suite/1, end_per_suite/1]).
-export([symbols/1, annotate/1]).

suite() ->
    [{timetrap, {minutes, 4}}].

all() -> 
    [symbols, annotate].

init_per_suite(Config) ->
    case os:find_executable("perf") of
        false ->
            {skip, "perf not found"};
        _Perf ->
            PerfVsn = os:cmd("perf version"),
            {match,[Vsn]} = re:run(PerfVsn, "perf version ([^.])",
                                   [{capture,all_but_first,list}]),
            case list_to_integer(Vsn) >= 5 of
                true ->
                    DataFile = filename:join(
                                 proplists:get_value(priv_dir, Config),
                                 "init_test.data"),
                    Cmd = "perf record -q -o " ++ DataFile ++ " ls",
                    os:cmd(Cmd),
                    Script = os:cmd("perf script -i " ++ DataFile),
                    ct:log("~ts",[Script]),
                    case re:run(Script, "^\\W+ls",[multiline]) of
                        {match, _} ->
                            [{sobefore,get_tmp_so_files()}|Config];
                        nomatch ->
                            {skip, "could not run `"++ Cmd ++"`"}
                    end;
                false ->
                    {skip,"too old perf version: " ++ PerfVsn}
            end
    end.

end_per_suite(Config) ->
    %% perf inject writes data to /tmp and ~/.debug/tmp so we need to clean
    %% that up after the tests are done.
    SoToDelete = get_tmp_so_files() -- proplists:get_value(sobefore, Config),
    lists:foreach(
      fun(File) ->
              case file:delete(File) of
                  {error,eperm} ->
                      ok = file:del_dir_r(File);
                  ok ->
                      ok
              end
      end, SoToDelete),
    ok.

get_tmp_so_files() ->
    {ok, Home} = init:get_argument(home),
    filelib:wildcard("/tmp/jitted-*.so") ++
        filelib:wildcard(filename:join([Home,".debug","tmp","jitted-*.so"])).

symbols(Config) ->
    DataFile = filename:join(
                 proplists:get_value(priv_dir, Config),
                 atom_to_list(?FUNCTION_NAME) ++ ".data"),
    os:cmd("perf record -q -o " ++ DataFile ++ " " ++
           "erl +S 1 +JPperf true -noshell -eval '[lists:seq(1,10000) || _ <- lists:seq(1,1000)].' -s init stop"),
    Script = os:cmd("perf script -i " ++ DataFile),
    case re:run(Script,"\\$lists:seq[^/]+/[0-9]",[global,{capture,first,list}]) of
        {match,Matches} ->
            ct:log("Found these symbols: ~p",[lists:usort(Matches)]),
            ok;
        nomatch ->
            ct:fail("Did not find lists:seq symbol in:~n~ts",[Script])
    end.
    
annotate(Config) ->
    DataFile = filename:join(
                 proplists:get_value(priv_dir, Config),
                 atom_to_list(?FUNCTION_NAME) ++ ".data"),
    os:cmd("perf record -k mono -q -o " ++ DataFile ++ " " ++
           "erl +S 1 +JPperf true -noshell -eval '[lists:seq(1,10000) || _ <- lists:seq(1,1000)].' -s init stop"),
    Script = os:cmd("perf script -i " ++ DataFile),

    %% When doing "perf inject" the symbol of each function is changed
    %% from $lists:seq_loop/3 to lists:seq_loop/3. I don't know why that
    %% is, seems like a very odd bug in perf... so we only include it
    %% in our match if it exists
    {match, Symbols} = re:run(Script, "\\$?lists:seq[^/]+/[0-9]+",
                              [global,{capture,first,list}]),
    [Symbol|_] = lists:usort(Symbols),
    JitFile = DataFile ++ ".jit.data",
    "" = os:cmd("perf inject -j -i " ++ DataFile ++ " -o " ++ JitFile),
    Anno = os:cmd("perf annotate --stdio -i " ++ JitFile ++ " " ++ Symbol ++ ""),
    case re:run(Anno,"Disassembly of section .text:") of
        {match,_} ->
            ok;
        nomatch ->
            ct:log("Did not find disassembly test for ~ts.~n~ts",
                   [Symbol, Anno])
    end.
    
