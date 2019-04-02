%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2018. All Rights Reserved.
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

-module(efile_SUITE).
-export([all/0, suite/0]).
-export([iter_max_files/1, proc_zero_sized_files/1]).

-export([do_iter_max_files/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [iter_max_files, proc_zero_sized_files].

%%
%% Open as many files as possible. Do this several times and check 
%% that we get the same number of files every time.
%%

iter_max_files(Config) when is_list(Config) ->
    case os:type() of
        {win32, _} -> {skip, "Windows lacks a hard limit on file handles"};
        _ -> iter_max_files_1(Config)
    end.

iter_max_files_1(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    TestFile = filename:join(DataDir, "existing_file"),
    case erlang:system_info(debug_compiled) of
        true ->
            N = 5;
        false ->
            N = 10
    end,
    %% Run on a different node in order to make the test more stable.
    Dir = filename:dirname(code:which(?MODULE)),
    {ok,Node} = test_server:start_node(test_iter_max_files,slave,
                                       [{args,"-pa " ++ Dir}]),
    L = rpc:call(Node,?MODULE,do_iter_max_files,[N, TestFile]),
    test_server:stop_node(Node),
    io:format("Number of files opened in each test:~n~w\n", [L]),
    verify_max_files(L),
    Head = hd(L),
    if  Head >= 2 -> ok;
        true -> ct:fail(too_few_files)
    end,
    {comment, "Max files: " ++ integer_to_list(hd(L))}.

do_iter_max_files(N, Name) when N > 0 -> 
    [max_files(Name)| do_iter_max_files(N-1, Name)];
do_iter_max_files(_, _) ->
    [].

%% The attempts shouldn't vary too much; we used to require that they were all
%% exactly equal, but after we reimplemented the file driver as a NIF we
%% noticed that the only reason it was stable on Darwin was because the port
%% limit was hit before ulimit.
verify_max_files(Attempts) ->
    N = length(Attempts),
    Mean = lists:sum(Attempts) / N,
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Attempts]) / N,
    true = math:sqrt(Variance) =< 1 + (Mean / 1000).

max_files(Name) ->
    Fds = open_files(Name),
    N = length(Fds),
    close_files(Fds),
    N.

close_files([Fd| Fds]) ->
    file:close(Fd),
    close_files(Fds);
close_files([]) ->
    ok.

open_files(Name) ->
    case file:open(Name, [read,raw]) of
        {ok, Fd} ->
            [Fd| open_files(Name)];
        {error, _Reason} ->
            %		  io:format("Error reason: ~p", [_Reason]),
            []
    end.

%% @doc If /proc filesystem exists (no way to know if it is real proc or just
%% a /proc directory), let's read some zero sized files 500 times each, while
%% ensuring that response isn't empty << >>
proc_zero_sized_files(Config) when is_list(Config) ->
    TestFiles0 = [%% Some files which exist on Linux but might be missing on
                  %% other systems
                  "/proc/cpuinfo",
                  "/proc/meminfo",
                  "/proc/partitions",
                  "/proc/swaps",
                  "/proc/version",
                  "/proc/uptime",
                  %% curproc is present on FreeBSD
                  "/proc/curproc/cmdline"],

    TestFiles = [F || F <- TestFiles0, filelib:is_file(F)],

    case TestFiles of
        [_|_] ->
            %% For 6 inputs and 500 attempts each this do run anywhere
            %% between 500 and 3000 function calls.
            [do_proc_zero_sized(F, 500) || F <- TestFiles],
            ok;
        [] ->
            {skip, "Failed to find any known zero-sized files"}
    end.

%% @doc Test one file N times to also trigger possible leaking fds and memory
do_proc_zero_sized(_Filename, 0) -> ok;
do_proc_zero_sized(Filename, N) ->
    Data = file:read_file(Filename),
    ?assertNotEqual(<<>>, Data),
    do_proc_zero_sized(Filename, N-1).
