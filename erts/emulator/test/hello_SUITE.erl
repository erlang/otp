%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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

-module(hello_SUITE).
-export([all/0, suite/0, hello/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() ->
    [hello].

hello(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    HelloSrc = filename:join(DataDir, "hello.erl"),
    HelloRoot = filename:rootname(HelloSrc),
    Options = [binary,report_errors,{d,'RUN_AS_TESTCASE'}],
    {ok,hello,Code} = compile:file(HelloSrc, Options),
    {module,Mod} = code:load_binary(hello, HelloRoot, Code),

    ok = hello_test(),

    true = code:delete(Mod),
    false = code:purge(Mod),

    ok.

hello_test() ->
    Ps0 = processes(),
    SystemLogger = erlang:system_info(system_logger),

    L = [<<"-root">>,<<"/home/user/git/otp">>,<<"-progname">>,<<"/home/user/git/otp/bin/cerl -debug +asmdump">>,<<"--">>,<<"-home">>,<<"/home/user">>,<<"--">>,<<"-kernel">>,<<"shell_history">>,<<"enabled">>,<<"--">>,<<"--">>],

    try
        hello:hello(L)
    after
        _ = erlang:system_flag(system_logger, SystemLogger),
        Ps1 = processes(),
        Ps = Ps1 -- Ps0,

        %% Kill any new test processes. There should at least be the
        %% system logger process installed by hello.
        [begin
             io:format("~p\n", [process_info(Pid)]),
             unlink(Pid),
             exit(Pid, kill)
         end || Pid <- Ps]
    end,

    ok.
