%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2020-2025. All Rights Reserved.
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

-module(io_ansi_SUITE).
-moduledoc false.

-behaviour(ct_suite).

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

-export([enabled/1, fwrite/1, fwrite_test/0, doctests/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

suite() ->
    [].

all() ->
    [ doctests, enabled, fwrite ].


groups() ->
    [ ].

init_per_suite(Config) ->
    shell_test_lib:start_tmux(Config).

end_per_suite(Config) ->
    shell_test_lib:stop_tmux(Config).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

enabled(Config) ->

    %% Test that when stdout is not a tty, enabled returns false
    Erl = ct:get_progname(),
    ?assertEqual("false", os:cmd(Erl ++ ~s` -noshell -eval 'io:format("~p",[io_ansi:enabled()])' -s init stop`)),

    %% Test that when stdout is a tty and various term settings
    enabled_test(true, "xterm-256color", Config),
    enabled_test(false, "abc", Config),
    enabled_test(false, "dumb", Config).

enabled_test(Expect, TermType, Config) ->
    Term = shell_test_lib:setup_tty([{env, [{"TERM",TermType}]}|Config]),
    try
        ?assertEqual(Expect,
                     shell_test_lib:rpc(
                       Term,
                       fun() ->
                               group_leader(whereis(user), self()),
                               io_ansi:enabled()
                       end)),
        ?assertEqual(Expect,
                     shell_test_lib:rpc(
                       Term,
                       fun() ->
                               io_ansi:enabled(user)
                       end)),
        ?assertEqual(Expect,
                     shell_test_lib:rpc(
                       Term,
                       fun() ->
                               group_leader(whereis(user), self()),
                               {group_leader, GL} = erlang:process_info(shell:whereis(), group_leader),
                               io_ansi:enabled(GL)
                       end))
    after
        shell_test_lib:stop_tty(Term)
    end.

fwrite(Config) ->
    {ok, Peer, Node} = ?CT_PEER(#{ env => [{"TERM","dumb"}] }),
    DumbUser = erpc:call(Node, erlang, whereis, [user]),
    false = erpc:call(Node, io_ansi, enabled, [DumbUser]),

    Term = shell_test_lib:setup_tty([{args,["-noshell","-eval","io_ansi_SUITE:fwrite_test()."]},
                                     {env, [{"TERM","xterm-256color"}]} | Config]),

    try
        try
            shell_test_lib:send_tty(Term, atom_to_list(Node) ++ "\n"),

            shell_test_lib:check_content(Term, "\n\e\\[34mblue", #{ args => "-e" }),
            shell_test_lib:check_content(Term, "\n\e\\[31mred", #{ args => "-e" })

        after
            shell_test_lib:stop_tty(Term)
        end,

        DumbTerm = shell_test_lib:setup_tty([{args,["-noshell","-eval","io_ansi_SUITE:fwrite_test()."]},
                                             {env, [{"TERM","dumb"}]} | Config]),

        try
            shell_test_lib:send_tty(DumbTerm, atom_to_list(Node) ++ "\n"),

            shell_test_lib:check_content(DumbTerm, "\nblue", #{ args => "-e" }),
            shell_test_lib:check_content(DumbTerm, "\nred", #{ args => "-e" })

        after
            shell_test_lib:stop_tty(DumbTerm)
        end
    after
        peer:stop(Peer)
    end.

fwrite_test() ->
    NodeName = string:trim(io:get_line("")),

    io_ansi:fwrite([blue, "blue\n"]),
    erpc:call(list_to_atom(NodeName), fun() -> io_ansi:fwrite([red, "red\n"]) end),
    
    ok.

doctests(Config) ->
    Term = shell_test_lib:setup_tty([{env, [{"TERM","xterm-256color"}]}|Config]),
    try
        shell_test_lib:rpc(Term, fun() ->
            group_leader(whereis(user), self()),
            shell_docs:test(io_ansi, [])
        end)
    after
        shell_test_lib:stop_tty(Term)
    end.