%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2018. All Rights Reserved.
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

%%
-module(erl_global_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erl_global_SUITE_data/erl_global_test_cases.hrl").

-export([all/0,suite/0,
         init_per_testcase/2,
         erl_global_registration/1,
         erl_global_whereis/1, erl_global_names/1]).

-import(runner, [get_term/1,send_term/2]).

-define(GLOBAL_NAME, global_register_node_test).

all() ->
    [erl_global_registration, erl_global_whereis, erl_global_names].

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

erl_global_registration(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    ok = erl_global_register(P, Fd, ?GLOBAL_NAME),
    ok = erl_global_unregister(P, Fd, ?GLOBAL_NAME),

    0 = erl_close_connection(P,Fd),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

erl_global_whereis(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    Self = self(),
    yes = global:register_name(?GLOBAL_NAME, Self),
    Self = erl_global_whereis(P, Fd, ?GLOBAL_NAME),
    global:unregister_name(?GLOBAL_NAME),
    0 = erl_close_connection(P, Fd),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

erl_global_names(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    Self = self(),
    global:register_name(?GLOBAL_NAME, Self),
    {Names1, _N1} = erl_global_names(P, Fd),
    true = lists:member(atom_to_list(?GLOBAL_NAME), Names1),
    global:unregister_name(?GLOBAL_NAME),
    {Names2, _N2} = erl_global_names(P, Fd),
    false = lists:member(atom_to_list(?GLOBAL_NAME), Names2),
    0 = erl_close_connection(P, Fd),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

%%% Interface functions for erl_interface functions.

erl_connect(P, Node, Num, Cookie, Creation) ->
    send_command(P, erl_connect, [Num, Node, Cookie, Creation]),
    case get_term(P) of
        {term,{Fd,_}} when Fd >= 0 -> {ok,Fd};
        {term,{-1,Errno}} -> {error,Errno}
    end.

erl_close_connection(P, FD) ->
    send_command(P, erl_close_connection, [FD]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

erl_global_register(P, Fd, Name) ->
    send_command(P, erl_global_register, [Fd,Name]),
    get_send_result(P).

erl_global_whereis(P, Fd, Name) ->
    send_command(P, erl_global_whereis, [Fd,Name]),
    case get_term(P) of
        {term, What} ->
            What
    end.

erl_global_names(P, Fd) ->
    send_command(P, erl_global_names, [Fd]),
    case get_term(P) of
        {term, What} ->
            What
    end.

erl_global_unregister(P, Fd, Name) ->
    send_command(P, erl_global_unregister, [Fd,Name]),
    get_send_result(P).

get_send_result(P) ->
    case get_term(P) of
        {term,{1,_}} -> ok;
        {term,{0, 0}} -> ok;
        {term,{-1, Errno}} -> {error,Errno};
        {term,{_,_}}->
            ct:fail(bad_return_value)
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).
