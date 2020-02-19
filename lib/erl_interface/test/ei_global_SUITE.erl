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
-module(ei_global_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ei_global_SUITE_data/ei_global_test_cases.hrl").

-export([all/0,suite/0,
         init_per_testcase/2,
         ei_global_registration/1,
         ei_global_whereis/1,
         ei_global_names/1
        ]).

-import(runner, [get_term/1,send_term/2]).

-define(GLOBAL_NAME, global_register_node_test).

all() ->
    [ei_global_registration, ei_global_whereis, ei_global_names].

get_group(Config) ->
    proplists:get_value(name, proplists:get_value(tc_group_properties,Config)).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

init_per_testcase(Case, Config) ->
    runner:init_per_testcase(?MODULE, Case, Config).

ei_global_registration(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, ussi),
    {ok,Fd} = ei_connect(P, node()),

    ok = ei_global_register(P, Fd, ?GLOBAL_NAME),
    ok = ei_global_unregister(P, Fd, ?GLOBAL_NAME),

    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

ei_global_whereis(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, ussi),
    {ok,Fd} = ei_connect(P, node()),

    Self = self(),
    yes = global:register_name(?GLOBAL_NAME, Self),
    Self = ei_global_whereis(P, Fd, ?GLOBAL_NAME),
    global:unregister_name(?GLOBAL_NAME),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

ei_global_names(Config) when is_list(Config) ->
    P = runner:start(Config, ?interpret),
    0 = ei_connect_init(P, 42, erlang:get_cookie(), 0, ussi),
    {ok,Fd} = ei_connect(P, node()),

    Self = self(),
    global:register_name(?GLOBAL_NAME, Self),
    {Names1, _N1} = ei_global_names(P, Fd),
    true = lists:member(atom_to_list(?GLOBAL_NAME), Names1),
    global:unregister_name(?GLOBAL_NAME),
    {Names2, _N2} = ei_global_names(P, Fd),
    false = lists:member(atom_to_list(?GLOBAL_NAME), Names2),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

%% %%% Interface functions for erl_interface functions.

%% erl_connect(P, Node, Num, Cookie, Creation) ->
%%     send_command(P, erl_connect, [Num, Node, Cookie, Creation]),
%%     case get_term(P) of
%%         {term,{Fd,_}} when Fd >= 0 -> {ok,Fd};
%%         {term,{-1,Errno}} -> {error,Errno}
%%     end.

ei_global_register(P, Fd, Name) ->
    send_command(P, ei_global_register, [Fd,Name]),
    get_send_result(P).

ei_global_whereis(P, Fd, Name) ->
    send_command(P, ei_global_whereis, [Fd,Name]),
    case get_term(P) of
        {term, What} ->
            What
    end.

ei_global_names(P, Fd) ->
    send_command(P, ei_global_names, [Fd]),
    case get_term(P) of
        {term, What} ->
            What
    end.

ei_global_unregister(P, Fd, Name) ->
    send_command(P, ei_global_unregister, [Fd,Name]),
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


ei_connect_init(P, Num, Cookie, Creation, SockImpl) ->
    send_command(P, ei_connect_init, [Num,Cookie,Creation,SockImpl]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

ei_connect(P, Node) ->
    send_command(P, ei_connect, [Node]),
    case get_term(P) of
        {term,{Fd,_}} when Fd >= 0 -> {ok,Fd};
        {term,{-1,Errno}} -> {error,Errno}
    end.
