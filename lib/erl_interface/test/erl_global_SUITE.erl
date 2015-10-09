%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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

-include_lib("test_server/include/test_server.hrl").
-include("erl_global_SUITE_data/erl_global_test_cases.hrl").

-export([all/0,suite/0,init_per_suite/1,end_per_suite/1,
	 init_per_testcase/2,end_per_testcase/2,
	 erl_global_registration/1, erl_global_whereis/1, erl_global_names/1]).

-import(runner, [get_term/1,send_term/2]).

-define(GLOBAL_NAME, global_register_node_test).

all() ->
    [erl_global_registration, erl_global_whereis, erl_global_names].

suite() -> [{ct_hooks,[ts_install_cth]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(0.25)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

erl_global_registration(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    ?line ok = erl_global_register(P, Fd, ?GLOBAL_NAME),
    ?line ok = erl_global_unregister(P, Fd, ?GLOBAL_NAME),

    ?line 0 = erl_close_connection(P,Fd),
    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

erl_global_whereis(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    ?line Self = self(),
    ?line yes = global:register_name(?GLOBAL_NAME, Self),
    ?line Self = erl_global_whereis(P, Fd, ?GLOBAL_NAME),
    ?line global:unregister_name(?GLOBAL_NAME),
    ?line 0 = erl_close_connection(P, Fd),
    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

erl_global_names(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line {ok, Fd} = erl_connect(P, node(), 42, erlang:get_cookie(), 0),

    ?line Self = self(),
    ?line global:register_name(?GLOBAL_NAME, Self),
    ?line {Names1, _N1} = erl_global_names(P, Fd),
    ?line true = lists:member(atom_to_list(?GLOBAL_NAME), Names1),
    ?line global:unregister_name(?GLOBAL_NAME),
    ?line {Names2, _N2} = erl_global_names(P, Fd),
    ?line false = lists:member(atom_to_list(?GLOBAL_NAME), Names2),
    ?line 0 = erl_close_connection(P, Fd),
    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
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
	    ?t:fail(bad_return_value)
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).
