%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
-module(erl_connect_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erl_connect_SUITE_data/erl_connect_test_cases.hrl").

-export([all/0, suite/0,
         erl_send/1, erl_reg_send/1,
         erl_send_cookie_file/1]).

-import(runner, [get_term/1,send_term/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {seconds, 30}}].

all() -> 
    [erl_send, erl_reg_send, erl_send_cookie_file].


erl_send(Config) when is_list(Config) ->
    P = runner:start(?interpret),
    1 = erl_connect_init(P, 42, erlang:get_cookie(), 0),
    {ok,Fd} = erl_connect(P, node()),

    ok = erl_send(P, Fd, self(), AMsg={a,message}),
    receive AMsg -> ok end,

    0 = erl_close_connection(P,Fd),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.

erl_send_cookie_file(Config) when is_list(Config) ->
    case os:type() of
        vxworks ->
            {skip,"Skipped on VxWorks"};
        _ ->
            P = runner:start(?interpret),
            1 = erl_connect_init(P, 42, '', 0),
            {ok,Fd} = erl_connect(P, node()),

            ok = erl_send(P, Fd, self(), AMsg={a,message}),
            receive AMsg -> ok end,

            0 = erl_close_connection(P,Fd),
            runner:send_eot(P),
            runner:recv_eot(P),
            ok
    end.

erl_reg_send(Config) when is_list(Config) ->
    P = runner:start(?interpret),
    1 = erl_connect_init(P, 42, erlang:get_cookie(), 0),
    {ok,Fd} = erl_connect(P, node()),

    ARegName = a_strange_registred_name,
    register(ARegName, self()),
    ok = erl_reg_send(P, Fd, ARegName, AMsg={another,[strange],message}),
    receive AMsg -> ok end,

    0 = erl_close_connection(P,Fd),
    runner:send_eot(P),
    runner:recv_eot(P),
    ok.


%%% Interface functions for erl_interface functions.

erl_connect_init(P, Num, Cookie, Creation) ->
    send_command(P, erl_connect_init, [Num,Cookie,Creation]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

erl_connect(P, Node) ->
    send_command(P, erl_connect, [Node]),
    case get_term(P) of
        {term,{Fd,_}} when Fd >= 0 -> {ok,Fd};
        {term,{-1,Errno}} -> {error,Errno}
    end.

erl_close_connection(P, FD) ->
    send_command(P, erl_close_connection, [FD]),
    case get_term(P) of
        {term,Int} when is_integer(Int) -> Int
    end.

erl_send(P, Fd, To, Msg) ->
    send_command(P, erl_send, [Fd,To,Msg]),
    get_send_result(P).

erl_reg_send(P, Fd, To, Msg) ->
    send_command(P, erl_reg_send, [Fd,To,Msg]),
    get_send_result(P).

get_send_result(P) ->
    case get_term(P) of
        {term,{1,_}} -> ok;
        {term,{-1,Errno}} -> {error,Errno};
        {term,{Res,Errno}}->
            io:format("Return value: ~p\nerl_errno: ~p", [Res,Errno]),
            ct:fail(bad_return_value)
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).
