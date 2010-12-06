%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2010. All Rights Reserved.
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
-module(erl_connect_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include("erl_connect_SUITE_data/erl_connect_test_cases.hrl").

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,init_per_testcase/2,end_per_testcase/2,
	 erl_send/1,erl_reg_send/1, erl_send_cookie_file/1]).

-import(runner, [get_term/1,send_term/2]).

suite() -> [{suite_callbacks,[ts_install_scb]}].

all() -> 
[erl_send, erl_reg_send, erl_send_cookie_file].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?t:minutes(0.25)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

erl_send(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 1 = erl_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = erl_connect(P, node()),

    ?line ok = erl_send(P, Fd, self(), AMsg={a,message}),
    ?line receive AMsg -> ok end,

    ?line 0 = erl_close_connection(P,Fd),
    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
    ok.

erl_send_cookie_file(Config) when is_list(Config) ->
    case os:type() of
	vxworks ->
	    {skip,"Skipped on VxWorks"};
	_ ->
	    ?line P = runner:start(?interpret),
	    ?line 1 = erl_connect_init(P, 42, '', 0),
	    ?line {ok,Fd} = erl_connect(P, node()),
	    
	    ?line ok = erl_send(P, Fd, self(), AMsg={a,message}),
	    ?line receive AMsg -> ok end,
	    
	    ?line 0 = erl_close_connection(P,Fd),
	    ?line runner:send_eot(P),
	    ?line runner:recv_eot(P),
	    ok
    end.

erl_reg_send(Config) when is_list(Config) ->
    ?line P = runner:start(?interpret),
    ?line 1 = erl_connect_init(P, 42, erlang:get_cookie(), 0),
    ?line {ok,Fd} = erl_connect(P, node()),

    ARegName = a_strange_registred_name,
    ?line register(ARegName, self()),
    ?line ok = erl_reg_send(P, Fd, ARegName, AMsg={another,[strange],message}),
    ?line receive AMsg -> ok end,

    ?line 0 = erl_close_connection(P,Fd),
    ?line runner:send_eot(P),
    ?line runner:recv_eot(P),
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
	    ?t:fail(bad_return_value)
    end.

send_command(P, Name, Args) ->
    runner:send_term(P, {Name,list_to_tuple(Args)}).

    
    


