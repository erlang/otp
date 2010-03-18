%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File: ct_config_SUITE
%%%
%%% Description:
%%% Config server used in the CT's tests (config_2_SUITE)
%%%-------------------------------------------------------------------
-module(config_server).
-export([start/0, stop/0, loop/1, init/1, get_config/0]).

-define(REGISTERED_NAME, ct_test_config_server).
-define(vsn, 0.19).

start()->
    case whereis(?REGISTERED_NAME) of
        undefined->
	    spawn(?MODULE, init, [?REGISTERED_NAME]),
	    wait();
	_Pid->
	    ok
    end,
    ?REGISTERED_NAME.

init(Name)->
    register(Name, self()),
    loop(0).

get_config()->
    call(self(), get_config).

stop()->
    call(self(), stop).

call(Client, Request)->
    case whereis(?REGISTERED_NAME) of
	undefined->
	    {error, not_started, Request};
	Pid->
	    Pid ! {Client, Request},
	    receive
	        Reply->
		    {ok, Reply}
	    after 4000->
		{error, timeout, Request}
	    end
    end.

loop(Iteration)->
    receive
	{Pid, stop}->
	    Pid ! ok;
	{Pid, get_config}->
	    {D,T} = erlang:localtime(),
	    Config =
		[{localtime, [{date, D}, {time, T}]},
		 {node, erlang:node()},
		 {config_server_iteration, Iteration},
		 {now, erlang:now()},
		 {config_server_pid, self()},
		 {config_server_vsn, ?vsn}],
	    Config2 = if Iteration rem 2 == 0->
		Config ++ [{disappearable_variable, hereAmI}];
		true-> Config
	    end,
	    Pid ! Config2,
	    ?MODULE:loop(Iteration+1)
    end.

wait()->
    case whereis(?REGISTERED_NAME) of
	undefined->
	    wait();
	_Pid->
	    ok
    end.
