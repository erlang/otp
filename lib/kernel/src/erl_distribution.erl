%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
-module(erl_distribution).

-behaviour(supervisor).

-export([start_link/0,start_link/1,init/1,start/1,stop/0]).

%-define(DBG,io:format("~p:~p~n",[?MODULE,?LINE])).
-define(DBG,erlang:display([?MODULE,?LINE])).

start_link() ->
    case catch start_p() of
	{ok,Args} ->
	    start_link(Args);
	_ ->
	    ignore
    end.

start_link(Args) ->
    supervisor:start_link({local,net_sup},erl_distribution,Args).

init(NetArgs) ->
    Epmd = 
	case init:get_argument(no_epmd) of
	    {ok, [[]]} ->
		[];
	    _ ->
		EpmdMod = net_kernel:epmd_module(),
		[{EpmdMod,{EpmdMod,start_link,[]},
		  permanent,2000,worker,[EpmdMod]}]
	end,
    Auth = {auth,{auth,start_link,[]},permanent,2000,worker,[auth]},
    Kernel = {net_kernel,{net_kernel,start_link,[NetArgs]},
	      permanent,2000,worker,[net_kernel]},
    EarlySpecs = net_kernel:protocol_childspecs(),
    {ok,{{one_for_all,0,1}, EarlySpecs ++ Epmd ++ [Auth,Kernel]}}.

start_p() ->
    sname(),
    lname(),
    false.

sname() ->
    case init:get_argument(sname) of
	{ok,[[Name]]} ->
	    throw({ok,[list_to_atom(Name),shortnames|ticktime()]});
	_ ->
	    false
    end.

lname() ->
    case init:get_argument(name) of
	{ok,[[Name]]} ->
	    throw({ok,[list_to_atom(Name),longnames|ticktime()]});
	_ ->
	    false
    end.

ticktime() ->
    %% catch, in case the system was started with boot file start_old,
    %% i.e. running without the application_controller.
    %% Time is given in seconds. The net_kernel tick time is
    %% Time/4 milliseconds.
    case catch application:get_env(net_ticktime) of
	{ok, Value} when is_integer(Value), Value > 0 ->
	    [Value * 250]; %% i.e. 1000 / 4 = 250 ms.
	_ ->
	    []
    end.

start(Args) ->
    C = {net_sup_dynamic, {erl_distribution, start_link, [Args]}, permanent,
	 1000, supervisor, [erl_distribution]},
    supervisor:start_child(kernel_sup, C).

stop() ->
    case supervisor:terminate_child(kernel_sup, net_sup_dynamic) of
	ok ->
	    supervisor:delete_child(kernel_sup, net_sup_dynamic);
	Error ->
	    case whereis(net_sup) of
		Pid when is_pid(Pid) ->
		    %% Dist. started through -sname | -name flags
		    {error, not_allowed};
		_ ->
		    Error
	    end
    end.

