%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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

%%%-------------------------------------------------------------------
%%% File    : loose_node.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Creation of nodes which are not supervised by
%%%               the test_server. Currently needed by init_SUITE
%%%               and heart_SUITE (until the test_server can
%%%               handle node restart).
%%%
%%% Created : 22 Sep 2004 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(loose_node).
-author('rickard.s.green@ericsson.com').

%%
%% Exports
%%
-export([start/3, start/2, stop/1]).

%%
%% Internal exports
%%
-export([loose_node_started/1]).

%% 
%% Exported functions for internal use.
%%

%%
%% Defines
%%
-define(L2A, list_to_atom).
-define(A2L, atom_to_list).
-define(I2L, integer_to_list).

%% 
%% Exported functions.
%%

stop(Node) when is_atom(Node) ->
    erlang:monitor_node(Node, true),
    rpc:cast(Node, erlang, halt, []),
    receive
	{nodedown, Node} ->
	   io:format("Stopped loose node ~p~n", [Node]),
	   ok
    after 10000 ->
	io:format("Failed to stop loose node: ~p~n", [Node]),
	{error, node_not_stopped}
    end.

start(Name, Args) ->
    start(Name, Args, -1).

start(Name, Args, TimeOut) when is_atom(Name) ->
    start(atom_to_list(Name), Args, TimeOut);
start(Name, Args, TimeOut)
  when is_list(Name), is_list(Args), is_integer(TimeOut) ->
    Parent = self(),
    Ref = make_ref(),
    Starter
	= fun () ->
		  Erl = case init:get_argument(progname) of
			    {ok,[[Prog]]} ->
				Prog;
			    _ ->
				"erl"
			end,
		  RegName = until_success(fun () ->
						  {A, B, C} = now(),
						  Reg =
						      ?L2A(?A2L(?MODULE)
							   ++ "-" ++ ?I2L(A)
							   ++ "-" ++ ?I2L(B)
							   ++ "-" ++ ?I2L(C)),
						  true = register(Reg, self()),
						  Reg
					  end),
		  NameCmd = case net_kernel:longnames() of
				true ->  " -name " ++ Name;
				false -> " -sname " ++ Name
			    end,
		  Cookie = " -setcookie " ++ atom_to_list(auth:get_cookie()),
		  Pa = " -pa " ++ filename:dirname(code:which(?MODULE)),
		  ThisNode = node(),
		  NodeStarted
		      = " -run "
		      ++ atom_to_list(?MODULE)
		      ++ " loose_node_started "
		      ++ atom_to_list(RegName)
		      ++ " "
		      ++ atom_to_list(ThisNode)
		      ++ " "
		      ++ integer_to_list(TimeOut),
		  CrashDump =
		      " -env ERL_CRASH_DUMP"
		      ++ " erl_crash.dump.loose_node."
		      ++ Name,
		  Cmd =
		      Erl
		      ++ " -detached"
		      ++ NameCmd
		      ++ Cookie
		      ++ Pa
		      ++ NodeStarted
		      ++ CrashDump
		      ++ " "
		      ++ Args,
		  io:format("Trying to start loose node...~n"
			    "  --> ~p~n", [Cmd]),
		  Res = case open_port({spawn, Cmd}, []) of
			    P when is_port(P) ->
				receive
				    {loose_node_started,
				     Node,
				     {RegName, ThisNode}} ->
					io:format("Loose node ~p started.~n",
						  [Node]),
					{ok, Node}
				after 10000 ->	
					io:format("Start of loose node ~p "
						  "timed out.", [Name]),
					{error, timeout}
				end;
			    _ ->
				io:format("Start of loose node ~p failed.",
					  [Name]),
				{error, open_port_failed}
			end,
		  Parent ! {Ref, Res}
	  end,
    spawn_opt(Starter, [link, {priority, max}]),
    receive
	{Ref, Result} ->
	    Result
    end.


%% 
%% Exported functions for internal use.
%%

loose_node_started([Name, Node, TimeOutSecs]) when is_list(Name),
						   is_list(Node),
						   is_list(TimeOutSecs) ->
    spawn_opt(fun () ->
		      process_flag(trap_exit, true),
		      Proc = {list_to_atom(Name), list_to_atom(Node)},
		      Timeout = case catch list_to_integer(TimeOutSecs) of
				    I when is_integer(I), I >= 0 -> I*1000;
				    _ -> infinity
				end,
		      wait_until(fun () -> is_alive() end),
		      Proc ! {loose_node_started, node(), Proc},
		      receive
		      after Timeout ->
			      timeout
		      end,
		      erlang:halt("Loose node timeout")
	      end,
	      [{priority, max}]),
    ok.

%% 
%% Internal functions.
%%

until_success(Fun) ->
    case catch Fun() of
	{'EXIT', _} -> until_success(Fun);
	Res -> Res
    end.

wait_until(Fun) -> 
    case Fun() of
	true -> true;
	_ ->
	    receive
	    after 100 ->
		    wait_until(Fun)
	    end
    end.

