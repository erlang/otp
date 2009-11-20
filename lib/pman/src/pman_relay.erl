%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose : Interface function to  relay calls (esp. trace calls) 
%%           to processes on other  nodes. Some of the calls
%%           are conditionally relayed.
%%----------------------------------------------------------------------

-module(pman_relay).

%%-compile(export_all).
-export([start/1,
	 ok_to_trace/1,
	 trac/3]).


-include("assert.hrl").

%% --------------------------------------------------------------
%%                  DISTRIBUTION
%% --------------------------------------------------------------
%% (???) Process dictionary alert!!!
%%
%% Since we are not allowed to do erlang:trace/3 on remote
%% processe we create a help process at the remote node to
%% do the job for us 
%% ---------------------------------------------------------------

start(P) when is_pid(P), node(P)/=node() ->
    
    %% Remote supervision, relaying necessary
    
    put(relay, spawn_link(node(P), pman_relay_server, init, [self()]));


start(_) ->
    
    %% Local supervision, no relaying
    
    ignore.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ok_to_trace/1 - Tests wheter we can actually start tracing 
%%    a process.
%%
%% Arguments:
%%   Pid		Pid of the process to trace (on local or remote node)
%%
%% Returns
%%   true		If it is OK to trace the process
%%   false		If the process is already traced, or some other
%%			condition prevents it from being traced.

ok_to_trace(Pid) when node(Pid) == node()->

    %% Local trace, no relaying

    case catch erlang:trace(Pid, false, [send]) of
	1 ->
	    true;
	_Otherwise ->
	    false
    end;
ok_to_trace(Pid) ->

    %% Remote trace, relaying necessary

    PidRelay =  get(relay),
    PidRelay ! {ok_to_trace, self(), Pid},
    receive
	{ok_to_trace, PidRelay} ->
	    true;
	{not_ok_to_trace, PidRelay} ->
	    false;
	_Otherwise ->
	    ?ALWAYS_ASSERT("Unexpected message from relay process")
    after
	5000 ->
	    false
    end.


    
	     
    

%% ---------------------------------------------------------------
%% Possibly send a request to do tracing to a remote node.
%% ---------------------------------------------------------------

trac(Pid, How, Flag) when node(Pid) == node() ->

    %% Local trace, no relaying necessary


    case catch erlang:trace(Pid, How, Flag) of
	1 -> ok;
	_ -> pman_win:format("** Illegal trace request ** \n", [])
    end;
	
trac(Pid, How, Flag) ->



    %% Remote trace, relaying necessary

    get(relay) ! {self(), erlang, trace, [Pid, How, Flag]}.

