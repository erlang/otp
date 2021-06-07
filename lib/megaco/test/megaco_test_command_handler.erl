%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2020-2020. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose:
%%----------------------------------------------------------------------
-module(megaco_test_command_handler).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include("megaco_test_lib.hrl").


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 start/4,
	 await_completion/2,

	 print/1, print/2
	]).


%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Exported functions
%%----------------------------------------------------------------------

start(Node, Commands, InitialState, ShortName)
  when is_atom(Node) andalso
       is_list(Commands) andalso
       is_map(InitialState) andalso
       is_list(ShortName) ->
    Fun = fun() ->
		  put(sname, ShortName), 
		  process_flag(trap_exit, true),
		  Result = command_handler(Commands, InitialState),
		  print("command handler terminated with: "
			"~n      ~p", [Result]),
		  exit(Result)
	  end,
    erlang:spawn_link(Node, Fun).
		  
command_handler([], State) ->
    print("command_handler -> entry when done with"
	  "~n      State: ~p", [State]),
    {ok, State};
command_handler([#{id   := Id,
		   desc := Desc,
		   cmd  := Cmd}|Commands], State)
  when is_list(Desc) andalso is_function(Cmd, 1) ->
    print("command_handler -> [~w] ~s", [Id, Desc]),
    try Cmd(State) of
	{ok, NewState} ->
	    print("command_handler -> [~w] cmd ok", [Id]),
	    command_handler(Commands, NewState);
	{skip, _} = SKIP ->
	    print("command_handler -> [~w] cmd skip (returned)", [Id]),
	    SKIP;
	{error, Reason} ->
	    print("command_handler -> [~w] cmd error: "
		  "~n   Reason: ~p", [Id, Reason]),
	    {error, {cmd_error, Reason}}

    catch
	throw:{skip, _} = SKIP:_ ->
	    print("command_handler -> [~w] cmd skip (throw)", [Id]),
	    SKIP;
	exit:{skip, _} = SKIP:_ ->
	    print("command_handler -> [~w] cmd skip (exit)", [Id]),
	    SKIP;
	C:E:S ->
	    print("command_handler -> [~w] cmd failure:"
		  "~n   C: ~p"
		  "~n   E: ~p"
		  "~n   S: ~p", [Id, C, E, S]),
	    {error, {cmd_failure, {C, E, S}}}
    end.


%% --- Await completion of one or more command handler(s)

await_completion(Pids, Timeout) ->
    await_completion(Pids, [], [], Timeout).

await_completion([], [], _Good, _Timeout) ->
    print("await_completion -> entry when done (success)"),
    ok;
await_completion([], Bad, Good, _Timeout) ->
    print("await_completion -> entry when done with bad result: "
	  "~n      Bad:  ~p"
	  "~n      Good: ~p", [Bad, Good]),
    {error, Bad, Good};
await_completion(Pids, Bad, Good, Timeout) ->
    print("await_completion -> entry when waiting for"
	  "~n      Pids:    ~p"
	  "~n      Bad:     ~p"
	  "~n      Good:    ~p"
	  "~n      Timeout: ~p", [Pids, Bad, Good, Timeout]), 
    Begin = ms(), 
    receive 
	{'EXIT', Pid, {ok, FinalState}} ->
	    case lists:delete(Pid, Pids) of
		Pids ->
		    print("await_completion -> "
			  "received ok EXIT signal from unknown ~p", [Pid]),
		    await_completion(Pids, Bad, Good, 
				     Timeout - (ms() - Begin));
		Pids2 ->
		    print("await_completion -> success from  ~p", [Pid]),
		    await_completion(Pids2, 
				     Bad, 
				     [{Pid, FinalState}|Good],
				     Timeout - (ms() - Begin))
	    end;

	{'EXIT', Pid, {error, Reason}} ->
	    case lists:delete(Pid, Pids) of
		Pids ->
		    print("await_completion -> "
			  "received error EXIT signal from unknown ~p", [Pid]),
		    await_completion(Pids, Bad, Good,
				     Timeout - (ms() - Begin));
		Pids2 ->
		    print("await_completion -> failure from ~p", [Pid]), 
		    await_completion(Pids2, 
				     [{Pid, Reason}|Bad], 
				     Good, 
				     Timeout - (ms() - Begin))
	    end;

	{'EXIT', Pid, {skip, Reason}} ->
	    print("await_completion -> skip (exit) from ~p:"
		  "      ~p", [Pid, Reason]), 
	    ?SKIP(Reason)

    after Timeout ->
	    print("await_completion -> timeout"), 
	    exit({timeout, Pids})
    end.



%% ------- Misc functions --------

print(F) ->
    print(F, []).

print(F, A) ->
    print(get(sname), F, A).

print(N, F, A) when is_list(N) ->
    io:format("*** [~s] ~p ~s ***" 
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self(), N | A]);
print(_N, F, A) ->
    io:format("*** [~s] ~p *** "
	      "~n   " ++ F ++ "~n", 
	      [?FTS(), self() | A]).


ms() ->
    erlang:monotonic_time(milli_seconds).
    

