%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the persistent-table component of the Diameter application
%%----------------------------------------------------------------------
%% 
-module(diameter_persistent_table_test).

-export([
	 init_per_testcase/2, fin_per_testcase/2, 

	 all/1,
	 suite_init/1, suite_fin/1,

	 simple_start_and_stop/1, 
	 table_create_and_delete/1

	]).

-export([t/0, t/1]).

-include("diameter_test_lib.hrl").

-record(command, {id, desc, cmd, verify}).


t()     -> diameter_test_server:t(?MODULE).
t(Case) -> diameter_test_server:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    diameter_test_server:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    diameter_test_server:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 simple_start_and_stop,
	 table_create_and_delete
	],
    {req, [], {conf, suite_init, Cases, suite_fin}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite_init(suite) -> [];
suite_init(doc) -> [];
suite_init(Config) when is_list(Config) ->
    Config.


suite_fin(suite) -> [];
suite_fin(doc) -> [];
suite_fin(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Test case(s)
%% 

simple_start_and_stop(suite) ->
    [];
simple_start_and_stop(doc) ->
    [];
simple_start_and_stop(Config) when is_list(Config) ->
    diameter:enable_trace(100, io),
    case diameter_persistent_table:start_link() of
	{ok, Pid} ->
	    unlink(Pid);
	{error, Reason} ->
	    exit({failed_starting, Reason})
    end,

    ok = diameter_persistent_table:stop(),
    ok.


table_create_and_delete(suite) ->
    [];
table_create_and_delete(doc) ->
    [];
table_create_and_delete(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    
    %% Command range values
    Initial        = 100,
    ClientCreation = 200,
    Nice           = 300,
    Evil           = 400,
    End            = 500,

    Verbosity = min, 
    %% Verbosity = max, 

    Data01 = lists:sort([{a, 10},  {b, 20},  {c, 30}]), 
    Data02 = lists:sort([{x, 100}, {y, 200}, {z, 300}]),

    Commands = 
	[
	 %% Initial commands
	 initial_command( Initial + 0,
			  "enable trace", 
			  fun() -> diameter:enable_trace(Verbosity, io) end, 
			  ok),
	 initial_command( Initial + 1,
                          "start persistent-table process",
                          fun() -> 
				  case diameter_persistent_table:start_link() of
				      {ok, Pid} when is_pid(Pid) ->
					  ok;
				      Error ->
					  Error
				  end
			  end,
			  ok),

	 client_create_command( ClientCreation + 1,
				"1",
				client01), 
				  
	 client_create_command( ClientCreation + 2,
				"2",
				client02),

	 nice_command( Nice + 1,
		       "client 1 create table 1",
		       fun() -> 
			       create_table(client01, tab01, []),
			       diameter_persistent_table:which_tables()
		       end,
		       fun([tab01] = Tabs) ->
			       {ok, Tabs};
			  (Unexpected) ->
			       {error, {bad_tables, Unexpected}}
		       end),

	 nice_command( Nice + 2,
		       "client 1 create table 2",
		       fun() -> 
			       create_table(client01, tab02, []),
			       diameter_persistent_table:which_tables()
		       end,
		       fun([tab01, tab02] = Tabs) ->
			       {ok, Tabs};
			  ([tab02, tab01] = Tabs) ->
			       {ok, Tabs};
			  (Unexpected) ->
			       {error, {bad_tables, Unexpected}}
		       end),

	 nice_command( Nice + 3,
		       "client 2 create table 1",
		       fun() -> 
			       create_table(client02, tab03, []),
			       diameter_persistent_table:which_tables(whereis(client02))
		       end,
		       fun([tab03] = Tabs) ->
			       {ok, Tabs}; 
			  (Unexpected) ->
			       {error, {bad_tables, Unexpected}}
		       end), 

	 nice_command( Nice + 4,
		       "client 1 delete table 1",
		       fun() -> 
			       delete_table(client01, tab01),
			       diameter_persistent_table:which_tables(whereis(client01))
		       end,
		       fun([tab02] = Tabs) ->
			       {ok, Tabs}; 
			  (Unexpected) ->
			       {error, {bad_tables, Unexpected}}
		       end), 

	 nice_command( Nice + 5,
		       "client 1 fill in some data in tab02",
		       fun() ->
			       populate_table(client01, tab02, Data01),
			       lists:sort(ets:tab2list(tab02))
		       end,
		       fun(Data) when Data =:= Data01 ->
			       {ok, Data}; 
			  (Unexpected) ->
			       {error, {bad_data, Unexpected}}
		       end), 

	 nice_command( Nice + 6,
		       "client 2 fill in some data in tab03",
		       fun() ->
			       populate_table(client02, tab03, Data02),
			       lists:sort(ets:tab2list(tab03))
		       end,
		       fun(Data) when Data =:= Data02 ->
			       {ok, Data}; 
			  (Unexpected) ->
			       {error, {bad_data, Unexpected}}
		       end), 

	 nice_command( Nice + 7,
		       "simulate client 1 crash",
		       fun() ->
			       simulate_crash(client01)
		       end,
		       fun(ok) ->
			       {ok, crashed}; 
			  (Unexpected) ->
			       {error, {bad_simulation_result, Unexpected}}
		       end), 

	 client_create_command( Nice + 8,
				"1 restarted",
				client01),

	 nice_command( Nice + 9,
		       "client 1 create tab02 - verify data",
		       fun() -> 
			       create_table(client01, tab02, []),
			       lists:sort(ets:tab2list(tab02))
		       end,
		       fun(Data) when Data =:= Data01 ->
			       {ok, Data}; 
			  (Unexpected) ->
			       {error, {bad_data, Unexpected}}
		       end),

	 evil_command( Evil + 1,
		       "try (and fail) to delete the non-existing table tab04",
		       fun() ->
			       delete_table(client02, tab04)
		       end,
		       fun({error, {unknown_table, tab04}}) ->
			       {ok, tab04};
			  (X) ->
			       {error, {bad_result, X}}
		       end),

	 evil_command( Evil + 2,
		       "try (and fail) to delete a not owned table tab02",
		       fun() ->
			       delete_table(client02, tab02)
		       end,
		       fun({error, {not_owner, tab02}}) ->
			       {ok, tab02};
			  (X) ->
			       {error, {bad_result, X}}
		       end),

	 evil_command( Evil + 3,
		       "try (and fail) to create an already existing *and* owned table - tab03",
		       fun() ->
			       create_table(client02, tab03, [])
		       end,
		       fun({error, {already_owner, tab03}}) ->
			       {ok, tab03};
			  (X) ->
			       {error, {bad_result, X}}
		       end), 
				  
	 evil_command( Evil + 4,
		       "try (and fail) to create an already existing not owned table - tab02",
		       fun() ->
			       create_table(client02, tab02, [])
		       end,
		       fun({error, {not_owner, _Owner, tab02}}) ->
			       {ok, tab02};
			  (X) ->
			       {error, {bad_result, X}}
		       end),

	 end_command( End + 1,
		      "stop client01",
		      fun() -> stop_client(client01) end),

	 end_command( End + 2,
		      "stop client02",
		      fun() -> stop_client(client02) end),

	 end_command( End + 2,
		      "stop persistent-table",
		      fun() -> diameter_persistent_table:stop() end), 

	 evil_command( Evil + 5,
		       "try (and fail) to stop a not running persistent-table process",
		       fun() ->
			       diameter_persistent_table:stop()
		       end,
		       fun({'EXIT', {noproc, _}}) ->
			       {ok, not_running};
			  (X) ->
			       {error, {bad_result, X}}
		       end)
				  
	],
    
    exec(Commands).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% 
%% Command engine
%% 

exec([]) ->
    ok;
exec([#command{id     = No,
               desc   = Desc,
               cmd    = Cmd,
               verify = Verify}|Commands]) ->
    io:format("Executing command ~2w: ~s: ", [No, Desc]),
    case (catch Verify((catch Cmd()))) of
        {ok, OK} ->
            io:format("ok => ~p~n", [OK]),
            exec(Commands);
        {error, Reason} ->
            io:format("error => ~p~n", [Reason]),
            {error, {bad_result, No, Reason}};
        Error ->
            io:format("exit => ~p~n", [Error]),
            {error, {unexpected_result, No, Error}}
    end.

initial_command(No, Desc0, Cmd, VerifyVal) when is_function(Cmd) ->
    Desc = lists:flatten(io_lib:format("Initial - ~s", [Desc0])),
    command(No, Desc, Cmd, VerifyVal).

client_create_command(No, Desc0, Name) ->
    Desc = lists:flatten(io_lib:format("Client create - ~s", [Desc0])),
    Self = self(), 
    Cmd  = fun() -> start_client(Self, Name) end,
    command(No, Desc, Cmd, ok).

nice_command(No, Desc0, Cmd, Verify) 
  when is_function(Cmd) andalso is_function(Verify) ->
    Desc = lists:flatten(io_lib:format("Nice - ~s", [Desc0])),
    command(No, Desc, Cmd, Verify).

evil_command(No, Desc0, Cmd, Verify) 
  when is_function(Cmd) andalso is_function(Verify) ->
    Desc = lists:flatten(io_lib:format("Evil - ~s", [Desc0])),
    command(No, Desc, Cmd, Verify).

end_command(No, Desc0, Cmd) when is_function(Cmd) ->
    Desc = lists:flatten(io_lib:format("End - ~s", [Desc0])),
    command(No, Desc, Cmd, ok).

command(No, Desc, Cmd, Verify) 
  when (is_integer(No) andalso 
	is_list(Desc) andalso
	is_function(Cmd) andalso 
	is_function(Verify)) ->
    #command{id     = No,
             desc   = Desc,
             cmd    = Cmd,
             verify = Verify};
command(No, Desc, Cmd, VerifyVal) 
  when (is_integer(No) andalso 
	is_list(Desc) andalso
	is_function(Cmd)) ->
    Verify = 
	fun(Val) ->
		case Val of
		    VerifyVal ->
			{ok, Val};
		    _ ->
			{error, Val}
		end
	end,
    #command{id     = No,
             desc   = Desc,
             cmd    = Cmd,
             verify = Verify}.


start_client(Parent, Name) ->
    ClientPid = spawn_link(fun() -> client_init(Parent, Name) end),
    receive
	{ClientPid, started} ->
	    ClientPid,
	    ok;
	{'EXIT', ClientPid, Reason} ->
	    {error, {failed_starting_client, Reason}}
    end.

stop_client(Client) ->
    Pid = whereis(Client),
    Pid ! stop,
    receive
	{'EXIT', Pid, normal} ->
	    ok
    end.

create_table(Client, Tab, Opts) ->
    Self = self(),
    Pid = whereis(Client),
    Pid ! {create_table, Tab, Opts, Self},
    receive
	{Pid, created} ->
	    ok;
	{Pid, {create_failed, Error}} ->
	    Error
    end.

delete_table(Client, Tab) ->
    Self = self(),
    Pid = whereis(Client),
    Pid ! {delete_table, Tab, Self},
    receive
	{Pid, deleted} ->
	    ok;
	{Pid, {delete_failed, Error}} ->
	    Error
    end.

populate_table(Client, Tab, Data) ->
    Self = self(),
    Pid = whereis(Client),
    Pid ! {populate_table, Tab, Data, Self},
    receive
	{Pid, populated} ->
	    ok
    end.

simulate_crash(Client) ->
    Pid = whereis(Client),
    Pid ! simulate_crash, 
    receive
	{'EXIT', Pid, simulated_crash} ->
	    ok
    end.

client_init(Parent, Name) ->
    erlang:register(Name, self()),
    process_flag(trap_exit, true),
    Parent ! {self(), started}, 
    client_loop().

client_loop() ->
    receive
	stop ->
	    exit(normal);

	{create_table, T, Opts, From} when is_atom(T) andalso is_list(Opts) ->
	    case diameter_persistent_table:create(T, Opts) of
		ok ->
		    From ! {self(), created};
		Error ->
		    From ! {self(), {create_failed, Error}}
	    end,
	    client_loop();

	{delete_table, T, From} ->
	    case diameter_persistent_table:delete(T) of
		ok ->
		    From ! {self(), deleted};
		Error ->
		    From ! {self(), {delete_failed, Error}}
	    end,
	    client_loop();
	
	{populate_table, Tab, Data, From} ->
	    ets:insert(Tab, Data),
	    From ! {self(), populated},
	    client_loop();	
	
	simulate_crash ->
	    exit(simulated_crash)
    end.

