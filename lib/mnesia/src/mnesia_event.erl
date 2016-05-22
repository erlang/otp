%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(mnesia_event).

-behaviour(gen_event).
%-behaviour(mnesia_event).

%% gen_event callback interface
-export([init/1,
	 handle_event/2,
	 handle_call/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {nodes = [], 
		dumped_core = false,  %% only dump fatal core once
		args}).

%%%----------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------

%%-----------------------------------------------------------------
%% init(Args) ->
%%     {ok, State} | Error
%%-----------------------------------------------------------------

init(Args) ->
    {ok, #state{args = Args}}.

%%-----------------------------------------------------------------
%% handle_event(Event, State) -> 
%%    {ok, NewState} | remove_handler |
%%    {swap_handler, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_event(Event, State) ->
    handle_any_event(Event, State).

%%-----------------------------------------------------------------
%% handle_info(Msg, State) ->
%%    {ok, NewState} | remove_handler |
%%    {swap_handler, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_info(Msg, State) ->
    {ok, _} = handle_any_event(Msg, State),
    {ok, State}.

%%-----------------------------------------------------------------
%% handle_call(Event, State) -> 
%%    {ok, Reply, NewState} | {remove_handler, Reply} | 
%%    {swap_handler, Reply, Args1, State1, Mod2, Args2}
%%-----------------------------------------------------------------

handle_call(Msg, State) ->
    Reply = ok,
    {ok, NewState} = handle_any_event(Msg, State),
    {ok, Reply, NewState}.

%%-----------------------------------------------------------------
%% terminate(Reason, State) ->
%%     AnyVal
%%-----------------------------------------------------------------

terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Upgrade process when its code is to be changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

handle_any_event({mnesia_system_event, Event}, State) ->
    handle_system_event(Event, State);
handle_any_event({mnesia_table_event, Event}, State) ->
    handle_table_event(Event, State);
handle_any_event(Msg, State) ->
    report_error("~p got unexpected event: ~p~n", [?MODULE, Msg]),
    {ok, State}.

handle_table_event({Oper, Record, TransId}, State) ->
    report_info("~p performed by ~p on record:~n\t~p~n",
		[Oper, TransId, Record]),
    {ok, State}.  

handle_system_event({mnesia_checkpoint_activated, _Checkpoint}, State) ->
    {ok, State};

handle_system_event({mnesia_checkpoint_deactivated, _Checkpoint}, State) ->
    {ok, State};

handle_system_event({mnesia_up, Node}, State) ->
    Nodes = [Node | State#state.nodes],
    {ok, State#state{nodes = Nodes}}; 

handle_system_event({mnesia_down, Node}, State) ->
    case mnesia:system_info(fallback_activated) andalso Node =/= node() of
	true ->
	    case mnesia_monitor:get_env(fallback_error_function) of
		{mnesia, lkill} ->
		    Msg = "A fallback is installed and Mnesia "
			"must be restarted. Forcing shutdown "
			"after mnesia_down from ~p...~n",
		    report_fatal(Msg, [Node], nocore, State#state.dumped_core),
		    catch exit(whereis(mnesia_monitor), fatal),
		    {ok, State};
		{UserMod, UserFunc} ->
		    Msg = "Warning: A fallback is installed and Mnesia got mnesia_down "
			"from ~p. ~n",
		    report_info(Msg, [Node]),
		    case catch apply(UserMod, UserFunc, [Node]) of
			{'EXIT', {undef, _Reason}} ->
			    %% Backward compatibility
			    apply(UserMod, UserFunc, []);
			{'EXIT', Reason} ->
			    exit(Reason);
			_ ->
			    ok
		    end,
		    Nodes = lists:delete(Node, State#state.nodes),
		    {ok, State#state{nodes = Nodes}}
	    end;
	false ->
	    Nodes = lists:delete(Node, State#state.nodes),
	    {ok, State#state{nodes = Nodes}}
    end;

handle_system_event({mnesia_overload, Details}, State) ->
    report_warning("Mnesia is overloaded: ~w~n", [Details]),
    {ok, State}; 

handle_system_event({mnesia_info, Format, Args}, State) ->
    report_info(Format, Args),
    {ok, State}; 

handle_system_event({mnesia_warning, Format, Args}, State) ->
    report_warning(Format, Args),
    {ok, State}; 

handle_system_event({mnesia_error, Format, Args}, State) ->
    report_error(Format, Args),
    {ok, State}; 

handle_system_event({mnesia_fatal, Format, Args, BinaryCore}, State) ->
    report_fatal(Format, Args, BinaryCore, State#state.dumped_core),
    {ok, State#state{dumped_core = true}};

handle_system_event({inconsistent_database, Reason, Node}, State) ->
    report_error("mnesia_event got {inconsistent_database, ~w, ~w}~n",
		 [Reason, Node]),
    {ok, State}; 

handle_system_event({mnesia_user, Event}, State) ->
    report_info("User event: ~p~n", [Event]),
    {ok, State}; 

handle_system_event(Msg, State) ->
    report_error("mnesia_event got unexpected system event: ~p~n", [Msg]),
    {ok, State}.

report_info(Format0, Args0) ->
    Format = "Mnesia(~p): " ++ Format0,
    Args = [node() | Args0],
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    io:format(Format, Args);
	Pid ->
	    io:format(Pid, Format, Args)
    end.

report_warning(Format0, Args0) ->
    Format = "Mnesia(~p): ** WARNING ** " ++ Format0,
    Args = [node() | Args0],
    case erlang:function_exported(error_logger, warning_msg, 2) of
	true -> 
	    error_logger:warning_msg(Format, Args);
	false ->
	    error_logger:format(Format, Args)
    end,
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    ok;
	Pid ->
	    io:format(Pid, Format, Args)
    end.

report_error(Format0, Args0) ->
    Format = "Mnesia(~p): ** ERROR ** " ++ Format0,
    Args = [node() | Args0],
    error_logger:format(Format, Args),
    case global:whereis_name(mnesia_global_logger) of
	undefined ->
	    ok;
	Pid ->
	    io:format(Pid, Format, Args)
    end.

report_fatal(Format, Args, BinaryCore, CoreDumped) ->
    UseDir = mnesia_monitor:use_dir(),
    CoreDir = mnesia_monitor:get_env(core_dir),
    if
	is_list(CoreDir),CoreDumped == false, is_binary(BinaryCore) ->	    
	    core_file(CoreDir,BinaryCore,Format,Args);
	(UseDir == true),CoreDumped == false, is_binary(BinaryCore) ->
	    core_file(CoreDir,BinaryCore,Format,Args);
	true ->
	    report_error("(ignoring core) ** FATAL ** " ++ Format, Args)
    end.

core_file(CoreDir,BinaryCore,Format,Args) ->
    Integers = tuple_to_list(erlang:timestamp()),
    Fun = fun(I) when I < 10 -> ["_0",I];
	     (I) -> ["_",I]
	  end,
    List = lists:append([Fun(I) || I <- Integers]),
    CoreFile = if is_list(CoreDir) ->
		       filename:absname(lists:concat(["MnesiaCore.", node()] ++ List), 
					CoreDir);
		  true ->
		       filename:absname(lists:concat(["MnesiaCore.", node()] ++ List))
	       end,
    case file:write_file(CoreFile, BinaryCore) of
	ok ->
	    report_error("(core dumped to file: ~p)~n ** FATAL ** " ++ Format,
			 [CoreFile] ++ Args);
	{error, Reason} ->
	    report_error("(could not write core file: ~p)~n ** FATAL ** " ++ Format,
			 [Reason] ++ Args)
    end.


	
