%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
%% Purpose: Handle the flex scanner
%%----------------------------------------------------------------------

-module(megaco_flex_scanner_handler).

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/app/megaco_internal.hrl"). 


%% External exports
-export([
	 start_link/0, start_link/1, 
	 stop/1, 
	 get_config/1
	]).

%% gen_server callbacks
-export([
	 init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2,
	 code_change/3
	]).

-record(state, {conf}).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start_link() -> 
    start_link([]).

start_link(Opts) ->
    case gen_server:start_link(?MODULE, Opts, []) of
	{ok, Pid} ->
	    Conf = get_config(Pid),
	    {ok, Pid, Conf};
	Else ->
	    Else
    end.
	
stop(Pid) ->
    gen_server:call(Pid, stop).

get_config(Pid) ->
    gen_server:call(Pid, get_config).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(_Opts) ->
    process_flag(trap_exit, true),
    case start_flex_scanners() of
	{ok, PortOrPorts} ->
	    {ok, #state{conf = {flex, PortOrPorts}}};
	{error, Reason} ->
	    %% {stop, {failed_starting_scanner, Reason, Opts}};
	    {stop, {failed_starting_scanner, Reason, []}};
	Else ->
	    {stop, {failed_starting_scanner, Else}}
    end.


%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(get_config, _From, #state{conf = Conf} = S) ->
    {reply, Conf, S};

handle_call(stop, _From, #state{conf = {flex, PortOrPorts}} = S) ->
    megaco_flex_scanner:stop(PortOrPorts),
    Reason = normal, 
    Reply  = ok, 
    {stop, Reason, Reply, S};

handle_call(Req, From, S) ->
    warning_msg("received unexpected request from ~p: "
		"~n~w", [From, Req]),
    {reply, {error, {unknown_request, Req}}, S}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    warning_msg("received unexpected message: "
		"~n~w", [Msg]),
    {noreply, S}.


%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Port, Error}, #state{conf = {flex, PortOrPorts}} = S) ->
    case megaco_flex_scanner:is_scanner_port(Port, PortOrPorts) of
	true ->
	    error_msg("Port [~p] exited:"
		      "~n~w", [Port, Error]),
	    {stop, {port_exit, Port, Error}, S};
	false ->
	    {noreply, S}
    end;

handle_info({'EXIT', Port, _Error}, S) when is_port(Port) ->
    %% This is propably the old flex scanner, 
    %% terminating after a code change...
    {noreply, S};

handle_info({'EXIT', Id, Error}, S) ->
    warning_msg("received unexpected 'EXIT' signal from ~p:"
		"~n~w", [Id, Error]),
    {noreply, S};

handle_info(Info, S) ->
    warning_msg("received unexpected info: "
		"~n~w", [Info]),
    {noreply, S}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _S) ->
    ok.


%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Called to change the internal state
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change({down, _Vsn}, #state{conf = Conf} = State, downgrade_to_pre_3_17) ->
    NewPorts = bump_flex_scanner(Conf),
    {ok, State#state{conf = {flex, NewPorts}}};

code_change(_Vsn, #state{conf = Conf} = State, upgrade_from_pre_3_17) ->
    NewPorts = bump_flex_scanner(Conf),
    {ok, State#state{conf = {flex, NewPorts}}};

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

bump_flex_scanner({flex, Ports}) ->
    megaco_flex_scanner:stop(Ports), 
    case start_flex_scanners() of
	{ok, NewPorts} ->
	    NewPorts;
	Error ->
	    exit(Error)
    end;
bump_flex_scanner(BadConfig) ->
    exit({invalid_config, BadConfig}).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

start_flex_scanners() ->
    megaco_flex_scanner:start().

				  
%% get_env(Key, Opts, Default) ->
%%     case lists:keysearch(Key, 1, Opts) of
%% 	{value, {Key, Value}} ->
%% 	    Value;
%% 	false ->
%% 	    Default
%%     end.

warning_msg(F, A) ->
    ?megaco_warning("Flex scanner handler: " ++ F, A).

error_msg(F, A) ->
    ?megaco_error("Flex scanner handler: " ++ F, A).



% d(F, A) ->
%     io:format("~w:" ++ F ++ "~n", [?MODULE|A]).
	      
