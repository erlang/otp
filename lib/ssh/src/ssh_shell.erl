%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2016. All Rights Reserved.
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

-module(ssh_shell).

-include("ssh_connect.hrl").

%%% As this is an user interactive client it behaves like a daemon
%%% channel inspite of it being a client. 
-behaviour(ssh_daemon_channel).

%% ssh_channel callbacks
-export([init/1, handle_msg/2, handle_ssh_msg/2, terminate/2]).

%% Spawn export
-export([input_loop/2]).

-record(state, 
	{
	 io,      %% Io process
	 channel, %% Id of the ssh channel
	 cm       %% Ssh connection manager
	 }
       ).

%%====================================================================
%% ssh_channel callbacks
%%====================================================================
-spec init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().

-spec handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::integer(), State::term()}. 
-spec handle_ssh_msg({ssh_cm, ConnectionRef::term(), SshMsg::term()},
			 State::term()) -> {ok, State::term()} |
					   {stop, ChannelId::integer(),
					    State::term()}.

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} 
%%                        
%% Description: Initiates the CLI
%%--------------------------------------------------------------------
init([ConnectionManager, ChannelId] = Args) ->
    
    %% Make sure that we are proclib compatible as
    %% this client should be runnable from the
    %% erlang shell.
    case get('$initial_call') of
	undefined ->
	    Me = get_my_name(),
	    Ancestors = get_ancestors(),
	    put('$ancestors', [Me | Ancestors]),
	    put('$initial_call', {?MODULE, init, Args});
	_ ->
	    ok
    end,

    case ssh_connection:shell(ConnectionManager, ChannelId) of
	ok ->
	    {group_leader, GIO} = 
		process_info(self(), group_leader),
	    IoPid = spawn_link(?MODULE, input_loop,
			       [GIO, self()]),
	    {ok, #state{io = IoPid, 
			channel = ChannelId, 
			cm = ConnectionManager}};
	Error ->
	    {stop, Error}
    end.

%%--------------------------------------------------------------------
%% Function: handle_ssh_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles channel messages received on the ssh-connection.
%%--------------------------------------------------------------------
handle_ssh_msg({ssh_cm, _, {data, _ChannelId, 0, Data}}, State) ->
    %% TODO: When unicode support is ready
    %% should we call this function or perhaps a new
    %% function.
    io:put_chars(Data),
    {ok, State};

handle_ssh_msg({ssh_cm, _, 
		{data, _ChannelId, ?SSH_EXTENDED_DATA_STDERR, Data}},
	       State) ->
    %% TODO: When unicode support is ready
    %% should we call this function or perhaps a new
    %% function.
    io:put_chars(Data),
    {ok, State};

handle_ssh_msg({ssh_cm, _, {eof, _ChannelId}}, State) ->
    {ok, State};

handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    %% Ignore signals according to RFC 4254 section 6.9.
    {ok, State};

handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, Error, _}}, State) ->
    io:put_chars("Connection closed by peer"),
    %% TODO: When unicode support is ready
    %% should we call this function or perhaps a new
    %% function. The error is encoded as UTF-8!
    io:put_chars(Error),
    {stop, ChannelId,  State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, 0}}, State) ->
    io:put_chars("logout"),
    io:put_chars("Connection closed"),
    {stop, ChannelId, State};

handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, Status}}, State) ->
    io:put_chars("Connection closed by peer"),
    io:put_chars("Status: " ++ integer_to_list(Status)),
    {stop, ChannelId, State}.

%%--------------------------------------------------------------------
%% Function: handle_msg(Args) -> {ok, State} | {stop, ChannelId, State}
%%                        
%% Description: Handles other channel messages
%%--------------------------------------------------------------------
handle_msg({ssh_channel_up, ChannelId, ConnectionManager},
	   #state{channel = ChannelId,
		  cm = ConnectionManager} = State) ->
    {ok,  State};

handle_msg({input, IoPid, eof}, #state{io = IoPid, channel = ChannelId, 
				       cm = ConnectionManager} = State) ->
    ssh_connection:send_eof(ConnectionManager, ChannelId),
    {ok, State};

handle_msg({input, IoPid, Line}, #state{io = IoPid,
					channel = ChannelId,
					cm = ConnectionManager} = State) ->
    ssh_connection:send(ConnectionManager, ChannelId, Line),
    {ok, State}.
    
%%--------------------------------------------------------------------
%% Function: terminate(Reasons, State) -> _
%%                        
%% Description: Cleanup when shell channel is terminated
%%--------------------------------------------------------------------
terminate(_Reason, #state{io = IoPid}) ->
    exit(IoPid, kill).
    
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

input_loop(Fd, Pid) ->
    case io:get_line(Fd, '>') of
	eof ->
	    Pid ! {input, self(), eof},
	    ok; 
	Line ->
	    Pid ! {input, self(), Line},
	    input_loop (Fd, Pid)
    end.
    
get_my_name() ->
    case process_info(self(),registered_name) of
	{registered_name,Name} -> Name;
	_                      -> self()
    end.

get_ancestors() ->
    case get('$ancestors') of
	A when is_list(A) -> A;
	_              -> []
    end.
