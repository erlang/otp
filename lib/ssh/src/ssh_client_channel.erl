%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2024. All Rights Reserved.
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

-module(ssh_client_channel).
-moduledoc """
\-behaviour(ssh_client_channel). (Replaces ssh_channel)

> #### Note {: .info }
>
> This module replaces ssh_channel.
>
> The old module is still available for compatibility, but should not be used
> for new programs. The old module will not be maintained except for some error
> corrections

SSH services (clients and servers) are implemented as channels that are
multiplexed over an SSH connection and communicates over the
[SSH Connection Protocol](http://www.ietf.org/rfc/rfc4254.txt). This module
provides a callback API that takes care of generic channel aspects for clients,
such as flow control and close messages. It lets the callback functions take
care of the service (application) specific parts. This behavior also ensures
that the channel process honors the principal of an OTP-process so that it can
be part of a supervisor tree. This is a requirement of channel processes
implementing a subsystem that will be added to the `ssh` applications supervisor
tree.

> #### Note {: .info }
>
> When implementing a `ssh` subsystem for daemons, use
> [\-behaviour(ssh_server_channel)](`m:ssh_server_channel`) (Replaces
> ssh_daemon_channel) instead.

> #### Dont {: .error }
>
> Functions in this module are not supposed to be called outside a module
> implementing this behaviour\!

## Callback timeouts

The timeout values that can be returned by the callback functions have the same
semantics as in a `m:gen_server`. If the time-out occurs, `c:handle_msg/2` is called as
handle_msg(timeout, State).
""".
-moduledoc(#{since => "OTP 21.0",
             titles => [{callback,<<"Callback Functions">>}]}).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-doc """
Makes necessary initializations and returns the initial channel state if the
initializations succeed.

For more detailed information on time-outs, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-doc """
Handles messages sent by calling [call/\[2,3]](`call/2`)

For more detailed information on time-outs,, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-doc """
Handles messages sent by calling [`cast/2`](`cast/2`).

For more detailed information on time-outs, see Section
[Callback timeouts](`m:ssh_client_channel#module-callback-timeouts`).
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-doc """
This function is called by a channel process when it is about to terminate.
Before this function is called,
[ssh_connection:close/2 ](`ssh_connection:close/2`)is called, if it has not been
called earlier. This function does any necessary cleaning up. When it returns,
the channel process terminates with reason `Reason`. The return value is
ignored.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-doc """
Converts process state when code is changed.

This function is called by a client-side channel when it is to update its
internal state during a release upgrade or downgrade, that is, when the
instruction `{update,Module,Change,...}`, where `Change={advanced,Extra}`, is
given in the `appup` file. For more information, refer to Section 9.11.6 Release
Handling Instructions in the
[System Documentation](`e:system:release_handling.md#instr`).

> #### Note {: .info }
>
> Soft upgrade according to the OTP release concept is not straight forward for
> the server side, as subsystem channel processes are spawned by the `ssh`
> application and hence added to its supervisor tree. The subsystem channels can
> be upgraded when upgrading the user application, if the callback functions can
> handle two versions of the state, but this function cannot be used in the
> normal way.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-doc """
Handles other messages than SSH Connection Protocol, call, or cast messages sent
to the channel.

Possible Erlang 'EXIT' messages is to be handled by this function and all
channels are to handle the following message.

- **`{ssh_channel_up, ``t:ssh:channel_id/0``, ``t:ssh:connection_ref/0``}`** -
  This is the first message that the channel receives. It is sent just before
  the `init/1` function returns successfully. This is especially useful if the
  server wants to send a message to the client without first receiving a message
  from it. If the message is not useful for your particular scenario, ignore it
  by immediately returning `{ok, State}`.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::ssh:channel_id(), State::term()}. 

-doc """
Handles SSH Connection Protocol messages that may need service-specific
attention. For details, see `t:ssh_connection:event/0`.

The following message is taken care of by the `ssh_client_channel` behavior.

- **`{closed, ``t:ssh:channel_id/0``}`** - The channel behavior sends a close
  message to the other side, if such a message has not already been sent. Then
  it terminates the channel with reason `normal`.
""".
-doc(#{title => <<"Callback Functions">>,since => <<"OTP 21.0">>}).
-callback handle_ssh_msg(ssh_connection:event(),
 			 State::term()) -> {ok, State::term()} | 
 					   {stop, ChannelId::ssh:channel_id(), 
 					    State::term()}.
-behaviour(gen_server).

%%% API
-export([start/4, start/5, start_link/4, start_link/5, call/2, call/3,
	 cast/2, reply/2, enter_loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal application API
-export([cache_create/0, cache_lookup/2, cache_update/2,
	 cache_delete/1, cache_delete/2, cache_foldl/3,
	 cache_info/2, get_print_info/1, get_print_info/2
        ]).

-behaviour(ssh_dbg).
-export([ssh_dbg_trace_points/0, ssh_dbg_flags/1, ssh_dbg_on/1, ssh_dbg_off/1, ssh_dbg_format/2]).
-export_type([client/0]).

-record(state, {
	  cm,
	  channel_cb,
	  channel_state,
	  channel_id,
	  close_sent = false
	 }).

%%====================================================================
%% API
%%====================================================================
-doc(#{equiv => call/3}).
-doc(#{since => <<"OTP 21.0">>}).
-spec call(ChannelRef, Msg) -> Reply | {error, Reason} when
      ChannelRef :: pid(),
      Msg :: term(),
      Reply :: term(),
      Reason :: closed | timeout.
call(ChannelPid, Msg) ->
    call(ChannelPid, Msg, infinity).

-doc """
call(ChannelRef, Msg, Timeout) -> Reply | {error, Reason}

Makes a synchronous call to the channel process by sending a message and waiting
until a reply arrives, or a time-out occurs. The channel calls
[Module:handle_call/3](`c:handle_call/3`) to handle the message. If the channel
process does not exist, `{error, closed}` is returned.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec call(ChannelRef, Msg, Timeout) -> Reply | {error, Reason} when
      ChannelRef :: pid(),
      Msg :: term(),
      Timeout :: timeout(),
      Reply :: term(),
      Reason :: closed | timeout.
call(ChannelPid, Msg, TimeOute) ->
    try gen_server:call(ChannelPid, Msg, TimeOute) of
	Result ->
	    Result
    catch 
 	exit:{noproc, _} ->
 	    {error, closed};
	exit:{normal, _} ->
	    {error, closed};
	exit:{shutdown, _} ->
	    {error, closed};
	exit:{{shutdown, _}, _} ->
	    {error, closed};
 	exit:{timeout, _} ->
 	    {error, timeout}
    end.

-doc """
cast(ChannelRef, Msg) -> ok

Sends an asynchronous message to the channel process and returns ok immediately,
ignoring if the destination node or channel process does not exist. The channel
calls [Module:handle_cast/2](`c:handle_cast/2`) to handle the message.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec cast(ChannelRef, Msg) -> ok when
      ChannelRef :: pid(),
      Msg :: term().
cast(ChannelRef, Msg) ->
    gen_server:cast(ChannelRef, Msg).

-doc """
reply(Client, Reply) -> \_

This function can be used by a channel to send a reply to a client that called
`call/[2,3]` when the reply cannot be defined in the return value of
[Module:handle_call/3](`c:handle_call/3`).

`Client` must be the `From` argument provided to the callback function
[`handle_call/3`](`c:handle_call/3`). `Reply` is an arbitrary term, which is
given back to the client as the return value of [call/\[2,3].](`call/2`)
""".
-doc(#{since => <<"OTP 21.0">>}).
-opaque client() :: term().
-spec reply(Client, Reply) -> _ when
      Client :: client(),
      Reply :: term().
reply(From, Msg) ->
    gen_server:reply(From, Msg).

%%====================================================================
%% Internal application API
%%====================================================================

-doc(#{equiv => start_link/4}).
-doc(#{since => <<"OTP 21.0">>}).
-spec start(SshConnection, ChannelId, ChannelCb, CbInitArgs) ->
          {ok, ChannelRef} | {error, Reason :: term()}
              when
      SshConnection :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      ChannelCb :: atom(),
      CbInitArgs :: [term()],
      ChannelRef :: pid().
start(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    start(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

-doc false.
start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    Options = [{channel_cb, CallBack},
	       {channel_id, ChannelId},
	       {init_args, CbInitArgs},
	       {cm, ConnectionManager},
	       {exec, Exec}],
    gen_server:start(?MODULE, [Options], []).

-doc """
start_link(SshConnection, ChannelId, ChannelCb, CbInitArgs) -> {ok, ChannelRef}
| {error, Reason}

Starts a process that handles an SSH channel. It is called internally, by the
`ssh` daemon, or explicitly by the `ssh` client implementations. The behavior
sets the `trap_exit` flag to `true`.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec start_link(SshConnection, ChannelId, ChannelCb, CbInitArgs) ->
          {ok, ChannelRef} | {error, Reason :: term()}
              when
      SshConnection :: ssh:connection_ref(),
      ChannelId :: ssh:channel_id(),
      ChannelCb :: atom(),
      CbInitArgs :: [term()],
      ChannelRef :: pid().
start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

-doc false.
start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    Options = [{channel_cb, CallBack},
	       {channel_id, ChannelId},
	       {init_args, CbInitArgs},
	       {cm, ConnectionManager},
	       {exec, Exec}],
    gen_server:start_link(?MODULE, [Options], []).

-doc """
enter*loop(State) -> *

Makes an existing process an `ssh_client_channel` (replaces ssh_channel)
process. Does not return, instead the calling process enters the
`ssh_client_channel` (replaces ssh_channel) process receive loop and become an
`ssh_client_channel` process. The process must have been started using one of
the start functions in `proc_lib`, see the `m:proc_lib` manual page in STDLIB.
The user is responsible for any initialization of the process and must call
`init/1`.
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec enter_loop(State) -> _  when State :: term().
enter_loop(State) ->
    gen_server:enter_loop(?MODULE, [], State).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-doc """
init(Options) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}

The following options must be present:

- **`{channel_cb, atom()}`** - The module that implements the channel behaviour.

- **`{init_args(), list()}`** - The list of arguments to the `init` function of
  the callback module.

- **`{cm, ``t:ssh:connection_ref/0``}`** - Reference to the `ssh` connection as
  returned by `ssh:connect/3`.

- **`{channel_id, ``t:ssh:channel_id/0``}`** - Id of the `ssh` channel as
  returned by
  [ssh_connection:session_channel/2,4](`ssh_connection:session_channel/2`).

> #### Note {: .info }
>
> This function is normally not called by the user. The user only needs to call
> if the channel process needs to be started with help of `proc_lib` instead of
> calling [`start/4`](`start/4`) or [`start_link/4`](`start_link/4`).
""".
-doc(#{since => <<"OTP 21.0">>}).
-spec init(Options) ->
              {ok, State} | {ok, State, Timeout} | {stop, Reason}
              when
                  Options :: [[{Option :: term(), Value :: term()}]],
                  State :: term(),
                  Timeout :: timeout(),
                  Reason :: term().
init([Options]) ->    
    Cb = proplists:get_value(channel_cb, Options),
    ConnectionManager =  proplists:get_value(cm, Options),
    ChannelId = proplists:get_value(channel_id, Options),
    process_flag(trap_exit, true),
    try Cb:init(channel_cb_init_args(Options)) of
	{ok, ChannelState} ->
	    State = #state{cm = ConnectionManager, 
			   channel_cb = Cb,
			   channel_id = ChannelId,
			   channel_state = ChannelState},
	    self() ! {ssh_channel_up, ChannelId, ConnectionManager}, 
	    {ok, State};
	{ok, ChannelState, Timeout} ->
	    State = #state{cm = ConnectionManager, 
			   channel_cb = Cb,
			   channel_id = ChannelId,
			   channel_state = ChannelState},
	    self() ! {ssh_channel_up, ChannelId, ConnectionManager}, 
	    {ok, State, Timeout};
	{stop, Why} ->
	    {stop, Why}
    catch 
        _:undef ->
            {stop, {bad_channel_callback_module,Cb}};
	_:Reason ->
	    {stop, Reason}
    end.

channel_cb_init_args(Options) ->
    case proplists:get_value(exec, Options) of
        undefined ->
            proplists:get_value(init_args, Options);
        Exec ->
            proplists:get_value(init_args, Options) ++ [Exec]
    end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
-doc false.
handle_call(get_print_info, _From, State) ->
    Reply =
	{{State#state.cm,
	  State#state.channel_id},
	 io_lib:format('CB=~p',[State#state.channel_cb])
	},
    {reply, Reply, State};

handle_call({get_print_info,channel_cb}, _From, State) ->
    {reply, State#state.channel_cb, State};

handle_call(Request, From, #state{channel_cb = Module, 
				  channel_state = ChannelState} = State) ->
   try Module:handle_call(Request, From, ChannelState) of
       Result ->
	   handle_cb_result(Result, State)
   catch
       error:{undef, _} -> 
	   {noreply, State}
   end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
-doc false.
handle_cast(Msg, #state{channel_cb = Module, 
			channel_state = ChannelState} = State) ->
    
    try Module:handle_cast(Msg, ChannelState) of
	Result ->
	    handle_cb_result(Result, State)
    catch
       error:{undef, _} -> 
	    {noreply, State}
   end.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
-doc false.
handle_info({ssh_cm, ConnectionManager, {closed, _ChannelId}},  
	    #state{cm = ConnectionManager,
		   close_sent = true} = State) ->
    {stop, normal, State};
handle_info({ssh_cm, ConnectionManager, {closed, ChannelId}},  
	    #state{cm = ConnectionManager, 
		   close_sent = false} = State) ->
    %% To be on the safe side, i.e. the manager has already been terminated.
    (catch ssh_connection:close(ConnectionManager, ChannelId)),
    {stop, normal, State#state{close_sent = true}};

handle_info({ssh_cm, _, _} = Msg, #state{channel_cb = Module, 
                                         channel_state = ChannelState0} = State) ->
    try Module:handle_ssh_msg(Msg, ChannelState0) of
	{ok, ChannelState} ->
	    adjust_window(Msg),
	    {noreply, State#state{channel_state = ChannelState}};
	{ok, ChannelState, Timeout} ->
	    adjust_window(Msg),
	    {noreply, State#state{channel_state = ChannelState}, Timeout};
	{stop, ChannelId, ChannelState} ->
            do_the_close(Msg, ChannelId, State#state{channel_state = ChannelState})
    catch
        error:_ ->
            do_the_close(Msg, State#state.channel_id, State)
    end;

handle_info(Msg, #state{channel_cb = Module, 
			channel_state = ChannelState0} = State) -> 
    try Module:handle_msg(Msg, ChannelState0)
    of 
	{ok, ChannelState} ->
	    {noreply, State#state{channel_state = ChannelState}};
	{ok, ChannelState, Timeout} ->
	    {noreply, State#state{channel_state = ChannelState}, Timeout};
	{stop, ChannelId, ChannelState} ->
            do_the_close(Msg, ChannelId, State#state{channel_state = ChannelState})
    catch
        error:function_clause when tuple_size(Msg) == 3,
                                   element(1,Msg) == 'EXIT' ->
            do_the_close(Msg, State#state.channel_id, State)
    end.



do_the_close(Msg, ChannelId, State = #state{close_sent = false,
                                            cm = ConnectionManager}) ->
    catch ssh_connection:close(ConnectionManager, ChannelId),
    do_the_close(Msg, ChannelId, State#state{close_sent=true});
do_the_close({'EXIT', _Pid, shutdown=Reason},     _, State) -> {stop, Reason, State};
do_the_close({'EXIT', _Pid, {shutdown,_}=Reason}, _, State) -> {stop, Reason, State};
do_the_close(_Msg,                                _, State) -> {stop, normal, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-doc false.
terminate(Reason, #state{cm = ConnectionManager, 
 			 channel_id = ChannelId,
 			 close_sent = false} = State) ->
    catch ssh_connection:close(ConnectionManager, ChannelId),
    terminate(Reason, State#state{close_sent = true});
terminate(Reason, #state{channel_cb = Cb, channel_state = ChannelState}) ->
    catch Cb:terminate(Reason, ChannelState),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
-doc false.
code_change(OldVsn, #state{channel_cb = Module, 
			   channel_state = ChannelState0} = State, Extra) ->
    {ok, ChannelState} = Module:code_change(OldVsn, ChannelState0, Extra),
    {ok, State#state{channel_state = ChannelState}}.

%%====================================================================
%% Internal application API
%%====================================================================
-doc false.
cache_create() ->
    ets:new(cm_tab, [set,{keypos, #channel.local_id}]).

-doc false.
cache_lookup(Cache, Key) ->
    case ets:lookup(Cache, Key) of
	[Channel] ->
	    Channel;
	[] ->
	    undefined
    end.

-doc false.
cache_update(Cache, #channel{local_id = Id} = Entry) when Id =/= undefined ->
    ets:insert(Cache, Entry).

-doc false.
cache_delete(Cache, Key) ->
    ets:delete(Cache, Key).

-doc false.
cache_delete(Cache) ->
    ets:delete(Cache).

-doc false.
cache_foldl(Fun, Acc, Cache) ->
    ets:foldl(Fun, Acc, Cache).
    
-doc false.
cache_info(num_entries, Cache) ->
    proplists:get_value(size, ets:info(Cache)).

-doc false.
get_print_info(Pid) ->
    call(Pid, get_print_info, 1000).

-doc false.
get_print_info(Pid, Arg) ->
    call(Pid, {get_print_info,Arg}, 1000).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
handle_cb_result({reply, Reply, ChannelState}, State) ->
    {reply, Reply, State#state{channel_state = ChannelState}};
handle_cb_result({reply, Reply, ChannelState, Timeout}, State) ->
    {reply, Reply,State#state{channel_state = ChannelState}, Timeout};
handle_cb_result({noreply, ChannelState}, State) ->
    {noreply, State#state{channel_state = ChannelState}};
handle_cb_result({noreply, ChannelState, Timeout}, State) ->
    {noreply, State#state{channel_state = ChannelState}, Timeout};
handle_cb_result({stop, Reason, Reply, ChannelState}, State) ->
    {stop, Reason, Reply,  State#state{channel_state = ChannelState}};
handle_cb_result({stop, Reason, ChannelState}, State) ->
    {stop, Reason, State#state{channel_state = ChannelState}}.

adjust_window({ssh_cm, ConnectionManager,
	       {data, ChannelId, _, Data}}) ->
    ssh_connection:adjust_window(ConnectionManager, ChannelId, byte_size(Data));
adjust_window(_) ->
    ok.
    

%%%################################################################
%%%#
%%%# Tracing
%%%#

-doc false.
ssh_dbg_trace_points() -> [terminate, channels, channel_events].

-doc false.
ssh_dbg_flags(channels) -> [c];
ssh_dbg_flags(terminate) -> [c];
ssh_dbg_flags(channel_events) -> [c].

-doc false.
ssh_dbg_on(terminate) -> dbg:tp(?MODULE,  terminate, 2, x);
ssh_dbg_on(channels) -> dbg:tp(?MODULE,  init, 1, x),
                        ssh_dbg_on(terminate);
ssh_dbg_on(channel_events) -> dbg:tp(?MODULE,  handle_call, 3, x),
                              dbg:tp(?MODULE,  handle_cast, 2, x),
                              dbg:tp(?MODULE,  handle_info, 2, x).

-doc false.
ssh_dbg_off(terminate) -> dbg:ctpg(?MODULE, terminate, 2);
ssh_dbg_off(channels) -> dbg:ctpg(?MODULE, init, 1),
                         ssh_dbg_off(terminate);
ssh_dbg_off(channel_events) -> dbg:ctpg(?MODULE,  handle_call, 3),
                               dbg:ctpg(?MODULE,  handle_cast, 2),
                               dbg:ctpg(?MODULE,  handle_info, 2).

-doc false.
ssh_dbg_format(channels, {call, {?MODULE,init, [[KVs]]}}) ->
    ["Server Channel Starting:\n",
     io_lib:format("Connection: ~p, ChannelId: ~p, CallBack: ~p\nCallBack init args = ~p", 
                   [proplists:get_value(K,KVs) || K <- [cm, channel_id, channel_cb]]
                   ++ [channel_cb_init_args(KVs)])
    ];
ssh_dbg_format(channels, {return_from, {?MODULE,init,1}, {stop,Reason}}) ->
    ["Server Channel Start FAILED!\n",
     io_lib:format("Reason = ~p", [Reason])
    ];

ssh_dbg_format(channels, F) ->
    ssh_dbg_format(terminate, F);

ssh_dbg_format(terminate, {call, {?MODULE,terminate, [Reason, State]}}) ->
    ["Server Channel Terminating:\n",
     io_lib:format("Reason: ~p,~nState:~n~s", [Reason, wr_record(State)])
    ];
ssh_dbg_format(terminate, {return_from, {?MODULE,terminate,2}, _Ret}) ->
    skip;

ssh_dbg_format(channel_events, {call, {?MODULE,handle_call, [Call,From,State]}}) ->
    [hdr("is called", State),
     io_lib:format("From: ~p~nCall: ~p~n", [From, Call])
    ];
ssh_dbg_format(channel_events, {return_from, {?MODULE,handle_call,3}, Ret}) ->
    ["Server Channel call returned:\n",
     io_lib:format("~p~n", [ssh_dbg:reduce_state(Ret,#state{})])
    ];

ssh_dbg_format(channel_events, {call, {?MODULE,handle_cast, [Cast,State]}}) ->
    [hdr("got cast", State),
     io_lib:format("Cast: ~p~n", [Cast])
    ];
ssh_dbg_format(channel_events, {return_from, {?MODULE,handle_cast,2}, Ret}) ->
    ["Server Channel cast returned:\n",
     io_lib:format("~p~n", [ssh_dbg:reduce_state(Ret,#state{})])
    ];

ssh_dbg_format(channel_events, {call, {?MODULE,handle_info, [Info,State]}}) ->
    [hdr("got info", State),
     io_lib:format("Info: ~p~n", [Info])
    ];
ssh_dbg_format(channel_events, {return_from, {?MODULE,handle_info,2}, Ret}) ->
    ["Server Channel info returned:\n",
     io_lib:format("~p~n", [ssh_dbg:reduce_state(Ret,#state{})])
    ].

hdr(Title, S) ->
    io_lib:format("Server Channel (Id=~p, CB=~p) ~s:\n", [S#state.channel_id, S#state.channel_cb, Title]).

?wr_record(state).


