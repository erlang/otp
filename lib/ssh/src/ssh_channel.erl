%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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

-module(ssh_channel).

-include("ssh_connect.hrl").

-callback init(Args :: term()) ->
    {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                      State :: term()) ->
    {reply, Reply :: term(), NewState :: term()} |
    {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
    {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
    {noreply, NewState :: term()} |
    {noreply, NewState :: term(), timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: term()}.

-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} |
                               term()),
                    State :: term()) ->
    term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
                      Extra :: term()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-callback handle_msg(Msg ::term(), State :: term()) ->
    {ok, State::term()} | {stop, ChannelId::integer(), State::term()}. 

-callback handle_ssh_msg({ssh_cm, ConnectionRef::term(), SshMsg::term()}, 
 			 State::term()) -> {ok, State::term()} | 
 					   {stop, ChannelId::integer(), 
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
	 cache_delete/1, cache_delete/2,  cache_foldl/3,
	 cache_find/2,
	 get_print_info/1]).

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

call(ChannelPid, Msg) ->
    call(ChannelPid, Msg, infinity).

call(ChannelPid, Msg, TimeOute) ->
    try gen_server:call(ChannelPid, Msg, TimeOute) of
	Result ->
	    Result
    catch 
 	exit:{noproc, _} ->
 	    {error, closed};
 	exit:{timeout, _} ->
 	    {error, timeout}
    end.


cast(ChannelPid, Msg) ->
    gen_server:cast(ChannelPid, Msg).


reply(From, Msg) ->
    gen_server:reply(From, Msg).

%%====================================================================
%% Internal application API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    start(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    Options = [{channel_cb, CallBack},
	       {channel_id, ChannelId},
	       {init_args, CbInitArgs},
	       {cm, ConnectionManager},
	       {exec, Exec}],	  
    gen_server:start(?MODULE, [Options], []).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs) ->
    start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, undefined).

start_link(ConnectionManager, ChannelId, CallBack, CbInitArgs, Exec) ->
    Options = [{channel_cb, CallBack},
	       {channel_id, ChannelId},
	       {init_args, CbInitArgs},
	       {cm, ConnectionManager},
	       {exec, Exec}],	  
    gen_server:start_link(?MODULE, [Options], []).

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
init([Options]) ->    
    Cb = proplists:get_value(channel_cb, Options),
    ConnectionManager =  proplists:get_value(cm, Options),
    ChannelId = proplists:get_value(channel_id, Options),
    process_flag(trap_exit, true),
    InitArgs =
	case proplists:get_value(exec, Options) of
	    undefined ->
		proplists:get_value(init_args, Options);
	    Exec ->
		proplists:get_value(init_args, Options) ++ [Exec]
	end,
    try Cb:init(InitArgs) of
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
	_:Reason ->
	    {stop, Reason}
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
handle_call(get_print_info, _From, State) ->
    Reply =
	{{State#state.cm,
	  State#state.channel_id},
	 io_lib:format('CB=~p',[State#state.channel_cb])
	},
    {reply, Reply, State};

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

handle_info({ssh_cm, _, _} = Msg, #state{cm = ConnectionManager,
			channel_cb = Module, 
			channel_state = ChannelState0} = State) ->
    case Module:handle_ssh_msg(Msg, ChannelState0) of
	{ok, ChannelState} ->
	    adjust_window(Msg),
	    {noreply, State#state{channel_state = ChannelState}};
	{ok, ChannelState, Timeout} ->
	    adjust_window(Msg),
	    {noreply, State#state{channel_state = ChannelState}, Timeout};
	{stop, ChannelId, ChannelState} ->
	    ssh_connection:close(ConnectionManager, ChannelId),
	    {stop, normal, State#state{close_sent = true,
				       channel_state = ChannelState}}
    end;

handle_info(Msg, #state{cm = ConnectionManager, channel_cb = Module, 
			channel_state = ChannelState0} = State) -> 
    case Module:handle_msg(Msg, ChannelState0) of 
	{ok, ChannelState} ->
	    {noreply, State#state{channel_state = ChannelState}};
	{ok, ChannelState, Timeout} ->
	    {noreply, State#state{channel_state = ChannelState}, Timeout};
	{stop, Reason, ChannelState} when is_atom(Reason)->
	    {stop, Reason, State#state{close_sent = true,
				       channel_state = ChannelState}};
	{stop, ChannelId, ChannelState} ->
	    Reason =
		case Msg of
		    {'EXIT', _Pid, shutdown} ->
			shutdown;
		    _ ->
			normal
		end,
	    (catch ssh_connection:close(ConnectionManager, ChannelId)),
	    {stop, Reason, State#state{close_sent = true,
				       channel_state = ChannelState}}
    end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{cm = ConnectionManager, 
 			 channel_id = ChannelId,
 			 close_sent = false} = State) ->
    catch ssh_connection:close(ConnectionManager, ChannelId),
    terminate(Reason, State#state{close_sent = true});
terminate(_, #state{channel_cb = Cb, channel_state = ChannelState}) ->
    catch Cb:terminate(Cb, ChannelState),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(OldVsn, #state{channel_cb = Module, 
			   channel_state = ChannelState0} = State, Extra) ->
    {ok, ChannelState} = Module:code_change(OldVsn, ChannelState0, Extra),
    {ok, State#state{channel_state = ChannelState}}.

%%====================================================================
%% Internal application API
%%====================================================================
cache_create() ->
    ets:new(cm_tab, [set,{keypos, #channel.local_id}]).

cache_lookup(Cache, Key) ->
    case ets:lookup(Cache, Key) of
	[Channel] ->
	    Channel;
	[] ->
	    undefined
    end.

cache_update(Cache, #channel{local_id = Id} = Entry) when Id =/= undefined ->
    ets:insert(Cache, Entry).

cache_delete(Cache, Key) ->
    ets:delete(Cache, Key).

cache_delete(Cache) ->
    ets:delete(Cache).

cache_foldl(Fun, Acc, Cache) ->
    ets:foldl(Fun, Acc, Cache).
    
cache_find(ChannelPid, Cache) ->
   case ets:match_object(Cache, #channel{user = ChannelPid}) of
       [] ->
	   undefined;
       [Channel] ->
	   Channel
   end.

get_print_info(Pid) ->
    call(Pid, get_print_info, 1000).

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
    ssh_connection:adjust_window(ConnectionManager, ChannelId, size(Data));
adjust_window(_) ->
    ok.
    

