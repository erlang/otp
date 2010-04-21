%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2010. All Rights Reserved.
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

-module(ssh_channel).

-include("ssh_connect.hrl").

-behaviour(gen_server).

%%% API
-export([behaviour_info/1, start/4, start/5, start_link/4, start_link/5, call/2, call/3,
	 cast/2, reply/2, enter_loop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Internal application API
-export([cache_create/0, cache_lookup/2, cache_update/2, 
	 cache_delete/1, cache_delete/2,  cache_foldl/3,
	 cache_find/2]).

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

%%% Optionel callbacks handle_call/3, handle_cast/2, handle_msg/2,
%%% code_change/3
behaviour_info(callbacks) ->
    [
     {init, 1}, 
     {terminate, 2}, 
     {handle_ssh_msg, 2},
     {handle_msg, 2}
     ].


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
    {stop, normal, State};

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
	{stop, ChannelId, ChannelState} ->
	    ssh_connection:close(ConnectionManager, ChannelId),
	    {stop, normal, State#state{close_sent = true,
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
    ssh_connection:close(ConnectionManager, ChannelId),
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
    

