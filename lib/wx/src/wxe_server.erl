%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2018. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wxe_server.erl
%%% Author  : Dan Gudmundsson <dgud@erix.ericsson.se>
%%% Description : Application server monitors the application and handles
%%%               callbacks, some cleaning if processes dies.
%%%               The interface functions is found in wxe_util.erl
%%% Created : 17 Jan 2007 by Dan Gudmundsson <dgud@erix.ericsson.se>
%%%-------------------------------------------------------------------

%% @hidden
-module(wxe_server).
-behaviour(gen_server).

%% API
-export([start/1, stop/0, register_me/1, set_debug/2, invoke_callback/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port,cb_port,users,cleaners=[],cb,cb_cnt}).
-record(user,  {events=[]}).
%%-record(event, {object, callback, cb_handler}).

-define(APPLICATION, wxe).
-define(log(S,A), log(?MODULE,?LINE,S,A)).

-include("wxe.hrl").
-include("../include/wx.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(SilentStart) -> #wx_env{}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(SilentStart) ->
    case get(?WXE_IDENTIFIER) of
	undefined ->
	    case gen_server:start(?MODULE, [SilentStart], []) of
		{ok, Pid}  ->
		    {ok, Port} = gen_server:call(Pid, get_port, infinity),
		    wx:set_env(Env = #wx_env{port=Port,sv=Pid}),
		    Env;
		{error, {Reason, _Stack}} ->
		    erlang:error(Reason)
	    end;
	Env = #wx_env{sv=Pid} ->
	    case erlang:is_process_alive(Pid) of
		true ->
		    Env;
		false ->  %% Ok we got an old wx env, someone forgot
		    erase(?WXE_IDENTIFIER),  %% to call wx:destroy()
		    start(SilentStart)
	    end
    end.

stop() ->
    #wx_env{sv=Pid} = get(?WXE_IDENTIFIER),
    catch gen_server:call(Pid, stop, infinity),
    ok.

register_me(Pid) ->
    ok = gen_server:call(Pid, register_me, infinity).

set_debug(Pid, Level) ->
    gen_server:cast(Pid, {debug, Level}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([SilentStart]) ->
    {Port,CBPort} = wxe_master:init_port(SilentStart),
    put(?WXE_IDENTIFIER, #wx_env{port=Port,sv=self()}),
    {ok,#state{port=Port, cb_port=CBPort,
	       users=gb_trees:empty(), cb=gb_trees:empty(), cb_cnt=1}}.

%% Register process
handle_call(register_me, {From,_}, State=#state{users=Users}) ->
    erlang:monitor(process, From),
    case gb_trees:is_defined(From, Users) of
	true ->
	    {reply, ok, State};
	false ->
	    New = gb_trees:insert(From, #user{}, Users),
	    {reply, ok, State#state{users=New}}
    end;
%% Port request
handle_call(get_port, _, State=#state{port=Port}) ->
    {reply, {ok,Port}, State};

%% Connect callback
handle_call({connect_cb,Obj,Msg},{From,_},State) ->
    handle_connect(Obj,Msg, From, State);

handle_call({disconnect_cb,Obj,Msg},{From,_},State) ->
    handle_disconnect(Obj,Msg, From, State);

handle_call(stop,{_From,_},State = #state{users=Users0, cleaners=Cs0}) ->
    Env = get(?WXE_IDENTIFIER),
    Users = gb_trees:to_list(Users0),
    Cs = lists:map(fun({_Pid,User}) ->
			   spawn_link(fun() -> cleanup(Env,[User]) end)
		   end, Users),
    {noreply, State#state{users=gb_trees:empty(), cleaners=Cs ++ Cs0}};

handle_call({register_cb, Fun}, _, State0) ->
    {FunId, State} = attach_fun(Fun,State0),
    {reply, FunId, State};

%% Error
handle_call(_Request, _From, State) ->
    ?log("Unknown request ~p sent to ~p from ~p ~n",[_Request, ?MODULE, _From]),
    Reply = ok,
    {reply, Reply, State}.

%%%%%%%%%%%% Cast's

handle_cast({cleaned, From}, State=#state{users=Users,cleaners=Cs0}) ->
    Cs = lists:delete(From,Cs0),
    case Cs =:= [] andalso gb_trees:is_empty(Users) of
	true  -> {stop, normal, State#state{cleaners=Cs}};
	false -> {noreply,State#state{cleaners=Cs}}
    end;

handle_cast({debug, Level}, State) ->
    Env = get(?WXE_IDENTIFIER),
    put(?WXE_IDENTIFIER, Env#wx_env{debug=Level}),
    {noreply, State};

handle_cast(_Msg, State) ->
    ?log("Unknown message ~p sent to ~p~n",[_Msg, ?MODULE]),
    {noreply, State}.

%%%% Info

%% Callback request from driver
handle_info(Cb = {_, _, '_wx_invoke_cb_'}, State) ->
    invoke_cb(Cb, State),
    {noreply, State};

handle_info({wx_delete_cb, FunId}, State)
  when is_integer(FunId) ->
    {noreply, delete_fun(FunId, State)};

handle_info({wx_delete_cb, Id, EvtListener, Obj}, State = #state{users=Users}) ->
    From = erase(EvtListener),
    case gb_trees:lookup(From, Users) of
	none ->
	    {noreply, delete_fun(Id, State)};
	{value, User0} ->
	    User = cleanup_evt_listener(User0, EvtListener, Obj),
	    {noreply, delete_fun(Id, State#state{users=gb_trees:update(From, User, Users)})}
    end;

handle_info({'DOWN',_,process,Pid,_}, State=#state{users=Users0,cleaners=Cs}) ->
    try
	User = gb_trees:get(Pid,Users0),
	Users = gb_trees:delete(Pid,Users0),
	Env = wx:get_env(),
	case User of
	    #user{events=[]} -> %% No need to spawn
		case Cs =:= [] andalso gb_trees:is_empty(Users) of
		    true  -> {stop, normal, State#state{users=Users}};
		    false -> {noreply, State#state{users=Users}}
		end;
	    _ ->
		Cleaner = spawn_link(fun() -> cleanup(Env,[User]) end),
		{noreply, State#state{users=Users,cleaners=[Cleaner|Cs]}}
	end
    catch  _E:_R ->
	    %% ?log("Error: ~p ~p", [_E,_R]),
	    {noreply, State}
    end;

handle_info(_Info, State) ->
    ?log("Unknown message ~p sent to ~p~n",[_Info, ?MODULE]),
    {noreply, State}.

terminate(_Reason, _State) ->
    %% erlang:display({?MODULE, killed, process_info(self(),trap_exit),_Reason}),
    %% timer:sleep(250), %% Give driver a chance to clean up
    shutdown.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

log(Mod,Line,Str,Args) ->
    error_logger:format("~p:~p: " ++ Str, [Mod,Line|Args]).

handle_connect(Object, #evh{handler=undefined, cb=Callback} = EvData0, 
	       From, State0) ->
    %% Callback let this process listen to the events
    {FunId, State} = attach_fun(Callback,State0),
    EvData1 = EvData0#evh{cb=FunId},
    case wxEvtHandler:connect_impl(Object,EvData1) of
	{ok, Handler} ->
	    EvData = EvData1#evh{handler=Handler,userdata=undefined},
	    handle_connect(Object, EvData, From, State);
	Error ->
	    {reply, Error, State0}
    end;
handle_connect(Object, EvData=#evh{handler=Handler},
	       From, State0 = #state{users=Users}) ->
    %% Correct process is already listening just register it
    put(Handler, From),
    case gb_trees:lookup(From, Users) of
	{value, User0 = #user{events=Listeners0}} ->
	    User  = User0#user{events=[{Object,EvData}|Listeners0]},
	    State = State0#state{users=gb_trees:update(From, User, Users)},
	    {reply, ok, State};
	none -> %% We are closing up the shop
	    {reply, {error, terminating}, State0}
    end.

invoke_cb({{Ev=#wx{}, Ref=#wx_ref{}}, FunId,_}, _S) ->
    %% Event callbacks
    case get(FunId) of
	{{nospawn, Fun}, _} when is_function(Fun) ->
	    invoke_callback_fun(fun() -> Fun(Ev, Ref), <<>> end);
	{Fun,_} when is_function(Fun) ->
	    invoke_callback(fun() -> Fun(Ev, Ref), <<>> end);
	{Pid,_} when is_pid(Pid) -> %% wx_object sync event
	    invoke_callback(Pid, Ev, Ref);
	Err ->
	    ?log("Internal Error ~p~n",[Err])
    end;
invoke_cb({FunId, Args, _}, _S) when is_list(Args), is_integer(FunId) ->
    %% Overloaded functions
    case get(FunId) of
	{Fun,_} when is_function(Fun) ->
	    invoke_callback(fun() -> Fun(Args) end);
	Err ->
	    ?log("Internal Error ~p ~p ~p~n",[Err, FunId, Args])
    end.

invoke_callback(Fun) ->
    Env = get(?WXE_IDENTIFIER),
    spawn(fun() ->
		  wx:set_env(Env),
		  invoke_callback_fun(Fun)
	  end),
    ok.

invoke_callback(Pid, Ev, Ref) ->
    Env = get(?WXE_IDENTIFIER),
    CB = fun() ->
		 wx:set_env(Env),
		 wxe_util:cast(?WXE_CB_START, <<>>),
		 try
		     case get_wx_object_state(Pid, 5) of
			 ignore ->
			     %% Ignore early events
			     wxEvent:skip(Ref);
			 {Mod, State} ->
			     case Mod:handle_sync_event(Ev, Ref, State) of
				 ok -> ok;
				 noreply -> ok;
				 Return -> exit({bad_return, Return})
			     end
		     end
		 catch _:Reason:Stacktrace ->
			 wxEvent:skip(Ref),
			 ?log("Callback fun crashed with {'EXIT, ~p, ~p}~n",
			      [Reason, Stacktrace])
		 end,
		 wxe_util:cast(?WXE_CB_RETURN, <<>>)
	 end,
    spawn(CB),
    ok.

invoke_callback_fun(Fun) ->
    wxe_util:cast(?WXE_CB_START, <<>>),
    Res = try
	      Return = Fun(),
	      true = is_binary(Return),
	      Return
	  catch _:Reason:Stacktrace ->
		  ?log("Callback fun crashed with {'EXIT, ~p, ~p}~n",
		       [Reason, Stacktrace]),
		  <<>>
	  end,
    wxe_util:cast(?WXE_CB_RETURN, Res).


get_wx_object_state(Pid, N) when N > 0 ->
    case process_info(Pid, dictionary) of
	{dictionary, Dict} ->
	    case lists:keysearch('_wx_object_',1,Dict) of
		{value, {'_wx_object_', {_Mod, '_wx_init_'}}} ->
		    timer:sleep(50),
		    get_wx_object_state(Pid, N-1);
		{value, {'_wx_object_', Value}} ->
		    Value;
		_ ->
		    ignore
	    end;
	_ ->
	    ignore
    end;
get_wx_object_state(_, _) ->
    ignore.


attach_fun(Fun, S = #state{cb=CB,cb_cnt=Next}) ->
    case gb_trees:lookup(Fun,CB) of
	{value, ID} ->
	    {Fun, N} = get(ID),
	    put(ID, {Fun,N+1}),
	    {ID,S};
	none ->
	    put(Next,{Fun, 1}),
	    {Next,S#state{cb=gb_trees:insert(Fun,Next,CB),cb_cnt=Next+1}}
    end.

delete_fun(0, State) -> State;
delete_fun(FunId, State = #state{cb=CB}) ->
    case get(FunId) of
	undefined ->
	    State;
	{Fun,N} when N < 2 ->
	    erase(FunId),
	    State#state{cb=gb_trees:delete(Fun, CB)};
	{Fun,N} ->
	    put(FunId, {Fun, N-1}),
	    State
    end.

cleanup_evt_listener(U=#user{events=Evs0}, EvtListener, Object) ->
    Filter = fun({Obj,#evh{handler=Evl}}) -> 
		     not (Object =:= Obj andalso Evl =:= EvtListener) 
	     end,
    U#user{events=lists:filter(Filter, Evs0)}.

handle_disconnect(Object, Evh = #evh{cb=Fun}, From, 
		  State0 = #state{users=Users0, cb=Callbacks}) ->
    #user{events=Evs0} = gb_trees:get(From, Users0),
    FunId = gb_trees:lookup(Fun, Callbacks),
    Handlers = find_handler(Evs0, Object, Evh#evh{cb=FunId}),
    {reply, {try_in_order, Handlers}, State0}.

find_handler([{Object,Evh}|Evs], Object, Match) ->
    case match_handler(Match, Evh) of
	false -> find_handler(Evs, Object, Match);
	Res  -> [Res|find_handler(Evs,Object,Match)]
    end;
find_handler([_|Evs], Object, Match) ->
    find_handler(Evs, Object, Match);
find_handler([], _, _) -> [].

match_handler(M=#evh{et=MET, cb=MCB}, 
	      #evh{et=ET, cb=CB, handler=Handler}) ->
    %% Let wxWidgets handle the id matching
    Match = match_et(MET, ET) 
	andalso match_cb(MCB, CB),
    Match andalso M#evh{handler=Handler}.

match_et(null, _) -> true;
match_et(Met, Et) -> Met =:= Et.

match_cb(none, _) -> true;
match_cb({value,MId}, Id) ->  MId =:= Id.

%% Cleanup
%% The server handles callbacks from driver so every other wx call must
%% be called from another process, therefore the cleaning must be spawned.
%%
cleanup(Env, Data) ->
    put(?WXE_IDENTIFIER, Env),
    Disconnect = fun({Object, Ev}) ->
			 try wxEvtHandler:disconnect_impl(Object,Ev)
			 catch _:_ -> ok
			 end
		 end,

    lists:foreach(fun(#user{events=Evs}) -> 
			  [Disconnect(Ev) || Ev <- Evs]
		  end, Data),
    gen_server:cast(Env#wx_env.sv, {cleaned, self()}),
    normal.
