%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2013. All Rights Reserved.
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
-record(user,  {events=[], evt_handler}).
-record(event, {object, callback, cb_handler}).

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
			   spawn_link(fun() -> cleanup(Env,[User], false) end)
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
	    #user{events=[], evt_handler=undefined} -> %% No need to spawn
		case Cs =:= [] andalso gb_trees:is_empty(Users) of
		    true  -> {stop, normal, State#state{cleaners=Cs}};
		    false -> {noreply, State#state{users=Users,cleaners=Cs}}
		end;
	    _ ->
		Cleaner = spawn_link(fun() -> cleanup(Env,[User],true) end),
		{noreply, State#state{users=Users,cleaners=[Cleaner|Cs]}}
	end
    catch  _E:_R ->
	    %% ?log("Error: ~p ~p", [_E,_R]),
	    {noreply, State}
    end;

handle_info(Msg = {'_wxe_destroy_', Pid}, State)
  when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
	true ->
	    Pid ! Msg,
	    ok;
	false ->
	    ok
    end,
    {noreply, State};

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

handle_connect(Object, EvData, From, State0 = #state{users=Users}) ->
    User0 = #user{events=Evs0,evt_handler=Handler0} = gb_trees:get(From, Users),
    Callback = wxEvtHandler:get_callback(EvData),
    case Handler0 of
	#wx_ref{} when Callback =:= 0 ->
	    CBHandler = Handler0,
	    Handler = Handler0;
	undefined when Callback =:= 0 ->
	    Handler = new_evt_listener(State0, From),
	    CBHandler = Handler;
	_ ->
	    CBHandler = new_evt_listener(State0, From),
	    Handler = Handler0
    end,
    Evs = [#event{object=Object,callback=Callback, cb_handler=CBHandler}|Evs0],
    User = User0#user{events=Evs, evt_handler=Handler},
    State1 = State0#state{users=gb_trees:update(From, User, Users)},
    if is_function(Callback) orelse is_pid(Callback) ->
	    {FunId, State} = attach_fun(Callback,State1),
	    Res = wxEvtHandler:connect_impl(CBHandler,Object,
					    wxEvtHandler:replace_fun_with_id(EvData,FunId)),
	    case Res of
		ok     -> {reply,Res,State};
		_Error -> {reply,Res,State0}
	    end;

       true ->
	    Res = {call_impl, connect_cb, CBHandler},
	    {reply, Res, State1}
    end.

invoke_cb({{Ev=#wx{}, Ref=#wx_ref{}}, FunId,_}, _S) ->
    %% Event callbacks
    case get(FunId) of
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
    CB = fun() ->
		 wx:set_env(Env),
		 wxe_util:cast(?WXE_CB_START, <<>>),
		 Res = try
			   Return = Fun(),
			   true = is_binary(Return),
			   Return
		       catch _:Reason ->
			       ?log("Callback fun crashed with {'EXIT, ~p, ~p}~n",
				    [Reason, erlang:get_stacktrace()]),
			       <<>>
		       end,
		 wxe_util:cast(?WXE_CB_RETURN, Res)
	 end,
    spawn(CB),
    ok.

invoke_callback(Pid, Ev, Ref) ->
    Env = get(?WXE_IDENTIFIER),
    CB = fun() ->
		 wx:set_env(Env),
		 wxe_util:cast(?WXE_CB_START, <<>>),
		 try
		     case get_wx_object_state(Pid) of
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
		 catch _:Reason ->
			 wxEvent:skip(Ref),
			 ?log("Callback fun crashed with {'EXIT, ~p, ~p}~n",
			      [Reason, erlang:get_stacktrace()])
		 end,
		 wxe_util:cast(?WXE_CB_RETURN, <<>>)
	 end,
    spawn(CB),
    ok.

get_wx_object_state(Pid) ->
    case process_info(Pid, dictionary) of
	{dictionary, Dict} ->
	    case lists:keysearch('_wx_object_',1,Dict) of
		{value, {'_wx_object_', {_Mod, '_wx_init_'}}} -> ignore;
		{value, {'_wx_object_', Value}} -> Value;
		_ -> ignore
	    end;
	_ -> ignore
    end.

new_evt_listener(State, Owner) ->
    #wx_env{port=Port} = wx:get_env(),
    _ = erlang:port_control(Port,98,<<>>),
    Listener = get_result(State),
    put(Listener, Owner),
    Listener.

get_result(_State) ->
    receive
	{'_wxe_result_', Res} -> Res;
	{'_wxe_error_', Op, Error} ->
	    erlang:error({Error, {wxEvtHandler, {internal_installer, Op}}})
    end.

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

cleanup_evt_listener(U=#user{events=Evs0,evt_handler=Handler}, EvtListener, Object) ->
    {Evs, Del} = lists:foldl(fun(#event{object=Obj,cb_handler=CBH}, {Acc, Delete})
				   when CBH =:= EvtListener, Obj =:= Object ->
				     {Acc, Delete};
				(Event = #event{cb_handler=CBH}, {Acc, _Delete})
				   when CBH =:= EvtListener ->
				     {[Event|Acc], false};
				(Event, {Acc, Delete}) ->
				     {[Event|Acc], Delete}
			     end, {[], true}, Evs0),
    Del andalso wxEvtHandler:destroy_evt_listener(EvtListener),
    case Del andalso Handler =:= EvtListener of
	true ->
	    U#user{events=Evs, evt_handler=undefined};
	false ->
	    U#user{events=Evs}
    end.

handle_disconnect(Object, Evh, From, State0 = #state{users=Users0}) ->
    #user{events=Evs0} = gb_trees:get(From, Users0),
    Fun = wxEvtHandler:get_callback(Evh),
    case find_handler(Evs0, Object, Fun) of
	[] -> {reply, false, State0};
	Handlers ->
	    case disconnect(Object,Evh, Handlers) of
		#event{} ->
		    {reply, true, State0};
		Result ->
		    {reply, Result, State0}
	    end
    end.

disconnect(Object,Evh,[Ev=#event{cb_handler=Handler}|Evs]) ->
    try wxEvtHandler:disconnect_impl(Handler,Object,Evh) of
	true ->  Ev;
	false -> disconnect(Object, Evh, Evs);
	Error -> Error
    catch _:_ ->
	    false
    end;
disconnect(_, _, []) -> false.

find_handler(Evs, Object, Fun) ->
    find_handler(Evs, Object, Fun, []).

find_handler([Ev =#event{object=Object,callback=FunReg}|Evs],Object,Search,Acc) ->
    case FunReg =:= Search of
	true -> find_handler(Evs,Object,Search,[Ev|Acc]);
	false when is_function(FunReg), Search =:= 0 ->
	    find_handler(Evs,Object,Search,[Ev|Acc]);
	_ ->
	    find_handler(Evs,Object,Search,Acc)
    end;
find_handler([_|Evs],Object,Fun,Res) ->
    find_handler(Evs,Object,Fun,Res);
find_handler([],_Object,_Fun,Res) ->
    Res.


%% Cleanup
%% The server handles callbacks from driver so every other wx call must
%% be called from another process, therefore the cleaning must be spawned.
%%
%% Using Disconnect when we terminate can crash, it is timing releated
%% but it seems that disconnect on windows that are being deleted are bad.
%% Since we are terminating the data will be cleaned up anyway.
cleanup(Env, Data, Disconnect) ->
    put(?WXE_IDENTIFIER, Env),
    lists:foreach(fun(User) -> cleanup(User, Disconnect) end, Data),
    gen_server:cast(Env#wx_env.sv, {cleaned, self()}),
    normal.

cleanup(#user{events=Evs, evt_handler=Handler}, Disconnect) ->
    lists:foreach(fun(#event{object=O, callback=CB, cb_handler=CbH}) ->
			  Disconnect andalso (catch wxEvtHandler:disconnect_impl(CbH,O)),
			  case is_function(CB) of
			      true ->
				  wxEvtHandler:destroy_evt_listener(CbH);
			      false ->
				  ignore
			  end
		  end, Evs),
    case Handler of
	undefined -> ignore;
	_ ->  wxEvtHandler:destroy_evt_listener(Handler)
    end,
    ok.
