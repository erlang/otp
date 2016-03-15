%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
%% ------------------------------------------------------------
%% Erlang Graphics Interface front-end server
%% ------------------------------------------------------------
%%

-module(gs_frontend).
-compile([{nowarn_deprecated_function,{gs,assq,2}},
          {nowarn_deprecated_function,{gs,error,2}}]).

-export([create/2,
	 config/2,
	 read/2,
	 destroy/2,
	 info/1,
	 start/1,
	 stop/0,
	 init/1,
	 event/3]).


-include("gstk.hrl").


%%----------------------------------------------------------------------
%% The ets contains: {Obj,lives}|{Obj,{Name,Pid}}
%% new obj is {Int,Node}
%%                   {{Name,Pid},Obj}
%%----------------------------------------------------------------------
-record(state, {db,user,user_count,kernel,kernel_count,self}).

%%----------------------------------------------------------------------
%% The interface.
%%----------------------------------------------------------------------
create(GsPid,Args) -> 
    request(GsPid,{create,Args}).

config(GsPid,Args) ->
    request(GsPid,{config, Args}).

read(GsPid,Args) -> 
    request(GsPid,{read, Args}).

destroy(GsPid,IdOrName) -> 
    request(GsPid,{destroy, IdOrName}).

info(Option) ->
    request(gs_frontend,{info,Option}).


%%----------------------------------------------------------------------
%% Comment: Frontend is only locally registered. These functions are called
%%          by any backend.
%%----------------------------------------------------------------------
event(FrontEnd,ToOwner,EventMsg) ->
    FrontEnd ! {event, ToOwner,EventMsg}.


request(GsPid,Msg) ->
    GsPid ! {self(),Msg},
    receive
	{gs_reply,R} -> R
    end.

%%----------------------------------------------------------------------
%% The server
%%----------------------------------------------------------------------

start(Opts) ->
    case whereis(gs_frontend) of
	undefined ->
	    P = spawn_link(gs_frontend,init,[Opts]),
	    case catch register(gs_frontend, P) of
		true ->
		    request(gs_frontend,{instance, backend_name(Opts), Opts});
		{'EXIT', _} ->
		    exit(P,kill), % a raise... and I lost this time
		    start(Opts)   
	    end;
	P ->
	    request(P,{instance,backend_name(Opts),Opts})
    end.

backend_name(Opts) ->
    case gs:assq(kernel,Opts) of
	{value,true} -> kernel;
	_ -> user
    end.


stop() ->
    request(gs_frontend,stop).

%% ------------------------------------------------------------
%%       THE FRONT END SERVER
%% ------------------------------------------------------------
%% Initialize
%%
init(_Opts) ->
    process_flag(trap_exit, true),
    DB=ets:new(gs_names,[set,public]),
    loop(#state{db=DB,self=self()}).

loop(State) ->
    receive
	X ->
						%	    io:format("frontend received: ~p~n",[X]),
	    case catch (doit(X,State)) of
		done -> loop(State);
		NewState when is_record(NewState,state) ->
		    loop(NewState);
		stop -> stop;
		Reason ->
		    io:format("GS frontend. Last mgs in was:~p~n",[X]),
		    io:format("exit:~p~n",[X]),
		    io:format("Reason: ~p~n", [Reason]),
		    terminate(Reason,State),
		    exit(Reason)
	    end
    end.

reply(To,Msg) ->
    To ! {gs_reply,Msg},
    done.

doit({FromOwner,{config, Args}},State) ->
    {IdOrName, Opts} = Args,
    #state{db=DB} = State,
    case idOrName_to_id(DB,IdOrName,FromOwner) of
	undefined ->
	    reply(FromOwner,{error,{no_such_object,IdOrName}});
	Obj ->
	    reply(FromOwner,gstk:config(backend(State,Obj),{Obj,Opts}))
    end;

doit({event,ToOwner,{gs,Obj,Etype,Data,Args}}, #state{db=DB,self=Self}) ->
    case ets:lookup(DB,Obj) of
	[{_,{Name,ToOwner}}] -> ToOwner ! {gs,Name,Etype,Data,Args};
	_ -> ToOwner ! {gs,{Obj,Self},Etype,Data,Args}
    end,
    done;

doit({FromOwner,{create,Args}}, State) ->
    {Objtype, Name, Parent, Opts} = Args,
    #state{db=DB} = State,
    NameOccupied = case {Name, ets:lookup(DB,{Name,FromOwner})} of
		       {undefined,_} -> false;
		       {_, []} -> false;
		       _ -> true
		   end,
    if NameOccupied == true ->
	    reply(FromOwner, {error,{name_occupied,Name}});
       true -> 
	    case idOrName_to_id(DB,Parent,FromOwner) of
		undefined ->
		    reply(FromOwner, {error,{no_such_parent,Parent}});
		ParentObj ->
		    {Id,NewState} = inc(ParentObj,State),
		    case gstk:create(backend(State,ParentObj),
				     {FromOwner,{Objtype,Id,ParentObj,Opts}}) of
			ok ->
			    link(FromOwner),
			    if Name == undefined ->
				    ets:insert(DB,{Id,lives}),
				    reply(FromOwner, Id),
				    NewState;
			       true -> % it's a real name, register it
				    NamePid = {Name,FromOwner},
				    ets:insert(DB,{NamePid,Id}),
				    ets:insert(DB,{Id,NamePid}),
				    reply(FromOwner,Id),
				    NewState
			    end;
			Err -> reply(FromOwner,Err)
		    end
	    end
    end;

doit({FromOwner,{read, Args}}, State) ->
    #state{db=DB} = State,    
    {IdOrName, Opt} = Args,
    case idOrName_to_id(DB,IdOrName,FromOwner) of
	undefined ->
	    reply(FromOwner,{error,{no_such_object,IdOrName}});
	Obj -> 
	    reply(FromOwner,gstk:read(backend(State,Obj),{Obj,Opt}))
    end;

doit({'EXIT', UserBackend, Reason}, State)
  when State#state.user == UserBackend ->
    gs:error("user backend died reason ~w~n", [Reason]),
    remove_user_objects(State#state.db),
    State#state{user=undefined};  

doit({'EXIT', KernelBackend, Reason}, State)
  when State#state.kernel == KernelBackend ->
    gs:error("kernel backend died reason ~w~n", [Reason]),
    exit({gs_kernel_died,Reason});

doit({'EXIT', Pid, _Reason}, #state{kernel=K,user=U,db=DB}) ->
    %% io:format("Pid ~w died reason ~w~n", [Pid, _Reason]),
    if is_pid(U) -> 
	    DeadObjU = gstk:pid_died(U,Pid),
	    remove_objs(DB,DeadObjU);
       true -> ok
    end,
    if is_pid(K) ->
	    DeadObjK = gstk:pid_died(K,Pid),
	    remove_objs(DB,DeadObjK);
       true -> true end,
    done;

doit({FromOwner,{destroy, IdOrName}}, State) ->
    #state{db=DB} = State,
    case idOrName_to_id(DB,IdOrName,FromOwner) of
	undefined ->
	    reply(FromOwner, {error,{no_such_object,IdOrName}});
	Obj ->
	    DeadObj = gstk:destroy(backend(State,Obj),Obj),
	    remove_objs(DB,DeadObj),
	    reply(FromOwner,done)	    
    end;

doit({From,{instance,user,Opts}},State) ->
    #state{db=DB, self=Self, user_count=UC} = State,
    case ets:lookup(DB,1) of
	[_] -> reply(From, {1,Self});
	[] ->
	    ets:insert(DB,{1,lives}), % parent of all user gs objs
	    case gstk:start_link(1, Self, Self, Opts) of
		{ok, UserBackend} ->
		    reply(From, {1, Self}),
		    case UC of
			undefined -> 
			    State#state{user_count=1, user=UserBackend};
			_N ->
			    State#state{user_count=UC+2, user=UserBackend}
		    end;
		{error, Reason} ->
		    reply(From, {error, Reason}),
		    stop
	    end
    end;

doit({From,{instance,kernel,Opts}},State) ->
    #state{db=DB,self=Self} = State,
    case ets:lookup(DB,0) of
	[_] -> reply(From, {0,Self});
	[] ->
	    ets:insert(DB,{0,lives}), % parent of all user gs objs
	    case gstk:start_link(0,Self,Self,Opts) of
		{ok, KernelBackend} ->
		    reply(From, {0,Self}),
		    State#state{kernel_count=0,kernel=KernelBackend};
		{error, Reason} ->
		    reply(From, {error,Reason}),
		    stop
	    end
    end;


doit({From,stop}, State) ->
    #state{kernel=K,user=U} = State,
    if is_pid(U) -> gstk:stop(U);
       true -> true end,
    if is_pid(K) -> gstk:stop(K);
       true -> true end,
    reply(From,stopped),
    stop;

doit({From,{gstk,user,Msg}},State) ->
    reply(From,gstk:request(State#state.user,Msg));
doit({From,{gstk,kernel,Msg}},State) ->
    reply(From,gstk:request(State#state.kernel,Msg));

doit({From,{info,gs_db}},State) ->
    io:format("gs_db:~p~n",[ets:tab2list(State#state.db)]),
    reply(From,State);
doit({From,{info,kernel_db}},State) ->
    reply(From,gstk:request(State#state.kernel,dump_db));
doit({From,{info,user_db}},State) ->
    reply(From,gstk:request(State#state.user,dump_db));
doit({From,{info,Unknown}},_State) ->
    io:format("gs: unknown info option '~w', use one of 'gs_db', 'kernel_db' or 'user_db'~n",[Unknown]),
    reply(From,ok).

terminate(_Reason,#state{db=DB}) ->
    if DB==undefined -> ok;
       true ->
						%	    io:format("frontend db:~p~n",[ets:tab2list(DB)])
	    ok
    end.


backend(#state{user=Upid,kernel=Kpid},Obj) ->
    if Obj rem 2 == 0 -> Kpid;
       true -> Upid
    end.

%%----------------------------------------------------------------------
%% Returns: {NewId,NewState}
%%----------------------------------------------------------------------
inc(ParInt,State) when ParInt rem 2 == 1 ->
    X=State#state.user_count+2,
    {X,State#state{user_count=X}};
inc(ParInt,State) when ParInt rem 2 == 0 ->
    X=State#state.kernel_count+2,
    {X,State#state{kernel_count=X}}.

remove_user_objects(DB) ->
    DeadObj = find_user_obj(ets:first(DB),DB),
    remove_objs(DB,DeadObj).

find_user_obj(Int,DB) when is_integer(Int) ->
    if Int rem 2 == 0 -> %% a kernel obj
	    find_user_obj(ets:next(DB,Int),DB);
       true -> %% a user obj
	    [Int|find_user_obj(ets:next(DB,Int),DB)]
    end;
find_user_obj('$end_of_table',_DB) ->
    [];
find_user_obj(OtherKey,DB) ->
    find_user_obj(ets:next(DB,OtherKey),DB).

remove_objs(DB,[Obj|Objs]) ->
    case ets:lookup(DB, Obj) of
	[{_,NamePid}] ->
	    ets:delete(DB,Obj),
	    ets:delete(DB,NamePid);
	[] -> backend_only
    end,
    remove_objs(DB,Objs);
remove_objs(_DB,[]) -> done.

idOrName_to_id(DB,IdOrName,Pid) when is_atom(IdOrName) ->
    case ets:lookup(DB,{IdOrName,Pid}) of
	[{_,Obj}] -> Obj;
	_ -> undefined
    end;
idOrName_to_id(DB,Obj,_Pid) ->
    case ets:lookup(DB,Obj) of
	[_] -> Obj;
	_ -> undefined
    end.




%% ----------------------------------------
%% done

