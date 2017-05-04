%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2017. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: orber_objectkeys.erl
%% 
%% Description:
%%    This file contains the object keyserver in Orber
%%
%%-----------------------------------------------------------------
-module(orber_objectkeys).

-behaviour(gen_server).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, stop/0, stop_all/0, get_pid/1, is_persistent/1,
	 register/2, register/3, delete/1, create_schema/1, check/1,
	 remove_old_keys/0]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, install/2, handle_call/3, handle_info/2, code_change/3]).
-export([handle_cast/2, dump/0, get_key_from_pid/1, gc/1]).

%%-----------------------------------------------------------------
%% Mnesia Table definition record
%%-----------------------------------------------------------------
-record(orber_objkeys, {object_key, pid, persistent=false, timestamp}).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(dirty_query_context, true).

%% This macro returns a read fun suitable for evaluation in a transaction
-define(read_function(Objkey),
	fun() ->
		mnesia:dirty_read(Objkey)
	end).

%% This macro returns a write fun suitable for evaluation in a transaction
-define(write_function(R),
	fun() ->
		mnesia:dirty_write(R)
	end).

%% This macro returns a delete fun suitable for evaluation in a transaction
-define(delete_function(R),
	fun() ->
		mnesia:delete(R)
	end).

%% Use this fun inside a transaction to get a list of all keys.
-define(match_function(),
	fun() ->
		mnesia:match_object({orber_objkeys, '_', '_','_','_'})
	end).

-ifdef(dirty_query_context).
-define(query_check(Q_res), Q_res).
-else.
-define(query_check(Q_res), {atomic, Q_res}).
-endif.


-define(CHECK_EXCEPTION(Res), case Res of
				  {'EXCEPTION', E} ->
				      corba:raise(E);
				   R ->
				      R
			      end).

-define(DEBUG_LEVEL, 6).



%%-----------------------------------------------------------------
%% Debugging function
%%-----------------------------------------------------------------
dump() ->
    case catch mnesia:dirty_first('orber_objkeys') of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_loop(PreviousKey) ->
    case catch mnesia:dirty_next('orber_objkeys', PreviousKey) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	'$end_of_table' ->
	    ok;
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_print(Key) ->
    case catch mnesia:dirty_read({'orber_objkeys', Key}) of
       {'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	[X] ->
	    io:format("object_key: ~p, pid: ~p, persistent: ~p, timestamp: ~p\n",
		      [binary_to_term(X#orber_objkeys.object_key),
		       X#orber_objkeys.pid,
		       X#orber_objkeys.persistent,
		       X#orber_objkeys.timestamp]);
	_ ->
	    ok
    end.
       

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_objkeyserver}, orber_objectkeys, Opts, []).

stop() ->
    gen_server:call(orber_objkeyserver, stop, infinity).

remove_old_keys() ->
    %% This function may ONLY be used when restarting a crashed node.
    %% We must remove all objects started with {global, "name"} otherwise
    %% we cannot restart the node using the same name.
    Fun = fun() -> 
		  Node = node(),
		  mnesia:write_lock_table(orber_objkeys),
		  Objects = mnesia:match_object(orber_objkeys,
						mnesia:table_info(orber_objkeys,
								  wild_pattern),
						read),
		  lists:foreach(fun(Obj) ->
					case node(Obj#orber_objkeys.pid) of
					    Node ->
						mnesia:delete({orber_objkeys, 
							       Obj#orber_objkeys.object_key});
					    _->
						ok
					end
				end,
				Objects),
		  ok
	  end,
    write_result(mnesia:transaction(Fun)).

stop_and_remove_local(Reason) ->
    %% This function may ONLY be used when this server terminates with reason 
    %% normal or shutdown.
    Fun = fun() -> 
		  Node = node(),
		  mnesia:write_lock_table(orber_objkeys),
		  Objects = mnesia:match_object(orber_objkeys, 
						mnesia:table_info(orber_objkeys,
								  wild_pattern),
						read),
		  lists:foreach(fun(Obj) ->
					case node(Obj#orber_objkeys.pid) of
					    Node ->
						exit(Obj#orber_objkeys.pid, Reason),
						mnesia:delete({orber_objkeys, 
							       Obj#orber_objkeys.object_key});
					    _->
						ok
					end
				end,
				Objects),
		  ok
	  end,
    write_result(mnesia:transaction(Fun)).
    
stop_all() ->
    Fun = ?match_function(),
    case mnesia:transaction(Fun) of
	{atomic, Objects} ->
	    lists:foreach(fun(Obj) ->
				  gen_server:call(Obj#orber_objkeys.pid, 
						  stop, infinity)
			  end,
			  Objects);
	R ->
	    R
    end.

get_pid(Objkey) ->
    case catch ets:lookup_element(orber_objkeys, Objkey, 3) of
	Pid when is_pid(Pid) ->
	    Pid;
	dead ->
	    {error, "unable to contact object"};
	_ ->
	    %% This call is necessary if a persistent object have died
	    %% and the objectkey server is currently updating the Pid
	    %% to equal 'dead'. Without this case 'OBJECT_NOT_EXIST'
	    %% would be raised which is uncorrect if the object is 
	    %% persistent.
	    ?CHECK_EXCEPTION(gen_server:call(orber_objkeyserver,
					     {get_pid, Objkey}, 
					     infinity))
    end.

is_persistent(Pid) when is_pid(Pid) ->
    case catch get_key_from_pid(Pid) of
	{'EXCEPTION', _} ->
	    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
	Key ->
	    is_persistent(Key)
    end;
is_persistent(Objkey) ->
    case catch ets:lookup_element(orber_objkeys, Objkey, 4) of
	{'EXIT', _R} ->
	    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
	Boolean ->
	    Boolean
    end.


gc(Sec) when is_integer(Sec) ->
    Fun = fun() -> 
		  mnesia:write_lock_table(orber_objkeys),
		  Objects = mnesia:match_object({orber_objkeys, '_', dead, true,'_'}),
		  lists:foreach(fun(Obj) ->
					case timetest(Sec, Obj#orber_objkeys.timestamp) of
					    true ->
						mnesia:delete({orber_objkeys, 
							       Obj#orber_objkeys.object_key});
					    _->
						ok
					end
				end,
				Objects),
		  ok
	  end,
    write_result(mnesia:transaction(Fun)).

register(Objkey, Pid) ->
    'register'(Objkey, Pid, false).

register(Objkey, Pid, Type) when is_pid(Pid) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_objkeyserver,
				     {register, Objkey, Pid, Type}, 
				     infinity));
register(Objkey, Pid, Type) ->
    orber:dbg("[~p] orber_objectkeys:register(~p, ~p); Not a Pid ~p", 
	      [?LINE, Objkey, Type, Pid], ?DEBUG_LEVEL),
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

delete(Objkey) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_objkeyserver,
				     {delete, Objkey}, infinity)).

check(Objkey) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_objkeyserver,
				     {check, Objkey}, infinity)).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
init(_Env) ->
    case mnesia:wait_for_tables(['orber_objkeys'], infinity) of
	ok ->
	    process_flag(trap_exit, true),
	    start_gc_timer(orber:objectkeys_gc_time());
	StopReason ->
	    {stop, StopReason}
    end.

terminate(shutdown, _State) ->
    stop_and_remove_local(shutdown),
    ok;
terminate(normal, _State) ->
    stop_and_remove_local(normal),
    ok;
terminate(_Reason, _State) ->
    ok.

start_gc_timer(infinity) ->
    {ok, []};
start_gc_timer(Time) ->
    timer:start(),   
    case timer:send_after(timer:seconds(Time), 
			  orber_objkeyserver, {oe_gc, Time}) of
	{ok, _} ->
	    {ok, []};
	StopReason ->
	    {stop, StopReason}
    end.
	
install(Timeout, Options) ->
    %% check if there already exists a database. If not, create one.
    %% DB_initialized = perhaps_create_schema(Nodelist),
    %% check if mnesia is running. If not, start mnesia.
    perhaps_start_mnesia(),

    %% Do we have a complete set of IFR tables? If not, create them.
    AllTabs = mnesia:system_info(tables),

    DB_Result = case lists:member(orber_objkeys, AllTabs) of
		    true ->
			case lists:member({local_content, true},
					  Options) of
			    true->
				mnesia:add_table_copy(orber_objkeys,
						      node(),
						      ram_copies);
			    _ ->
				mnesia:create_table(orber_objkeys,
						    [{attributes,
						      record_info(fields,
								  orber_objkeys)}
						     |Options])
			end;
		    _ ->
			mnesia:create_table(orber_objkeys,
					    [{attributes,
					      record_info(fields,
							  orber_objkeys)}
					     |Options])	
		end,
    
    Wait = mnesia:wait_for_tables([orber_objkeys], Timeout),
    %% Check if any error has occurred yet. If there are errors, return them.
    if
	DB_Result == {atomic, ok},
	Wait == ok ->
	    ok;
	true ->
	    {error, {DB_Result, Wait}}
    end.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%
%% Comment:
%%   In objectkey gen_server all exceptions are tupples and corba:raise
%%   may not be used. It is too time consuming to add catches in every 
%%   function before returning. On the client side there is a case which 
%%   maps every tupple on the format {'exception', E} to corba:raise(E).
%%-----------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, [], State};
handle_call({get, Objkey}, _From, State) ->
    R = query_result(mnesia:dirty_read({orber_objkeys, Objkey})),
    {reply, R, State};

handle_call({register, Objkey, Pid, Type}, _From, State) ->
    _WF = fun() ->
		  case mnesia:wread({orber_objkeys, Objkey}) of
		      [] ->
			  %% No key exists. Ok to register.
			  mnesia:write(#orber_objkeys{object_key=Objkey, pid=Pid,
						      persistent=Type,
						      timestamp=erlang:monotonic_time(seconds)});
		      [X] when X#orber_objkeys.persistent==true,
			       X#orber_objkeys.pid == dead ->
			  %% A persistent object is being restarted. Update Pid & time.
			  mnesia:write(X#orber_objkeys{pid=Pid, timestamp=erlang:monotonic_time(seconds)});
		      [X] when is_pid(X#orber_objkeys.pid) ->
			  %% Object exists, i.e., trying to create an object with
			  %% the same name.
			  orber:dbg("[~p] orber_objectkeys:register(~p, ~p); Object already exists.", 
				    [?LINE, Objkey, Type], ?DEBUG_LEVEL),
			  {'EXCEPTION', #'BAD_PARAM'{completion_status=?COMPLETED_NO}};
		      Why ->
			  %% Something else occured.
			  orber:dbg("[~p] orber_objectkeys:register(~p, ~p); error reading from DB(~p)", 
				      [?LINE, Objkey, Type, Why], ?DEBUG_LEVEL),
			  {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}
		  end
	  end,
    R = write_result(mnesia:transaction(_WF)),
    if
	R == ok andalso is_pid(Pid) ->
	    link(Pid);
	true ->
	    true
    end,
    {reply, R, State};

handle_call({delete, Objkey}, _From, State) ->
    ?query_check(Qres) = mnesia:dirty_read({orber_objkeys, Objkey}),
    case Qres of
	[] ->
	    true;
	[X] when is_pid(X#orber_objkeys.pid) ->
	    unlink(X#orber_objkeys.pid);
	_ ->
	    true
    end,
    _F = ?delete_function({orber_objkeys, Objkey}),
    R = write_result(mnesia:transaction(_F)),
    {reply, R, State};

handle_call({get_pid, Objkey}, _From, State) ->
    _F = fun() ->
		 mnesia:read({orber_objkeys, Objkey})
	 end,
    case mnesia:transaction(_F) of
	{atomic, [X]} when is_pid(X#orber_objkeys.pid) ->
	    {reply, X#orber_objkeys.pid, State};
	{atomic, [X]} when X#orber_objkeys.pid == dead ->
	    {reply,
	     {'EXCEPTION', #'TRANSIENT'{completion_status=?COMPLETED_NO}},
	     State};
	_Res ->
	    {reply, 
	     {'EXCEPTION', #'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO}},
	     State}
    end;
handle_call({check, {_, 'key', Objkey, _, _, _}}, _From, State) ->
    ?query_check(Qres) = mnesia:dirty_read({orber_objkeys, Objkey}),
    case Qres of
	[_X] ->
	    {reply, 'object_here', State};
	_ ->
	    {reply, 'unknown_object', State}
    end;
handle_call({check, {_, 'registered', Objkey, _, _, _}}, _From, State) ->
    case whereis(Objkey) of
	undefined ->
	    case catch ets:lookup_element(orber_objkeys, Objkey, 4) of
		true ->
		    {reply, 'object_here', State};
		_->
		    {reply, 'unknown_object', State}
	    end;
	_ ->
	    {reply, 'object_here', State}
    end;
handle_call({check, {_, 'pseudo', Module, _, _, _}}, _From, State) ->
    case code:is_loaded(Module) of
	false ->
	    {reply, 'unknown_object', State};
	_ ->
	    {reply, 'object_here', State}
    end;

handle_call({check, "INIT"}, _From, State) ->
    {reply, 'object_here', State};
handle_call({check, _}, _From, State) ->
    {reply, 'unknown_object', State}.

	    
handle_info({'EXIT', Pid, Reason}, State) when is_pid(Pid) ->
    _WF = fun() ->
		  case mnesia:match_object({orber_objkeys, '_', Pid,'_','_'}) of
		      [] ->
			  ok;
		      [X] when X#orber_objkeys.persistent==false ->
			  mnesia:delete({orber_objkeys, X#orber_objkeys.object_key});
		      [X] when is_pid(X#orber_objkeys.pid) andalso
			       X#orber_objkeys.persistent==true andalso
			       Reason /= normal andalso
			       Reason /= shutdown ->
			  mnesia:write(X#orber_objkeys{pid=dead,
						       timestamp=erlang:monotonic_time(seconds)});
		      [X] when X#orber_objkeys.persistent==true ->
			  mnesia:delete({orber_objkeys, X#orber_objkeys.object_key});
		      _->
			  ok
		  end
	  end,
    case write_result(mnesia:transaction(_WF)) of
	ok ->
	    unlink(Pid);
	_->
	    true
    end,
    {noreply, State};

handle_info({oe_gc, Secs}, State) ->
    catch gc(Secs),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

timetest(S, TimeStamp) ->
    TimeStamp+S < erlang:monotonic_time(seconds).

get_key_from_pid(Pid) ->
    case  mnesia:dirty_match_object({orber_objkeys, '_', Pid,'_','_'}) of
	[Keys] -> 
	    Keys#orber_objkeys.object_key;
	_ ->
	    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO})
    end.

%remove_keys([], _) ->
%    ok;
%remove_keys([H|T], R) when H#orber_objkeys.persistent==false ->
%    _F = ?delete_function({orber_objkeys, H#orber_objkeys.object_key}),
%    write_result(mnesia:transaction(_F)),
%    remove_keys(T, R).

%%-----------------------------------------------------------------
%% Check a read transaction
query_result(?query_check(Qres)) -> 
    case Qres of
	[Hres] ->
	    Hres#orber_objkeys.pid;
	[] ->
	    {'EXCEPTION', #'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO}};
	Other ->
	    orber:dbg("[~p] orber_objectkeys:query_result(); DB lookup failed(~p)", 
				    [?LINE, Other], ?DEBUG_LEVEL),
	    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}
    end.

%%-----------------------------------------------------------------
%% Check a write transaction
write_result({atomic,ok}) -> ok;
write_result(Foo) ->
    orber:dbg("[~p] orber_objectkeys:query_result(); DB write failed(~p)", 
	      [?LINE, Foo], ?DEBUG_LEVEL),
    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}.


create_schema(Nodes) ->
    case mnesia:system_info(use_dir) of
	false ->
	    mnesia:create_schema(Nodes);
	_ ->
	    ok
    end.

perhaps_start_mnesia() ->
    case mnesia:system_info(is_running) of
	no ->
	    mnesia:start();
	_ ->
	    ok
    end.


%%------------------------------------------------------------
%% Standard gen_server cast handle
%%
handle_cast(_, State) ->
    {noreply,  State}.


