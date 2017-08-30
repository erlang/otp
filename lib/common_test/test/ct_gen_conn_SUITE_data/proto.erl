%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2016. All Rights Reserved.
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
-module(proto).

-compile(export_all).

-record(conn_state, {id, pid, ref, data}).

%% TEST1: N connections (same key) -> N conn pids
%% TEST2: N connections (same key) -> 1 conn pid
%% TEST3: N aliases (same key) -> N conn pids
%% TEST4: N aliases (same key) -> 1 conn pid

open(KeyOrAlias) ->
    case ct:get_config(KeyOrAlias) of
	undefined ->
	    {error,{not_available,KeyOrAlias}};
	ConnData ->
	    io:format("Opening connection with ~p~n", [ConnData]),

	    %% if KeyOrAlias == Key, each call returns unique handle
	    %% if KeyOrAlias == Alias, successive calls return same handle
	    {ok,Handle} = ct_gen_conn:start(ConnData,
					    [],
					    ?MODULE,
					    [{name,KeyOrAlias}]),	    
	    io:format("Handle for ~p = ~p~n", [KeyOrAlias,Handle]),
	    Handle
    end.

close(AliasOrHandle) ->
    Handle = get_handle(AliasOrHandle),
    io:format("Closing connection for ~p (~p)~n", [AliasOrHandle,Handle]),
    case ct_gen_conn:stop(Handle) of
	E = {error,_} ->
	    E;
	Result ->
	    Result
    end.

kill_conn_proc(AliasOrHandle) ->
    ConnPid = ct_gen_conn:get_conn_pid(get_handle(AliasOrHandle)),
    io:format("Killing connection process ~p~n", [ConnPid]),
    ConnPid ! fail,
    ok.

send(_) ->
    ok.

%%%-----------------------------------------------------------------
%%% 

init(KeyOrAlias, ConnData, []) ->
    Addr = proplists:get_value(addr, ConnData),
    Port = proplists:get_value(port, ConnData),
    Ref = make_ref(),
    Starter = self(),
    MultConnPids = proplists:get_value(multiple_conn_pids, ConnData),
    ConnPid =
	case MultConnPids of
	    true ->
		spawn(fun() -> active_conn(Starter, KeyOrAlias, Ref, 
					   ConnData) end);
	    _ ->
		ConnMgr = proplists:get_value(conn_mgr_name, ConnData),
		case whereis(ConnMgr) of
		    undefined ->
			MgrPid =
			    spawn(fun() -> active_conn(Starter, KeyOrAlias,
						       Ref, ConnData) end),
			receive MgrPid -> 
				MgrPid
			end;			
		    MgrPid when is_pid(MgrPid) ->
			MgrPid ! {connect,Ref},
			MgrPid
		end
	end,
    io:format("Connection ~p opened on ~p:~p -> ~p (~p)~n",
	      [KeyOrAlias,Addr,Port,ConnPid,Ref]),
    {ok,ConnPid,#conn_state{id=KeyOrAlias, pid=ConnPid, ref=Ref, data=ConnData}}.


terminate(ConnPid, #conn_state{id=Id, pid=ConnPid, ref = Ref, data=Data}) ->
    case proplists:get_value(multiple_conn_pids, Data) of
	true ->
	    ConnPid ! close;
	_ ->
	    ConnPid ! {close,Ref}
    end,
    io:format("Connection ~p on ~p (~p) closing!~n", [Id,ConnPid,Ref]),
    ok.


reconnect(ConnData, State = #conn_state{id=Id, ref=DeadRef}) ->    
    io:format("Reconnect for ~p initiated...~n", [DeadRef]),
    case application:get_env(ct_test, reconnect) of
	{ok,true} ->
	    ConnMgr = proplists:get_value(conn_mgr_name, ConnData),
	    NewRef = make_ref(),
	    Starter = self(),
	    ConnPid =
		case proplists:get_value(multiple_conn_pids, ConnData) of
		    true ->
			spawn(fun() ->
				      active_conn(Starter, Id, NewRef,
						  ConnData)
			      end);
		    _ ->
			case whereis(ConnMgr) of
			    undefined ->
				MgrPid =
				    spawn(fun() ->
						  active_conn(Starter, Id,
							      NewRef, ConnData)
					  end),
				receive MgrPid -> 
					MgrPid
				end;			
			    MgrPid ->
				MgrPid ! {reconnect,DeadRef,NewRef},
				MgrPid
			end
		end,
	    io:format("Connection ~p reopened on ~p (~p)~n",
		      [Id,ConnPid,NewRef]),
	    {ok,ConnPid,State#conn_state{pid=ConnPid, ref=NewRef}};
	_ ->
	    {error,no_reconnection_allowed}
    end.

%%%-----------------------------------------------------------------
%%% 

active_conn(Starter, Id, Ref, ConnData) ->
    ConnMgr = proplists:get_value(conn_mgr_name, ConnData),
    case proplists:get_value(multiple_conn_pids, ConnData) of
	true ->
	    ok;
	_ ->
	    register(ConnMgr,self()),
	    io:format("Connection manager ~p on ~p started for "
		      "~p and ~p~n",
		      [ConnMgr,self(),Id,Ref])
    end,
    Starter ! self(),
    active_conn_loop(ConnData, [Ref]).

active_conn_loop(ConnData, Conns) ->
    receive
	{connect,Ref} ->
	    io:format("Connecting ~p on ~p~n",
		      [Ref,self()]),	    
	    active_conn_loop(ConnData, [Ref | Conns]);
	{reconnect,DeadRef,NewRef} ->
	    Conns1 = [NewRef | lists:delete(DeadRef, Conns)],
	    io:format("Reconnecting on ~p: ~p -> ~p~n",
		      [self(),DeadRef,NewRef]),
	    active_conn_loop(ConnData, Conns1);
	close ->
	    io:format("Conn process ~p shutting down~n", [self()]),
	    ok;
	{close,Ref} ->
	    io:format("Closing connection ~p on ~p~n", [Ref,self()]),
	    case proplists:delete(Ref, Conns) of
		[] ->
		    io:format("Last connection on ~p closed, "
			      "now stopping~n", [self()]),
		    ok;
		Conns1 ->
		    active_conn_loop(ConnData, Conns1)
	    end;
	fail ->
	    io:format("Connection process not feeling good...~n", []),
	    exit(kaboom);
	{respond,To} ->
	    To ! {self(),hello},
	    active_conn_loop(ConnData, Conns)
    end.
    
%%%-----------------------------------------------------------------
%%% 

get_handle(AliasOrHandle) when is_pid(AliasOrHandle) ->
    AliasOrHandle;

get_handle(AliasOrHandle) ->
    {ok,{H,_}} = ct_util:get_connection(AliasOrHandle,
					?MODULE),
    H.

