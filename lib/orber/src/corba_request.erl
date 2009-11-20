%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File: corba_request.erl
%% Description:
%%    This file contains an corba request server for Orber
%%
%%-----------------------------------------------------------------
-module(corba_request).

-behaviour(gen_server).

-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, stop/0, stop_all/0, create/1,
	 create_schema/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, install/2, handle_call/3, handle_info/2]).
-export([handle_cast/2, dump/0, get_key_from_pid/1]).

%%-----------------------------------------------------------------
%% Standard interface CORBA::Request
%%-----------------------------------------------------------------
-export([add_arg/6,
	 invoke/2,
	 delete/1, 
	 send/2,
	 get_response/2]).

%%-----------------------------------------------------------------
%% Mnesia table definition
%%-----------------------------------------------------------------
-record('corba_request', {reqid, ctx, operation, arg_list, result, req_flags, pid}).


%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(dirty_query_context, true).

%% This macro returns a read fun suitable for evaluation in a transaction
-define(read_function(ReqId),
	fun() ->
		mnesia:dirty_read(ReqId)
	end).

%% This macro returns a write fun suitable for evaluation in a transaction
-define(write_function(R),
	fun() ->
		mnesia:dirty_write(R)
	end).

%% This macro returns a write fun suitable for evaluation in a transaction
-define(update(R),
	fun() ->
		mnesia:dirty_write(R)
	end).

%% This macro returns a delete fun suitable for evaluation in a transaction
-define(delete_function(R),
	fun() ->
		mnesia:delete(R)
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

%%-----------------------------------------------------------------
%% Debugging function
%%-----------------------------------------------------------------
dump() ->
    case catch mnesia:dirty_first('orber_request') of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_loop(PreviousKey) ->
    case catch mnesia:dirty_next('orber_request', PreviousKey) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	'$end_of_table' ->
	    ok;
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_print(Key) ->
    case catch mnesia:dirty_read({'orber_request', Key}) of
       {'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	[X] ->
	    io:format("Req Id: ~p, op: ~p\n",[binary_to_term(X#orber_request.object_key),
						   X#orber_request.pid]);
	_ ->
	    ok
    end.
       

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_requestserver}, orber_request, Opts, []).

stop() ->
    gen_server:call(orber_requestserver, stop, infinity).


stop_all() ->
    Fun = fun() ->
		  mnesia:match_object({orber_request, '_', '_', '_', '_', '_', '_', '_'})
	  end,
    case catch mnesia:transaction(Fun) of
	{atomic, Objects} ->
	    lists:foreach(fun({orber_request, _, _, _, _, _, _, _ }) ->
				  ok %gen_server:call(Pid, stop, infinity)
			  end,
			  Objects);
	R ->
	    R
    end.

create() ->
    ?CHECK_EXCEPTION(gen_server:call(orber_requestserver,
				     create, infinity)).

create(Ctx, OP, Args, Flags) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_requestserver,
				     {create, Ctx, OP, Args, Flags}, infinity)).

delete(ReqId) ->
    ?CHECK_EXCEPTION(gen_server:call(orber_requestserver,
				     {delete, ReqId}, infinity)).

%%------------------------------------------------------------
%% Implementation of standard interface
%%------------------------------------------------------------
add_arg(ReqId, ArgumentName, TC, Value, Len, ArgFlags) ->
    Request =  ets:lookup_element(orber_request, ReqId),
    case Request of
	[] ->
	    ok;
	R ->
	    Args = Request#orber_request.arg_list,
	    NewArgs = lists:append(Args, []),
	    ets:insert(orber_request, NewArgs),
	    ok
    end.

invoke(ReqId, InvokeFlags) ->
    ok.



send(ReqId, InvokeFlags) ->
    ok.

get_response(ReqId, ResponseFlags) ->
    [{_, Val}] = ets:lookup_element(orber_request, ReqId),
    Val#'orber_request'.result.

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
init(Env) ->
    case mnesia:wait_for_tables(['orber_request'], infinity) of
	ok ->
	    process_flag(trap_exit, true),
	    {ok, []};
	StopReason ->
	    {stop, StopReason}
    end.

terminate(From, Reason) ->
    ok.



install(Timeout, Options) ->
    %% check if there already exists a database. If not, create one.
    %% DB_initialized = perhaps_create_schema(Nodelist),
    %% check if mnesia is running. If not, start mnesia.
    DB_started = perhaps_start_mnesia(),

    %% Do we have a complete set of IFR tables? If not, create them.
    AllTabs = mnesia:system_info(tables),

    DB_Result = case lists:member(orber_request, AllTabs) of
		    true ->
			case lists:member({local_content, true},
					  Options) of
			    true->
				mnesia:add_table_copy(orber_request,
						      node(),
						      ram_copies);
			    _ ->
				mnesia:create_table(orber_request,
						    [{attributes,
						      record_info(fields,
								  orber_objkeys)}
						     |Options])
			end;
		    _ ->
			mnesia:create_table(orber_request,
					    [{attributes,
					      record_info(fields,
							  orber_objkeys)}
					     |Options])	
		end,
    
    Wait = mnesia:wait_for_tables([orber_request], Timeout),
    %% Check if any error has occured yet. If there are errors, return them.
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
handle_call(stop, From, State) ->
    {stop, normal, [], State};
handle_call(create, From, State) ->
    ReqId = term_to_binary({node(), now()}),
    _F = ?write_function(#'corba_request'{reqid=ReqId}),
    R = write_result(mnesia:transaction(_F)),

    ReqId

    ?query_check(Qres) = mnesia:dirty_read({orber_request, Objkey}),
    case Qres of
	[] ->
	    _F = ?write_function(#orber_requests{object_key=Objkey, pid=Pid}),
	    R = write_result(mnesia:transaction(_F)),
	    if
		R == ok, pid(Pid) ->
		    link(Pid);
		true ->
		    true
	    end,
	    {reply, R, State};
	X ->
	    {reply, {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}},
	     State}
    end;
handle_call({delete, ReqId}, From, State) ->
    ?query_check(Qres) = mnesia:dirty_read({orber_request, ReqId}),
    case Qres of
	[] ->
	    true;
	[X] when pid(X#orber_request.pid) ->
	    unlink(X#orber_request.pid);
	_ ->
	    true
    end,
    _F = ?delete_function({orber_request, ReqId}),
    R = write_result(mnesia:transaction(_F)),
    {reply, R, State}.
	    
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    _MF = fun() ->
		  mnesia:match_object({orber_request, '_', '_', '_', '_', '_', '_', Pid})
	  end,
    ?query_check(Qres) = mnesia:ets(_MF),
    case Qres of
	[] ->
	    true;
	X ->
	    remove_requests(X),
	    unlink(Pid);
	_ ->
	    true
    end,
    {noreply, State}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
get_reqids_from_pid(Pid) ->
    case  mnesia:dirty_match_object({orber_request, '_', '_', '_', '_', '_', '_', Pid}) of
	Keys -> 
	    [Keys]
	_ ->
	    corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO})
    end.

remove_requests([]) ->
    ok;
remove_requests([H|T]) ->
    _F = ?delete_function({orber_request, H#orber_request.reqid}),
    write_result(mnesia:transaction(_F)),
    remove_requests(T).

%%-----------------------------------------------------------------
%% Check a read transaction
query_result(?query_check(Qres)) -> 
    case Qres of
	[Hres] ->
	    Hres#orber_request.pid;
	[] ->
	    {'excpetion', #'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO}};
	Other ->
	    {'excpetion', #'INTERNAL'{completion_status=?COMPLETED_NO}}
    end.

%%-----------------------------------------------------------------
%% Check a write transaction
write_result({atomic,ok}) -> ok;
write_result(Foo) ->
    {'excpetion', #'INTERNAL'{completion_status=?COMPLETED_NO}}.


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


