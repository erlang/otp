%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
%% File: CosNaming_NamingContextExt_impl.erl
%% Modified:
%%
%%-----------------------------------------------------------------
%% README: 
%% (1) 
%%
%%-----------------------------------------------------------------
-module('CosNaming_NamingContextExt_impl').
 
%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("CosNaming.hrl").
-include("CosNaming_NamingContext.hrl").
-include("CosNaming_NamingContextExt.hrl").
-include("orber_cosnaming.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory callbacks
-export([init/1, 
         terminate/2,
         code_change/3]).
 
%% Inherrit from CosNaming::NamingContext
-export([bind/4, 
	 rebind/4, 
	 bind_context/4, 
	 rebind_context/4,
	 resolve/3, 
	 unbind/3, 
	 new_context/2, 
	 bind_new_context/3,
	 list/3, 
	 destroy/2]).
 
%% CosNaming::NamingContextExt
-export([to_string/3,
	 to_name/3,
	 to_url/4,
	 resolve_str/3]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([dump/0, 
	 install/2]).
 
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
%% DEBUG INFO
-define(DEBUG_LEVEL, 5).

%%======================================================================
%% External functions
%%======================================================================
%%---------------------------------------------------------------------%
%% Function   : init/1
%% Description: Initiates the server
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%%----------------------------------------------------------------------
init([]) ->
    {ok, term_to_binary('undefined')};

init(DBKey) ->
    _F = ?write_function(#orber_CosNaming{name_context=DBKey,
					  nameindex=[]}),
    write_result(mnesia:transaction(_F)),
    {ok, DBKey}.
 
%%---------------------------------------------------------------------%
%% Function   : terminate
%% Description: Shutdown the server
%% Returns    : any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
 
%%---------------------------------------------------------------------%
%% Function   : code_change
%% Description: Convert process state when code is changed
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
 
%%---------------------------------------------------------------------%
%% Function   : install
%% Arguments  : Timeout - abort if timeout triggered.
%%              Options - mnesia options
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
install(Timeout, Options) ->
    %% Fetch a list of the defined tables to see if 'CosNaming' is defined.
    AllTabs = mnesia:system_info(tables),
    DB_tables_created =
	case lists:member('orber_CosNaming', AllTabs) of
	    true ->
		case lists:member({local_content, true},
				  Options) of
		    true->
			mnesia:add_table_copy('orber_CosNaming',
					      node(),
					      ram_copies);
		    _->
			mnesia:create_table('orber_CosNaming',[{attributes,
								record_info(fields,
									    'orber_CosNaming')}
							       |Options])
		end;
	    _ ->
		mnesia:create_table('orber_CosNaming',[{attributes,
							record_info(fields,
								    'orber_CosNaming')}
						       |Options])
	end,
    Wait = mnesia:wait_for_tables(['orber_CosNaming'], Timeout),
    %% Check if any error has occured yet. If there are errors, return them.

    if
	DB_tables_created == {atomic, ok},
	Wait == ok ->
	    _F = ?write_function(#orber_CosNaming{name_context=
						  term_to_binary('undefined'),
						  nameindex=[]}),
	    write_result(mnesia:transaction(_F));
	true -> 
	    {error, [DB_tables_created, Wait]}
    end.


%%----------------------------------------------------------------------
%% Interface CosNaming::NamingContext
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Function   : bind
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
bind(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _BF = 
	fun() ->
		case mnesia:wread({orber_CosNaming, SubobjKey}) of
		    [#orber_CosNaming{nameindex = X}] ->
			case lists:keysearch(N, 1, X) of
			    {value, _} ->
				{'EXCEPTION', #'CosNaming_NamingContext_AlreadyBound'{}};
			    false ->
				mnesia:write(#orber_CosNaming{name_context=SubobjKey,
							      nameindex=[{N, nobject, Obj} | X]})
			end;
		    Other ->
			orber:dbg("[~p] ~p:bind(~p, ~p);~n"
				  "DB access returned ~p", 
				  [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
			{'EXCEPTION', #'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
									       cxt=OE_THIS}}
		end
	end,
    case mnesia:transaction(_BF) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    {reply, ok, OE_State};
	Other ->
	    orber:dbg("[~p] ~p:bind(~p, ~p);~n"
		      "DB transaction returned ~p", 
		      [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
bind(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':bind(NC, T, Obj), OE_State};
		{value, {H, ncontext, NC}} ->
		    bind(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
bind(_OE_THIS, _OE_State, [], _Obj) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:bind();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).

%%----------------------------------------------------------------------
%% Function   : rebind
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
rebind(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RBF = 
	fun() ->
		case mnesia:wread({orber_CosNaming, SubobjKey}) of
		    [#orber_CosNaming{nameindex = X}] ->
			KList = 
			    case lists:keysearch(N, 1, X) of
				{value, {N, _, _V}} ->
				    lists:keyreplace(N, 1, X, {N, nobject, Obj});
				false ->
				    [{N, nobject, Obj} | X]
			    end,
			mnesia:write(#orber_CosNaming{name_context=SubobjKey,
						      nameindex=KList});
		    Other ->
			orber:dbg("[~p] ~p:rebind(~p, ~p);~n"
				  "DB access returned ~p", 
				  [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
			{'EXCEPTION', #'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
									       cxt=OE_THIS}}
		end
	end,
    case mnesia:transaction(_RBF) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    {reply, ok, OE_State};
	Other ->
	    orber:dbg("[~p] ~p:rebind(~p, ~p);~n"
		      "DB transaction returned ~p", 
		      [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
rebind(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
						       cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':rebind(NC, T, Obj), OE_State};
		{value, {H, ncontext, NC}} ->
		    rebind(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
rebind(_OE_THIS, _OE_State, [], _Obj) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:rebind();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).

%%----------------------------------------------------------------------
%% Function   : bind_context
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
bind_context(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _BCF = 
	fun() ->
		case mnesia:wread({orber_CosNaming, SubobjKey}) of
		    [#orber_CosNaming{nameindex = X}] ->
			case lists:keysearch(N, 1, X) of
			    {value, _} ->
				{'EXCEPTION', #'CosNaming_NamingContext_AlreadyBound'{}};
			    false ->
				mnesia:write(#orber_CosNaming{name_context=SubobjKey,
							      nameindex=
							      [{N, ncontext, Obj} | X]})
			end;
		    Other ->
			orber:dbg("[~p] ~p:bind_context(~p, ~p);~n"
				  "DB access returned ~p", 
				  [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
			{'EXCEPTION', #'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
									       cxt=OE_THIS}}
		end
	end,
    case mnesia:transaction(_BCF) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    {reply, ok, OE_State};
	Other ->
	    orber:dbg("[~p] ~p:bind_context(~p, ~p);~n"
		      "DB transaction returned ~p", 
		      [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
bind_context(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':bind_context(NC, T, Obj), 
		     OE_State};
		{value, {H, ncontext, NC}} ->
		    bind_context(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
bind_context(_OE_THIS, _OE_State, [], _Obj) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:bind_context();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).

%%----------------------------------------------------------------------
%% Function   : rebind_context
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
rebind_context(OE_THIS, OE_State, [N], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RBCF = 
	fun() ->
		case mnesia:wread({orber_CosNaming, SubobjKey}) of
		    [#orber_CosNaming{nameindex = X}] ->
			KList = 
			    case lists:keysearch(N, 1, X) of
				{value, {N, _, _V}} ->
				    lists:keyreplace(N, 1, X, {N, ncontext, Obj});
				false ->
				    [{N, ncontext, Obj} | X]
			    end,
			mnesia:write(#orber_CosNaming{name_context=SubobjKey,
						      nameindex= KList});
		    Other ->
			orber:dbg("[~p] ~p:rebind_context(~p, ~p);~n"
				  "DB access returned ~p", 
				  [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
			{'EXCEPTION', #'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
									       cxt=OE_THIS}}
		end
	end,
    case mnesia:transaction(_RBCF) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    {reply, ok, OE_State};
	Other ->
	    orber:dbg("[~p] ~p:rebind_context(~p, ~p);~n"
		      "DB transaction returned ~p", 
		      [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
rebind_context(OE_THIS, OE_State, [H|T], Obj) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H,ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':rebind_context(NC, T, Obj), 
		    OE_State};
		{value, {H,ncontext, NC}} ->
		    rebind_context(NC, OE_State, T, Obj);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
rebind_context(_OE_THIS, _OE_State, [], _Obj) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:rebind_context();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).

%%----------------------------------------------------------------------
%% Function   : resolve
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
resolve(OE_THIS, OE_State, [N]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(N, 1, X) of
		{value, {N, _, Value}} ->
		    {reply, Value, OE_State};
		false ->
		    corba:raise(#'CosNaming_NamingContext_NotFound'
				{rest_of_name=[N], why='not_object'})
	    end
    end;
resolve(OE_THIS, OE_State, [H|T]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':resolve(NC, T), OE_State};
		{value, {H, ncontext, NC}} ->
		    resolve(NC, OE_State, T);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
resolve(_OE_THIS, _OE_State, []) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:resolve();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).

%%----------------------------------------------------------------------
%% Function   : unbind
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
unbind(OE_THIS, OE_State, [N]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _UBF = 
	fun() ->
		case mnesia:wread({orber_CosNaming, SubobjKey}) of
		    [#orber_CosNaming{nameindex = X}] ->
			KList = lists:keydelete(N, 1, X),
			mnesia:write(#orber_CosNaming{name_context=SubobjKey,
						      nameindex= KList});
		    Other ->
			orber:dbg("[~p] ~p:unbind(~p, ~p);~n"
				  "DB transaction returned ~p", 
				  [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
			{'EXCEPTION', #'CosNaming_NamingContext_CannotProceed'{rest_of_name=[N],
									       cxt=OE_THIS}}
		end
	end,
    case mnesia:transaction(_UBF) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    {reply, ok, OE_State};
	Other ->
	    orber:dbg("[~p] ~p:unbind(~p, ~p);~n"
		      "DB transaction returned ~p", 
		      [?LINE, ?MODULE, N, SubobjKey, Other], ?DEBUG_LEVEL),
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
unbind(OE_THIS, OE_State, [H|T]) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'CosNaming_NamingContext_CannotProceed'{rest_of_name=[H|T],
								 cxt=OE_THIS});
	X ->
	    case lists:keysearch(H, 1, X) of
		{value, {H, ncontext, NC}} when is_record(NC, 'IOP_IOR') ->
		    {reply, 'CosNaming_NamingContext':unbind(NC, T), OE_State};
		{value, {H, ncontext, NC}} ->
		    unbind(NC, OE_State, T);
		_ ->
		    corba:raise(#'CosNaming_NamingContext_CannotProceed'
				{rest_of_name=[H|T], cxt=OE_THIS})
	    end
    end;
unbind(_OE_THIS, _OE_State, []) ->
    orber:dbg("[~p] CosNaming_NamingContextExt:unbind();~n"
	      "Invoked this operation with an empty list", 
	      [?LINE], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_YES}).


%%----------------------------------------------------------------------
%% Function   : new_context
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
new_context(_OE_THIS, OE_State) ->
    DBKey = term_to_binary({{erlang:system_time(),
			     erlang:unique_integer()}, 
			    node()}),
    %% Create a record in the table and set the key to a newly
    {reply, 
     'CosNaming_NamingContextExt':oe_create(DBKey, 
					    [{pseudo, true}|?CREATE_OPTS]), 
     OE_State}.

%%----------------------------------------------------------------------
%% Function   : bind_new_context
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
bind_new_context(OE_THIS, OE_State, N) ->
    DBKey = term_to_binary({{erlang:system_time(),
			     erlang:unique_integer()}, 
			    node()}),
    %% Create a record in the table and set the key to a newly
    %% generated objectkey.
    %%?PRINTDEBUG("bind_new_context"),
    NewCtx = 'CosNaming_NamingContextExt':oe_create(DBKey, 
						    [{pseudo, true}|?CREATE_OPTS]),
    %% Bind the created name context to a name
    case catch bind_context(OE_THIS, OE_State, N, NewCtx) of
	{'EXCEPTION', E} ->
	    'CosNaming_NamingContextExt':destroy(NewCtx),
	    corba:raise(E);
	{reply, ok, _} ->
	    {reply, NewCtx, OE_State}
    end.


%%----------------------------------------------------------------------
%% Function   : list
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
list(OE_THIS, OE_State, HowMany) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    _RF = ?read_function({orber_CosNaming, SubobjKey}),
    case orber_cosnaming_utils:query_result(mnesia:transaction(_RF)) of
	error ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
	X ->
	    case convert_list(X, HowMany, 0, []) of
		{false, List} ->
		    {reply, {ok, List, ?ORBER_NIL_OBJREF}, OE_State};
		{true, List, Rest} ->
		    %% By setting HowMany to '-1' it will never match
		    %% the Counter. Hence, the whole list will be transformed.
		    {false, List2} = convert_list(Rest, -1, 0, []),
		    BIterator = 'CosNaming_BindingIterator':
			oe_create(List2, ?CREATE_OPTS),
		    {reply, {ok, List, BIterator}, OE_State}
	    end
    end.

convert_list([], _, _, Acc) ->
    {false, Acc};
convert_list(Rest, Counter, Counter, Acc) ->
    {true, Acc, Rest};
convert_list([{N, T, _O}|Rest], HowMany, Counter, Acc) ->
    convert_list(Rest, HowMany, Counter+1,
		 [#'CosNaming_Binding'{binding_name=[N],
				       binding_type=T}|Acc]).

%%----------------------------------------------------------------------
%% Function   : destroy
%% Arguments  : 
%% Description: 
%% Returns    : 
%%----------------------------------------------------------------------
destroy(OE_THIS, OE_State) ->
    SubobjKey = corba:get_subobject_key(OE_THIS),
    try begin
            true = (byte_size(SubobjKey) < 20),
            undefined = binary_to_term(SubobjKey)
        end
    of
        _ ->
            corba:raise(#'NO_PERMISSION'{completion_status=?COMPLETED_NO})
    catch
        error:_ ->  %% Not atom 'undefined', carry on...
	    _DF = 
		fun() ->
			case mnesia:wread({orber_CosNaming, SubobjKey}) of
			    [#orber_CosNaming{nameindex = []}] ->
				mnesia:delete({orber_CosNaming, SubobjKey});
			    Other when is_list(Other) ->
				orber:dbg("[~p] ~p:destroy(~p);~n"
					  "DB access returned ~p", 
					  [?LINE, ?MODULE, SubobjKey, Other], ?DEBUG_LEVEL),
				{'EXCEPTION', #'CosNaming_NamingContext_NotEmpty'{}}
			end
		end,
	    case mnesia:transaction(_DF) of 
		{atomic, {'EXCEPTION', E}} ->
		    corba:raise(E);
		{atomic, ok} ->
		    {reply, ok, OE_State};
		Other ->
		    orber:dbg("[~p] ~p:destroy(~p);~n"
			      "DB transaction returned ~p", 
			      [?LINE, ?MODULE, SubobjKey, Other], ?DEBUG_LEVEL),
		    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
	    end
    end.

%%----------------------------------------------------------------------
%% Interface CosNaming::NamingContextExt
%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
%% Function   : to_string
%% Arguments  : Name
%% Description: 
%% Returns    : StringName |
%%              {'EXCEPTION', NamingContext::InvalidName{}}
%%----------------------------------------------------------------------
to_string(_OE_This, OE_State, Name) ->
    {reply, orber_cosnaming_utils:name2string(Name), OE_State}.


%%----------------------------------------------------------------------
%% Function   : to_name
%% Arguments  : StringName
%% Description: 
%% Returns    : Name |
%%              {'EXCEPTION', NamingContext::InvalidName{}}
%%----------------------------------------------------------------------
to_name(_OE_This, OE_State, StringName) ->
    {reply, orber_cosnaming_utils:string2name(StringName), OE_State}.


%%----------------------------------------------------------------------
%% Function   : to_url
%% Arguments  : Address
%%              StringName
%% Description: 
%% Returns    : URLString |
%%              {'EXCEPTION', NamingContext::InvalidName{}}
%%              {'EXCEPTION', NamingContextExt::InvalidAddress{}}
%%----------------------------------------------------------------------
to_url(_, _, "", _) ->
    %% Empty address not allowed.
    corba:raise(#'CosNaming_NamingContextExt_InvalidAddress'{});
to_url(_OE_This, OE_State, Address, "") ->
    %% Empty stringname => use corbaloc
    orber_cosnaming_utils:check_addresses(Address),
    {reply, "corbaloc:"++orber_cosnaming_utils:escape_string(Address), OE_State};
to_url(_OE_This, OE_State, Address, StringName) ->
    %% Non-empty stringname => use corbaname
    orber_cosnaming_utils:check_addresses(Address),
    orber_cosnaming_utils:check_name(StringName),
    {reply, 
     "corbaname:"++orber_cosnaming_utils:escape_string(Address)++"#"++
     orber_cosnaming_utils:escape_string(StringName), 
     OE_State}.

%%----------------------------------------------------------------------
%% Function   : resolve_str
%% Arguments  : StringName
%% Description: 
%% Returns    : Object |
%%              {'EXCEPTION', NamingContext::InvalidName{}}
%%              {'EXCEPTION', NamingContext::NotFound{why, rest_of_name}}
%%              {'EXCEPTION', NamingContext::CannotProceed{cxt, rest_of_name}}
%%----------------------------------------------------------------------
resolve_str(OE_This, OE_State, StringName) ->
    Name = orber_cosnaming_utils:string2name(StringName),
    resolve(OE_This, OE_State, Name).

%%======================================================================
%% Internal functions
%%======================================================================
%% Check a write transaction
write_result({atomic,ok}) -> ok;
write_result(_What) ->
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

 
%%----------------------------------------------------------------------
%% Debugging functions
%%----------------------------------------------------------------------
dump() ->
    case catch mnesia:dirty_first('orber_CosNaming') of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_loop(PreviousKey) ->
    case catch mnesia:dirty_next('orber_CosNaming', PreviousKey) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	'$end_of_table' ->
	    ok;
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_print(Key) ->
    case catch mnesia:dirty_read({'orber_CosNaming', Key}) of
       {'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	[X] ->
	    io:format("name_context: ~p\n-----------------------------\n"
		      " nameindex structure\n-----------------------------\n~p\n\n",
		      [binary_to_term(X#orber_CosNaming.name_context),
		       X#orber_CosNaming.nameindex]);
	_ ->
	    ok
    end.

%%-------------------------- END OF MODULE -----------------------------
