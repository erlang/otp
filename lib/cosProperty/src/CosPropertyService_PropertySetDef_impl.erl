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
%% File: CosPropertyService_PropertySetDef_impl.erl
%% Modified:
%%
%%-----------------------------------------------------------------
%% README: 
%% (1) The OMG specification states that a property name may not
%%     be an empty string (""). We may restrict this further
%%     but there is no reason for that.
%%-----------------------------------------------------------------
-module('CosPropertyService_PropertySetDef_impl').

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("cosProperty/include/CosPropertyService.hrl").
-include("cosProperty.hrl").

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
%% Mandatory callbacks
-export([init/1, 
	 terminate/2,
	 code_change/3]).

%% Inherrit from CosPropertyService::PropertySet
-export([define_property/4,
	 define_properties/3,
	 get_number_of_properties/2,
	 get_all_property_names/3,
	 get_property_value/3,
	 get_properties/3,
	 get_all_properties/3,
	 delete_property/3,
	 delete_properties/3,
	 delete_all_properties/2,
	 is_property_defined/3]).

%% CosPropertyService::PropertySetDef
-export([get_allowed_property_types/2,
	 get_allowed_properties/2,
	 define_property_with_mode/5,
	 define_properties_with_modes/3,
	 get_property_mode/3,
	 get_property_modes/3,
	 set_property_mode/4,
	 set_property_modes/3]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([dump/0]).

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------
-record(state, {dbKey, defaultMode, okTypes, okProperties, myType}).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

-define(create_InitState(K, D, AT, AP, MT), #state{dbKey = K, defaultMode = D,
						   okTypes = AT, okProperties = AP,
						   myType = MT}).
%% Selectors
-define(get_DBKey(S),           S#state.dbKey).
-define(get_DefaultMode(S),     S#state.defaultMode).
-define(get_okTypes(S),         S#state.okTypes).
-define(get_okProperties(S),    S#state.okProperties).
%% MISC
-define(is_NotSetDef(S),        S#state.myType =/= ?PropertySetDef).
-define(no_PropertyLimits(S),   S#state.okProperties == []).
-define(no_TypeLimits(S),       S#state.okTypes == []).
-define(is_NotStatic(S),        is_binary(S#state.dbKey)).

%% Fun:s
-define(Local2Property,      fun({N,V,_M}) -> 
				     #'CosPropertyService_Property'{property_name = N,
								    property_value = V}
			     end).
-define(Local2Names,      fun({N,_V,_M}) -> 
				  N
			  end).
-define(MemberName(N),    fun(R) -> 
				  case R of
				      Property when is_record(R, 'CosPropertyService_Property') ->
					  Property#'CosPropertyService_Property'.property_name == N;
				      PropertyDef when is_record(R, 'CosPropertyService_PropertyDef') ->
					  PropertyDef#'CosPropertyService_PropertyDef'.property_name == N;
				      _->
					  false
				  end
			  end).

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function   : init/1
%% Description: Initiates the server
%% Returns    : {ok, State}          |
%%              {ok, State, Timeout} |
%%              ignore               |
%%              {stop, Reason}
%%----------------------------------------------------------------------
init({DefMode, AllowedTypes, AllowedProperties, InitProperties, MyType}) ->
    Key = term_to_binary({{erlang:system_time(), 
			   erlang:unique_integer()}, 
			  node()}),
    _F = ?write_function(#oe_CosPropertyService{key=Key,
						properties=InitProperties}),
    write_result(mnesia:transaction(_F)),
    {ok, ?create_InitState(Key, DefMode, AllowedTypes, AllowedProperties, MyType)};
init({static, DefMode, AllowedTypes, AllowedProperties, InitProperties, MyType}) ->
    {ok, ?create_InitState(InitProperties, DefMode, AllowedTypes, 
			   AllowedProperties, MyType)}.

%%---------------------------------------------------------------------%
%% Function   : terminate
%% Description: Shutdown the server
%% Returns    : any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(_Reason, State) when ?is_NotStatic(State) ->
    _DF = ?delete_function({oe_CosPropertyService, ?get_DBKey(State)}),
    catch write_result(mnesia:transaction(_DF)),
    ok;
terminate(_Reason, _State) ->
    ok.

%%---------------------------------------------------------------------%
%% Function   : code_change
%% Description: Convert process state when code is changed
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%----------------------------------------------------------------------
%% Interface CosPropertyService::PropertySet
%%----------------------------------------------------------------------
%%---------------------------------------------------------------------%
%% Function   : define_property
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
define_property(_, _, "", _) ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
define_property(_OE_This, State, Name, Value) when ?is_NotStatic(State) ->
    evaluate_property_data(State, Value, Name),
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch update_property(X, Name, value, Value, 
						   ?get_DefaultMode(State)) of
			    {'EXCEPTION', E} when 
				  is_record(E, 'CosPropertyService_PropertyNotFound') ->
				mnesia_write(State, [{Name, Value, ?get_DefaultMode(State)}|X]);
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
define_property(_OE_This, State, Name, Value) ->
    evaluate_property_data(State, Value, Name),
    X = ?get_DBKey(State),
    case catch update_property(X, Name, value, Value, ?get_DefaultMode(State)) of
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_PropertyNotFound') ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
 	{'EXCEPTION', E} ->
	    corba:raise(E);
 	_NewProperties ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.


%%---------------------------------------------------------------------%
%% Function   : get_property_value
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_property_value(_, _, "") ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
get_property_value(_OE_THIS, State, Name) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, find_property(X, Name, value), State}.

%%---------------------------------------------------------------------%
%% Function   : delete_property
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
delete_property(_, _, "") ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
delete_property(_OE_THIS, State, Name) when ?is_NotStatic(State) ->
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch remove_property(X, Name) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
delete_property(_OE_THIS, State, Name) ->
    X = lookup_table(?get_DBKey(State)),
    %% Check the properties; must raise an exception.
    remove_property(X, Name),
    %% Something is not correct.
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).


%%---------------------------------------------------------------------%
%% Function   : define_properties
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
define_properties(_OE_THIS, State, PropertySeq) when ?is_NotStatic(State) ->
    {OKProperties, Exc} = evaluate_properties_data(State, PropertySeq),
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch define_properties_helper(State, 
							    OKProperties, X, Exc) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
define_properties(_OE_THIS, State, PropertySeq) ->
    {OKProperties, Exc} = evaluate_properties_data(State, PropertySeq),
    X = lookup_table(?get_DBKey(State)),
    case define_properties_helper(State, OKProperties, X, Exc) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

define_properties_helper(_State, [], NewProperties, []) ->
    %% No exceptions, insert the properties.
    NewProperties;
define_properties_helper(_State, [], _, MultipleExceptions) ->
    {'EXCEPTION', #'CosPropertyService_MultipleExceptions'{exceptions = MultipleExceptions}};
define_properties_helper(State, [#'CosPropertyService_Property'
			    {property_name = Name,
			     property_value = Value}|T], Properties, Exc) ->
    case catch update_property(Properties, Name, value, Value, ?get_DefaultMode(State)) of
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_PropertyNotFound') ->
 	     define_properties_helper(State, T, [{Name, Value, ?get_DefaultMode(State)}|Properties], Exc);
 	{'EXCEPTION', E} ->
 	     define_properties_helper(State, T, Properties, 
				 [#'CosPropertyService_PropertyException'
				  {reason = remap_exception(E),
				   failing_property_name = Name}|Exc]);
 	NewProperties ->
	    define_properties_helper(State, T, NewProperties, Exc)
    end.

%%---------------------------------------------------------------------%
%% Function   : get_number_of_properties
%% Arguments  : -
%% Description: Returns the number of properties currently associated
%%              with this object.
%% Returns    : {ok, ulong(), State}
%%----------------------------------------------------------------------
get_number_of_properties(_OE_THIS, State) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, length(X), State}.

%%---------------------------------------------------------------------%
%% Function   : get_all_property_names
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_all_property_names(_OE_THIS, State, Max) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, get_all_property_names_helper(X, [], Max), State}.

get_all_property_names_helper([], Acc, _) -> 
    %% There are no more properties; return a nil-object refernce.
    {ok, Acc, corba:create_nil_objref()};
get_all_property_names_helper(Left, Acc, 0) -> 
    %% There are more properties; create Name Iterartor.
    PropertyNames = lists:map(?Local2Names, Left),
    {ok, Acc, cosProperty:start_PropertyNamesIterator(PropertyNames)};
get_all_property_names_helper([{Name, _, _}|T], Acc, No) ->
    get_all_property_names_helper(T, [Name|Acc], No-1).


%%---------------------------------------------------------------------%
%% Function   : get_properties
%% Arguments  : A list of property names, i.e., string()
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_properties(_OE_THIS, State, PropertyNames) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, locate_names(PropertyNames, X, true, []), State}.

locate_names([], _, AllOK, Acc) ->
    {AllOK, Acc};
locate_names([""|T], X, _AllOK, Acc) ->
    locate_names(T, X, false, [#'CosPropertyService_Property'
			       {property_name = "",
				property_value = 
				any:create(tk_void, ok)}|Acc]);
locate_names([H|T], X, AllOK, Acc) ->
    case catch find_property(X, H, value) of
	{'EXCEPTION', _} ->
	    locate_names(T, X, false, [#'CosPropertyService_Property'
				       {property_name = H,
					property_value = 
					any:create(tk_void, ok)}|Acc]);
	Val ->
	    locate_names(T, X, AllOK, [#'CosPropertyService_Property'
				       {property_name = H,
					property_value = Val}|Acc])
    end.

%%---------------------------------------------------------------------%
%% Function   : get_all_properties
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_all_properties(_OE_THIS, State, Max) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, get_all_properties_helper(X, [], Max), State}.

get_all_properties_helper([], Acc, _) -> 
%% There are no more properties; return a nil-object refernce.
    {ok, Acc, corba:create_nil_objref()};
get_all_properties_helper(Left, Acc, 0) -> 
    %% There are more properties; create Iterartor.
    Properties = lists:map(?Local2Property, Left),
    {ok, Acc, cosProperty:start_PropertiesIterator(Properties)};
get_all_properties_helper([{Name, Val, _}|T], Acc, No) ->
    get_all_properties_helper(T, [#'CosPropertyService_Property'
				  {property_name = Name,
				   property_value = Val}|Acc], No-1).

%%---------------------------------------------------------------------%
%% Function   : delete_properties
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
delete_properties(_OE_THIS, State, []) ->
    {reply, ok, State};
delete_properties(_OE_THIS, State, PropertyNames) when ?is_NotStatic(State) ->
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch delete_properties_helper(X, [], [], 
							    PropertyNames, State, 
							    length(X)) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    {{'EXCEPTION', E}, NotDeleted} ->
				ok = mnesia_write(State, NotDeleted),
				{'EXCEPTION', E};
			    {ok, NotDeleted} ->
				mnesia_write(State, NotDeleted)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
delete_properties(_OE_THIS, State, PropertyNames) ->
    X = lookup_table(?get_DBKey(State)),
    case delete_properties_helper(X, [], [], PropertyNames, State, length(X)) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	_->
	    %% Not acceptable if it was possible to delete one or more Properties.
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

delete_properties_helper([], [], NotDeleted, [], _State, _Len) ->
    %% Since there are no exceptions we have been able to delete all
    %% properties.
    {ok, NotDeleted};
delete_properties_helper([], MultipleExc, NotDeleted, Names, _State, Len) ->
    %% Write remaining events to DB.
    case length(NotDeleted) of
	Len ->
	    {'EXCEPTION', #'CosPropertyService_MultipleExceptions'
	     {exceptions = add_not_found(Names, MultipleExc)}};
	_->
	    {{'EXCEPTION', #'CosPropertyService_MultipleExceptions'
	      {exceptions = add_not_found(Names, MultipleExc)}},
	     NotDeleted}
    end;
delete_properties_helper([{Name, Val, Mode}|T], MultipleExc, NotDeleted, 
			 Names, State, Len) ->
    case lists:member(Name, Names) of
	true when Mode =/= fixed_normal, Mode =/= fixed_readonly ->
 	    delete_properties_helper(T, MultipleExc, NotDeleted, 
				     lists:delete(Name, Names), State, Len);
	true ->
 	    delete_properties_helper(T, [#'CosPropertyService_PropertyException'
					 {reason = fixed_property,
					  failing_property_name = Name}|MultipleExc], 
				     [{Name, Val, Mode}|NotDeleted], 
				     lists:delete(Name, Names), State, Len);
	false ->
 	    delete_properties_helper(T, MultipleExc, [{Name, Val, Mode}|NotDeleted], 
				     Names, State, Len)
    end.

add_not_found([], MultipleExc) ->
    MultipleExc;
add_not_found([Name|T], MultipleExc) ->
    add_not_found(T, [#'CosPropertyService_PropertyException'
		      {reason = property_not_found,
		       failing_property_name = Name}|MultipleExc]).
    


%%---------------------------------------------------------------------%
%% Function   : delete_all_properties
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
delete_all_properties(_OE_THIS, State) when ?is_NotStatic(State) ->
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch delete_all_properties_helper(X, [], State, 
								length(X)) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    true ->
				ok = mnesia_write(State, []),
				true;
			    false ->
				false;
			    {false, NotDeleted} ->
				ok = mnesia_write(State, NotDeleted),
				false
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
delete_all_properties(_OE_THIS, State) ->
    X = lookup_table(?get_DBKey(State)),
    case delete_all_properties_helper(X, [], State, length(X)) of
	false ->
	    {reply, false, State};
	_->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

delete_all_properties_helper([], [], _State, _) ->
    %% Was able to delete all properties.
    true;
delete_all_properties_helper([], NotDeleted, _State, Len) ->
    %% Write remaining events to DB.
    case length(NotDeleted) of
	Len ->
	    false;
	_->
	    {false, NotDeleted}
    end;
delete_all_properties_helper([{Name, Val, fixed_normal}|T], NotDeleted, State, Len) ->
    delete_all_properties_helper(T, [{Name, Val, fixed_normal}|NotDeleted], State, Len);
delete_all_properties_helper([{Name, Val, fixed_readonly}|T], NotDeleted, State, Len) ->
    delete_all_properties_helper(T, [{Name, Val, fixed_readonly}|NotDeleted], State, Len);
delete_all_properties_helper([_|T], NotDeleted, State, Len) ->
    delete_all_properties_helper(T, NotDeleted, State, Len).

%%---------------------------------------------------------------------%
%% Function   : is_property_defined
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
is_property_defined(_, _, "") ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
is_property_defined(_OE_THIS, State, Name) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, lists:keymember(Name, 1, X), State}.

%%----------------------------------------------------------------------
%% Interface CosPropertyService::PropertySetDef
%%----------------------------------------------------------------------
%%---------------------------------------------------------------------%
%% Function   : get_allowed_property_types
%% Arguments  : -
%% Description: Returns the initially supplied restrictions. An empty
%%              list means no restrictions.
%% Returns    : {ok, TypeCodeList,State}
%%----------------------------------------------------------------------
get_allowed_property_types(_OE_THIS, State) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
get_allowed_property_types(_OE_THIS, State) ->
    {reply, {ok, ?get_okTypes(State)}, State}.

%%---------------------------------------------------------------------%
%% Function   : get_allowed_properties
%% Arguments  : 
%% Description: Returns the initially supplied restrictions. An empty
%%              list means no restrictions.
%% Returns    : {ok, PropertyDefList, State}
%%----------------------------------------------------------------------
get_allowed_properties(_OE_THIS, State) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
get_allowed_properties(_OE_THIS, State) ->
    {reply, {ok, ?get_okProperties(State)}, State}.

%%---------------------------------------------------------------------%
%% Function   : define_property_with_mode
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
define_property_with_mode(_OE_THIS, State, _, _, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
define_property_with_mode(_, _, "", _, _) ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
define_property_with_mode(_OE_THIS, State, Name, Value, Mode) 
  when ?is_NotStatic(State) ->
    evaluate_property_data(State, Value, Name),
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch update_property(X, Name, both, Value, Mode) of
			    {'EXCEPTION', E} 
			    when is_record(E, 'CosPropertyService_PropertyNotFound') ->
				mnesia_write(State, [{Name, Value, Mode}|X]);
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
define_property_with_mode(_OE_THIS, State, Name, Value, Mode) ->
    evaluate_property_data(State, Value, Name),
    X = lookup_table(?get_DBKey(State)),
    case catch update_property(X, Name, both, Value, Mode) of
	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_PropertyNotFound') ->
	    %% Should get not allowed exception.
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO});
	{'EXCEPTION', E} ->
	    corba:raise(E);
	_ ->
	    %% Should be impossible.
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%---------------------------------------------------------------------%
%% Function   : define_properties_with_modes
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
define_properties_with_modes(_OE_THIS, State, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
define_properties_with_modes(_OE_THIS, State, PropertyDefSeq) 
  when ?is_NotStatic(State)->
    {OKProperteDefs, Exc} = evaluate_properties_data(State, PropertyDefSeq),
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch define_properties_with_modes_helper(OKProperteDefs, 
								       X, Exc, State) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
define_properties_with_modes(_OE_THIS, State, PropertyDefSeq) ->
    {OKProperteDefs, Exc} = evaluate_properties_data(State, PropertyDefSeq),
    X = lookup_table(?get_DBKey(State)),
     case define_properties_with_modes_helper(OKProperteDefs, X, Exc, State) of
	 {'EXCEPTION', E} -> 
	     corba:raise(E);
	 _ ->
	     corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
     end.
	     

define_properties_with_modes_helper([], NewPropertyDefs, [], _State) ->
    %% No exceptions found.
    NewPropertyDefs;
define_properties_with_modes_helper([], _, Exc, _) ->
    {'EXCEPTION', #'CosPropertyService_MultipleExceptions'{exceptions = Exc}};
define_properties_with_modes_helper([#'CosPropertyService_PropertyDef'
				     {property_name = Name,
				      property_value = Value,
				      property_mode = Mode}|T], X, Exc, State) ->
    case catch update_property(X, Name, both, Value, Mode) of
	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_PropertyNotFound') ->
	    define_properties_with_modes_helper(T, [{Name, Value, Mode}|X], Exc, State);
	{'EXCEPTION', E} ->
	    define_properties_with_modes_helper(T, X, 
						[#'CosPropertyService_PropertyException'
						 {reason = remap_exception(E),
						  failing_property_name = Name}|Exc], 
						State);
	NewX ->
	    define_properties_with_modes_helper(T, NewX, Exc, State)
    end.

%%---------------------------------------------------------------------%
%% Function   : get_property_mode
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_property_mode(_OE_THIS, State, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
get_property_mode(_, _, "") ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
get_property_mode(_OE_THIS, State, Name) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, find_property(X, Name, mode), State}.

%%---------------------------------------------------------------------%
%% Function   : get_property_modes
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
get_property_modes(_OE_THIS, State, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}); 
get_property_modes(_OE_THIS, State, PropertyNames) ->
    X = lookup_table(?get_DBKey(State)),
    {reply, get_property_modes_helper(PropertyNames, X, [], true), State}.

get_property_modes_helper([], _, Acc, Bool) ->
    {Bool, Acc};
get_property_modes_helper([""|T], Properties, Acc, _) ->
    get_property_modes_helper(T, Properties, 
			      [#'CosPropertyService_PropertyMode'
			       {property_name = "",
				property_mode = undefined}|Acc], false);
get_property_modes_helper([Name|T], Properties, Acc, Bool) ->
    case lists:keysearch(Name, 1, Properties) of
	{value, {Name, _, Mode}} ->
	    get_property_modes_helper(T, Properties, 
				      [#'CosPropertyService_PropertyMode'
				       {property_name = Name,
					property_mode = Mode}|Acc], Bool);
	false ->
	    get_property_modes_helper(T, Properties, 
				      [#'CosPropertyService_PropertyMode'
				       {property_name = Name,
					property_mode = undefined}|Acc], false)
    end.

%%---------------------------------------------------------------------%
%% Function   : set_property_mode
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
set_property_mode(_OE_THIS, State, _, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
set_property_mode(_, _, "", _) ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
set_property_mode(_OE_THIS, State, Name, Mode) when ?is_NotStatic(State) ->
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch update_property(X, Name, mode, undefined, Mode) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
set_property_mode(_OE_THIS, State, Name, Mode) ->
    X = lookup_table(?get_DBKey(State)),
    update_property(X, Name, mode, undefined, Mode),
    %% Something is not correct, shouldn't be allowed to update a property when
    %% static.
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

%%---------------------------------------------------------------------%
%% Function   : set_property_modes
%% Arguments  : 
%% Description: 
%% Returns    : {ok, State}
%%----------------------------------------------------------------------
set_property_modes(_OE_THIS, State, _) when ?is_NotSetDef(State) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO});
set_property_modes(_OE_THIS, State, PropertyModes) when ?is_NotStatic(State) ->
    _DF = 
	fun() ->
		case mnesia_read(State) of
		    {'EXCEPTION', E} ->
			{'EXCEPTION', E};
		    X ->
			case catch set_property_modes_helper(PropertyModes, X, [], 
							     State) of
			    {'EXCEPTION', E} ->
				{'EXCEPTION', E};
			    NewProperties ->
				mnesia_write(State, NewProperties)
			end
		end
	end,
    {reply, mnesia_transaction(_DF), State};
set_property_modes(_OE_THIS, State, PropertyModes) ->
    X = lookup_table(?get_DBKey(State)),
    case set_property_modes_helper(PropertyModes, X, [], State) of
	{'EXCEPTION', E} ->
	    corba:raise(E);
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

set_property_modes_helper([], NewProperties, [], _State) ->
    %% No exceptions, write to DB.
    NewProperties;
set_property_modes_helper([], _, Exc, _) ->
    {'EXCEPTION', #'CosPropertyService_MultipleExceptions'{exceptions = Exc}};
set_property_modes_helper([#'CosPropertyService_PropertyMode'
				     {property_name = Name,
				      property_mode = Mode}|T], X, Exc, State) ->
    case catch update_property(X, Name, mode, undefined, Mode) of
	{'EXCEPTION', E} ->
	    set_property_modes_helper(T, X, 
				      [#'CosPropertyService_PropertyException'
				       {reason = remap_exception(E),
					failing_property_name = Name}|Exc], 
				      State);
	NewX ->
	    set_property_modes_helper(T, NewX, Exc, State)
    end.


%%======================================================================
%% Internal functions
%%======================================================================

remap_exception(#'CosPropertyService_ConflictingProperty'{}) -> conflicting_property;
remap_exception(#'CosPropertyService_FixedProperty'{}) ->       fixed_property;
remap_exception(#'CosPropertyService_InvalidPropertyName'{}) -> invalid_property_name;
remap_exception(#'CosPropertyService_PropertyNotFound'{}) ->    property_not_found;
remap_exception(#'CosPropertyService_UnsupportedTypeCode'{}) -> unsupported_type_code;
remap_exception(#'CosPropertyService_UnsupportedProperty'{}) -> unsupported_property;
remap_exception(#'CosPropertyService_ReadOnlyProperty'{}) ->    read_only_property;
remap_exception(#'CosPropertyService_UnsupportedMode'{}) ->     unsupported_mode.

find_property([], _, _) ->
    corba:raise(#'CosPropertyService_PropertyNotFound'{});
find_property([{Name, Value, _}|_], Name, value) ->
    Value; 
find_property([{Name, _, Mode}|_], Name, mode) ->
    Mode; 
% Left out for now to avoid dialyzer warning.
%find_property([{Name, Value, Mode}|_], Name, all) ->
%    {Name, Value, Mode}; 
find_property([_|T], Name, Which) ->
    find_property(T, Name, Which).

remove_property(PropertList, Name) ->
    remove_property(PropertList, Name, []).
remove_property([], _, _) ->
    corba:raise(#'CosPropertyService_PropertyNotFound'{});
remove_property([{Name, _, fixed_normal}|_T], Name, _) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
remove_property([{Name, _, fixed_readonly}|_T], Name, _) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
remove_property([{Name, _, _}|T], Name, Acc) ->
    T++Acc; 
remove_property([H|T], Name, Acc) ->
    remove_property(T, Name, [H|Acc]).


update_property(_, "", _, _, _) ->
    corba:raise(#'CosPropertyService_InvalidPropertyName'{});
update_property(PropertyList, Name, Which, Value, Mode) ->
    update_property(PropertyList, Name, Which, Value, Mode, []).

update_property([], _, _, _, _, _) ->
    corba:raise(#'CosPropertyService_PropertyNotFound'{});
update_property([{Name, _, fixed_readonly}|_], Name, value, _, _, _) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
update_property([{Name, _, fixed_normal}|_], Name, both, _, _, _) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
update_property([{Name, _, fixed_readonly}|_], Name, both, _, _, _) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
update_property([{Name, #any{typecode = TC}, Mode}|T], Name, 
		value, #any{typecode = TC, value = Value}, _Mod, Acc) ->
    [{Name, #any{typecode = TC, value = Value}, Mode}|T]++Acc;
update_property([{Name, #any{typecode = TC}, _Mode}|T], Name, 
		both, #any{typecode = TC, value = Value}, Mod, Acc) ->
    [{Name, #any{typecode = TC, value = Value}, Mod}|T]++Acc;
update_property([{Name, _, _}|_], Name, value, _, _, _) ->
    corba:raise(#'CosPropertyService_ConflictingProperty'{});
update_property([{Name, _, _}|_], Name, both, _, _, _) ->
    corba:raise(#'CosPropertyService_ConflictingProperty'{});
%% Normally we don't need to raise an exception for the two following cases but
%% to be able to manage static Properties we must raise an exception. Well,
%% on the other hand, why should a user try to change a mode to the same value?!
%% But we have no other option.
update_property([{Name, _Value, fixed_normal}|_T], Name, mode, _, fixed_normal, _Acc) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
update_property([{Name, _Value, fixed_readonly}|_T], Name, mode, _, fixed_readonly, _Acc) ->
    corba:raise(#'CosPropertyService_FixedProperty'{});
update_property([{Name, _Value, fixed_normal}|_T], Name, mode, _, _Mode, _Acc) ->
    corba:raise(#'CosPropertyService_UnsupportedMode'{});
update_property([{Name, _Value, fixed_readonly}|_T], Name, mode, _, _Mode, _Acc) ->
    corba:raise(#'CosPropertyService_UnsupportedMode'{});
update_property([{Name, Value, _}|T], Name, mode, _, Mode, Acc) ->
    [{Name, Value, Mode}|T]++Acc;
update_property([H|T], Name, Which, Value, Mode, Acc) ->
    update_property(T, Name, Which, Value, Mode, [H|Acc]).

   
lookup_table(Key) when is_binary(Key) ->
    _RF = ?read_function({oe_CosPropertyService, Key}),
    case mnesia:transaction(_RF) of
        {atomic, [#oe_CosPropertyService{properties=Properties}]} ->
            Properties;
        {atomic, []} ->
            corba:raise(#'OBJECT_NOT_EXIST'{completion_status=?COMPLETED_NO});
        _Other ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end;
lookup_table(Key) when is_list(Key) ->
    Key;
lookup_table(_) ->
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

mnesia_transaction(Fun) ->
    case mnesia:transaction(Fun) of 
	{atomic, {'EXCEPTION', E}} ->
	    corba:raise(E);
	{atomic, ok} ->
	    ok;
	{atomic, Reply} ->
	    Reply;
	_Other ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

mnesia_read(State) ->
    case mnesia:wread({oe_CosPropertyService, ?get_DBKey(State)}) of
	[#oe_CosPropertyService{properties = X}] ->
	    X;
        _Other ->
	    {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}
    end.

mnesia_write(State, X) ->
    mnesia:write(#oe_CosPropertyService{key = ?get_DBKey(State), properties = X}).

%% Check a write transaction
write_result({atomic,ok}) -> ok;
write_result(_Foo) ->
    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO}).

evaluate_properties_data(State, PropertySeq) ->
    evaluate_properties_data(State, PropertySeq, [], []).

evaluate_properties_data(_State, [], OKProperties, Exc) ->
    {OKProperties, Exc};

evaluate_properties_data(State, [#'CosPropertyService_Property'
				 {property_name = Name,
				  property_value = Value}|T], Acc, Exc) ->
    case catch evaluate_property_data(State, Value, Name) of
	ok ->
	    evaluate_properties_data(State, T, [#'CosPropertyService_Property'
						{property_name = Name,
						 property_value = Value}|Acc], Exc);
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_UnsupportedTypeCode') ->
	    evaluate_properties_data(State, T, Acc, 
				     [#'CosPropertyService_PropertyException'
				      {reason = unsupported_type_code,
				       failing_property_name = Name}|Exc]);
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_UnsupportedProperty') ->
	    evaluate_properties_data(State, T, Acc, 
				     [#'CosPropertyService_PropertyException'
				      {reason = unsupported_property,
				       failing_property_name = Name}|Exc])
    end;
evaluate_properties_data(State, [#'CosPropertyService_PropertyDef'
				 {property_name = Name,
				  property_value = Value,
				  property_mode = Mode}|T], Acc, Exc) ->
    case catch evaluate_property_data(State, Value, Name) of
	ok ->
	    evaluate_properties_data(State, T, [#'CosPropertyService_PropertyDef'
						{property_name = Name,
						 property_value = Value,
						 property_mode = Mode}|Acc], Exc);
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_UnsupportedTypeCode') ->
	    evaluate_properties_data(State, T, Acc, 
				     [#'CosPropertyService_PropertyException'
				      {reason = unsupported_type_code,
				       failing_property_name = Name}|Exc]);
 	{'EXCEPTION', E} when is_record(E, 'CosPropertyService_UnsupportedProperty') ->
	    evaluate_properties_data(State, T, Acc, 
				     [#'CosPropertyService_PropertyException'
				      {reason = unsupported_property,
				       failing_property_name = Name}|Exc])
    end;
evaluate_properties_data(_, _, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

evaluate_property_data(State, _, _) when ?no_PropertyLimits(State),
					 ?no_TypeLimits(State) ->
    ok;
evaluate_property_data(State, Value, _Name) when ?no_PropertyLimits(State) ->
    case lists:member(any:get_typecode(Value), ?get_okTypes(State)) of
	true ->
	    ok;
	_ ->
	    corba:raise(#'CosPropertyService_UnsupportedTypeCode'{})
    end;
evaluate_property_data(State, _Value, Name) when ?no_TypeLimits(State) ->
    case lists:any(?MemberName(Name), ?get_okProperties(State)) of
	true ->
	    ok;
	_ ->
	    corba:raise(#'CosPropertyService_UnsupportedProperty'{})
    end;    
evaluate_property_data(State, Value, Name) ->     
    case lists:any(?MemberName(Name), ?get_okProperties(State)) of
	true ->
	    case lists:member(any:get_typecode(Value), ?get_okTypes(State)) of
		true ->
		    ok;
		_ ->
		    corba:raise(#'CosPropertyService_UnsupportedTypeCode'{})  
	    end;
	_ ->
	    corba:raise(#'CosPropertyService_UnsupportedProperty'{})
    end.


%%----------------------------------------------------------------------
%% Debugging functions
%%----------------------------------------------------------------------
dump() ->
    case catch mnesia:dirty_first('oe_CosPropertyService') of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_loop(PreviousKey) ->
    case catch mnesia:dirty_next('oe_CosPropertyService', PreviousKey) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	'$end_of_table' ->
	    ok;
	Key ->
	    dump_print(Key),
	    dump_loop(Key)
    end.

dump_print(Key) ->
    case catch mnesia:dirty_read({'oe_CosPropertyService', Key}) of
	{'EXIT', R} ->
	    io:format("Exited with ~p\n",[R]);
	[{_,_,X}] ->
	    io:format("Property: ~p~n", [X]);
	_ ->
	    ok
    end.
       

%%-------------------------- END OF MODULE -----------------------------
