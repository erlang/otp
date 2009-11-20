%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2009. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : CosNotifyFilter_MappingFilter_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosNotifyFilter_MappingFilter_impl').

%%--------------- INCLUDES -----------------------------------
%% Application files
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").
-include("CosNotification_Definitions.hrl").

%%--------------- IMPORTS ------------------------------------

%%--------------- EXPORTS ------------------------------------
%% External Attributes
-export(['_get_constraint_grammar'/2, 
	 '_get_value_type'/2, 
	 '_get_default_value'/2]).
%% External Functions
-export([add_mapping_constraints/3, 
	 modify_mapping_constraints/4, 
	 get_mapping_constraints/3,
	 get_all_mapping_constraints/2, 
	 remove_all_mapping_constraints/2, 
	 destroy/2,
	 match/3, 
	 match_structured/3, 
	 match_typed/3]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%%######### MISC ##########
-define(create_MappingInfo(_Types, _Con, _ID, _A), 
	#'CosNotifyFilter_MappingConstraintInfo'{
				      constraint_expression = 
				      #'CosNotifyFilter_ConstraintExp'{
					event_types = _Types,
					constraint_expr = _Con},
				      constraint_id = _ID,
				      value = _A
				     }).
%%#### Data structures ####
-record(state, {constraint_grammar,
		value,
		typeC,
		constraints = [],
		filters = [],
		idCounter = 0,
		filterFactory,
		factoryPid,
		etsR}).

%% Data structures constructors
-define(get_InitState(Gr, DVal, FF, FP), 
	#state{constraint_grammar=Gr,
	       value = DVal,
	       typeC = any:get_typecode(DVal),
	       filterFactory = FF,
	       factoryPid = FP,
	       etsR = ets:new(oe_ets, [bag, protected])}).

%%------------------- Data structures selectors -------------------
%% Attributes
-define(get_Grammar(S),         S#state.constraint_grammar).
-define(get_DefVal(S),          S#state.value).
-define(get_DefTC(S),           S#state.typeC).
-define(get_DefAny(S),          S#state.value).

%% ID:s
-define(get_IdCounter(S),       S#state.idCounter).

%% Constraints
-define(get_Constraint(S,I),    find_obj(lists:keysearch(I, 1, S#state.constraints),
					 constraint)).
-define(get_AllConstraints(S),  lists:map(fun({I, C, _W, _WC, _K, T, A}) -> 
						  ?create_MappingInfo(T, C, I, A) 
					  end, 
					  S#state.constraints)).
-define(get_ConstraintAllData(S), S#state.constraints).
-define(get_ConstraintData(S,I), lists:keysearch(I, 1, S#state.constraints)).
-define(match_Type(S,I,ET),     ets:lookup(S#state.etsR, {I, ET})).
%% Parse Tree
-define(get_ParseTree(S,K),     find_obj(ets:lookup(S#state.etsR, K), tree)).

%%------------------- Data structures modifiers -------------------
%% ID:s
-define(set_IdCounter(S,V),     S#state{idCounter=V}).
-define(new_Id(S),              'CosNotification_Common':create_id(S#state.idCounter)).

%% Constraints
-define(del_Constraint(S, I),   match_delete(S, S#state.constraints, I)).
-define(del_AllConstraints(S),  clear_DB(S)).
-define(add_Constraint(S,I,C,W,_WC,K,T,A), S#state{constraints = 
					  [{I, C, W, _WC, K, T, A}|S#state.constraints]}).
-define(set_Constraints(S,C),   S#state{constraints = C}).

-define(del_AllTypes(S),        ets:match_delete(S#state.etsR, {'_','_',types})).
-define(del_Type(S,I),          ets:match_delete(S#state.etsR, {{I, '_'}, '_', types})).
-define(add_Type(S,I,ET,K),     ets:insert(S#state.etsR, {{I, ET}, K, types})).

%% Parse Tree
-define(add_ParseTree(S,K,T),   ets:insert(S#state.etsR, {K, T, tree})).
-define(del_ParseTree(S,K),     ets:delete(S#state.etsR, K)).
-define(del_AllParseTress(S),   ets:match_delete(S#state.etsR, {'_','_',tree})).

%%------------------- MISC ----------------------------------------
-define(is_EmptyFilter(S),     S#state.constraints==[]).
-define(is_EqualType(S,T),     S#state.typeC==any:get_typecode(T)).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: See gen_server documentation.
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(Info, State) ->
    ?debug_print("INFO: ~p  DATA: ~p~n", [State, Info]),
    case Info of
        {'EXIT', _Pid, _Reason} ->
            {noreply, State};
        _ ->
            {noreply, State}
    end.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([FiFac, FacPid, InitGr, DefVal]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(InitGr, DefVal, FiFac, FacPid)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external attributes ----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Function : '_get_constraint_grammar'/2
%% Type     : readonly
%% Returns  : string()
%%-----------------------------------------------------------
'_get_constraint_grammar'(_OE_THIS, State) ->
    {reply, ?get_Grammar(State), State}.
%%----------------------------------------------------------%
%% Function : '_get_value_type'/2
%% Type     : readonly
%% Returns  : CORBA::TypeCode
%%-----------------------------------------------------------
'_get_value_type'(_OE_THIS, State) ->
    {reply, ?get_DefTC(State), State}.
%%----------------------------------------------------------%
%% Function : '_get_default_value'/2
%% Type     : readonly
%% Returns  : #any{}
%%-----------------------------------------------------------
'_get_default_value'(_OE_THIS, State) ->
    {reply, ?get_DefVal(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Function : add_mapping_constraints/3
%% Arguments: Pairs - CosNotifyFilter::MappingConstraintPairSeq
%% Returns  : CosNotifyFilter::MappingConstraintInfoSeq |
%%            {'EXCEPTION', CosNotifyFilter::InvalidConstraint} |
%%            {'EXCEPTION', CosNotifyFilter::InvalidValue}
%%-----------------------------------------------------------
add_mapping_constraints(_OE_THIS, State, Pairs) ->
    {NewState, Filters, Info} = try_create_filters(State, Pairs),
    NewState2=store_filters(NewState, Filters),
    {reply, Info, NewState2}.

%%----------------------------------------------------------%
%% Function : modify_mapping_constraints/4
%% Arguments: IDs - CosNotifyFilter::ConstraintIDSeq
%%            Info - CosNotifyFilter::MappingConstraintInfoSeq
%% Returns  : ok |
%%            {'EXCEPTION', CosNotifyFilter::InvalidConstraint} |
%%            {'EXCEPTION', CosNotifyFilter::InvalidValue} |
%%            {'EXCEPTION', CosNotifyFilter::ConstraintNotFound}
%%-----------------------------------------------------------
modify_mapping_constraints(_OE_THIS, State, IDs, InfoSeq) ->
    lookup_constraints(IDs, State),
    lookup_constraints(InfoSeq, State),
    {NewState, Filters, _Info} = try_create_filters(State, InfoSeq),

    %% We cannot change anything before our checks (see above). Hence,
    %% do NOT move the following lines above this point.

    NewState2  = delete_constraints(IDs, NewState),
    NewState3  = delete_constraints(InfoSeq, NewState2),
    NewState4  = store_filters(NewState3, Filters),
    {reply, ok, NewState4}.

%%----------------------------------------------------------%
%% Function : get_mapping_constraints/3
%% Arguments: IDs - CosNotifyFilter::ConstraintIDSeq
%% Returns  : CosNotifyFilter::MappingConstraintInfoSeq |
%%            {'EXCEPTION', CosNotifyFilter::ConstraintNotFound}
%%-----------------------------------------------------------
get_mapping_constraints(_OE_THIS, State, IDs) ->
    {reply, lookup_constraints(IDs, State), State}.

%%----------------------------------------------------------%
%% Function : get_all_mapping_constraints/2
%% Arguments: -
%% Returns  : CosNotifyFilter::MappingConstraintInfoSeq 
%%-----------------------------------------------------------
get_all_mapping_constraints(_OE_THIS, State) ->
    {reply, ?get_AllConstraints(State), State}.

%%----------------------------------------------------------%
%% Function : remove_all_mapping_constraints/2
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
remove_all_mapping_constraints(_OE_THIS, State) ->
    {reply, ok, ?del_AllConstraints(State)}.

%%----------------------------------------------------------%
%% Function : destroy/2
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
destroy(_OE_THIS, State) ->
    {stop, normal, ok, State}.

%%----------------------------------------------------------%
%% Function : match/3
%% Arguments: Event - #any{}
%% Returns  : boolean(), #any{} (out-type) |
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------
match(_OE_THIS, State, Event) when is_record(Event,'any') andalso ?is_EmptyFilter(State) ->
    {reply, {false, ?get_DefAny(State)}, State};
match(_OE_THIS, State, Event) when is_record(Event,'any') ->
    match_any_event(State, Event, ?get_ConstraintAllData(State));
match(_,_,_) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%----------------------------------------------------------%
%% Function : match_structured/3
%% Arguments: Event - CosNotification::StructuredEvent
%% Returns  : boolean(), #any{} (out-type) |
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------
match_structured(_OE_THIS, State, Event) when 
  is_record(Event,'CosNotification_StructuredEvent') andalso ?is_EmptyFilter(State) ->
    {reply, {false, ?get_DefAny(State)}, State};
match_structured(_OE_THIS, State, Event) when 
  is_record(Event,'CosNotification_StructuredEvent') ->
    match_str_event(State, Event, ?get_ConstraintAllData(State));
match_structured(_,_,_) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------*
%% Function : match_typed/3
%% Arguments: Data - CosNotification::PropertySeq
%% Returns  : boolean() , #any{} (out-type) |
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------
match_typed(_OE_THIS, _State, _Data) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).
	 
%%--------------- LOCAL FUNCTIONS ----------------------------
%% To match constraints
find_obj({value, {Id, Con, _, _, _, Types, Any}}, _) -> 
    ?create_MappingInfo(Types, Con, Id, Any);
find_obj([{_, Tree, tree}|_], tree) -> Tree;
find_obj(_,tree) -> undefined;
find_obj(_,constraint) -> error.

%% Delete given object from list and all related objects in DB (parse tree and types).
match_delete(State, Constraints, ID) ->
    match_delete(State, Constraints, ID, []).
match_delete(_, [], _, _) ->
    error;
match_delete(State, [{ID, _Con, _Which, _WC, Key, _Types, _Any}|T], ID, Acc) ->
    ?del_Type(State, ID),
    ?del_ParseTree(State, Key),
    {ok, ?set_Constraints(State, Acc++T)};
match_delete(State, [H|T], ID, Acc) ->
    match_delete(State, T, ID, [H|Acc]).

%% Remove all data related with constraints; for now, since no other data 
%% stored in DB, we do in a rather brutal way.
clear_DB(State) ->
    catch ets:delete(State#state.etsR),
    State#state{etsR = ets:new(oe_ets, [bag, protected]), constraints=[]}.

%% Given a list of Constrain IDs we want to find the related constraints.
%% !!!!!! This function may not alter any data in DB in any way !!!!!!!!!!
lookup_constraints(IDs, State) ->
    lookup_constraints(IDs, State, []).
lookup_constraints([], _State, Accum) ->
    Accum;
lookup_constraints([H|T], State, Accum) 
  when is_record(H, 'CosNotifyFilter_MappingConstraintInfo') ->
    case ?get_Constraint(State, H#'CosNotifyFilter_MappingConstraintInfo'.constraint_id) of
	error ->
	    corba:raise(#'CosNotifyFilter_ConstraintNotFound'
			{id = H#'CosNotifyFilter_MappingConstraintInfo'.constraint_id});
	_Con ->
	    %% We don't need to collect the result since the input already is of
	    %% the correct type, i.e., ConstraintInfoSeq
	    lookup_constraints(T, State, Accum)
    end;
lookup_constraints([H|T], State, Accum) when is_integer(H) ->
    case ?get_Constraint(State,H) of
	error ->
	    corba:raise(#'CosNotifyFilter_ConstraintNotFound'{id=H});
	Con ->
	    lookup_constraints(T, State, [Con|Accum])
    end;
lookup_constraints(_, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%% Given a list of Constrain IDs we want to delet the related constraints.
%% We need also to return the ConstraintInfoSeq described related to the
%% given ID's.
delete_constraints([], State) ->
    State;
delete_constraints([H|T], State) 
  when is_record(H, 'CosNotifyFilter_MappingConstraintInfo') ->
    case catch ?del_Constraint(State,
			       H#'CosNotifyFilter_MappingConstraintInfo'.constraint_id) of
	{ok, NewState} ->
	    delete_constraints(T, NewState);
	Reason ->
	    orber:dbg("[~p] 'CosNotifyFilter_MappingFilter':modify_mapping_constraints().~n"
		      "Unable to remove: ~p~n"
		      "Reason: ~p~n", 
		      [?LINE, H, Reason], ?DEBUG_LEVEL),
	    delete_constraints(T, State)
    end;
delete_constraints([H|T], State) ->
    case catch ?del_Constraint(State,H) of
	{ok, NewState} ->
	    delete_constraints(T, NewState);
	Reason ->
	    orber:dbg("[~p] 'CosNotifyFilter_MappingFilter':modify_mapping_constraints().~n"
		      "Unable to remove: ~p~n"
		      "Reason: ~p~n", 
		      [?LINE, H, Reason], ?DEBUG_LEVEL),
	    delete_constraints(T, State)
    end.
    
%%-----------------------------------------------------------
%% Function : try_create_filters/2
%% Arguments: CL - #'CosNotifyFilter_MappingConstraintPair{
%%                   constraint_expression = 
%%                      #'CosNotifyFilter_ConstraintExp'{
%%                             event_types = 
%%                                   [#'CosNotification_EventType'{
%%                                      domain_name = Str, type_name = Str}]
%%                             constraint_expr = Str},
%%                   result_to_set = Any}
%% Returns  : {State, AccumList}
%%-----------------------------------------------------------
%% !!!!!! This function may not alter any data in DB in any way !!!!!!!!!!
try_create_filters(State, CL) ->
    try_create_filters(State, CL, [], []).
try_create_filters(State, [], Accum, InfoSeq) ->
    {State, Accum, InfoSeq};
try_create_filters(State, [#'CosNotifyFilter_MappingConstraintPair'
			   {constraint_expression = 
			    #'CosNotifyFilter_ConstraintExp'{event_types = Types,
							     constraint_expr = Con},
			    result_to_set=Any}|T], Accum, InfoSeq) ->
    case catch {?is_EqualType(State,Any), cosNotification_Filter:create_filter(Con)} of
	{false, _} ->
	    corba:raise(#'CosNotifyFilter_InvalidValue'
			{constr =  #'CosNotifyFilter_ConstraintExp'
			 {event_types = Types, constraint_expr = Con},
			 value=Any});
	{_, {ok, Tree}} ->
	    case catch cosNotification_Filter:check_types(Types) of
		true ->
		    ID = ?new_Id(State),
		    Key = ?not_CreateDBKey,
		    try_create_filters(?set_IdCounter(State, ID), T, 
				       [{ID, true, [], Key, Types, Con, Tree, Any}|Accum],
				       [?create_MappingInfo(Types, Con, ID, Any)|InfoSeq]);
		{ok, Which, WC} ->
		    ID = ?new_Id(State),
		    Key = ?not_CreateDBKey,
		    try_create_filters(?set_IdCounter(State, ID), T, 
				       [{ID, Which, WC, Key, Types, Con, Tree, Any}|Accum],
				       [?create_MappingInfo(Types, Con, ID, Any)|InfoSeq]);
		_ ->
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end;
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidConstraint'
			{constr =  #'CosNotifyFilter_ConstraintExp'
			 {event_types = Types, constraint_expr = Con}})
    end;
try_create_filters(State, [#'CosNotifyFilter_MappingConstraintInfo'
			   {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			    {event_types = Types, constraint_expr = Con},
			    constraint_id=ID,
			    value=Any}|T], Accum, InfoSeq) ->
    case catch cosNotification_Filter:create_filter(Con) of
	{ok, Tree} ->
	    case catch cosNotification_Filter:check_types(Types) of
		true ->
		    Key = ?not_CreateDBKey,
		    try_create_filters(State, T, 
				       [{ID, true, [], Key, Types, Con, Tree, Any}|Accum],
				       [?create_MappingInfo(Types, Con, ID, Any)|InfoSeq]);
		{ok, Which, WC} ->
		    Key = ?not_CreateDBKey,
		    try_create_filters(State, T, 
				       [{ID, Which, WC, Key, Types, Con, Tree, Any}|Accum],
				       [?create_MappingInfo(Types, Con, ID, Any)|InfoSeq]);
		_ ->
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end;
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidConstraint'
			{constr =  #'CosNotifyFilter_ConstraintExp'
			 {event_types = Types, constraint_expr = Con}})
    end;
try_create_filters(_,_,_,_) ->
    %% The list contained something else but ConstraintExp.
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------
%% Function : store_filters/4
%% Arguments: Filters - a list of filters.
%% Returns  : 
%%-----------------------------------------------------------

store_filters(State, []) ->
    State;
store_filters(State, [{ID, Which, WC, Key, Types, Con, Tree, Any}|T]) ->
    ?add_ParseTree(State, Key, Tree),
    write_types(State, Types, ID, Key),
    store_filters(?add_Constraint(State, ID, Con, Which, WC, Key, Types, Any), T).
    

write_types(_State, [],_, _) ->
    ok;
write_types(State, [EventType|T], ID, Key) ->
    ?add_Type(State, ID, EventType, Key),
    ?debug_print("FILTER:  ~p   ~p  ~p~n", [ID, Key, EventType]),
    write_types(State, T, ID, Key).

%%-----------------------------------------------------------
%% Function : match_any_event
%% Arguments: Event - #any{}
%% Returns  : 
%%-----------------------------------------------------------
match_any_event(State, _Event, []) ->
    ?debug_print("FILTER REJECTED:  ~p~n", [_Event]),
    {reply, {false, ?get_DefAny(State)}, State};
match_any_event(State, Event, [{_, _, _, _, Key, Any}|T]) ->
    case catch cosNotification_Filter:eval(?get_ParseTree(State,Key), Event) of
	true ->
	    ?debug_print("FILTER APPROVED (WC):  ~p~n", [Event]),
	    {reply, {true, Any}, State};
	_ ->
	    match_any_event(State, Event, T)
    end.
    

%%-----------------------------------------------------------
%% Function : match_str_event
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------

match_str_event(State, _Event, []) ->
    ?debug_print("FILTER REJECTED:  ~p~n", [_Event]),
    {reply, {false, ?get_DefAny(State)}, State};
match_str_event(State, Event, [{ID, _Con, Which, WC, Key, _Types, Any}|T]) ->
    ET = ((Event#'CosNotification_StructuredEvent'.header)
	  #'CosNotification_EventHeader'.fixed_header)
	#'CosNotification_FixedEventHeader'.event_type,
    CheckList = 
	case Which of
	    both ->
		[ET];
	    domain ->
		[ET, 
		 ET#'CosNotification_EventType'{type_name=""},
		 ET#'CosNotification_EventType'{type_name="*"}];
	    type ->
		[ET, 
		 ET#'CosNotification_EventType'{domain_name=""},
		 ET#'CosNotification_EventType'{domain_name="*"}];
	    _ ->
		[ET,
		 ET#'CosNotification_EventType'{type_name=""},
		 ET#'CosNotification_EventType'{type_name="*"},
		 ET#'CosNotification_EventType'{domain_name=""},
		 ET#'CosNotification_EventType'{domain_name="*"}]
	end,
    case check_DB(State, ID, CheckList) of
	false ->
	    %% No match, may have used wildcards, e.g., "dom*".
	    case catch cosNotification_Filter:match_types(
			 ET#'CosNotification_EventType'.domain_name,
			 ET#'CosNotification_EventType'.type_name, 
			 WC) of
		true ->
		    case catch cosNotification_Filter:eval(?get_ParseTree(State,Key), 
							   Event) of
			true ->
			    ?debug_print("FILTER APPROVED (WC):  ~p~n", [Event]),
			    {reply, {true, Any}, State};
			_ ->
			    match_str_event(State, Event, T)
		    end;
		_->
		    match_str_event(State, Event, T)
	    end;
	Key ->
	    case catch cosNotification_Filter:eval(?get_ParseTree(State,Key),
						   Event) of
		true ->
		    ?debug_print("FILTER APPROVED:  ~p~n", [Event]),
		    {reply, {true, Any}, State};
		_ ->
		    match_str_event(State, Event, T)
	    end
    end.

check_DB(_, _, []) ->
    false;
check_DB(State, ID, [H|T]) ->
    case ?match_Type(State, ID, H) of
	[] ->
	    check_DB(State, ID, T);
	[{_, K, types}|_] ->
	    K
    end.
%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------

