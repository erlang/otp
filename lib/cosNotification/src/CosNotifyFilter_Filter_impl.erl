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
%% File    : CosNotifyFilter_Filter_impl.erl
%% Purpose : 
%% Note    : For an event to be forwarded it's sufficient that at least
%%           one of the constraints return 'true'.
%%           ALL functions in this module are internal. May NOT be used
%%           externally in ANY WAY; only CosNotification system modules.
%%----------------------------------------------------------------------

-module('CosNotifyFilter_Filter_impl').

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
-export(['_get_constraint_grammar'/2]).
%% External Functions
-export([add_constraints/3, 
	 modify_constraints/4,
	 get_constraints/3, 
	 get_all_constraints/2, 
	 remove_all_constraints/2,
	 destroy/2,
	 match/3, 
	 match_structured/3,
	 match_typed/3, 
	 attach_callback/3, 
	 detach_callback/3,
	 get_callbacks/2]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).

%%--------------- LOCAL DEFINITIONS --------------------------
%%######### MISC ##########
-define(create_ConstraintInfo(_Types, _Con, _ID), 
	#'CosNotifyFilter_ConstraintInfo'{
			       constraint_expression = 
			       #'CosNotifyFilter_ConstraintExp'{
				 event_types = _Types,
				 constraint_expr = _Con},
			       constraint_id = _ID
			      }).

%%#### Data structures ####
-record(state, {constraint_grammar,
		constraints = [],
		filters = [],
		callbacks = [],
		idCounter = 0,
		filterFactory,
		factoryPid,
		etsR}).

%% Data structures constructors
-define(get_InitState(Fac, Pid, Gr), 
	#state{constraint_grammar=Gr,
	       filterFactory=Fac,
	       factoryPid=Pid,
	       etsR = ets:new(oe_ets, [bag, protected])}).

%%------------------- Data structures selectors -------------------
%% Grammar
-define(get_Grammar(S),         S#state.constraint_grammar).
%% Callbacks
% Left out for now to avoid dialyzer warning.
%-define(get_Callback(S,I),      find_obj(lists:keysearch(I, 1, S#state.callbacks),
%					 callback)).
-define(get_AllCallback(S),     lists:map(fun({_V, C}) -> C end, 
					  S#state.callbacks)).
-define(get_AllCallbackID(S),   lists:map(fun({V, _C}) -> V end, 
					  S#state.callbacks)).
%% ID:s
-define(get_IdCounter(S),       S#state.idCounter).

%% Constraints
-define(get_Constraint(S,I),    find_obj(lists:keysearch(I, 1, S#state.constraints),
					 constraint)).
-define(get_AllConstraints(S),  lists:map(fun({I, C, _W, _WC, _K, T}) -> 
						  ?create_ConstraintInfo(T, C, I) 
					  end, 
					  S#state.constraints)).
-define(get_ConstraintAllData(S), S#state.constraints).
-define(get_ConstraintData(S,I), lists:keysearch(I, 1, S#state.constraints)).
-define(match_Type(S,I,ET),     ets:lookup(S#state.etsR, {I, ET})).
%% Parse Tree
-define(get_ParseTree(S,K),     find_obj(ets:lookup(S#state.etsR, K), tree)).

%%------------------- Data structures modifiers -------------------
%% Callbacks
-define(del_Callback(S,I),      S#state{callbacks =
					delete_obj(lists:keydelete(I,1,
								   S#state.callbacks),
						   S#state.callbacks, callback)}).
-define(del_AllCallbacks(S),    S#state{callbacks=[]}).
-define(add_Callback(S,V,C),    S#state{idCounter=V, 
					callbacks = [{V,C}|S#state.callbacks]}).
%% ID:s
-define(set_IdCounter(S,V),     S#state{idCounter=V}).
-define(new_Id(S),              'CosNotification_Common':create_id(S#state.idCounter)).

%% Constraints
-define(del_Constraint(S, I),   match_delete(S, S#state.constraints, I)).
-define(del_AllConstraints(S),  clear_DB(S)).
-define(add_Constraint(S,I,C,W,_WC,K,T), S#state{constraints = 
					  [{I, C, W, _WC, K, T}|S#state.constraints]}).
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

-define(InfoSeq2EventTypeSeq(L), lists:flatten(
				   lists:map(fun(#'CosNotifyFilter_ConstraintInfo'
						 {constraint_expression=
						  #'CosNotifyFilter_ConstraintExp'
						  {event_types = ET}}) -> 
						     ET 
					     end, 
					     L))).

%%-----------------------------------------------------------%
%% Function : handle_info, code_change
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
%% Function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([FiFac, FacPid, ConstraintGr]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(FiFac, FacPid, ConstraintGr)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external attributes ----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Function : '_get_constraint_grammar'/2
%% Type     : readonly
%% Returns  : Grammar - string() 
%%-----------------------------------------------------------
'_get_constraint_grammar'(_OE_THIS, State) ->
    {reply, ?get_Grammar(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Function : add_constraints/3
%% Arguments: CL - CosNotifyFilter::ConstraintExpSeq
%% Returns  : CosNotifyFilter::ConstraintInfoSeq |
%%            {'EXCEPTION', CosNotifyFilter::InvalidConstraint}
%%-----------------------------------------------------------
add_constraints(_OE_THIS, State, CL) ->
    {NewState, Filters, Info, EventTSeq} = try_create_filters(State, CL),
    NewState2=store_filters(NewState, Filters),
    inform_callbacks(?get_AllCallback(NewState2), EventTSeq, []),
    {reply, Info, NewState2}.

%%----------------------------------------------------------%
%% Function : modify_constraints/4
%% Arguments: IDs - CosNotifyFilter::ConstraintIDSeq
%%            AddConstraintInfoSeq - CosNotifyFilter::ConstraintInfoSeq
%% Returns  : ok |
%%            {'EXCEPTION', CosNotifyFilter::InvalidConstraint} |
%%            {'EXCEPTION', CosNotifyFilter::ConstraintNotFound}
%%-----------------------------------------------------------
%% The OMG specification (TC Document telecom/98-11-01, chapter
%% 3.2.1.3) states (concerning IDs):
%%
%% "If all input values supplied within a particular invocation 
%% of this operation are valid, then the specific constraints 
%% identified by the values contained in the first input parameter 
%% will be deleted from the list of those encapsulated by the target 
%% filter object."
%% 
%% Hence, first we must check if all ID's exists before deleting.
modify_constraints(_OE_THIS, State, IDs, AddConstraintInfoSeq) ->
    %% The following functions are 'safe', i.e., they do not alter any data.
    RemoveConstraintInfoSeq = lookup_constraints(IDs, State),
    lookup_constraints(AddConstraintInfoSeq, State),
    {NewState, Filters, _Info, AddedEventTSeq} = 
	try_create_filters(State, AddConstraintInfoSeq),
    RemovedEventTSeq = ?InfoSeq2EventTypeSeq(RemoveConstraintInfoSeq),

    %% We cannot change anything before our checks (see above). Hence,
    %% do NOT move the following lines above this point.
    NewState2  = delete_constraints(IDs, NewState),

%% The OMG specification (TC Document telecom/98-11-01, chapter
%% 3.2.1.3) states (concerning AddConstraintInfoSeq):
%%
%% "If all input values supplied within a particular invocation of this 
%% operation are valid, then the constraint expression associated with the 
%% already encapsulated constraint identified by the numeric value contained
%% within each element of the input sequence will be modified to the new 
%% constraint expression that is contained within the same sequence element."
%%
%% This, our interpretation, implies that ALL previous data related
%% to each unique ID should be removed and replaced by the new data.

    NewState3  = delete_constraints(AddConstraintInfoSeq, NewState2),
    NewState4  = store_filters(NewState3, Filters),
    inform_callbacks(?get_AllCallback(NewState4), 
		     AddedEventTSeq, RemovedEventTSeq),
    {reply, ok, NewState4}.

%%----------------------------------------------------------%
%% Function : get_constraints/3
%% Arguments: IDs - CosNotifyFilter::ConstraintIDSeq
%% Returns  : CosNotifyFilter::ConstraintInfoSeq |
%%            {'EXCEPTION', CosNotifyFilter::ConstraintNotFound}
%%-----------------------------------------------------------
get_constraints(_OE_THIS, State, IDs) ->
    {reply, lookup_constraints(IDs, State), State}.

%%----------------------------------------------------------%
%% Function : get_all_constraints/2
%% Arguments: -
%% Returns  : CosNotifyFilter::ConstraintInfoSeq
%%-----------------------------------------------------------
get_all_constraints(_OE_THIS, State) ->
    {reply, ?get_AllConstraints(State), State}.

%%----------------------------------------------------------%
%% Function : remove_all_constraints/2
%% Arguments: -
%% Returns  : ok
%%-----------------------------------------------------------
remove_all_constraints(_OE_THIS, State) ->
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
%% Returns  : boolean() | 
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------

match(_OE_THIS, State, Event) when is_record(Event,'any'), ?is_EmptyFilter(State) ->
    {reply, true, State};
match(_OE_THIS, State, Event) when is_record(Event,'any') ->
    match_any_event(State, Event, ?get_ConstraintAllData(State));
match(_,_,What) ->
    orber:dbg("[~p] CosNotifyFilter_Filter:match(~p);~n"
	      "Not an CORBA::Any", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% Function : match_structured/3
%% Arguments: Event - CosNotification::StructuredEvent
%% Returns  : boolean() | 
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------
match_structured(_OE_THIS, State, Event) when 
  is_record(Event,'CosNotification_StructuredEvent') andalso ?is_EmptyFilter(State) ->
    {reply, true, State};
match_structured(_OE_THIS, State, Event) when 
  is_record(Event,'CosNotification_StructuredEvent') ->
    match_str_event(State, Event, ?get_ConstraintAllData(State));
match_structured(_,_,What) ->
    orber:dbg("[~p] CosNotifyFilter_Filter:match_structured(~p);~n"
	      "Not a StructuredEvent", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------*
%% Function : match_typed/3
%% Arguments: Data - CosNotification::PropertySeq
%% Returns  : boolean() | 
%%            {'EXCEPTION', CosNotifyFilter::UnsupportedFilterableData}
%%-----------------------------------------------------------
match_typed(_OE_THIS, _State, _Data) ->
    corba:raise(#'NO_IMPLEMENT'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% Function : attach_callback/3
%% Arguments: CB - CosNotifyComm::NotifySubscribe
%% Returns  : ID - CosNotifyFilter::CallbackID
%%-----------------------------------------------------------
attach_callback(_OE_THIS, State, CB) ->
    'CosNotification_Common':type_check(CB, 'CosNotifyComm_NotifySubscribe'),
    CBID = ?new_Id(State),
    {reply, CBID, ?add_Callback(State, CBID, CB)}.

%%----------------------------------------------------------%
%% Function : detach_callback/3
%% Arguments: ID - CosNotifyFilter::CallbackID
%% Returns  : ok | {'EXCEPTION', CosNotifyFilter::CallbackNotFound}
%%-----------------------------------------------------------
detach_callback(_OE_THIS, State, ID) when is_integer(ID) ->
    {reply, ok, ?del_Callback(State, ID)};
detach_callback(_,_,What) ->
    orber:dbg("[~p] CosNotifyFilter_Filter:detach_callback(~p);~n"
	      "Not an integer", [?LINE, What], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% Function : get_callbacks/2
%% Arguments: -
%% Returns  : CosNotifyFilter::CallbackIDSeq
%%-----------------------------------------------------------
get_callbacks(_OE_THIS, State) ->
    {reply, ?get_AllCallbackID(State), State}.

%%--------------- LOCAL FUNCTIONS ----------------------------
%% To match callbacks
find_obj({value, {_, Obj}},_) -> Obj;
%% To match constraints
find_obj({value, {Id, Con, _, _, _, Types}}, _) -> 
    ?create_ConstraintInfo(Types, Con, Id);
find_obj([{_, Tree, tree}|_], tree) -> Tree;
% Left out for now to avoid dialyzer warning.
%find_obj(_,callback) -> {'EXCEPTION', #'CosNotifyFilter_CallbackNotFound'{}};
find_obj(_,tree) -> undefined;
find_obj(_,constraint) -> error.

%% Delete a single object.
delete_obj(List,List,callback) -> corba:raise(#'CosNotifyFilter_CallbackNotFound'{});
delete_obj(List,_,_) -> List.

%% Delete given object from list and all related objects in DB (parse tree and types).
match_delete(State, Constraints, ID) ->
    match_delete(State, Constraints, ID, []).
match_delete(_, [], _, _) ->
    error;
match_delete(State, [{ID, _Con, _Which, _WC, Key, _Types}|T], ID, Acc) ->
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
  when is_record(H, 'CosNotifyFilter_ConstraintInfo') ->
    case ?get_Constraint(State, H#'CosNotifyFilter_ConstraintInfo'.constraint_id) of
	error ->
	    corba:raise(#'CosNotifyFilter_ConstraintNotFound'
			{id = H#'CosNotifyFilter_ConstraintInfo'.constraint_id});
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
  when is_record(H, 'CosNotifyFilter_ConstraintInfo') ->
    case catch ?del_Constraint(State,
			       H#'CosNotifyFilter_ConstraintInfo'.constraint_id) of
	{ok, NewState} ->
	    delete_constraints(T, NewState);
	Reason ->
	    orber:dbg("[~p] 'CosNotifyFilter_Filter':modify_constraints().~n"
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
	    orber:dbg("[~p] 'CosNotifyFilter_Filter':modify_constraints().~n"
		      "Unable to remove: ~p~n"
		      "Reason: ~p~n", 
		      [?LINE, H, Reason], ?DEBUG_LEVEL),
	    delete_constraints(T, State)
    end.
    
%% Inform all registered callbacks that the constraints have changed.
%% Added and Removed must be a CosNotification::EventTypeSeq
inform_callbacks([],_,_) ->
    ok;
inform_callbacks([H|T], Added, Removed) ->
    case catch 'CosNotifyComm_NotifySubscribe':subscription_change(H, Added, Removed) of
	ok ->
	    ?debug_print("INFORMED CALLBACK: ~p   ADDED: ~p   REMOVED: ~p",
			 [H, Added, Removed]),
	    inform_callbacks(T, Added, Removed);
	Other ->
	    orber:dbg("[~p] 'CosNotifyComm_NotifySubscribe':subscription_change().~n"
		      "Unable to inform callback: ~p~n"
		      "Reason: ~p~n", 
		      [?LINE, H, Other], ?DEBUG_LEVEL),
	    inform_callbacks(T, Added, Removed)
    end.

%%-----------------------------------------------------------
%% Function : try_create_filters/2
%% Arguments: CL - #'CosNotifyFilter_ConstraintExp'{
%%                  event_types = [#'CosNotification_EventType'{
%%                                    domain_name = Str, type_name = Str}]
%%                 constraint_expr = Str}
%% Returns  : {State, AccumList}
%%-----------------------------------------------------------
%% !!!!!! This function may not alter any data in DB in any way !!!!!!!!!!
try_create_filters(State, CL) ->
    try_create_filters(State, CL, [], [], []).
try_create_filters(State, [], Accum, InfoSeq, EventTSeq) ->
    {State, Accum, InfoSeq, EventTSeq};
try_create_filters(State, [#'CosNotifyFilter_ConstraintExp'{event_types = Types,
							    constraint_expr = Con}|T],
		   Accum, InfoSeq, EventTSeq) ->
    case catch cosNotification_Filter:create_filter(Con) of
	{ok, Tree} ->
	    case catch cosNotification_Filter:check_types(Types) of
		true ->
		    ID = ?new_Id(State),
		    Key = ?not_CreateDBKey,
		    NewETSeq = Types ++ EventTSeq,
		    try_create_filters(?set_IdCounter(State, ID), T, 
				       [{ID, true, [], Key, Types, Con, Tree}|Accum],
				       [?create_ConstraintInfo(Types, Con, ID)|InfoSeq], 
				       NewETSeq);
		{ok, Which, WC} ->
		    ID = ?new_Id(State),
		    Key = ?not_CreateDBKey,
		    NewETSeq = Types ++ EventTSeq,
		    try_create_filters(?set_IdCounter(State, ID), T, 
				       [{ID, Which, WC, Key, Types, Con, Tree}|Accum],
				       [?create_ConstraintInfo(Types, Con, ID)|InfoSeq], 
				       NewETSeq);
		_ ->
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end;
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidConstraint'
			{constr =  #'CosNotifyFilter_ConstraintExp'
			 {event_types = Types, constraint_expr = Con}})
    end;
try_create_filters(State, [#'CosNotifyFilter_ConstraintInfo'
			   {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			    {event_types = Types, constraint_expr = Con},
			    constraint_id=ID}|T], Accum, InfoSeq, EventTSeq) ->
    case catch cosNotification_Filter:create_filter(Con) of
	{ok, Tree} ->
	    case catch cosNotification_Filter:check_types(Types) of
		true ->
		    Key = ?not_CreateDBKey,
		    NewETSeq = Types ++ EventTSeq,
		    try_create_filters(State, T, 
				       [{ID, true, [], Key, Types, Con, Tree}|Accum],
				       [?create_ConstraintInfo(Types, Con, ID)|InfoSeq], 
				       NewETSeq);
		{ok, Which, WC} ->
		    Key = ?not_CreateDBKey,
		    NewETSeq = Types ++ EventTSeq,
		    try_create_filters(State, T, 
				       [{ID, Which, WC, Key, Types, Con, Tree}|Accum],
				       [?create_ConstraintInfo(Types, Con, ID)|InfoSeq], 
				       NewETSeq);
		_ ->
		    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
	    end;
	_ ->
	    corba:raise(#'CosNotifyFilter_InvalidConstraint'
			{constr =  #'CosNotifyFilter_ConstraintExp'
			 {event_types = Types, constraint_expr = Con}})
    end;
try_create_filters(_,_,_,_,_) ->
    %% The list contained something else but ConstraintExp.
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%-----------------------------------------------------------
%% Function : store_filters/4
%% Arguments: Filters - a list of filters.
%% Returns  : 
%%-----------------------------------------------------------

store_filters(State, []) ->
    State;
store_filters(State, [{ID, Which, WC, Key, Types, Con, Tree}|T]) ->
    ?add_ParseTree(State, Key, Tree),
    write_types(State, Types, ID, Key),
    store_filters(?add_Constraint(State, ID, Con, Which, WC, Key, Types), T).
    

write_types(_State, [],_, _) ->
    ok;
write_types(State, [EventType|T], ID, Key) ->
    ?add_Type(State, ID, EventType, Key),
    ?debug_print("FILTER:  ~p   ~p  ~p~n", [ID, Key, EventType]),
    write_types(State, T, ID, Key).

%%-----------------------------------------------------------
%% Function : match_any_event
%% Arguments: 
%% Returns  : 
%%-----------------------------------------------------------
match_any_event(State, _Event, []) ->
    ?debug_print("FILTER REJECTED:  ~p~n", [_Event]),
    {reply, false, State};
match_any_event(State, Event, [{_, _, _, _, Key, _}|T]) ->
    case catch cosNotification_Filter:eval(?get_ParseTree(State,Key), Event) of
	true ->
	    ?debug_print("FILTER APPROVED (WC):  ~p~n", [Event]),
	    {reply, true, State};
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
    {reply, false, State};
match_str_event(State, Event, [{ID, _Con, Which, WC, Key, _Types}|T]) ->
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
			    {reply, true, State};
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
		    {reply, true, State};
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

