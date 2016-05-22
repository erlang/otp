%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : cosNotification_Filter.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(cosNotification_Filter).


%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% Application files
-include("CosNotification.hrl").
-include("CosNotifyChannelAdmin.hrl").
-include("CosNotifyComm.hrl").
-include("CosNotifyFilter.hrl").

-include("CosNotification_Definitions.hrl").

%%--------------- EXPORTS ------------------------------------
%% Internal Filter Functions
-export([eval/1, 
	 eval/2,
	 create_filter/1,
	 check_types/1,
	 match_types/3,
	 validate_types/1]).

%%--------------- DEFINES ------------------------------------
-define(EVENT_PATH,         [{dotid,"header"}, {dotid,"fixed_header"},
			     {dotid,"event_name"}]).
-define(DOMAIN_PATH,        [{dotid,"header"}, {dotid,"fixed_header"},
			     {dotid,"event_type"}, {dotid,"domain_name"}]).
-define(TYPE_PATH,          [{dotid,"header"}, {dotid,"fixed_header"},
			     {dotid,"event_type"}, {dotid,"type_name"}]).
-define(VARIABLE_PATH(I),   [{dotid,"header"}, {dotid,"variable_header"}, {dotid,I}]).
-define(FILTERABLE_PATH(I), [{dotid,"filterable_data"}, {dotid,I}]).


%%------------------------------------------------------------
%%--------------- FILTER FUNCTIONS ---------------------------
%%------------------------------------------------------------
%%------------------------------------------------------------
%% function : create_filter/1
%% Arguments: String - Filter grammar
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------
create_filter(Str) ->
    {ok, Tokens} = cosNotification_Scanner:scan(Str),
    case cosNotification_Grammar:parse(Tokens) of
	{ok, Filter} ->
	    {ok, Filter};
	_->
	    corba:raise(#'CosNotifyFilter_InvalidConstraint'{constr = Str})
    end.

%%------------------------------------------------------------
%% function : eval
%% Arguments: 
%% Returns  : 
%% Effect   : 
%%------------------------------------------------------------

eval('$empty') -> true;
eval(Tree)     -> eval(Tree, []).


%% Leaf expressions (literals and idents).
eval('$empty', _)               -> true;
eval(Lit, _Env) when is_number(Lit) -> Lit;
eval(Lit, _Env) when is_list(Lit)   -> Lit; %list == string
eval(Lit, _Env) when is_atom(Lit)   -> Lit;   %atom == bool
eval({component, V}, []) ->
    %% Cannot evaluate variables at this stage.
    throw({error, {unbound_variable, V}});
eval({component, V}, Env) ->
    case catch lookup(V, Env, undefined) of
	{ok, Val} -> 
	    Val;
	_X -> 
	    {error, {unbound_variable, V}}
    end;

%% CORBA2.3-15/26 states:
%% "The name parameters in tk_objref, tk_struct, tk_union, tk_enum, tk_alias, 
%% tk_value, tk_value_box, tk_abstract_interface, tk_native and tk_except TypeCodes
%% and the member name parameters in tk_struct, tk_union, tk_enum, tk_value and
%% tk_except TypeCodes are not specified by (or significant in) GIOP. Agents should
%% not make assumptions about type equivalence based on these name values; only the
%% structural information (including RepositoryId values, if provided) is
%% significant. If provided, the strings should be the simple, unscoped names
%% supplied in the OMG IDL definition text. If omitted, they are encoded as empty
%% strings."
%% Makes it rather hard to follow the grammar 100 %.
eval({default_component, V}, Env) ->
    case catch lookup(V, Env, default_component) of
	{ok, false} -> 
	    false;
	{ok, true} -> 
	    true;
	_X -> 
	    {error, {unbound_variable, V}}
    end;
eval({exist_component, V}, Env) ->
    case catch lookup(V, Env, exist_component) of
	{ok, false} -> 
	    false;
	{ok, _} -> 
	    true;
	{error, _} -> 
	    false;
	_X -> 
	    {error, {unbound_variable, V}}
    end;
%% Arithmetic expressions.
eval({'*', X, Y}, Env) ->
    eval_arith({fun(_X, _Y) -> _X*_Y end, X, Y}, Env);
eval({'/', X, Y}, Env) ->
    eval_arith({fun(_X, _Y) -> _X/_Y end, X, Y}, Env);
eval({'+', X, Y}, Env) ->
    eval_arith({fun(_X, _Y) -> _X+_Y end, X, Y}, Env);
eval({'-', X, Y}, Env) ->
    eval_arith({fun(_X, _Y) -> _X-_Y end, X, Y}, Env);
eval({'u-', X}, Env) ->
    eval_arith({fun(_X) -> -_X end, X}, Env);
%% Relational expressions.
eval({'==', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X == _Y end, X, Y}, Env);
eval({'!=', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X /= _Y end, X, Y}, Env);
eval({'<', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X < _Y end, X, Y}, Env);
eval({'<=', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X =< _Y end, X, Y}, Env);
eval({'>', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X > _Y end, X, Y}, Env);
eval({'>=', X, Y}, Env) ->
    eval_rel({fun(_X, _Y) -> _X >= _Y end, X, Y}, Env);
eval({'~', Needle, Haystack}, Env) ->		%substring match
    N = eval(Needle, Env),
    H = eval(Haystack, Env),
    if
	is_list(N) andalso is_list(H) ->
	    string:str(H, N) /= 0;
	true ->
	    throw({error, {bad_type, Needle, Haystack}})
    end;
eval({'in', Needle, Haystack}, Env) ->		%set membership
    N = eval(Needle, Env),
    H = eval(Haystack, Env),
    if
	is_list(H) ->
	    lists:member(N, H);
	true ->
	    throw({error, {bad_type, Needle, Haystack}})
    end;
%% Boolean expressions.
eval({'and', false, _Y}, _Env) ->
    false;
eval({'and', _X, false}, _Env) ->
    false;
eval({'and', X, Y}, Env) ->
    eval_and_bool({fun(_X, _Y) -> _X and _Y end, X, Y}, Env);

eval({'or', true, _Y}, _Env) ->
    true;
eval({'or', _X, true}, _Env) ->
    true;
eval({'or', X, Y}, Env) ->
    eval_or_bool({fun(_X, _Y) -> _X or _Y end, X, Y}, Env);
eval({'not', X}, Env) ->
    eval_bool({fun(_X) -> not _X end, X}, Env);
%% Catch-all
eval(_T, _Env) ->
    throw({error, internal}).

eval_bool({Fun, X}, Env) ->
    Xe = eval(X, Env),
    if 
	is_atom(Xe) ->
	    Fun(Xe);
	true ->
	    throw({error, {bad_type, X}})
    end.
eval_and_bool({Fun, X, Y}, Env) ->
    case eval(X, Env) of
	false ->
	    %% No need for evaluating the other expression.
	    false;
	Xe ->
	    Ye = eval(Y, Env),
	    if 
		is_atom(Xe) andalso is_atom(Ye) ->
		    Fun(Xe, Ye);
		true ->
		    throw({error, {bad_type, X, Y}})
	    end
    end.
eval_or_bool({Fun, X, Y}, Env) ->
    case eval(X, Env) of
	true ->
	    %% No need for evaluating the other expression.
	    true;
	Xe ->
	    Ye = eval(Y, Env),
	    if 
		is_atom(Xe) andalso is_atom(Ye) ->
		    Fun(Xe, Ye);
		true ->
		    throw({error, {bad_type, X, Y}})
	    end
    end.


%% According to issue 2203, OMG stated that arithmetic operations involving booleans
%% is allowed. TRUE equals 1 and FALSE 0. They refer to:

%% "We at NEC like this feature, and feel it is both required and
%% standard with the way CORBA treats boolean values. We feel it's
%% required because it allows the constraint grammar to handle
%% expressions that combine the results of boolean comparisons, 
%% which we feel is typically expected of a constraint grammar
%% (e.g., ($.fruit == apples) + ($.color == red) + ($.kind == macintosh) > 2)
%% Furthermore, while we have no fundamental opposition to explicitly
%% stating that TRUE=1 and FALSE=0, we don't necessarily feel it's
%% necessary because section 12.3.1 of CORBA alread states that 
%% "Boolean values are encoded as single octets, where TRUE is the
%% value 1, and FALSE is 0." Essentially, we feel CORBA already 
%% defines TRUE to be 1 and FALSE to be 0, however we are not 
%% opposed to adding such a statement into Notification if folks 
%% feel it's necessary."
%% If still valid, see: ftp://ftp.omg.org/pub/docs/telecom/99-07-06.txt

%% The section they refer to (CORBA-2.0) merely states that TRUE and FALSE are
%% encoded as 1 and 0 in GIOP. Does not imply that booleans may be used as numeric.
%% But, they have stated that this should be the case so..... 

remap_bool(Num) when is_number(Num) -> Num;
remap_bool(true) -> 1;
remap_bool(false) -> 0;
remap_bool(X) -> throw({error, {bad_type, X}}).

eval_arith({Fun, X}, Env) ->
    Xe = remap_bool(eval(X, Env)),
    Fun(Xe);
eval_arith({Fun, X, Y}, Env) ->
    Xe = remap_bool(eval(X, Env)),
    Ye = remap_bool(eval(Y, Env)),
    Fun(Xe, Ye).

eval_rel({Fun, X, Y}, Env) ->
    Xe = eval(X, Env),
    Ye = eval(Y, Env),
    if 
	is_number(Xe) andalso is_number(Ye) ->
	    Fun(Xe, Ye);
	is_list(Xe) andalso is_list(Ye) ->
	    Fun(Xe, Ye);
	is_atom(Xe) andalso is_atom(Ye) ->
	    Fun(Xe, Ye);
	true ->
	    throw({error, {bad_type, X, Y}})
    end.

%%------------------------------------------------------------
%% function : get_variable
%% Arguments: A sequence of CosNotification::Property{}, i.e., 
%%            name-value pairs.
%%            ID - name in the Property
%%            Any - remainder of body
%% Returns  : Value in the Property | false
%% Comment  : When searching for a variable we must start with
%%            'variable_header' followed by 'filterable_body'.
%%            If not found we will then look in the 'remainder_of_body'
%%------------------------------------------------------------

get_variable([], ID, Any) when is_record(Any, any) ->
    case {any:get_value(Any), any:get_typecode(Any)} of
	{#'CosNotification_Property'{name=ID, value=A}, _} ->
	    any:get_value(A);
	{_, TC} when is_atom(TC) ->
	    %% Since TC atom it must be a simple type, which don't have members.
	    throw({error, {bad_id, ID}});
	{Value, {tk_alias,_,ID,_}} when is_record(Value, any) ->
	    %% {tk_alias, IFRId, ID, TypeCode}
	    any:get_value(Value);
	{Value, {tk_alias,_,ID,_}} ->
	    %% {tk_alias, IFRId, ID, TypeCode}
	    Value;
	{Value, _TC} ->
	    get_variable([],ID, Value)
    end;
get_variable([], ID, #'CosNotification_Property'{name=ID, value=Any}) ->
    any:get_value(Any);
get_variable([], ID, [#'CosNotification_Property'{name=ID, value=Any}|_]) ->
    any:get_value(Any);
get_variable([], ID, [H|T]) when is_record(H, 'CosNotification_Property') ->
    get_variable([], ID, T);
get_variable([], ID, false) ->
    throw({error, {bad_id, ID}});
get_variable([], ID, Value) ->
    M = element(1, Value),
    case catch M:tc() of
	{tk_struct,_,_,SList} ->
	    %% {tk_struct, Id, Name, ElementList}.
	    Field = get_field(ID, SList),
	    element(Field, Value);
	{tk_union,_,_,_, DefNo, UList} ->
	    %% {tk_union, Id, Name, DiscrTC, Default, ElementList}
	    case id2switch(UList, ID) of
		[default] when DefNo >= 0 ->
		    element(3, Value);
		[default] ->
		    throw({error, {bad_id, "Bad Union ID supplied"}});
		Found ->
		    case catch lists:member(element(2, Value), Found) of
			true ->
			    element(3, Value);
			_ ->
			    throw({error, {bad_id, "Bad Union ID supplied"}})
		    end
	    end;
	_->
	    throw({error, {bad_id, ID}})
    end;
get_variable([#'CosNotification_Property'{name=ID, value=A}|_], ID, _) ->
    any:get_value(A);
get_variable([_|T], ID, Any) ->
    get_variable(T, ID, Any).

%%------------------------------------------------------------
%% function : lookup
%% Arguments: T - A parse tree representing the grammar.
%%            S - The event we want to extract data from
%%            Op - which type of lookup should be done on this
%%                 component, e.g., 'default' or 'exist'.
%% Returns  : {ok, boolean()} |
%%            {error, _}
%% Comment  : WARNING!!!! 
%%            This function uses some Orber core information to 
%%            extract data, e.g., TypeCode representation. Why?
%%            We don't want to see the performance take a plunge
%%            due to that users write constraints which they
%%            can/may not know is slow. The alternative would be
%%            to use the IFR. However, these shortcuts aren't
%%            that frequent and we can easily update the code.
%%            To update, investigate:
%%            * lookup/3 cases related to '_type_id'
%%            * lookup/3 cases related to unions
%%            * get_variable/3
%%            * id2switch/2
%%            * switch2alias/2
%%------------------------------------------------------------
%% Done parsing, return the result.
lookup([],S,_) -> {ok, S};
lookup('$empty', #'CosNotification_StructuredEvent'{remainder_of_body = Any},_) -> 
    {ok, any:get_value(Any)};
lookup('$empty',S,_) when is_record(S, any) -> 
    {ok, any:get_value(S)};

%%------- varid --------
%% CosNotification-revision-98-11-01/46 states:
%% "The following rules govern translation of a run-time variable,   $variable , 
%% into a specific event field. If the run-time variable is reserved 
%% (e.g., $curtime) this translation takes precedence. Next, the first matching
%% translation is chosen respectively from: 
%% * a simple-typed member of $.header.fixed_header, 
%% * properties in $.header.variable_header, 
%% and properties in $.header.filterable_data.
%% If no match is found, the translation defaults to $.variable. 
%% Given these rules, an unstructured event with a $.priority member and a 
%% structured event using $.header.variable_header(priority) can be specified 
%% in a generic constraint using the run-time variable  $priority . 
%% Alternatively, a constraint can be written specifically for a structured or 
%% unstructured event by avoiding the use of run-time variables."

%% The above contains one error; $.header.filterable_data is not a part of the
%% header, but contained in the event body.

%% For any events we must first verify that a path exist, e.g.,
%% "header"->"fixed_header"->"event_type"->"domain_name", otherwise we will
%% use {dotid, "xxx"}.
lookup([{varid, "type_name"}|T], 
       #'CosNotification_StructuredEvent'
       {header = #'CosNotification_EventHeader'
	{fixed_header = #'CosNotification_FixedEventHeader'
	 {event_type =  #'CosNotification_EventType'{type_name=TN}}}}, Op) ->
    lookup(T, TN, Op);
lookup([{varid, "type_name"}|T], Any, Op) when is_record(Any, any) ->
    case locate_var([?TYPE_PATH, ?VARIABLE_PATH("type_name"), 
		     ?FILTERABLE_PATH("type_name")], Any, Op) of
	{ok, Val} ->
	    lookup(T, Val, Op);
	_ ->
	    lookup(T, get_variable([], "type_name", Any), Op)
    end;
lookup([{varid, "domain_name"}|T], 
       #'CosNotification_StructuredEvent'
       {header = #'CosNotification_EventHeader'
	{fixed_header = #'CosNotification_FixedEventHeader'
	 {event_type =  #'CosNotification_EventType'{domain_name=DN}}}}, Op) ->
    lookup(T, DN, Op);
lookup([{varid, "domain_name"}|T], Any, Op) when is_record(Any, any) ->
    case locate_var([?DOMAIN_PATH, ?VARIABLE_PATH("domain_name"), 
		     ?FILTERABLE_PATH("domain_name")], Any, Op) of
	{ok, Val} ->
	    lookup(T, Val, Op);
	_ ->
	    lookup(T, get_variable([], "domain_name", Any), Op)
    end;
lookup([{varid, "event_name"}|T], 
       #'CosNotification_StructuredEvent'
       {header = #'CosNotification_EventHeader'
	{fixed_header = #'CosNotification_FixedEventHeader'
	 {event_name =  EN}}}, Op) ->
    lookup(T, EN, Op);
lookup([{varid, "event_name"}|T], Any, Op) when is_record(Any, any) ->
    case locate_var([?EVENT_PATH, ?VARIABLE_PATH("event_name"), 
		     ?FILTERABLE_PATH("event_name")], Any, Op) of
	{ok, Val} ->
	    lookup(T, Val, Op);
	_ ->
	    lookup(T, get_variable([], "event_name", Any), Op)
    end;

lookup([{varid, ID}|T], 
	     #'CosNotification_StructuredEvent'{header =
		#'CosNotification_EventHeader'{variable_header = VS},
						filterable_data = FS,
						remainder_of_body = Any}, Op) ->
    lookup(T, get_variable(VS++FS, ID, Any), Op);
lookup([{varid, ID}|T], Any, Op) ->
    case locate_var([?VARIABLE_PATH(ID), ?FILTERABLE_PATH(ID)], Any, Op) of
	{ok, Val} ->
	    lookup(T, Val, Op);
	_ ->
	    lookup(T, get_variable([], ID, Any), Op)
    end;

%%------- dotid --------
%% First level
lookup([{dotid, "header"}|T], 
	     #'CosNotification_StructuredEvent'{header = S}, Op) ->
    lookup(T, S, Op);
lookup([{dotid, "filterable_data"}|T], 
	     #'CosNotification_StructuredEvent'{filterable_data = S}, Op) ->
    lookup(T, S, Op);

lookup([{dotid, remainder_of_body}|T], 
	     #'CosNotification_StructuredEvent'{remainder_of_body = S}, Op) ->
    lookup(T, S, Op);
%% Second level. Previous token must have been header
lookup([{dotid, "fixed_header"}|T], 
	     #'CosNotification_EventHeader'{fixed_header = S}, Op) ->
    lookup(T, S, Op);
lookup([{dotid, "variable_header"}|T], 
	     #'CosNotification_EventHeader'{variable_header = S}, Op) ->
    lookup(T, S, Op);
%% Third level. Previous token must have been fixed_header.
lookup([{dotid, "event_type"}|T], 
	     #'CosNotification_FixedEventHeader'{event_type = S}, Op) ->
    lookup(T, S, Op);
lookup([{dotid, "event_name"}|T], 
	     #'CosNotification_FixedEventHeader'{event_name = S}, Op) ->
    lookup(T, S, Op);
%% Fourth level. Previous token must have been event_type
lookup([{dotid, "domain_name"}|T], #'CosNotification_EventType'{domain_name = S}, Op) ->
    lookup(T, S, Op);
lookup([{dotid, "type_name"}|T], #'CosNotification_EventType'{type_name = S}, Op) ->
    lookup(T, S, Op);

%% Leaf expressions
lookup([{dotid, "name"}|T], #'CosNotification_Property'{name=S}, Op)  ->
    lookup(T, S, Op); 
lookup([{dotid, "value"}|T], #'CosNotification_Property'{value=S}, Op)  ->
    lookup(T, S, Op); 

lookup([{dotid, ID}|T], 
       #'CosNotification_StructuredEvent'{remainder_of_body = Any}, Op) ->
    lookup(T, get_variable([], ID, Any), Op);
lookup([{dotid, ID}|T], Any, Op) ->
    lookup(T, get_variable([], ID, Any), Op);

lookup([{associd, ID}|T], S, Op) when is_list(S) ->
    %% Refers to an associative array, i.e., a list of
    %% #'CosNotification_Property'{name=ID, value=A}
    lookup(T, get_variable(S, ID, false), Op);

lookup([{dotint, Position}|T], S, Op) when is_record(S, any) ->
    lookup(T, element(Position+2, any:get_value(S)), Op);
lookup([{dotint, Position}|T], S, Op) ->
    lookup(T, element(Position+2, S), Op);

lookup([{uint, ID}|T], S, Op) when is_record(S, any) ->
    lookup([{uint, ID}|T], any:get_value(S), Op);
lookup([{uint, ID} |T], S, Op) when is_tuple(S) ->
    case catch element(2, S) of
	ID ->
	    %% The supplied union do contain the requested discriminator.
	    lookup(T, element(3, S), Op);
	_Other when Op == exist_component ->
	    throw({error, {bad_id, "Bad Union ID"}});
	Other ->
	    %% Check if default is allowed.
	    M = element(1, S),
	    case catch M:tc() of
		{tk_union,_,_,_,DefNo, UList} ->
		    %% {tk_union, Id, Name, DiscrTC, Default, ElementList}
		    case switch2alias(UList, ID) of
			{ok, [], _} ->
			    throw({error, {bad_id, "Bad Union ID"}});
			{ok, default, _} when DefNo >= 0 ->
			    lookup(T, element(3, S), Op);
			{ok, List, _} ->
			    case lists:member(Other, List) of
				true ->
				    lookup(T, element(3, S), Op);
				_->
				    throw({error, {bad_id, "Bad Union ID"}})
			    end
		    end
	    end
    end;
lookup([{ustr, ID}|T], S, Op) when is_record(S, any) ->
    lookup([{ustr, ID}|T], any:get_value(S), Op);
lookup([{ustr, ID}|T], S, Op) when is_tuple(S) ->
    M = element(1, S),
    case catch M:tc() of
	{tk_union,_,_,_,DefNo, UList} ->
	    case id2switch(UList, ID) of
		[default] when DefNo >= 0 ->
		    lookup(T, element(3, S), Op);
		[default] ->
		    throw({error, {bad_id, "Bad Union ID supplied"}});
		Found ->
		    case catch lists:member(element(2, S), Found) of
			true ->
			    lookup(T, element(3, S), Op);
			_ ->
			    throw({error, {bad_id, "Bad Union ID supplied"}})
		    end
	    end
    end;
lookup([default|T], S, Op) when is_record(S, any) ->
    lookup([default|T], any:get_value(S), Op);
lookup([default|T], S, Op) when is_tuple(S) ->
    M = element(1, S),
    case catch M:tc() of
	{tk_union,_,_,_,DefNo, _UList} when DefNo < 0 ->
	    %% {tk_union, Id, Name, DiscrTC, Default, ElementList}
	    throw({error, {bad_id, "No default discriminator"}});
	{tk_union,_,_,_,_DefNo, UList} ->
	    %% {tk_union, Id, Name, DiscrTC, Default, ElementList}
	    %% Check if the label really is default.
	    case lists:keymember(element(2, S), 1, UList) of
		false ->
		    lookup(T, element(3, S), Op);
		_->
		    throw({error, {bad_id, "Bad Union"}})
	    end;
	_->
	    throw({error, {bad_id, "Bad Union"}})
    end;

lookup([{arrindex, Index}|T], S, Op) when is_tuple(S) ->
    %% The OMG uses c/c++ index. We must add one.
    lookup(T, element(Index+1,S), Op);

%%%%%%%%%%%%%%%%%%%%%%% LEAF EXPRESSIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% got '$._length', which maps to the 'remainder_of_body'
lookup(['_length'],
	     #'CosNotification_StructuredEvent'{remainder_of_body = Any}, _Op) ->
    {ok, length(any:get_value(Any))};
lookup(['_length'], S, _Op)  when is_record(S, any) ->
    {ok, length(any:get_value(S))};
lookup(['_length'], S, _Op)  when is_list(S) ->
    {ok, length(S)};
lookup(['_length'], S, _Op)  when is_tuple(S) ->
    {ok, length(tuple_to_list(S))};

%% got '$._d', which maps to the 'remainder_of_body'
%% The discriminator may, accordiong to the CORBA specification, be (2.3/p3-37):
%% * integer_type
%% * char_type
%% * boolean_type
%% * enum_type
%% * scoped_name
lookup(['_d'], 
       #'CosNotification_StructuredEvent'{remainder_of_body = Any}, 
       default_component) ->
    lookup(['_d'], any:get_value(Any), default_component);
lookup(['_d'], S, default_component) when is_record(S, any) ->
    lookup(['_d'], any:get_value(S), default_component);
lookup(['_d'], S, default_component) ->
    M = element(1, S),
    case catch M:tc() of
	{tk_union,_,_,_,DefNo,_} when DefNo < 0 -> 
	    %% '-1' indicates that no default value may exist.
	    {ok, false};
	{tk_union,_,_,_,_,UList} -> 
	    %% May be using the default setting; check if the Value is in the list.
	    {ok, not lists:keymember(element(2, S), 1, UList)};
	_ ->
	    {ok, false}
    end;
lookup(['_d'], 
       #'CosNotification_StructuredEvent'{remainder_of_body = Any}, _Op) ->
    {ok, element(2, any:get_value(Any))};
lookup(['_d'], S, _Op) when is_record(S, any) ->
    {ok, element(2, any:get_value(S))};
lookup(['_d'], S, _Op) ->
    {ok, element(2, S)};


lookup(['_type_id'], S, _Op) when is_record(S,'CosNotification_StructuredEvent') ->
    {ok, "StructuredEvent"};
lookup(['_type_id'], S, _Op) when is_record(S,'CosNotification_EventHeader') ->
    {ok, "EventHeader"};
lookup(['_type_id'], S, _Op) when is_record(S,'CosNotification_FixedEventHeader') ->
    {ok, "FixedEventHeader"};
lookup(['_type_id'], S, _Op) when is_record(S,'CosNotification_EventType') ->
    {ok, "EventType"};
lookup(['_type_id'], S, _Op) when is_record(S,'CosNotification_Property') ->
    {ok, "Property"};
lookup(['_type_id'], S, _Op) when is_tuple(S) ->
    M=element(1, S),
    Name = case catch M:tc() of
	       {tk_union,_,ID,_,_,_} ->
		   ID;
	       {tk_enum, _, ID, _} ->
		   ID;
	       {tk_exception, _, ID, _} ->
		   ID;
	       {tk_alias, _, ID, _} ->
		   ID;
	       {tk_struct,_,ID,_} ->
		   ID
	   end,
    {ok, Name};

lookup(['_repos_id'], S, _Op) when is_record(S,'CosNotification_StructuredEvent') ->
    {ok, 'CosNotification_StructuredEvent':id()};
lookup(['_repos_id'], S, _Op) when is_record(S,'CosNotification_EventHeader') ->
    {ok, 'CosNotification_EventHeader':id()};
lookup(['_repos_id'], S, _Op) when is_record(S,'CosNotification_FixedEventHeader') ->
    {ok, 'CosNotification_FixedEventHeader':id()};
lookup(['_repos_id'], S, _Op) when is_record(S,'CosNotification_EventType') ->
    {ok, 'CosNotification_EventType':id()};
lookup(['_repos_id'], S, _Op) when is_record(S,'CosNotification_Property') ->
    {ok, 'CosNotification_Property':id()};
lookup(['_repos_id'], S, _Op) when is_tuple(S) ->
    M = element(1, S),
    {ok, M:id()};

lookup(_, _, _) ->
    error.


%%------------------------------------------------------------
%% function : locate_var
%% Arguments: Paths - A list of path-lists which tells us where
%%                    to search for runtime variables and in which
%%                    order.
%%            S - Data
%%            Op - se lookup/3
%% Returns  : {error, _} |
%%            {ok, Val}
%%------------------------------------------------------------
locate_var([], _S, _) ->
    {error, "not found"};
locate_var([H|T], S, Op) ->
    case catch lookup(H, S, Op) of
	{ok, Val} ->
	    {ok,Val};
	_ ->
	    locate_var(T, S, Op)
    end.

%%------------------------------------------------------------
%% function : id2switch
%% Arguments: UList - The list of elements contained in the 
%%                    Union TypeCode.
%%            ID  - string() eq name of element.
%% Returns  : Acc - A list of switches related to given ID.
%%------------------------------------------------------------
id2switch(UList, ID) ->
    id2switch(UList, ID, [], false).
id2switch([], _, Acc, _) ->
    Acc;
id2switch([{Sw, ID, _}|T], ID, Acc, _) ->
    id2switch(T, ID, [Sw|Acc], true);
id2switch([_|_T], _ID, Acc, true) ->
    Acc;
id2switch([_|T], ID, Acc, Found) ->
    id2switch(T, ID, Acc, Found).

%%------------------------------------------------------------
%% function : switch2alias
%% Arguments: UList - The list of elements contained in the 
%%                    Union TypeCode.
%%            Switch - the union discriminator.
%% Returns  : Acc - A list of switches that are defined with the same 
%%            ID  - The switches common ID.
%% Comment  : A union IDL code can look like:
%%            union Union switch(long) {
%%              case 1:
%%              case 2: long ID; };
%%            In this case supplying Switch == 1 (or) the result
%%            should be {ok, [1,2], "ID"}
%%------------------------------------------------------------
switch2alias([], _Switch) ->
    %% Is it really possible to define an empty union??
    {ok, [], undefined};
switch2alias([{Sw, ID, TC}|UList], Switch) ->
    switch2alias([{Sw, ID, TC}|UList], Switch, [], ID, false).


switch2alias([{default, ID, _}], _, _, _, false) ->
    {ok, default, ID};
switch2alias([], _, _Acc, _, false) ->
    {ok, [], undefined};
switch2alias([], _, Acc, PreviousID, _) ->
    {ok, Acc, PreviousID};

%% Seen the ID before but just found the correct switch, e.g.,
%% [... {0,"K",{tk_string,0}}, {2,"K",{tk_string,0}}...] and switch eq '2'
switch2alias([{Switch, PreviousID, _}|T], Switch, Acc, PreviousID, _Found) ->
    switch2alias(T, Switch, [Switch|Acc], PreviousID, true);

%% First time for this ID and found the correct switch
switch2alias([{Switch, ID, _}|T], Switch, _Acc, _PreviousID, false) ->
    switch2alias(T, Switch, [Switch], ID, true);

%% Seen this ID and found the correct switch before.
switch2alias([{Sw, PreviousID, _}|T], Switch, Acc, PreviousID, true) ->
    switch2alias(T, Switch, [Sw|Acc], PreviousID, true);

%% Seen this ID but not found the correct switch.
switch2alias([{Sw, PreviousID, _}|T], Switch, Acc, PreviousID, false) ->
    switch2alias(T, Switch, [Sw|Acc], PreviousID, false);

%% No more of the correct ID/Switch. Done.
switch2alias([{_, _ID, _}|_], _, Acc, PreviousID, true) ->
    {ok, Acc, PreviousID};
%% Not found correct switch and ID is updated.
switch2alias([{Sw, ID, _}|T], Switch, _Acc, _PreviousID, Found) ->
    switch2alias(T, Switch, [Sw], ID, Found).


%%------------------------------------------------------------
%% function : get_field
%% Arguments: ID - element name
%%            List - The list of elements contained in the 
%%                    TypeCode.
%% Returns  : false |
%%            offset
%%------------------------------------------------------------
get_field(ID, List) ->
    get_field(ID, List, 2).
get_field(_ID, [], _) ->
    false;
get_field(ID, [ID|_], I) ->
    %% Memberlists in enum.
    I;
get_field(ID, [{ID,_}|_], I) ->
    %% Memberlists in structs.
    I;
get_field(ID, [_|T], I) ->
    get_field(ID, T, I+1).

%%------------------------------------------------------------
%% function : check_types
%% Arguments: A sequence of CosNotification::EventType{}, i.e., 
%%            name-value pairs.
%% Returns  : {ok, WhichType, WC}
%%            WhichType - type/domain/both
%%            WC - [Types using wildcard]
%%------------------------------------------------------------
%% With check_types we try to determin if one or more EventTypes force us to check
%% all events against this constraint. For example:
%% EventType A1 has domain_name="car",type_name = "*"
%% EventType A2 has domain_name="*",type_name = "DodgeViper"
%% Since A1 says that we must test against any type_name and A2 
%% against any domain_name, we must test all events using these permutations.
%% It's better to do these test now instead of when we are up and running. But
%% if a client change the constraints VERY often it's up to them and they have
%% to accept the delay.
%%------------------------------------------------------------

%% If types is an empty list it means that this constraint must be used 
%% for all events.
check_types([]) -> true;
check_types(Types) -> check_types(Types, both, []).
check_types([], Which, WildCard) -> {ok, Which, WildCard};
%% The following cases means that all events matches.
check_types([#'CosNotification_EventType'{domain_name="",type_name = ""}|_T],_,_) ->
    true;
check_types([#'CosNotification_EventType'{domain_name="",type_name = "*"}|_T],_,_) ->
    true;
check_types([#'CosNotification_EventType'{domain_name="*",type_name = ""}|_T],_,_) ->
    true;
check_types([#'CosNotification_EventType'{domain_name="*",type_name = "*"}|_T],_,_) ->
    true;
%% The following cases means that all events must be tested using this constraint.
check_types([#'CosNotification_EventType'{domain_name="",type_name = Ty}|T], domain,WC) when is_list(Ty) ->
    check_wildcard(T, all, WC, "", Ty);
check_types([#'CosNotification_EventType'{domain_name="*",type_name = Ty}|T], domain, WC) when is_list(Ty) ->
    check_wildcard(T, all, WC, "", Ty);
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = ""}|T], type,WC) when is_list(Do) ->
    check_wildcard(T, all, WC, Do, "");
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = "*"}|T], type,WC) when is_list(Do) ->
    check_wildcard(T, all, WC, Do, "");
%% The following cases is used to prevent other cases from converting,
%% for example, all->type.
check_types([#'CosNotification_EventType'{domain_name="",type_name = Ty}|T], all,WC) when is_list(Ty) ->
    check_wildcard(T, all, WC, "", Ty);
check_types([#'CosNotification_EventType'{domain_name="*",type_name = Ty}|T], all,WC) when is_list(Ty) ->
    check_wildcard(T, all, WC, "", Ty);
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = ""}|T], all,WC) when is_list(Do) ->
    check_wildcard(T, all, WC, Do, "");
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = "*"}|T], all,WC) when is_list(Do) ->
    check_wildcard(T, all, WC, Do, "");
%% The following cases means that all events with matching Type must be 
%% tested using this constraint.
check_types([#'CosNotification_EventType'{domain_name="",type_name = Ty}|T], _W,WC) when is_list(Ty) ->
    check_wildcard(T, type, WC, "", Ty);
check_types([#'CosNotification_EventType'{domain_name="*",type_name = Ty}|T], _W,WC) when is_list(Ty) ->
    check_wildcard(T, type, WC, "", Ty);
%% The following cases means that all events with matching Domain must be 
%% tested using this constraint.
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = ""}|T], _W,WC) when is_list(Do) ->
    check_wildcard(T, domain, WC, Do, "");
check_types([#'CosNotification_EventType'{domain_name=Do,type_name = "*"}|T], _W,WC) when is_list(Do) ->
    check_wildcard(T, domain, WC, Do, "");
%% Sorry, no shortcuts.
check_types([#'CosNotification_EventType'{domain_name=Do,type_name=Ty}|T], W,WC) when is_list(Do) andalso is_list(Ty) ->
    check_wildcard(T, W, WC, Do, Ty);
check_types([H|_], _,_) when is_record(H, 'CosNotification_EventType') ->
    %% Not valid.
    corba:raise(#'CosNotifyComm_InvalidEventType'{type=H});
check_types(_,_,_) ->
    %% Wasn't even a correct input.
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

check_wildcard(Types, Which, WC, Domain, Type) ->
    NewWC =
	case {string:chr(Domain, $*), string:chr(Type, $*)} of
	    {0, 0} ->
		WC;
	    {0, _}->
		[{type, Domain, convert_wildcard(Type, [])}|WC];
	    {_, 0}->
		[{domain, convert_wildcard(Domain, []), Type}|WC];
	    _->
		[{both, convert_wildcard(Domain, []), convert_wildcard(Type, [])}|WC]
	end,
    check_types(Types, Which, NewWC).

%% Change '*' to '.*', see re:compile/1 documentation.
convert_wildcard([], Acc) ->
    case re:compile(lists:reverse(Acc)) of
	{ok, Expr} ->
	    Expr;
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
convert_wildcard([$*|T], Acc) ->
    convert_wildcard(T, [$*, $.|Acc]);
convert_wildcard([H|T], Acc) ->
    convert_wildcard(T, [H|Acc]).
    
%%------------------------------------------------------------
%% function : match_types
%% Arguments: A sequence of {Which, Domain, Type}, i.e., the same as
%%            returned from cosNotification_Filter:check_types/3
%% Returns  : bolean()
%%------------------------------------------------------------
match_types(_, _, []) ->
    false;
match_types(Domain, Type, [{domain, WCDomain, Type}|T]) ->
    L=length(Domain),
    case catch re:run(Domain, WCDomain) of
	nomatch ->
	    match_types(Domain, Type, T);
	{match, [{0, L}]} ->
	    true;
	_->
	    match_types(Domain, Type, T)
    end;
match_types(Domain, Type, [{type, Domain, WCType}|T]) ->
    L=length(Type),
    case catch re:run(Type, WCType) of
	nomatch ->
	    match_types(Domain, Type, T);
	{match, [{0, L}]} ->
	    true;
	_->
	    match_types(Domain, Type, T)
    end;
match_types(Domain, Type, [{both, WCDomain, WCType}|T]) ->
    L1=length(Domain),
    case catch re:run(Domain, WCDomain) of
	nomatch ->
	    match_types(Domain, Type, T);
	{match, [{0, L1}]} ->
	    L2=length(Type),
	    case catch re:run(Type, WCType) of
		nomatch ->
		    match_types(Domain, Type, T);
		{match, [{0, L2}]} ->
		    true;
		_ ->
		    match_types(Domain, Type, T)
	    end;
	_->
	    match_types(Domain, Type, T)
    end;
match_types(Domain, Type, [_|T]) ->
    match_types(Domain, Type, T).

%%------------------------------------------------------------
%% function : validate_types
%% Arguments: A sequence of CosNotification::EventType{}, i.e., 
%%            name-value pairs.
%% Returns  : ok |
%%            {'EXCEPTION', #'CosNotifyComm_InvalidEventType'{}}
%%------------------------------------------------------------

validate_types([]) ->
    ok;
validate_types([#'CosNotification_EventType'{domain_name=Do,type_name=Ty}|T]) 
  when is_list(Do) andalso is_list(Ty) ->
    validate_types(T);
validate_types([H|_]) 
  when is_record(H, 'CosNotification_EventType') ->
    %% Not valid.
    corba:raise(#'CosNotifyComm_InvalidEventType'{type=H});
validate_types(_) ->
    %% Wasn't even a correct input.
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%--------------- END OF MODULE ------------------------------
