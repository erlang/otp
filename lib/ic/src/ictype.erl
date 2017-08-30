%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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
-module(ictype).


-include("ic.hrl").
-include("icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([type_check/2, scoped_lookup/4, maybe_array/5, to_uppercase/1]).

-export([name2type/2, member2type/3, isBasicTypeOrEterm/3, isEterm/3]).
-export([isBasicType/1, isBasicType/2, isBasicType/3, isString/3, isWString/3, 
	 isArray/3, isStruct/3, isUnion/3, isEnum/3, isSequence/3, isBoolean/3 ]).
-export([fetchTk/3, fetchType/1, tk/4]).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
%%-define(DBG(F,A), io:format(F,A)).
-define(DBG(F,A), true).
-define(STDDBG, ?DBG("    dbg: ~p: ~p~n", [element(1,X), ic_forms:get_id2(X)])).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

type_check(G, Forms) ->
    S = ic_genobj:tktab(G),
    check_list(G, S, [], Forms).

scoped_lookup(G, S, N, X) ->
    Id = ic_symtab:scoped_id_strip(X),
    case ic_symtab:scoped_id_is_global(X) of
	true ->
	    lookup(G, S, [], X, Id);
	false ->
	    lookup(G, S, N, X, Id)
    end.


%%--------------------------------------------------------------------
%% maybe_array
%%
%%	Array declarators are indicated on the declarator and not on
%%	the type, therefore the declarator decides if the array type
%%	kind is added or not.
%%
maybe_array(G, S, N, X, TK) when is_record(X, array) ->
    mk_array(G, S, N, X#array.size, TK);
maybe_array(_G, _S, _N, _, TK) -> TK.



name2type(G, Name) ->
    S = ic_genobj:tktab(G),
    ScopedName = lists:reverse(string:tokens(Name, "_")),
    InfoList = ets:lookup(S, ScopedName ),
    filter( InfoList ).


%% This is en overloaded function,
%% differs in input on unions
member2type(_G, X, I) when is_record(X, union)->
    Name = ic_forms:get_id2(I),
    case lists:keysearch(Name,2,element(6,X#union.tk)) of
	false ->
	    error;
	{value,Rec} ->
	    fetchType(element(3,Rec))
    end;
member2type( G, SName, MName ) ->

    S = ic_genobj:tktab( G ),
    SNList = lists:reverse(string:tokens(SName,"_")),
    ScopedName = [MName | SNList],
    InfoList = ets:lookup( S, ScopedName ),

    case filter( InfoList ) of
	error ->
	    %% Try a little harder, seeking inside tktab
	    case lookup_member_type_in_tktab(S, ScopedName, MName) of
		error ->
		    %% Check if this is the "return to return1" case
		    case MName of
			"return1" ->
			    %% Do it all over again !
			    ScopedName2 = ["return" | SNList],
			    InfoList2 = ets:lookup( S, ScopedName2 ),
			    case filter( InfoList2 ) of
				error ->
				    %% Last resort: seek in pragma table
				    lookup_type_in_pragmatab(G, SName);

				Other ->
				    Other
			    end;		
			_ ->
			    %% Last resort: seek in pragma table
			    lookup_type_in_pragmatab(G, SName)
		    end;
		Other ->
		    Other
	    end;
	Other ->
	    Other
    end.


lookup_member_type_in_tktab(S, ScopedName, MName) ->
    case ets:match_object(S, {'_',member,{MName,'_'},nil}) of
	[] ->
	    error;
	[{_FullScopedName,member,{MName,TKInfo},nil}]->
	    fetchType( TKInfo );
	List ->
	    lookup_member_type_in_tktab(List,ScopedName)
    end.

lookup_member_type_in_tktab([], _ScopedName) ->
    error;
lookup_member_type_in_tktab([{FullScopedName,_,{_,TKInfo},_}|Rest],ScopedName) ->
    case lists:reverse(string:tokens(ic_util:to_undersc(FullScopedName),"_")) of
	ScopedName ->
	    fetchType(TKInfo);
	_ ->
	    lookup_member_type_in_tktab(Rest,ScopedName)
    end.


lookup_type_in_pragmatab(G, SName) ->    
    S = ic_genobj:pragmatab(G),

    %% Look locally first
    case ets:match(S,{file_data_local,'_','_','$2','_','_',SName,'_','_'}) of 
	[] ->
	    %% No match, seek included
	    case ets:match(S,{file_data_included,'_','_','$2','_','_',SName,'_','_'}) of 

		[] ->
		    error;
		[[Type]] ->
		    io:format("1 Found(~p) : ~p~n",[SName,Type]),
		    Type
	    end;

	[[Type]] ->
	    io:format("2 Found(~p) : ~p~n",[SName,Type]),
	    Type
    end.




isString(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, {'tk_string',_}, _} ->
	    true;
	_ ->
	    false
    end; 
isString(_G, _N, T)  when is_record(T, string) ->
    true;
isString(_G, _N, _Other) ->
    false. 


isWString(G, N, T) when element(1, T) == scoped_id ->  %% WSTRING
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, {'tk_wstring',_}, _} ->
	    true;
	_ ->
	    false
    end; 
isWString(_G, _N, T)  when is_record(T, wstring) ->
    true;
isWString(_G, _N, _Other) ->
    false. 


isArray(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, {'tk_array', _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isArray(_G, _N, T)  when is_record(T, array) ->
    true;
isArray(_G, _N, _Other) ->
    false. 


isSequence(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, {'tk_sequence', _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isSequence(_G, _N, T)  when is_record(T, sequence) ->
    true;
isSequence(_G, _N, _Other) ->
    false. 


isStruct(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
	{_FullScopedName, _, {'tk_struct', _, _, _}, _} ->
	    true;
	_ ->
	    false
    end; 
isStruct(_G, _N, T)  when is_record(T, struct) ->
    true;
isStruct(_G, _N, _Other) ->
    false.


isUnion(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
        {_FullScopedName, _, {'tk_union', _, _, _,_,_}, _} ->
            true;
        _Other ->
            false
    end; 
isUnion(_G, _N, T)  when is_record(T, union) ->
    true;
isUnion(_G, _N, _Other) ->
    false.



isEnum(G, N, T) when element(1, T) == scoped_id ->
    case ic_symtab:get_full_scoped_name(G, N, T) of
        {_FullScopedName, _, {'tk_enum',_,_,_}, _} ->
            true;
        _Other ->
            false
    end; 
isEnum(_G, _N, T)  when is_record(T, enum) ->
    true;
isEnum(_G, _N, _Other) ->
    false.



isBoolean(G, N, T) when element(1, T) == scoped_id ->
    {_, _, TK, _} =
	ic_symtab:get_full_scoped_name(G, N, T),
    case fetchType(TK) of
	'boolean' ->
	    true;
	_ ->
	    false
    end;
isBoolean(_, _, {'tk_boolean',_}) ->
    true;
isBoolean(_, _, {'boolean',_}) ->
    true;
isBoolean(_, _, _) ->
    false.


%%%  Just used for C

isBasicTypeOrEterm(G, N, S) ->
    case isBasicType(G, N, S) of
	true ->
	    true;
	false ->
	    isEterm(G, N, S)
    end.

isEterm(G, N, S) when element(1, S) == scoped_id -> 
    {FullScopedName, _, _TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    case ic_code:get_basetype(G, ic_util:to_undersc(FullScopedName)) of
	"erlang_term" ->
	    true;
	"ETERM*" ->
	    true;
	_X ->
	    false
    end;
isEterm(_G, _Ni, _X) -> 
    false.

isBasicType(_G, _N,  {scoped_id,_,_,["term","erlang"]}) ->
    false;
isBasicType(G, N, S) when element(1, S) == scoped_id ->
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isBasicType(fetchType(TK));
isBasicType(_G, _N, {string, _} ) -> 
    false;
isBasicType(_G, _N, {wstring, _} ) ->  %% WSTRING 
    false;
isBasicType(_G, _N, {unsigned, {long, _}} ) -> 
    true;
isBasicType(_G, _N, {unsigned, {short, _}} ) -> 
    true;
isBasicType(_G, _N, {Type, _} ) ->
    isBasicType(Type);
isBasicType(_G, _N, _X) ->
    false.


isBasicType( G, Name ) ->
    isBasicType( name2type( G, Name ) ).


isBasicType( Type ) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_longlong,longlong,  %% LLONG
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_ulonglong,ulonglong,  %% ULLONG
		  tk_float,float,
		  tk_double,double,
		  tk_boolean,boolean,
		  tk_char,char,
		  tk_wchar,wchar,  %% WCHAR
		  tk_octet,octet,
		  tk_any,any]).    %% Fix for any



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
check(G, _S, N, X) when is_record(X, preproc) ->
    handle_preproc(G, N, X#preproc.cat, X),
    X;

check(G, S, N, X) when is_record(X, op) ->
    ?STDDBG,
    TK = tk_base(G, S, N, ic_forms:get_type(X)),
    tktab_add(G, S, N, X),
    N2 = [ic_forms:get_id2(X) | N],
    Ps = lists:map(fun(P) -> 
			   tktab_add(G, S, N2, P),
			   P#param{tk=tk_base(G, S, N, ic_forms:get_type(P))} end,
		   X#op.params),
    %% Check for exception defs.
    Raises = lists:map(fun(E) -> name_lookup(G, S, N, E) end,
		       X#op.raises),
    case ic_forms:is_oneway(X) of
	true ->
	    if  TK /= tk_void ->
		    ic_error:error(G, {bad_oneway_type, X, TK});
		true -> ok
	    end,
	    case ic:filter_params([inout, out], X#op.params) of
		[] -> ok;			% No out parameters!
		_ ->
		    ic_error:error(G, {oneway_outparams, X})
	    end,
	    case X#op.raises of
		[] -> ok;
		_ ->
		    ic_error:error(G, {oneway_raises, X})
	    end;
	false -> 
	    ok
    end,
    X#op{params=Ps, tk=TK, raises=Raises};

check(G, S, N, X) when is_record(X, interface) ->
    ?STDDBG,
    N2 = [ic_forms:get_id2(X) | N],
    TK = {tk_objref, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X)},
    Inherit = inherit_resolve(G, S, N, X#interface.inherit, []),
    tktab_add(G, S, N, X, TK, Inherit),
    CheckedBody = check_list(G, S, N2, ic_forms:get_body(X)),
    InhBody = calc_inherit_body(G, N2, CheckedBody, Inherit, []),
    X2 = X#interface{inherit=Inherit, tk=TK, body=CheckedBody,
		     inherit_body=InhBody},
    ic_symtab:store(G, N, X2),
    X2;

check(G, S, N, X) when is_record(X, forward) ->
    ?STDDBG,
    tktab_add(G, S, N, X, {tk_objref, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X)}),
    X;

check(G, S, N, #constr_forward{tk = tk_struct} = X) ->
    ?STDDBG,
    ID = ic_forms:get_id2(X),
    Module = list_to_atom(string:join(lists:reverse([ID|N]), "_")),
    tktab_add(G, S, N, X, {tk_struct, ictk:get_IR_ID(G, N, X), ID, Module}),
    X;
check(G, S, N, #constr_forward{tk = tk_union} = X) ->
    ?STDDBG,
    ID = ic_forms:get_id2(X),
    Module = list_to_atom(string:join(lists:reverse([ID|N]), "_")),
    tktab_add(G, S, N, X, {tk_union, ictk:get_IR_ID(G, N, X), ID, [], [], Module}),
    X;

check(G, S, N, X) when is_record(X, const) ->
    ?STDDBG,
    case tk_base(G, S, N, ic_forms:get_type(X)) of
	Err when element(1, Err) == error -> X;
	TK ->
	    check_const_tk(G, S, N, X, TK),
	    case iceval:eval_const(G, S, N, TK, X#const.val) of
		Err when element(1, Err) == error -> X;
		{ok, NewTK, Val} ->
		    V = iceval:get_val(Val),
		    tktab_add(G, S, N, X, NewTK, V),
		    X#const{val=V, tk=NewTK};
		Val ->
		    V = iceval:get_val(Val),
		    tktab_add(G, S, N, X, TK, V),
		    X#const{val=V, tk=TK}
	    end
    end;

check(G, S, N, X) when is_record(X, except) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#except{tk=TK};

check(G, S, N, X) when is_record(X, struct) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#struct{tk=TK};

check(G, S, N, X) when is_record(X, enum) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#enum{tk=TK};

check(G, S, N, X) when is_record(X, union) ->
    ?STDDBG,
    TK = tk(G, S, N, X),
    X#union{tk=TK};

check(G, S, N, X) when is_record(X, attr) ->
    ?STDDBG,
    TK = tk_base(G, S, N, ic_forms:get_type(X)),
    XX = #id_of{type=X},
    lists:foreach(fun(Id) -> tktab_add(G, S, N, XX#id_of{id=Id}) end,
		  ic_forms:get_idlist(X)),
    X#attr{tk=TK};

check(G, S, N, X) when is_record(X, module) -> 
    ?STDDBG,
    tktab_add(G, S, N, X),
    X#module{body=check_list(G, S, [ic_forms:get_id2(X) | N], ic_forms:get_body(X))};

check(G, S, N, X) when is_record(X, typedef) ->
    ?STDDBG,
    TKbase = tk(G, S, N, X),
    X#typedef{tk=TKbase};

check(_G, _S, _N, X) ->
    ?DBG("    dbg: ~p~n", [element(1,X)]),
    X.

handle_preproc(G, _N, line_nr, X) -> ic_genobj:set_idlfile(G, ic_forms:get_id2(X));
handle_preproc(_G, _N, _C, _X) -> ok.


%%--------------------------------------------------------------------
%%
%% TK calculation
%%
%%--------------------------------------------------------------------

tk(G, S, N, X) when is_record(X, union) ->
    N2 = [ic_forms:get_id2(X) | N],
    DisrcTK = tk(G, S, N, ic_forms:get_type(X)),
    case check_switch_tk(G, S, N, X, DisrcTK) of
	true ->
	    do_special_enum(G, S, N2, ic_forms:get_type(X)),
	    BodyTK = lists:reverse(
		       tk_caselist(G, S, N2, DisrcTK, ic_forms:get_body(X))),
	    tktab_add(G, S, N, X, 
		      {tk_union, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X),
		       DisrcTK, default_count(ic_forms:get_body(X)), BodyTK});
	false ->
	    tk_void
    end;

tk(G, S, N, X) when is_record(X, enum) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X,
	      {tk_enum, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
	       enum_body(G, S, N2, ic_forms:get_body(X))});


%% Note that the TK returned from this function is the base TK. It
%% must be modified for each of the identifiers in the idlist (for
%% array reasons).
tk(G, S, N, X) when is_record(X, typedef) ->
    case X of
	%% Special case only for term and java backend !
	{typedef,{any,_},[{'<identifier>',_,"term"}],undefined} ->
	    case ic_options:get_opt(G, be) of
		java ->
		    tktab_add(G, S, N, X, tk_term), 
		    tk_term;
		_ ->
		    TK = tk(G, S, N, ic_forms:get_body(X)),
		    lists:foreach(fun(Id) ->
					  tktab_add(G, S, N, #id_of{id=Id, type=X}, 
						    maybe_array(G, S, N, Id, TK))
				  end,
				  X#typedef.id),
		    TK
	    end;
	_ ->
	    TK = tk(G, S, N, ic_forms:get_body(X)),
	    lists:foreach(fun(Id) ->
				  tktab_add(G, S, N, #id_of{id=Id, type=X}, 
					    maybe_array(G, S, N, Id, TK))
			  end,
			  X#typedef.id),
	    TK
    end;

tk(G, S, N, X) when is_record(X, struct) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X, {tk_struct, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
			   tk_memberlist(G, S, N2, ic_forms:get_body(X))});

tk(G, S, N, X) when is_record(X, except) ->
    N2 = [ic_forms:get_id2(X) | N],
    tktab_add(G, S, N, X, {tk_except, ictk:get_IR_ID(G, N, X), ic_forms:get_id2(X), 
			   tk_memberlist(G, S, N2, ic_forms:get_body(X))});

tk(G, S, N, X) -> tk_base(G, S, N, X).


tk_base(G, S, N, X) when is_record(X, sequence) ->
    {tk_sequence, tk(G, S, N, X#sequence.type), 
     len_eval(G, S, N, X#sequence.length)};

tk_base(G, S, N, X) when is_record(X, string) ->
    {tk_string, len_eval(G, S, N, X#string.length)};

tk_base(G, S, N, X) when is_record(X, wstring) ->  %% WSTRING
    {tk_wstring, len_eval(G, S, N, X#wstring.length)};

%% Fixed constants can be declared as:
%% (1)  const fixed pi = 3.14D; or
%% (2)  typedef fixed<3,2> f32;
%%      const f32 pi = 3.14D;
tk_base(G, S, N, X) when is_record(X, fixed) -> 
    %% Case 2
    {tk_fixed, len_eval(G, S, N, X#fixed.digits), len_eval(G, S, N, X#fixed.scale)};
tk_base(_G, _S, _N, {fixed, _}) -> 
    %% Case 1
    tk_fixed;


%% Special case, here CORBA::TypeCode is built in 
%% ONLY when erl_corba is the backend of choice 
tk_base(G, S, N, {scoped_id,V1,V2,["TypeCode","CORBA"]}) ->
    case ic_options:get_opt(G, be) of
	false ->
	    tk_TypeCode;
	erl_corba ->
	    tk_TypeCode;
	erl_template ->
	    tk_TypeCode;
	_ ->
	    case scoped_lookup(G, S, N, {scoped_id,V1,V2,["TypeCode","CORBA"]}) of
		T when element(1, T) == error -> T;
		T when is_tuple(T) -> element(3, T)
	    end 
    end;

tk_base(G, S, N, X) when element(1, X) == scoped_id ->
    case scoped_lookup(G, S, N, X) of
	T when element(1, T) == error -> T;
	T when is_tuple(T) -> element(3, T)
    end;
tk_base(_G, _S, _N, {long, _})			-> tk_long;
tk_base(_G, _S, _N, {'long long', _})		-> tk_longlong;  %% LLONG
tk_base(_G, _S, _N, {short, _})			-> tk_short;
tk_base(_G, _S, _N, {'unsigned', {short, _}})	-> tk_ushort;
tk_base(_G, _S, _N, {'unsigned', {long, _}})	-> tk_ulong;
tk_base(_G, _S, _N, {'unsigned', {'long long', _}})-> tk_ulonglong;  %% ULLONG
tk_base(_G, _S, _N, {float, _})			-> tk_float;
tk_base(_G, _S, _N, {double, _})		-> tk_double;
tk_base(_G, _S, _N, {boolean, _})		-> tk_boolean;
tk_base(_G, _S, _N, {char, _})			-> tk_char;
tk_base(_G, _S, _N, {wchar, _})			-> tk_wchar;  %% WCHAR
tk_base(_G, _S, _N, {octet, _})			-> tk_octet;
tk_base(_G, _S, _N, {null, _})			-> tk_null;
tk_base(_G, _S, _N, {void, _})			-> tk_void;
tk_base(_G, _S, _N, {any, _})			-> tk_any;
tk_base(_G, _S, _N, {'Object', _})		-> {tk_objref, "", "Object"}.


%%--------------------------------------------------------------------
%%
%% Special handling of idlists. Note that the recursion case is given
%% as accumulator to foldr. Idlists are those lists of identifiers
%% that share the same definition, i.e. multiple cases, multiple type
%% declarations, multiple member names.
%%
tk_memberlist(G, S, N, [X | Xs]) ->
    BaseTK = tk(G, S, N, ic_forms:get_type(X)),

    XX = #id_of{type=X},
    lists:foldr(fun(Id, Acc) ->
			[tk_member(G, S, N, XX#id_of{id=Id}, BaseTK) | Acc] end, 
		tk_memberlist(G, S, N, Xs), 
		ic_forms:get_idlist(X));
tk_memberlist(_G, _S, _N, []) -> [].

%% same as above but for case dcls
tk_caselist(G, S, N, DiscrTK, Xs) ->
    lists:foldl(fun(Case, Acc) ->
			BaseTK = tk(G, S, N, ic_forms:get_type(Case)),
			%% tktab_add for the uniqueness check of the declarator
			tktab_add(G, S, N, Case),
			lists:foldl(fun(Id, Acc2) ->
					    case tk_case(G, S, N, Case, BaseTK,
							 DiscrTK, Id) of
						Err when element(1, Err)==error ->
						    Acc2;
						TK ->
						    unique_add_case_label(G, S, N, Id, 
									  TK, Acc2)
					    end
				    end, 
				    Acc,
				    ic_forms:get_idlist(Case))
		end,
		[],
		Xs).


%% Handling of the things that can be in an idlist or caselist
tk_member(G, S, N, X, BaseTK) ->
    tktab_add(G, S, N, X, 
	      {ic_forms:get_id2(X), maybe_array(G, S, N, X#id_of.id, BaseTK)}).


get_case_id_and_check(G, _S, _N, _X, ScopedId) ->
    case ic_symtab:scoped_id_is_global(ScopedId) of
	true -> ic_error:error(G, {bad_scope_enum_case, ScopedId});
	false -> ok
    end,
    case ic_symtab:scoped_id_strip(ScopedId) of
	[Id] -> Id;
	_List -> 
	    ic_error:error(G, {bad_scope_enum_case, ScopedId}), 
	    ""
    end.


tk_case(G, S, N, X, BaseTK, DiscrTK, Id) ->
    case case_eval(G, S, N, DiscrTK, Id) of
	Err when element(1, Err) == error -> Err;
	Val -> 
	    case iceval:check_tk(G, DiscrTK, Val) of
		true -> 
		    {iceval:get_val(Val), ic_forms:get_id2(X),
		     maybe_array(G, S, N, X#case_dcl.id, BaseTK)};
		false ->
		    ic_error:error(G, {bad_case_type, DiscrTK, X, 
				       iceval:get_val(Val)})
	    end
    end.

tktab_add(G, S, N, X) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), nil, nil).
tktab_add(G, S, N, X, TK) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), TK, nil).
tktab_add(G, S, N, X, TK, Aux) ->
    tktab_add_id(G, S, N, X, ic_forms:get_id2(X), TK, Aux).


tktab_add_id(G, S, N, X, Id, TK, Aux) when is_record(X,enumerator) ->

    %% Check if the "scl" flag is set to true
    %% if so, allow old semantics ( errornous )
    %% Warning, this is for compatibility reasons only.
    Name = case ic_options:get_opt(G, scl) of 
	       true -> 
		   [Id | N];
	       false ->
		   [Id | tl(N)]
	   end,

    UName = mk_uppercase(Name),
    case ets:lookup(S, Name) of
	[_] -> ic_error:error(G, {multiply_defined, X});
	[] ->
	    case ets:lookup(S, UName) of
		[] -> ok;
		[_] -> ic_error:error(G, {illegal_spelling, X})
	    end
    end,
    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
	true -> true end,
    TK;
%%
%% Fixes the multiple file module definition check 
%% but ONLY for Corba backend
%%				
tktab_add_id(G, S, N, X, Id, TK, Aux) when is_record(X,module) ->
    case ic_options:get_opt(G, be) of
	erl_template ->
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	erl_corba ->
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	false -> %% default == erl_corba
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	java -> 
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	erl_genserv -> 
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	erl_plain -> 
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK;
	_Be -> 
	    Name = [Id | N],
	    UName = mk_uppercase(Name),
	    case ets:lookup(S, Name) of
		[_] -> ic_error:error(G, {multiply_defined, X});
		[] ->
		    case ets:lookup(S, UName) of
			[] -> ok;
			[_] -> ic_error:error(G, {illegal_spelling, X})
		    end
	    end,
	    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
	    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
		true -> true end,
	    TK
    end;
tktab_add_id(G, S, N, X, Id, TK, Aux) ->
    Name = [Id | N],
    UName = mk_uppercase(Name),
    case ets:lookup(S, Name) of
	[{_, forward, _, _}] when is_record(X, interface) ->
	    ok;
	[{_, constr_forward, _, _}] when is_record(X, union) orelse 
					 is_record(X, struct) -> 
	    ok;
	[XX] when is_record(X, forward) andalso element(2, XX)==interface -> 
	    ok;
	[_] -> 
	    ic_error:error(G, {multiply_defined, X});
	[] ->
	    case ets:lookup(S, UName) of
		[] -> ok;
		[_] -> ic_error:error(G, {illegal_spelling, X})
	    end
    end,
    ets:insert(S, {Name, element(1, get_beef(X)), TK, Aux}),
    if  UName =/= Name -> ets:insert(S, {UName, spellcheck});
	true -> true end,
    TK.




%%--------------------------------------------------------------------
%% enum_body
%%
%%	Special because ids are treated different than usual.
%%
enum_body(G, S, N, [Enum | EnumList]) -> 
    tktab_add(G, S, N, Enum), %%%, enum_val, Enum),
    %%    tktab_add(G, S, N, X, TK, V),
    [ic_forms:get_id2(Enum) | enum_body(G, S, N, EnumList)];
enum_body(_G, _S, _N, []) -> [].


%%--------------------------------------------------------------------
%% mk_array
%%
%%	Multi dimensional arrays are written as nested tk_array
%%
mk_array(G, S, N, [Sz | Szs], TK) ->
    case iceval:eval_const(G, S, N, positive_int, Sz) of
	Err when element(1, Err) == error -> TK;
	Val ->
	    {tk_array, mk_array(G, S, N, Szs, TK), iceval:get_val(Val)}
    end;
mk_array(_G, _S, _N, [], TK) -> TK.


%%--------------------------------------------------------------------
%% len_eval
%%
%%	Evaluates the length, which in case it has been left out is a
%%	plain 0 (zero)
%%
len_eval(_G, _S, _N, 0) -> 0;
len_eval(G, S, N, X) -> %%iceval:eval_const(G, S, N, positive_int, X).
    case iceval:eval_const(G, S, N, positive_int, X) of
	Err when element(1, Err) == error -> 0;
	Val -> iceval:get_val(Val)
    end.


%%--------------------------------------------------------------------
%% case_eval
%%
%%	Evaluates the case label.
%%

case_eval(G, S, N, DiscrTK, X) when element(1, DiscrTK) == tk_enum,
				    element(1, X) == scoped_id -> 
    {tk_enum, _, _, Cases} = DiscrTK,
    Id = get_case_id_and_check(G, S, N, X, X),
    %%io:format("Matching: ~p to ~p~n", [Id, Cases]),
    case lists:member(Id, Cases) of
	true ->
	    {enum_id, Id};
	false ->
	    iceval:mk_val(scoped_lookup(G, S, N, X)) % Will generate error
    end;

case_eval(G, S, N, DiscrTK, X) -> 
    iceval:eval_e(G, S, N, DiscrTK, X).


%% The enum declarator is in the union scope.
do_special_enum(G, S, N, X) when is_record(X, enum) ->
    tktab_add(G, S, N, #id_of{id=X#enum.id, type=X});
do_special_enum(_G, _S, _N, _X) ->
    ok.


unique_add_case_label(G, _S, _N, Id, TK, TKList) ->
%%%io:format("check_case_labels: TK:~p TKLIST:~p ~n", [TK, TKList]),
    if  element(1, TK) == error -> 
	    TKList;
	true ->
	    case lists:keysearch(element(1, TK), 1, TKList) of
		{value, _} -> 
		    ic_error:error(G, {multiple_cases, Id}),
		    TKList;
		false -> 
		    [TK | TKList]
	    end
    end.


%%--------------------------------------------------------------------
%% default_count
%%
%%	Returns the position of the default case.
%%
%%	Modified for OTP-2007
%%
default_count(Xs) ->
    default_count2(Xs, 0).

default_count2([X | Xs], N) -> default_count3(X#case_dcl.label, Xs, N);
default_count2([], _) -> -1.

default_count3([{default, _} | _Ys], _Xs, N) -> N;
default_count3([_ | Ys], Xs, N) -> default_count3(Ys, Xs, N+1);
default_count3([], Xs, N) -> default_count2(Xs, N).




%%
%% Type checks.
%%
%% Check constant type references (only for the scoped id case, others
%% are caught by the BNF)
%%
check_const_tk(_G, _S, _N, _X, tk_long) -> true;
check_const_tk(_G, _S, _N, _X, tk_longlong) -> true; %% LLONG
check_const_tk(_G, _S, _N, _X, tk_short) -> true;
check_const_tk(_G, _S, _N, _X, tk_ushort) -> true;
check_const_tk(_G, _S, _N, _X, tk_ulong) -> true; 
check_const_tk(_G, _S, _N, _X, tk_ulonglong) -> true;  %% ULLONG
check_const_tk(_G, _S, _N, _X, tk_float) -> true;
check_const_tk(_G, _S, _N, _X, tk_double) -> true;
check_const_tk(_G, _S, _N, _X, tk_boolean) -> true;
check_const_tk(_G, _S, _N, _X, tk_char) -> true;
check_const_tk(_G, _S, _N, _X, tk_wchar) -> true; %% WCHAR
check_const_tk(_G, _S, _N, _X, tk_octet) -> true;
check_const_tk(_G, _S, _N, _X, {tk_string, _Len}) -> true;
check_const_tk(_G, _S, _N, _X, {tk_wstring, _Len}) -> true; %% WSTRING
check_const_tk(_G, _S, _N, _X, tk_fixed) -> true;
check_const_tk(_G, _S, _N, _X, {tk_fixed, _Digits, _Scale}) -> true;
check_const_tk(G, _S, _N, X, TK) -> ic_error:error(G, {illegal_const_t, X, TK}).


check_switch_tk(_G, _S, _N, _X, tk_long) -> true;
check_switch_tk(_G, _S, _N, _X, tk_longlong) -> true; %% LLONG
check_switch_tk(_G, _S, _N, _X, tk_short) -> true;
check_switch_tk(_G, _S, _N, _X, tk_ushort) -> true;
check_switch_tk(_G, _S, _N, _X, tk_ulong) -> true;
check_switch_tk(_G, _S, _N, _X, tk_ulonglong) -> true;  %% ULLONG
check_switch_tk(_G, _S, _N, _X, tk_boolean) -> true;
check_switch_tk(_G, _S, _N, _X, tk_char) -> true;
check_switch_tk(_G, _S, _N, _X, tk_wchar) -> true;  %% WCHAR
check_switch_tk(_G, _S, _N, _X, TK) when element(1, TK) == tk_enum -> true;
check_switch_tk(G, _S, _N, X, TK) -> ic_error:error(G, {illegal_switch_t, X, TK}),
				     false.



%% Lookup a name
name_lookup(G, S, N, X) ->
    case scoped_lookup(G, S, N, X) of
	T when is_tuple(T) -> element(1, T)
    end.


lookup(G, S, N, X, Id) ->
    N2 = Id ++ N,
    ?DBG("  Trying ~p ...~n", [N2]),
    case ets:lookup(S, N2) of
	[] ->	    
	    case look_for_interface(G, S, [hd(N2)], tl(N2)) of

		%% First attempt: filtering inherited members !
		[{_, member, _, _}] ->	    
		    case look_for_interface(G, S, [hd(N)], tl(N2)) of
			[T] -> 
			    ?DBG("    --  found ~p~n", [T]), 
			    T;
			_ ->
			    lookup(G, S, tl(N), X, Id)
		    end;
		%%

		[T] -> 
		    ?DBG("    --  found ~p~n", [T]), 
		    T;

		_ ->
		    if  N == [] -> 
			    ic_error:error(G, {tk_not_found, X});
			true ->
			    lookup(G, S, tl(N), X, Id)
		    end

	    end;

	%% Second attempt: filtering members !
	[{_, member, _, _}] ->	    
	    case look_for_interface(G, S, [hd(N2)], tl(N2)) of
		[T] -> 
		    ?DBG("    --  found ~p~n", [T]), 
		    T;
		_ ->
		    if  N == [] -> 
			    ic_error:error(G, {tk_not_found, X});
			true ->
			    lookup(G, S, tl(N), X, Id)
		    end
	    end;
	%%
	[T] -> 
	    ?DBG("    --  found ~p~n", [T]),
	    T
    end.


look_for_interface(_G, _S, _Hd, []) -> 
    false;
look_for_interface(G, S, Hd, Tl) ->
    case ets:lookup(S, Tl) of
	[{_, interface, _TK, Inh}] -> 
	    case look_in_inherit(G, S, Hd, Inh) of
		%% gather_inherit(G, S, Inh, [])) of
		[X] when is_tuple(X) -> 
		    [X];
		_ -> 
		    look_for_interface(G, S, Hd ++ [hd(Tl)], tl(Tl))
	    end;
	_ -> 
	    look_for_interface(G, S, Hd ++ [hd(Tl)], tl(Tl))
    end.

look_in_inherit(G, S, Id, [I | Is]) ->
    case ets:lookup(S, Id ++ I) of
	[X] when is_tuple(X) -> 
	    [X];
	[] ->  
	    look_in_inherit(G, S, Id, Is)
    end;
look_in_inherit(_G, _S, _Id, []) -> 
    false.


%% L is a list of names
mk_uppercase(L) ->
    lists:map(fun(Z) -> lists:map(fun(X) when X>=$a, X=<$z -> X-$a+$A;
				     (X) -> X end, Z) end, L).


%%--------------------------------------------------------------------
%%
%% Inheritance stuff
%%
%%
%%--------------------------------------------------------------------

%% InhBody is an accumulating parameter

calc_inherit_body(G, N, OrigBody, [X|Xs], InhBody) ->
    case ic_symtab:retrieve(G, X) of
	Intf when is_record(Intf, interface) ->
	    Body = filter_body(G, X, ic_forms:get_body(Intf), N, OrigBody, InhBody),
	    calc_inherit_body(G, N, OrigBody, Xs, [{X, Body} | InhBody]);
	XXX ->
	    io:format("Oops, not found ~p~n", [XXX]),
	    calc_inherit_body(G, N, OrigBody, Xs, InhBody)
    end;
calc_inherit_body(_G, _N, _OrigBody, [], InhBody) -> lists:reverse(InhBody).


filter_body(G, XPath, [X | Xs], OrigPath, OrigBody, InhBody) ->
    case complex_body_member(G, XPath, X, OrigPath, OrigBody, InhBody) of
	true -> 
	    %%io:format("NOT adding ~p~n", [ic_forms:get_id2(X)]),
	    filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody);
	{false, NewX} ->			% For those with idlist
	    %%io:format("Adding from idlist~n", []),
	    [NewX | filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody)];
	false ->
	    %%io:format("Adding: ~p~n", [ic_forms:get_id2(X)]),
	    [X | filter_body(G, XPath, Xs, OrigPath, OrigBody, InhBody)]
    end;
filter_body(_G, _XPath, [], _OrigPath, _OrigBody, _InhBody) -> [].


complex_body_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->
    case has_idlist(X) of
	true ->
	    idlist_member(G, XPath, X, OrigPath, OrigBody, InhBody);
	false ->
	    straight_member(G, XPath, X, OrigPath, OrigBody, InhBody)
    end.


idlist_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->    
    XX = #id_of{type=X},
    F = fun(Id) ->
		not(straight_member(G, XPath, XX#id_of{id=Id}, OrigPath,
				    OrigBody, InhBody))
	end,
    case lists:filter(F, ic_forms:get_idlist(X)) of
	[] -> 
	    true;
	IdList ->
%%%	    io:format("Idlist added: ~p~n",[IdList]),
	    {false, replace_idlist(X, IdList)}
    end.


straight_member(G, XPath, X, OrigPath, OrigBody, InhBody) ->
    %%io:format("straight member: ~p~n", [ic_forms:get_id2(X)]),
    case body_member(G, XPath, X, OrigPath, OrigBody) of
	true ->
	    true;
	false -> 
	    inh_body_member(G, XPath, X, InhBody)
    end.


inh_body_member(G, XPath, X, [{Name, Body} | InhBody]) ->
    case body_member(G, XPath, X, Name, Body) of
	true ->
	    true;
	false -> 
	    inh_body_member(G, XPath, X, InhBody)
    end;
inh_body_member(_G, _XPath, _X, []) -> false.


body_member(G, XPath, X, YPath, [Y|Ys]) ->
    case has_idlist(Y) of
	true -> 
	    YY = #id_of{type=Y},
	    case list_and(fun(Y2) -> 
				  not(is_equal(G, XPath, X, YPath, 
					       YY#id_of{id=Y2})) end,
			  ic_forms:get_idlist(Y)) of
		true -> 
		    body_member(G, XPath, X, YPath, Ys);
		false ->
		    true
	    end;
	false ->
	    case is_equal(G, XPath, X, YPath, Y) of
		false ->
		    body_member(G, XPath, X, YPath, Ys);
		true ->
		    true
	    end
    end;
body_member(_G, _XPath, _X, _YPath, []) -> false.


is_equal(G, XPath, X, YPath, Y) ->
    case {ic_forms:get_id2(X), ic_forms:get_id2(Y)} of
	{ID, ID} ->
	    collision(G, XPath, X, YPath, Y),
	    true;
	_ -> 
	    false
    end.


%% X is the new item, Y is the old one. So it is X that collides with
%% Y and Y shadows X.
collision(G, XPath, X, YPath, Y) ->
    I1 = get_beef(X),
						%    I2 = get_beef(Y),
    if is_record(I1, op) -> %%, record(I2, op) ->
	    ic_error:error(G, {inherit_name_collision, 
			       {YPath, Y}, {XPath, X}});
       is_record(I1, attr) -> %%, record(I2, attr) ->
	    ic_error:error(G, {inherit_name_collision, 
			       {YPath, Y}, {XPath, X}});
       true ->
	    ?ifopt(G, warn_name_shadow, 
		   ic_error:warn(G, {inherit_name_shadow, 
				     {YPath, Y}, {XPath, X}}))
    end.

has_idlist(X) when is_record(X, typedef) -> true;
has_idlist(X) when is_record(X, member) -> true;
has_idlist(X) when is_record(X, case_dcl) -> true;
has_idlist(X) when is_record(X, attr) -> true;
has_idlist(_) -> false.

replace_idlist(X, IdList) when is_record(X, typedef) -> X#typedef{id=IdList};
replace_idlist(X, IdList) when is_record(X, attr) -> X#attr{id=IdList}.

get_beef(X) when is_record(X, id_of) -> X#id_of.type;
get_beef(X) -> X.


%% And among all elements in list
list_and(F, [X|Xs]) ->
    case F(X) of
	true -> list_and(F, Xs);
	false -> false
    end;
list_and(_F, []) -> true.





%%--------------------------------------------------------------------
%%
%%	resolve_inherit shall return a list of resolved inheritances,
%%	that is all names replaced with their global names.
%%

inherit_resolve(G, S, N, [X|Rest], Out) ->
    case scoped_lookup(G, S, N, X) of
	{Name, _T, _TK, Inh} ->
	    case lists:member(Name, Out) of
		true -> 
		    inherit_resolve(G, S, N, Rest, Out);
		false ->
		    case unique_append(Inh, [Name|Out]) of
			error ->
			    ic_error:error(G, {inherit_resolve, X, Name}),
			    inherit_resolve(G, S, N, Rest, []);
			UA ->
			    inherit_resolve(G, S, N, Rest, UA)
		    end
	    end;
	_ -> inherit_resolve(G, S, N, Rest, Out)
    end;
inherit_resolve(_G, _S, _N, [], Out) -> lists:reverse(Out).

unique_append([X|Xs], L) ->
    case lists:member(X, L) of
	true -> unique_append(Xs, L);
	false -> unique_append(Xs, [X|L])
    end;
unique_append([], L) -> L;
%% Error 
unique_append(_, _L) -> error.




%%--------------------------------------------------------------------
%%
%%	Utilities
%%

%% Must preserve order, therefore had to write my own (instead of lists:map)
check_list(G, S, N, [X|Xs]) ->
    X1 = check(G, S, N, X),
    [X1 | check_list(G, S, N, Xs)];
check_list(_G, _S, _N, []) -> [].



filter( [] ) ->
    error;
filter( [I | Is ] ) ->
    case I of
	{ _, member, { _, TKINFO }, _ } ->
	    fetchType( TKINFO );

        { _, struct, _, _ } ->
	    struct;

	{ _, typedef, TKINFO, _ } ->
	    fetchType( TKINFO );

	{ _, module, _, _ } ->
	    module;

	{ _, interface, _, _ } ->
	    interface;

	{ _, op, _, _ } ->
	    op;

	{ _,enum, _, _ } ->
	    enum;

	{ _, spellcheck } ->
	    filter( Is );

	_ ->
	    error
    end.


fetchType( { tk_sequence, _, _ } ) ->
    sequence;
fetchType( { tk_array, _, _ } ) ->
    array;
fetchType( { tk_struct, _, _, _} ) ->
    struct;
fetchType( { tk_string, _} ) ->
    string;
fetchType( { tk_wstring, _} ) ->  %% WSTRING
    wstring;
fetchType( { tk_fixed, _, _} ) ->
    fixed;
fetchType( tk_short ) ->
    short;
fetchType( tk_long ) ->
    long;
fetchType( tk_longlong ) ->  %% LLONG
    longlong;
fetchType( tk_ushort ) ->
    ushort;
fetchType( tk_ulong ) ->
    ulong;
fetchType( tk_ulonglong ) ->  %% ULLONG
    ulonglong;
fetchType( tk_float ) ->
    float;
fetchType( tk_double ) ->
    double;
fetchType( tk_boolean ) ->
    boolean;
fetchType( tk_char ) ->
    char;
fetchType( tk_wchar ) ->  %% WCHAR
    wchar;
fetchType( tk_octet ) ->
    octet;
fetchType( { tk_enum, _, _, _ } ) ->
    enum;
fetchType( { tk_union, _, _, _, _, _ } ) ->
    union;
fetchType( tk_any ) ->
    any;
fetchType( _ ) ->
    error.

%% Z is a single name
to_uppercase(Z) ->
    lists:map(fun(X) when X>=$a, X=<$z -> X-$a+$A;
		 (X) -> X end, Z).


%%------------------------------------------------------------
%%
%% Always fetchs TK of a record.
%%
%%------------------------------------------------------------
fetchTk(G,N,X) ->
    case ic_forms:get_tk(X) of
        undefined ->
            searchTk(G,ictk:get_IR_ID(G, N, X));
        TK ->
            TK
    end.


%%------------------------------------------------------------
%%
%% seek type code when not accessible by get_tk/1
%%
%%------------------------------------------------------------
searchTk(G,IR_ID) ->
    S = ic_genobj:tktab(G),
    case catch searchTk(S,IR_ID,typedef) of
        {value,TK} ->
            TK;
        _ -> %% false / exit
            case catch searchTk(S,IR_ID,struct) of
                {value,TK} ->
                    TK;
                _  ->  %% false / exit
                    case catch searchTk(S,IR_ID,union) of
                        {value,TK} ->
                            TK;
                        _ ->
                            undefined
                    end
            end
    end.


searchTk(S,IR_ID,Type) ->
    L = lists:flatten(ets:match(S,{'_',Type,'$1','_'})),
    case lists:keysearch(IR_ID,2,L) of
        {value,TK} ->
            {value,TK};
        false ->
            searchInsideTks(L,IR_ID)
    end.


searchInsideTks([],_IR_ID) ->
    false;
searchInsideTks([{tk_array,TK,_}|Xs],IR_ID) ->
    case searchIncludedTk(TK,IR_ID) of
        {value,TK} ->
            {value,TK};
        false ->
            searchInsideTks(Xs,IR_ID)
    end.


searchIncludedTk({tk_array,TK,_},IR_ID) ->
    searchIncludedTk(TK,IR_ID);
searchIncludedTk({tk_sequence,TK,_},IR_ID) ->
    searchIncludedTk(TK,IR_ID);
searchIncludedTk(TK, _IR_ID) when is_atom(TK) ->
    false;
searchIncludedTk(TK,IR_ID) ->
    case element(2,TK) == IR_ID of
        true ->
            {value,TK};
        false ->
            false
    end.
        
