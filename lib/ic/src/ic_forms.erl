%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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

-module(ic_forms).

-include_lib("ic/src/ic.hrl").
-include_lib("ic/src/icforms.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([get_id/1, get_id2/1, get_java_id/1, get_line/1]).
-export([get_type_code/3, search_tk/2, clean_up_scope/1]).
-export([get_body/1, get_dimension/1, get_idlist/1, get_type/1, get_tk/1, is_oneway/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% Generation go-get utilities
%%
%%	Feeble attempt at virtual funtions.
%%
%%--------------------------------------------------------------------

get_dimension(X) when is_record(X, array)       -> 
    [element(3, L) || L <- X#array.size].

%% Should find the name hidden in constructs
get_id( [{'<identifier>', _LineNo, Id}] ) -> Id;
get_id( {'<identifier>', _LineNo, Id} ) -> Id;
get_id(Id) when is_list(Id) andalso is_integer(hd(Id)) -> Id;
get_id(X) when is_record(X, scoped_id) -> X#scoped_id.id;
get_id(X) when is_record(X, array) -> get_id(X#array.id);
get_id( {'<string_literal>', _LineNo, Id} ) -> Id;
get_id( {'<wstring_literal>', _LineNo, Id} ) -> Id.

get_line([{'<identifier>', LineNo, _Id}]) -> LineNo;
get_line({'<identifier>', LineNo, _Id}) -> LineNo;
get_line(X) when is_record(X, scoped_id) -> X#scoped_id.line;
get_line(X) when is_record(X, module)      -> get_line(X#module.id);
get_line(X) when is_record(X, interface)   -> get_line(X#interface.id);
get_line(X) when is_record(X, forward)     -> get_line(X#forward.id);
get_line(X) when is_record(X, constr_forward) -> get_line(X#constr_forward.id);
get_line(X) when is_record(X, const)       -> get_line(X#const.id);
get_line(X) when is_record(X, typedef)     -> get_line(X#typedef.id);
get_line(X) when is_record(X, struct)      -> get_line(X#struct.id);
get_line(X) when is_record(X, member)      -> get_line(X#member.id);
get_line(X) when is_record(X, union)       -> get_line(X#union.id);
get_line(X) when is_record(X, case_dcl)    -> get_line(X#case_dcl.id);
get_line(X) when is_record(X, enum)	-> get_line(X#enum.id);
get_line(X) when is_record(X, enumerator)	-> get_line(X#enumerator.id);
get_line(X) when is_record(X, array)       -> get_line(X#array.id);
get_line(X) when is_record(X, attr)	-> get_line(X#attr.id);
get_line(X) when is_record(X, except)	-> get_line(X#except.id);
get_line(X) when is_record(X, op)		-> get_line(X#op.id);
get_line(X) when is_record(X, param)       -> get_line(X#param.id);
get_line(X) when is_record(X, id_of)       -> get_line(X#id_of.id);

get_line({'or', T1, _T2}) ->	get_line(T1);
get_line({'xor', T1, _T2}) ->	get_line(T1);
get_line({'and', T1, _T2}) ->	get_line(T1);
get_line({'rshift', T1, _T2}) ->get_line(T1);
get_line({'lshift', T1, _T2}) ->get_line(T1);
get_line({'+', T1, _T2}) ->	get_line(T1);
get_line({'-', T1, _T2}) ->	get_line(T1);
get_line({'*', T1, _T2}) ->	get_line(T1);
get_line({'/', T1, _T2}) ->	get_line(T1);
get_line({'%', T1, _T2}) ->	get_line(T1);
get_line({{'-', _Line}, T}) ->	get_line(T);
get_line({{'+', _Line}, T}) ->	get_line(T);
get_line({{'~', _Line}, T}) ->	get_line(T);
get_line({_, X, _}) when is_integer(X) -> X;
get_line({_A, N}) when is_integer(N)	-> N;
get_line(_)				-> -1.


%%--------------------------------------------------------------------
%%
%% High level get functions.
%%
%%	These are highly polymorphic functions that will get the id,
%%	body and type of a record (those records output from the
%%	parser).
%%
%% NOTE: The typedef node (the alias) is special, because the type
%%	field is a type definition and therefore considered a body,
%%	and the type of a typedef is its name.
%%

get_id2(X) when is_record(X, module)       -> get_id(X#module.id);
get_id2(X) when is_record(X, interface)    -> get_id(X#interface.id);
get_id2(X) when is_record(X, forward)      -> get_id(X#forward.id);
get_id2(X) when is_record(X, constr_forward) -> get_id(X#constr_forward.id);
get_id2(X) when is_record(X, const)        -> get_id(X#const.id);
get_id2(X) when is_record(X, typedef)      -> get_id(hd(X#typedef.id));
get_id2(X) when is_record(X, struct)       -> get_id(X#struct.id);
get_id2(X) when is_record(X, member)       -> get_id(hd(X#member.id));
get_id2(X) when is_record(X, union)        -> get_id(X#union.id);
get_id2(X) when is_record(X, case_dcl)     -> get_id(X#case_dcl.id);
get_id2(X) when is_record(X, enum)		-> get_id(X#enum.id);
get_id2(X) when is_record(X, enumerator)	-> get_id(X#enumerator.id);
get_id2(X) when is_record(X, array)        -> get_id(X#array.id);
get_id2(X) when is_record(X, attr)		-> get_id(X#attr.id);
get_id2(X) when is_record(X, except)       -> get_id(X#except.id);
get_id2(X) when is_record(X, op)		-> get_id(X#op.id);
get_id2(X) when is_record(X, param)        -> get_id(X#param.id);
get_id2(X) when is_record(X, type_dcl)     -> get_id2(X#type_dcl.type);
get_id2(X) when is_record(X, scoped_id)	-> ic_symtab:scoped_id_strip(X);
get_id2(X) when is_record(X, preproc)	-> get_id(X#preproc.id);
get_id2(X) when is_record(X, id_of)	-> get_id2(X#id_of.id);
get_id2(X) -> get_id(X).

get_body(X) when is_record(X, module)      -> X#module.body;
get_body(X) when is_record(X, interface)   -> X#interface.body;
get_body(X) when is_record(X, struct)      -> X#struct.body;
get_body(X) when is_record(X, union)       -> X#union.body;
get_body(X) when is_record(X, enum)        -> X#enum.body;
get_body(X) when is_record(X, typedef)     -> X#typedef.type; % See Note 
get_body(X) when is_record(X, except)      -> X#except.body.

get_type(X) when is_record(X, const)       -> X#const.type;
get_type(X) when is_record(X, type_dcl)    -> X#type_dcl.type;
get_type(X) when is_record(X, typedef)     -> X#typedef.id; % See Note 
get_type(X) when is_record(X, member)      -> X#member.type;
get_type(X) when is_record(X, union)       -> X#union.type;
get_type(X) when is_record(X, case_dcl)    -> X#case_dcl.type;
get_type(X) when is_record(X, sequence)    -> X#sequence.type;
get_type(X) when is_record(X, attr)        -> X#attr.type;
get_type(X) when is_record(X, op)		-> X#op.type;
get_type(X) when is_record(X, param)       -> X#param.type.
%%get_type(X) when record(X, id_of)       -> get_type(X#id_of.type).

%% Temporary place
get_tk(X) when is_record(X, interface)  -> X#interface.tk;
get_tk(X) when is_record(X, forward)    -> X#forward.tk;
get_tk(X) when is_record(X, constr_forward) -> X#constr_forward.tk;
get_tk(X) when is_record(X, const)      -> X#const.tk;
get_tk(X) when is_record(X, type_dcl)   -> X#type_dcl.tk;
get_tk(X) when is_record(X, typedef)    -> X#typedef.tk;
get_tk(X) when is_record(X, struct)     -> X#struct.tk;
get_tk(X) when is_record(X, union)      -> X#union.tk;
get_tk(X) when is_record(X, enum)       -> X#enum.tk;
get_tk(X) when is_record(X, attr)       -> X#attr.tk;
get_tk(X) when is_record(X, except)     -> X#except.tk;
get_tk(X) when is_record(X, op)         -> X#op.tk;
get_tk(X) when is_record(X, id_of)      -> X#id_of.tk;
get_tk(X) when is_record(X, param)      -> X#param.tk.


%% Get idlist returns the list of identifiers found in typedefs, case
%% dcls etc.
get_idlist(X) when is_record(X, typedef)	-> X#typedef.id;
get_idlist(X) when is_record(X, member)	-> X#member.id;
get_idlist(X) when is_record(X, case_dcl)	-> X#case_dcl.label;
get_idlist(X) when is_record(X, attr)	-> X#attr.id.


is_oneway(X) when is_record(X, op)  ->
    case  X#op.oneway of
	{oneway, _} -> true;
	_ -> false
    end;
is_oneway(_X) -> false.





%%------------------------------------------------------------
%%
%% Analyze the record and seek the correct type code.
%%
%% NOT equal to get_tk, this will always succed !
%%
%%------------------------------------------------------------
get_type_code(G, N, X) ->
    case get_type_code2(G, N, X) of
	undefined ->
	    %% Remove "Package" suffix from scope
	    N2 = clean_up_scope(N),
	    search_tk(G,ictk:get_IR_ID(G, N2, X));
	TC ->
	    TC
    end.

clean_up_scope(N) ->
    clean_up_scope(N,[]).

clean_up_scope([],N) ->
    lists:reverse(N);
clean_up_scope([N|Ns],Found) ->
    case lists:suffix("Package",N) of
	true ->
	    Len = length(N),
	    case Len > 7 of
		true ->
		    N2 = string:substr(N,1,Len-7),
		    clean_up_scope(Ns,[N2|Found]);
		false ->
		    clean_up_scope(Ns,[N|Found])
	    end;
	false ->
	    clean_up_scope(Ns,[N|Found])
    end.
    

get_type_code2(_, _, X) when is_record(X, interface)  -> X#interface.tk;
get_type_code2(_, _, X) when is_record(X, forward)    -> X#forward.tk;
get_type_code2(_, _, X) when is_record(X, constr_forward) -> X#constr_forward.tk;
get_type_code2(_, _, X) when is_record(X, const)      -> X#const.tk;
get_type_code2(_, _, X) when is_record(X, type_dcl)   -> X#type_dcl.tk;
get_type_code2(_, _, X) when is_record(X, typedef)    ->
    Id = X#typedef.id,
    ET = X#typedef.tk,
    if is_list(Id) ->
	    Head = hd(Id),
	    if is_tuple(Head) ->
		    case element(1,Head) of
			array ->
			    get_array_tc(ET, element(3,Head));
			_ ->
			    ET
		    end;
	       true ->
		    ET
	    end;
       true ->
	    ET
    end;

get_type_code2(_, _, X) when is_record(X, struct)     -> X#struct.tk;
get_type_code2(_, _, X) when is_record(X, union)      -> X#union.tk;
get_type_code2(_, _, X) when is_record(X, enum)       -> X#enum.tk;
get_type_code2(_, _, X) when is_record(X, attr)       -> X#attr.tk;
get_type_code2(_, _, X) when is_record(X, except)     -> X#except.tk;
get_type_code2(_, _, X) when is_record(X, op)         -> X#op.tk;
get_type_code2(_, _, X) when is_record(X, id_of)      -> X#id_of.tk;
get_type_code2(_, _, X) when is_record(X, param)      -> X#param.tk;

get_type_code2(G, N, X) when is_record(X, member) ->
    ET = get_type_code(G, N, element(2,X)),
    Id = element(3,X),

    if is_list(Id) ->
	    Head = hd(Id),
	    if is_tuple(Head) ->
		    case element(1,Head) of
			array ->
			    get_array_tc(ET, element(3,Head));
			_ ->
			    ET
		    end;
	       true ->
		    ET
	    end;
       true ->
	    ET
    end;

get_type_code2(G, N, X) when is_record(X, scoped_id) ->
    element(3,ic_symtab:get_full_scoped_name(G, N, X));

get_type_code2(G, N, X) when is_record(X, sequence) -> 
    if is_tuple(X#sequence.length) ->
	    {tk_sequence, 
	     get_type_code(G, N, X#sequence.type), 
	     list_to_integer(element(3,X#sequence.length))};
       true ->
	    {tk_sequence, 
	     get_type_code(G, N, X#sequence.type), 
	     X#sequence.length}
    end;
  
get_type_code2(_G, _N, {unsigned,{short,_}}) -> tk_ushort;

get_type_code2(_G, _N, {unsigned,{long,_}}) -> tk_ulong;

get_type_code2(_G, _N, {unsigned,{'long long',_}}) -> tk_ulonglong;

get_type_code2(_G, _N, X) when is_record(X, fixed) -> 
    {tk_fixed, X#fixed.digits, X#fixed.scale};

get_type_code2(G, N, {X,_}) ->
    get_type_code2(G, N, X);

get_type_code2(_, _, short) -> tk_short;
get_type_code2(_, _, long) -> tk_long;
get_type_code2(_, _, 'long long') -> tk_longlong;
get_type_code2(_, _, float) -> tk_float;
get_type_code2(_, _, double) -> tk_double;                              
get_type_code2(_, _, boolean) -> tk_boolean;
get_type_code2(_, _, char) -> tk_char;
get_type_code2(_, _, wchar) -> tk_wchar;
get_type_code2(_, _, octet) -> tk_octet;                                                     
get_type_code2(_, _, string) -> tk_string;
get_type_code2(_, _, wstring) -> tk_wstring;
get_type_code2(_, _, any) -> tk_any.


get_array_tc(ET, []) ->
    ET;
get_array_tc(ET, [L|Ls]) ->
    {tk_array,
     get_array_tc(ET,Ls),
     list_to_integer(element(3,L))}.
    



%%------------------------------------------------------------
%%
%% seek type code when not accessible by ic_forms:get_tk/1 ( should be
%% a part of "do_gen" related functions later )
%%
%%------------------------------------------------------------
search_tk(G, IR_ID) ->
    S = ic_genobj:tktab(G),
    case catch search_tk(S,IR_ID,typedef) of
	{value,TK} ->
	    TK;
	_ -> %% false / exit
 	    case catch search_tk(S,IR_ID,struct) of
		{value,TK} ->
		    TK;
		_  ->  %% false / exit
		    case catch search_tk(S,IR_ID,union) of
			{value,TK} ->
			    TK;
			_ ->
			    undefined
		    end
	    end
    end.


search_tk(S, IR_ID, Type) ->
    L = lists:flatten(ets:match(S,{'_',Type,'$1','_'})),
    case lists:keysearch(IR_ID,2,L) of
	{value,TK} ->
	    {value,TK};
	false ->
	    search_inside_tks(L,IR_ID)
    end.


search_inside_tks([],_IR_ID) ->
    false;
search_inside_tks([{tk_array,TK,_}|Xs],IR_ID) ->
    case search_included_tk(TK,IR_ID) of
	{value,TK} ->
	    {value,TK};
	false ->
	    search_inside_tks(Xs,IR_ID)
    end.


search_included_tk({tk_array,TK,_}, IR_ID) ->
    search_included_tk(TK,IR_ID);
search_included_tk({tk_sequence,TK,_}, IR_ID) ->
    search_included_tk(TK,IR_ID);
search_included_tk(TK, _IR_ID) when is_atom(TK) ->
    false;
search_included_tk(TK, IR_ID) ->
    case element(2,TK) == IR_ID of
	true ->
	    {value,TK};
	false ->
	    false
    end.




%% This is similar to get_id2 but in everything else 
%% than a module it will generate an id prefixed  
get_java_id(Id) when is_list(Id) ->
    case java_keyword_coalition(Id) of
	true ->
	    "_" ++ Id;
	false ->
	    Id
    end;
get_java_id(Id_atom) when is_atom(Id_atom) ->
    Id = atom_to_list(Id_atom),
    case java_keyword_coalition(Id) of
	true ->
	    "_" ++ Id;
	false ->
	    Id
    end;
get_java_id(X) ->
    Id = get_id2(X),
    case java_keyword_coalition(Id) of
	true ->
	    "_" ++ Id;
	false ->
	    Id
    end.

java_keyword_coalition(Id) ->
    lists:member(list_to_atom(Id),
		 [abstract, default, 'if', private, throw, boolean,
		  do, implements, protected, throws, break, 
		  double, import, public, transient, byte,
		  else, instanceof, return, 'try', 'case', extends,
		  int, short, void, 'catch', final, interface, static,
		  volatile, char, finally, long, super, while, class,
		  float, native, switch, const, for, new, synchronized, 
		  continue, goto, package, this, true, false]).
    



%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
