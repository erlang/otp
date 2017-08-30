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
 
-module(ic_fetch).

-include("icforms.hrl").

-export([member2type/3]).

-export([fetchTk/3, isArray/3, isBasicType/1, isBasicType/2,
	 isBasicType/3, isBasicTypeOrEterm/3, isEterm/3, isString/3,
	 isStruct/3, isUnion/3, name2type/2, searchIncludedTk/2,
	 searchInsideTks/2, searchTk/2, searchTk/3]).

name2type(G, Name) ->
    S = ic_genobj:tktab(G),
    ScopedName = lists:reverse(string:tokens(Name,"_")),
    InfoList = ets:lookup( S, ScopedName ),
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

lookup_member_type_in_tktab([],_ScopedName) ->
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
fetchType( tk_short ) ->
    short;
fetchType( tk_long ) ->
    long;
fetchType( tk_ushort ) ->
    ushort;
fetchType( tk_ulong ) ->
    ulong;
fetchType( tk_float ) ->
    float;
fetchType( tk_double ) ->
    double;
fetchType( tk_boolean ) ->
    boolean;
fetchType( tk_char ) ->
    char;
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

isBasicType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isBasicType(fetchType(TK));
isBasicType(_G, _N, {string, _} ) -> 
    false;
isBasicType(_G, _N, {Type, _} ) -> 
    isBasicType(Type).


isBasicType(G, Name) ->
    isBasicType(name2type(G, Name )).


isBasicType(Type) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_float,float,
		  tk_double,double,
		  tk_boolean,boolean,
		  tk_char,char,
		  tk_octet,octet]).



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
searchIncludedTk(TK,_IR_ID) when is_atom(TK) ->
    false;
searchIncludedTk(TK,IR_ID) ->
    case element(2,TK) == IR_ID of
	true ->
	    {value,TK};
	false ->
	    false
    end.
	










