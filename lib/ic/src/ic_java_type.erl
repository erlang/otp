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

-module(ic_java_type).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([getType/3, getHolderType/3,
	 getParamType/4, inlinedTypes/2,
	 marshalFun/4, unMarshalFun/4, getFullType/4, 
	 getFullType/3, getMarshalType/4, getUnmarshalType/4,
	 getdim/1]).
-export([isBasicType/3, isBasicType/1]).
-export([isIntegerType/3, isIntegerType/1]).
-export([isTermType/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: getType/3
%%-----------------------------------------------------------------
getType(G, N, T) when is_record(T, scoped_id) ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "Pid";
	"erlang.port" ->
	    ?ICPACKAGE ++ "Port";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "Ref";
	"erlang.term" ->
	    ?ICPACKAGE ++ "Term";
	{enum, Type} ->
	    getType(G, N, Type);
	Type -> 
	    case TK of
		{tk_array,_,_} ->
		    tk2type(G,N,T,TK);
		{tk_sequence,_,_} ->
		    tk2type(G,N,T,TK);
		tk_any ->
		    ?ICPACKAGE ++ "Any";
		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    tk2type(G,N,T,TK);
			false ->
			    Type %% Other types
		    end
	    end
    end;

getType(_G, _N, S) when is_list(S) ->
    S;

getType(_G, _N, T) when is_record(T, string) ->
    "java.lang.String";

getType(_G, _N, T) when is_record(T, wstring) ->  %% WSTRING
    "java.lang.String";

getType(G, N, T) when is_record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

getType(G, N, T) when is_record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

getType(G, N, T) when is_record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ "[]";

getType(G, N, T) when is_record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]);

%% NOTE i am using the new isJavaElementaryType
%% to avoid members declared as keywords (except
%% all java elementary types) to be used as a 
%% class
getType(G, N, T) when is_record(T, member) ->
    Type = tk2type(G,N,T,ic_forms:get_type_code(G, N, T)),
    case isJavaElementaryType(list_to_atom(Type)) of
	true ->
	    Type;
	false ->
	    Prefix = list_to_atom(lists:flatten(string:tokens(Type,"[]"))),
	    case isJavaElementaryType(Prefix) of %% Checks if Type is an array
		                                 %% of elementary java types
		true ->
		    Type;
		false -> 
		    ic_forms:get_java_id(getType(G,N,ic_forms:get_type(T))) ++
			if is_record(hd(T#member.id),array) ->
				arrayEmptyDim(hd(T#member.id));
			   true ->
				""
			end
	    end
    end;

getType(_G, _N, {boolean, _}) ->
    "boolean";

getType(_G, _N, {octet, _}) ->
    "byte";

getType(_G, _N, {void, _}) ->
    "void";

getType(_G, _N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    "short";
	{long,_} ->
	    "int";
	{'long long',_} ->
	    "long"
    end;

getType(_G, _N, {char, _}) ->
    "char";

getType(_G, _N, {wchar, _}) ->  %% WCHAR 
    "char";

getType(_G, _N, {short, _}) ->
    "short";

getType(_G, _N, {long, _}) ->
    "int";

getType(_G, _N, {'long long', _}) ->
    "long";

getType(_G, _N, {float, _}) ->
    "float";

getType(_G, _N, {double, _}) ->
    "double";
    
getType(_G, _N, {any, _}) ->
    ?ICPACKAGE ++ "Any".






%%-----------------------------------------------------------------
%% Func: getHolderType/3
%%-----------------------------------------------------------------
getHolderType(G, N, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHolder";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHolder";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHolder";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHolder";
	{enum, Type} ->
	    getHolderType(G, N, Type);
	
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Holder";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Holder";

		{'tk_array', _ , _} ->
		    Type ++ "Holder";

		{'tk_sequence', _ , _} ->
		    Type ++ "Holder";

		{'tk_string', _} ->
		    ?ICPACKAGE ++ "StringHolder";

		{'tk_wstring', _} ->  %% WSTRING
		    ?ICPACKAGE ++ "StringHolder";

		{'tk_enum', _, _, _} ->
		    Type ++ "Holder";

		'tk_boolean' ->
		    ?ICPACKAGE ++ "BooleanHolder";
		
		'tk_octet' ->
		    ?ICPACKAGE ++ "ByteHolder";
		
		'tk_ushort' ->
		    ?ICPACKAGE ++ "ShortHolder";
		
		'tk_ulong' ->
		    ?ICPACKAGE ++ "IntHolder";
		
		'tk_ulonglong' ->               %% ULLONG
		    ?ICPACKAGE ++ "LongHolder";
		
		'tk_short' ->
		    ?ICPACKAGE ++ "ShortHolder";
		
		'tk_long' ->
		    ?ICPACKAGE ++ "IntHolder";

		'tk_longlong' ->
		    ?ICPACKAGE ++ "LongHolder"; %% LLONG
		
		'tk_float' ->
		    ?ICPACKAGE ++ "FloatHolder";
		
		'tk_double' ->
		    ?ICPACKAGE ++ "DoubleHolder";
		
		'tk_char' ->
		    ?ICPACKAGE ++ "CharHolder";

		'tk_wchar' ->                    %% WCHAR
		    ?ICPACKAGE ++ "CharHolder";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHolder";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getHolderType(G, N, {list_to_atom(tk2type(G,N,T,TK)), -1}); 
			false ->
			    %%io:format("TK = ~p, Type = ~p\n",[TK,Type]),
			    ic_util:to_dot(G,FullScopedName) ++ "Holder"
		    end
	    end
    end;

getHolderType(G, N, S) when is_list(S) ->
    ic_util:to_dot(G,[S|N]) ++ "Holder";

getHolderType(_G, _N, T) when is_record(T, string) ->
    ?ICPACKAGE ++"StringHolder";

getHolderType(_G, _N, T) when is_record(T, wstring) ->  %% WSTRING
    ?ICPACKAGE ++"StringHolder";

getHolderType(G, N, T) when is_record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when is_record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when is_record(T, array) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(G, N, T) when is_record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ "Holder[]";

getHolderType(G, N, T) when is_record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Holder";

getHolderType(_G, _N, {boolean, _}) ->
    ?ICPACKAGE ++"BooleanHolder";

getHolderType(_G, _N, {octet, _}) ->
    ?ICPACKAGE ++"ByteHolder";

getHolderType(_G, _N, {void, _}) ->
    "void";

getHolderType(_G, _N, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ?ICPACKAGE ++"ShortHolder";
	{long,_} ->
	    ?ICPACKAGE ++"IntHolder";
	{'long long',_} ->
	    ?ICPACKAGE ++"LongHolder"
    end;

getHolderType(_G, _N, {char, _}) ->
    ?ICPACKAGE ++"CharHolder";

getHolderType(_G, _N, {wchar, _}) ->  %% WCHAR
    ?ICPACKAGE ++"CharHolder";

getHolderType(_G, _N, {short, _}) ->
    ?ICPACKAGE ++"ShortHolder";

getHolderType(_G, _N, {long, _}) ->
    ?ICPACKAGE ++"IntHolder";

getHolderType(_G, _N, {'long long', _}) ->
    ?ICPACKAGE ++"LongHolder";

getHolderType(_G, _N, {float, _}) ->
    ?ICPACKAGE ++"FloatHolder";

getHolderType(_G, _N, {double, _}) ->
    ?ICPACKAGE ++"DoubleHolder";

getHolderType(_G, _N, {any,_}) ->
    ?ICPACKAGE ++ "AnyHolder".


%%-----------------------------------------------------------------
%% Func: getParamType/4
%%-----------------------------------------------------------------
getParamType(G, N, S, in) ->
    getType(G, N, S);
getParamType(G, N, S, ret) ->
    getType(G, N, S);
getParamType(G, N, S, out) ->
    getHolderType(G, N, S);
getParamType(G, N, S, inout) ->
    getHolderType(G, N, S).


%%-----------------------------------------------------------------
%% Func: getUnmarshalType/4
%%-----------------------------------------------------------------
getUnmarshalType(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHelper";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHelper";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHelper";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHelper";
	{enum, Type} ->
	    getUnmarshalType(G, N, X, Type);
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Helper";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Helper";

		{'tk_sequence', _ , _} ->
		    Type ++ "Helper";

		{'tk_array', _ , _} ->
		    Type ++ "Helper";

		{'tk_enum', _, _, _} ->
		    Type ++ "Helper";

		{'tk_string',_} ->
		    ?ERLANGPACKAGE ++ "OtpErlangString";

		{'tk_wstring',_} ->                     %% WSTRING
		    ?ERLANGPACKAGE ++ "OtpErlangString";

		'tk_char' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_wchar' ->                           %% WCHAR
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_octet' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_ushort' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_ulong' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_ulonglong' ->                       %% ULLONG
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_short' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_long' ->
		    ?ERLANGPACKAGE ++ "OtpErlangLong";

		'tk_longlong' ->                        %% LLONG
		    ?ERLANGPACKAGE ++ "OtpErlangLong";
		
		'tk_float' ->
		    ?ERLANGPACKAGE ++ "OtpErlangDouble";
		
		'tk_double' ->
		    ?ERLANGPACKAGE ++ "OtpErlangDouble";
		
		'tk_boolean' ->
		    ?ERLANGPACKAGE ++ "OtpErlangAtom";

		'tk_void' ->
		    ?ERLANGPACKAGE ++ "OtpErlangAtom";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHelper";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getUnmarshalType(G, N, X, {list_to_atom(tk2type(G,N,T,TK)), -1});
			false ->
			    ic_util:to_dot(G,FullScopedName) ++ "Helper"
		    end
	    end
    end;

getUnmarshalType(_G, _N, _X, S) when is_list(S) ->
    S ++ "Helper";

getUnmarshalType(_G, _N, _X, T) when is_record(T, string) ->
    ?ERLANGPACKAGE ++ "OtpErlangString";

getUnmarshalType(_G, _N, _X, T) when is_record(T, wstring) ->  %% WSTRING
    ?ERLANGPACKAGE ++ "OtpErlangString";

getUnmarshalType(G, N, _X, T) when is_record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, _X, T) when is_record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when is_record(T, sequence) andalso
				  is_record(X, member) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when is_record(T, sequence) andalso
				  is_record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ "Helper";

getUnmarshalType(G, N, X, T) when is_record(T, sequence) ->
    getUnmarshalType(G, N, X, ic_forms:get_type(T)) ++ "Helper";

getUnmarshalType(G, N, X, T) when is_record(T, array) andalso
				  is_record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ "Helper";

getUnmarshalType(G, N, _X, T) when is_record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getUnmarshalType(_G, _N, _X, {boolean, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getUnmarshalType(_G, _N, _X, {octet, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {void, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangAtom";

getUnmarshalType(_G, _N, _X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong";
	{long,_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong";
	{'long long',_} ->
	    ?ERLANGPACKAGE ++ "OtpErlangLong"
    end;

getUnmarshalType(_G, _N, _X, {char, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {wchar, _}) ->  %% WCHAR
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {short, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {long, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {'long long', _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangLong";

getUnmarshalType(_G, _N, _X, {float, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";

getUnmarshalType(_G, _N, _X, {double, _}) ->
    ?ERLANGPACKAGE ++ "OtpErlangDouble";

getUnmarshalType(_G, _N, _X, {any, _}) ->
    ?ICPACKAGE ++ "AnyHelper".

%%-----------------------------------------------------------------
%% Func: getMarshalType/4
%%-----------------------------------------------------------------
getMarshalType(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ?ICPACKAGE ++ "PidHelper";
	"erlang.port" ->
	    ?ICPACKAGE ++ "PortHelper";
	"erlang.ref" ->
	    ?ICPACKAGE ++ "RefHelper";
	"erlang.term" ->
	    ?ICPACKAGE ++ "TermHelper";
	{enum, Type} ->
	    getMarshalType(G, N, X, Type);
	Type ->
	    case TK of
		{'tk_struct', _, _, _} ->
		    Type ++ "Helper";

		{'tk_union', _, _, _, _, _} ->
		    Type ++ "Helper";

		{'tk_array', _ , _} ->
		    Type ++ "Helper";

		{'tk_sequence', _ , _} ->
		    Type ++ "Helper";
		
		{'tk_enum', _, _, _} ->
		    Type ++ "Helper";

		{'tk_string',_} ->
		    "string";

		{'tk_wstring',_} ->  %% WSTRING
		    "string";

		'tk_char' ->
		    "char";

		'tk_wchar' ->  %% WCHAR
		    "char";

		'tk_octet' ->
 		    "byte";
		
		'tk_ushort' ->
		    "ushort";
		
		'tk_ulong' ->
		    "uint";

		'tk_ulonglong' ->  %% ULLONG
		    "ulong";
		
		'tk_short' ->
		    "short";
		
		'tk_long' ->
		    "int";

		'tk_longlong' ->  %% LLONG
		    "long";
		
		'tk_float' ->
		    "float";
		
		'tk_double' ->
		    "double";
		
		'tk_boolean' ->
		    "boolean";

		'tk_void' ->
		    "atom";

		'tk_any' ->
		    ?ICPACKAGE ++ "AnyHelper";

		_ ->
		    case isBasicType(G,N,TK) of
			true ->
			    %% Faked the type !
			    getMarshalType(G, N, X, {list_to_atom(tk2type(G,N,T,TK)), -1}); 
			false ->
			    ic_util:to_dot(G,FullScopedName) ++ "Helper"
		    end
	    end
    end;

getMarshalType(_G, _N, _X, S) when is_list(S) ->
    S ++ "Helper";

getMarshalType(_G, _N, _X, T) when is_record(T, string) ->
    "string";

getMarshalType(_G, _N, _X, T) when is_record(T, wstring) ->  %% WSTRING
    "string";

getMarshalType(G, N, _X, T) when is_record(T, struct) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(G, N, _X, T) when is_record(T, union) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(G, N, X, T) when is_record(T, array) andalso
				is_record(X, case_dcl) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++ 
	"Helper";

getMarshalType(G, N, X, T) when is_record(T, sequence) andalso
				is_record(X, member) ->
    ic_util:to_dot(G,[ic_forms:get_id2(X)|N]) ++ 
	"Helper";

getMarshalType(G, N, _X, T) when is_record(T, sequence) ->
    getType(G, N, ic_forms:get_type(T)) ++ 
	"Helper";

getMarshalType(G, N, _X, T) when is_record(T, enum) ->
    ic_util:to_dot(G,[ic_forms:get_id2(T)|N]) ++
	"Helper";

getMarshalType(_G, _N, _X, {boolean, _}) ->
    "boolean";

getMarshalType(_G, _N, _X, {octet, _}) ->
    "byte";

getMarshalType(_G, _N, _X, {void, _}) ->
    ""; % <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

getMarshalType(_G, _N, _X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    "ushort";
	{long,_} ->
	    "uint";
	{'long long',_} ->
	    "ulong"
    end;

getMarshalType(_G, _N, _X, {short, _}) ->
    "short";
getMarshalType(_G, _N, _X, {long, _}) ->
    "int";
getMarshalType(_G, _N, _X, {'long long', _}) ->
    "long";
getMarshalType(_G, _N, _X, {float, _}) ->
    "float";
getMarshalType(_G, _N, _X, {double, _}) ->
    "double";
getMarshalType(_G, _N, _X, {char, _}) ->
    "char";
getMarshalType(_G, _N, _X, {wchar, _}) ->  %% WCHAR
    "char";
getMarshalType(_G, _N, _X, {any, _}) ->
    ?ICPACKAGE ++ "AnyHelper".




%%-----------------------------------------------------------------
%% Func: unMarshalFun/4
%%-----------------------------------------------------------------
unMarshalFun(G, N, X, T) when element(1, T) == scoped_id ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    BT = ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)), 
    case BT of
	"erlang.pid" ->
	    ".read_pid()";
	"erlang.port" ->
	    ".read_port()";
	"erlang.ref" ->
	    ".read_ref()";
	"erlang.term" ->
	    ".read_term()";
	{enum, Type} ->
	    unMarshalFun(G, N, X, Type);
	_Type ->
	    case isBasicType(G,N,TK) of
		true ->
		    case TK of
			{'tk_string',_} ->
			    ".read_string()";

			{'tk_wstring',_} ->  %% WSTRING
			    ".read_string()";
			
			'tk_boolean' ->
			    ".read_boolean()";
			
			'tk_octet' ->
			    ".read_byte()";
			
			'tk_ushort' ->
			    ".read_ushort()";

			'tk_ulong' ->
			    ".read_uint()";

			'tk_ulonglong' ->  %% ULLONG
			    ".read_ulong()";
			 
			'tk_short' ->
			    ".read_short()";

			'tk_long' ->
			    ".read_int()";

			'tk_longlong' ->  %% LLONG
			    ".read_long()";

			'tk_float' ->
			    ".read_float()";

			'tk_double' ->
			    ".read_double()";

			'tk_char' ->
			    ".read_char()";

			'tk_wchar' ->          %% WCHAR
			    ".read_char()";

			_ ->
			    %% Faked the type !
			    unMarshalFun(G, N, X, {list_to_atom(tk2type(G,N,X,TK)), -1})
		    end;
		false ->
		    ".unmarshal()"
	    end
    end;

unMarshalFun(_G, _N, _X, S) when is_list(S) ->
    ".unmarshal()";

unMarshalFun(_G, _N, _X, T) when is_record(T, string) ->
    ".read_string()";

unMarshalFun(_G, _N, _X, T) when is_record(T, wstring) ->  %% WSTRING
    ".read_string()";

unMarshalFun(_G, _N, _X, T) when is_record(T, struct) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangTuple)";

unMarshalFun(_G, _N, _X, T) when is_record(T, union) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangTuple)";

unMarshalFun(_G, _N, _X, T) when is_record(T, sequence) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlanglist)";

unMarshalFun(_G, _N, _X, T) when is_record(T, enum) ->
    ".unmarshal((" ++ ?ERLANGPACKAGE ++ "OtpErlangAtom)";

unMarshalFun(_G, _N, _X, {boolean, _}) ->
    ".read_boolean()";

unMarshalFun(_G, _N, _X, {octet, _}) ->
    ".read_byte()";

unMarshalFun(_G, _N, _X, {void, _}) ->
    "";

unMarshalFun(_G, _N, _X, {unsigned, U}) ->
    case U of
	{short,_} ->
	    ".read_ushort()";
	{long,_} ->
	    ".read_uint()";
	{'long long',_} ->
	    ".read_ulong()"
    end;

unMarshalFun(_G, _N, _X, {short, _}) ->
    ".read_short()";
unMarshalFun(_G, _N, _X, {long, _}) ->
    ".read_int()";
unMarshalFun(_G, _N, _X, {'long long', _}) ->
    ".read_long()";
unMarshalFun(_G, _N, _X, {float, _}) ->
    ".read_float()";
unMarshalFun(_G, _N, _X, {double, _}) ->
    ".read_double()";
unMarshalFun(_G, _N, _X, {char, _}) ->
    ".read_char()";
unMarshalFun(_G, _N, _X, {wchar, _}) ->  %% WCHAR
    ".read_char()".





%%-----------------------------------------------------------------
%% Func: getFullType/4 - /3
%%
%% Note : Similar to the getType/3 with the major difference 
%%        thet on arrays and sequences it will also declare
%%        their sizes. Used for "new" declarations
%%
%%-----------------------------------------------------------------


getFullType(G, N, X, T) when is_record(X, typedef) andalso is_record(T, array) -> 
    FullDim = 
	tk2FullType(G,N,X,ic_forms:get_tk(X)) ++
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, X, T) when is_record(X, member) andalso is_record(T, array) -> 
    FullDim = 
	getFullType(G, N, ic_forms:get_type(X)) ++ 
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, X, T) when is_record(X, case_dcl) andalso is_record(T, array) -> 
    FullDim = 
	getFullType(G, N, ic_forms:get_type(X)) ++ 
	getFullDim(G,N,T#array.size),
    fixArrayDims(FullDim);

getFullType(G, N, _X, T)  ->
    getFullType(G, N, T).



getFullType(G, N, T) when is_record(T, scoped_id) ->
    {FullScopedName, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, T),
    case TK of
	{tk_array,_,_} ->
	    tk2FullType(G,N,T,TK);
	{tk_sequence,_,_} ->
	    tk2FullType(G,N,T,TK);
	_ ->
	    case isBasicType(G,N,TK) of
		true ->
		    tk2FullType(G,N,T,TK);
		false ->
		    %% Other types
		    ic_code:get_basetype(G, ic_util:to_dot(G,FullScopedName)) 
	    end
    end;

getFullType(G, N, T) when is_record(T, sequence) ->
    fixSeqDims(getType(G,N,T),"_length");

getFullType(G, N, T)  ->
    getType(G, N, T).



%% In order to make a legal declaration 
%% of an assignable array, the dimensions 
%% of empty array sequences are swifted to 
%% the end of the type
fixArrayDims(Cs) ->
    fixArrayDims(Cs,[],[]).

fixArrayDims([],Fulls,Emptys) ->
    lists:reverse(Fulls) ++ Emptys;
fixArrayDims([91,93|Rest],Fulls,Emptys) ->
    fixArrayDims(Rest,Fulls,[91,93|Emptys]);
fixArrayDims([C|Rest],Fulls,Emptys) ->
    fixArrayDims(Rest,[C|Fulls],Emptys).


%% In order to make a legal declaration 
%% of an assignable array, the dimensions 
%% of empty array of sequences are swifted 
%% to the end of the type
fixSeqDims(Cs,Length) ->
    fixSeqDims(Cs,Length,[]).

fixSeqDims([],_Length,Found) ->
    lists:reverse(Found);
fixSeqDims([91,93|Rest],Length,Found) when is_list(Length) ->
    lists:reverse([93|lists:reverse(Length)] ++ 
		  [91|Found]) ++ Rest;
fixSeqDims([C|Rest],Length,Found) ->
    fixSeqDims(Rest,Length,[C|Found]).


  
%%-----------------------------------------------------------------
%% Func: inlinedTypes/2
%%-----------------------------------------------------------------
inlinedTypes(PkgName, Type) when is_record(Type, struct) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(PkgName, Type) when is_record(Type, union) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(PkgName, Type) when is_record(Type, enum) ->
    "_" ++ PkgName ++ ".";
inlinedTypes(_, _) ->
    "".

%%-----------------------------------------------------------------
%% Func: marshalFun/4
%%-----------------------------------------------------------------
marshalFun(G, N, X, Type) ->
    case isBasicType(G, N, Type) of
	true ->
	    ".write_" ++ getMarshalType(G, N, X, Type);
	_ ->
	    getMarshalType(G, N, X, Type) ++ ".marshal"
    end.


%%-----------------------------------------------------------------
%% Func: isBasicType/3
%%-----------------------------------------------------------------
isBasicType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isBasicType(ictype:fetchType(TK));

isBasicType(G, N, X) when is_record(X, member) ->
    if is_record(hd(element(3,X)), array) ->
	    false;
       true ->
	    isBasicType(G, N, element(2,X))
    end;

isBasicType(_G, _N, {unsigned, {long, _}} ) -> 
    true;

isBasicType(_G, _N, {unsigned, {short, _}} ) -> 
    true;

isBasicType(_G, _N, {unsigned, {'long long', _}} ) -> 
    true;

isBasicType(_G, _N, {'long long', _} ) -> 
    true;

isBasicType(_G, _N, {Type, _} ) -> 
    isBasicType(Type);

isBasicType(_G, _N, Type) ->
    isBasicType(Type).


%%-----------------------------------------------------------------
%% Func: isBasicType/1
%%-----------------------------------------------------------------

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
		  tk_wchar,wchar,         %% WCHAR
		  tk_octet,octet,
		  tk_wstring,wstring,     %% WSTRING
		  tk_string,string]).

%% returns true if the Type is a java elementary type
isJavaElementaryType( Type ) ->
    lists:member(Type,
		 [byte, char, wchar, boolean, 
		  int, short, long, 'long long', float, double]).

%%-----------------------------------------------------------------
%% Func: isIntegerType/3
%%-----------------------------------------------------------------
isIntegerType(G, N, S) when element(1, S) == scoped_id -> 
    {_, _, TK, _} = ic_symtab:get_full_scoped_name(G, N, S),
    isIntegerType(ictype:fetchType(TK));
isIntegerType(_G, _N, {unsigned, {long, _}} ) -> 
    true;
isIntegerType(_G, _N, {unsigned, {short, _}} ) -> 
    true;
isIntegerType(_G, _N, {unsigned, {'long long', _}} ) -> 
    true;
isIntegerType(_G, _N, {'long long', _} ) -> 
    true;
isIntegerType(_G, _N, {Type, _} ) -> 
    isIntegerType(Type);
isIntegerType(_G, _N, Type) ->
    isIntegerType(Type).

%%-----------------------------------------------------------------
%% Func: isIntegerType/1
%%-----------------------------------------------------------------

isIntegerType( Type ) ->
    lists:member(Type,
		 [tk_short,short,
		  tk_long,long,
		  tk_longlong,longlong,    %% LLONG
		  tk_ushort,ushort,
		  tk_ulong,ulong,
		  tk_ulonglong,ulonglong,  %% ULLONG
		  tk_char,char,
		  tk_wchar,wchar,          %% WCHAR 
		  tk_octet,octet]).



%%-----------------------------------------------------------------
%% Func: isTerm/3
%%-----------------------------------------------------------------
isTermType(G, N, T) ->
    case getType(G,N,T) of
	"com.ericsson.otp.ic.Term" ->
	    true;
	_ ->
	    false
    end.




%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------


%% Changes the typecode to the 
%% corresponding "basic" type
tk2type(_G,_N,_X,{'tk_struct', _IFRId, "port", _ElementList}) ->
    ?ICPACKAGE ++ "Port";
tk2type(_G,_N,_X,{'tk_struct', _IFRId, "pid", _ElementList}) ->
    ?ICPACKAGE ++ "Pid";
tk2type(_G,_N,_X,{'tk_struct', _IFRId, "ref", _ElementList}) ->
    ?ICPACKAGE ++ "Ref";
tk2type(_G,_N,_X,{'tk_struct', _IFRId, "term", _ElementList}) ->
    ?ICPACKAGE ++ "Term";
tk2type(_G,_N,_X,{'tk_string', _}) -> 
    "java.lang.String";
tk2type(_G,_N,_X,{'tk_wstring', _}) ->  %% WSTRING 
    "java.lang.String";
tk2type(G,N,X,{'tk_array', ElemTC, Dim}) -> 
    tkarr2decl(G,N,X,{'tk_array', ElemTC, Dim});
tk2type(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength}) -> 
    tkseq2decl(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength});
tk2type(G,N,_X,{'tk_struct', IFRId, Name, _ElementList}) ->
    ScopedId=
	lists:reverse(string:tokens(lists:nth(2,string:tokens(IFRId,":")),"/")),

    case ic_forms:clean_up_scope([Name|N]) of
	ScopedId ->
	    %% Right path, use N instead
	    ic_util:to_dot(G,[Name|N]);
	_ ->
	    %% Ugly work arround 
	    ic_util:to_dot(G,ScopedId)
    end;
tk2type(G,N,_X,{'tk_union', IFRId, Name, _, _, _ElementList}) ->
    ScopedId=
	lists:reverse(string:tokens(lists:nth(2,string:tokens(IFRId,":")),"/")),

    case ic_forms:clean_up_scope([Name|N]) of
	ScopedId ->
	    %% Right path, use N instead
	    ic_util:to_dot(G,[Name|N]);
	_ ->
	    %% Ugly work arround 
	    ic_util:to_dot(G,ScopedId)
    end;
tk2type(_G,_N,_X,{'tk_enum', _Id, Name, _ElementList}) -> 
    Name;
tk2type(_G,_N,_X,tk_void) ->
    "void";
tk2type(_G,_N,_X,tk_long) ->
    "int";
tk2type(_G,_N,_X,tk_longlong) ->  %% LLONG
    "long";
tk2type(_G,_N,_X,tk_short) ->
    "short";
tk2type(_G,_N,_X,tk_ulong) ->
    "int";
tk2type(_G,_N,_X,tk_ulonglong) ->  %% ULLONG
    "long";
tk2type(_G,_N,_X,tk_ushort) ->
    "short";
tk2type(_G,_N,_X,tk_float) ->
    "float";
tk2type(_G,_N,_X,tk_double) ->
    "double";
tk2type(_G,_N,_X,tk_boolean) ->
    "boolean";
tk2type(_G,_N,_X,tk_char) ->
    "char";
tk2type(_G,_N,_X,tk_wchar) ->   %% WCHAR
    "char";
tk2type(_G,_N,_X,tk_octet) ->
    "byte";
tk2type(_G,_N,_X,tk_string) ->
    "java.lang.String";
tk2type(_G,_N,_X,tk_wstring) ->  %% WSTRING
    "java.lang.String";
tk2type(_G,_N,_X,tk_any) ->
    ?ICPACKAGE ++ "Any";
tk2type(_G,_N,_X,tk_term) ->     %% Term
    ?ICPACKAGE ++ "Term".

%% Changes the sequence typecode to the 
%% corresponding "basic" structure
tkseq2decl(G,N,X,TKSeq) ->
    tkseq2decl2(G,N,X,TKSeq,[],[]).

tkseq2decl2(G,N,X,{tk_sequence,E,D},[],Ds) ->
    tkseq2decl2(G,N,X,E,[],[D|Ds]);
tkseq2decl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2type(G,N,X,TkEl),
    ElName ++ getdim(Ds).

%% Changes the array typecode to the 
%% corresponding "basic" structure
tkarr2decl(G,N,X,TKArr) ->
    tkarr2decl2(G,N,X,TKArr,[],[]).

tkarr2decl2(G,N,X,{tk_array,E,D},[],Ds) ->
    tkarr2decl2(G,N,X,E,[],[D|Ds]);
tkarr2decl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2type(G,N,X,TkEl),
    ElName ++ getdim(Ds).

getdim([]) ->
    "";
getdim([_D|Ds]) ->
    getdim(Ds) ++ "[]".



%% Changes the typecode to the corresponding "basic" type
%% used for variable declarations where arrays and sequences 
%% are declared with there full dimensions 
tk2FullType(G,N,X,{'tk_array', ElemTC, Dim}) -> 
    tkarr2FullDecl(G,N,X,{'tk_array', ElemTC, Dim});
tk2FullType(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength}) -> 
    tkseq2FullDecl(G,N,X,{'tk_sequence', ElemTC, MaxLsextractength});
tk2FullType(G,N,X,TK) ->
    tk2type(G,N,X,TK).


%% Changes the sequence typecode to the 
%% corresponding "basic" structure here
%% arrays and sequences are declared with 
%% their full dimensions 
tkseq2FullDecl(G,N,X,TKSeq) ->
    tkseq2FullDecl2(G,N,X,TKSeq,[],[]).

tkseq2FullDecl2(G,N,X,{tk_sequence,E,D},[],Ds) ->
    tkseq2FullDecl2(G,N,X,E,[],[D|Ds]);
tkseq2FullDecl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2FullType(G,N,X,TkEl),
    ElName ++ getdim(Ds).

%% Changes the array typecode to the 
%% corresponding "basic" structure
tkarr2FullDecl(G,N,X,TKArr) ->
    tkarr2FullDecl2(G,N,X,TKArr,[],[]).

tkarr2FullDecl2(G,N,X,{tk_array,E,D},[],Ds) ->
    tkarr2FullDecl2(G,N,X,E,[],[D|Ds]);
tkarr2FullDecl2(G,N,X,TkEl,[],Ds) ->
    ElName = tk2FullType(G,N,X,TkEl),
    ElName ++ getFullDim(G,N,Ds).

getFullDim(_G,_N,[]) ->
    "";
getFullDim(G,N,[D|Ds]) when is_record(D,scoped_id) ->
    {FSN, _, _, _} = ic_symtab:get_full_scoped_name(G, N, D),
    "[" ++ ic_util:to_dot(G,FSN) ++ "]" ++ getFullDim(G,N,Ds);
getFullDim(G,N,[D|Ds]) when is_integer(D) ->
    "[" ++ integer_to_list(D) ++ "]" ++ getFullDim(G,N,Ds);
getFullDim(G,N,[D|Ds]) when is_tuple(D) ->
    "[" ++ ic_util:eval_java(G,N,D) ++ "]" ++ getFullDim(G,N,Ds).



%% Constructs an array empty dimension string
%% used for array variable declaration
arrayEmptyDim(X) ->
    arrayEmptyDim2(X#array.size).

arrayEmptyDim2([_D]) ->
    "[]";
arrayEmptyDim2([_D |Ds]) ->
    "[]" ++ arrayEmptyDim2(Ds).



