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

-module(ic_util).


-include("icforms.hrl").
-include("ic.hrl").
-include("ic_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------

-export([mk_align/1, mk_list/1, join/2, chain/2, mk_name/2,
	 mk_OE_name/2, mk_oe_name/2, mk_var/1]).

-export([to_atom/1, to_colon/1, to_list/1, to_undersc/1, to_dot/1,
	 to_dot/2]).  
-export([to_uppercase/1, adjustScopeToJava/2, eval_java/3, eval_c/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------

%% mk_list produces a nice comma separated string of variable names
mk_list([]) -> [];
mk_list([Arg | Args]) ->
    Arg ++ mk_list2(Args).
mk_list2([Arg | Args]) ->
    ", " ++ Arg ++ mk_list2(Args);
mk_list2([]) -> [].

%% Produce a list of items separated by S.
join([E1, E2| Es], S) ->
        [E1, S| join([E2| Es], S)];
join([E], _) ->
    [E];
join([], _) ->
    [].

%% Produce a list of items, each terminated by T.
chain([E| Es], T) ->
    [E, T| chain(Es, T)];
chain([], _) ->
    [].


%% Shall convert a string to a Erlang variable name (Capitalise)
mk_var( [N | Str] ) when N >= $a, N =< $z ->
    [ N+$A-$a | Str ];
mk_var( [N | Str] )  when N >= $A, N =< $Z -> [N | Str].
    
%% Shall produce a "public" name for name. When we introduce new
%% identifiers in the mapping that must not collide with those from
%% the IDL spec.
%%
%% NOTE: Change name of IFR ID in system exceptions in corba.hrl when
%% prefix is changed here.
%%
mk_name(_Gen, Name) -> lists:flatten(["OE_" | Name]).
mk_OE_name(_Gen, Name) -> lists:flatten(["OE_" | Name]).
mk_oe_name(_Gen, Name) -> lists:flatten(["oe_" | Name]).

mk_align(String) ->
    io_lib:format("OE_ALIGN(~s)",[String]).

to_atom(A) when is_atom(A) -> A;
to_atom(L) when is_list(L) -> list_to_atom(L).

to_list(A) when is_list(A) -> A;
to_list(L) when is_atom(L) -> atom_to_list(L);
to_list(X) when is_integer(X) -> integer_to_list(X).



%% Produce a colon (or under score) separated string repr of the name
%% X
%%
to_colon(X) when element(1, X) == scoped_id ->
    to_colon2(ic_symtab:scoped_id_strip(X));
to_colon(L) -> to_colon2(L).

to_colon2([X]) -> X;
to_colon2([X | Xs]) -> to_colon2(Xs) ++ "::" ++ X;
to_colon2([]) -> "".


to_undersc(X) when element(1, X) == scoped_id ->
    to_undersc2(ic_symtab:scoped_id_strip(X));
to_undersc(L) -> to_undersc2(L).

to_undersc2([X]) -> X;
to_undersc2([X | Xs]) -> to_undersc2(Xs) ++ "_" ++ X;
to_undersc2([]) -> "".


%% Z is a single name
to_uppercase(Z) ->
    lists:map(fun(X) when X>=$a, X=<$z -> X-$a+$A;
		 (X) -> X end, Z).


%%
to_dot(X) when element(1, X) == scoped_id ->
    to_dotLoop(ic_symtab:scoped_id_strip(X));
to_dot(L) -> to_dotLoop(L).

to_dotLoop([X]) -> ic_forms:get_java_id(X);
to_dotLoop([X | Xs]) -> to_dotLoop(Xs) ++ "." ++ ic_forms:get_java_id(X);
to_dotLoop([]) -> "".



%%
to_dot(G,X) when element(1, X) == scoped_id ->
    S = ic_genobj:pragmatab(G),
    ScopedId = ic_symtab:scoped_id_strip(X),
    case isConstScopedId(S, ScopedId) of  %% Costants are left as is
	true ->
	    to_dotLoop(ScopedId) ++ addDotValue(S, ScopedId);
	false ->
	    to_dotLoop(S,ScopedId)
    end;
to_dot(G,ScopedId) -> 
    S = ic_genobj:pragmatab(G),
    case isConstScopedId(S, ScopedId) of  %% Costants are left as is
	true ->
	    to_dotLoop(ScopedId) ++ addDotValue(S, ScopedId);
	false ->
	    to_dotLoop(S,ScopedId)
    end.

addDotValue(S, [_C | Ss]) ->
    case isInterfaceScopedId(S, Ss) of
	true ->
	    "";
	false ->
	    ".value"
    end.
       
to_dotLoop(S,[X]) -> 
    case isInterfaceScopedId(S, [X]) of
	true ->
	    ic_forms:get_java_id(X) ++ "Package";
	false ->
	    ic_forms:get_java_id(X)
    end;
to_dotLoop(S,[X | Xs]) ->
    case isInterfaceScopedId(S, [X | Xs]) of
	true ->
	    to_dotLoop(S,Xs) ++ "." ++ ic_forms:get_java_id(X) ++ "Package";
	false ->
	    to_dotLoop(S,Xs) ++ "." ++ ic_forms:get_java_id(X)
    end;
to_dotLoop(_S,[]) -> "".

isInterfaceScopedId(_S,[]) ->
    false;
isInterfaceScopedId(S,[X|Xs]) ->
    case ets:match(S,{file_data_local,'_','_',interface,Xs,X,'_','_','_'}) of
	[] ->
	    case ets:match(S,{file_data_included,'_','_',interface,Xs,X,'_','_','_'}) of
		[] ->
		    false;
		_ ->
		    true
	    end;
	_ ->
	    true
    end.

isConstScopedId(_S,[]) ->
    false;
isConstScopedId(S,[X|Xs]) ->
    case ets:match(S,{file_data_local,'_','_',const,Xs,X,'_','_','_'}) of
	[] ->
	    case ets:match(S,{file_data_included,'_','_',const,Xs,X,'_','_','_'}) of
		[] ->
		    false;
		_ ->
		    true
	    end;
	_ ->
	    true
    end.



%%
adjustScopeToJava(G,X) when element(1, X) == scoped_id ->
    S = ic_genobj:pragmatab(G),
    ScopedId = ic_symtab:scoped_id_strip(X),
    case isConstScopedId(S, ScopedId) of  %% Costants are left as is
	true ->
	    ic_forms:get_java_id(ScopedId);
	false ->
	    adjustScopeToJavaLoop(S,ScopedId)
    end;
adjustScopeToJava(G,ScopedId) -> 
    S = ic_genobj:pragmatab(G),
    case isConstScopedId(S, ScopedId) of  %% Costants are left as is
	true ->
	    ic_forms:get_java_id(ScopedId);
	false ->
	    adjustScopeToJavaLoop(S,ScopedId)
    end.



adjustScopeToJavaLoop(_S,[]) -> 
    [];
adjustScopeToJavaLoop(S,[X | Xs]) ->
    case isInterfaceScopedId(S, [X | Xs]) of
	true ->
	    [ic_forms:get_java_id(X) ++ "Package" | adjustScopeToJavaLoop(S,Xs)];
	false ->
	    [ic_forms:get_java_id(X) | adjustScopeToJavaLoop(S,Xs)]
    end.


%%
%%  Expression evaluator for java
%%
%% Well, this is not an evaluator, it just 
%% prints the hole operation, sorry.
%%
eval_java(G,N,Arg) when is_record(Arg, scoped_id) ->
    {FSN, _, _, _} = 
	ic_symtab:get_full_scoped_name(G, N, Arg),
    ic_util:to_dot(G,FSN);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<integer_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<character_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<wcharacter_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<boolean_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<floating_pt_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<string_literal>' ->
    element(3,Arg);
eval_java(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<wstring_literal>' ->
    element(3,Arg);
eval_java(G,N,{Op,Arg1,Arg2}) ->
    "(" ++ eval_java(G,N,Arg1) ++ 
	ic_forms:get_java_id(Op) ++ 
	eval_java(G,N,Arg2) ++ ")".



%%
%%  Expression evaluator for c
%%
%% Well, this is not an evaluator, it just 
%% prints the hole operation, sorry.
%%
eval_c(G,N,Arg) when is_record(Arg, scoped_id) ->
    {FSN, _, _, _} = 
	ic_symtab:get_full_scoped_name(G, N, Arg),
    ic_util:to_undersc(FSN);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<integer_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<character_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<wcharacter_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<boolean_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<floating_pt_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<string_literal>' ->
    element(3,Arg);
eval_c(_G,_N,Arg) when is_tuple(Arg) andalso element(1,Arg) == '<wstring_literal>' ->
    element(3,Arg);
eval_c(G,N,{Op,Arg1,Arg2}) ->
    "(" ++ eval_c(G,N,Arg1) ++ 
	atom_to_list(Op) ++ 
	eval_c(G,N,Arg2) ++ ")".


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------






