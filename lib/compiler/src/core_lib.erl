%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%% Purpose: Core Erlang abstract syntax functions.

-module(core_lib).

-export([make_values/1]).
-export([is_var_used/2]).

-include("core_parse.hrl").

%% Make a suitable values structure, expr or values, depending on Expr.
-spec make_values([cerl:cerl()] | cerl:cerl()) -> cerl:cerl().

make_values([E]) -> E;
make_values([H|_]=Es) -> #c_values{anno=cerl:get_ann(H),es=Es};
make_values([]) -> #c_values{es=[]};
make_values(E) -> E.

%% Test if the variable VarName is used in Expr.
-spec is_var_used(cerl:var_name(), cerl:cerl()) -> boolean().

is_var_used(V, B) -> vu_expr(V, B).

vu_expr(V, #c_values{es=Es}) ->
    vu_expr_list(V, Es);
vu_expr(V, #c_var{name=V2}) -> V =:= V2;
vu_expr(V, #c_alias{var=V2,pat=Pat}) ->
    %% XXX Must handle aliases in expressions because of sys_core_fold:kill_types/2,
    %% that uses a pattern as if it was an expression.
    V =:= V2 orelse vu_expr(V, Pat);
vu_expr(_, #c_literal{}) -> false;
vu_expr(V, #c_cons{hd=H,tl=T}) ->
    vu_expr(V, H) orelse vu_expr(V, T);
vu_expr(V, #c_tuple{es=Es}) ->
    vu_expr_list(V, Es);
vu_expr(V, #c_map{arg=M,es=Es}) ->
    vu_expr(V, M) orelse vu_expr_list(V, Es);
vu_expr(V, #c_map_pair{key=Key,val=Val}) ->
    vu_expr_list(V, [Key,Val]);
vu_expr(V, #c_binary{segments=Ss}) ->
    vu_seg_list(V, Ss);
vu_expr(V, #c_fun{vars=Vs,body=B}) ->
    %% Variables in fun shadow previous variables
    case vu_var_list(V, Vs) of
	true -> false;
	false -> vu_expr(V, B)
    end;
vu_expr(V, #c_let{vars=Vs,arg=Arg,body=B}) ->
    case vu_expr(V, Arg) of
	true -> true;
	false ->
	    %% Variables in let shadow previous variables.
	    case vu_var_list(V, Vs) of
		true -> false;
		false -> vu_expr(V, B)
	    end
    end;
vu_expr(V, #c_letrec{defs=Fs,body=B}) ->
    lists:any(fun ({_,Fb}) -> vu_expr(V, Fb) end, Fs) orelse vu_expr(V, B);
vu_expr(V, #c_seq{arg=Arg,body=B}) ->
    vu_expr(V, Arg) orelse vu_expr(V, B);
vu_expr(V, #c_case{arg=Arg,clauses=Cs}) ->
    vu_expr(V, Arg) orelse vu_clauses(V, Cs);
vu_expr(V, #c_apply{op=Op,args=As}) ->
    vu_expr_list(V, [Op|As]);
vu_expr(V, #c_call{module=M,name=N,args=As}) ->
    vu_expr_list(V, [M,N|As]);
vu_expr(V, #c_primop{args=As}) ->		%Name is an atom
    vu_expr_list(V, As);
vu_expr(V, #c_catch{body=B}) ->
    vu_expr(V, B);
vu_expr(V, #c_try{arg=E,vars=Vs,body=B,evars=Evs,handler=H}) ->
    case vu_expr(V, E) of
	true -> true;
	false ->
	    %% Variables shadow previous ones.
	    case case vu_var_list(V, Vs) of
		     true -> false;
		     false -> vu_expr(V, B)
		 end of
		true -> true;
		false ->
		    case vu_var_list(V, Evs) of
			true -> false;
			false -> vu_expr(V, H)
		    end
	    end
    end.

vu_expr_list(V, Es) ->
    lists:any(fun(E) -> vu_expr(V, E) end, Es).

vu_seg_list(V, Ss) ->
    lists:any(fun (#c_bitstr{val=Val,size=Size}) ->
		      vu_expr(V, Val) orelse vu_expr(V, Size)
	      end, Ss).

-spec vu_clause(cerl:var_name(), cerl:c_clause()) -> boolean().

vu_clause(V, #c_clause{pats=Ps,guard=G,body=B}) ->
    vu_pattern_list(V, Ps) orelse  vu_expr(V, G) orelse vu_expr(V, B).

-spec vu_clauses(cerl:var_name(), [cerl:c_clause()]) -> boolean().

vu_clauses(V, Cs) ->
    lists:any(fun(C) -> vu_clause(V, C) end, Cs).

%% vu_pattern(VarName, Pattern) -> Used.
%% vu_pattern_list(VarName, [Pattern]) -> Used.
%%  Binary and map patterns can use variables.

vu_pattern(V, #c_var{name=V2}) ->
    V =:= V2;
vu_pattern(V, #c_cons{hd=H,tl=T}) ->
    vu_pattern(V, H) orelse vu_pattern(V, T);
vu_pattern(V, #c_tuple{es=Es}) ->
    vu_pattern_list(V, Es);
vu_pattern(V, #c_binary{segments=Ss}) ->
    vu_pat_seg_list(V, Ss);
vu_pattern(V, #c_map{es=Es}) ->
    vu_map_pairs(V, Es);
vu_pattern(V, #c_alias{var=Var,pat=P}) ->
    vu_pattern(V, Var) orelse vu_pattern(V, P);
vu_pattern(_V, #c_literal{}) -> false.

vu_pattern_list(V, Ps) ->
    lists:any(fun(P) -> vu_pattern(V, P) end, Ps).

vu_pat_seg_list(V, Ss) ->
    lists:any(fun(#c_bitstr{size=Size}) ->
                      vu_pattern(V, Size)
              end, Ss).

vu_map_pairs(V, [#c_map_pair{key=Key,val=Pat}|T]) ->
    vu_expr(V, Key) orelse
        vu_pattern(V, Pat) orelse
        vu_map_pairs(V, T);
vu_map_pairs(_, []) -> false.

-spec vu_var_list(cerl:var_name(), [cerl:c_var()]) -> boolean().

vu_var_list(V, Vs) ->
    lists:any(fun (#c_var{name=V2}) -> V =:= V2 end, Vs).
