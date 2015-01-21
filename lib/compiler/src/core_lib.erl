%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%% Purpose: Core Erlang abstract syntax functions.

-module(core_lib).

-export([get_anno/1,set_anno/2]).
-export([is_literal/1,is_literal_list/1]).
-export([literal_value/1]).
-export([make_values/1]).
-export([is_var_used/2]).

-include("core_parse.hrl").

%%
%% Generic get/set annotation that should be used only with cerl() structures.
%%
-spec get_anno(cerl:cerl()) -> term().

get_anno(C) -> element(2, C).

-spec set_anno(cerl:cerl(), term()) -> cerl:cerl().

set_anno(C, A) -> setelement(2, C, A).

-spec is_literal(cerl:cerl()) -> boolean().

is_literal(#c_literal{}) -> true;
is_literal(#c_cons{hd=H,tl=T}) ->
    is_literal(H) andalso is_literal(T);
is_literal(#c_tuple{es=Es}) -> is_literal_list(Es);
is_literal(#c_binary{segments=Es}) -> is_lit_bin(Es);
is_literal(_) -> false.

-spec is_literal_list([cerl:cerl()]) -> boolean().

is_literal_list(Es) -> lists:all(fun is_literal/1, Es).

is_lit_bin(Es) ->
    lists:all(fun (#c_bitstr{val=E,size=S}) ->
		      is_literal(E) andalso is_literal(S)
	      end, Es).

%% Return the value of LitExpr.
-spec literal_value(cerl:c_literal() | cerl:c_binary() |
		    cerl:c_map() | cerl:c_cons() | cerl:c_tuple()) -> term().

literal_value(#c_literal{val=V}) -> V;
literal_value(#c_binary{segments=Es}) ->
    list_to_binary([literal_value_bin(Bit) || Bit <- Es]);
literal_value(#c_cons{hd=H,tl=T}) ->
    [literal_value(H)|literal_value(T)];
literal_value(#c_tuple{es=Es}) ->
    list_to_tuple(literal_value_list(Es));
literal_value(#c_map{arg=Cm,es=Cmps}) ->
    M = literal_value(Cm),
    lists:foldl(fun(#c_map_pair{ key=Ck, val=Cv },Mi) ->
		K = literal_value(Ck),
		V = literal_value(Cv),
		maps:put(K,V,Mi)
	end, M, Cmps).

literal_value_list(Vals) -> [literal_value(V) || V <- Vals].

literal_value_bin(#c_bitstr{val=Val,size=Sz,unit=U,type=T,flags=Fs}) ->
    %% We will only handle literals constructed by make_literal/1.
    %% Could be made more general in the future if the need arises.
    8 = literal_value(Sz),
    1 = literal_value(U),
    integer = literal_value(T),
    [unsigned,big] = literal_value(Fs),
    literal_value(Val).

%% Make a suitable values structure, expr or values, depending on Expr.
-spec make_values([cerl:cerl()] | cerl:cerl()) -> cerl:cerl().

make_values([E]) -> E;
make_values([H|_]=Es) -> #c_values{anno=get_anno(H),es=Es};
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
vu_expr(V, #c_receive{clauses=Cs,timeout=T,action=A}) ->
    vu_clauses(V, Cs) orelse vu_expr(V, T) orelse vu_expr(V, A);
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

%% Have to get the pattern results right.

-spec vu_clause(cerl:var_name(), cerl:c_clause()) -> boolean().

vu_clause(V, #c_clause{pats=Ps,guard=G,body=B}) ->
    case vu_pattern_list(V, Ps) of
	{true,_Shad} -> true;			%It is used
	{false,true} -> false;			%Shadowed
	{false,false} ->			%Not affected
	    %% Neither used nor shadowed. Check guard and body.
	    vu_expr(V, G) orelse vu_expr(V, B)
    end.

-spec vu_clauses(cerl:var_name(), [cerl:c_clause()]) -> boolean().

vu_clauses(V, Cs) ->
    lists:any(fun(C) -> vu_clause(V, C) end, Cs).

%% vu_pattern(VarName, Pattern) -> {Used,Shadow}.
%% vu_pattern_list(VarName, [Pattern]) -> {Used,Shadow}.
%%  Binaries complicate patterns as a variable can both be properly
%%  used, in a bit segment size, and shadow.  They can also do both.
 
%% vu_pattern(V, Pat) -> vu_pattern(V, Pat, {false,false}).

vu_pattern(V, #c_var{name=V2}, {Used,_}) ->
    {Used,V =:= V2};
vu_pattern(V, #c_cons{hd=H,tl=T}, St0) ->
    case vu_pattern(V, H, St0) of
	{true,_}=St1 -> St1;			%Nothing more to know
	St1 -> vu_pattern(V, T, St1)
    end;
vu_pattern(V, #c_tuple{es=Es}, St) ->
    vu_pattern_list(V, Es, St);
vu_pattern(V, #c_binary{segments=Ss}, St) ->
    vu_pat_seg_list(V, Ss, St);
vu_pattern(V, #c_map{es=Es}, St) ->
    vu_map_pairs(V, Es, St);
vu_pattern(V, #c_alias{var=Var,pat=P}, St0) ->
    case vu_pattern(V, Var, St0) of
	{true,_}=St1 -> St1;
	St1 -> vu_pattern(V, P, St1)
    end;
vu_pattern(_, _, St) -> St.

vu_pattern_list(V, Ps) -> vu_pattern_list(V, Ps, {false,false}).

vu_pattern_list(V, Ps, St0) ->
    lists:foldl(fun(P, St) -> vu_pattern(V, P, St) end, St0, Ps).

vu_pat_seg_list(V, Ss, St) ->
    lists:foldl(fun(_, {true,_}=St0) -> St0;
		   (#c_bitstr{val=Val,size=Size}, St0) ->
			case vu_pattern(V, Val, St0) of
			    {true,_}=St1 -> St1;
			    {false,Shad} ->
				{vu_expr(V, Size),Shad}
			end
		end, St, Ss).

vu_map_pairs(V, [#c_map_pair{val=Pat}|T], St0) ->
    case vu_pattern(V, Pat, St0) of
	{true,_}=St -> St;
	St -> vu_map_pairs(V, T, St)
    end;
vu_map_pairs(_, [], St) -> St.

-spec vu_var_list(cerl:var_name(), [cerl:c_var()]) -> boolean().

vu_var_list(V, Vs) ->
    lists:any(fun (#c_var{name=V2}) -> V =:= V2 end, Vs).
