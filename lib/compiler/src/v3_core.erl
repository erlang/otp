%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2010. All Rights Reserved.
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
%% Purpose : Transform normal Erlang to Core Erlang

%% At this stage all preprocessing has been done. All that is left are
%% "pure" Erlang functions.
%%
%% Core transformation is done in three stages:
%%
%% 1. Flatten expressions into an internal core form without doing
%%    matching.
%%
%% 2. Step "forwards" over the icore code annotating each "top-level"
%%    thing with variable usage.  Detect bound variables in matching
%%    and replace with explicit guard test.  Annotate "internal-core"
%%    expressions with variables they use and create.  Convert matches
%%    to cases when not pure assignments.
%%
%% 3. Step "backwards" over icore code using variable usage
%%    annotations to change implicit exported variables to explicit
%%    returns.
%%
%% To ensure the evaluation order we ensure that all arguments are
%% safe.  A "safe" is basically a core_lib simple with VERY restricted
%% binaries.
%%
%% We have to be very careful with matches as these create variables.
%% While we try not to flatten things more than necessary we must make
%% sure that all matches are at the top level.  For this we use the
%% type "novars" which are non-match expressions.  Cases and receives
%% can also create problems due to exports variables so they are not
%% "novars" either.  I.e. a novars will not export variables.
%%
%% Annotations in the #iset, #iletrec, and all other internal records
%% is kept in a record, #a, not in a list as in proper core.  This is
%% easier and faster and creates no problems as we have complete control
%% over all annotations.
%%
%% On output, the annotation for most Core Erlang terms will contain
%% the source line number. A few terms will be marked with the atom
%% atom 'compiler_generated', to indicate that the compiler has generated
%% them and that no warning should be generated if they are optimized
%% away.
%% 
%%
%% In this translation:
%%
%% call ops are safes
%% call arguments are safes
%% match arguments are novars
%% case arguments are novars
%% receive timeouts are novars
%% let/set arguments are expressions
%% fun is not a safe

-module(v3_core).

-export([module/2,format_error/1]).

-import(lists, [reverse/1,reverse/2,map/2,member/2,foldl/3,foldr/3,mapfoldl/3,
		splitwith/2,keyfind/3,sort/1,foreach/2]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).
-import(cerl, [ann_c_cons/3,ann_c_cons_skel/3,ann_c_tuple/2,c_tuple/1]).

-include("core_parse.hrl").

%% Internal core expressions and help functions.
%% N.B. annotations fields in place as normal Core expressions.

-record(a, {us=[],ns=[],anno=[]}).		%Internal annotation

-record(iapply,    {anno=#a{},op,args}).
-record(ibinary,   {anno=#a{},segments}).	%Not used in patterns.
-record(icall,     {anno=#a{},module,name,args}).
-record(icase,     {anno=#a{},args,clauses,fc}).
-record(icatch,    {anno=#a{},body}).
-record(iclause,   {anno=#a{},pats,pguard=[],guard,body}).
-record(ifun,      {anno=#a{},id,vars,clauses,fc}).
-record(iletrec,   {anno=#a{},defs,body}).
-record(imatch,    {anno=#a{},pat,guard=[],arg,fc}).
-record(iprimop,   {anno=#a{},name,args}).
-record(iprotect,  {anno=#a{},body}).
-record(ireceive1, {anno=#a{},clauses}).
-record(ireceive2, {anno=#a{},clauses,timeout,action}).
-record(iset,      {anno=#a{},var,arg}).
-record(itry,      {anno=#a{},args,vars,body,evars,handler}).

-type iapply()    :: #iapply{}.
-type ibinary()   :: #ibinary{}.
-type icall()     :: #icall{}.
-type icase()     :: #icase{}.
-type icatch()    :: #icatch{}.
-type iclause()   :: #iclause{}.
-type ifun()      :: #ifun{}.
-type iletrec()   :: #iletrec{}.
-type imatch()    :: #imatch{}.
-type iprimop()   :: #iprimop{}.
-type iprotect()  :: #iprotect{}.
-type ireceive1() :: #ireceive1{}.
-type ireceive2() :: #ireceive2{}.
-type iset()      :: #iset{}.
-type itry()      :: #itry{}.

-type i() :: iapply()   | ibinary()   | icall()     | icase()  | icatch()
           | iclause()  | ifun()      | iletrec()   | imatch() | iprimop()
           | iprotect() | ireceive1() | ireceive2() | iset()   | itry().

-type warning() :: {file:filename(), [{integer(), module(), term()}]}.

-record(core, {vcount=0 :: non_neg_integer(),	%Variable counter
	       fcount=0 :: non_neg_integer(),	%Function counter
	       in_guard=false :: boolean(),	%In guard or not.
	       wanted=true :: boolean(),	%Result wanted or not.
	       opts     :: [compile:option()],	%Options.
	       ws=[]    :: [warning()],		%Warnings.
               file=[{file,""}]}).              %File

%% XXX: The following type declarations do not belong in this module
-type fa()        :: {atom(), arity()}.
-type attribute() :: atom().
-type form()      :: {function, integer(), atom(), arity(), _}
                   | {attribute, integer(), attribute(), _}.

-spec module({module(), [fa()], [form()]}, [compile:option()]) ->
        {'ok',cerl:c_module(),[warning()]}.

module({Mod,Exp,Forms}, Opts) ->
    Cexp = map(fun ({_N,_A} = NA) -> #c_var{name=NA} end, Exp),
    {Kfs0,As0,Ws,_File} = foldl(fun (F, Acc) ->
					form(F, Acc, Opts)
				end, {[],[],[],[]}, Forms),
    Kfs = reverse(Kfs0),
    As = reverse(As0),
    {ok,#c_module{name=#c_literal{val=Mod},exports=Cexp,attrs=As,defs=Kfs},Ws}.

form({function,_,_,_,_}=F0, {Fs,As,Ws0,File}, Opts) ->
    {F,Ws} = function(F0, Ws0, File, Opts),
    {[F|Fs],As,Ws,File};
form({attribute,_,file,{File,_Line}}, {Fs,As,Ws,_}, _Opts) ->
    {Fs,As,Ws,File};
form({attribute,_,_,_}=F, {Fs,As,Ws,File}, _Opts) ->
    {Fs,[attribute(F)|As],Ws,File}.

attribute({attribute,Line,Name,Val}) ->
    {#c_literal{val=Name, anno=[Line]}, #c_literal{val=Val, anno=[Line]}}.

function({function,_,Name,Arity,Cs0}, Ws0, File, Opts) ->
    %%ok = io:fwrite("~p - ", [{Name,Arity}]),
    St0 = #core{vcount=0,opts=Opts,ws=Ws0,file=[{file,File}]},
    {B0,St1} = body(Cs0, Name, Arity, St0),
    %%ok = io:fwrite("1", []),
    %%ok = io:fwrite("~w:~p~n", [?LINE,B0]),
    {B1,St2} = ubody(B0, St1),
    %%ok = io:fwrite("2", []),
    %%ok = io:fwrite("~w:~p~n", [?LINE,B1]),
    {B2,#core{ws=Ws}} = cbody(B1, St2),
    %%ok = io:fwrite("3~n", []),
    %%ok = io:fwrite("~w:~p~n", [?LINE,B2]),
    {{#c_var{name={Name,Arity}},B2},Ws}.

body(Cs0, Name, Arity, St0) ->
    Anno = lineno_anno(element(2, hd(Cs0)), St0),
    {Args,St1} = new_vars(Anno, Arity, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Ps,St3} = new_vars(Arity, St2),    %Need new variables here
    Fc = function_clause(Ps, {Name,Arity}),
    {#ifun{anno=#a{anno=Anno},id=[],vars=Args,clauses=Cs1,fc=Fc},St3}.

%% clause(Clause, State) -> {Cclause,State} | noclause.
%% clauses([Clause], State) -> {[Cclause],State}.
%%  Convert clauses.  Trap bad pattern aliases and remove clause from
%%  clause list.

clauses([C0|Cs0], St0) ->
    case clause(C0, St0) of
	{noclause,St} -> clauses(Cs0, St);
	{C,St1} ->
	    {Cs,St2} = clauses(Cs0, St1),
	    {[C|Cs],St2}
    end;
clauses([], St) -> {[],St}.

clause({clause,Lc,H0,G0,B0}, St0) ->
    try head(H0, St0) of
	H1 ->
	    {G1,St1} = guard(G0, St0),
	    {B1,St2} = exprs(B0, St1),
            Anno = lineno_anno(Lc, St2),
	    {#iclause{anno=#a{anno=Anno},pats=H1,guard=G1,body=B1},St2}
    catch
	throw:nomatch ->
	    St = add_warning(Lc, nomatch, St0),
	    {noclause,St}			%Bad pattern
    end.

clause_arity({clause,_,H0,_,_}) -> length(H0).

%% head([P], State) -> [P].

head(Ps, St) -> pattern_list(Ps, St).

%% guard([Expr], State) -> {[Cexpr],State}.
%%  Build an explict and/or tree of guard alternatives, then traverse
%%  top-level and/or tree and "protect" inner tests.

guard([], St) -> {[],St};
guard(Gs0, St0) ->
    Gs1 = foldr(fun (Gt0, Rhs) ->
			Gt1 = guard_tests(Gt0),
			L = element(2, Gt1),
			{op,L,'or',Gt1,Rhs}
		end, guard_tests(last(Gs0)), first(Gs0)),
    {Gs,St} = gexpr_top(Gs1, St0#core{in_guard=true}),
    {Gs,St#core{in_guard=false}}.
    
guard_tests(Gs) ->
    L = element(2, hd(Gs)),
    {protect,L,foldr(fun (G, Rhs) -> {op,L,'and',G,Rhs} end, last(Gs), first(Gs))}.

%% gexpr_top(Expr, State) -> {Cexpr,State}.
%%  Generate an internal core expression of a guard test.  Explicitly
%%  handle outer boolean expressions and "protect" inner tests in a
%%  reasonably smart way.

gexpr_top(E0, St0) ->
    {E1,Eps0,Bools,St1} = gexpr(E0, [], St0),
    {E,Eps,St} = force_booleans(Bools, E1, Eps0, St1),
    {Eps++[E],St}.

%% gexpr(Expr, Bools, State) -> {Cexpr,[PreExp],Bools,State}.
%%  Generate an internal core expression of a guard test.

gexpr({protect,Line,Arg}, Bools0, St0) ->
    case gexpr(Arg, [], St0) of
	{E0,[],Bools,St1} ->
	    {E,Eps,St} = force_booleans(Bools, E0, [], St1),
	    {E,Eps,Bools0,St};
	{E0,Eps0,Bools,St1} ->
	    {E,Eps,St} = force_booleans(Bools, E0, Eps0, St1),
            Anno = lineno_anno(Line, St),
	    {#iprotect{anno=#a{anno=Anno},body=Eps++[E]},[],Bools0,St}
    end;
gexpr({op,L,'andalso',E1,E2}, Bools, St0) ->
    {#c_var{name=V0},St} = new_var(L, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch_guard(L, E1, V, E2, False),
    gexpr(E, Bools, St);
gexpr({op,L,'orelse',E1,E2}, Bools, St0) ->
    {#c_var{name=V0},St} = new_var(L, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch_guard(L, E1, V, True, E2),
    gexpr(E, Bools, St);
gexpr({op,Line,Op,L,R}=Call, Bools0, St0) ->
    case erl_internal:bool_op(Op, 2) of
	true ->
	    {Le,Lps,Bools1,St1} = gexpr(L, Bools0, St0),
	    {Ll,Llps,St2} = force_safe(Le, St1),
	    {Re,Rps,Bools,St3} = gexpr(R, Bools1, St2),
	    {Rl,Rlps,St4} = force_safe(Re, St3),
	    Anno = lineno_anno(Line, St4),
	    {#icall{anno=#a{anno=Anno},	%Must have an #a{}
		    module=#c_literal{anno=Anno,val=erlang},
		    name=#c_literal{anno=Anno,val=Op},
		    args=[Ll,Rl]},Lps ++ Llps ++ Rps ++ Rlps,Bools,St4};
	false ->
	    gexpr_test(Call, Bools0, St0)
    end;
gexpr({op,Line,Op,A}=Call, Bools0, St0) ->
    case Op of
	'not' ->
	    {Ae0,Aps,Bools,St1} = gexpr(A, Bools0, St0),
	    case Ae0 of
		#icall{module=#c_literal{val=erlang},
		       name=#c_literal{val='=:='},
		       args=[E,#c_literal{val=true}]}=EqCall ->
		    %%
		    %% Doing the following transformation
		    %%    not(Expr =:= true)  ==>  Expr =:= false
		    %% will help eliminating redundant is_boolean/1 tests.
		    %%
		    Ae = EqCall#icall{args=[E,#c_literal{val=false}]},
		    {Al,Alps,St2} = force_safe(Ae, St1),
		    {Al,Aps ++ Alps,Bools,St2};
		Ae ->
		    {Al,Alps,St2} = force_safe(Ae, St1),
		    Anno = lineno_anno(Line, St2),
		    {#icall{anno=#a{anno=Anno},	%Must have an #a{}
			    module=#c_literal{anno=Anno,val=erlang},
			    name=#c_literal{anno=Anno,val=Op},
			    args=[Al]},Aps ++ Alps,Bools,St2}
	    end;
	_ ->
	    gexpr_test(Call, Bools0, St0)
    end;
gexpr(E0, Bools, St0) ->
    gexpr_test(E0, Bools, St0).

%% gexpr_test(Expr, Bools, State) -> {Cexpr,[PreExp],Bools,State}.
%%  Generate a guard test.  At this stage we must be sure that we have
%%  a proper boolean value here so wrap things with an true test if we
%%  don't know, i.e. if it is not a comparison or a type test.

gexpr_test({atom,L,true}, Bools, St0) ->
    {#c_literal{anno=lineno_anno(L, St0),val=true},[],Bools,St0};
gexpr_test({atom,L,false}, Bools, St0) ->
    {#c_literal{anno=lineno_anno(L, St0),val=false},[],Bools,St0};
gexpr_test(E0, Bools0, St0) ->
    {E1,Eps0,St1} = expr(E0, St0),
    %% Generate "top-level" test and argument calls.
    case E1 of
	#icall{anno=Anno,module=#c_literal{val=erlang},name=#c_literal{val=N},args=As} ->
	    Ar = length(As),
	    case erl_internal:type_test(N, Ar) orelse
		erl_internal:comp_op(N, Ar) of
		true -> {E1,Eps0,Bools0,St1};
		false ->
		    Lanno = Anno#a.anno,
		    {New,St2} = new_var(Lanno, St1),
		    Bools = [New|Bools0],
		    {#icall{anno=Anno,	%Must have an #a{}
			    module=#c_literal{anno=Lanno,val=erlang},
			    name=#c_literal{anno=Lanno,val='=:='},
			    args=[New,#c_literal{anno=Lanno,val=true}]},
		     Eps0 ++ [#iset{anno=Anno,var=New,arg=E1}],Bools,St2}
	    end;
	_ ->
	    Anno = get_ianno(E1),
	    Lanno = get_lineno_anno(E1),
	    case is_simple(E1) of
		true ->
		    Bools = [E1|Bools0],
		    {#icall{anno=Anno,	%Must have an #a{}
			    module=#c_literal{anno=Lanno,val=erlang},
			    name=#c_literal{anno=Lanno,val='=:='},
			    args=[E1,#c_literal{anno=Lanno,val=true}]},Eps0,Bools,St1};
		false ->
		    {New,St2} = new_var(Lanno, St1),
		    Bools = [New|Bools0],
		    {#icall{anno=Anno,	%Must have an #a{}
			    module=#c_literal{anno=Lanno,val=erlang},
			    name=#c_literal{anno=Lanno,val='=:='},
			    args=[New,#c_literal{anno=Lanno,val=true}]},
		     Eps0 ++ [#iset{anno=Anno,var=New,arg=E1}],Bools,St2}
	    end
    end.

force_booleans(Vs0, E, Eps, St) ->
    Vs1 = [set_anno(V, []) || V <- Vs0],
    Vs = unforce(E, Eps, Vs1),
    force_booleans_1(Vs, E, Eps, St).

force_booleans_1([], E, Eps, St) ->
    {E,Eps,St};
force_booleans_1([V|Vs], E0, Eps0, St0) ->
    {E1,Eps1,St1} = force_safe(E0, St0),
    Lanno = element(2, V),
    Anno = #a{anno=Lanno},
    Call = #icall{anno=Anno,module=#c_literal{anno=Lanno,val=erlang},
		  name=#c_literal{anno=Lanno,val=is_boolean},
		  args=[V]},
    {New,St} = new_var(Lanno, St1),
    Iset = #iset{anno=Anno,var=New,arg=Call},
    Eps = Eps0 ++ Eps1 ++ [Iset],
    E = #icall{anno=Anno,
	       module=#c_literal{anno=Lanno,val=erlang},name=#c_literal{anno=Lanno,val='and'},
	       args=[E1,New]},
    force_booleans_1(Vs, E, Eps, St).


%% unforce(Expr, PreExprList, BoolExprList) -> BoolExprList'.
%%  Filter BoolExprList. BoolExprList is a list of simple expressions
%%  (variables or literals) of which we are not sure whether they are booleans.
%%
%%  The basic idea for filtering is the following transformation
%%
%%      (E =:= Bool) and is_boolean(E)   ==>  E =:= Bool
%%
%%  where E is an arbitrary expression and Bool is 'true' or 'false'.
%%
%%  The transformation is still valid if there are other expressions joined
%%  by 'and' operations:
%%
%%      E1 and (E2 =:= true) and E3 and is_boolean(E)   ==>  E1 and (E2 =:= true) and E3
%%
%%  but expressions such as
%%
%%     not (E =:= true) and is_boolean(E)
%%
%%  cannot be transformed in this way (such expressions are the reason for
%%  adding the is_boolean/1 test in the first place).
%%
unforce(_, _, []) ->
    [];
unforce(E, Eps, Vs) ->
    Tree = unforce_tree(Eps++[E], gb_trees:empty()),
    unforce(Tree, Vs).

unforce_tree([#iset{var=#c_var{name=V},arg=Arg0}|Es], D0) ->
    Arg = unforce_tree_subst(Arg0, D0),
    D = gb_trees:insert(V, Arg, D0),
    unforce_tree(Es, D);
unforce_tree([#icall{}=Call], D) ->
    unforce_tree_subst(Call, D);
unforce_tree([Top], _) -> Top.

unforce_tree_subst(#icall{module=#c_literal{val=erlang},
			  name=#c_literal{val='=:='},
			  args=[_Expr,#c_literal{val=Bool}]}=Call, _)
  when is_boolean(Bool) ->
    %% We have erlang:'=:='(Expr, Bool). We must not expand this call any more
    %% or we will not recognize is_boolean(Expr) later.
    Call;
unforce_tree_subst(#icall{args=Args0}=Call, D) ->
    Args = map(fun(#c_var{name=V}=Var) ->
		       case gb_trees:lookup(V, D) of
			   {value,Val} -> Val;
			   none -> Var
		       end;
		  (Expr) -> Expr
	       end, Args0),
    Call#icall{args=Args};
unforce_tree_subst(Expr, _) -> Expr.

unforce(#icall{module=#c_literal{val=erlang},
	       name=#c_literal{val=Name},
	       args=Args}, Vs0) ->
    case {Name,Args} of
	{'and',[Arg1,Arg2]} ->
	    Vs = unforce(Arg1, Vs0),
	    unforce(Arg2, Vs);
	{'=:=',[E,#c_literal{val=Bool}]} when is_boolean(Bool) ->
	    Vs0 -- [set_anno(E, [])];
	{_,_} ->
	    %% Give up.
	    Vs0
    end;
unforce(_, Vs) -> Vs.

%% exprs([Expr], State) -> {[Cexpr],State}.
%%  Flatten top-level exprs.

exprs([E0|Es0], St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Es1,St2} = exprs(Es0, St1),
    {Eps ++ [E1] ++ Es1,St2};
exprs([], St) -> {[],St}.

%% expr(Expr, State) -> {Cexpr,[PreExp],State}.
%%  Generate an internal core expression.

expr({var,L,V}, St) -> {#c_var{anno=lineno_anno(L, St),name=V},[],St};
expr({char,L,C}, St) -> {#c_literal{anno=lineno_anno(L, St),val=C},[],St};
expr({integer,L,I}, St) -> {#c_literal{anno=lineno_anno(L, St),val=I},[],St};
expr({float,L,F}, St) -> {#c_literal{anno=lineno_anno(L, St),val=F},[],St};
expr({atom,L,A}, St) -> {#c_literal{anno=lineno_anno(L, St),val=A},[],St};
expr({nil,L}, St) -> {#c_literal{anno=lineno_anno(L, St),val=[]},[],St};
expr({string,L,S}, St) -> {#c_literal{anno=lineno_anno(L, St),val=S},[],St};
expr({cons,L,H0,T0}, St0) ->
    {H1,Hps,St1} = safe(H0, St0),
    {T1,Tps,St2} = safe(T0, St1),
    A = lineno_anno(L, St2),
    {ann_c_cons(A, H1, T1),Hps ++ Tps,St2};
expr({lc,L,E,Qs}, St) ->
    lc_tq(L, E, Qs, #c_literal{anno=lineno_anno(L, St),val=[]}, St);
expr({bc,L,E,Qs}, St) ->
    bc_tq(L, E, Qs, {nil,L}, St);
expr({tuple,L,Es0}, St0) ->
    {Es1,Eps,St1} = safe_list(Es0, St0),
    A = lineno_anno(L, St1),
    {ann_c_tuple(A, Es1),Eps,St1};
expr({bin,L,Es0}, St0) ->
    try expr_bin(Es0, lineno_anno(L, St0), St0) of
	{_,_,_}=Res -> Res
    catch
	throw:bad_binary ->
	    St = add_warning(L, bad_binary, St0),
	    LineAnno = lineno_anno(L, St),
	    As = [#c_literal{anno=LineAnno,val=badarg}],
	    {#icall{anno=#a{anno=LineAnno},	%Must have an #a{}
		    module=#c_literal{anno=LineAnno,val=erlang},
		    name=#c_literal{anno=LineAnno,val=error},
		    args=As},[],St}
    end;
expr({block,_,Es0}, St0) ->
    %% Inline the block directly.
    {Es1,St1} = exprs(first(Es0), St0),
    {E1,Eps,St2} = expr(last(Es0), St1),
    {E1,Es1 ++ Eps,St2};
expr({'if',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Fc = fail_clause([], #c_literal{val=if_clause}),
    Lanno = lineno_anno(L, St1),
    {#icase{anno=#a{anno=Lanno},args=[],clauses=Cs1,fc=Fc},[],St1};
expr({'case',L,E0,Cs0}, St0) ->
    {E1,Eps,St1} = novars(E0, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Fpat,St3} = new_var(St2),
    Fc = fail_clause([Fpat], c_tuple([#c_literal{val=case_clause},Fpat])),
    Lanno = lineno_anno(L, St3),
    {#icase{anno=#a{anno=Lanno},args=[E1],clauses=Cs1,fc=Fc},Eps,St3};
expr({'receive',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {#ireceive1{anno=#a{anno=lineno_anno(L, St1)},clauses=Cs1}, [], St1};
expr({'receive',L,Cs0,Te0,Tes0}, St0) ->
    {Te1,Teps,St1} = novars(Te0, St0),
    {Tes1,St2} = exprs(Tes0, St1),
    {Cs1,St3} = clauses(Cs0, St2),
    {#ireceive2{anno=#a{anno=lineno_anno(L, St3)},
		clauses=Cs1,timeout=Te1,action=Tes1},Teps,St3};
expr({'try',L,Es0,[],Ecs,[]}, St0) ->
    %% 'try ... catch ... end'
    {Es1,St1} = exprs(Es0, St0),
    {V,St2} = new_var(St1),		%This name should be arbitrary
    {Evs,Hs,St3} = try_exception(Ecs, St2),
    Lanno = lineno_anno(L, St3),
    {#itry{anno=#a{anno=Lanno},args=Es1,vars=[V],body=[V],
	   evars=Evs,handler=Hs},
     [],St3};
expr({'try',L,Es0,Cs0,Ecs,[]}, St0) ->
    %% 'try ... of ... catch ... end'
    {Es1,St1} = exprs(Es0, St0),
    {V,St2} = new_var(St1),		%This name should be arbitrary
    {Cs1,St3} = clauses(Cs0, St2),
    {Fpat,St4} = new_var(St3),
    Fc = fail_clause([Fpat], c_tuple([#c_literal{val=try_clause},Fpat])),
    {Evs,Hs,St5} = try_exception(Ecs, St4),
    Lanno = lineno_anno(L, St1),
    {#itry{anno=#a{anno=lineno_anno(L, St5)},args=Es1,
	   vars=[V],body=[#icase{anno=#a{anno=Lanno},args=[V],clauses=Cs1,fc=Fc}],
	   evars=Evs,handler=Hs},
     [],St5};
expr({'try',L,Es0,[],[],As0}, St0) ->
    %% 'try ... after ... end'
    {Es1,St1} = exprs(Es0, St0),
    {As1,St2} = exprs(As0, St1),
    {Evs,Hs0,St3} = try_after(As1, St2),
    %% We must kill the id for any funs in the duplicated after body,
    %% to avoid getting two local functions having the same name.
    Hs = kill_id_anns(Hs0),
    {V,St4} = new_var(St3),		% (must not exist in As1)
    %% TODO: this duplicates the 'after'-code; should lift to function.
    Lanno = lineno_anno(L, St4),
    {#itry{anno=#a{anno=Lanno},args=Es1,vars=[V],body=As1++[V],
	   evars=Evs,handler=Hs},
     [],St4};
expr({'try',L,Es,Cs,Ecs,As}, St0) ->
    %% 'try ... [of ...] [catch ...] after ... end'
    expr({'try',L,[{'try',L,Es,Cs,Ecs,[]}],[],[],As}, St0);
expr({'catch',L,E0}, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    Lanno = lineno_anno(L, St1),
    {#icatch{anno=#a{anno=Lanno},body=Eps ++ [E1]},[],St1};
expr({'fun',L,{function,F,A},{_,_,_}=Id}, St) ->
    Lanno = lineno_anno(L, St),
    {#c_var{anno=Lanno++[{id,Id}],name={F,A}},[],St};
expr({'fun',L,{clauses,Cs},Id}, St) ->
    fun_tq(Id, Cs, L, St);
expr({call,L,{remote,_,M,F},As0}, #core{wanted=Wanted}=St0) ->
    {[M1,F1|As1],Aps,St1} = safe_list([M,F|As0], St0),
    Lanno = lineno_anno(L, St1),
    Anno = case Wanted of
	       false -> [result_not_wanted|Lanno];
	       true -> Lanno
	   end,
    {#icall{anno=#a{anno=Anno},module=M1,name=F1,args=As1},Aps,St1};
expr({call,Lc,{atom,Lf,F},As0}, St0) ->
    {As1,Aps,St1} = safe_list(As0, St0),
    Op = #c_var{anno=lineno_anno(Lf, St1),name={F,length(As1)}},
    {#iapply{anno=#a{anno=lineno_anno(Lc, St1)},op=Op,args=As1},Aps,St1};
expr({call,L,FunExp,As0}, St0) ->
    {Fun,Fps,St1} = safe(FunExp, St0),
    {As1,Aps,St2} = safe_list(As0, St1),
    Lanno = lineno_anno(L, St2),
    {#iapply{anno=#a{anno=Lanno},op=Fun,args=As1},Fps ++ Aps,St2};
expr({match,L,P0,E0}, St0) ->
    %% First fold matches together to create aliases.
    {P1,E1} = fold_match(E0, P0),
    St1 = case P1 of
	      {var,_,'_'} -> St0#core{wanted=false};
	      _ -> St0
	  end,
    {E2,Eps,St2} = novars(E1, St1),
    St3 = St2#core{wanted=St0#core.wanted},
    P2 = try
	     pattern(P1, St3)
	 catch
	     throw:Thrown ->
		 Thrown
	 end,
    {Fpat,St4} = new_var(St3),
    Fc = fail_clause([Fpat], c_tuple([#c_literal{val=badmatch},Fpat])),
    Lanno = lineno_anno(L, St4),
    case P2 of
	nomatch ->
	    St = add_warning(L, nomatch, St4),
	    {#icase{anno=#a{anno=Lanno},
		    args=[E2],clauses=[],fc=Fc},Eps,St};
	Other when not is_atom(Other) ->
	    {#imatch{anno=#a{anno=Lanno},pat=P2,arg=E2,fc=Fc},Eps,St4}
    end;
expr({op,_,'++',{lc,Llc,E,Qs},More}, St0) ->
    %% Optimise '++' here because of the list comprehension algorithm.
    %%
    %% To avoid achieving quadratic complexity if there is a chain of
    %% list comprehensions without generators combined with '++', force
    %% evaluation of More now. Evaluating More here could also reduce the
    %% number variables in the environment for letrec.
    {Mc,Mps,St1} = safe(More, St0),
    {Y,Yps,St} = lc_tq(Llc, E, Qs, Mc, St1),
    {Y,Mps++Yps,St};
expr({op,L,'andalso',E1,E2}, St0) ->
    {#c_var{name=V0},St} = new_var(L, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch(L, E1, V, E2, False, St0),
    expr(E, St);
expr({op,L,'orelse',E1,E2}, St0) ->
    {#c_var{name=V0},St} = new_var(L, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch(L, E1, V, True, E2, St0),
    expr(E, St);
expr({op,L,Op,A0}, St0) ->
    {A1,Aps,St1} = safe(A0, St0),
    LineAnno = lineno_anno(L, St1),
    {#icall{anno=#a{anno=LineAnno},		%Must have an #a{}
	    module=#c_literal{anno=LineAnno,val=erlang},
	    name=#c_literal{anno=LineAnno,val=Op},args=[A1]},Aps,St1};
expr({op,L,Op,L0,R0}, St0) ->
    {As,Aps,St1} = safe_list([L0,R0], St0),
    LineAnno = lineno_anno(L, St1),
    {#icall{anno=#a{anno=LineAnno},		%Must have an #a{}
	    module=#c_literal{anno=LineAnno,val=erlang},
	    name=#c_literal{anno=LineAnno,val=Op},args=As},Aps,St1}.

make_bool_switch(L, E, V, T, F, #core{in_guard=true}) ->
    make_bool_switch_guard(L, E, V, T, F);
make_bool_switch(L, E, V, T, F, #core{}) ->
    make_bool_switch_body(L, E, V, T, F).

make_bool_switch_body(L, E, V, T, F) ->
    NegL = neg_line(abs_line(L)),
    Error = {tuple,NegL,[{atom,NegL,badarg},V]},
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],[T]},
      {clause,NegL,[{atom,NegL,false}],[],[F]},
      {clause,NegL,[V],[],
       [{call,NegL,{remote,NegL,{atom,NegL,erlang},{atom,NegL,error}},
	 [Error]}]}]}.

make_bool_switch_guard(_, E, _, {atom,_,true}, {atom,_,false}) -> E;
make_bool_switch_guard(L, E, V, T, F) ->
    NegL = neg_line(abs_line(L)),
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],[T]},
      {clause,NegL,[{atom,NegL,false}],[],[F]},
      {clause,NegL,[V],[],[V]}
     ]}.


%% try_exception([ExcpClause], St) -> {[ExcpVar],Handler,St}.

try_exception(Ecs0, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Evs,St1} = new_vars(3, St0), % Tag, Value, Info
    {Ecs1,St2} = clauses(Ecs0, St1),
    [_,Value,Info] = Evs,
    Ec = #iclause{anno=#a{anno=[compiler_generated]},
		  pats=[c_tuple(Evs)],guard=[#c_literal{val=true}],
		  body=[#iprimop{anno=#a{},       %Must have an #a{}
				 name=#c_literal{val=raise},
				 args=[Info,Value]}]},
    Hs = [#icase{anno=#a{},args=[c_tuple(Evs)],clauses=Ecs1,fc=Ec}],
    {Evs,Hs,St2}.

try_after(As, St0) ->
    %% See above.
    {Evs,St1} = new_vars(3, St0),		% Tag, Value, Info
    [_,Value,Info] = Evs,
    B = As ++ [#iprimop{anno=#a{},       %Must have an #a{}
			 name=#c_literal{val=raise},
			 args=[Info,Value]}],
    Ec = #iclause{anno=#a{anno=[compiler_generated]},
		  pats=[c_tuple(Evs)],guard=[#c_literal{val=true}],
		  body=B},
    Hs = [#icase{anno=#a{},args=[c_tuple(Evs)],clauses=[],fc=Ec}],
    {Evs,Hs,St1}.

%% expr_bin([ArgExpr], St) -> {[Arg],[PreExpr],St}.
%%  Flatten the arguments of a bin. Do this straight left to right!
%%  Note that ibinary needs to have its annotation wrapped in a #a{}
%%  record whereas c_literal should not have a wrapped annotation
 
expr_bin(Es0, Anno, St0) ->
    case constant_bin(Es0) of
	error ->
	    {Es,Eps,St} = expr_bin_1(Es0, St0),
	    {#ibinary{anno=#a{anno=Anno},segments=Es},Eps,St};
	Bin ->
	    {#c_literal{anno=Anno,val=Bin},[],St0}
    end.

%% constant_bin([{bin_element,_,_,_,_}]) -> binary() | error
%%  If the binary construction is truly constant (no variables,
%%  no native fields), and does not contain fields whose expansion
%%  become huge (such as <<0:100000000>>), evaluate and return the binary;
%%  otherwise return 'error'.

constant_bin(Es) ->
    try
	constant_bin_1(Es)
    catch
	error -> error
    end.

constant_bin_1(Es) ->
    verify_suitable_fields(Es),
    EmptyBindings = erl_eval:new_bindings(),
    EvalFun = fun({integer,_,I}, B) -> {value,I,B};
		 ({char,_,C}, B) -> {value,C,B};
		 ({float,_,F}, B) -> {value,F,B};
		 ({atom,_,undefined}, B) -> {value,undefined,B}
	      end,
    case catch eval_bits:expr_grp(Es, EmptyBindings, EvalFun) of
	{value,Bin,EmptyBindings} ->
	    Bin;
	_ ->
	    error
    end.

%% verify_suitable_fields([{bin_element,_,Sz,Opts}=E|Es]) ->
    
verify_suitable_fields([{bin_element,_,Val,SzTerm,Opts}|Es]) ->
    case member(big, Opts) orelse member(little, Opts) of
	true -> ok;
	false -> throw(error)			%Native endian.
    end,
    {unit,Unit} = keyfind(unit, 1, Opts),
    case {SzTerm,Val} of
	{{atom,_,undefined},{char,_,_}} ->
	    %% UTF-8/16/32.
	    ok;
	{{atom,_,undefined},{integer,_,_}} ->
	    %% UTF-8/16/32.
	    ok;
	{{integer,_,Sz},_} when Sz*Unit =< 256 ->
	    %% Don't be cheap - always accept fields up to this size.
	    ok;
	{{integer,_,Sz0},{integer,_,Int}} ->
	    %% Estimate the number of bits needed to to hold the integer
	    %% literal. Check whether the field size is reasonable in
	    %% proportion to the number of bits needed.
	    Sz = Sz0*Unit,
	    case count_bits(Int) of
		BitsNeeded when 2*BitsNeeded >= Sz ->
		    ok;
		_ ->
		    %% More than about half of the field size will be
		    %% filled out with zeroes - not acceptable.
		    throw(error)
	    end;
	{_,_} ->
	    %% Reject anything else. There are either variables,
	    %% or a float with a huge size or an embedded binary.
	    throw(error)
    end,
    verify_suitable_fields(Es);
verify_suitable_fields([]) -> ok.

%% Count the number of bits approximately needed to store Int.
%% (We don't need an exact result for this purpose.)

count_bits(Int) -> 
    count_bits_1(abs_line(Int), 64).

count_bits_1(0, Bits) -> Bits;
count_bits_1(Int, Bits) -> count_bits_1(Int bsr 64, Bits+64).

expr_bin_1(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = bitstr(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

bitstr({bin_element,_,E0,Size0,[Type,{unit,Unit}|Flags]}, St0) ->
    {E1,Eps,St1} = safe(E0, St0),
    {Size1,Eps2,St2} = safe(Size0, St1),
    case {Type,E1} of
	{_,#c_var{}} -> ok;
	{integer,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf8,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf16,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf32,#c_literal{val=I}} when is_integer(I) -> ok;
	{float,#c_literal{val=V}} when is_number(V) -> ok;
	{binary,#c_literal{val=V}} when is_bitstring(V) -> ok;
	{_,_} ->
	    throw(bad_binary)
    end,
    {#c_bitstr{val=E1,size=Size1,
	       unit=#c_literal{val=Unit},
	       type=#c_literal{val=Type},
	       flags=#c_literal{val=Flags}},
     Eps ++ Eps2,St2}.

%% fun_tq(Id, [Clauses], Line, State) -> {Fun,[PreExp],State}.

fun_tq({_,_,Name}=Id, Cs0, L, St0) ->
    Arity = clause_arity(hd(Cs0)),
    {Cs1,St1} = clauses(Cs0, St0),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = function_clause(Ps, {Name,Arity}),
    Fun = #ifun{anno=#a{anno=lineno_anno(L, St3)},
		id=[{id,Id}],				%We KNOW!
		vars=Args,clauses=Cs1,fc=Fc},
    {Fun,[],St3}.

%% lc_tq(Line, Exp, [Qualifier], Mc, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Simon PJ pp 127-138.  
%%  This gets a bit messy as we must transform all directly here.  We
%%  recognise guard tests and try to fold them together and join to a
%%  preceding generators, this should give us better and more compact
%%  code.

lc_tq(Line, E, [{generate,Lg,P,G}|Qs0], Mc, St0) ->
    {Gs,Qs1} =  splitwith(fun is_guard_test/1, Qs0),
    {Name,St1} = new_fun_name("lc", St0),
    {Head,St2} = new_var(St1),
    {Tname,St3} = new_var_name(St2),
    LA = lineno_anno(Line, St3),
    LAnno = #a{anno=LA},
    Tail = #c_var{anno=LA,name=Tname},
    {Arg,St4} = new_var(St3),
    {Nc,[],St5} = expr({call,Lg,{atom,Lg,Name},[{var,Lg,Tname}]}, St4),
    {Guardc,St6} = lc_guard_tests(Gs, St5),	%These are always flat!
    {Lc,Lps,St7} = lc_tq(Line, E, Qs1, Nc, St6),
    {Pc,St8} = list_gen_pattern(P, Line, St7),
    {Gc,Gps,St9} = safe(G, St8),		%Will be a function argument!
    Fc = function_clause([Arg], LA, {Name,1}),

    %% Avoid constructing a default clause if the list comprehension
    %% only has a variable as generator and there are no guard
    %% tests. In other words, if the comprehension is equivalent to
    %% lists:map/2.
    Cs0 = case {Guardc, Pc} of
	      {[], #c_var{}} ->
		  [#iclause{anno=LAnno,
			    pats=[#c_literal{anno=LA,val=[]}],guard=[],
			    body=[Mc]}];
	      _ ->
		  [#iclause{anno=#a{anno=[compiler_generated|LA]},
			    pats=[ann_c_cons(LA, Head, Tail)],
			    guard=[],
			    body=[Nc]},
		   #iclause{anno=LAnno,
			    pats=[#c_literal{anno=LA,val=[]}],guard=[],
			    body=[Mc]}]
	  end,
    Cs = case Pc of
	     nomatch -> Cs0;
	     _ ->
		 [#iclause{anno=LAnno,
			   pats=[ann_c_cons(LA, Pc, Tail)],
			   guard=Guardc,
			   body=Lps ++ [Lc]}|Cs0]
	 end,
    Fun = #ifun{anno=LAnno,id=[],vars=[Arg],clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno,defs=[{{Name,1},Fun}],
	      body=Gps ++ [#iapply{anno=LAnno,
				   op=#c_var{anno=LA,name={Name,1}},
				   args=[Gc]}]},
     [],St9};
lc_tq(Line, E, [{b_generate,Lg,P,G}|Qs0], Mc, St0) ->
    {Gs,Qs1} =  splitwith(fun is_guard_test/1, Qs0),
    {Name,St1} = new_fun_name("blc", St0),
    {Tname,St2} = new_var_name(St1),
    LA = lineno_anno(Line, St2),
    LAnno = #a{anno=LA},
    HeadBinPattern = pattern(P,St2),
    #c_binary{segments=Ps} = HeadBinPattern,
    {EPs,St3} = emasculate_segments(Ps,St2),
    Tail = #c_var{anno=LA,name=Tname},
    TailSegment = #c_bitstr{val=Tail,size=#c_literal{val=all},
			    unit=#c_literal{val=1},
			    type=#c_literal{val=binary},
			    flags=#c_literal{val=[big,unsigned]}},
    Pattern = HeadBinPattern#c_binary{segments=Ps ++ [TailSegment]},
    EPattern = HeadBinPattern#c_binary{segments=EPs ++ [TailSegment]},
    {Arg,St4} = new_var(St3),
    {Guardc,St5} = lc_guard_tests(Gs, St4),	%These are always flat!
    {Nc,[],St6} = expr({call,Lg,{atom,Lg,Name},[{var,Lg,Tname}]}, St5),
    {Bc,Bps,St7} = lc_tq(Line, E, Qs1, Nc, St6),
    {Gc,Gps,St10} = safe(G, St7),		%Will be a function argument!
    Fc = function_clause([Arg], LA, {Name,1}),
    Cs = [#iclause{anno=#a{anno=[compiler_generated|LA]},
		   pats=[Pattern],
		   guard=Guardc,
		   body=Bps ++ [Bc]},
	  #iclause{anno=#a{anno=[compiler_generated|LA]},
		   pats=[EPattern],
		   guard=[],
		   body=[#iapply{anno=LAnno,
				 op=#c_var{anno=LA,name={Name,1}},
				 args=[Tail]}]},
	  #iclause{anno=LAnno,
		   pats=[#c_binary{anno=LA, segments=[TailSegment]}],guard=[],
		   body=[Mc]}],
    Fun = #ifun{anno=LAnno,id=[],vars=[Arg],clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno,defs=[{{Name,1},Fun}],
	      body=Gps ++ [#iapply{anno=LAnno,
				   op=#c_var{anno=LA,name={Name,1}},
				   args=[Gc]}]},
     [],St10};
lc_tq(Line, E, [Fil0|Qs0], Mc, St0) ->
    %% Special case sequences guard tests.
    LA = lineno_anno(Line, St0),
    LAnno = #a{anno=LA},
    case is_guard_test(Fil0) of
	true ->
	    {Gs0,Qs1} = splitwith(fun is_guard_test/1, Qs0),
	    {Lc,Lps,St1} = lc_tq(Line, E, Qs1, Mc, St0),
	    {Gs,St2} = lc_guard_tests([Fil0|Gs0], St1), %These are always flat!
	    {#icase{anno=LAnno,
		    args=[],
		    clauses=[#iclause{anno=LAnno,pats=[],
				      guard=Gs,body=Lps ++ [Lc]}],
		    fc=#iclause{anno=LAnno,pats=[],guard=[],body=[Mc]}},
	     [],St2};
	false ->
	    {Lc,Lps,St1} = lc_tq(Line, E, Qs0, Mc, St0),
	    {Fpat,St2} = new_var(St1),
	    Fc = fail_clause([Fpat], c_tuple([#c_literal{val=case_clause},Fpat])),
	    %% Do a novars little optimisation here.
	    {Filc,Fps,St3} = novars(Fil0, St2),
	    {#icase{anno=LAnno,
		    args=[Filc],
		    clauses=[#iclause{anno=LAnno,
				      pats=[#c_literal{anno=LA,val=true}],
				      guard=[],
				      body=Lps ++ [Lc]},
			     #iclause{anno=LAnno#a{anno=[compiler_generated|LA]},
				      pats=[#c_literal{anno=LA,val=false}],
				      guard=[],
				      body=[Mc]}],
		    fc=Fc},
	     Fps,St3}
    end;
lc_tq(Line, E0, [], Mc0, St0) ->
    {H1,Hps,St1} = safe(E0, St0),
    {T1,Tps,St} = force_safe(Mc0, St1),
    Anno = lineno_anno(Line, St),
    E = ann_c_cons(Anno, H1, T1),
    {set_anno(E, [compiler_generated|Anno]),Hps ++ Tps,St}.

%% bc_tq(Line, Exp, [Qualifier], More, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Gustafsson ERLANG'05.  
%%  This gets a bit messy as we must transform all directly here.  We
%%  recognise guard tests and try to fold them together and join to a
%%  preceding generators, this should give us better and more compact
%%  code.
%%  More could be transformed before calling bc_tq.

bc_tq(Line, Exp, Qualifiers, _, St0) ->
    {BinVar,St1} = new_var(St0),
    {Sz,SzPre,St2} = bc_initial_size(Exp, Qualifiers, St1),
    {E,BcPre,St} = bc_tq1(Line, Exp, Qualifiers, BinVar, St2),
    Pre = SzPre ++
	[#iset{var=BinVar,
	       arg=#iprimop{name=#c_literal{val=bs_init_writable},
			    args=[Sz]}}] ++ BcPre,
    {E,Pre,St}.

bc_tq1(Line, E, [{generate,Lg,P,G}|Qs0], AccExpr, St0) ->
    {Gs,Qs1} =  splitwith(fun is_guard_test/1, Qs0),
    {Name,St1} = new_fun_name("lbc", St0),
    LA = lineno_anno(Line, St1),
    {[Head,Tail,AccVar],St2} = new_vars(LA, 3, St1),
    LAnno = #a{anno=LA},
    {Arg,St3} = new_var(St2),
    NewMore = {call,Lg,{atom,Lg,Name},[{var,Lg,Tail#c_var.name},
				       {var,Lg,AccVar#c_var.name}]},
    {Guardc,St4} = lc_guard_tests(Gs, St3),	%These are always flat!
    {Lc,Lps,St5} = bc_tq1(Line, E, Qs1, AccVar, St4),
    {Nc,Nps,St6} = expr(NewMore, St5),
    {Pc,St7} = list_gen_pattern(P, Line, St6),
    {Gc,Gps,St8} = safe(G, St7),		%Will be a function argument!
    Fc = function_clause([Arg,AccVar], LA, {Name,2}),
    Cs0 = case {Guardc, Pc} of
	      {[], #c_var{}} ->
		  [#iclause{anno=LAnno,
			    pats=[#c_literal{anno=LA,val=[]},AccVar],guard=[],
			    body=[AccVar]}];
	      _ ->
		  [#iclause{anno=#a{anno=[compiler_generated|LA]},
			    pats=[ann_c_cons(LA, Head, Tail),AccVar],
			    guard=[],
			    body=Nps ++ [Nc]},
		   #iclause{anno=LAnno,
			    pats=[#c_literal{anno=LA,val=[]},AccVar],guard=[],
			    body=[AccVar]}]
	  end,
    Cs = case Pc of
	     nomatch -> Cs0;
	     _ ->
		 Body = Lps ++ Nps ++ [#iset{var=AccVar,arg=Lc},Nc],
		 [#iclause{anno=LAnno,
			   pats=[ann_c_cons(LA,Pc,Tail),AccVar],
			   guard=Guardc,
			   body=Body}|Cs0]
	 end,
    Fun = #ifun{anno=LAnno,id=[],vars=[Arg,AccVar],clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno,defs=[{{Name,2},Fun}],
	      body=Gps ++ [#iapply{anno=LAnno,
				   op=#c_var{anno=LA,name={Name,2}},
				   args=[Gc,AccExpr]}]},
     [],St8};
bc_tq1(Line, E, [{b_generate,Lg,P,G}|Qs0], AccExpr, St0) ->
    {Gs,Qs1} =  splitwith(fun is_guard_test/1, Qs0),
    {Name,St1} = new_fun_name("lbc", St0),
    LA = lineno_anno(Line, St1),
    {[Tail,AccVar],St2} = new_vars(LA, 2, St1),
    LAnno = #a{anno=LA},
    HeadBinPattern = pattern(P, St2),
    #c_binary{segments=Ps} = HeadBinPattern,
    {EPs,St3} = emasculate_segments(Ps, St2),
    TailSegment = #c_bitstr{val=Tail,size=#c_literal{val=all},
			    unit=#c_literal{val=1},
			    type=#c_literal{val=binary},
			    flags=#c_literal{val=[big,unsigned]}},
    Pattern = HeadBinPattern#c_binary{segments=Ps ++ [TailSegment]},
    EPattern = HeadBinPattern#c_binary{segments=EPs ++ [TailSegment]},
    {Arg,St4} = new_var(St3),
    NewMore = {call,Lg,{atom,Lg,Name},[{var,Lg,Tail#c_var.name},
				       {var,Lg,AccVar#c_var.name}]},
    {Guardc,St5} = lc_guard_tests(Gs, St4),	%These are always flat!
    {Bc,Bps,St6} = bc_tq1(Line, E, Qs1, AccVar, St5),
    {Nc,Nps,St7} = expr(NewMore, St6),
    {Gc,Gps,St8} = safe(G, St7),		%Will be a function argument!
    Fc = function_clause([Arg,AccVar], LA, {Name,2}),
    Body = Bps ++ Nps ++ [#iset{var=AccVar,arg=Bc},Nc],
    Cs = [#iclause{anno=LAnno,
		   pats=[Pattern,AccVar],
		   guard=Guardc,
		   body=Body},
	  #iclause{anno=#a{anno=[compiler_generated|LA]},
		   pats=[EPattern,AccVar],
		   guard=[],
		   body=Nps ++ [Nc]},
	  #iclause{anno=LAnno,
		   pats=[#c_binary{anno=LA,segments=[TailSegment]},AccVar],
		   guard=[],
		   body=[AccVar]}],
    Fun = #ifun{anno=LAnno,id=[],vars=[Arg,AccVar],clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno,defs=[{{Name,2},Fun}],
	      body=Gps ++ [#iapply{anno=LAnno,
				   op=#c_var{anno=LA,name={Name,2}},
				   args=[Gc,AccExpr]}]},
     [],St8};
bc_tq1(Line, E, [Fil0|Qs0], AccVar, St0) ->
    %% Special case sequences guard tests.
    LA = lineno_anno(Line, St0),
    LAnno = #a{anno=LA},
    case is_guard_test(Fil0) of
	true ->
	    {Gs0,Qs1} = splitwith(fun is_guard_test/1, Qs0),
	    {Bc,Bps,St1} = bc_tq1(Line, E, Qs1, AccVar, St0),
	    {Gs,St} = lc_guard_tests([Fil0|Gs0], St1), %These are always flat!
	    {#icase{anno=LAnno,
		    args=[],
		    clauses=[#iclause{anno=LAnno,
				      pats=[],
				      guard=Gs,body=Bps ++ [Bc]}],
		    fc=#iclause{anno=LAnno,pats=[],guard=[],body=[AccVar]}},
	     [],St};
	false ->
	    {Bc,Bps,St1} = bc_tq1(Line, E, Qs0, AccVar, St0),
	    {Fpat,St2} = new_var(St1),
	    Fc = fail_clause([Fpat], c_tuple([#c_literal{val=case_clause},Fpat])),
	    %% Do a novars little optimisation here.
	    {Filc,Fps,St} = novars(Fil0, St2),
	    {#icase{anno=LAnno,
		    args=[Filc],
		    clauses=[#iclause{anno=LAnno,
				      pats=[#c_literal{anno=LA,val=true}],
				      guard=[],
				      body=Bps ++ [Bc]},
			     #iclause{anno=LAnno#a{anno=[compiler_generated|LA]},
				      pats=[#c_literal{anno=LA,val=false}],
				      guard=[],
				      body=[AccVar]}],
		    fc=Fc},
	     Fps,St}
    end;
bc_tq1(_, {bin,Bl,Elements}, [], AccVar, St0) ->
    {E,Pre,St} = expr({bin,Bl,[{bin_element,Bl,
				{var,Bl,AccVar#c_var.name},
				{atom,Bl,all},
				[binary,{unit,1}]}|Elements]}, St0),
    #a{anno=A} = Anno0 = get_anno(E),
    Anno = Anno0#a{anno=[compiler_generated,single_use|A]},
    %%Anno = Anno0#a{anno=[compiler_generated|A]},
    {set_anno(E, Anno),Pre,St}.

emasculate_segments(Segs, St) ->
    emasculate_segments(Segs, St, []).

emasculate_segments([#c_bitstr{val=#c_var{}}=B|Rest], St, Acc) ->
    emasculate_segments(Rest, St, [B|Acc]);
emasculate_segments([B|Rest], St0, Acc) ->
    {Var,St1} = new_var(St0),
    emasculate_segments(Rest, St1, [B#c_bitstr{val=Var}|Acc]);
emasculate_segments([], St, Acc) ->
    {lists:reverse(Acc),St}.

lc_guard_tests([], St) -> {[],St};
lc_guard_tests(Gs0, St0) ->
    Gs1 = guard_tests(Gs0),
    {Gs,St} = gexpr_top(Gs1, St0#core{in_guard=true}),
    {Gs,St#core{in_guard=false}}.

list_gen_pattern(P0, Line, St) ->
    try
	{pattern(P0, St),St}
    catch 
	nomatch -> {nomatch,add_warning(Line, nomatch, St)}
    end.

%%%
%%% Generate code to calculate the initial size for
%%% the result binary in a binary comprehension.
%%%

bc_initial_size(E, Q, St0) ->
    try
	{ElemSzExpr,ElemSzPre,St1} = bc_elem_size(E, St0),
	{V,St2} = new_var(St1),
	{GenSzExpr,GenSzPre,St3} = bc_gen_size(Q, St2),
	case ElemSzExpr of
	    #c_literal{val=ElemSz} when ElemSz rem 8 =:= 0 ->
		NumBytesExpr = #c_literal{val=ElemSz div 8},
		BytesExpr = [#iset{var=V,
				   arg=bc_mul(GenSzExpr, NumBytesExpr)}],
		{V,ElemSzPre++GenSzPre++BytesExpr,St3};
	    _ ->
		{[BitsV,PlusSevenV],St} = new_vars(2, St3),
		BitsExpr = #iset{var=BitsV,arg=bc_mul(GenSzExpr, ElemSzExpr)},
		PlusSevenExpr = #iset{var=PlusSevenV,
				      arg=bc_add(BitsV, #c_literal{val=7})},
		Expr = #iset{var=V,
			     arg=bc_bsr(PlusSevenV, #c_literal{val=3})},
		{V,ElemSzPre++GenSzPre++
		 [BitsExpr,PlusSevenExpr,Expr],St}
	end
    catch
	throw:impossible ->
	    {#c_literal{val=256},[],St0}
    end.

bc_elem_size({bin,_,El}, St0) ->
    case bc_elem_size_1(El, 0, []) of
	{Bits,[]} ->
	    {#c_literal{val=Bits},[],St0};
	{Bits,Vars0} ->
	    [{U,V0}|Pairs]  = sort(Vars0),
	    F = bc_elem_size_combine(Pairs, U, [V0], []),
	    bc_mul_pairs(F, #c_literal{val=Bits}, [], St0)
    end.

bc_elem_size_1([{bin_element,_,_,{integer,_,N},Flags}|Es], Bits, Vars) ->
    {unit,U} = keyfind(unit, 1, Flags),
    bc_elem_size_1(Es, Bits+U*N, Vars);
bc_elem_size_1([{bin_element,_,_,{var,_,Var},Flags}|Es], Bits, Vars) ->
    {unit,U} = keyfind(unit, 1, Flags),
    bc_elem_size_1(Es, Bits, [{U,#c_var{name=Var}}|Vars]);
bc_elem_size_1([_|_], _, _) ->
    throw(impossible);
bc_elem_size_1([], Bits, Vars) ->
    {Bits,Vars}.

bc_elem_size_combine([{U,V}|T], U, UVars, Acc) ->
    bc_elem_size_combine(T, U, [V|UVars], Acc);
bc_elem_size_combine([{U,V}|T], OldU, UVars, Acc) ->
    bc_elem_size_combine(T, U, [V], [{OldU,UVars}|Acc]);
bc_elem_size_combine([], U, Uvars, Acc) ->
    [{U,Uvars}|Acc].

bc_mul_pairs([{U,L0}|T], E0, Pre, St0) ->
    {AddExpr,AddPre,St1} = bc_add_list(L0, St0),
    {[V1,V2],St} = new_vars(2, St1),
    Set1 = #iset{var=V1,arg=bc_mul(AddExpr, #c_literal{val=U})},
    Set2 = #iset{var=V2,arg=bc_add(V1, E0)},
    bc_mul_pairs(T, V2, [Set2,Set1|reverse(AddPre, Pre)], St);
bc_mul_pairs([], E, Pre, St) ->
    {E,reverse(Pre),St}.

bc_add_list([V], St) ->
    {V,[],St};
bc_add_list([H|T], St) ->
    bc_add_list_1(T, [], H, St).

bc_add_list_1([H|T], Pre, E, St0) ->
    {Var,St} = new_var(St0),
    Set = #iset{var=Var,arg=bc_add(H, E)},
    bc_add_list_1(T, [Set|Pre], Var, St);
bc_add_list_1([], Pre, E, St) ->
    {E,reverse(Pre),St}.

bc_gen_size(Q, St) ->
    bc_gen_size_1(Q, #c_literal{val=1}, [], St).

bc_gen_size_1([{generate,L,El,Gen}|Qs], E0, Pre0, St0) ->
    bc_verify_non_filtering(El),
    case Gen of
	{var,_,ListVar} ->
	    Lanno = lineno_anno(L, St0),
	    {LenVar,St1} = new_var(St0),
	    Set = #iset{var=LenVar,
			arg=#icall{anno=#a{anno=Lanno},
				   module=#c_literal{val=erlang},
				   name=#c_literal{val=length},
				   args=[#c_var{name=ListVar}]}},
	    {E,Pre,St} = bc_gen_size_mul(E0, LenVar, [Set|Pre0], St1),
	    bc_gen_size_1(Qs, E, Pre, St);
	_ ->
	    %% The only expressions we handle is literal lists.
	    Len = bc_list_length(Gen, 0),
	    {E,Pre,St} = bc_gen_size_mul(E0, #c_literal{val=Len}, Pre0, St0),
	    bc_gen_size_1(Qs, E, Pre, St)
    end;
bc_gen_size_1([{b_generate,_,El,Gen}|Qs], E0, Pre0, St0) ->
    bc_verify_non_filtering(El),
    {MatchSzExpr,Pre1,St1} = bc_elem_size(El, St0),
    Pre2 = reverse(Pre1, Pre0),
    {ResVar,St2} = new_var(St1),
    {BitSizeExpr,Pre3,St3} = bc_gen_bit_size(Gen, Pre2, St2),
    Div = #iset{var=ResVar,arg=bc_div(BitSizeExpr,
				      MatchSzExpr)},
    Pre4 = [Div|Pre3],
    {E,Pre,St} = bc_gen_size_mul(E0, ResVar, Pre4, St3),
    bc_gen_size_1(Qs, E, Pre, St);
bc_gen_size_1([], E, Pre, St) ->
    {E,reverse(Pre),St};
bc_gen_size_1(_, _, _, _) ->
    throw(impossible).

bc_gen_bit_size({var,L,V}, Pre0, St0) ->
    Lanno = lineno_anno(L, St0),
    {SzVar,St} = new_var(St0),
    Pre = [#iset{var=SzVar,
		 arg=#icall{anno=#a{anno=Lanno},
			    module=#c_literal{val=erlang},
			    name=#c_literal{val=bit_size},
			    args=[#c_var{name=V}]}}|Pre0],
    {SzVar,Pre,St};
bc_gen_bit_size({bin,_,_}=Bin, Pre, St) ->
    {#c_literal{val=bc_bin_size(Bin)},Pre,St};
bc_gen_bit_size(_, _, _) ->
    throw(impossible).

bc_verify_non_filtering({bin,_,Els}) ->
    foreach(fun({bin_element,_,{var,_,_},_,_}) -> ok;
	       (_) -> throw(impossible)
	    end, Els);
bc_verify_non_filtering({var,_,_}) ->
    ok;
bc_verify_non_filtering(_) ->
    throw(impossible).

bc_list_length({string,_,Str}, Len) ->
    Len + length(Str);
bc_list_length({cons,_,_,T}, Len) ->
    bc_list_length(T, Len+1);
bc_list_length({nil,_}, Len) ->
    Len;
bc_list_length(_, _) ->
    throw(impossible).

bc_bin_size({bin,_,Els}) ->
    bc_bin_size_1(Els, 0).

bc_bin_size_1([{bin_element,_,_,{integer,_,Sz},Flags}|Els], N) ->
    {unit,U} = keyfind(unit, 1, Flags),
    bc_bin_size_1(Els, N+U*Sz);
bc_bin_size_1([], N) -> N;
bc_bin_size_1(_, _) -> throw(impossible).

bc_gen_size_mul(#c_literal{val=1}, E, Pre, St) ->
    {E,Pre,St};
bc_gen_size_mul(E1, E2, Pre, St0) ->
    {V,St} = new_var(St0),
    {V,[#iset{var=V,arg=bc_mul(E1, E2)}|Pre],St}.

bc_mul(E1, #c_literal{val=1}) ->
    E1;
bc_mul(E1, E2) ->
    #icall{module=#c_literal{val=erlang},
	   name=#c_literal{val='*'},
	   args=[E1,E2]}.

bc_div(E1, E2) ->
    #icall{module=#c_literal{val=erlang},
	   name=#c_literal{val='div'},
	   args=[E1,E2]}.

bc_add(E1, #c_literal{val=0}) ->
    E1;
bc_add(E1, E2) ->
    #icall{module=#c_literal{val=erlang},
	   name=#c_literal{val='+'},
	   args=[E1,E2]}.

bc_bsr(E1, E2) ->
    #icall{module=#c_literal{val=erlang},
	   name=#c_literal{val='bsr'},
	   args=[E1,E2]}.

%% is_guard_test(Expression) -> true | false.
%%  Test if a general expression is a guard test.  Use erl_lint here
%%  as it now allows sys_pre_expand transformed source.

is_guard_test(E) -> erl_lint:is_guard_test(E).

%% novars(Expr, State) -> {Novars,[PreExpr],State}.
%%  Generate a novars expression, basically a call or a safe.  At this
%%  level we do not need to do a deep check.

novars(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_novars(E1, St1),
    {Se,Eps ++ Sps,St2}.

force_novars(#iapply{}=App, St) -> {App,[],St};
force_novars(#icall{}=Call, St) -> {Call,[],St};
force_novars(#ifun{}=Fun, St) -> {Fun,[],St};	%These are novars too
force_novars(#ibinary{}=Bin, St) -> {Bin,[],St};
force_novars(Ce, St) ->
    force_safe(Ce, St).

%% safe(Expr, State) -> {Safe,[PreExpr],State}.
%%  Generate an internal safe expression.  These are simples without
%%  binaries which can fail.  At this level we do not need to do a
%%  deep check.  Must do special things with matches here.

safe(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_safe(E1, St1),
    {Se,Eps ++ Sps,St2}.

safe_list(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = safe(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

force_safe(#imatch{pat=P,arg=E}=Imatch, St0) ->
    {Le,Lps0,St1} = force_safe(E, St0),
    Lps = Lps0 ++ [Imatch#imatch{arg=Le}],

    %% Make sure we don't duplicate the expression E. sys_core_fold
    %% will usually optimize away the duplicate expression, but may
    %% generate a warning while doing so.
    case Le of
	#c_var{} ->
	    %% Le is a variable.
	    %% Thus: P = Le, Le.  (Traditional, since the V2 compiler.)
	    {Le,Lps,St1};
	_ ->
	    %% Le is not a variable.
	    %% Thus: NewVar = P = Le, NewVar.   (New for R12B-1.)
	    %%
	    %% Note: It is tempting to rewrite V = Le to V = Le, V,
	    %% but that will generate extra warnings in sys_core_fold
	    %% for this expression:
	    %%
	    %%    [{X,Y} || {X,_} <- E, (Y = X) =:= (Y = 1 + 1)]
	    %%
	    %% (There will be a 'case Y =:= Y of...' which will generate
	    %% a warning.)
	    {V,St2} = new_var(St1),
	    {V,Lps0 ++ [Imatch#imatch{pat=#c_alias{var=V,pat=P},arg=Le}],St2}
    end;
force_safe(Ce, St0) ->
    case is_safe(Ce) of
	true -> {Ce,[],St0};
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{var=V,arg=Ce}],St1}
    end.

is_safe(#c_cons{}) -> true;
is_safe(#c_tuple{}) -> true;
is_safe(#c_var{}) -> true;
is_safe(#c_literal{}) -> true;
is_safe(_) -> false.

%% fold_match(MatchExpr, Pat) -> {MatchPat,Expr}.
%%  Fold nested matches into one match with aliased patterns.

fold_match({match,L,P0,E0}, P) ->
    {P1,E1} = fold_match(E0, P),
    {{match,L,P0,P1},E1};
fold_match(E, P) -> {P,E}.

%% pattern(Pattern, State) -> CorePat.
%% Transform a pattern by removing line numbers.  We also normalise
%% aliases in patterns to standard form, {alias,Pat,[Var]}.

pattern({var,L,V}, St) -> #c_var{anno=lineno_anno(L, St),name=V};
pattern({char,L,C}, St) -> #c_literal{anno=lineno_anno(L, St),val=C};
pattern({integer,L,I}, St) -> #c_literal{anno=lineno_anno(L, St),val=I};
pattern({float,L,F}, St) -> #c_literal{anno=lineno_anno(L, St),val=F};
pattern({atom,L,A}, St) -> #c_literal{anno=lineno_anno(L, St),val=A};
pattern({string,L,S}, St) -> #c_literal{anno=lineno_anno(L, St),val=S};
pattern({nil,L}, St) -> #c_literal{anno=lineno_anno(L, St),val=[]};
pattern({cons,L,H,T}, St) ->
    ann_c_cons(lineno_anno(L, St), pattern(H, St), pattern(T, St));
pattern({tuple,L,Ps}, St) ->
    ann_c_tuple(lineno_anno(L, St), pattern_list(Ps, St));
pattern({bin,L,Ps}, St) ->
    %% We don't create a #ibinary record here, since there is
    %% no need to hold any used/new annotations in a pattern.
    #c_binary{anno=lineno_anno(L, St),segments=pat_bin(Ps, St)};
pattern({match,_,P1,P2}, St) ->
    pat_alias(pattern(P1, St), pattern(P2, St)).

%% pat_bin([BinElement], State) -> [BinSeg].

pat_bin(Ps, St) -> [pat_segment(P, St) || P <- Ps].

pat_segment({bin_element,_,Term,Size,[Type,{unit,Unit}|Flags]}, St) ->
    #c_bitstr{val=pattern(Term, St),size=pattern(Size, St),
	      unit=#c_literal{val=Unit},
	      type=#c_literal{val=Type},
	      flags=#c_literal{val=Flags}}.

%% pat_alias(CorePat, CorePat) -> AliasPat.
%%  Normalise aliases.  Trap bad aliases by throwing 'nomatch'.

pat_alias(#c_var{name=V1}, P2) -> #c_alias{var=#c_var{name=V1},pat=P2};
pat_alias(P1, #c_var{name=V2}) -> #c_alias{var=#c_var{name=V2},pat=P1};
pat_alias(#c_cons{}=Cons, #c_literal{anno=A,val=[H|T]}=S) ->
    pat_alias(Cons, ann_c_cons_skel(A, #c_literal{anno=A,val=H},
				    S#c_literal{val=T}));
pat_alias(#c_literal{anno=A,val=[H|T]}=S, #c_cons{}=Cons) ->
    pat_alias(ann_c_cons_skel(A, #c_literal{anno=A,val=H},
			      S#c_literal{val=T}), Cons);
pat_alias(#c_cons{anno=Anno,hd=H1,tl=T1}, #c_cons{hd=H2,tl=T2}) ->
    ann_c_cons(Anno, pat_alias(H1, H2), pat_alias(T1, T2));
pat_alias(#c_tuple{anno=Anno,es=Es1}, #c_literal{val=T}) when is_tuple(T) ->
    Es2 = [#c_literal{val=E} || E <- tuple_to_list(T)],
    ann_c_tuple(Anno, pat_alias_list(Es1, Es2));
pat_alias(#c_literal{anno=Anno,val=T}, #c_tuple{es=Es2}) when is_tuple(T) ->
    Es1 = [#c_literal{val=E} || E <- tuple_to_list(T)],
    ann_c_tuple(Anno, pat_alias_list(Es1, Es2));
pat_alias(#c_tuple{anno=Anno,es=Es1}, #c_tuple{es=Es2}) ->
    ann_c_tuple(Anno, pat_alias_list(Es1, Es2));
pat_alias(#c_alias{var=V1,pat=P1},
	  #c_alias{var=V2,pat=P2}) ->
    if V1 =:= V2 -> #c_alias{var=V1,pat=pat_alias(P1, P2)};
       true -> #c_alias{var=V1,pat=#c_alias{var=V2,pat=pat_alias(P1, P2)}}
    end;
pat_alias(#c_alias{var=V1,pat=P1}, P2) ->
    #c_alias{var=V1,pat=pat_alias(P1, P2)};
pat_alias(P1, #c_alias{var=V2,pat=P2}) ->
    #c_alias{var=V2,pat=pat_alias(P1, P2)};
pat_alias(P1, P2) ->
    case {set_anno(P1, []),set_anno(P2, [])} of
	{P,P} -> P;
	_ -> throw(nomatch)
    end.

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [];
pat_alias_list(_, _) -> throw(nomatch).

%% pattern_list([P], State) -> [P].

pattern_list(Ps, St) -> [pattern(P, St) || P <- Ps].

%% first([A]) -> [A].
%% last([A]) -> A.

first([_]) -> [];
first([H|T]) -> [H|first(T)].

last([L]) -> L;
last([_|T]) -> last(T).

%% make_vars([Name]) -> [{Var,Name}].

make_vars(Vs) -> [ #c_var{name=V} || V <- Vs ].

%% new_fun_name(Type, State) -> {FunName,State}.

new_fun_name(Type, #core{fcount=C}=St) ->
    {list_to_atom(Type ++ "$^" ++ integer_to_list(C)),St#core{fcount=C+1}}.

%% new_var_name(State) -> {VarName,State}.

new_var_name(#core{vcount=C}=St) ->
    {list_to_atom("cor" ++ integer_to_list(C)),St#core{vcount=C + 1}}.

%% new_var(State) -> {{var,Name},State}.
%% new_var(LineAnno, State) -> {{var,Name},State}.

new_var(St) ->
    new_var([], St).

new_var(Anno, St0) ->
    {New,St} = new_var_name(St0),
    {#c_var{anno=Anno,name=New},St}.

%% new_vars(Count, State) -> {[Var],State}.
%% new_vars(Anno, Count, State) -> {[Var],State}.
%%  Make Count new variables.

new_vars(N, St) -> new_vars_1(N, [], St, []).
new_vars(Anno, N, St) -> new_vars_1(N, Anno, St, []).

new_vars_1(N, Anno, St0, Vs) when N > 0 ->
    {V,St1} = new_var(Anno, St0),
    new_vars_1(N-1, Anno, St1, [V|Vs]);
new_vars_1(0, _, St, Vs) -> {Vs,St}.

function_clause(Ps, Name) ->
    function_clause(Ps, [], Name).

function_clause(Ps, LineAnno, Name) ->
    FcAnno = [{function_name,Name}],
    fail_clause(Ps, FcAnno,
		ann_c_tuple(LineAnno, [#c_literal{val=function_clause}|Ps])).

fail_clause(Pats, Arg) ->
    fail_clause(Pats, [], Arg).

fail_clause(Pats, Anno, Arg) ->
    #iclause{anno=#a{anno=[compiler_generated]},
	     pats=Pats,guard=[],
	     body=[#iprimop{anno=#a{anno=Anno},name=#c_literal{val=match_fail},
			    args=[Arg]}]}.

ubody(B, St) -> uexpr(B, [], St).

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(Cl0, Ks, St0) ->
    {Cl1,_Pvs,Used,New,St1} = uclause(Cl0, Ks, Ks, St0),
    A0 = get_ianno(Cl1),
    A = A0#a{us=Used,ns=New},
    {Cl1#iclause{anno=A},St1}.

uclause(#iclause{anno=Anno,pats=Ps0,guard=G0,body=B0}, Pks, Ks0, St0) ->
    {Ps1,Pg,Pvs,Pus,St1} = upattern_list(Ps0, Pks, St0),
    Pu = union(Pus, intersection(Pvs, Ks0)),
    Pn = subtract(Pvs, Pu),
    Ks1 = union(Pn, Ks0),
    {G1,St2} = uguard(Pg, G0, Ks1, St1),
    Gu = used_in_any(G1),
    Gn = new_in_any(G1),
    Ks2 = union(Gn, Ks1),
    {B1,St3} = uexprs(B0, Ks2, St2),
    Used = intersection(union([Pu,Gu,used_in_any(B1)]), Ks0),
    New = union([Pn,Gn,new_in_any(B1)]),
    {#iclause{anno=Anno,pats=Ps1,guard=G1,body=B1},Pvs,Used,New,St3}.

%% uguard([Test], [Kexpr], [KnownVar], State) -> {[Kexpr],State}.
%%  Build a guard expression list by folding in the equality tests.

uguard([], [], _, St) -> {[],St};
uguard(Pg, [], Ks, St) ->
    %% No guard, so fold together equality tests.
    uguard(first(Pg), [last(Pg)], Ks, St);
uguard(Pg, Gs0, Ks, St0) ->
    %% Gs0 must contain at least one element here.
    {Gs3,St5} = foldr(fun (T, {Gs1,St1}) ->
			      {L,St2} = new_var(St1),
			      {R,St3} = new_var(St2),
			      {[#iset{var=L,arg=T}] ++ first(Gs1) ++
			       [#iset{var=R,arg=last(Gs1)},
				#icall{anno=#a{}, %Must have an #a{}
				       module=#c_literal{val=erlang},
				       name=#c_literal{val='and'},
				       args=[L,R]}],
			       St3}
		      end, {Gs0,St0}, Pg),
    %%ok = io:fwrite("core ~w: ~p~n", [?LINE,Gs3]),
    uexprs(Gs3, Ks, St5).

%% uexprs([Kexpr], [KnownVar], State) -> {[Kexpr],State}.

uexprs([#imatch{anno=A,pat=P0,arg=Arg,fc=Fc}|Les], Ks, St0) ->
    %% Optimise for simple set of unbound variable.
    case upattern(P0, Ks, St0) of
	{#c_var{},[],_Pvs,_Pus,_} ->
	    %% Throw our work away and just set to iset.
	    uexprs([#iset{var=P0,arg=Arg}|Les], Ks, St0);
	_Other ->
	    %% Throw our work away and set to icase.
	    if
		Les =:= [] ->
		    %% Need to explicitly return match "value", make
		    %% safe for efficiency.
		    {La0,Lps,St1} = force_safe(Arg, St0),
		    La = mark_compiler_generated(La0),
		    Mc = #iclause{anno=A,pats=[P0],guard=[],body=[La]},
		    uexprs(Lps ++ [#icase{anno=A,
					  args=[La0],clauses=[Mc],fc=Fc}], Ks, St1);
		true ->
		    Mc = #iclause{anno=A,pats=[P0],guard=[],body=Les},
		    uexprs([#icase{anno=A,args=[Arg],
				   clauses=[Mc],fc=Fc}], Ks, St0)
	    end
    end;
uexprs([Le0|Les0], Ks, St0) ->
    {Le1,St1} = uexpr(Le0, Ks, St0),
    {Les1,St2} = uexprs(Les0, union((get_anno(Le1))#a.ns, Ks), St1),
    {[Le1|Les1],St2};
uexprs([], _, St) -> {[],St}.

%% Mark a "safe" as compiler-generated.
mark_compiler_generated(#c_cons{anno=A,hd=H,tl=T}) ->
    ann_c_cons([compiler_generated|A], mark_compiler_generated(H),
	       mark_compiler_generated(T));
mark_compiler_generated(#c_tuple{anno=A,es=Es0}) ->
    Es = [mark_compiler_generated(E) || E <- Es0],
    ann_c_tuple([compiler_generated|A], Es);
mark_compiler_generated(#c_var{anno=A}=Var) ->
    Var#c_var{anno=[compiler_generated|A]};
mark_compiler_generated(#c_literal{anno=A}=Lit) ->
    Lit#c_literal{anno=[compiler_generated|A]}.

uexpr(#iset{anno=A,var=V,arg=A0}, Ks, St0) ->
    {A1,St1} = uexpr(A0, Ks, St0),
    {#iset{anno=A#a{us=del_element(V#c_var.name, (get_anno(A1))#a.us),
		    ns=add_element(V#c_var.name, (get_anno(A1))#a.ns)},
	   var=V,arg=A1},St1};
%% imatch done in uexprs.
uexpr(#iletrec{anno=A,defs=Fs0,body=B0}, Ks, St0) ->
    %%ok = io:fwrite("~w: ~p~n", [?LINE,{Fs0,B0}]),
    {Fs1,St1} = mapfoldl(fun ({Name,F0}, S0) ->
				 {F1,S1} = uexpr(F0, Ks, S0),
				 {{Name,F1},S1}
			 end, St0, Fs0),
    {B1,St2} = uexprs(B0, Ks, St1),
    Used = used_in_any(map(fun ({_,F}) -> F end, Fs1) ++ B1),
    {#iletrec{anno=A#a{us=Used,ns=[]},defs=Fs1,body=B1},St2};
uexpr(#icase{anno=A,args=As0,clauses=Cs0,fc=Fc0}, Ks, St0) ->
    %% As0 will never generate new variables.
    {As1,St1} = uexpr_list(As0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Fc1,St3} = uclause(Fc0, Ks, St2),
    Used = union(used_in_any(As1), used_in_any(Cs1)),
    New = new_in_all(Cs1),
    {#icase{anno=A#a{us=Used,ns=New},args=As1,clauses=Cs1,fc=Fc1},St3};
uexpr(#ifun{anno=A,id=Id,vars=As,clauses=Cs0,fc=Fc0}, Ks0, St0) ->
    Avs = lit_list_vars(As),
    Ks1 = union(Avs, Ks0),
    {Cs1,St1} = ufun_clauses(Cs0, Ks1, St0),
    {Fc1,St2} = ufun_clause(Fc0, Ks1, St1),
    Used = subtract(intersection(used_in_any(Cs1), Ks0), Avs),
    {#ifun{anno=A#a{us=Used,ns=[]},id=Id,vars=As,clauses=Cs1,fc=Fc1},St2};
uexpr(#iapply{anno=A,op=Op,args=As}, _, St) ->
    Used = union(lit_vars(Op), lit_list_vars(As)),
    {#iapply{anno=A#a{us=Used},op=Op,args=As},St};
uexpr(#iprimop{anno=A,name=Name,args=As}, _, St) ->
    Used = lit_list_vars(As),
    {#iprimop{anno=A#a{us=Used},name=Name,args=As},St};
uexpr(#icall{anno=A,module=Mod,name=Name,args=As}, _, St) ->
    Used = union([lit_vars(Mod),lit_vars(Name),lit_list_vars(As)]),
    {#icall{anno=A#a{us=Used},module=Mod,name=Name,args=As},St};
uexpr(#itry{anno=A,args=As0,vars=Vs,body=Bs0,evars=Evs,handler=Hs0}, Ks, St0) ->
    %% Note that we export only from body and exception.
    {As1,St1} = uexprs(As0, Ks, St0),
    {Bs1,St2} = uexprs(Bs0, Ks, St1),
    {Hs1,St3} = uexprs(Hs0, Ks, St2),
    Used = intersection(used_in_any(Bs1++Hs1++As1), Ks),
    New = new_in_all(Bs1++Hs1),
    {#itry{anno=A#a{us=Used,ns=New},
	   args=As1,vars=Vs,body=Bs1,evars=Evs,handler=Hs1},St3};
uexpr(#icatch{anno=A,body=Es0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    {#icatch{anno=A#a{us=used_in_any(Es1)},body=Es1},St1};
uexpr(#ireceive1{anno=A,clauses=Cs0}, Ks, St0) ->
    {Cs1,St1} = uclauses(Cs0, Ks, St0),
    {#ireceive1{anno=A#a{us=used_in_any(Cs1),ns=new_in_all(Cs1)},
		clauses=Cs1},St1};
uexpr(#ireceive2{anno=A,clauses=Cs0,timeout=Te0,action=Tes0}, Ks, St0) ->
    %% Te0 will never generate new variables.
    {Te1,St1} = uexpr(Te0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Tes1,St3} = uexprs(Tes0, Ks, St2),
    Used = union([used_in_any(Cs1),used_in_any(Tes1),(get_anno(Te1))#a.us]),
    New = case Cs1 of
	      [] -> new_in_any(Tes1);
	      _ -> intersection(new_in_all(Cs1), new_in_any(Tes1))
	  end,
    {#ireceive2{anno=A#a{us=Used,ns=New},
		clauses=Cs1,timeout=Te1,action=Tes1},St3};
uexpr(#iprotect{anno=A,body=Es0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    Used = used_in_any(Es1),
    {#iprotect{anno=A#a{us=Used},body=Es1},St1}; %No new variables escape!
uexpr(#ibinary{anno=A,segments=Ss}, _, St) ->
    Used = bitstr_vars(Ss),
    {#ibinary{anno=A#a{us=Used},segments=Ss},St};
uexpr(#c_literal{}=Lit, _, St) ->
    Anno = get_anno(Lit),
    {set_anno(Lit, #a{us=[],anno=Anno}),St};
uexpr(Lit, _, St) ->
    true = is_simple(Lit),			%Sanity check!
    Vs = lit_vars(Lit),
    Anno = get_anno(Lit),
    {set_anno(Lit, #a{us=Vs,anno=Anno}),St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% ufun_clauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

ufun_clauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> ufun_clause(Lc, Ks, St) end, St0, Lcs).

%% ufun_clause(Lclause, [KnownVar], State) -> {Lclause,State}.

ufun_clause(Cl0, Ks, St0) ->
    {Cl1,Pvs,Used,_,St1} = uclause(Cl0, [], Ks, St0),
    A0 = get_ianno(Cl1),
    A = A0#a{us=subtract(intersection(Used, Ks), Pvs),ns=[]},
    {Cl1#iclause{anno=A},St1}.

%% upattern(Pat, [KnownVar], State) ->
%%              {Pat,[GuardTest],[NewVar],[UsedVar],State}.

upattern(#c_var{name='_'}, _, St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},[],[New],[],St1};
upattern(#c_var{name=V}=Var, Ks, St0) ->
    case is_element(V, Ks) of
	true ->
	    {N,St1} = new_var_name(St0),
	    New = #c_var{name=N},
	    Test = #icall{anno=#a{us=add_element(N, [V])},
			  module=#c_literal{val=erlang},
			  name=#c_literal{val='=:='},
			  args=[New,Var]},
	    %% Test doesn't need protecting.
	    {New,[Test],[N],[],St1};
	false -> {Var,[],[V],[],St0}
    end;
upattern(#c_cons{hd=H0,tl=T0}=Cons, Ks, St0) ->
    {H1,Hg,Hv,Hu,St1} = upattern(H0, Ks, St0),
    {T1,Tg,Tv,Tu,St2} = upattern(T0, union(Hv, Ks), St1),
    {Cons#c_cons{hd=H1,tl=T1},Hg ++ Tg,union(Hv, Tv),union(Hu, Tu),St2};
upattern(#c_tuple{es=Es0}=Tuple, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {Tuple#c_tuple{es=Es1},Esg,Esv,Eus,St1};
upattern(#c_binary{segments=Es0}=Bin, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upat_bin(Es0, Ks, St0),
    {Bin#c_binary{segments=Es1},Esg,Esv,Eus,St1};
upattern(#c_alias{var=V0,pat=P0}=Alias, Ks, St0) ->
    {V1,Vg,Vv,Vu,St1} = upattern(V0, Ks, St0),
    {P1,Pg,Pv,Pu,St2} = upattern(P0, union(Vv, Ks), St1),
    {Alias#c_alias{var=V1,pat=P1},Vg ++ Pg,union(Vv, Pv),union(Vu, Pu),St2};
upattern(Other, _, St) -> {Other,[],[],[],St}.	%Constants

%% upattern_list([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upattern_list([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upattern(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upattern_list(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upattern_list([], _, St) -> {[],[],[],[],St}.    

%% upat_bin([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.
upat_bin(Es0, Ks, St0) ->
  upat_bin(Es0, Ks, [], St0).

%% upat_bin([Pat], [KnownVar], [LocalVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.
upat_bin([P0|Ps0], Ks, Bs, St0) ->
    {P1,Pg,Pv,Pu,Bs1,St1} = upat_element(P0, Ks, Bs, St0),
    {Ps1,Psg,Psv,Psu,St2} = upat_bin(Ps0, union(Pv, Ks), Bs1, St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upat_bin([], _, _, St) -> {[],[],[],[],St}.    


%% upat_element(Segment, [KnownVar], [LocalVar], State) ->
%%                        {Segment,[GuardTest],[NewVar],[UsedVar],[LocalVar],State}
upat_element(#c_bitstr{val=H0,size=Sz}=Seg, Ks, Bs, St0) ->
  {H1,Hg,Hv,[],St1} = upattern(H0, Ks, St0),
  Bs1 = case H0 of
	  #c_var{name=Hname} ->
	    case H1 of
	      #c_var{name=Hname} ->
		Bs;
	      #c_var{name=Other} ->
		[{Hname, Other}|Bs]
	    end;
	  _ ->
	    Bs
	end,
  {Sz1, Us} = case Sz of
		  #c_var{name=Vname} -> 
		    rename_bitstr_size(Vname, Bs);
		  _Other -> {Sz, []}
		end,
  {Seg#c_bitstr{val=H1, size=Sz1},Hg,Hv,Us,Bs1,St1}.

rename_bitstr_size(V, [{V, N}|_]) ->
   New = #c_var{name=N},
  {New, [N]};
rename_bitstr_size(V, [_|Rest]) ->
  rename_bitstr_size(V, Rest);
rename_bitstr_size(V, []) ->
  Old = #c_var{name=V},
  {Old, [V]}.
 
used_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((get_anno(Le))#a.us, Ns) end,
	  [], Les).

new_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((get_anno(Le))#a.ns, Ns) end,
	  [], Les).

new_in_all([Le|Les]) ->
    foldl(fun (L, Ns) -> intersection((get_anno(L))#a.ns, Ns) end,
	  (get_anno(Le))#a.ns, Les);
new_in_all([]) -> [].

%% The AfterVars are the variables which are used afterwards.  We need
%% this to work out which variables are actually exported and used
%% from case/receive.  In subblocks/clauses the AfterVars of the block
%% are just the exported variables.

cbody(B0, St0) ->
    {B1,_,_,St1} = cexpr(B0, [], St0),
    {B1,St1}.

%% cclause(Lclause, [AfterVar], State) -> {Cclause,State}.
%%  The AfterVars are the exported variables.

cclause(#iclause{anno=#a{anno=Anno},pats=Ps,guard=G0,body=B0}, Exp, St0) ->
    {B1,_Us1,St1} = cexprs(B0, Exp, St0),
    {G1,St2} = cguard(G0, St1),
    {#c_clause{anno=Anno,pats=Ps,guard=G1,body=B1},St2}.

cclauses(Lcs, Es, St0) ->
    mapfoldl(fun (Lc, St) -> cclause(Lc, Es, St) end, St0, Lcs).

cguard([], St) -> {#c_literal{val=true},St};
cguard(Gs, St0) ->
    {G,_,St1} = cexprs(Gs, [], St0),
    {G,St1}.
 
%% cexprs([Lexpr], [AfterVar], State) -> {Cexpr,[AfterVar],State}.
%%  Must be sneaky here at the last expr when combining exports for the
%%  whole sequence and exports for that expr.

cexprs([#iset{var=#c_var{name=Name}=Var}=Iset], As, St) ->
    %% Make return value explicit, and make Var true top level.
    cexprs([Iset,Var#c_var{anno=#a{us=[Name]}}], As, St);
cexprs([Le], As, St0) ->
    {Ce,Es,Us,St1} = cexpr(Le, As, St0),
    Exp = make_vars(As),			%The export variables
    if
	Es =:= [] -> {core_lib:make_values([Ce|Exp]),union(Us, As),St1};
	true ->
	    {R,St2} = new_var(St1),
	    {#c_let{anno=get_lineno_anno(Ce),
		    vars=[R|make_vars(Es)],arg=Ce,
		    body=core_lib:make_values([R|Exp])},
	     union(Us, As),St2}
    end;
cexprs([#iset{anno=#a{anno=A},var=V,arg=A0}|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {A1,Es,Us,St2} = cexpr(A0, As1, St1),
    {#c_let{anno=A,vars=[V|make_vars(Es)],arg=A1,body=Ces},
     union(Us, As1),St2};
cexprs([Le|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {Ce,Es,Us,St2} = cexpr(Le, As1, St1),
    if
	Es =:= [] ->
	    {#c_seq{arg=Ce,body=Ces},union(Us, As1),St2};
	true ->
	    {R,St3} = new_var(St2),
	    {#c_let{vars=[R|make_vars(Es)],arg=Ce,body=Ces},
	     union(Us, As1),St3}
    end.

%% cexpr(Lexpr, [AfterVar], State) -> {Cexpr,[ExpVar],[UsedVar],State}.

cexpr(#iletrec{anno=A,defs=Fs0,body=B0}, As, St0) ->
    {Fs1,{_,St1}} = mapfoldl(fun ({{_Name,_Arity}=NA,F0}, {Used,S0}) ->
				     {F1,[],Us,S1} = cexpr(F0, [], S0),
				     {{#c_var{name=NA},F1},
				      {union(Us, Used),S1}}
			     end, {[],St0}, Fs0),
    Exp = intersection(A#a.ns, As),
    {B1,_Us,St2} = cexprs(B0, Exp, St1),
    {#c_letrec{anno=A#a.anno,defs=Fs1,body=B1},Exp,A#a.us,St2};
cexpr(#icase{anno=A,args=Largs,clauses=Lcs,fc=Lfc}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cargs,St1} = foldr(fun (La, {Cas,Sta}) ->
				{Ca,[],_Us1,Stb} = cexpr(La, As, Sta),
				{[Ca|Cas],Stb}
			end, {[],St0}, Largs),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Cfc,St3} = cclause(Lfc, [], St2),		%Never exports
    {#c_case{anno=A#a.anno,
	     arg=core_lib:make_values(Cargs),clauses=Ccs ++ [Cfc]},
     Exp,A#a.us,St3};
cexpr(#ireceive1{anno=A,clauses=Lcs}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Ccs,St1} = cclauses(Lcs, Exp, St0),
    {#c_receive{anno=A#a.anno,
		clauses=Ccs,
		timeout=#c_literal{val=infinity},action=#c_literal{val=true}},
     Exp,A#a.us,St1};
cexpr(#ireceive2{anno=A,clauses=Lcs,timeout=Lto,action=Les}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cto,[],_Us1,St1} = cexpr(Lto, As, St0),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Ces,_Us2,St3} = cexprs(Les, Exp, St2),
    {#c_receive{anno=A#a.anno,
		clauses=Ccs,timeout=Cto,action=Ces},
     Exp,A#a.us,St3};
cexpr(#itry{anno=A,args=La,vars=Vs,body=Lb,evars=Evs,handler=Lh}, As, St0) ->
    Exp = intersection(A#a.ns, As),           %Exports
    {Ca,_Us1,St1} = cexprs(La, [], St0),
    {Cb,_Us2,St2} = cexprs(Lb, Exp, St1),
    {Ch,_Us3,St3} = cexprs(Lh, Exp, St2),
    {#c_try{anno=A#a.anno,arg=Ca,vars=Vs,body=Cb,evars=Evs,handler=Ch},
     Exp,A#a.us,St3};
cexpr(#icatch{anno=A,body=Les}, _As, St0) ->
    {Ces,_Us1,St1} = cexprs(Les, [], St0),	%Never export!
    {#c_catch{body=Ces},[],A#a.us,St1};
cexpr(#ifun{anno=A,id=Id,vars=Args,clauses=Lcs,fc=Lfc}, _As, St0) ->
    {Ccs,St1} = cclauses(Lcs, [], St0),		%NEVER export!
    {Cfc,St2} = cclause(Lfc, [], St1),
    Anno = A#a.anno,
    {#c_fun{anno=Id++Anno,vars=Args,
	    body=#c_case{anno=Anno,
			 arg=set_anno(core_lib:make_values(Args), Anno),
			 clauses=Ccs ++ [Cfc]}},
     [],A#a.us,St2};
cexpr(#iapply{anno=A,op=Op,args=Args}, _As, St) ->
    {#c_apply{anno=A#a.anno,op=Op,args=Args},[],A#a.us,St};
cexpr(#icall{anno=A,module=Mod,name=Name,args=Args}, _As, St) ->
    {#c_call{anno=A#a.anno,module=Mod,name=Name,args=Args},[],A#a.us,St};
cexpr(#iprimop{anno=A,name=Name,args=Args}, _As, St) ->
    {#c_primop{anno=A#a.anno,name=Name,args=Args},[],A#a.us,St};
cexpr(#iprotect{anno=A,body=Es}, _As, St0) ->
    {Ce,_,St1} = cexprs(Es, [], St0),
    V = #c_var{name='Try'},		%The names are arbitrary
    Vs = [#c_var{name='T'},#c_var{name='R'}],
    {#c_try{anno=A#a.anno,arg=Ce,vars=[V],body=V,
	    evars=Vs,handler=#c_literal{val=false}},
     [],A#a.us,St1};
cexpr(#ibinary{anno=#a{anno=Anno,us=Us},segments=Segs}, _As, St) ->
    {#c_binary{anno=Anno,segments=Segs},[],Us,St};
cexpr(#c_literal{}=Lit, _As, St) ->
    Anno = get_anno(Lit),
    Vs = Anno#a.us,
    {set_anno(Lit, Anno#a.anno),[],Vs,St};
cexpr(Lit, _As, St) ->
    true = is_simple(Lit),		%Sanity check!
    Anno = get_anno(Lit),
    Vs = Anno#a.us,
    %%Vs = lit_vars(Lit),
    {set_anno(Lit, Anno#a.anno),[],Vs,St}.

%% Kill the id annotations for any fun inside the expression.
%% Necessary when duplicating code in try ... after.

kill_id_anns(#ifun{clauses=Cs0}=Fun) ->
    Cs = kill_id_anns(Cs0),
    Fun#ifun{clauses=Cs,id=[]};
kill_id_anns(#a{}=A) ->
    %% Optimization: Don't waste time searching for funs inside annotations.
    A;
kill_id_anns([H|T]) ->
    [kill_id_anns(H)|kill_id_anns(T)];
kill_id_anns([]) -> [];
kill_id_anns(Tuple) when is_tuple(Tuple) ->
    L0 = tuple_to_list(Tuple),
    L = kill_id_anns(L0),
    list_to_tuple(L);
kill_id_anns(Other) -> Other.

%% lit_vars(Literal) -> [Var].

lit_vars(Lit) -> lit_vars(Lit, []).

lit_vars(#c_cons{hd=H,tl=T}, Vs) -> lit_vars(H, lit_vars(T, Vs));
lit_vars(#c_tuple{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(#c_var{name=V}, Vs) -> add_element(V, Vs); 
lit_vars(_, Vs) -> Vs.				%These are atomic

% lit_bin_vars(Segs, Vs) ->
%     foldl(fun (#c_bitstr{val=V,size=S}, Vs0) ->
% 		  lit_vars(V, lit_vars(S, Vs0))
% 	  end, Vs, Segs).

lit_list_vars(Ls) -> lit_list_vars(Ls, []).

lit_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> lit_vars(L, Vs0) end, Vs, Ls).

bitstr_vars(Segs) ->
    bitstr_vars(Segs, []).

bitstr_vars(Segs, Vs) ->
    foldl(fun (#c_bitstr{val=V,size=S}, Vs0) ->
 		  lit_vars(V, lit_vars(S, Vs0))
	  end, Vs, Segs).

lineno_anno(L, St) ->
    {line, Line} = erl_parse:get_attribute(L, line),
    [Line] ++ St#core.file.

get_ianno(Ce) ->
    case get_anno(Ce) of
	#a{}=A -> A;
	A when is_list(A) -> #a{anno=A}
    end.

get_lineno_anno(Ce) ->
    case get_anno(Ce) of
	#a{anno=A} -> A;
	A when is_list(A) -> A
    end.

location(L) ->
    {location,Location} = erl_parse:get_attribute(L, location),
    Location.

abs_line(L) ->
    erl_parse:set_line(L, fun(Line) -> abs(Line) end).

neg_line(L) ->
    erl_parse:set_line(L, fun(Line) -> -abs(Line) end).

%%
%% The following three functions are used both with cerl:cerl() and with i()'s
%%
-spec get_anno(cerl:cerl() | i()) -> term().

get_anno(C) -> element(2, C).

-spec set_anno(cerl:cerl() | i(), term()) -> cerl:cerl().

set_anno(C, A) -> setelement(2, C, A).

-spec is_simple(cerl:cerl() | i()) -> boolean().

is_simple(#c_var{}) -> true;
is_simple(#c_literal{}) -> true;
is_simple(#c_cons{hd=H,tl=T}) ->
    is_simple(H) andalso is_simple(T);
is_simple(#c_tuple{es=Es}) -> is_simple_list(Es);
is_simple(_) -> false.

-spec is_simple_list([cerl:cerl()]) -> boolean().

is_simple_list(Es) -> lists:all(fun is_simple/1, Es).

%%%
%%% Handling of warnings.
%%%

-type err_desc() :: 'bad_binary' | 'nomatch'.

-spec format_error(err_desc()) -> nonempty_string().

format_error(nomatch) ->
    "pattern cannot possibly match";
format_error(bad_binary) ->
    "binary construction will fail because of a type mismatch".

add_warning(Line, Term, #core{ws=Ws,file=[{file,File}]}=St) when Line >= 0 ->
    St#core{ws=[{File,[{location(Line),?MODULE,Term}]}|Ws]};
add_warning(_, _, St) -> St.
