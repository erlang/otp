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
%% binaries and maps are novars
%% let/set arguments are expressions
%% fun is not a safe

-module(v3_core).

-export([module/2,format_error/1]).

-import(lists, [reverse/1,reverse/2,map/2,member/2,foldl/3,foldr/3,mapfoldl/3,
		splitwith/2,keyfind/3,sort/1,foreach/2,droplast/1,last/1]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).
-import(cerl, [ann_c_cons/3,ann_c_tuple/2,c_tuple/1,
	       ann_c_map/3]).

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
-record(ifun,      {anno=#a{},id,vars,clauses,fc,name=unnamed}).
-record(iletrec,   {anno=#a{},defs,body}).
-record(imatch,    {anno=#a{},pat,guard=[],arg,fc}).
-record(iprimop,   {anno=#a{},name,args}).
-record(iprotect,  {anno=#a{},body}).
-record(ireceive1, {anno=#a{},clauses}).
-record(ireceive2, {anno=#a{},clauses,timeout,action}).
-record(iset,      {anno=#a{},var,arg}).
-record(itry,      {anno=#a{},args,vars,body,evars,handler}).
-record(ifilter,   {anno=#a{},arg}).
-record(igen,      {anno=#a{},ceps=[],acc_pat,acc_guard,
		    skip_pat,tail,tail_pat,arg}).
-record(isimple,   {anno=#a{},term :: cerl:cerl()}).

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
-type ifilter()   :: #ifilter{}.
-type igen()      :: #igen{}.
-type isimple()   :: #isimple{}.

-type i() :: iapply()   | ibinary()   | icall()     | icase()  | icatch()
           | iclause()  | ifun()      | iletrec()   | imatch() | iprimop()
           | iprotect() | ireceive1() | ireceive2() | iset()   | itry()
           | ifilter()  | igen()      | isimple().

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

attribute(Attribute) ->
    Fun = fun(A) ->  [erl_anno:location(A)] end,
    {attribute,Line,Name,Val} = erl_parse:map_anno(Fun, Attribute),
    {#c_literal{val=Name, anno=Line}, #c_literal{val=Val, anno=Line}}.

%% function_dump(module_info,_,_,_) -> ok;
%% function_dump(Name,Arity,Format,Terms) ->
%%     io:format("~w/~w " ++ Format,[Name,Arity]++Terms),
%%     ok.

function({function,_,Name,Arity,Cs0}, Ws0, File, Opts) ->
    St0 = #core{vcount=0,opts=Opts,ws=Ws0,file=[{file,File}]},
    {B0,St1} = body(Cs0, Name, Arity, St0),
    %% ok = function_dump(Name,Arity,"body:~n~p~n",[B0]),
    {B1,St2} = ubody(B0, St1),
    %% ok = function_dump(Name,Arity,"ubody:~n~p~n",[B1]),
    {B2,#core{ws=Ws}} = cbody(B1, St2),
    %% ok = function_dump(Name,Arity,"cbody:~n~p~n",[B2]),
    {{#c_var{name={Name,Arity}},B2},Ws}.

body(Cs0, Name, Arity, St0) ->
    Anno = lineno_anno(element(2, hd(Cs0)), St0),
    {Args,St1} = new_vars(Anno, Arity, St0),
    case clauses(Cs0, St1) of
	{Cs1,[],St2} ->
	    {Ps,St3} = new_vars(Arity, St2),    %Need new variables here
	    Fc = function_clause(Ps, Anno, {Name,Arity}),
	    {#ifun{anno=#a{anno=Anno},id=[],vars=Args,clauses=Cs1,fc=Fc},St3};
	{Cs1,Eps,St2} ->
	    %% We have pre-expressions from patterns and
	    %% these needs to be letified before matching
	    %% since only bound variables are allowed
	    AnnoGen = #a{anno=[compiler_generated]},
	    {Ps1,St3} = new_vars(Arity, St2),    %Need new variables here
	    Fc1 = function_clause(Ps1, Anno, {Name,Arity}),
	    {Ps2,St4} = new_vars(Arity, St3),    %Need new variables here
	    Fc2 = function_clause(Ps2, Anno, {Name,Arity}),
	    Case = #icase{anno=AnnoGen,args=Args,
			  clauses=Cs1,
			  fc=Fc2},
	    {#ifun{anno=#a{anno=Anno},id=[],vars=Args,
		   clauses=[#iclause{anno=AnnoGen,pats=Ps1,
				     guard=[#c_literal{val=true}],
				     body=Eps ++ [Case]}],
		   fc=Fc1},St4}
    end.

%% clause(Clause, State) -> {Cclause,State} | noclause.
%% clauses([Clause], State) -> {[Cclause],State}.
%%  Convert clauses.  Trap bad pattern aliases and remove clause from
%%  clause list.

clauses([C0|Cs0],St0) ->
    case clause(C0, St0) of
	{noclause,_,St} -> clauses(Cs0,St);
	{C,Eps1,St1} ->
	    {Cs,Eps2,St2} = clauses(Cs0, St1),
	    {[C|Cs],Eps1++Eps2,St2}
    end;
clauses([],St) -> {[],[],St}.

clause({clause,Lc,H0,G0,B0}, St0) ->
    try head(H0, St0) of
	{H1,Eps,St1} ->
	    {G1,St2} = guard(G0, St1),
	    {B1,St3} = exprs(B0, St2),
            Anno = lineno_anno(Lc, St3),
	    {#iclause{anno=#a{anno=Anno},pats=H1,guard=G1,body=B1},Eps,St3}
    catch
	throw:nomatch ->
	    St = add_warning(Lc, nomatch, St0),
	    {noclause,[],St}			%Bad pattern
    end.

clause_arity({clause,_,H0,_,_}) -> length(H0).

%% head([P], State) -> {[P],[Cexpr],State}.

head(Ps, St) ->
    pattern_list(Ps, St).

%% guard([Expr], State) -> {[Cexpr],State}.
%%  Build an explict and/or tree of guard alternatives, then traverse
%%  top-level and/or tree and "protect" inner tests.

guard([], St) -> {[],St};
guard(Gs0, St0) ->
    Gs1 = foldr(fun (Gt0, Rhs) ->
			Gt1 = guard_tests(Gt0),
			L = element(2, Gt1),
			{op,L,'or',Gt1,Rhs}
		end, guard_tests(last(Gs0)), droplast(Gs0)),
    {Gs,St} = gexpr_top(Gs1, St0#core{in_guard=true}),
    {Gs,St#core{in_guard=false}}.
    
guard_tests(Gs) ->
    L = element(2, hd(Gs)),
    {protect,L,foldr(fun (G, Rhs) -> {op,L,'and',G,Rhs} end, last(Gs), droplast(Gs))}.

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
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch_guard(L, E1, V, E2, False),
    gexpr(E, Bools, St);
gexpr({op,L,'orelse',E1,E2}, Bools, St0) ->
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch_guard(L, E1, V, True, E2),
    gexpr(E, Bools, St);
gexpr({op,Line,Op,L,R}=E, Bools, St) ->
    case erl_internal:bool_op(Op, 2) of
        true ->
            gexpr_bool(Op, L, R, Bools, St, Line);
        false ->
            gexpr_test(E, Bools, St)
    end;
gexpr({call,Line,{remote,_,{atom,_,erlang},{atom,_,Op}},[L,R]}=E, Bools, St) ->
    case erl_internal:bool_op(Op, 2) of
        true ->
            gexpr_bool(Op, L, R, Bools, St, Line);
        false ->
            gexpr_test(E, Bools, St)
    end;
gexpr({op,Line,'not',A}, Bools, St) ->
    gexpr_not(A, Bools, St, Line);
gexpr({call,Line,{remote,_,{atom,_,erlang},{atom,_,'not'}},[A]}, Bools, St) ->
    gexpr_not(A, Bools, St, Line);
gexpr(E0, Bools, St0) ->
    gexpr_test(E0, Bools, St0).

%% gexpr_not(L, R, Bools, State) -> {Cexpr,[PreExp],Bools,State}.
%%  Generate a guard for boolean operators

gexpr_bool(Op, L, R, Bools0, St0, Line) ->
    {Le,Lps,Bools1,St1} = gexpr(L, Bools0, St0),
    {Ll,Llps,St2} = force_safe(Le, St1),
    {Re,Rps,Bools,St3} = gexpr(R, Bools1, St2),
    {Rl,Rlps,St4} = force_safe(Re, St3),
    Anno = lineno_anno(Line, St4),
    {#icall{anno=#a{anno=Anno}, %Must have an #a{}
            module=#c_literal{anno=Anno,val=erlang},
            name=#c_literal{anno=Anno,val=Op},
            args=[Ll,Rl]},Lps ++ Llps ++ Rps ++ Rlps,Bools,St4}.

%% gexpr_not(Expr, Bools, State) -> {Cexpr,[PreExp],Bools,State}.
%%  Generate an erlang:'not'/1 guard test.

gexpr_not(A, Bools0, St0, Line) ->
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
            {#icall{anno=#a{anno=Anno}, %Must have an #a{}
                    module=#c_literal{anno=Anno,val=erlang},
                    name=#c_literal{anno=Anno,val='not'},
                    args=[Al]},Aps ++ Alps,Bools,St2}
    end.

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
                erl_internal:comp_op(N, Ar) orelse
                erl_internal:bool_op(N, Ar) of
		true -> {E1,Eps0,Bools0,St1};
		false ->
		    Lanno = Anno#a.anno,
		    {New,St2} = new_var(Lanno, St1),
		    Bools = [New|Bools0],
		    {icall_eq_true(New),
		     Eps0 ++ [#iset{anno=Anno,var=New,arg=E1}],Bools,St2}
	    end;
	_ ->
	    Lanno = get_lineno_anno(E1),
	    ACompGen = #a{anno=[compiler_generated]},
	    case is_simple(E1) of
		true ->
		    Bools = [E1|Bools0],
		    {icall_eq_true(E1),Eps0,Bools,St1};
		false ->
		    {New,St2} = new_var(Lanno, St1),
		    Bools = [New|Bools0],
		    {icall_eq_true(New),
		     Eps0 ++ [#iset{anno=ACompGen,var=New,arg=E1}],Bools,St2}
	    end
    end.

icall_eq_true(Arg) ->
    #icall{anno=#a{anno=[compiler_generated]},
	   module=#c_literal{val=erlang},
	   name=#c_literal{val='=:='},
	   args=[Arg,#c_literal{val=true}]}.

force_booleans(Vs0, E, Eps, St) ->
    Vs1 = [set_anno(V, []) || V <- Vs0],
    Vs = unforce(E, Eps, Vs1),
    force_booleans_1(Vs, E, Eps, St).

force_booleans_1([], E, Eps, St) ->
    {E,Eps,St};
force_booleans_1([V|Vs], E0, Eps0, St0) ->
    {E1,Eps1,St1} = force_safe(E0, St0),
    ACompGen = #a{anno=[compiler_generated]},
    Call = #icall{anno=ACompGen,module=#c_literal{val=erlang},
		  name=#c_literal{val=is_boolean},
		  args=[V]},
    {New,St} = new_var([], St1),
    Iset = #iset{var=New,arg=Call},
    Eps = Eps0 ++ Eps1 ++ [Iset],
    E = #icall{anno=ACompGen,
	       module=#c_literal{val=erlang},name=#c_literal{val='and'},
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
unforce_tree([#c_var{name=V}], D) ->
    gb_trees:get(V, D).

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
expr({char,L,C}, St) -> {#c_literal{anno=full_anno(L, St),val=C},[],St};
expr({integer,L,I}, St) -> {#c_literal{anno=full_anno(L, St),val=I},[],St};
expr({float,L,F}, St) -> {#c_literal{anno=full_anno(L, St),val=F},[],St};
expr({atom,L,A}, St) -> {#c_literal{anno=full_anno(L, St),val=A},[],St};
expr({nil,L}, St) -> {#c_literal{anno=full_anno(L, St),val=[]},[],St};
expr({string,L,S}, St) -> {#c_literal{anno=full_anno(L, St),val=S},[],St};
expr({cons,L,H0,T0}, St0) ->
    {H1,Hps,St1} = safe(H0, St0),
    {T1,Tps,St2} = safe(T0, St1),
    A = full_anno(L, St2),
    {annotate_cons(A, H1, T1, St2),Hps ++ Tps,St2};
expr({lc,L,E,Qs0}, St0) ->
    {Qs1,St1} = preprocess_quals(L, Qs0, St0),
    lc_tq(L, E, Qs1, #c_literal{anno=lineno_anno(L, St1),val=[]}, St1);
expr({bc,L,E,Qs}, St) ->
    bc_tq(L, E, Qs, St);
expr({tuple,L,Es0}, St0) ->
    {Es1,Eps,St1} = safe_list(Es0, St0),
    A = record_anno(L, St1),
    {annotate_tuple(A, Es1, St1),Eps,St1};
expr({map,L,Es0}, St0) ->
    map_build_pairs(#c_literal{val=#{}}, Es0, full_anno(L, St0), St0);
expr({map,L,M,Es}, St) ->
    expr_map(M, Es, L, St);
expr({bin,L,Es0}, St0) ->
    try expr_bin(Es0, full_anno(L, St0), St0) of
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
    {Es1,St1} = exprs(droplast(Es0), St0),
    {E1,Eps,St2} = expr(last(Es0), St1),
    {E1,Es1 ++ Eps,St2};
expr({'if',L,Cs0}, St0) ->
    {Cs1,Ceps,St1} = clauses(Cs0, St0),
    Lanno = lineno_anno(L, St1),
    Fc = fail_clause([], Lanno, #c_literal{val=if_clause}),
    {#icase{anno=#a{anno=Lanno},args=[],clauses=Cs1,fc=Fc},Ceps,St1};
expr({'case',L,E0,Cs0}, St0) ->
    {E1,Eps,St1} = novars(E0, St0),
    {Cs1,Ceps,St2} = clauses(Cs0, St1),
    {Fpat,St3} = new_var(St2),
    Lanno = lineno_anno(L, St2),
    Fc = fail_clause([Fpat], Lanno, c_tuple([#c_literal{val=case_clause},Fpat])),
    {#icase{anno=#a{anno=Lanno},args=[E1],clauses=Cs1,fc=Fc},Eps++Ceps,St3};
expr({'receive',L,Cs0}, St0) ->
    {Cs1,Ceps,St1} = clauses(Cs0, St0),
    {#ireceive1{anno=#a{anno=lineno_anno(L, St1)},clauses=Cs1},Ceps, St1};
expr({'receive',L,Cs0,Te0,Tes0}, St0) ->
    {Te1,Teps,St1} = novars(Te0, St0),
    {Tes1,St2} = exprs(Tes0, St1),
    {Cs1,Ceps,St3} = clauses(Cs0, St2),
    {#ireceive2{anno=#a{anno=lineno_anno(L, St3)},
		clauses=Cs1,timeout=Te1,action=Tes1},Teps++Ceps,St3};
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
    {Cs1,Ceps,St3} = clauses(Cs0, St2),
    {Fpat,St4} = new_var(St3),
    Lanno = lineno_anno(L, St4),
    Fc = fail_clause([Fpat], Lanno,
		     c_tuple([#c_literal{val=try_clause},Fpat])),
    {Evs,Hs,St5} = try_exception(Ecs, St4),
    {#itry{anno=#a{anno=lineno_anno(L, St5)},args=Es1,
	   vars=[V],body=[#icase{anno=#a{anno=Lanno},args=[V],clauses=Cs1,fc=Fc}],
	   evars=Evs,handler=Hs},
     Ceps,St5};
expr({'try',L,Es0,[],[],As0}, St0) ->
    %% 'try ... after ... end'
    {Es1,St1} = exprs(Es0, St0),
    {As1,St2} = exprs(As0, St1),
    {Name,St3} = new_fun_name("after", St2),
    {V,St4} = new_var(St3),		% (must not exist in As1)
    LA = lineno_anno(L, St4),
    Lanno = #a{anno=LA},
    Fc = function_clause([], LA, {Name,0}),
    Fun = #ifun{anno=Lanno,id=[],vars=[],
		clauses=[#iclause{anno=Lanno,pats=[],
				  guard=[#c_literal{val=true}],
				  body=As1}],
		fc=Fc},
    App = #iapply{anno=#a{anno=[compiler_generated|LA]},
		  op=#c_var{anno=LA,name={Name,0}},args=[]},
    {Evs,Hs,St5} = try_after([App], St4),
    Try = #itry{anno=Lanno,args=Es1,vars=[V],body=[App,V],evars=Evs,handler=Hs},
    Letrec = #iletrec{anno=Lanno,defs=[{{Name,0},Fun}],
		      body=[Try]},
    {Letrec,[],St5};
expr({'try',L,Es,Cs,Ecs,As}, St0) ->
    %% 'try ... [of ...] [catch ...] after ... end'
    expr({'try',L,[{'try',L,Es,Cs,Ecs,[]}],[],[],As}, St0);
expr({'catch',L,E0}, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    Lanno = lineno_anno(L, St1),
    {#icatch{anno=#a{anno=Lanno},body=Eps ++ [E1]},[],St1};
expr({'fun',L,{function,F,A},{_,_,_}=Id}, St) ->
    Lanno = full_anno(L, St),
    {#c_var{anno=Lanno++[{id,Id}],name={F,A}},[],St};
expr({'fun',L,{function,M,F,A}}, St0) ->
    {As,Aps,St1} = safe_list([M,F,A], St0),
    Lanno = full_anno(L, St1),
    {#icall{anno=#a{anno=Lanno},
	    module=#c_literal{val=erlang},
	    name=#c_literal{val=make_fun},
	    args=As},Aps,St1};
expr({'fun',L,{clauses,Cs},Id}, St) ->
    fun_tq(Id, Cs, L, St, unnamed);
expr({named_fun,L,'_',Cs,Id}, St) ->
    fun_tq(Id, Cs, L, St, unnamed);
expr({named_fun,L,Name,Cs,Id}, St) ->
    fun_tq(Id, Cs, L, St, {named,Name});
expr({call,L,{remote,_,M,F},As0}, St0) ->
    {[M1,F1|As1],Aps,St1} = safe_list([M,F|As0], St0),
    Anno = full_anno(L, St1),
    {#icall{anno=#a{anno=Anno},module=M1,name=F1,args=As1},Aps,St1};
expr({call,Lc,{atom,Lf,F},As0}, St0) ->
    {As1,Aps,St1} = safe_list(As0, St0),
    Op = #c_var{anno=lineno_anno(Lf, St1),name={F,length(As1)}},
    {#iapply{anno=#a{anno=lineno_anno(Lc, St1)},op=Op,args=As1},Aps,St1};
expr({call,L,FunExp,As0}, St0) ->
    {Fun,Fps,St1} = safe_fun(length(As0), FunExp, St0),
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
    {E2,Eps1,St2} = novars(E1, St1),
    St3 = St2#core{wanted=St0#core.wanted},
    {P2,Eps2,St4} = try
	    pattern(P1, St3)
	 catch
	     throw:Thrown ->
		{Thrown,[],St3}
	 end,
    {Fpat,St5} = new_var(St4),
    Lanno = lineno_anno(L, St5),
    Fc = fail_clause([Fpat], Lanno, c_tuple([#c_literal{val=badmatch},Fpat])),
    case P2 of
	nomatch ->
	    %% The pattern will not match. We must take care here to
	    %% bind all variables that the pattern would have bound
	    %% so that subsequent expressions do not refer to unbound
	    %% variables.
	    %%
	    %% As an example, this code:
	    %%
	    %%   [X] = {Y} = E,
	    %%   X + Y.
	    %%
	    %% will be rewritten to:
	    %%
	    %%   error({badmatch,E}),
	    %%   case E of
	    %%      {[X],{Y}} ->
	    %%        X + Y;
	    %%      Other ->
	    %%        error({badmatch,Other})
	    %%   end.
	    %%
	    St6 = add_warning(L, nomatch, St5),
	    {Expr,Eps3,St7} = safe(E1, St6),
	    SanPat0 = sanitize(P1),
	    {SanPat,Eps4,St} = pattern(SanPat0, St7),
	    Badmatch = c_tuple([#c_literal{val=badmatch},Expr]),
	    Fail = #iprimop{anno=#a{anno=Lanno},
			    name=#c_literal{val=match_fail},
			    args=[Badmatch]},
	    Eps = Eps3 ++ Eps4 ++ [Fail],
	    {#imatch{anno=#a{anno=Lanno},pat=SanPat,arg=Expr,fc=Fc},Eps,St};
	Other when not is_atom(Other) ->
	    {#imatch{anno=#a{anno=Lanno},pat=P2,arg=E2,fc=Fc},Eps1++Eps2,St5}
    end;
expr({op,_,'++',{lc,Llc,E,Qs0},More}, St0) ->
    %% Optimise '++' here because of the list comprehension algorithm.
    %%
    %% To avoid achieving quadratic complexity if there is a chain of
    %% list comprehensions without generators combined with '++', force
    %% evaluation of More now. Evaluating More here could also reduce the
    %% number variables in the environment for letrec.
    {Mc,Mps,St1} = safe(More, St0),
    {Qs,St2} = preprocess_quals(Llc, Qs0, St1),
    {Y,Yps,St} = lc_tq(Llc, E, Qs, Mc, St2),
    {Y,Mps++Yps,St};
expr({op,L,'andalso',E1,E2}, St0) ->
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch(L, E1, V, E2, False, St0),
    expr(E, St);
expr({op,L,'orelse',E1,E2}, St0) ->
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch(L, E1, V, True, E2, St0),
    expr(E, St);
expr({op,L,Op,A0}, St0) ->
    {A1,Aps,St1} = safe(A0, St0),
    LineAnno = full_anno(L, St1),
    {#icall{anno=#a{anno=LineAnno},		%Must have an #a{}
	    module=#c_literal{anno=LineAnno,val=erlang},
	    name=#c_literal{anno=LineAnno,val=Op},args=[A1]},Aps,St1};
expr({op,L,Op,L0,R0}, St0) ->
    {As,Aps,St1} = safe_list([L0,R0], St0),
    LineAnno = full_anno(L, St1),
    {#icall{anno=#a{anno=LineAnno},		%Must have an #a{}
	    module=#c_literal{anno=LineAnno,val=erlang},
	    name=#c_literal{anno=LineAnno,val=Op},args=As},Aps,St1}.


%% sanitize(Pat) -> SanitizedPattern
%%  Rewrite Pat so that it will be accepted by pattern/2 and will
%%  bind the same variables as the original pattern.
%%
%%  Here is an example of a pattern that would cause a pattern/2
%%  to generate a 'nomatch' exception:
%%
%%      #{k:=X,k:=Y} = [Z]
%%
%%  The sanitized pattern will look like:
%%
%%      {{X,Y},[Z]}

sanitize({match,L,P1,P2}) ->
    {tuple,L,[sanitize(P1),sanitize(P2)]};
sanitize({cons,L,H,T}) ->
    {cons,L,sanitize(H),sanitize(T)};
sanitize({tuple,L,Ps0}) ->
    Ps = [sanitize(P) || P <- Ps0],
    {tuple,L,Ps};
sanitize({map,L,Ps0}) ->
    Ps = [sanitize(V) || {map_field_exact,_,_,V} <- Ps0],
    {tuple,L,Ps};
sanitize(P) -> P.

make_bool_switch(L, E, V, T, F, #core{in_guard=true}) ->
    make_bool_switch_guard(L, E, V, T, F);
make_bool_switch(L, E, V, T, F, #core{}) ->
    make_bool_switch_body(L, E, V, T, F).

make_bool_switch_body(L, E, V, T, F) ->
    NegL = no_compiler_warning(L),
    Error = {tuple,NegL,[{atom,NegL,badarg},V]},
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],[T]},
      {clause,NegL,[{atom,NegL,false}],[],[F]},
      {clause,NegL,[V],[],
       [{call,NegL,{remote,NegL,{atom,NegL,erlang},{atom,NegL,error}},
	 [Error]}]}]}.

make_bool_switch_guard(_, E, _, {atom,_,true}, {atom,_,false}) -> E;
make_bool_switch_guard(L, E, V, T, F) ->
    NegL = no_compiler_warning(L),
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],[T]},
      {clause,NegL,[{atom,NegL,false}],[],[F]},
      {clause,NegL,[V],[],[V]}
     ]}.

expr_map(M0, Es0, L, St0) ->
    {M1,Eps0,St1} = safe(M0, St0),
    Badmap = badmap_term(M1, St1),
    A = lineno_anno(L, St1),
    Fc = fail_clause([], [{eval_failure,badmap}|A], Badmap),
    case is_valid_map_src(M1) of
	true ->
	    {M2,Eps1,St2} = map_build_pairs(M1, Es0, full_anno(L, St1), St1),
	    M3 = case Es0 of
		     [] -> M1;
		     [_|_] -> M2
		 end,
	    Cs = [#iclause{
		     anno=#a{anno=[compiler_generated|A]},
		     pats=[],
		     guard=[#icall{anno=#a{anno=A},
				   module=#c_literal{anno=A,val=erlang},
			           name=#c_literal{anno=A,val=is_map},
				   args=[M1]}],
		     body=[M3]}],
	    Eps = Eps0 ++ Eps1,
	    {#icase{anno=#a{anno=A},args=[],clauses=Cs,fc=Fc},Eps,St2};
	false ->
	    %% Not a map source. The update will always fail.
	    St2 = add_warning(L, badmap, St1),
	    #iclause{body=[Fail]} = Fc,
	    {Fail,Eps0,St2}
    end.

badmap_term(_Map, #core{in_guard=true}) ->
    %% The code generator cannot handle complex error reasons
    %% in guards. But the exact error reason does not matter anyway
    %% since it is not user-visible.
    #c_literal{val=badmap};
badmap_term(Map, #core{in_guard=false}) ->
    c_tuple([#c_literal{val=badmap},Map]).

map_build_pairs(Map, Es0, Ann, St0) ->
    {Es,Pre,St1} = map_build_pairs_1(Es0, St0),
    {ann_c_map(Ann, Map, Es),Pre,St1}.

map_build_pairs_1([{Op0,L,K0,V0}|Es], St0) ->
    {K,Pre0,St1} = safe(K0, St0),
    {V,Pre1,St2} = safe(V0, St1),
    {Pairs,Pre2,St3} = map_build_pairs_1(Es, St2),
    As = lineno_anno(L, St3),
    Op = map_op(Op0),
    Pair = cerl:ann_c_map_pair(As, Op, K, V),
    {[Pair|Pairs],Pre0++Pre1++Pre2,St3};
map_build_pairs_1([], St) ->
    {[],[],St}.

map_op(map_field_assoc) -> #c_literal{val=assoc};
map_op(map_field_exact) -> #c_literal{val=exact}.

is_valid_map_src(#c_literal{val = M}) when is_map(M) -> true;
is_valid_map_src(#c_var{}=Var)  -> not cerl:is_c_fname(Var);
is_valid_map_src(_)         -> false.

%% try_exception([ExcpClause], St) -> {[ExcpVar],Handler,St}.

try_exception(Ecs0, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Evs,St1} = new_vars(3, St0), % Tag, Value, Info
    {Ecs1,Ceps,St2} = clauses(Ecs0, St1),
    [_,Value,Info] = Evs,
    LA = case Ecs1 of
	     [] -> [];
	     [C|_] -> get_lineno_anno(C)
	 end,
    Ec = #iclause{anno=#a{anno=[compiler_generated|LA]},
		  pats=[c_tuple(Evs)],guard=[#c_literal{val=true}],
		  body=[#iprimop{anno=#a{},       %Must have an #a{}
				 name=#c_literal{val=raise},
				 args=[Info,Value]}]},
    Hs = [#icase{anno=#a{anno=LA},args=[c_tuple(Evs)],clauses=Ecs1,fc=Ec}],
    {Evs,Ceps++Hs,St2}.

try_after(As, St0) ->
    %% See above.
    {Evs,St1} = new_vars(3, St0),	 % Tag, Value, Info
    [_,Value,Info] = Evs,
    B = As ++ [#iprimop{anno=#a{},       % Must have an #a{}
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
    try eval_bits:expr_grp(Es, EmptyBindings, EvalFun) of
	{value,Bin,EmptyBindings} ->
	    Bin
    catch error:_ ->
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
    count_bits_1(abs(Int), 64).

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
    case Size1 of
	#c_var{} -> ok;
	#c_literal{val=Sz} when is_integer(Sz), Sz >= 0 -> ok;
	#c_literal{val=undefined} -> ok;
	#c_literal{val=all} -> ok;
	_ -> throw(bad_binary)
    end,
    {#c_bitstr{val=E1,size=Size1,
	       unit=#c_literal{val=Unit},
	       type=#c_literal{val=Type},
	       flags=#c_literal{val=Flags}},
     Eps ++ Eps2,St2}.

%% fun_tq(Id, [Clauses], Line, State, NameInfo) -> {Fun,[PreExp],State}.

fun_tq({_,_,Name}=Id, Cs0, L, St0, NameInfo) ->
    Arity = clause_arity(hd(Cs0)),
    {Cs1,Ceps,St1} = clauses(Cs0, St0),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Anno = full_anno(L, St3),
    Fc = function_clause(Ps, Anno, {Name,Arity}),
    Fun = #ifun{anno=#a{anno=Anno},
		id=[{id,Id}],				%We KNOW!
		vars=Args,clauses=Cs1,fc=Fc,name=NameInfo},
    {Fun,Ceps,St3}.

%% lc_tq(Line, Exp, [Qualifier], Mc, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Simon PJ pp 127-138.  

lc_tq(Line, E, [#igen{anno=GAnno,ceps=Ceps,
		      acc_pat=AccPat,acc_guard=AccGuard,
                      skip_pat=SkipPat,tail=Tail,tail_pat=TailPat,
                      arg={Pre,Arg}}|Qs], Mc, St0) ->
    {Name,St1} = new_fun_name("lc", St0),
    LA = lineno_anno(Line, St1),
    LAnno = #a{anno=LA},
    F = #c_var{anno=LA,name={Name,1}},
    Nc = #iapply{anno=GAnno,op=F,args=[Tail]},
    {Var,St2} = new_var(St1),
    Fc = function_clause([Var], LA, {Name,1}),
    TailClause = #iclause{anno=LAnno,pats=[TailPat],guard=[],body=[Mc]},
    Cs0 = case {AccPat,AccGuard} of
              {SkipPat,[]} ->
                  %% Skip and accumulator patterns are the same and there is
                  %% no guard, no need to generate a skip clause.
                  [TailClause];
              _ ->
                  [#iclause{anno=#a{anno=[compiler_generated|LA]},
                            pats=[SkipPat],guard=[],body=[Nc]},
                   TailClause]
          end,
    {Cs,St4} = case AccPat of
                   nomatch ->
                       %% The accumulator pattern never matches, no need
                       %% for an accumulator clause.
                       {Cs0,St2};
                   _ ->
                       {Lc,Lps,St3} = lc_tq(Line, E, Qs, Nc, St2),
                       {[#iclause{anno=LAnno,pats=[AccPat],guard=AccGuard,
                                  body=Lps ++ [Lc]}|Cs0],
                        St3}
               end,
    Fun = #ifun{anno=LAnno,id=[],vars=[Var],clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno#a{anno=[list_comprehension|LA]},defs=[{{Name,1},Fun}],
              body=Pre ++ [#iapply{anno=LAnno,op=F,args=[Arg]}]},
     Ceps,St4};
lc_tq(Line, E, [#ifilter{}=Filter|Qs], Mc, St) ->
    filter_tq(Line, E, Filter, Mc, St, Qs, fun lc_tq/5);
lc_tq(Line, E0, [], Mc0, St0) ->
    {H1,Hps,St1} = safe(E0, St0),
    {T1,Tps,St} = force_safe(Mc0, St1),
    Anno = lineno_anno(Line, St),
    E = ann_c_cons(Anno, H1, T1),
    {set_anno(E, [compiler_generated|Anno]),Hps ++ Tps,St}.

%% bc_tq(Line, Exp, [Qualifier], More, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Gustafsson ERLANG'05.  
%%  More could be transformed before calling bc_tq.

bc_tq(Line, Exp, Qs0, St0) ->
    {BinVar,St1} = new_var(St0),
    {Sz,SzPre,St2} = bc_initial_size(Exp, Qs0, St1),
    {Qs,St3} = preprocess_quals(Line, Qs0, St2),
    {E,BcPre,St} = bc_tq1(Line, Exp, Qs, BinVar, St3),
    Pre = SzPre ++
	[#iset{var=BinVar,
	       arg=#iprimop{name=#c_literal{val=bs_init_writable},
			    args=[Sz]}}] ++ BcPre,
    {E,Pre,St}.

bc_tq1(Line, E, [#igen{anno=GAnno,ceps=Ceps,
		       acc_pat=AccPat,acc_guard=AccGuard,
                       skip_pat=SkipPat,tail=Tail,tail_pat=TailPat,
                       arg={Pre,Arg}}|Qs], Mc, St0) ->
    {Name,St1} = new_fun_name("lbc", St0),
    LA = lineno_anno(Line, St1),
    LAnno = #a{anno=LA},
    {Vars=[_,AccVar],St2} = new_vars(LA, 2, St1),
    F = #c_var{anno=LA,name={Name,2}},
    Nc = #iapply{anno=GAnno,op=F,args=[Tail,AccVar]},
    Fc = function_clause(Vars, LA, {Name,2}),
    TailClause = #iclause{anno=LAnno,pats=[TailPat,AccVar],guard=[],
                          body=[AccVar]},
    Cs0 = case {AccPat,AccGuard} of
              {SkipPat,[]} ->
                  %% Skip and accumulator patterns are the same and there is
                  %% no guard, no need to generate a skip clause.
                  [TailClause];
              _ ->
                  [#iclause{anno=#a{anno=[compiler_generated|LA]},
                            pats=[SkipPat,AccVar],guard=[],body=[Nc]},
                   TailClause]
          end,
    {Cs,St4} = case AccPat of
                   nomatch ->
                       %% The accumulator pattern never matches, no need
                       %% for an accumulator clause.
                       {Cs0,St2};
                   _ ->
                       {Bc,Bps,St3} = bc_tq1(Line, E, Qs, AccVar, St2),
                       Body = Bps ++ [#iset{var=AccVar,arg=Bc},Nc],
                       {[#iclause{anno=LAnno,
                                  pats=[AccPat,AccVar],guard=AccGuard,
                                  body=Body}|Cs0],
                        St3}
               end,
    Fun = #ifun{anno=LAnno,id=[],vars=Vars,clauses=Cs,fc=Fc},
    {#iletrec{anno=LAnno#a{anno=[list_comprehension|LA]},defs=[{{Name,2},Fun}],
              body=Pre ++ [#iapply{anno=LAnno,op=F,args=[Arg,Mc]}]},
     Ceps,St4};
bc_tq1(Line, E, [#ifilter{}=Filter|Qs], Mc, St) ->
    filter_tq(Line, E, Filter, Mc, St, Qs, fun bc_tq1/5);
bc_tq1(_, {bin,Bl,Elements}, [], AccVar, St0) ->
    bc_tq_build(Bl, [], AccVar, Elements, St0);
bc_tq1(Line, E0, [], AccVar, St0) ->
    BsFlags = [binary,{unit,1}],
    BsSize = {atom,Line,all},
    {E1,Pre0,St1} = safe(E0, St0),
    case E1 of
	#c_var{name=VarName} ->
	    Var = {var,Line,VarName},
	    Els = [{bin_element,Line,Var,BsSize,BsFlags}],
	    bc_tq_build(Line, Pre0, AccVar, Els, St1);
	#c_literal{val=Val} when is_bitstring(Val) ->
	    Bits = bit_size(Val),
	    <<Int0:Bits>> = Val,
	    Int = {integer,Line,Int0},
	    Sz = {integer,Line,Bits},
	    Els = [{bin_element,Line,Int,Sz,[integer,{unit,1},big]}],
	    bc_tq_build(Line, Pre0, AccVar, Els, St1);
	_ ->
	    %% Any other safe (cons, tuple, literal) is not a
	    %% bitstring. Force the evaluation to fail (and
	    %% generate a warning).
	    Els = [{bin_element,Line,{atom,Line,bad_value},BsSize,BsFlags}],
	    bc_tq_build(Line, Pre0, AccVar, Els, St1)
    end.

bc_tq_build(Line, Pre0, #c_var{name=AccVar}, Elements0, St0) ->
    Elements = [{bin_element,Line,{var,Line,AccVar},{atom,Line,all},
		 [binary,{unit,1}]}|Elements0],
    {E,Pre,St} = expr({bin,Line,Elements}, St0),
    #a{anno=A} = Anno0 = get_anno(E),
    Anno = Anno0#a{anno=[compiler_generated,single_use|A]},
    {set_anno(E, Anno),Pre0++Pre,St}.


%% filter_tq(Line, Expr, Filter, Mc, State, [Qualifier], TqFun) ->
%%     {Case,[PreExpr],State}.
%%  Transform an intermediate comprehension filter to its intermediate case
%%  representation.

filter_tq(Line, E, #ifilter{anno=#a{anno=LA}=LAnno,arg={Pre,Arg}},
          Mc, St0, Qs, TqFun) ->
    %% The filter is an expression, it is compiled to a case of degree 1 with
    %% 3 clauses, one accumulating, one skipping and the final one throwing
    %% {case_clause,Value} where Value is the result of the filter and is not a
    %% boolean.
    {Lc,Lps,St1} = TqFun(Line, E, Qs, Mc, St0),
    {FailPat,St2} = new_var(St1),
    Fc = fail_clause([FailPat], LA,
                     c_tuple([#c_literal{val=case_clause},FailPat])),
    {#icase{anno=LAnno#a{anno=[list_comprehension|LA]},args=[Arg],
            clauses=[#iclause{anno=LAnno,
                              pats=[#c_literal{val=true}],guard=[],
                              body=Lps ++ [Lc]},
                     #iclause{anno=LAnno#a{anno=[compiler_generated|LA]},
                              pats=[#c_literal{val=false}],guard=[],
                              body=[Mc]}],
            fc=Fc},
     Pre,St2};
filter_tq(Line, E, #ifilter{anno=#a{anno=LA}=LAnno,arg=Guard},
          Mc, St0, Qs, TqFun) when is_list(Guard) ->
    %% Otherwise it is a guard, compiled to a case of degree 0 with 2 clauses,
    %% the first matches if the guard succeeds and the comprehension continues
    %% or the second one is selected and the current element is skipped.
    {Lc,Lps,St1} = TqFun(Line, E, Qs, Mc, St0),
    {#icase{anno=LAnno#a{anno=[list_comprehension|LA]},args=[],
            clauses=[#iclause{anno=LAnno,pats=[],guard=Guard,body=Lps ++ [Lc]}],
            fc=#iclause{anno=LAnno#a{anno=[compiler_generated|LA]},
                        pats=[],guard=[],body=[Mc]}},
     [],St1}.

%% preprocess_quals(Line, [Qualifier], State) -> {[Qualifier'],State}.
%%  Preprocess a list of Erlang qualifiers into its intermediate representation,
%%  represented as a list of #igen{} and #ifilter{} records. We recognise guard
%%  tests and try to fold them together and join to a preceding generators, this
%%  should give us better and more compact code.

preprocess_quals(Line, Qs, St) ->
    preprocess_quals(Line, Qs, St, []).

preprocess_quals(Line, [Q|Qs0], St0, Acc) ->
    case is_generator(Q) of
        true ->
            {Gs,Qs} = splitwith(fun is_guard_test/1, Qs0),
            {Gen,St} = generator(Line, Q, Gs, St0),
            preprocess_quals(Line, Qs, St, [Gen|Acc]);
        false ->
            LAnno = #a{anno=lineno_anno(get_qual_anno(Q), St0)},
            case is_guard_test(Q) of
                true ->
                    %% When a filter is a guard test, its argument in the
                    %% #ifilter{} record is a list as returned by
                    %% lc_guard_tests/2.
                    {Gs,Qs} = splitwith(fun is_guard_test/1, Qs0),
                    {Cg,St} = lc_guard_tests([Q|Gs], St0),
                    Filter = #ifilter{anno=LAnno,arg=Cg},
                    preprocess_quals(Line, Qs, St, [Filter|Acc]);
                false ->
                    %% Otherwise, it is a pair {Pre,Arg} as in a generator
                    %% input.
                    {Ce,Pre,St} = novars(Q, St0),
                    Filter = #ifilter{anno=LAnno,arg={Pre,Ce}},
                    preprocess_quals(Line, Qs0, St, [Filter|Acc])
            end
    end;
preprocess_quals(_, [], St, Acc) ->
    {reverse(Acc),St}.

is_generator({generate,_,_,_}) -> true;
is_generator({b_generate,_,_,_}) -> true;
is_generator(_) -> false.

%% Retrieve the annotation from an Erlang AST form.
%% (Use get_anno/1 to retrieve the annotation from Core Erlang forms).

get_qual_anno(Abstract) -> element(2, Abstract).

%%
%% Generators are abstracted as sextuplets:
%%  - acc_pat is the accumulator pattern, e.g. [Pat|Tail] for Pat <- Expr.
%%  - acc_guard is the list of guards immediately following the current
%%    generator in the qualifier list input.
%%  - skip_pat is the skip pattern, e.g. <<X,_:X,Tail/bitstring>> for
%%    <<X,1:X>> <= Expr.
%%  - tail is the variable used in AccPat and SkipPat bound to the rest of the
%%    generator input.
%%  - tail_pat is the tail pattern, respectively [] and <<_/bitstring>> for list
%%    and bit string generators.
%%  - arg is a pair {Pre,Arg} where Pre is the list of expressions to be
%%    inserted before the comprehension function and Arg is the expression
%%    that it should be passed.
%%

%% generator(Line, Generator, Guard, State) -> {Generator',State}.
%%  Transform a given generator into its #igen{} representation.

generator(Line, {generate,Lg,P0,E}, Gs, St0) ->
    LA = lineno_anno(Line, St0),
    GA = lineno_anno(Lg, St0),
    {Head,Ceps,St1} = list_gen_pattern(P0, Line, St0),
    {[Tail,Skip],St2} = new_vars(2, St1),
    {Cg,St3} = lc_guard_tests(Gs, St2),
    {AccPat,SkipPat} = case Head of
                           #c_var{} ->
                               %% If the generator pattern is a variable, the
                               %% pattern from the accumulator clause can be
                               %% reused in the skip one. lc_tq and bc_tq1 takes
                               %% care of dismissing the latter in that case.
                               Cons = ann_c_cons(LA, Head, Tail),
                               {Cons,Cons};
                           nomatch ->
                               %% If it never matches, there is no need for
                               %% an accumulator clause.
                               {nomatch,ann_c_cons(LA, Skip, Tail)};
                           _ ->
                               {ann_c_cons(LA, Head, Tail),
                                ann_c_cons(LA, Skip, Tail)}
                       end,
    {Ce,Pre,St4} = safe(E, St3),
    Gen = #igen{anno=#a{anno=GA},ceps=Ceps,
		acc_pat=AccPat,acc_guard=Cg,skip_pat=SkipPat,
                tail=Tail,tail_pat=#c_literal{anno=LA,val=[]},arg={Pre,Ce}},
    {Gen,St4};
generator(Line, {b_generate,Lg,P,E}, Gs, St0) ->
    LA = lineno_anno(Line, St0),
    GA = lineno_anno(Lg, St0),
    {Cp = #c_binary{segments=Segs},[],St1} = pattern(P, St0),
    
    %% The function append_tail_segment/2 keeps variable patterns as-is, making
    %% it possible to have the same skip clause removal as with list generators.
    {AccSegs,Tail,TailSeg,St2} = append_tail_segment(Segs, St1),
    AccPat = Cp#c_binary{segments=AccSegs},
    {Cg,St3} = lc_guard_tests(Gs, St2),
    {SkipSegs,St4} = emasculate_segments(AccSegs, St3),
    SkipPat = Cp#c_binary{segments=SkipSegs},
    {Ce,Pre,St5} = safe(E, St4),
    Gen = #igen{anno=#a{anno=GA},acc_pat=AccPat,acc_guard=Cg,skip_pat=SkipPat,
                tail=Tail,tail_pat=#c_binary{anno=LA,segments=[TailSeg]},
                arg={Pre,Ce}},
    {Gen,St5}.

append_tail_segment(Segs, St0) ->
    {Var,St} = new_var(St0),
    Tail = #c_bitstr{val=Var,size=#c_literal{val=all},
		     unit=#c_literal{val=1},
		     type=#c_literal{val=binary},
		     flags=#c_literal{val=[unsigned,big]}},
    {Segs++[Tail],Var,Tail,St}.

emasculate_segments(Segs, St) ->
    emasculate_segments(Segs, St, []).

emasculate_segments([#c_bitstr{val=#c_var{}}=B|Rest], St, Acc) ->
    emasculate_segments(Rest, St, [B|Acc]);
emasculate_segments([B|Rest], St0, Acc) ->
    {Var,St1} = new_var(St0),
    emasculate_segments(Rest, St1, [B#c_bitstr{val=Var}|Acc]);
emasculate_segments([], St, Acc) ->
    {reverse(Acc),St}.

lc_guard_tests([], St) -> {[],St};
lc_guard_tests(Gs0, St0) ->
    Gs1 = guard_tests(Gs0),
    {Gs,St} = gexpr_top(Gs1, St0#core{in_guard=true}),
    {Gs,St#core{in_guard=false}}.

list_gen_pattern(P0, Line, St) ->
    try
	pattern(P0,St)
    catch 
	nomatch -> {nomatch,[],add_warning(Line, nomatch, St)}
    end.

%%%
%%% Generate code to calculate the initial size for
%%% the result binary in a binary comprehension.
%%%

bc_initial_size(E, Q, St0) ->
    try
	{ElemSzExpr,ElemSzPre,EVs,St1} = bc_elem_size(E, St0),
	{V,St2} = new_var(St1),
	{GenSzExpr,GenSzPre,St3} = bc_gen_size(Q, EVs, St2),
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
	    {#c_literal{val=Bits},[],[],St0};
	{Bits,Vars0} ->
	    [{U,V0}|Pairs]  = sort(Vars0),
	    F = bc_elem_size_combine(Pairs, U, [V0], []),
	    Vs = [V || {_,#c_var{name=V}} <- Vars0],
	    {E,Pre,St} = bc_mul_pairs(F, #c_literal{val=Bits}, [], St0),
	    {E,Pre,Vs,St}
    end;
bc_elem_size(_, _) ->
    throw(impossible).

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

bc_gen_size(Q, EVs, St) ->
    bc_gen_size_1(Q, EVs, #c_literal{val=1}, [], St).

bc_gen_size_1([{generate,L,El,Gen}|Qs], EVs, E0, Pre0, St0) ->
    bc_verify_non_filtering(El, EVs),
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
	    bc_gen_size_1(Qs, EVs, E, Pre, St);
	_ ->
	    %% The only expressions we handle is literal lists.
	    Len = bc_list_length(Gen, 0),
	    {E,Pre,St} = bc_gen_size_mul(E0, #c_literal{val=Len}, Pre0, St0),
	    bc_gen_size_1(Qs, EVs, E, Pre, St)
    end;
bc_gen_size_1([{b_generate,_,El,Gen}|Qs], EVs, E0, Pre0, St0) ->
    bc_verify_non_filtering(El, EVs),
    {MatchSzExpr,Pre1,_,St1} = bc_elem_size(El, St0),
    Pre2 = reverse(Pre1, Pre0),
    {ResVar,St2} = new_var(St1),
    {BitSizeExpr,Pre3,St3} = bc_gen_bit_size(Gen, Pre2, St2),
    Div = #iset{var=ResVar,arg=bc_div(BitSizeExpr,
				      MatchSzExpr)},
    Pre4 = [Div|Pre3],
    {E,Pre,St} = bc_gen_size_mul(E0, ResVar, Pre4, St3),
    bc_gen_size_1(Qs, EVs, E, Pre, St);
bc_gen_size_1([], _, E, Pre, St) ->
    {E,reverse(Pre),St};
bc_gen_size_1(_, _, _, _, _) ->
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

bc_verify_non_filtering({bin,_,Els}, EVs) ->
    foreach(fun({bin_element,_,{var,_,V},_,_}) ->
		   case member(V, EVs) of
		       true -> throw(impossible);
		       false -> ok
		   end;
	       (_) -> throw(impossible)
	    end, Els);
bc_verify_non_filtering({var,_,V}, EVs) ->
    case member(V, EVs) of
	true -> throw(impossible);
	false -> ok
    end;
bc_verify_non_filtering(_, _) ->
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
force_novars(#c_map{}=Bin, St) -> {Bin,[],St};
force_novars(Ce, St) ->
    force_safe(Ce, St).


%% safe_pattern_expr(Expr, State) -> {Cexpr,[PreExpr],State}.
%%   only literals and variables are safe expressions in patterns
safe_pattern_expr(E,St0) ->
    case safe(E,St0) of
	{#c_var{},_,_}=Safe -> Safe;
	{#c_literal{},_,_}=Safe -> Safe;
	{Ce,Eps,St1} ->
	    {V,St2} = new_var(St1),
	    {V,Eps++[#iset{var=V,arg=Ce}],St2}
    end.

%% safe(Expr, State) -> {Safe,[PreExpr],State}.
%%  Generate an internal safe expression.  These are simples without
%%  binaries which can fail.  At this level we do not need to do a
%%  deep check.  Must do special things with matches here.

safe(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_safe(E1, St1),
    {Se,Eps ++ Sps,St2}.

safe_fun(A0, E0, St0) ->
    case safe(E0, St0) of
        {#c_var{name={_,A1}}=E1,Eps,St1} when A1 =/= A0 ->
            {V,St2} = new_var(St1),
            {V,Eps ++ [#iset{var=V,arg=E1}],St2};
        Result ->
            Result
    end.

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

%% pattern(Pattern, State) -> {CorePat,[PreExp],State}.
%% Transform a pattern by removing line numbers.  We also normalise
%% aliases in patterns to standard form, {alias,Pat,[Var]}.
%%
%% In patterns we may have expressions
%% 1) Binaries -> #c_bitstr{size=Expr}
%% 2) Maps -> #c_map_pair{key=Expr}
%%
%% Both of these may generate pre-expressions since only bound variables
%% or literals are allowed for these in core patterns.
%%
%% Therefor, we need to drag both the state and the collection of pre-expression
%% around in the whole pattern transformation tree.

pattern({var,L,V}, St) -> {#c_var{anno=lineno_anno(L, St),name=V},[],St};
pattern({char,L,C}, St) -> {#c_literal{anno=lineno_anno(L, St),val=C},[],St};
pattern({integer,L,I}, St) -> {#c_literal{anno=lineno_anno(L, St),val=I},[],St};
pattern({float,L,F}, St) -> {#c_literal{anno=lineno_anno(L, St),val=F},[],St};
pattern({atom,L,A}, St) -> {#c_literal{anno=lineno_anno(L, St),val=A},[],St};
pattern({string,L,S}, St) -> {#c_literal{anno=lineno_anno(L, St),val=S},[],St};
pattern({nil,L}, St) -> {#c_literal{anno=lineno_anno(L, St),val=[]},[],St};
pattern({cons,L,H,T}, St) ->
    {Ph,Eps1,St1} = pattern(H, St),
    {Pt,Eps2,St2} = pattern(T, St1),
    {annotate_cons(lineno_anno(L, St), Ph, Pt, St2),Eps1++Eps2,St2};
pattern({tuple,L,Ps}, St) ->
    {Ps1,Eps,St1} = pattern_list(Ps,St),
    {annotate_tuple(record_anno(L, St), Ps1, St),Eps,St1};
pattern({map,L,Pairs}, St0) ->
    {Ps,Eps,St1} = pattern_map_pairs(Pairs, St0),
    {#c_map{anno=lineno_anno(L, St1),es=Ps,is_pat=true},Eps,St1};
pattern({bin,L,Ps}, St) ->
    %% We don't create a #ibinary record here, since there is
    %% no need to hold any used/new annotations in a pattern.
    {#c_binary{anno=lineno_anno(L, St),segments=pat_bin(Ps, St)},[],St};
pattern({match,_,P1,P2}, St) ->
    {Cp1,Eps1,St1} = pattern(P1,St),
    {Cp2,Eps2,St2} = pattern(P2,St1),
    {pat_alias(Cp1,Cp2),Eps1++Eps2,St2}.

%% pattern_map_pairs([MapFieldExact],State) -> [#c_map_pairs{}]
pattern_map_pairs(Ps, St) ->
    %% check literal key uniqueness
    %%   - guaranteed via aliasing map pairs
    %% pattern all pairs in two steps
    %% 1) Construct Core Pattern
    %% 2) Alias Keys in Core Pattern
    {CMapPairs, {Eps,St1}} = lists:mapfoldl(fun
	    (P,{EpsM,Sti0}) ->
		{CMapPair,EpsP,Sti1} = pattern_map_pair(P,Sti0),
		{CMapPair, {EpsM++EpsP,Sti1}}
	end, {[],St}, Ps),
    {pat_alias_map_pairs(CMapPairs),Eps,St1}.

pattern_map_pair({map_field_exact,L,K,V}, St0) ->
    {Ck,EpsK,St1} = safe_pattern_expr(K, St0),
    {Cv,EpsV,St2} = pattern(V, St1),
    {#c_map_pair{anno=lineno_anno(L, St2),
		 op=#c_literal{val=exact},
		 key=Ck,
		 val=Cv},EpsK++EpsV,St2}.

pat_alias_map_pairs(Ps) ->
    D = foldl(fun(#c_map_pair{key=K0}=Pair, D0) ->
		      K = cerl:set_ann(K0, []),
		      dict:append(K, Pair, D0)
	      end, dict:new(), Ps),
    pat_alias_map_pairs_1(dict:to_list(D)).

pat_alias_map_pairs_1([{_,[#c_map_pair{val=V0}=Pair|Vs]}|T]) ->
    V = foldl(fun(#c_map_pair{val=V}, Pat) ->
		      pat_alias(V, Pat)
	      end, V0, Vs),
    [Pair#c_map_pair{val=V}|pat_alias_map_pairs_1(T)];
pat_alias_map_pairs_1([]) -> [].

%% pat_bin([BinElement], State) -> [BinSeg].

pat_bin(Ps, St) -> [pat_segment(P, St) || P <- Ps].

pat_segment({bin_element,L,Val,Size,[Type,{unit,Unit}|Flags]}, St) ->
    Anno = lineno_anno(L, St),
    {Pval,[],St1} = pattern(Val,St),
    {Psize,[],_St2} = pattern(Size,St1),
    #c_bitstr{anno=Anno,
	      val=Pval,size=Psize,
	      unit=#c_literal{val=Unit},
	      type=#c_literal{val=Type},
	      flags=#c_literal{val=Flags}}.

%% pat_alias(CorePat, CorePat) -> AliasPat.
%%  Normalise aliases.  Trap bad aliases by throwing 'nomatch'.

pat_alias(#c_var{name=V1}=P, #c_var{name=V1}) -> P;
pat_alias(#c_var{name=V1}=Var,
	  #c_alias{var=#c_var{name=V2},pat=Pat}=Alias) ->
    if
	V1 =:= V2 ->
	    Alias;
	true ->
	    Alias#c_alias{pat=pat_alias(Var, Pat)}
    end;
pat_alias(#c_var{}=P1, P2) -> #c_alias{var=P1,pat=P2};

pat_alias(#c_alias{var=#c_var{name=V1}}=Alias, #c_var{name=V1}) ->
    Alias;
pat_alias(#c_alias{var=#c_var{name=V1}=Var1,pat=P1},
	  #c_alias{var=#c_var{name=V2}=Var2,pat=P2}) ->
    Pat = pat_alias(P1, P2),
    if
	V1 =:= V2 ->
	    #c_alias{var=Var1,pat=Pat};
	true ->
	    pat_alias(Var1, pat_alias(Var2, Pat))
    end;
pat_alias(#c_alias{var=#c_var{}=Var,pat=P1}, P2) ->
    #c_alias{var=Var,pat=pat_alias(P1, P2)};

pat_alias(#c_map{es=Es1}=M, #c_map{es=Es2}) ->
    M#c_map{es=pat_alias_map_pairs(Es1 ++ Es2)};

pat_alias(P1, #c_var{}=Var) ->
    #c_alias{var=Var,pat=P1};
pat_alias(P1, #c_alias{pat=P2}=Alias) ->
    Alias#c_alias{pat=pat_alias(P1, P2)};

pat_alias(P1, P2) ->
    %% Aliases between binaries are not allowed, so the only
    %% legal patterns that remain are data patterns.
    case cerl:is_data(P1) andalso cerl:is_data(P2) of
	false -> throw(nomatch);
	true -> ok
    end,
    Type = cerl:data_type(P1),
    case cerl:data_type(P2) of
	Type -> ok;
	_ -> throw(nomatch)
    end,
    Es1 = cerl:data_es(P1),
    Es2 = cerl:data_es(P2),
    Es = pat_alias_list(Es1, Es2),
    cerl:make_data(Type, Es).

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [];
pat_alias_list(_, _) -> throw(nomatch).

%% pattern_list([P], State) -> {[P],Exprs,St}

pattern_list([P0|Ps0], St0) ->
    {P1,Eps,St1} = pattern(P0, St0),
    {Ps1,Epsl,St2} = pattern_list(Ps0, St1),
    {[P1|Ps1], Eps ++ Epsl, St2};
pattern_list([], St) ->
    {[],[],St}.


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

new_var(Anno, St0) when is_list(Anno) ->
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

function_clause(Ps, LineAnno, Name) ->
    FcAnno = [{function_name,Name}|LineAnno],
    fail_clause(Ps, FcAnno,
		ann_c_tuple(LineAnno, [#c_literal{val=function_clause}|Ps])).

fail_clause(Pats, Anno, Arg) ->
    #iclause{anno=#a{anno=[compiler_generated]},
	     pats=Pats,guard=[],
	     body=[#iprimop{anno=#a{anno=Anno},name=#c_literal{val=match_fail},
			    args=[Arg]}]}.

annotate_tuple(A, Es, St) ->
    case member(dialyzer, St#core.opts) of
        true ->
            %% Do not coalesce constant tuple elements. A Hack.
            Node = cerl:ann_c_tuple(A, [cerl:c_var(any)]),
            cerl:update_c_tuple_skel(Node, Es);
        false ->
            ann_c_tuple(A, Es)
    end.

annotate_cons(A, H, T, St) ->
    case member(dialyzer, St#core.opts) of
        true ->
            %% Do not coalesce constant conses. A Hack.
            Node= cerl:ann_c_cons(A, cerl:c_var(any), cerl:c_var(any)),
            cerl:update_c_cons_skel(Node, H, T);
        false ->
            ann_c_cons(A, H, T)
    end.

ubody(B, St) -> uexpr(B, [], St).

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(Cl0, Ks, St0) ->
    {Cl1,_Pvs,Used,New,St1} = uclause(Cl0, Ks, Ks, St0),
    A0 = get_anno(Cl1),
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
    uguard(droplast(Pg), [last(Pg)], Ks, St);
uguard(Pg, Gs0, Ks, St0) ->
    %% Gs0 must contain at least one element here.
    {Gs3,St5} = foldr(fun (T, {Gs1,St1}) ->
			      {L,St2} = new_var(St1),
			      {R,St3} = new_var(St2),
			      {[#iset{var=L,arg=T}] ++ droplast(Gs1) ++
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
    case upat_is_new_var(P0, Ks) of
	true ->
	    %% Assignment to a new variable.
	    uexprs([#iset{var=P0,arg=Arg}|Les], Ks, St0);
	false when Les =:= [] ->
	    %% Need to explicitly return match "value", make
	    %% safe for efficiency.
	    {La0,Lps,St1} = force_safe(Arg, St0),
	    La = mark_compiler_generated(La0),
	    Mc = #iclause{anno=A,pats=[P0],guard=[],body=[La]},
	    uexprs(Lps ++ [#icase{anno=A,
				  args=[La0],clauses=[Mc],fc=Fc}], Ks, St1);
	false ->
	    Mc = #iclause{anno=A,pats=[P0],guard=[],body=Les},
	    uexprs([#icase{anno=A,args=[Arg],
			   clauses=[Mc],fc=Fc}], Ks, St0)
    end;
uexprs([Le0|Les0], Ks, St0) ->
    {Le1,St1} = uexpr(Le0, Ks, St0),
    {Les1,St2} = uexprs(Les0, union((get_anno(Le1))#a.ns, Ks), St1),
    {[Le1|Les1],St2};
uexprs([], _, St) -> {[],St}.

%% upat_is_new_var(Pattern, [KnownVar]) -> true|false.
%%  Test whether the pattern is a single, previously unknown
%%  variable.

upat_is_new_var(#c_var{name=V}, Ks) ->
    not is_element(V, Ks);
upat_is_new_var(_, _) ->
    false.

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
uexpr(#icase{anno=#a{anno=Anno}=A,args=As0,clauses=Cs0,fc=Fc0}, Ks, St0) ->
    %% As0 will never generate new variables.
    {As1,St1} = uexpr_list(As0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Fc1,St3} = uclause(Fc0, Ks, St2),
    Used = union(used_in_any(As1), used_in_any(Cs1)),
    New = case member(list_comprehension, Anno) of
              true -> [];
              false -> new_in_all(Cs1)
          end,
    {#icase{anno=A#a{us=Used,ns=New},args=As1,clauses=Cs1,fc=Fc1},St3};
uexpr(#ifun{anno=A0,id=Id,vars=As,clauses=Cs0,fc=Fc0,name=Name}, Ks0, St0) ->
    Avs = lit_list_vars(As),
    Ks1 = case Name of
              unnamed -> Ks0;
              {named,FName} -> union(subtract([FName], Avs), Ks0)
          end,
    Ks2 = union(Avs, Ks1),
    {Cs1,St1} = ufun_clauses(Cs0, Ks2, St0),
    {Fc1,St2} = ufun_clause(Fc0, Ks2, St1),
    Used = subtract(intersection(used_in_any(Cs1), Ks1), Avs),
    A1 = A0#a{us=Used,ns=[]},
    {#ifun{anno=A1,id=Id,vars=As,clauses=Cs1,fc=Fc1,name=Name},St2};
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
uexpr(Simple, _, St) ->
    true = is_simple(Simple),			%Sanity check!
    Vs = lit_vars(Simple),
    Anno = get_anno(Simple),
    {#isimple{anno=#a{us=Vs,anno=Anno},term=Simple},St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% ufun_clauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

ufun_clauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> ufun_clause(Lc, Ks, St) end, St0, Lcs).

%% ufun_clause(Lclause, [KnownVar], State) -> {Lclause,State}.

ufun_clause(Cl0, Ks, St0) ->
    {Cl1,Pvs,Used,_,St1} = uclause(Cl0, [], Ks, St0),
    A0 = get_anno(Cl1),
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
	    LA = get_lineno_anno(Var),
	    Test = #icall{anno=#a{anno=LA,us=add_element(N, [V])},
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
upattern(#c_map{es=Es0}=Map, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {Map#c_map{es=Es1},Esg,Esv,Eus,St1};
upattern(#c_map_pair{op=#c_literal{val=exact},key=K0,val=V0}=Pair,Ks,St0) ->
    {V,Vg,Vn,Vu,St1} = upattern(V0, Ks, St0),
    % A variable key must be considered used here
    Ku = case K0 of
	#c_var{name=Name} -> [Name];
	_ -> []
    end,
    {Pair#c_map_pair{val=V},Vg,Vn,union(Ku,Vu),St1};
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
    {Es1,Pg,Pv,Pu0,St1} = upat_bin(Es0, Ks, [], St0),

    %% In a clause such as <<Sz:8,V:Sz>> in a function head, Sz will both
    %% be new and used; a situation that is not handled properly by
    %% uclause/4.  (Basically, since Sz occurs in two sets that are
    %% subtracted from each other, Sz will not be added to the list of
    %% known variables and will seem to be new the next time it is
    %% used in a match.)
    %%   Since the variable Sz really is new (it does not use a
    %% value bound prior to the binary matching), Sz should only be
    %% included in the set of new variables. Thus we should take it
    %% out of the set of used variables.

    Pu1 = subtract(Pu0, intersection(Pv, Pu0)),
    {Es1,Pg,Pv,Pu1,St1}.

%% upat_bin([Pat], [KnownVar], [LocalVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.
upat_bin([P0|Ps0], Ks, Bs, St0) ->
    {P1,Pg,Pv,Pu,Bs1,St1} = upat_element(P0, Ks, Bs, St0),
    {Ps1,Psg,Psv,Psu,St2} = upat_bin(Ps0, union(Pv, Ks), Bs1, St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upat_bin([], _, _, St) -> {[],[],[],[],St}.    


%% upat_element(Segment, [KnownVar], [LocalVar], State) ->
%%        {Segment,[GuardTest],[NewVar],[UsedVar],[LocalVar],State}
upat_element(#c_bitstr{val=H0,size=Sz0}=Seg, Ks, Bs0, St0) ->
    {H1,Hg,Hv,[],St1} = upattern(H0, Ks, St0),
    Bs1 = case H0 of
	      #c_var{name=Hname} ->
		  case H1 of
		      #c_var{name=Hname} ->
			  Bs0;
		      #c_var{name=Other} ->
			  [{Hname,Other}|Bs0]
		  end;
	      _ ->
		  Bs0
	  end,
    {Sz1,Us} = case Sz0 of
		   #c_var{name=Vname} -> 
		       rename_bitstr_size(Vname, Bs0);
		   _Other ->
		       {Sz0,[]}
	       end,
    {Seg#c_bitstr{val=H1,size=Sz1},Hg,Hv,Us,Bs1,St1}.

rename_bitstr_size(V, [{V,N}|_]) ->
    New = #c_var{name=N},
    {New,[N]};
rename_bitstr_size(V, [_|Rest]) ->
    rename_bitstr_size(V, Rest);
rename_bitstr_size(V, []) ->
    Old = #c_var{name=V},
    {Old,[V]}.
 
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
    Isimple = #isimple{anno=#a{us=[Name]},term=Var},
    cexprs([Iset,Isimple], As, St);
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
cexpr(#ifun{name=unnamed}=Fun, As, St0) ->
    cfun(Fun, As, St0);
cexpr(#ifun{anno=#a{us=Us0}=A0,name={named,Name},fc=#iclause{pats=Ps}}=Fun0,
      As, St0) ->
    case is_element(Name, Us0) of
        false ->
            cfun(Fun0, As, St0);
        true ->
            A1 = A0#a{us=del_element(Name, Us0)},
            Fun1 = Fun0#ifun{anno=A1},
            {#c_fun{body=Body}=CFun0,[],Us1,St1} = cfun(Fun1, As, St0),
            RecVar = #c_var{name={Name,length(Ps)}},
            Let = #c_let{vars=[#c_var{name=Name}],arg=RecVar,body=Body},
            CFun1 = CFun0#c_fun{body=Let},
            Letrec = #c_letrec{anno=A0#a.anno,
			       defs=[{RecVar,CFun1}],
                               body=RecVar},
            {Letrec,[],Us1,St1}
    end;
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
cexpr(#isimple{anno=#a{us=Vs},term=Simple}, _As, St) ->
    true = is_simple(Simple),		%Sanity check!
    {Simple,[],Vs,St}.

cfun(#ifun{anno=A,id=Id,vars=Args,clauses=Lcs,fc=Lfc}, _As, St0) ->
    {Ccs,St1} = cclauses(Lcs, [], St0),     %NEVER export!
    {Cfc,St2} = cclause(Lfc, [], St1),
    Anno = A#a.anno,
    {#c_fun{anno=Id++Anno,vars=Args,
            body=#c_case{anno=Anno,
                         arg=set_anno(core_lib:make_values(Args), Anno),
                         clauses=Ccs ++ [Cfc]}},
     [],A#a.us,St2}.

%% lit_vars(Literal) -> [Var].

lit_vars(Lit) -> lit_vars(Lit, []).

lit_vars(#c_cons{hd=H,tl=T}, Vs) -> lit_vars(H, lit_vars(T, Vs));
lit_vars(#c_tuple{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(#c_map{arg=V,es=Es}, Vs) -> lit_vars(V, lit_list_vars(Es, Vs));
lit_vars(#c_map_pair{key=K,val=V}, Vs) -> lit_vars(K, lit_vars(V, Vs));
lit_vars(#c_var{name=V}, Vs) -> add_element(V, Vs); 
lit_vars(_, Vs) -> Vs.				%These are atomic

lit_list_vars(Ls) -> lit_list_vars(Ls, []).

lit_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> lit_vars(L, Vs0) end, Vs, Ls).

bitstr_vars(Segs) ->
    bitstr_vars(Segs, []).

bitstr_vars(Segs, Vs) ->
    foldl(fun (#c_bitstr{val=V,size=S}, Vs0) ->
 		  lit_vars(V, lit_vars(S, Vs0))
	  end, Vs, Segs).

record_anno(L, St) ->
    case
        erl_anno:record(L) andalso member(dialyzer, St#core.opts)
    of
        true ->
            [record | lineno_anno(L, St)];
        false ->
            full_anno(L, St)
    end.

full_anno(L, #core{wanted=false}=St) ->
    [result_not_wanted|lineno_anno(L, St)];
full_anno(L, #core{wanted=true}=St) ->
    lineno_anno(L, St).

lineno_anno(L, St) ->
    Line = erl_anno:line(L),
    Generated = erl_anno:generated(L),
    CompilerGenerated = [compiler_generated || Generated],
    [Line] ++ St#core.file ++ CompilerGenerated.

get_lineno_anno(Ce) ->
    case get_anno(Ce) of
	#a{anno=A} -> A;
	A when is_list(A) -> A
    end.

no_compiler_warning(Anno) ->
    erl_anno:set_generated(true, Anno).

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
is_simple(#c_map{es=Es}) -> is_simple_list(Es);
is_simple(#c_map_pair{key=K,val=V}) ->
    is_simple(K) andalso is_simple(V);
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
    "binary construction will fail because of a type mismatch";
format_error(badmap) ->
    "map construction will fail because of a type mismatch".

add_warning(Anno, Term, #core{ws=Ws,file=[{file,File}]}=St) ->
    case erl_anno:generated(Anno) of
        false ->
            St#core{ws=[{File,[{erl_anno:location(Anno),?MODULE,Term}]}|Ws]};
        true ->
            St
    end.
