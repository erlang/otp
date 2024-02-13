%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2024. All Rights Reserved.
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
%% Core transformation is done in four stages:
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
%% 4. Lower receives to more primitive operations.  Split binary
%%    patterns where a value is matched out and then used used as
%%    a size in the same pattern.  That simplifies the subsequent
%%    passes as all variables are within a single pattern are either
%%    new or used, but never both at the same time.
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
-moduledoc false.

-export([module/2,format_error/1]).

-import(lists, [any/2,reverse/1,reverse/2,map/2,member/2,foldl/3,foldr/3,mapfoldl/3,
                splitwith/2,keydelete/3,keyfind/3,keymember/3,sort/1,droplast/1,last/1,
                duplicate/2]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).
-import(cerl, [ann_c_cons/3,ann_c_tuple/2,c_tuple/1,
	       ann_c_map/3,cons_hd/1,cons_tl/1]).

-include("core_parse.hrl").

%% Matches expansion max segment in v3_kernel.
-define(COLLAPSE_MAX_SIZE_SEGMENT, 1024).

%% Internal core expressions and help functions.
%% N.B. annotations fields in place as normal Core expressions.

-record(a, {us=[],ns=[],anno=[]}).		%Internal annotation

-record(iapply,    {anno=#a{},op,args}).
-record(ibinary,   {anno=#a{},segments}).	%Not used in patterns.
-record(ibitstr,   {anno=#a{},val,size,unit,type,flags}).
-record(icall,     {anno=#a{},module,name,args}).
-record(icase,     {anno=#a{},args,clauses,fc}).
-record(icatch,    {anno=#a{},body}).
-record(iclause,   {anno=#a{},pats,guard,body}).
-record(ifun,      {anno=#a{},id,vars,clauses,fc,name=unnamed}).
-record(iletrec,   {anno=#a{},defs,body}).
-record(imatch,    {anno=#a{},pat,guard=[],arg,fc}).
-record(iexprs,    {anno=#a{},bodies=[]}).
-record(imap,      {anno=#a{},arg=#c_literal{val=#{}},es,is_pat=false}).
-record(imappair,  {anno=#a{},op,key,val}).
-record(iprimop,   {anno=#a{},name,args}).
-record(iprotect,  {anno=#a{},body}).
-record(ireceive1, {anno=#a{},clauses}).
-record(ireceive2, {anno=#a{},clauses,timeout,action}).
-record(iset,      {anno=#a{},var,arg}).
-record(itry,      {anno=#a{},args,vars,body,evars,handler}).
-record(ifilter,   {anno=#a{},arg}).
-record(igen,      {anno=#a{},acc_pat,acc_guard,
                    skip_pat,tail,tail_pat,arg,
                    refill={nomatch,ignore}}).
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
-type imap()      :: #imap{}.
-type iprimop()   :: #iprimop{}.
-type iprotect()  :: #iprotect{}.
-type ireceive1() :: #ireceive1{}.
-type ireceive2() :: #ireceive2{}.
-type iset()      :: #iset{}.
-type itry()      :: #itry{}.
-type ifilter()   :: #ifilter{}.
-type igen()      :: #igen{}.
-type isimple()   :: #isimple{}.

-type i() :: iapply()    | ibinary()   | icall()     | icase()  | icatch()
           | iclause()   | ifun()      | iletrec()   | imatch() | imap()
           | iprimop()   | iprotect()  | ireceive1() | ireceive2()
           | iset()      | itry()      | ifilter()
           | igen()      | isimple().

-type warning() :: {file:filename(), [{integer(), module(), term()}]}.

-record(core, {vcount=0 :: non_neg_integer(),	%Variable counter
	       fcount=0 :: non_neg_integer(),	%Function counter
               gcount=0 :: non_neg_integer(),   %Goto counter
	       function={none,0} :: fa(),	%Current function.
	       in_guard=false :: boolean(),	%In guard or not.
	       wanted=true :: boolean(),	%Result wanted or not.
	       opts=[]     :: [compile:option()], %Options.
               dialyzer=false :: boolean(),     %Help dialyzer or not.
	       ws=[]    :: [warning()],		%Warnings.
               file=[{file,""}],                %File.
               load_nif=false :: boolean()      %true if calls erlang:load_nif/2
	      }).

%% XXX: The following type declarations do not belong in this module
-type fa()        :: {atom(), arity()}.
-type attribute() :: atom().
-type form()      :: {function, integer(), atom(), arity(), _}
                   | {attribute, integer(), attribute(), _}.

-record(imodule, {name = [],
		  exports = ordsets:new(),
                  nifs = none ::
                    'none' | sets:set(), % Is a set if the attribute is
                                         % present in the module.
		  attrs = [],
		  defs = [],
		  file = [],
		  opts = [],
		  ws = [],
                  load_nif=false :: boolean() %true if calls erlang:load_nif/2
                 }).

-spec module([form()], [compile:option()]) ->
        {'ok',cerl:c_module(),[warning()]}.

module(Forms0, Opts) ->
    Forms = erl_internal:add_predefined_functions(Forms0),
    Module = foldl(fun (F, Acc) ->
			   form(F, Acc, Opts)
		   end, #imodule{}, Forms),
    #imodule{name=Mod,exports=Exp0,attrs=As0,
             defs=Kfs0,ws=Ws,load_nif=LoadNif,nifs=Nifs} = Module,
    Exp = case member(export_all, Opts) of
	      true -> defined_functions(Forms);
	      false -> Exp0
	  end,
    Cexp = [#c_var{name=FA} || {_,_}=FA <- Exp],
    Kfs1 = reverse(Kfs0),
    Kfs = if LoadNif and (Nifs =:= none) ->
                  insert_nif_start(Kfs1);
             true ->
                  Kfs1
          end,
    As = reverse(As0),

    {ok,#c_module{name=#c_literal{val=Mod},exports=Cexp,attrs=As,defs=Kfs},Ws}.

form({function,_,_,_,_}=F0,
     #imodule{defs=Defs,load_nif=LoadNif0}=Module,
     Opts) ->
    {F,Ws,LoadNif} = function(F0, Module, Opts),
    Module#imodule{defs=[F|Defs],ws=Ws,load_nif=LoadNif or LoadNif0};
form({attribute,_,module,Mod}, Module, _Opts) ->
    true = is_atom(Mod),
    Module#imodule{name=Mod};
form({attribute,_,file,{File,_Line}}=F, #imodule{attrs=As}=Module, _Opts) ->
    Module#imodule{file=File, attrs=[attribute(F)|As]};
form({attribute,_,import,_}, Module, _Opts) ->
    %% Ignore. We have no further use for imports.
    Module;
form({attribute,_,export,Es}, #imodule{exports=Exp0}=Module, _Opts) ->
    Exp = ordsets:union(ordsets:from_list(Es), Exp0),
    Module#imodule{exports=Exp};
form({attribute,_,nifs,Ns}, #imodule{nifs=Nifs0}=Module, _Opts) ->
    Nifs1 = case Nifs0 of
                none ->
                    sets:new([{version, 2}]);
                _ ->
                    Nifs0
            end,
    Nifs = sets:union(sets:from_list(Ns, [{version,2}]), Nifs1),
    Module#imodule{nifs=Nifs};
form({attribute,_,_,_}=F, #imodule{attrs=As}=Module, _Opts) ->
    Module#imodule{attrs=[attribute(F)|As]};
form(_, Module, _Opts) ->
    %% Ignore uninteresting forms such as 'eof'.
    Module.

attribute({attribute,A,Name,Val0}) ->
    Line = [erl_anno:location(A)],
    Val = if
	      is_list(Val0) -> Val0;
	      true -> [Val0]
	  end,
    {#c_literal{val=Name, anno=Line}, #c_literal{val=Val, anno=Line}}.

defined_functions(Forms) ->
    Fs = [{Name,Arity} || {function,_,Name,Arity,_} <- Forms],
    ordsets:from_list(Fs).

%% function_dump(module_info,_,_,_) -> ok;
%% function_dump(Name,Arity,Format,Terms) ->
%%     io:format("~w/~w " ++ Format,[Name,Arity]++Terms),
%%     ok.

function({function,_,Name,Arity,Cs0}, Module, Opts)
  when is_integer(Arity), 0 =< Arity, Arity =< 255 ->
    #imodule{file=File, ws=Ws0, nifs=Nifs} = Module,
    try
        St0 = #core{vcount=0,function={Name,Arity},opts=Opts,
                    dialyzer=member(dialyzer, Opts),
                    ws=Ws0,file=[{file,File}]},
        {B0,St1} = body(Cs0, Name, Arity, St0),
        %% ok = function_dump(Name, Arity, "body:~n~p~n",[B0]),
        {B1,St2} = ubody(B0, St1),
        %% ok = function_dump(Name, Arity, "ubody:~n~p~n",[B1]),
        {B2,St3} = cbody(B1, Nifs, St2),
        %% ok = function_dump(Name, Arity, "cbody:~n~p~n",[B2]),
        {B3,#core{ws=Ws,load_nif=LoadNif}} = lbody(B2, St3),
        %% ok = function_dump(Name, Arity, "lbody:~n~p~n",[B3]),
        {{#c_var{name={Name,Arity}},B3},Ws,LoadNif}
    catch
        Class:Error:Stack ->
	    io:fwrite("Function: ~w/~w\n", [Name,Arity]),
	    erlang:raise(Class, Error, Stack)
    end.

body(Cs0, Name, Arity, St0) ->
    Anno = lineno_anno(element(2, hd(Cs0)), St0),
    FunAnno = [{function,{Name,Arity}} | Anno],
    {Args0,St1} = new_vars(Anno, Arity, St0),
    Args = reverse(Args0),                      %Nicer order
    {Cs1,St2} = clauses(Cs0, St1),
    {Ps,St3} = new_vars(Arity, St2),    %Need new variables here
    Fc = function_clause(Ps, FunAnno),
    {#ifun{anno=#a{anno=FunAnno},id=[],vars=Args,clauses=Cs1,fc=Fc},St3}.

%% clause(Clause, State) -> {Cclause,State}.
%% clauses([Clause], State) -> {[Cclause],State}.
%%  Convert clauses. Trap bad pattern aliases.

clauses([C0|Cs0], St0) ->
    {C,St1} = clause(C0, St0),
    {Cs,St2} = clauses(Cs0, St1),
    {[C|Cs],St2};
clauses([], St) -> {[],St}.

clause({clause,Lc,H0,G0,B0}, St0) ->
    try head(H0, St0) of
	{H1,St1} ->
	    {G1,St2} = guard(G0, St1),
	    {B1,St3} = exprs(B0, St2),
            Anno = lineno_anno(Lc, St3),
            {#iclause{anno=#a{anno=Anno},pats=H1,guard=G1,body=B1},St3}
    catch
	throw:nomatch ->
            %% This pattern can't possibly match. If we simply remove
            %% the clause, variables that are used later might not be
            %% bound. Therefore, we must keep the clause, but rewrite
            %% the pattern to a pattern that will bind the same
            %% variables and ensure that the clause can't be executed
            %% by letting the guard return false.
            St1 = add_warning(Lc, {nomatch,pattern}, St0),
            H1 = [sanitize(P) || P <- H0],
            false = H0 =:= H1,                  %Assertion.
            G1 = [[{atom,Lc,false}]],
            LcNoWarn = no_compiler_warning(Lc),
            clause({clause,LcNoWarn,H1,G1,B0}, St1)
    end.

clause_arity({clause,_,H0,_,_}) -> length(H0).

%% head([P], State) -> {[P],[Cexpr],State}.

head(Ps, St) ->
    pattern_list(Ps, St).

%% guard([Expr], State) -> {[Cexpr],State}.
%%  Build an explicit and/or tree of guard alternatives, then traverse
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
gexpr({op,_,'andalso',_,_}=E0, Bools, St0) ->
    {op,L,'andalso',E1,E2} = right_assoc(E0, 'andalso'),
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch(L, E1, V, E2, False),
    gexpr(E, Bools, St);
gexpr({op,_,'orelse',_,_}=E0, Bools, St0) ->
    {op,L,'orelse',E1,E2} = right_assoc(E0, 'orelse'),
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch(L, E1, V, True, E2),
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

%% gexpr_bool(L, R, Bools, State) -> {Cexpr,[PreExp],Bools,State}.
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
        #icall{anno=#a{anno=[v3_core,compiler_generated]},
               module=#c_literal{val=erlang},
               name=#c_literal{val='=:='},
               args=[E,#c_literal{val=true}]}=EqCall ->
            %%
            %% We here have the expression:
            %%
            %%    not(Expr =:= true)
            %%
            %% The annotations tested in the code above guarantees
            %% that the original expression in the Erlang source
            %% code was:
            %%
            %%    not Expr
            %%
            %% That expression can be transformed as follows:
            %%
            %%    not Expr  ==>  Expr =:= false
            %%
            %% which will produce the same result, but may eliminate
            %% redundant is_boolean/1 tests (see unforce/3).
            %%
            %% Note that this transformation would not be safe if the
            %% original expression had been:
            %%
            %%    not(Expr =:= true)
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
        #icall{anno=Anno,module=#c_literal{val=erlang},
               name=#c_literal{val=is_function},
               args=[_,_]} ->
            %% is_function/2 is not a safe type test. We must force
            %% it to be protected.
            Lanno = Anno#a.anno,
            {New,St2} = new_var(Lanno, St1),
            {icall_eq_true(New),
             Eps0 ++ [#iset{anno=Anno,var=New,arg=E1}],Bools0,St2};
	#icall{anno=Anno,module=#c_literal{val=erlang},name=#c_literal{val=N},args=As} ->
            %% Note that erl_expand_records has renamed type
            %% tests to the new names; thus, float/1 as a type
            %% test will now be named is_float/1.
	    Ar = length(As),
	    case erl_internal:new_type_test(N, Ar) orelse
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
    %% We need to recognize a '=:=' added by this pass, so we will add
    %% an extra 'v3_core' annotation. (Being paranoid, we don't want
    %% to trust 'compiler_generated' alone as it could have been added
    %% by a parse transform.)
    #icall{anno=#a{anno=[v3_core,compiler_generated]},
	   module=#c_literal{val=erlang},
	   name=#c_literal{val='=:='},
	   args=[Arg,#c_literal{val=true}]}.

%% force_booleans([Var], E, Eps, St) -> Expr.
%%  Variables used in the top-level of a guard must be booleans.
%%
%%  Add necessary is_boolean/1 guard tests to ensure that the guard
%%  will fail if any of the variables is not a boolean.

force_booleans(Vs0, E, Eps, St) ->
    Vs1 = [set_anno(V, []) || V <- Vs0],

    %% Prune the list of variables that will need is_boolean/1
    %% tests. Basically, if the guard consists of simple expressions
    %% joined by 'and's no is_boolean/1 tests are needed.
    Vs = unforce(E, Eps, Vs1),

    %% Add is_boolean/1 tests for the remaining variables.
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
%%  The basic idea for filtering is the following transformation:
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
%%  but expressions such as:
%%
%%     not (E =:= true) and is_boolean(E)
%%
%%  or expression using 'or' or 'xor' cannot be transformed in this
%%  way (such expressions are the reason for adding the is_boolean/1
%%  test in the first place).
%%
unforce(_, _, []) ->
    [];
unforce(E, Eps, Vs) ->
    Tree = unforce_tree(Eps++[E], gb_trees:empty()),
    unforce(Tree, Vs).

unforce_tree([#iexprs{bodies=Exprs}|Es], D0) ->
    unforce_tree(lists:append(Exprs) ++ Es, D0);
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

%% exprs([Expr], State) -> {[Cexpr],State}.
%%  Flatten top-level exprs while handling maybe_match operators.

maybe_match_exprs([{maybe_match,L,P0,E0}|Es0], Fail, St0) ->
    {Es1,St1} = maybe_match_exprs(Es0, Fail, St0),
    {C,St2} =
        case Es1 of
            [] ->
                {AllName,StInt} = new_var_name(St1),
                All = {var,L,AllName},
                clause({clause,L,[{match,L,P0,All}],[],[All]}, StInt);
            [_|_] ->
                {C0,StInt} = clause({clause,L,[P0],[],[{nil,0}]}, St1),
                {C0#iclause{body=Es1},StInt}
        end,
    {E1,Eps,St3} = novars(E0, St2),
    {Fpat,St4} = new_var(St3),
    Lanno = lineno_anno(L, St4),
    Fc = #iclause{anno=#a{anno=[dialyzer_ignore,compiler_generated|Lanno]},pats=[Fpat],guard=[],
                  body=[#iapply{op=Fail,args=[Fpat]}]},
    {Eps ++ [#icase{anno=#a{anno=Lanno},args=[E1],clauses=[C],fc=Fc}],St4};
maybe_match_exprs([E0|Es0], Fail, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Es1,St2} = maybe_match_exprs(Es0, Fail, St1),
    {Eps ++ [E1|Es1],St2};
maybe_match_exprs([], _Fail, St) ->
    {[],St}.

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
    {[H1,T1],Eps,St1} = safe_list([H0,T0], St0),
    A = full_anno(L, St1),
    {annotate_cons(A, H1, T1, St1),Eps,St1};
expr({lc,L,E,Qs0}, St0) ->
    {Qs1,St1} = preprocess_quals(L, Qs0, St0),
    lc_tq(L, E, Qs1, #c_literal{anno=lineno_anno(L, St1),val=[]}, St1);
expr({bc,L,E,Qs}, St) ->
    bc_tq(L, E, Qs, St);
expr({mc,L,E,Qs0}, St0) ->
    {Qs1,St1} = preprocess_quals(L, Qs0, St0),
    mc_tq(L, E, Qs1, #c_literal{anno=lineno_anno(L, St1),val=[]}, St1);
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
	throw:{bad_binary,Eps,St1} ->
	    St = add_warning(L, {failed,bad_binary}, St1),
	    LineAnno = lineno_anno(L, St),
	    As = [#c_literal{anno=LineAnno,val=badarg}],
	    {#icall{anno=#a{anno=LineAnno},	%Must have an #a{}
		    module=#c_literal{anno=LineAnno,val=erlang},
		    name=#c_literal{anno=LineAnno,val=error},
		    args=As},Eps,St}
    end;
expr({block,_,Es0}, St0) ->
    %% Inline the block directly.
    {Es1,St1} = exprs(droplast(Es0), St0),
    {E1,Eps,St2} = expr(last(Es0), St1),
    {E1,Es1 ++ Eps,St2};
expr({'maybe',L,Es}, St0) ->
    {V,St1} = new_var_name(St0),
    Var = {var,L,V},
    Cs = [{clause,L,[Var],[],[Var]}],
    expr({'maybe',L,Es,{'else',L,Cs}}, St1);
expr({'maybe',L,Es0,{'else',_,Cs0}}, St0) ->
    %% Translate the maybe ... else ... end construct.
    %%
    %% As an example, the following Erlang code:
    %%
    %% foo(A) ->
    %%     maybe
    %%         {ok, V} ?= A,
    %%         V
    %%     else
    %%         Other ->
    %%             {error, Other}
    %%     end.
    %%
    %% is translated into Core Erlang like this:
    %%
    %% 'foo'/1 =
    %%     fun (_0) ->
    %%         case _0 of
    %%             <A> when 'true' ->
    %%                 ( letrec
    %%                       'maybe_else_fail'/1 =
    %%                           fun (_3) ->
    %%                               case _3 of
    %%                                 <_2> when 'true' ->
    %%                                     case _2 of
    %%                                       <Other> when 'true' ->
    %%                                           {'error',Other}
    %%                                       ( <_1> when 'true' ->
    %%                                             primop 'match_fail'({'else_clause',_1})
    %%                                         -| ['compiler_generated'] )
    %%                                     end
    %%                                 ( <_1> when 'true' ->
    %%                                       primop 'match_fail'('never_fails')
    %%                                   -| ['compiler_generated'] )
    %%                               end
    %%                   in
    %%                       case A of
    %%                         <{'ok',V}> when 'true' ->
    %%                             V
    %%                         ( <_4> when 'true' ->
    %%                               apply 'maybe_else_fail'/1(_4)
    %%                           -| ['dialyzer_ignore','compiler_generated'] )
    %%                       end
    %%                   -| ['letrec_goto','no_inline'] )
    %%             ( <_5> when 'true' ->
    %%                   primop 'match_fail'({'function_clause',_5})
    %%               -| ['compiler_generated'] )
    %%           end
    {[V1,V2,FailVar],St1} = new_vars(3, St0),

    %% Translate the body of the letrec.
    Fail = {maybe_else_fail,1},
    Lanno = lineno_anno(L, St1),
    {Es1,St2} = maybe_match_exprs(Es0, #c_var{name=Fail}, St1),

    %% Translate the 'else' clauses. Note that we must not put the clauses
    %% as the top-level clauses in the fun, because all shawdowing variables
    %% in a fun head will be renamed.
    {Cs1,St3} = clauses(Cs0, St2),
    Fc1 = fail_clause([FailVar], Lanno, c_tuple([#c_literal{val=else_clause},FailVar])),
    FailCase = #icase{args=[V2],clauses=Cs1,fc=Fc1},
    FailFunCs = [#iclause{pats=[V2],guard=[#c_literal{val=true}],
                          body=[FailCase]}],
    Anno = #a{anno=[letrec_goto,no_inline|Lanno]},
    Fc2 = fail_clause([FailVar], Lanno, #c_literal{val=never_fails}),
    FailFun = #ifun{id=[],vars=[V1],
                    clauses=FailFunCs,
                    fc=Fc2},

    %% Construct the letrec.
    Letrec = #iletrec{anno=Anno,defs=[{Fail,FailFun}],body=Es1},
    {Letrec,[],St3};
expr({'if',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Lanno = lineno_anno(L, St1),
    Fc = fail_clause([], Lanno, #c_literal{val=if_clause}),
    {#icase{anno=#a{anno=Lanno},args=[],clauses=Cs1,fc=Fc},[],St1};
expr({'case',L,E0,Cs0}, St0) ->
    {E1,Eps,St1} = novars(E0, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Fpat,St3} = new_var(St2),
    Lanno = lineno_anno(L, St2),
    Fc = fail_clause([Fpat], Lanno, c_tuple([#c_literal{val=case_clause},Fpat])),
    {#icase{anno=#a{anno=Lanno},args=[E1],clauses=Cs1,fc=Fc},Eps,St3};
expr({'receive',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {#ireceive1{anno=#a{anno=lineno_anno(L, St1)},clauses=Cs1},[],St1};
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
    Lanno = lineno_anno(L, St4),
    Fc = fail_clause([Fpat], Lanno,
		     c_tuple([#c_literal{val=try_clause},Fpat])),
    {Evs,Hs,St5} = try_exception(Ecs, St4),
    {#itry{anno=#a{anno=lineno_anno(L, St5)},args=Es1,
	   vars=[V],body=[#icase{anno=#a{anno=Lanno},args=[V],clauses=Cs1,fc=Fc}],
	   evars=Evs,handler=Hs},
     [],St5};
expr({'try',L,Es0,[],[],As0}, St0) ->
    %% 'try ... after ... end'
    try_after(L, Es0, As0, St0);
expr({'try',L,Es,Cs,Ecs,As}, St0) ->
    %% 'try ... [of ...] [catch ...] after ... end'
    expr({'try',L,[{'try',L,Es,Cs,Ecs,[]}],[],[],As}, St0);
expr({'catch',L,E0}, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    Lanno = lineno_anno(L, St1),
    {#icatch{anno=#a{anno=Lanno},body=Eps ++ [E1]},[],St1};
expr({'fun',L,{function,F,A}}, St0) ->
    Lanno = full_anno(L, St0),
    {#c_var{anno=Lanno,name={F,A}},[],St0};
expr({'fun',L,{function,M,F,A}}, St0) ->
    {As,Aps,St1} = safe_list([M,F,A], St0),
    Lanno = full_anno(L, St1),
    {#icall{anno=#a{anno=Lanno},
	    module=#c_literal{val=erlang},
	    name=#c_literal{val=make_fun},
	    args=As},Aps,St1};
expr({'fun',L,{clauses,Cs}}, St) ->
    fun_tq(Cs, L, St, unnamed);
expr({named_fun,L,'_',Cs}, St) ->
    fun_tq(Cs, L, St, unnamed);
expr({named_fun,L,Name,Cs}, St) ->
    fun_tq(Cs, L, St, {named,Name});
expr({call,L,{remote,_,M0,F0},As0}, St0) ->
    {[M1,F1|As1],Aps,St1} = safe_list([M0,F0|As0], St0),
    Anno = full_anno(L, St1),
    case {M1,F1,As1} of
        {#c_literal{val=erlang},
         #c_literal{val=error},
         [#c_tuple{es=[#c_literal{val=badrecord},_]}=Tuple]} ->
            Fail = #iprimop{anno=#a{anno=Anno},
                            name=#c_literal{val=match_fail},
                            args=[Tuple]},
            {Fail,Aps,St1};
        {#c_literal{val=erlang},#c_literal{val=load_nif},[_,_]} ->
            {#icall{anno=#a{anno=Anno},module=M1,name=F1,args=As1},
             Aps,St1#core{load_nif=true}};
        {_,_,_} ->
            {#icall{anno=#a{anno=Anno},module=M1,name=F1,args=As1},Aps,St1}
    end;
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
    St1 = set_wanted(P0, St0),
    case fold_match(E0, P0) of
        {{sequential_match,_,_,_}=P1,E1} ->
            %% Matching of an expression to more than one pattern. Example:
            %%
            %%    #{Key := Value} = #{key := Key} = Expr
            {E2,Eps1,St2} = safe(E1, St1),
            St3 = St2#core{wanted=St0#core.wanted},

            %% If necessary, bind the expression to a variable to ensure it is
            %% only evaluted once.
            {Var,Eps2,St4} =
                case E2 of
                    #c_var{} ->
                        {E2,[],St3};
                    _ ->
                        {Var0,StInt} = new_var(St3),
                        {Var0,[#iset{var=Var0,arg=E2}],StInt}
                end,

            %% Rewrite to a begin/end block matching one pattern at the time
            %% (using the `single_match` operator). Example:
            %%
            %% begin
            %%   V = Expr,
            %%   #{key := Key} = V,
            %%   #{Key := Value} = V
            %% end
            Block = blockify(L, P1, Var),
            {E3,Eps3,St5} = expr({block,L,Block}, St4),
            {E3,Eps1 ++ Eps2 ++ Eps3,St5};
        {P0,E1} ->
            %% Matching of an expression to a single pattern. Example:
            %%    {A,B} = Expr
            {E2,Eps1,St2} = novars(E1, St1),
            St3 = St2#core{wanted=St0#core.wanted},
            {E3,Eps2,St4} = single_match(L, P0, E2, St3),
            {E3,Eps1 ++ Eps2,St4}
    end;
expr({single_match,L,P,#c_var{}=E}, St0) ->
    single_match(L, P, E, St0);
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
expr({op,_,'andalso',_,_}=E0, St0) ->
    {op,L,'andalso',E1,E2} = right_assoc(E0, 'andalso'),
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    False = {atom,L,false},
    E = make_bool_switch(L, E1, V, E2, False),
    expr(E, St);
expr({op,_,'orelse',_,_}=E0, St0) ->
    {op,L,'orelse',E1,E2} = right_assoc(E0, 'orelse'),
    Anno = lineno_anno(L, St0),
    {#c_var{name=V0},St} = new_var(Anno, St0),
    V = {var,L,V0},
    True = {atom,L,true},
    E = make_bool_switch(L, E1, V, True, E2),
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
	    name=#c_literal{anno=LineAnno,val=Op},args=As},Aps,St1};
expr({executable_line,L,_}, St0) ->
    {#iprimop{anno=#a{anno=lineno_anno(L, St0)},
              name=#c_literal{val=executable_line},
              args=[]},[],St0};
expr({ssa_check_when,L,WantedResult,Args,Tag,Clauses}, St) ->
    {#c_opaque{anno=full_anno(L, St),val={ssa_check_when,WantedResult,Tag,Args,Clauses}}, [], St}.

blockify(L0, {sequential_match,_L1,First,Then}, E) ->
    [{single_match,L0,First,E}|blockify(L0, Then, E)];
blockify(L, P, E) ->
    [{single_match,L,P,E}].

%% single_match(Line, AbstractPattern, CoreExpr, State0) -> {Expr,Pre,State}.
%%  Generate the code for matching an expression against a single pattern.
single_match(L, P0, E, St0) ->
    {Fpat,St1} = new_var(St0),
    Lanno = lineno_anno(L, St1),
    Fc = fail_clause([Fpat], Lanno, c_tuple([#c_literal{val=badmatch},Fpat])),
    try pattern(P0, St1) of
        {P1,St2} ->
            St3 = set_wanted(P0, St2),
            St4 = St3#core{wanted=St0#core.wanted},
            {#imatch{anno=#a{anno=Lanno},pat=P1,arg=E,fc=Fc},[],St4}
    catch
        throw:nomatch ->
            %% The pattern will not match. We must take care here to
            %% bind all variables that the pattern would have bound
            %% so that subsequent expressions do not refer to unbound
            %% variables.
            %%
            %% As an example, this code:
            %%
            %%   ([X] = {Y}) = E,
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
            St2 = add_warning(L, {nomatch,pattern}, St1),
            {Expr,Eps0,St3} = force_safe(E, St2),
            SanPat0 = sanitize(P0),
            {SanPat,St} = pattern(SanPat0, St3),
            Badmatch = c_tuple([#c_literal{val=badmatch},Expr]),
            Fail = #iprimop{anno=#a{anno=Lanno},
                            name=#c_literal{val=match_fail},
                            args=[Badmatch]},
            Eps = Eps0 ++ [Fail],
            {#imatch{anno=#a{anno=Lanno},pat=SanPat,arg=Expr,fc=Fc},Eps,St}
    end.

%% set_wanted(Pattern, St) -> St'.
%%  Suppress warnings for expressions that are bound to the '_'
%%  variable and variables that begin with '_'.
set_wanted({var,_,'_'}, St) ->
    St#core{wanted=false};
set_wanted({var,_,Var}, St) ->
    case atom_to_list(Var) of
        "_" ++ _ ->
            St#core{wanted=false};
        _ ->
            St
    end;
set_wanted(_, St) -> St.

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
sanitize({bin,L,Segs0}) ->
    Segs = [Var || {bin_element,_,{var,_,_}=Var,_,_} <- Segs0],
    {tuple,L,Segs};
sanitize({map,L,Ps0}) ->
    Ps = [sanitize(V) || {map_field_exact,_,_,V} <- Ps0],
    {tuple,L,Ps};
sanitize({op,L,_Name,P1,P2}) ->
    {tuple,L,[sanitize(P1),sanitize(P2)]};
sanitize(P) -> P.

make_bool_switch(L, E, V, T, F) ->
    NegL = no_compiler_warning(L),
    Error = {tuple,NegL,[{atom,NegL,badarg},V]},
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],[T]},
      {clause,NegL,[{atom,NegL,false}],[],[F]},
      {clause,NegL,[V],[],
       [{call,NegL,{remote,NegL,{atom,NegL,erlang},{atom,NegL,error}},
	 [Error]}]}]}.

expr_map(M0, Es0, L, St0) ->
    {M1,Eps0,St1} = safe_map(M0, St0),
    Badmap = badmap_term(M1, St1),
    A = lineno_anno(L, St1),
    Fc = fail_clause([], [{eval_failure,badmap}|A], Badmap),
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
    {#icase{anno=#a{anno=A},args=[],clauses=Cs,fc=Fc},Eps,St2}.

safe_map(M0, St0) ->
    case safe(M0, St0) of
        {#c_var{},_,_}=Res ->
            Res;
        {#c_literal{val=Map},_,_}=Res when is_map(Map) ->
            Res;
        {NotMap,Eps0,St1} ->
            %% Not a map. There will be a syntax error if we try to
            %% pretty-print the Core Erlang code and then try to parse
            %% it. To avoid the syntax error, force the term into a
            %% variable.
	    {V,St2} = new_var(St1),
            Anno = cerl:get_ann(NotMap),
            Eps1 = [#iset{anno=#a{anno=Anno},var=V,arg=NotMap}],
	    {V,Eps0++Eps1,St2}
    end.

badmap_term(_Map, #core{in_guard=true}) ->
    %% The code generator cannot handle complex error reasons
    %% in guards. But the exact error reason does not matter anyway
    %% since it is not user-visible.
    #c_literal{val=badmap};
badmap_term(Map, #core{in_guard=false}) ->
    c_tuple([#c_literal{val=badmap},Map]).

map_build_pairs(Map, Es0, Ann, St0) ->
    {Es,Pre,_,St1} = map_build_pairs_1(Es0, sets:new([{version, 2}]), St0),
    {ann_c_map(Ann, Map, Es),Pre,St1}.

map_build_pairs_1([{Op0,L,K0,V0}|Es], Used0, St0) ->
    {K,Pre0,St1} = safe(K0, St0),
    {V,Pre1,St2} = safe(V0, St1),
    {Pairs,Pre2,Used1,St3} = map_build_pairs_1(Es, Used0, St2),
    As = lineno_anno(L, St3),
    Op = map_op(Op0),
    {Used2,St4} = maybe_warn_repeated_keys(K, K0, Used1, St3),
    Pair = cerl:ann_c_map_pair(As, Op, K, V),
    {[Pair|Pairs],Pre0++Pre1++Pre2,Used2,St4};
map_build_pairs_1([], Used, St) ->
    {[],[],Used,St}.

maybe_warn_repeated_keys(Ck, K0, Used, St) ->
    case cerl:is_literal(Ck) of
        false -> {Used,St};
        true ->
            K = cerl:concrete(Ck),
            case sets:is_element(K,Used) of
                true ->
                    L = erl_parse:first_anno(K0),
                    {Used, add_warning(L, {map_key_repeated,K}, St)};
                false ->
                    {sets:add_element(K,Used), St}
            end
    end.

map_op(map_field_assoc) -> #c_literal{val=assoc};
map_op(map_field_exact) -> #c_literal{val=exact}.

%% try_exception([ExcpClause], St) -> {[ExcpVar],Handler,St}.

try_exception(Ecs0, St0) ->
    %% Note that Tag is not needed for rethrow - it is already in Info.
    {Evs,St1} = new_vars(3, St0), % Tag, Value, Info
    {Ecs1,St2} = clauses(Ecs0, St1),
    Ecs2 = try_build_stacktrace(Ecs1, hd(Evs)),
    [_,Value,Info] = Evs,
    LA = case Ecs2 of
	     [] -> [];
	     [C|_] -> get_lineno_anno(C)
	 end,
    Ec = #iclause{anno=#a{anno=[compiler_generated|LA]},
		  pats=[c_tuple(Evs)],guard=[#c_literal{val=true}],
		  body=[#iprimop{anno=#a{},       %Must have an #a{}
				 name=#c_literal{val=raise},
				 args=[Info,Value]}]},
    Hs = [#icase{anno=#a{anno=LA},args=[c_tuple(Evs)],clauses=Ecs2,fc=Ec}],
    {Evs,Hs,St2}.

try_after(Line, Es0, As0, St0) ->
    %% 'try ... after ... end'
    As1 = ta_sanitize_as(As0, Line),

    {Es, St1} = exprs(Es0, St0),
    {As, St2} = exprs(As1, St1),
    {V, St3} = new_var(St2),                    % (must not exist in As1)
    LineAnno = lineno_anno(Line, St3),

    case is_iexprs_small(As, 20) of
        true -> try_after_small(LineAnno, Es, As, V, St3);
        false -> try_after_large(LineAnno, Es, As, V, St3)
    end.

%% 'after' blocks don't have a result, so we match the last expression with '_'
%% to suppress false "unmatched return" warnings in tools that look at core
%% Erlang, such as `dialyzer`.
ta_sanitize_as([Expr], Line) ->
    [{match, Line, {var,Line,'_'}, Expr}];
ta_sanitize_as([Expr | Exprs], Line) ->
    [Expr | ta_sanitize_as(Exprs, Line)].

try_after_large(LA, Es, As, V, St0) ->
    %% Large 'after' block; break it out into a wrapper function to reduce
    %% code size.
    Lanno = #a{anno=LA},
    {Name, St1} = new_fun_name("after", St0),
    Fc = function_clause([], LA),
    Fun = #ifun{anno=Lanno,id=[],vars=[],
                clauses=[#iclause{anno=Lanno,pats=[],
                                  guard=[#c_literal{val=true}],
                                  body=As}],
                fc=Fc},
    App = #iapply{anno=#a{anno=[compiler_generated|LA]},
                  op=#c_var{anno=LA,name={Name,0}},
                  args=[]},
    {Evs, Hs, St} = after_block([App], St1),
    Try = #itry{anno=Lanno,
                args=Es,
                vars=[V],
                body=[App,V],
                evars=Evs,
                handler=Hs},
    Letrec = #iletrec{anno=Lanno,defs=[{{Name,0},Fun}],
                      body=[Try]},
    {Letrec, [], St}.

try_after_small(LA, Es, As, V, St0)  ->
    %% Small 'after' block; inline it.
    Lanno = #a{anno=LA},
    {Evs, Hs, St1} = after_block(As, St0),
    Try = #itry{anno=Lanno,args=Es,vars=[V],
                body=(As ++ [V]),
                evars=Evs,handler=Hs},
    {Try, [], St1}.

after_block(As, St0) ->
    %% See above.
    {Evs, St1} = new_vars(3, St0),              % Tag, Value, Info
    [_,Value,Info] = Evs,
    B = As ++ [#iprimop{anno=#a{},              % Must have an #a{}
                        name=#c_literal{val=raise},
                        args=[Info,Value]}],
    Ec = #iclause{anno=#a{anno=[compiler_generated]},
                  pats=[c_tuple(Evs)],guard=[#c_literal{val=true}],
                  body=B},
    Hs = [#icase{anno=#a{},args=[c_tuple(Evs)],clauses=[],fc=Ec}],
    {Evs, Hs, St1}.

try_build_stacktrace([#iclause{pats=Ps0,body=B0}=C0|Cs], RawStk) ->
    [#c_tuple{es=[Class,Exc,Stk]}=Tup] = Ps0,
    case Stk of
        #c_var{name='_'} ->
            %% Stacktrace variable is not used. Nothing to do.
            [C0|try_build_stacktrace(Cs, RawStk)];
        _ ->
            %% Add code to build the stacktrace.
            Ps = [Tup#c_tuple{es=[Class,Exc,RawStk]}],
            Call = #iprimop{anno=#a{},
                            name=#c_literal{val=build_stacktrace},
                            args=[RawStk]},
            Iset = #iset{var=Stk,arg=Call},
            B = [Iset|B0],
            C = C0#iclause{pats=Ps,body=B},
            [C|try_build_stacktrace(Cs, RawStk)]
    end;
try_build_stacktrace([], _) -> [].

%% is_iexprs_small([Exprs], Threshold) -> boolean().
%%  Determines whether a list of expressions is "smaller" than the given
%%  threshold. This is largely analogous to cerl_trees:size/1 but operates on
%%  our internal #iexprs{} and bails out as soon as the threshold is exceeded.
is_iexprs_small(Exprs, Threshold) ->
    0 < is_iexprs_small_1(Exprs, Threshold).

is_iexprs_small_1(_, 0) ->
    0;
is_iexprs_small_1([], Threshold) ->
    Threshold;
is_iexprs_small_1([Expr | Exprs], Threshold0) ->
    Threshold = is_iexprs_small_2(Expr, Threshold0 - 1),
    is_iexprs_small_1(Exprs, Threshold).

is_iexprs_small_2(#iclause{guard=Guards,body=Body}, Threshold0) ->
    Threshold = is_iexprs_small_1(Guards, Threshold0),
    is_iexprs_small_1(Body, Threshold);
is_iexprs_small_2(#itry{body=Body,handler=Handler}, Threshold0) ->
    Threshold = is_iexprs_small_1(Body, Threshold0),
    is_iexprs_small_1(Handler, Threshold);
is_iexprs_small_2(#imatch{guard=Guards}, Threshold) ->
    is_iexprs_small_1(Guards, Threshold);
is_iexprs_small_2(#icase{clauses=Clauses}, Threshold) ->
    is_iexprs_small_1(Clauses, Threshold);
is_iexprs_small_2(#ifun{clauses=Clauses}, Threshold) ->
    is_iexprs_small_1(Clauses, Threshold);
is_iexprs_small_2(#ireceive1{clauses=Clauses}, Threshold) ->
    is_iexprs_small_1(Clauses, Threshold);
is_iexprs_small_2(#ireceive2{clauses=Clauses}, Threshold) ->
    is_iexprs_small_1(Clauses, Threshold);
is_iexprs_small_2(#icatch{body=Body}, Threshold) ->
    is_iexprs_small_1(Body, Threshold);
is_iexprs_small_2(#iletrec{body=Body}, Threshold) ->
    is_iexprs_small_1(Body, Threshold);
is_iexprs_small_2(#iprotect{body=Body}, Threshold) ->
    is_iexprs_small_1(Body, Threshold);
is_iexprs_small_2(#iset{arg=Arg}, Threshold) ->
    is_iexprs_small_2(Arg, Threshold);
is_iexprs_small_2(_, Threshold) ->
    Threshold.

%% expr_bin([ArgExpr], St) -> {[Arg],[PreExpr],St}.
%%  Flatten the arguments of a bin. Do this straight left to right!
%%  Note that ibinary needs to have its annotation wrapped in a #a{}
%%  record whereas c_literal should not have a wrapped annotation

expr_bin(Es0, Anno, St0) ->
    Es1 = bin_elements(Es0, 1),
    case constant_bin(Es1) of
	error ->
            case expr_bin_1(Es1, St0) of
                {[],Eps,St} ->
                    EmptyBin = <<>>,
                    {#c_literal{anno=Anno,val=EmptyBin},Eps,St};
                {Es,Eps,St} ->
                    {#ibinary{anno=#a{anno=Anno},segments=Es},Eps,St}
            end;
	Bin ->
	    {#c_literal{anno=Anno,val=Bin},[],St0}
    end.

expr_bin_1(Es, St0) ->
    Res = foldr(fun (E, {Ces,Eps0,S0}) ->
                        try bitstr(E, S0) of
                            {Ce,Eps,S1} when is_list(Ces) ->
                                {Ce++Ces,Eps ++ Eps0,S1};
                            {_Ce,Eps,S1} ->
                                {Ces,Eps ++ Eps0,S1}
                        catch
                            {bad_binary,Eps,S1} ->
                                {bad_binary,Eps ++ Eps0,S1}
                        end
                end, {[],[],St0}, Es),
    case Res of
        {bad_binary,Eps,St} ->
            throw({bad_binary,Eps,St});
        {_,_,_}=Res ->
            Res
    end.

bitstrs([E0|Es0], St0) ->
    {E,Eps0,St1} = bitstr(E0, St0),
    {Es,Eps1,St2} = bitstrs(Es0, St1),
    {E++Es,Eps0++Eps1,St2};
bitstrs([], St) ->
    {[],[],St}.

bitstr({bin_element,{sl,_,Line},{string,_,S},{integer,_,8},_}, St) ->
    bitstrs(bin_expand_string(S, {sl,0,Line}, 0, 0, []), St);
bitstr({bin_element,{sl,_,Line},{string,_,[]},Sz0,Ts}, St0) ->
    %% Empty string. We must make sure that the type is correct.
    {[#c_bitstr{size=Sz}],Eps0,St1} =
        bitstr({bin_element,{sl,0,Line},{char,Line,0},Sz0,Ts}, St0),

    %% At this point, the type is either a correct literal or
    %% an expression.
    case Sz of
        #c_literal{val=undefined} ->
            %% One of the utf* types. The size is not used.
            {[],[],St1};
        #c_literal{val=Int} when is_integer(Int), Int >= 0 ->
            {[],[],St1};
        #c_var{} ->
            %% Must add a test to verify that the size expression is
            %% an integer >= 0.
            Erlang = {atom,Line,erlang},
            Test0 = {call,Line,{remote,Line,Erlang,{atom,Line,is_integer}},
                     [Sz0]},
            Test1 = {call,Line,{remote,Line,Erlang,{atom,Line,'>='}},
                     [Sz0,{integer,Line,0}]},
            Test2 = {op,Line,'andalso',Test0,Test1},
            Fail = {call,Line,{remote,Line,Erlang,{atom,Line,error}},
                    [{atom,Line,badarg}]},
            Test = {op,Line,'orelse',Test2,Fail},
            Match = {match,Line,{var,Line,'_'},Test},
            {_,Eps1,St2} = expr(Match, St1),
            Eps = Eps0 ++ Eps1,
            {[],Eps,St2}
    end;
bitstr({bin_element,{sl,_,Line},{string,_,S},Sz0,Ts}, St0) ->
    {[Bitstr],Eps,St1} = bitstr({bin_element,{sl,0,Line},{char,Line,0},Sz0,Ts}, St0),
    Es = [Bitstr#c_bitstr{val=#c_literal{anno=full_anno(Line, St1),val=C}} ||
             C <- S],
    {Es,Eps,St1};
bitstr({bin_element,{sl,Seg,Line},E0,Size0,[Type,{unit,Unit}|Flags]}, St0) ->
    {E1,Eps0,St1} = safe(E0, St0),
    {Size1,Eps1,St2} = safe(Size0, St1),
    Eps = Eps0 ++ Eps1,
    case {Type,E1} of
	{_,#c_var{}} -> ok;
	{integer,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf8,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf16,#c_literal{val=I}} when is_integer(I) -> ok;
	{utf32,#c_literal{val=I}} when is_integer(I) -> ok;
	{float,#c_literal{val=V}} when is_number(V) -> ok;
	{binary,#c_literal{val=V}} when is_bitstring(V) -> ok;
	{_,_} ->
            %% Note that the pre expressions may bind variables that
            %% are used later or have side effects.
	    throw({bad_binary,Eps,St2})
    end,
    case Size1 of
	#c_var{} -> ok;
	#c_literal{val=Sz} when is_integer(Sz), Sz >= 0 -> ok;
	#c_literal{val=undefined} -> ok;
	#c_literal{val=all} -> ok;
	_ -> throw({bad_binary,Eps,St2})
    end,
    Anno0 = lineno_anno(Line, St2),

    %% We will add a 'segment' annotation to segments that could
    %% fail. There is no need to add it to literal segments of fixed
    %% sized. The annotation will be used by the runtime system to
    %% provide extended error information if construction of the
    %% binary fails.
    Anno = if Seg =:= 0 ->
                   Anno0;
              true ->
                   [{segment,Seg}|Anno0]
           end,

    {[#c_bitstr{anno=Anno,
                val=E1,size=Size1,
                unit=#c_literal{val=Unit},
                type=#c_literal{val=Type},
                flags=#c_literal{val=Flags}}],
     Eps,St2}.

bin_elements([{bin_element,Line,Expr,Size0,Type0}|Es], Seg) ->
    {Size,Type} = make_bit_type(Line, Size0, Type0, construction),
    [{bin_element,{sl,Seg,Line},Expr,Size,Type}|bin_elements(Es, Seg+1)];
bin_elements([], _) -> [].

make_bit_type(Line, default, Type0, _Context) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {make_all_size(Line),erl_bits:as_list(Bt)};
	{ok,undefined,Bt} -> {{atom,Line,undefined},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(_Line, {atom,Anno,all}=Size, Type0, Context) ->
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    Type = erl_bits:as_list(Bt),
    case erl_anno:generated(Anno) of
        true ->
            %% This `all` was created by the compiler from a binary
            %% segment without a size.
            {Size,Type};
        false ->
            %% This `all` was present in the source code. It is not
            %% a valid size.
            case Context of
                matching ->
                    throw(nomatch);
                construction ->
                    {{atom,Anno,bad_size},Type}
            end
    end;
make_bit_type(_Line, Size0, Type0, _Context) ->
    {ok,Size1,Bt} = erl_bits:set_bit_type(Size0, Type0),
    Size = case Size1 of
               {char,Anno,CharVal} -> {integer,Anno,CharVal};
               _ -> Size1
           end,
    {Size,erl_bits:as_list(Bt)}.

make_all_size(Line) ->
    Anno = erl_anno:set_generated(true, Line),
    {atom,Anno,all}.

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
    EvalFun = fun({string,_,S}, B) -> {value,S,B};
		 ({integer,_,I}, B) -> {value,I,B};
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
	{{atom,_,undefined},{string,_,_}} ->
	    %% UTF-8/16/32.
	    ok;
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

count_bits(Int) when is_integer(Int) ->
    count_bits_1(abs(Int), 64).

count_bits_1(0, Bits) -> Bits;
count_bits_1(Int, Bits) -> count_bits_1(Int bsr 64, Bits+64).

bin_expand_string(S, Line, Val, Size, Last) when Size >= ?COLLAPSE_MAX_SIZE_SEGMENT ->
    Combined = make_combined(Line, Val, Size),
    [Combined|bin_expand_string(S, Line, 0, 0, Last)];
bin_expand_string([H|T], Line, Val, Size, Last) ->
    bin_expand_string(T, Line, (Val bsl 8) bor H, Size+8, Last);
bin_expand_string([], Line, Val, Size, Last) ->
    [make_combined(Line, Val, Size) | Last].

make_combined(SegLine, Val, Size) ->
    Line = case SegLine of
               {sl,_,Line0} -> Line0;
               _ -> SegLine
           end,
    {bin_element,SegLine,{integer,Line,Val},
     {integer,Line,Size},
     [integer,{unit,1},unsigned,big]}.

%% fun_tq(Id, [Clauses], Line, State, NameInfo) -> {Fun,[PreExp],State}.

fun_tq(Cs0, L, St0, NameInfo) ->
    Arity = clause_arity(hd(Cs0)),
    {Cs1,St1} = clauses(Cs0, St0),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Anno = full_anno(L, St3),
    {Name,St4} = new_fun_name(St3),
    Fc = function_clause(Ps, Anno),
    Id = {0,0,Name},
    Fun = #ifun{anno=#a{anno=Anno},
		id=[{id,Id}],				%We KNOW!
		vars=Args,clauses=Cs1,fc=Fc,name=NameInfo},
    {Fun,[],St4}.

%% lc_tq(Line, Exp, [Qualifier], Mc, State) -> {LetRec,[PreExp],State}.
%%  This TQ from Simon PJ pp 127-138.

lc_tq(Line, E, [#igen{anno=#a{anno=GA}=GAnno,
		      acc_pat=AccPat,acc_guard=AccGuard,
                      skip_pat=SkipPat,tail=Tail,tail_pat=TailPat,
                      refill={RefillPat,RefillAction},
                      arg={Pre,Arg}}|Qs], Mc, St0) ->
    {Name,St1} = new_fun_name("lc", St0),
    LA = lineno_anno(Line, St1),
    F = #c_var{anno=LA,name={Name,1}},
    Nc = #iapply{anno=GAnno,op=F,args=[Tail]},
    {[FcVar,Var],St2} = new_vars(2, St1),
    Fc = bad_generator([FcVar], FcVar, Arg),
    SkipClause = make_clause([skip_clause,compiler_generated|LA],
                             SkipPat, [], [], [Nc]),
    TailClause = make_clause(LA, TailPat, [], [], [Mc]),
    {Lc,Lps,St3} = lc_tq(Line, E, Qs, Nc, St2),
    AccClause = make_clause(LA, AccPat, [], AccGuard, Lps ++ [Lc]),
    RefillClause = make_clause(LA, RefillPat, [], [], [RefillAction,Nc]),
    Cs0 = [AccClause,SkipClause,TailClause,RefillClause],
    Cs = [C || C <- Cs0, C =/= nomatch],
    Fun = #ifun{anno=GAnno,id=[],vars=[Var],clauses=Cs,fc=Fc},
    {#iletrec{anno=GAnno#a{anno=[list_comprehension|GA]},defs=[{{Name,1},Fun}],
              body=Pre ++ [#iapply{anno=GAnno,op=F,args=[Arg]}]},
     [],St3};
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
    {Qs1,St2} = preprocess_quals(Line, Qs0, St1),
    {PrePre,Qs} = case Qs1 of
                      [#igen{arg={IgenPre,Arg}}=Igen|Igens] ->
                          {IgenPre,[Igen#igen{arg={[],Arg}}|Igens]};
                      _ ->
                          {[],Qs1}
                  end,
    {E,BcPre,St} = bc_tq1(Line, Exp, Qs, BinVar, St2),
    InitialSize = #c_literal{val=256},
    Pre = PrePre ++
        [#iset{var=BinVar,
               arg=#iprimop{anno=#a{anno=lineno_anno(Line, St)},
                            name=#c_literal{val=bs_init_writable},
                            args=[InitialSize]}}] ++ BcPre,
    {E,Pre,St}.

bc_tq1(Line, E, [#igen{anno=GAnno,
		       acc_pat=AccPat,acc_guard=AccGuard,
                       skip_pat=SkipPat,tail=Tail,tail_pat=TailPat,
                       refill={RefillPat,RefillAction},
                       arg={Pre,Arg}}|Qs], Mc, St0) ->
    {Name,St1} = new_fun_name("lbc", St0),
    LA = lineno_anno(Line, St1),
    LAnno = #a{anno=LA},
    {[_,AccVar]=Vars,St2} = new_vars(LA, 2, St1),
    {[_,_]=FcVars,St3} = new_vars(LA, 2, St2),
    {IgnoreVar,St4} = new_var(LA, St3),
    F = #c_var{anno=LA,name={Name,2}},
    Nc = #iapply{anno=GAnno,op=F,args=[Tail,AccVar]},
    Fc = bad_generator(FcVars, hd(FcVars), Arg),
    SkipClause = make_clause([compiler_generated,skip_clause|LA],
                             SkipPat, [IgnoreVar], [], [Nc]),
    TailClause = make_clause(LA, TailPat, [IgnoreVar], [], [AccVar]),
    {Bc,Bps,St5} = bc_tq1(Line, E, Qs, AccVar, St4),
    Body = Bps ++ [#iset{var=AccVar,arg=Bc},Nc],
    AccClause = make_clause(LA, AccPat, [IgnoreVar], AccGuard, Body),
    RefillClause = make_clause(LA, RefillPat, [AccVar], [], [RefillAction,Nc]),
    Cs0 = [AccClause,SkipClause,TailClause,RefillClause],
    Cs = [C || C <- Cs0, C =/= nomatch],
    Fun = #ifun{anno=GAnno,id=[],vars=Vars,clauses=Cs,fc=Fc},

    %% Inlining would disable the size calculation optimization for
    %% bs_init_writable.
    {#iletrec{anno=LAnno#a{anno=[list_comprehension,no_inline|LA]},
              defs=[{{Name,2},Fun}],
              body=Pre ++ [#iapply{anno=LAnno,op=F,args=[Arg,Mc]}]},
     [],St5};
bc_tq1(Line, E, [#ifilter{}=Filter|Qs], Mc, St) ->
    filter_tq(Line, E, Filter, Mc, St, Qs, fun bc_tq1/5);
bc_tq1(_, {bin,Bl,Elements}, [], AccVar, St0) ->
    bc_tq_build(Bl, [], AccVar, Elements, St0);
bc_tq1(Line, E0, [], AccVar, St0) ->
    BsFlags = [binary,{unit,1}],
    BsSize = make_all_size(Line),
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
    Elements = [{bin_element,Line,{var,Line,AccVar},make_all_size(Line),
		 [binary,{unit,1}]}|Elements0],
    {E,Pre,St} = expr({bin,Line,Elements}, St0),
    #a{anno=A} = Anno0 = get_anno(E),
    Anno = Anno0#a{anno=[compiler_generated,single_use|A]},
    {set_anno(E, Anno),Pre0++Pre,St}.

mc_tq(Line, {map_field_assoc,Lf,K,V}, Qs, Mc, St0) ->
    E = {tuple,Lf,[K,V]},
    {Lc,Pre0,St1} = lc_tq(Line, E, Qs, Mc, St0),
    {LcVar,St2} = new_var(St1),
    Pre = Pre0 ++ [#iset{var=LcVar,arg=Lc}],
    Call = #icall{module=#c_literal{val=maps},
                  name=#c_literal{val=from_list},
                  args=[LcVar]},
    {Call,Pre,St2}.

make_clause(_Anno, nomatch, _PatExtra, _Guard, _Body) ->
    nomatch;
make_clause(Anno, Pat, PatExtra, Guard, Body) ->
    #iclause{anno=#a{anno=Anno},pats=[Pat|PatExtra],guard=Guard,body=Body}.

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
                     c_tuple([#c_literal{val=bad_filter},FailPat])),
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
is_generator({m_generate,_,_,_}) -> true;
is_generator(_) -> false.

%% Retrieve the annotation from an Erlang AST form.
%% (Use get_anno/1 to retrieve the annotation from Core Erlang forms).

get_qual_anno(Abstract) -> element(2, Abstract).

%%
%% Generators are abstracted as a record #igen{}:
%%  - acc_pat is the accumulator pattern, e.g. [Pat|Tail] for Pat <- Expr.
%%  - acc_guard is the list of guards immediately following the current
%%    generator in the qualifier list input.
%%  - skip_pat is the skip pattern, e.g. <<X,_:X,Tail/bitstring>> for
%%    <<X,1:X>> <= Expr.
%%  - tail is the variable used in AccPat and SkipPat bound to the rest of the
%%    generator input.
%%  - tail_pat is the tail pattern, respectively [] and <<_/bitstring>> for list
%%    and bit string generators.
%%  - refill is a pair {RefillPat,RefillAction}, used to refill the iterator
%%    argument (used by map generators).
%%  - arg is a pair {Pre,Arg} where Pre is the list of expressions to be
%%    inserted before the comprehension function and Arg is the expression
%%    that it should be passed.
%%

%% generator(Line, Generator, Guard, State) -> {Generator',State}.
%%  Transform a given generator into its #igen{} representation.

generator(Line, {generate,Lg,P0,E}, Gs, St0) ->
    LA = lineno_anno(Line, St0),
    GA = lineno_anno(Lg, St0),
    {Head,St1} = list_gen_pattern(P0, Line, St0),
    {[Tail,Skip],St2} = new_vars(2, St1),
    {Cg,St3} = lc_guard_tests(Gs, St2),
    AccPat = case Head of
                 nomatch ->
                     nomatch;
                 _ ->
                     ann_c_cons(LA, Head, Tail)
             end,
    SkipPat = ann_c_cons(LA, Skip, Tail),
    {Ce,Pre,St4} = safe(E, St3),
    Gen = #igen{anno=#a{anno=GA},
		acc_pat=AccPat,acc_guard=Cg,skip_pat=SkipPat,
                tail=Tail,tail_pat=#c_literal{anno=LA,val=[]},arg={Pre,Ce}},
    {Gen,St4};
generator(Line, {b_generate,Lg,P,E}, Gs, St0) ->
    LA = lineno_anno(Line, St0),
    GA = lineno_anno(Lg, St0),
    try pattern(P, St0) of
        {#ibinary{segments=Segs}=Cp,St1} ->
            %% The function append_tail_segment/2 keeps variable
            %% patterns as-is, making it possible to have the same
            %% skip clause removal as with list generators.
            {AccSegs,Tail,TailSeg,St2} = append_tail_segment(Segs, St1),
            AccPat = Cp#ibinary{segments=AccSegs},
            {Cg,St3} = lc_guard_tests(Gs, St2),
            {SkipSegs,St4} = skip_segments(AccSegs, St3, []),
            SkipPat = Cp#ibinary{segments=SkipSegs},
            {Ce,Pre,St5} = safe(E, St4),
            Gen = #igen{anno=#a{anno=GA},acc_pat=AccPat,acc_guard=Cg,
                        skip_pat=SkipPat,tail=Tail,
                        tail_pat=#ibinary{anno=#a{anno=LA},segments=[TailSeg]},
                        arg={Pre,Ce}},
            {Gen,St5}
    catch
        throw:nomatch ->
            {Ce,Pre,St1} = safe(E, St0),
            Gen = #igen{anno=#a{anno=GA},acc_pat=nomatch,acc_guard=[],
                        skip_pat=nomatch,
                        tail_pat=#c_var{name='_'},
                        arg={Pre,Ce}},
            {Gen,St1}
    end;
generator(Line, {m_generate,Lg,{map_field_exact,_,K0,V0},E}, Gs, St0) ->
    %% Consider this example:
    %%
    %%   [{K,V} || K := V <- L].
    %%
    %% The following Core Erlang code will be generated:
    %%
    %% letrec
    %%     'lc$^0'/1 =
    %%         fun (Iter0) ->
    %%             case Iter0 of
    %%               <{K,V,NextIter}> when 'true' ->
    %%                   let <Tail> =
    %%                       apply 'lc$^0'/1(NextIter)
    %%                   in [{K,V}|Tail]
    %%               <{_K,_V,NextIter}> when 'true' ->
    %%                   %% Skip clause; will be optimized away later
    %%                   %% since there are no filters.
    %%                   apply 'lc$^0'/1(NextIter)
    %%               <'none'> when 'true' ->
    %%                   []
    %%               <Iter> when 'true' ->
    %%                   let NextIter =
    %%                       call 'erts_internal':'mc_refill'(Iter)
    %%                   in apply 'lc$^0'/1(NextIter)
    %%               <Bad> when 'true' ->
    %%                     %% Generated by lc_tq/5. Never reached;
    %%                     %% will be optimized away.
    %%                     call 'erlang':'error'({'bad_generator',Bad})
    %%             end
    %% in let <Iter> =
    %%         case call 'erts_internal':'mc_iterator'(L) of
    %%           <[]> when 'true' ->
    %%                 call 'erlang':'error'
    %%                     ({'bad_generator',L})
    %%           <Iter0> when 'true' ->
    %%               Iter0
    %%         end
    %%     in apply 'lc$^0'/1(Iter0)
    LA = lineno_anno(Line, St0),
    GA = lineno_anno(Lg, St0),
    {Pat,St1} = list_gen_pattern({cons,Lg,K0,V0}, Line, St0),
    {[SkipK,SkipV,IterVar,OuterIterVar,_BadGenVar],St2} = new_vars(5, St1),
    {Cg,St3} = lc_guard_tests(Gs, St2),
    {Ce,Pre0,St4} = safe(E, St3),
    AccPat = case Pat of
                 nomatch ->
                     nomatch;
                 _ ->
                     K = cons_hd(Pat),
                     V = cons_tl(Pat),
                     #c_tuple{es=[K,V,IterVar]}
             end,
    SkipPat = #c_tuple{es=[SkipK,SkipV,IterVar]},

    Refill = {SkipK,
              #iset{var=IterVar,
                    arg=#icall{anno=#a{anno=GA},
                               module=#c_literal{val=erts_internal},
                               name=#c_literal{val=mc_refill},
                               args=[SkipK]}}},

    InitIter = #icall{anno=#a{anno=GA},
                      module=#c_literal{val=erts_internal},
                      name=#c_literal{val=mc_iterator},
                      args=[Ce]},

    BadGenerator = bad_generator([#c_literal{val=[]}], Ce,
                                 #c_literal{val=[],anno=GA}),
    BeforeFc = #iclause{anno=#a{anno=GA},
                        pats=[IterVar],
                        guard=[],
                        body=[IterVar]},
    Before = #iset{var=OuterIterVar,
                   arg=#icase{args=[InitIter],
                              clauses=[BadGenerator],
                              fc=BeforeFc}},

    Pre = Pre0 ++ [Before],
    Gen = #igen{anno=#a{anno=GA},
		acc_pat=AccPat,acc_guard=Cg,skip_pat=SkipPat,
                tail=IterVar,tail_pat=#c_literal{anno=LA,val=none},
                refill=Refill,
                arg={Pre,OuterIterVar}},
    {Gen,St4}.

append_tail_segment(Segs, St0) ->
    {Var,St} = new_var(St0),
    Tail = #ibitstr{val=Var,size=[#c_literal{val=all}],
                    unit=#c_literal{val=1},
                    type=#c_literal{val=binary},
                    flags=#c_literal{val=[unsigned,big]}},
    {Segs++[Tail],Var,Tail,St}.

%% skip_segments(Segments, St0, Acc) -> {SkipSegments,St}.
%%  Generate the segments for a binary pattern that can be used
%%  in the skip clause that will continue the iteration when
%%  the accumulator pattern didn't match.

skip_segments([#ibitstr{val=#c_var{}}=B|Rest], St, Acc) ->
    %% We must keep the names of existing variables to ensure that
    %% patterns such as <<Size,X:Size>> will work.
    skip_segments(Rest, St, [B|Acc]);
skip_segments([B|Rest], St0, Acc) ->
    %% Replace literal or expression with a variable (whose value will
    %% be ignored).
    {Var,St1} = new_var(St0),
    skip_segments(Rest, St1, [B#ibitstr{val=Var}|Acc]);
skip_segments([], St, Acc) ->
    {reverse(Acc),St}.

lc_guard_tests([], St) -> {[],St};
lc_guard_tests(Gs0, St0) ->
    Gs1 = guard_tests(Gs0),
    {Gs,St} = gexpr_top(Gs1, St0#core{in_guard=true}),
    {Gs,St#core{in_guard=false}}.

list_gen_pattern(P0, Line, St) ->
    try
	pattern(P0, St)
    catch
	nomatch -> {nomatch,add_warning(Line, {nomatch,pattern}, St)}
    end.

%% is_guard_test(Expression) -> true | false.
%%  Test if a general expression is a guard test.
%%
%%  Note that a local function overrides a BIF with the same name.
%%  For example, if there is a local function named is_list/1,
%%  any unqualified call to is_list/1 will be to the local function.
%%  The guard function must be explicitly called as erlang:is_list/1.

is_guard_test(E) ->
    %% erl_expand_records has added a module prefix to any call
    %% to a BIF or imported function. Any call without a module
    %% prefix that remains must therefore be to a local function.
    IsOverridden = fun({_,_}) -> true end,
    erl_lint:is_guard_test(E, [], IsOverridden).

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


%% safe_list(Expr, State) -> {Safe,[PreExpr],State}.
%%  Generate an internal safe expression for a list of
%%  expressions.

safe_list(Es, St0) ->
    {Vs,Eps0,St} =
        foldr(fun (E, {Ces,Eps,Sti0}) ->
                      {Ce,Ep,Sti1} = safe(E, Sti0),
                      case Eps of
                          [[#iexprs{bodies=Bs}]|T] ->
                              %% A cons within a cons.
                              {[Ce|Ces],[Ep|Bs]++T,Sti1};
                          _ ->
                              {[Ce|Ces],[Ep|Eps],Sti1}
                      end
              end, {[],[],St0}, Es),
    case [Ep || [_|_]=Ep <- Eps0] of
        [] ->
            {Vs,[],St};
        [Ep] ->
            {Vs,Ep,St};
        [_|_]=Eps ->
            %% Two or more bodies. They see the same variables.
            {Vs,[#iexprs{bodies=Eps}],St}
    end.

%% safe(Expr, State) -> {Safe,[PreExpr],State}.
%%  Generate an internal safe expression.  These are simples without
%%  binaries which can fail.  At this level we do not need to do a
%%  deep check.  Must do special things with matches here.

safe(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_safe(E1, St1),
    {Se,Eps ++ Sps,St2}.

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
	    {V,St1} = new_var(get_lineno_anno(Ce), St0),
	    {V,[#iset{var=V,arg=Ce}],St1}
    end.

is_safe(#c_cons{}) -> true;
is_safe(#c_tuple{}) -> true;
is_safe(#c_var{name={_,_}}) -> false;           %Fun. Not safe.
is_safe(#c_var{name=_}) -> true;                %Ordinary variable.
is_safe(#c_literal{}) -> true;
is_safe(_) -> false.

%% fold_match(MatchExpr, Pat) -> {MatchPat,Expr}.
%%  Fold nested matches into one match with aliased patterns.

fold_match({match, L, P, E}, E0) ->
    fold_match(E, {sequential_match, L, P, E0});
fold_match(E, E0) ->
    {E0, E}.

%% pattern(Pattern, State) -> {CorePat,[PreExp],State}.
%%  Transform a pattern by removing line numbers.  We also normalise
%%  aliases in patterns to standard form: {alias,Pat,[Var]}.

pattern({var,L,V}, St) -> {#c_var{anno=lineno_anno(L, St),name=V},St};
pattern({char,L,C}, St) -> {#c_literal{anno=lineno_anno(L, St),val=C},St};
pattern({integer,L,I}, St) -> {#c_literal{anno=lineno_anno(L, St),val=I},St};
pattern({float,L,F}, St) -> {#c_literal{anno=lineno_anno(L, St),val=F},St};
pattern({atom,L,A}, St) -> {#c_literal{anno=lineno_anno(L, St),val=A},St};
pattern({string,L,S}, St) -> {#c_literal{anno=lineno_anno(L, St),val=S},St};
pattern({nil,L}, St) -> {#c_literal{anno=lineno_anno(L, St),val=[]},St};
pattern({cons,L,H,T}, St) ->
    {Ph,St1} = pattern(H, St),
    {Pt,St2} = pattern(T, St1),
    {annotate_cons(lineno_anno(L, St), Ph, Pt, St2),St2};
pattern({tuple,L,Ps}, St) ->
    {Ps1,St1} = pattern_list(Ps, St),
    {annotate_tuple(record_anno(L, St), Ps1, St),St1};
pattern({map,L,Pairs}, St0) ->
    {Ps,St1} = pattern_map_pairs(Pairs, St0),
    {#imap{anno=#a{anno=lineno_anno(L, St1)},es=Ps},St1};
pattern({bin,L,Ps}, St0) ->
    {Segments,St} = pat_bin(Ps, St0),
    {#ibinary{anno=#a{anno=lineno_anno(L, St)},segments=Segments},St};
pattern({match,_,P1,P2}, St) ->
    %% Handle aliased patterns in a clause. Example:
    %%
    %%      f({a,b} = {A,B}) -> . . .
    %%
    %% The `=` operator does not have any defined order in which the
    %% two patterns are matched. Therefore, this example can safely be
    %% rewritten like so:
    %%
    %%      f({a=A,b=B}) -> . . .
    %%
    %% Aliased patterns that are illegal, such as:
    %%
    %%      f(#{Key := Value} = {key := Key}) -> . . .
    %%
    %% have already been rejected by erl_lint.
    %%
    {Cp1,St1} = pattern(P1, St),
    {Cp2,St2} = pattern(P2, St1),
    {pat_alias(Cp1, Cp2),St2};
%% Evaluate compile-time expressions.
pattern({op,_,'++',{nil,_},R}, St) ->
    pattern(R, St);
pattern({op,_,'++',{cons,Li,H,T},R}, St) ->
    pattern({cons,Li,H,{op,Li,'++',T,R}}, St);
pattern({op,_,'++',{string,Li,L},R}, St) ->
    pattern(string_to_conses(Li, L, R), St);
pattern({op,_Line,_Op,_A}=Op, St) ->
    pattern(erl_eval:partial_eval(Op), St);
pattern({op,_Line,_Op,_L,_R}=Op, St) ->
    pattern(erl_eval:partial_eval(Op), St).

%% pattern_map_pairs([MapFieldExact],State) -> [#c_map_pairs{}]
pattern_map_pairs(Ps, St0) ->
    {CMapPairs,St1} = mapfoldl(fun pattern_map_pair/2, St0, Ps),
    {pat_alias_map_pairs(CMapPairs),St1}.

pattern_map_pair({map_field_exact,L,K,V}, St0) ->
    Ck0 = erl_eval:partial_eval(K),
    {Ck,St1} = exprs([Ck0], St0),
    {Cv,St2} = pattern(V, St1),
    {#imappair{anno=#a{anno=lineno_anno(L, St2)},
               op=#c_literal{val=exact},
               key=Ck,
               val=Cv},St2}.

pat_alias_map_pairs(Ps) ->
    D0 = foldl(fun(#imappair{key=K0}=Pair, A) ->
                       K = map_sort_key(K0, A),
                       case A of
                           #{K:=Aliases} ->
                               A#{K:=[Pair|Aliases]};
                           #{} ->
                               A#{K=>[Pair]}
                       end
               end, #{}, Ps),
    %% We must sort to ensure that the order remains consistent
    %% between compilations.
    D = sort(maps:to_list(D0)),
    pat_alias_map_pairs_1(D).

pat_alias_map_pairs_1([{_,[#imappair{val=V0}=Pair|Vs]}|T]) ->
    V = foldl(fun(#imappair{val=V}, Pat) ->
		      pat_alias(V, Pat)
	      end, V0, Vs),
    [Pair#imappair{val=V}|pat_alias_map_pairs_1(T)];
pat_alias_map_pairs_1([]) -> [].

map_sort_key(Key, KeyMap) ->
    case Key of
        [#c_literal{}=Lit] ->
            {atomic,cerl:set_ann(Lit, [])};
        [#c_var{}=Var] ->
            {atomic,cerl:set_ann(Var, [])};
        _ ->
            {expr,map_size(KeyMap)}
    end.

%% pat_bin([BinElement], State) -> [BinSeg].

pat_bin(Ps0, St) ->
    Ps = pat_bin_expand_strings(Ps0, St),
    pat_segments(Ps, St).

pat_bin_expand_strings(Es0, #core{dialyzer=Dialyzer}) ->
    foldr(fun ({bin_element,Line,{string,_,[_|_]=S},default,default}, Es1)
                when not Dialyzer ->
                  bin_expand_string(S, Line, 0, 0, Es1);
              ({bin_element,Line,{string,_,S},Sz,Ts}, Es1) ->
                  foldr(
                    fun (C, Es) ->
                            [{bin_element,Line,{char,Line,C},Sz,Ts}|Es]
                    end, Es1, S);
              (E, Es) ->
                  [E|Es]
	  end, [], Es0).

pat_segments([P0|Ps0], St0) ->
    {P,St1} = pat_segment(P0, St0),
    {Ps,St2} = pat_segments(Ps0, St1),
    {[P|Ps],St2};
pat_segments([], St) -> {[],St}.

pat_segment({bin_element,L,Val,Size0,Type0}, St) ->
    {Size1,Type1} = make_bit_type(L, Size0, Type0, matching),
    [Type,{unit,Unit}|Flags] = Type1,
    Anno = lineno_anno(L, St),
    {Pval0,St1} = pattern(Val, St),
    Pval = coerce_to_float(Pval0, Type),
    Size = erl_eval:partial_eval(Size1),
    {Psize,St2} = exprs([Size], St1),
    {#ibitstr{anno=#a{anno=Anno},
              val=Pval,size=Psize,
              unit=#c_literal{val=Unit},
              type=#c_literal{val=Type},
              flags=#c_literal{val=Flags}},St2}.

coerce_to_float(#c_literal{val=Int}=E, float) when is_integer(Int) ->
    try
	E#c_literal{val=float(Int)}
    catch
        error:badarg -> E
    end;
coerce_to_float(E, _) -> E.

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

pat_alias(#imap{es=Es1}=M, #imap{es=Es2}) ->
    M#imap{es=pat_alias_map_pairs(Es1 ++ Es2)};

pat_alias(P1, #c_var{}=Var) ->
    #c_alias{var=Var,pat=P1};
pat_alias(P1, #c_alias{pat=P2}=Alias) ->
    Alias#c_alias{pat=pat_alias(P1, P2)};

pat_alias(#ibinary{segments=[]}=P, #ibinary{segments=[]}) ->
    P;
pat_alias(#ibinary{segments=[_|_]=Segs1}=P, #ibinary{segments=[S0|Segs2]}) ->
    %% Handle aliases of binary patterns in a clause. Example:
    %%     f(<<A:8,B:8>> = <<C:16>>) -> . . .
    #ibitstr{anno=#a{anno=Anno}=A} = S0,
    S = S0#ibitstr{anno=A#a{anno=[sequential_match|Anno]}},
    P#ibinary{segments=Segs1++[S|Segs2]};
pat_alias(#ibinary{segments=[S0|Segs1]}=P, #ibinary{segments=[]}) ->
    %% Example: f(<<_:0>> == <>>) -> . . .
    #ibitstr{anno=#a{anno=Anno}=A} = S0,
    S = S0#ibitstr{anno=A#a{anno=[sequential_match|Anno]}},
    P#ibinary{segments=[S|Segs1]};

pat_alias(P1, P2) ->
    %% The only legal patterns that remain are data patterns.
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
    {P1,St1} = pattern(P0, St0),
    {Ps1,St2} = pattern_list(Ps0, St1),
    {[P1|Ps1],St2};
pattern_list([], St) ->
    {[],St}.

string_to_conses(Line, Cs, Tail) ->
    foldr(fun (C, T) -> {cons,Line,{char,Line,C},T} end, Tail, Cs).

%% make_vars([Name]) -> [{Var,Name}].

make_vars(Vs) -> [ #c_var{name=V} || V <- Vs ].

new_fun_name(#core{function={F,A},fcount=I}=St) when is_integer(I) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
        ++ "-fun-" ++ integer_to_list(I) ++ "-",
    {list_to_atom(Name),St#core{fcount=I+1}}.

%% new_fun_name(Type, State) -> {FunName,State}.

new_fun_name(Type, #core{fcount=C}=St) when is_integer(C) ->
    {list_to_atom(Type ++ "$^" ++ integer_to_list(C)),St#core{fcount=C+1}}.

%% new_var_name(State) -> {VarName,State}.

new_var_name(#core{vcount=C}=St) when is_integer(C) ->
    {C,St#core{vcount=C + 1}}.

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

bad_generator(Ps, Generator, Arg) ->
    Anno = get_anno(Arg),
    Tuple = ann_c_tuple(Anno, [#c_literal{val=bad_generator},Generator]),
    Call = #icall{anno=#a{anno=Anno},           %Must have an #a{}
                  module=#c_literal{anno=Anno,val=erlang},
                  name=#c_literal{anno=Anno,val=error},
                  args=[Tuple]},
    #iclause{anno=#a{anno=[compiler_generated]},
             pats=Ps,guard=[],body=[Call]}.

function_clause(Ps, LineAnno) ->
    fail_clause(Ps, LineAnno,
		ann_c_tuple(LineAnno, [#c_literal{val=function_clause}|Ps])).

fail_clause(Pats, Anno, Arg) ->
    #iclause{anno=#a{anno=[compiler_generated]},
	     pats=Pats,guard=[],
	     body=[#iprimop{anno=#a{anno=Anno},name=#c_literal{val=match_fail},
			    args=[Arg]}]}.

%% Optimization for Dialyzer.
right_assoc({op,L1,Op,{op,L2,Op,E1,E2},E3}, Op) ->
    right_assoc({op,L2,Op,E1,{op,L1,Op,E2,E3}}, Op);
right_assoc(E, _Op) -> E.

annotate_tuple(A, Es, #core{dialyzer=Dialyzer}) ->
    case Dialyzer of
        true ->
            %% Do not coalesce constant tuple elements. A Hack.
            Node = cerl:ann_c_tuple(A, [cerl:c_var(any)]),
            cerl:update_c_tuple_skel(Node, Es);
        false ->
            ann_c_tuple(A, Es)
    end.

annotate_cons(A, H, T, #core{dialyzer=Dialyzer}) ->
    case Dialyzer of
        true ->
            %% Do not coalesce constant conses. A Hack.
            Node= cerl:ann_c_cons(A, cerl:c_var(any), cerl:c_var(any)),
            cerl:update_c_cons_skel(Node, H, T);
        false ->
            ann_c_cons(A, H, T)
    end.

%%%
%%% Here follows an abstract data structure to help us handle Erlang's
%%% implicit matching that occurs when a variable is bound more than
%%% once:
%%%
%%%     X = Expr1(),
%%%     X = Expr2()
%%%
%%% What is implicit in Erlang, must be explicit in Core Erlang; that
%%% is, repeated variables must be eliminated and explicit matching
%%% must be added. For simplicity, examples that follow will be given
%%% in Erlang and not in Core Erlang. Here is how the example can be
%%% rewritten in Erlang to eliminate the repeated variable:
%%%
%%%     X = Expr1(),
%%%     X1 = Expr2(),
%%%     if
%%%         X1 =:= X -> X;
%%%         true -> error({badmatch,X1})
%%%     end
%%%
%%% To implement the renaming, keeping a set of the variables that
%%% have been bound so far is **almost** sufficient. When a variable
%%% in the set is bound a again, it will be renamed and a `case` with
%%% guard test will be added.
%%%
%%% Here is another example:
%%%
%%%     (X=A) + (X=B)
%%%
%%% Note that the operands for a binary operands are allowed to be
%%% evaluated in any order. Therefore, variables bound on the left
%%% hand side must not referenced on the right hand side, and vice
%%% versa. If a variable is bound on both sides, it must be bound
%%% to the same value.
%%%
%%% Using the simple scheme of keeping track of known variables,
%%% the example can be rewritten like this:
%%%
%%%     X = A,
%%%     X1 = B,
%%%     if
%%%         X1 =:= X -> ok;
%%%         true -> error({badmatch,X1})
%%%     end,
%%%     X + X1
%%%
%%% However, this simple scheme of keeping all previously bound variables in
%%% a set breaks down for this example:
%%%
%%%     (X=A) + fun() -> X = B end()
%%%
%%% The rewritten code would be:
%%%
%%%     X = A,
%%%     Tmp = fun() ->
%%%               X1 = B,
%%%               if
%%%                   X1 =:= X -> ok;
%%%                   true -> error({badmatch,X1})
%%%               end
%%%           end(),
%%%     X + Tmp
%%%
%%% That is wrong, because the binding of `X` created on the left hand
%%% side of `+` must not be seen inside the fun. The correct rewrite
%%% would be like this:
%%%
%%%     X = A,
%%%     Tmp = fun() ->
%%%               X1 = B
%%%           end(),
%%%     X + Tmp
%%%
%%% To correctly rewrite fun bodies, we will need to keep addtional
%%% information in a record so that we can remove `X` from the known
%%% variables when rewriting the body of the fun.
%%%

-record(known, {base=[],ks=[],prev_ks=[]}).

known_init() ->
    #known{}.

%% known_get(#known{}) -> [KnownVar].
%%  Get the currently known variables.

known_get(#known{ks=Ks}) ->
    Ks.

%% known_start_group(#known{}) -> #known{}.

known_start_group(#known{base=OldBase,ks=Ks,prev_ks=PrevKs}=K) ->
    K#known{base=[Ks|OldBase],prev_ks=[[]|PrevKs]}.

%% known_end_body(#known{}) -> #known{}.

known_end_body(#known{ks=Ks,prev_ks=[_|OldPrevKs]}=K) ->
    K#known{prev_ks=[Ks|OldPrevKs]}.

%% known_end_group(#known{}) -> #known{}.
%%  Consolidate the known variables after having processed the
%%  last body in a group of bodies that see the same bindings.

known_end_group(#known{base=[_|OldBase],prev_ks=[_|OldPrevKs]}=K) ->
    K#known{base=OldBase,prev_ks=OldPrevKs}.

%% known_union(#known{}, KnownVarsSet) -> #known{}.
%%  Update the known variables to be the union of the previous
%%  known variables and the set KnownVarsSet.

known_union(#known{ks=Ks}=K, Set) ->
    K#known{ks=union(Ks, Set)}.

%% known_bind(#known{}, BoundVarsSet) -> #known{}.
%%  Add variables that are known to be bound in the current
%%  body.

known_bind(#known{prev_ks=[PrevKs0|OldPrevKs]}=K, BoundVs) ->
    PrevKs = subtract(PrevKs0, BoundVs),
    K#known{prev_ks=[PrevKs|OldPrevKs]};
known_bind(#known{}=K, _) -> K.

%% known_in_fun(#known{}) -> #known{}.
%%  Update the known variables to only the set of variables that
%%  should be known when entering the fun.

known_in_fun(#known{base=[BaseKs|_],ks=Ks0,prev_ks=[PrevKs|_]}=K, Name) ->
    %% Within a group of bodies that see the same bindings, calculate
    %% the known variables for a fun. Example:
    %%
    %%     A = 1,
    %%     {X = 2, fun() -> X = 99, A = 1 end()}.
    %%
    %% In this example:
    %%
    %%     BaseKs = ['A'], Ks0 = ['A','X'], PrevKs = ['A','X']
    %%
    %% Thus, only `A` is known when entering the fun.

    Ks1 = union(BaseKs, subtract(Ks0, PrevKs)),
    Ks = case Name of
             unnamed -> Ks1;
             {named,FName} -> union(Ks1, [FName])
         end,
    K#known{base=[],ks=Ks,prev_ks=[]};
known_in_fun(#known{ks=Ks0}=K, Name) ->
    case Name of
        unnamed ->
            K;
        {named,FName} ->
            Ks = union(Ks0, [FName]),
            K#known{ks=Ks}
    end.

%%%
%%% End of abstract data type for known variables.
%%%

ubody(B, St) -> uexpr(B, known_init(), St).

%% ufun_clauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

ufun_clauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> ufun_clause(Lc, Ks, St) end, St0, Lcs).

%% ufun_clause(Lclause, [KnownVar], State) -> {Lclause,State}.

ufun_clause(Cl0, Ks, St0) ->
    %% Since variables in fun heads shadow previous variables
    %% with the same name, we used to send an empty list as the
    %% known variables when doing liveness analysis of the patterns
    %% (in the upattern functions).
    %%
    %% With the introduction of expressions in size for binary
    %% segments and in map keys, all known variables must be
    %% available when analysing those expressions, or some variables
    %% might not be seen as used if, for example, the expression includes
    %% a case construct.
    %%
    %% Therefore, we will send in the complete list of known variables
    %% when doing liveness analysis of patterns. This is
    %% safe because any shadowing variables in a fun head has
    %% been renamed.

    {Cl1,Pvs,Used,_,St1} = do_uclause(Cl0, Ks, St0),
    A0 = get_anno(Cl1),
    A = A0#a{us=subtract(Used, Pvs),ns=[]},
    {Cl1#iclause{anno=A},St1}.

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(Cl0, Ks, St0) ->
    {Cl1,_Pvs,Used,New,St1} = do_uclause(Cl0, Ks, St0),
    A0 = get_anno(Cl1),
    A = A0#a{us=Used,ns=New},
    {Cl1#iclause{anno=A},St1}.

do_uclause(#iclause{anno=A0,pats=Ps0,guard=G0,body=B0}, Ks0, St0) ->
    {Ps1,Pg0,Pvs,Pus,St1} = upattern_list(Ps0, Ks0, St0),
    Anno = A0#a.anno,
    {Pg,A} = case member(skip_clause, Anno) of
                 true ->
                     %% This is the skip clause for a binary generator.
                     %% To ensure that it will properly skip the nonmatching
                     %% patterns in generators such as:
                     %%
                     %%   <<V,V>> <= Gen
                     %%
                     %% we must remove any generated pre guard.
                     {[],A0#a{anno=Anno -- [skip_clause]}};
                 false ->
                     {Pg0,A0}
             end,
    Pu = union(Pus, intersection(Pvs, known_get(Ks0))),
    Pn = subtract(Pvs, Pu),
    Ks1 = known_union(Ks0, Pn),
    {G1,St2} = uguard(Pg, G0, Ks1, St1),
    Gu = used_in_any(G1),
    Gn = new_in_any(G1),
    Ks2 = known_union(Ks1, Gn),

    %% Consider this example:
    %%
    %%     {X = A,
    %%      begin X = B, fun() -> X = C end() end}.
    %%
    %% At this point it has been rewritten to something similar
    %% like this (the fun body has not been rewritten yet):
    %%
    %%     {X = A,
    %%      begin
    %%           X1 = B,
    %%           if
    %%             X1 =:= X -> ok;
    %%              true -> error({badmatch,X1})
    %%           end,
    %%           fun() -> ... end() end
    %%      end}.
    %%
    %% In this example, the variable `X` is a known variable that must
    %% be passed into the fun body (because of `X = B` above). To ensure
    %% that it is, we must call known_bind/2 with the variables used
    %% in the guard (`X1` and `X`; any variables used must surely be
    %% bound).

    Ks3 = known_bind(Ks2, Gu),
    {B1,_,St3} = uexprs(B0, Ks3, St2),
    Used = intersection(union([Pu,Gu,used_in_any(B1)]), known_get(Ks0)),
    New = union([Pn,Gn,new_in_any(B1)]),
    {#iclause{anno=A,pats=Ps1,guard=G1,body=B1},Pvs,Used,New,St3}.

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
    {Gs4,_,St6} = uexprs(Gs3, Ks, St5),
    {Gs4,St6}.

%% ulinearize_exprs([[Kexpr]], [Kexpr]) -> [Kexpr].
%%  Linearize a group of bodies to a linear list of Kernel expressions
%%  while inserting markers for the end of each body and the group
%%  itself.

ulinearize_exprs([Bs|Bss], Les) ->
    [known_end_body|Bs] ++ ulinearize_exprs(Bss, Les);
ulinearize_exprs([], Les) ->
    [known_end_group|Les].

%% uexprs([Kexpr], [KnownVar], State) -> {[Kexpr],State}.

uexprs([known_end_body|Les], Ks0, St0) ->
    Ks1 = known_end_body(Ks0),
    uexprs(Les, Ks1, St0);
uexprs([known_end_group|Les], Ks0, St0) ->
    Ks1 = known_end_group(Ks0),
    uexprs(Les, Ks1, St0);
uexprs([#iexprs{bodies=Es0}|Les], Ks0, St0) ->
    Es = ulinearize_exprs(Es0, Les),
    Ks1 = known_start_group(Ks0),
    uexprs(Es, Ks1, St0);
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
uexprs([#iset{}|_]=Les0, Ks0, St0) ->
    uexprs_iset(Les0, [], Ks0, St0);
uexprs([Le0|Les0], Ks0, St0) ->
    {Le1,St1} = uexpr(Le0, Ks0, St0),
    {Les1,Ks,St2} = uexprs(Les0, known_union(Ks0, (get_anno(Le1))#a.ns), St1),
    {[Le1|Les1],Ks,St2};
uexprs([], Ks, St) -> {[],Ks,St}.

%% Since the set of known variables can grow quite large, try minimize
%% the number of union operations on it.
uexprs_iset([#iset{anno=A0,var=V,arg=Arg0}=Le0|Les0], New0, Ks0, St0) ->
    case uexpr_need_known(Arg0) of
        true ->
            Ks1 = known_union(Ks0, New0),
            {Le1,St1} = uexpr(Le0, Ks1, St0),
            New = (get_anno(Le1))#a.ns,
            {Les1,Ks,St2} = uexprs_iset(Les0, New, Ks1, St1),
            {[Le1|Les1],Ks,St2};
        false ->
            %% We don't need the set of known variables when processing
            %% Arg0, so we can postpone the call to known_union/2. This
            %% will save time for functions with a huge number of variables.
            {Arg,St1} = uexpr(Arg0, none, St0),
            #a{us=Us,ns=Ns} = get_anno(Arg),
            A = A0#a{us=del_element(V#c_var.name, Us),
                     ns=add_element(V#c_var.name, Ns)},
            Le1 = Le0#iset{anno=A,arg=Arg},
            New = union(New0, A#a.ns),
            {Les1,Ks,St2} = uexprs_iset(Les0, New, Ks0, St1),
            {[Le1|Les1],Ks,St2}
    end;
uexprs_iset(Les, New, Ks0, St) ->
    Ks = known_union(Ks0, New),
    uexprs(Les, Ks, St).

uexpr_need_known(#icall{}) -> false;
uexpr_need_known(#iapply{}) -> false;
uexpr_need_known(#ibinary{}) -> false;
uexpr_need_known(#iprimop{}) -> false;
uexpr_need_known(#c_literal{}) -> false;
uexpr_need_known(Core) -> not is_simple(Core).

%% upat_is_new_var(Pattern, [KnownVar]) -> true|false.
%%  Test whether the pattern is a single, previously unknown
%%  variable.

upat_is_new_var(#c_var{name=V}, Ks) ->
    not is_element(V, known_get(Ks));
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
    {B1,_,St2} = uexprs(B0, Ks, St1),
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
uexpr(#ifun{anno=A0,id=Id,vars=As,clauses=Cs0,fc=Fc0,name=Name}=Fun0, Ks0, St0) ->
    {Fun1,St2} = case known_get(Ks0) of
                     [] ->
                         {Fun0,St0};
                     [_|_] ->
                         {Cs1,St1} = rename_shadowing_clauses(Cs0, Ks0, St0),
                         {Fun0#ifun{clauses=Cs1},St1}
                 end,
    #ifun{clauses=Cs2} = Fun1,
    Avs = lit_list_vars(As),
    Ks1 = case Name of
              unnamed -> Ks0;
              {named,FName} -> known_union(Ks0, subtract([FName], Avs))
          end,
    Ks2 = known_union(Ks1, Avs),
    KnownInFun = known_in_fun(Ks2, Name),
    {Cs3,St3} = ufun_clauses(Cs2, KnownInFun, St2),
    {Fc1,St4} = ufun_clause(Fc0, KnownInFun, St3),
    Used = subtract(intersection(used_in_any(Cs3), known_get(Ks1)), Avs),
    A1 = A0#a{us=Used,ns=[]},
    {#ifun{anno=A1,id=Id,vars=As,clauses=Cs3,fc=Fc1,name=Name},St4};
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
    %% No variables are exported from try/catch. Starting in OTP 24,
    %% variables bound in the argument (the code between the 'try' and
    %% the 'of' keywords) are exported to the body (the code following
    %% the 'of' keyword).
    {As1,_,St1} = uexprs(As0, Ks, St0),
    ArgKs = known_union(Ks, new_in_any(As1)),
    {Bs1,_,St2} = uexprs(Bs0, ArgKs, St1),
    {Hs1,_,St3} = uexprs(Hs0, Ks, St2),
    Used = intersection(used_in_any(Bs1++Hs1++As1), known_get(Ks)),
    {#itry{anno=A#a{us=Used,ns=[]},
	   args=As1,vars=Vs,body=Bs1,evars=Evs,handler=Hs1},St3};
uexpr(#icatch{anno=A,body=Es0}, Ks, St0) ->
    {Es1,_,St1} = uexprs(Es0, Ks, St0),
    {#icatch{anno=A#a{us=used_in_any(Es1)},body=Es1},St1};
uexpr(#ireceive1{anno=A,clauses=Cs0}, Ks, St0) ->
    {Cs1,St1} = uclauses(Cs0, Ks, St0),
    {#ireceive1{anno=A#a{us=used_in_any(Cs1),ns=new_in_all(Cs1)},
		clauses=Cs1},St1};
uexpr(#ireceive2{anno=A,clauses=Cs0,timeout=Te0,action=Tes0}, Ks, St0) ->
    %% Te0 will never generate new variables.
    {Te1,St1} = uexpr(Te0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Tes1,_,St3} = uexprs(Tes0, Ks, St2),
    Used = union([used_in_any(Cs1),used_in_any(Tes1),(get_anno(Te1))#a.us]),
    New = case Cs1 of
	      [] -> new_in_any(Tes1);
	      _ -> intersection(new_in_all(Cs1), new_in_any(Tes1))
	  end,
    {#ireceive2{anno=A#a{us=Used,ns=New},
		clauses=Cs1,timeout=Te1,action=Tes1},St3};
uexpr(#iprotect{anno=A,body=Es0}, Ks, St0) ->
    {Es1,_,St1} = uexprs(Es0, Ks, St0),
    Used = used_in_any(Es1),
    {#iprotect{anno=A#a{us=Used},body=Es1},St1}; %No new variables escape!
uexpr(#ibinary{anno=A,segments=Ss}, _, St) ->
    Used = bitstr_vars(Ss),
    {#ibinary{anno=A#a{us=Used},segments=Ss},St};
uexpr(#c_literal{}=Lit, _, St) ->
    Anno = get_anno(Lit),
    {set_anno(Lit, #a{us=[],anno=Anno}),St};
uexpr(#c_opaque{}=Opaque, _, St) ->
    {set_anno(Opaque, #a{us=[],anno=get_anno(Opaque)}),St};
uexpr(Simple, _, St) ->
    true = is_simple(Simple),			%Sanity check!
    Vs = lit_vars(Simple),
    Anno = get_anno(Simple),
    {#isimple{anno=#a{us=Vs,anno=Anno},term=Simple},St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% upattern(Pat, [KnownVar], State) ->
%%              {Pat,[GuardTest],[NewVar],[UsedVar],State}.

upattern(#c_var{anno=Anno,name='_'}, _, St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{anno=Anno,name=New},[],[New],[],St1};
upattern(#c_var{name=V}=Var, Ks, St0) ->
    case is_element(V, known_get(Ks)) of
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
    {T1,Tg,Tv,Tu,St2} = upattern(T0, known_union(Ks, Hv), St1),
    {Cons#c_cons{hd=H1,tl=T1},Hg ++ Tg,union(Hv, Tv),union(Hu, Tu),St2};
upattern(#c_tuple{es=Es0}=Tuple, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {Tuple#c_tuple{es=Es1},Esg,Esv,Eus,St1};
upattern(#imap{es=Es0}=Map, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {Map#imap{es=Es1},Esg,Esv,Eus,St1};
upattern(#imappair{op=#c_literal{val=exact},key=K0,val=V0}=Pair,Ks,St0) ->
    {V,Vg,Vn,Vu,St1} = upattern(V0, Ks, St0),
    {K,_,St2} = uexprs(K0, Ks, St1),
    Ku = used_in_expr(K),
    {Pair#imappair{key=K,val=V},Vg,Vn,union(Ku, Vu),St2};
upattern(#ibinary{segments=Es0}=Bin, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upat_bin(Es0, Ks, St0),
    {Bin#ibinary{segments=Es1},Esg,Esv,Eus,St1};
upattern(#c_alias{var=V0,pat=P0}=Alias, Ks, St0) ->
    {V1,Vg,Vv,Vu,St1} = upattern(V0, Ks, St0),
    {P1,Pg,Pv,Pu,St2} = upattern(P0, known_union(Ks, Vv), St1),
    {Alias#c_alias{var=V1,pat=P1},Vg ++ Pg,union(Vv, Pv),union(Vu, Pu),St2};
upattern(Other, _, St) -> {Other,[],[],[],St}.	%Constants

%% upattern_list([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upattern_list([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upattern(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upattern_list(Ps0, known_union(Ks, Pv), St1),
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
    {Ps1,Psg,Psv,Psu,St2} = upat_bin(Ps0, known_union(Ks, Pv), Bs1, St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upat_bin([], _, _, St) -> {[],[],[],[],St}.    


%% upat_element(Segment, [KnownVar], [LocalVar], State) ->
%%        {Segment,[GuardTest],[NewVar],[UsedVar],[LocalVar],State}
upat_element(#ibitstr{val=H0,size=Sz0}=Seg, Ks, Bs0, St0) ->
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
    case Sz0 of
        [#c_var{name=Vname}] ->
            {Sz1,Us} = rename_bitstr_size(Vname, Bs0),
            {Sz2,_,St2} = uexprs([Sz1], Ks, St1),
            {Seg#ibitstr{val=H1,size=Sz2},Hg,Hv,Us,Bs1,St2};
        [#c_literal{}] ->
            {Sz1,_,St2} = uexprs(Sz0, Ks, St1),
            Us = [],
            {Seg#ibitstr{val=H1,size=Sz1},Hg,Hv,Us,Bs1,St2};
        Expr when is_list(Expr) ->
            Sz1 = [#iset{var=#c_var{name=Old},arg=#c_var{name=New}} ||
                      {Old,New} <- Bs0] ++ Expr,
            {Sz2,_,St2} = uexprs(Sz1, Ks, St1),
            Us = used_in_expr(Sz2),
            {Seg#ibitstr{val=H1,size=Sz2},Hg,Hv,Us,Bs1,St2}
    end.

rename_bitstr_size(V, [{V,N}|_]) ->
    New = #c_var{name=N},
    {New,[N]};
rename_bitstr_size(V, [_|Rest]) ->
    rename_bitstr_size(V, Rest);
rename_bitstr_size(V, []) ->
    Old = #c_var{name=V},
    {Old,[V]}.

used_in_expr([Le|Les]) ->
    #a{us=Us,ns=Ns} = get_anno(Le),
    Used = used_in_expr(Les),
    union(Us, subtract(Used, Ns));
used_in_expr([]) -> [].

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

%%%
%%% Rename shadowing variables in fun heads.
%%%
%%% Pattern variables in fun heads always shadow variables bound in
%%% the enclosing environment. Because that is the way that variables
%%% behave in Core Erlang, there was previously no need to rename
%%% the variables.
%%%
%%% However, to support splitting of patterns and/or pattern matching
%%% compilation in Core Erlang, there is a need to rename all
%%% shadowing variables to avoid changing the semantics of the Erlang
%%% program.
%%%

rename_shadowing_clauses([C0|Cs0], Ks, St0) ->
    {C,St1} = rename_shadowing_clause(C0, Ks, St0),
    {Cs,St} = rename_shadowing_clauses(Cs0, Ks, St1),
    {[C|Cs],St};
rename_shadowing_clauses([], _Ks, St) ->
    {[],St}.

rename_shadowing_clause(#iclause{pats=Ps0,guard=G0,body=B0}=C, Ks, St0) ->
    Subs = {[],[]},
    {Ps,{_Isub,Osub},St} = ren_pats(Ps0, Ks, Subs, St0),
    G = case G0 of
            [] -> G0;
            [_|_] -> Osub ++ G0
        end,
    B = Osub ++ B0,
    {C#iclause{pats=Ps,guard=G,body=B},St}.

ren_pats([P0|Ps0], Ks, {_,_}=Subs0, St0) ->
    {P,Subs1,St1} = ren_pat(P0, Ks, Subs0, St0),
    {Ps,Subs,St} = ren_pats(Ps0, Ks, Subs1, St1),
    {[P|Ps],Subs,St};
ren_pats([], _Ks, {_,_}=Subs, St) ->
    {[],Subs,St}.

ren_pat(#c_var{name='_'}=P, _Ks, Subs, St) ->
    {P,Subs,St};
ren_pat(#c_var{name=V}=Old, Ks, {Isub0,Osub0}=Subs, St0) ->
    case member(V, known_get(Ks)) of
        true ->
            case ren_is_subst(V, Osub0) of
                {yes,New} ->
                    {New,Subs,St0};
                no ->
                    {New,St} = new_var(St0),
                    Osub = [#iset{var=Old,arg=New}|Osub0],
                    {New,{Isub0,Osub},St}
            end;
        false ->
            {Old,Subs,St0}
    end;
ren_pat(#c_literal{}=P, _Ks, {_,_}=Subs, St) ->
    {P,Subs,St};
ren_pat(#c_alias{var=Var0,pat=Pat0}=Alias, Ks, {_,_}=Subs0, St0) ->
    {Var,Subs1,St1} = ren_pat(Var0, Ks, Subs0, St0),
    {Pat,Subs,St} = ren_pat(Pat0, Ks, Subs1, St1),
    {Alias#c_alias{var=Var,pat=Pat},Subs,St};
ren_pat(#imap{es=Es0}=Map, Ks, {_,_}=Subs0, St0) ->
    {Es,Subs,St} = ren_pat_map(Es0, Ks, Subs0, St0),
    {Map#imap{es=Es},Subs,St};
ren_pat(#ibinary{segments=Es0}=P, Ks, {Isub,Osub0}, St0) ->
    {Es,_Isub,Osub,St} = ren_pat_bin(Es0, Ks, Isub, Osub0, St0),
    {P#ibinary{segments=Es},{Isub,Osub},St};
ren_pat(P, Ks0, {_,_}=Subs0, St0) ->
    Anno = cerl:get_ann(P),
    Es0 = cerl:data_es(P),
    {Es,Subs,St} = ren_pats(Es0, Ks0, Subs0, St0),
    {cerl:ann_make_data(Anno, cerl:data_type(P), Es),Subs,St}.

ren_pat_bin([#ibitstr{val=Val0,size=Sz0}=E|Es0], Ks, Isub0, Osub0, St0) ->
    Sz = ren_get_subst(Sz0, Isub0),
    {Val,{_,Osub1},St1} = ren_pat(Val0, Ks, {Isub0,Osub0}, St0),
    Isub1 = case Val0 of
                #c_var{} ->
                    [#iset{var=Val0,arg=Val}|Isub0];
                _ ->
                    Isub0
            end,
    {Es,Isub,Osub,St} = ren_pat_bin(Es0, Ks, Isub1, Osub1, St1),
    {[E#ibitstr{val=Val,size=Sz}|Es],Isub,Osub,St};
ren_pat_bin([], _Ks, Isub, Osub, St) ->
    {[],Isub,Osub,St}.

ren_pat_map([#imappair{val=Val0}=MapPair|Es0], Ks, Subs0, St0) ->
    {Val,Subs1,St1} = ren_pat(Val0, Ks, Subs0, St0),
    {Es,Subs,St} = ren_pat_map(Es0, Ks, Subs1, St1),
    {[MapPair#imappair{val=Val}|Es],Subs,St};
ren_pat_map([], _Ks, Subs, St) ->
    {[],Subs,St}.

ren_get_subst([#c_var{name=V}]=Old, Sub) ->
    case ren_is_subst(V, Sub) of
        no -> Old;
        {yes,New} -> [New]
    end;
ren_get_subst([#c_literal{}]=Old, _Sub) ->
    Old;
ren_get_subst(Expr, Sub) when is_list(Expr) ->
    Sub ++ Expr.

ren_is_subst(V, [#iset{var=#c_var{name=V},arg=Arg}|_]) ->
    {yes,Arg};
ren_is_subst(V, [_|Sub]) ->
    ren_is_subst(V, Sub);
ren_is_subst(_V, []) -> no.

%% The AfterVars are the variables which are used afterwards.  We need
%% this to work out which variables are actually exported and used
%% from case/receive.  In subblocks/clauses the AfterVars of the block
%% are just the exported variables.

cbody(B0, none, St0) ->
    {B1,_,_,St1} = cexpr(B0, [], St0),
    {B1,St1};
cbody(B0, Nifs, St0) ->
    {B1,_,_,St1} = cexpr(B0, [], St0),
    B2 = case sets:is_element(St1#core.function,Nifs) of
             true ->
                 #c_fun{body=Body0} = B1,
                 Body1 = #c_seq{arg=#c_primop{name=#c_literal{val=nif_start},
                                              args=[]},
                                body=Body0},
                 B1#c_fun{body=Body1};
             false ->
                 B1
         end,
    {B2,St1}.

%% cclause(Lclause, [AfterVar], State) -> {Cclause,State}.
%%  The AfterVars are the exported variables.

cclause(#iclause{anno=#a{anno=Anno},pats=Ps0,guard=G0,body=B0}, Exp, St0) ->
    Ps = cpattern_list(Ps0),
    {B1,_Us1,St1} = cexprs(B0, Exp, St0),
    {G1,St2} = cguard(G0, St1),
    {#c_clause{anno=Anno,pats=Ps,guard=G1,body=B1},St2}.

cclauses(Lcs, Es, St0) ->
    mapfoldl(fun (Lc, St) -> cclause(Lc, Es, St) end, St0, Lcs).

cguard([], St) -> {#c_literal{val=true},St};
cguard(Gs, St0) ->
    {G,_,St1} = cexprs(Gs, [], St0),
    {G,St1}.

cpattern_list([P|Ps]) ->
    [cpattern(P)|cpattern_list(Ps)];
cpattern_list([]) -> [].

cpattern(#c_alias{pat=Pat}=Alias) ->
    Alias#c_alias{pat=cpattern(Pat)};
cpattern(#c_cons{hd=Hd,tl=Tl}=Cons) ->
    Cons#c_cons{hd=cpattern(Hd),tl=cpattern(Tl)};
cpattern(#c_tuple{es=Es}=Tup) ->
    Tup#c_tuple{es=cpattern_list(Es)};
cpattern(#imap{anno=#a{anno=Anno},es=Es}) ->
    #c_map{anno=Anno,es=cpat_map_pairs(Es),is_pat=true};
cpattern(#ibinary{anno=#a{anno=Anno},segments=Segs0}) ->
    Segs = [cpat_bin_seg(S) || S <- Segs0],
    #c_binary{anno=Anno,segments=Segs};
cpattern(Other) -> Other.

cpat_map_pairs([#imappair{anno=#a{anno=Anno},op=Op,key=Key0,val=Val0}|T]) ->
    {Key,_,_} = cexprs(Key0, [], #core{}),
    Val = cpattern(Val0),
    Pair = #c_map_pair{anno=Anno,op=Op,key=Key,val=Val},
    [Pair|cpat_map_pairs(T)];
cpat_map_pairs([]) -> [].

cpat_bin_seg(#ibitstr{anno=#a{anno=Anno},val=E,size=Sz0,unit=Unit,
                      type=Type,flags=Flags}) ->
    {Sz,_,_} = cexprs(Sz0, [], #core{}),
    #c_bitstr{anno=Anno,val=E,size=Sz,unit=Unit,type=Type,flags=Flags}.

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
    {Cfc0,St3} = cclause(Lfc, [], St2),		%Never exports
    {Cfc,St4} = c_add_dummy_export(Cfc0, Exp, St3),
    {#c_case{anno=A#a.anno,
	     arg=core_lib:make_values(Cargs),clauses=Ccs ++ [Cfc]},
     Exp,A#a.us,St4};
cexpr(#ireceive1{anno=A,clauses=Lcs}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Ccs,St1} = cclauses(Lcs, Exp, St0),
    True = #c_literal{val=true},
    Action = core_lib:make_values(lists:duplicate(1+length(Exp), True)),
    {#c_receive{anno=A#a.anno,
		clauses=Ccs,
		timeout=#c_literal{val=infinity},action=Action},
     Exp,A#a.us,St1};
cexpr(#ireceive2{anno=A,clauses=Lcs,timeout=Lto,action=Les}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cto,[],_Us1,St1} = cexpr(Lto, As, St0),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Ces,_Us2,St3} = cexprs(Les, Exp, St2),
    {#c_receive{anno=A#a.anno,
		clauses=Ccs,timeout=Cto,action=Ces},
     Exp,A#a.us,St3};
cexpr(#itry{anno=A,args=La,vars=Vs0,body=Lb,evars=Evs,handler=Lh}, _As, St0) ->
    %% No variables are exported from try/catch. Starting in OTP 24,
    %% variables bound in the argument (the code between the 'try' and
    %% the 'of' keywords) are exported to the body (the code following
    %% the 'of' keyword).
    AsExp = intersection(new_in_any(La), used_in_any(Lb)),
    {Ca,_Us1,St1} = cexprs(La, AsExp, St0),
    {Cb,_Us2,St2} = cexprs(Lb, [], St1),
    {Ch,_Us3,St3} = cexprs(Lh, [], St2),
    Vs = Vs0 ++ [#c_var{name=V} || V <- AsExp],
    {#c_try{anno=A#a.anno,arg=Ca,vars=Vs,body=Cb,evars=Evs,handler=Ch},
     [],A#a.us,St3};
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
cexpr(#icall{anno=A,module=Mod,name=Name,args=Args}, _As, St0) ->
    Anno = A#a.anno -- [v3_core],
    case (not cerl:is_c_atom(Mod)) andalso member(tuple_calls, St0#core.opts) of
	true ->
	    GenAnno = [compiler_generated|Anno],

	    %% Generate the clause that matches on the tuple
	    {TupleVar,St1} = new_var(GenAnno, St0),
	    {TupleSizeVar, St2} = new_var(GenAnno, St1),
	    {TupleModVar, St3} = new_var(GenAnno, St2),
	    {TupleArgsVar, St4} = new_var(GenAnno, St3),
	    TryVar = cerl:c_var('Try'),

	    TupleGuardExpr =
		cerl:c_let([TupleSizeVar],
			   c_call_erl(tuple_size, [TupleVar]),
			   c_call_erl('>', [TupleSizeVar, cerl:c_int(0)])),

	    TupleGuard =
		cerl:c_try(TupleGuardExpr, [TryVar], TryVar,
			   [cerl:c_var('T'),cerl:c_var('R')], cerl:c_atom(false)),

	    TupleApply =
		cerl:c_let([TupleModVar],
			   c_call_erl(element, [cerl:c_int(1),TupleVar]),
			   cerl:c_let([TupleArgsVar],
				      cerl:make_list(Args ++ [TupleVar]),
				      c_call_erl(apply, [TupleModVar,Name,TupleArgsVar]))),

	    TupleClause = cerl:ann_c_clause(GenAnno, [TupleVar], TupleGuard, TupleApply),

	    %% Generate the fallback clause
	    {OtherVar,St5} = new_var(GenAnno, St4),
	    OtherApply = cerl:ann_c_call(GenAnno, OtherVar, Name, Args),
	    OtherClause = cerl:ann_c_clause(GenAnno, [OtherVar], OtherApply),

	    {cerl:ann_c_case(GenAnno, Mod, [TupleClause,OtherClause]),[],A#a.us,St5};
	false ->
	    {#c_call{anno=Anno,module=Mod,name=Name,args=Args},[],A#a.us,St0}
    end;
cexpr(O=#c_opaque{}, _As, St) ->
    {O,[],[],St};
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

c_call_erl(Fun, Args) ->
    As = [compiler_generated],
    cerl:ann_c_call(As, cerl:c_atom(erlang), cerl:c_atom(Fun), Args).


c_add_dummy_export(#c_clause{body=B0}=C, [_|_]=Exp, St0) ->
    %% Add dummy export in order to always return the correct number
    %% of values for the default clause.
    {V,St1} = new_var(St0),
    B = #c_let{vars=[V],arg=B0,
               body=#c_values{es=[V|duplicate(length(Exp), #c_literal{val=[]})]}},
    {C#c_clause{body=B},St1};
c_add_dummy_export(C, [], St) ->
    {C,St}.

%%%
%%% Lower a `receive` to more primitive operations. Rewrite patterns
%%% that use and bind the same variable as nested cases.
%%%
%%% Here follows an example of how a receive in this Erlang code:
%%%
%%% foo(Timeout) ->
%%%     receive
%%%         {tag,Msg} -> Msg
%%%     after
%%%         Timeout ->
%%%             no_message
%%%     end.
%%%
%%% is translated into Core Erlang:
%%%
%%% 'foo'/1 =
%%%     fun (Timeout) ->
%%%         ( letrec
%%%               'recv$^0'/0 =
%%%                   fun () ->
%%%                       let <PeekSucceeded,Message> =
%%%                           primop 'recv_peek_message'()
%%%                       in  case PeekSucceeded of
%%%                             <'true'> when 'true' ->
%%%                                 case Message of
%%%                                   <{'tag',Msg}> when 'true' ->
%%%                                       do  primop 'remove_message'()
%%%                                           Msg
%%%                                   ( <Other> when 'true' ->
%%%                                         do  primop 'recv_next'()
%%%                                             apply 'recv$^0'/0()
%%%                                     -| ['compiler_generated'] )
%%%                                 end
%%%                             <'false'> when 'true' ->
%%%                                 let <TimedOut> =
%%%                                     primop 'recv_wait_timeout'(Timeout)
%%%                                 in  case TimedOut of
%%%                                       <'true'> when 'true' ->
%%%                                           'no_message'
%%%                                       <'false'> when 'true' ->
%%%                                           apply 'recv$^0'/0()
%%%                                     end
%%%                           end
%%%           in  apply 'recv$^0'/0()
%%%           -| ['letrec_goto'] )

lbody(B, St) ->
    cerl_trees:mapfold(fun skip_lowering/2, fun lexpr/2, St, B).

%% These nodes do not have case or receive within them,
%% so we can speed up lowering by not traversing them.
skip_lowering(#c_binary{}, _A) -> skip;
skip_lowering(#c_call{}, _A) -> skip;
skip_lowering(#c_cons{}, _A) -> skip;
skip_lowering(#c_literal{}, _A) -> skip;
skip_lowering(#c_map{}, _A) -> skip;
skip_lowering(#c_opaque{}, _A) -> skip;
skip_lowering(#c_primop{}, _A) -> skip;
skip_lowering(#c_tuple{}, _A) -> skip;
skip_lowering(T, A) -> {T, A}.

lexpr(#c_case{}=Case, St) ->
    %% Split patterns that bind and use the same variable.
    split_case(Case, St);
lexpr(#c_receive{clauses=[],timeout=Timeout0,action=Action}, St0) ->
    %% Lower a receive with only an after to its primitive operations.
    False = #c_literal{val=false},
    True = #c_literal{val=true},

    {Timeout,Outer0,St1} =
        case is_safe(Timeout0) of
            true ->
                {Timeout0,False,St0};
            false ->
                {TimeoutVar,Sti0} = new_var(St0),
                OuterLet = #c_let{vars=[TimeoutVar],arg=Timeout0,body=False},
                {TimeoutVar,OuterLet,Sti0}
        end,

    MaybeIgnore = case Timeout of
                      #c_literal{val=infinity} -> [dialyzer_ignore];
                      _ -> []
                  end,

    {LoopName,St2} = new_fun_name("recv", St1),
    LoopFun = #c_var{name={LoopName,0}},
    ApplyLoop = #c_apply{anno=[dialyzer_ignore],op=LoopFun,args=[]},

    AfterCs = [#c_clause{anno=MaybeIgnore,pats=[True],guard=True,
                         body=Action},
               #c_clause{anno=[compiler_generated,dialyzer_ignore],
                         pats=[False],guard=True,
                         body=ApplyLoop}],
    {TimeoutBool,St3} = new_var(St2),
    TimeoutCase = #c_case{anno=[receive_timeout],arg=TimeoutBool,
                          clauses=AfterCs},
    TimeoutLet = #c_let{vars=[TimeoutBool],
                        arg=primop(recv_wait_timeout, [Timeout]),
                        body=TimeoutCase},

    Fun = #c_fun{vars=[],body=TimeoutLet},

    Letrec = #c_letrec{anno=[letrec_goto,no_inline],
                       defs=[{LoopFun,Fun}],
                       body=ApplyLoop},

    %% If the 'after' expression is unsafe we evaluate it in an outer 'let'.
    Outer = case Outer0 of
                #c_let{} -> Outer0#c_let{body=Letrec};
                _ -> Letrec
            end,
    {Outer,St3};
lexpr(#c_receive{anno=RecvAnno,clauses=Cs0,timeout=Timeout0,action=Action}, St0) ->
    %% Lower receive to its primitive operations.
    False = #c_literal{val=false},
    True = #c_literal{val=true},

    {Timeout,Outer0,St1} =
        case is_safe(Timeout0) of
            true ->
                {Timeout0,False,St0};
            false ->
                {TimeoutVar,Sti0} = new_var(St0),
                OuterLet = #c_let{vars=[TimeoutVar],arg=Timeout0,body=False},
                {TimeoutVar,OuterLet,Sti0}
        end,

    MaybeIgnore = case Timeout of
                      #c_literal{val=infinity} -> [dialyzer_ignore];
                      _ -> []
                  end,

    {LoopName,St2} = new_fun_name("recv", St1),
    LoopFun = #c_var{name={LoopName,0}},
    ApplyLoop = #c_apply{anno=[dialyzer_ignore],op=LoopFun,args=[]},

    Cs1 = rewrite_cs(Cs0),
    RecvNext = #c_seq{arg=primop(recv_next),
                      body=ApplyLoop},
    RecvNextC = #c_clause{anno=[compiler_generated,dialyzer_ignore],
                          pats=[#c_var{name='Other'}],guard=True,body=RecvNext},
    Cs = Cs1 ++ [RecvNextC],
    {Msg,St3} = new_var(St2),
    {MsgCase,St4} = split_case(#c_case{anno=RecvAnno,arg=Msg,clauses=Cs}, St3),

    AfterCs = [#c_clause{pats=[True],guard=True,body=Action},
               #c_clause{anno=[dialyzer_ignore],pats=[False],guard=True,
                         body=ApplyLoop}],
    {TimeoutBool,St5} = new_var(St4),
    TimeoutCase = #c_case{arg=TimeoutBool,clauses=AfterCs},
    TimeoutLet = #c_let{vars=[TimeoutBool],
                        arg=primop(recv_wait_timeout, [Timeout]),
                        body=TimeoutCase},

    {PeekSucceeded,St6} = new_var(St5),
    PeekCs = [#c_clause{pats=[True],guard=True,
                        body=MsgCase},
              #c_clause{anno=MaybeIgnore,
                        pats=[False],guard=True,
                        body=TimeoutLet}],
    PeekCase = #c_case{arg=PeekSucceeded,clauses=PeekCs},
    PeekLet = #c_let{vars=[PeekSucceeded,Msg],
                     arg=primop(recv_peek_message, [], RecvAnno),
                     body=PeekCase},
    Fun = #c_fun{vars=[],body=PeekLet},

    Letrec = #c_letrec{anno=[letrec_goto,no_inline],
                       defs=[{LoopFun,Fun}],
                       body=ApplyLoop},

    %% If the 'after' expression is unsafe we evaluate it in an outer 'let'.
    Outer = case Outer0 of
                #c_let{} -> Outer0#c_let{body=Letrec};
                _ -> Letrec
            end,
    {Outer,St6};
lexpr(Tree, St) ->
    {Tree,St}.

rewrite_cs([#c_clause{body=B0}=C|Cs]) ->
    B = #c_seq{arg=primop(remove_message),body=B0},
    [C#c_clause{body=B}|rewrite_cs(Cs)];
rewrite_cs([]) -> [].

primop(Name) ->
    primop(Name, []).

primop(Name, Args) ->
    primop(Name, Args, []).

primop(Name, Args, Anno) ->
    #c_primop{anno=Anno,name=#c_literal{val=Name},args=Args}.

%%%
%%% Split patterns such as <<Size:32,Tail:Size>> that bind
%%% and use a variable in the same pattern. Rewrite to a
%%% nested case in a letrec.
%%%

split_case(#c_case{anno=CaseAnno,arg=Arg,clauses=Cs0}=Case0, St0) ->
    Args = case Arg of
               #c_values{es=Es} -> Es;
               _ -> [Arg]
           end,
    {VarArgs,St1} = split_var_args(Args, St0),
    case split_clauses(Cs0, VarArgs, CaseAnno, St1) of
        none ->
            {Case0,St0};
        {PreCase,AftCs,St2} ->
            AftCase = Case0#c_case{arg=core_lib:make_values(VarArgs),
                                   clauses=AftCs},
            AftFun = #c_fun{vars=[],body=AftCase},
            {Letrec,St3} = split_case_letrec(AftFun, PreCase, St2),
            Body = split_letify(VarArgs, Args, Letrec, [], []),
            {Body,St3}
    end.

split_var_args(Args, St) ->
    mapfoldl(fun(#c_var{}=Var, S0) ->
                     {Var,S0};
                (#c_literal{}=Lit, S0) ->
                     {Lit,S0};
                (_, S0) ->
                     new_var(S0)
             end, St, Args).

split_letify([Same|Vs], [Same|Args], Body, VsAcc, ArgAcc) ->
    split_letify(Vs, Args, Body, VsAcc, ArgAcc);
split_letify([V|Vs], [Arg|Args], Body, VsAcc, ArgAcc) ->
    split_letify(Vs, Args, Body, [V|VsAcc], [Arg|ArgAcc]);
split_letify([], [], Body, [], []) ->
    Body;
split_letify([], [], Body, [_|_]=VsAcc, [_|_]=ArgAcc) ->
    #c_let{vars=reverse(VsAcc),
           arg=core_lib:make_values(reverse(ArgAcc)),
           body=Body}.

split_case_letrec(#c_fun{anno=FunAnno0}=Fun0, Body, #core{gcount=C}=St0) ->
    FunAnno = [compiler_generated|FunAnno0],
    Fun = Fun0#c_fun{anno=FunAnno},
    Anno = [letrec_goto,no_inline],
    DefFunName = goto_func(C),
    Letrec = #c_letrec{anno=Anno,defs=[{#c_var{name=DefFunName},Fun}],body=Body},
    St = St0#core{gcount=C+1},
    lbody(Letrec, St).

split_clauses([C0|Cs0], Args, CaseAnno, St0) ->
    case split_clauses(Cs0, Args, CaseAnno, St0) of
        none ->
            case split_clause(C0, St0) of
                none ->
                    none;
                {Ps,Nested,St1} ->
                    {Case,St2} = split_reconstruct(Args, Ps, Nested,
                                                   C0, CaseAnno, St1),
                    {Case,Cs0,St2}
            end;
        {Case0,Cs,St} ->
            #c_case{clauses=NewClauses} = Case0,
            Case = Case0#c_case{clauses=[C0|NewClauses]},
            {Case,Cs,St}
    end;
split_clauses([], _, _, _) ->
    none.

goto_func(Count) ->
    {list_to_atom("label^" ++ integer_to_list(Count)),0}.

split_reconstruct(Args, Ps, nil, #c_clause{anno=Anno}=C0, CaseAnno, St0) ->
    C = C0#c_clause{pats=Ps},
    {Fc,St1} = split_fc_clause(Ps, Anno, St0),
    {#c_case{anno=CaseAnno,arg=core_lib:make_values(Args),clauses=[C,Fc]},St1};
split_reconstruct(Args, Ps, {split,SplitArgs,Pat,Nested}, C, CaseAnno, St) ->
    Split = {split,SplitArgs,fun(Body) -> Body end,Pat,Nested},
    split_reconstruct(Args, Ps, Split, C, CaseAnno, St);
split_reconstruct(Args, Ps, {split,SplitArgs,Wrap,Pat,Nested},
                  #c_clause{anno=Anno}=C0, CaseAnno, St0) ->
    {InnerCase,St1} = split_reconstruct(SplitArgs, [Pat], Nested, C0,
                                        CaseAnno, St0),
    {Fc,St2} = split_fc_clause(Args, Anno, St1),
    Wrapped = Wrap(InnerCase),
    C = C0#c_clause{pats=Ps,guard=#c_literal{val=true},body=Wrapped},
    {#c_case{anno=CaseAnno,arg=core_lib:make_values(Args),clauses=[C,Fc]},St2}.

split_fc_clause(Args, Anno0, #core{gcount=Count}=St0) ->
    Anno = [compiler_generated|Anno0],
    Arity = length(Args),
    {Vars,St1} = new_vars(Arity, St0),
    Op = #c_var{name=goto_func(Count)},
    Apply = #c_apply{anno=Anno,op=Op,args=[]},
    {#c_clause{anno=[dialyzer_ignore|Anno],pats=Vars,
               guard=#c_literal{val=true},body=Apply},St1}.

split_clause(#c_clause{pats=Ps0}, St0) ->
    case split_pats(Ps0, St0) of
        none ->
            none;
        {Ps,Case,St} ->
            {Ps,Case,St}
    end.

split_pats([P0|Ps0], St0) ->
    case split_pats(Ps0, St0) of
        none ->
            case split_pat(P0, St0) of
                none ->
                    none;
                {P,Case,St} ->
                    {[P|Ps0],Case,St}
            end;
        {Ps,Case,St} ->
            {[P0|Ps],Case,St}
    end;
split_pats([], _) ->
    none.

split_pat(#c_binary{anno=Anno0,segments=Segs0}=Bin, St0) ->
    Vars = gb_sets:empty(),
    case split_bin_segments(Segs0, Vars, St0, []) of
        none ->
            none;
        {size_var,TailVar,Wrap,Bef,Aft,St1} ->
            {BefBin,Anno,St} = size_var_before_bin(Bin, Bef, St1),
            {BefBin,{split,[TailVar],Wrap,Bin#c_binary{anno=Anno,segments=Aft},nil},St};
        {sequential_match,Bef,Aft,St1} ->
            Anno = keydelete(binary_var, 1, Anno0),
            {BefBin,St} =
                case keyfind(binary_var, 1, Anno0) of
                    false ->
                        {BinVar,StInt} = new_var(St1),
                        {#c_alias{var=BinVar,pat=Bin#c_binary{segments=Bef}},StInt};
                    {binary_var,BinVar} ->
                        {Bin#c_binary{anno=Anno,segments=Bef},St1}
                end,
            Wrap = fun(Body) -> Body end,
            {BefBin,{split,[BinVar],Wrap,Bin#c_binary{anno=Anno,segments=Aft},nil},St}
    end;
split_pat(#c_map{es=Es}=Map, St) ->
    split_map_pat(Es, Map, St, []);
split_pat(#c_var{}, _) ->
    none;
split_pat(#c_alias{pat=Pat}=Alias0, St0) ->
    case split_pat(Pat, St0) of
        none ->
            none;
        {Ps,Split,St1} ->
            {Var,St} = new_var(St1),
            Alias = Alias0#c_alias{pat=Var},
            {Alias,{split,[Var],Ps,Split},St}
    end;
split_pat(Data, St0) ->
    Type = cerl:data_type(Data),
    Es = cerl:data_es(Data),
    split_data(Es, Type, St0, []).

size_var_before_bin(#c_binary{anno=Anno0,segments=Segments}=Bin0, Bef, St0) ->
    case any(fun(#c_bitstr{anno=Anno}) ->
                     member(sequential_match, Anno)
             end, Segments) of
        true ->
            case keymember(binary_var, 1, Anno0) of
                false ->
                    {BinVar,St1} = new_var(St0),
                    Bin = Bin0#c_binary{segments=Bef},
                    P = #c_alias{var=BinVar,pat=Bin},
                    Anno = [{binary_var,BinVar}|Anno0],
                    {P,Anno,St1};
                true ->
                    Anno = keydelete(binary_var, 1, Anno0),
                    Bin = Bin0#c_binary{anno=Anno,segments=Bef},
                    {Bin,Anno0,St0}
            end;
        false ->
            Bin = Bin0#c_binary{segments=Bef},
            {Bin,Anno0,St0}
    end.

split_map_pat([#c_map_pair{key=Key,val=Val}=E0|Es], Map0, St0, Acc) ->
    case eval_map_key(Key, E0, Es, Map0, St0) of
        none ->
            case split_pat(Val, St0) of
                none ->
                    split_map_pat(Es, Map0, St0, [E0|Acc]);
                {Ps,Split,St1} ->
                    {Var,St} = new_var(St1),
                    E = E0#c_map_pair{val=Var},
                    Map = Map0#c_map{es=reverse(Acc, [E|Es])},
                    {Map,{split,[Var],Ps,Split},St}
            end;
        {MapVar,Split,St1} ->
            BefMap0 = Map0#c_map{es=reverse(Acc)},
            BefMap = #c_alias{var=MapVar,pat=BefMap0},
            {BefMap,Split,St1}
    end;
split_map_pat([], _, _, _) -> none.

eval_map_key(#c_var{}, _E, _Es, _Map, _St) ->
    none;
eval_map_key(#c_literal{}, _E, _Es, _Map, _St) ->
    none;
eval_map_key(Key, E0, Es, Map, St0) ->
    {[KeyVar,MapVar],St1} = new_vars(2, St0),
    E = E0#c_map_pair{key=KeyVar},
    AftMap0 = Map#c_map{es=[E|Es]},
    {Wrap,CaseArg,AftMap,St2} = wrap_map_key_fun(Key, KeyVar, MapVar, AftMap0, St1),
    {MapVar,{split,[CaseArg],Wrap,AftMap,nil},St2}.

wrap_map_key_fun(Key, KeyVar, MapVar, AftMap, St0) ->
    case is_safe(Key) of
        true ->
            {fun(Body) ->
                     #c_let{vars=[KeyVar],arg=Key,body=Body}
             end,MapVar,AftMap,St0};
        false ->
            {[SuccVar|Evars],St} = new_vars(4, St0),
            {fun(Body) ->
                     Try = #c_try{arg=Key,vars=[KeyVar],
                                  body=#c_values{es=[#c_literal{val=true},KeyVar]},
                                  evars=Evars,
                                  handler=#c_values{es=[#c_literal{val=false},
                                                        #c_literal{val=false}]}},
                     #c_let{vars=[SuccVar,KeyVar],arg=Try,body=Body}
             end,
             #c_tuple{es=[SuccVar,MapVar]},
             #c_tuple{es=[#c_literal{val=true},AftMap]},
             St}
    end.

split_data([E|Es0], Type, St0, Acc) ->
    case split_pat(E, St0) of
        none ->
            split_data(Es0, Type, St0, [E|Acc]);
        {Ps,Split,St1} ->
            {Var,St} = new_var(St1),
            Data = cerl:make_data(Type, reverse(Acc, [Var|Es0])),
            {Data,{split,[Var],Ps,Split},St}
    end;
split_data([], _, _, _) -> none.

split_bin_segments([#c_bitstr{anno=Anno0}=S0|Segs], Vars, St, Acc) ->
    case member(sequential_match, Anno0) of
        true ->
            Anno = Anno0 -- [sequential_match],
            S = S0#c_bitstr{anno=Anno},
            {sequential_match,reverse(Acc),[S|Segs],St};
        false ->
            split_bin_segments_1(S0, Segs, Vars, St, Acc)
    end;
split_bin_segments(_, _, _, _) ->
    none.

split_bin_segments_1(#c_bitstr{val=Val,size=Size}=S0, Segs, Vars0, St0, Acc) ->
    Vars = case Val of
               #c_var{name=V} -> gb_sets:add(V, Vars0);
               _ -> Vars0
           end,
    case Size of
        #c_literal{} ->
            split_bin_segments(Segs, Vars, St0, [S0|Acc]);
        #c_var{name=SizeVar} ->
            case gb_sets:is_member(SizeVar, Vars0) of
                true ->
                    %% The size variable is variable previously bound
                    %% in this same segment. Split the clause here to
                    %% avoid a variable that is both defined and used
                    %% in the same pattern.
                    {TailVar,Tail,St} = split_tail_seg(S0, Segs, St0),
                    Wrap = fun(Body) -> Body end,
                    {size_var,TailVar,Wrap,reverse(Acc, [Tail]),[S0|Segs],St};
                false ->
                    split_bin_segments(Segs, Vars, St0, [S0|Acc])
            end;
        _ ->
            %% The size is an expression. Split the clause here,
            %% calculate the expression in a try/catch, and finally
            %% continue the match in an inner case.
            {TailVar,Tail,St1} = split_tail_seg(S0, Segs, St0),
            {SizeVar,St2} = new_var(St1),
            S = S0#c_bitstr{size=SizeVar},
            {Wrap,St3} = split_wrap(SizeVar, Size, St2),
            {size_var,TailVar,Wrap,reverse(Acc, [Tail]),[S|Segs],St3}
    end.

split_tail_seg(#c_bitstr{anno=A}=S, Segs, St0) ->
    {TailVar,St} = new_var(St0),
    Unit = split_bin_unit([S|Segs], St0),
    {TailVar,
     #c_bitstr{anno=A,val=TailVar,
               size=#c_literal{val=all},
               unit=#c_literal{val=Unit},
               type=#c_literal{val=binary},
               flags=#c_literal{val=[unsigned,big]}},
    St}.

split_wrap(SizeVar, SizeExpr, St0) ->
    {Evars,St1} = new_vars(3, St0),
    {fun(Body) ->
             Try = #c_try{arg=SizeExpr,vars=[SizeVar],body=SizeVar,
                          evars=Evars,handler=#c_literal{val=bad_size}},
             #c_let{vars=[SizeVar],arg=Try,body=Body}
     end,St1}.

split_bin_unit(Ss, #core{dialyzer=Dialyzer}) ->
    case Dialyzer of
        true ->
            %% When a binary match has been rewritten to a nested
            %% case like this:
            %%
            %%    case Bin of
            %%      <<Size:32,Tail:Size/bitstring-unit:1>> ->
            %%         case Tail of
            %%            <<Result/binary-unit:8>> -> Result;
            %%         ...
            %%    end
            %%
            %% dialyzer will determine the type of Bin based solely on
            %% the binary pattern in the outer case. It will not
            %% back-propagate any type information for Tail to Bin. For
            %% this example, dialyzer would infer the type of Bin to
            %% be <<_:8,_:_*1>>.
            %%
            %% Help dialyzer to infer a better type by calculating the
            %% greatest common unit for the segments in the inner case
            %% expression. For this example, the greatest common unit
            %% for the pattern in the inner case is 8; it will allow
            %% dialyzer to infer the type for Bin to be
            %% <<_:32,_:_*8>>.

            split_bin_unit_1(Ss, 0);
        false ->
            %% Return the unit for pattern in the outer case that
            %% results in the best code.

            1
    end.

split_bin_unit_1([#c_bitstr{type=#c_literal{val=Type},size=Size,
                            unit=#c_literal{val=U}}|Ss],
                 GCU) ->
    Bits = case {Type,Size} of
               {utf8,_} -> 8;
               {utf16,_} -> 16;
               {utf32,_} -> 32;
               {_,#c_literal{val=0}} -> 1;
               {_,#c_literal{val=Sz}} when is_integer(Sz) -> Sz * U;
               {_,_} -> U
           end,
    split_bin_unit_1(Ss, gcd(GCU, Bits));
split_bin_unit_1([], GCU) -> GCU.

gcd(A, B) ->
    case A rem B of
        0 -> B;
        X -> gcd(B, X)
    end.

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

record_anno(L, #core{dialyzer=Dialyzer}=St) ->
    case erl_anno:record(L) andalso Dialyzer of
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
    Location = erl_anno:location(L),
    Generated = erl_anno:generated(L),
    CompilerGenerated = [compiler_generated || Generated],
    [Location] ++ St#core.file ++ CompilerGenerated.

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

insert_nif_start([VF={V,F=#c_fun{body=Body}}|Funs]) ->
    case Body of
        #c_seq{arg=#c_primop{name=#c_literal{val=nif_start}}} ->
            [VF|insert_nif_start(Funs)];
        #c_case{} ->
            NifStart = #c_primop{name=#c_literal{val=nif_start},args=[]},
            [{V,F#c_fun{body=#c_seq{arg=NifStart,body=Body}}}
            |insert_nif_start(Funs)];
        #c_letrec{defs=Defs,body=LetrecBody0}=LR0 ->
            NifStart = #c_primop{name=#c_literal{val=nif_start},args=[]},
            LetrecBody = #c_seq{arg=NifStart,body=LetrecBody0},
            LR = LR0#c_letrec{defs=insert_nif_start(Defs), body=LetrecBody},
            [{V,F#c_fun{body=LR}}|insert_nif_start(Funs)]
    end;
insert_nif_start([]) ->
    [].

%%%
%%% Handling of warnings.
%%%

-type err_desc() :: {'failed' | 'nomatch' | 'ignored', term()} |
                    {'map_key_repeated',term()}.

-spec format_error(err_desc()) -> nonempty_string().

format_error({nomatch,pattern}) ->
    "pattern cannot possibly match";
format_error({failed,bad_binary}) ->
    "binary construction will fail because of a type mismatch";
format_error({map_key_repeated,Key}) ->
    %% This warning does not fit neatly into any category.
    if
        is_atom(Key) ->
            io_lib:format("key '~w' will be overridden in expression", [Key]);
        true ->
            io_lib:format("key ~p will be overridden in expression", [Key])
    end.

add_warning(Anno, Term, #core{ws=Ws,file=[{file,File}]}=St) ->
    case erl_anno:generated(Anno) of
        false ->
            St#core{ws=[{File,[{erl_anno:location(Anno),?MODULE,Term}]}|Ws]};
        true ->
            St
    end.
