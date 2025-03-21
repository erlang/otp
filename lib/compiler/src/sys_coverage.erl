%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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
%% Purpose: Instrument abstract code for coverage.

-module(sys_coverage).
-moduledoc false.
-export([module/2,cover_transform/2,beam_debug_info/1]).
-import(lists, [duplicate/2,member/2,reverse/1,reverse/2]).

-type attribute() :: atom().
-type form()      :: {function, integer(), atom(), arity(), _}
                   | {attribute, integer(), attribute(), _}.
-type index_fun() :: fun((module(), atom(), arity(),
                          non_neg_integer(), non_neg_integer()) ->
                                non_neg_integer()).

-spec module([form()], [compile:option()]) ->
        {'ok',[form()]}.

module(Forms, _Opts) when is_list(Forms) ->
    transform(Forms, executable_line).

%% Undocumented helper function for the `cover` module.
-spec cover_transform([form()], index_fun()) ->
          {'ok',[form()]}.

cover_transform(Forms, IndexFun) when is_list(Forms),
                                      is_function(IndexFun, 5) ->
    transform(Forms, IndexFun, executable_line).

%% Undocumented helper function for inserting `debug_line` instructions.

-spec beam_debug_info([form()]) -> {'ok',[form()]}.

beam_debug_info(Forms) when is_list(Forms) ->
    transform(Forms, debug_line).

%%%
%%% Local functions.
%%%

%% Line doesn't matter.
-define(BLOCK(Expr), {block,erl_anno:new(0),[Expr]}).
-define(BLOCK1(Expr),
        if
            element(1, Expr) =:= block ->
                Expr;
            true ->
                ?BLOCK(Expr)
        end).

-type bump_instruction() :: 'executable_line' | 'debug_line'.

-record(vars,
        {module=[]      :: module() | [],
         function=none  :: atom(),
         arity=0        :: arity(),
         clause=0       :: non_neg_integer(),
         lines=[]       :: [non_neg_integer()],
         bump_lines=[]  :: [non_neg_integer()],
         in_guard=false :: boolean(),
         index_fun      :: index_fun(),
         bump_instr     :: bump_instruction()
        }).

transform(Forms, BumpInstr) ->
    put(bump_index, 1),
    GetIndex = fun(_, _, _, _, _) ->
                       Index = get(bump_index),
                       put(bump_index, Index + 1),
                       Index
               end,
    Result = transform(Forms, GetIndex, BumpInstr),
    erase(bump_index),
    Result.

transform(Code, IndexFun, BumpInstr) ->
    Vars = #vars{index_fun=IndexFun,bump_instr=BumpInstr},
    transform(Code, [], Vars, none, on).

transform([Form0|Forms], MungedForms, Vars0, MainFile0, Switch0) ->
    Form = expand(Form0),
    case munge(Form, Vars0, MainFile0, Switch0) of
        ignore ->
            transform(Forms, MungedForms, Vars0, MainFile0, Switch0);
        {MungedForm, Vars, Switch} ->
            transform(Forms, [MungedForm|MungedForms], Vars, MainFile0, Switch);
        {MungedForm,Vars,MainFile,Switch} ->
            transform(Forms, [MungedForm|MungedForms], Vars, MainFile, Switch)
    end;
transform([], MungedForms, _Vars, _, _) ->
    {ok, reverse(MungedForms)}.

%% Expand short-circuit Boolean expressions.
expand(Expr0) ->
    AllVars = sets:from_list(ordsets:to_list(vars([], Expr0)), [{version,2}]),
    {Expr,_} = expand(Expr0, AllVars, 1),
    Expr.

expand({clause,Anno,Pattern,Guards,Body}, Vs, N) ->
    %% We must not expand andalso/orelse in guards.
    {ExpandedBody,N2} = expand(Body, Vs, N),
    {{clause,Anno,Pattern,Guards,ExpandedBody},N2};
expand({lc,Anno,Expr,Qs}, Vs, N) ->
    {ExpandedExpr,N2} = expand(Expr, Vs, N),
    {ExpandedQs,N3} = expand_qualifiers(Qs, Vs, N2),
    {{lc,Anno,ExpandedExpr,ExpandedQs},N3};
expand({bc,Anno,Expr,Qs}, Vs, N) ->
    {ExpandedExpr,N2} = expand(Expr, Vs, N),
    {ExpandedQs,N3} = expand_qualifiers(Qs, Vs, N2),
    {{bc,Anno,ExpandedExpr,ExpandedQs},N3};
expand({mc,Anno,Expr,Qs}, Vs, N) ->
    {ExpandedExpr,N2} = expand(Expr, Vs, N),
    {ExpandedQs,N3} = expand_qualifiers(Qs, Vs, N2),
    {{mc,Anno,ExpandedExpr,ExpandedQs},N3};
expand({op,_Anno,'andalso',ExprL,ExprR}, Vs, N) ->
    {ExpandedExprL,N2} = expand(ExprL, Vs, N),
    {ExpandedExprR,N3} = expand(ExprR, Vs, N2),
    Anno = element(2, ExpandedExprL),
    {bool_switch(ExpandedExprL,
                 ExpandedExprR,
                 {atom,Anno,false},
                 Vs, N3),
     N3 + 1};
expand({op,_Anno,'orelse',ExprL,ExprR}, Vs, N) ->
    {ExpandedExprL,N2} = expand(ExprL, Vs, N),
    {ExpandedExprR,N3} = expand(ExprR, Vs, N2),
    Anno = element(2, ExpandedExprL),
    {bool_switch(ExpandedExprL,
                 {atom,Anno,true},
                 ExpandedExprR,
                 Vs, N3),
     N3 + 1};
expand(T, Vs, N) when is_tuple(T) ->
    {TL,N2} = expand(tuple_to_list(T), Vs, N),
    {list_to_tuple(TL),N2};
expand([E|Es], Vs, N) ->
    {E2,N2} = expand(E, Vs, N),
    {Es2,N3} = expand(Es, Vs, N2),
    {[E2|Es2],N3};
expand(T, _Vs, N) ->
    {T,N}.

expand_qualifiers([Q|Qs], Vs, N) ->
    {Q2,N2} = case erl_lint:is_guard_test(Q) of
                  true ->
                      %% This qualifier is a guard test and will be
                      %% compiled as such. Don't expand andalso/orelse
                      %% because that would turn it into a body
                      %% expression that may raise an exception. Here
                      %% is an example of a filter where the error
                      %% behaviour would change:
                      %%
                      %%      V == a orelse element(1, V) == a
                      %%
                      {Q,N};
                  false ->
                      %% A generator or a filter that is not a guard
                      %% test.
                      expand(Q, Vs, N)
              end,
    {Qs2,N3} = expand_qualifiers(Qs, Vs, N2),
    {[Q2|Qs2],N3};
expand_qualifiers([], _Vs, N) ->
    {[],N}.

vars(A, {var,_,V}) when V =/= '_' ->
    [V|A];
vars(A, T) when is_tuple(T) ->
    vars(A, tuple_to_list(T));
vars(A, [E|Es]) ->
    vars(vars(A, E), Es);
vars(A, _T) ->
    A.

bool_switch(E, T, F, AllVars, AuxVarN) ->
    Anno = element(2, E),
    AuxVar = {var,Anno,aux_var(AllVars, AuxVarN)},
    {'case',Anno,E,
     [{clause,Anno,[{atom,Anno,true}],[],[T]},
      {clause,Anno,[{atom,Anno,false}],[],[F]},
      %% Mark the next clause as compiler-generated to suppress
      %% a warning if the case expression is an obvious boolean
      %% value.
      {clause,erl_anno:set_generated(true, Anno),[AuxVar],[],
       [{call,Anno,
         {remote,Anno,{atom,Anno,erlang},{atom,Anno,error}},
         [{tuple,Anno,[{atom,Anno,badarg},AuxVar]}]}]}]}.

aux_var(Vars, N) ->
    Name = list_to_atom(lists:concat(["cov", N])),
    case sets:is_element(Name, Vars) of
        true -> aux_var(Vars, N + 1);
        false -> Name
    end.

%% This code traverses the abstract code, stored as the abstract_code
%% chunk in the BEAM file, as described in absform(3). The switch is
%% turned off when we encounter other files than the main file. This
%% way we will be able to exclude functions defined in include files.

munge({attribute,_,file,{File,_}}=Form, Vars, none, _) ->
    {Form,Vars,File,on};
munge({attribute,_,module,Mod}=Form, Vars, MainFile, Switch) when is_atom(Mod) ->
    {Form,Vars#vars{module=Mod},MainFile,Switch};
munge({function,Anno,Function,Arity,Clauses0},
      #vars{bump_instr=debug_line}=Vars0, _MainFile, on) ->
    %% We want to insert a `debug_line` instruction at the beginning
    %% of the function before all clauses, but there is not really a
    %% way to express that. So we insert an extra clause before all
    %% other clauses. The v3_core pass will pick up the annotation and
    %% index from the `debug_line` instruction in the body of the
    %% clause and will then discard it.
    Vars = Vars0#vars{function=Function,
                      arity=Arity,
                      clause=1,
                      lines=[],
                      bump_lines=[]},
    Args = duplicate(Arity, {var,Anno,'_'}),
    FakeBif = {remote,Anno,{atom,Anno,fake},{atom,Anno,is_beam_bif_info}},
    Gs = [[{call,Anno,FakeBif,[]}]],
    Body = [{atom,Anno,ignore}],
    Clauses = [{clause,Anno,Args,Gs,Body}|Clauses0],
    MungedClauses = munge_fun_clauses(Clauses, Vars),
    {{function,Anno,Function,Arity,MungedClauses},Vars,on};
munge({function,Anno,Function,Arity,Clauses}, Vars0, _MainFile, on) ->
    Vars = Vars0#vars{function=Function,
                      arity=Arity,
                      clause=1,
                      lines=[],
                      bump_lines=[]},
    MungedClauses = munge_fun_clauses(Clauses, Vars),
    {{function,Anno,Function,Arity,MungedClauses},Vars,on};
munge({attribute,_,file,{MainFile,_}}=Form, Vars, MainFile, _Switch) ->
    {Form,Vars,on};                     % Switch on transformation!
munge({attribute,_,file,{_InclFile,_}}=Form, Vars, _MainFile, _Switch) ->
    {Form,Vars,off};                    % Switch off transformation!
munge({attribute,_,compile,{parse_transform,_}}, _, _, _) ->
    %% Don't want to run parse transforms more than once.
    ignore;
munge(Form, Vars, _MainFile, Switch) -> % Other attributes and skipped includes.
    {Form,Vars,Switch}.

munge_fun_clauses([Clause0|Clauses], #vars{clause=ClauseIndex}=Vars0) ->
    {clause,Anno,Pattern,Guards,Body} = Clause0,
    {MungedGuards,_} = munge_exprs(Guards, Vars0#vars{in_guard=true}),
    {MungedBody,_} = munge_body(Body, Vars0),
    Vars = Vars0#vars{clause=ClauseIndex + 1},
    Clause = {clause,Anno,Pattern,MungedGuards,MungedBody},
    [Clause|munge_fun_clauses(Clauses, Vars)];
munge_fun_clauses([], _Vars) -> [].

%% Munge clauses in case, if, maybe, receive, and try.
munge_clauses(Clauses, Vars) ->
    munge_clauses(Clauses, Vars, Vars#vars.lines, []).

munge_clauses([Clause|Clauses], Vars0, Lines0, MClauses) ->
    {clause,Anno,Pattern,Guards,Body} = Clause,
    {MungedGuards, _Vars} = munge_exprs(Guards, Vars0#vars{in_guard=true}),
    {MungedBody, Vars} = munge_body(Body, Vars0),
    NewBumps = new_bumps(Vars, Vars0),
    Lines = NewBumps ++ Lines0,
    munge_clauses(Clauses, Vars#vars{lines=Vars0#vars.lines},
                  Lines,
                  [{clause,Anno,Pattern,MungedGuards,MungedBody}|
                   MClauses]);
munge_clauses([], Vars, Lines, MungedClauses) ->
    {reverse(MungedClauses), Vars#vars{lines=Lines}}.

munge_body(Expr, Vars) ->
    munge_body(Expr, Vars, [], []).

munge_body([E0|Es], Vars0, Acc0, LastExprBumpLines) ->
    %% Here is the place to add an executable_line instruction.
    Line = erl_anno:line(element(2, E0)),
    Lines0 = Vars0#vars.lines,
    case member(Line, Lines0) of
	true ->
            %% There is already a bump at this line.
	    {E1,Vars1} = munge_expr(E0, Vars0),
            NewBumps = new_bumps(Vars1, Vars0),
            BumpLines = [Line|Vars0#vars.bump_lines],
            Vars2 = Vars1#vars{bump_lines=BumpLines},
            Acc1 = maybe_fix_last_expr(Acc0, Vars2, LastExprBumpLines),
            Acc = [E1|Acc1],
	    munge_body(Es, Vars2, Acc, NewBumps);
	false ->
            %% Put a bump at this line.
            Bump = bump_call(Vars0, Line),
	    Lines1 = [Line|Lines0],
	    {E1,Vars1} = munge_expr(E0, Vars0#vars{lines=Lines1}),
            NewBumps = new_bumps(Vars1, Vars0),
            BumpLines = subtract(Vars1#vars.bump_lines, NewBumps),
            Vars2 = Vars1#vars{bump_lines=BumpLines},
            Acc1 = maybe_fix_last_expr(Acc0, Vars2, LastExprBumpLines),
            Acc = [E1,Bump|Acc1],
	    munge_body(Es, Vars2, Acc, NewBumps)
    end;
munge_body([], Vars, Acc, _LastExprBumpLines) ->
    {reverse(Acc), Vars}.

%%% Fix last expression (OTP-8188). A typical example:
%%%
%%%  3:   case X of
%%%  4:       1 -> a; % Bump line 5 after "a" has been evaluated!
%%%  5:       2 -> b; 3 -> c end, F()
%%%
%%% Line 5 wasn't bumped just before "F()" since it was already bumped
%%% before "b" (and before "c") (one mustn't bump a line more than
%%% once in a single "evaluation"). The expression "case X ... end" is
%%% now traversed again ("fixed"), this time adding bumps of line 5
%%% where appropriate, in this case when X matches 1.
%%%
%%% This doesn't solve all problems with expressions on the same line,
%%% though. 'case' and 'try' are tricky. An example:
%%%
%%% 7:    case case X of 1 -> foo(); % ?
%%% 8:                   2 -> bar() end of a -> 1;
%%% 9:                                     b -> 2 end.
%%%
%%% If X matches 1 and foo() evaluates to a then line 8 should be
%%% bumped, but not if foo() evaluates to b. In other words, line 8
%%% cannot be bumped after "foo()" on line 7, so one has to bump line
%%% 8 before "begin 1 end". But if X matches 2 and bar evaluates to a
%%% then line 8 would be bumped twice (there has to be a bump before
%%% "bar()". It is like one would have to have two copies of the inner
%%% clauses, one for each outer clause. Maybe the munging should be
%%% done on some of the compiler's "lower level" format.
%%%
%%% 'fun' is also problematic since a bump inside the body "shadows"
%%% the rest of the line.

maybe_fix_last_expr(MungedExprs, Vars, LastExprBumpLines) ->
    case last_expr_needs_fixing(Vars, LastExprBumpLines) of
        {yes, Line} ->
            fix_last_expr(MungedExprs, Line, Vars);
        no ->
            MungedExprs
    end.

last_expr_needs_fixing(Vars, LastExprBumpLines) ->
    case common_elems(Vars#vars.bump_lines, LastExprBumpLines) of
        [Line] ->
            {yes, Line};
        _ ->
            no
    end.

fix_last_expr([MungedExpr|MungedExprs], Line, Vars) ->
    %% No need to update ?COVER_TABLE.
    Bump = bump_call(Vars, Line),
    [fix_expr(MungedExpr, Line, Bump)|MungedExprs].

fix_expr({'if',A,Clauses}, Line, Bump) ->
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'if',A,FixedClauses};
fix_expr({'case',A,Expr,Clauses}, Line, Bump) ->
    FixedExpr = fix_expr(Expr, Line, Bump),
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'case',A,FixedExpr,FixedClauses};
fix_expr({'receive',A,Clauses}, Line, Bump) ->
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    {'receive',A,FixedClauses};
fix_expr({'receive',A,Clauses,Expr,Body}, Line, Bump) ->
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    FixedExpr = fix_expr(Expr, Line, Bump),
    FixedBody = fix_expr(Body, Line, Bump),
    {'receive',A,FixedClauses,FixedExpr,FixedBody};
fix_expr({'try',A,Exprs,Clauses,CatchClauses,After}, Line, Bump) ->
    FixedExprs = fix_expr(Exprs, Line, Bump),
    FixedClauses = fix_clauses(Clauses, Line, Bump),
    FixedCatchClauses = fix_clauses(CatchClauses, Line, Bump),
    FixedAfter = fix_expr(After, Line, Bump),
    {'try',A,FixedExprs,FixedClauses,FixedCatchClauses,FixedAfter};
fix_expr([E | Es], Line, Bump) ->
    [fix_expr(E, Line, Bump) | fix_expr(Es, Line, Bump)];
fix_expr(T, Line, Bump) when is_tuple(T) ->
    list_to_tuple(fix_expr(tuple_to_list(T), Line, Bump));
fix_expr(E, _Line, _Bump) ->
    E.

fix_clauses([], _Line, _Bump) ->
    [];
fix_clauses(Cs, Line, Bump) ->
    case bumps_line(lists:last(Cs), Line, Bump) of
        true ->
            fix_cls(Cs, Line, Bump);
        false ->
            Cs
    end.

fix_cls([], _Line, _Bump) ->
    [];
fix_cls([Cl | Cls], Line, Bump) ->
    case bumps_line(Cl, Line, Bump) of
        true ->
            [fix_expr(C, Line, Bump) || C <- [Cl | Cls]];
        false ->
            {clause,CA,P,G,Body} = Cl,
            UniqueVarName = list_to_atom(lists:concat(["$cover$ ",Line])),
            A = erl_anno:new(0),
            V = {var,A,UniqueVarName},
            [Last|Rest] = reverse(Body),
            Body1 = reverse(Rest, [{match,A,V,Last},Bump,V]),
            [{clause,CA,P,G,Body1} | fix_cls(Cls, Line, Bump)]
    end.

bumps_line(E, L, Bump) ->
    try
        bumps_line1(E, L, Bump)
    catch
        throw:true ->
            true
    end.

bumps_line1({BumpInstr,Line,_}, Line, {BumpInstr,_,_}) ->
    throw(true);
bumps_line1([E | Es], Line, Bump) ->
    bumps_line1(E, Line, Bump),
    bumps_line1(Es, Line, Bump);
bumps_line1(T, Line, Bump) when is_tuple(T) ->
    bumps_line1(tuple_to_list(T), Line, Bump);
bumps_line1(_, _, _Bump) ->
    false.

%% Insert an executable_line instruction in the abstract code.
bump_call(Vars, Line) ->
    #vars{module=M,function=F,arity=A,clause=C,index_fun=GetIndex,
          bump_instr=BumpInstr} = Vars,
    Index = GetIndex(M, F, A, C, Line),
    {BumpInstr,Line,Index}.

%%% End of fix of last expression.

munge_expr({match,Anno,ExprL,ExprR}, Vars0) ->
    {MungedExprL, Vars1} = munge_expr(ExprL, Vars0),
    {MungedExprR, Vars2} = munge_expr(ExprR, Vars1),
    {{match,Anno,MungedExprL,MungedExprR}, Vars2};
munge_expr({maybe_match,Anno,ExprL,ExprR}, Vars0) ->
    {MungedExprL, Vars1} = munge_expr(ExprL, Vars0),
    {MungedExprR, Vars2} = munge_expr(ExprR, Vars1),
    {{maybe_match,Anno,MungedExprL,MungedExprR}, Vars2};
munge_expr({tuple,Anno,Exprs}, Vars0) ->
    {MungedExprs, Vars1} = munge_exprs(Exprs, Vars0),
    {{tuple,Anno,MungedExprs}, Vars1};
munge_expr({record,Anno,Name,Exprs}, Vars0) ->
    {MungedExprFields, Vars1} = munge_exprs(Exprs, Vars0),
    {{record,Anno,Name,MungedExprFields}, Vars1};
munge_expr({record,Anno,Arg,Name,Exprs}, Vars0) ->
    {MungedArg, Vars1} = munge_expr(Arg, Vars0),
    {MungedExprFields, Vars2} = munge_exprs(Exprs, Vars1),
    {{record,Anno,MungedArg,Name,MungedExprFields}, Vars2};
munge_expr({record_field,Anno,ExprL,ExprR}, Vars0) ->
    {MungedExprR, Vars1} = munge_expr(ExprR, Vars0),
    {{record_field,Anno,ExprL,MungedExprR}, Vars1};
munge_expr({map,Anno,Fields}, Vars0) ->
    {MungedFields, Vars1} = munge_exprs(Fields, Vars0),
    {{map,Anno,MungedFields}, Vars1};
munge_expr({map,Anno,Arg,Fields}, Vars0) ->
    {MungedArg, Vars1} = munge_expr(Arg, Vars0),
    {MungedFields, Vars2} = munge_exprs(Fields, Vars1),
    {{map,Anno,MungedArg,MungedFields}, Vars2};
munge_expr({map_field_assoc,Anno,Name,Value}, Vars0) ->
    {MungedName, Vars1} = munge_expr(Name, Vars0),
    {MungedValue, Vars2} = munge_expr(Value, Vars1),
    {{map_field_assoc,Anno,MungedName,MungedValue}, Vars2};
munge_expr({map_field_exact,Anno,Name,Value}, Vars0) ->
    {MungedName, Vars1} = munge_expr(Name, Vars0),
    {MungedValue, Vars2} = munge_expr(Value, Vars1),
    {{map_field_exact,Anno,MungedName,MungedValue}, Vars2};
munge_expr({cons,Anno,ExprH,ExprT}, Vars0) ->
    {MungedExprH, Vars1} = munge_expr(ExprH, Vars0),
    {MungedExprT, Vars2} = munge_expr(ExprT, Vars1),
    {{cons,Anno,MungedExprH,MungedExprT}, Vars2};
munge_expr({op,Anno,Op,ExprL,ExprR}, Vars0) ->
    {MungedExprL, Vars1} = munge_expr(ExprL, Vars0),
    {MungedExprR, Vars2} = munge_expr(ExprR, Vars1),
    {{op,Anno,Op,MungedExprL,MungedExprR}, Vars2};
munge_expr({op,Anno,Op,Expr}, Vars0) ->
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    {{op,Anno,Op,MungedExpr}, Vars1};
munge_expr({'catch',Anno,Expr}, Vars0) ->
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    {{'catch',Anno,MungedExpr}, Vars1};
munge_expr({call,Anno1,{remote,Anno2,ExprM,ExprF},Exprs}, Vars0) ->
    {MungedExprM, Vars1} = munge_expr(ExprM, Vars0),
    {MungedExprF, Vars2} = munge_expr(ExprF, Vars1),
    {MungedExprs, Vars3} = munge_args(Exprs, Vars2),
    {{call,Anno1,{remote,Anno2,MungedExprM,MungedExprF},MungedExprs}, Vars3};
munge_expr({call,Anno,Expr,Exprs}, Vars0) ->
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    {MungedExprs, Vars2} = munge_args(Exprs, Vars1),
    {{call,Anno,MungedExpr,MungedExprs}, Vars2};
munge_expr({lc,Anno,Expr,Qs}, Vars0) ->
    {MungedExpr, Vars1} = munge_expr(?BLOCK1(Expr), Vars0),
    {MungedQs, Vars2} = munge_qualifiers(Qs, Vars1),
    {{lc,Anno,MungedExpr,MungedQs}, Vars2};
munge_expr({bc,Anno,Expr,Qs}, Vars0) ->
    {MungedExpr,Vars1} = munge_expr(?BLOCK1(Expr), Vars0),
    {MungedQs, Vars2} = munge_qualifiers(Qs, Vars1),
    {{bc,Anno,MungedExpr,MungedQs}, Vars2};
munge_expr({mc,Anno,{map_field_assoc,FAnno,K,V},Qs}, Vars0) ->
    Expr = {map_field_assoc,FAnno,?BLOCK1(K),?BLOCK1(V)},
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    {MungedQs, Vars2} = munge_qualifiers(Qs, Vars1),
    {{mc,Anno,MungedExpr,MungedQs}, Vars2};
munge_expr({block,Anno,Body}, Vars0) ->
    {MungedBody, Vars1} = munge_body(Body, Vars0),
    {{block,Anno,MungedBody}, Vars1};
munge_expr({'if',Anno,Clauses}, Vars0) ->
    {MungedClauses,Vars1} = munge_clauses(Clauses, Vars0),
    {{'if',Anno,MungedClauses}, Vars1};
munge_expr({'case',Anno,Expr,Clauses}, Vars0) ->
    {MungedExpr,Vars1} = munge_expr(Expr, Vars0),
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars1),
    {{'case',Anno,MungedExpr,MungedClauses}, Vars2};
munge_expr({'receive',Anno,Clauses}, Vars0) ->
    {MungedClauses,Vars1} = munge_clauses(Clauses, Vars0),
    {{'receive',Anno,MungedClauses}, Vars1};
munge_expr({'receive',Anno,Clauses,Expr,Body}, Vars0) ->
    {MungedExpr,Vars1} = munge_expr(Expr, Vars0),
    {MungedClauses,Vars2} = munge_clauses(Clauses, Vars1),
    {MungedBody,Vars3} = munge_body(Body, Vars2#vars{lines=Vars1#vars.lines}),
    Vars4 = Vars3#vars{lines=Vars2#vars.lines ++ new_bumps(Vars3, Vars2)},
    {{'receive',Anno,MungedClauses,MungedExpr,MungedBody}, Vars4};
munge_expr({'try',Anno,Body,Clauses,CatchClauses,After}, Vars0) ->
    {MungedBody, Vars01} = munge_body(Body, Vars0),
    {MungedClauses, Vars1} = munge_clauses(Clauses, Vars01),
    {MungedCatchClauses, Vars2} = munge_clauses(CatchClauses, Vars1),
    {MungedAfter, Vars3} = munge_body(After, Vars2),
    {{'try',Anno,MungedBody,MungedClauses,MungedCatchClauses,MungedAfter},
     Vars3};
munge_expr({'maybe',Anno,Exprs}, Vars0) ->
    {MungedExprs, Vars1} = munge_body(Exprs, Vars0),
    {{'maybe',Anno,MungedExprs}, Vars1};
munge_expr({'maybe',MaybeAnno,Exprs,{'else',ElseAnno,Clauses}}, Vars0) ->
    {MungedExprs, Vars1} = munge_body(Exprs, Vars0),
    {MungedClauses, Vars2} = munge_clauses(Clauses, Vars1),
    {{'maybe',MaybeAnno,MungedExprs,{'else',ElseAnno,MungedClauses}}, Vars2};
munge_expr({'fun',Anno,{clauses,Clauses}}, Vars0) ->
    {MungedClauses, Vars1} = munge_fun(Anno, Clauses, Vars0),
    {{'fun',Anno,{clauses,MungedClauses}}, Vars1};
munge_expr({named_fun,Anno,Name,Clauses}, Vars0) ->
    {MungedClauses, Vars1} = munge_fun(Anno, Clauses, Vars0),
    {{named_fun,Anno,Name,MungedClauses}, Vars1};
munge_expr({bin,Anno,BinElements}, Vars0) ->
    {MungedBinElements,Vars1} = munge_exprs(BinElements, Vars0),
    {{bin,Anno,MungedBinElements}, Vars1};
munge_expr({bin_element,Anno,Value,Size,TypeSpecifierList}, Vars0) ->
    {MungedValue,Vars1} = munge_expr(Value, Vars0),
    {MungedSize,Vars2} = munge_expr(Size, Vars1),
    {{bin_element,Anno,MungedValue,MungedSize,TypeSpecifierList},Vars2};
munge_expr(Form, Vars0) ->
    {Form, Vars0}.

munge_fun(Anno, Clauses, #vars{bump_instr=debug_line}=Vars0) ->
    [{clause,_,Args0,_,_}|_] = Clauses,
    Arity = length(Args0),
    Args = duplicate(Arity, {var,Anno,'_'}),
    FakeBif = {remote,Anno,{atom,Anno,fake},{atom,Anno,is_beam_bif_info}},
    Gs = [[{call,Anno,FakeBif,[]}]],
    Bump = bump_call(Vars0, Anno),
    Body = [Bump],
    C = {clause,Anno,Args,Gs,Body},
    {MungedClauses0,Vars1} = munge_clauses(Clauses, Vars0),
    MungedClauses = [C|MungedClauses0],
    {MungedClauses,Vars1};
munge_fun(_Anno, Clauses, Vars) ->
    munge_clauses(Clauses, Vars).

munge_args(Args0, #vars{in_guard=false,bump_instr=debug_line}=Vars) ->
    %% We want to have `debug_line` instructions inserted before each line in
    %% this example:
    %%
    %% bar:f(
    %%     bar:g(X),
    %%     bar:h(X)).
    Args = [case is_atomic(Arg) of
                true -> Arg;
                false -> ?BLOCK(Arg)
            end || Arg <- Args0],
    munge_exprs(Args, Vars);
munge_args(Args, Vars) ->
    munge_exprs(Args, Vars).

is_atomic({atom,_,_}) -> true;
is_atomic({float,_,_}) -> true;
is_atomic({integer,_,_}) -> true;
is_atomic({nil,_}) -> true;
is_atomic({var,_,_}) -> true;
is_atomic(_) -> false.

munge_exprs(Exprs, Vars) ->
    munge_exprs(Exprs, Vars, []).

munge_exprs([Expr|Exprs], #vars{in_guard=true}=Vars0, MungedExprs)
  when is_list(Expr) ->
    {MungedExpr, _Vars0} = munge_exprs(Expr, Vars0),
    munge_exprs(Exprs, Vars0, [MungedExpr|MungedExprs]);
munge_exprs([Expr|Exprs], Vars0, MungedExprs) ->
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_exprs(Exprs, Vars1, [MungedExpr|MungedExprs]);
munge_exprs([], Vars0, MungedExprs) ->
    {reverse(MungedExprs), Vars0}.

%% Every qualifier is decorated with a counter.
munge_qualifiers(Qualifiers, Vars) ->
    munge_qs(Qualifiers, Vars, []).

munge_qs([{generate,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {generate,Anno,Pattern,MungedExpr}, Vars0, Vars1, MQs);
munge_qs([{generate_strict,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {generate_strict,Anno,Pattern,MungedExpr}, Vars0, Vars1, MQs);
munge_qs([{b_generate,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {b_generate,Anno,Pattern,MExpr}, Vars0, Vars1, MQs);
munge_qs([{b_generate_strict,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {b_generate_strict,Anno,Pattern,MExpr}, Vars0, Vars1, MQs);
munge_qs([{m_generate,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {m_generate,Anno,Pattern,MExpr}, Vars0, Vars1, MQs);
munge_qs([{m_generate_strict,Anno,Pattern,Expr}|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, {m_generate_strict,Anno,Pattern,MExpr}, Vars0, Vars1, MQs);
munge_qs([{zip,Anno,Gs0}|Qs], Vars0, MQs) ->
    {Gs, Pre, Vars1} = munge_zip(Gs0, Vars0),
    munge_qs1(Qs, Anno, {zip,Anno,Gs}, Vars0, Vars1, Pre ++ MQs);
munge_qs([Expr|Qs], Vars0, MQs) ->
    A = element(2, Expr),
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    munge_qs1(Qs, A, MungedExpr, Vars0, Vars1, MQs);
munge_qs([], Vars0, MQs) ->
    {reverse(MQs), Vars0}.

munge_zip([G0|Gs0], Vars0) ->
    {Gen,Anno,Pattern,Expr} = G0,
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    G1 = {Gen,Anno,Pattern,MungedExpr},
    case munge_qs1([], Anno, G1, Vars0, Vars1, []) of
        {[{block,_,_}=Blk,G], Vars2} ->
            {Gs, Vars3} = munge_zip_1(Gs0, Vars2, [G]),
            {Gs, [Blk], Vars3};
        {[G], Vars2} ->
            {Gs, Vars3} = munge_zip_1(Gs0, Vars2, [G]),
            {Gs, [], Vars3}
    end.

munge_zip_1([G0|Gs], Vars0, Acc) ->
    {Gen,Anno,Pattern,Expr} = G0,
    {MungedExpr, Vars1} = munge_expr(Expr, Vars0),
    G1 = {Gen,Anno,Pattern,MungedExpr},
    munge_zip_1(Gs, Vars1, [G1|Acc]);
munge_zip_1([], Vars, Acc) ->
    {reverse(Acc), Vars}.

munge_qs1(Qs, Anno, NQ, Vars0, Vars1, MQs) ->
    case new_bumps(Vars1, Vars0) of
        [_] ->
            munge_qs(Qs, Vars1, [NQ | MQs]);
        _ ->
            {MungedTrue, Vars2} = munge_expr(?BLOCK({atom,Anno,true}), Vars1),
            munge_qs(Qs, Vars2, [NQ, MungedTrue | MQs])
    end.

new_bumps(#vars{lines=New}, #vars{lines=Old}) ->
    subtract(New, Old).

subtract(L1, L2) ->
    [E || E <- L1, not member(E, L2)].

common_elems(L1, L2) ->
    [E || E <- L1, member(E, L2)].
