%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2011. All Rights Reserved.
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

-module(op_SUITE).

-include_lib("test_server/include/test_server.hrl").

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 bsl_bsr/1,logical/1,t_not/1,relop_simple/1,relop/1,complex_relop/1]).

-export([]).
-import(lists, [foldl/3,flatmap/2]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [bsl_bsr, logical, t_not, relop_simple, relop,
     complex_relop].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_testcase(Case, Config) when is_atom(Case), is_list(Config) ->
    Dog=?t:timetrap(?t:minutes(3)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    ?t:timetrap_cancel(Dog).

%% Test the bsl and bsr operators.
bsl_bsr(Config) when is_list(Config) ->
    Vs = [unvalue(V) || V <- [-16#8000009-2,-1,0,1,2,73,16#8000000,bad,[]]],
    Cases = [{Op,X,Y} || Op <- ['bsr','bsl'], X <- Vs, Y <- Vs],
    ?line run_test_module(Cases, false),
    {comment,integer_to_list(length(Cases)) ++ " cases"}.

logical(doc) -> "Test the logical operators and internal BIFs.";
logical(Config) when is_list(Config) ->
    Vs0 = [true,false,bad],
    Vs = [unvalue(V) || V <- Vs0],
    Cases = [{Op,X,Y} || Op <- ['and','or','xor'], X <- Vs, Y <- Vs],
    ?line run_test_module(Cases, false),
    {comment,integer_to_list(length(Cases)) ++ " cases"}.

t_not(doc) -> "Test the not operator and internal BIFs.";
t_not(Config) when is_list(Config) ->
    ?line Cases = [{'not',unvalue(V)} || V <- [true,false,42,bad]],
    ?line run_test_module(Cases, false),
    {comment,integer_to_list(length(Cases)) ++ " cases"}.

relop_simple(doc) -> "Test that simlpe relations between relation operators hold.";
relop_simple(Config) when is_list(Config) ->
    Big1 = 19738924729729787487784874,
    Big2 = 38374938373887374983978484,
    F1 = float(Big1),
    F2 = float(Big2),
    T1 = erlang:make_tuple(3,87),
    T2 = erlang:make_tuple(3,87),
    Terms = [-F2,Big2,-F1,-Big1,-33,-33.0,0,0.0,42,42.0,Big1,F1,Big2,F2,a,b,
	     {T1,a},{T2,b},[T1,Big1],[T2,Big2]],
    
    ?line Combos = [{V1,V2} || V1 <- Terms, V2 <- Terms],
    
    lists:foreach(fun({A,B}) -> relop_simple_do(A,B) end,
		  Combos),

    repeat(fun() -> Size = random:uniform(100),
		    Rnd1 = make_rand_term(Size),
		    {Rnd2,0} = clone_and_mutate(Rnd1, random:uniform(Size)),
		    relop_simple_do(Rnd1,Rnd2)
	   end,
	   1000),
    ok.

relop_simple_do(V1,V2) ->
    %%io:format("compare ~p\n   and  ~p\n",[V1,V2]),

    L = V1 < V2,
    ?line L = not (V1 >= V2),
    ?line L = V2 > V1,
    ?line L = not (V2 =< V1),

    G = V1 > V2,
    ?line G = not (V1 =< V2),
    ?line G = V2 < V1,
    ?line G = not (V2 >= V1),
    
    ID = V1 =:= V2,
    ?line ID = V2 =:= V1,
    ?line ID = not (V1 =/= V2),
    ?line ID = not (V2 =/= V1),
    
    EQ = V1 == V2,
    ?line EQ = V2 == V1,
    ?line EQ = not (V1 /= V2),
    ?line EQ = not (V2 /= V1),

    ?line case {L, EQ, ID, G, cmp_emu(V1,V2)} of
	      { true, false, false, false, -1} -> ok;
	      {false, true,  false, false,  0} -> ok;
	      {false, true,   true, false,  0} -> ok;
	      {false, false, false, true,  +1} -> ok
	  end.
    
%% Emulate internal "cmp"
cmp_emu(A,B) when is_tuple(A), is_tuple(B) ->
    SA = size(A),
    SB = size(B),
    if SA =:= SB -> cmp_emu(tuple_to_list(A),tuple_to_list(B));
       SA > SB -> +1;
       SA < SB -> -1
    end;
cmp_emu([A|TA],[B|TB]) ->
    case cmp_emu(A,B) of
	0   -> cmp_emu(TA,TB);
	CMP -> CMP
    end;
cmp_emu(A,B) ->
    %% We cheat and use real "cmp" for the primitive types.
    if A < B -> -1;
       A > B -> +1;
       true -> 0
    end.					              
    
make_rand_term(1) ->
    make_rand_term_single();
make_rand_term(Arity) ->
    case random:uniform(3) of
	1 ->
	    make_rand_list(Arity);
	2 ->
	    list_to_tuple(make_rand_list(Arity));
	3 ->
	    {Car,Rest} = make_rand_term_rand_size(Arity),
	    [Car|make_rand_term(Rest)]
    end.

make_rand_term_single() ->
    Range = 1 bsl random:uniform(200),
    case random:uniform(12) of
	1 -> random;
	2 -> uniform;
	3 -> random:uniform(Range) - (Range div 2);
	4 -> Range * (random:uniform() - 0.5);
	5 -> 0;
	6 -> 0.0;
	7 -> make_ref();
	8 -> self();
	9 -> term_to_binary(random:uniform(Range));
	10 -> fun(X) -> X*Range end; 
	11 -> fun(X) -> X/Range end;
	12 -> []
    end.	    

make_rand_term_rand_size(1) ->
    {make_rand_term(1), 0};
make_rand_term_rand_size(MaxArity) ->
    Arity = random:uniform(MaxArity-1),
    {make_rand_term(Arity), MaxArity-Arity}.

make_rand_list(0) -> [];
make_rand_list(Arity) ->
    {Term, Rest} = make_rand_term_rand_size(Arity),
    [Term | make_rand_list(Rest)].
	    

clone_and_mutate(Term, 0) ->
    {clone(Term), 0};
clone_and_mutate(_Term, 1) ->
    {Mutation, _} = make_rand_term_rand_size(10), % MUTATE!
    {Mutation, 0};
clone_and_mutate(Term, Cnt) when is_tuple(Term) ->
    {Clone,NewCnt} = clone_and_mutate(tuple_to_list(Term),Cnt),
    {my_list_to_tuple(Clone), NewCnt};
clone_and_mutate([Term|Tail], Cnt) ->
    {Car,Cnt1} = clone_and_mutate(Term,Cnt),
    {Cdr,Cnt2} = clone_and_mutate(Tail,Cnt1),
    {[Car | Cdr], Cnt2};
clone_and_mutate(Term, Cnt) ->
    {clone(Term), Cnt-1}.

clone(Term) ->
    binary_to_term(term_to_binary(Term)).

my_list_to_tuple(List) ->
    try list_to_tuple(List)
    catch
	error:badarg -> 
	    %%io:format("my_list_to_tuple got badarg exception.\n"),
	    list_to_tuple(purify_list(List))
    end.
	
purify_list(List) ->
    lists:reverse(purify_list(List, [])).
purify_list([], Acc) -> Acc;
purify_list([H|T], Acc) -> purify_list(T, [H|Acc]);
purify_list(Other, Acc) -> [Other|Acc].
    

relop(doc) -> "Test the relational operators and internal BIFs on literals.";
relop(Config) when is_list(Config) ->
    Big1 = -38374938373887374983978484,
    Big2 = 19738924729729787487784874,
    F1 = float(Big1),
    F2 = float(Big2),
    Vs0 = [a,b,-33,-33.0,0,0.0,42,42.0,Big1,Big2,F1,F2],
    ?line Vs = [unvalue(V) || V <- Vs0],
    Ops = ['==', '/=', '=:=', '=/=', '<', '=<', '>', '>='],
    ?line binop(Ops, Vs).

complex_relop(doc) ->
    "Test the relational operators and internal BIFs on lists and tuples.";
complex_relop(Config) when is_list(Config) ->
    Big = 99678557475484872464269855544643333,
    Float = float(Big),
    Vs0 = [an_atom,42.0,42,Big,Float],
    Vs = flatmap(fun(X) -> [unvalue({X}),unvalue([X])] end, Vs0),
    Ops = ['==', '/=', '=:=', '=/=', '<', '=<', '>', '>='],
    ?line binop(Ops, Vs).

binop(Ops, Vs) ->
    Run = fun(Op, N) -> ?line Cases = [{Op,V1,V2} || V1 <- Vs, V2 <- Vs],
			?line run_test_module(Cases, true),
			N + length(Cases) end,
    ?line NumCases = foldl(Run, 0, Ops),
    {comment,integer_to_list(NumCases) ++ " cases"}.
    
run_test_module(Cases, GuardsOk) ->
    ?line Es = [expr(C) || C <- Cases],
    ?line Ok = unvalue(ok),
    ?line Gts = case GuardsOk of
		    true ->
			Ges = [guard_expr(C) || C <- Cases],
			?line lists:foldr(fun guard_test/2, [Ok], Ges);
		    false ->
			[Ok]
		end,
    ?line Fun1 = make_function(guard_tests, Gts),
    ?line Bts = lists:foldr(fun body_test/2, [Ok], Es),
    ?line Fun2 = make_function(body_tests, Bts),
    ?line Bbts = lists:foldr(fun internal_bif/2, [Ok], Es),
    ?line Fun3 = make_function(bif_tests, Bbts),
    ?line Id = {function,1,id,1,[{clause,1,[{var,1,'I'}],[],[{var,1,'I'}]}]},
    ?line Module = make_module(op_tests, [Fun1,Fun2,Fun3,Id]),
    ?line lists:foreach(fun(F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Module),

    %% Compile, load, and run the generated module.

    Native = case ?t:is_native(?MODULE) of
		 true -> [native];
		 false -> []
	     end,
    ?line {ok,Mod,Code1} = compile:forms(Module, [time|Native]),
    ?line code:delete(Mod),
    ?line code:purge(Mod),
    ?line {module,Mod} = code:load_binary(Mod, Mod, Code1),
    ?line run_function(Mod, guard_tests),
    ?line run_function(Mod, body_tests),
    ?line run_function(Mod, bif_tests),

    ?line true = code:delete(Mod),
    ?line code:purge(Mod),

    ok.

expr({Op,X}) ->
    E = {op,1,Op,{call,1,{atom,1,id},[X]}},
    Res = eval([{op,1,Op,X}]),
    {E,{Op,X},Res};
expr({Op,X,Y}) ->
    E = {op,1,Op,{call,1,{atom,1,id},[X]},Y},
    Res = eval([{op,1,Op,X,Y}]),
    {E,{Op,value(X),value(Y)},Res}.

guard_expr({Op,X}) ->
    E = {op,1,Op,X},
    Res = eval([E]),
    {E,{Op,X},Res};
guard_expr({Op,X,Y}) ->
    E = {op,1,Op,X,Y},
    Res = eval([E]),
    {E,{Op,value(X),value(Y)},Res}.

run_function(Mod, Name) ->
    case catch Mod:Name() of
	{'EXIT',Reason} ->
	    io:format("~p", [get(last)]),
	    ?t:fail({'EXIT',Reason});
	_Other ->
	    ok
    end.
    
guard_test({E,Expr,Res}, Tail) ->
    True = unvalue(true),
    [save_term(Expr),
     {match,1,unvalue(Res),
      {'if',1,[{clause,1,[],[[E]],[True]},
	       {clause,1,[],[[True]],[unvalue(false)]}]}}|Tail].

body_test({E,Expr,{'EXIT',_}}, Tail) ->
    [save_term(Expr),
     {match,1,{tuple,1,[unvalue('EXIT'), {var,1,'_'}]},
      {'catch',1,E}}|Tail];
body_test({E,Expr,Res}, Tail) ->
    [save_term(Expr),
     {match,1,unvalue(Res),E}|Tail].

internal_bif({{op,_,Op,X},Expr,Res}, Tail) ->
    internal_bif(Op, [X], Expr, Res, Tail);
internal_bif({{op,_,Op,X,Y},Expr,Res}, Tail) ->
    internal_bif(Op, [X,Y], Expr, Res, Tail).

internal_bif(Op, Args, Expr, {'EXIT',_}, Tail) ->
    [save_term(Expr),
     {match,1,{tuple,1,[unvalue('EXIT'), {var,1,'_'}]},
      {'catch',1,{call,1,{remote,1,{atom,1,erlang},unvalue(Op)},Args}}}|Tail];
internal_bif(Op, Args, Expr, Res, Tail) ->
    [save_term(Expr),
     {match,1,unvalue(Res),
      {call,1,{remote,1,{atom,1,erlang},unvalue(Op)},Args}}|Tail].

save_term(Term) ->
    {call,1,
       {atom,1,put},
       [{atom,1,last},unvalue(Term)]}.

make_module(Name, Funcs) ->
    [{attribute,1,module,Name},
     {attribute,0,compile,export_all},
     {attribute,0,compile,[{hipe,[{regalloc,linear_scan}]}]} |
     Funcs ++ [{eof,0}]].

make_function(Name, Body) ->
    {function,1,Name,0,[{clause,1,[],[],Body}]}.
       
eval(E) ->
    ?line case catch erl_eval:exprs(E, []) of
	      {'EXIT',Reason} -> {'EXIT',Reason};
	      {value,Val,_Bs} -> Val
	  end.

unvalue(V) -> erl_parse:abstract(V).
    
value({nil,_}) -> [];
value({integer,_,X}) -> X;
value({string,_,X}) -> X;
value({float,_,X})   -> X;
value({atom,_,X})    -> X;
value({tuple,_,Es}) ->
    list_to_tuple(lists:map(fun(X) -> value(X) end, Es));
value({cons,_,H,T}) ->
    [value(H) | value(T)].

repeat(_, 0) -> ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).
