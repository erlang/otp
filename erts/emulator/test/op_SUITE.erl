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

-module(op_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0,
         bsl_bsr/1,logical/1,t_not/1,relop_simple/1,relop/1,
         complex_relop/1,unsafe_fusing/1,
         range_tests/1,combined_relops/1,typed_relop/1,
         term_equivalence/1]).

-import(lists, [foldl/3,flatmap/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() ->
    [bsl_bsr, logical, t_not, relop_simple, relop,
     complex_relop, unsafe_fusing, range_tests,
     combined_relops, typed_relop, term_equivalence].

%% Test the bsl and bsr operators.
bsl_bsr(Config) when is_list(Config) ->
    RawValues = [-16#8000009-2,-1,0,1,2,73,16#8000000,bad,[]],

    [bsl_bsr_const(V) || V <- RawValues],

    Vs = [unvalue(V) || V <- RawValues],
    %% Try to use less memory by splitting the cases

    Cases1 = [{Op,X,Y} || Op <- ['bsl'], X <- Vs, Y <- Vs],
    N1 = length(Cases1),
    run_test_module(Cases1, false),

    Cases2 = [{Op,X,Y} || Op <- ['bsr'], X <- Vs, Y <- Vs],
    N2 = length(Cases2),
    run_test_module(Cases2, false),

    {comment,integer_to_list(N1 + N2) ++ " cases"}.

%% Tests constant-argument optimizations in `bsl`/`bsr`
bsl_bsr_const(A) ->
    BSL = id('bsl'),
    BSR = id('bsr'),

    bsl_bsr_compare_results((catch erlang:BSL(1, A)), (catch 1 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(3, A)), (catch 3 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(7, A)), (catch 7 bsl A)),

    bsl_bsr_compare_results((catch erlang:BSL(A, 1)), (catch A bsl 1)),
    bsl_bsr_compare_results((catch erlang:BSL(A, 3)), (catch A bsl 3)),
    bsl_bsr_compare_results((catch erlang:BSL(A, 7)), (catch A bsl 7)),

    bsl_bsr_compare_results((catch erlang:BSL(-2, A)), (catch -2 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(-4, A)), (catch -4 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(-8, A)), (catch -8 bsl A)),

    bsl_bsr_compare_results((catch erlang:BSL(A, -2)), (catch A bsl -2)),
    bsl_bsr_compare_results((catch erlang:BSL(A, -4)), (catch A bsl -4)),
    bsl_bsr_compare_results((catch erlang:BSL(A, -8)), (catch A bsl -8)),

    bsl_bsr_compare_results((catch erlang:BSR(1, A)), (catch 1 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(3, A)), (catch 3 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(7, A)), (catch 7 bsr A)),

    bsl_bsr_compare_results((catch erlang:BSR(A, 1)), (catch A bsr 1)),
    bsl_bsr_compare_results((catch erlang:BSR(A, 3)), (catch A bsr 3)),
    bsl_bsr_compare_results((catch erlang:BSR(A, 7)), (catch A bsr 7)),

    bsl_bsr_compare_results((catch erlang:BSR(-2, A)), (catch -2 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(-4, A)), (catch -4 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(-8, A)), (catch -8 bsr A)),

    bsl_bsr_compare_results((catch erlang:BSR(A, -2)), (catch A bsr -2)),
    bsl_bsr_compare_results((catch erlang:BSR(A, -4)), (catch A bsr -4)),
    bsl_bsr_compare_results((catch erlang:BSR(A, -8)), (catch A bsr -8)),


    %% These numbers can be shifted left one or zero times while remaining a
    %% small on 32-bit platforms.
    %%
    %% The test relies on the compiler turning these into constants.
    HighEdge32 = (1 bsl (32 - 6)) - 1,
    LowEdge32 = -(1 bsl (32 - 6)),

    bsl_bsr_compare_results((catch erlang:BSL(HighEdge32, A)), (catch HighEdge32 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(LowEdge32, A)), (catch LowEdge32 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSR(HighEdge32, A)), (catch HighEdge32 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(LowEdge32, A)), (catch LowEdge32 bsr A)),

    HighEdge64 = (1 bsl (64 - 6)) - 1,
    LowEdge64 = -(1 bsl (64 - 6)),

    bsl_bsr_compare_results((catch erlang:BSL(HighEdge64, A)), (catch HighEdge64 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSL(LowEdge64, A)), (catch LowEdge64 bsl A)),
    bsl_bsr_compare_results((catch erlang:BSR(HighEdge64, A)), (catch HighEdge64 bsr A)),
    bsl_bsr_compare_results((catch erlang:BSR(LowEdge64, A)), (catch LowEdge64 bsr A)),

    ok.

bsl_bsr_compare_results(Same, Same) ->
    ok;
bsl_bsr_compare_results({'EXIT',{Reason,[_|_]}}, {'EXIT',{Reason,[_|_]}}) ->
    %% The applied and inlined implementations may differ in whether they include
    %% the operator as the top element of the stack.
    ok.

%% Test the logical operators and internal BIFs.
logical(Config) when is_list(Config) ->
    Vs0 = [true,false,bad],
    Vs = [unvalue(V) || V <- Vs0],
    Cases = [{Op,X,Y} || Op <- ['and','or','xor'], X <- Vs, Y <- Vs],
    run_test_module(Cases, false),
    {comment,integer_to_list(length(Cases)) ++ " cases"}.

%% Test the not operator and internal BIFs.
t_not(Config) when is_list(Config) ->
    Cases = [{'not',unvalue(V)} || V <- [true,false,42,bad]],
    run_test_module(Cases, false),
    {comment,integer_to_list(length(Cases)) ++ " cases"}.

%% Test that simlpe relations between relation operators hold.
relop_simple(Config) when is_list(Config) ->
    Big1 = 19738924729729787487784874,
    Big2 = 38374938373887374983978484,
    F1 = float(Big1),
    F2 = float(Big2),
    T1 = erlang:make_tuple(3,87),
    T2 = erlang:make_tuple(3,87),
    Terms = [-F2,Big2,-F1,-Big1,-33,-33.0,0,0.0,-0.0,42,42.0,Big1,F1,Big2,F2,
             a,b,{T1,a},{T2,b},[T1,Big1],[T2,Big2]],

    Combos = [{V1,V2} || V1 <- Terms, V2 <- Terms],

    lists:foreach(fun({A,B}) -> relop_simple_do(A,B) end,
                  Combos),

    repeat(fun() ->
                   Size = rand:uniform(100),
                   Rnd1 = make_rand_term(Size),
                   {Rnd2,0} = clone_and_mutate(Rnd1, rand:uniform(Size)),
                   relop_simple_do(Rnd1,Rnd2)
           end,
           1000),
    ok.

relop_simple_do(V1,V2) ->
    %%io:format("compare ~p\n   and  ~p\n",[V1,V2]),

    L = V1 < V2,
    L = not (V1 >= V2),
    L = V2 > V1,
    L = not (V2 =< V1),

    G = V1 > V2,
    G = not (V1 =< V2),
    G = V2 < V1,
    G = not (V2 >= V1),

    ID = V1 =:= V2,
    ID = V2 =:= V1,
    ID = not (V1 =/= V2),
    ID = not (V2 =/= V1),

    implies(ID, V1 == V2),

    EQ = V1 == V2,
    EQ = V2 == V1,
    EQ = not (V1 /= V2),
    EQ = not (V2 /= V1),

    case {L, EQ, ID, G, cmp_emu(V1,V2)} of
        { true, false, false, false, -1} -> ok;
        {false, true,  false, false,  0} -> ok;
        {false, true,   true, false,  0} -> ok;
        {false, false, false, true,  +1} -> ok
    end.

implies(false, _) -> ok;
implies(true, true) -> ok.

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
    case rand:uniform(3) of
        1 ->
            make_rand_list(Arity);
        2 ->
            list_to_tuple(make_rand_list(Arity));
        3 ->
            {Car,Rest} = make_rand_term_rand_size(Arity),
            [Car|make_rand_term(Rest)]
    end.

make_rand_term_single() ->
    Range = 1 bsl rand:uniform(200),
    case rand:uniform(12) of
        1 -> random;
        2 -> uniform;
        3 -> rand:uniform(Range) - (Range div 2);
        4 -> Range * (rand:uniform() - 0.5);
        5 -> 0;
        6 -> 0.0;
        7 -> make_ref();
        8 -> self();
        9 -> term_to_binary(rand:uniform(Range));
        10 -> fun(X) -> X*Range end; 
        11 -> fun(X) -> X/Range end;
        12 -> []
    end.	    

make_rand_term_rand_size(1) ->
    {make_rand_term(1), 0};
make_rand_term_rand_size(MaxArity) ->
    Arity = rand:uniform(MaxArity-1),
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


%% Test the relational operators and internal BIFs on literals.
relop(Config) when is_list(Config) ->
    Big1 = -38374938373887374983978484,
    Big2 = 19738924729729787487784874,
    F1 = float(Big1),
    F2 = float(Big2),
    Bin = <<"abc">>,
    BitString = <<0:7>>,
    Map = #{a => b},
    EmptyMap = #{},
    Vs0 = [a,b,-33,-33.0,0,0.0,-0,0,42,42.0,Big1,Big2,F1,F2,
           Bin,BitString,Map,EmptyMap,[16#1234_5678_abcd]],
    Vs = [unvalue(V) || V <- Vs0],
    Ops = ['==', '/=', '=:=', '=/=', '<', '=<', '>', '>='],
    binop(Ops, Vs).

%% Test the relational operators and internal BIFs on lists and tuples.
complex_relop(Config) when is_list(Config) ->
    Big = 99678557475484872464269855544643333,
    Float = float(Big),
    Bin = <<"abc">>,
    BitString = <<0:7>>,
    EmptyBitString = <<>>,
    Map = #{a => b},
    EmptyMap = #{},
    Vs0 = [an_atom,42.0,42,0.0,-0.0,Big,Float,Bin,BitString,
           EmptyBitString,Map,EmptyMap],
    Vs = flatmap(fun(X) -> [unvalue({X}),unvalue([X])] end, Vs0),
    Ops = ['==', '/=', '=:=', '=/=', '<', '=<', '>', '>='],
    binop(Ops, Vs).

binop(Ops, Vs) ->
    Run = fun(Op, N) -> Cases = [{Op,V1,V2} || V1 <- Vs, V2 <- Vs],
                        run_test_module(Cases, true),
                        N + length(Cases) end,
    NumCases = foldl(Run, 0, Ops),
    {comment,integer_to_list(NumCases) ++ " cases"}.

run_test_module(Cases, GuardsOk) ->
    Es = [expr(C) || C <- Cases],
    Ok = unvalue(ok),
    Gts = case GuardsOk of
              true ->
                  Ges = [guard_expr(C) || C <- Cases],
                  lists:foldr(fun guard_test/2, [Ok], Ges);
              false ->
                  [Ok]
          end,
    Fun1 = make_function(guard_tests, Gts),
    Bts = lists:foldr(fun body_test/2, [Ok], Es),
    Fun2 = make_function(body_tests, Bts),
    Bbts = lists:foldr(fun internal_bif/2, [Ok], Es),
    Fun3 = make_function(bif_tests, Bbts),
    Id = {function,1,id,1,[{clause,1,[{var,1,'I'}],[],[{var,1,'I'}]}]},
    Module0 = make_module(op_tests, [Fun1,Fun2,Fun3,Id]),
    Module = erl_parse:new_anno(Module0),
    lists:foreach(fun(F) -> io:put_chars([erl_pp:form(F),"\n"]) end, Module),

    %% Compile, load, and run the generated module. Test both with and
    %% without compiler optimizations to ensure that we test both the
    %% implementation of the BIFs and the BEAM instructions.
    do_run_test_module(Module, []),
    do_run_test_module(Module, [no_copt,no_ssa_opt,no_postopt]).

do_run_test_module(Module, Opts) ->
    {ok,Mod,Code1} = compile:forms(Module, [time|Opts]),
    _ = code:delete(Mod),
    _ = code:purge(Mod),

    {module,Mod} = code:load_binary(Mod, Mod, Code1),

    run_function(Mod, guard_tests),
    run_function(Mod, body_tests),
    run_function(Mod, bif_tests),

    true = code:delete(Mod),
    _ = code:purge(Mod),

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
            ct:fail({'EXIT',Reason});
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
     {attribute,0,compile,export_all} |
     Funcs ++ [{eof,0}]].

make_function(Name, Body) ->
    {function,1,Name,0,[{clause,1,[],[],Body}]}.

eval(E0) ->
    E = erl_parse:new_anno(E0),
    case catch erl_eval:exprs(E, []) of
        {'EXIT',Reason} -> {'EXIT',Reason};
        {value,Val,_Bs} -> Val
    end.

unsafe_fusing(_Config) ->
    0 = usec_to_seconds(id(1)),
    234_567 = usec_to_seconds(id(1_234_567_890*1_000)),
    ok.

usec_to_seconds(Usec) when is_integer(Usec) ->
    %% The introduction of typed operands caused the loader
    %% to incorrectly fuse the following instrutions because
    %% the result register ({x,0}) from the 'div' instruction
    %% seemed to be distinct from both operands of the 'rem'
    %% instruction:
    %%
    %% {gc_bif,'div',
    %%         {f,0},
    %%         1,
    %%         [{tr,{x,0},{t_integer,any}},{integer,1000000}],
    %%         {x,0}}.
    %% {gc_bif,'rem',
    %%         {f,0},
    %%         1,
    %%         [{tr,{x,0},{t_integer,any}},{integer,1000000}],
    %%         {x,0}}.
    Sec = Usec div 1000000,
    Sec rem 1000000.

range_tests(_Config) ->
    %% Define the limits for smalls on a 64-bit system.
    {MinSmall, MaxSmall} = {-1 bsl 59, (1 bsl 59) - 1},
    case erlang:system_info(wordsize) of
        8 ->
            %% Assertions.
            2 = erts_debug:flat_size(MinSmall-1),
            0 = erts_debug:flat_size(MinSmall),
            0 = erts_debug:flat_size(MaxSmall),
            2 = erts_debug:flat_size(MaxSmall+1);
        4 ->
            ok
    end,

    lesser = range(-1 bsl 64),
    lesser = range(MinSmall),
    lesser = range(0),
    lesser = range(-1),
    lesser = range(0.9999),

    inside = range_any(1),
    inside = range_any(2),
    inside = range_any(2.5),
    inside = range_any(math:pi()),
    inside = range_any(5),
    inside = range_any(9),
    inside = range_any(10),

    greater = range(10.0001),
    greater = range(11),
    greater = range(MaxSmall),
    greater = range(1 bsl 64),
    greater = range(atom),
    greater = range(self()),
    greater = range(make_ref()),
    greater = range({a,b}),
    greater = range([a,b]),
    greater = range([]),
    greater = range(<<1,2,3>>),
    greater = range(fun() -> ok end),
    greater = range(fun ?MODULE:range_tests/1),

    lesser = range(-1 bsl 64),
    lesser = range(float(-1 bsl 64)),
    lesser = range_big(MinSmall - 2),
    lesser = range_barely_small(MinSmall - 1),

    inside = range_barely_small(MinSmall),
    inside = range_barely_small(-1 bsl 58),
    inside = range_barely_small(0),
    inside = range_barely_small(17.75),
    inside = range_barely_small(1 bsl 58),
    inside = range_barely_small(MaxSmall),

    greater = range_barely_small(MaxSmall + 1),
    greater = range_big(MaxSmall + 2),
    greater = range_big(1 bsl 64),
    greater = range_big(float(1 bsl 64)),

    lesser = range(-1 bsl 64),
    lesser = range(float(-1 bsl 64)),
    lesser = range_big(MinSmall - 2),

    inside = range_big(MinSmall),
    inside = range_big(-1 bsl 58),
    inside = range_big(0),
    inside = range_big(17.75),
    inside = range_big(1 bsl 58),
    inside = range_big(MaxSmall),

    greater = range_big(MaxSmall + 2),
    greater = range_big(1 bsl 64),
    greater = range_big(float(1 bsl 64)),

    inside = int_range_1(id(-100_000)),
    inside = int_range_1(id(-10)),
    inside = int_range_1(id(100)),
    inside = int_range_1(id(100_000)),

    outside = int_range_1(id(atom)),
    outside = int_range_1(id(-1 bsl 60)),
    outside = int_range_1(id(-100_001)),
    outside = int_range_1(id(100_001)),
    outside = int_range_1(id(1 bsl 60)),

    inside = int_range_2(id(1)),
    inside = int_range_2(id(42)),
    inside = int_range_2(id(16#f000_0000)),

    outside = int_range_2(id([a,list])),
    outside = int_range_2(id(0)),
    outside = int_range_1(id(-1 bsl 60)),
    outside = int_range_1(id(1 bsl 60)),

    inside = int_range_3(id(1 bsl 28)),
    inside = int_range_3(id((1 bsl 28) + 1)),
    inside = int_range_3(id((1 bsl 33) + 555)),
    inside = int_range_3(id((1 bsl 58) - 1)),
    inside = int_range_3(id(1 bsl 58)),

    outside = int_range_3(id({a,tuple})),
    outside = int_range_3(id(-1 bsl 60)),
    outside = int_range_3(id(-1000)),
    outside = int_range_3(id(100)),
    outside = int_range_3(id((1 bsl 58) + 1)),
    outside = int_range_3(id(1 bsl 60)),

    ok.

range(X) ->
    Res = range_any(X),
    if
        is_integer(X) ->
            Res = range_any(float(X)),
            Res = range_number(X),
            Res = range_number(float(X)),
            Res = range_int(X),
            if
                X =:= X band 16#ffff ->
                    Res = range_small_int(X);
                true ->
                    Res
            end;
        is_number(X) ->
            Res = range_number(X);
        true ->
            Res = range_big(X),
            Res = range_barely_small(X)
    end.

range_any(X0) ->
    X = id(X0),
    case range_any_1(X) of
        inside ->
            inside = range_any_2(X);
        Other ->
            outside = range_any_2(X),
            Other
    end.

%% The guard tests have different failure labels.
range_any_1(X) when 1 =< X, X =< 10 ->
    inside;
range_any_1(X) when X < 1 ->
    lesser;
range_any_1(X) when X > 10 ->
    greater.

%% The guard tests have the same failure label.
range_any_2(X) when 1 =< X, X =< 10 ->
    inside;
range_any_2(_) ->
    outside.

range_number(X) when is_number(X) ->
    case range_number_1(X) of
        inside ->
            inside = range_number_2(X);
        Other ->
            outside = range_number_2(X),
            Other
    end.

range_number_1(X) when 1 =< X, X =< 10 ->
    inside;
range_number_1(X) when X < 1 ->
    lesser;
range_number_1(X) when X > 10 ->
    greater.

range_number_2(X) when 1 =< X, X =< 10 ->
    inside;
range_number_2(_) ->
    outside.

range_int(X) when is_integer(X) ->
    case range_int_1(X) of
        inside ->
            inside = range_int_2(X);
        Other ->
            outside = range_int_2(X),
            Other
    end.

range_int_1(X) when 1 =< X, X =< 10 ->
    inside;
range_int_1(X) when X < 1 ->
    lesser;
range_int_1(X) when X > 10 ->
    greater.

range_int_2(X) when 1 =< X, X =< 10 ->
    inside;
range_int_2(_) ->
    outside.

range_small_int(X) when is_integer(X) ->
    case range_small_int_1(X) of
        inside ->
            inside = range_small_int_2(X);
        Other ->
            outside = range_small_int_2(X),
            Other
    end.

range_small_int_1(X) when 1 =< X, X =< 10 ->
    inside;
range_small_int_1(X) when X < 1 ->
    lesser;
range_small_int_1(X) when X > 10 ->
    greater.

range_small_int_2(X) when 1 =< X, X =< 10 ->
    inside;
range_small_int_2(_) ->
    outside.

range_barely_small(X) ->
    case range_barely_small_1(X) of
        inside ->
            inside = range_barely_small_2(X);
        Other ->
            outside = range_barely_small_2(X),
            Other
    end.

range_barely_small_1(X) when -1 bsl 59 =< X, X =< (1 bsl 59) - 1 ->
    inside;
range_barely_small_1(X) when X < -1 bsl 59 ->
    lesser;
range_barely_small_1(X) when X > (1 bsl 59) - 1 ->
    greater.

range_barely_small_2(X) when -1 bsl 59 =< X, X =< (1 bsl 59) - 1 ->
    inside;
range_barely_small_2(_) ->
    outside.

range_big(X) ->
    case range_big_1(X) of
        inside ->
            inside = range_big_2(X);
        Other ->
            outside = range_big_2(X),
            Other
    end.

range_big_1(X) when (-1 bsl 59) - 1 =< X, X =< 1 bsl 59 ->
    inside;
range_big_1(X) when X < (-1 bsl 59) - 1 ->
    lesser;
range_big_1(X) when X > 1 bsl 59 ->
    greater.

range_big_2(X) when (-1 bsl 59) - 1 =< X, X =< 1 bsl 59 ->
    inside;
range_big_2(_) ->
    outside.

int_range_1(X) when is_integer(X), -100_000 =< X, X =< 100_000 ->
    inside;
int_range_1(_) ->
    outside.

int_range_2(X) when is_integer(X), 1 =< X, X =< 16#f000_0000 ->
    inside;
int_range_2(_) ->
    outside.

int_range_3(X) when is_integer(X), 1 bsl 28 =< X, X =< 1 bsl 58 ->
    inside;
int_range_3(_) ->
    outside.

combined_relops(_Config) ->
    other = test_tok_char(-1 bsl 64),
    other = test_tok_char($A - 1),

    var = test_tok_char($A),
    var = test_tok_char($B),
    var = test_tok_char($P),
    var = test_tok_char($Y),
    var = test_tok_char($Z),

    other = test_tok_char($Z + 1),

    var = tok_char($_),
    other = tok_char(float($_)),

    other = test_tok_char(1 bsl 64),

    other = test_tok_char(atom),
    other = test_tok_char(self()),

    %%
    b = ge_ge_int_range_1(-200),
    b = ge_ge_int_range_1(-101),

    a = ge_ge_int_range_1(-100),
    a = ge_ge_int_range_1(-50),
    a = ge_ge_int_range_1(-10),

    b = ge_ge_int_range_1(-9),
    b = ge_ge_int_range_1(-6),

    a = ge_ge_int_range_1(-5),

    b = ge_ge_int_range_1(-4),
    b = ge_ge_int_range_1(0),
    b = ge_ge_int_range_1(42),

    %%
    b = ge_ge_int_range_2(-1 bsl 59),

    a = ge_ge_int_range_2((-1 bsl 59) + 1),
    a = ge_ge_int_range_2(-1 bsl 58),
    a = ge_ge_int_range_2(-1000),
    a = ge_ge_int_range_2(1 bsl 58),
    a = ge_ge_int_range_2((1 bsl 59) - 10),

    b = ge_ge_int_range_2((1 bsl 59) - 9),

    a = ge_ge_int_range_2((1 bsl 59) - 5),

    b = ge_ge_int_range_2((1 bsl 59) - 4),
    b = ge_ge_int_range_2((1 bsl 59) - 1),

    %%
    b = ge_ge_int_range_3(-1 bsl 59),

    b = ge_ge_int_range_3((-1 bsl 59) + 1),
    b = ge_ge_int_range_3(-1 bsl 58),
    b = ge_ge_int_range_3(-1000),
    b = ge_ge_int_range_3(1 bsl 58),

    a = ge_ge_int_range_3((1 bsl 59) - 20),
    a = ge_ge_int_range_3((1 bsl 59) - 15),
    a = ge_ge_int_range_3((1 bsl 59) - 10),

    b = ge_ge_int_range_3((1 bsl 59) - 9),

    a = ge_ge_int_range_3((1 bsl 59) - 5),

    b = ge_ge_int_range_3((1 bsl 59) - 4),
    b = ge_ge_int_range_3((1 bsl 59) - 1),

    %%
    b = ge_ge_int_range_4(-1 bsl 59),

    a = ge_ge_int_range_4((-1 bsl 59) + 1),
    a = ge_ge_int_range_4((-1 bsl 59) + 3),
    a = ge_ge_int_range_4((-1 bsl 59) + 5),

    b = ge_ge_int_range_4((-1 bsl 59) + 6),
    b = ge_ge_int_range_4((-1 bsl 59) + 9),

    a = ge_ge_int_range_4((-1 bsl 59) + 10),

    b = ge_ge_int_range_4((-1 bsl 59) + 11),

    b = ge_ge_int_range_4(0),
    b = ge_ge_int_range_4(1000),

    b = ge_ge_int_range_4((1 bsl 59) - 1),

    %% Test a sequence that can't occur in optimized code:
    %%   is_ge Fail Src 10
    %%   is_ge Fail Src 5
    Module = {?FUNCTION_NAME,[{test,1}],[],
              [{function, test, 1, 2,
                [{label,1},
                 {line,[{location,"t.erl",4}]},
                 {func_info,{atom,?FUNCTION_NAME},{atom,test},1},
                 {label,2},
                 {test,is_ge,{f,4},
                  [{tr,{x,0},{t_integer,{0,1000}}},
                   {integer,10}]},
                 {test,is_ge,
                  {f,3},
                  [{tr,{x,0},{t_integer,{0,1000}}},
                   {integer,5}]},
                 {label,3},
                 {move,{atom,a},{x,0}},
                 return,
                 {label,4},
                 {move,{atom,b},{x,0}},
                 return]}],
              5},

    {ok,Mod,Code} = compile:forms(Module, [from_asm,time,report]),
    {module,Mod} = code:load_binary(Mod, Mod, Code),

    b = Mod:test(0),
    b = Mod:test(5),
    b = Mod:test(9),

    a = Mod:test(10),
    a = Mod:test(11),
    a = Mod:test(1000),

    true = code:delete(Mod),
    _ = code:purge(Mod),

    LargeMap = #{K => K || K <- lists:seq(1, 64)},
    Ref = make_ref(),
    [{}] = empty_map_or_nil(id({})),
    [{a,b}] = empty_map_or_nil(id({a,b})),
    [#{a := b}] = empty_map_or_nil(id(#{a => b})),
    [LargeMap] = empty_map_or_nil(id(LargeMap)),
    [Ref] = empty_map_or_nil(id(Ref)),
    [[a,b,c]] = empty_map_or_nil(id([a,b,c])),
    [a] = empty_map_or_nil(id(a)),
    [42] = empty_map_or_nil(id(42)),
    [] = empty_map_or_nil(id(#{})),
    [] = empty_map_or_nil(id([])),

    ok.

test_tok_char(C) ->
    Result = tok_char(C),
    if
        is_integer(C) ->
            Result = tok_char(float(C)),
            Result = tok_char_int(C),
            if
                C band 16#FFFF =:= C ->
                    Result = tok_char_int_range(C);
                true ->
                    Result
            end;
        true ->
            Result
    end.

%% is_ge + is_lt
tok_char(C) when $A =< C, C =< $Z ->
    var;
tok_char($_) ->
    var;
tok_char(_) ->
    other.

%% is_ge + is_ge
tok_char_int(C) when $A =< C, C =< $Z ->
    var;
tok_char_int($_) ->
    var;
tok_char_int(_) ->
    other.

%% is_ge + is_ge
tok_char_int_range(C) when $A =< C, C =< $Z ->
    var;
tok_char_int_range($_) ->
    var;
tok_char_int_range(_) ->
    other.

%% is_ge + is_ge
ge_ge_int_range_1(X) when -100 =< X, X =< -10 ->
    a;
ge_ge_int_range_1(-5) ->
    a;
ge_ge_int_range_1(_) ->
    b.

ge_ge_int_range_2(X) when (-1 bsl 59) + 1 =< X, X =< (1 bsl 59) - 10 ->
    a;
ge_ge_int_range_2((1 bsl 59) - 5) ->
    a;
ge_ge_int_range_2(_) ->
    b.

ge_ge_int_range_3(X) when (1 bsl 59) - 20 =< X, X =< (1 bsl 59) - 10 ->
    a;
ge_ge_int_range_3((1 bsl 59) - 5) ->
    a;
ge_ge_int_range_3(_) ->
    b.

ge_ge_int_range_4(X) when (-1 bsl 59) + 1 =< X, X =< (-1 bsl 59) + 5 ->
    a;
ge_ge_int_range_4((-1 bsl 59) + 10) ->
    a;
ge_ge_int_range_4(_) ->
    b.

%% GH-8325. An inverted test with an empty map would fail for
%% non-boxed terms.
empty_map_or_nil(V) when V =:= #{}; V =:= [] ->
    [];
empty_map_or_nil(V) ->
    [V].

%% Tests operators where type hints are significant.
typed_relop(Config) when is_list(Config) ->
    _ = [compare_integer_pid(1 bsl N) || N <- lists:seq(1, 64)],

    {error,<<7:3>>} = compare_bitstring({text, <<7:3>>, 0}),
    {error,<<0:8>>} = compare_bitstring({text, <<0:8>>, 0}),
    {error,<<0:9>>} = compare_bitstring({text, <<0:9>>, 0}),
    {text, 42} = compare_bitstring({text, <<0:3>>, 42}),

    {error,<<7:3>>} = compare_bitstring({binary, <<7:3>>, 0}),
    {error,<<0:8>>} = compare_bitstring({binary, <<0:8>>, 0}),
    {error,<<0:9>>} = compare_bitstring({binary, <<0:9>>, 0}),
    {binary, 42} = compare_bitstring({binary, <<0:3>>, 42}),

    negative = classify_value(id(-1 bsl 128)),
    other = classify_value(id(0)),
    other = classify_value(id(42)),
    other = classify_value(id(1 bsl 64)),
    other = classify_value(id(a)),

    ok.

compare_integer_pid(N) when is_integer(N) ->
    Immed = self(),
    true = is_pid(Immed),
    if
        N >= Immed -> ct:fail("integer compared greater than pid");
        N < Immed -> ok
    end.

%% GH-7433. Equality and non-equality tests with a bitstring could fail when it
%% should succeed and vice versa.
compare_bitstring({text, Res, _Data}) when is_bitstring(Res), Res =/= <<0:3>> ->
    {error, Res};
compare_bitstring({binary, Res, _Data}) when is_bitstring(Res), Res =/= <<0:3>> ->
    {error, Res};
compare_bitstring({binary, _Res, Data}) ->
    {binary, Data};
compare_bitstring({text, _Res, Data}) ->
    {text, Data}.

classify_value(N) when is_integer(N), N < 0 ->
    negative;
classify_value(_N) ->
    other.

term_equivalence(_Config) ->
    %% Term equivalence has been tested before in this suite, but we need to
    %% massage some edge cases that cannot easily be put into the other test
    %% cases.
    <<PosZero/float>> = id(<<0:1,0:63>>),
    <<NegZero/float>> = id(<<1:1,0:63>>),

    +0.0 = id(PosZero),
    -0.0 = id(NegZero),

    -1 = erts_internal:cmp_term(NegZero, PosZero),
    1 = erts_internal:cmp_term(PosZero, NegZero),

    -1 = cmp_float(NegZero, PosZero),
    1 = cmp_float(PosZero, NegZero),

    Floats = [0.0, -0.0, 4711.0, -4711.0, 12.0, 943.0],

    [true = (cmp_float(A, B) =:= erts_internal:cmp_term(A, B)) ||
     A <- Floats, B <- Floats],

    ok.

cmp_float(A0, B0) ->
    A = float_comparable(A0),
    B = float_comparable(B0),
    if
        A < B -> -1;
        A > B -> +1;
        A =:= B -> 0
    end.

%% Converts a float to a number which, when compared with other such converted
%% floats, is ordered the same as '<' on the original inputs aside from the
%% fact that -0.0 < +0.0 as required by the term equivalence order.
%%
%% This has been proven correct with the SMT-LIB model below:
%%
%% (define-const SignBit_bv (_ BitVec 64) #x8000000000000000)
%% 
%% ; Two finite floats X and Y of unknown value
%% (declare-const X (_ FloatingPoint 11 53))
%% (declare-const Y (_ FloatingPoint 11 53))
%% (assert (= false (fp.isInfinite X) (fp.isInfinite Y)
%%            (fp.isNaN X) (fp.isNaN Y)))
%% 
%% ; ... the bit representations of the aforementioned floats. The Z3 floating-
%% ; point extension lacks a way to directly bit-cast a vector to a float, so
%% ; we rely on equivalence here.
%% (declare-const X_bv (_ BitVec 64))
%% (declare-const Y_bv (_ BitVec 64))
%% (assert (= ((_ to_fp 11 53) X_bv) X))
%% (assert (= ((_ to_fp 11 53) Y_bv) Y))
%% 
%% ; The bit hack we're going to test
%% (define-fun float_sortable ((value (_ BitVec 64))) (_ BitVec 64)
%% (ite (distinct (bvand value SignBit_bv) SignBit_bv)
%%  (bvxor value SignBit_bv)
%%  (bvnot value)))
%% 
%% (define-fun float_bv_lt ((LHS (_ BitVec 64))
%%                          (RHS (_ BitVec 64))) Bool
%%  (bvult (float_sortable LHS) (float_sortable RHS)))
%% 
%% (push 1)
%%   ; When either of X or Y are non-zero, (X < Y) = (bvX < bvY)
%%   (assert (not (and (fp.isZero X) (fp.isZero Y))))
%%   (assert (distinct (fp.lt X Y) (float_bv_lt X_bv Y_bv)))
%%   (check-sat) ; unsat, proving by negation that the above always holds
%% (pop 1)
%% 
%% (push 1)
%%   ; Negative zero should sort lower than positive zero
%%   (assert (and (fp.isNegative X) (fp.isPositive Y)
%%                (fp.isZero X) (fp.isZero Y)))
%%   (assert (not (float_bv_lt X_bv Y_bv)))
%%   (check-sat) ; unsat
%% (pop 1)
float_comparable(V0) ->
    Sign = 16#8000000000000000,
    Mask = 16#FFFFFFFFFFFFFFFF,
    <<V_bv:64/unsigned>> = <<V0/float>>,
    case V_bv band Sign of
        0 -> (V_bv bxor Sign) band Mask;
        Sign -> (bnot V_bv) band Mask
    end.

%%%
%%% Utilities.
%%%

unvalue(V) ->
    Abstr = erl_parse:abstract(V),
    erl_parse:anno_to_term(Abstr).

value({nil,_}) -> [];
value({integer,_,X}) -> X;
value({string,_,X}) -> X;
value({float,_,X})   -> X;
value({atom,_,X})    -> X;
value({tuple,_,Es}) ->
    list_to_tuple(lists:map(fun(X) -> value(X) end, Es));
value({cons,_,H,T}) ->
    [value(H) | value(T)];
value(Other) ->
    {value,Value,_} = erl_eval:expr(Other, erl_eval:new_bindings()),
    Value.

repeat(_, 0) -> ok;
repeat(Fun, N) ->
    Fun(),
    repeat(Fun, N-1).

id(I) -> I.
