%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2024. All Rights Reserved.
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
-module(fun_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 test1/1,overwritten_fun/1,otp_7202/1,bif_fun/1,
         external/1,eep37/1,badarity/1,badfun/1,
         duplicated_fun/1,unused_fun/1,parallel_scopes/1,
         coverage/1,leaky_environment/1]).

%% Internal exports.
-export([call_me/1,dup1/0,dup2/0]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [{group,p}].

groups() ->
    [{p,[parallel],
      [test1,overwritten_fun,otp_7202,bif_fun,external,eep37,
       badarity,badfun,duplicated_fun,unused_fun,
       parallel_scopes,
       coverage,leaky_environment]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%% The help functions below are copied from emulator:bs_construct_SUITE.

-define(T(B, L), {fun() -> B end(), ??B, L}).

l1() ->
    [
     ?T((begin _A = 3, F = fun(_A) -> 1; (_) -> 2 end, F(2) end), 1),
     ?T((begin G = fun(1=0) -> ok end, {'EXIT',_} = (catch G(2)), ok end), ok),
     ?T((begin F = fun(_, 1) -> 1; (F, N) -> N * F(F, N-1) end, F(F, 5) end), 120),
     ?T((begin F = fun(_, 1) -> 1; (F, N) -> N * F(F, N-1) end, F(F, 1), ok end), ok)
    ].

test1(Config) when is_list(Config) ->
    lists:foreach(fun one_test/1, eval_list(l1(), [])),
    ok.

evaluate(Str, Vars) ->
    {ok,Tokens,_} =
	erl_scan:string(Str ++ " . "),
    {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
    case erl_eval:expr(Expr, Vars) of
	{value, Result, _} ->
	    Result
    end.

eval_list([], _Vars) ->
    [];
eval_list([{C_bin, Str, Bytes} | Rest], Vars) ->
    case catch evaluate(Str, Vars) of
	{'EXIT', Error} ->
	    io:format("Evaluation error: ~p, ~p, ~p~n", [Str, Vars, Error]),
	    exit(Error);
	E_bin ->
	    [{C_bin, E_bin, Str, Bytes} | eval_list(Rest, Vars)]
    end.

one_test({C, E, Str, Correct}) ->
    io:format("  ~s, ~p~n", [Str, Correct]),
    if
	C == Correct ->
	    ok;
	true ->
	    io:format("ERROR: Compiled: ~p. Expected ~p. Got ~p.~n",
		      [Str, Correct, C]),
	    ct:fail(comp)
    end,
    if
	E == Correct ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p. Expected ~p. Got ~p.~n",
		      [Str, Correct, E]),
	    ct:fail(comp)
    end.

-record(b, {c}).

%% OTP-7102. (Thanks to Simon Cornish.)

overwritten_fun(Config) when is_list(Config) ->
    {a2,a} = overwritten_fun_1(a),
    {a2,{b,c}} = overwritten_fun_1(#b{c=c}),
    one = overwritten_fun_1(#b{c=[]}),
    ok.

overwritten_fun_1(A) ->
    F = fun() ->
		{ok, A}
	end,
    if A#b.c == [] ->
	    one;
       true ->
	    case F() of
		{ok, A2} ->
		    {a2, A2};
		_ ->
		    three
	    end
    end.

%% OTP-7202. The liveness calculation for the make_fun2 instruction was wrong.

otp_7202(Config) when is_list(Config) ->
    otp_7202().

otp_7202() ->
    List = [a],
    Error = case otp_7202_func() of
                no_value -> true;
                {ok, V} -> V
             end,
    lists:foreach(fun(_E) ->
                          case Error of
                              true ->
                                  ok;
                              false ->
                                  ok
                          end
                  end, List).

otp_7202_func() ->
    no_value.
    
bif_fun(Config) when is_list(Config) ->
    F = fun abs/1,
    5 = F(-5),
    ok.

-define(APPLY(M, F, A), (fun(Fun) -> {ok,{a,b}} = Fun({a,b}) end)(fun M:F/A)).
-define(APPLY2(M, F, A),
	(fun(Map) ->
		 Id = fun(I) -> I end,
		 List = [x,y],
		 List = Map(Id, List),
		 {type,external} = erlang:fun_info(Map, type)
	 end)(fun M:F/A)).
    
external(Config) when is_list(Config) ->
    Mod = id(?MODULE),
    Func = id(call_me),
    Arity = id(1),

    ?APPLY(?MODULE, call_me, 1),
    ?APPLY(?MODULE, call_me, Arity),
    ?APPLY(?MODULE, Func, 1),
    ?APPLY(?MODULE, Func, Arity),
    ?APPLY(Mod, call_me, 1),
    ?APPLY(Mod, call_me, Arity),
    ?APPLY(Mod, Func, 1),
    ?APPLY(Mod, Func, Arity),

    ListsMod = id(lists),
    ListsMap = id(map),
    ListsArity = id(2),

    ?APPLY2(lists, map, 2),
    ?APPLY2(lists, map, ListsArity),
    ?APPLY2(lists, ListsMap, 2),
    ?APPLY2(lists, ListsMap, ListsArity),
    ?APPLY2(ListsMod, map, 2),
    ?APPLY2(ListsMod, map, ListsArity),
    ?APPLY2(ListsMod, ListsMap, 2),
    ?APPLY2(ListsMod, ListsMap, ListsArity),

    42 = (fun erlang:abs/1)(-42),
    42 = (id(fun erlang:abs/1))(-42),
    42 = apply(fun erlang:abs/1, [-42]),
    42 = apply(id(fun erlang:abs/1), [-42]),
    6 = (fun lists:sum/1)([1,2,3]),
    6 = (id(fun lists:sum/1))([1,2,3]),

    {'EXIT',{{badarity,_},_}} = (catch (fun lists:sum/1)(1, 2, 3)),
    {'EXIT',{{badarity,_},_}} = (catch (id(fun lists:sum/1))(1, 2, 3)),
    {'EXIT',{{badarity,_},_}} = (catch apply(fun lists:sum/1, [1,2,3])),

    {'EXIT',{badarg,_}} = (catch bad_external_fun()),

    ok.

call_me(I) ->
    {ok,I}.

bad_external_fun() ->
    V0 = idea,
    fun V0:V0/V0,                               %Should fail.
    never_reached.

%% Named funs.
eep37(_Config) ->
    eep37_basic(),
    eep37_dup(),
    eep37_gh6515(),
    ok.

eep37_basic() ->
    F = fun Fact(N) when N > 0 -> N * Fact(N - 1); Fact(0) -> 1 end,
    Add = fun _(N) -> N + 1 end,
    UnusedName = fun _BlackAdder(N) -> N + 42 end,
    720 = F(6),
    10 = Add(9),
    50 = UnusedName(8),
    ok.

eep37_dup() ->
    dup1 = (dup1())(),
    dup2 = (dup2())(),
    ok.

dup1() ->
    fun _F() -> dup1 end.

dup2() ->
    fun _F() -> dup2 end.

eep37_gh6515() ->
    {0,F1} = eep37_gh6515_1(),
    F1 = F1(),

    [0,F2] = eep37_gh6515_2(),
    1 = F2(0),
    120 = F2(5),

    ok.

eep37_gh6515_1() ->
    {case [] of
         #{} ->
             X = 0;
         X ->
             0
     end,
     fun X() ->
             X
     end}.

eep37_gh6515_2() ->
    [case [] of
         #{} ->
             Fact = 0;
         Fact ->
             0
     end,
     fun Fact(N) when N > 0 ->
             N * Fact(N - 1);
         Fact(0) -> 1
     end].

badarity(Config) when is_list(Config) ->
    {'EXIT',{{badarity,{_,[]}},_}} = (catch (fun badarity/1)()),
    {'EXIT',{{badarity,_},_}} = (catch fun() -> 42 end(0)),
    ok.

badfun(_Config) ->
    X = not_a_fun,
    expect_badfun(42, catch 42()),
    expect_badfun(42.0, catch 42.0(1)),
    expect_badfun(X, catch X()),
    expect_badfun(X, catch X(1)),
    Len = length(atom_to_list(X)),
    expect_badfun(Len, catch begin length(atom_to_list(X)) end(1)),

    expect_badfun(42, catch 42(put(?FUNCTION_NAME, yes))),
    yes = erase(?FUNCTION_NAME),

    expect_badfun(X, catch X(put(?FUNCTION_NAME, of_course))),
    of_course = erase(?FUNCTION_NAME),

    %% A literal as a Fun used to crash the code generator. This only happened
    %% when type optimization had reduced `Fun` to a literal, hence the match.
    Literal = fun(literal = Fun) ->
                      Fun()
              end,
    expect_badfun(literal, catch Literal(literal)),

    ok.

expect_badfun(Term, Exit) ->
    {'EXIT',{{badfun,Term},_}} = Exit.

duplicated_fun(_Config) ->
    try
        %% The following code used to crash the compiler before
        %% v3_core:is_safe/1 was corrected to consider fun variables
        %% unsafe.
        id([print_result_paths_fun = fun duplicated_fun_helper/1]),
        ct:error(should_fail)
    catch
        error:{badmatch,F} when is_function(F, 1) ->
            ok
    end.

duplicated_fun_helper(_) ->
    ok.

%% ERL-1166: beam_kernel_to_ssa would crash if a fun was unused.
unused_fun(_Config) ->
    _ = fun() -> ok end,
    try id(ok) of
        _ -> fun() -> ok end
    catch _ -> ok end,
    ok.

parallel_scopes(_Config) ->
    1 = parallel_scopes_1a(),
    1 = parallel_scopes_1b(),
    {'EXIT',{{badmatch,99},_}} = catch parallel_scopes_1c(),

    10 = parallel_scopes_2a(),
    {'EXIT',{{badmatch,15},_}} = catch parallel_scopes_2b(),
    500 = parallel_scopes_2c(500, 500),
    {'EXIT',{{badmatch,1000},_}} = catch parallel_scopes_2c(500, 1000),
    600 = parallel_scopes_2d(600, 600),
    {'EXIT',{{badmatch,1000},_}} = catch parallel_scopes_2d(600, 1000),
    {a,20} = parallel_scopes_2e(20, 20),
    {'EXIT',{{badmatch,{a,25}},_}} = catch parallel_scopes_2e(20, 25),

    {[42,2],42,a} = parallel_scopes_3(a),

    42 = parallel_scopes_4a(id(42), id(42)),
    {'EXIT',{{badmatch,77},_}} = catch parallel_scopes_4a(42, 77),
    42 = parallel_scopes_4b(id(42), id(42)),
    {'EXIT',{{badmatch,77},_}} = catch parallel_scopes_4b(42, 77),
    [same,2,same,2] = parallel_scopes_4c(id(same), id(same)),
    {'EXIT',{{badmatch,55},_}} = catch parallel_scopes_4c(42, 55),

    33 = parallel_scopes_5(id(33), id(33)),
    {'EXIT',{{badmatch,44},_}} = catch parallel_scopes_5(33, 44),

    99 = parallel_scopes_6(id(99), id(99)),
    {'EXIT',{{badmatch,88},_}} = catch parallel_scopes_6(77, 88),

    99 = parallel_scopes_7(id(99), id(99)),
    {'EXIT',{{badmatch,88},_}} = catch parallel_scopes_7(77, 88),

    199 = parallel_scopes_8(id(199), id(199)),
    {'EXIT',{{badmatch,200},_}} = catch parallel_scopes_8(id(199), id(200)),

    {299,299+299} = parallel_scopes_9(id(299), id(299), id(299+299)),
    {'EXIT',{{badmatch,300},_}} = catch parallel_scopes_9(id(299), id(300), id(0)),
    {'EXIT',{{badmatch,0},_}} = catch parallel_scopes_9(id(299), id(299), id(0)),

    999 = parallel_scopes_10(false, 999, ignored, 999),
    {'EXIT',{{badmatch,999},_}} = catch parallel_scopes_10(false, 700, ignored, 700),
    {'EXIT',{{badmatch,1000},_}} = catch parallel_scopes_10(false, 999, ignored, 1000),
    999 = parallel_scopes_10(true, 999, 999, ignored),
    333 = parallel_scopes_10(true, 333, 333, ignored),
    {'EXIT',{{badmatch,901},_}} = catch parallel_scopes_10(true, 900, 901, ignored),

    889 = parallel_scopes_11(889, 889, 889),
    {'EXIT',{{badmatch,800},_}} = catch parallel_scopes_11(889, 800, 889),
    {'EXIT',{{badmatch,810},_}} = catch parallel_scopes_11(889, 889, 810),
    {'EXIT',{{badmatch,889},_}} = catch parallel_scopes_11(a, a, a),

    333 = parallel_scopes_12(333, 333, 333),
    {'EXIT',{{badmatch,other},_}} = catch parallel_scopes_12(333, other, 333),
    {'EXIT',{{badmatch,nope},_}} = catch parallel_scopes_12(333, 333, nope),

    [1,100] = parallel_scopes_13(99, 100),
    {'EXIT',{{badmatch,no},_}} = catch parallel_scopes_13(no, 100),
    {'EXIT',{{badmatch,nope},_}} = catch parallel_scopes_13(99, nope),

    ok.

parallel_scopes_1a() ->
    (begin X=1, true end
     and
     begin F=(fun () -> X=2 end), F(), true end) andalso X.

parallel_scopes_1b() ->
    (begin X=1, true end
     and
     begin F=(fun () -> X=2 end), F(), true end) andalso (X = 1).

parallel_scopes_1c() ->
    (begin X=1, true end
     and
     begin F=(fun () -> X=2 end), F(), true end) andalso (X = 99).

parallel_scopes_2a() ->
    begin X=10, true end
        and
          begin F=(fun () -> X=20 end), F(), true end
        and
        begin X=10, true end andalso X.

parallel_scopes_2b() ->
    begin X=10, true end
        and
          begin F=(fun () -> X=20 end), F(), true end
        and
        begin X=15, true end andalso X.

parallel_scopes_2c(A, B) ->
    begin X=A, true end
        and
        begin F = (fun () -> X = make_ref() end), F(), true end
        and
        begin X=B, true end andalso X.

parallel_scopes_2d(A, B) ->
    begin X=A, true end
        and
        begin F = (fun () -> X = make_ref() end), F(), true end
        and
        begin X=B, true end andalso (X = A).

parallel_scopes_2e(A, B) ->
    begin X = {a,A}, true end
        and
        begin F=(fun () -> X = 20 end), F(), true end
        and
        begin X = {a,B}, true end andalso X.

parallel_scopes_3(A) ->
    L = [X = id(42),
         fun() -> X = 2 end()],
    {L,X,A}.

parallel_scopes_4a(A, B) ->
    4 = length([X = A,
                fun() -> X = 2 end(),
                X = B,
                fun() -> X = 2 end()]),
    X.

parallel_scopes_4b(A, B) ->
    4 = length([X = A,
                case id(true) of
                    true ->
                        fun() -> X = 2 end()
                end,
                X = B,
                case id(false) of
                    false ->
                        fun() -> X = 2 end()
                end]),
    X.

parallel_scopes_4c(A, B) ->
    [X = A,
     fun() -> X = 2 end(),
     X = B,
     fun() -> X = 2 end()].

parallel_scopes_5(A, B) ->
    4 = length([X = A,
                [fun() -> X = 2 end()],
                X = B |
                case id(false) of
                    false ->
                        [fun() -> X = 2 end()]
                end]),
    X.

parallel_scopes_6(A, B) ->
    4 = tuple_size({X = A,
                    fun() -> X = 40 end(),
                    X = B,
                    fun() -> X = 50 end()}),
    X.

parallel_scopes_7(A, B) ->
    4 = tuple_size({X = A,
                    [fun() -> X = 40 end()],
                    X = B,
                    [fun() -> X = 50 end()]}),
    X.

parallel_scopes_8(A, B) ->
    _ = [X = id(A),
         begin fun() -> X = 2 end(), X = id(B) end],
    X.

parallel_scopes_9(A, B, C) ->
    3 = length([begin X = id(A), Y = id(A+B) end,
                fun() -> X = 2 end(),
                X = id(B)]),
    {X,Y=C}.

parallel_scopes_10(Bool, A, B, C) ->
    T = {X = A,
         case id(Bool) of
             true ->
                 fun() -> X = 999 end(),
                 X = B;
             false ->
                 X = C,
                 fun() -> X = 999 end()
         end},
    2 = tuple_size(T),
    X.

parallel_scopes_11(A, B, C) ->
    T = {X = A,
         case id(true) of
             true ->
                 X = B,
                 2 = length([X = C, X = C]),
                 fun() -> X = 889 end();
             false ->
                 X = cannot_happen
         end},
    2 = tuple_size(T),
    X.

parallel_scopes_12(A, B, C) ->
    T = {X = A,
         case id(true) of
             true ->
                 fun() -> X = whatever end(),
                 2 = length([X = B, X = B]),
                 X = C;
             false ->
                 X = cannot_happen
         end},
    2 = tuple_size(T),
    X.

parallel_scopes_13(A, B) ->
    [X = 1,
     fun() ->
             X = id(whatever),
             99 = A,
             100 = B
     end()].

coverage(_Config) ->
    ok = coverage_1(),

    [2,3,4] = coverage_2(id([1,2,3])),

    {42,F} = coverage_3(id({[], x})),
    x = F(),

    ok.

coverage_1() ->
    %% Cover a line in beam_ssa_pre_codegen:need_frame_1/2 when the
    %% no_make_fun3 option is given.
    catch
        fun(whatever) -> 0;
           ("abc") -> party
        end,
        ok.

coverage_2(List) ->
    %% Cover a line in beam_ssa_pre_codegen:need_frame_1/2 when the
    %% no_make_fun3 option is given.
    lists:map(fun(E) -> E + 1 end, List).

%% Cover a line in beam_block when no_make_fun3 option is given.
coverage_3({[], A}) ->
    {id(42), fun() -> A end}.

leaky_environment(_Config) ->
    G = fun(X, Y) -> X + Y end,
    F = fun(A) -> G(A, 0) end,
    {'EXIT', {{badarity, {_, [1, flurb]}}, _}} = catch F(1, flurb),
    ok.

id(I) ->
    I.
