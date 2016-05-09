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
-module(fun_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 test1/1,overwritten_fun/1,otp_7202/1,bif_fun/1,
         external/1,eep37/1,eep37_dup/1,badarity/1,badfun/1]).

%% Internal exports.
-export([call_me/1,dup1/0,dup2/0]).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() ->
    [{p,[parallel],
      [test1,overwritten_fun,otp_7202,bif_fun,external,eep37,
       eep37_dup,badarity,badfun]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%% The help functions below are copied from emulator:bs_construct_SUITE.

-define(T(B, L), {B, ??B, L}).

l1() ->
    [
     ?T((begin A = 3, F = fun(A) -> 1; (_) -> 2 end, F(2) end), 1),
     ?T((begin G = fun(1=0) -> ok end, {'EXIT',_} = (catch G(2)), ok end), ok)
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

    ok.

call_me(I) ->
    {ok,I}.

eep37(Config) when is_list(Config) ->
    F = fun Fact(N) when N > 0 -> N * Fact(N - 1); Fact(0) -> 1 end,
    Add = fun _(N) -> N + 1 end,
    UnusedName = fun BlackAdder(N) -> N + 42 end,
    720 = F(6),
    10 = Add(9),
    50 = UnusedName(8),
    ok.

eep37_dup(Config) when is_list(Config) ->
    dup1 = (dup1())(),
    dup2 = (dup2())(),
    ok.

dup1() ->
    fun _F() -> dup1 end.

dup2() ->
    fun _F() -> dup2 end.

badarity(Config) when is_list(Config) ->
    {'EXIT',{{badarity,{_,[]}},_}} = (catch (fun badarity/1)()),
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

    ok.

expect_badfun(Term, Exit) ->
    {'EXIT',{{badfun,Term},_}} = Exit.

id(I) ->
    I.
