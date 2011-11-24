%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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
-module(fun_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 test1/1,overwritten_fun/1,otp_7202/1,bif_fun/1,
	 external/1]).

%% Internal export.
-export([call_me/1]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    test_lib:recompile(?MODULE),
    [test1,overwritten_fun,otp_7202,bif_fun,external].

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

%%% The help functions below are copied from emulator:bs_construct_SUITE.

-define(T(B, L), {B, ??B, L}).

l1() ->
    [
     ?T((begin A = 3, F = fun(A) -> 1; (_) -> 2 end, F(2) end), 1),
     ?T((begin G = fun(1=0) -> ok end, {'EXIT',_} = (catch G(2)), ok end), ok)
    ].

test1(suite) -> [];
test1(Config) when is_list(Config) ->
    ?line lists:foreach(fun one_test/1, eval_list(l1(), [])),
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
	    test_server:fail(comp)
    end,
    if
	E == Correct ->
	    ok;
	true ->
	    io:format("ERROR: Interpreted: ~p. Expected ~p. Got ~p.~n",
		      [Str, Correct, E]),
	    test_server:fail(comp)
    end.

-record(b, {c}).

%% OTP-7102. (Thanks to Simon Cornish.)

overwritten_fun(Config) when is_list(Config) ->
    ?line {a2,a} = overwritten_fun_1(a),
    ?line {a2,{b,c}} = overwritten_fun_1(#b{c=c}),
    ?line one = overwritten_fun_1(#b{c=[]}),
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
    ?line F = fun abs/1,
    ?line 5 = F(-5),
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

id(I) ->
    I.
