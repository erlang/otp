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

%%
-module(fun_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 init_per_suite/1,end_per_suite/1,
	 good_call/1,bad_apply/1,bad_fun_call/1,badarity/1,
	 ext_badarity/1,otp_6061/1,external/1,eep37/1]).

%% Internal exports.
-export([nothing/0,call_me/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [good_call, bad_apply, bad_fun_call, badarity,
     ext_badarity, otp_6061, external, eep37].

init_per_testcase(_Case, Config) ->
    test_lib:interpret(?MODULE),
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

init_per_suite(Config) when is_list(Config) ->
    test_lib:interpret(?MODULE),
    true = lists:member(?MODULE, int:interpreted()),
    Config.

end_per_suite(Config) when is_list(Config) ->
    ok.

good_call(Config) when is_list(Config) ->
    F = fun() -> ok end,
    ok = F(),
    FF = fun ?MODULE:nothing/0,
    ok = FF(),
    ok.

%% Test that the correct EXIT code is returned for all types of bad funs.
bad_apply(Config) when is_list(Config) ->
    bad_apply_fc(42, [0]),
    bad_apply_fc(xx, [1]),
    bad_apply_fc({}, [2]),
    bad_apply_fc({1}, [3]),
    bad_apply_fc({1,2,3}, [4]),
    bad_apply_fc({1,2,3}, [5]),
    bad_apply_fc({1,2,3,4}, [6]),
    bad_apply_fc({1,2,3,4,5,6}, [7]),
    bad_apply_fc({1,2,3,4,5}, [8]),
    bad_apply_badarg({1,2}, [9]),
    ok.

bad_apply_fc(Fun, Args) ->
    Res = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badfun,Fun},_Where}} ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res]);
	Other ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result,Other})
    end.

bad_apply_badarg(Fun, Args) ->
    Res = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badfun,Fun},_Where}} ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res]);
	Other ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result, Other})
    end.

%% Try directly calling bad funs.
bad_fun_call(Config) when is_list(Config) ->
    bad_call_fc(42),
    bad_call_fc(xx),
    bad_call_fc({}),
    bad_call_fc({1}),
    bad_call_fc({1,2,3}),
    bad_call_fc({1,2,3}),
    bad_call_fc({1,2,3,4}),
    bad_call_fc({1,2,3,4,5,6}),
    bad_call_fc({1,2,3,4,5}),
    bad_call_fc({1,2}),
    ok.

bad_call_fc(Fun) ->
    Args = [some,stupid,args],
    Res = (catch Fun(Args)),
    case Res of
	{'EXIT',{{badfun,Fun},_Where}} ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	Other ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result,Other})
    end.

%% Call and apply valid external funs with wrong number of arguments.

badarity(Config) when is_list(Config) ->
    Fun = fun() -> ok end,
    Stupid = {stupid,arguments},
    Args = [some,{stupid,arguments},here],

    %% Simple call.

    Res = (catch Fun(some, Stupid, here)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badarity,{Fun,Args}},[_|_]}} ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	_ ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result,Res})
    end,

    %% Apply.

    Res2 = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res2 of
	{'EXIT',{{badarity,{Fun,Args}},[_|_]}} ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]);
	_ ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]),
	    ct:fail({bad_result,Res2})
    end,
    ok.

%% Call and apply valid external funs with wrong number of arguments.

ext_badarity(Config) when is_list(Config) ->
    Fun = fun ?MODULE:nothing/0,
    Stupid = {stupid,arguments},
    Args = [some,{stupid,arguments},here],

    %% Simple call.

    Res = (catch Fun(some, Stupid, here)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	_ ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result,Res})
    end,

    %% Apply.

    Res2 = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res2 of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]);
	_ ->
	    ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]),
	    ct:fail({bad_result,Res2})
    end,
    ok.

nothing() ->
    ok.

%% Test handling of fun expression referring to uninterpreted code.
otp_6061(Config) when is_list(Config) ->

    OrigFlag = process_flag(trap_exit, true),

    Self = self(),
    Pid = spawn_link(fun() -> test_otp_6061(Self) end),

    receive
	working ->
	    ok;
	not_working ->
	    ct:fail(not_working);
	{'EXIT', Pid, Reason} ->
	    ct:fail({crash, Reason})
    after
	5000 ->
	    ct:fail(timeout)
    end,

    process_flag(trap_exit, OrigFlag),

    ok.

test_otp_6061(Starter) ->
    Passes = [2],
    PassesF = [fun() -> Starter ! not_working end,
	       fun() -> Starter ! working end,
	       fun() -> Starter ! not_working end],
    lists:foreach(fun(P)->(lists:nth(P,PassesF))() end,Passes).

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
    [1,1,2,6,24,120] = lists:map(F, lists:seq(0, 5)),
    {'EXIT',{{badarity,_},_}} = (catch lists:map(fun G() -> G() end, [1])),
    {'EXIT',{{badarity,_},_}} = (catch F()),

    ok.

id(I) ->
    I.
