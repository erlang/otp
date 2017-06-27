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

-module(fun_SUITE).

-export([all/0, suite/0,
	 bad_apply/1,bad_fun_call/1,badarity/1,ext_badarity/1,
         bad_arglist/1,
	 equality/1,ordering/1,
	 fun_to_port/1,t_phash/1,t_phash2/1,md5/1,
	 refc/1,refc_ets/1,refc_dist/1,
	 const_propagation/1,t_arity/1,t_is_function2/1,
	 t_fun_info/1,t_fun_info_mfa/1]).

-export([nothing/0]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].


all() ->
    [bad_apply, bad_fun_call, badarity, ext_badarity,
     bad_arglist,
     equality, ordering, fun_to_port, t_phash,
     t_phash2, md5, refc, refc_ets, refc_dist,
     const_propagation, t_arity, t_is_function2, t_fun_info,
     t_fun_info_mfa].

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
    Res = (catch Fun(Fun(Args))),
    case Res of
	{'EXIT',{{badfun,Fun},_Where}} ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	Other ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ct:fail({bad_result,Other})
    end.

% Test erlang:apply with non-proper arg-list
bad_arglist(Config) when is_list(Config) ->
    Fun = fun(A,B) -> A+B end,
    {'EXIT', {badarg,_}} = (catch apply(Fun, 17)),
    {'EXIT', {badarg,_}} = (catch apply(Fun, [17|18])),
    {'EXIT', {badarg,_}} = (catch apply(Fun, [17,18|19])),
    {'EXIT', {badarg,_}} = (catch apply(lists,seq, 17)),
    {'EXIT', {badarg,_}} = (catch apply(lists,seq, [17|18])),
    {'EXIT', {badarg,_}} = (catch apply(lists,seq, [17,18|19])),
    ok.


%% Call and apply valid funs with wrong number of arguments.

badarity(Config) when is_list(Config) ->
    Fun = fun() -> ok end,
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

%% Test equality of funs.

equality(Config) when is_list(Config) ->
    F0 = fun() -> 1 end,
    F0_copy = copy_term(F0),
    true = eq(F0, F0),
    true = eq(F0, F0_copy),

    %% Compare different arities.
    F1 = fun(X) -> X + 1 end,
    true = eq(F1, F1),
    false = eq(F0, F1),
    false = eq(F0_copy, F1),

    %% Compare different environments.
    G1 = make_fun(1),
    G2 = make_fun(2),
    true = eq(G1, G1),
    true = eq(G2, G2),
    false = eq(G1, G2),
    false = eq(G2, G1),
    G1_copy = copy_term(G1),
    true = eq(G1, G1_copy),

    %% Compare fun with binaries.
    B = list_to_binary([7,8,9]),
    false = eq(B, G1),
    false = eq(G1, B),

    %% Compare external funs.
    FF0 = fun aa:blurf/0,
    FF0_copy = copy_term(FF0),
    FF1 = fun erlang:abs/0,
    FF2 = fun erlang:exit/1,
    FF3 = fun erlang:exit/2,
    FF4 = fun z:ff/0,

    true = eq(FF0, FF0),
    true = eq(FF0, FF0_copy),
    true = eq(FF1, FF1),
    true = eq(FF2, FF2),
    true = eq(FF3, FF3),
    true = eq(FF4, FF4),
    false = eq(FF0, FF1),
    false = eq(FF0, FF2),
    false = eq(FF0, FF3),
    false = eq(FF0, FF4),
    false = eq(FF1, FF0),
    false = eq(FF1, FF2),
    false = eq(FF1, FF3),
    false = eq(FF1, FF4),
    false = eq(FF2, FF3),
    false = eq(FF2, FF4),
    false = eq(FF3, FF4),

    %% EEP37
    H1 = fun Fact(N) when N > 0 -> N * Fact(N - 1); Fact(0) -> 1 end,
    H2 = fun Pow(N, M) when M > 0 -> N * Pow(N, M - 1); Pow(_, 0) -> 1 end,
    H1_copy = copy_term(H1),

    true = eq(H1, H1),
    true = eq(H1, H1_copy),
    true = eq(H2, H2),
    false = eq(H1, H2),

    ok.

eq(X, X) -> true;
eq(_, _) -> false.

copy_term(Term) ->
    binary_to_term(term_to_binary(Term)).

make_fun(X) ->
    fun() -> X end.
	    
%% Tests ordering of funs.
ordering(Config) when is_list(Config) ->
    F1 = make_fun(1, 2),
    F1_copy = copy_term(F1),
    F2 = make_fun(1, 3),
    F3 = make_fun(3, 4),

    FF0 = fun aa:blurf/0,
    FF1 = fun erlang:abs/0,
    FF2 = fun erlang:exit/1,
    FF3 = fun erlang:exit/2,
    FF4 = fun z:ff/0,

    true = FF0 < FF1,
    true = FF1 < FF2,
    true = FF2 < FF3,
    true = FF3 < FF4,

    true = FF0 > F1,
    true = FF0 > F2,
    true = FF0 > F3,
    true = FF4 > F1,
    true = FF4 > F2,
    true = FF4 > F3,

    true = F1 == F1,
    true = F1 == F1_copy,
    true = F1 /= F2,

    true = F1 < F2,
    true = F2 > F1,
    true = F2 < F3,
    true = F3 > F2,

    false = F1 > F2,
    false = F2 > F3,

    %% Compare with binaries.

    B = list_to_binary([7,8,9,10]),
    false = B == F1,
    false = F1 == B,

    true = F1 < B,
    true = B > F2,

    false = F1 > B,
    false = B < F2,

    false = F1 >= B,
    false = B =< F2,

    %% Compare module funs with binaries.
    false = B == FF1,
    false = FF1 == B,

    true = FF1 < B,
    true = B > FF2,

    false = FF1 > B,
    false = B < FF2,

    false = FF1 >= B,
    false = B =< FF2,

    %% Create a port and ref.

    Path = proplists:get_value(priv_dir, Config),
    AFile = filename:join(Path, "vanilla_file"),
    P = open_port(AFile, [out]),
    R = make_ref(),

    %% Compare funs with ports and refs.

    true = R < F3,
    true = F3 > R,
    true = F3 < P,
    true = P > F3,

    true = R =< F3,
    true = F3 >= R,
    true = F3 =< P,
    true = P >= F3,

    false = R > F3,
    false = F3 < R,
    false = F3 > P,
    false = P < F3,

    %% Compare funs with conses and nils.

    true = F1 < [a],
    true = F1 < [],
    true = [a,b] > F1,
    true = [] > F1,

    false = [1] < F1,
    false = [] < F1,
    false = F1 > [2],
    false = F1 > [],

    false = [1] =< F1,
    false = [] =< F1,
    false = F1 >= [2],
    false = F1 >= [],

    %% Compare module funs with conses and nils.

    true = FF1 < [a],
    true = FF1 < [],
    true = [a,b] > FF1,
    true = [] > FF1,

    false = [1] < FF1,
    false = [] < FF1,
    false = FF1 > [2],
    false = FF1 > [],

    false = [1] =< FF1,
    false = [] =< FF1,
    false = FF1 >= [2],
    false = FF1 >= [],
    ok.

make_fun(X, Y) ->
    fun(A) -> A*X+Y end.

%% Try sending funs to ports (should fail).
fun_to_port(Config) when is_list(Config) ->
    fun_to_port(Config, xxx),
    fun_to_port(Config, fun() -> 42 end),
    fun_to_port(Config, [fun() -> 43 end]),
    fun_to_port(Config, [1,fun() -> 44 end]),
    fun_to_port(Config, [0,1|fun() -> 45 end]),
    B64K = build_io_list(65536),
    fun_to_port(Config, [B64K,fun() -> 45 end]),
    fun_to_port(Config, [B64K|fun() -> 45 end]),
    ok.

fun_to_port(Config, IoList) ->
    Path = proplists:get_value(priv_dir, Config),
    AFile = filename:join(Path, "vanilla_file"),
    Port = open_port(AFile, [out]),
    case catch port_command(Port, IoList) of
	{'EXIT',{badarg,_}} -> ok;
	Other -> ct:fail({unexpected_retval,Other})
    end.

build_io_list(0) -> [];
build_io_list(1) -> [7];
build_io_list(N) ->
    L = build_io_list(N div 2),
    case N rem 2 of
	0 -> [L|L];
	1 -> [7,L|L]
    end.

%% Test the phash/2 BIF on funs.
t_phash(Config) when is_list(Config) ->
    F1 = fun(_X) -> 1 end,
    F2 = fun(_X) -> 2 end,
    true = phash(F1) /= phash(F2),

    G1 = make_fun(1, 2, 3),
    G2 = make_fun(1, 2, 3),
    G3 = make_fun(1, 2, 4),
    true = phash(G1) == phash(G2),
    true = phash(G2) /= phash(G3),

    FF0 = fun erlang:abs/1,
    FF1 = fun erlang:exit/1,
    FF2 = fun erlang:exit/2,
    FF3 = fun blurf:exit/2,
    true = phash(FF0) =/= phash(FF1),
    true = phash(FF0) =/= phash(FF2),
    true = phash(FF0) =/= phash(FF3),
    true = phash(FF1) =/= phash(FF2),
    true = phash(FF1) =/= phash(FF3),
    true = phash(FF2) =/= phash(FF3),
    ok.

phash(Term) ->
    erlang:phash(Term, 16#7ffffff).

%% Test the phash2/2 BIF on funs.
t_phash2(Config) when is_list(Config) ->
    F1 = fun(_X) -> 1 end,
    F2 = fun(_X) -> 2 end,
    true = phash2(F1) /= phash2(F2),

    G1 = make_fun(1, 2, 3),
    G2 = make_fun(1, 2, 3),
    G3 = make_fun(1, 2, 4),
    true = phash2(G1) == phash2(G2),
    true = phash2(G2) /= phash2(G3),

    FF0 = fun erlang:abs/1,
    FF1 = fun erlang:exit/1,
    FF2 = fun erlang:exit/2,
    FF3 = fun blurf:exit/2,
    true = phash2(FF0) =/= phash2(FF1),
    true = phash2(FF0) =/= phash2(FF2),
    true = phash2(FF0) =/= phash2(FF3),
    true = phash2(FF1) =/= phash2(FF2),
    true = phash2(FF1) =/= phash2(FF3),
    true = phash2(FF2) =/= phash2(FF3),
    
    ok.

phash2(Term) ->
    erlang:phash2(Term, 16#7ffffff).

make_fun(X, Y, Z) ->
    fun() -> {X,Y,Z} end.

%% Test that MD5 bifs reject funs properly.
md5(Config) when is_list(Config) ->
    _ = size(erlang:md5_init()),

    %% Try funs in the i/o list.
    bad_md5(fun(_X) -> 42 end),
    bad_md5([fun(_X) -> 43 end]),
    bad_md5([1,fun(_X) -> 44 end]),
    bad_md5([1|fun(_X) -> 45 end]),
    B64K = build_io_list(65536),
    bad_md5([B64K,fun(_X) -> 46 end]),
    bad_md5([B64K|fun(_X) -> 46 end]),
    ok.
    
bad_md5(Bad) ->
    {'EXIT',{badarg,_}} = (catch erlang:md5(Bad)).

refc(Config) when is_list(Config) ->
    F1 = fun_factory(2),
    {refc,2} = erlang:fun_info(F1, refc),
    F2 = fun_factory(42),
    {refc,3} = erlang:fun_info(F1, refc),

    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> {refc,4} = erlang:fun_info(F1, refc) end),
    receive
	{'EXIT',Pid,normal} -> ok;
	Other -> ct:fail({unexpected,Other})
    end,
    process_flag(trap_exit, false),
    {refc,3} = erlang:fun_info(F1, refc),

    %% Garbage collect. Only the F2 fun will be left.
    7 = F1(5),
    true = erlang:garbage_collect(),
    40 = F2(-2),
    {refc,2} = erlang:fun_info(F2, refc),
    ok.

fun_factory(Const) ->
    fun(X) -> X + Const end.

refc_ets(Config) when is_list(Config) ->
    F = fun(X) -> X + 33 end,
    {refc,2} = erlang:fun_info(F, refc),

    refc_ets_set(F, [set]),
    refc_ets_set(F, [ordered_set]),
    refc_ets_bag(F, [bag]),
    refc_ets_bag(F, [duplicate_bag]),
    ok.

refc_ets_set(F1, Options) ->
    io:format("~p", [Options]),
    Tab = ets:new(kalle, Options),
    true = ets:insert(Tab, {a_key,F1}),
    3 = fun_refc(F1),
    [{a_key,F3}] = ets:lookup(Tab, a_key),
    4 = fun_refc(F1),
    true = ets:insert(Tab, {a_key,not_a_fun}),
    3 = fun_refc(F1),
    true = ets:insert(Tab, {another_key,F1}),
    4 = fun_refc(F1),
    true = ets:delete(Tab),
    3 = fun_refc(F1),
    10 = F3(-23),
    true = erlang:garbage_collect(),
    2 = fun_refc(F1),
    ok.

refc_ets_bag(F1, Options) ->
    io:format("~p", [Options]),
    Tab = ets:new(kalle, Options),
    true = ets:insert(Tab, {a_key,F1}),
    3 = fun_refc(F1),
    [{a_key,F3}] = ets:lookup(Tab, a_key),
    4 = fun_refc(F1),
    true = ets:insert(Tab, {a_key,not_a_fun}),
    4 = fun_refc(F1),
    true = ets:insert(Tab, {another_key,F1}),
    5 = fun_refc(F1),
    true = ets:delete(Tab),
    3 = fun_refc(F1),
    10 = F3(-23),
    true = erlang:garbage_collect(),
    2 = fun_refc(F1),
    ok.

refc_dist(Config) when is_list(Config) ->
    {ok,Node} = start_node(fun_SUITE_refc_dist),
    process_flag(trap_exit, true),
    Pid = spawn_link(Node, fun() -> receive
                                        Fun when is_function(Fun) ->
                                            2 = fun_refc(Fun),
                                            exit({normal,Fun}) end
                           end),
    F = fun() -> 42 end,
    2 = fun_refc(F),
    Pid ! F,
    F2 = receive
	     {'EXIT',Pid,{normal,Fun}} -> Fun;
	     Other -> ct:fail({unexpected,Other})
	 end,
    %% dist.c:net_mess2 have a reference to Fun for a while since
    %% Fun is passed in an exit signal. Wait until it is gone.
    wait_until(fun () -> 4 =/= fun_refc(F2) end),
    3 = fun_refc(F2),
    true = erlang:garbage_collect(),
    2 = fun_refc(F),
    refc_dist_send(Node, F).

refc_dist_send(Node, F) ->
    Pid = spawn_link(Node, fun() -> receive
                                        {To,Fun} when is_function(Fun) ->
                                            wait_until(fun () ->
                                                               2 =:= fun_refc(Fun)
                                                       end),
                                            To ! Fun
                                    end
                           end),
    2 = fun_refc(F),
    Pid ! {self(),F},
    F2 = receive
	     Fun when is_function(Fun) -> Fun;
	     Other -> ct:fail({unexpected,Other})
	 end,
    receive {'EXIT',Pid,normal} -> ok end,
    %% No reference from dist.c:net_mess2 since Fun is passed
    %% in an ordinary message.
    3 = fun_refc(F),
    3 = fun_refc(F2),
    refc_dist_reg_send(Node, F).

refc_dist_reg_send(Node, F) ->
    true = erlang:garbage_collect(),
    2 = fun_refc(F),
    Ref = make_ref(),
    Me = self(),
    Pid = spawn_link(Node, fun() ->
                                   true = register(my_fun_tester, self()),
                                   Me ! Ref,
                                   receive
                                       {Me,Fun} when is_function(Fun) ->
                                           2 = fun_refc(Fun),
                                           Me ! Fun
                                   end
                           end),
    erlang:yield(),
    2 = fun_refc(F),
    receive Ref -> ok end,
    {my_fun_tester,Node} ! {self(),F},
    F2 = receive
	     Fun when is_function(Fun) -> Fun;
	     Other -> ct:fail({unexpected,Other})
	 end,
    receive {'EXIT',Pid,normal} -> ok end,

    3 = fun_refc(F),
    3 = fun_refc(F2),
    ok.
    
fun_refc(F) ->
    {refc,Count} = erlang:fun_info(F, refc),
    Count.

const_propagation(Config) when is_list(Config) ->
    Fun1 = fun start_node/1,
    2 = fun_refc(Fun1),
    Fun2 = Fun1,
    my_cmp({Fun1,Fun2}),

    Fun3 = fun() -> ok end,
    2 = fun_refc(Fun3),
    Fun4 = Fun3,
    my_cmp({Fun3,Fun4}),
    ok.

my_cmp({Fun,Fun}) -> ok;
my_cmp({Fun1,Fun2}) ->
    io:format("Fun1: ~p", [erlang:fun_info(Fun1)]),
    io:format("Fun2: ~p", [erlang:fun_info(Fun2)]),
    ct:fail(no_match).

t_arity(Config) when is_list(Config) ->
    0 = fun_arity(fun() -> ok end),
    0 = fun_arity(fun() -> Config end),
    1 = fun_arity(fun(X) -> X+1 end),
    1 = fun_arity(fun(X) -> Config =:= X end),
    A = id(42),

    %% Test that the arity is transferred properly.
    process_flag(trap_exit, true),
    {ok,Node} = start_node(fun_test_arity),
    hello_world = spawn_call(Node, fun() -> hello_world end),
    0 = spawn_call(Node, fun(X) -> X end),
    42 = spawn_call(Node, fun(_X) -> A end),
    43 = spawn_call(Node, fun(X, Y) -> A+X+Y end),
    1 = spawn_call(Node, fun(X, Y) -> X+Y end),
    45 = spawn_call(Node, fun(X, Y, Z) -> A+X+Y+Z end),
    ok.

t_is_function2(Config) when is_list(Config) ->
    false = is_function(id({a,b}), 0),
    false = is_function(id({a,b}), 234343434333433433),
    true = is_function(fun() -> ok end, 0),
    true = is_function(fun(_) -> ok end, 1),
    false = is_function(fun(_) -> ok end, 0),

    true = is_function(fun erlang:abs/1, 1),
    true = is_function(fun erlang:abs/99, 99),
    false = is_function(fun erlang:abs/1, 0),
    false = is_function(fun erlang:abs/99, 0),

    false = is_function(id(self()), 0),
    false = is_function(id({a,b,c}), 0),
    false = is_function(id({a}), 0),
    false = is_function(id([a,b,c]), 0),

    %% Bad arity argument.
    bad_arity(a),
    bad_arity(-1),
    bad_arity(-9738974938734938793873498378),
    bad_arity([]),
    bad_arity(fun() -> ok end),
    bad_arity({}),
    bad_arity({a,b}),
    bad_arity(self()),
    ok.

bad_arity(A) ->
    {'EXIT',_} = (catch is_function(fun() -> ok end, A)),
    {'EXIT',_} = (catch is_function(no_fun, A)),
    ok.

t_fun_info(Config) when is_list(Config) ->
    F = fun t_fun_info/1,
    try F(blurf) of
	      FAny ->
		  ct:fail("should fail; returned ~p\n", [FAny])
	  catch
	      error:function_clause -> ok
	  end,
    {module,?MODULE} = erlang:fun_info(F, module),
    case erlang:fun_info(F, name) of
	      undefined ->
		  ct:fail(no_fun_info);
	      _ -> ok
	  end,
    {arity,1} = erlang:fun_info(F, arity),
    {env,[]} = erlang:fun_info(F, env),
    verify_not_undef(F, index),
    verify_not_undef(F, uniq),
    verify_not_undef(F, new_index),
    verify_not_undef(F, new_uniq),
    verify_not_undef(F, refc),
    {'EXIT',_} = (catch erlang:fun_info(F, blurf)),    

    %% Module fun.
    FF = fun ?MODULE:t_fun_info/1,
    try FF(blurf) of
	      FFAny ->
		  ct:fail("should fail; returned ~p\n", [FFAny])
	  catch
	      error:function_clause -> ok
	  end,

    {module,?MODULE} = erlang:fun_info(FF, module),
    {name,t_fun_info} = erlang:fun_info(FF, name),
    {arity,1} = erlang:fun_info(FF, arity),
    {env,[]} = erlang:fun_info(FF, env),
    verify_undef(FF, index),
    verify_undef(FF, uniq),
    verify_undef(FF, new_index),
    verify_undef(FF, new_uniq),
    verify_undef(FF, refc),
    {'EXIT',_} = (catch erlang:fun_info(FF, blurf)),

    %% Not fun.
    bad_info(abc),
    bad_info(42),
    bad_info({fun erlang:list_to_integer/1}),
    bad_info([42]),
    bad_info([]),
    bad_info(self()),
    bad_info(<<>>),
    bad_info(<<1,2>>),
    ok.

t_fun_info_mfa(Config) when is_list(Config) ->
    Fun1 = fun spawn_call/2,
    {module,M1}  = erlang:fun_info(Fun1, module),
    {name,F1}    = erlang:fun_info(Fun1, name),
    {arity,A1}   = erlang:fun_info(Fun1, arity),
    {M1,F1,A1=2} = erlang:fun_info_mfa(Fun1),
    %% Module fun.
    Fun2 = fun ?MODULE:t_fun_info/1,
    {module,M2}  = erlang:fun_info(Fun2, module),
    {name,F2}    = erlang:fun_info(Fun2, name),
    {arity,A2}   = erlang:fun_info(Fun2, arity),
    {M2,F2,A2=1} = erlang:fun_info_mfa(Fun2),

    %% Not fun.
    {'EXIT',_} = (catch erlang:fun_info_mfa(id(d))),
    ok.


bad_info(Term) ->
    try	erlang:fun_info(Term, module) of
	Any ->
	    ict:fail("should fail; returned ~p\n", [Any])
    catch
	error:badarg -> ok
    end.

verify_undef(Fun, Tag) ->
    {Tag,undefined} = erlang:fun_info(Fun, Tag).

verify_not_undef(Fun, Tag) ->
    case erlang:fun_info(Fun, Tag) of
	{Tag,undefined} ->
	    ct:fail("tag ~w not defined in fun_info", [Tag]);
	{Tag,_} -> ok
    end.
	    
id(X) ->
    X.

spawn_call(Node, AFun) ->
    Pid = spawn_link(Node,
		     fun() ->
			     receive
				 {Fun,Fun,Fun} when is_function(Fun) ->
				     Arity = fun_arity(Fun),
				     Args = case Arity of
						0 -> [];
						_ -> lists:seq(0, Arity-1)
					    end,
				     Res = apply(Fun, Args),
				     {pid,Creator} = erlang:fun_info(Fun, pid),
				     Creator ! {result,Res}
			     end
		     end),
    Pid ! {AFun,AFun,AFun},
    Res = receive
	      {result,R} -> R;
	      Other -> ct:fail({bad_message,Other})
	  after 10000 ->
		  ct:fail(timeout_waiting_for_result)
	  end,
    receive
	{'EXIT',Pid,normal} -> ok;
	Other2 -> ct:fail({bad_message_waiting_for_exit,Other2})
    after 10000 ->
	    ct:fail(timeout_waiting_for_exit)
    end,
    Res.

fun_arity(F) ->
    {arity,Arity} = erlang:fun_info(F, arity),
    Arity.

start_node(Name) ->
    Pa = filename:dirname(code:which(?MODULE)),
    Cookie = atom_to_list(erlang:get_cookie()),
    test_server:start_node(Name, slave, 
			   [{args, "-setcookie " ++ Cookie ++" -pa " ++ Pa}]).

wait_until(Fun) ->
    case catch Fun() of
	true -> ok;
	_ -> receive after 100 -> wait_until(Fun) end
    end.
