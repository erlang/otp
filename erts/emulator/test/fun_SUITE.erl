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

-module(fun_SUITE).

-export([all/0, suite/0,
         init_per_testcase/2, end_per_testcase/2,
	 bad_apply/1,bad_fun_call/1,badarity/1,ext_badarity/1,
         bad_arglist/1,
	 equality/1,ordering/1,
	 fun_to_port/1,t_phash/1,t_phash2/1,md5/1,
	 refc/1,refc_ets/1,refc_dist/1,
	 const_propagation/1,t_arity/1,t_is_function2/1,
	 t_fun_info/1,t_fun_info_mfa/1,t_fun_to_list/1]).

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
     t_fun_info_mfa,t_fun_to_list].

init_per_testcase(_TestCase, Config) ->
    Config.
end_per_testcase(_TestCase, Config) ->
    erts_test_utils:ept_check_leaked_nodes(Config).


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

    P = hd(erlang:ports()),
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
    Port = open_port({spawn_executable, os:find_executable("erl")},
                     [{args, ["-noshell", "-eval", "timer:sleep(2000)",
                              "-run", "erlang", "halt"]},
                      use_stdio]),
    fun_to_port(Port, xxx),
    fun_to_port(Port, fun() -> 42 end),
    fun_to_port(Port, [fun() -> 43 end]),
    fun_to_port(Port, [1,fun() -> 44 end]),
    fun_to_port(Port, [0,1|fun() -> 45 end]),
    B64K = build_io_list(65536),
    fun_to_port(Port, [B64K,fun() -> 45 end]),
    fun_to_port(Port, [B64K|fun() -> 45 end]),
    unlink(Port),
    exit(Port, kill),
    ok.

fun_to_port(Port, IoList) ->
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
    %% As the fun entry is owned by the fun's shared reference holder and not
    %% the fun itself, its reference count should be generally be unchanged
    %% regardless of how many copies we create, and on which process we do so.
    %%
    %% Only certain operations that break the sharing of the literal reference
    %% holder should have an impact.
    F1 = fun_factory(2),
    {refc,2} = erlang:fun_info(F1, refc),
    F2 = fun_factory(42),
    {refc,2} = erlang:fun_info(F1, refc),

    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> {refc,2} = erlang:fun_info(F1, refc) end),
    receive
	{'EXIT',Pid,normal} -> ok;
	Other -> ct:fail({unexpected,Other})
    end,
    process_flag(trap_exit, false),
    %% Wait to make sure that the process has terminated completely.
    receive after 1 -> ok end,
    {refc,2} = erlang:fun_info(F1, refc),

    %% Force a copy of the underlying reference holder by passing through the
    %% external term format.
    F3 = binary_to_term(term_to_binary(F1)),
    3 = fun_refc(F1),
    3 = fun_refc(F3),

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
    {ok, Peer, Node} = ?CT_PEER(),
    process_flag(trap_exit, true),
    Pid = spawn_link(Node, fun() -> receive
                                        Fun when is_function(Fun) ->
                                            3 = fun_refc(Fun),
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
    refc_dist_send(Node, F),
    peer:stop(Peer).

refc_dist_send(Node, F) ->
    Pid = spawn_link(Node, fun() -> receive
                                        {To,Fun} when is_function(Fun) ->
                                            wait_until(fun () ->
                                                               3 =:= fun_refc(Fun)
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
                                           3 = fun_refc(Fun),
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
    Fun1 = fun wait_until/1,
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
    {ok, Peer, Node} = ?CT_PEER(),
    hello_world = spawn_call(Node, fun() -> hello_world end),
    0 = spawn_call(Node, fun(X) -> X end),
    42 = spawn_call(Node, fun(_X) -> A end),
    43 = spawn_call(Node, fun(X, Y) -> A+X+Y end),
    1 = spawn_call(Node, fun(X, Y) -> X+Y end),
    45 = spawn_call(Node, fun(X, Y, Z) -> A+X+Y+Z end),
    peer:stop(Peer),
    ok.

t_is_function2(Config) when is_list(Config) ->
    false = is_function(id({a,b}), 0),
    false = is_function(id({a,b}), 234343434333433433),
    true = is_function(id(fun() -> ok end), 0),
    true = is_function(id(fun(_) -> ok end), 1),
    false = is_function(id(fun(_) -> ok end), 0),

    true = is_function(id(fun erlang:abs/1), 1),
    true = is_function(id(fun erlang:abs/99), 99),
    false = is_function(id(fun erlang:abs/1), 0),
    false = is_function(id(fun erlang:abs/99), 0),

    false = is_function(id(self()), 0),
    false = is_function(id({a,b,c}), 0),
    false = is_function(id({a}), 0),
    false = is_function(id([a,b,c]), 0),

    %% Larger arities.
    F16 = id(fun f/16),
    F255 = id(fun f/255),

    false = is_function(id(self()), 16),
    true = is_function(F16, 16),
    ok = id(if is_function(F16, 16) -> ok; true -> error end),
    false = is_function(F255, 16),
    error = id(if is_function(F255, 16) -> ok; true -> error end),

    false = is_function(id(self()), 255),
    true = is_function(F255, 255),
    false = is_function(F16, 255),
    error = id(if is_function(F16, 255) -> ok; true -> error end),
    ok = id(if is_function(F255, 255) -> ok; true -> error end),

    %% Bad arity argument.
    bad_arity(a),
    bad_arity(-1),
    bad_arity(-9738974938734938793873498378),
    bad_arity([]),
    bad_arity(fun() -> ok end),
    bad_arity({}),
    bad_arity({a,b}),
    bad_arity(self()),

    %% Bad arity argument in guard test.
    Fun = id(fun erlang:abs/1),
    ok = if
             is_function(Fun, -1) -> error;
             is_function(Fun, 256) -> error;
             is_function(Fun, a) -> error;
             is_function(Fun, Fun) -> error;
             true -> ok
         end,
    ok.

f(_A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8,
  _A9, _A10, _A11, _A12, _A13, _A14, _A15, _A16) ->
    ok.

f(_A1, _A2, _A3, _A4, _A5, _A6, _A7, _A8,
  _A9, _A10, _A11, _A12, _A13, _A14, _A15, _A16,
  _A17, _A18, _A19, _A20, _A21, _A22, _A23, _A24,
  _A25, _A26, _A27, _A28, _A29, _A30, _A31, _A32,
  _A33, _A34, _A35, _A36, _A37, _A38, _A39, _A40,
  _A41, _A42, _A43, _A44, _A45, _A46, _A47, _A48,
  _A49, _A50, _A51, _A52, _A53, _A54, _A55, _A56,
  _A57, _A58, _A59, _A60, _A61, _A62, _A63, _A64,
  _A65, _A66, _A67, _A68, _A69, _A70, _A71, _A72,
  _A73, _A74, _A75, _A76, _A77, _A78, _A79, _A80,
  _A81, _A82, _A83, _A84, _A85, _A86, _A87, _A88,
  _A89, _A90, _A91, _A92, _A93, _A94, _A95, _A96,
  _A97, _A98, _A99, _A100, _A101, _A102, _A103, _A104,
  _A105, _A106, _A107, _A108, _A109, _A110, _A111, _A112,
  _A113, _A114, _A115, _A116, _A117, _A118, _A119, _A120,
  _A121, _A122, _A123, _A124, _A125, _A126, _A127, _A128,
  _A129, _A130, _A131, _A132, _A133, _A134, _A135, _A136,
  _A137, _A138, _A139, _A140, _A141, _A142, _A143, _A144,
  _A145, _A146, _A147, _A148, _A149, _A150, _A151, _A152,
  _A153, _A154, _A155, _A156, _A157, _A158, _A159, _A160,
  _A161, _A162, _A163, _A164, _A165, _A166, _A167, _A168,
  _A169, _A170, _A171, _A172, _A173, _A174, _A175, _A176,
  _A177, _A178, _A179, _A180, _A181, _A182, _A183, _A184,
  _A185, _A186, _A187, _A188, _A189, _A190, _A191, _A192,
  _A193, _A194, _A195, _A196, _A197, _A198, _A199, _A200,
  _A201, _A202, _A203, _A204, _A205, _A206, _A207, _A208,
  _A209, _A210, _A211, _A212, _A213, _A214, _A215, _A216,
  _A217, _A218, _A219, _A220, _A221, _A222, _A223, _A224,
  _A225, _A226, _A227, _A228, _A229, _A230, _A231, _A232,
  _A233, _A234, _A235, _A236, _A237, _A238, _A239, _A240,
  _A241, _A242, _A243, _A244, _A245, _A246, _A247, _A248,
  _A249, _A250, _A251, _A252, _A253, _A254, _A255) ->
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

t_fun_to_list(Config) when is_list(Config) ->
    "fun a:b/1" = erlang:fun_to_list(fun a:b/1),
    "fun 'a-esc':'b-esc'/1" = erlang:fun_to_list(fun 'a-esc':'b-esc'/1),
    "fun 'a-esc':b/1" = erlang:fun_to_list(fun 'a-esc':b/1),
    "fun a:'b-esc'/1" = erlang:fun_to_list(fun a:'b-esc'/1),
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
    Self = self(),
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
                                     Self ! {result,Res}
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

wait_until(Fun) ->
    case catch Fun() of
	true -> ok;
	_ -> receive after 100 -> wait_until(Fun) end
    end.
