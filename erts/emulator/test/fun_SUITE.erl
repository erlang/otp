%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2012. All Rights Reserved.
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
-compile({nowarn_deprecated_function, {erlang,hash,2}}).

-define(default_timeout, ?t:minutes(1)).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 bad_apply/1,bad_fun_call/1,badarity/1,ext_badarity/1,
	 equality/1,ordering/1,
	 fun_to_port/1,t_hash/1,t_phash/1,t_phash2/1,md5/1,
	 refc/1,refc_ets/1,refc_dist/1,
	 const_propagation/1,t_arity/1,t_is_function2/1,
	 t_fun_info/1,t_fun_info_mfa/1]).

-export([nothing/0]).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [bad_apply, bad_fun_call, badarity, ext_badarity,
     equality, ordering, fun_to_port, t_hash, t_phash,
     t_phash2, md5, refc, refc_ets, refc_dist,
     const_propagation, t_arity, t_is_function2, t_fun_info,
     t_fun_info_mfa].

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


init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

bad_apply(doc) ->
    "Test that the correct EXIT code is returned for all types of bad funs.";
bad_apply(suite) -> [];
bad_apply(Config) when is_list(Config) ->
    ?line bad_apply_fc(42, [0]),
    ?line bad_apply_fc(xx, [1]),
    ?line bad_apply_fc({}, [2]),
    ?line bad_apply_fc({1}, [3]),
    ?line bad_apply_fc({1,2,3}, [4]),
    ?line bad_apply_fc({1,2,3}, [5]),
    ?line bad_apply_fc({1,2,3,4}, [6]),
    ?line bad_apply_fc({1,2,3,4,5,6}, [7]),
    ?line bad_apply_fc({1,2,3,4,5}, [8]),
    ?line bad_apply_badarg({1,2}, [9]),
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
	    ?t:fail({bad_result,Other})
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
	    ?t:fail({bad_result, Other})
    end.

bad_fun_call(doc) ->
    "Try directly calling bad funs.";
bad_fun_call(suite) -> [];
bad_fun_call(Config) when is_list(Config) ->
    ?line bad_call_fc(42),
    ?line bad_call_fc(xx),
    ?line bad_call_fc({}),
    ?line bad_call_fc({1}),
    ?line bad_call_fc({1,2,3}),
    ?line bad_call_fc({1,2,3}),
    ?line bad_call_fc({1,2,3,4}),
    ?line bad_call_fc({1,2,3,4,5,6}),
    ?line bad_call_fc({1,2,3,4,5}),
    ?line bad_call_fc({1,2}),
    ok.

bad_call_fc(Fun) ->
    Args = [some,stupid,args],
    Res = (catch Fun(Fun(Args))),
    case Res of
	{'EXIT',{{badfun,Fun},_Where}} ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	Other ->
	    ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ?t:fail({bad_result,Other})
    end.

%% Call and apply valid funs with wrong number of arguments.

badarity(Config) when is_list(Config) ->
    ?line Fun = fun() -> ok end,
    ?line Stupid = {stupid,arguments},
    ?line Args = [some,{stupid,arguments},here],

    %% Simple call.

    ?line Res = (catch Fun(some, Stupid, here)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ?line ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	_ ->
	    ?line ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ?line ?t:fail({bad_result,Res})
    end,

    %% Apply.

    ?line Res2 = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res2 of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ?line ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]);
	_ ->
	    ?line ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]),
	    ?line ?t:fail({bad_result,Res2})
    end,
    ok.

%% Call and apply valid external funs with wrong number of arguments.

ext_badarity(Config) when is_list(Config) ->
    ?line Fun = fun ?MODULE:nothing/0,
    ?line Stupid = {stupid,arguments},
    ?line Args = [some,{stupid,arguments},here],

    %% Simple call.

    ?line Res = (catch Fun(some, Stupid, here)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ?line ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]);
	_ ->
	    ?line ok = io:format("~p(~p) -> ~p\n", [Fun,Args,Res]),
	    ?line ?t:fail({bad_result,Res})
    end,

    %% Apply.

    ?line Res2 = (catch apply(Fun, Args)),
    erlang:garbage_collect(),
    erlang:yield(),
    case Res2 of
	{'EXIT',{{badarity,{Fun,Args}},_}} ->
	    ?line ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]);
	_ ->
	    ?line ok = io:format("apply(~p, ~p) -> ~p\n", [Fun,Args,Res2]),
	    ?line ?t:fail({bad_result,Res2})
    end,
    ok.

nothing() ->
    ok.

%% Test equality of funs.

equality(Config) when is_list(Config) ->
    F0 = fun() -> 1 end,
    F0_copy = copy_term(F0),
    ?line true = eq(F0, F0),
    ?line true = eq(F0, F0_copy),

    %% Compare different arities.
    F1 = fun(X) -> X + 1 end,
    ?line true = eq(F1, F1),
    ?line false = eq(F0, F1),
    ?line false = eq(F0_copy, F1),

    %% Compare different environments.
    G1 = make_fun(1),
    G2 = make_fun(2),
    ?line true = eq(G1, G1),
    ?line true = eq(G2, G2),
    ?line false = eq(G1, G2),
    ?line false = eq(G2, G1),
    G1_copy = copy_term(G1),
    ?line true = eq(G1, G1_copy),

    %% Compare fun with binaries.
    B = list_to_binary([7,8,9]),
    ?line false = eq(B, G1),
    ?line false = eq(G1, B),

    %% Compare external funs.
    FF0 = fun aa:blurf/0,
    FF0_copy = copy_term(FF0),
    FF1 = fun erlang:abs/0,
    FF2 = fun erlang:exit/1,
    FF3 = fun erlang:exit/2,
    FF4 = fun z:ff/0,

    ?line true = eq(FF0, FF0),
    ?line true = eq(FF0, FF0_copy),
    ?line true = eq(FF1, FF1),
    ?line true = eq(FF2, FF2),
    ?line true = eq(FF3, FF3),
    ?line true = eq(FF4, FF4),
    ?line false = eq(FF0, FF1),
    ?line false = eq(FF0, FF2),
    ?line false = eq(FF0, FF3),
    ?line false = eq(FF0, FF4),
    ?line false = eq(FF1, FF0),
    ?line false = eq(FF1, FF2),
    ?line false = eq(FF1, FF3),
    ?line false = eq(FF1, FF4),
    ?line false = eq(FF2, FF3),
    ?line false = eq(FF2, FF4),
    ?line false = eq(FF3, FF4),

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
	    
ordering(doc) -> "Tests ordering of funs.";
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

    ?line true = FF0 < FF1,
    ?line true = FF1 < FF2,
    ?line true = FF2 < FF3,
    ?line true = FF3 < FF4,

    ?line true = FF0 > F1,
    ?line true = FF0 > F2,
    ?line true = FF0 > F3,
    ?line true = FF4 > F1,
    ?line true = FF4 > F2,
    ?line true = FF4 > F3,

    ?line true = F1 == F1,
    ?line true = F1 == F1_copy,
    ?line true = F1 /= F2,

    ?line true = F1 < F2,
    ?line true = F2 > F1,
    ?line true = F2 < F3,
    ?line true = F3 > F2,

    ?line false = F1 > F2,
    ?line false = F2 > F3,

    %% Compare with binaries.

    B = list_to_binary([7,8,9,10]),
    ?line false = B == F1,
    ?line false = F1 == B,

    ?line true = F1 < B,
    ?line true = B > F2,

    ?line false = F1 > B,
    ?line false = B < F2,

    ?line false = F1 >= B,
    ?line false = B =< F2,

    %% Compare module funs with binaries.
    ?line false = B == FF1,
    ?line false = FF1 == B,

    ?line true = FF1 < B,
    ?line true = B > FF2,

    ?line false = FF1 > B,
    ?line false = B < FF2,

    ?line false = FF1 >= B,
    ?line false = B =< FF2,

    %% Create a port and ref.

    ?line Path = ?config(priv_dir, Config),
    ?line AFile = filename:join(Path, "vanilla_file"),
    ?line P = open_port(AFile, [out]),
    ?line R = make_ref(),

    %% Compare funs with ports and refs.

    ?line true = R < F3,
    ?line true = F3 > R,
    ?line true = F3 < P,
    ?line true = P > F3,

    ?line true = R =< F3,
    ?line true = F3 >= R,
    ?line true = F3 =< P,
    ?line true = P >= F3,

    ?line false = R > F3,
    ?line false = F3 < R,
    ?line false = F3 > P,
    ?line false = P < F3,

    %% Compare funs with conses and nils.

    ?line true = F1 < [a],
    ?line true = F1 < [],
    ?line true = [a,b] > F1,
    ?line true = [] > F1,

    ?line false = [1] < F1,
    ?line false = [] < F1,
    ?line false = F1 > [2],
    ?line false = F1 > [],

    ?line false = [1] =< F1,
    ?line false = [] =< F1,
    ?line false = F1 >= [2],
    ?line false = F1 >= [],

    %% Compare module funs with conses and nils.

    ?line true = FF1 < [a],
    ?line true = FF1 < [],
    ?line true = [a,b] > FF1,
    ?line true = [] > FF1,

    ?line false = [1] < FF1,
    ?line false = [] < FF1,
    ?line false = FF1 > [2],
    ?line false = FF1 > [],

    ?line false = [1] =< FF1,
    ?line false = [] =< FF1,
    ?line false = FF1 >= [2],
    ?line false = FF1 >= [],
    ok.

make_fun(X, Y) ->
    fun(A) -> A*X+Y end.

fun_to_port(doc) -> "Try sending funs to ports (should fail).";
fun_to_port(suite) -> [];
fun_to_port(Config) when is_list(Config) ->
    ?line fun_to_port(Config, xxx),
    ?line fun_to_port(Config, fun() -> 42 end),
    ?line fun_to_port(Config, [fun() -> 43 end]),
    ?line fun_to_port(Config, [1,fun() -> 44 end]),
    ?line fun_to_port(Config, [0,1|fun() -> 45 end]),
    B64K = build_io_list(65536),
    ?line fun_to_port(Config, [B64K,fun() -> 45 end]),
    ?line fun_to_port(Config, [B64K|fun() -> 45 end]),
    ok.

fun_to_port(Config, IoList) ->
    Path = ?config(priv_dir, Config),
    AFile = filename:join(Path, "vanilla_file"),
    Port = open_port(AFile, [out]),
    case catch port_command(Port, IoList) of
	{'EXIT',{badarg,_}} -> ok;
	Other -> ?t:fail({unexpected_retval,Other})
    end.

build_io_list(0) -> [];
build_io_list(1) -> [7];
build_io_list(N) ->
    L = build_io_list(N div 2),
    case N rem 2 of
	0 -> [L|L];
	1 -> [7,L|L]
    end.

t_hash(doc) -> "Test the hash/2 BIF on funs.";
t_hash(suite) -> [];
t_hash(Config) when is_list(Config) ->
    F1 = fun(_X) -> 1 end,
    F2 = fun(_X) -> 2 end,
    ?line true = hash(F1) /= hash(F2),

    G1 = make_fun(1, 2, 3),
    G2 = make_fun(1, 2, 3),
    G3 = make_fun(1, 2, 4),
    ?line true = hash(G1) == hash(G2),
    ?line true = hash(G2) /= hash(G3),

    FF0 = fun erlang:abs/1,
    FF1 = fun erlang:exit/1,
    FF2 = fun erlang:exit/2,
    FF3 = fun blurf:exit/2,
    ?line true = hash(FF0) =/= hash(FF1),
    ?line true = hash(FF0) =/= hash(FF2),
    ?line true = hash(FF0) =/= hash(FF3),
    ?line true = hash(FF1) =/= hash(FF2),
    ?line true = hash(FF1) =/= hash(FF3),
    ?line true = hash(FF2) =/= hash(FF3),
    ok.

hash(Term) ->
    erlang:hash(Term, 16#7ffffff).

t_phash(doc) -> "Test the phash/2 BIF on funs.";
t_phash(suite) -> [];
t_phash(Config) when is_list(Config) ->
    F1 = fun(_X) -> 1 end,
    F2 = fun(_X) -> 2 end,
    ?line true = phash(F1) /= phash(F2),

    G1 = make_fun(1, 2, 3),
    G2 = make_fun(1, 2, 3),
    G3 = make_fun(1, 2, 4),
    ?line true = phash(G1) == phash(G2),
    ?line true = phash(G2) /= phash(G3),

    FF0 = fun erlang:abs/1,
    FF1 = fun erlang:exit/1,
    FF2 = fun erlang:exit/2,
    FF3 = fun blurf:exit/2,
    ?line true = phash(FF0) =/= phash(FF1),
    ?line true = phash(FF0) =/= phash(FF2),
    ?line true = phash(FF0) =/= phash(FF3),
    ?line true = phash(FF1) =/= phash(FF2),
    ?line true = phash(FF1) =/= phash(FF3),
    ?line true = phash(FF2) =/= phash(FF3),
    
    ok.

phash(Term) ->
    erlang:phash(Term, 16#7ffffff).

t_phash2(doc) -> "Test the phash2/2 BIF on funs.";
t_phash2(suite) -> [];
t_phash2(Config) when is_list(Config) ->
    F1 = fun(_X) -> 1 end,
    F2 = fun(_X) -> 2 end,
    ?line true = phash2(F1) /= phash2(F2),

    G1 = make_fun(1, 2, 3),
    G2 = make_fun(1, 2, 3),
    G3 = make_fun(1, 2, 4),
    ?line true = phash2(G1) == phash2(G2),
    ?line true = phash2(G2) /= phash2(G3),

    FF0 = fun erlang:abs/1,
    FF1 = fun erlang:exit/1,
    FF2 = fun erlang:exit/2,
    FF3 = fun blurf:exit/2,
    ?line true = phash2(FF0) =/= phash2(FF1),
    ?line true = phash2(FF0) =/= phash2(FF2),
    ?line true = phash2(FF0) =/= phash2(FF3),
    ?line true = phash2(FF1) =/= phash2(FF2),
    ?line true = phash2(FF1) =/= phash2(FF3),
    ?line true = phash2(FF2) =/= phash2(FF3),
    
    ok.

phash2(Term) ->
    erlang:phash2(Term, 16#7ffffff).

make_fun(X, Y, Z) ->
    fun() -> {X,Y,Z} end.

md5(doc) -> "Test that MD5 bifs reject funs properly.";
md5(suite) -> [];
md5(Config) when is_list(Config) ->
    _ = size(erlang:md5_init()),

    %% Try funs in the i/o list.
    ?line bad_md5(fun(_X) -> 42 end),
    ?line bad_md5([fun(_X) -> 43 end]),
    ?line bad_md5([1,fun(_X) -> 44 end]),
    ?line bad_md5([1|fun(_X) -> 45 end]),
    ?line B64K = build_io_list(65536),
    ?line bad_md5([B64K,fun(_X) -> 46 end]),
    ?line bad_md5([B64K|fun(_X) -> 46 end]),
    ok.
    
bad_md5(Bad) ->
    {'EXIT',{badarg,_}} = (catch erlang:md5(Bad)).

refc(Config) when is_list(Config) ->
    ?line F1 = fun_factory(2),
    ?line {refc,2} = erlang:fun_info(F1, refc),
    ?line F2 = fun_factory(42),
    ?line {refc,3} = erlang:fun_info(F1, refc),

    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(fun() -> {refc,4} = erlang:fun_info(F1, refc) end),
    receive
	{'EXIT',Pid,normal} -> ok;
	Other -> ?line ?t:fail({unexpected,Other})
    end,
    ?line process_flag(trap_exit, false),
    ?line {refc,3} = erlang:fun_info(F1, refc),

    %% Garbage collect. Only the F2 fun will be left.
    ?line 7 = F1(5),
    ?line true = erlang:garbage_collect(),
    ?line 40 = F2(-2),
    ?line {refc,2} = erlang:fun_info(F2, refc),
    ok.

fun_factory(Const) ->
    fun(X) -> X + Const end.

refc_ets(Config) when is_list(Config) ->
    ?line F = fun(X) -> X + 33 end,
    ?line {refc,2} = erlang:fun_info(F, refc),

    refc_ets_set(F, [set]),
    refc_ets_set(F, [ordered_set]),
    refc_ets_bag(F, [bag]),
    refc_ets_bag(F, [duplicate_bag]),
    ok.

refc_ets_set(F1, Options) ->
    ?line io:format("~p", [Options]),
    ?line Tab = ets:new(kalle, Options),
    ?line true = ets:insert(Tab, {a_key,F1}),
    ?line 3 = fun_refc(F1),
    ?line [{a_key,F3}] = ets:lookup(Tab, a_key),
    ?line 4 = fun_refc(F1),
    ?line true = ets:insert(Tab, {a_key,not_a_fun}),
    ?line 3 = fun_refc(F1),
    ?line true = ets:insert(Tab, {another_key,F1}),
    ?line 4 = fun_refc(F1),
    ?line true = ets:delete(Tab),
    ?line 3 = fun_refc(F1),
    ?line 10 = F3(-23),
    ?line true = erlang:garbage_collect(),
    ?line 2 = fun_refc(F1),
    ok.

refc_ets_bag(F1, Options) ->
    ?line io:format("~p", [Options]),
    ?line Tab = ets:new(kalle, Options),
    ?line true = ets:insert(Tab, {a_key,F1}),
    ?line 3 = fun_refc(F1),
    ?line [{a_key,F3}] = ets:lookup(Tab, a_key),
    ?line 4 = fun_refc(F1),
    ?line true = ets:insert(Tab, {a_key,not_a_fun}),
    ?line 4 = fun_refc(F1),
    ?line true = ets:insert(Tab, {another_key,F1}),
    ?line 5 = fun_refc(F1),
    ?line true = ets:delete(Tab),
    ?line 3 = fun_refc(F1),
    ?line 10 = F3(-23),
    ?line true = erlang:garbage_collect(),
    ?line 2 = fun_refc(F1),
    ok.

refc_dist(Config) when is_list(Config) ->
    ?line {ok,Node} = start_node(fun_SUITE_refc_dist),
    ?line process_flag(trap_exit, true),
    ?line Pid = spawn_link(Node,
			   fun() -> receive
					Fun when is_function(Fun) ->
					    2 = fun_refc(Fun),
					    exit({normal,Fun}) end
			   end),
    ?line F = fun() -> 42 end,
    ?line 2 = fun_refc(F),
    ?line Pid ! F,
    F2 = receive
	     {'EXIT',Pid,{normal,Fun}} -> Fun;
	     Other -> ?line ?t:fail({unexpected,Other})
	 end,
    %% dist.c:net_mess2 have a reference to Fun for a while since
    %% Fun is passed in an exit signal. Wait until it is gone.
    ?line wait_until(fun () -> 4 =/= fun_refc(F2) end),
    ?line 3 = fun_refc(F2),
    ?line true = erlang:garbage_collect(),
    ?line 2 = fun_refc(F),
    refc_dist_send(Node, F).

refc_dist_send(Node, F) ->
    ?line Pid = spawn_link(Node,
			   fun() -> receive
					{To,Fun} when is_function(Fun) ->
					    wait_until(fun () ->
							       2 =:= fun_refc(Fun)
						       end),
					    To ! Fun
				    end
			   end),
    ?line 2 = fun_refc(F),
    Pid ! {self(),F},
    F2 = receive
	     Fun when is_function(Fun) -> Fun;
	     Other -> ?line ?t:fail({unexpected,Other})
	 end,
    receive {'EXIT',Pid,normal} -> ok end,
    %% No reference from dist.c:net_mess2 since Fun is passed
    %% in an ordinary message.
    ?line 3 = fun_refc(F),
    ?line 3 = fun_refc(F2),
    refc_dist_reg_send(Node, F).

refc_dist_reg_send(Node, F) ->
    ?line true = erlang:garbage_collect(),
    ?line 2 = fun_refc(F),
    ?line Ref = make_ref(),
    ?line Me = self(),
    ?line Pid = spawn_link(Node,
			   fun() ->
				   true = register(my_fun_tester, self()),
				   Me ! Ref,
				   receive
				       {Me,Fun} when is_function(Fun) ->
					   2 = fun_refc(Fun),
					   Me ! Fun
				   end
			   end),
    erlang:yield(),
    ?line 2 = fun_refc(F),
    receive Ref -> ok end,
    {my_fun_tester,Node} ! {self(),F},
    F2 = receive
	     Fun when is_function(Fun) -> Fun;
	     Other -> ?line ?t:fail({unexpected,Other})
	 end,
    receive {'EXIT',Pid,normal} -> ok end,

    ?line 3 = fun_refc(F),
    ?line 3 = fun_refc(F2),
    ok.
    
fun_refc(F) ->
    {refc,Count} = erlang:fun_info(F, refc),
    Count.

const_propagation(Config) when is_list(Config) ->
    ?line Fun1 = fun start_node/1,
    ?line 2 = fun_refc(Fun1),
    ?line Fun2 = Fun1,
    ?line my_cmp({Fun1,Fun2}),

    ?line Fun3 = fun() -> ok end,
    ?line 2 = fun_refc(Fun3),
    ?line Fun4 = Fun3,
    ?line my_cmp({Fun3,Fun4}),
    ok.

my_cmp({Fun,Fun}) -> ok;
my_cmp({Fun1,Fun2}) ->
    io:format("Fun1: ~p", [erlang:fun_info(Fun1)]),
    io:format("Fun2: ~p", [erlang:fun_info(Fun2)]),
    ?t:fail().

t_arity(Config) when is_list(Config) ->
    ?line 0 = fun_arity(fun() -> ok end),
    ?line 0 = fun_arity(fun() -> Config end),
    ?line 1 = fun_arity(fun(X) -> X+1 end),
    ?line 1 = fun_arity(fun(X) -> Config =:= X end),
    A = id(42),

    %% Test that the arity is transferred properly.
    ?line process_flag(trap_exit, true),
    ?line {ok,Node} = start_node(fun_test_arity),
    ?line hello_world = spawn_call(Node, fun() -> hello_world end),
    ?line 0 = spawn_call(Node, fun(X) -> X end),
    ?line 42 = spawn_call(Node, fun(_X) -> A end),
    ?line 43 = spawn_call(Node, fun(X, Y) -> A+X+Y end),
    ?line 1 = spawn_call(Node, fun(X, Y) -> X+Y end),
    ?line 45 = spawn_call(Node, fun(X, Y, Z) -> A+X+Y+Z end),
    ok.

t_is_function2(Config) when is_list(Config) ->
    false = is_function(id({a,b}), 0),
    false = is_function(id({a,b}), 234343434333433433),
    ?line true = is_function(fun() -> ok end, 0),
    ?line true = is_function(fun(_) -> ok end, 1),
    ?line false = is_function(fun(_) -> ok end, 0),

    ?line true = is_function(fun erlang:abs/1, 1),
    ?line true = is_function(fun erlang:abs/99, 99),
    ?line false = is_function(fun erlang:abs/1, 0),
    ?line false = is_function(fun erlang:abs/99, 0),

    ?line false = is_function(id(self()), 0),
    ?line false = is_function(id({a,b,c}), 0),
    ?line false = is_function(id({a}), 0),
    ?line false = is_function(id([a,b,c]), 0),

    %% Bad arity argument.
    ?line bad_arity(a),
    ?line bad_arity(-1),
    ?line bad_arity(-9738974938734938793873498378),
    ?line bad_arity([]),
    ?line bad_arity(fun() -> ok end),
    ?line bad_arity({}),
    ?line bad_arity({a,b}),
    ?line bad_arity(self()),
    ok.

bad_arity(A) ->
    {'EXIT',_} = (catch is_function(fun() -> ok end, A)),
    {'EXIT',_} = (catch is_function(no_fun, A)),
    ok.

t_fun_info(Config) when is_list(Config) ->
    ?line F = fun t_fun_info/1,
    ?line try F(blurf) of
	      FAny ->
		  io:format("should fail; returned ~p\n", [FAny]),
		  ?line ?t:fail()
	  catch
	      error:function_clause -> ok
	  end,
    ?line {module,?MODULE} = erlang:fun_info(F, module),
    ?line case erlang:fun_info(F, name) of
	      undefined ->
		  ?line ?t:fail();
	      _ -> ok
	  end,
    ?line {arity,1} = erlang:fun_info(F, arity),
    ?line {env,[]} = erlang:fun_info(F, env),
    ?line verify_not_undef(F, index),
    ?line verify_not_undef(F, uniq),
    ?line verify_not_undef(F, new_index),
    ?line verify_not_undef(F, new_uniq),
    ?line verify_not_undef(F, refc),
    ?line {'EXIT',_} = (catch erlang:fun_info(F, blurf)),    

    %% Module fun.
    ?line FF = fun ?MODULE:t_fun_info/1,
    ?line try FF(blurf) of
	      FFAny ->
		  io:format("should fail; returned ~p\n", [FFAny]),
		  ?line ?t:fail()
	  catch
	      error:function_clause -> ok
	  end,

    ?line {module,?MODULE} = erlang:fun_info(FF, module),
    ?line {name,t_fun_info} = erlang:fun_info(FF, name),
    ?line {arity,1} = erlang:fun_info(FF, arity),
    ?line {env,[]} = erlang:fun_info(FF, env),
    ?line verify_undef(FF, index),
    ?line verify_undef(FF, uniq),
    ?line verify_undef(FF, new_index),
    ?line verify_undef(FF, new_uniq),
    ?line verify_undef(FF, refc),
    ?line {'EXIT',_} = (catch erlang:fun_info(FF, blurf)),

    %% Not fun.
    ?line bad_info(abc),
    ?line bad_info(42),
    ?line bad_info({fun erlang:list_to_integer/1}),
    ?line bad_info([42]),
    ?line bad_info([]),
    ?line bad_info(self()),
    ?line bad_info(<<>>),
    ?line bad_info(<<1,2>>),
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
	    io:format("should fail; returned ~p\n", [Any]),
	    ?t:fail()
    catch
	error:badarg -> ok
    end.

verify_undef(Fun, Tag) ->
    {Tag,undefined} = erlang:fun_info(Fun, Tag).

verify_not_undef(Fun, Tag) ->
    case erlang:fun_info(Fun, Tag) of
	{Tag,undefined} ->
	    ?t:fail();
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
	      Other -> ?t:fail({bad_message,Other})
	  after 10000 ->
		  ?t:fail(timeout_waiting_for_result)
	  end,
    receive
	{'EXIT',Pid,normal} -> ok;
	Other2 -> ?t:fail({bad_message_waiting_for_exit,Other2})
    after 10000 ->
	    ?t:fail(timeout_waiting_for_exit)
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

% stop_node(Node) ->
%    test_server:stop_node(Node).
