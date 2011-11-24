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

%%
-module(int_eval_SUITE).

%% Purpose: Deeper test of the evaluator.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
	 bifs_outside_erlang/1, spawning/1, applying/1,
	 catch_and_throw/1, external_call/1, test_module_info/1,
	 apply_interpreted_fun/1, apply_uninterpreted_fun/1,
	 interpreted_exit/1, otp_8310/1, stacktrace/1]).

%% Helpers.
-export([applier/3]).

-define(IM, my_int_eval_module).

-include_lib("test_server/include/test_server.hrl").

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap,{minutes,1}}].

all() -> 
    [bifs_outside_erlang, spawning, applying,
     catch_and_throw, external_call, test_module_info,
     apply_interpreted_fun, apply_uninterpreted_fun,
     interpreted_exit, otp_8310, stacktrace].

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
    ?line DataDir = ?config(data_dir, Config),
    ?line {module,?IM} = int:i(filename:join(DataDir, ?IM)),
    ?line ok = io:format("Interpreted modules: ~p",[int:interpreted()]),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = io:format("Interpreted modules: ~p", [int:interpreted()]),
    ok.

bifs_outside_erlang(doc) ->
    "Test that BIFs outside the erlang module are correctly evaluated.";
bifs_outside_erlang(suite) ->
    [];
bifs_outside_erlang(Config) when is_list(Config) ->
    Fun = fun() ->
		  Id = ?IM:ets_new(),
		  Self = self(),
		  ok = io:format("Self: ~p", [Self]),
		  Info = ets:info(Id),
		  Self = proplists:get_value(owner, Info),
		  ?IM:ets_delete(Id),
		  ok
	  end,
    ?line ok = spawn_eval(Fun),
    ok.

spawning(doc) ->
    "Try evalutate spawn_link/3.";
spawning(suite) ->
    [];
spawning(Config) when is_list(Config) ->
    ?line ok = spawn_eval(fun() -> ?IM:spawn_test() end).

applying(doc) ->
    "Try various sorts of applies.";
applying(suite) ->
    [];
applying(Config) when is_list(Config) ->
    Fun = fun({number,X}, {number,Y}) -> X+Y end,
    ?line ok = spawn_eval(fun() -> ?IM:apply_test(Fun) end).

catch_and_throw(doc) ->
    "Test catch and throw/1.";
catch_and_throw(suite) ->
    [];
catch_and_throw(Config) when is_list(Config) ->
    {a,ball} = spawn_eval(fun() -> ok = ?IM:catch_a_ball(),
				   catch ?IM:throw_a_ball() end),

    %% Throw and catch without any extra outer catch.

    ?line process_flag(trap_exit, true),
    ?line Pid1 = spawn_link(fun() -> exit(?IM:catch_a_ball()) end),
    receive
	{'EXIT',Pid1,ok} -> ok;
	{'EXIT',Pid1,Bad1} -> ?line ?t:fail({bad_message,Bad1})
    after 5000 ->
	    ?line ?t:fail(timeout)
    end,


    %% Throw without catch.

    ?line Pid2 = spawn_link(fun() -> ?IM:throw_a_ball() end),
    receive
	{'EXIT',Pid2,{{nocatch,{a,ball}},[_|_]}} -> ok;
	{'EXIT',Pid2,Bad2} -> ?line ?t:fail({bad_message,Bad2})
    after 5000 ->
	    ?line ?t:fail(timeout)
    end,

    ?line ok = ?IM:more_catch(fun(_) -> ?IM:exit_me() end),
    ?line ok = ?IM:more_catch(fun(_) -> exit({unint, exit}) end),
    ?line {a, ball} = ?IM:more_catch(fun(_) -> ?IM:throw_a_ball() end),
    ?line {b, ball} = ?IM:more_catch(fun(_) -> throw({b,ball}) end),

    ExitInt = {'EXIT',{int,exit}},
    ExitU   = {'EXIT',{unint,exit}},

    ?line ExitInt = (catch ?IM:more_nocatch(fun(_) -> ?IM:exit_me() end)),
    ?line ExitU   = (catch ?IM:more_nocatch(fun(_) -> exit({unint, exit}) end)),
    ?line {a, ball} = (catch {error, ?IM:more_nocatch(fun(_) -> ?IM:throw_a_ball() end)}),
    ?line {b, ball} = (catch {error, ?IM:more_nocatch(fun(_) -> throw({b,ball}) end)}),
    ok.

external_call(doc) ->
    "Test external calls.";
external_call(suite) ->
    [];
external_call(Config) when is_list(Config) ->
    ?line ok = spawn_eval(fun() -> ?IM:external_call_test({some,stupid,data}) end).

test_module_info(doc) ->
    "Test the module_info/0,1 functions.";
test_module_info(suite) ->
    [];
test_module_info(Config) when is_list(Config) ->
    ?line ModInfo = ?IM:module_info(),
    ?line {value,{exports,Exp}} = lists:keysearch(exports, 1, ModInfo),
    ?line {value,{attributes,Attr}} = lists:keysearch(attributes, 1, ModInfo),
    ?line Exp = ?IM:module_info(exports),
    ?line Attr = ?IM:module_info(attributes),
    ?line {value,{stupid_attribute,[{a,b}]}} =
	lists:keysearch(stupid_attribute, 1, Attr),

    %% Check exports using a list comprehension in the module itself.

    ?line ok = ?IM:check_exports(Exp),

    %% Call module_info/0,1 from the module itself.

    ?line ok = ?IM:check_module_info(ModInfo, Exp),

    ok.

apply_interpreted_fun(doc) ->
    "Apply a fun defined in interpreted code.";
apply_interpreted_fun(suite) -> [];
apply_interpreted_fun(Config) when is_list(Config) ->

    %% Called from uninterpreted code
    ?line F1 = spawn_eval(fun() -> ?IM:give_me_a_fun_0() end),
    ?line perfectly_alright = spawn_eval(fun() -> F1() end),
    ?line ATerm = {a,term},
    ?line F2 = spawn_eval(fun() -> ?IM:give_me_a_fun_0(ATerm) end),
    ?line {ok,ATerm} = spawn_eval(fun() -> F2() end),

    %% Called from uninterpreted code, badarity
    ?line {'EXIT',{{badarity,{F1,[snape]}},[{?MODULE,_,_,_}|_]}} =
	spawn_eval(fun() -> F1(snape) end),

    %% Called from uninterpreted code, error in fun
    ?line F3 = spawn_eval(fun() -> ?IM:give_me_a_bad_fun() end),
    ?line {'EXIT',{snape,[{?IM,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> F3(snape) end),

    %% Called from within interpreted code
    ?line perfectly_alright = spawn_eval(fun() -> ?IM:do_apply(F1) end),

    %% Called from within interpreted code, badarity
    ?line {'EXIT',{{badarity,{F1,[snape]}},[{?IM,do_apply,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1, snape) end),

    %% Called from within interpreted code, error in fun
    ?line {'EXIT',{snape,[{?IM,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F3, snape) end),

    %% Try some more complex funs.
    ?line F4 = ?IM:give_me_a_fun_1(14, 42),
    ?line {false,yes,yeah,false} =
	F4({{1,nope},{14,yes},{42,yeah},{100,forget_it}}),
    ?line [this_is_ok,me_too] =
	F4([{-24,no_way},{15,this_is_ok},{1333,forget_me},{37,me_too}]),

    %% OTP-5837
    %% Try fun with guard containing variable bound in environment
    ?line [yes,no,no,no] = ?IM:otp_5837(1),

    ok.

apply_uninterpreted_fun(doc) ->
    "Apply a fun defined outside interpreted code.";
apply_uninterpreted_fun(suite) -> [];
apply_uninterpreted_fun(Config) when is_list(Config) ->

    ?line F1 = fun(snape) ->
		       erlang:error(snape);
		  (_Arg) ->
		       perfectly_alright
	       end,

    %% Ok
    ?line perfectly_alright =
	spawn_eval(fun() -> ?IM:do_apply(F1, any_arg) end),

    %% Badarity (evaluated in dbg_debugged, which calls erlang:apply/2)
    ?line {'EXIT',{{badarity,{F1,[]}},[{erlang,apply,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1) end),

    %% Error in fun
    ?line {'EXIT',{snape,[{?MODULE,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1, snape) end),

    ok.

%%
%% Try executing an interpreted exit/1 call.
%%

interpreted_exit(Config) when is_list(Config) ->
    ?line process_flag(trap_exit, true),
    ?line Reason = make_ref(),
    ?line Pid = spawn_link(fun() -> ?IM:please_call_exit(Reason) end),
    ?line receive
	      {'EXIT',Pid,Reason} ->
		  ok;
	      {'EXIT',Pid,BadReason} ->
		  ?line ?t:fail({bad_message,BadReason})
	  after 10000 ->
		  ?line ?t:fail(timeout)
	  end,
    ok.

otp_8310(doc) ->
    "OTP-8310. Bugfixes lc/bc and andalso/orelse.";
otp_8310(Config) when is_list(Config) ->
    ?line ok = ?IM:otp_8310(),
    ok.

applier(M, F, A) ->
    Res = apply(M, F, A),
    io:format("~p:~p(~p) => ~p\n", [M,F,A,Res]),
    Res.

stacktrace(Config) when is_list(Config) ->
    ?line {done,Stk} = do_eval(Config, stacktrace),
    ?line 13 = length(Stk),
    ?line OldStackTraceFlag = int:stack_trace(),
    ?line int:stack_trace(no_tail),
    try
	?line Res = spawn_eval(fun() -> stacktrace:stacktrace() end),
	?line io:format("\nInterpreted (no_tail):\n~p", [Res]),
	?line {done,Stk} = Res
	after
	    ?line int:stack_trace(OldStackTraceFlag)
	end,
    ok.


do_eval(Config, Mod) ->
    ?line DataDir = ?config(data_dir, Config),
    ?line ok = file:set_cwd(DataDir),

    ?line {ok,Mod} = compile:file(Mod, [report,debug_info]),
    ?line {module,Mod} = code:load_file(Mod),
    ?line CompiledRes = Mod:Mod(),
    ?line ok = io:format("Compiled:\n~p", [CompiledRes]),
    io:nl(),

    ?line {module,Mod} = int:i(Mod),
    ?line IntRes = Mod:Mod(),
    ?line ok = io:format("Interpreted:\n~p", [IntRes]),

    ?line CompiledRes = IntRes.

%%
%% Evaluate in another process, to prevent the test_case process to become
%% interpreted.
%%

spawn_eval(Fun) ->
    Self = self(),
    spawn_link(fun() -> Self ! (catch Fun()) end),
    receive
	Result ->
	    Result
    end.
