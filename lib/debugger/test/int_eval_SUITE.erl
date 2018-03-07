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
-module(int_eval_SUITE).

%% Purpose: Deeper test of the evaluator.

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2,
	 bifs_outside_erlang/1, spawning/1, applying/1,
	 catch_and_throw/1, external_call/1, test_module_info/1,
	 apply_interpreted_fun/1, apply_uninterpreted_fun/1,
	 interpreted_exit/1, otp_8310/1, stacktrace/1, maps/1,
	 call_inside_binary/1]).

%% Helpers.
-export([applier/3]).

-define(IM, my_int_eval_module).

-include_lib("common_test/include/ct.hrl").

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap,{minutes,1}}].

all() -> 
    [bifs_outside_erlang, spawning, applying,
     catch_and_throw, external_call, test_module_info,
     apply_interpreted_fun, apply_uninterpreted_fun,
     interpreted_exit, otp_8310, stacktrace, maps,
     call_inside_binary].

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
    DataDir = proplists:get_value(data_dir, Config),
    {module,?IM} = int:i(filename:join(DataDir, ?IM)),
    ok = io:format("Interpreted modules: ~p",[int:interpreted()]),
    Config.

end_per_testcase(_Case, _Config) ->
    ok = io:format("Interpreted modules: ~p", [int:interpreted()]),
    ok.

%% Test that BIFs outside the erlang module are correctly evaluated.
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
    ok = spawn_eval(Fun),
    ok.

%% Try evalutate spawn_link/3.
spawning(Config) when is_list(Config) ->
    ok = spawn_eval(fun() -> ?IM:spawn_test() end).

%% Try various sorts of applies.
applying(Config) when is_list(Config) ->
    Fun = fun({number,X}, {number,Y}) -> X+Y end,
    ok = spawn_eval(fun() -> ?IM:apply_test(Fun) end).

%% Test catch and throw/1.
catch_and_throw(Config) when is_list(Config) ->
	{a,ball} = spawn_eval(fun() -> ok = ?IM:catch_a_ball(),
				       catch ?IM:throw_a_ball() end),

	%% Throw and catch without any extra outer catch.

	process_flag(trap_exit, true),
	Pid1 = spawn_link(fun() -> exit(?IM:catch_a_ball()) end),
	receive
	    {'EXIT',Pid1,ok} -> ok;
	    {'EXIT',Pid1,Bad1} -> ct:fail({bad_message,Bad1})
	after 5000 ->
		ct:fail(timeout)
	end,


	%% Throw without catch.

	Pid2 = spawn_link(fun() -> ?IM:throw_a_ball() end),
	receive
	    {'EXIT',Pid2,{{nocatch,{a,ball}},[_|_]}} -> ok;
	    {'EXIT',Pid2,Bad2} -> ct:fail({bad_message,Bad2})
	after 5000 ->
		ct:fail(timeout)
	end,

	ok = ?IM:more_catch(fun(_) -> ?IM:exit_me() end),
	ok = ?IM:more_catch(fun(_) -> exit({unint, exit}) end),
	{a, ball} = ?IM:more_catch(fun(_) -> ?IM:throw_a_ball() end),
	{b, ball} = ?IM:more_catch(fun(_) -> throw({b,ball}) end),

	ExitInt = {'EXIT',{int,exit}},
	ExitU   = {'EXIT',{unint,exit}},

	ExitInt = (catch ?IM:more_nocatch(fun(_) -> ?IM:exit_me() end)),
	ExitU   = (catch ?IM:more_nocatch(fun(_) -> exit({unint, exit}) end)),
	{a, ball} = (catch {error, ?IM:more_nocatch(fun(_) -> ?IM:throw_a_ball() end)}),
	{b, ball} = (catch {error, ?IM:more_nocatch(fun(_) -> throw({b,ball}) end)}),
	ok.

%% Test external calls.
external_call(Config) when is_list(Config) ->
    ok = spawn_eval(fun() -> ?IM:external_call_test({some,stupid,data}) end).

%% Test the module_info/0,1 functions.
test_module_info(Config) when is_list(Config) ->
    ModInfo = ?IM:module_info(),
    {value,{exports,Exp}} = lists:keysearch(exports, 1, ModInfo),
    {value,{attributes,Attr}} = lists:keysearch(attributes, 1, ModInfo),
    Exp = ?IM:module_info(exports),
    Attr = ?IM:module_info(attributes),
    {value,{stupid_attribute,[{a,b}]}} =
	lists:keysearch(stupid_attribute, 1, Attr),

    %% Check exports using a list comprehension in the module itself.

    ok = ?IM:check_exports(Exp),

    %% Call module_info/0,1 from the module itself.

    ok = ?IM:check_module_info(ModInfo, Exp),

    ok.

%% Apply a fun defined in interpreted code.
apply_interpreted_fun(Config) when is_list(Config) ->

    %% Called from uninterpreted code
    F1 = spawn_eval(fun() -> ?IM:give_me_a_fun_0() end),
    perfectly_alright = spawn_eval(fun() -> F1() end),
    ATerm = {a,term},
    F2 = spawn_eval(fun() -> ?IM:give_me_a_fun_0(ATerm) end),
    {ok,ATerm} = spawn_eval(fun() -> F2() end),

    %% Called from uninterpreted code, badarity
    {'EXIT',{{badarity,{F1,[snape]}},[{?MODULE,_,_,_}|_]}} =
	spawn_eval(fun() -> F1(snape) end),

    %% Called from uninterpreted code, error in fun
    F3 = spawn_eval(fun() -> ?IM:give_me_a_bad_fun() end),
    {'EXIT',{snape,[{?IM,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> F3(snape) end),

    %% Called from within interpreted code
    perfectly_alright = spawn_eval(fun() -> ?IM:do_apply(F1) end),

    %% Called from within interpreted code, badarity
    {'EXIT',{{badarity,{F1,[snape]}},[{?IM,do_apply,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1, snape) end),

    %% Called from within interpreted code, error in fun
    {'EXIT',{snape,[{?IM,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F3, snape) end),

    %% Try some more complex funs.
    F4 = ?IM:give_me_a_fun_1(14, 42),
    {false,yes,yeah,false} =
	F4({{1,nope},{14,yes},{42,yeah},{100,forget_it}}),
    [this_is_ok,me_too] =
	F4([{-24,no_way},{15,this_is_ok},{1333,forget_me},{37,me_too}]),

    %% OTP-5837
    %% Try fun with guard containing variable bound in environment
    [yes,no,no,no] = ?IM:otp_5837(1),

    ok.

%% Apply a fun defined outside interpreted code.
apply_uninterpreted_fun(Config) when is_list(Config) ->

    F1 = fun(snape) ->
		 erlang:error(snape);
	    (_Arg) ->
		 perfectly_alright
	 end,

    %% Ok
    perfectly_alright =
	spawn_eval(fun() -> ?IM:do_apply(F1, any_arg) end),

    %% Badarity (evaluated in dbg_debugged, which calls erlang:apply/2)
    {'EXIT',{{badarity,{F1,[]}},[{erlang,apply,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1) end),

    %% Error in fun
    {'EXIT',{snape,[{?MODULE,_FunName,_,_}|_]}} =
	spawn_eval(fun() -> ?IM:do_apply(F1, snape) end),

    ok.

%%
%% Try executing an interpreted exit/1 call.
%%

interpreted_exit(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    Reason = make_ref(),
    Pid = spawn_link(fun() -> ?IM:please_call_exit(Reason) end),
    receive
	{'EXIT',Pid,Reason} ->
	    ok;
	{'EXIT',Pid,BadReason} ->
	    ct:fail({bad_message,BadReason})
    after 10000 ->
	    ct:fail(timeout)
    end,
    ok.

%% OTP-8310. Bugfixes lc/bc and andalso/orelse.
otp_8310(Config) when is_list(Config) ->
    ok = ?IM:otp_8310(),
    ok.

applier(M, F, A) ->
    Res = apply(M, F, A),
    io:format("~p:~p(~p) => ~p\n", [M,F,A,Res]),
    Res.

stacktrace(Config) when is_list(Config) ->
    {done,Stk} = do_eval(Config, stacktrace),
    13 = length(Stk),
    OldStackTraceFlag = int:stack_trace(),
    int:stack_trace(no_tail),
    try
	Res = spawn_eval(fun() -> stacktrace:stacktrace() end),
	io:format("\nInterpreted (no_tail):\n~p", [Res]),
	{done,Stk} = Res
    after
	int:stack_trace(OldStackTraceFlag)
    end,
    ok.

maps(Config) when is_list(Config) ->
    Fun = fun () -> ?IM:empty_map_update([camembert]) end,
    {'EXIT',{{badmap,[camembert]},_}} = spawn_eval(Fun),
    [#{hello := 0, price := 0}] = spawn_eval(fun () -> ?IM:update_in_fun() end),
    ok.

call_inside_binary(Config) when is_list(Config) ->
    <<"1">> = ?IM:call_inside_binary(fun erlang:integer_to_binary/1),
    ok.

do_eval(Config, Mod) ->
    DataDir = proplists:get_value(data_dir, Config),
    ok = file:set_cwd(DataDir),

    {ok,Mod} = compile:file(Mod, [report,debug_info]),
    {module,Mod} = code:load_file(Mod),
    CompiledRes = Mod:Mod(),
    ok = io:format("Compiled:\n~p", [CompiledRes]),
    io:nl(),

    {module,Mod} = int:i(Mod),
    IntRes = Mod:Mod(),
    ok = io:format("Interpreted:\n~p", [IntRes]),

    CompiledRes = IntRes.

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
