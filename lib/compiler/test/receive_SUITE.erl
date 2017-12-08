%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%% Purpose : Compiles various modules with tough code

-module(receive_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 export/1,recv/1,coverage/1,otp_7980/1,ref_opt/1,
	 wait/1]).

-include_lib("common_test/include/ct.hrl").

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    test_lib:recompile(?MODULE),
    [{group,p}].

groups() -> 
    [{p,test_lib:parallel(),
      [recv,coverage,otp_7980,ref_opt,export,wait]}].


init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

-record(state, {ena = true}).

recv(Config) when is_list(Config) ->
    Pid = spawn_link(fun() -> loop(#state{}) end),
    Self = self(),
    Pid ! {Self,test},
    receive
	{ok,test} -> ok;
	{error,Other} ->
	    io:format("Got unpexected ~p", [Other]),
	    ct:fail(unexpected)
    after 10000 ->
	    ct:fail(no_answer)
    end,
    receive
	X ->
	    io:format("Unexpected extra message: ~p", [X]),
	    ct:fail(unexpected)
    after 10 ->
	    ok
    end,
    ok.

loop(S) ->
    receive
	_ when S#state.ena == false ->
	    loop(S);
	{P,test} ->
	    P ! {ok,test},
	    loop(S);
	_X ->
	    loop(S)
    end.

coverage(Config) when is_list(Config) ->
    do_link(self()),
    do_unlink(self()),
    do_monitor_node(node(), true),
    do_monitor_node(node(), false),
    do_group_leader(group_leader(), self()),
    id(node(self())),

    erlang:'!'(self(), {a,10}),
    self() ! {b,20},
    [{a,10},{b,20}] = receive_all(),
    self() ! {c,42},
    receive
	{c,42} ->
	    ok
    after infinity ->
	    exit(cant_happen)
    end,

    self() ! 17,
    self() ! 19,
    59 = tuple_to_values(infinity, x),
    61 = tuple_to_values(999999, x),
    0 = tuple_to_values(1, x),

    {'EXIT',{{badmap,[]},_}} = (catch monitor_plus_badmap(self())),

    ok.

monitor_plus_badmap(Pid) ->
    monitor(process, Pid) + []#{}.

receive_all() ->
    receive
	Any ->
	    [Any|receive_all()]
    after 0 ->
	    []
    end.

do_monitor_node(Node, Bool) ->
    monitor_node(Node, Bool).

do_link(Pid) ->
    link(Pid).

do_unlink(Pid) ->
    unlink(Pid).

do_group_leader(Leader, Pid) ->
    group_leader(Leader, Pid).


%% cover sys_core_fold:tuple_to_values/2
tuple_to_values(infinity, X) ->
    {A,B} = case X of
		x ->
		    receive
			Any ->
			    {42,Any}
		    end
	    end,
    A+B;
tuple_to_values(Timeout, X) ->
    {A,B} = case X of
		x ->
		    receive
			Any ->
			    {42,Any}
		    after Timeout ->
			    {0,0}
		    end
	    end,
    A+B.

%% OTP-7980. Thanks to Vincent de Phily. The following code would
%% be inccorrectly optimized by beam_jump.

otp_7980(Config) when is_list(Config) ->
    7 = otp_7980_add_clients(10),
    ok.

otp_7980_add_clients(Count) ->
    Timeout = 42,
    lists:foldl(fun(_, N) ->
			case N of
                           1 -> ok;
			    _ -> receive after Timeout -> ok end
			end,
			N - 1
		end, Count, [1,2,3]).

ref_opt(Config) when is_list(Config) ->
    case ?MODULE of
	receive_SUITE -> ref_opt_1(Config);
	_ -> {skip,"Enough to run this case once."}
    end.

ref_opt_1(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    Sources = filelib:wildcard(filename:join([DataDir,"ref_opt","*.{erl,S}"])),
    test_lib:p_run(fun(Src) ->
			   do_ref_opt(Src, PrivDir)
		   end, Sources),
    cover_recv_instructions(),
    ok.

do_ref_opt(Source, PrivDir) ->
    try
	Ext = filename:extension(Source),
	{ok,Mod} = compile:file(Source, [report_errors,report_warnings,
					 {outdir,PrivDir}] ++
					[from_asm || Ext =:= ".S" ]),
	Base = filename:rootname(filename:basename(Source), Ext),
	code:purge(list_to_atom(Base)),
	BeamFile = filename:join(PrivDir, Base),
	code:load_abs(BeamFile),
	ok = Mod:Mod(),
	{beam_file,Mod,_,_,_,Code} = beam_disasm:file(BeamFile),
	case Base of
	    "no_"++_ ->
		[] = collect_recv_opt_instrs(Code);
	    "yes_"++_ ->
		[{recv_mark,{f,L}},{recv_set,{f,L}}] =
		    collect_recv_opt_instrs(Code)
	end,
	ok
    catch Class:Error:Stk ->
	    io:format("~s: ~p ~p\n~p\n", [Source,Class,Error,Stk]),
	    error
    end.

collect_recv_opt_instrs(Code) ->
    L = [ [I || I <- Is,
		begin
		    case I of
			{recv_mark,{f,_}} -> true;
			{recv_set,{f,_}} -> true;
			_ -> false
		    end
		end] || {function,_,_,_,Is} <- Code],
    lists:append(L).

cover_recv_instructions() ->
    %% We want to cover the handling of recv_mark and recv_set in beam_utils.
    %% Since those instructions are introduced in a late optimization pass,
    %% beam_utils:live_opt() will not see them unless the compilation is
    %% started from a .S file. The compile_SUITE:asm/1 test case will
    %% compile all test suite files to .S and then run them through the
    %% compiler again.
    %%
    %% Here will we will ensure that this modules contains recv_mark
    %% and recv_set instructions.
    Pid = spawn_link(fun() ->
			     receive {Parent,Ref} ->
				     Parent ! Ref
			     end
		     end),
    Ref = make_ref(),
    Pid ! {self(),Ref},
    receive
	Ref -> ok
    end.

export(Config) when is_list(Config) ->
    Ref = make_ref(),
    self() ! {result,Ref,42},
    42 = export_1(Ref),
    {error,timeout} = export_1(Ref),

    self() ! {result,Ref},
    {ok,Ref} = export_2(),

    ok.

export_1(Reference) ->
    id(Reference),
    receive
	{result,Reference,Result} ->
	    Result
    after 1 ->
	    Result = {error,timeout}
    end,
    %% Result ({x,1}) is used, but not the return value ({x,0})
    %% of the receive. Used to be incorrectly optimized
    %% by beam_block.
    id({build,self()}),
    Result.

export_2() ->
    receive {result,Result} -> ok end,
    {ok,Result}.

wait(Config) when is_list(Config) ->
    self() ! <<42>>,
    <<42>> = wait_1(r, 1, 2),
    {1,2,3} = wait_1(1, 2, 3),
    {'EXIT',{timeout_value,_}} = (catch receive after [] -> timeout end),
    ok.

wait_1(r, _, _) ->
    receive
	B when byte_size(B) > 0 ->
	    B
    end;
%% beam_utils would wrongly assume that wait/1 could fall through
%% to the next clause.
wait_1(A, B, C) ->
    {A,B,C}.

id(I) -> I.
