%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2013. All Rights Reserved.
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
%%% Purpose : Compiles various modules with tough code

-module(receive_SUITE).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2,
	 init_per_testcase/2,end_per_testcase/2,
	 export/1,recv/1,coverage/1,otp_7980/1,ref_opt/1,
	 wait/1]).

-include_lib("test_server/include/test_server.hrl").

init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

suite() -> [{ct_hooks,[ts_install_cth]}].

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
    ?line Pid = spawn_link(fun() -> loop(#state{}) end),
    Self = self(),
    ?line Pid ! {Self,test},
    receive
	{ok,test} -> ok;
	{error,Other} ->
	    io:format("Got unpexected ~p", [Other]),
	    ?line ?t:fail()
    after 10000 ->
	    ?line ?t:fail(no_answer)
    end,
    receive
	X ->
	    io:format("Unexpected extra message: ~p", [X]),
	    ?line ?t:fail()
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
    ?line 59 = tuple_to_values(infinity, x),
    ?line 61 = tuple_to_values(999999, x),
    ?line 0 = tuple_to_values(1, x),
    ok.

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
    ?line DataDir = ?config(data_dir, Config),
    ?line PrivDir = ?config(priv_dir, Config),
    Sources = filelib:wildcard(filename:join([DataDir,"ref_opt","*.{erl,S}"])),
    ?line test_lib:p_run(fun(Src) ->
				 do_ref_opt(Src, PrivDir)
			 end, Sources),
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
    catch Class:Error ->
	    io:format("~s: ~p ~p\n~p\n",
		      [Source,Class,Error,erlang:get_stacktrace()]),
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

export(Config) when is_list(Config) ->
    Ref = make_ref(),
    ?line self() ! {result,Ref,42},
    ?line 42 = export_1(Ref),
    ?line {error,timeout} = export_1(Ref),
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

wait(Config) when is_list(Config) ->
    self() ! <<42>>,
    <<42>> = wait_1(r, 1, 2),
    {1,2,3} = wait_1(1, 2, 3),
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
