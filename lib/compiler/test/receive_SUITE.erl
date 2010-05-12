%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2010. All Rights Reserved.
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

-export([all/1,init_per_testcase/2,fin_per_testcase/2,
	 recv/1,coverage/1,otp_7980/1,ref_opt/1]).

-include("test_server.hrl").

init_per_testcase(_Case, Config) ->
    ?line Dog = test_server:timetrap(test_server:minutes(2)),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(suite) ->
    test_lib:recompile(?MODULE),
    [recv,coverage,otp_7980,ref_opt].

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
    ?line Sources = filelib:wildcard(filename:join([DataDir,"ref_opt","*.erl"])),
    ?line test_lib:p_run(fun(Src) ->
				 do_ref_opt(Src, PrivDir)
			 end, Sources),
    ok.

do_ref_opt(Source, PrivDir) ->
    try
	{ok,Mod} = c:c(Source, [{outdir,PrivDir}]),
	ok = Mod:Mod(),
	Base = filename:rootname(filename:basename(Source), ".erl"),
	BeamFile = filename:join(PrivDir, Base),
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

id(I) -> I.
