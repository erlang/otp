%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2010. All Rights Reserved.
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

-module(reltool_test_lib).
-compile(export_all).

-include("reltool_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) when is_list(Config)->
    global:register_name(reltool_global_logger, group_leader()),
    incr_timetrap(Config, 5).

end_per_suite(Config) when is_list(Config)->
    global:unregister_name(reltool_global_logger),
    ok.

incr_timetrap(Config, Times) ->
    Key = tc_timeout,
    KeyPos = 1,
    NewTime = 
	case lists:keysearch(Key, KeyPos, Config) of
	    {value, {Key, OldTime}} ->
		(timer:minutes(1) + OldTime) * Times;
	    false ->
		timer:minutes(1) * Times
	end,
    lists:keystore(Key, KeyPos, Config, {Key, NewTime}).	    

set_kill_timer(Config) ->
    case init:get_argument(reltool_test_timeout) of
	{ok, _} -> 
	    Config;
	_ ->
	    Time = 
		case lookup_config(tc_timeout, Config) of
		    [] ->
			timer:minutes(5);
		    ConfigTime when is_integer(ConfigTime) ->
			ConfigTime
		end,
	    WatchDog = test_server:timetrap(Time),
	    [{kill_timer, WatchDog} | Config]
    end.

reset_kill_timer(Config) ->
    DogKiller = 
	case get(reltool_test_server) of
	    true ->
		fun(P) when is_pid(P) -> P ! stop;
		   (_) -> ok 
		end;
	    _ ->
		fun(Ref) -> test_server:timetrap_cancel(Ref) end
	end,
    case lists:keysearch(kill_timer, 1, Config) of
	{value, {kill_timer, WatchDog}} ->
	    DogKiller(WatchDog), 
	    lists:keydelete(kill_timer, 1, Config);
	_ ->
	    Config
    end.

lookup_config(Key,Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    []
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wx_init_per_suite(Config) ->
    {_Pid, Ref} = 
	spawn_monitor(fun() ->
			      %% Avoid test case crash if wx master process dies
			      process_flag(trap_exit, true),
			      try 
				  case os:type() of
				      {unix,darwin} ->
					  exit({skipped, "Can not test on MacOSX"});
				      {unix, _} ->
					  io:format("DISPLAY ~s~n", [os:getenv("DISPLAY")]),
					  case ct:get_config(xserver, none) of
					      none   -> ignore;
					      Server -> os:putenv("DISPLAY", Server)
					  end;
				      _ -> 
					  ignore
				  end,
				  wx:new(),
				  wx:destroy()
			      catch 
				  error:undef ->
				      exit({skipped, "No wx compiled for this platform"});
				    _:Reason ->
				      exit({skipped, lists:flatten(io_lib:format("Start wx failed: ~p", [Reason]))})
			      end,
			      exit(normal)			      
		      end),
    receive
	{'DOWN', Ref, _, _, normal} ->
	    init_per_suite(Config);	    
	{'DOWN', Ref, _, _, {skipped, _} = Skipped} ->
	    Skipped;
	{'DOWN', Ref, _, _, Reason} ->
	    exit({wx_init_per_suite, Reason})
    after timer:minutes(1) ->
	    exit({wx_init_per_suite, timeout})
    end.

wx_end_per_suite(Config) ->
    end_per_suite(Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_testcase(_Func, Config) when is_list(Config) ->
    set_kill_timer(Config),
    Config.

end_per_testcase(_Func, Config) when is_list(Config) ->
    reset_kill_timer(Config),
    Config.

%% Backwards compatible with test_server
tc_info(suite) -> [];
tc_info(doc) -> "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Use ?log(Format, Args) as wrapper
log(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Format2 = lists:concat([File, "(", Line, ")", ": ", Format]),
    log(Format2, Args).

log(Format, Args) ->
    case global:whereis_name(reltool_global_logger) of
	undefined ->
	    io:format(user, Format, Args);
	Pid ->
	    io:format(Pid, Format, Args)
    end.

verbose(Format, Args, File, Line) ->
    Arg = reltool_test_verbose,
    case get(Arg) of
	false ->
	    ok;
	true ->
	    log(Format, Args, File, Line);
	undefined ->
	    case init:get_argument(Arg) of
		{ok, List} when is_list(List) ->
		    case lists:last(List) of
			["true"] ->
			    put(Arg, true),
			    log(Format, Args, File, Line);
			_ ->
			    put(Arg, false),
			    ok
		    end;
		_ ->
		    put(Arg, false),
		    ok
	    end
    end.

error(Format, Args, File, Line) ->
    global:send(reltool_global_logger, {failed, File, Line}),
    Fail = {filename:basename(File),Line,Args},
    case global:whereis_name(reltool_test_case_sup) of
	undefined -> ignore;
	Pid -> Pid ! Fail
	    %% 	    global:send(reltool_test_case_sup, Fail),
    end,
    log("<ERROR>~n" ++ Format, Args, File, Line).


pick_msg() ->
    receive
	Message -> Message
    after 4000 -> timeout
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Utility functions

user_available(Config) ->
    false /= proplists:get_value(user, Config, false).
   	

wx_destroy(Frame, Config) ->
    case proplists:get_value(user, Config, false) of
	false ->
	    timer:sleep(100),
	    ?m(ok, wxFrame:destroy(Frame)),
	    ?m(ok, wx:destroy());
	true ->
	    timer:sleep(500),
	    ?m(ok, wxFrame:destroy(Frame)),
	    ?m(ok, wx:destroy());	
	step -> %% Wait for user to close window
	    ?m(ok, wxEvtHandler:connect(Frame, close_window, [{skip,true}])),
	    wait_for_close()
    end.

wait_for_close() ->
    receive 
	#wx{event=#wxClose{}} ->
	    ?log("Got close~n",[]),
	    ?m(ok, wx:destroy());
	#wx{obj=Obj, event=Event} ->
	    try 
		Name = wxTopLevelWindow:getTitle(Obj),
		?log("~p Event: ~p~n", [Name, Event])
	    catch _:_ ->
		?log("Event: ~p~n", [Event])
	    end,
	    wait_for_close();
	Other ->
	    ?log("Unexpected: ~p~n", [Other]),
	    wait_for_close()
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A small test server, which can be run standalone in a shell

run_test(Test = {_,_},Config) ->
    run_test([Test],Config);
run_test([{Module, TC} | Rest], Config) ->
    log("\n\n=== Eval test suite: ~w ===~n", [Module]),
    case catch Module:init_per_suite(Config) of
	{skipped, Reason} ->
	    log("Test suite skipped: ~s~n", [Reason]),
	    [{skipped, Reason}];
	NewConfig when is_list(NewConfig) ->
	    Res =
		if
		    TC =:= all ->
			[do_run_test(Module, Test, NewConfig) || Test <- Module:all()];
		    is_list(TC) ->
			[do_run_test(Module, Test, NewConfig) || Test <- TC];
		    true ->
			[do_run_test(Module, TC, NewConfig)]
		end,
	    Module:end_per_suite(NewConfig),
	    Res ++ run_test(Rest, NewConfig);
	Error ->
	    ?error("Test suite skipped: ~w~n", [Error]),
	    [{skipped, Error}]
    end;
run_test([], _Config) ->
    [].

do_run_test(Module, all, Config) ->
    All = [{Module, Test} || Test <- Module:all()],
    run_test(All, Config);
do_run_test(Module, TestCase, Config) ->
    log("Eval test case: ~w~n", [{Module, TestCase}]),
    Sec = timer:seconds(1) * 1000,
    {T, Res} =
	timer:tc(?MODULE, eval_test_case, [Module, TestCase, Config]),
    log("Tested ~w in ~w sec~n", [TestCase, T div Sec]),
    {T div Sec, Res}.
    
eval_test_case(Mod, Fun, Config) ->
    flush(),
    global:register_name(reltool_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, test_case_evaluator, [Mod, Fun, [Config]]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config),
    global:unregister_name(reltool_test_case_sup),
    process_flag(trap_exit, Flag),
    R.

test_case_evaluator(Mod, Fun, [Config]) ->
    NewConfig = Mod:init_per_testcase(Fun, Config),
    R = apply(Mod, Fun, [NewConfig]),
    Mod:end_per_testcase(Fun, NewConfig),
    exit({test_case_ok, R}).

wait_for_evaluator(Pid, Mod, Fun, Config) ->
    receive
	{'EXIT', Pid, {test_case_ok, _PidRes}} ->
	    Errors = flush(),
	    Res = 
		case Errors of
		    [] -> ok;
		    Errors -> failed
		end,
	    {Res, {Mod, Fun}, Errors};
	{'EXIT', Pid, {skipped, Reason}} ->
	    log("<WARNING> Test case ~w skipped, because ~p~n",
		[{Mod, Fun}, Reason]),
	    Mod:end_per_testcase(Fun, Config),
	    {skip, {Mod, Fun}, Reason};
	{'EXIT', Pid, Reason} ->
	    log("<ERROR> Eval process ~w exited, because\n\t~p~n",
		[{Mod, Fun}, Reason]),
	    Mod:end_per_testcase(Fun, Config),
	    {crash, {Mod, Fun}, Reason}
    end.

flush() ->
    receive Msg -> [Msg | flush()]
    after 0 -> []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
