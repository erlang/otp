%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
%%%-------------------------------------------------------------------
%%% File    : wx_test_lib.erl
%%% Author  : Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%% Description : Library for testing wxerlang.
%%%
%%% Created : 30 Oct 2008 by Dan Gudmundsson <dan.gudmundsson@ericsson.com>
%%%-------------------------------------------------------------------
-module(wx_test_lib).
-compile(export_all).

-include("wx_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init_per_suite(Config) ->
    try 
	case os:type() of
	    {unix,darwin} ->
		exit("Can not test on MacOSX");
	    {unix, _} ->
		io:format("DISPLAY ~s~n", [os:getenv("DISPLAY")]),
		case ct:get_config(xserver, none) of
		    none -> ignore;
		    Server -> 
			os:putenv("DISPLAY", Server)
		end;
	    _ -> ignore
	end,
	wx:new(),
	wx:destroy(),
	Config
    catch 
	_:undef ->
	    {skipped, "No wx compiled for this platform"};
	_:Reason ->
	    {skipped, lists:flatten(io_lib:format("Start wx failed: ~p", [Reason]))}
    end.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Func, Config) ->
    global:register_name(wx_global_logger, group_leader()),
    Config.

end_per_testcase(_Func, Config) ->
    global:unregister_name(wx_global_logger),
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
    case global:whereis_name(wx_global_logger) of
	undefined ->
	    io:format(user, Format, Args);
	Pid ->
	    io:format(Pid, Format, Args)
    end.

verbose(Format, Args, File, Line) ->
    Arg = wx_test_verbose,
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
    global:send(wx_global_logger, {failed, File, Line}),
    Fail = {filename:basename(File),Line,Args},
    case global:whereis_name(wx_test_case_sup) of
	undefined -> ignore;
	Pid -> Pid ! Fail
	    %% 	    global:send(wx_test_case_sup, Fail),
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
    wx_close(Frame, Config),
    ?m(ok, wx:destroy()).

wx_close(Frame, Config) ->
    case proplists:get_value(user, Config, false) of
	false ->
	    timer:sleep(100),
	    ?m(ok, wxWindow:destroy(Frame));
	true ->
	    timer:sleep(500),
	    ?m(ok, wxWindow:destroy(Frame));
	step -> %% Wait for user to close window
	    ?m(ok, wxEvtHandler:connect(Frame, close_window, [{skip,true}])),
	    wait_for_close(),
	    catch wxEvtHandler:disconnect(Frame, close_window),
	    ok
    end.

wait_for_close() ->
    receive 
	#wx{event=#wxClose{}} ->
	    ?log("Got close~n",[]);
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
run_test([{Module, TC}|Rest], Config) ->
    [run_test(Module, TC, Config) |
     run_test(Rest, Config)];
run_test([], _Config) -> [].

run_test(Module, all, Config) ->
    All = [{Module, Test} || Test <- Module:all()],
    run_test(All, Config);
run_test(Module, TestCase, Config) ->
    log("Eval test case: ~w~n", [{Module, TestCase}]),
    Sec = timer:seconds(1) * 1000,
    {T, Res} =
	timer:tc(?MODULE, eval_test_case, [Module, TestCase, Config]),
    log("Tested ~w in ~w sec~n", [TestCase, T div Sec]),
    {T div Sec, Res}.
    
eval_test_case(Mod, Fun, Config) ->
    flush(),
    global:register_name(wx_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    Pid = spawn_link(?MODULE, test_case_evaluator, [Mod, Fun, [Config]]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config),
    global:unregister_name(wx_test_case_sup),
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
	    log("<ERROR> Eval process ~w exited, because ~p~n",
		[{Mod, Fun}, Reason]),
	    Mod:end_per_testcase(Fun, Config),
	    {crash, {Mod, Fun}, Reason}
    end.

flush() ->
    receive Msg -> [Msg | flush()]
    after 0 -> []
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
