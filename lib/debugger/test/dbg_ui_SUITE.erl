%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-module(dbg_ui_SUITE).


-include_lib("common_test/include/ct.hrl").


%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%% Test cases must be exported.
-export ([dbg_ui/1]).

%% Manual test suites/cases exports
-export([start1/1, interpret1/1, quit1/1,
	 start2/1, interpret2/1, break2/1, options2/1, quit2/1,
	 interpret3/1, all_step3/1,all_next3/1,save3/1,restore3/1,finish3/1,
	 killinit3/1, killone3/1, killall3/1, deleteone3/1, deleteall3/1,
	 viewbreak4/1, delete4/1,
	 attach5/1, normal5/1, exit5/1, options5/1,
	 distsetup6/1, all_step6/1, all_next6/1]).

-export([init_per_testcase/2, end_per_testcase/2]).

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Func, _Config) ->
    ok.

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() -> 
    [dbg_ui, {group, manual_tests}].

groups() -> 
    [{manual_tests, [],
      [start1, interpret1, quit1, start2, interpret2, break2,
       options2, interpret3, all_step3, all_next3, save3,
       restore3, finish3, killinit3, killone3, killall3,
       deleteone3, deleteall3, viewbreak4, delete4, attach5,
       normal5, exit5, options5, distsetup6, all_step6,
       all_next6]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%% Test Debugger GUI.
dbg_ui (_Config) ->
    case os:getenv("DISPLAY") of
	false ->
	    {skipped,"No display"};
	Other when is_list(Other) ->
	    %%	    {ok, Pid} = debugger:start (),
	    %%	    ok = is_pid (Pid),
	    %%	    true = erlang:is_process_alive(Pid),
	    %%	    ok = debugger:stop(),
	    %%	    false = erlang:is_process_alive(Pid)
	    {skipped,"Gunilla: Workaround"}
    end.

%% check/2 - returns the result for the specified testcase.
%% pass - means the user has run the case, and it passed
%% fail - means the user has run the case, and it failed
%% unknown - means the user has (tried to) run the case, and the result is unclear.
%% skip - means the user has not yet run the case.

check(Case, Config) ->

    DataDir = proplists:get_value(data_dir, Config),
    ResultFileName =  filename:join([DataDir, "manual_results.erl"]),
    case file:consult(ResultFileName) of
	{ok, Results} ->
	    io:format("Results: ~p~n",[Results]),
	    case Results of
		[] ->
		    no_result;
		%% Incomplete "sanity" check of file contents.
		[{_Key,_Value}|_Rest] ->
		    case lists:keysearch(Case, 1, Results) of
			{value, {Case, Value}} ->
			    Value;		% pass, fail, unknown
			false ->
			    no_result;				% skip
			_Otherwise ->
			    {error, "Contents of results file not valid"}
		    end;
		_Otherwise2 ->
		    {error, "Contents of results file not valid"}
	    end;
	_Otherwise3 ->
	    {error, "Problems reading results file"}
    end.






-define(MAN_CASE(Name,Doc, Description),
	Name(Config) ->
	       io:format("Checking ~p~n",[Name]),
	       io:format("Config = ~p~n",[Config]),
	       case check(Name, Config) of
		   pass ->
		       ok;
		   fail ->
		       ct:fail("Manual test failed");
		   unknown ->
		       {skipped, "Manual test result unknown"};

		   no_result ->
		       {skipped, Description};

		   {error, _Reason} ->
		       %%		       Text = lists:flatten(
		       %%				io_lib:format("[File problem: ~s]~s",
		       %%		       [Reason,Description])),
		       {skipped, Description}
	       end
		   ).


%% SET 1
?MAN_CASE(start1, "Start the debugger from the toolbar",
	  "Before proceeding with the test cases, please move or remove
the directory .erlang_tools/debugger in your home directory. Next,
please start the debugger from the toolbar").

?MAN_CASE(interpret1, "Interpreting modules",
	  "In this test case and all of the ones following, the source code
files to use can be found in the test data directory for this debugger test
	  suite (probably in
		 /clearcase/otp/tools/debugger/test/dbg_ui_SUITE_data/manual_data/src ).
Interpret one module").

?MAN_CASE(quit1, "Quit the debugger",
	  "Quit the debugger using File->Exit in the main window").


%% SET 2
?MAN_CASE(start2, "Start the debugger from the shell",
	  "Start the debugger from the shell. Use debugger:start()").

?MAN_CASE(interpret2, "Interpret all modules",
	  "Interpret all modules").

?MAN_CASE(break2, "Set break points",
	  "Set break points").

?MAN_CASE(options2, "Set options to attach on break",
	  "Set options to attach on break").

?MAN_CASE(quit2, "Quit the debugger",
	  "Quit the debugger using the close box in the main window title frame").


%% SET3
?MAN_CASE(interpret3, "Test attach options",
	  "Start the debugger and interpret the modules [test, lists1, ordsets1]. Close the Interpret dialog. Set Attach on First Call and Attach on Break.").

?MAN_CASE(all_step3, "Click Step through all evaluation",
	  "In the shell, call test:test1(). Use the Step button, the Process->Step menu item and the ctrl-s shortcut to step through the *entire* execution of the call. (Approx 36 steps). Then close the Attach window. The result printed in the shell should be: {\"peter\",[1,2,4,a,b,c],\"olin\"}").

?MAN_CASE(all_next3,"Click Next through all evaluation",
	  "Again call test:test1() in the shell. This time Use the Next button,  the Process->Next menu and the ctrl-n shortcut to quickly step over the execution of the four lines in the test1-function. The result printed in the shell should be: {\"peter\",[1,2,4,a,b,c],\"olin\"}").

?MAN_CASE(save3, "Save the debugger state",
	  "Use File->Save Settings to save the debugger state with the name 'three.state'").

?MAN_CASE(restore3,"Quit the debugger, restart and restore the state",
	  "Quit the debugger. Start it again. Use File->Load Settings to restore the state saved in 'three.state'. Check that the Attach-options are the same as what you set them to in the interpret3 test case. Check that the three modules [test,lists1,ordsets1] are interpreted.").


?MAN_CASE(finish3, "Finish the current function body",
	  "Call the fucntion test:test1() from the shell. Press Finish to evaluate the remaining lines in the function. The result printed in the shell should be: {\"peter\",[1,2,4,a,b,c],\"olin\"}").

?MAN_CASE(killinit3,"Set up for killing and clearing processes",
	  "Call test:test2() from the shell. Set a break point at the last line of test:test2. Click Continue. This should open three new attach windows. One for each spawn called in test:test2/0.   ").

?MAN_CASE(killone3, "Kill a process and clear it",
	  "In one of the newly openend Attach windows: select Process->Kill. A message should appear above the Code Area in the Attach window. Use Windows->Monitor to verify that the Monitor window also shows that the process has been killed. In the Monitor window: select Edit->Clear. This should do two things: 1) close/remove the window of the killed process. 2) Remove the entry of the killed process from the monitor window.").

?MAN_CASE(killall3,"KIll all processes, and clear them",
	  "In the Monitor window: Select Edit->Kill All. Verify that all processes have been killed (in their respective windows and in the monitor window). Windows will be raised as their processes die. Next select, Edit->Clear. All attach windows should now be closed. Their entris should also disappear from the monitor window. The shell should have reported: ** exited: killed **").

?MAN_CASE(deleteone3,"Delete/uniterpret one module",
	  "In the Monitor window: Select Module->test->Delete. This should remove the breakpoints set in the test module, and the test module should disappear from the Module menu.").

?MAN_CASE(deleteall3,"Delete/uniterpret all modules",
	  "In the Monitor window: Select Module->Delete All Modules. This should remove all modules from the Module menu. ").

%% SET 4



?MAN_CASE(viewbreak4, "Test the View window",
	  "Restore the settings from the three.state file again. In the Monitor window: Use Module->test->View to view the source code of the test module. In the View window, select Break->Line Break and set a break at line 53. Check that it appears in the View window and in the Monitor Window Break-menu. Also in the View window, select Break->Function Break and set a break at function test:test4. Check that the break (at line 59) appears in the View Window and in the Monitor Window Break-menu.").

?MAN_CASE(delete4, "Remove breaks",
	  "Use the Break->Delete All function in the View window to remove all breaks in the test module. Check that they are all removed. Close the View window.").

%% SET 5

?MAN_CASE(attach5,"Set attach options",
	  "Set the attach options to only attach on exit").

?MAN_CASE(normal5, "Test normal exit",
	  "Call test:test12(normal) in the shell. This should return the atom 'done', and no windows should be opened.").

?MAN_CASE(exit5, "Test abnormal exit",
	  "Call test:test12(crash) in the shell. This should give the error message ** exited: crash **, and an attach window should be opened highlighting the last line in the test12-function.").

?MAN_CASE(options5, "Experiment with the frames in the attach window",
	  "Try all possible configurations of the [Button, Evaluator, Bindings, Trace] Frames in the attach window and see that the expected frames are shown/hidden.").


%% SET 6 (Distribution)

?MAN_CASE(distsetup6,"Set up distribution",
	  "Start two erlang systems [foo,bar] (with option -sname), make them aware of eachother using net_adm:ping/1. Start the debugger on foo. Interpret the modules [test, lists1, ordsets1]. Set attach on First call.  ").




?MAN_CASE(all_step6, "Click Step through all evaluation",
	  "In the bar shell, call test:test1().This should open an attach window. Use the Step button, the Process->Step menu item and the ctrl-s shortcut to step through the *entire* execution of the call. (Approx 36 steps). Then close the Attach window. The result printed in the bar shell should be: {\"peter\",[1,2,4,a,b,c],\"olin\"}").

?MAN_CASE(all_next6,"Click Next through all evaluation",
	  "Again, in the bar shell, call test:test1(). This time Use the Next button,  the Process->Next menu and the ctrl-n shortcut to quickly step over the execution of the four lines in the test1-function. The result printed in the shell should be: {\"peter\",[1,2,4,a,b,c],\"olin\"}").
