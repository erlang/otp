%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012-2018. All Rights Reserved.
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

-module(reltool_manual_gui_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2]).

-export([config/1, depgraphs/1]).

-include_lib("common_test/include/ct.hrl").
-include("reltool_test_lib.hrl").

%% Initialization functions.
init_per_suite(Config) ->
    reltool_test_lib:wx_init_per_suite(Config).

end_per_suite(Config) ->
    reltool_test_lib:wx_end_per_suite(Config).

init_per_testcase(Func,Config) ->
    reltool_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) ->
    reltool_test_lib:end_per_testcase(Func,Config).

%% SUITE specification
suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [config, depgraphs].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

%% Semi-automatic walkthrough of the GUI
config(Config) ->
    DataDir = ?config(data_dir,Config),
    PrivDir = ?config(priv_dir,Config),
    {ok, SysPid} = ?msym({ok, _}, reltool:start([])),
    link(SysPid),

    SimpleConfigFile = create_simple_config(PrivDir),
    WarningConfigFile = create_warning_config(PrivDir,DataDir),

    break("there are no modules in the 'Included' and 'Excluded' columns, "
	  "and now warnings are displayed",
	  {"load configuration ~p",[SimpleConfigFile]}),
    break("kernel, stdlib and sasl are included and all other are excluded",
	  "undo"),
    break("we are back to default - no included and no excluded applications",
	  "undo again"),
    break("kernel, stdlib and sasl are included and all other are excluded",
	  {"load configuration ~p",
	   [WarningConfigFile]}),
    break("a warning is displayed in the warning list",
	  "undo"),
    break("no warning is displayed in the warning list",
	  "load same configuration again"),
    break("application a is added in the 'Included' column and "
	  "one warning is displayed",
	  "reset configuration"),
    break("we are back to default - no included and no excluded applications, "
	  "and no warnings",
	  "undo"),
    break("a, kernel, stdlib and sasl are included and all other are excluded. "
	  "One warning should now exist through the rest of this test case",
	  "double click the warning"),
    break("a popup window occurs displaying the warning text",
	  "close it with the 'Close' button"),
    break("it disappears",
	  "open it again"),
    break("the warning text can be marked, copied and pasted",
	  "close the popup with the close box on the top frame"),
    break("it disappears",
	  "select application a from 'Included' column and click 'cross'-button "
	  "with to exclude it"),
    break("application a is moved to 'Excluded' column",
	  "select application tools from 'Excluded' column and click "
	  "'tick'-button to include it"),
    break("application tools is moved to 'Included' column",
	  "select application runtime_tools from 'Excluded' column and click "
	  "'tick'-button to include it"),
    break("application runtime_tools is moved to 'Included' column",
	  "undo"),

    ExplicitConfig = filename:join(PrivDir,"explicit.config"),
    break("application runtime_tools is moved back to 'Excluded' column",
	  {"save configuration as 'explicit' to ~p",[ExplicitConfig]}),
    ExpectedExplicitConfig =
	{sys,[{lib_dirs,[filename:join(DataDir,"faulty_app_file")]},
	      {incl_cond,exclude},
	      {app,a,[{incl_cond,exclude}]},
	      {app,kernel,[{incl_cond,include}]},
	      {app,sasl,[{incl_cond,include}]},
	      {app,stdlib,[{incl_cond,include}]},
	      {app,tools,[{incl_cond,include}]}]},
    check_config(ExpectedExplicitConfig,ExplicitConfig),

    break("The saved configuration file is checked and is ok.\n"
	  "Now go to the 'Libraries' tab and change the root directory to "
	  "some invalid directory."),
    break("an error dialog occurs saying that there is no lib dir",
	  {"add library directory ~p",
	   [filename:join(DataDir,"dependencies")]}),
    break("applications x, y and z are added to the 'Excluded' column in "
	  "'Applications' tab",
	  "go to the 'System settings' tab and set application inclusion policy "
	  "to 'derived'"),
    break("a is excluded, kernel, stdlib, sasl and tools are included and "
	  "x, y and z are available in the 'Applications' tab",
	  "undo"),
    break("all non included application are moved to the 'Excluded' column "
	  "and that 'Application inclusion policy' under 'System settings' "
	  "tab is set back to 'exclude'",
	  "undo again"),
    break("applications x, y and z are in the 'Available' column",
	  "open application x, go to the 'Application settings' tab and set "
	  "'Application inclusion policy' to 'Use application specific config' "
	  "and 'include'. Close the window"),
    break("application x is moved to the 'Included' column and z and y are moved "
	  "to the 'Derived' column in the system window",
	  "open application y"),
    break("modules y1, y2 and y3 are in the 'Derived' column",
	  "go to 'Application dependencies' tab"),
    break("application y is used by x, requires kernel and stdlib, and uses z",
	  "go to 'Module dependencies' tab"),
    break("y is used by x2 and x3, and uses z1",
	  "got to 'Application settings' tab and select "
	  "'Source selection policy' 'Use selected version'"),
    break("'Directories' frame becomes enabled (clickable)",
	  "select 'Module inclusion policy' 'Use application specific config' "
	  "and select 'app file + derived'"),
    break("module y3 is moved to the 'Available' column in the 'Modules' tab",
	  "open module y1"),
    break("module y1 is used by x2 and uses z1",
	  "go to the 'Code' tab and double click the line with call to z1:f()"),
    break("new tab is opened with module z1",
	  "find something"),
    break("it is found",
	  "goto some line"),
    break("the cursor is moved to that line",
	  "click the 'Back' button"),
    break("the cursor is moved back to the line of your previous 'find'",
	  "terminate the application window (but keep the module window)."),

    {ok,ServerPid} = reltool:get_server(SysPid),
    unlink(SysPid),
    break("the system window is still alive",
	  "terminate reltool by hitting 'Ctrl-q' (linux) or clicking the "
	  "close box on the top fram when system window is active"),
    false = erlang:is_process_alive(SysPid),
    false = erlang:is_process_alive(ServerPid),

    break("Check that both module window and system window are terminated"),

    ok.


depgraphs(Config) ->
    PrivDir = ?config(priv_dir,Config),
    SimpleConfigFile = create_simple_config(PrivDir),
    {ok, SysPid} = ?msym({ok, _}, reltool:start([{config,SimpleConfigFile}])),
    link(SysPid),

    break("Open the application dependency graph and \n\n"
	  "*move the complete graph by left clicking somewhere over it and drag\n"
	  "*move one node left clicking the node and drag\n"
	  "*lock node to position by holding down shift while releasing\n"
	  "*select several nodes with ctrl and left mouse button\n"
	  "*lock/unlock selected nodes with suitable buttons\n"
	  "*freeze\n"
	  "*reset\n"
	  "*left slider: push nodes apart\n"
	  "*right slider: pull nodes together\n"
	  "*middle slider: adjust length of links\n"
	  "*select node and delete\n"),
    break("Open the module dependency graph and meditate over it... "),

    unlink(SysPid),
    break("Terminate reltool from the file menu in the system window"),
    false = erlang:is_process_alive(SysPid),

    break("Check that system window and graphs are terminated"),

    ok.




%%%-----------------------------------------------------------------
%%% Internal functions
break(CheckStr,DoStr) when is_list(CheckStr), is_list(DoStr) ->
    Str = io_lib:format("Check that ~s.~n~nThen ~s.",[CheckStr,DoStr]),
    break(Str);
break(Check,Do) ->
    CheckStr =
	case Check of
	    {CheckFormat,CheckArgs} -> io_lib:format(CheckFormat,CheckArgs);
	    _ -> Check
	end,
    DoStr =
	case Do of
	    {DoFormat,DoArgs} -> io_lib:format(DoFormat,DoArgs);
	    _ -> Do
	end,
    break(CheckStr,DoStr).

break(Str) ->
    Count =
	case get(count) of
	    undefined -> 1;
	    C -> C
	end,
    put(count,Count+1),
    test_server:break("Step " ++ integer_to_list(Count) ++ ":\n" ++ Str).

check_config(Expected,File) ->
    {ok,[Config]} = file:consult(File),
    ?m(Expected,Config).

create_simple_config(PrivDir) ->
    SimpleConfigFile = filename:join(PrivDir,"simple.config"),
    SimpleConfig = {sys,[{incl_cond,exclude},
			 {app,kernel,[{incl_cond,include}]},
			 {app,stdlib,[{incl_cond,include}]},
			 {app,sasl,[{incl_cond,include}]}]},
    ok=file:write_file(SimpleConfigFile,io_lib:format("~p.~n",[SimpleConfig])),
    SimpleConfigFile.

create_warning_config(PrivDir,DataDir) ->
    WarningConfigFile = filename:join(PrivDir,"warning.config"),
    FaultyAppFileDir = filename:join(DataDir,"faulty_app_file"),
    WarningConfig = {sys,[{lib_dirs,[FaultyAppFileDir]},
			  {incl_cond,exclude},
			  {app,kernel,[{incl_cond,include}]},
			  {app,sasl,[{incl_cond,include}]},
			  {app,stdlib,[{incl_cond,include}]},
			  {app,a,[{incl_cond,include}]}
			 ]},
    ok=file:write_file(WarningConfigFile,io_lib:format("~p.~n",[WarningConfig])),
    WarningConfigFile.
