%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2012. All Rights Reserved.
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

-module(reltool_server_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("reltool/src/reltool.hrl").
-include("reltool_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").

-define(NODE_NAME, '__RELTOOL__TEMPORARY_TEST__NODE__').
-define(WORK_DIR, "reltool_work_dir").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialization functions.

init_per_suite(Config) ->
    ?ignore(file:make_dir(?WORK_DIR)),
    reltool_test_lib:init_per_suite(Config).

end_per_suite(Config) ->
    reltool_test_lib:end_per_suite(Config).

init_per_testcase(Func,Config) ->
    reltool_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) -> 
    reltool_test_lib:end_per_testcase(Func,Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUITE specification

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    [start_server,
     set_config,
     get_config,
     create_release,
     create_release_sort,
     create_script,
     create_script_sort,
     create_target,
     create_embedded,
     create_standalone,
     create_old_target,
     eval_target_spec,
     otp_9135,
     otp_9229_exclude_app,
     otp_9229_exclude_mod,
     get_apps,
     get_mod,
     get_sys,
     set_app_and_undo,
     set_apps_and_undo,
     set_sys_and_undo,
     load_config_and_undo,
     reset_config_and_undo,
     gen_rel_files,
     save_config,
     dependencies].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start a server process and check that it does not crash

start_server(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
start_server(_Config) ->
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([])),
    Libs = lists:sort(erl_libs()),
    StrippedDefault =
        case Libs of
            [] -> {sys, []};
            _  -> {sys, [{lib_dirs, Libs}]}
        end,
    ?m({ok, StrippedDefault}, reltool:get_config(Pid)),
    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start a server process and check that it does not crash

set_config(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
set_config(_Config) ->
    Libs = lists:sort(erl_libs()),
    Default =
        {sys,
         [
          {mod_cond, all},
          {incl_cond, derived},
          {root_dir, code:root_dir()},
          {lib_dirs, Libs}
         ]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Default}])),
    StrippedDefault =
        case Libs of
            [] -> {sys, []};
            _  -> {sys, [{lib_dirs, Libs}]}
        end,
    ?m({ok, StrippedDefault}, reltool:get_config(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check that get_config returns the expected derivates and defaults
%% as specified
get_config(_Config) ->
    Sys = {sys,[{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?m({ok, Sys}, reltool:get_config(Pid)),
    ?m({ok, Sys}, reltool:get_config(Pid,false,false)),

    ?msym({ok,{sys,[{incl_cond, exclude},
		    {erts,[]},
		    {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
		    {app,sasl,[{incl_cond,include},{mod,_,[]}|_]},
		    {app,stdlib,[{incl_cond,include},{mod,_,[]}|_]}]}},
	  reltool:get_config(Pid,false,true)),

    ?msym({ok,{sys,[{root_dir,_},
		    {lib_dirs,_},
		    {mod_cond,all},
		    {incl_cond,exclude},
		    {app,kernel,[{incl_cond,include},{vsn,undefined}]},
		    {app,sasl,[{incl_cond,include},{vsn,undefined}]},
		    {app,stdlib,[{incl_cond,include},{vsn,undefined}]},
		    {boot_rel,"start_clean"},
		    {rel,"start_clean","1.0",[]},
		    {rel,"start_sasl","1.0",[sasl]},
		    {emu_name,"beam"},
		    {relocatable,true},
		    {profile,development},
		    {incl_sys_filters,[".*"]},
		    {excl_sys_filters,[]},
		    {incl_app_filters,[".*"]},
		    {excl_app_filters,[]},
		    {incl_archive_filters,[".*"]},
		    {excl_archive_filters,["^include$","^priv$"]},
		    {archive_opts,[]},
		    {rel_app_type,permanent},
		    {app_file,keep},
		    {debug_info,keep}]}},
	  reltool:get_config(Pid,true,false)),

    KVsn = latest(kernel),
    StdVsn = latest(stdlib),

    ?msym({ok,{sys,[{root_dir,_},
		    {lib_dirs,_},
		    {mod_cond,all},
		    {incl_cond,exclude},
		    {erts,[]},
		    {app,kernel,[{incl_cond,include},{vsn,KVsn},{mod,_,[]}|_]},
		    {app,sasl,[{incl_cond,include},{vsn,_},{mod,_,[]}|_]},
		    {app,stdlib,[{incl_cond,include},{vsn,StdVsn},{mod,_,[]}|_]},
		    {boot_rel,"start_clean"},
		    {rel,"start_clean","1.0",[]},
		    {rel,"start_sasl","1.0",[sasl]},
		    {emu_name,"beam"},
		    {relocatable,true},
		    {profile,development},
		    {incl_sys_filters,[".*"]},
		    {excl_sys_filters,[]},
		    {incl_app_filters,[".*"]},
		    {excl_app_filters,[]},
		    {incl_archive_filters,[".*"]},
		    {excl_archive_filters,["^include$","^priv$"]},
		    {archive_opts,[]},
		    {rel_app_type,permanent},
		    {app_file,keep},
		    {debug_info,keep}]}},
	  reltool:get_config(Pid,true,true)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTP-9135, test that app_file option can be set to all | keep | strip

otp_9135(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
otp_9135(_Config) ->
    Libs = lists:sort(erl_libs()),
    StrippedDefaultSys = 
        case Libs of
            [] -> [];
            _  -> {lib_dirs, Libs}
        end,
    
    Config1 = {sys,[{app_file, keep}]}, % this is the default
    {ok, Pid1} = ?msym({ok, _}, reltool:start_server([{config, Config1}])),
    ?m({ok, {sys,StrippedDefaultSys}}, reltool:get_config(Pid1)),
    ?m(ok, reltool:stop(Pid1)),
       
    Config2 = {sys,[{app_file, strip}]},
    {ok, Pid2} = ?msym({ok, _}, reltool:start_server([{config, Config2}])),
    ExpectedConfig2 = StrippedDefaultSys++[{app_file,strip}],
    ?m({ok, {sys,ExpectedConfig2}}, reltool:get_config(Pid2)),
    ?m(ok, reltool:stop(Pid2)),

    Config3 = {sys,[{app_file, all}]},
    {ok, Pid3} = ?msym({ok, _}, reltool:start_server([{config, Config3}])),
    ExpectedConfig3 = StrippedDefaultSys++[{app_file,all}],
    ?m({ok, {sys,ExpectedConfig3}}, reltool:get_config(Pid3)),
    ?m(ok, reltool:stop(Pid3)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate releases

create_release(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_release(_Config) ->
    %% Configure the server
    RelName = "Just testing...",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [kernel, stdlib]}
         ]},
    %% Generate release 
    ErtsVsn = erlang:system_info(version),
    Apps = application:loaded_applications(),
    {value, {_, _, KernelVsn}} = lists:keysearch(kernel, 1, Apps),
    {value, {_, _, StdlibVsn}} = lists:keysearch(stdlib, 1, Apps),
    Rel = {release, {RelName, RelVsn}, 
           {erts, ErtsVsn}, 
           [{kernel, KernelVsn}, {stdlib, StdlibVsn}]},
    ?m({ok, Rel}, reltool:get_rel([{config, Config}], RelName)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate releases and make sure order of applications specified in
%% 'rel' parameter is preserved and that included applications are
%% started before the including application.
%% Circular dependencies shall also be detected and cause error.

create_release_sort(_Config) -> {skip, "Multiple known problems - see OTP-9792"};
create_release_sort(Config) ->
    DataDir = ?config(data_dir,Config),
    %% Configure the server
    RelName1 = "MnesiaFirst",
    RelName2 = "SaslFirst",
    RelName3 = "Include-both",
    RelName4 = "Include-only-app",
    RelName5 = "Include-only-rel",
    RelName6 = "Include-missing-app",
    RelName7 = "Circular",
    RelName8 = "Include-both-missing-app",
    RelName9 = "Include-overwrite",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {lib_dirs, [filename:join(DataDir,"sort_apps")]},
          {boot_rel, RelName1},
          {rel, RelName1, RelVsn, [stdlib, kernel, mnesia, sasl]},
          {rel, RelName2, RelVsn, [stdlib, kernel, sasl, mnesia]},
          {rel, RelName3, RelVsn, [stdlib, kernel, {z,[tools]}, tools]},
          {rel, RelName4, RelVsn, [stdlib, kernel, z, tools]},
          {rel, RelName5, RelVsn, [stdlib, kernel, {sasl,[tools]}]},
          {rel, RelName6, RelVsn, [stdlib, kernel, z]}, %z includes tools in .app
	  {rel, RelName7, RelVsn, [stdlib, kernel, mnesia, y, sasl, x]},
          {rel, RelName8, RelVsn, [stdlib, kernel, {z,[tools]}]},
          {rel, RelName9, RelVsn, [stdlib, kernel, {z,[]}]},
	  {incl_cond,exclude},
	  {mod_cond,app},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,mnesia,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,include}]},
	  {app,z,[{incl_cond,include}]},
	  {app,tools,[{mod_cond,app},{incl_cond,include}]}
         ]},
    %% Generate release

    %% BUG: reltool reverses the list of applications after kernel and stdlib
    ?msym({ok, {release, {RelName1, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {mnesia, _},
		 {sasl, _}]}},
	  reltool:get_rel([{config, Sys}], RelName1)),

    %% BUG: reltool reverses the list of applications after kernel and stdlib
    ?msym({ok, {release, {RelName2, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {sasl, _},
		 {mnesia, _}]}},
	  reltool:get_rel([{config, Sys}], RelName2)),

    ?msym({ok, {release, {RelName3, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {tools, _},
		 {z, _, [tools]}]}},
	  reltool:get_rel([{config, Sys}], RelName3)),

    %% BUG: reltool does not honor included applications in .app files
    %% unless they are also mentioned in the 'rel' specification in
    %% the reltool config.
    %% => order of tools and z does not become correct in rel (tools
    %% should be first since it is included in z)
    ?msym({ok, {release, {RelName4, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {tools, _},
		 {z, _}]}},
	  reltool:get_rel([{config, Sys}], RelName4)),

    ?m({error,"sasl: These applications are used by release "
	"Include-only-rel but are missing as included_applications "
	"in the app file: [tools]"},
       reltool:get_rel([{config, Sys}], RelName5)),

    %% BUG: reltool does not honor included applications in .app files
    %% unless they are also mentioned in the 'rel' specification in
    %% the reltool config.
    %% => does not detect that tools (included in z) is missing
    ?m({error, "Undefined applications: [tools]"},
       reltool:get_rel([{config, Sys}], RelName6)),

    ?m({error,"Circular dependencies: [x,y]"},
       reltool:get_rel([{config, Sys}], RelName7)),

    ?m({error,"Undefined applications: [tools]"},
       reltool:get_rel([{config, Sys}], RelName8)),

    %% BUG: Reltool looses the empty include list for z, which should
    %% overwrite included_applications statement from the .app file.
    ?msym({ok,{release,{RelName9,RelVsn},
	       {erts,_},
	       [{kernel,_},
		{stdlib,_},
		{z,_,[]}]}},
	  reltool:get_rel([{config, Sys}], RelName9)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate boot scripts

create_script(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_script(_Config) ->
    %% Configure the server
    RelName = "Just testing",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [stdlib, kernel]}
         ]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Config}])),

    %% Generate release file
    ErtsVsn = erlang:system_info(version),
    Apps = application:loaded_applications(),
    {value, {_, _, KernelVsn}} = lists:keysearch(kernel, 1, Apps),
    {value, {_, _, StdlibVsn}} = lists:keysearch(stdlib, 1, Apps),
    Rel = {release, 
           {RelName, RelVsn}, 
           {erts, ErtsVsn},
           [{kernel, KernelVsn}, {stdlib, StdlibVsn}]},
    ?m({ok, Rel}, reltool:get_rel(Pid, RelName)),
    ?m(ok, file:write_file(filename:join([?WORK_DIR, RelName ++ ".rel"]),
			   io_lib:format("~p.\n", [Rel]))),

    %% Generate script file
    {ok, Cwd} = file:get_cwd(),
    ?m(ok, file:set_cwd(?WORK_DIR)),
    ?m(ok, systools:make_script(RelName, [])),
    {ok, [OrigScript]} = ?msym({ok, [_]}, file:consult(RelName ++ ".script")),
    ?m(ok, file:set_cwd(Cwd)),
    {ok, Script} = ?msym({ok, _}, reltool:get_script(Pid, RelName)),
    %% OrigScript2 = sort_script(OrigScript),
    %% Script2 = sort_script(Script),
    %% ?m(OrigScript2, Script2),
    
    ?m(equal, diff_script(OrigScript, Script)),
    
    %% Stop server
    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test creation of .script with different sorting of applications and
%% included applications.
%% Test that result is equal to what systools produces
create_script_sort(_Config) -> {skip, "Multiple known problems - see OTP-9792"};
create_script_sort(Config) ->
    DataDir = ?config(data_dir,Config),
    %% Configure the server
    RelName1 = "MnesiaFirst",
    RelName2 = "SaslFirst",
    RelName3 = "Include-both",
    RelName4 = "Include-only-app",
    RelName5 = "Include-only-rel",
    RelName6 = "Include-missing-app",
    RelName7 = "Circular",
    RelName8 = "Include-both-missing-app",
    RelName9 = "Include-overwrite",
    RelVsn = "1.0",
    LibDir = filename:join(DataDir,"sort_apps"),
    Sys =
        {sys,
         [
          {lib_dirs, [LibDir]},
          {boot_rel, RelName1},
          {rel, RelName1, RelVsn, [stdlib, kernel, mnesia, sasl]},
          {rel, RelName2, RelVsn, [stdlib, kernel, sasl, mnesia]},
          {rel, RelName3, RelVsn, [stdlib, kernel, {z,[tools]}, tools]},
          {rel, RelName4, RelVsn, [stdlib, kernel, z, tools]},
          {rel, RelName5, RelVsn, [stdlib, kernel, {sasl,[tools]}]},
          {rel, RelName6, RelVsn, [stdlib, kernel, z]}, %z includes tools in .app
	  {rel, RelName7, RelVsn, [stdlib, kernel, mnesia, y, sasl, x]},
          {rel, RelName8, RelVsn, [stdlib, kernel, {z,[tools]}]},
          {rel, RelName9, RelVsn, [stdlib, kernel, {z,[]}]},
	  {incl_cond,exclude},
	  {mod_cond,app},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,mnesia,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,include}]},
	  {app,z,[{incl_cond,include}]},
	  {app,tools,[{mod_cond,app},{incl_cond,include}]}
         ]},

    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    %% Generate release files
    application:load(sasl),
    application:load(mnesia),
    application:load(tools),
    {ok,KernelVsn} = application:get_key(kernel,vsn),
    {ok,StdlibVsn} = application:get_key(stdlib,vsn),
    {ok,SaslVsn} = application:get_key(sasl,vsn),
    {ok,MnesiaVsn} = application:get_key(mnesia,vsn),
    {ok,ToolsVsn} = application:get_key(tools,vsn),
    ErtsVsn = erlang:system_info(version),

    Rel1 = {release, {RelName1,RelVsn}, {erts,ErtsVsn},
	    [{kernel,KernelVsn},
	     {stdlib,StdlibVsn},
	     {mnesia,MnesiaVsn},
	     {sasl,SaslVsn}]},
    FullName1 = filename:join(?WORK_DIR,RelName1),
    ?m(ok, file:write_file(FullName1 ++ ".rel", io_lib:format("~p.\n", [Rel1]))),
    Rel2 = {release, {RelName2,RelVsn}, {erts,ErtsVsn},
	    [{kernel,KernelVsn},
	     {stdlib,StdlibVsn},
	     {sasl,SaslVsn},
	     {mnesia,MnesiaVsn}]},
    FullName2 = filename:join(?WORK_DIR,RelName2),
    ?m(ok, file:write_file(FullName2 ++ ".rel", io_lib:format("~p.\n", [Rel2]))),
    Rel3 = {release, {RelName3,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0",[tools]},
	      {tools,ToolsVsn}]},
    FullName3 = filename:join(?WORK_DIR,RelName3),
    ?m(ok, file:write_file(FullName3 ++ ".rel", io_lib:format("~p.\n", [Rel3]))),
    Rel4 = {release, {RelName4,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0"},
	      {tools,ToolsVsn}]},
    FullName4 = filename:join(?WORK_DIR,RelName4),
    ?m(ok, file:write_file(FullName4 ++ ".rel", io_lib:format("~p.\n", [Rel4]))),
    Rel5 = {release, {RelName5,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {sasl,SaslVsn,[tools]},
	      {tools,ToolsVsn}]},
    FullName5 = filename:join(?WORK_DIR,RelName5),
    ?m(ok, file:write_file(FullName5 ++ ".rel", io_lib:format("~p.\n", [Rel5]))),
    Rel6 = {release, {RelName6,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0"}]},
    FullName6 = filename:join(?WORK_DIR,RelName6),
    ?m(ok, file:write_file(FullName6 ++ ".rel", io_lib:format("~p.\n", [Rel6]))),
    Rel7 = {release, {RelName7,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {mnesia,MnesiaVsn},
	      {y,"1.0"},
	      {sasl,SaslVsn},
	      {x,"1.0"}]},
    FullName7 = filename:join(?WORK_DIR,RelName7),
    ?m(ok, file:write_file(FullName7 ++ ".rel", io_lib:format("~p.\n", [Rel7]))),
    Rel8 = {release, {RelName8,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0",[tools]}]},
    FullName8 = filename:join(?WORK_DIR,RelName8),
    ?m(ok, file:write_file(FullName8 ++ ".rel", io_lib:format("~p.\n", [Rel8]))),
    Rel9 = {release, {RelName9,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0",[]}]},
    FullName9 = filename:join(?WORK_DIR,RelName9),
    ?m(ok, file:write_file(FullName9 ++ ".rel", io_lib:format("~p.\n", [Rel9]))),

    %% Generate script files with systools and reltool and compare
    ZPath = filename:join([LibDir,"*",ebin]),

    %% BUG: reltool reverses the list of applications after kernel and stdlib
    %% => mnesia and sasl are reverted
    ?msym({ok,_,_}, systools_make_script(FullName1,ZPath)),
    {ok, [SystoolsScript1]} = ?msym({ok,[_]}, file:consult(FullName1++".script")),
    {ok, Script1} = ?msym({ok, _}, reltool:get_script(Pid, RelName1)),
    ?m(equal, diff_script(SystoolsScript1, Script1)),

    %% BUG: reltool reverses the list of applications after kernel and stdlib
    %% => mnesia and sasl are reverted
    ?msym({ok,_,_}, systools_make_script(FullName2,ZPath)),
    {ok, [SystoolsScript2]} = ?msym({ok,[_]}, file:consult(FullName2++".script")),
    {ok, Script2} = ?msym({ok, _}, reltool:get_script(Pid, RelName2)),
    ?m(equal, diff_script(SystoolsScript2, Script2)),

    %% BUG1: reltool loads all modules in the ebin dir of an application,
    %% even if mod_cond is set to 'app'.
    %% BUG2: reltool shall not start included applications!!
    ?msym({ok,_,_}, systools_make_script(FullName3,ZPath)),
    {ok, [SystoolsScript3]} = ?msym({ok,[_]}, file:consult(FullName3++".script")),
    {ok, Script3} = ?msym({ok, _}, reltool:get_script(Pid, RelName3)),
    ?m(equal, diff_script(SystoolsScript3, Script3)),

    %% BUG1:  reltool loads all modules in the ebin dir of an application,
    %% even if mod_cond is set to 'app'.
    %% BUG2: reltool does not honor included applications in .app files
    %% unless they are also mentioned in the 'rel' specification in
    %% the reltool config.
    %% => faulty order of load instructions for tools and z. tools
    %% should be first since it is included in z.
    ?msym({ok,_,_}, systools_make_script(FullName4,ZPath)),
    {ok, [SystoolsScript4]} = ?msym({ok,[_]}, file:consult(FullName4++".script")),
    {ok, Script4} = ?msym({ok, _}, reltool:get_script(Pid, RelName4)),
    ?m(equal, diff_script(SystoolsScript4, Script4)),

    ?msym({error,_,[{error_reading,{sasl,{override_include,_}}}]},
	  systools_make_script(FullName5,ZPath)),
    ?m({error,"sasl: These applications are used by release "
	"Include-only-rel but are missing as included_applications "
	"in the app file: [tools]"},
       reltool:get_script(Pid, RelName5)),

    %% BUG: reltool does not honor included applications in .app files
    %% unless they are also mentioned in the 'rel' specification in
    %% the reltool config.
    %% => does not detect that tools (included in z) is missing
    ?msym({error,_,{undefined_applications,_}},
	  systools_make_script(FullName6,ZPath)),
    ?m({error, "Undefined applications: [tools]"},
       reltool:get_script(Pid, RelName6)),

    ?msym({error,_,{circular_dependencies,_}},
	  systools_make_script(FullName7,ZPath)),
    ?m({error,"Circular dependencies: [x,y]"},
       reltool:get_script(Pid, RelName7)),

    ?msym({error,_,{undefined_applications,_}},
	  systools_make_script(FullName8,ZPath)),
    ?m({error, "Undefined applications: [tools]"},
       reltool:get_script(Pid, RelName8)),

    ?msym({ok,_,_}, systools_make_script(FullName9,ZPath)),
    {ok, [SystoolsScript9]} = ?msym({ok,[_]}, file:consult(FullName9++".script")),
    {ok, Script9} = ?msym({ok, _}, reltool:get_script(Pid, RelName9)),
    ?m(equal, diff_script(SystoolsScript9, Script9)),

    %% Stop server
    ?m(ok, reltool:stop(Pid)),
    ok.


systools_make_script(Name,Path) ->
    systools:make_script(Name,[{path,[Path]},{outdir,?WORK_DIR},silent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system

create_target(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_target(_Config) ->
    %% Configure the server
    RelName1 = "Just testing",
    RelName2 = "Just testing with SASL",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, []},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {app, sasl, [{incl_cond, include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_development"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Config}])]),
    ok = ?m(ok, reltool:create_target([{config, Config}], TargetDir)),
    
    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate embedded target system

create_embedded(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_embedded(_Config) ->
    %% Configure the server
    RelName1 = "Just testing",
    RelName2 = "Just testing with SASL",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {lib_dirs, []},
          {profile, embedded},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {app, sasl, [{incl_cond, include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_embedded"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config, Config}], TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),
        
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate standalone system

create_standalone(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_standalone(_Config) ->
    %% Configure the server
    ExDir = code:lib_dir(reltool, examples),
    EscriptName = "display_args",
    Escript = filename:join([ExDir, EscriptName]),
    Config =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_standalone"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config, Config}], TargetDir)),

    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)), 
    RootDir = ?ignore(rpc:call(Node, code, root_dir, [])),
    ?msym(ok, stop_node(Node)),
    
    Expected =  iolist_to_binary(["Root dir: ", RootDir, "\n"
				  "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
				  "Smp: false\n",
				  "ExitCode:0"]),
    io:format("Expected: ~s\n", [Expected]),
    ?m(Expected, run(BinDir, EscriptName ++ " -arg1 arg2 arg3")),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate old type of target system

create_old_target(TestInfo) when is_atom(TestInfo) -> 
    reltool_test_lib:tc_info(TestInfo);
create_old_target(_Config) ->
    ?skip("Old style of target", []),
    
    %% Configure the server
    RelName1 = "Just testing",
    RelName2 = "Just testing with SASL",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {relocatable, false}, % Implies explicit old style installation
          {app, sasl, [{incl_cond, include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_old_style"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config, Config}], TargetDir)),
    
    %% io:format("Will fail on Windows (should patch erl.ini)\n", []),
    ok = ?m(ok, reltool:install(RelName2, TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system with eval_target_spec/3

eval_target_spec(_Config) ->
    %% Configure the server
    RelName1 = "Just testing",
    RelName2 = "Just testing with SASL",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, []},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {app, sasl, [{incl_cond, include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "eval_target_spec"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    {ok, Spec} = ?msym({ok,_}, reltool:get_target_spec([{config, Config}])),
    ok = ?m(ok, reltool:eval_target_spec(Spec, code:root_dir(), TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OTP-9229 - handle duplicated module names, i.e. same module name
%% exists in two applications.

%% Include on app, exclude the other
otp_9229_exclude_app(Config) ->
    DataDir =  ?config(data_dir,Config),
    LibDir = filename:join(DataDir,"otp_9229"),

    %% Configure the server
    ExclApp =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, [LibDir]},
	  {incl_cond,exclude},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,exclude}]},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_dupl_mod_excl_app"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, ExclApp}])]),
    ?m({ok,["Module mylib exists in applications x and y. Using module from application x."]}, reltool:get_status([{config, ExclApp}])),
    ok = ?m(ok, reltool:create_target([{config, ExclApp}], TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),

    AbsTargetDir = filename:absname(TargetDir),
    XArchive = "x-1.0.ez",
    AbsXArchive = filename:join([AbsTargetDir,lib,XArchive]),
    XEbin = ["ebin","x-1.0",XArchive],
    YArchive = "y-1.0.ez",
    AbsYArchive = filename:join([AbsTargetDir,lib,YArchive]),

    ?m(true, filelib:is_file(AbsXArchive)),
    ?m(XEbin, mod_path(Node,x)),
    ?m(XEbin, mod_path(Node,mylib)),
    ?m(false, filelib:is_file(AbsYArchive)),
    ?m(non_existing, mod_path(Node,y)),

    ?msym(ok, stop_node(Node)),

    ok.

%% Include both apps, but exclude common module from one app
otp_9229_exclude_mod(Config) ->
    DataDir =  ?config(data_dir,Config),
    LibDir = filename:join(DataDir,"otp_9229"),

    %% Configure the server
    ExclMod =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, [LibDir]},
	  {incl_cond,exclude},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,include},{mod, mylib,[{incl_cond,exclude}]}]},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_dupl_mod_excl_mod"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, ExclMod}])]),
    ?m({ok,["Module mylib exists in applications x and y. Using module from application x."]}, reltool:get_status([{config, ExclMod}])),
    ok = ?m(ok, reltool:create_target([{config, ExclMod}], TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),

    AbsTargetDir = filename:absname(TargetDir),
    XArchive = "x-1.0.ez",
    AbsXArchive = filename:join([AbsTargetDir,lib,XArchive]),
    XEbin = ["ebin","x-1.0",XArchive],
    YArchive = "y-1.0.ez",
    AbsYArchive = filename:join([AbsTargetDir,lib,YArchive]),
    YEbin = ["ebin","y-1.0",YArchive],

    ?m(true, filelib:is_file(AbsXArchive)),
    ?m(XEbin, mod_path(Node,x)),
    ?m(XEbin, mod_path(Node,mylib)),
    ?m(true, filelib:is_file(AbsYArchive)),
    ?m(YEbin, mod_path(Node,y)),

    %% Remove path to XEbin and check that mylib is not located in YEbin
    Mylib = rpc:call(Node,code,which,[mylib]),
    rpc:call(Node,code,del_path,[filename:dirname(Mylib)]),
    ?m(non_existing, mod_path(Node,mylib)),

    ?msym(ok, stop_node(Node)),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test the interface used by the GUI:
%%  get_app
%%  get_apps
%%  set_app
%%  set_apps
%%  load_config
%%  reset_config
%%
%% Also, for each operation which manipulates the config, test
%% get_status and undo_config.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_apps(_Config) ->
    Sys = {sys,[{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,tools,[{incl_cond,derived}]},
		{app,runtime_tools,[{incl_cond,exclude}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    {ok,Sasl} = ?msym({ok,#app{name=sasl}}, reltool_server:get_app(Pid,sasl)),
    {ok,[#app{name=kernel},
	 #app{name=sasl}=Sasl,
	 #app{name=stdlib}] = White} =
	?msym({ok,_}, reltool_server:get_apps(Pid,whitelist)),
    {ok,[#app{name=runtime_tools}] = Black} =
	?msym({ok,_}, reltool_server:get_apps(Pid,blacklist)),

    {ok,Derived} = ?msym({ok,_}, reltool_server:get_apps(Pid,derived)),
    true = lists:keymember(tools,#app.name,Derived),

    {ok,Source} = ?msym({ok,_}, reltool_server:get_apps(Pid,source)),
    true = lists:keymember(common_test,#app.name,Source),

    %% Check that the four lists are disjoint
    Number = length(White) + length(Black) + length(Derived) + length(Source),
    WN = lists:usort([N || #app{name=N} <- White]),
    BN = lists:usort([N || #app{name=N} <- Black]),
    DN = lists:usort([N || #app{name=N} <- Derived]),
    SN = lists:usort([N || #app{name=N} <- Source]),
    AllN = lists:umerge([WN,BN,DN,SN]),
    ?m(Number,length(AllN)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_mod(_Config) ->
    Sys = {sys,[{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,tools,[{incl_cond,derived}]},
		{app,runtime_tools,[{incl_cond,exclude}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    %% Read app and get a module from the #app record
    {ok,Tools} = ?msym({ok,#app{name=tools}}, reltool_server:get_app(Pid,tools)),
    Cover = lists:keyfind(cover,#mod.name,Tools#app.mods),

    %% get_mod - and check that it is equal to the one in #app.mods
    ?m({ok,Cover}, reltool_server:get_mod(Pid,cover)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_sys(_Config) ->
    Sys = {sys,[{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,tools,[{incl_cond,derived}]},
		{app,runtime_tools,[{incl_cond,exclude}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    RootDir = code:root_dir(),
    ?msym({ok,#sys{root_dir=RootDir,apps=undefined}},reltool_server:get_sys(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_app_and_undo(Config) ->
    Sys = {sys,[{lib_dirs,[filename:join(datadir(Config),"faulty_app_file")]},
		{incl_cond, exclude},
		{app,a,[{incl_cond,include}]},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?m({ok, Sys}, reltool:get_config(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    {ok,Cover} = ?msym({ok,#mod{name=cover, is_included=true}},
		       reltool_server:get_mod(Pid,cover)),

    %% Exclude one module with set_app
    ExclCover = Cover#mod{incl_cond=exclude},
    Mods = Tools#app.mods,
    Tools1 = Tools#app{mods = lists:keyreplace(cover,#mod.name,Mods,ExclCover)},
    {ok,ToolsNoCover,[]} = ?msym({ok,_,[]}, reltool_server:set_app(Pid,Tools1)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Check that the module is no longer included
    ?m({ok,ToolsNoCover}, reltool_server:get_app(Pid,tools)),
    {ok,NoIncludeCover} = ?msym({ok,#mod{name=cover, is_included=false}},
				reltool_server:get_mod(Pid,cover)),

    %% Undo
    %%! warning can come twice here... :(
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:undo_config(Pid)),
    ?m({ok,Tools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m({ok,[]}, reltool_server:undo_config(Pid)),
    ?m({ok,ToolsNoCover}, reltool_server:get_app(Pid,tools)),
    ?m({ok,NoIncludeCover}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_apps_and_undo(Config) ->
    Sys = {sys,[{lib_dirs,[filename:join(datadir(Config),"faulty_app_file")]},
		{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?m({ok, Sys}, reltool:get_config(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(true, Tools#app.is_pre_included),
    ?m(true, Tools#app.is_included),
    {ok,Cover} = ?msym({ok,#mod{name=cover, is_included=true}},
		       reltool_server:get_mod(Pid,cover)),

    %% Exclude one application with set_apps
    ExclTools = Tools#app{incl_cond=exclude},
    ?m({ok,[]}, reltool_server:set_apps(Pid,[ExclTools])),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Check that the app and its modules (one of them) are no longer included
    {ok,NoTools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(false, NoTools#app.is_pre_included),
    ?m(false, NoTools#app.is_included),
    {ok,NoIncludeCover} = ?msym({ok,#mod{name=cover, is_included=false}},
				reltool_server:get_mod(Pid,cover)),

    %% Undo
    %%! warning can come twice here... :(
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:undo_config(Pid)),
    ?m({ok,Tools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m({ok,[]}, reltool_server:undo_config(Pid)),
    ?m({ok,NoTools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,NoIncludeCover}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_sys_and_undo(Config) ->
    Sys1 = {sys,[{incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Read sys record
    {ok, SysRec} = reltool_server:get_sys(Pid),

    %% Set lib dirs by call to set_sys
    NewLib = filename:join(datadir(Config),"faulty_app_file"),
    NewLibDirs = [NewLib | SysRec#sys.lib_dirs],
    NewSysRec = SysRec#sys{lib_dirs=NewLibDirs},
    ?msym({ok,["a: Cannot parse app file"++_|_]},
	  reltool_server:set_sys(Pid, NewSysRec)),
    ?m({ok,NewSysRec}, reltool_server:get_sys(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Undo
    ?m({ok,[]}, reltool_server:undo_config(Pid)),
    ?m({ok,SysRec}, reltool_server:get_sys(Pid)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:undo_config(Pid)),
    ?m({ok,NewSysRec}, reltool_server:get_sys(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_config_and_undo(Config) ->
    Sys1 = {sys,[{incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    ?m({ok, Sys1}, reltool:get_config(Pid)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools1} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(true, Tools1#app.is_pre_included),
    ?m(true, Tools1#app.is_included),
    {ok,Cover1} = ?msym({ok,#mod{name=cover,
				 is_included=true,
				 is_pre_included=true}},
			reltool_server:get_mod(Pid,cover)),

    %% Change tools from include to derived by loading new config
    Sys2 = {sys,[{lib_dirs,[filename:join(datadir(Config),"faulty_app_file")]},
		 {app,a,[{incl_cond,include}]},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,tools,[{incl_cond,derived}]}]},
    ?msym({ok,["a: Cannot parse app file"++_]},
	  reltool_server:load_config(Pid,Sys2)),
%%% OTP-0702, 15)    ?m({ok, Sys2}, reltool:get_config(Pid)),
%%% Note that {incl_cond,exclude} is removed compared to Sys1 -
%%% config is merged, not overwritten - is this correct???
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Check that tools is included (since it is used by sasl) but not
    %% pre-included (neither included or excluded => undefined)
    {ok,Tools2} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(undefined, Tools2#app.is_pre_included),
    ?m(true, Tools2#app.is_included),
    {ok,Cover2} = ?msym({ok,#mod{name=cover,
				 is_included=true,
				 is_pre_included=undefined}},
			reltool_server:get_mod(Pid,cover)),

    %% Undo
    ?m({ok,[]}, reltool_server:undo_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:undo_config(Pid)),
    ?m({ok,Tools2}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover2}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reset_config_and_undo(Config) ->
    Sys1 = {sys,[{lib_dirs,[filename:join(datadir(Config),"faulty_app_file")]},
		 {incl_cond, exclude},
		 {app,a,[{incl_cond,include}]},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    ?m({ok, Sys1}, reltool:get_config(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools1} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(true, Tools1#app.is_pre_included),
    ?m(true, Tools1#app.is_included),
    {ok,Cover1} = ?msym({ok,#mod{name=cover,
				 is_included=true,
				 is_pre_included=true}},
			reltool_server:get_mod(Pid,cover)),

    %% Exclude tools by loading new config
    Sys2 = {sys,[{incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,tools,[{incl_cond,exclude}]}]},
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys2)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Check that tools is excluded
    {ok,Tools2} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(false, Tools2#app.is_pre_included),
    ?m(false, Tools2#app.is_included),
    {ok,Cover2} = ?msym({ok,#mod{name=cover,
				 is_included=false,
				 is_pre_included=false}},
			reltool_server:get_mod(Pid,cover)),

    %% Reset
    %%! warning can come twice here... :(
    ?msym({ok,["a: Cannot parse app file"++_|_]},
	  reltool_server:reset_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    %% Undo
    ?m({ok,[]}, reltool_server:undo_config(Pid)),
    ?m({ok,Tools2}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover2}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:undo_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_|_]},reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
gen_rel_files(_Config) ->
    %% Configure the server
    RelName = "gen_fel_files_test",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [kernel, stdlib]}
         ]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    %% Generate .rel, .script and .boot
    Dir = filename:join(?WORK_DIR,"gen_rel_files"),
    ok = file:make_dir(Dir),
    ?m({ok,[]}, reltool_server:gen_rel_files(Pid,Dir)),

    Script = RelName ++ ".script",
    Rel = RelName ++ ".rel",
    Boot = RelName ++ ".boot",
    {ok,Files} = ?msym({ok,_}, file:list_dir(Dir)),
    [Boot,Rel,Script] = lists:sort(Files),

    %% Check that contents is reasonable
    {ok,[S]} = ?msym({ok,[{script,_,_}]},file:consult(filename:join(Dir,Script))),
    ?msym({ok,[{release,_,_,_}]}, file:consult(filename:join(Dir,Rel))),
    {ok,Bin} = ?msym({ok,_}, file:read_file(filename:join(Dir,Boot))),
    ?m(S,binary_to_term(Bin)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
save_config(Config) ->
    PrivDir = ?config(priv_dir,Config),
    Sys = {sys,[{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?m({ok, Sys}, reltool:get_config(Pid)),

    Simple = filename:join(PrivDir,"save_simple.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Simple,false,false)),
    ?m({ok,[Sys]}, file:consult(Simple)),

    Derivates = filename:join(PrivDir,"save_derivates.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Derivates,false,true)),
    ?msym({ok,[{sys,[{incl_cond, exclude},
		     {erts,[]},
		     {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
		     {app,sasl,[{incl_cond,include},{mod,_,[]}|_]},
		     {app,stdlib,[{incl_cond,include},{mod,_,[]}|_]}]}]},
	  file:consult(Derivates)),

    Defaults = filename:join(PrivDir,"save_defaults.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Defaults,true,false)),
    ?msym({ok,[{sys,[{root_dir,_},
		     {lib_dirs,_},
		     {mod_cond,all},
		     {incl_cond,exclude},
		     {app,kernel,[{incl_cond,include},{vsn,undefined}]},
		     {app,sasl,[{incl_cond,include},{vsn,undefined}]},
		     {app,stdlib,[{incl_cond,include},{vsn,undefined}]},
		     {boot_rel,"start_clean"},
		     {rel,"start_clean","1.0",[]},
		     {rel,"start_sasl","1.0",[sasl]},
		     {emu_name,"beam"},
		     {relocatable,true},
		     {profile,development},
		     {incl_sys_filters,[".*"]},
		     {excl_sys_filters,[]},
		     {incl_app_filters,[".*"]},
		     {excl_app_filters,[]},
		     {incl_archive_filters,[".*"]},
		     {excl_archive_filters,["^include$","^priv$"]},
		     {archive_opts,[]},
		     {rel_app_type,permanent},
		     {app_file,keep},
		     {debug_info,keep}]}]},
	  file:consult(Defaults)),

    KVsn = latest(kernel),
    StdVsn = latest(stdlib),

    All = filename:join(PrivDir,"save_all.reltool"),
    ?m(ok, reltool_server:save_config(Pid,All,true,true)),
    ?msym({ok,[{sys,[{root_dir,_},
		     {lib_dirs,_},
		     {mod_cond,all},
		     {incl_cond,exclude},
		     {erts,[]},
		     {app,kernel,[{incl_cond,include},{vsn,KVsn},{mod,_,[]}|_]},
		     {app,sasl,[{incl_cond,include},{vsn,_},{mod,_,[]}|_]},
		     {app,stdlib,[{incl_cond,include},{vsn,StdVsn},{mod,_,[]}|_]},
		     {boot_rel,"start_clean"},
		     {rel,"start_clean","1.0",[]},
		     {rel,"start_sasl","1.0",[sasl]},
		     {emu_name,"beam"},
		     {relocatable,true},
		     {profile,development},
		     {incl_sys_filters,[".*"]},
		     {excl_sys_filters,[]},
		     {incl_app_filters,[".*"]},
		     {excl_app_filters,[]},
		     {incl_archive_filters,[".*"]},
		     {excl_archive_filters,["^include$","^priv$"]},
		     {archive_opts,[]},
		     {rel_app_type,permanent},
		     {app_file,keep},
		     {debug_info,keep}]}]},
	  file:consult(All)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test calculation of dependencies
%% The following test applications are used
%%
%% x-1.0: x1.erl   x2.erl   x3.erl
%%                   \        /         (x2 calls y1, x3 calls y2)
%% y-1.0:          y1.erl   y2.erl
%%                    \                 (y1 calls z1)
%% z-1.0            z1.erl
%%
%% Test includes x and derives y and z.
%%
dependencies(Config) ->
    %% Default: all modules included => y and z are included (derived)
    Sys = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,x,[{incl_cond,include}]},
		{app,y,[{incl_cond,derived}]},
		{app,z,[{incl_cond,derived}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[y]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der} = ?msym({ok,_},
		      reltool_server:get_apps(Pid,derived)),
    ?msym([#app{name=y,uses_apps=[z]},
	   #app{name=z}],
	  rm_missing_app(Der)),
    ?msym({ok,[]},
	  reltool_server:get_apps(Pid,source)),

    %% Excluding x2 => y still included since y2 is used by x3
    %%                 z still included since z1 is used by y1
    Sys2 = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,x,[{incl_cond,include},{mod,x2,[{incl_cond,exclude}]}]},
		 {app,y,[{incl_cond,derived}]},
		 {app,z,[{incl_cond,derived}]}]},
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys2)),
    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[y]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der2} = ?msym({ok,_},
		       reltool_server:get_apps(Pid,derived)),
    ?msym([#app{name=y,uses_apps=[z]},
	   #app{name=z}],
	  rm_missing_app(Der2)),
    ?msym({ok,[]},
	  reltool_server:get_apps(Pid,source)),

    %% Excluding x3 => y still included since y1 is used by x2
    %%                 z still included since z1 is used by y1
    Sys3 = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,x,[{incl_cond,include},{mod,x3,[{incl_cond,exclude}]}]},
		 {app,y,[{incl_cond,derived}]},
		 {app,z,[{incl_cond,derived}]}]},
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys3)),
    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[y]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der3} = ?msym({ok,_},
		       reltool_server:get_apps(Pid,derived)),
    ?msym([#app{name=y,uses_apps=[z]},
	   #app{name=z}],
	  rm_missing_app(Der3)),
    ?msym({ok,[]},
	  reltool_server:get_apps(Pid,source)),

    %% Excluding x2 and x3 => y and z excluded
    Sys4 = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,x,[{incl_cond,include},
			 {mod,x2,[{incl_cond,exclude}]},
			 {mod,x3,[{incl_cond,exclude}]}]},
		 {app,y,[{incl_cond,derived}]},
		 {app,z,[{incl_cond,derived}]}]},
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys4)),
    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der4} = ?msym({ok,_},
		       reltool_server:get_apps(Pid,derived)),
    ?msym([], rm_missing_app(Der4)),
    ?msym({ok,[#app{name=y},
	       #app{name=z}]},
	  reltool_server:get_apps(Pid,source)),

    %% Excluding y1 => y still included since y2 is used by x3
    %%                 z excluded since not used by any other than y1
    Sys5 = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,x,[{incl_cond,include}]},
		 {app,y,[{incl_cond,derived},
			 {mod,y1,[{incl_cond,exclude}]}]},
		 {app,z,[{incl_cond,derived}]}]},
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys5)),
    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[y]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der5} = ?msym({ok,_},
		       reltool_server:get_apps(Pid,derived)),
    ?msym([#app{name=y,uses_apps=[]}], rm_missing_app(Der5)),
    ?msym({ok,[#app{name=z}]},
	  reltool_server:get_apps(Pid,source)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Library functions

erl_libs() ->
    case os:getenv("ERL_LIBS") of
        false  -> [];
        LibStr -> string:tokens(LibStr, ":;")
    end.

datadir(Config) ->
    %% Removes the trailing slash...
    filename:nativename(?config(data_dir,Config)).

latest(App) ->
    AppStr = atom_to_list(App),
    AppDirs = filelib:wildcard(filename:join(code:lib_dir(),AppStr++"-*")),
    [LatestAppDir|_] = lists:reverse(AppDirs),
    [_,Vsn] = string:tokens(filename:basename(LatestAppDir),"-"),
    Vsn.

rm_missing_app(Apps) ->
    lists:keydelete(?MISSING_APP_NAME,#app.name,Apps).

diff_script(Script, Script) ->
    equal;
diff_script({script, Rel, Commands1}, {script, Rel, Commands2}) ->
    diff_cmds(Commands1, Commands2);
diff_script({script, Rel1, _}, {script, Rel2, _}) ->
    {error, {Rel1, Rel2}}.

diff_cmds([Cmd | Commands1], [Cmd | Commands2]) ->
    diff_cmds(Commands1, Commands2);
diff_cmds([Cmd1 | _Commands1], [Cmd2 | _Commands2]) ->
    {diff, {expected, Cmd1}, {actual, Cmd2}};
diff_cmds([], []) ->
    equal.

os_cmd(Cmd) when is_list(Cmd) ->
    %% Call the plain os:cmd with an echo command appended to print command status
    %% io:format("os:cmd(~p).\n", [Cmd]),
    case os:cmd(Cmd++";echo \"#$?\"") of
        %% There is (as far as I can tell) only one thing that will match this
        %% and that is too silly to ever be used, but...
        []->
            {99, []};
        Return->
            %% Find the position of the status code wich is last in the string
            %% prepended with #
            case string:rchr(Return, $#) of
                
                %% This happens only if the sh command pipe is somehow interrupted
                0->
                {98, Return};
                
                Position->
                Result = string:left(Return,Position - 1),
                Status = string:substr(Return,Position + 1, length(Return) - Position - 1),
                {list_to_integer(Status), Result}
            end
    end.

%% Returns the location (directory) of the given module. Split,
%% reverted and relative to the lib dir.
mod_path(Node,Mod) ->
    case rpc:call(Node,code,which,[Mod]) of
	Path when is_list(Path) ->
	    lists:takewhile(
	      fun("lib") -> false;
		 (_) -> true
	      end,
	      lists:reverse(filename:split(filename:dirname(Path))));
	Other ->
	    Other
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Node handling

start_node(Name, ErlPath) ->
    FullName = full_node_name(Name),
    CmdLine = mk_node_cmdline(Name, ErlPath),
    io:format("Starting node ~p: ~s~n", [FullName, CmdLine]),
    case open_port({spawn, CmdLine}, []) of
        Port when is_port(Port) ->
            unlink(Port),
            erlang:port_close(Port),
            case ping_node(FullName, 50) of
                ok -> {ok, FullName};
                Other -> exit({failed_to_start_node, FullName, Other})
            end;
        Error ->
            exit({failed_to_start_node, FullName, Error})
    end.

stop_node(Node) ->
    monitor_node(Node, true),
    spawn(Node, fun () -> halt() end),
    receive {nodedown, Node} -> ok end.

mk_node_cmdline(Name) ->
    Prog = case catch init:get_argument(progname) of
               {ok,[[P]]} -> P;
               _ -> exit(no_progname_argument_found)
           end,
    mk_node_cmdline(Name, Prog).

mk_node_cmdline(Name, Prog) ->
    Static = "-detached -noinput",
    Pa = filename:dirname(code:which(?MODULE)),
    NameSw = case net_kernel:longnames() of
                 false -> "-sname ";
                 true -> "-name ";
                 _ -> exit(not_distributed_node)
             end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    Prog ++ " "
        ++ Static ++ " "
        ++ NameSw ++ " " ++ NameStr ++ " "
        ++ "-pa " ++ Pa ++ " "
        ++ "-env ERL_CRASH_DUMP " ++ Pwd ++ "/erl_crash_dump." ++ NameStr ++ " "
        ++ "-setcookie " ++ atom_to_list(erlang:get_cookie()).

full_node_name(PreName) ->
    HostSuffix = lists:dropwhile(fun ($@) -> false; (_) -> true end,
                                 atom_to_list(node())),
    list_to_atom(atom_to_list(PreName) ++ HostSuffix).

ping_node(_Node, 0) ->
    {error, net_adm};
ping_node(Node, N) when is_integer(N), N > 0 ->
    case catch net_adm:ping(Node) of
        pong -> 
	    wait_for_process(Node, code_server, 50);
        _ ->
	    timer:sleep(1000),
            ping_node(Node, N-1)
    end.

wait_for_process(_Node, Name, 0) ->
    {error, Name};
wait_for_process(Node, Name, N) when is_integer(N), N > 0 ->
    case rpc:call(Node, erlang, whereis, [Name]) of
	undefined ->
	    timer:sleep(1000),
	    wait_for_process(Node, Name, N-1);
	{badrpc, _} = Reason ->
	    erlang:error({Reason, Node});
	Pid when is_pid(Pid) ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run escript

run(Dir, Cmd0) ->
    Cmd = case os:type() of
              {win32,_} -> filename:nativename(Dir) ++ "\\" ++ Cmd0;
              _ -> Cmd0
          end,
    do_run(Dir, Cmd).

run(Dir, Opts, Cmd0) ->
    Cmd = case os:type() of
              {win32,_} -> Opts ++ " " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
              _ -> Opts ++ " " ++ Dir ++ "/" ++ Cmd0
          end,
    do_run(Dir, Cmd).

do_run(Dir, Cmd) ->
    io:format("Run: ~p\n", [Cmd]),
    Env = [{"PATH",Dir++":"++os:getenv("PATH")}],
    Port = open_port({spawn,Cmd}, [exit_status,eof,in,{env,Env}]),
    Res = get_data(Port, []),
    receive
        {Port,{exit_status,ExitCode}} ->
            iolist_to_binary([Res,"ExitCode:"++integer_to_list(ExitCode)])
    end.

get_data(Port, SoFar) ->
    receive
        {Port,{data,Bytes}} ->
            get_data(Port, [SoFar|Bytes]);
        {Port,eof} ->
            erlang:port_close(Port),
            SoFar
    end.

expected_output([data_dir|T], Data) ->
    Slash = case os:type() of
                {win32,_} -> "\\";
                _ -> "/"
            end,
    [filename:nativename(Data)++Slash|expected_output(T, Data)];
expected_output([H|T], Data) ->
    [H|expected_output(T, Data)];
expected_output([], _) -> 
    [];
expected_output(Bin, _) when is_binary(Bin) -> 
    Bin.
