%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2009-2018. All Rights Reserved.
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

-module(reltool_server_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("reltool/src/reltool.hrl").
-include("reltool_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(NODE_NAME, '__RELTOOL__TEMPORARY_TEST__NODE__').
-define(WORK_DIR, "reltool_work_dir").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialization functions.

init_per_suite(Config) ->
    {ok,Cwd} = file:get_cwd(),
    ?ignore(file:make_dir(?WORK_DIR)),
    [{cwd,Cwd}|reltool_test_lib:init_per_suite(Config)].

end_per_suite(Config) ->
    reltool_test_lib:end_per_suite(Config).

init_per_testcase(Func,Config) ->
    Node = full_node_name(?NODE_NAME),
    case net_adm:ping(Node) of
	pong -> stop_node(Node);
	pang -> ok
    end,
    reltool_test_lib:init_per_testcase(Func,Config).
end_per_testcase(Func,Config) ->
    ok = file:set_cwd(filename:join(?config(cwd,Config),?WORK_DIR)),
    {ok,All}  = file:list_dir("."),
    Files = [F || F <- All, false == lists:prefix("save.",F)],
    case ?config(tc_status,Config) of
	ok ->
	    ok;
	_Fail ->
	    SaveDir = "save."++atom_to_list(Func),
	    ok = file:make_dir(SaveDir),
	    save_test_result(Files,SaveDir)
    end,
    rm_files(Files),
    ok = file:set_cwd(?config(cwd,Config)),
    reltool_test_lib:end_per_testcase(Func,Config).


save_test_result(Files,DestDir) ->
    Tar = "copy.tar",
    ok = erl_tar:create(Tar, Files),
    ok = erl_tar:extract(Tar, [{cwd,DestDir}]),
    ok = file:delete(Tar),
    ok.

rm_files([F | Fs]) ->
    case file:read_file_info(F) of
	{ok,#file_info{type=directory}} ->
	    rm_dir(F);
	{ok,_Regular} ->
	    ok = file:delete(F)
    end,
    rm_files(Fs);
rm_files([]) ->
    ok.

rm_dir(Dir) ->
    {ok,Files} = file:list_dir(Dir),
    rm_files([filename:join(Dir, F) || F <- Files]),
    ok = file:del_dir(Dir).




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
     create_script_without_dot_erlang,
     create_script_sort,
     create_target,
     create_target_unicode,
     create_embedded,
     create_standalone,
     create_standalone_beam,
     create_standalone_app,
     create_standalone_app_clash,
     create_multiple_standalone,
     create_old_target,
     create_slim,
     eval_target_spec,
     otp_9135,
     otp_9229_dupl_mod_exclude_app,
     otp_9229_dupl_mod_exclude_mod,
     dupl_mod_in_app_file,
     include_non_existing_app,
     exclude_non_existing_app,
     get_apps,
     get_mod,
     get_sys,
     set_app_and_undo,
     set_apps_and_undo,
     set_apps_inlined,
     set_sys_and_undo,
     load_config_and_undo,
     load_config_fail,
     load_config_escript_path,
     load_config_same_escript_source,
     load_config_same_escript_beam,
     load_config_add_escript,
     reset_config_and_undo,
     gen_rel_files,
     save_config,
     dependencies,
     mod_incl_cond_derived,
     dep_in_app_not_xref,
     use_selected_vsn,
     use_selected_vsn_relative_path,
     non_standard_vsn_id,
     undefined_regexp,
     windows_erl_libs].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%% The test cases

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A dummy break test case which is NOT in all(), but can be run
%% directly from the command line with ct_run. It just does a
%% test_server:break()...
break(_Config) ->
    test_server:break(""),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Start a server process and check that it does not crash

start_server(_Config) ->
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([])),
    Libs = reltool_test_lib:erl_libs(),
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

set_config(_Config) ->
    Libs = reltool_test_lib:erl_libs(),
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

    KVsn = latest(kernel),
    StdVsn = latest(stdlib),
    SaslVsn = latest(sasl),

    LibDir = code:lib_dir(),
    KLibDir = filename:join(LibDir,"kernel-"++KVsn),
    StdLibDir = filename:join(LibDir,"stdlib-"++StdVsn),
    SaslLibDir = filename:join(LibDir,"sasl-"++SaslVsn),

    Libs = reltool_test_lib:erl_libs(),
    LibDirs =
        case Libs of
            [] -> [];
            _ -> [{lib_dirs,Libs}]
        end,

    Sys = {sys,LibDirs ++
               [{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include},{vsn,SaslVsn}]},
		{app,stdlib,[{incl_cond,include},{lib_dir,StdLibDir}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?m({ok, Sys}, reltool:get_config(Pid)),
    ?m({ok, Sys}, reltool:get_config(Pid,false,false)),

    %% Include derived info
    case Libs of
        [] ->
            ?msym({ok,{sys,[{incl_cond, exclude},
                            {erts,[]},
                            {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
                            {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
                                       {mod,_,[]}|_]},
                            {app,stdlib,[{incl_cond,include},{lib_dir,StdLibDir},
                                         {mod,_,[]}|_]}]}},
                  reltool:get_config(Pid,false,true));
        _ ->
            ?msym({ok,{sys,[{lib_dirs,Libs},
                            {incl_cond, exclude},
                            {erts,[]},
                            {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
                            {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
                                       {mod,_,[]}|_]},
                            {app,stdlib,[{incl_cond,include},{lib_dir,StdLibDir},
                                         {mod,_,[]}|_]}]}},
                  reltool:get_config(Pid,false,true))
    end,

    %% Include defaults
    ?msym({ok,{sys,[{root_dir,_},
		    {lib_dirs,_},
		    {mod_cond,all},
		    {incl_cond,exclude},
		    {app,kernel,[{incl_cond,include},{vsn,undefined},
				 {lib_dir,undefined}]},
		    {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
			       {lib_dir,undefined}]},
		    {app,stdlib,[{incl_cond,include},{vsn,undefined},
				 {lib_dir,StdLibDir}]},
		    {boot_rel,"start_clean"},
                    {rel,"no_dot_erlang","1.0",[],[{load_dot_erlang,false}]},
		    {rel,"start_clean","1.0",[],[{load_dot_erlang,true}]},
		    {rel,"start_sasl","1.0",[sasl],[{load_dot_erlang,true}]},
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

    %% Include both defaults and derived info
    ?msym({ok,{sys,[{root_dir,_},
		    {lib_dirs,_},
		    {mod_cond,all},
		    {incl_cond,exclude},
		    {erts,[]},
		    {app,kernel,[{incl_cond,include},{vsn,KVsn},
				 {lib_dir,KLibDir},{mod,_,[]}|_]},
		    {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
				 {lib_dir,SaslLibDir},{mod,_,[]}|_]},
		    {app,stdlib,[{incl_cond,include},{vsn,StdVsn},
				 {lib_dir,StdLibDir},{mod,_,[]}|_]},
		    {boot_rel,"start_clean"},
                    {rel,"no_dot_erlang","1.0",[],[{load_dot_erlang,false}]},
		    {rel,"start_clean","1.0",[],[{load_dot_erlang,true}]},
		    {rel,"start_sasl","1.0",[sasl],[{load_dot_erlang,true}]},
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

otp_9135(_Config) ->
    Libs = reltool_test_lib:erl_libs(),
    StrippedDefaultSys = 
        case Libs of
            [] -> [];
            _  -> [{lib_dirs, Libs}]
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

create_release_sort(Config) ->
    DataDir = ?config(data_dir,Config),
    %% Configure the server
    RelName1 = "MnesiaFirst",
    RelName2 = "SaslFirst",
    RelName3 = "Include-both",
    RelName4 = "Include-only-app",
    RelName5 = "Include-only-rel",
    RelName6 = "Auto-add-missing-apps",
    RelName7 = "Circular",
    RelName8 = "Include-rel-alter-order",
    RelName9 = "Include-none-overwrite",
    RelName10= "Uses-order-as-rel",
    RelName11= "Auto-add-dont-overwrite-load",
    RelVsn = "1.0",
    %% Application z (.app file):
    %%     includes [tools, mnesia]
    %%     uses [kernel, stdlib, sasl, inets]
    Sys =
        {sys,
         [
          {lib_dirs, [filename:join(DataDir,"sort_apps")]},
          {boot_rel, RelName1},
          {rel, RelName1, RelVsn, [stdlib, kernel, mnesia, sasl]},
          {rel, RelName2, RelVsn, [stdlib, kernel, sasl, mnesia]},
          {rel, RelName3, RelVsn, [stdlib, kernel, {z,[tools]}, tools, mnesia]},
          {rel, RelName4, RelVsn, [stdlib, kernel, z, mnesia, tools]},
          {rel, RelName5, RelVsn, [stdlib, kernel, {sasl,[tools]}]},
          {rel, RelName6, RelVsn, [z]},
	  {rel, RelName7, RelVsn, [stdlib, kernel, mnesia, y, sasl, x]},
          {rel, RelName8, RelVsn, [stdlib, kernel, {z,[mnesia,tools]}]},
          {rel, RelName9, RelVsn, [stdlib, kernel, {z,[]}]},
          {rel, RelName10, RelVsn, [stdlib, kernel, {z,[]}, inets, sasl]},
          {rel, RelName11, RelVsn, [stdlib, kernel, z, {inets, load}]},
	  {incl_cond,exclude},
	  {mod_cond,app},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,mnesia,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]},
	  {app,inets,[{incl_cond,include}]},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,include}]},
	  {app,z,[{incl_cond,include}]},
	  {app,tools,[{mod_cond,app},{incl_cond,include}]}
         ]},
    %% Generate release
    ?msym({ok, {release, {RelName1, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {mnesia, _},
		 {sasl, _}]}},
	  reltool:get_rel([{config, Sys}], RelName1)),

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
		 {sasl, _},
		 {inets, _},
		 {tools, _},
		 {z, _, [tools]},
		 {mnesia, _}]}},
	  reltool:get_rel([{config, Sys}], RelName3)),

    ?msym({ok, {release, {RelName4, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {sasl, _},
		 {inets, _},
		 {mnesia, _},
		 {tools, _},
		 {z, _}]}},
	  reltool:get_rel([{config, Sys}], RelName4)),

    ?m({error,"sasl: These applications are used by release "
	"Include-only-rel but are missing as included_applications "
	"in the app file: [tools]"},
       reltool:get_rel([{config, Sys}], RelName5)),

    ?msym({ok, {release, {RelName6, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {sasl, _},
		 {inets, _},
		 {tools, _},
		 {mnesia, _},
		 {z, _}]}},
       reltool:get_rel([{config, Sys}], RelName6)),

    ?m({error,"Circular dependencies: [x,y]"},
       reltool:get_rel([{config, Sys}], RelName7)),

    ?msym({ok, {release, {RelName8, RelVsn},
		{erts, _},
		[{kernel, _},
		 {stdlib, _},
		 {sasl, _},
		 {inets, _},
		 {mnesia, _},
		 {tools, _},
		 {z, _, [mnesia,tools]}]}},
       reltool:get_rel([{config, Sys}], RelName8)),

    ?msym({ok,{release,{RelName9,RelVsn},
	       {erts,_},
	       [{kernel,_},
		{stdlib,_},
		{sasl, _},
		{inets, _},
		{z,_,[]}]}},
	  reltool:get_rel([{config, Sys}], RelName9)),

    ?msym({ok,{release,{RelName10,RelVsn},
	       {erts,_},
	       [{kernel,_},
		{stdlib,_},
		{inets, _},
		{sasl, _},
		{z,_,[]}]}},
	  reltool:get_rel([{config, Sys}], RelName10)),

    ?msym({ok,{release,{RelName11,RelVsn},
	       {erts,_},
	       [{kernel,_},
		{stdlib,_},
		{sasl, _},
		{inets, _, load},
		{tools, _},
		{mnesia, _},
		{z,_}]}},
	  reltool:get_rel([{config, Sys}], RelName11)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate boot scripts

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

    %% A release defaults to load_dot_erlang == true
    {script, {RelName, RelVsn}, ScriptInstructions} = Script,
    ?m(true, lists:member({apply,{c,erlangrc,[]}}, ScriptInstructions)),

    %% Stop server
    ?m(ok, reltool:stop(Pid)),
    ok.

create_script_without_dot_erlang(_Config) ->
    %% Configure the server
    RelName = "Just testing",
    RelVsn = "1.0",
    Config =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [stdlib, kernel], [{load_dot_erlang, false}]}
         ]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Config}])),

    %% Confirm that load_dot_erlang == false was used
    {ok, Script} = ?msym({ok, _}, reltool:get_script(Pid, RelName)),
    {script, {RelName, RelVsn}, ScriptInstructions} = Script,
    ?m(false, lists:member({apply,{c,erlangrc,[]}}, ScriptInstructions)),

    %% Stop server
    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test creation of .script with different sorting of applications and
%% included applications.
%% Test that result is equal to what systools produces
create_script_sort(Config) ->
    DataDir = ?config(data_dir,Config),
    %% Configure the server
    RelName1 = "MnesiaFirst",
    RelName2 = "SaslFirst",
    RelName3 = "Include-both",
    RelName4 = "Include-only-app",
    RelName5 = "Include-only-rel",
    RelName6 = "Auto-add-missing-apps",
    RelName7 = "Circular",
    RelName8 = "Include-rel-alter-order",
    RelName9 = "Include-none-overwrite",
    RelName10= "Uses-order-as-rel",
    RelVsn = "1.0",
    LibDir = filename:join(DataDir,"sort_apps"),
    %% Application z (.app file):
    %%     includes [tools, mnesia]
    %%     uses [kernel, stdlib, sasl, inets]
    Sys =
        {sys,
         [
          {lib_dirs, [LibDir]},
          {boot_rel, RelName1},
          {rel, RelName1, RelVsn, [stdlib, kernel, mnesia, sasl]},
          {rel, RelName2, RelVsn, [stdlib, kernel, sasl, mnesia]},
          {rel, RelName3, RelVsn, [stdlib, kernel, {z,[tools]}, tools, mnesia]},
          {rel, RelName4, RelVsn, [stdlib, kernel, z, mnesia, tools]},
          {rel, RelName5, RelVsn, [stdlib, kernel, {sasl,[tools]}]},
          {rel, RelName6, RelVsn, [z]},
	  {rel, RelName7, RelVsn, [stdlib, kernel, mnesia, y, sasl, x]},
          {rel, RelName8, RelVsn, [stdlib, kernel, {z,[mnesia,tools]}]},
          {rel, RelName9, RelVsn, [stdlib, kernel, {z,[]}]},
          {rel, RelName10, RelVsn, [stdlib, kernel, {z,[]}, inets, sasl]},
	  {incl_cond,exclude},
	  {mod_cond,app},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,mnesia,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]},
	  {app,inets,[{incl_cond,include}]},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,include}]},
	  {app,z,[{incl_cond,include}]},
	  {app,tools,[{mod_cond,app},{incl_cond,include}]}
         ]},

    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    %% Generate release files
    application:load(sasl),
    application:load(inets),
    application:load(mnesia),
    application:load(tools),
    {ok,KernelVsn} = application:get_key(kernel,vsn),
    {ok,StdlibVsn} = application:get_key(stdlib,vsn),
    {ok,SaslVsn} = application:get_key(sasl,vsn),
    {ok,InetsVsn} = application:get_key(inets,vsn),
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
	      {tools,ToolsVsn},
	      {mnesia,MnesiaVsn},
	      {sasl,SaslVsn},
	      {inets,InetsVsn}]},
    FullName3 = filename:join(?WORK_DIR,RelName3),
    ?m(ok, file:write_file(FullName3 ++ ".rel", io_lib:format("~p.\n", [Rel3]))),
    Rel4 = {release, {RelName4,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0"},
	      {mnesia,MnesiaVsn},
	      {tools,ToolsVsn},
	      {sasl,SaslVsn},
	      {inets,InetsVsn}]},
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
	      {sasl,SaslVsn},
	      {inets,InetsVsn},
	      {tools,ToolsVsn},
	      {mnesia,MnesiaVsn},
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
	      {z,"1.0",[mnesia,tools]},
	      {sasl,SaslVsn},
	      {inets,InetsVsn},
	      {mnesia,MnesiaVsn},
	      {tools,ToolsVsn}]},
    FullName8 = filename:join(?WORK_DIR,RelName8),
    ?m(ok, file:write_file(FullName8 ++ ".rel", io_lib:format("~p.\n", [Rel8]))),
    Rel9 = {release, {RelName9,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0",[]},
	      {sasl,SaslVsn},
	      {inets,InetsVsn}]},
    FullName9 = filename:join(?WORK_DIR,RelName9),
    ?m(ok, file:write_file(FullName9 ++ ".rel", io_lib:format("~p.\n", [Rel9]))),
    Rel10 = {release, {RelName10,RelVsn}, {erts,ErtsVsn},
	     [{kernel,KernelVsn},
	      {stdlib,StdlibVsn},
	      {z,"1.0",[]},
	      {inets,InetsVsn},
	      {sasl,SaslVsn}]},
    FullName10 = filename:join(?WORK_DIR,RelName10),
    ?m(ok, file:write_file(FullName10 ++ ".rel", io_lib:format("~p.\n", [Rel10]))),

    %% Generate script files with systools and reltool and compare
    ZPath = filename:join([LibDir,"*",ebin]),

    ?msym({ok,_,_}, systools_make_script(FullName1,ZPath)),
    {ok, [SystoolsScript1]} = ?msym({ok,[_]}, file:consult(FullName1++".script")),
    {ok, Script1} = ?msym({ok, _}, reltool:get_script(Pid, RelName1)),
    ?m(equal, diff_script(SystoolsScript1, Script1)),

    ?msym({ok,_,_}, systools_make_script(FullName2,ZPath)),
    {ok, [SystoolsScript2]} = ?msym({ok,[_]}, file:consult(FullName2++".script")),
    {ok, Script2} = ?msym({ok, _}, reltool:get_script(Pid, RelName2)),
    ?m(equal, diff_script(SystoolsScript2, Script2)),

    ?msym({ok,_,_}, systools_make_script(FullName3,ZPath)),
    {ok, [SystoolsScript3]} = ?msym({ok,[_]}, file:consult(FullName3++".script")),
    {ok, Script3} = ?msym({ok, _}, reltool:get_script(Pid, RelName3)),
    ?m(equal, diff_script(SystoolsScript3, Script3)),

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

    ?msym({ok,_,_}, systools_make_script(FullName6,ZPath)),
    {ok, [SystoolsScript6]} = ?msym({ok,[_]}, file:consult(FullName6++".script")),
    {ok, Script6} = ?msym({ok, _}, reltool:get_script(Pid, RelName6)),
    ?m(equal, diff_script(SystoolsScript6, Script6)),

    ?msym({error,_,{circular_dependencies,_}},
	  systools_make_script(FullName7,ZPath)),
    ?m({error,"Circular dependencies: [x,y]"},
       reltool:get_script(Pid, RelName7)),

    ?msym({ok,_,_}, systools_make_script(FullName8,ZPath)),
    {ok, [SystoolsScript8]} = ?msym({ok,[_]}, file:consult(FullName8++".script")),
    {ok, Script8} = ?msym({ok, _}, reltool:get_script(Pid, RelName8)),
    ?m(equal, diff_script(SystoolsScript8, Script8)),

    ?msym({ok,_,_}, systools_make_script(FullName9,ZPath)),
    {ok, [SystoolsScript9]} = ?msym({ok,[_]}, file:consult(FullName9++".script")),
    {ok, Script9} = ?msym({ok, _}, reltool:get_script(Pid, RelName9)),
    ?m(equal, diff_script(SystoolsScript9, Script9)),

    ?msym({ok,_,_}, systools_make_script(FullName10,ZPath)),
    {ok, [SystoolsScript10]} = ?msym({ok,[_]}, file:consult(FullName10++".script")),
    {ok, Script10} = ?msym({ok, _}, reltool:get_script(Pid, RelName10)),
    ?m(equal, diff_script(SystoolsScript10, Script10)),

    %% Stop server
    ?m(ok, reltool:stop(Pid)),
    ok.


systools_make_script(Name,Path) ->
    systools:make_script(Name,[{path,[Path]},{outdir,?WORK_DIR},silent]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system

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
%% Generate target system

create_target_unicode(Config) ->
    DataDir = ?config(data_dir,Config),

    %% If file name translation mode is unicode, then use unicode
    %% characters release name (which will be used as file name for
    %% .rel, .script and .boot), and install the release under a path
    %% which icludes unicode characters.
    {RelNamePrefix,TargetDirName} =
	case file:native_name_encoding() of
	    utf8 ->
		{"Unicode test αβ","target_unicode_αβ"} ;
	    latin1 ->
		{"Unicode test","target_unicode"}
	end,

    %% Configure the server
    RelName1 = RelNamePrefix,
    RelName2 = RelNamePrefix ++ " with SASL",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, [filename:join(DataDir,"unicode")]},
	  {app_file, all},
	  {incl_cond,exclude},
	  {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel, ua]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel, ua]},
          {app, kernel, [{incl_cond, include}]},
          {app, stdlib, [{incl_cond, include}]},
          {app, sasl, [{incl_cond, include}]},
          {app, ua, [{incl_cond, include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, TargetDirName]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ok = ?m(ok, reltool:create_target([{config, Sys}], TargetDir)),

    %% Start a node
    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),


    %% The ua application has a unicode string as description - check
    %% that it is translated correctly.
    wait_for_app(Node,ua,50),
    Apps = rpc:call(Node,application,which_applications,[]),
    ?m({ua,"Application for testing unicode in reltool - αβ","1.0"},
       lists:keyfind(ua,1,Apps)),

    %% Check that the release name is correct (really only
    %% insteresting if file name translation mode is utf8)
    [{RelName,_,_,_}] =
	?msym([{_,_,_,_}],rpc:call(Node,release_handler,which_releases,[])),
    ?m(true,lists:prefix(RelNamePrefix,RelName)),

    ?msym(ok, stop_node(Node)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate embedded target system

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

    %% Start the target system and fetch root dir
    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)), 
    RootDir = ?ignore(rpc:call(Node, code, root_dir, [])),
    ?msym(ok, stop_node(Node)),
    
    %% Execute escript
    Expected =  s2b(["Root dir: ", RootDir, "\n"
		     "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
		     "Emuarg: [\"emuvalue\"]\n",
		     "ExitCode:0"]),
    io:format("Expected: ~ts\n", [Expected]),
    ?m(Expected, run(BinDir, EscriptName, "-arg1 arg2 arg3")),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate standalone system with inlined beam file

create_standalone_beam(Config) ->
    %% Read beam file
    DataDir = ?config(data_dir,Config),
    BeamFile = filename:join([DataDir,escript,"someapp-1.0",ebin,"mymod.beam"]),
    {ok,BeamBin} = file:read_file(BeamFile),

    %% Create the escript
    EscriptName = "mymod.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,{beam,BeamBin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Configure the server
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_standalone_beam"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config, Sys}], TargetDir)),

    %% Start the target system and fetch root dir
    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    RootDir = ?ignore(rpc:call(Node, code, root_dir, [])),
    ?msym(ok, stop_node(Node)),

    %% Execute escript
    Expected =  s2b(["Module: mymod\n"
		     "Root dir: ", RootDir, "\n"
		     "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
		     "ExitCode:0"]),
    io:format("Expected: ~ts\n", [Expected]),
    ?m(Expected, run(BinDir, EscriptName, "-arg1 arg2 arg3")),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate standalone system with inlined archived application

create_standalone_app(Config) ->
    %% Create archive
    DataDir = ?config(data_dir,Config),
    EscriptDir = filename:join(DataDir,escript),
    {ok,{_Archive,Bin}} = zip:create("someapp-1.0.ez",["someapp-1.0"],
				     [memory,
				      {cwd,EscriptDir},
				      {compress,all},
				      {uncompress,[".beam",".app"]}]),

    %% Create the escript
    EscriptName = "someapp.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,
				 {emu_args,"-escript main mymod"},
				 {archive,Bin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Configure the server
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_standalone_app"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config, Sys}], TargetDir)),

    %% Start the target system and fetch root dir
    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    RootDir = ?ignore(rpc:call(Node, code, root_dir, [])),
    ?msym(ok, stop_node(Node)),

    %% Execute escript
    Expected =  s2b(["Module: mymod\n"
		     "Root dir: ", RootDir, "\n"
		     "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
		     "ExitCode:0"]),
    io:format("Expected: ~ts\n", [Expected]),
    ?m(Expected, run(BinDir, EscriptName, "-arg1 arg2 arg3")),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate standalone system with inlined archived application
%% Check that the inlined app cannot be explicitly configured

create_standalone_app_clash(Config) ->
    %% Create archive
    DataDir = ?config(data_dir,Config),
    EscriptDir = filename:join(DataDir,escript),
    {ok,{_Archive,Bin}} = zip:create("someapp-1.0.ez",["someapp-1.0"],
				     [memory,
				      {cwd,EscriptDir},
				      {compress,all},
				      {uncompress,[".beam",".app"]}]),

    %% Create the escript
    EscriptName = "someapp.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,
				 {emu_args,"-escript main mymod"},
				 {archive,Bin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Configure the server
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone},
	  {app, someapp, [{incl_cond,include}]}
         ]},

    ?msym({error,"someapp: Application name clash. Escript "++_},
	  reltool:start_server([{config,Sys}])),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate standalone system with multiple escripts

create_multiple_standalone(Config) ->
    %% First escript
    ExDir = code:lib_dir(reltool, examples),
    EscriptName1 = "display_args",
    Escript1 = filename:join([ExDir, EscriptName1]),

    %% Second escript
    DataDir = ?config(data_dir,Config),
    BeamFile = filename:join([DataDir,escript,"someapp-1.0",ebin,"mymod.beam"]),
    {ok,BeamBin} = file:read_file(BeamFile),
    EscriptName2 = "mymod.escript",
    Escript2 = filename:join(?WORK_DIR,EscriptName2),
    ok = escript:create(Escript2,[shebang,{beam,BeamBin}]),
    ok = file:change_mode(Escript2,8#00744),

    %% Configure server
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript1, [{incl_cond, include}]},
          {escript, Escript2, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    %% Generate target system
    TargetDir = filename:join([?WORK_DIR, "target_multiple_standalone"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ok = ?m(ok, reltool:create_target([{config,Sys}], TargetDir)),

    %% Start the target system and fetch root dir
    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    RootDir = ?ignore(rpc:call(Node, code, root_dir, [])),
    ?msym(ok, stop_node(Node)),

    %% Execute escript1
    Expected1 =  s2b(["Root dir: ", RootDir, "\n"
		      "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
		      "Emuarg: [\"emuvalue\"]\n",
		      "ExitCode:0"]),
    io:format("Expected1: ~ts\n", [Expected1]),
    ?m(Expected1, run(BinDir, EscriptName1, "-arg1 arg2 arg3")),


    %% Execute escript2
    Expected2 =  s2b(["Module: mymod\n"
		      "Root dir: ", RootDir, "\n"
		      "Script args: [\"-arg1\",\"arg2\",\"arg3\"]\n",
		      "ExitCode:0"]),
    io:format("Expected2: ~ts\n", [Expected2]),
    ?m(Expected2, run(BinDir, EscriptName2, "-arg1 arg2 arg3")),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate old type of target system
create_old_target(_Config) ->
    
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

    ok = ?m(ok, reltool:install(RelName2, TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system

create_slim(Config) ->
    %% Configure the server
    RelName = "slim",
    RelVsn = "1.0",

    DataDir =  ?config(data_dir,Config),
    LibDir = filename:join(DataDir,"slim"),

    Sys =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [sasl, stdlib, kernel, a]},
          {app, sasl, [{incl_cond, include}]},
          {app, a, [{incl_cond, include},
		    {lib_dir,filename:join(LibDir,"a-1.0")}]},
	  {excl_lib,otp_root}
         ]},

    %% Generate target file
    TargetDir = filename:absname(filename:join([?WORK_DIR, "target_slim"])),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ok = ?m(ok, reltool:create_target([{config, Sys}], TargetDir)),

    TargetLibDir = filename:join(TargetDir,"lib"),
    TargetRelDir = filename:join(TargetDir,"releases"),
    TargetRelVsnDir = filename:join(TargetRelDir,RelVsn),

    {ok,["a-1.0.ez"]} = file:list_dir(TargetLibDir),

    RootDir = code:root_dir(),
    Erl = filename:join([RootDir, "bin", "erl"]),
    Args = ["-boot_var", "RELTOOL_EXT_LIB", TargetLibDir,
	    "-boot", filename:join(TargetRelVsnDir,RelName),
	    "-sasl", "releases_dir", "\""++TargetRelDir++"\""],
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl, Args)),
    ?msym(RootDir, rpc:call(Node, code, root_dir, [])),
    wait_for_app(Node,sasl,50),
    ?msym([{RelName,RelVsn,_,permanent}],
	  rpc:call(Node,release_handler,which_releases,[])),
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
otp_9229_dupl_mod_exclude_app(Config) ->
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
otp_9229_dupl_mod_exclude_mod(Config) ->
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
%% Test that if a module is duplicated in a .app file, then a warning
%% is produced, but target can still be created.
dupl_mod_in_app_file(Config) ->
    DataDir =  ?config(data_dir,Config),
    LibDir = filename:join(DataDir,"dupl_mod"),

    %% Configure the server
    Sys =
        {sys,
         [
          {lib_dirs, [LibDir]},
	  {incl_cond,exclude},
	  {app,a,[{incl_cond,include}]},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,sasl,[{incl_cond,include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_dupl_mod_in_app_file"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ?m({ok,["Module a duplicated in app file for application a."]},
       reltool:get_status([{config, Sys}])),

    %%! test that only one module installed (in spec)

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that a reasonable error message is returned if an application
%% is missing
include_non_existing_app(_Config) ->
    %% Configure the server
    Sys =
        {sys,
         [
          {incl_cond,exclude},
          {app,foobar,[{incl_cond,include}]},
          {app,kernel,[{incl_cond,include}]},
          {app,stdlib,[{incl_cond,include}]},
          {app,sasl,[{incl_cond,include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_include_non_existing_app"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ?m({error,"foobar: Missing application directory."},
       reltool:get_status([{config, Sys}])),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that if a missing application is explicitly excluded a warning
%% should be issued.
exclude_non_existing_app(_Config) ->
    %% Configure the server
    Sys =
        {sys,
         [
          {incl_cond,exclude},
          {app,foobar,[{incl_cond,exclude}]},
          {app,kernel,[{incl_cond,include}]},
          {app,stdlib,[{incl_cond,include}]},
          {app,sasl,[{incl_cond,include}]}
         ]},

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_exclude_non_existing_app"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ?m({ok,["foobar: Missing application directory."]},
       reltool:get_status([{config, Sys}])),

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
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    {ok,Cover} = ?msym({ok,#mod{name=cover, is_included=true}},
		       reltool_server:get_mod(Pid,cover)),

    %% Exclude one module with set_app
    ExclCover = Cover#mod{incl_cond=exclude},
    Mods = Tools#app.mods,
    Tools1 = Tools#app{mods = lists:keyreplace(cover,#mod.name,Mods,ExclCover)},
    {ok,ToolsNoCover,["a: Cannot parse app file"++_|_]} =
	?msym({ok,_,["a: Cannot parse app file"++_|_]},
	      reltool_server:set_app(Pid,Tools1)),
    ?msym({ok,["a: Cannot parse app file"++_]}, reltool_server:get_status(Pid)),

    %% Check that the module is no longer included
    ?m({ok,ToolsNoCover}, reltool_server:get_app(Pid,tools)),
    {ok,NoIncludeCover} = ?msym({ok,#mod{name=cover, is_included=false}},
				reltool_server:get_mod(Pid,cover)),

    %% Undo
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?msym(ok, reltool_server:undo_config(Pid)),
    ?m({ok,ToolsNoCover}, reltool_server:get_app(Pid,tools)),
    ?m({ok,NoIncludeCover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]}, reltool_server:get_status(Pid)),

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
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(true, Tools#app.is_pre_included),
    ?m(true, Tools#app.is_included),
    {ok,Cover} = ?msym({ok,#mod{name=cover, is_included=true}},
		       reltool_server:get_mod(Pid,cover)),

    %% Exclude one application with set_apps
    ExclTools = Tools#app{incl_cond=exclude},
    ?msym({ok,["a: Cannot parse app file"++_]},
       reltool_server:set_apps(Pid,[ExclTools])),
    ?msym({ok,["a: Cannot parse app file"++_]}, reltool_server:get_status(Pid)),

    %% Check that the app and its modules (one of them) are no longer included
    {ok,NoTools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),
    ?m(false, NoTools#app.is_pre_included),
    ?m(false, NoTools#app.is_included),
    {ok,NoIncludeCover} = ?msym({ok,#mod{name=cover, is_included=false}},
				reltool_server:get_mod(Pid,cover)),

    %% Undo
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,NoTools}, reltool_server:get_app(Pid,tools)),
    ?m({ok,NoIncludeCover}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]}, reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that escript can be configured, but not its inlined applications
set_apps_inlined(Config) ->
    %% Create archive
    DataDir = ?config(data_dir,Config),
    EscriptDir = filename:join(DataDir,escript),
    {ok,{_Archive,Bin}} = zip:create("someapp-1.0.ez",["someapp-1.0"],
				     [memory,
				      {cwd,EscriptDir},
				      {compress,all},
				      {uncompress,[".beam",".app"]}]),

    %% Create the escript
    EscriptName = "someapp.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,
				 {emu_args,"-escript main mymod"},
				 {archive,Bin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Configure the server
    Sys = {sys,[{incl_cond, exclude},
		{escript,Escript,[]},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    ?msym({ok,[]},reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,EApp} = ?msym({ok,_}, reltool_server:get_app(Pid,'*escript* someapp')),
    {ok,Someapp} = ?msym({ok,_}, reltool_server:get_app(Pid,someapp)),
    ?m(undefined, EApp#app.incl_cond),
    ?m(undefined, Someapp#app.incl_cond),
    ?m(false, Someapp#app.is_included),
    ?m(false, Someapp#app.is_pre_included),

    %% Include escript
    EApp1 = EApp#app{incl_cond=include},
    ?m({ok,[]}, reltool_server:set_apps(Pid,[EApp1])),
    ExpectedEApp = EApp1#app{is_included=true,is_pre_included=true},
    ?m({ok,ExpectedEApp}, reltool_server:get_app(Pid,'*escript* someapp')),
    {ok,Someapp1} = ?msym({ok,_}, reltool_server:get_app(Pid,someapp)),
    ?m(include, Someapp1#app.incl_cond),
    ?m(true, Someapp1#app.is_included),
    ?m(true, Someapp1#app.is_pre_included),

    %% Check that inlined app cannot be configured
    Someapp2 = Someapp1#app{incl_cond=exclude},
    ?msym({error,
	   "Application someapp is inlined in '*escript* someapp'. "
	   "Can not change configuration for an inlined application."},
	  reltool_server:set_apps(Pid,[Someapp2])),
    ?m({ok,Someapp1}, reltool_server:get_app(Pid,someapp)),

    %% Exclude escript
    {ok,EApp2} = ?msym({ok,_}, reltool_server:get_app(Pid,'*escript* someapp')),
    EApp3 = EApp2#app{incl_cond=exclude},
    ?m({ok,[]}, reltool_server:set_apps(Pid,[EApp3])),
    ExpectedEApp3 = EApp3#app{is_included=false,is_pre_included=false},
    ?m({ok,ExpectedEApp3}, reltool_server:get_app(Pid,'*escript* someapp')),
    {ok,Someapp3} = ?msym({ok,_}, reltool_server:get_app(Pid,someapp)),
    ?m(exclude, Someapp3#app.incl_cond),
    ?m(false, Someapp3#app.is_included),
    ?m(false, Someapp3#app.is_pre_included),

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
    ?msym({ok,["a: Cannot parse app file"++_]},
	  reltool_server:set_sys(Pid, NewSysRec)),
    ?m({ok,NewSysRec}, reltool_server:get_sys(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    %% Undo
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,SysRec}, reltool_server:get_sys(Pid)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m(ok,reltool_server:undo_config(Pid)),
    ?m({ok,NewSysRec}, reltool_server:get_sys(Pid)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_config_and_undo(Config) ->
    Sys1 = {sys,Cfg1=[{incl_cond, exclude},
                      {app,kernel,[{incl_cond,include}]},
                      {app,sasl,[{incl_cond,include}]},
                      {app,stdlib,[{incl_cond,include}]},
                      {app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    Libs = reltool_test_lib:erl_libs(),
    Sys11 =
        case Libs of
            [] -> Sys1;
            _  -> {sys, [{lib_dirs, Libs}|Cfg1]}
        end,
    ?m({ok, Sys11}, reltool:get_config(Pid)),
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
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

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
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools2}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover2}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that load_config is properly rolled back if it fails
load_config_fail(_Config) ->
    Sys1 = {sys,Cfg1=[{incl_cond, exclude},
                      {app,kernel,[{incl_cond,include}]},
                      {app,sasl,[{incl_cond,include}]},
                      {app,stdlib,[{incl_cond,include}]},
                      {app,tools,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    Libs = reltool_test_lib:erl_libs(),
    Sys11 =
        case Libs of
            [] -> Sys1;
            _  -> {sys, [{lib_dirs, Libs}|Cfg1]}
        end,
    ?m({ok, Sys11}, reltool:get_config(Pid)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Get app and mod
    {ok,Tools} = ?msym({ok,_}, reltool_server:get_app(Pid,tools)),

    %% Try to load a config with a faulty rel statement (includes a
    %% non-existing application)
    Sys2 = {sys,[{incl_cond, exclude},
		 {boot_rel, "faulty_rel"},
		 {rel, "faulty_rel", "1.0", [kernel, sasl, stdlib, xxx]},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]}]},
    ?msym({error,"Release \"faulty_rel\" uses non existing application xxx"},
	  reltool_server:load_config(Pid,Sys2)),

    %% Check that a rollback is done to the old configuration
    ?m({ok, Sys11}, reltool:get_config(Pid,false,false)),

    %% and that tools is not changed (i.e. that the new configuration
    %% is not applied)
    ?m({ok,Tools}, reltool_server:get_app(Pid,tools)),

    ?m(ok, reltool:stop(Pid)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load config with escript

load_config_escript_path(Config) ->
    %% Create escript
    DataDir = ?config(data_dir,Config),
    BeamFile = filename:join([DataDir,escript,"someapp-1.0",ebin,"mymod.beam"]),
    {ok,BeamBin} = file:read_file(BeamFile),
    EscriptName = "mymod.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,{beam,BeamBin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Start reltool_server with one escript in configuration
    EscriptSys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    {ok, Pid1} = ?msym({ok, _}, reltool:start_server([{config, EscriptSys}])),
    {ok,[#app{name='*escript* mymod'}=A]} =
	?msym({ok,[_]}, reltool_server:get_apps(Pid1,whitelist)),
    ?m(ok, reltool:stop(Pid1)),


    %% Do same again, but now start reltool first with simple config,
    %% then add escript by loading new configuration and check that
    %% #app is the same
    SimpleSys =
        {sys,
         [
          {lib_dirs, []}
         ]},

    {ok, Pid2} = ?msym({ok, _}, reltool:start_server([{config, SimpleSys}])),
    ?m({ok,[]}, reltool_server:get_apps(Pid2,whitelist)),
    ?m({ok,[]}, reltool_server:load_config(Pid2,EscriptSys)),
    ?m({ok,[A]}, reltool_server:get_apps(Pid2,whitelist)),

    ?m(ok, reltool:stop(Pid2)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load config with same (source) escript twice and check that the
%% application information is not changed.

load_config_same_escript_source(_Config) ->
    %% Create escript
    ExDir = code:lib_dir(reltool, examples),
    EscriptName = "display_args",
    Escript = filename:join([ExDir, EscriptName]),

    %% Start reltool_server with one escript in configuration
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
%    {ok,[#app{name='*escript* display_args'}]} =
    ?msym({ok,[#app{name='*escript* display_args',mods=[_]}]},
	  reltool_server:get_apps(Pid,whitelist)),

    %% Load the same config again, then check that app is not changed
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys)),
    ?msym({ok,[#app{name='*escript* display_args',mods=[_]}]},
	  reltool_server:get_apps(Pid,whitelist)),

    ?m(ok, reltool:stop(Pid)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load config with same (beam) escript twice and check that the
%% application information is not changed.

load_config_same_escript_beam(Config) ->
    %% Create escript
    DataDir = ?config(data_dir,Config),
    BeamFile = filename:join([DataDir,escript,"someapp-1.0",ebin,"mymod.beam"]),
    {ok,BeamBin} = file:read_file(BeamFile),
    EscriptName = "mymod.escript",
    Escript = filename:join(?WORK_DIR,EscriptName),
    ok = escript:create(Escript,[shebang,{beam,BeamBin}]),
    ok = file:change_mode(Escript,8#00744),

    %% Start reltool_server with one escript in configuration
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    {ok,[#app{name='*escript* mymod'}=A]} =
	?msym({ok,[_]}, reltool_server:get_apps(Pid,whitelist)),

    %% Load the same config again, then check that app is not changed
    ?m({ok,[]}, reltool_server:load_config(Pid,Sys)),
    ?m({ok,[A]}, reltool_server:get_apps(Pid,whitelist)),

    ?m(ok, reltool:stop(Pid)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load config with escript

load_config_add_escript(Config) ->
    %% First escript
    ExDir = code:lib_dir(reltool, examples),
    EscriptName1 = "display_args",
    Escript1 = filename:join([ExDir, EscriptName1]),

    %% Second escript
    DataDir = ?config(data_dir,Config),
    BeamFile = filename:join([DataDir,escript,"someapp-1.0",ebin,"mymod.beam"]),
    {ok,BeamBin} = file:read_file(BeamFile),
    EscriptName2 = "mymod.escript",
    Escript2 = filename:join(?WORK_DIR,EscriptName2),
    ok = escript:create(Escript2,[shebang,{beam,BeamBin}]),
    ok = file:change_mode(Escript2,8#00744),

    %% Start reltool_server with one escript in configuration
    Sys1 =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript2, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),

    %% Add second escript by loading new configuration
    Sys2 =
        {sys,
         [
          {lib_dirs, []},
          {escript, Escript1, [{incl_cond, include}]},
          {escript, Escript2, [{incl_cond, include}]},
          {profile, standalone}
         ]},

    {ok,[]} = ?m({ok,[]}, reltool_server:load_config(Pid,Sys2)),
    {ok,[#app{name='*escript* display_args'},
	 #app{name='*escript* mymod'}]} =
	?msym({ok,[_,_]}, reltool_server:get_apps(Pid,whitelist)),

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
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

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
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:reset_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

    %% Undo
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools2}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover2}, reltool_server:get_mod(Pid,cover)),
    ?m({ok,[]}, reltool_server:get_status(Pid)),

    %% Undo again, to check that it toggles
    ?m(ok, reltool_server:undo_config(Pid)),
    ?m({ok,Tools1}, reltool_server:get_app(Pid,tools)),
    ?m({ok,Cover1}, reltool_server:get_mod(Pid,cover)),
    ?msym({ok,["a: Cannot parse app file"++_]},reltool_server:get_status(Pid)),

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
    Sys = {sys,Cfg=[{incl_cond, exclude},
                    {app,kernel,[{incl_cond,include}]},
                    {app,sasl,[{incl_cond,include}]},
                    {app,stdlib,[{incl_cond,include}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),
    Libs = reltool_test_lib:erl_libs(),
    Sys1 =
        case Libs of
            [] -> Sys;
            _  -> {sys, [{lib_dirs, Libs}|Cfg]}
        end,
    ?m({ok, Sys1}, reltool:get_config(Pid)),

    Simple = filename:join(PrivDir,"save_simple.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Simple,false,false)),
    ?m({ok,[Sys1]}, file:consult(Simple)),

    Derivates = filename:join(PrivDir,"save_derivates.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Derivates,false,true)),
    case Libs of
        [] ->
            ?msym({ok,[{sys,[{incl_cond, exclude},
                             {erts,[]},
                             {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
                             {app,sasl,[{incl_cond,include},{mod,_,[]}|_]},
                             {app,stdlib,[{incl_cond,include},{mod,_,[]}|_]}]}]},
                  file:consult(Derivates));
        _ ->
            ?msym({ok,[{sys,[{lib_dirs,Libs},
                             {incl_cond, exclude},
                             {erts,[]},
                             {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
                             {app,sasl,[{incl_cond,include},{mod,_,[]}|_]},
                             {app,stdlib,[{incl_cond,include},{mod,_,[]}|_]}]}]},
                  file:consult(Derivates))
    end,

    Defaults = filename:join(PrivDir,"save_defaults.reltool"),
    ?m(ok, reltool_server:save_config(Pid,Defaults,true,false)),
    ?msym({ok,[{sys,[{root_dir,_},
		     {lib_dirs,_},
		     {mod_cond,all},
		     {incl_cond,exclude},
		     {app,kernel,[{incl_cond,include},{vsn,undefined},
				  {lib_dir,undefined}]},
		     {app,sasl,[{incl_cond,include},{vsn,undefined},
				{lib_dir,undefined}]},
		     {app,stdlib,[{incl_cond,include},{vsn,undefined},
				  {lib_dir,undefined}]},
		     {boot_rel,"start_clean"},
                     {rel,"no_dot_erlang","1.0",[],[{load_dot_erlang,false}]},
		     {rel,"start_clean","1.0",[],[{load_dot_erlang,true}]},
		     {rel,"start_sasl","1.0",[sasl],[{load_dot_erlang,true}]},
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
    SaslVsn = latest(sasl),

    LibDir = code:lib_dir(),
    KLibDir = filename:join(LibDir,"kernel-"++KVsn),
    StdLibDir = filename:join(LibDir,"stdlib-"++StdVsn),
    SaslLibDir = filename:join(LibDir,"sasl-"++SaslVsn),

    All = filename:join(PrivDir,"save_all.reltool"),
    ?m(ok, reltool_server:save_config(Pid,All,true,true)),
    ?msym({ok,[{sys,[{root_dir,_},
		     {lib_dirs,_},
		     {mod_cond,all},
		     {incl_cond,exclude},
		     {erts,[]},
		     {app,kernel,[{incl_cond,include},{vsn,KVsn},
				  {lib_dir,KLibDir},{mod,_,[]}|_]},
		     {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
				{lib_dir,SaslLibDir},{mod,_,[]}|_]},
		     {app,stdlib,[{incl_cond,include},{vsn,StdVsn},
				  {lib_dir,StdLibDir},{mod,_,[]}|_]},
		     {boot_rel,"start_clean"},
                     {rel,"no_dot_erlang","1.0",[],[{load_dot_erlang,false}]},
		     {rel,"start_clean","1.0",[],[{load_dot_erlang,true}]},
		     {rel,"start_sasl","1.0",[sasl],[{load_dot_erlang,true}]},
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
%% y-1.0: y0.erl    y1.erl   y2.erl
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
%% Test that incl_cond on mod level overwrites mod_cond on app level
%% Uses same test applications as dependencies/1 above
mod_incl_cond_derived(Config) ->
    %% In app y: mod_cond=none means no module shall be included
    %% but mod_cond is overwritten by incl_cond on mod level
    Sys = {sys,[{lib_dirs,[filename:join(datadir(Config),"dependencies")]},
		{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,x,[{incl_cond,include}]},
		{app,y,[{incl_cond,include},
			{mod_cond,none},
			{mod,y0,[{incl_cond,derived}]},
			{mod,y2,[{incl_cond,derived}]}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    ?msym({ok,[#app{name=kernel},
	       #app{name=sasl},
	       #app{name=stdlib},
	       #app{name=x,uses_apps=[y]},
	       #app{name=y,uses_apps=[]}]},
	  reltool_server:get_apps(Pid,whitelist)),
    {ok, Der} = ?msym({ok,_},reltool_server:get_apps(Pid,derived)),
    ?msym([], rm_missing_app(Der)),
    ?msym({ok,[]}, reltool_server:get_apps(Pid,source)),

    %% 1. check that y0 is not included since it has
    %% incl_cond=derived, but is not used by any other module.
    ?msym({ok,#mod{is_included=undefined}}, reltool_server:get_mod(Pid,y0)),

    %% 2. check that y1 is excluded since it has undefined incl_cond
    %% on mod level, so mod_cond on app level shall be used.
    ?msym({ok,#mod{is_included=false}}, reltool_server:get_mod(Pid,y1)),

    %% 3. check that y2 is included since it has incl_cond=derived and
    %% is used by x3.
    ?msym({ok,#mod{is_included=true}}, reltool_server:get_mod(Pid,y2)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ERL-167, OTP-11993: For applications that are not included in a
%% release spec ('rel'), dependencies in the .app files are not
%% considered - only those found with xref.
dep_in_app_not_xref(Config) ->
    RelName = "Just testing...",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
	  {lib_dirs,[filename:join(datadir(Config),"dep_in_app_not_xref")]},
	  {incl_cond,exclude},
	  {incl_archive_filters,[]},
	  {erts,[{incl_cond,exclude}]},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [kernel, stdlib]},
	  {app,kernel,[{incl_cond,include}]},
	  {app,stdlib,[{incl_cond,include}]},
	  {app,x,[{incl_cond,include}]},
	  {app,y,[{incl_cond,derived}]},
	  {app,z,[{incl_cond,derived}]}
         ]},

    TargetDir = filename:join([?WORK_DIR, "target_dep_in_app_not_xref"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),
    ?log("SPEC: ~p\n", [reltool:get_target_spec([{config, Sys}])]),
    ok = ?m(ok, reltool:create_target([{config, Sys}], TargetDir)),
    ?log("~p~n",[file:list_dir(filename:join([TargetDir,"lib"]))]),

    ?m(true, filelib:is_dir(filename:join([TargetDir,"lib","y-1.0"]))),
    ?m(true, filelib:is_dir(filename:join([TargetDir,"lib","z-1.0"]))),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_selected_vsn(Config) ->
    LibDir1 = filename:join(datadir(Config),"use_selected_vsn"),
    B1Dir = filename:join(LibDir1,"b-1.0"),
    B3Dir = filename:join(LibDir1,"b-3.0"),

    LibDir2 = filename:join(LibDir1,"lib2"),
    B2Dir = filename:join(LibDir2,"b-2.0"),

    %%-----------------------------------------------------------------
    %% Pre-selected vsn of app b
    Sys1 = {sys,[{lib_dirs,[LibDir1]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,b,[{incl_cond,include},{vsn,"1.0"}]}]},
    {ok, Pid1} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    {ok,B11} = ?msym({ok,#app{vsn="1.0",active_dir=B1Dir}},
		     reltool_server:get_app(Pid1,b)),

    %% Change from a pre-selected vsn to use a specific dir
    ?msym({ok, #app{vsn ="3.0", active_dir = B3Dir}, []},
	  reltool_server:set_app(Pid1,
				 B11#app{active_dir = B3Dir,
					 use_selected_vsn = dir,
					 label = undefined,
					 vsn = undefined,
					 info = undefined})),
    ?m(ok, reltool:stop(Pid1)),


    %%-----------------------------------------------------------------
    %% Pre-selected vsn of app b
    Sys2 = {sys,[{lib_dirs,[LibDir1]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,b,[{incl_cond,include},{vsn,"1.0"}]}]},
    {ok, Pid2} = ?msym({ok, _}, reltool:start_server([{config, Sys2}])),
    {ok,B21} = ?msym({ok,#app{vsn="1.0",active_dir=B1Dir}},
		     reltool_server:get_app(Pid2,b)),

    %% Change from a pre-selected vsn to use latest
    ?msym({ok, #app{vsn ="3.0", active_dir = B3Dir}, []},
	  reltool_server:set_app(Pid2,
				 B21#app{use_selected_vsn=undefined,
					 label = undefined,
					 vsn = undefined,
					 info = undefined})),
    ?m(ok, reltool:stop(Pid2)),


    %%-----------------------------------------------------------------
    %% Pre-selected directory for app b
    Sys3 = {sys,[{lib_dirs,[LibDir1]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,b,[{incl_cond,include},{lib_dir,B2Dir}]}]},
    {ok, Pid3} = ?msym({ok, _}, reltool:start_server([{config, Sys3}])),
%    test_server:break("Pid3 = list_to_pid(\""++pid_to_list(Pid3)++"\")."),
    {ok,B31} = ?msym({ok,#app{vsn="2.0",active_dir=B2Dir}},
		     reltool_server:get_app(Pid3,b)),
    %% Change from a pre-selected dir to use latest
    {ok,B32,_} = ?msym({ok, #app{vsn ="3.0", active_dir = B3Dir}, []},
		       reltool_server:set_app(Pid3,
					      B31#app{use_selected_vsn=undefined,
						      label = undefined,
						      vsn = undefined,
						      info = undefined})),
    %% Change back to use selected dir
    {ok,B33,_} = ?msym({ok, #app{vsn ="3.0", active_dir = B3Dir}, []},
		       reltool_server:set_app(Pid3,
					      B32#app{use_selected_vsn = dir})),
    %% use dir 1
    {ok,B34,_} = ?msym({ok, #app{vsn ="1.0", active_dir = B1Dir}, []},
		       reltool_server:set_app(Pid3,
					      B33#app{active_dir = B1Dir,
						      label = undefined,
						      vsn = undefined,
						      info = undefined})),
    %% use dir 2
    {ok,B35,_} = ?msym({ok, #app{vsn ="2.0", active_dir = B2Dir}, []},
		       reltool_server:set_app(Pid3,
					      B34#app{active_dir = B2Dir,
						      label = undefined,
						      vsn = undefined,
						      info = undefined})),
    %% use dir 3
    ?msym({ok, #app{vsn ="3.0", active_dir = B3Dir}, []},
	  reltool_server:set_app(Pid3,
				 B35#app{active_dir = B3Dir,
					 label = undefined,
					 vsn = undefined,
					 info = undefined})),
    ?m(ok, reltool:stop(Pid3)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
use_selected_vsn_relative_path(Config) ->
    LibDir = filename:join([datadir(Config),"use_selected_vsn","b-1.0"]),
    RelDir = filename:join(LibDir,"rel"),

    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(RelDir),

    Sys = {sys,[{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include}]},
		{app,stdlib,[{incl_cond,include}]},
		{app,b,[{incl_cond,include},{lib_dir,".."}]}]},
    {ok, Pid} = ?msym({ok, _}, reltool:start_server([{config, Sys}])),

    ?msym({ok,#app{vsn="1.0",active_dir=LibDir}},reltool_server:get_app(Pid,b)),

    ?m(ok, reltool:stop(Pid)),

    ok = file:set_cwd(Cwd),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test that reltool recognizes an application with its real name even
%% though it uses non standard format for its version number (in the
%% directory name)
non_standard_vsn_id(Config) ->
    LibDir = filename:join(datadir(Config),"non_standard_vsn_id"),
    B1Dir = filename:join(LibDir,"b-first"),
    B2Dir = filename:join(LibDir,"b-second"),

    %%-----------------------------------------------------------------
    %% Default vsn of app b
    Sys1 = {sys,[{lib_dirs,[LibDir]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,b,[{incl_cond,include}]}]},
    {ok, Pid1} = ?msym({ok, _}, reltool:start_server([{config, Sys1}])),
    ?msym({ok,#app{vsn="first",active_dir=B1Dir,sorted_dirs=[B1Dir,B2Dir]}},
	  reltool_server:get_app(Pid1,b)),

    %%-----------------------------------------------------------------
    %% Pre-selected vsn of app b
    Sys2 = {sys,[{lib_dirs,[LibDir]},
		 {incl_cond, exclude},
		 {app,kernel,[{incl_cond,include}]},
		 {app,sasl,[{incl_cond,include}]},
		 {app,stdlib,[{incl_cond,include}]},
		 {app,b,[{incl_cond,include},{vsn,"second"}]}]},
    {ok, Pid2} = ?msym({ok, _}, reltool:start_server([{config, Sys2}])),
    ?msym({ok,#app{vsn="second",active_dir=B2Dir,sorted_dirs=[B1Dir,B2Dir]}},
	  reltool_server:get_app(Pid2,b)),
   ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
undefined_regexp(_Config) ->
    ?msym({ok,_},
          reltool:get_config([{sys,[{app,asn1,[{excl_app_filters,
                                                {add, ["^priv"]}}]}]}])),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks that reltool_utils can correctly read Windows ERL_LIBS

windows_erl_libs(_Config) ->
    WinErlLibs =
        "C:\\Program Files\\Erlang Libs;C:\\Program Files\\More Erlang Libs",
    Ret = reltool_utils:erl_libs(WinErlLibs, {win32, nt}),
    ?m(["C:\\Program Files\\Erlang Libs","C:\\Program Files\\More Erlang Libs"],
       Ret),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Library functions

datadir(Config) ->
    %% Removes the trailing slash...
    filename:nativename(?config(data_dir,Config)).

latest(App) ->
    AppStr = atom_to_list(App),
    AppDirs = filelib:wildcard(filename:join(code:lib_dir(),AppStr++"-*")),
    [LatestAppDir|_] = lists:reverse(AppDirs),
    [_,Vsn] = string:lexemes(filename:basename(LatestAppDir),"-"),
    Vsn.

rm_missing_app(Apps) ->
    lists:keydelete(?MISSING_APP_NAME,#app.name,Apps).

%% We will compare the script generated by systools with
%% the script generated by Reltool.
%%
%% The systools script may include additional modules in
%% the first primLoad command (as a pure optimization).
%% Therefore, we cannot compare the primLoad commands
%% directly. Instead we will collect all modules from
%% all primLoad commands in each script. The same
%% modules must be loaded by both scripts. In addition,
%% the error_handler module must be included in the
%% first primLoad in each script.

diff_script(Script, Script) ->
    equal;
diff_script({script, Rel, Commands1}, {script, Rel, Commands2}) ->
    case diff_cmds(Commands1, Commands2) of
	equal ->
	    Loaded = diff_get_prim_load(Commands1),
	    case diff_get_prim_load(Commands2) of
		Loaded ->
		    equal;
		Other ->
		    io:format("Only loaded by systools: ~p",
			      [Loaded--Other]),
		    io:format("Only loaded by reltool: ~p",
			      [Other--Loaded]),
		    ct:fail(different_prim_loads)
	    end;
	Error ->
	    Error
    end;
diff_script({script, Rel1, _}, {script, Rel2, _}) ->
    {error, {Rel1, Rel2}}.

diff_cmds([{primLoad, Ms1}=Cmd1 | Commands1],
	  [{primLoad, Ms2}=Cmd2 | Commands2]) ->
    case lists:member(error_handler, Ms1) xor
	lists:member(error_handler, Ms2) of
	false ->
	    %% error_handler either present in both or
	    %% absent in both.
	    diff_cmds(Commands1, Commands2);
	true ->
	    %% error_handler only present in one primLoad.
	    %% Not OK.
	    {diff, missing_error_handler,
	     {expected, Cmd1}, {actual, Cmd2}}
    end;
diff_cmds([Cmd | Commands1], [Cmd | Commands2]) ->
    diff_cmds(Commands1, Commands2);
diff_cmds([Cmd1 | _Commands1], [Cmd2 | _Commands2]) ->
    {diff, {expected, Cmd1}, {actual, Cmd2}};
diff_cmds([], []) ->
    equal.

diff_get_prim_load(Cmds) ->
    L = [Ms || {primLoad, Ms} <- Cmds],
    lists:sort(lists:flatten(L)).

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
            case string:split(Return, "$#", trailing) of
                [_] -> %% This happens only if the sh command pipe is somehow interrupted
                    {98, Return};
                [Result, Status0] ->
                    {list_to_integer(string:trim(Status0)), Result}
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
    start_node(Name, ErlPath, []).
start_node(Name, ErlPath, Args0) ->
    FullName = full_node_name(Name),
    Args = mk_node_args(Name, Args0),
    io:format("Starting node ~p: ~ts~n",
	      [FullName, lists:flatten([[X," "] || X <- [ErlPath|Args]])]),
    %io:format("open_port({spawn_executable, ~p}, [{args,~p}])~n",[ErlPath,Args]),
    case open_port({spawn_executable, ErlPath}, [{args,Args}]) of
        Port when is_port(Port) ->
	    %% no need to close port since node is detached (see
	    %% mk_node_args) so port will be closed anyway.
            case ping_node(FullName, 50) of
                ok -> {ok, FullName};
                Other -> exit({failed_to_start_node, FullName, Other})
            end;
        Error ->
            exit({failed_to_start_node, FullName, Error})
    end.

stop_node(Node) ->
    rpc:call(Node,erlang,halt,[]),
    wait_for_node_down(Node,50).

wait_for_node_down(Node,0) ->
    test_server:fail({cant_terminate_node,Node});
wait_for_node_down(Node,N) ->
    case net_adm:ping(Node) of
	pong ->
	    timer:sleep(1000),
	    wait_for_node_down(Node,N-1);
	pang ->
	    ok
    end.

mk_node_args(Name, Args) ->
    Pa = filename:dirname(code:which(?MODULE)),
    NameSw = case net_kernel:longnames() of
                 false -> "-sname";
                 true -> "-name";
                 _ -> exit(not_distributed_node)
             end,
    {ok, Pwd} = file:get_cwd(),
    NameStr = atom_to_list(Name),
    ["-detached",
     %% Don't want to try to run the debug emulator here
     "-emu_type","opt",
     NameSw, NameStr,
     "-pa", Pa,
     "-env", "ERL_CRASH_DUMP", Pwd ++ "/erl_crash_dump." ++ NameStr,
     "-setcookie", atom_to_list(erlang:get_cookie())
     | Args].

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

wait_for_app(_Node, Name, 0) ->
    {error, Name};
wait_for_app(Node, Name, N) when is_integer(N), N > 0 ->
    case rpc:call(Node,application,which_applications,[]) of
	{badrpc,Reason} ->
	    test_server:fail({failed_to_get_applications,Reason});
	Apps ->
	    case lists:member(Name,Apps) of
		false ->
		    timer:sleep(1000),
		    wait_for_app(Node, Name, N-1);
		true ->
		    ok
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Run escript

run(Dir, Script, Args) ->
    Cmd0 = filename:rootname(Script) ++ " " ++ Args,
    Cmd = case os:type() of
              {win32,_} -> filename:nativename(Dir) ++ "\\" ++ Cmd0;
              _ -> Cmd0
          end,
    do_run(Dir, Cmd).

run(Dir, Opts, Script, Args) ->
    Cmd0 = filename:rootname(Script) ++ " " ++ Args,
    Cmd = case os:type() of
              {win32,_} -> Opts ++ " " ++ filename:nativename(Dir) ++ "\\" ++ Cmd0;
              _ -> Opts ++ " " ++ Dir ++ "/" ++ Cmd0
          end,
    do_run(Dir, Cmd).

do_run(Dir, Cmd) ->
    io:format("Run: ~p\n", [Cmd]),
    Env = [{"PATH",Dir++":"++os:getenv("PATH")},
	   {"ERL_FLAGS",""},   % Make sure no flags are set that can override
	   {"ERL_ZFLAGS",""}], % any of the flags set in the escript.
    Port = open_port({spawn,Cmd}, [exit_status,eof,in,{env,Env}]),
    Res = get_data(Port, []),
    receive
        {Port,{exit_status,ExitCode}} ->
            s2b([Res,"ExitCode:"++integer_to_list(ExitCode)])
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

%% Convert the given list to a binary with the same encoding as the
%% file name translation mode
s2b(List) ->
    Enc = file:native_name_encoding(),
    unicode:characters_to_binary(List,Enc,Enc).
