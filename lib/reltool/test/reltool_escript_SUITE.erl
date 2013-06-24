%% -*- coding: utf-8 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
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

-module(reltool_escript_SUITE).

-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2,
         init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include_lib("reltool/src/reltool.hrl").
-include("reltool_test_lib.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").

-define(NODE_NAME, '__RELTOOL__TEMPORARY_TEST__NODE__').
-define(WORK_DIR, "reltool_work_dir").
-define(TMP_IN_FILE, "tmp_reltool_infile").
-define(TMP_OUT_FILE, "tmp_reltool_outfile").

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
    {ok,All} = file:list_dir("."),
    Files = [F || F <- All, false =:= lists:prefix("save.",F)],
    case ?config(tc_status,Config) of
        ok ->
            ok;
        _Fail ->
            SaveDir = "save."++atom_to_list(Func),
            ok = rm_files([SaveDir]),
            ok = file:make_dir(SaveDir),
            save_test_result(Files,SaveDir)
    end,
    rm_files(Files),
    ok = file:set_cwd(?config(cwd,Config)),
    rm_files([?TMP_IN_FILE, ?TMP_OUT_FILE]),
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
            ok = file:delete(F);
        {error, enoent} ->
            ok
    end,
    rm_files(Fs);
rm_files([]) ->
    ok.

rm_dir(Dir) ->
    {ok,Files} = file:list_dir(Dir),
    rm_files([filename:join(Dir, F) || F <- Files]),
    ok = file:del_dir(Dir).

oscmd(CmdStr) ->
    io:format("os:cmd(~p)\n", [CmdStr]),
    Output0 = os:cmd(CmdStr ++ "; echo \";$?\""),
    Pred = fun(Char) -> Char =/= $; end,
    {ExitStatus, [$; | RevOutput]} =
        lists:splitwith(Pred, lists:reverse(Output0)),
    Output = lists:reverse(RevOutput),
    ExitStatus2 = string:strip(ExitStatus, left, $\n),
    {list_to_integer(ExitStatus2), Output}.

-define(w,'%.*%').
-define(f(Term), lists:flatten(io_lib:format("~p", [Term]))).

reltool_plain(Cmd) ->
    file:delete(?TMP_OUT_FILE),
    {ExitCode, Output} = oscmd(Cmd),
    file:write_file(?TMP_OUT_FILE, Output),
    case file:consult(?TMP_OUT_FILE) of
        {ok, Term} ->
            file:delete(?TMP_OUT_FILE),
            {ExitCode, {ok, Term}};
        {error, _Reason} ->
            file:delete(?TMP_OUT_FILE),
            {ExitCode, {error, Output}}
    end.

reltool(Cmd) ->
    Plain = reltool_plain(Cmd),
    File = reltool_file(Cmd),
    ?m(Plain, File).

reltool_file(Cmd) ->
    file:delete(?TMP_OUT_FILE),
    {ExitCode, Output} = oscmd(Cmd ++ " " ++ ?TMP_OUT_FILE),
    case file:consult(?TMP_OUT_FILE) of
        {ok, Term} ->
            file:delete(?TMP_OUT_FILE),
            {ExitCode, {ok, Term}};
        {error, _Reason} ->
            file:delete(?TMP_OUT_FILE),
            {ExitCode, {error, _Reason, Output}}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUITE specification

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [start_gui,
     get_config,
     create_release,
     create_script,
     create_target,
     eval_target_spec
    ].

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
%% Start the GUI

start_gui(_Config) ->
    ?m({1, {error, "reltool: noSUCHfile: no such file or directory\n"}},
       reltool_plain("reltool noSUCHfile --gui")),
    %% ?m({0,""}, oscmd("reltool --gui")),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check that get_config returns the expected derivates and defaults
%% as specified
get_config(_Config) ->
    KernVsn = latest(kernel),
    StdVsn = latest(stdlib),
    SaslVsn = latest(sasl),
    LibDir = code:lib_dir(),
    StdLibDir = filename:join(LibDir,"stdlib-"++StdVsn),
    KernLibDir = filename:join(LibDir,"kernel-"++KernVsn),
    SaslLibDir = filename:join(LibDir,"sasl-"++SaslVsn),

    ?m({0, {ok, [{sys,[]}]}},
       reltool("reltool --get_config")),

    Sys = {sys,[{incl_cond, exclude},
		{app,kernel,[{incl_cond,include}]},
		{app,sasl,[{incl_cond,include},{vsn,SaslVsn}]},
		{app,stdlib,[{incl_cond,include},{lib_dir,StdLibDir}]}]},
    ?m(ok, file:write_file(?TMP_IN_FILE, ?f(Sys) ++ ".")),

    ?m({0, {ok, [Sys]}},
       reltool("reltool " ++ ?TMP_IN_FILE ++ " --get_config")),

    %% Include derived info
    ?msym({0,
           {ok, [{sys,[{incl_cond, exclude},
                       {erts,[]},
                       {app,kernel,[{incl_cond,include},{mod,_,[]}|_]},
                       {app,sasl,[{incl_cond,include},{vsn,SaslVsn},{mod,_,[]}|_]},
                       {app,stdlib,[{incl_cond,include},{lib_dir,StdLibDir},
                                    {mod,_,[]}|_]}]}]}},
          reltool("reltool " ++ ?TMP_IN_FILE ++ " --get_config -derived")),

    %% Include defaults
    ?msym({0,
           {ok, [{sys,[{root_dir,_},
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
                       {debug_info,keep}]}]}},
          reltool("reltool " ++ ?TMP_IN_FILE ++ " --get_config -defaults")),

    %% Include both defaults and derived info
    ?msym({0,
           {ok, [{sys,[{root_dir,_},
                       {lib_dirs,_},
                       {mod_cond,all},
                       {incl_cond,exclude},
                       {erts,[]},
                       {app,kernel,[{incl_cond,include},{vsn,KernVsn},
                                    {lib_dir,KernLibDir},{mod,_,[]}|_]},
                       {app,sasl,[{incl_cond,include},{vsn,SaslVsn},
                                  {lib_dir,SaslLibDir},{mod,_,[]}|_]},
                       {app,stdlib,[{incl_cond,include},{vsn,StdVsn},
                                    {lib_dir,StdLibDir},{mod,_,[]}|_]},
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
                       {debug_info,keep}]}]}},
          reltool("reltool " ++ ?TMP_IN_FILE ++
                      " --get_config -derived -defaults")),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate releases

create_release(_Config) ->
    %% Configure the server
    RelName = "Testing",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [kernel, stdlib]}
         ]},
    ?m(ok, file:write_file(?TMP_IN_FILE, ?f(Sys) ++ ".")),

    %% Generate release
    ErtsVsn = erlang:system_info(version),
    Apps = application:loaded_applications(),
    {value, {_, _, KernelVsn}} = lists:keysearch(kernel, 1, Apps),
    {value, {_, _, StdlibVsn}} = lists:keysearch(stdlib, 1, Apps),
    Rel =
        {release, {RelName, RelVsn},
         {erts, ErtsVsn},
         [{kernel, KernelVsn}, {stdlib, StdlibVsn}]},
    ?m({0, {ok,[Rel]}},
       reltool("reltool " ++ ?TMP_IN_FILE ++
                   " --get_rel " ++ RelName)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate boot scripts

create_script(_Config) ->
    %% Configure the server
    RelName = "TestingMore",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {lib_dirs, []},
          {boot_rel, RelName},
          {rel, RelName, RelVsn, [stdlib, kernel]}
         ]},
    ?m(ok, file:write_file(?TMP_IN_FILE, ?f(Sys) ++ ".")),

    %% Generate release file
    ErtsVsn = erlang:system_info(version),
    Apps = application:loaded_applications(),
    {value, {_, _, KernelVsn}} = lists:keysearch(kernel, 1, Apps),
    {value, {_, _, StdlibVsn}} = lists:keysearch(stdlib, 1, Apps),
    Rel = {release,
           {RelName, RelVsn},
           {erts, ErtsVsn},
           [{kernel, KernelVsn}, {stdlib, StdlibVsn}]},
    ?m({0,{ok, [Rel]}},
       reltool("reltool " ++ ?TMP_IN_FILE ++
                   " --get_rel " ++ RelName)),

    ?m(ok, file:write_file(filename:join([?WORK_DIR, RelName ++ ".rel"]),
                           io_lib:format("~p.\n", [Rel]))),

    %% Generate script file
    {ok, Cwd} = file:get_cwd(),
    ?m(ok, file:set_cwd(?WORK_DIR)),
    ?m(ok, systools:make_script(RelName, [])),
    {ok, [OrigScript]} = ?msym({ok, [_]}, file:consult(RelName ++ ".script")),
    ?m(ok, file:set_cwd(Cwd)),

    {_, {ok, [Script]}} =
        ?msym({0, {ok, [_]}},
           reltool("reltool " ++ ?TMP_IN_FILE ++
                       " --get_script " ++ RelName)),

    %% OrigScript2 = sort_script(OrigScript),
    %% Script2 = sort_script(Script),
    %% ?m(OrigScript2, Script2),

    ?m(equal, diff_script(OrigScript, Script)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system

create_target(_Config) ->
    %% Configure the server
    RelName1 = "Testing1",
    RelName2 = "Testing2",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, []},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {app, sasl, [{incl_cond, include}]}
         ]},
    ?m(ok, file:write_file(?TMP_IN_FILE, ?f(Sys) ++ ".")),

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "target_development"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),

    ?log("SPEC: ~p\n",
         [?msym({0, {ok,[_]}},
                reltool_plain("reltool " ++ ?TMP_IN_FILE ++
                                  " --get_target_spec"))]),

    ?msym({0, {ok,[_]}},
          reltool_file("reltool " ++ ?TMP_IN_FILE ++ " --get_target_spec")),

    ?msym({0, {ok,[]}},
          reltool_plain("reltool " ++ ?TMP_IN_FILE ++
                            " --create_target " ++ TargetDir)),

    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),

    %% Verify that reltool is included
    Reltool = filename:join([BinDir, "reltool"]),
    ?m(Reltool, os:find_executable("reltool", BinDir)),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate target system with eval_target_spec/3

eval_target_spec(_Config) ->
    %% Configure the server
    RelName1 = "Testing3",
    RelName2 = "Testing4",
    RelVsn = "1.0",
    Sys =
        {sys,
         [
          {root_dir, code:root_dir()},
          {lib_dirs, []},
          {boot_rel, RelName2},
          {rel, RelName1, RelVsn, [stdlib, kernel]},
          {rel, RelName2, RelVsn, [sasl, stdlib, kernel]},
          {app, sasl, [{incl_cond, include}]}
         ]},
    ?m(ok, file:write_file(?TMP_IN_FILE, ?f(Sys) ++ ".")),

    %% Generate target file
    TargetDir = filename:join([?WORK_DIR, "eval_target_spec"]),
    ?m(ok, reltool_utils:recursive_delete(TargetDir)),
    ?m(ok, file:make_dir(TargetDir)),

    {_, {ok, Spec}} =
        ?msym({0, {ok,_}},
              reltool_plain("reltool " ++ ?TMP_IN_FILE ++
                                " --get_target_spec")),
    ?msym({0, {ok,_}},
              reltool_file("reltool " ++ ?TMP_IN_FILE ++ " --get_target_spec")),

    ?m(ok, file:write_file(?TMP_IN_FILE, ?f({spec, Spec}) ++ ".")),

    ?msym({0, {ok,""}},
          reltool_plain("reltool --eval_target_spec " ++ " " ++
                            ?TMP_IN_FILE ++ " " ++
                            code:root_dir() ++ " " ++
                            TargetDir)),

    BinDir = filename:join([TargetDir, "bin"]),
    Erl = filename:join([BinDir, "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),

    %% Verify that reltool is included
    Reltool = filename:join([BinDir, "reltool"]),
    ?m(Reltool, os:find_executable("reltool", BinDir)),

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Node handling

start_node(Name, ErlPath) ->
    start_node(Name, ErlPath, []).
start_node(Name, ErlPath, Args0) ->
    FullName = full_node_name(Name),
    Args = mk_node_args(Name, Args0),
    io:format("Starting node ~p: ~ts~n",
              [FullName, lists:flatten([[X," "] || X <- [ErlPath|Args]])]),
    %% io:format("open_port({spawn_executable, ~p},
    %% [{args,~p}])~n",[ErlPath,Args]),
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
