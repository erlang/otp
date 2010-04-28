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

-module(reltool_server_SUITE).

-export([all/0, init_per_suite/1, end_per_suite/1, 
         init_per_testcase/2, fin_per_testcase/2, end_per_testcase/2]).

-compile(export_all).

-include("reltool_test_lib.hrl").

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
fin_per_testcase(Func,Config) -> %% For test_server
    reltool_test_lib:end_per_testcase(Func,Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SUITE specification

all() ->
    all(suite).
all(suite) ->
    [
     start_server,
     set_config,
     create_release,
     create_script,
     create_target,
     create_embedded,
     create_standalone,
     create_old_target
    ].

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
    ?m(ok, reltool:create_target([{config, Config}], TargetDir)),
    
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
    ?m(ok, reltool:create_target([{config, Config}], TargetDir)),

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
    ?m(ok, reltool:create_target([{config, Config}], TargetDir)),

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
    ?m(ok, reltool:create_target([{config, Config}], TargetDir)),
    
    %% io:format("Will fail on Windows (should patch erl.ini)\n", []),
    ?m(ok, reltool:install(RelName2, TargetDir)),

    Erl = filename:join([TargetDir, "bin", "erl"]),
    {ok, Node} = ?msym({ok, _}, start_node(?NODE_NAME, Erl)),
    ?msym(ok, stop_node(Node)),
    
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Library functions

erl_libs() ->
    case os:getenv("ERL_LIBS") of
        false  -> [];
        LibStr -> string:tokens(LibStr, ":;")
    end.

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
