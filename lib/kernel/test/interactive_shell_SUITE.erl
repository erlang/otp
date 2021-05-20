%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2020. All Rights Reserved.
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
-module(interactive_shell_SUITE).
-include_lib("kernel/include/file.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
	 get_columns_and_rows/1, exit_initial/1, job_control_local/1,
	 job_control_remote/1,stop_during_init/1,
         shell_history/1, shell_history_resize/1, shell_history_eaccess/1,
         shell_history_repair/1, shell_history_repair_corrupt/1,
         shell_history_corrupt/1,
         shell_history_custom/1, shell_history_custom_errors/1,
	 job_control_remote_noshell/1,ctrl_keys/1,
         get_columns_and_rows_escript/1,
         remsh_basic/1, remsh_longnames/1, remsh_no_epmd/1]).

%% For spawn
-export([toerl_server/3]).
%% Exports for custom shell history module
-export([load/0, add/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() ->
    [get_columns_and_rows_escript,get_columns_and_rows,
     exit_initial, job_control_local,
     job_control_remote, job_control_remote_noshell,
     ctrl_keys, stop_during_init,
     {group, shell_history},
     {group, remsh}].

groups() ->
    [{shell_history, [],
      [shell_history,
       shell_history_resize,
       shell_history_eaccess,
       shell_history_repair,
       shell_history_repair_corrupt,
       shell_history_corrupt,
       {group, sh_custom}
      ]},
     {sh_custom, [],
      [shell_history_custom,
       shell_history_custom_errors]},
     {remsh, [],
      [remsh_basic,
       remsh_longnames,
       remsh_no_epmd]}
    ].

init_per_suite(Config) ->
    case get_progs() of
        {error, Error} ->
            {skip, Error};
        _ ->
            Term = os:getenv("TERM", "dumb"),
            os:putenv("TERM", "vt100"),
            DefShell = get_default_shell(),
            [{default_shell,DefShell},{term,Term}|Config]
    end.

end_per_suite(Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(remsh, Config) ->
    case proplists:get_value(default_shell, Config) of
        old -> {skip, "Not supported in old shell"};
        new -> Config
    end;
init_per_group(shell_history, Config) ->
    case proplists:get_value(default_shell, Config) of
        old -> {skip, "Not supported in old shell"};
        new -> Config
    end;
init_per_group(sh_custom, Config) ->
    %% Ensure that ERL_AFLAGS will not override the value of the
    %% shell_history variable.
    Name = interactive_shell_sh_custom,
    Args = "-noshell -kernel shell_history not_overridden",
    {ok, Node} = test_server:start_node(Name, slave, [{args,Args}]),
    try erpc:call(Node, application, get_env, [kernel, shell_history], timeout(normal)) of
        {ok, not_overridden} ->
            Config;
        _ ->
            SkipText = "shell_history variable is overridden (probably by ERL_AFLAGS)",
            {skip, SkipText}
    catch
        C:R:Stk ->
            io:format("~p\n~p\n~p\n", [C,R,Stk]),
            {skip, "Unexpected error"}
    after
        test_server:stop_node(Node)
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_Func, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    %% Terminate any connected nodes. They may disturb test cases that follow.
    lists:foreach(fun(Node) ->
                          catch erpc:call(Node, erlang, halt, [])
                  end, nodes()),
    ok.

%%-define(DEBUG,1).
-ifdef(DEBUG).
-define(dbg(Data),ct:pal("~p",[Data])).
-else.
-define(dbg(Data),noop).
-endif.

string_to_term(Str) ->
    {ok,Tokens,_EndLine} = erl_scan:string(Str ++ "."),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

run_unbuffer_escript(Rows, Columns, EScript, NoTermStdIn, NoTermStdOut) ->
    DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
    TmpFile = filename:join(DataDir, "tmp"),
    ok = file:write_file(TmpFile, <<>>),
    CommandModifier =
        case {NoTermStdIn, NoTermStdOut} of
            {false, false} -> "";
            {true, false} -> io_lib:format(" < ~s", [TmpFile]);
            {false, true} -> io_lib:format(" > ~s ; cat ~s", [TmpFile, TmpFile]);
            {true, true} -> io_lib:format(" > ~s < ~s ; cat ~s", [TmpFile, TmpFile, TmpFile])
        end,
    Command = io_lib:format("unbuffer -p bash -c \"stty rows ~p; stty columns ~p; escript ~s ~s\"",
                               [Rows, Columns, EScript, CommandModifier]),
    %% io:format("Command: ~s ~n", [Command]),
    Out = os:cmd(Command),
    %% io:format("Out: ~p ~n", [Out]),
    string_to_term(Out).

get_columns_and_rows_escript(Config) when is_list(Config) ->
    ExpectUnbufferInstalled =
        try
            "79" = string:trim(os:cmd("unbuffer -p bash -c \"stty columns 79 ; tput cols\"")),
            true
        catch
            _:_ -> false
        end,
    case ExpectUnbufferInstalled of
        false ->
            {skip,
             "The unbuffer tool (https://core.tcl-lang.org/expect/index) does not seem to be installed.~n"
             "On Ubuntu/Debian: \"sudo apt-get install expect\""};
        true ->
            DataDir = filename:join(filename:dirname(code:which(?MODULE)), "interactive_shell_SUITE_data"),
            IoColumnsErl = filename:join(DataDir, "io_columns.erl"),
            IoRowsErl = filename:join(DataDir, "io_rows.erl"),
            [
             begin
                 {ok, 42} = run_unbuffer_escript(99, 42, IoColumnsErl, NoTermStdIn, NoTermStdOut),
                 {ok, 99} = run_unbuffer_escript(99, 42, IoRowsErl, NoTermStdIn, NoTermStdOut)
             end
             ||
                {NoTermStdIn, NoTermStdOut} <- [{false, false}, {true, false}, {false, true}]
            ],
            {error,enotsup} = run_unbuffer_escript(99, 42, IoRowsErl, true, true),
            {error,enotsup} = run_unbuffer_escript(99, 42, IoColumnsErl, true, true),
            ok
    end.

%% Test that the shell can access columns and rows.
get_columns_and_rows(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
            test_columns_and_rows(old, []);
	new ->
            test_columns_and_rows(old, ["-oldshell"]),
            test_columns_and_rows(new, [])
    end,
    ok.

test_columns_and_rows(old, Args) ->
    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "io:columns()."},
            {expect, "{error,enotsup}\r\n"},
            {putline, "io:rows()."},
            {expect, "{error,enotsup}\r\n"}
           ], [], [], Args),

    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "io:columns()."},
            {expect, "{ok,90}\r\n"},
            {putline,"io:rows()."},
            {expect, "{ok,40}\r\n"}],
           [],
           "stty rows 40; stty columns 90; ",
           Args);
test_columns_and_rows(new, _Args) ->
    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect, "\r\n2\r\n"},
            {expect, "> $"},
            {putline, "io:columns()."},
            {expect, "{ok,80}\r\n"},
            {expect, "> $"},
            {putline, "io:rows()."},
            {expect, "\r\n{ok,24}\r\n"}
           ]),

    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect,  "\r\n2\r\n"},
            {expect, "> $"},
            {putline, "io:columns()."},
            {expect, "\r\n{ok,90}\r\n"},
            {expect, "> $"},
            {putline, "io:rows()."},
            {expect, "\r\n{ok,40}\r\n"}],
           [],
           "stty rows 40; stty columns 90; ").

%% Tests that exit of initial shell restarts shell.
exit_initial(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
            test_exit_initial(old);
        new ->
            test_exit_initial(old),
            test_exit_initial(new)
    end,
    ok.

test_exit_initial(old) ->
    rtnode([{putline, ""},
            {putline, "2."},
            {expect, "2\r\n"},
            {putline, "exit()."},
            {expect, "Eshell"},
            {putline, ""},
            {putline, "35."},
            {expect, "35\r\n"}],
           [], [], ["-oldshell"]);
test_exit_initial(new) ->
    rtnode([{putline, ""},
            {expect, "1> $"},
            {putline, "2."},
            {expect, "2"},
            {putline,"exit()."},
            {expect, "Eshell"},
            {expect, "1> $"},
            {putline, "35."},
            {expect, "35\r\n"}]).

stop_during_init(Config) when is_list(Config) ->
    {RunErl,_ToErl,Erl} = get_progs(),
    case create_tempdir() of
        {error, Reason} ->
            {skip, Reason};
        Tempdir ->
            XArg = " -kernel shell_history enabled -s init stop",
            start_runerl_command(RunErl, Tempdir, "\\\""++Erl++"\\\""++XArg),
            Logs = rtnode_read_logs(Tempdir),
            rtnode_dump_logs(Logs),
            nomatch = binary:match(map_get("erlang.log.1", Logs),
                                   <<"*** ERROR: Shell process terminated! ***">>),
            ok
    end.

%% This testcase tests that shell_history works as it should.
%% We use Ctrl + P = Cp=[$\^p] in order to navigate up
%% We use Ctrl + N = Cp=[$\^n] in order to navigate down
%% We use Ctrl + B = Cp=[$\^b] in order to navigate left
%% in the console. We also need to sleep for a while in order
%% for the system to update the display before we enter more
%% commands.
shell_history(Config) when is_list(Config) ->
    Path = shell_history_path(Config, "basic"),
    rtnode([
            {putline, "echo1."},
            {expect, "echo1\r\n"},
            {putline, "echo2."},
            {expect, "echo2\r\n"},
            {putline, "echo3."},
            {expect, "echo3\r\n"},
            {putline, "echo4."},
            {expect, "echo4\r\n"},
            {putline, "echo5."},
            {expect, "echo5\r\n"}
           ], [], [], " -kernel shell_history enabled " ++
               "-kernel shell_history_drop '[\\\"init:stop().\\\"]' " ++
               mk_sh_param(Path)),
    receive after 1000 -> ok end,
    rtnode([
            {putline, ""},
            %% the init:stop that stopped the node is dropped
            {putdata, [$\^p]}, {expect, "echo5[.]$"},
            {putdata, [$\n]},
            {expect, "echo5\r\n"},
            {putdata, [$\^p]}, {expect, "echo5[.]$"},
            {putdata, [$\^p]}, {expect, "echo4[.]$"},
            {putdata, [$\^p]}, {expect, "echo3[.]$"},
            {putdata, [$\^p]}, {expect, "echo2[.]$"},
            {putdata, [$\^n]}, {expect, "echo3[.]$"},
            {putdata, [$\^n]}, {expect, "echo4[.]$"},
            {putdata, [$\^b]}, {sleep,50}, %% the echo4. (cursor moved one left)
            {putline, ["ECHO"]},
            {expect, "echo4ECHO\r\n"}
           ], [], [], " -kernel shell_history enabled " ++ mk_sh_param(Path)),
    ok.

shell_history_resize(Config) ->
    Path = shell_history_path(Config, "resize"),
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history_file_bytes 123456 " ++
               "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "init:stop\\(\\)[.]$"},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo"}
               ], [], [], " -kernel shell_history_file_bytes 654321 " ++
                   "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    rtnode_check_logs(
      "erlang.log.1",
      "The configured log history file size is different from the size "
      "of the log file on disk", Logs),

    ok.

shell_history_eaccess(Config) ->
    Path = shell_history_path(Config, "eaccess"),
    file:make_dir(filename:dirname(Path)),
    ok = file:make_dir(Path),
    {ok, Info} = file:read_file_info(Path),
    try
        NoExecMode = Info#file_info.mode band (bnot 8#111),
        file:write_file_info(Path,Info#file_info{ mode = NoExecMode }),

        %% Cannot create history log in folder
        {ok, Logs1} =
            rtnode([
                    {putline, "echo."},
                    {expect, "echo\r\n"}
                   ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path)),

        rtnode_check_logs("erlang.log.1", "Error handling file", Logs1),

        %% shell_docs recursively creates the folder to store the
        %% logs. This test checks that erlang still starts if we
        %% cannot create the folders to the path.
        {ok, Logs2} =
            rtnode([
                    {putline, "echo."},
                    {expect, "echo\r\n"}
                   ], [], [], "-kernel shell_history enabled " ++
                       mk_sh_param(filename:join(Path,"logs"))),

        rtnode_check_logs("erlang.log.1", "Error handling file", Logs2)

    after
        file:write_file_info(Path, Info)
    end,
    ok.

shell_history_repair(Config) ->
    Path = shell_history_path(Config, "repair"),

    %% We stop a node without closing the log
    shell_history_halt(Path),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    %% The regexp below checks that he string is NOT part of the log
    rtnode_check_logs("erlang.log.1",
                      "The shell history log file was corrupted and was repaired",
                      false,
                      Logs),
    ok.

shell_history_repair_corrupt(Config) ->
    Path = shell_history_path(Config, "repair_corrupt"),

    %% We stop a node without closing the log
    shell_history_halt(Path),

    %% We corrupt the disklog
    {ok, D} = file:open(filename:join(Path,"erlang-shell-log.1"), [read,append]),
    ok = file:write(D, [10,10]),
    ok = file:close(D),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    rtnode_check_logs("erlang.log.1",
                      "The shell history log file was corrupted and was repaired.",
                      Logs),
    ok.

shell_history_corrupt(Config) ->
    Path = shell_history_path(Config, "corrupt"),

    %% We initialize the shell history log with a known value.
    rtnode([{putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    %% We corrupt the disklog.
    {ok, D} = file:open(filename:join(Path,"erlang-shell-log.1"), [read, append]),
    ok = file:write(D, [10, 10]),
    ok = file:close(D),

    {ok, Logs} =
        rtnode([
                {putline, ""},
                {putdata, [$\^p]}, {expect, "init:stop\\(\\)[.]$"},
                {putdata, [$\^p]}, {expect, "echo[.]$"},
                {putdata, [$\n]},
                {expect, "echo\r\n"}
               ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path)),

    rtnode_check_logs("erlang.log.1", "Invalid chunk in the file", Logs),
    ok.

%% Stop the node without closing the log.
shell_history_halt(Path) ->
    try
        rtnode([
                {putline, "echo."},
                {expect, "echo\r\n"},
                {sleep, 2500}, % disk_log internal cache timer is 2000 ms
                {putline, "halt(0)."}
               ], [], [], "-kernel shell_history enabled " ++ mk_sh_param(Path))
    catch
        _:_ ->
            ok
    end.

shell_history_path(Config, TestCase) ->
        filename:join([proplists:get_value(priv_dir, Config),
                       "shell_history", TestCase]).

mk_sh_param(Path) ->
    "-kernel shell_history_path '\\\""  ++ Path ++ "\\\"'".

shell_history_custom(_Config) ->
    %% Up key: Ctrl + P = Cp=[$\^p]
    rtnode([{expect, "1> $"},
            %% {putline, ""},
            {putdata, [$\^p]}, {expect, "0[.]"},
            {putdata, [$\n]},
            {expect, "0\r\n"},
            {putline, "echo."},
            {expect, "!echo\r\n"} % exclamation mark is printed by custom history module
           ], [], [], " -kernel shell_history " ++ atom_to_list(?MODULE) ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),
    ok.

shell_history_custom_errors(_Config) ->

    %% Check that we can start with a node with an undefined
    %% provider module.
    rtnode([{expect, "1> $"},
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history very_broken " ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history " ++ atom_to_list(?MODULE) ++
               " -kernel provider_load crash" ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history " ++ atom_to_list(?MODULE) ++
               " -kernel provider_load badreturn" ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "Disabling shell history logging.\r\n"},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history " ++ atom_to_list(?MODULE) ++
               " -kernel provider_add crash" ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode([
            {putline, "echo."},
            {expect, "It returned {error,badreturn}.\r\n"},
            {expect, "echo\r\n"}
           ], [], [], " -kernel shell_history " ++ atom_to_list(?MODULE) ++
               " -kernel provider_add badreturn" ++
               " -pz " ++ filename:dirname(code:which(?MODULE))),

    ok.

load() ->
    case application:get_env(kernel,provider_load) of
        {ok, crash} ->
            error(crash);
        {ok, badreturn} ->
            %% Should return a list of string()
            ok;
        _ ->
            ["0.\n\n"]
    end.

add(_Line) ->
    case application:get_env(kernel,provider_add) of
        {ok, crash} ->
            error(crash);
        {ok, badreturn} ->
            %% Should return ok
            {error, badreturn};
        _ ->
            io:format("!", [])
    end.

%% Tests that local shell can be started by means of job control.
job_control_local(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    %% Old shell tests
	    {skip,"No new shell found"};
	new ->
	    %% New shell tests
	    rtnode([{putline, ""},
                    {expect, "1> $"},
		    {putline, "2."},
		    {expect, "\r\n2\r\n"},
		    {putline, "\^g"},
                    {expect, ["--> $"]},
		    {putline, "s"},
                    {expect, ["--> $"]},
		    {putline, "c"},
                    {expect, ["\r\nEshell"]},
                    {expect, ["1> $"]},
		    {putline, "35."},
                    {expect, "\r\n35\r\n2> $"}],
                   []),
            ok
    end.

%% Tests that remote shell can be started by means of job control.
job_control_remote(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
	    NSNode = start_node(?FUNCTION_NAME, []),
            try
                test_remote_job_control(NSNode)
            after
                test_server:stop_node(NSNode)
            end
    end.

%% Tests that remote shell can be started by means of job control to
%% -noshell node.
job_control_remote_noshell(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
	    NSNode = start_node(?FUNCTION_NAME, ["-noshell"]),
            try
                test_remote_job_control(NSNode)
            after
                test_server:stop_node(NSNode)
            end
    end.

test_remote_job_control(Node) ->
    RemNode = create_nodename(),
    Pid = spawn_link(Node, fun() ->
                                   receive die ->
                                           ok
                                   end
                             end),
    PidStr = erpc:call(Node, erlang, pid_to_list, [Pid]),
    true = erpc:call(Node, erlang, register, [kalaskula,Pid]),
    PrintedNode = printed_atom(Node),
    CookieString = printed_atom(erlang:get_cookie()),

    rtnode([{putline, ""},
            {putline, "erlang:get_cookie()."},
            {expect, "\r\n\\Q" ++ CookieString ++ "\\E"},
            {putdata, "\^g"},
            {expect, " --> $"},
            {putline, "r " ++ PrintedNode},
            {expect, "\r\n"},
            {putline, "c"},
            {expect, "\r\n"},
            {expect, "Eshell"},
            {expect, "\\Q(" ++ atom_to_list(Node) ++")1> \\E$"},
            {putline, "whereis(kalaskula)."},
            {expect, PidStr},
            {putline, "exit()."},
            {expect, "[*][*][*] Shell process terminated!"},
            {putdata, "\^g"},
            {expect, " --> $"},
            {putline, "c 1"},
            {expect, "\r\n"},
            {putline, ""},
            {expect, "\\Q("++RemNode++")\\E[12]> $"}
           ], RemNode),
    Pid ! die,
    ok.

%% Tests various control keys.
ctrl_keys(_Config) ->
    Cu = [$\^u],
    Cw = [$\^w],
    Cy = [$\^y],
    Home = [27,$O,$H],
    End = [27,$O,$F],
    rtnode([{putline,""},
	    {putline,"2."},
	    {expect,"2"},
	    {putline,"\"hello "++Cw++"world\"."},	% test <CTRL>+W
	    {expect,"\"world\""},
	    {putline,"\"hello "++Cu++"\"world\"."},	% test <CTRL>+U
	    {expect,"\"world\""},
	    {putline,"world\"."++Home++"\"hello "},	% test <HOME>
	    {expect,"\"hello world\""},
	    {putline,"world"++Home++"\"hello "++End++"\"."},	% test <END>
	    {expect,"\"hello world\""},
	    {putline,"\"hello world\""++Cu++Cy++"."},
	    {expect,"\"hello world\""}] ++
               wordLeft() ++ wordRight(), []),
    ok.

wordLeft() ->
    L1 = "\e\e[D",
    L2 = "\e[5D",
    L3 = "\e[1;5D",
    wordLeft(L1) ++ wordLeft(L2) ++ wordLeft(L3).

wordLeft(Chars) ->
    End = "\eOF",
    [{putline,"\"world\""++Chars++"hello "++End++"."},
     {expect,"\"hello world\""}].

wordRight() ->
    R1 = "\e\e[C",
    R2 = "\e[5C",
    R3 = "\e[1;5C",
    wordRight(R1) ++ wordRight(R2) ++ wordRight(R3).

wordRight(Chars) ->
    Home = "\eOH",
    [{putline,"world"++Home++"\"hello "++Chars++"\"."},
     {expect,"\"hello world\""}].

%% Test that -remsh works
remsh_basic(Config) when is_list(Config) ->
    TargetNode = start_node(?FUNCTION_NAME, []),
    TargetNodeStr = printed_atom(TargetNode),
    [_Name,Host] = string:split(atom_to_list(node()), "@"),

    PreCmds = [{putline,""},
               {putline,"node()."},
               {expect, "\\Q" ++ TargetNodeStr ++ "\\E\r\n"}],

    PostCmds = quit_hosting_node(),

    %% Test that remsh works with explicit -sname.
    HostNode = atom_to_list(?FUNCTION_NAME) ++ "_host",
    HostNodeStr = printed_atom(list_to_atom(HostNode ++ "@" ++ Host)),
    rtnode(PreCmds ++
               [{putline,"nodes()."},
                {expect, "\\Q" ++ HostNodeStr ++ "\\E"}] ++
               PostCmds,
           HostNode, [], "-remsh " ++ TargetNodeStr),

    %% Test that remsh works without -sname.
    rtnode(PreCmds ++ PostCmds, [], [], " -remsh " ++ TargetNodeStr),

    test_server:stop_node(TargetNode),

    ok.

quit_hosting_node() ->
    %% Command sequence for entering a shell on the hosting node.
    [{putdata, "\^g"},
     {expect, "--> $"},
     {putline, "s"},
     {expect, "--> $"},
     {putline, "c"},
     {expect, ["Eshell"]},
     {expect, ["1> $"]}].

%% Test that -remsh works with long names.
remsh_longnames(Config) when is_list(Config) ->
    %% If we cannot resolve the domain, we need to add localhost to the longname
    Domain =
        case inet_db:res_option(domain) of
            [] ->
                "@127.0.0.1";
            _ -> ""
        end,
    case rtstart(" -name " ++ atom_to_list(?FUNCTION_NAME)++Domain) of
        {ok, _SRPid, STPid, SState} ->
            {ok, _CRPid, CTPid, CState} =
                rtstart("-name undefined" ++ Domain ++
                            " -remsh " ++ atom_to_list(?FUNCTION_NAME)),
            ok = send_commands(
                   STPid,
                   [{putline, ""},
                    {putline, "node()."},
                    {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
            try
                ok = send_commands(
                       CTPid,
                       [{putline, ""},
                        {putline, "node()."},
                        {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
            after
                rtstop(CState), %% Stop client before server
                rtstop(SState)
            end;
        Else ->
            Else
    end.

%% Test that -remsh works without epmd.
remsh_no_epmd(Config) when is_list(Config) ->
    EPMD_ARGS = "-start_epmd false -erl_epmd_port 12345 ",
    case rtstart([],"ERL_EPMD_PORT=12345 ",
                 EPMD_ARGS ++ " -sname " ++ atom_to_list(?FUNCTION_NAME)) of
        {ok, _SRPid, STPid, SState} ->
            try
                ok = send_commands(
                       STPid,
                       [{putline, ""},
                        {putline, "node()."},
                        {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
                {ok, _CRPid, CTPid, CState} =
                    rtstart([],"ERL_EPMD_PORT=12345 ",
                            EPMD_ARGS ++ " -remsh "++atom_to_list(?FUNCTION_NAME)),
                try
                    ok = send_commands(
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtstop(CState)
                end
            after
                rtstop(SState)
            end;
        Else ->
            Else
    end.

rtnode(C) ->
    rtnode(C, []).

rtnode(C, N) ->
    rtnode(C, N, []).

rtnode(Commands, Nodename, ErlPrefix) ->
    rtnode(Commands, Nodename, ErlPrefix, []).

rtnode(Commands, Nodename, ErlPrefix, Args) ->
    case rtstart(Nodename, ErlPrefix, Args) of
        {ok, _SPid, CPid, RTState} ->
            Res = catch send_commands(CPid, Commands, 1),
            Logs = rtstop(RTState),
            case Res of
                ok ->
                    rtnode_dump_logs(Logs),
                    ok;
                _ ->
                    rtnode_dump_logs(Logs),
                    ok = Res
            end,
            {ok, Logs};
        Skip ->
            Skip
    end.

rtstart(Args) ->
    rtstart([], [], Args).

rtstart(Nodename, ErlPrefix, Args) ->
    case get_progs() of
	{error,_Reason} ->
	    {skip,"No runerl present"};
	{RunErl,ToErl,Erl} ->
	    case create_tempdir() of
		{error, Reason2} ->
		    {skip, Reason2};
		Tempdir ->
		    SPid =
			start_runerl_node(RunErl,ErlPrefix++"\\\""++Erl++"\\\"",
					  Tempdir,Nodename,Args),
		    CPid = start_toerl_server(ToErl,Tempdir),
                    {ok, SPid, CPid, {CPid, SPid, ToErl, Tempdir}}
            end
    end.

rtstop({CPid, SPid, ToErl, Tempdir}) ->
    case stop_runerl_node(CPid) of
        {error,_} ->
            catch rtstop_try_harder(ToErl, Tempdir);
        _ ->
            ok
    end,
    wait_for_runerl_server(SPid),
    Logs = rtnode_read_logs(Tempdir),
    file:del_dir_r(Tempdir),
    Logs.

rtstop_try_harder(ToErl, Tempdir) ->
    CPid = start_toerl_server(ToErl, Tempdir),
    ok = send_commands(CPid,
                       [{putline,[7]},
                        {expect, " --> $"},
                        {putline, "s"},
                        {putline, "c"},
                        {putline, ""}], 1),
    stop_runerl_node(CPid).

timeout(longest) ->
    timeout(long) + timeout(normal);
timeout(long) ->
    2 * timeout(normal);
timeout(short) ->
    timeout(normal) div 10;
timeout(normal) ->
    10000 * test_server:timetrap_scale_factor().

start_node(Name, Args0) ->
    PaDir =  filename:dirname(code:which(?MODULE)),
    Args1 = ["-pa",PaDir|Args0],
    Args = lists:append(lists:join(" ", Args1)),
    {ok, Node} = test_server:start_node(Name, slave, [{args,Args}]),
    Node.

send_commands(CPid, [{sleep, X}|T], N) ->
    ?dbg({sleep, X}),
    receive
    after X ->
	    send_commands(CPid, T, N+1)
    end;
send_commands(CPid, [{expect, Expect}|T], N) when is_list(Expect) ->
    ?dbg(Exp),
    case command(CPid, {expect, [Expect], timeout(normal)}) of
        ok ->
            send_commands(CPid, T, N + 1);
        {expect_timeout, Got} ->
            ct:pal("expect timed out waiting for ~p\ngot: ~p\n", [Expect,Got]),
            {error, timeout};
        Other ->
            Other
    end;
send_commands(CPid, [{putline, Line}|T], N) ->
    send_commands(CPid, [{putdata, Line ++ "\n"}|T], N);
send_commands(CPid, [{putdata, Data}|T], N) ->
    ?dbg({putdata, Data}),
    case command(CPid, {send_data, Data}) of
        ok ->
	    send_commands(CPid, T, N+1);
        Error ->
            Error
    end;
send_commands(_CPid, [], _) ->
    ok.

command(Pid, Req) ->
    Timeout = timeout(longest),
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, Req},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, _, _, Reason} ->
            {error, Reason}
    after Timeout ->
            io:format("timeout while executing ~p\n", [Req]),
            {error, timeout}
    end.

wait_for_runerl_server(SPid) ->
    Ref = erlang:monitor(process, SPid),
    Timeout = timeout(long),
    receive
	{'DOWN', Ref, process, SPid, _Reason} ->
	    ok
    after Timeout ->
	    {error, runerl_server_timeout}
    end.

stop_runerl_node(CPid) ->
    Ref = erlang:monitor(process, CPid),
    CPid ! {self(), kill_emulator},
    Timeout = timeout(longest),
    receive
	{'DOWN', Ref, process, CPid, noproc} ->
	    ok;
	{'DOWN', Ref, process, CPid, normal} ->
	    ok;
	{'DOWN', Ref, process, CPid, {error, Reason}} ->
	    {error, Reason}
    after Timeout ->
	    {error, toerl_server_timeout}
    end.

get_progs() ->
    try
        do_get_progs()
    catch
        throw:Thrown ->
            {error, Thrown}
    end.

do_get_progs() ->
    case os:type() of
	{unix,freebsd} ->
	    throw("Can't use run_erl on FreeBSD");
	{unix,openbsd} ->
	    throw("Can't use run_erl on OpenBSD");
	{unix,_} ->
            RunErl = find_executable("run_erl"),
            ToErl = find_executable("to_erl"),
            Erl = find_executable("erl"),
            {RunErl, ToErl, Erl};
	_ ->
	    throw("Not a Unix OS")
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        Prog when is_list(Prog) ->
            Prog;
        false ->
            throw("Could not find " ++ Name)
    end.

create_tempdir() ->
    create_tempdir(filename:join(["/tmp","rtnode"++os:getpid()]),$A).

create_tempdir(Dir,X) when X > $Z, X < $a ->
    create_tempdir(Dir,$a);
create_tempdir(Dir,X) when X > $z -> 
    Estr = lists:flatten(
	     io_lib:format("Unable to create ~s, reason eexist",
			   [Dir++[$z]])),
    {error, Estr};
create_tempdir(Dir0, Ch) ->
    %% Expect fairly standard unix.
    Dir = Dir0++[Ch],
    case file:make_dir(Dir) of
	{error, eexist} ->
	    create_tempdir(Dir0, Ch+1);
	{error, Reason} ->
	    Estr = lists:flatten(
		     io_lib:format("Unable to create ~s, reason ~p",
				   [Dir,Reason])),
	    {error,Estr};
	ok ->
	    Dir
    end.

create_nodename() ->
    create_nodename($A).

create_nodename(X) when X > $Z, X < $a ->
    create_nodename($a);
create_nodename(X) when X > $z -> 
    {error,out_of_nodenames};
create_nodename(X) ->
    NN = "rtnode"++os:getpid()++[X],
    case file:read_file_info(filename:join(["/tmp",NN])) of
	{error,enoent} ->
	    Host = lists:nth(2,string:tokens(atom_to_list(node()),"@")),
	    NN++"@"++Host;
	_ ->
	    create_nodename(X+1)
    end.


start_runerl_node(RunErl,Erl,Tempdir,Nodename,Args) ->
    XArg = case Nodename of
	       [] ->
		   [];
	       _ ->
		   " -sname "++(if is_atom(Nodename) -> atom_to_list(Nodename);
				   true -> Nodename
				end)++
		       " -setcookie "++atom_to_list(erlang:get_cookie())
	   end ++ " " ++ Args,
    spawn(fun() -> start_runerl_command(RunErl, Tempdir, Erl++XArg) end).

start_runerl_command(RunErl, Tempdir, Cmd) ->
    FullCmd = "\""++RunErl++"\" "++Tempdir++"/ "++Tempdir++" \""++Cmd++"\"",
    ct:pal("~ts",[FullCmd]),
    os:cmd(FullCmd).

start_toerl_server(ToErl,Tempdir) ->
    Pid = spawn(?MODULE,toerl_server,[self(),ToErl,Tempdir]),
    receive
	{Pid,started} ->
	    Pid;
	{Pid,error,Reason} ->
	    {error,Reason}
    end.

try_to_erl(_Command, 0) ->
    {error, cannot_to_erl};
try_to_erl(Command, N) ->
    ?dbg({?LINE,N}),
    Port = open_port({spawn, Command},[eof]),
    Timeout = timeout(short) div 2,
    receive
	{Port, eof} ->
            timer:sleep(Timeout),
	    try_to_erl(Command, N-1)
    after Timeout ->
	    ?dbg(Port),
	    Port
    end.

toerl_server(Parent, ToErl, TempDir) ->
    Port = try_to_erl("\""++ToErl++"\" "++TempDir++"/ 2>/dev/null", 8),
    case Port of
	P when is_port(P) ->
	    Parent ! {self(),started};
	{error,Other} ->
	    Parent ! {self(),error,Other},
	    exit(Other)
    end,

    State = #{port => Port, acc => [], kill_emulator_command => init_stop},
    case toerl_loop(State) of
	normal ->
	    ok;
	{error, Reason} ->
	    error_logger:error_msg("toerl_server exit with reason ~p~n",
				   [Reason]),
	    exit(Reason)
    end.

toerl_loop(#{port := Port} = State0) ->
    ?dbg({toerl_loop, Port, map_get(acc, State0),
          maps:get(match, State0, nomatch)}),

    State = handle_expect(State0),

    receive
	{Port,{data,Data}} when is_port(Port) ->
	    ?dbg({?LINE,Port,{data,Data}}),
            toerl_loop(State#{acc => map_get(acc, State) ++ Data});
        {Pid, Ref, {expect, Expect, Timeout}} ->
            toerl_loop(init_expect(Pid, Ref, Expect, Timeout, State));
        {Pid, Ref, {send_data, Data}} ->
            Port ! {self(), {command, Data}},
	    Pid ! {Ref, ok},
	    toerl_loop(State);
	{_Pid, kill_emulator} ->
            kill_emulator(State);
        {timeout,Timer,expect_timeout} ->
            toerl_loop(handle_expect_timeout(Timer, State));
	{Port, eof} ->
	    {error, unexpected_eof};
	Other ->
	    {error, {unexpected, Other}}
    end.

kill_emulator(#{port := Port}) ->
    %% If the line happens to end in a ".", issuing "init:stop()."
    %% will result in a syntax error.  To avoid that, issue a "\n"
    %% before "init:stop().".
    Port ! {self(),{command, "\ninit:stop().\n"}},
    wait_for_eof(Port).

wait_for_eof(Port) ->
    receive
        {Port,eof} ->
            normal;
        _Other ->
            wait_for_eof(Port)
    after
        timeout(long) ->
            {error, kill_timeout}
    end.

init_expect(Pid, Ref, ExpectList, Timeout, State) ->
    try compile_expect(ExpectList) of
        Expect ->
            Exp = #{expect => Expect,
                    ref => Ref,
                    source => ExpectList,
                    timer => erlang:start_timer(Timeout, self(), expect_timeout),
                    from => Pid},
            State#{expect => Exp}
    catch
        Class:Reason:Stk ->
            io:put_chars("Compilation of expect pattern failed:"),
            io:format("~p\n", [ExpectList]),
            io:put_chars(erl_error:format_exception(Class, Reason, Stk)),
            exit(expect_pattern_error)
    end.

handle_expect(#{acc := Acc, expect := Exp} = State) ->
    #{expect := Expect, from := Pid, ref := Ref} = Exp,
    case Expect(Acc) of
        nomatch ->
            State;
        {matched, Eaten, Result} ->
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end;
handle_expect(State) ->
    State.

handle_expect_timeout(Timer, State) ->
    #{acc := Acc, expect := Exp} = State,
    #{expect := Expect, timer := Timer, from := Pid, ref := Ref} = Exp,
    case Expect({timeout, Acc}) of
        nomatch ->
            Result = {expect_timeout, Acc},
            Pid ! {Ref, Result},
            finish_expect(0, State);
        {matched, Eaten, Result} ->
            Pid ! {Ref, Result},
            finish_expect(Eaten, State)
    end.

finish_expect(Eaten, #{acc := Acc0,
                       expect := #{timer := Timer}}=State) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout,Timer,timeout} ->
            ok
    after 0 ->
            ok
    end,
    Acc = lists:nthtail(Eaten, Acc0),
    maps:remove(expect, State#{acc := Acc}).

compile_expect([{timeout,Action}|T]) when is_function(Action, 1) ->
    Next = compile_expect(T),
    fun({timeout, _}=Tm) ->
            {matched, 0, Action(Tm)};
       (Subject) ->
            Next(Subject)
    end;
compile_expect([{{re,RE0},Action}|T]) when is_binary(RE0), is_function(Action, 1) ->
    {ok, RE} = re:compile(RE0),
    Next = compile_expect(T),
    fun({timeout, _}=Subject) ->
            Next(Subject);
       (Subject) ->
            case re:run(Subject, RE, [{capture,first,index}]) of
                nomatch ->
                    Next(Subject);
                {match, [{Pos,Len}]} ->
                    Matched = binary:part(list_to_binary(Subject), Pos, Len),
                    {matched, Pos+Len, Action(Matched)}
            end
    end;
compile_expect([RE|T]) when is_list(RE) ->
    Ok = fun(_) -> ok end,
    compile_expect([{{re,list_to_binary(RE)},Ok}|T]);
compile_expect([]) ->
    fun(_) ->
            nomatch
    end.

rtnode_check_logs(Logname, Pattern, Logs) ->
rtnode_check_logs(Logname, Pattern, true, Logs).
rtnode_check_logs(Logname, Pattern, Match, Logs) ->
        case re:run(maps:get(Logname, Logs), Pattern) of
            {match, [_]} when Match ->
                ok;
            nomatch when not Match ->
                ok;
            _ ->
                rtnode_dump_logs(Logs),
                ct:fail("~p not found in log ~ts",[Pattern, Logname])
        end.

rtnode_dump_logs(Logs) ->
    maps:foreach(
      fun(File, Data) ->
              ct:pal("~ts: ~ts",[File, Data])
      end, Logs).

rtnode_read_logs(Tempdir) ->
    {ok, LogFiles0} = file:list_dir(Tempdir),

    %% Make sure that we only read log files and not any named pipes.
    LogFiles = [F || F <- LogFiles0,
                     case F of
                         "erlang.log" ++ _ -> true;
                         _ -> false
                     end],

    lists:foldl(
      fun(File, Acc) ->
              case file:read_file(filename:join(Tempdir, File)) of
                  {ok, Data} ->
                      Acc#{ File => Data };
                  _ ->
                      Acc
              end
      end, #{}, LogFiles).

get_default_shell() ->
    try
        rtnode([{putline,""},
                {putline, "is_pid(whereis(user_drv))."},
                {expect, "true\r\n"}], []),
        new
    catch _E:_R ->
            ?dbg({_E,_R}),
            old
    end.

printed_atom(A) ->
    lists:flatten(io_lib:format("~w", [A])).
