%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2022. All Rights Reserved.
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
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2,
	 get_columns_and_rows/1, exit_initial/1, job_control_local/1,
	 job_control_remote/1,stop_during_init/1,wrap/1,
         shell_history/1, shell_history_resize/1, shell_history_eaccess/1,
         shell_history_repair/1, shell_history_repair_corrupt/1,
         shell_history_corrupt/1,
         shell_history_custom/1, shell_history_custom_errors/1,
	 job_control_remote_noshell/1,ctrl_keys/1,
         get_columns_and_rows_escript/1,
         remsh_basic/1, remsh_longnames/1, remsh_no_epmd/1]).

%% Exports for custom shell history module
-export([load/0, add/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() ->
    [get_columns_and_rows_escript,get_columns_and_rows,
     exit_initial, job_control_local,
     job_control_remote, job_control_remote_noshell,
     ctrl_keys, stop_during_init, wrap,
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
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM", "vt100"),
    case rtnode:get_default_shell() of
        noshell ->
            os:putenv("TERM",Term),
            {skip, "No run_erl"};
        DefShell ->
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
    %% Ensure that ERL_AFLAGS will not override the value of the shell_history variable.
    {ok, Peer, Node} = ?CT_PEER(["-noshell","-kernel","shell_history","not_overridden"]),
    try erpc:call(Node, application, get_env, [kernel, shell_history], rtnode:timeout(normal)) of
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
        peer:stop(Peer)
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
    rtnode:run(
      [{putline, ""},
       {putline, "2."},
       {expect, "2\r\n"},
       {putline, "io:columns()."},
       {expect, "{error,enotsup}\r\n"},
       {putline, "io:rows()."},
       {expect, "{error,enotsup}\r\n"}
      ], [], [], Args),

    rtnode:run(
      [{putline, ""},
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
    rtnode:run(
      [{putline, ""},
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

    rtnode:run(
      [{putline, ""},
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
    rtnode:run(
      [{putline, ""},
       {putline, "2."},
       {expect, "2\r\n"},
       {putline, "exit()."},
       {expect, "Eshell"},
       {putline, ""},
       {putline, "35."},
       {expect, "35\r\n"}],
      [], [], ["-oldshell"]);
test_exit_initial(new) ->
    rtnode:run(
      [{putline, ""},
       {expect, "1> $"},
       {putline, "2."},
       {expect, "2"},
       {putline,"exit()."},
       {expect, "Eshell"},
       {expect, "1> $"},
       {putline, "35."},
       {expect, "35\r\n"}]).

stop_during_init(Config) when is_list(Config) ->
    {RunErl,_ToErl,[Erl|ErlArgs]} = rtnode:get_progs(),
    case rtnode:create_tempdir() of
        {error, Reason} ->
            {skip, Reason};
        Tempdir ->
            XArg = " -kernel shell_history enabled -s init stop",
            rtnode:start_runerl_command(RunErl, Tempdir, "\\\""++Erl++"\\\""++ErlArgs++XArg),
            Logs = rtnode:read_logs(Tempdir),
            rtnode:dump_logs(Logs),
            nomatch = binary:match(map_get("erlang.log.1", Logs),
                                   <<"*** ERROR: Shell process terminated! ***">>),
            ok
    end.

%% This testcase tests that the correct wrapping characters are added
%% When a terminal has the xn flag set, it means that wrapping may not
%% work as expected and historically the ttysl driver has always inserted
%% a " \b" (i.e. space + backspace) when an output string ends on that line
%% in order for the cursor to be at col 0 on the next line instead of col max
%% on the current line.
%%
%% This caused problems when a string was `columns` long and then ended in "\r\n"
%% as it would first wrap due to " \b" and then output "\r\n" that cause a double
%% newline to happen.
%%
%% This testcase tests that we get a " \b" when we should and we get a "\r\n" when
%% we should.
wrap(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
        new ->
            As = lists:duplicate(20,"a"),
            rtnode:run(
              [{putline, "io:columns()."},
               {expect, "{ok,20}\r\n"},
               {putline, ["io:format(\"~s\",[lists:duplicate(20,\"a\")])."]},
               {expect, As ++ " \b"},
               {putline, ["io:format(\"~s~n~s\",[lists:duplicate(20,\"a\"),lists:duplicate(20,\"a\")])."]},
               {expect, As ++ "\r\n" ++ As ++ " \b"}
              ],
              [],
              "stty rows 40; stty columns 20; ");
        _ ->
            ok
    end,
    ok.

%% This testcase tests that shell_history works as it should.
%% We use Ctrl + P = Cp=[$\^p] in order to navigate up
%% We use Ctrl + N = Cp=[$\^n] in order to navigate down
%% We use Ctrl + B = Cp=[$\^b] in order to navigate left
%% in the console. We also need to sleep for a while in order
%% for the system to update the display before we enter more
%% commands.
shell_history(Config) when is_list(Config) ->
    Path = shell_history_path(Config, "basic"),
    rtnode:run(
      [{putline, "echo1."},
       {expect, "echo1\r\n"},
       {putline, "echo2."},
       {expect, "echo2\r\n"},
       {putline, "echo3."},
       {expect, "echo3\r\n"},
       {putline, "echo4."},
       {expect, "echo4\r\n"},
       {putline, "echo5."},
       {expect, "echo5\r\n"}
      ], [], [], mk_history_param(Path)),
    receive after 1000 -> ok end,
    rtnode:run(
      [{sleep,100},
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
      ], [], [],
      mk_history_param(Path)),
    ok.

shell_history_resize(Config) ->
    Path = shell_history_path(Config, "resize"),
    rtnode:run(
      [{putline, "echo."},
       {expect, "echo\r\n"},
       {putline, "echo2."},
       {expect, "echo2\r\n"}
      ], [], [], ["-kernel","shell_history_file_bytes","123456"] ++
          mk_history_param(Path)),

    {ok, Logs} =
        rtnode:run(
          [{sleep,100},
           {putline, ""},
           {putdata, [$\^p]}, {expect, "echo2[.]$$"},
           {putdata, [$\^p]}, {expect, "echo[.]$"},
           {putdata, [$\n]},
           {expect, "echo"}
          ], [], [], ["-kernel","shell_history_file_bytes","654321"] ++
              mk_history_param(Path)),

    rtnode:check_logs(
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
            rtnode:run(
              [{putline, "echo."},
               {expect, "echo\r\n"}
              ], [], [], mk_history_param(Path)),

        ct:pal("~p",[Logs1]),
        rtnode:check_logs("erlang.log.1", "Error handling file", Logs1),

        %% shell_docs recursively creates the folder to store the
        %% logs. This test checks that erlang still starts if we
        %% cannot create the folders to the path.
        {ok, Logs2} =
            rtnode:run(
              [{putline, "echo."},
               {expect, "echo\r\n"}
              ], [], [], mk_history_param(filename:join(Path,"logs"))),

        rtnode:check_logs("erlang.log.1", "Error handling file", Logs2)

    after
        file:write_file_info(Path, Info)
    end,
    ok.

shell_history_repair(Config) ->
    Path = shell_history_path(Config, "repair"),

    %% We stop a node without closing the log
    shell_history_halt(Path),

    {ok, Logs} =
        rtnode:run(
          [{putline, ""},
           {putdata, [$\^p]}, {expect, "echo[.]$"},
           {putdata, [$\n]},
           {expect, "echo\r\n"}
          ], [], [], mk_history_param(Path)),

    %% The regexp below checks that he string is NOT part of the log
    rtnode:check_logs("erlang.log.1",
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
        rtnode:run(
          [{putline, ""},
           {putdata, [$\^p]}, {expect, "echo[.]$"},
           {putdata, [$\n]},
           {expect, "echo\r\n"}
          ], [], [], mk_history_param(Path)),

    rtnode:check_logs("erlang.log.1",
                      "The shell history log file was corrupted and was repaired.",
                      Logs),
    ok.

shell_history_corrupt(Config) ->
    Path = shell_history_path(Config, "corrupt"),

    %% We initialize the shell history log with a known value.
    rtnode:run(
      [{putline, "echo."},
       {expect, "echo\r\n"},
       {putline, "echo2."},
       {expect, "echo2\r\n"}
      ], [], [], mk_history_param(Path)),

    %% We corrupt the disklog.
    {ok, D} = file:open(filename:join(Path,"erlang-shell-log.1"), [read, append]),
    ok = file:write(D, [10, 10]),
    ok = file:close(D),

    {ok, Logs} =
        rtnode:run(
          [{putline, ""},
           {putdata, [$\^p]}, {expect, "echo2[.]$"},
           {putdata, [$\^p]}, {expect, "echo[.]$"},
           {putdata, [$\n]},
           {expect, "echo\r\n"}
          ], [], [], mk_history_param(Path)),

    rtnode:check_logs("erlang.log.1", "Invalid chunk in the file", Logs),
    ok.

%% Stop the node without closing the log.
shell_history_halt(Path) ->
    try
        rtnode:run(
          [{putline, "echo."},
           {expect, "echo\r\n"},
           {sleep, 2500}, % disk_log internal cache timer is 2000 ms
           {putline, "halt(0)."},
           {expect, "\r\n"},
           {sleep, 1000} %% wait for node to terminate
          ], [], [], mk_history_param(Path))
    catch
        _:_ ->
            ok
    end.

shell_history_path(Config, TestCase) ->
    filename:join([proplists:get_value(priv_dir, Config),
                   "shell_history", TestCase]).

mk_history_param(Path) ->
    ["-kernel","shell_history","enabled",
     "-kernel","shell_history_path","\"" ++ Path ++ "\"",
     "-kernel","shell_history_drop","[\"init:stop().\"]"
    ].

shell_history_custom(_Config) ->
    %% Up key: Ctrl + P = Cp=[$\^p]
    rtnode:run(
      [{expect, "1> $"},
       %% {putline, ""},
       {putdata, [$\^p]}, {expect, "0[.]"},
       {putdata, [$\n]},
       {expect, "0\r\n"},
       {putline, "echo."},
       {expect, "!echo\r\n"} % exclamation mark is printed by custom history module
      ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                  "-pz",filename:dirname(code:which(?MODULE))]),
    ok.

shell_history_custom_errors(_Config) ->

    %% Check that we can start with a node with an undefined
    %% provider module.
    rtnode:run(
      [{putline, "echo."},
       {expect, "echo\r\n"}
      ], [], [], ["-kernel","shell_history","very_broken",
                  "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode:run(
      [{putline, "echo."},
       {expect, "echo\r\n"}
      ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                  "-kernel","provider_load","crash",
                  "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode:run(
      [{putline, "echo."},
       {expect, "echo\r\n"}
      ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                  "-kernel","provider_load","badreturn",
                  "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that crashes in load/0.
    rtnode:run(
      [{putline, "echo."},
       {expect, "(Disabling shell history logging.|echo)\r\n"},
       {expect, "(Disabling shell history logging.|echo)\r\n"}
      ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                  "-kernel","provider_add","crash",
                  "-pz",filename:dirname(code:which(?MODULE))]),

    %% Check that we can start with a node with a provider module
    %% that return incorrect in load/0.
    rtnode:run(
      [{putline, "echo."},
       {expect, "It returned {error,badreturn}.\r\n"},
       {expect, "echo\r\n"}
      ], [], [], ["-kernel","shell_history",atom_to_list(?MODULE),
                  "-kernel","provider_add","badreturn",
                  "-pz",filename:dirname(code:which(?MODULE))]),

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
	    rtnode:run(
              [{putline, ""},
               {expect,  "1> $"},
               {putline, "2."},
               {expect,  "\r\n2\r\n"},
               {putline, "\^g"},
               {expect,  "--> $"},
               {putline, "s"},
               {expect,  "--> $"},
               {putline, "c"},
               {expect,  "\r\nEshell"},
               {expect,  "1> $"},
               {putline, "35."},
               {expect,  "\r\n35\r\n"},
               {expect,  "2> $"},
               {putline, "receive M -> M end.\r\n"},
               {putline, "\^g"},
               {expect,  "--> $"},
               {putline, "i 3"},
               {expect,  "Unknown job"},
               {expect,  "--> $"},
               {putline, "i 2"},
               {expect,  "--> $"},
               {putline, "c"},
               {expect,  "[*][*] exception exit: killed"},
               {expect,  "[23]>"},
               {putline, "\^g"},
               {expect,  "--> $"},
               {putline, "k 3"},
               {expect,  "Unknown job"},
               {expect,  "--> $"},
               {putline, "k 2"},
               {expect,  "--> $"},
               {putline, "k"},
               {expect,  "Unknown job"},
               {expect,  "--> $"},
               {putline, "c"},
               {expect,  "Unknown job"},
               {expect,  "--> $"},
               {putline, "i"},
               {expect,  "Unknown job"},
               {expect,  "--> $"},
               {putline, "?"},
               {expect,  "this message"},
               {expect,  "--> $"},
               {putline, "h"},
               {expect,  "this message"},
               {expect,  "--> $"},
               {putline, "c 1"},
               {expect, "\r\n"},
               {putline, "35."},
               {expect, "\r\n35\r\n"},
               {expect, "[23]> $"}
              ]),
            ok
    end.

%% Tests that remote shell can be started by means of job control.
job_control_remote(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
            {ok, Peer, NSNode} = ?CT_PEER(#{ args => ["-connect_all","false"],
                                             peer_down => continue }),
            try
                test_remote_job_control(NSNode)
            after
                peer:stop(Peer)
            end
    end.

%% Tests that remote shell can be started by means of job control to
%% -noshell node.
job_control_remote_noshell(Config) when is_list(Config) ->
    case proplists:get_value(default_shell, Config) of
	old ->
	    {skip,"No new shell found"};
	_ ->
	    {ok, Peer, NSNode} = ?CT_PEER(#{ name => peer:random_name(test_remote_job_control),
                                             args => ["-connect_all","false","-noshell"],
                                             peer_down => continue }),
            try
                test_remote_job_control(NSNode)
            after
                peer:stop(Peer)
            end
    end.

test_remote_job_control(Node) ->
    RemNode = peer:random_name(test_remote_job_control),
    Pid = spawn_link(Node, fun() ->
                                   receive die ->
                                           ok
                                   end
                           end),
    PidStr = erpc:call(Node, erlang, pid_to_list, [Pid]),
    true = erpc:call(Node, erlang, register, [kalaskula,Pid]),
    PrintedNode = printed_atom(Node),
    CookieString = printed_atom(erlang:get_cookie()),

    rtnode:run(
      [{putline, ""},
       {putline, "erlang:get_cookie()."},
       {expect, "\r\n\\Q" ++ CookieString ++ "\\E"},
       {putdata, "\^g"},
       {expect, " --> $"},
       {putline, "r " ++ PrintedNode},
       {expect, "\r\n"},
       {putline, "j"},
       {expect, "1  {shell,start,\\[init]}"},
       {expect, "2[*] {\\Q"++PrintedNode++"\\E,shell,start,\\[]}"},
       {expect, " --> $"},
       {putline, "c"},
       {expect, "\r\n"},
       {expect, "Eshell"},
       {expect, "\\Q(" ++ atom_to_list(Node) ++")1> \\E$"},
       {putline, "whereis(kalaskula)."},
       {expect, PidStr},
       {putline, "kalaskula ! die."},
       {putline, "exit()."},
       {expect, "[*][*][*] Shell process terminated!"},
       {putdata, "\^g"},
       {expect, " --> $"},
       {putline, "j"},
       {expect, "1  {shell,start,\\[init]}"},
       {expect, " --> $"},
       {putline, "c"},
       {expect, "Unknown job"},
       {expect, " --> $"},
       {putline, "c 1"},
       {expect, "\r\n"},
       {putline, ""},
       {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[12]> $"},
       {putdata, "\^g"},
       {expect, " --> $"},
       {putline, "j"},
       {expect, "1[*] {shell,start,\\[init]}"},
       {putline, "c"},
       {expect, "\r\n"},
       {sleep, 100},
       {putline, "35."},
       {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[123]> $"}
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
    rtnode:run(
      [{putline,""},
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
          wordLeft() ++ wordRight()),
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
    {ok, Peer, TargetNode} = ?CT_PEER(),
    TargetNodeStr = printed_atom(TargetNode),
    [_Name,Host] = string:split(atom_to_list(node()), "@"),

    PreCmds = [{putline,""},
               {putline,"node()."},
               {expect, "\\Q" ++ TargetNodeStr ++ "\\E\r\n"}],

    PostCmds = quit_hosting_node(),

    %% Test that remsh works with explicit -sname.
    HostNode = atom_to_list(?FUNCTION_NAME) ++ "_host",
    HostNodeStr = printed_atom(list_to_atom(HostNode ++ "@" ++ Host)),
    rtnode:run(
      PreCmds ++
          [{putline,"nodes()."},
           {expect, "\\Q" ++ HostNodeStr ++ "\\E"}] ++
          PostCmds,
      HostNode, " ", "-remsh " ++ TargetNodeStr),

    %% Test that remsh works without -sname.
    rtnode:run(PreCmds ++ PostCmds, [], " ", "-remsh " ++ TargetNodeStr),

    peer:stop(Peer),

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
    case rtnode:start(" -name " ++ atom_to_list(?FUNCTION_NAME)++Domain) of
        {ok, _SRPid, STPid, SNode, SState} ->
            try
                {ok, _CRPid, CTPid, CNode, CState} =
                    rtnode:start("-name undefined" ++ Domain ++
                                     " -remsh " ++ atom_to_list(?FUNCTION_NAME)),
                try
                    ok = rtnode:send_commands(
                           SNode,
                           STPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
                    ok = rtnode:send_commands(
                           CNode,
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtnode:dump_logs(rtnode:stop(CState))
                end
            after
                rtnode:dump_logs(rtnode:stop(SState))
            end;
        Else ->
            Else
    end.

%% Test that -remsh works without epmd.
remsh_no_epmd(Config) when is_list(Config) ->
    EPMD_ARGS = "-start_epmd false -erl_epmd_port 12345 ",
    case rtnode:start([],"ERL_EPMD_PORT=12345 ",
                      EPMD_ARGS ++ " -sname " ++ atom_to_list(?FUNCTION_NAME)) of
        {ok, _SRPid, STPid, SNode, SState} ->
            try
                ok = rtnode:send_commands(
                       SNode,
                       STPid,
                       [{putline, ""},
                        {putline, "node()."},
                        {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"}], 1),
                {ok, _CRPid, CTPid, CNode, CState} =
                    rtnode:start([],"ERL_EPMD_PORT=12345 ",
                                 EPMD_ARGS ++ " -remsh "++atom_to_list(?FUNCTION_NAME)),
                try
                    ok = rtnode:send_commands(
                           CNode,
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ atom_to_list(?FUNCTION_NAME) ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtnode:stop(CState)
                end
            after
                rtnode:stop(SState)
            end;
        Else ->
            Else
    end.

printed_atom(A) ->
    lists:flatten(io_lib:format("~w", [A])).
