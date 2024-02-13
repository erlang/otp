%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2007-2024. All Rights Reserved.
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

%% Things to add tests for:
%%  - TERM=dumb
%%  - Editing line > MAXSIZE (1 << 16)
%%  - \t tests (use io:format("\t"))
%%  - xn fix after Delete and Backspace
%%  - octal_to_hex > 255 length (is this possible?)
%% 1222           0 :         } else if (lastput == 0) { /* A multibyte UTF8 character */
%% 1223           0 :             for (i = 0; i < ubytes; ++i) {
%% 1224           0 :                 outc(ubuf[i]);
%% 1225             :             }
%% 1226             :         } else {
%% 1227           0 :             outc(lastput);
%%  - $TERM set to > 1024 long value

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
         shell_navigation/1, shell_multiline_navigation/1, shell_multiline_prompt/1,
         shell_xnfix/1, shell_delete/1,
         shell_transpose/1, shell_search/1, shell_insert/1,
         shell_update_window/1, shell_small_window_multiline_navigation/1, shell_huge_input/1,
         shell_invalid_unicode/1, shell_support_ansi_input/1,
         shell_invalid_ansi/1, shell_suspend/1, shell_full_queue/1,
         shell_unicode_wrap/1, shell_delete_unicode_wrap/1,
         shell_delete_unicode_not_at_cursor_wrap/1,
         shell_expand_location_above/1,
         shell_expand_location_below/1,
         shell_update_window_unicode_wrap/1,
         shell_receive_standard_out/1,
         shell_standard_error_nlcr/1, shell_clear/1,
         shell_format/1,
         remsh_basic/1, remsh_error/1, remsh_longnames/1, remsh_no_epmd/1,
         remsh_expand_compatibility_25/1, remsh_expand_compatibility_later_version/1,
         external_editor/1, external_editor_visual/1,
         external_editor_unicode/1, shell_ignore_pager_commands/1]).

-export([test_invalid_keymap/1, test_valid_keymap/1]).
%% Exports for custom shell history module
-export([load/0, add/1]).
%% For custom prompt testing
-export([prompt/1]).
-record(tmux, {peer, node, name, orig_location }).
suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,3}}].

all() ->
    [{group, to_erl},
     {group, tty}].

groups() ->
    [{to_erl,[],
      [get_columns_and_rows_escript,get_columns_and_rows,
       exit_initial, job_control_local,
       job_control_remote, job_control_remote_noshell,
       ctrl_keys, stop_during_init, wrap,
       shell_invalid_ansi,
       {group, shell_history},
       {group, remsh}]},
     {shell_history, [],
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
       remsh_error,
       remsh_longnames,
       remsh_no_epmd,
       remsh_expand_compatibility_25,
       remsh_expand_compatibility_later_version]},
     {tty,[],
      [{group,tty_unicode},
       {group,tty_latin1},
       test_invalid_keymap, test_valid_keymap,
       shell_suspend,
       shell_full_queue,
       external_editor,
       external_editor_visual,
       shell_ignore_pager_commands
      ]},
     {tty_unicode,[parallel],
      [{group,tty_tests},
       shell_invalid_unicode,
       external_editor_unicode
       %% unicode wrapping does not work right yet
       %% shell_unicode_wrap,
       %% shell_delete_unicode_wrap,
       %% shell_delete_unicode_not_at_cursor_wrap,
       %% shell_update_window_unicode_wrap
      ]},
     {tty_latin1,[],[{group,tty_tests}]},
     {tty_tests, [parallel],
      [shell_navigation, shell_multiline_navigation, shell_multiline_prompt,
       shell_xnfix, shell_delete, shell_format,
       shell_transpose, shell_search, shell_insert,
       shell_update_window, shell_small_window_multiline_navigation, shell_huge_input,
       shell_support_ansi_input,
       shell_receive_standard_out,
       shell_standard_error_nlcr,
       shell_expand_location_above,
       shell_expand_location_below,
       shell_clear]}
    ].

init_per_suite(Config) ->
    Term = os:getenv("TERM", "dumb"),
    os:putenv("TERM", "vt100"),
    [{term,Term}|Config].

end_per_suite(Config) ->
    Term = proplists:get_value(term,Config),
    os:putenv("TERM",Term),
    ok.

init_per_group(to_erl, Config) ->
    case rtnode:get_progs() of
        {error, Error} ->
            {skip, Error};
        _ ->
            DefShell = rtnode:get_default_shell(),
            [{default_shell,DefShell}|Config]
    end;
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
init_per_group(tty, Config) ->
    case string:split(tmux("-V")," ") of
        ["tmux",[Num,$.|_]] when Num >= $3, Num =< $9 ->
            tmux("kill-session"),
            "" = tmux("-u new-session -x 50 -y 60 -d"),
            ["" = tmux(["set-environment '",Name,"' '",Value,"'"])
             || {Name,Value} <- os:env()],
            Config;
        ["tmux", Vsn] ->
            {skip, "invalid tmux version " ++ Vsn ++ ". Need vsn 3 or later"};
        Error ->
            {skip, "tmux not installed " ++ Error}
    end;
init_per_group(Group, Config) when Group =:= tty_unicode;
                                   Group =:= tty_latin1 ->
    [Lang,_] =
        string:split(
          os:getenv("LC_ALL",
                    os:getenv("LC_CTYPE",
                              os:getenv("LANG","en_US.UTF-8"))),"."),
    case Group of
        tty_unicode ->
            [{encoding, unicode},{env,[{"LC_ALL",Lang++".UTF-8"}]}|Config];
        tty_latin1 ->
            % [{encoding, latin1},{env,[{"LC_ALL",Lang++".ISO-8859-1"}]}|Config],
            {skip, "latin1 tests not implemented yet"}
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

end_per_group(tty, _Config) ->
    Windows = string:split(tmux("list-windows"), "\n", all),
    lists:foreach(
      fun(W) ->
              case string:split(W, " ", all) of
                  ["0:" | _] -> ok;
                  [No, _Name | _] ->
                      "" = os:cmd(["tmux select-window -t ", string:split(No,":")]),
                      ct:log("~ts~n~ts",[W, os:cmd(lists:concat(["tmux capture-pane -p -e"]))])
              end
      end, Windows),
%    "" = os:cmd("tmux kill-session")
    ok;
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Func, Config) ->
    Path = [Func,
            [proplists:get_value(name,P) ||
                P <- [proplists:get_value(tc_group_properties,Config,[])] ++
                    proplists:get_value(tc_group_path,Config,[])]],
    [{tc_path, lists:concat(lists:join("-",lists:flatten(Path)))} | Config].

end_per_testcase(_Case, Config) ->
    GroupProperties = proplists:get_value(tc_group_properties, Config),
    case (GroupProperties =/= false) andalso proplists:get_value(parallel, GroupProperties, false) of
        true -> ok;
        false ->
            %% Terminate any connected nodes. They may disturb test cases that follow.
            lists:foreach(fun(Node) ->
                                  catch erpc:call(Node, erlang, halt, [])
                          end, nodes()),
            ok
    end.

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

shell_navigation(Config) ->

    Term = start_tty(Config),

    try
        [begin
             send_tty(Term,"{aaa,'b"++U++"b',ccc}"),
             check_location(Term, {0, 0}), %% Check that cursor jump backward
             check_content(Term, "{aaa,'b"++U++"b',ccc}$"),
             timer:sleep(1000), %% Wait for cursor to jump back
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"Home"),
             check_location(Term, {0, 0}),
             send_tty(Term,"End"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"Left"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,")}),
             send_tty(Term,"C-Right"),
             check_location(Term, {0, width("{aaa,'b"++U++"b'")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{aaa,")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, width("{")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, 0}),
             send_tty(Term,"C-E"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc}")}),
             send_tty(Term,"C-H"),
             check_location(Term, {0, width("{aaa,'b"++U++"b',ccc")}),
             send_tty(Term,"C-A"),
             check_location(Term, {0, 0}),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()],
        ok
    after
        stop_tty(Term)
    end.
shell_multiline_navigation(Config) ->
    Term = start_tty(Config),

    try
        [begin
             check_location(Term, {0, 0}),
             send_tty(Term,"{aaa,"),
             check_location(Term, {0,width("{aaa,")}),
             send_tty(Term,"\n'b"++U++"b',"),
             check_location(Term, {0, width("'b"++U++"b',")}),
             send_tty(Term,"\nccc}"),
             check_location(Term, {-2, 0}), %% Check that cursor jump backward (blink)
             timer:sleep(1000), %% Wait for cursor to jump back
             check_location(Term, {0, width("ccc}")}),
             send_tty(Term,"Home"),
             check_location(Term, {0, 0}),
             send_tty(Term,"End"),
             check_location(Term, {0, width("ccc}")}),
             send_tty(Term,"Left"),
             check_location(Term, {0, width("ccc")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {0, 0}),
             send_tty(Term,"C-Left"),
             check_location(Term, {-1, width("'b"++U++"b',")}),
             send_tty(Term,"C-Left"),
             check_location(Term, {-1, 0}),
             %send_tty(Term,"C-Left"),
             %check_location(Term, {-1, 0}),
             %send_tty(Term,"C-Right"),
             %check_location(Term, {-1, 1}),
             send_tty(Term,"C-Right"),
             check_location(Term, {-1, width("'b"++U++"b'")}),
             send_tty(Term,"C-Up"),
             check_location(Term, {-2, width("{aaa,")}),
             send_tty(Term,"C-Down"),
             send_tty(Term,"C-Down"),
             check_location(Term, {0, width("ccc}")}),
             send_tty(Term,"Left"),
             send_tty(Term,"C-Up"),
             check_location(Term, {-1, width("'b"++U)}),
             send_tty(Term,"M-<"),
             check_location(Term, {-2, 0}),
             send_tty(Term,"M->"),
             send_tty(Term,"Left"),
             check_location(Term, {0,width("ccc")}),
             send_tty(Term,"Enter"),
             send_tty(Term,"Right"),
             check_location(Term, {0,0}),
             send_tty(Term,"C-h"), % Backspace
             check_location(Term, {-1,width("ccc}")}),
             send_tty(Term, "C-Up"),
             send_tty(Term, "End"),
             send_tty(Term,"Left"),
             send_tty(Term,"M-Enter"),
             check_content(Term, ".. ,"),
             check_location(Term, {-1,0}),
             send_tty(Term,"M-c"),
             check_location(Term, {-3,0}),
             send_tty(Term,"{'"++U++"',\n\n\nworks}.\n")
            end || U <- hard_unicode()],
        ok
    after
        stop_tty(Term)
    end.

shell_format(Config) ->
    Term1 = start_tty([{args,["-stdlib","format_shell_func","{shell,erl_pp_format_func}"]}|Config]),
    DataDir = proplists:get_value(data_dir, Config),
    EmacsFormat = "\""++DataDir ++ "emacs-format-file\"\n",
    try
        send_tty(Term1,"fun(X) ->\n  X\nend.\n"),
        send_tty(Term1,"Up"),
        %% Note, erl_pp puts 7 spaces before X
        check_content(Term1, "fun\\(X\\) ->\\s*..        X\\s*.. end."),
        send_tty(Term1, "Down"),
        tmux(["resize-window -t ",tty_name(Term1)," -x ",200]),
        timer:sleep(1000),
        send_stdin(Term1, "shell:format_shell_func(\"emacs -batch \${file} -l \"\n"),
        send_stdin(Term1, EmacsFormat),
        send_stdin(Term1, "\" -f emacs-format-function\").\n"),
        check_content(Term1, "{shell,erl_pp_format_func}"),
        send_tty(Term1, "Up"),
        send_tty(Term1, "Up"),
        send_tty(Term1, "\n"),
        timer:sleep(1000),
        send_tty(Term1, "Up"),
        %% Note, emacs-format puts 8 spaces before X
        check_content(Term1, "fun\\(X\\) ->\\s*..         X\\s*.. end."),
        send_tty(Term1, "Down"),
        send_stdin(Term1, "shell:format_shell_func({bad,format}).\n"),
        send_tty(Term1, "Up"),
        send_tty(Term1, "Up"),
        send_tty(Term1, "\n"),
        timer:sleep(1000),
        check_content(Term1, "\\Q* Bad format function: {bad,format}\\E"),
        send_tty(Term1, "Up"),
        %% No modifications should be made, when default format function is used
        check_content(Term1, "fun\\(X\\) ->\\s*..         X\\s*.. end."),
        ok
    after
        stop_tty(Term1)
    end.

shell_multiline_prompt(Config) ->
    Term1 = start_tty([{args,["-stdlib","shell_multiline_prompt","{shell,inverted_space_prompt}"]}|Config]),
    Term2 = start_tty([{args,["-stdlib","shell_multiline_prompt","\"...> \""]}|Config]),
    Term3 = start_tty([{args,["-stdlib","shell_multiline_prompt","edlin"]}|Config]),
    Term4 = start_tty(Config),
    try
        check_location(Term1, {0, 0}),
        send_tty(Term1,"\na"),
        check_location(Term1, {0, 1}),
        check_content(Term1, "   a"),
        ok
    after
        stop_tty(Term1)
    end,
    try
        check_location(Term2, {0, 0}),
        send_tty(Term2,"\na"),
        check_location(Term2, {0, 1}),
        check_content(Term2, "...> a"),
        ok
    after
        stop_tty(Term2)
    end,
    try
        send_tty(Term3,"\na"),
        check_location(Term3, {0, 1}),
        check_content(Term3, ".. a"),
        ok
    after
        stop_tty(Term3)
    end,
    try
        send_tty(Term4, "shell:multiline_prompt_func(\"-> \").\n"),
        check_content(Term4, "default"),
        send_tty(Term4, "a\nb"),
        check_content(Term4, "-> b"),
        ok
    after
        stop_tty(Term4)
    end.

shell_clear(Config) ->

    Term = start_tty(Config),
    {Rows, _Cols} = get_window_size(Term),

    try
        send_tty(Term,"foobar."),
        send_tty(Term,"Enter"),
        check_content(Term, "foobar"),
        check_location(Term, {0, 0}),
        send_tty(Term,"bazbat"),
        check_location(Term, {0, 6}),
        send_tty(Term,"C-L"),
        check_location(Term, {-Rows+1, 6}),
        check_content(Term, "bazbat")
    after
        stop_tty(Term)
    end.

shell_xnfix(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    As = lists:duplicate(Cols - Col - 1,"a"),

    try
        [begin
             check_location(Term, {0, 0}),
             send_tty(Term,As),
             check_content(Term,[As,$$]),
             check_location(Term, {0, Cols - Col - 1}),
             send_tty(Term,"a"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"aaa"),
             check_location(Term, {0, -Col + 3}),
             [send_tty(Term,"Left") || _ <- lists:seq(1,3 + width(U))],
             send_tty(Term,U),
             %% a{Cols-1}U\naaaaa
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U),$a),
                                 U,"\n",lists:duplicate(3+width(U), $a),"$"]),
             check_location(Term, {0, -Col}),
             send_tty(Term,"Left"),
             send_tty(Term,U),
             %% a{Cols-1}U\nUaaaaa
             check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U),$a),
                                 U,"\n",U,lists:duplicate(3+width(U), $a),"$"]),
             check_location(Term, {0, -Col}),
             %% send_tty(Term,"Left"),
             %% send_tty(Term,"BSpace"),
             %% a{Cols-2}U\nUaaaaa
             %% check_content(Term,[lists:duplicate(Cols - Col - 2 - width(U),$a),
             %%                     U,"\n",U,lists:duplicate(3+width(U), $a),"$"]),
             %% send_tty(Term,"BSpace"),
             %% check_content(Term,[lists:duplicate(Cols - Col - 1 - width(U),$a),
             %%                     U,U,"\n",lists:duplicate(3+width(U), $a),"$"]),
             %% send_tty(Term,"aa"),
             %% check_content(Term,[lists:duplicate(Cols - Col - 2 - width(U),$a),
             %%                     U,"a\n",U,lists:duplicate(3+width(U), $a),"$"]),
             %% check_location(Term, {0, -Col}),
             send_tty(Term,"C-K"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"C-A"),
             check_location(Term, {-1, 0}),
             send_tty(Term,"C-E"),
             check_location(Term, {0, -Col}),
             send_tty(Term,"Enter"),
             ok
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.


%% Characters that are larger than 2 wide need special handling when they
%% are at the end of the current line.
shell_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             FirstLine = [U,lists:duplicate(Cols - Col - width(U)*2 + 1,"a")],
             OtherLineA = [U,lists:duplicate(Cols - width(U) * 2+1,"a")],
             OtherLineB = [U,lists:duplicate(Cols - width(U) * 2+1,"b")],
             OtherLineC = [U,lists:duplicate(Cols - width(U) * 2+1,"c")],
             OtherLineD = [U,lists:duplicate(Cols - width(U) * 2+1,"d")],
             send_tty(Term,FirstLine),
             check_content(Term, [FirstLine,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineA),
             check_content(Term, [OtherLineA,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineB),
             check_content(Term, [OtherLineB,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineC),
             check_content(Term, [OtherLineC,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,OtherLineD),
             check_content(Term, [OtherLineD,$$]),
             check_location(Term, {0, Cols - Col - width(U)+1}),

             send_tty(Term,"C-A"),
             check_location(Term, {-4, 0}), %% Broken
             send_tty(Term,"Right"),
             check_location(Term, {-4, width(U)}), %% Broken

             send_tty(Term,"DC"), %% Broken
             check_content(Term, ["a.*",U,"$"]),
             check_content(Term, ["^b.*",U,"c$"]),
             check_content(Term, ["^c.*",U,"dd$"]),

             send_tty(Term,"a"),
             check_content(Term, [FirstLine,$$]),
             check_content(Term, [OtherLineA,$$]),
             check_content(Term, [OtherLineB,$$]),
             check_content(Term, [OtherLineC,$$]),
             check_content(Term, [OtherLineD,$$]),

             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

shell_delete(Config) ->

    Term = start_tty(Config),

    try

        [ begin
              send_tty(Term,"a"),
              check_content(Term, "> a$"),
              check_location(Term, {0, 1}),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, 0}),
              check_content(Term, ">$"),
              send_tty(Term,"a"),
              send_tty(Term,U),
              check_location(Term, {0, width([$a, U])}),
              send_tty(Term,"a"),
              send_tty(Term,U),
              check_location(Term, {0, width([$a,U,$a,U])}),
              check_content(Term, ["> a",U,$a,U,"$"]),
              send_tty(Term,"Left"),
              send_tty(Term,"Left"),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, width([$a])}),
              check_content(Term, ["> aa",U,"$"]),
              send_tty(Term,U),
              check_location(Term, {0, width([$a,U])}),
              send_tty(Term,"Left"),
              send_tty(Term,"DC"),
              check_location(Term, {0, width([$a])}),
              check_content(Term, ["> aa",U,"$"]),
              send_tty(Term,"DC"),
              send_tty(Term,"DC"),
              check_content(Term, ["> a$"]),
              send_tty(Term,"C-E"),
              check_location(Term, {0, width([$a])}),
              send_tty(Term,"BSpace"),
              check_location(Term, {0, width([])})
          end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters at the edge of the screen that are "large",
%% we need to take special care.
shell_delete_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,[U,U,"aaaaa"]),
             check_content(Term,["\n",U,U,"aaaaa$"]),
             [send_tty(Term,"Left") || _ <- lists:seq(1,5+2)],
             check_location(Term,{0,-Col}),
             send_tty(Term,"BSpace"),
             check_content(Term,"> a* \n"),
             check_location(Term,{-1,Cols - Col - 1}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,"\n"]),
             check_location(Term,{-1,Cols - Col - 2}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U," \n"]),
             check_location(Term,{-1,Cols - Col - 3}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,U,"\n"]),
             check_content(Term,["\naaaaa$"]),
             check_location(Term,{-1,Cols - Col - 4}),
             send_tty(Term,"BSpace"),
             check_content(Term,["> a*",U,U,"a\n"]),
             check_content(Term,["\naaaa$"]),
             check_location(Term,{-1,Cols - Col - 5}),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters and a "large" characters is changing line we need
%% to take extra care
shell_delete_unicode_not_at_cursor_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,["a",U,"aaaaa"]),
             check_content(Term,["\na",U,"aaaaa$"]),
             send_tty(Term,"C-A"),
             send_tty(Term,"DC"),
             check_content(Term,["\n",U,"aaaaa$"]),
             send_tty(Term,"DC"),
             check_content(Term,["\n",U,"aaaaa$"]),
             check_content(Term,["> a* \n"]),
             send_tty(Term,"DC"),
             check_content(Term,["\naaaaa$"]),
             check_content(Term,["> a*",U,"\n"]),
             send_tty(Term,"DC"),
             check_content(Term,["\naaaa$"]),
             check_content(Term,["> a*",U,"a\n"]),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

%% When deleting characters and a "large" characters is changing line we need
%% to take extra care
shell_update_window_unicode_wrap(Config) ->

    Term = start_tty(Config),

    {_Rows, Cols} = get_window_size(Term),
    {_Row, Col} = get_location(Term),

    try
        [begin
             send_tty(Term,lists:duplicate(Cols - Col - width(U) + 1,"a")),
             check_content(Term,"> a*$"),
             send_tty(Term,[U,"aaaaa"]),
             check_content(Term,["> a* ?\n",U,"aaaaa$"]),
             tmux(["resize-window -t ",tty_name(Term)," -x ",Cols+1]),
             check_content(Term,["> a*",U,"\naaaaa$"]),
             tmux(["resize-window -t ",tty_name(Term)," -x ",Cols]),
             check_content(Term,["> a* ?\n",U,"aaaaa$"]),
             send_tty(Term,"Enter")
         end || U <- hard_unicode()]
    after
        stop_tty(Term)
    end.

shell_transpose(Config) ->

    Term = start_tty(Config),

    Unicode = [[$a]] ++ hard_unicode(),

    try
        [
         begin
             send_tty(Term,"a"),
             [send_tty(Term,[CP]) || CP <- U],
             send_tty(Term,"b"),
             [[send_tty(Term,[CP]) || CP <- U2] || U2 <- Unicode],
             send_tty(Term,"cde"),
             check_content(Term, ["a",U,"b",Unicode,"cde$"]),
             check_location(Term, {0, width(["a",U,"b",Unicode,"cde"])}),
             send_tty(Term,"Home"),
             check_location(Term, {0, 0}),
             send_tty(Term,"Right"),
             send_tty(Term,"Right"),
             check_location(Term, {0, 1+width([U])}),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",U,Unicode,"cde$"]),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",hd(Unicode),U,tl(Unicode),"cde$"]),
             [send_tty(Term,"C-T") || _ <- lists:seq(1,length(Unicode)-1)],
             check_content(Term, ["ab",Unicode,U,"cde$"]),
             send_tty(Term,"C-T"),
             check_content(Term, ["ab",Unicode,"c",U,"de$"]),
             check_location(Term, {0, width(["ab",Unicode,"c",U])}),
             send_tty(Term,"End"),
             check_location(Term, {0, width(["ab",Unicode,"c",U,"de"])}),
             send_tty(Term,"Left"),
             send_tty(Term,"Left"),
             send_tty(Term,"BSpace"),
             check_content(Term, ["ab",Unicode,"cde$"]),
             send_tty(Term,"End"),
             send_tty(Term,"Enter")
         end || U <- Unicode],
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_search(C) ->
    Term = start_tty(C),

    try
        send_tty(Term,"a"),
        send_tty(Term,"."),
        send_tty(Term,"Enter"),
        send_tty(Term,"'"),
        send_tty(Term,"a"),
        send_tty(Term,[16#1f600]),
        send_tty(Term,"'"),
        send_tty(Term,"."),
        send_tty(Term,"Enter"),
        check_location(Term, {0, 0}),
        send_tty(Term,"C-r"),
        check_content(Term, "search:\\s*\n\\s*'a😀'."),
        send_tty(Term,"C-a"),
        check_location(Term, {-1, width(C, "'a😀'.")}),
        send_tty(Term,"Enter"),
        send_tty(Term,"C-r"),
        check_content(Term, "search:\\s*\n\\s*'a😀'."),
        send_tty(Term,"a"),
        check_content(Term, "search: a\\s*\n\\s*'a😀'."),
        send_tty(Term,"C-r"),
        check_content(Term, "search: a\\s*\n\\s*a."),
        send_tty(Term,"BSpace"),
        check_content(Term, "search:\\s*\n\\s*'a😀'."),
        send_tty(Term,"BSpace"),
        check_content(Term, "search:\\s*\n\\s*'a😀'."),
        send_tty(Term,"M-c"),
        check_location(Term, {-1, 0}),
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_insert(Config) ->
    Term = start_tty(Config),

    try
        send_tty(Term,"abcdefghijklm"),
        check_content(Term, "abcdefghijklm$"),
        check_location(Term, {0, 13}),
        send_tty(Term,"Home"),
        send_tty(Term,"Right"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        send_tty(Term,"C-T"),
        check_content(Term, "bcdeafghijklm$"),
        send_tty(Term,"End"),
        send_tty(Term,"Left"),
        send_tty(Term,"Left"),
        send_tty(Term,"BSpace"),
        check_content(Term, "bcdeafghijlm$"),
        ok
    after
        stop_tty(Term)
    end.

shell_update_window(Config) ->
    Term = start_tty(Config),

    Text = lists:flatten(["abcdefghijklmabcdefghijklm"]),
    {_Row, Col} = get_location(Term),

    try
        send_tty(Term,Text),
        check_content(Term,Text),
        check_location(Term, {0, width(Text)}),
        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text)+Col+1]),
        send_tty(Term,"a"),
        check_location(Term, {0, -Col}),
        send_tty(Term,"BSpace"),
        check_location(Term, {-1, width(Text)}),
        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text)+Col]),
        %% When resizing, tmux does not xnfix the cursor, so it will remain
        %% at the previous locations
        check_location(Term, {-1, width(Text)}),
        send_tty(Term,"a"),
        check_location(Term, {0, -Col + 1}),

        %% When we do backspace here, tmux seems to place the cursor in an
        %% incorrect position except when a terminal is attached.
        send_tty(Term,"BSpace"),
        %% This really should be {0, -Col}, but sometimes tmux sets it to
        %% {-1, width(Text)} instead.
        check_location(Term, [{0, -Col}, {-1, width(Text)}]),

        tmux(["resize-window -t ",tty_name(Term)," -x ",width(Text) div 2 + Col]),
        %% Depending on what happened with the cursor above, the line will be
        %% different here.
        check_location(Term, [{0, -Col + width(Text) div 2},
                              {-1, -Col + width(Text) div 2}]),
        ok
    after
        stop_tty(Term)
    end.
shell_small_window_multiline_navigation(Config) ->
    Term0 = start_tty(Config),
    tmux(["resize-window -t ",tty_name(Term0)," -x ",30, " -y ", 6]),
    {Row, Col} = get_location(Term0),
    Term = Term0#tmux{orig_location = {Row, Col}},
    Text = ("xbcdefghijklmabcdefghijklm\n"++
        "abcdefghijkl\n"++
        "abcdefghijklmabcdefghijklm\n"++
        "abcdefghijklmabcdefghijklx"),
    try
        send_tty(Term,Text),
        check_location(Term, {0, -4}),
        send_tty(Term,"Home"),
        check_location(Term, {-1, 0}),
        send_tty(Term, "C-Up"),
        check_location(Term, {-2, 0}),
        send_tty(Term, "C-Down"),
        check_location(Term, {-1, 0}),
        send_tty(Term, "Left"),
        check_location(Term, {-1, -4}),
        send_tty(Term, "Right"),
        check_location(Term, {-1, 0}),
        send_tty(Term, "\e[1;4A"),
        check_location(Term, {-5, 0}),
        check_content(Term,"xbc"),
        send_tty(Term, "\e[1;4B"),
        check_location(Term, {0, -4}),
        check_content(Term,"klx"),
        send_tty(Term, " sets:is_e\t"),
        check_content(Term,"is_element"),
        check_content(Term,"is_empty"),
        check_location(Term, {-3, 6}),
        send_tty(Term, "C-Up"),
        send_tty(Term,"Home"),
        check_location(Term, {-2, 0}),
        send_tty(Term, "sets:is_e\t"),
        check_content(Term,"is_element"),
        check_content(Term,"is_empty"),
        check_location(Term, {-4, 9}),
        send_tty(Term, "M-Enter"),
        check_location(Term, {-1, 0}),
        ok
    after
        stop_tty(Term)
    end.
shell_huge_input(Config) ->
    Term = start_tty(Config),

    ManyUnicode = lists:duplicate(100,hard_unicode()),

    try
        send_tty(Term,ManyUnicode),
        check_content(Term, hard_unicode_match(Config) ++ "$",
                      #{ replace => {"\n",""} }),
        send_tty(Term,"Enter"),
        ok
    after
        stop_tty(Term)
    end.
shell_receive_standard_out(Config) ->
    Term = start_tty(Config),
    try
        send_tty(Term,"my_fun(5) -> ok; my_fun(N) -> receive after 100 -> io:format(\"~p\\n\", [N]), my_fun(N+1) end.\n"),
        send_tty(Term, "spawn(shell_default, my_fun, [0]). ABC\n"),
        timer:sleep(1000),
        check_location(Term, {0, 0}), %% Check that we are at the same location relative to the start.
        check_content(Term, "3\\s+4\\s+.+>\\sABC"),
        ok
    after
        stop_tty(Term)
    end.
%% Test that the shell works when invalid utf-8 (aka latin1) is sent to it
shell_invalid_unicode(Config) ->
    Term = start_tty(Config),

    InvalidUnicode = <<$å,$ä,$ö>>, %% åäö in latin1

    try
        send_tty(Term,hard_unicode()),
        check_content(Term, hard_unicode() ++ "$"),
        send_tty(Term,"Enter"),
        check_content(Term, "illegal character"),
        %% Send invalid utf-8
        send_stdin(Term,InvalidUnicode),
        %% Check that the utf-8 was echoed
        check_content(Term, "\\\\345\\\\344\\\\366$"),
        send_tty(Term,"Enter"),
        %% Check that the terminal entered "latin1" mode
        send_tty(Term,"😀한."),
        check_content(Term, "\\Q\\360\\237\\230\\200\\355\\225\\234.\\E$"),
        send_tty(Term,"Enter"),
        %% Check that we can reset the encoding to unicode
        send_tty(Term,"io:setopts([{encoding,unicode}])."),
        send_tty(Term,"Enter"),
        check_content(Term, "\nok\n"),
        send_tty(Term,"😀한"),
        check_content(Term, "😀한$"),
        ok
    after
        stop_tty(Term),
        ok
    end.


%% Test the we can handle ansi insert, navigation and delete
%%   We currently can not so skip this test
shell_support_ansi_input(Config) ->

    Term = start_tty(Config),

    BoldText = "\e[;1m",
    ClearText = "\e[0m",

    try
        send_stdin(Term,["{",BoldText,"a😀b",ClearText,"}"]),
        timer:sleep(1000),
        try check_location(Term, {0, width("{1ma😀bm}")}) of
            _ ->
                throw({skip, "Do not support ansi input"})
        catch _:_ ->
                ok
        end,
        check_location(Term, {0, width("{a😀b}")}),
        check_content(fun() -> get_content(Term,"-e") end,
                      ["{", BoldText, "a😀b", ClearText, "}"]),
        send_tty(Term,"Left"),
        send_tty(Term,"Left"),
        check_location(Term, {0, width("{a😀")}),
        send_tty(Term,"C-Left"),
        check_location(Term, {0, width("{")}),
        send_tty(Term,"End"),
        send_tty(Term,"BSpace"),
        send_tty(Term,"BSpace"),
        check_content(Term, ["{", BoldText, "a😀"]),
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_expand_location_below(Config) ->

    Term = start_tty(Config),

    {Rows, _} = get_window_size(Term),
    {Row, Col} = get_location(Term),

    NumFunctions = lists:seq(0, Row*2),
    FunctionName = "a_long_function_name",

    Module = lists:flatten(
               ["-module(long_module).\n",
                "-export([",lists:join($,,[io_lib:format("~s~p/0",[FunctionName,I]) || I <- NumFunctions]),"]).\n\n",
                [io_lib:format("~s~p() -> ok.\n",[FunctionName,I]) || I <- NumFunctions]]),

    DoScan = fun F([]) ->
                     [];
                 F(Str) ->
                     {done,{ok,T,_},C} = erl_scan:tokens([],Str,0),
                     [ T | F(C) ]
             end,
    Forms = [ begin {ok,Y} = erl_parse:parse_form(X),Y end || X <- DoScan(Module) ],
    {ok,_,Bin} = compile:forms(Forms, [debug_info]),

    try
        tmux(["resize-window -t ",tty_name(Term)," -x 80"]),
        Cols = 80,

        %% First check that basic completion works
        send_stdin(Term, "escript:"),
        send_stdin(Term, "\t"),
        %% Cursor at correct place
        check_location(Term, {-3, width("escript:")}),
        %% Nothing after the start( completion
        check_content(Term, "start\\($"),

        %% Check that completion is cleared when we type
        send_stdin(Term, "s"),
        check_location(Term, {-3, width("escript:s")}),
        check_content(Term, "escript:s$"),

        %% Check that completion works when in the middle of a term
        send_tty(Term, "Home"),
        send_tty(Term, "["),
        send_tty(Term, "End"),
        send_tty(Term, ", test_after]"),
        [send_tty(Term, "Left") || _ <- ", test_after]"],
        send_stdin(Term, "\t"),
        check_location(Term, {-3, width("[escript:s")}),
        check_content(Term, "script_name\\([ ]+start\\($"),
        send_tty(Term, "C-K"),

        %% Check that completion works when in the middle of a long term
        send_tty(Term, ", "++ lists:duplicate(80*2, $a)++"]"),
        [send_tty(Term, "Left") || _ <- ", "++ lists:duplicate(80*2, $a)++"]"],
        send_stdin(Term, "\t"),
        check_location(Term, {-4, width("[escript:s")}),
        check_content(Term, "script_name\\([ ]+start\\($"),
        send_tty(Term, "End"),
        send_stdin(Term, ".\n"),

        %% Check that we behave as we should with very long completions
        rpc(Term, fun() ->
                          {module, long_module} = code:load_binary(long_module, "long_module.beam", Bin)
                  end),
        check_content(Term, "3>"),
        tmux(["resize-window -t ",tty_name(Term)," -y 50"]),
        timer:sleep(1000), %% Sleep to make sure window has resized
        Result = 61,
        Rows1 = 48,
        send_stdin(Term, "long_module:" ++ FunctionName),
        send_stdin(Term, "\t"),
        check_content(Term, "3> long_module:" ++ FunctionName ++ "\nfunctions(\n|.)*a_long_function_name0\\("),

        %% Check that correct text is printed below expansion
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [1, 7, Result])),
        send_stdin(Term, "\t"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
            [1, Rows1, Result])),
        send_tty(Term, "Down"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [2, Rows1+1, Result])),
        send_tty(Term, "PgDn"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [7, Rows1+6, Result])),
        send_tty(Term, "PgUp"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [2, Rows1+1, Result])),
        send_tty(Term, "PgUp"),
        %% Overshoot up
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [1, Rows1, Result])),
        %% Overshoot down
        send_tty(Term, "PgDn"),
        send_tty(Term, "PgDn"),
        send_tty(Term, "PgDn"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [14, Rows1+13, Result])),
        check_content(Term, "\n.*a_long_function_name99\\("),
        send_tty(Term, "Up"),
        check_content(Term, io_lib:format("rows ~w to ~w of ~w",
                                          [13, Rows1+12, Result])),

        send_tty(Term, "\t"),

        %% We resize the terminal to make everything fit and test that
        %% expand below displays everything
        tmux(["resize-window -t ", tty_name(Term), " -y ", integer_to_list(Row+10)]),
        timer:sleep(1000), %% Sleep to make sure window has resized
        send_tty(Term, "\t\t"),
        check_content(Term, "3> long_module:" ++ FunctionName ++ "\nfunctions(\n|.)*a_long_function_name99\\($"),

        %% Check that doing an expansion when cursor is in xnfix position works
        send_tty(Term, "BSpace"),
        check_content(Term, "3> long_module:a_long_function_nam$"),
        send_tty(Term, "Home"),
        send_tty(Term, lists:duplicate(Cols - Col - width(", long_module:a_long_function_name"), "a")),
        send_tty(Term, ", "),
        send_tty(Term, "End"),
        send_tty(Term, "\t"),
        check_location(Term, {-Rows + 2, -Col}),
        send_tty(Term, "\t"),
        check_content(Term, "3> a+, long_module:" ++ FunctionName ++ "\n\nfunctions(\n|.)*a_long_function_name0\\("),
        check_location(Term, {-Rows + 2, -Col}),
        send_tty(Term, "Down"),
        check_location(Term, {-Rows + 2, -Col}),
        send_tty(Term, "Down"),
        check_location(Term, {-Rows + 2, -Col}),

        send_tty(Term, "Home"),
        send_tty(Term, lists:duplicate(Cols, "b")),
        send_tty(Term, "End"),
        send_tty(Term, "\t"),
        check_content(Term, "3> b+\nb+a+, long_module:" ++ FunctionName ++ "\n\nfunctions(\n|.)*a_long_function_name0\\("),
        check_location(Term, {-Rows + 3, -Col}),
        send_tty(Term, "Down"),
        check_location(Term, {-Rows + 3, -Col}),
        send_tty(Term, "Down"),
        check_location(Term, {-Rows + 3, -Col}),

        ok
    after
        stop_tty(Term),
        ok
    end.

shell_expand_location_above(Config) ->

    Term = start_tty([{args,["-stdlib","shell_expand_location","above"]}|Config]),

    try
        tmux(["resize-window -t ",tty_name(Term)," -x 80"]),
        send_stdin(Term, "escript:"),
        send_stdin(Term, "\t"),
        check_location(Term, {0, width("escript:")}),
        check_content(Term, "start\\(\n"),
        check_content(Term, "escript:$"),
        send_stdin(Term, "s"),
        send_stdin(Term, "\t"),
        check_location(Term, {0, width("escript:s")}),
        check_content(Term, "\nscript_name\\([ ]+start\\(\n"),
        check_content(Term, "escript:s$"),
        ok
    after
        stop_tty(Term),
        ok
    end.

shell_help(Config) ->
    Term = start_tty(Config),
    try
        send_stdin(Term, "lists"),
        send_stdin(Term, "\^[h"),
        check_content(Term, "List processing functions."),
        send_stdin(Term, ":all"),
        send_stdin(Term, "\^[h"),
        check_content(Term, "-spec all(Pred, List) -> boolean()"),
        ok
    after
        stop_tty(Term),
        ok
    end.
%% Test the we can handle invalid ansi escape chars.
%%   tmux cannot handle this... so we test this using to_erl
shell_invalid_ansi(_Config) ->

    InvalidAnsiPrompt =
        case proplists:get_value(encoding, io:getopts(user)) of
            unicode ->
                ["\e]94m",54620,44397,50612,47,51312,49440,47568,"\e]0m"];
            latin1 ->
                ["\e]94minvalid_test\e]0m"]
        end,

    rtnode:run(
      [{eval, fun() -> application:set_env(
                         stdlib, shell_prompt_func_test,
                         fun() -> InvalidAnsiPrompt end)
              end },
       {putline,"a."},
       {expect, "a[.]"},
       {expect, ["\\Q",InvalidAnsiPrompt,"\\E"]}],
      "", "",
      ["-pz",filename:dirname(code:which(?MODULE)),
       "-connect_all","false",
       "-kernel","logger_level","all",
       "-kernel","shell_history","disabled",
       "-kernel","prevent_overlapping_partitions","false",
       "-eval","shell:prompt_func({interactive_shell_SUITE,prompt})."
      ]).

shell_ignore_pager_commands(Config) ->
    Term = start_tty(Config),
    case code:get_doc(file, #{sources=>[eep48]}) of
        {error, _} -> {skip, "No documentation available"};
        _ ->
            try
                send_tty(Term, "h(file).\n"),
                check_content(Term,"\\Qmore (y/n)? (y)\\E"),
                send_tty(Term, "n\n"),
                check_content(Term,"ok"),
                send_tty(Term, "C-P"),
                check_content(Term,"\\Qh(file).\\E"),
                ok
            after
                stop_tty(Term),
                ok
            end
    end.
test_valid_keymap(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    Term = setup_tty([{args, ["-config", DataDir ++ "valid_keymap.config"]} | Config]),
    Prompt = fun() -> ["\e[94m",54620,44397,50612,47,51312,49440,47568,"\e[0m"] end,
    erpc:call(Term#tmux.node, application, set_env,
              [stdlib, shell_prompt_func_test,
               proplists:get_value(shell_prompt_func_test, Config, Prompt)]),
    try
        check_not_in_content(Term, "Invalid key"),
        check_not_in_content(Term, "Invalid function"),
        send_tty(Term, "asdf"),
        send_tty(Term, "C-u"),
        check_content(Term, ">$"),
        send_tty(Term, "1.\n"),
        send_tty(Term, "C-b"),
        check_content(Term, "2>\\s1.$"),
        ok
    after
        stop_tty(Term),
        ok
    end.

test_invalid_keymap(Config) when is_list(Config) ->
    DataDir = proplists:get_value(data_dir,Config),
    Term1 = setup_tty([{args, ["-config", DataDir ++ "invalid_keymap.config"]} | Config]),
    try
        check_content(Term1, "Invalid key"),
        check_content(Term1, "Invalid function"),
        send_tty(Term1, "asdf"),
        send_tty(Term1, "C-u"),
        check_content(Term1, ">$"),
        ok
    after
        stop_tty(Term1),
        ok
    end.
external_editor(Config) ->
    case os:find_executable("nano") of
        false -> {skip, "nano is not installed"};
        _ ->
            Term = start_tty(Config),
            try
                tmux(["resize-window -t ",tty_name(Term)," -x 80"]),
                send_tty(Term,"os:putenv(\"EDITOR\",\"nano\").\n"),
                send_tty(Term, "\"some text with\nnewline in it\""),
                check_content(Term,"3> \"some text with\\s*\n.+\\s*newline in it\""),
                send_tty(Term, "C-O"),
                check_content(Term,"GNU nano [\\d.]+"),
                check_content(Term,"\"some text with\\s*\n\\s*newline in it\""),
                send_tty(Term, "Right"),
                send_tty(Term, "still"),
                send_tty(Term, "Enter"),
                send_tty(Term, "C-O"), %% save in nano
                send_tty(Term, "Enter"),
                send_tty(Term, "C-X"), %% quit in nano
                check_content(Term,"3> \"still\\s*\n\\s*.+\\s*some text with\\s*\n.+\\s*newline in it\""),
                send_tty(Term,".\n"),
                check_content(Term,"\\Q\"still\\nsome text with\\nnewline in it\"\\E"),
                ok
            after
                stop_tty(Term),
                ok
            end
    end.

external_editor_visual(Config) ->
    case os:find_executable("vim") of
        false ->
            {skip, "vim is not installed"};
        _ ->
            Term = start_tty(Config),
            try
                tmux(["resize-window -t ",tty_name(Term)," -x 80"]),
                send_tty(Term,"os:putenv(\"EDITOR\",\"nano\").\n"),
                check_content(Term, "3>"),
                send_tty(Term,"os:putenv(\"VISUAL\",\"vim -u DEFAULTS -U NONE -i NONE\").\n"),
                check_content(Term, "4>"),
                send_tty(Term,"\"hello"),
                send_tty(Term, "C-O"), %% Open vim
                check_content(Term, "^\"hello"),
                send_tty(Term, "$"), %% Set cursor at end
                send_tty(Term, "a"), %% Enter insert mode at end
                check_content(Term, "-- INSERT --"),
                send_tty(Term, "\"."),
                send_tty(Term,"Escape"),
                send_tty(Term,":wq"),
                send_tty(Term,"Enter"),
                check_content(Term, "\"hello\"[.]$"),
                send_tty(Term,"Enter"),
                check_content(Term, "\"hello\""),
                check_content(Term, "5>$")
            after
                stop_tty(Term),
                ok
            end
    end.

external_editor_unicode(Config) ->
    NanoPath = os:find_executable("nano"),
    case NanoPath of
        false -> {skip, "nano is not installed"};
        _ ->
            Term = start_tty(Config),
            try
                tmux(["resize-window -t ",tty_name(Term)," -x 80"]),
                send_tty(Term,"os:putenv(\"EDITOR\",\"nano\").\n"),
                send_tty(Term, hard_unicode()),
                check_content(Term,"3> " ++ hard_unicode_match(Config)),
                send_tty(Term, "C-O"), %% open external editor (nano)
                check_content(Term,"GNU nano [\\d.]+"),
                send_tty(Term, "still "),
                check_content(Term,"\nstill "),
                send_tty(Term, "C-O"), %% save in nano
                send_tty(Term, "Enter"),
                send_tty(Term, "C-X"), %% quit in nano
                check_content(Term,"still "++hard_unicode_match(Config)),
                ok
            after
                stop_tty(Term),
                ok
            end
    end.

%% There used to be a race condition when using shell:start_interactive where
%% the newline handling of standard_error did not change correctly to compensate
%% for the tty changing to canonical mode
shell_standard_error_nlcr(Config) ->

    [
     begin
         Term = setup_tty([{env,[{"TERM",TERM}]},{args, ["-noshell"]} | Config]),
         try
             rpc(Term, io, format, [standard_error,"test~ntest~ntest", []]),
             check_content(Term, "test\ntest\ntest$"),
             rpc(Term, fun() -> shell:start_interactive(),
                                io:format(standard_error,"test~ntest~ntest", [])
                       end),
             check_content(Term, "test\ntest\ntest(\n|.)*test\ntest\ntest")
         after
             stop_tty(Term)
         end
     end || TERM <- ["dumb",os:getenv("TERM")]].

%% We test that suspending of `erl` and then resuming restores the shell
shell_suspend(Config) ->

    Name = peer:random_name(proplists:get_value(tc_path,Config)),
    %% In order to suspend `erl` we need it to run in a shell that has job control
    %% so we start the peer within a tmux window instead of having it be the original
    %% process.
    os:cmd("tmux new-window -n " ++ Name ++ " -d -- bash --norc"),

    Peer = #{ name => Name,
              post_process_args =>
                        fun(["new-window","-n",_,"-d","--"|CmdAndArgs]) ->
                                FlatCmdAndArgs =
                                      lists:join(
                                        " ",[[$',A,$'] || A <- CmdAndArgs]),
                                ["send","-t",Name,lists:flatten(FlatCmdAndArgs),"Enter"]
                        end
            },


    Term = start_tty([{peer, Peer}|Config]),

    try
        send_tty(Term, hard_unicode()),
        check_content(Term,["2> ",hard_unicode(),"$"]),
        send_tty(Term, "C-Z"),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        send_tty(Term, "M-l"),
        check_content(Term,["2> ",hard_unicode(),"$"]),
        check_location(Term,{0,width(hard_unicode())}),
        ok
    after
        stop_tty(Term),
        ok
    end.

%% We test that suspending of `erl` and then resuming restores the shell
shell_full_queue(Config) ->

    [throw({skip,"Need unbuffered to run"}) || os:find_executable("unbuffered") =:= false],

    %% In order to fill the read buffer of the terminal we need to get a
    %% bit creative. We first need to start erl in bash in order to be
    %% able to get access to job control for suspended processes.
    %% We then also wrap `erl` in `unbuffer -p` so that we can suspend
    %% that program in order to block writing to stdout for a while.

    Name = peer:random_name(proplists:get_value(tc_path,Config)),
    os:cmd("tmux new-window -n " ++ Name ++ " -d -- bash --norc"),

    Peer = #{ name => Name,
              post_process_args =>
                        fun(["new-window","-n",_,"-d","--"|CmdAndArgs]) ->
                                FlatCmdAndArgs = ["unbuffer -p "] ++
                                      lists:join(
                                        " ",[[$',A,$'] || A <- CmdAndArgs]),
                                ["send","-t",Name,lists:flatten(FlatCmdAndArgs),"Enter"]
                        end
            },


    Term = start_tty([{peer, Peer}|Config]),

    UnbufferedPid = os:cmd("ps -o ppid= -p " ++ rpc(Term,os,getpid,[])),

    WriteUntilStopped =
        fun F(Char) ->
                rpc(Term,io,format,[user,[Char],[]]),
                put(bytes,get(bytes,0)+1),
                receive
                    stop ->
                        rpc(Term,io,format,[user,[Char+1],[]])
                after 0 -> F(Char)
                end
        end,

    WaitUntilBlocked =
        fun(Pid, Ref) ->
                (fun F(Cnt) ->
                         receive
                             {'DOWN',Ref,_,_,_} = Down ->
                                 ct:fail({io_format_did_not_block, Down})
                         after 1000 ->
                                 ok
                         end,
                         case process_info(Pid,dictionary) of
                             {dictionary,[{bytes,Cnt}]} ->
                                 ct:log("Bytes until blocked: ~p~n",[Cnt]),
                                 %% Add one extra byte as for
                                 %% the current blocking call
                                 Cnt + 1;
                             {dictionary,[{bytes,NewCnt}]} ->
                                 F(NewCnt)
                         end
                 end)(0)
        end,

    try
        %% First test that we can suspend and then resume
        os:cmd("kill -TSTP " ++ UnbufferedPid),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        {Pid, Ref} = spawn_monitor(fun() -> WriteUntilStopped($a) end),
        WaitUntilBlocked(Pid, Ref),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        Pid ! stop,
        check_content(Term,"b$"),

        send_tty(Term, "."),
        send_tty(Term, "Enter"),

        %% Then we test that all characters are written when system
        %% is terminated just after writing
        {ok,Cols} = rpc(Term,io,columns,[user]),
        send_tty(Term, "Enter"),
        os:cmd("kill -TSTP " ++ UnbufferedPid),
        check_content(Term,"\\Q[1]+\\E\\s*Stopped"),
        {Pid2, Ref2} = spawn_monitor(fun() -> WriteUntilStopped($c) end),
        Bytes = WaitUntilBlocked(Pid2, Ref2) - 1,
        stop_tty(Term),
        send_tty(Term, "fg"),
        send_tty(Term, "Enter"),
        check_content(
          fun() ->
                  tmux(["capture-pane -p -S - -E - -t ",tty_name(Term)])
          end, lists:flatten([lists:duplicate(Cols,$c) ++ "\n" ||
                                 _ <- lists:seq(1,(Bytes) div Cols)]
                             ++ [lists:duplicate((Bytes) rem Cols,$c)])),
        ct:log("~ts",[tmux(["capture-pane -p -S - -E - -t ",tty_name(Term)])]),
        ok
    after
        stop_tty(Term),
        ok
    end.

get(Key,Default) ->
    case get(Key) of
        undefined ->
            Default;
        Value ->
            Value
    end.

%% A list of unicode graphemes that are notoriously hard to render
hard_unicode() ->
    ZWJ =
        case os:type() of
            %% macOS has very good rendering of ZWJ,
            %% but the cursor does not agree with it..
            {unix, darwin} -> [];
            _ -> [[16#1F91A,16#1F3FC]]    % Hand with skintone 🤚🏼
        end,
    [[16#1f600],             % Smilie 😀
    "한",                   % Hangul
    "Z̤͔ͧ̑̓","ä͖̭̈̇","lͮ̒ͫ","ǧ̗͚̚","o̙̔ͮ̇͐̇" %% Vertically stacked chars
     %%"👩‍👩",            % Zero width joiner
     %%"👩‍👩‍👧‍👦"                % Zero width joiner
     | ZWJ].

hard_unicode_match(Config) ->
    ["\\Q",[unicode_to_octet(Config, U) || U <- hard_unicode()],"\\E"].

unicode_to_octet(Config, U) ->
    case ?config(encoding,Config) of
        unicode -> U;
        latin1 -> unicode_to_octet(U)
    end.

unicode_to_octet(U) ->
    [if Byte >= 128 -> [$\\,integer_to_list(Byte,8)];
        true -> Byte
     end || <<Byte>> <= unicode:characters_to_binary(U)].

unicode_to_hex(Config, U) ->
    case ?config(encoding,Config) of
        unicode -> U;
        latin1 -> unicode_to_hex(U)
    end.

unicode_to_hex(U) when is_integer(U) ->
    unicode_to_hex([U]);
unicode_to_hex(Us) ->
    [if U < 128 -> U;
        U < 512 -> ["\\",integer_to_list(U,8)];
        true -> ["\\x{",integer_to_list(U,16),"}"]
     end || U <- Us].

width(C, Str) ->
    case ?config(encoding, C) of
        unicode -> width(Str);
        latin1 -> width(unicode_to_octet(Str))
    end.
width(Str) ->
    lists:sum(
      [npwcwidth(CP) || CP <- lists:flatten(Str)]).

npwcwidth(CP) ->
    try prim_tty:npwcwidth(CP)
    catch error:undef ->
            if CP =:= 16#D55C ->
                    2; %% 한
               CP =:= 16#1f91A ->
                    2; %% hand
               CP =:= 16#1F3Fc ->
                    2; %% Skintone
               CP =:= 16#1f600 ->
                    2; %% smilie
               true ->
                    case lists:member(CP, [775,776,780,785,786,787,788,791,793,794,
                                           804,813,848,852,854,858,871,875,878]) of
                        true ->
                            0;
                        false ->
                            1
                    end
            end
    end.

tmux([Cmd|_] = Command) when is_list(Cmd) ->
    tmux(lists:concat(Command));
tmux(Command) ->
    string:trim(os:cmd(["tmux ",Command])).

rpc(#tmux{ node = N }, Fun) ->
    erpc:call(N, Fun).
rpc(#tmux{ node = N }, M, F, A) ->
    erpc:call(N, M, F, A).

%% Setup a TTY, but do not type anything in terminal
setup_tty(Config) ->
    Name = maps:get(name,proplists:get_value(peer, Config, #{}),
                    peer:random_name(proplists:get_value(tc_path, Config))),

    Envs = lists:flatmap(fun({Key,Value}) ->
                                 ["-env",Key,Value]
                         end, proplists:get_value(env,Config,[])),

    ExtraArgs = proplists:get_value(args,Config,[]),

    ExecArgs = case os:getenv("TMUX_DEBUG") of
                   "strace" ->
                       STraceLog = filename:join(proplists:get_value(priv_dir,Config),
                                                 Name++".strace"),
                       ct:pal("Link to strace: file://~ts", [STraceLog]),
                       [os:find_executable("strace"),"-f",
                        "-o",STraceLog,
                        "-e","trace=all",
                        "-e","read=0,1,2",
                        "-e","write=0,1,2"
                       ] ++ string:split(ct:get_progname()," ",all);
                   "rr" ->
                       [os:find_executable("cerl"),"-rr"];
                   _ ->
                       string:split(ct:get_progname()," ",all)
               end,
    DefaultPeerArgs = #{ name => Name,
                         exec =>
                             {os:find_executable("tmux"),
                              ["new-window","-n",Name,"-d","--"] ++ ExecArgs },

                         args => ["-pz",filename:dirname(code:which(?MODULE)),
                                  "-connect_all","false",
%                                  "-kernel","logger_level","all",
                                  "-kernel","shell_history","disabled",
                                  "-kernel","prevent_overlapping_partitions","false",
                                  "-eval","shell:prompt_func({interactive_shell_SUITE,prompt})."
                                 ] ++ Envs ++ ExtraArgs,
                         wait_boot => 60_000,
                         detached => false
                       },

    {ok, Peer, Node} =
        ?CT_PEER(maps:merge(proplists:get_value(peer,Config,#{}),
                            DefaultPeerArgs)),

    Self = self(),

    %% By default peer links with the starter. For these TCs we however only
    %% want the peer to die if we die, so we create a "unidirection link" using
    %% monitors.
    spawn(fun() ->
                  TCRef = erlang:monitor(process, Self),
                  PeerRef = erlang:monitor(process, Peer),
                  receive
                      {'DOWN',TCRef,_,_,Reason} ->
                          exit(Peer, Reason);
                      {'DOWN',PeerRef,_,_,_} ->
                          ok
                  end
          end),
    unlink(Peer),

    "" = tmux(["set-option -t ",Name," remain-on-exit on"]),

    %% We start tracing on the remote node in order to help debugging
    TraceLog = filename:join(proplists:get_value(priv_dir,Config),Name++".trace"),
    ct:log("Link to trace: file://~ts",[TraceLog]),

    spawn(Node,
          fun() ->
                  {ok, _} = dbg:tracer(file,TraceLog),
                  %% dbg:p(whereis(user_drv),[c,m]),
                  %% dbg:p(whereis(user_drv_writer),[c,m]),
                  %% dbg:p(whereis(user_drv_reader),[c,m]),
                  %% dbg:tp(user_drv,x),
                  %% dbg:tp(prim_tty,x),
                  %% dbg:tpl(prim_tty,write_nif,x),
                  %% dbg:tpl(prim_tty,read_nif,x),
                  monitor(process, Self),
                  receive _ -> ok end
          end),

    #tmux{ peer = Peer, node = Node, name = Name }.

%% Start a tty, setup custom prompt and set cursor at bottom
start_tty(Config) ->

    Term = setup_tty(Config),

    Prompt = fun() -> ["\e[94m",54620,44397,50612,47,51312,49440,47568,"\e[0m"] end,
    erpc:call(Term#tmux.node, application, set_env,
              [stdlib, shell_prompt_func_test,
               proplists:get_value(shell_prompt_func_test, Config, Prompt)]),

    {Rows, _} = get_window_size(Term),

    %% We send a lot of newlines here in order for the number of rows
    %% in the window to be max so that we can predict what the cursor
    %% position is.
    [send_tty(Term,"\n") || _ <- lists:seq(1, Rows)],

    %% We enter an 'a' here so that we can get the correct orig position
    %% with an alternative prompt.
    send_tty(Term,"a.\n"),
    check_content(Term,"2>$"),
    OrigLocation = get_location(Term),
    Term#tmux{ orig_location = OrigLocation }.

prompt(L) ->
    N = proplists:get_value(history, L, 0),
    Fun = application:get_env(stdlib, shell_prompt_func_test,
                              fun() -> atom_to_list(node()) end),
    io_lib:format("(~ts)~w> ",[Fun(),N]).

stop_tty(Term) ->
    catch peer:stop(Term#tmux.peer),
    ct:log("~ts",[get_content(Term, "-e")]),
%    "" = tmux("kill-window -t " ++ Term#tmux.name),
    ok.

tty_name(Term) ->
    Term#tmux.name.

send_tty(Term, "Home") ->
    %% https://stackoverflow.com/a/55616731
    send_tty(Term,"Escape"),
    send_tty(Term,"OH");
send_tty(Term, "End") ->
    send_tty(Term,"Escape"),
    send_tty(Term,"OF");
send_tty(#tmux{ name = Name } = _Term,Value) ->
    [Head | Quotes] = string:split(Value, "'", all),
    "" = tmux("send -t " ++ Name ++ " '" ++ Head ++ "'"),
    [begin
         "" = tmux("send -t " ++ Name ++ " \"'\""),
         "" = tmux("send -t " ++ Name ++ " '" ++ V ++ "'")
     end || V <- Quotes].

%% We use send_stdin for testing of things that we cannot sent via
%% the tmux send command, such as invalid unicode
send_stdin(Term, Chars) when is_binary(Chars) ->
    rpc(Term,erlang,display_string,[stdin,Chars]);
send_stdin(Term, Chars) ->
    send_stdin(Term, iolist_to_binary(unicode:characters_to_binary(Chars))).

check_location(Term, Where) ->
    check_location(Term, Where, 5).
check_location(Term, Where, Attempt) when is_tuple(Where) ->
    check_location(Term, [Where], Attempt);
check_location(#tmux{ orig_location = {OrigRow, OrigCol} = Orig } = Term,
               Where, Attempt) ->
    NewLocation = get_location(Term),
    case lists:any(fun({AdjRow, AdjCol}) ->
                           {OrigRow+AdjRow,OrigCol+AdjCol} =:= NewLocation
                   end, Where) of
        true -> NewLocation;
        false when Attempt =:= 0 ->
            {NewRow, NewCol} = NewLocation,
            ct:fail({wrong_location, {expected,Where},
                     {got,{NewRow - OrigRow, NewCol - OrigCol},
                      {NewLocation, Orig}}});
        false ->
            timer:sleep(50),
            check_location(Term, Where, Attempt -1)
    end.

get_location(Term) ->
    RowAndCol = tmux("display -pF '#{cursor_y} #{cursor_x}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

get_window_size(Term) ->
    RowAndCol = tmux("display -pF '#{window_height} #{window_width}' -t "++Term#tmux.name),
    [Row, Col] = string:lexemes(string:trim(RowAndCol,both)," "),
    {list_to_integer(Row), list_to_integer(Col)}.

check_not_in_content(Term, NegativeMatch) ->
    check_not_in_content(Term, NegativeMatch, #{}, 5).
check_not_in_content(Term, NegativeMatch, Opts, Attempt) ->
    Opts = #{},
    OrigContent = case Term of
        #tmux{} -> get_content(Term);
        Fun when is_function(Fun,0) -> Fun()
    end,
    Content = case maps:find(replace, Opts) of
                {ok, {RE,Repl} } ->
                    re:replace(OrigContent, RE, Repl, [global]);
                error ->
                    OrigContent
                end,
    case re:run(string:trim(Content, both), lists:flatten(NegativeMatch), [unicode]) of
        {match,_} ->
            io:format("Failed, found '~ts' in ~n'~ts'~n",
            [unicode:characters_to_binary(NegativeMatch), Content]),
            io:format("Failed, found '~w' in ~n'~w'~n",
                        [unicode:characters_to_binary(NegativeMatch), Content]),
            ct:fail(match);
        _ when Attempt =:= 0 ->
            ok;
        _ ->
            timer:sleep(500),
            check_not_in_content(Term, NegativeMatch, Opts, Attempt - 1)
    end.
check_content(Term, Match) ->
    check_content(Term, Match, #{}).
check_content(Term, Match, Opts) when is_map(Opts) ->
    check_content(Term, Match, Opts, 5).
check_content(Term, Match, Opts, Attempt) ->
    OrigContent = case Term of
                  #tmux{} -> get_content(Term);
                  Fun when is_function(Fun,0) -> Fun()
              end,
    Content = case maps:find(replace, Opts) of
                  {ok, {RE,Repl} } ->
                      re:replace(OrigContent, RE, Repl, [global]);
                  error ->
                      OrigContent
              end,
    case re:run(string:trim(Content, both), lists:flatten(Match), [unicode]) of
        {match,_} ->
            ok;
        _ when Attempt =:= 0 ->
            io:format("Failed to find '~ts' in ~n'~ts'~n",
                      [unicode:characters_to_binary(Match), Content]),
            io:format("Failed to find '~w' in ~n'~w'~n",
                      [unicode:characters_to_binary(Match), Content]),
            ct:fail(nomatch);
        _ ->
            timer:sleep(500),
            check_content(Term, Match, Opts, Attempt - 1)
    end.

get_content(Term) ->
    get_content(Term, "").
get_content(#tmux{ name = Name }, Args) ->
    Content = unicode:characters_to_binary(tmux("capture-pane -p " ++ Args ++ " -t " ++ Name)),
    case string:split(Content,"a.\na") of
        [_Ignore,C] ->
            C;
        [C] ->
            C
    end.

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
    receive after 1000 -> ok end,

    %% ignore input given after io:get_line, and after h(module) pager
    rtnode:run(
       [{putdata, "io:get_line(\"getline>\").\n"},
        {expect, "getline>"},
        {putline, "hej"}, {expect, "hej\r\n"},
        {putdata, [$\^p]}, {expect, "\\Qio:get_line(\"getline>\")\\E[.]$"}
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
       {expect, "(Disabling shell history logging.|echo)"},
       {expect, "(Disabling shell history logging.|echo)"}
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
       {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[12]> $"},
       {putdata, "\^g"},
       {expect, " --> $"},
       {putline, "j"},
       {expect, "1[*] {shell,start,\\[init]}"},
       {putline, "c"},
       {expect, "\\Q("++RemNode++"@\\E[^)]*\\)[123]> $"},
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

    %% Test that if multiple remsh are given, we select the first
    rtnode:run([{expect, "Multiple"}] ++ PreCmds ++ PostCmds,
               [], " ",
               "-remsh " ++ TargetNodeStr ++ " -remsh invalid_node"),

    peer:stop(Peer),

    ok.

%% Test that if we cannot connect to a node, we get a correct error
remsh_error(_Config) ->
    "Could not connect to \"invalid_node\"\n" =
        os:cmd(ct:get_progname() ++ " -remsh invalid_node").

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
    Name = peer:random_name(?FUNCTION_NAME),
    case rtnode:start(" -name " ++ Name ++ Domain) of
        {ok, _SRPid, STPid, SNode, SState} ->
            try
                {ok, _CRPid, CTPid, CNode, CState} =
                    rtnode:start("-name undefined" ++ Domain ++
                                     " -remsh " ++ Name),
                try
                    ok = rtnode:send_commands(
                           SNode,
                           STPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ Name ++ "\\E"}], 1),
                    ok = rtnode:send_commands(
                           CNode,
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ Name ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtnode:dump_logs(rtnode:stop(CState))
                end
            after
                rtnode:dump_logs(rtnode:stop(SState))
            end;
        {skip, _} = Else ->
            Else
    end.

%% Test that -remsh works without epmd.
remsh_no_epmd(Config) when is_list(Config) ->
    EPMD_ARGS = "-start_epmd false -erl_epmd_port 12346 ",
    Name = ?CT_PEER_NAME(),
    case rtnode:start([],"ERL_EPMD_PORT=12345 ",
                      EPMD_ARGS ++ " -sname " ++ Name) of
        {ok, _SRPid, STPid, SNode, SState} ->
            try
                ok = rtnode:send_commands(
                       SNode,
                       STPid,
                       [{putline, ""},
                        {putline, "node()."},
                        {expect, "\\Q" ++ Name ++ "\\E"}], 1),
                {ok, _CRPid, CTPid, CNode, CState} =
                    rtnode:start([],"ERL_EPMD_PORT=12345 ",
                                 EPMD_ARGS ++ " -remsh "++Name),
                try
                    ok = rtnode:send_commands(
                           CNode,
                           CTPid,
                           [{putline, ""},
                            {putline, "node()."},
                            {expect, "\\Q" ++ Name ++ "\\E"} | quit_hosting_node()], 1)
                after
                    rtnode:dump_logs(rtnode:stop(CState))
                end
            after
                rtnode:dump_logs(rtnode:stop(SState))
            end;
        {skip, _} = Else ->
            Else
    end.
remsh_expand_compatibility_25(Config) when is_list(Config) ->
    {ok, _Peer, TargetNode} = ?CT_PEER(#{}), %% Create a vsn 26 node
    NodeName = atom_to_list(TargetNode), %% compatibility
    %% Start a node on vsn 25 but run the shell on vsn 26
    case rtnode:start(peer:random_name(), "stty columns 200; ERL_AFLAGS= ", "-remsh "++NodeName, [{release, "25"}|Config]) of
        {ok, _SRPid, STPid, _, SState} ->
            try
                ok = rtnode:send_commands(undefined,
                       STPid,
                       [{putdata, "erlang:is_atom\t"},
                        {expect, "\\Qerlang:is_atom(\\E"}|
                        quit_hosting_node()], 1)
            after
                Logs = rtnode:stop(SState),
                rtnode:dump_logs(Logs)
            end;
        Else when element(1, Else) =/= ok -> Else
    end.
remsh_expand_compatibility_later_version(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    case ?CT_PEER_REL([], "25", PrivDir) of
        not_available -> {skip, "25 not available"};
        {ok, _Peer, TargetNode}  ->
            NodeName = atom_to_list(TargetNode),
            %% Start a node on later version but run the shell on vsn 25
            case rtnode:start(peer:random_name(), "stty columns 200; ERL_AFLAGS= ", "-remsh " ++ NodeName, Config) of
                {ok, _SRPid, STPid, _, SState} ->
                    try
                        ok = rtnode:send_commands(undefined,
                            STPid,
                            [{putdata, "erlang:is_atom\t"},
                             {expect, "\\Qerlang:is_atom(\\E"}|
                             quit_hosting_node()], 1)
                    after
                        Logs = rtnode:stop(SState),
                        rtnode:dump_logs(Logs)
                    end;
                Else when element(1, Else) =/= ok -> Else
            end
    end.

printed_atom(A) ->
    lists:flatten(io_lib:format("~w", [A])).
