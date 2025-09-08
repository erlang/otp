%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2002-2025. All Rights Reserved.
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

-module(etop_SUITE).

-define(ERASE_ALL, <<"\e[;H\e[2J">>).
-define(COOKED_ENDING, <<"Type Ctrl+G, then enter 'i' to interrupt etop.">>).
-define(RAW_ENDING, <<"Press 'q' to stop etop.">>).
-define(ATTEMPTS, 10).
-define(SUPPORTS_MEMORY, try erlang:memory(), true catch error : notsup -> false end).
-define(EXPECTED_LOAD_1(MEMORY),
    case MEMORY of
        true ->
            [<<"Load:">>, <<"cpu">>, <<"Memory:">>, <<"total">>, <<"binary">>];
        false ->
            [<<"Load:">>, <<"cpu">>]
    end
).
-define(EXPECTED_LOAD_2(MEMORY),
    case MEMORY of
        true ->
            [<<"procs">>, <<"processes">>, <<"code">>];
        false ->
            [<<"procs">>]
    end
).
-define(EXPECTED_LOAD_3(MEMORY),
    case MEMORY of
        true ->
            [<<"runq">>, <<"atom">>, <<"ets">>];
        false ->
            [<<"runq">>]
    end
).

%% Test functions
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).
%% Testcases
-export([text/1,
         text_tracing_off/1,
         text_in_terminal/1,
         text_in_raw_terminal/1,
         text_to_file/1,
         dump_to_file/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

all() ->
    [{group, normal},
     {group, shell}].

groups() ->
    [{normal, [], [text, text_tracing_off, text_to_file, dump_to_file]},
     {shell, [], [text_in_terminal, text_in_raw_terminal]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(shell, Config0) ->
    Config = [{width, 120}, {height, 80} | Config0],
    shell_test_lib:start_tmux(Config);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(shell, Config) ->
    shell_test_lib:stop_tmux(Config);
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(Case, Config) when Case == text; Case == text_tracing_off ->
    {ok,Node} = test_server:start_node(node2,peer,[]),
    [{node,Node} | Config];
init_per_testcase(Case, Config) when Case == text_to_file ->
    TempFile = test_server:temp_name(atom_to_list(Case)) ++ ".txt",
    ExtraArgs = ["-s","etop",
                 "-output","text",
                 "-interval","1"],
    PeerArgs = #{ name => "test",
                  exec =>
                      {os:find_executable("sh"),
                       ["-c"] ++ string:split(ct:get_progname(), " ", all) ++ ["-noshell"] },
                  args => ["-pz",filename:dirname(code:which(?MODULE))] ++ ExtraArgs,
                  post_process_args => fun(["-c" | Args]) ->
                                               ["-c", lists:flatten(lists:join(" ", Args ++ ["> " ++ TempFile]))] end,
                  wait_boot => 60_000,
                  detached => false
                },
    {ok, Peer, Node} = peer:start(PeerArgs),
    [{node,Node},{peer,Peer},{temp,TempFile} | Config];
init_per_testcase(Case, Config) when Case == dump_to_file ->
    TempFile = test_server:temp_name(atom_to_list(Case)) ++ ".txt",
    {ok,Node} = test_server:start_node(node2,peer,[]),
    [{node,Node},{temp,TempFile} | Config];
init_per_testcase(Case, Config) when Case == text_in_terminal ->
    {ok,Node} = test_server:start_node(node2,peer,[]),
    Term = shell_test_lib:setup_tty(Config),
    [{node,Node},{term,Term} | Config];
init_per_testcase(Case, Config0) when Case == text_in_raw_terminal ->
    {ok,Node} = test_server:start_node(node2,peer,[]),
    Args = ["-s","etop",
            "-output","text",
            "-noshell",
            "-node",atom_to_list(Node),
            "-interval","1"],
    Config = [{args, Args} | Config0],
    Term = shell_test_lib:setup_tty(Config),
    [{node,Node},{term,Term} | Config].

end_per_testcase(Case, Config) when Case == text; Case == text_tracing_off ->
    etop:stop(),
    Node = proplists:get_value(node, Config),
    test_server:stop_node(Node),
    ok;
end_per_testcase(Case, Config) when Case == text_to_file ->
    Peer = proplists:get_value(peer, Config),
    Node = proplists:get_value(node, Config),
    TempFile = proplists:get_value(temp, Config),
    peer:stop(Peer),
    test_server:stop_node(Node),
    file:delete(TempFile),
    ok;
end_per_testcase(Case, Config) when Case == dump_to_file ->
    Node = proplists:get_value(node, Config),
    TempFile = proplists:get_value(temp, Config),
    etop:stop(),
    test_server:stop_node(Node),
    file:delete(TempFile),
    ok;
end_per_testcase(Case, Config) when Case == text_in_terminal ->
    Node = proplists:get_value(node, Config),
    Term = proplists:get_value(term, Config),
    shell_test_lib:send_tty(Term, "etop:stop()."),
    shell_test_lib:send_tty(Term, "Enter"),
    shell_test_lib:stop_tty(Term),
    test_server:stop_node(Node),
    ok;
end_per_testcase(Case, Config) when Case == text_in_raw_terminal ->
    Node = proplists:get_value(node, Config),
    Term = proplists:get_value(term, Config),
    shell_test_lib:stop_tty(Term),
    test_server:stop_node(Node),
    ok.

text(suite) ->
    [];
text(doc) ->
    ["Start etop with text presentation"];
text(Config) ->
    Node = proplists:get_value(node, Config),

    %% Must spawn this process, else the test case will never end.
    spawn_link(etop,start,[[{node,Node},{output,text},{interval,3}]]),
    timer:sleep(4000),
    etop:config(interval,2),
    timer:sleep(3000),
    etop:config(lines,5),
    timer:sleep(3000),
    etop:config(accumulate,true),
    timer:sleep(3000),
    etop:config(sort,reductions),
    timer:sleep(3000),
    etop:config(sort,memory),
    timer:sleep(3000),
    etop:config(sort,msg_q),
    timer:sleep(3000),
    ok.

text_tracing_off(suite) ->
    [];
text_tracing_off(doc) ->
    ["Start etop with text presentation, and tracing turned off"];
text_tracing_off(Config) when is_list(Config) ->
    Node = proplists:get_value(node, Config),

    %% Must spawn this process, else the test case will never end.
    spawn_link(etop,start,[[{node,Node},
				  {output,text},
				  {interval,3},
				  {tracing,off}]]),
    timer:sleep(4000),
    etop:config(interval,2),
    timer:sleep(3000),
    etop:config(lines,5),
    timer:sleep(3000),
    etop:config(accumulate,true),
    timer:sleep(3000),
    etop:config(sort,memory),
    timer:sleep(3000),
    etop:config(sort,msg_q),
    timer:sleep(3000),
    etop:config(sort,runtime), % this should not crash, but has no effect
    timer:sleep(3000),
    ok.

text_in_terminal(suite) ->
    [];
text_in_terminal(doc) ->
    ["Start etop with shell, and verify that previous content is being overwritten"];
text_in_terminal(Config) when is_list(Config) ->
    Node = proplists:get_value(node, Config),
    Term = proplists:get_value(term, Config),
    ExpectedLineCount = 10 + proplists:get_value(lines, Config, 10) + 2,
    EtopStartCmd = io_lib:format("spawn_link(etop, start, [[{node,~w},{output,text},{interval,1}]]).", [Node]),
    shell_test_lib:send_tty(Term, EtopStartCmd),
    shell_test_lib:send_tty(Term, "Enter"),

    Provider = fun() ->
        Content = shell_test_lib:get_content(Term),
        string:split(Content, "\n", all)
    end,

    GoodOutput1 = fun() -> get_good_output(Provider, ExpectedLineCount) end,
    {ok, Content1} = wait_until_good_output(GoodOutput1, ?ATTEMPTS),
    Position = shell_test_lib:get_location(Term),
    verify_etop_output(Config, Content1, cooked),

    GoodOutput2 = fun() -> get_good_output(Provider, ExpectedLineCount, Content1) end,
    {ok, Content2} = wait_until_good_output(GoodOutput2, ?ATTEMPTS),
    %% Verify that position did not change, ie we've overwritten previous output
    Position = shell_test_lib:get_location(Term),
    verify_etop_output(Config, Content2, cooked),

    %% Stop etop
    shell_test_lib:send_tty(Term, "etop:stop()."),
    shell_test_lib:send_tty(Term, "Enter"),
    ExpectedLineCount2 = ExpectedLineCount + 2,

    GoodOutput3 = fun() -> get_good_output(Provider, ExpectedLineCount2) end,
    {ok, Content3} = wait_until_good_output(GoodOutput3, ?ATTEMPTS),

    %% Verify that etop is stopped, and content doesn't change
    GoodOutput4 = fun() -> get_good_output(Provider, ExpectedLineCount2, Content3) end,
    {error, timeout} = wait_until_good_output(GoodOutput4, ?ATTEMPTS),
    ok.

text_in_raw_terminal(suite) ->
    [];
text_in_raw_terminal(doc) ->
    ["Start etop with raw shell, and verify that previous content is being overwritten"];
text_in_raw_terminal(Config) when is_list(Config) ->
    Term = proplists:get_value(term, Config),
    ExpectedLineCount = 10 + proplists:get_value(lines, Config, 10) + 1,

    Provider = fun() ->
        Content = shell_test_lib:get_content(Term),
        string:split(Content, "\n", all)
    end,

    GoodOutput1 = fun() -> get_good_output(Provider, ExpectedLineCount) end,
    {ok, Content1} = wait_until_good_output(GoodOutput1, ?ATTEMPTS),
    Position = shell_test_lib:get_location(Term),
    verify_etop_output(Config, Content1, raw),

    GoodOutput2 = fun() -> get_good_output(Provider, ExpectedLineCount, Content1) end,
    {ok, Content2} = wait_until_good_output(GoodOutput2, ?ATTEMPTS),
    %% Verify that position did not change, ie we've overwritten previous output
    Position = shell_test_lib:get_location(Term),
    verify_etop_output(Config, Content2, raw),

    %% Stop etop
    shell_test_lib:send_tty(Term, "q"),
    timer:sleep(100),

    GoodOutput3 = fun() -> get_good_output(Provider, ExpectedLineCount) end,
    {ok, Content3} = wait_until_good_output(GoodOutput3, ?ATTEMPTS),

    %% Verify that etop is stopped, and content doesn't change
    GoodOutput4 = fun() -> get_good_output(Provider, ExpectedLineCount, Content3) end,
    {error, timeout} = wait_until_good_output(GoodOutput4, ?ATTEMPTS),
    ok.

text_to_file(suite) ->
    [];
text_to_file(doc) ->
    ["Start etop with output redirected to file, and verify it still behaves like a shell"];
text_to_file(Config) when is_list(Config) ->
    Node = proplists:get_value(node, Config),
    TempFile = proplists:get_value(temp, Config),
    ExpectedLineCount = ((10 + proplists:get_value(lines, Config, 10) + 2) * 3) + 1,

    Provider = fun() ->
        {ok, Content} = file:read_file(TempFile),
        string:split(Content, "\n", all)
    end,
    GoodOutput = fun() -> get_good_output(Provider, ExpectedLineCount) end,
    {ok, Content} = wait_until_good_output(GoodOutput, ?ATTEMPTS * 3),

    stop = erpc:call(Node, etop, stop, []),

    lists:foldl(fun({_Index, ?ERASE_ALL}, Acc) ->
                        Acc;
                   ({Index, Line}, Acc) ->
                        Lines = lists:append(Acc, [Line]),
                        case Index rem ExpectedLineCount of
                            0 ->
                                verify_etop_output(Config, Lines, cooked),
                                [];
                            _ ->
                                Lines
                        end
                end, [], lists:enumerate(Content)),
    ok.

dump_to_file(suite) ->
    [];
dump_to_file(doc) ->
    ["Start etop, send message to dump content to file and verify ansi escape sequences were not used"];
dump_to_file(Config) when is_list(Config) ->
    Node = proplists:get_value(node, Config),
    TempFile = proplists:get_value(temp, Config),
    ExpectedLineCount = 10 + proplists:get_value(lines, Config, 10) + 1,

    spawn_link(etop,start,[[{node,Node},
                            {output,text},
                            {interval,1},
                            {tracing,off}]]),

    ok = wait_until(fun() -> whereis(etop_server) /= undefined end, ?ATTEMPTS),
    ok = etop:dump(TempFile),
    
    Provider = fun() ->
        {ok, Content} = file:read_file(TempFile),
        string:split(Content, "\n", all)
    end,
    GoodOutput = fun() -> get_good_output(Provider, ExpectedLineCount) end,
    {ok, Content} = wait_until_good_output(GoodOutput, ?ATTEMPTS),

    [<<"">> | Lines] = Content,
    verify_etop_output(Config, Lines, unknown),
    ok.

verify_etop_output(Config, Content, ShellMode) when is_binary(Content) ->
    verify_etop_output(Config, string:split(Content, "\n", all), ShellMode);
verify_etop_output(Config, Lines, ShellMode) ->
    Node = proplists:get_value(node, Config),
    LineCount = proplists:get_value(lines, Config, 10),

    DupLine1 = lists:nth(1, Lines),
    verify_dup_line($=, DupLine1),

    NodeAndTimeLine = lists:nth(2, Lines),
    verify_node_and_time_line(Node, NodeAndTimeLine),

    LoadLine1 = lists:nth(3, Lines),
    verify_load_line(?EXPECTED_LOAD_1(?SUPPORTS_MEMORY), LoadLine1),

    LoadLine2 = lists:nth(4, Lines),
    verify_load_line(?EXPECTED_LOAD_2(?SUPPORTS_MEMORY), LoadLine2),

    LoadLine3 = lists:nth(5, Lines),
    verify_load_line(?EXPECTED_LOAD_3(?SUPPORTS_MEMORY), LoadLine3),

    <<"">> = lists:nth(6, Lines),

    ExpectedColumnsLine = [<<"Pid">>, <<"Name">>, <<"or">>, <<"Initial">>, <<"Func">>, <<"Time">>,
                           <<"Reds">>, <<"Memory">>, <<"MsgQ">>, <<"Current">>, <<"Function">>],
    ExpectedColumnsLine = string:lexemes(lists:nth(7, Lines), " "),

    DupLine2 = lists:nth(8, Lines),
    verify_dup_line($-, DupLine2),

    ContentLines = lists:sublist(Lines, 9, LineCount),
    verify_content_lines(ContentLines),

    DupLine3 = lists:nth(9 + LineCount, Lines),
    verify_dup_line($=, DupLine3),

    verify_etop_stop_message(lists:sublist(Lines, 10 + LineCount, 2), ShellMode).

verify_dup_line(Char, Line) ->
    ExpectedWidth = string:length(Line),
    ExpectedLine = lists:duplicate(ExpectedWidth, Char),
    true = string:equal(ExpectedLine, Line).

verify_node_and_time_line(Node, NodeAndTimeLine) ->
    [NodeString | SplitLine] = string:split(string:trim(NodeAndTimeLine), " ", all),
    Node = binary_to_atom(string:trim(NodeString, both, "'")),

    TimeLine = lists:last(SplitLine),
    [Hour, Minute, Second] = string:split(TimeLine, ":", all),
    verify_time(binary_to_integer(Hour), binary_to_integer(Minute), binary_to_integer(Second)).

verify_time(Hour, Minute, Second) when
    Hour >= 0, Hour =< 23, Minute >= 0, Minute =< 59, Second >= 0, Second =< 59 ->
    ok.

verify_load_line(Expected, LoadLine) when length(Expected) == 5 ->
    SplitLine = string:lexemes(LoadLine, " "),

    Expected1 = lists:nth(1, Expected),
    Expected1 = lists:nth(1, SplitLine),

    Expected2 = lists:nth(2, Expected),
    Expected2 = lists:nth(2, SplitLine),

    _ = binary_to_integer(lists:nth(3, SplitLine)),

    Expected3 = lists:nth(3, Expected),
    Expected3 = lists:nth(4, SplitLine),

    Expected4 = lists:nth(4, Expected),
    Expected4 = lists:nth(5, SplitLine),

    _ = binary_to_integer(lists:nth(6, SplitLine)),

    Expected5 = lists:nth(5, Expected),
    Expected5 = lists:nth(7, SplitLine),

    _ = binary_to_integer(lists:nth(8, SplitLine));
verify_load_line(Expected, LoadLine) when length(Expected) == 3 ->
    SplitLine = string:lexemes(LoadLine, " "),

    Expected1 = lists:nth(1, Expected),
    Expected1 = lists:nth(1, SplitLine),

    _ = binary_to_integer(lists:nth(2, SplitLine)),

    Expected2 = lists:nth(2, Expected),
    Expected2 = lists:nth(3, SplitLine),

    _ = binary_to_integer(lists:nth(4, SplitLine)),

    Expected3 = lists:nth(3, Expected),
    Expected3 = lists:nth(5, SplitLine),

    _ = binary_to_integer(lists:nth(6, SplitLine));
verify_load_line(Expected, LoadLine) when length(Expected) == 2 ->
    SplitLine = string:lexemes(LoadLine, " "),

    Expected1 = lists:nth(1, Expected),
    Expected1 = lists:nth(1, SplitLine),

    Expected2 = lists:nth(2, Expected),
    Expected2 = lists:nth(2, SplitLine),

    _ = binary_to_integer(lists:nth(3, SplitLine));
verify_load_line(Expected, LoadLine) when length(Expected) == 1 ->
    SplitLine = string:lexemes(LoadLine, " "),

    Expected1 = lists:nth(1, Expected),
    Expected1 = lists:nth(1, SplitLine),

    _ = binary_to_integer(lists:nth(2, SplitLine)).

verify_content_lines([]) ->
    ok;
verify_content_lines([Line | Rest]) ->
    SplitLine = string:lexemes(Line, " "),

    Match = re:run(lists:nth(1, SplitLine), ~S"<\d{1,5}\.\d{1,5}\.\d{1,5}>"),
    true = (Match /= nomatch),

    verify_name(lists:nth(2, SplitLine)),

    case lists:nth(3, SplitLine) of
        <<"'-'">> ->
            ok;
        Time ->
            _ = binary_to_integer(Time)
    end,

    _ = binary_to_integer(lists:nth(4, SplitLine)),
    _ = binary_to_integer(lists:nth(5, SplitLine)),
    _ = binary_to_integer(lists:nth(6, SplitLine)),

    verify_name(lists:nth(7, SplitLine)),

    verify_content_lines(Rest).

verify_name(Name) ->
    case string:split(Name, ":") of
        [_] ->
            ok;
        [_, []] ->
            ok;
        [_, Func] ->
            case string:split(Func, "/") of
                [_] ->
                    ok;
                [_, []] ->
                    ok;
                [_, _] ->
                    ok
            end
    end.

verify_etop_stop_message([<<"">>, ?COOKED_ENDING], cooked) ->
    ok;
verify_etop_stop_message([<<"">>, ?RAW_ENDING], raw) ->
    ok;
verify_etop_stop_message([<<"">>], _) ->
    ok.

get_good_output(Provider, ExpectedLineCount) ->
    Lines = Provider(),
    case length(Lines) of
        ExpectedLineCount ->
            {ok, Lines};
        _ ->
            {error, invalid_ouput}
    end.

get_good_output(Provider, ExpectedLineCount, DifferentThan) ->
    Lines = Provider(),
    case {length(Lines), Lines /= DifferentThan} of
        {ExpectedLineCount, true} ->
            {ok, Lines};
        _ ->
            {error, invalid_output}
    end.

wait_until_good_output(_GoodOutput, 0) ->
    {error, timeout};
wait_until_good_output(GoodOutput, Attempts) ->
    case GoodOutput() of
        {ok, Output} ->
            {ok, Output};
        _ ->
            timer:sleep(500),
            wait_until_good_output(GoodOutput, Attempts - 1)
    end.

wait_until(_Condition, 0) ->
    {error, timeout};
wait_until(Condition, Attempts) ->
    case Condition() of
        true ->
            ok;
        _ ->
            timer:sleep(500),
            wait_until(Condition, Attempts - 1)
    end.
