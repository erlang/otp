%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2018. All Rights Reserved.
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
-module(emacs_SUITE).

%%-define(line_trace, 1).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([bif_highlight/1,
         load_interpreted/1,  compile_and_load/1,
         indent/1,
         tests_interpreted/1, tests_compiled/1
        ]).

all() ->
    [bif_highlight, load_interpreted, compile_and_load,
     indent,
     tests_interpreted, tests_compiled
    ].

init_per_testcase(Case, Config) ->
    ErlangEl = filename:join([code:lib_dir(tools),"emacs","erlang.el"]),
    case file:read_file_info(ErlangEl) of
        {ok, _} ->
            case Case =:= bif_highlight orelse emacs_version_ok(24.3) of
                false -> {skip, "Old or no emacs found"};
                _ -> [{el, ErlangEl}|Config]
            end;
        _ ->
            {skip, "Could not find erlang.el"}
    end.

end_per_testcase(_Case, _Config) ->
    ok.

bif_highlight(Config) ->
    ErlangEl = proplists:get_value(el,Config),
    {ok, Bin} = file:read_file(ErlangEl),

    %% All auto-imported bifs
    IntBifs = lists:usort(
                [F  || {F,A} <- erlang:module_info(exports),
                       erl_internal:bif(F,A)]),

    %% all bif which need erlang: prefix and are not operands
    ExtBifs = lists:usort(
                [F  || {F,A} <- erlang:module_info(exports),
                       not erl_internal:bif(F,A) andalso
                           not is_atom(catch erl_internal:op_type(F,A))]),

    check_bif_highlight(Bin, <<"erlang-int-bifs">>, IntBifs),
    check_bif_highlight(Bin, <<"erlang-ext-bifs">>, ExtBifs).


check_bif_highlight(Bin, Tag, Compare) ->
    [_H,Match,_T] =
        re:split(Bin,<<"defvar ",Tag/binary,
                       "[^(]*\\(([^)]*)">>,[]),
    EmacsBifs = [list_to_atom(S) ||
                  S <- string:tokens(binary_to_list(Match)," '\"\n")],

    ct:log("Comparing ~s", [Tag]),
    ct:log("Emacs ~p",[EmacsBifs]),
    ct:log("Erlang ~p",[Compare]),

    ct:log("Only in Erlang ~p",[Compare -- EmacsBifs]),
    ct:log("Only in Emacs ~p",[EmacsBifs -- Compare]),
    [] = Compare -- EmacsBifs,
    [] = EmacsBifs -- Compare.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load_interpreted(_Config) ->
    _ = emacs(["-l erlang.el -f erlang-mode"]),
    ok.

compile_and_load(_Config) ->
    Dir = emacs_dir(),
    Files0 = filelib:wildcard("*.el", Dir),
    Files = case emacs_version_ok(24.3) of
                %% erldoc.el depends on cl-lib which was introduced in 24.3.
                false -> Files0 -- ["erldoc.el"];
                _ -> Files0
            end,
    Unforgiving =
        case emacs_version_ok(24) of
            Ver when Ver < 25 ->
                "";
            Ver when Ver < 26 ->
                %% Workaround byte-compile-error-on-warn which seem broken in
                %% Emacs 25.
                "\"(advice-add #'display-warning :after "
                    "(lambda (_ f _ _) (error \"%s\" f)))\"";
            _ ->
                "\"(setq byte-compile-error-on-warn t)\""
        end,
    %% Add files here whenever they are cleaned of warnings.
    NoWarn = ["erlang.el", "erlang-test.el", "erlang-edoc.el", "erlang-start.el", "erldoc.el"],
    Compile = fun(File) ->
                      Pedantic = case lists:member(File, NoWarn) andalso Unforgiving /= "" of
                                     true -> ["--eval ", Unforgiving, " "];
                                     false -> " "
                                 end,
                      emacs([Pedantic,
                             " -f batch-byte-compile ", dquote(filename:join(Dir, File))]),
                      true
              end,
    lists:foreach(Compile, Files),
    emacs(["-l erlang.elc -f erlang-mode"]),
    ok.

tests_interpreted(_Config) ->
    case emacs_version_ok(25) of
        false -> {skip, "Old or no emacs found"};
        _ ->
            emacs(["-l erlang.el ",
                   "-l erlang-test.el -f ert-run-tests-batch-and-exit"]),
            ok
    end.

tests_compiled(_Config) ->
    case emacs_version_ok(25) of
        false -> {skip, "Old or no emacs found"};
        _ ->
            emacs(["-l erlang.elc ",
                   "-l erlang-test.elc -f ert-run-tests-batch-and-exit"]),
            ok
    end.


dquote(Str) ->
    "\"" ++ Str ++ "\"".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indent(Config) ->
    Def = filename:dirname(code:which(?MODULE))
        ++ "/"
        ++ ?MODULE_STRING
        ++ "_data",
    Dir = proplists:get_value(data_dir, Config, Def),
    OrigFs = filelib:wildcard(Dir ++ "/*"),
    io:format("Dir: ~s~nFs: ~p~n", [Dir, OrigFs]),
    Fs = [{File, unindent(File)} || File <- OrigFs,
                                    filename:extension(File) =:= ""],
    Indent = fun(File) ->
                     emacs([
                            File, " ",
                            "--eval '(indent-region (point-min) (point-max) nil)' ",
                            "--eval '(save-buffer 0)'"
                           ]),
                     ok
             end,
    [Indent(File) || {_, File} <- Fs],
    Res = [diff(Orig, File) || {Orig, File} <- Fs],
    [file:delete(File) || {ok, File} <- Res],       %% Cleanup
    [] = [Fail || {fail, Fail} <- Res],
    ok.

unindent(Input) ->
    Output = Input ++ ".erl",
    {ok, Bin} = file:read_file(Input),
    Lines0 = string:split(Bin, "\n", all),
    Lines = [string:trim(Line, leading, [$\s,$\t]) || Line <- Lines0],
    %% io:format("File: ~s lines: ~w~n", [Input, length(Lines0)]),
    %% [io:format("~s~n", [L]) || L <- Lines],
    ok = file:write_file(Output, lists:join("\n", Lines)),
    Output.

diff(Orig, File) ->
    case os:cmd(["diff ", Orig, " ", File]) of
        "" -> {ok, File};
        Diff ->
            io:format("Fail: ~s vs ~s~n~s~n~n",[Orig, File, Diff]),
            {fail, File}
    end.

emacs_version_ok(AcceptVer) ->
    VersionLine = os:cmd("emacs --version | head -1"),
    io:format("~s~n", [VersionLine]),
    case VersionLine of
        "GNU Emacs " ++ Ver ->
            case string:to_float(Ver) of
                {Vsn, _} when Vsn >= AcceptVer ->
                    Vsn;
                _ ->
                    false
            end;
        Res ->
            io:format("Emacs version fail~n~s~n~n",[Res]),
            false
    end.

emacs(EmacsCmds) when is_list(EmacsCmds) ->
    Cmd = ["emacs ",
           "--batch --quick ",
           "--directory ", dquote(emacs_dir()), " ",
           "--eval \"(require 'erlang-start)\" "
           | EmacsCmds],
    Res0 = os:cmd(Cmd ++ " ; echo $?"),
    Rows = string:lexemes(Res0, ["\r\n", $\n]),
    Res = lists:last(Rows),
    Output = string:join(lists:droplast(Rows), "\n"),
    io:format("Cmd ~ts:~n  => ~s ~ts~n", [Cmd, Res, Output]),
    "0" = Res,
    Output.

emacs_dir() ->
    filename:join([code:lib_dir(tools), "emacs"]).
