%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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

-export([bif_highlight/1, indent/1]).

all() ->
    [bif_highlight, indent].

init_per_testcase(_Case, Config) ->
    ErlangEl = filename:join([code:lib_dir(tools),"emacs","erlang.el"]),
    case file:read_file_info(ErlangEl) of
	{ok, _} ->
	    [{el, ErlangEl}|Config];
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
    [_H,IntMatch,_T] = 
	re:split(Bin,<<"defvar ",Tag/binary,
		       "[^(]*\\(([^)]*)">>,[]),
    EmacsIntBifs = [list_to_atom(S) || 
		  S <- string:tokens(binary_to_list(IntMatch)," '\"\n")],
    
    ct:log("Emacs ~p",[EmacsIntBifs]),
    ct:log("Int ~p",[Compare]),

    ct:log("Diff1 ~p",[Compare -- EmacsIntBifs]),
    ct:log("Diff2 ~p",[EmacsIntBifs -- Compare]),
    [] = Compare -- EmacsIntBifs,
    [] = EmacsIntBifs -- Compare.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

indent(Config) ->
    case emacs_version_ok() of
        false -> {skip, "Old or no emacs found"};
        true ->
            Def = filename:dirname(code:which(?MODULE)) ++ "/" ++ ?MODULE_STRING ++ "_data",
            Dir = proplists:get_value(data_dir, Config, Def),
            OrigFs = filelib:wildcard(Dir ++ "/*"),
            io:format("Dir: ~s~nFs: ~p~n", [Dir, OrigFs]),
            Fs = [{File, unindent(File)} || File <- OrigFs,
                                            filename:extension(File) =:= ""],
            Indent = fun emacs/1,
            [Indent(File) || {_, File} <- Fs],
            Res = [diff(Orig, File) || {Orig, File} <- Fs],
            [file:delete(File) || {ok, File} <- Res],       %% Cleanup
            [] = [Fail || {fail, Fail} <- Res],
            ok
    end.

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

emacs_version_ok() ->
    case os:cmd("emacs --version | head -1") of
        "GNU Emacs " ++ Ver ->
            case string:to_float(Ver) of
                {Vsn, _} when Vsn >= 24.1 ->
                    true;
                _ ->
                    io:format("Emacs version fail~n~s~n~n",[Ver]),
                    false
            end;
        Res ->
            io:format("Emacs version fail~n~s~n~n",[Res]),
            false
    end.

emacs(File) ->
    EmacsErlDir = filename:join([code:lib_dir(tools), "emacs"]),
    Cmd = ["emacs ",
           "--batch --quick ",
           "--directory ", EmacsErlDir, " ",
           "--eval \"(require 'erlang-start)\" ",
           File, " ",
           "--eval '(indent-region (point-min) (point-max) nil)' ",
           "--eval '(save-buffer 0)'"
          ],
    _Res = os:cmd(Cmd),
    % io:format("cmd ~s:~n=> ~s~n", [Cmd, _Res]),
    ok.
