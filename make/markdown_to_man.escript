#!/usr/bin/env escript
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2024-2025. All Rights Reserved.
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
-module(markdown_to_man).
-include_lib("kernel/include/eep48.hrl").

-export([main/1]).

main(Args) ->
    try
        case parse_args(Args, ".", []) of
            {_,[]} ->
                ok;
            {OutDir,[_|_]=Files} ->
                convert_files(Files, OutDir)
        end
    catch
        throw:{error,Error} ->
            io:put_chars(standard_error, Error)
    end.

parse_args(["-o",OutDir|As], _OutDir, FilesAcc) ->
    parse_args(As, OutDir, FilesAcc);
parse_args([F|Fs], OutDir, FilesAcc) ->
    parse_args(Fs, OutDir, [F|FilesAcc]);
parse_args([], OutDir, FilesAcc) ->
    {OutDir,lists:reverse(FilesAcc)}.

convert_files([F|Fs], OutDir) ->
    convert_file(F, OutDir),
    convert_files(Fs, OutDir);
convert_files([], _) ->
    ok.

convert_file(Name, OutDir) ->
    case filename:extension(Name) of
        ".md" ->
            Base0 = filename:rootname(filename:basename(Name), ".md"),
            case lists:reverse(Base0) of
                    "dmc_" ++ Base1 ->
                            Base2 = lists:reverse(Base1),
                            convert_markdown_to_man(Base2, OutDir, Name, ".1");
                    _ ->
                            convert_markdown_to_man(Base0, OutDir, Name, ".3")
                end;
        ".erl" ->
            Base0 = filename:rootname(filename:basename(Name), ".erl"),
            Output = man_docs:module_to_manpage(list_to_atom(Base0), Name),
            Outfile = filename:join(OutDir, Base0 ++ ".3"),
            _ = filelib:ensure_dir(Outfile),
            case Output =/= <<>> andalso file:write_file(Outfile, Output) of
                ok ->
                    ok;
                false ->
                    ok; %% No documentation to write
                {error,Reason0} ->
                    Reason = file:format_error(Reason0),
                    fail(io_lib:format("~ts: write failed: ~ts",
                                       [Outfile,Reason]))
            end
    end.

convert_markdown_to_man(Base, OutDir, Name, Section) ->
    OutFile = filename:join(OutDir, Base ++ Section),
    _ = filelib:ensure_dir(OutFile),
    case file:read_file(Name) of
        {ok,Markdown} -> 
            Man = man_docs:markdown_to_manpage(Markdown, Name),
            case file:write_file(OutFile, Man) of
                ok ->
                    ok;
                {error,Reason0} ->
                    Reason = file:format_error(Reason0),
                    fail(io_lib:format("~ts: write failed: ~ts",
                                       [OutFile,Reason]))
            end;
        {error,Reason0} ->
            Reason = file:format_error(Reason0),
            fail(io_lib:format("~ts: ~ts", [Name,Reason]))
    end.

fail(String) ->
    E = io_lib:format("~p: ~ts\n", [?MODULE,String]),
    throw({error,E}).
