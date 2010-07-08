#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010. All Rights Reserved.
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

%%% <script> [-I<dir>]... [-o<dir>] [-module Module] [File]
%%%
%%% Use EDoc and the layout module 'otp_specs' to create an XML file
%%% containing Dialyzer types and specifications (-type, -spec).
%%%
%%% Options:
%%%
%%%  "-o<dir>" The output directory for the created file.
%%%            Default is ".".
%%%  "-I<dir>" Directory to be searched when including a file.
%%%  "-module Module"
%%%            Module name to use when there is no File argument.
%%%            A temporary file will be created.
%%%            Exactly one of -module Module and File must be given.
%%%
%%% The name of the generated file is "specs_<module>.xml". Its exact
%%% format is not further described here.

main(Args) ->
    case catch parse(Args, [], ".", no_module) of
        {ok, FileSpec, InclFs, Dir} ->
            call_edoc(FileSpec, InclFs, Dir);
        {error, Msg} ->
            io:format("~s\n", [Msg]),
            usage()
    end.

parse(["-o"++Dir | Opts], InclFs, _, Module) ->
    parse(Opts, InclFs, Dir, Module);
parse(["-I"++I | Opts], InclFs, Dir, Module) ->
    parse(Opts, [I | InclFs], Dir, Module);
parse(["-module", Module | Opts], InclFs, Dir, _) ->
    parse(Opts, InclFs, Dir, Module);
parse([File], InclFs, Dir, no_module) ->
    {ok, {file, File}, lists:reverse(InclFs), Dir};
parse([_], _, _, _) ->
    {error, io_lib:format("Cannot have both -module option and file", [])};
parse([], _, _, no_module) ->
    {error, io_lib:format("Missing -module option or file", [])};
parse([], InclFs, Dir, Module) ->
    {ok, {module, Module}, lists:reverse(InclFs), Dir};
parse(Args, _, _, _) ->
    {error, io_lib:format("Bad arguments: ~p", [Args])}.

usage() ->
    io:format("usage:  ~s [-I<include_dir>]... [-o<out_dir>] "
              "[-module <module>] [file]\n", [escript:script_name()]),
    halt(1).

call_edoc(FileSpec, InclFs, Dir) ->
    Incl = [{includes, InclFs}],
    Pre = [{preprocess, true}],
    Choice = [{dialyzer_specs, all}],
    DirOpt = [{dir, Dir}],
    Pretty = [{pretty_print, erl_pp}],
    Layout = [{layout, otp_specs},
              {file_suffix, ".specs"},
              {stylesheet, ""}],
    Warn = [{report_missing_type, false},
            {report_type_mismatch, false}],
    OptionList = (DirOpt ++ Choice ++ Pre ++ Warn ++ Pretty ++ Layout ++ Incl),
    {File, TmpFile} = case FileSpec of
                          {file, File0} ->
                              {File0, false};
                          {module, Module} ->
                              {create_tmp_file(Dir, Module), true}
                      end,
    try edoc:files([File], OptionList) of
        ok ->
            clean_up(Dir, File, TmpFile),
            rename(Dir, File)
    catch
        _:_ ->
            io:format("EDoc could not process file '~s'\n", [File]),
            clean_up(Dir, File, TmpFile),
            halt(3)
    end.

rename(Dir, F) ->
    Mod = filename:basename(F, ".erl"),
    Old = filename:join(Dir, Mod ++ ".specs"),
    New = filename:join(Dir, "specs_" ++ Mod ++ ".xml"),
    case file:rename(Old, New) of
        ok ->
            ok;
        {error, R} ->
            R1 = file:format_error(R),
            io:format("could not rename file '~s': ~s\n", [New, R1]),
            halt(2)
    end.

clean_up(Dir, File, TmpFile) ->
    [file:delete(File) || TmpFile],
    _ = [file:delete(filename:join(Dir, F)) ||
            F <- ["packages-frame.html",
                  "overview-summary.html",
                  "modules-frame.html",
                  "index.html", "erlang.png", "edoc-info"]],
    ok.

create_tmp_file(Dir, Module) ->
    TmpFile = filename:join(Dir, Module++".erl"),
    case file:write_file(TmpFile, "-module(" ++ Module ++ ").\n") of
        ok ->
            TmpFile;
        {error, R} ->
            R1 = file:format_error(R),
            io:format("could not write file '~s': ~s\n", [TmpFile, R1]),
            halt(2)
    end.
