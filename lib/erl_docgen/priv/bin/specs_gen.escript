#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2011-2016. All Rights Reserved.
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

%%% <script> [-I<dir>]... [-o<dir>] [-module Module] [File]
%%%
%%% Use EDoc and the layout module 'docgen_otp_specs' to create an XML file
%%% containing Dialyzer types and specifications (-type, -spec).
%%%
%%% Options:
%%%
%%%  "-o<dir>" The output directory for the created file.
%%%            Default is ".".
%%%  "-I<dir>" Directory to be searched when including a file.
%%%  "-module Module"
%%%            Module name to use when there is no File argument.
%%%            A empty specifications file will be created.
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
    ReadOpts = [{includes, InclFs}, {preprocess, true}],
    ExtractOpts = [{report_missing_type, false}],
    LayoutOpts = [{pretty_printer, erl_pp}, {layout, docgen_otp_specs}],
    File = case FileSpec of
               {file, File0} -> File0;
               {module, Module0} -> Module0
           end,
    try
        Fs = case FileSpec of
                 {file, _} ->
                     Fs0 = read_file(File, ReadOpts),
                     clauses(Fs0);
                 {module, Module} ->
                     [{attribute,0,module,list_to_atom(Module)}]
             end,
        Doc = extract(File, Fs, ExtractOpts),
        Text = edoc:layout(Doc, LayoutOpts),
        ok = write_text(Text, File, Dir),
        rename(Dir, File)
    catch
        _:_ ->
            io:format("EDoc could not process file '~s'\n", [File]),
            clean_up(Dir),
            halt(3)
    end.

read_file(File, Opts) ->
    edoc:read_source(File, Opts).

extract(File, Forms, Opts) ->
    Env = edoc_lib:get_doc_env([], [], _Opts=[]),
    {_Module, Doc} = edoc_extract:source(Forms, File, Env, Opts),
    Doc.

clauses(Fs) ->
    clauses(Fs, no).

clauses([], no) ->
    [];
clauses([F | Fs], Spec) ->
    case F of
        {attribute,_,spec,_} ->
            clauses(Fs, F);
        {function,_,_N,_A,_Cls} when Spec =/= no->
            {attribute,_,spec,{Name,FunTypes}} = Spec,
            %% [throw({no,Name,{_N,_A}}) || Name =/= {_N,_A}],
            %% EDoc doesn't care if a function appears more than once;
            %% this is how overloaded specs are handled:
            (lists:append([[setelement(4, Spec, {Name,[T]}),F] ||
                              T <- FunTypes])
             ++ clauses(Fs, no));
        _ ->
            [F | clauses(Fs, Spec)]
    end.

write_text(Text, File, Dir) ->
    Base = filename:basename(File, ".erl"),
    OutFile = filename:join(Dir, Base) ++ ".specs",
    case file:write_file(OutFile, Text) of
        ok ->
            ok;
        {error, R} ->
            R1 = file:format_error(R),
            io:format("could not write file '~s': ~s\n", [File, R1]),
            halt(2)
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

clean_up(Dir) ->
    _ = [file:delete(filename:join(Dir, F)) ||
            F <- ["packages-frame.html",
                  "overview-summary.html",
                  "modules-frame.html",
                  "index.html", "erlang.png", "edoc-info"]],
    ok.
