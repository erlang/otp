%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright 2004-2010 held by the authors. All Rights Reserved.
%% Copyright Ericsson AB 2017-2025. All Rights Reserved.
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

%%-----------------------------------------------------------------------
%% File        : typer.erl
%% Author(s)   : The first version of typer was written by Bingwen He
%%               with guidance from Kostis Sagonas and Tobias Lindahl.
%%               Since June 2008 typer is maintained by Kostis Sagonas.
%%               On 2022, Brujo, Pablo, and Mackenzie from NextRoll
%%               started working on the rebar3 plugin for typer and,
%%               with that in mind, split typer and typer_core apart.
%% Description : An Erlang/OTP application that shows type information
%%               for Erlang modules to the user.  Additionally, it can
%%               annotate the code of files with such type information.
%%-----------------------------------------------------------------------

-module(typer).
-moduledoc false.

-export([start/0]).

%%-----------------------------------------------------------------------

-define(SHOW, show).
-define(SHOW_EXPORTED, show_exported).
-define(ANNOTATE, annotate).
-define(ANNOTATE_IN_PLACE, annotate_in_place).
-define(ANNOTATE_INC_FILES, annotate_inc_files).

%%-----------------------------------------------------------------------

-spec start() -> no_return().

start() ->
  _ = io:setopts(standard_error, [{encoding,unicode}]),
  _ = io:setopts([{encoding,unicode}]),
  ok = typer_core:run(process_cl_args()),
  erlang:halt(0).

%%--------------------------------------------------------------------
%% Processing of command-line options and arguments.
%%--------------------------------------------------------------------

-spec process_cl_args() -> typer_core:opts().

process_cl_args() ->
  ArgList = init:get_plain_arguments(),
  %% io:format("Args is ~tp\n", [ArgList]),
  Opts = analyze_args(ArgList, #{}),
  %% if the mode has not been set, set it to the default mode (show)
  case Opts of
    #{mode := _} -> Opts;
    Opts -> Opts#{mode => ?SHOW}
  end.

analyze_args([], Opts) ->
  Opts;
analyze_args(ArgList, Opts) ->
  {Result, Rest} = cl(ArgList),
  NewOpts = analyze_result(Result, Opts),
  analyze_args(Rest, NewOpts).

cl(["-h"|_])     -> help_message();
cl(["--help"|_]) -> help_message();
cl(["-v"|_])        -> version_message();
cl(["--version"|_]) -> version_message();
cl(["--edoc"|Opts]) -> {edoc, Opts};
cl(["--show"|Opts]) -> {{mode, ?SHOW}, Opts};
cl(["--show_exported"|Opts]) -> {{mode, ?SHOW_EXPORTED}, Opts};
cl(["--show-exported"|Opts]) -> {{mode, ?SHOW_EXPORTED}, Opts};
cl(["--show_success_typings"|Opts]) -> {show_succ, Opts};
cl(["--show-success-typings"|Opts]) -> {show_succ, Opts};
cl(["--annotate"|Opts]) -> {{mode, ?ANNOTATE}, Opts};
cl(["--annotate-inc-files"|Opts]) -> {{mode, ?ANNOTATE_INC_FILES}, Opts};
cl(["--annotate-in-place"|Opts]) -> {{mode, ?ANNOTATE_IN_PLACE}, Opts};
cl(["--no_spec"|Opts]) -> {no_spec, Opts};
cl(["--plt",Plt|Opts]) -> {{plt, Plt}, Opts};
cl(["-D"|_Opts]) -> fatal_error("no variable name specified after -D");
cl(["-D"++Def|Opts]) ->
  DefPair = process_def_list(re:split(Def, "=", [{return, list}, unicode])),
  {{def, DefPair}, Opts};
cl(["-I",Dir|Opts]) -> {{inc, Dir}, Opts};
cl(["-I"|_Opts]) -> fatal_error("no include directory specified after -I");
cl(["-I"++Dir|Opts]) -> {{inc, Dir}, Opts};
cl(["-T"|Opts]) ->
  {Files, RestOpts} = collect_args(Opts),
  case Files of
    [] -> fatal_error("no file or directory specified after -T");
    [_|_] -> {{trusted, Files}, RestOpts}
  end;
cl(["-r"|Opts]) ->
  {Files, RestOpts} = collect_args(Opts),
  {{files_r, Files}, RestOpts};
cl(["-pa",Dir|Opts]) -> {{pa,Dir}, Opts};
cl(["-pz",Dir|Opts]) -> {{pz,Dir}, Opts};
cl(["-"++H|_]) -> fatal_error("unknown option -"++H);
cl(Opts) ->
  {Files, RestOpts} = collect_args(Opts),
  {{files, Files}, RestOpts}.

-spec collect_args([string()]) -> {[string()], [string()]}.

collect_args(List) ->
    collect_args_1(List, []).

collect_args_1(["-"++_|_] = L, Acc) ->
    {lists:reverse(Acc), L};
collect_args_1([Arg|T], Acc) ->
    collect_args_1(T, [Arg|Acc]);
collect_args_1([], Acc) ->
    {lists:reverse(Acc), []}.

process_def_list(L) ->
  case L of
    [Name, Value] ->
      {ok, Tokens, _} = erl_scan:string(Value ++ "."),
      {ok, ErlValue} = erl_parse:parse_term(Tokens),
      {list_to_atom(Name), ErlValue};
    [Name] ->
      {list_to_atom(Name), true}
  end.

%% Get information about files that the user trusts and wants to analyze
analyze_result({files, Val}, Opts) ->
  append_in_map(files, Val, Opts);
analyze_result({files_r, Val}, Opts) ->
  append_in_map(files_r, Val, Opts);
analyze_result({trusted, Val}, Opts) ->
  append_in_map(trusted, Val, Opts);
analyze_result(edoc, Opts) ->
  Opts#{edoc => true};
%% Get useful information for actual analysis
analyze_result({mode, Mode}, #{mode := OldMode}) ->
  mode_error(OldMode, Mode);
analyze_result({mode, Mode}, Opts) ->
  Opts#{mode => Mode};
analyze_result({def, Val}, Opts) ->
  append_in_map(macros, [Val], Opts);
analyze_result({inc, Val}, Opts) ->
  append_in_map(includes, [Val], Opts);
analyze_result({plt, Plt}, Opts) ->
  Opts#{plt => Plt};
analyze_result(show_succ, Opts) ->
  Opts#{show_succ => true};
analyze_result(no_spec, Opts) ->
  Opts#{no_spec => true};
analyze_result({pa, Dir}, Opts) ->
  true = code:add_patha(Dir),
  Opts;
analyze_result({pz, Dir}, Opts) ->
  true = code:add_pathz(Dir),
  Opts.

append_in_map(Key, List, Map) ->
  maps:update_with(Key, fun(L) -> L ++ List end, List, Map).

%%--------------------------------------------------------------------
%% Utilities for error reporting.
%%--------------------------------------------------------------------

-spec fatal_error(string()) -> no_return().

fatal_error(Slogan) ->
  msg(io_lib:format("typer: ~ts\n", [Slogan])),
  erlang:halt(1).

-spec mode_error(typer_core:mode(), typer_core:mode()) -> no_return().

mode_error(OldMode, NewMode) ->
  Msg = io_lib:format("Mode was previously set to '~s'; "
		      "cannot set it to '~s' now",
		      [OldMode, NewMode]),
  fatal_error(Msg).

-spec msg(string()) -> 'ok'.

msg(Msg) ->
  io:format(standard_error, "~ts", [Msg]).

%%--------------------------------------------------------------------
%% Version and help messages.
%%--------------------------------------------------------------------

-spec version_message() -> no_return().

version_message() ->
  io:format("TypEr version "++?VSN++"\n"),
  erlang:halt(0).

-spec help_message() -> no_return().

help_message() ->
  S = <<" Usage: typer [--help] [--version] [--plt PLT] [--edoc]
              [--show | --show-exported | --annotate | --annotate-inc-files | --annotate-in-place]
              [-Ddefine]* [-I include_dir]* [-pa dir]* [-pz dir]*
              [-T application]* [-r] file*

 Options:
   -r dir*
       search directories recursively for .erl files below them
   --show
       Prints type specifications for all functions on stdout.
       (this is the default behaviour; this option is not really needed)
   --show-exported (or --show_exported)
       Same as --show, but prints specifications for exported functions only
       Specs are displayed sorted alphabetically on the function's name
   --annotate
       Annotates the specified files with type specifications and writes
       the resulting files into a new typer_ann folder.
   --annotate-inc-files
       Same as --annotate but annotates all -include() files as well as
       all .erl files
   --annotate-in-place
       Annotate directly on the source code files, instead of dumping the
       annotated files in a different directory
   --edoc
       Prints type information as Edoc @spec comments, not as type specs
   --plt PLT
       Use the specified dialyzer PLT file rather than the default one
       (Incremental and non-incremental PLT files are supported)
   -T file*
       The specified file(s) already contain type specifications and these
       are to be trusted in order to print specs for the rest of the files
       (Multiple files or dirs, separated by spaces, can be specified.)
   -Dname (or -Dname=value)
       pass the defined name(s) to TypEr
       (The syntax of defines is the same as that used by \"erlc\".)
   -I include_dir
       pass the include_dir to TypEr
       (The syntax of includes is the same as that used by \"erlc\".)
   -pa dir
   -pz dir
       Set code path options to TypEr
       (This is useful for files that use parse transforms.)
   --version (or -v)
       prints the Typer version and exits
   --help (or -h)
       prints this message and exits

 Note:
   * denotes that multiple occurrences of these options are possible.
">>,
  io:put_chars(S),
  erlang:halt(0).
