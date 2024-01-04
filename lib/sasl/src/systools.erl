%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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
-module(systools).

%% Usage:
%%    systools:make_script("RelName")     
%%                        Make a boot file from RelName.rel.
%%                        Generates RelName.{script,boot}
%%    systools:make_tar("RelName")     
%%                        Make a release package from RelName.rel.
%%                        Generates RelName.tar,Z
%%    systools:script2boot(File)
%%                        File.script -> File.boot
%%    systools:mk_relup("Target", ["UpFromRel"...], ["DownToRel"...], Opts)
%%			  Gather all relup scripts to the relup file
%%

-export([script2boot/1, script2boot/3, compile_rel/3,
	 make_script/1, make_script/2,
	 make_tar/1, make_tar/2,
	 make_relup/3, make_relup/4]).

-include("erl_compile.hrl").

%%% The behaviour_info functions have been moved to erl_internal in stdlib.

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent | local where path sets
%% the search path, silent suppresses error message printing on console,
%% local generates a script with references to the directories there
%% the applications are found.
%%-----------------------------------------------------------------
-spec make_script(Name) -> Result when
      Name :: string(),
      Result :: ok | error | {ok,Module,Warnings} | {error,Module,Error},
      Module :: atom(),
      Warnings :: term(),
      Error :: term().
make_script([RelName|Opts]) when is_atom(RelName) ->
    systools_make:make_script([RelName], Opts);
make_script(RelName) -> make_script(RelName, []).

-spec make_script(Name, [Opt]) -> Result when
      Name :: string(),
      Opt :: src_tests | {path,[Dir]} | local | {variables,[Var]} | exref | {exref,[App]} |
             silent | {outdir,Dir} | no_dot_erlang | no_warn_sasl | warnings_as_errors | {script_name, Name},
      Dir :: string(),
      Var :: {VarName,Prefix},
      VarName :: string(),
      Prefix :: string(),
      App :: atom(),
      Result :: ok | error | {ok,Module,Warnings} | {error,Module,Error},
      Module :: atom(),
      Warnings :: term(),
      Error :: term().
make_script(RelName, Opt) ->
    systools_make:make_script(RelName, Opt).

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent |
%%    {dirs, [src,include,examples,..]} | {erts, ErtsDir} where path
%% sets the search path, silent suppresses error message printing on console,
%% dirs includes the specified directories (per application) in the
%% release package and erts specifies that the erts-Vsn/bin directory
%% should be included in the release package and there it can be found.
%%-----------------------------------------------------------------
-spec make_tar(Name) -> Result when
      Name :: string(),
      Result :: ok | error | {ok, Module :: module(), Warnings :: term()} |
                {error, Module :: module(), Error :: term()}.
make_tar(RelName) -> make_tar(RelName, []).

-spec make_tar(Name, Opts) -> Result when
      Name :: string(),
      Opts :: [Opt],
      Opt :: {dirs,[IncDir]} | {path,[Dir]} |
             {variables,[Var]} | {var_tar,VarTar} |
             {erts,Dir} | erts_all | src_tests | exref |
             {exref,[App]} | silent | {outdir,Dir} |
             no_warn_sasl | warnings_as_errors |
             {extra_files, ExtraFiles},
      Dir :: file:filename_all(),
      IncDir :: src | include | atom(),
      Var :: {VarName,PreFix},
      VarName :: string(),
      PreFix :: string(),
      VarTar :: include | ownfile | omit,
      App :: atom(),
      Result :: ok | error | {ok, Module :: module(), Warnings :: term()} |
                {error, Module :: module(), Error :: term()},
      ExtraFiles :: [{NameInArchive, file:filename_all()}],
      NameInArchive :: string().
make_tar(RelName, Opt) ->
    systools_make:make_tar(RelName, Opt).

%%-----------------------------------------------------------------
%% Create a binary form of a boot script.
%%-----------------------------------------------------------------
-spec script2boot(File) -> ok | error when File :: string().
script2boot(File) ->
    case systools_lib:file_term2binary(File ++ ".script", File ++ ".boot") of
	{error,Error} ->
	    io:format("~ts", [systools_make:format_error(Error)]),
	    error;
	_ ->
	    ok
    end.

script2boot(File, Output0, _Opt) ->
    Input = File++".script",
    Output = Output0++".boot",
    case systools_lib:file_term2binary(Input, Output) of
	{error,Error} ->
	    io:format("~ts", [systools_make:format_error(Error)]),
	    error;
	_ ->
	    ok
    end.

%%-----------------------------------------------------------------
%% Options is a list of {path, Path} | silent | noexec where path sets
%% search path, silent suppresses error message printing on console,
%% noexec suppresses writing the output "relup" file
%%-----------------------------------------------------------------
-spec make_relup(Name, UpFrom, DownTo) -> Result when Name :: string(),
   UpFrom :: [Name | {Name,Descr}],
   DownTo :: [Name | {Name,Descr}],
    Descr :: term(),
   Result :: ok | error | {ok,Relup :: term(),Module,Warnings} | {error,Module,Error},
    Module :: atom(),
    Warnings :: term(),
   Error :: term().
make_relup(ReleaseName, UpNameList, DownNameList) ->
    systools_relup:mk_relup(ReleaseName, UpNameList, DownNameList, []).
-spec make_relup(Name, UpFrom, DownTo, [Opt]) -> Result
                    when
                        Name :: string(),
                        UpFrom :: [Name | {Name, Descr}],
                        DownTo :: [Name | {Name, Descr}],
                        Descr :: term(),
                        Opt ::
                            {path, [Dir]} |
                            restart_emulator | silent | noexec |
                            {outdir, Dir} |
                            warnings_as_errors,
                        Dir :: string(),
                        Result ::
                            ok | error |
                            {ok, Relup :: term(), Module, Warnings} |
                            {error, Module, Error},
                        Module :: atom(),
                        Warnings :: term(),
                        Error :: term().
make_relup(ReleaseName, UpNameList, DownNameList, Opts) ->
    systools_relup:mk_relup(ReleaseName, UpNameList, DownNameList, Opts).

%%-----------------------------------------------------------------
%% Interface for erl_compile to compile .rel files.
%%-----------------------------------------------------------------
compile_rel(Input, Output, Options) ->
    systools_make:make_script(Input, Output, translate_options(Options)).

translate_options(Opts) ->
    [{path, Opts#options.includes}|Opts#options.specific].
