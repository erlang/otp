#!/usr/bin/env escript
%% -*- erlang -*-

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
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

-define(tmp_folder, "tmp").

main(Args) ->
    argparse:run(Args, cli(), #{progname => scancode}).

cli() ->
    #{ help =>
           """
           Run 'scancode' with multiple options
           """,
       arguments => [ scan_option(),
                      prefix_option(),
                      scan_results(),
                      file_or_dir() ],
       handler => fun scancode/1}.

approved() ->
    [ ~"mit", ~"agpl-3.0", ~"apache-2.0", ~"boost-1.0", ~"llvm-exception",
      ~"lgpl-2.1-plus", ~"cc0-1.0", ~"bsd-simplified", ~"bsd-new", ~"pcre",
      ~"fsf-free", ~"autoconf-exception-3.0", ~"mpl-1.1", ~"public-domain",
      ~"autoconf-simple-exception", ~"unicode", ~"tcl", ~"gpl-2.0 WITH classpath-exception-2.0",
      ~"zlib", ~"lgpl-2.0-plus WITH wxwindows-exception-3.1", ~"lgpl-2.0-plus",
      ~"openssl-ssleay", ~"cc-by-sa-3.0", ~"cc-by-4.0", ~"dco-1.1", ~"fsf-ap",
      ~"agpl-1.0-plus", ~"agpl-1.0", ~"agpl-3.0-plus", ~"classpath-exception-2.0",
      ~"ietf-trust"].

not_approved() ->
    [~"gpl", ~"gpl-3.0-plus", ~"gpl-2.0", ~"gpl-1.0-plus", ~"unlicense",
     ~"erlangpl-1.1", ~"gpl-2.0-plus", ~"null", 'null'].

scan_option() ->
    #{name => scan_option,
      type => string,
      default => "cli",
      long => "-scan-option",
      help => "scancode options to pass to the escript."}.

prefix_option() ->
    #{name => prefix,
      type => string,
      default => "",
      long => "-prefix",
      help => "Prefix used for all paths (main use case is Github CI)."}.

scan_results() ->
    #{name => scan_results,
      type => string,
      default => "scan-results.json",
      long => "-scan_results",
      help => "Output file where to scan the results."}.

file_or_dir() ->
    #{name => file_or_dir,
      type => string,
      required => true,
      long => "-file-or-dir",
      help => "Files and/or directories to analyse."}.

scancode(Config) ->
    ok = cp_files(Config),
    scan_folder(Config).

cp_files(#{file_or_dir := FilesOrDirs,
           prefix      := Prefix}) ->
    ok    = create_folder(Prefix, ?tmp_folder),
    Files = cleanup_files(FilesOrDirs),
    lists:foreach(fun (File) ->
                         Command = cp_with_path(Prefix, File, ?tmp_folder),
                         os:cmd(Command)
                 end, Files),
    ok.

create_folder(Prefix, Folder) ->
    [] = os:cmd("mkdir " ++ Prefix ++ Folder),
    ok.

cleanup_files(FilesOrDirs) ->
    lists:filter(fun ([]) -> false; (_) -> true end,
                 string:split(FilesOrDirs, " ", all)).

cp_with_path(Prefix, File, Folder) ->
    "cp -f --parents " ++ Prefix ++ File ++ " " ++ Folder.

scan_folder(Config) ->
    Command = scancode_command(Config),
    execute(Command, Config).

scan_result_path(#{scan_results  := ScanResult,
                   prefix        := Prefix}) ->
    Prefix ++ ScanResult.

folder_path(#{prefix        := Prefix}) ->
    Prefix ++ ?tmp_folder.

scancode_command(#{scan_option   := Options}=Config) ->
    ScanResultPath = scan_result_path(Config),
    FolderPath = folder_path(Config),
    "scancode -" ++ Options ++ " --json-pp " ++ ScanResultPath ++ " " ++ FolderPath.

execute(Command, Config) ->
    _ = os:cmd(Command),
    ScanResult = scan_result_path(Config),
    Json = decode(ScanResult),
    Licenses = fetch_licenses(Json),

    Errors = compliance_check(Licenses),
    io:format("~n~nResuling Errors: ~p~n~n", [Errors]),
    case Errors of
        [] ->
            ok;
        _ ->
            error(Errors)
    end.

compliance_check(Licenses) when is_list(Licenses) ->
    lists:filtermap(fun (License) ->
                            case compliance_check(License) of
                                ok ->
                                    false;
                                {error, Err} ->
                                    {true, Err}
                            end
                    end, Licenses);
compliance_check({Path, 'null'=License}) ->
    {error, {License, Path, no_license}};
compliance_check({Path, License}) ->
    case lists:member(License, not_approved()) of
        true ->
            {error, {License, Path, license_not_approved}};
        false ->
            case lists:member(License, approved()) of
                false ->
                    %% this can happen if a license is
                    %% not in the approve/not_approved list
                    {error, {License, Path, license_not_recognised}};
                true ->
                    ok
            end
    end.

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

fetch_licenses(#{<<"files">> := Files}) ->
    lists:filtermap(fun(#{<<"type">> := <<"file">>,
                          <<"detected_license_expression">> := License,
                          <<"path">> := Path}) ->
                            {true, {Path, License}};
                       (_) ->
                            false
                    end, Files).
