#!/usr/bin/env escript
%% -*- erlang -*-

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2024. All Rights Reserved.
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

-define(tmp_folder, "tmp/").

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
                      file_or_dir(),
                      sarif_option() ],
       handler => fun scancode/1}.

approved() ->
    [ <<"apache-2.0">> ].

reviewed() ->
    [ <<"mit">>, <<"boost-1.0">>, <<"llvm-exception">>,
      <<"cc0-1.0">>, <<"bsd-simplified">>, <<"bsd-new">>, <<"pcre">>,
      <<"fsf-free">>, <<"autoconf-exception-3.0">>, <<"public-domain">>,
      <<"autoconf-simple-exception">>, <<"unicode">>, <<"tcl">>, <<"gpl-2.0 WITH classpath-exception-2.0">>,
      <<"zlib">>, <<"lgpl-2.0-plus WITH wxwindows-exception-3.1">>,
      <<"openssl-ssleay">>, <<"cc-by-sa-3.0">>, <<"cc-by-4.0">>, <<"dco-1.1">>, <<"fsf-ap">>,
      <<"classpath-exception-2.0">>, <<"ietf-trust">>, <<"apache-2.0-or-lgpl-2.1-or-later">> ].

not_approved() ->
    [<<"gpl">>, <<"gpl-3.0-plus">>, <<"gpl-2.0">>, <<"gpl-1.0-plus">>, <<"unlicense">>,
     <<"lgpl-2.0-plus">>, <<"lgpl-2.1-plus">>, <<"agpl-1.0-plus">>, <<"agpl-1.0">>,
     <<"agpl-3.0-plus">>, <<"erlangpl-1.1">>, <<"gpl-2.0-plus">>, <<"agpl-3.0">>, <<"mpl-1.1">>].

no_license() ->
    [<<"null">>, 'null'].

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

sarif_option() ->
    #{name => sarif,
      type => string,
      default => undefined,
      long => "-sarif"}.

scancode(Config) ->
    io:format("Files to scan: ~ts~n", [maps:get(file_or_dir, Config, none)]),
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
    io:format("Running: ~ts~n", [Command]),
    R = os:cmd(Command),
    io:format("Result: ~ts~n",[R]),
    ScanResult = scan_result_path(Config),
    Json = decode(ScanResult),
    Licenses = fetch_licenses(folder_path(Config), Json),

    Errors = compliance_check(Licenses),

    maps:get(sarif, Config) =/= undefined andalso
        sarif(maps:get(sarif, Config), Errors),

    Errors =/= [] andalso erlang:raise(exit, Errors, []),
    ok.

compliance_check(Licenses) when is_list(Licenses) ->
    lists:foldl(fun ({Path, License, SPDX0, Copyright}, Acc) ->
                        SPDX = spdx_nonnull(SPDX0),
                        CopyrightResult = check_copyright(Copyright),
                        LicenseResult = compliance_check(License),
                        R = lists:foldl(fun (ok, Acc0) -> Acc0;
                                            ({error, Msg}, Acc0) -> [{SPDX, Path, Msg} | Acc0]
                                        end, [], [CopyrightResult, LicenseResult]),
                        R ++ Acc
                    end, [], Licenses);
compliance_check(License) ->
    Handler = [ {no_license(), {error, no_license}},
                {not_approved(), {error, license_not_approved}},
                {reviewed(), {error, license_to_be_reviewed}},
                {approved(), ok}],
    license_check(License, Handler).

spdx_nonnull(null) ->
    <<"no license/copyright">>;
spdx_nonnull(X) ->
    X.

check_copyright([]) ->
    {error, no_copyright};
check_copyright([#{<<"copyright">> := _} | _]) ->
    ok.

license_check(License, Handler) ->
    lists:foldl(fun(_, {error, X}=Error) when X =/= license_not_recognised ->
                        Error;
                   ({Licenses, Msg}, Acc) ->
                        case lists:member(License, Licenses) of
                            true ->
                                Msg;
                            false ->
                                Acc
                        end
                end, {error, license_not_recognised}, Handler).


decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

fetch_licenses(FolderPath, #{<<"files">> := Files}) ->
    lists:filtermap(fun(#{<<"type">> := <<"file">>,
                          <<"detected_license_expression">> := License,
                          <<"detected_license_expression_spdx">> := SPDX,
                          <<"copyrights">> := Copyrights,
                          <<"path">> := Path}) ->
                            {true, {string:trim(Path, leading, FolderPath), License, SPDX, Copyrights}};
                       (_) ->
                            false
                    end, Files).

sarif(SarifFile, Errors) ->
    file:write_file(SarifFile, sarif(Errors)).
sarif(Errors) ->
    ErrorTypes = lists:usort([{Type, License} || {License, _File, Type} <- Errors]),
    ErrorTypesIndex = lists:zip(ErrorTypes, lists:seq(0,length(ErrorTypes) - 1)),
    json:format(
      #{ ~"version" => ~"2.1.0",
         ~"$schema" => ~"https://raw.githubusercontent.com/oasis-tcs/sarif-spec/main/sarif-2.1/schema/sarif-schema-2.1.0.json",
         ~"runs" =>
             [ #{
                 ~"tool" =>
                     #{ ~"driver" =>
                            #{ ~"informationUri" => ~"https://github.com/erlang/otp/scripts/scan-code.escript",
                               ~"name" => ~"scan-code",
                               ~"rules" =>
                                   [ #{ ~"id" => error_type_to_id(ErrorType),
                                        ~"name" => error_type_to_name(ErrorType),
                                        ~"shortDescription" =>
                                            #{ ~"text" => error_type_to_text(ErrorType) },
                                                % ~"helpUri" => ~"????",
                                        ~"fullDescription" =>
                                            #{
                                              ~"text" => error_type_to_description(ErrorType)
                                             }
                                      }
                                     || ErrorType <- ErrorTypes],
                               ~"version" => ~"1.0"
                             }
                      },
                 ~"artifacts" =>
                     [ #{
                         ~"location" => #{
                                          ~"uri" => File
                                         },
                         ~"length" => -1
                        } || File <- lists:usort([F || {_, F, _} <- Errors])
                     ],
                 ~"results" =>
                     [ #{
                         ~"ruleId" => error_type_to_id({ErrorType, License}),
                         ~"ruleIndex" => proplists:get_value({ErrorType, License}, ErrorTypesIndex),
                         ~"level" => error_type_to_level({ErrorType, License}),
                         ~"message" => #{ ~"text" => error_type_to_text({ErrorType, License}) },
                         ~"locations" =>
                             [ #{ ~"physicalLocation" =>
                                      #{ ~"artifactLocation" =>
                                             #{ ~"uri" => File }
                                       }
                                } ]
                        } || {License, File, ErrorType} <- Errors]
                } ]
       }).

error_type_to_id({no_license, _}) ->
    atom_to_binary(no_license);
error_type_to_id(ErrorType) ->
    base64:encode(integer_to_binary(erlang:phash2(ErrorType))).
error_type_to_text({license_not_recognised, L}) ->
    <<"License not recognized: ", L/binary>>;
error_type_to_text({license_to_be_reviewed, L}) ->
    <<"License must be reviewed: ", L/binary>>;
error_type_to_text({no_license, _}) ->
    <<"License not found">>;
error_type_to_text({license_not_approved, L}) ->
    <<"License not approved: ",L/binary>>;
error_type_to_text({no_copyright, L}) ->
    <<"No copyright found for license: ", L/binary>>.

error_type_to_name({no_copyright, _}) ->
    ~"NoCopyright";
error_type_to_name({no_license, _}) ->
    ~"NoLicense";
error_type_to_name({license_not_recognised, _}) ->
    ~"NoLicense";
error_type_to_name({license_not_approved, _}) ->
    ~"UnapprovedLicense";
error_type_to_name({license_to_be_reviewed, _}) ->
    ~"LicenseMustBeReviewed".
error_type_to_level({no_license, _}) ->
    ~"warning";
error_type_to_level({no_copyright, _}) ->
    ~"warning";
error_type_to_level({license_to_be_reviewed, _}) ->
    ~"warning";
error_type_to_level({license_not_recognised, _}) ->
    ~"error";
error_type_to_level({license_not_approved, _}) ->
    ~"error".
error_type_to_description({no_license, _}) ->
    ~"""
    scancode has not found any license in this file.  To fix this,
    add a license declaration to the top of the file.
    """;
error_type_to_description({no_copyright, _}) ->
    ~"""
    scancode has not found any copyright in this file.  To fix this,
    add a copyright declaration to the top of the file.
    """;
error_type_to_description({license_to_be_reviewed, L}) ->
    unicode:characters_to_binary(
        io_lib:format(
            """
            The license ~ts must be reviewed manually.
            This license is only allowed under certain
            special circumstances.
            """, [L]));
error_type_to_description({license_not_recognised, L}) ->
    unicode:characters_to_binary(
        io_lib:format(
            """
            The license ~ts is not recognized by scancode.
            You need to update scripts/scan-code.escript to include the
            license, or change to another license.
            """, [L]));
error_type_to_description({license_not_approved, L}) ->
    unicode:characters_to_binary(
        io_lib:format(
            """
            scancode has detected ~ts, which is a license that is
            not on the list of approved licenses. This file is not
            allowed to be part of Erlang/OTP under this license.
            """, [L])).
