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

-include_lib("kernel/include/file.hrl").
-export([read_file_info/1, read_link_info/1, list_dir/1]).

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
                      scan_input(),
                      scan_cache(),
                      analyzer_result(),
                      file_or_dir(),
                      sarif_option(),
                      ort_option() ],
       handler => fun scancode/1}.

approved() ->
    [ <<"Apache-2.0">> ].

%% Licenses that need manual review. See $ERL_TOP/.ort.yml to see which files
%% have these licenses.
reviewed() ->
    [~"Apache-2.0 WITH LLVM-exception",
     ~"BSD-2-Clause",
     ~"BSD-3-Clause",
     ~"BSD-3-Clause OR GPL-2.0-only",
     ~"BSL-1.0",
     ~"CC0-1.0",
     ~"FSFUL",
     ~"GPL-3.0-or-later WITH Autoconf-exception-generic-3.0",
     ~"Apache-2.0 OR LGPL-2.0-or-later",
     ~"Apache-2.0 OR LGPL-2.1-or-later",
     ~"Apache-2.0 OR BSL-1.0",
     ~"MIT",
     ~"MPL-1.1",
     ~"TCL",
     ~"Unicode-3.0",
     ~"Unlicense",
     ~"Zlib"].

no_license() ->
    [<<"null">>, 'null', ~"no license/copyright"].

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

ort_option() ->
    #{name => ort,
      type => string,
      long => "-ort-yaml",
      help => ".ort.yaml file to use to curate results." }.

scan_results() ->
    #{name => scan_results,
      type => string,
      default => "scan-result.json",
      long => "-scan-result",
      help => "Output file where to put the scan result."}.

scan_input() ->
    #{name => scan_input,
      type => string,
      long => "-scan-input",
      help => "A scan-results.json file to use for testing."}.

scan_cache() ->
    #{name => scan_result_cache,
      type => string,
      long => "-scan-result-cache",
      help => "A scan-results.json file to use as cache."}.

analyzer_result() ->
    #{name => analyzer_result,
      type => string,
      long => "-analyzer-result",
      help => "A analyzer-results.json file to use."}.

file_or_dir() ->
    #{name => file_or_dir,
      type => string,
      long => "-file-or-dir",
      help => "Files and/or directories to analyse."}.

sarif_option() ->
    #{name => sarif,
      type => string,
      default => undefined,
      long => "-sarif"}.
scancode(#{ scan_input := ScanResultFile } = Config) ->
    check_scancode_results(decode(ScanResultFile), Config);
scancode(#{ analyzer_result := Analyzer, scan_result_cache := ScanResultFile} = Config) ->
    cache_scancode_results(
        decode(Analyzer),
        decode(ScanResultFile), Config);
scancode(#{ file_or_dir := Files} = Config) ->
    io:format("Files to scan: ~ts~n", [Files]),
    scan_folder(Config).

cache_scancode_results(Analyzer, OriginalScanResult, Config) ->

    %% Merge all fields from `ort analyze` into the cached `ort scan`
    %% and update the `id` and `revision` field in all `scanner` structures
    ScanWithAnalyzer =
        update_revision(
          maps:merge_with(
            fun(Key, Value, null) -> Value;
               (Key, _Value, Value) -> Value
            end,
            OriginalScanResult, Analyzer)),

    %% Get all sha1sum of files in directory
    Sha1Sums = string:trim(
                os:cmd("cd "++maps:get(file_or_dir, Config) ++ " && "
                       "find . -type f | grep -v '^./.git/' | xargs sha1sum")),
    Sha1SumFiles = [string:split(Line, "  ") || Line <- string:split(Sha1Sums,"\n",all)],
    FilesSha1Sum = [#{ ~"path" => prefix(Filename, "./"),
                       ~"sha1" => unicode:characters_to_binary(Sha1Sum)} || [Sha1Sum, Filename] <- Sha1SumFiles ],

    %% Check which files on disk do not have the same sha1 as the files in the original scan
    FilesToRescan = check_files_to_rescan(FilesSha1Sum, ScanWithAnalyzer),

    io:format("Files to rescan: ~p~n", [FilesToRescan]),

    %% Copy all files to tmp folder for scanning
    file:del_dir_r(?tmp_folder),
    ok = file:make_dir(?tmp_folder),

    os:cmd(lists:flatten(
        unicode:characters_to_list(
        ["cd "++maps:get(file_or_dir, Config) ++ " && cp --parent -a " ++
            lists:join(" ",[Path || #{ ~"path" := Path } <- FilesToRescan]),
            " ", filename:absname(?tmp_folder)]))),

    Command = scancode_command(Config),
    io:format("Running: ~ts~n", [Command]),
    R = os:cmd(Command),
    io:format("Result: ~ts~n",[R]),
    ScanResult = decode(scan_result_path(Config)),
    %% Get the new license and copyright results from the scancode output.
    NewResults = get_new_results(ScanResult, OriginalScanResult),

    %% Update the scan results by removing references to stale files
    %% and adding the new.
    FinalResults = update_scan_results(FilesToRescan, NewResults, ScanWithAnalyzer),

    file:write_file(maps:get(scan_results, Config) ++ ".old",
        json:format(sort_arrays(OriginalScanResult))),

    file:write_file(maps:get(scan_results, Config),
        json:format(sort_arrays(FinalResults))),

    ok.

update_revision(
  #{ ~"analyzer" :=
         #{ ~"result" :=
                #{ ~"projects" :=
                       [#{ ~"id" := NewID,
                           ~"vcs_processed" := #{ ~"revision" := NewRevision }}]}},
     ~"scanner" :=
         #{ ~"provenances" :=
                [ #{ ~"id" := OldID,
                     ~"package_provenance" := #{ ~"vcs_info" := #{ ~"revision" := OldRevision }}}]}}
  = ScanResult) ->
    deep_replace(OldRevision, NewRevision,
                deep_replace(OldID, NewID, ScanResult)).

deep_replace(Old, New, Map) when is_map(Map) ->
    maps:fold(fun (K, V, Acc) ->
                      Acc#{deep_replace(Old, New, K) => deep_replace(Old, New, V)}
              end, #{}, Map);
deep_replace(Old, New, List) when is_list(List) ->
    lists:sort(lists:map(fun(Item) -> deep_replace(Old, New, Item) end, List));
deep_replace(Old, New, Value) when is_binary(Value) ->
    case string:equal(Old, Value) of
        true -> New;
        false -> Value
    end;
deep_replace(Old, New, Value) ->
    case Old =:= Value of
        true -> New;
        false -> Value
    end.

sort_arrays(Map) when is_map(Map) ->
    maps:map(fun(_Key, Value) -> sort_arrays(Value) end, Map);
sort_arrays(List) when is_list(List) ->
    lists:sort(lists:map(fun sort_arrays/1, List));
sort_arrays(Value) -> Value.

%% This function updates the scan result with new values
%% TODO: Remove any files that have been removed since last scan.
update_scan_results(FilesToRescan, NewScanResults, OldScanResults) ->
    #{ ~"scanner" :=
           S = #{ ~"files" := [F = #{ ~"files" := Files }],
                  ~"scan_results" :=
                      [SR = #{ ~"summary" :=
                                   Sum = #{ ~"licenses" := Licenses,
                                            ~"copyrights" := Copyrights }}]}} = OldScanResults,

    PathFilter = fun(P) ->
        fun(#{ ~"path" := Path }) -> string:equal(Path, P) end
    end,

    StaleLicensesRemoved =
        [License || #{ ~"location" := #{ ~"path" := P }} = License <- Licenses,
                    not lists:any(PathFilter(P), FilesToRescan)
        ],

    StaleCopyrightsRemoved =
        [Copyright || #{ ~"location" := #{ ~"path" := P }} = Copyright <- Copyrights,
                    not lists:any(PathFilter(P), FilesToRescan)
        ],

    StaleFilesRemoved =
        [File || #{ ~"path" := P } = File <- Files,
                 not lists:any(PathFilter(P), FilesToRescan)
        ],

    NewLicenseScanResults = [R || R = #{ ~"license" := _ } <- NewScanResults],
    NewCopyrightScanResults = [R || R = #{ ~"statement" := _ } <- NewScanResults],

    OldScanResults#{ ~"scanner" :=
                         S#{ ~"files" := [F#{ ~"files" := StaleFilesRemoved ++ FilesToRescan }],
                             ~"scan_results" :=
                                 [SR#{ ~"summary" :=
                                           Sum#{ ~"licenses" := StaleLicensesRemoved ++ NewLicenseScanResults,
                                                 ~"copyrights" := StaleCopyrightsRemoved ++ NewCopyrightScanResults }}]}}.


%% Dig deep into the scancode result and fetch all license and copyright data from it.
get_new_results(#{ ~"files" := Files }, #{ ~"scanner" := #{ ~"config" := #{ ~"detected_license_mapping" := Mappings }}}) ->
    lists:flatmap(fun(File) -> get_new_results(File, Mappings) end, Files); 
get_new_results(#{ ~"type" :=  ~"file", ~"path" := Path,
                   ~"license_detections" := Licenses,
                   ~"copyrights" := Copyrights
                 } = File, Mappings) ->
    lists:flatmap(fun(#{ ~"license_expression_spdx" := SPDX, ~"matches" := Matches }) ->
                           [#{ ~"license" => replace_mappings(SPDX, Mappings),
                               ~"location" => #{ ~"path" => prefix(Path,?tmp_folder),
                                                 ~"start_line" => Start,
                                                 ~"end_line" => End},
                               ~"score" => Score }
                            || #{ ~"score" := Score,
                                  ~"start_line" := Start,
                                  ~"end_line" := End } <- Matches]
                   end, Licenses) ++
     lists:map(fun(#{ ~"copyright" := Copyright, ~"start_line" := Start,
                      ~"end_line" := End }) ->
                       #{ ~"statement" => Copyright,
                          ~"location" => #{ ~"path" => prefix(Path,?tmp_folder),
                                            ~"start_line" => Start,
                                            ~"end_line" => End} }
               end, Copyrights);
get_new_results(_, _) -> [].

%% ort replaces certain predefined mappings to NOASSERTION, so we must do the same
replace_mappings(SPDX, [{From,To} | T]) ->
    %% We add the spaced around everything to make the matching logic
    %% work for "abc", "abc-cde".
    replace_mappings(string:replace([" ", SPDX, " "], [" ",From, " "], [" ", To, " "]), T);
replace_mappings(SPDX, []) ->
    unicode:characters_to_binary(string:trim(SPDX, both, " "));
replace_mappings(SPDX, Map) ->
    %% We remove any AND NOASSERTION as it really means that we have the license on the
    %% other side of NOASSERTION.
    replace_mappings(SPDX, maps:to_list(Map) ++ [{"AND NOASSERTION",""},{"NOASSERTION AND",""}]).

prefix(Str, Prefix) ->
    case string:prefix(Str, Prefix) of
        nomatch -> unicode:characters_to_binary(Str);
        NoPrefix -> unicode:characters_to_binary(NoPrefix)
    end.

%% This check which files have changed since the previous scan.
%% TODO: Also include deleted files!
check_files_to_rescan(CurrentSha1Sum, #{ ~"scanner" := #{ ~"files" := OriginalSha1SumFiles }}) ->
    check_files_to_rescan(CurrentSha1Sum, OriginalSha1SumFiles);
check_files_to_rescan(CurrentSha1Sum, [OriginalSha1SumFiles | T]) ->
    check_files_to_rescan(check_files_to_rescan(CurrentSha1Sum, OriginalSha1SumFiles),T);
check_files_to_rescan(CurrentSha1Sum, []) ->
    CurrentSha1Sum;
check_files_to_rescan(CurrentSha1Sum, #{ ~"files" := OriginalSha1SumFiles }) ->
    CurrentSha1Sum -- OriginalSha1SumFiles.

create_folder(Prefix, Folder) ->
    Filename = filename:absolute(filename:join(Prefix, Folder)),
    [] = os:cmd("mkdir " ++ Filename),
    Filename.

cleanup_files(FilesOrDirs) ->
    lists:filter(fun ([]) -> false; (_) -> true end,
                 string:split(FilesOrDirs, " ", all)).

cp_with_path(Prefix, File, Folder) ->
    case file:read_file_info(File) of
        {ok,#file_info{ type = directory }} ->
            "cp -r -f --parents " ++ Prefix ++ File ++ " " ++ Folder;
        _ ->
            "cp -f --parents " ++ Prefix ++ File ++ " " ++ Folder
    end.

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
    N = max(erlang:system_info(schedulers_online) - 1, 1),
    "scancode -n "++integer_to_list(N)++" -" ++ Options ++ " --json-pp " ++ ScanResultPath ++ " " ++ FolderPath.

execute(Command, Config) ->
    io:format("Running: ~ts~n", [Command]),
    R = os:cmd(Command),
    io:format("Result: ~ts~n",[R]),
    ScanResult = scan_result_path(Config),

    check_scancode_results(decode(ScanResult), Config).

check_scancode_results(Json, Config) ->
    Licenses = fetch_licenses(folder_path(Config), Json, curations(Config)),

    Errors = compliance_check(Licenses),

    maps:get(sarif, Config) =/= undefined andalso
        sarif(maps:get(sarif, Config), Errors),

    SortedErrors = lists:sort(
                     fun(#{ msg := AMsg } = A, #{ msg := BMsg } = B) ->
                             if AMsg =:= BMsg ->
                                     A =< B;
                                true ->
                                     AMsg =< BMsg
                             end
                     end, Errors),

    [io:format(standard_error, "~ts:\n  Msg: ~p\n  License: ~ts\n  SPDX: ~ts\n\n", [Path, Msg, License, Spdx]) ||
        #{ msg := Msg, spdx := Spdx, license := License, path := Path } <- SortedErrors,
        Msg =:= license_detected_error],

    SortedErrors =/= [] andalso erlang:halt(1),
    ok.

-spec compliance_check([{Path, License, SPDX, Copyrights, Curated}]) -> Result when
      Result     :: #{ license => License, spdx => SPDX, path => Path, msg => Msg },
      License    :: binary(),
      SPDX       :: binary(),
      Copyrights :: binary(),
      Path       :: binary(),
      Curated    :: atom(),
      Msg        :: no_license | license_to_be_reviewed | license_not_approved |
        license_detected_error | no_copyright.
compliance_check(Licenses) when is_list(Licenses) ->
    lists:foldl(
      fun({Path, License, SPDX0, Copyright, Curated}, Acc) ->
              SPDX = spdx_nonnull(SPDX0),
              CopyrightResult = check_copyright(Copyright),
              LicenseResult = license_compliance_check(SPDX),
              R = lists:foldl(
                    fun(ok, Acc0) -> Acc0;
                       ({error, license_to_be_reviewed}, Acc0) when Curated ->
                            %% A license that should be reviewed, but it has
                            %% been curated in .ort.yml to it is ok.
                            Acc0;
                       ({error, Msg}, Acc0) ->
                            [#{ license => License,
                                spdx => SPDX,
                                path => Path,
                                msg => Msg} | Acc0]
                    end, [], [CopyrightResult, LicenseResult]),
              R ++ Acc
      end, [], Licenses).

-spec license_compliance_check(DetectedLicense) -> ok | {error, Err} when
      Err :: no_license | license_not_approved | license_to_be_reviewed |
             license_detected_error,
      DetectedLicense :: binary().
license_compliance_check(License) ->
    Handler = [ {no_license(), {error, no_license}},
                {reviewed(), {error, license_to_be_reviewed}},
                {approved(), ok}],
    license_check(License, Handler).

spdx_nonnull(null) ->
    <<"no license/copyright">>;
spdx_nonnull(X) ->
    X.

-spec check_copyright([#{binary() := binary()}]) -> ok | {error, no_copyright}.
check_copyright([]) ->
    {error, no_copyright};
check_copyright([#{<<"copyright">> := _} | _]) ->
    ok.

license_check(License, Handler) ->
    case string:prefix(License, "detected license") of
        nomatch ->
            lists:foldl(
            fun(_, {error, X}=Error) when X =/= license_not_approved ->
                    Error;
               ({Licenses, Msg}, Acc) ->
                    case lists:member(License, Licenses) of
                        true ->
                            Msg;
                        false ->
                            Acc
                    end
            end, {error, license_not_approved}, Handler);
        _ -> {error, license_detected_error}
    end.


curations(#{ ort := undefined }) ->
    fun(_Filename, SPDX) -> SPDX end;
curations(#{ ort := OrtYaml }) ->

    os:find_executable("yq") =:= undefined andalso throw({could_not_find_yq}),

    #{ ~"excludes" := #{ ~"paths" := ExcludePaths },
       ~"curations" := #{ ~"license_findings" := Curations } }
        = json:decode(unicode:characters_to_binary(os:cmd("yq -o=json eval " ++ OrtYaml))),

    fun F(Filename, null) -> F(Filename, ~"NONE");
        F(Filename, SPDX) ->
            case lists:any(fun(#{ ~"pattern" := Pattern }) ->
                                   glob_match(Filename, Pattern)
                           end, ExcludePaths) of
                true -> exclude;
                false ->
                    case lists:filter(fun(#{ ~"path" := Path }) ->
                                              glob_match(Filename, Path)
                                      end, Curations) of
                        [] -> keep;
                        [Curation | _] ->

                            DetectedLicense = maps:get(~"detected_license", Curation, "NONE"),
                            case string:equal(DetectedLicense, SPDX) of
                                false -> {curate, ["detected license '", SPDX, "', ort.yml has it as '", DetectedLicense, "'"]};
                                true ->
                                    {curate, maps:get(~"concluded_license", Curation)}
                            end
                    end
            end
    end.

glob_match(Path, Glob) ->
    put(path, filename:split("./" ++ unicode:characters_to_list(Path))),
    filelib:wildcard(unicode:characters_to_list(Glob), ?MODULE) =/= [].

read_link_info(File) ->
    read_link_info(filename:split(File), get(path)).

read_link_info([A],[A]) ->
    {ok, #file_info{ type = file }};
read_link_info([A|TA],[A|TB]) ->
    read_link_info(TA, TB);
read_link_info([], [_|_]) ->
    {ok, #file_info{ type = directory }};
read_link_info(_,_) ->
    {error, enoent}.

read_file_info(File) ->
    read_link_info(File).

list_dir(Dir) ->
    list_dir(filename:split(Dir), get(path)).

list_dir([A],[A]) ->
    {error, enotdir};
list_dir([A|TA],[A|TB]) ->
    list_dir(TA, TB);
list_dir([],[A|_TB]) ->
    {ok, [A]};
list_dir(_,_) ->
    {ok, []}.

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

fetch_licenses(_FolderPath, #{<<"files">> := Files}, Curate) ->
    lists:filtermap(
      fun(#{<<"type">> := <<"file">>,
            <<"detected_license_expression">> := License,
            <<"detected_license_expression_spdx">> := SPDX,
            <<"copyrights">> := Copyrights,
            <<"path">> := TmpPath}) ->
              Path = string:prefix(string:trim(TmpPath, leading), "tmp/otp-lukas-otp-ossf-compiler-flags/"),
              case Curate(Path, SPDX) of
                  exclude ->
                      false;
                  {curate, CuratedSPDX} ->
                      {true, {Path, License, CuratedSPDX, [#{ ~"copyright" => curated }], true}};
                  keep ->
                      {true, {Path, License, SPDX, Copyrights, false}}
              end;
         (_) ->
              false
      end, Files).

-spec sarif(file:name_all(), Errors) -> dynamic() when
      Errors :: [#{ license => License, spdx => SPDX, path => Path, msg => Msg}],
      License    :: binary(),
      SPDX       :: binary(),
      Path       :: binary(),
      Msg        :: no_license | license_not_approved | license_to_be_reviewed | license_not_recognised | no_copyright.
sarif(SarifFile, Errors) ->
    file:write_file(SarifFile, sarif(Errors)).

-spec sarif(Input) -> dynamic() when
      Input :: [#{ license => License, spdx => SPDX, path => Path, msg => Msg}],
      License    :: binary(),
      SPDX       :: binary(),
      Path       :: binary(),
      Msg        :: no_license | license_not_approved | license_to_be_reviewed | license_not_recognised | no_copyright.
sarif(Errors) when is_list(Errors) ->
    ErrorTypes = lists:usort([{Type, License} || #{spdx := License, msg := Type} <- Errors]),
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
                        } || File <- lists:usort([F || #{path := F} <- Errors])
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
                        } || #{spdx := License, path := File, msg := ErrorType} <- Errors]
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
