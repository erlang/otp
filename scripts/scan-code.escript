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
                      scan_cache(),
                      analyzer_result(),
                      file_or_dir()],
       handler => fun scancode/1}.

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
      default => "scan-result.json",
      long => "-scan-result",
      help => "Output file where to put the scan result."}.

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

scancode(#{ analyzer_result := Analyzer, scan_result_cache := ScanResultFile} = Config) ->
    cache_scancode_results(
        decode(Analyzer),
        decode(ScanResultFile), Config).

cache_scancode_results(Analyzer, OriginalScanResult, Config) ->

    %% Merge all fields from `ort analyze` into the cached `ort scan`
    %% and update the `id` and `revision` field in all `scanner` structures
    ScanWithAnalyzer =
        update_revision(
          maps:merge_with(
            fun(_Key, Value, null) -> Value;
               (_Key, _Value, Value) -> Value
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
                 }, Mappings) ->
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

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).