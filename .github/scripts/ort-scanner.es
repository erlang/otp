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
    argparse:run(Args, cli(), #{progname => 'ort-scanner'}).

cli() ->
    #{ commands =>
           #{ "init" =>
                  #{ help => "Create an ort scan-result from an analyze-result.",
                     arguments => [input_option("analyze-result.json"),
                                   output_option("scan-result.json"),
                                   path_option("Path to git repository to be scanned.")],
                     handler => fun init/1},
              "restore-cache" =>
                #{ help => "Restore license results from an old scan-result.json file.",
                     arguments => [input_option("scan-result.json"),
                                   output_option("scan-result.json"),
                                   path_option("Path to old scan-result.json file.")],
                     handler => fun 'restore-cache'/1},
                
              "scan" =>
                  #{ help => "Update a scan-result.json file using 'scancode' or 'reuse'.",
                    arguments => [input_option("scan-result.json"),
                                  output_option("scan-result.json"),
                                  strategy_option(),
                                  tool_option(),
                                  path_option("Path to git repository to be scanned.")],
                    handler => fun scan/1}
            },
        help =>
            """
            ort-scanner can be used as a replacement to running the built-in
            scanner in ORT. Typical usage is:

              # Initialize a scan-result.json with the correct metadata but no license
              # information.
              $ ort-scanner init -i analyze-result.json -o scan-result.json /erlang/otp

              # Populate the scan results with an old scan-result
              $ ort-scanner restore-cache -i scan-result.json \
                    -o scan-result.json old-scan-result.json

              # Populate the missing files using `scancode`
              $ ort-scanner scan -t scancode -s fill -i scan-result.json \
                    -o scan-result.json /erlang/otp

              # Overwrite all results with the results found by reuse
              $ ort-scanner scan -t reuse -s overwrite -i scan-result.json \
                    -o scan-result.json /erlang/otp
            """
     }.

input_option(Default) ->
    #{name => input,
      type => string,
      default => Default,
      long => "-input",
      short => $i,
      help => "The analyzer."}.

output_option(Default) ->
    #{name => output,
      type => string,
      default => Default,
      long => "-output",
      short => $o,
      help => "Output file where to put the scan result."}.

strategy_option() ->
    #{ name => strategy,
       long => "-strategy",
       short => $s,
       help => "The scan strategy to use. Valid options are 'overwrite' and 'fill'."
     }.

tool_option() ->
    #{ name => tool,
       long => "-tool",
       short => $t,
       help => "The scan tool to use. Valid options are 'scancode' and 'reuse'."
     }.

path_option(Help) ->
    #{ name => path,
       required => true,
       help => Help }.

init(#{ input := AnalyzerResult, path := GitRepo } = Args) ->
    have_tools(["git","sha1sum"]),

    Analyzer = decode(AnalyzerResult),
    #{ ~"repository" :=
           #{ ~"vcs" :=
                  #{ ~"type" := ~"Git", ~"revision" := Sha } = VCS } } = Analyzer,
    #{ ~"analyzer" :=
           #{ ~"result" := #{ ~"projects" :=
                                  [#{ ~"id" := Id, ~"vcs_processed" := VCS }] }}}
        = Analyzer,

    %% Check that sha is correct
    GitSha = cmd("cd "++GitRepo++" && git rev-parse HEAD"),
    string:equal(GitSha, Sha) orelse
        fail("Sha in '~ts' (~ts) did not match sha in '~ts' (~ts)",
        [AnalyzerResult, Sha, GitRepo, GitSha]),

    StartTime = rfctime(),

    Provenance =
        #{ ~"id" => Id,
           ~"package_provenance" =>
               PP = #{
                      ~"vcs_info" => VCS,
                      ~"resolved_revision" => Sha
                     }
         },

    Sha1Sums = sha1sums(GitRepo),
    Sha1SumFiles = [string:split(Line, "  ") || Line <- string:split(Sha1Sums,"\n",all)],
    FilesSha1Sum = [#{ ~"path" => unicode:characters_to_binary(Filename),
                       ~"sha1" => unicode:characters_to_binary(Sha1Sum)} ||
                       [Sha1Sum, Filename] <- Sha1SumFiles ],

    Scanner =
        #{
          ~"start_time" => StartTime,
          ~"end_time" => rfctime(),
          ~"environment" => maps:get(~"environment", maps:get(~"analyzer", Analyzer)),
          ~"provenances" => [ Provenance ],
          ~"scan_results" => [],
          ~"scanners" => #{ Id => [] },
          ~"files" => [#{ ~"provenance" => PP,
                          ~"files" => FilesSha1Sum}]
         },

    ok = file:write_file(maps:get(output, Args),
                         format_scan_results(Analyzer#{ ~"scanner" := Scanner })),

    ok.

sha1sums(GitRepo) ->
    %% We use -z (that is null separated output) in order to deal with unicode
    Filenames = cmd("cd "++GitRepo++" && git ls-tree -z -r --name-only HEAD"),
    %% Batch sha1sum to speed things up a bit
    batch(
      fun(FN) ->
        {ok, #file_info{ type = Type }} = file:read_link_info(filename:join(GitRepo, FN)),
        Type =:= regular
     end,
      fun(Names) ->
              [cmd("cd "++GitRepo++" && sha1sum" ++ [[" '",N,"'"] || N <- Names]),"\n"]
      end,string:split(string:trim(Filenames, both, "\0"),"\0",all)).

'restore-cache'(#{ input := InputScanResult, path := CacheScanResult } = Args) ->
    Input = decode(InputScanResult),
    Cache = decode(CacheScanResult),

    #{ ~"scanner" := #{ ~"provenances" := [#{~"package_provenance" := InputProvenance}]}} = Input,

    %% Check which files on disk do not have the same sha1 as the files in the original scan
    FilesToRestore = check_files_to_restore(Input, Cache),

    ScanResults = restore_from_cache(FilesToRestore,
                                     InputProvenance,
                                     Cache),

    InputScanner = maps:get(~"scanner", Input),
    InputScanners = maps:get(~"scanners", InputScanner),
    Scanners = [maps:get(~"name", maps:get(~"scanner", R)) || R <- ScanResults],
    NewScanners = #{ K => Scanners || K := _ <- InputScanners},

    CacheConfig = maps:get(~"config", maps:get(~"scanner", Cache)),

    Output =
        format_scan_results(Input#{ ~"scanner" := InputScanner#{ ~"config" => CacheConfig,
                                                                ~"scan_results" => ScanResults,
                                                                ~"scanners" => NewScanners }}),

    ok = file:write_file(maps:get(output, Args), Output),

    ok.

check_files_to_restore(#{ ~"scanner" := #{ ~"files" := [#{ ~"files" := InputSha1SumFiles }]}},
                       #{ ~"scanner" := #{ ~"files" := [#{ ~"files" := CacheSha1SumFiles }]}}) ->
    ToRestore = [Filename || #{ ~"path" := Filename} = File <- CacheSha1SumFiles,
        lists:member(File, InputSha1SumFiles)],
    io:format("Restoring ~p of ~p files~n",[length(ToRestore), length(InputSha1SumFiles)]),
    ToRestore.

restore_from_cache(FilesToRestore, Provenance, #{ ~"scanner" := #{ ~"scan_results" := ScanResults }}) ->
    restore_from_cache(FilesToRestore, Provenance, ScanResults);
restore_from_cache(FilesToRestore, Provenance, [#{ ~"scanner" := Scanner, ~"summary" := Summary} | T]) ->
    #{ ~"copyrights" := Copyrights, ~"licenses" := Licenses} = Summary,

    NewCopyrights = [C || #{ ~"location" := #{ ~"path" := Path }} = C <- Copyrights,
        lists:member(Path, FilesToRestore)],

    NewLicenses = [C || #{ ~"location" := #{ ~"path" := Path }} = C <- Licenses,
        lists:member(Path, FilesToRestore)],

    NewSummary = Summary#{ ~"copyrights" := NewCopyrights, ~"licenses" := NewLicenses},

    [#{ ~"provenance" => Provenance, ~"scanner" => Scanner, ~"summary" => NewSummary} |
     restore_from_cache(FilesToRestore, Provenance, T)];
restore_from_cache(_, _, []) -> [].

scan(#{ input := InputScanResult, strategy := Strategy, tool := Tool, path := Path } = Args) ->
    have_tools(["git", "mktemp"]),

    #{ ~"scanner" := InputScanner } = Input = decode(InputScanResult),
    #{ ~"provenances" := [#{~"package_provenance" := InputProvenance}]} = InputScanner,
    #{ ~"scan_results" := ScanResults, ~"scanners" := Scanners } = InputScanner,

    Area = setup_scan_area(InputScanner, Path, Strategy),

    Config = maps:get(~"config", InputScanner,
                      #{ ~"skip_concluded" => false,
                         ~"skip_excluded" => false,
                         ~"include_files_without_findings" => true,
                         ~"detected_license_mapping" =>  #{
                                                           ~"LicenseRef-scancode-agpl-generic-additional-terms" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-free-unknown" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-generic-cla" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-generic-exception" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-generic-export-compliance" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-generic-tos" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-generic-trademark" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-gpl-generic-additional-terms" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-other-copyleft" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-other-permissive" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-patent-disclaimer" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-unknown" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-unknown-license-reference" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-unknown-spdx" =>  ~"NOASSERTION",
                                                           ~"LicenseRef-scancode-warranty-disclaimer" =>  ~"NOASSERTION"
                                                          },
                         ~"ignore_patterns" =>  [ ~"**/*.ort.yml", ~"**/*.spdx.yml", ~"**/*.spdx.yaml", ~"**/*.spdx.json", ~"**/META-INF/DEPENDENCIES", ~"**/META-INF/DEPENDENCIES.txt", ~"**/META-INF/NOTICE", ~"**/META-INF/NOTICE.txt" ]
                       }),

    #{ ~"scanner" := #{ ~"name" := ScannerName }} = NewScanResult =
        scan_area(Area, InputProvenance,
                  maps:get(~"detected_license_mapping", Config), Tool),

    NewScanners = #{ K => lists:usort([ScannerName | V]) || K := V <- Scanners},

    ScanResult = hd(filelib:wildcard(Area ++ "/*.json")),
    {ok, _} = file:copy(ScanResult,
                        filename:join(filename:dirname(maps:get(output, Args)),
                                      filename:basename(ScanResult))),

    ok = file:write_file(
           maps:get(output, Args),
           format_scan_results(
             Input#{ ~"scanner" :=
                         InputScanner#{ ~"config" => Config,
                                        ~"scan_results" := deduplicate([NewScanResult | ScanResults]),
                                        ~"scanners" := NewScanners }})),

    do_cleanup(),

    ok.

setup_scan_area(Scanner, GitPath, Strategy) ->
    
    TmpDir = cmd("mktemp -d /tmp/ort-scanner.XXXXXXXXX"),

    cleanup(fun() -> file:del_dir_r(TmpDir) end),

    #{ ~"scan_results" := Results,
       ~"files" := [#{ ~"files" := Sha1Files }]} = Scanner,

    %% fill means that we should scan only the files that have no scan result yet
    %% so we fill the scan area with the files that are present in "files"
    %% but not yet in "scan_result". For "overwrite" we should copy all files.
    ScannedFiles = case Strategy of
        "fill" ->
            lists:flatmap(fun(#{ ~"summary" := #{ ~"copyrights" := CS, ~"licenses" := LS }}) -> 
                    [Path || #{ ~"location" := #{ ~"path" := Path }} <- CS ++ LS]
                end, Results);
        "overwrite" ->
            []
        end,
    
    FilesToScan = [Path || #{ ~"path" := Path } <- Sha1Files]
        -- ScannedFiles,

    io:format("Copying ~p files to ~ts~n",[length(FilesToScan), TmpDir]),

    batch(fun(Names) ->
        cmd("cd " ++ GitPath ++ " && cp --parent -a " ++ [[" '",N,"'"] || N <- Names] ++ " " ++ TmpDir)
    end, FilesToScan),

    TmpDir.

scan_area(Area, Provenance, _Mappings, "reuse") ->
    have_tools(["reuse"]),
    StartTime = rfctime(),
    Cmd = "cd " ++ Area ++ " && reuse lint -j",
    io:format("Running: ~ts~n", [Cmd]),
    JsonBin = unicode:characters_to_binary(cmd(Cmd ++ " 2>&1 | grep -v \"^reuse.extract - ERROR\" || true")),
    file:write_file(filename:join(Area, "reuse.json"), JsonBin),
    ReuseJson = json:decode(JsonBin),

    #{ ~"reuse_tool_version" := Vsn, ~"files" := Files,
       ~"non_compliant" :=
           #{
             ~"missing_copyright_info" := _MissingCS,
             ~"missing_licensing_info" := _MissingLS
            }
     } = ReuseJson,

    {CS, LS} =
        lists:foldl(
          fun(#{ ~"path" := Path, ~"copyrights" := C, ~"spdx_expressions" := L}, {CS, LS}) ->
                  {[#{ ~"location" => #{ ~"path" => Path, ~"start_line" => -1, ~"end_line" => -1 },
                       ~"statement" => string:trim(prefix(V, "SPDX-FileCopyrightText:"))}
                    || #{ ~"value" := V } <- C, L =/= []] ++ CS,
                   [#{ ~"location" => #{ ~"path" => Path, ~"start_line" => -1, ~"end_line" => -1 },
                       ~"license" => V }
                    || #{ ~"value" := V } <- L] ++ LS}
          end, {[], []}, Files),

    #{ ~"scanner" => #{ ~"configuration" => ~"lint -j", ~"name" => ~"reuse", ~"version" => Vsn},
       ~"provenance" => Provenance,
       ~"summary" =>
           #{ ~"start_time" => StartTime,
              ~"end_time" => rfctime(),
              ~"copyrights" => CS,
              ~"licenses" => LS
            }
     };
scan_area(Area, Provenance, Mappings, "scancode") ->
    have_tools(["scancode"]),
    StartTime = rfctime(),
    ScancodeResultFile = filename:join(Area,"scancode.json"),
    N = max(erlang:system_info(schedulers_online) - 1, 1),
    Cmd = "scancode -n "++integer_to_list(N)++" -cli --strip-root --json-pp " ++ ScancodeResultFile ++ " " ++ Area,
    io:format("Running: ~ts~n", [Cmd]),
    cmd(Cmd),
    ScancodeJson = decode(ScancodeResultFile),
    #{ ~"headers" := [#{ ~"tool_version" := Vsn }] } = ScancodeJson,
    NewScanResults = get_new_results(ScancodeJson, Area, Mappings),

    CS = [R || R = #{ ~"statement" := _ } <- NewScanResults],
    LS = [R || R = #{ ~"license" := _ } <- NewScanResults],

    #{ ~"scanner" => #{ ~"configuration" => ~"-cli --strip-root --json-pp",
                        ~"name" => ~"ScanCode", ~"version" => Vsn},
       ~"provenance" => Provenance,
       ~"summary" =>
           #{ ~"start_time" => StartTime,
              ~"end_time" => rfctime(),
              ~"copyrights" => CS,
              ~"licenses" => LS
            }
     }.

%% Dig deep into the scancode result and fetch all license and copyright data from it.
get_new_results(#{ ~"files" := Files }, Area, Mappings) ->
    lists:flatmap(fun(File) -> get_new_results(File, Area, Mappings) end, Files);
get_new_results(#{ ~"type" :=  ~"file",
                   ~"license_detections" := []
                 } = F, Area, Mappings) ->
    NONEMatch = [#{ ~"matches" => [#{~"license_expression_spdx" => "NONE",
                                     ~"score" => 100.0,
                                     ~"start_line" => -1,
                                     ~"end_line" => -1}]}],
    get_new_results(F#{ ~"license_detections" := NONEMatch}, Area, Mappings);
get_new_results(#{ ~"type" :=  ~"file", ~"path" := Path,
                   ~"license_detections" := Licenses,
                   ~"copyrights" := Copyrights
                 }, Area, Mappings) ->
    lists:flatmap(fun(#{ ~"matches" := Matches }) ->
                           [#{ ~"license" => replace_mappings(SPDX, Mappings),
                               ~"location" => #{ ~"path" => prefix(Path,Area),
                                                 ~"start_line" => Start,
                                                 ~"end_line" => End},
                               ~"score" => Score }
                            || #{ ~"license_expression_spdx" := SPDX,
                                  ~"score" := Score,
                                  ~"start_line" := Start,
                                  ~"end_line" := End } <- Matches]
                   end, Licenses) ++
     lists:map(fun(#{ ~"copyright" := Copyright, ~"start_line" := Start,
                      ~"end_line" := End }) ->
                       #{ ~"statement" => Copyright,
                          ~"location" => #{ ~"path" => prefix(Path,Area),
                                            ~"start_line" => Start,
                                            ~"end_line" => End} }
               end, Copyrights);
get_new_results(_, _, _) -> [].

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

deduplicate(ScanResults) ->
    deduplicate(ScanResults, []).
deduplicate([H | T], Found) ->
    #{ ~"summary" := #{ ~"copyrights" := CS, ~"licenses" := LS} = Summary} = H,
    FilteredCS = [C || C = #{ ~"location" := #{ ~"path" := Path }} <- CS,
                        not lists:member(Path, Found)],
    FilteredLS = [L || L = #{ ~"location" := #{ ~"path" := Path }} <- LS,
                        not lists:member(Path, Found)],
    case [Path || #{ ~"location" := #{ ~"path" := Path }} <- FilteredCS ++ FilteredLS] of
        [] -> deduplicate(T, Found);
        NewPaths ->
            [H#{ ~"summary" := Summary#{ ~"copyrights" := FilteredCS, ~"licenses" := FilteredLS } }
             | deduplicate(T, NewPaths ++ Found)]
    end;
deduplicate([], _) ->   
    [].

prefix(Str, Prefix) ->
    case string:prefix(Str, Prefix) of
        nomatch -> unicode:characters_to_binary(Str);
        NoPrefix -> unicode:characters_to_binary(NoPrefix)
    end.

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

batch(Batch, Items) ->
    batch(fun(_) -> true end, Batch, Items, 1000).
batch(Batch, Items, Cnt) when is_function(Batch), is_list(Items), is_integer(Cnt) ->
    batch(fun(_) -> true end, Batch, Items, Cnt);
batch(Filter, Batch, Items) when is_function(Filter), is_function(Batch), is_list(Items) ->
    batch(Filter, Batch, Items, 1000).
batch(Filter, Batch, Items, Cnt) ->
    batch(Filter, Batch, Items, [], 0, Cnt).

batch(_Filter, _Batch, Items, [], Curr, _Cnt) when Items =:= [], Curr =:= 0  ->
    [];
batch(Filter, Batch, Items, Acc, Curr, Cnt) when Items =:= []; Curr =:= Cnt  ->
    Batch(Acc) ++ batch(Filter, Batch, Items, [], 0, Cnt);
batch(Filter, Batch, [H | T], Acc, Curr, Cnt) ->
    case Filter(H) of
        true -> batch(Filter, Batch, T, [H | Acc], Curr + 1, Cnt);
        false -> batch(Filter, Batch, T, Acc, Curr, Cnt)
    end.

format_scan_results(ScanResult) ->
    json:format(sort_arrays(ScanResult, 
    fun(Key) ->
        if Key =:= ~"scan_results" -> false;
           Key =:= ~"licenses"; Key =:= ~"copyrights" ->
             fun(#{ ~"location" := #{ ~"path" := AP }} = A, #{ ~"location" := #{ ~"path" := BP }} = B) ->
                if AP =:= BP -> A =< B;
                   true -> AP =< BP
                end
             end;
           true -> true
        end
     end)).

sort_arrays(JSon) ->
    sort_arrays(JSon, fun(_) -> true end).
sort_arrays(JSon, KeyFun) ->
    sort_arrays(JSon, KeyFun, true).
sort_arrays(Map, KeyFun, _Sort) when is_map(Map) ->
    maps:map(fun(Key, Value) -> sort_arrays(Value, KeyFun, KeyFun(Key)) end, Map);
sort_arrays(List, KeyFun, Sort) when is_list(List) ->
    UnsortedArray = lists:map(fun(A) -> sort_arrays(A, KeyFun, true) end, List),
    if Sort ->
        lists:sort(UnsortedArray);
       not Sort ->
        UnsortedArray;
       is_function(Sort) ->
         lists:sort(Sort, UnsortedArray)
    end;
sort_arrays(Value, _KeyFun, _Sort) -> Value.

have_tools([H|T]) ->
    case os:find_executable(H) of
        false -> fail("Could not find '~ts' in PATH", [H]);
        _ -> have_tools(T)
    end;
have_tools([]) -> ok.

fail(Fmt, Args) ->
    io:format(standard_error, Fmt++"\n", Args),
    do_cleanup(),
    erlang:halt(1).

cleanup(Fun) ->
    [put(cleanup, []) || get(cleanup) =:= undefined],
    put(cleanup, [Fun | get(cleanup)]).

do_cleanup() ->
    [Fun() || Fun <- get(cleanup)],
    erase(cleanup).

cmd(Cmd) ->
    string:trim(os:cmd(unicode:characters_to_list(Cmd),
        #{ exception_on_failure => true })).

rfctime() ->
    unicode:characters_to_binary(
        calendar:system_time_to_rfc3339(erlang:system_time(second))).