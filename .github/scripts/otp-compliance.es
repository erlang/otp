#!/usr/bin/env escript
%% -*- erlang -*-

%% SPDX-License-Identifier: Apache-2.0
%% SPDX-FileCopyrightText: 2024 Erlang/OTP and its contributors

-define(default_classified_result, "scan-result-classified.json").
-define(default_scan_result, "scan-result.json").
-define(diff_classified_result, "scan-result-diff.json").
-define(license_ref_name, "LicenseRef-NOASSERTION").
-define(license_ref_copyright, "Erlang/OTP contributors").


%%
%% Commands
%%
%% sbom
%%
%%    otp-info: given an oss-review-toolkit (ORT) scan result and a
%%              source SBOM, it populates the fields that ORT can't
%%              in Unmanaged projects.
%%
%%    fix-licenses: fixes licenses from NOASSERTION to Apache-2.0
%%              for files that existed since OTP-18.
%%
%% compliance   useful for CI/CD compliance checks.
%%
%%    detect:   given a scan-result from ORT, it detects files without license
%%              and writes them into disk.
%%
%%    check:    given a recent scan-result from ORT (possibly from PR), and an
%%              existing file with known files without licenses (from prev. commit),
%%              calculate if new files without licenses have been added to the repo.
%%
%% explore
%%
%%    classify: takes as input a scan of ort and returns a json file containing
%%              as keys the licenses and as values the files under those licenses.
%%
%%    diff:     performs a diff of existing classification file against
%%              other classification files. this is useful to guarantee that
%%              files that had license X had not unexpectedly been reported differently.
%%

%%
%% USE OF COMMANDS
%%
%% The commands `classify` and `diff` are useful for exploring the licenses.
%% ORT does not report in an easy way which files have been attached to which licenses,
%% unless one generates a report. At the time, we cannot generate an SBOM,
%% so we are in the dark.
%%
%% The commands `detect` and `check` can be used in CI/CD to
%% prevent entering new files with unknown license. In the normal case,
%% the `detect` command only needs to be issued once in the repo.
%% Once we keep track of this file, the command is not needed anymore,
%% as the list of files with no license should not grow, and only
%% the `check` command should be executed in the CI/CD.
%%
%%

main(Args) ->
    argparse:run(Args, cli(), #{progname => otp_compliance}).

cli() ->
    #{ commands =>
           #{"sbom" =>
                 #{ help => """
                            Contains useful commands to fix an ORT generated source SBOM.

                            """,
                   commands =>
                        #{"otp-info" =>
                              #{ help =>
                                     """
                                     Adds information missing in ORT's Erlang/OTP source SBOM
                                       - Add homepage
                                       - Fixes license of `*.beam` files
                                       - Fixes project name

                                     """,
                                 arguments => [ sbom_option(),
                                                input_option() ],
                                 handler => fun sbom_otp/1}}},
             "explore" =>
                 #{  help => """
                            Explore license data.
                            Useful to figure out the mapping files-to-licenses.

                            """,
                    commands =>
                        #{"classify-license" =>
                              #{ help =>
                                     """
                                     Classify files by their license group.
                                       - Input file expects a scan-result from ORT.
                                       - Output file shows mapping between licenses and files.
                                         The output file can be fed to the `explore diff` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                output_option(?default_classified_result),
                                                apply_excludes(),
                                                apply_curations() ],
                                 handler => fun classify_license/1},
                          "classify-license-copyright" =>
                              #{ help =>
                                     """
                                     Pair files with their copyright and license.
                                     Depends on a `scan-result.json` and the output of the `classify-license`.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                base_file(?default_classified_result),
                                                output_option() ],
                                 handler => fun classify_path_license_copyright/1},

                          "reuse-gen-toml" =>
                              #{ help =>
                                     """
                                     Generates a TOML file based on the 'classify-license-copyright' results.
                                     Files with no license are given LicenseRef-UnknownLicense.txt.

                                     """,
                                   arguments => [input_option()],
                                   handler => fun reuse_gen_toml/1},
                          "diff" =>
                              #{ help =>
                                     """
                                     Compare against previous license results.
                                       - Input file should be the output of the `classify` command for input and base files.
                                       - Output returns a summary of additions and deletions per license.

                                     """,
                                 arguments => [ input_option(?default_classified_result),
                                                base_file(),
                                                output_option(?diff_classified_result) ],
                                 handler => fun diff/1}
                         }
                  },
             "compliance" =>
                 #{ help => """
                            Commands to enforce compliance policy towards unlicensed files.

                            """,
                    commands =>
                        #{"detect" =>
                              #{ help =>
                                     """
                                     Detects unlicensed files.
                                     - Input file expects a scan-result from ORT.
                                     - Output file is a list of files without license.
                                       The output file can be fed to the `compliance check` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                output_option(),
                                                apply_excludes() ],
                                 handler => fun detect_no_license/1},
                          "check" =>
                              #{ help =>
                                     """
                                     Checks that no new unlicensed files have been added.
                                     - Input file expects scan-result from ORT.
                                     - Base file expects output file from `no_license` command.

                                     """,
                                 arguments => [ input_option(?default_scan_result),
                                                base_file(),
                                                apply_excludes(),
                                                output_option() ],
                                 handler => fun check_no_license/1}}}}}.

%%
%% Options
%%
input_option() ->
    #{name => input_file,
      type => binary,
      long => "-input-file"}.


input_option(Default) ->
    (input_option())#{default => Default}.

sbom_option() ->
    #{name => sbom_file,
      type => binary,
      default => "bom.spdx.json",
      long => "-sbom-file"}.

output_option(Default) ->
    #{name => output_file,
      type => binary,
      default => Default,
      long => "-output-file"}.

output_option() ->
    #{name => output_file,
      type => binary,
      required => true,
      long => "-output-file"}.

apply_excludes() ->
    #{name => exclude,
      type => boolean,
      short => $e,
      default => true,
      long => "-apply-excludes"}.

apply_curations() ->
    #{name => curations,
      type => boolean,
      short => $c,
      default => true,
      long => "-apply-curations"}.

base_file() ->
    #{name => base_file,
      type => binary,
      long => "-base-file"}.
base_file(DefaultFile) ->
    #{name => base_file,
      type => binary,
      default => DefaultFile,
      long => "-base-file"}.


%%
%% Commands
%%

sbom_otp(#{sbom_file  := SbomFile}=Input) ->
    Sbom = decode(SbomFile),

    Licenses = path_to_license(Input),
    Copyrights = path_to_copyright(Input, Licenses),
    Fixes = sbom_fixes(Licenses, Copyrights),
    Spdx = execute_sbom_fixes(Sbom, Fixes),
    file:write_file(SbomFile, json:encode(Spdx)).

execute_sbom_fixes(Sbom, Fixes) ->
    lists:foldl(fun ({Fun, Data}, Acc) -> Fun(Data, Acc) end, Sbom, Fixes).

sbom_fixes(Licenses, Copyrights) ->
    [{fun fix_name/2, ~"Erlang/OTP"},
     {fun fix_creators_tooling/2, ~"Tool: otp_compliance"},
     {fun fix_supplier/2, ~"Organization: Ericsson AB"},
     {fun fix_download_location/2, ~"https://github.com/erlang/otp/releases"},
     {fun fix_beam_licenses/2, {Licenses, Copyrights}} ].


fix_name(Name, Sbom) ->
    Sbom#{ ~"name" := Name}.

fix_creators_tooling(Tool, #{ ~"creationInfo" := #{~"creators" := [ORT | _]}=Creators}=Sbom) ->
    SHA = list_to_binary(string:trim(".sha." ++ os:cmd("git rev-parse HEAD"))),
    Sbom#{~"creationInfo" := Creators#{ ~"creators" := [ORT, <<Tool/binary, SHA/binary>>]}}.

fix_supplier(Name, #{~"packages" := [ Packages ] }=Sbom) ->
    Sbom#{~"packages" := [maps:update_with(~"supplier", fun(_) -> Name end, Name, Packages)]}.


fix_download_location(Url, #{~"packages" := [ Packages ] }=Sbom) ->
    Packages1 = Packages#{~"downloadLocation" := Url },
    Sbom#{~"packages" := [ Packages1 ]}.

%% re-populate licenses to .beam files from their .erl files
%% e.g., the lists.beam file should have the same license as lists.erl
fix_beam_licenses(LicensesAndCopyrights,
                  #{ ~"packages" := [Package],
                     ~"files"   := Files}=Sbom) ->
    Package1 = Package#{ ~"homepage" := ~"https://www.erlang.org",
                         ~"licenseConcluded" := ~"Apache-2.0"},
    Files1= lists:map(
              fun (SPDX) ->
                      %% Adds license and copyright from .erl or .hrl file to its .beam equivalent
                      case SPDX of
                          #{~"fileName" := <<"lib/stdlib/uc_spec/", _Filename/binary>>,
                            ~"licenseInfoInFiles" := [License]}  when License =/= ~"NONE", License =/= ~"NOASSERTION"->
                              files_have_no_license(SPDX#{~"licenseConcluded" := License});

                          #{~"fileName" := ~"bootstrap/lib/stdlib/ebin/erl_parse.beam"} ->
                              %% beam file auto-generated from grammar file
                              files_have_no_license(fix_beam_spdx_license(~"lib/stdlib/src/erl_parse.yrl", LicensesAndCopyrights, SPDX));

                          #{~"fileName" := ~"bootstrap/lib/stdlib/ebin/unicode_util.beam"} ->
                              %% follows from otp/lib/stdlib/uc_spec/README-UPDATE.txt
                              files_have_no_license(SPDX#{~"licenseConcluded" := ~"Unicode-3.0 AND Apache-2.0"});

                          #{~"fileName" := <<"erts/emulator/internal_doc/",Filename/binary>>} ->
                              case binary:split(Filename, ~".md") of
                                  [_File, _Ext] ->
                                      SPDX#{~"licenseConcluded" := ~"Apache-2.0"};
                                  _ ->
                                      SPDX
                              end;

                          #{~"fileName" := Filename} ->
                              case bootstrap_mappings(Filename) of
                                  {error, not_beam_file} ->
                                      fix_spdx_license(SPDX);
                                  {Path, Filename1} ->
                                      case binary:split(Filename1, ~".beam") of
                                          [File, _] ->
                                              files_have_no_license(fix_beam_spdx_license(Path, File, LicensesAndCopyrights, SPDX));
                                          _ ->
                                              SPDX
                                      end
                              end
                          end
              end, Files),
    Sbom#{ ~"files" := Files1, ~"packages" := [Package1]}.

bootstrap_mappings(<<"bootstrap/lib/compiler/ebin/", Filename/binary>>) -> {~"lib/compiler/src/", Filename};
bootstrap_mappings(<<"bootstrap/lib/kernel/ebin/",Filename/binary>>) -> {<<"lib/kernel/src/">>, Filename};
bootstrap_mappings(<<"bootstrap/lib/kernel/include/",Filename/binary>>) -> {<<"lib/kernel/include/">>, Filename};
bootstrap_mappings(<<"bootstrap/lib/stdlib/ebin/",Filename/binary>>) -> {<<"lib/stdlib/src/">>, Filename};
bootstrap_mappings(<<"erts/preloaded/ebin/",Filename/binary>>) -> {<<"erts/preloaded/src/">>, Filename};
bootstrap_mappings(_Other) ->
    {error, not_beam_file}.

%% fixes spdx license of beam files
fix_beam_spdx_license(Path, {Licenses, Copyrights}, SPDX) ->
    License = maps:get(Path, Licenses, ~"NOASSERTION"),
    Copyright = maps:get(Path, Copyrights, ~"NOASSERTION"),
    fix_spdx_license(SPDX#{ ~"copyrightText" := Copyright, ~"licenseConcluded" := License }).

fix_beam_spdx_license(Path, File, LicensesAndCopyrights, SPDX) when is_binary(Path),
                                                                    is_binary(File) ->
    Spdx0 = fix_beam_spdx_license(<<Path/binary, File/binary, ".erl">>, LicensesAndCopyrights, SPDX),
    case maps:get(~"licenseConcluded", Spdx0) of
        ~"NOASSERTION" ->
            fix_beam_spdx_license(<<Path/binary, File/binary, ".hrl">>, LicensesAndCopyrights, Spdx0);
        _ ->
            Spdx0
    end.

files_have_no_license(Spdx) ->
    Spdx#{~"licenseInfoInFiles" := [~"NONE"]}.

none_to_noassertion(~"NONE") ->
    ~"NOASSERTION";
none_to_noassertion(X) ->
    X.

%% TODO: check which license curations have actually licenses in files, and which ones
%%       are added to annotate a file with a license. this latter should not be a curation
%%       in ORT, but in this script.
%% fixes spdx license of non-beam files
fix_spdx_license(#{~"licenseInfoInFiles" := [LicenseInFile],
                   ~"licenseConcluded" := License,
                   ~"copyrightText" := C}=SPDX) ->
    License1 = case License of
                   ~"NONE" -> LicenseInFile;
                   ~"NOASSERTION" -> LicenseInFile;
                   Other -> Other
               end,
    SPDX#{ ~"licenseConcluded" := none_to_noassertion(License1),
           ~"copyrightText" := none_to_noassertion(C)
         };
fix_spdx_license(#{~"copyrightText" := C}=SPDX) ->
    SPDX#{ ~"copyrightText" := none_to_noassertion(C)}.

%% Given an input file, returns a mapping of
%% #{filepath => license} for each file path towards its license.
path_to_license(Input) ->
    ClassifyInput = Input#{ exclude => true,
                            curations => false},
    ClassifyLicense = group_by_licenses(ClassifyInput),
    maps:fold(fun (K, Vs, Acc) ->
                      maps:merge(maps:from_keys(Vs, K), Acc)
              end, #{}, ClassifyLicense).

path_to_copyright(Input, _Licenses) ->
    ClassifyInput = Input#{ exclude => true},
    ClassifyCopyright = group_by_copyrights(ClassifyInput),
    maps:fold(fun (K, Vs, Acc) ->
                      maps:merge(maps:from_keys(Vs, K), Acc)
              end, #{}, ClassifyCopyright).

classify_license(#{output_file := Output}=Input) ->
    R = group_by_licenses(Input),
    ok = file:write_file(Output, json:encode(R)).

classify_path_license_copyright(#{output_file := Output,
                     input_file := Filename,
                     base_file  := LicenseFileGroup}) ->
    Copyrights = classify_copyright_result(Filename),
    Licenses = expand_license_result(LicenseFileGroup),
    Files = lists:sort(lists:uniq(maps:keys(Copyrights) ++ maps:keys(Licenses))),
    X = lists:foldl(fun (Path, Acc) ->
                          Copyright = maps:get(Path, Copyrights, ~"NONE"),
                          License = maps:get(Path, Licenses, ~"NONE"),
                          Acc#{Path => #{ ~"Copyright" => Copyright, ~"License" => License}}
                    end, #{}, Files),
    ok = file:write_file(Output, json:encode(X)).

expand_license_result(Filename) ->
    Json = decode(Filename),
    maps:fold(fun (License, Paths, Acc) ->
                      maps:merge(Acc, maps:from_list([{Path, License} || Path <- Paths]))
              end, #{}, Json).

classify_copyright_result(Filename) ->
    Json = decode(Filename),
    Copyrights = copyrights(scan_results(Json)),
    lists:foldl(fun (Copyright, Acc) ->
                        #{<<"statement">> := CopyrightSt, <<"location">> := Location} = Copyright,
                        #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
                        Acc#{Path => CopyrightSt}
                    end, #{}, Copyrights).

reuse_gen_toml(#{input_file := Input}) ->
    #{~"files" := Files} = decode(Input),
    GitIgnore = lists:foldl(fun (Path, Acc) ->
                        add_annotation(Path, ?license_ref_name, ?license_ref_copyright) ++ Acc
                end, "", gitignore_files()),
    Result = lists:foldl(fun(#{~"copyrightText" := C, ~"licenseConcluded" := L, ~"fileName" := Path}, Acc) ->
                               {LicenseString, CopyrightString}=
                                   case L of
                                       ~"NONE"  ->
                                           {?license_ref_name, "NOASSERTION"};
                                       ~"NOASSERTION" ->
                                           {?license_ref_name, "NOASSERTION"};
                                       _ ->
                                           case C of
                                               _ when C == ~"NOASSERTION"; C=="NONE" ->
                                                   {L, ?license_ref_copyright};
                                               _ ->
                                                   {L, C}
                                           end

                                   end,
                               add_annotation(Path, LicenseString, CopyrightString) ++ Acc
                       end, GitIgnore, Files),
    TOML = "version = 1\n\n" ++ Result,
    io:format("~ts", [TOML]).


add_annotation(Path, License, Copyright) ->
    LicenseId = io_lib:format("SPDX-License-Identifier = \"~ts\"\n", [License]),
    CopyrightId = io_lib:format("SPDX-FileCopyrightText = ~p\n", [conversion_from_bin(string:split(Copyright, "\n", all))]),
    io_lib:format("[[annotations]]\npath = \"~ts\"\n~s~s\n",
                  [Path, LicenseId, CopyrightId]).

conversion_from_bin([]) -> [];
conversion_from_bin([Bin | Ls]) when is_binary(Bin) ->
    [erlang:binary_to_list(Bin) | conversion_from_bin(Ls)];
conversion_from_bin([NotBin | Ls]) when is_list(NotBin) ->
    [NotBin | conversion_from_bin(Ls)].


group_by_licenses(#{input_file := Filename,
                    exclude := ApplyExclude,
                    curations := ApplyCuration}) ->
    Json = decode(Filename),
    Excludes = apply_excludes(Json, ApplyExclude),
    Curations = apply_curations(Json, ApplyCuration),

    Licenses = licenses(scan_results(Json)),
    lists:foldl(fun (License, Acc) ->
                            group_by_license(Excludes, Curations, License, Acc)
                    end, #{}, Licenses).

group_by_copyrights(#{input_file := Filename,
                      exclude := ApplyExclude}) ->
    Json = decode(Filename),
    Excludes = apply_excludes(Json, ApplyExclude),
    Copyrights = copyrights(scan_results(Json)),
    lists:foldl(fun (Copyright, Acc) ->
                            group_by_copyright(Excludes, Copyright, Acc)
                    end, #{}, Copyrights).


apply_excludes(Json, ApplyExclude) ->
    onlyif([], ApplyExclude, fun () -> convert_excludes(excludes(Json)) end).

apply_curations(Json, ApplyCuration) ->
    onlyif([], ApplyCuration, fun () -> curations(Json) end).

diff(#{input_file := InputFile, base_file := BaseFile, output_file := Output}) ->
    Input = decode(InputFile),
    Base = decode(BaseFile),
    KeyList = maps:keys(Input) ++ maps:keys(Base),
    KeySet = sets:from_list(KeyList),
    Data = sets:fold(fun(Key, Acc) -> set_difference(Key, Input, Base, Acc) end, #{}, KeySet),
    file:write_file(Output, json:encode(Data)).

detect_no_license(#{input_file := InputFile,
                    output_file := OutputFile,
                    exclude := ApplyExcludes}) ->
    Input = decode(InputFile),
    SortedResult = compute_unlicense_files(Input, ApplyExcludes),
    file:write_file(OutputFile, json:encode(SortedResult)).

compute_unlicense_files(Input, ApplyExcludes) ->
    Licenses = licenses(scan_results(Input)),

    PathsWithLicense =
        lists:foldl(fun (#{<<"location">> := #{<<"path">> := Path}}, Acc) ->
                            sets:add_element(Path, Acc)
                    end, sets:new(), Licenses),

    %% Get all files, incluiding those without license
    Files = files_from_scanner(Input),
    AllPaths =
        lists:foldl(fun (#{<<"path">> := Path}, Acc) ->
                            sets:add_element(Path, Acc)
                    end, sets:new(), Files),

    %% Paths without license
    PathsWithoutLicense = sets:to_list(sets:subtract(AllPaths, PathsWithLicense)),

    %% Excluded files that should be ignored
    Excludes = excludes(Input),
    ExcludeRegex = onlyif([], ApplyExcludes, fun () -> convert_excludes(Excludes) end),
    Result = lists:foldl(fun(Path, Acc) ->
                                 case exclude_path(Path, ExcludeRegex) of
                                     true ->
                                         Acc;
                                     false ->
                                         [Path | Acc]
                                 end
                         end, [], PathsWithoutLicense),
    lists:sort(Result).

check_no_license(#{input_file := InputFile,
                   base_file := BaseFile,
                   exclude := ApplyExcludes,
                   output_file := OutputFile}) ->
    UnlicenseNew = compute_unlicense_files(decode(InputFile), ApplyExcludes),
    Unlicense = decode(BaseFile),
    UnlicenseSet = sets:from_list(Unlicense),
    UnlicenseNewSet =  sets:from_list(UnlicenseNew),
    Result = sets:to_list(sets:subtract(UnlicenseNewSet, UnlicenseSet)),
    file:write_file(OutputFile, json:encode(Result)).


%%
%% Helper functions
%%

excludes(Input) ->
    try
        #{<<"repository">> :=
              #{<<"config">> :=
                    #{<<"excludes">> := #{<<"paths">> := Excludes}}}} = Input,
        Excludes
    catch
        _:_ ->
            []
    end.


curations(Input) ->
    #{<<"repository">> :=
          #{<<"config">> :=
                #{<<"curations">> := #{<<"license_findings">> := Curations}}}} = Input,
    Curations.

scan_results(Input) ->
    #{<<"scanner">> := #{<<"scan_results">> := [ScanResults]}} = Input,
    ScanResults.

licenses(Input) ->
    #{<<"summary">> := #{<<"licenses">> := Licenses}} = Input,
    Licenses.

copyrights(Input) ->
    #{<<"summary">> := #{<<"copyrights">> := Copyrights}} = Input,
    Copyrights.


files_from_scanner(Input) ->
    #{<<"scanner">> := #{<<"files">> := [#{<<"files">> := Files}]}} = Input,
    Files.

set_difference(Key, Input, Base, Acc) ->
    InputValues = sets:from_list(maps:get(Key, Input, [])),
    BaseValues = sets:from_list(maps:get(Key, Base, [])),
    Additions = sets:subtract(InputValues, BaseValues),
    Deletions = sets:subtract(BaseValues, InputValues),
    Acc#{Key => #{addition => sets:to_list(Additions), deletions => sets:to_list(Deletions)}}.

onlyif(_Default, true, Command) -> Command();
onlyif(Default, false, _Command) -> Default.

decode(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    json:decode(Bin).

group_by_license(ExcludeRegexes, Curations, License, Acc) ->
    #{<<"license">> := LicenseName, <<"location">> := Location} = License,
    #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
    maybe
        false ?= exclude_path(Path, ExcludeRegexes),
        LicenseName1 = curated_path_license(LicenseName, Path, Curations),
        case maps:get(LicenseName1, Acc, []) of
            [] ->
                Acc#{LicenseName1 => [Path]};
            Ls ->
                Ls1 = case lists:search(fun(X) -> X == Path end, Ls) of
                          false -> [Path | Ls];
                          _ -> Ls
                      end,
                Acc#{LicenseName1 => Ls1}
        end
    else
        _ ->
            Acc
    end.

group_by_copyright(ExcludeRegexes, Copyright, Acc) ->
    #{<<"statement">> := CopyrightSt, <<"location">> := Location} = Copyright,
    #{<<"path">> := Path, <<"start_line">> := _StartLine, <<"end_line">> := _EndLine} = Location,
    maybe
        false ?= exclude_path(Path, ExcludeRegexes),
        case maps:get(CopyrightSt, Acc, []) of
            [] ->
                Acc#{CopyrightSt => [Path]};
            Ls ->
                Ls1 = case lists:search(fun(X) -> X == Path end, Ls) of
                          false -> [Path | Ls];
                          _ -> Ls
                      end,
                Acc#{CopyrightSt => Ls1}
        end
    else
        _ ->
            Acc
    end.

convert_excludes(Excludes) ->
    lists:map(fun (#{<<"pattern">> := Pattern}) ->
                      Pattern1 = re:replace(Pattern, <<"\\.">>, <<"\\\\.">>, [global, {return, binary}]),
                      re:replace(Pattern1, <<"\\*\\*">>, <<".*">>, [global, {return, binary}])
              end, Excludes).

exclude_path(_Path, []) ->
    false;
exclude_path(Path, ExcludeRegexes) ->
    lists:any(fun (Regex) ->
                      case re:run(Path, Regex) of
                          {match, _} -> true;
                          _ -> false
                      end
              end, ExcludeRegexes).

curated_path_license(Name, _Path, []) -> Name;
curated_path_license(_Name, Path, [#{<<"path">> := Path}=Cur | _Curations]) ->
    maps:get(<<"concluded_license">>, Cur);
curated_path_license(Name, Path, [_Cur | Curations]) ->
    curated_path_license(Name, Path, Curations).

gitignore_files() ->
    [".gitattributes",
     ".gitignore",
     "erts/.gitignore",
     "erts/doc/assets/.gitignore",
     "erts/doc/src/.gitignore",
     "erts/emulator/internal_doc/assets/.gitignore",
     "erts/lib_src/yielding_c_fun/.gitignore",
     "erts/lib_src/yielding_c_fun/lib/simple_c_gc/.gitignore",
     "erts/lib_src/yielding_c_fun/test/examples/sha256_erlang_nif/.gitignore",
     "erts/lib_src/yielding_c_fun/test/examples/sha256_erlang_nif/c_src/sha-2/.gitignore",
     "erts/preloaded/.gitignore",
     "erts/preloaded/src/.gitignore",
     "lib/.gitignore",
     "lib/asn1/src/.gitignore",
     "lib/common_test/test_server/.gitignore",
     "lib/compiler/scripts/.gitignore",
     "lib/compiler/src/.gitignore",
     "lib/compiler/test/core_SUITE_data/.gitignore",
     "lib/dialyzer/doc/.gitignore",
     "lib/dialyzer/test/incremental_SUITE_data/extra_modules/ebin/.gitignore",
     "lib/dialyzer/test/options1_SUITE_data/my_include/CVS/Entries",
     "lib/dialyzer/test/options1_SUITE_data/my_include/CVS/Repository",
     "lib/dialyzer/test/options1_SUITE_data/my_include/CVS/Root",
     "lib/diameter/.gitignore",
     "lib/diameter/doc/.gitignore",
     "lib/diameter/doc/src/.gitignore",
     "lib/diameter/ebin/.gitignore",
     "lib/diameter/examples/.gitignore",
     "lib/diameter/examples/dict/.gitignore",
     "lib/diameter/src/.gitignore",
     "lib/diameter/src/gen/.gitignore",
     "lib/diameter/test/.gitignore",
     "lib/edoc/.gitignore",
     "lib/edoc/doc/assets/.gitignore",
     "lib/edoc/doc/chunks/.gitignore",
     "lib/eldap/.gitignore",
     "lib/eunit/doc/.gitignore",
     "lib/inets/priv/plt/.gitignore",
     "lib/jinterface/.gitignore",
     "lib/jinterface/doc/assets/.gitignore",
     "lib/jinterface/test/jinterface_SUITE_data/.gitignore",
     "lib/jinterface/test/nc_SUITE_data/.gitignore",
     "lib/kernel/doc/src/.gitignore",
     "lib/kernel/test/interactive_shell_SUITE_data/.gitignore",
     "lib/megaco/.gitignore",
     "lib/megaco/doc/specs/.gitignore",
     "lib/megaco/priv/plt/.gitignore",
     "lib/mnesia/test/.gitignore",
     "lib/odbc/doc/specs/.gitignore",
     "lib/public_key/.gitignore",
     "lib/sasl/test/.gitignore",
     "lib/sasl/test/release_handler_SUITE_data/relocatable_release/hello_server/ebin/.gitignore",
     "lib/sasl/test/release_handler_SUITE_data/relocatable_release/hello_server_new/ebin/.gitignore",
     "lib/snmp/.gitignore",
     "lib/snmp/doc/specs/.gitignore",
     "lib/snmp/include/.gitignore",
     "lib/snmp/priv/mibs/.gitignore",
     "lib/snmp/priv/plt/.gitignore",
     "lib/snmp/test/snmp_test_data/.gitignore",
     "lib/snmp/test/test_config/.gitignore",
     "lib/ssh/src/.gitignore",
     "lib/ssh/test/.gitignore",
     "lib/ssl/src/.gitignore",
     "lib/syntax_tools/doc/assets/.gitignore",
     "lib/wx/.gitignore",
     "system/doc/.gitignore",
     "system/doc/assets/.gitignore",
     "system/doc/general_info/.gitignore",
     "system/doc/installation_guide/.gitignore",
     "system/doc/top/.gitignore"].
