%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2019. All Rights Reserved.
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

-module(otp_SUITE).

-export([all/0, suite/0,
         init_per_suite/1,end_per_suite/1]).
-export([undefined_functions/1,deprecated_not_in_obsolete/1,
         obsolete_but_not_deprecated/1,call_to_deprecated/1,
         call_to_size_1/1,call_to_now_0/1,strong_components/1,
         erl_file_encoding/1,xml_file_encoding/1,
         runtime_dependencies_functions/1,
         runtime_dependencies_modules/1,
         test_runtime_dependencies_versions/1]).

-include_lib("common_test/include/ct.hrl").

-import(lists, [filter/2,foldl/3,foreach/2]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 30}}].

all() ->
    [undefined_functions, deprecated_not_in_obsolete,
     obsolete_but_not_deprecated, call_to_deprecated,
     call_to_size_1, call_to_now_0, strong_components,
     erl_file_encoding, xml_file_encoding,
     runtime_dependencies_functions,
     runtime_dependencies_modules,
     test_runtime_dependencies_versions
].

init_per_suite(Config) ->
    Server = start_xref_server(daily_xref, functions),
    [{xref_server,Server}|Config].

end_per_suite(Config) ->
    Server = proplists:get_value(xref_server, Config),
    catch xref:stop(Server),
    Config.

undefined_functions(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),

    %% Exclude calls from generated modules in the SSL application.
    ExcludeFrom = "SSL-PKIX|PKIX.*|ssl_pkix_oid",
    UndefS = xref_base:analysis(undefined_function_calls),
    Q = io_lib:format("Undef = ~s,"
                      "ExcludedFrom = ~p:_/_,"
                      "Undef - Undef | ExcludedFrom",
                      [UndefS,ExcludeFrom]),
    {ok,Undef0} = xref:q(Server, lists:flatten(Q)),

    Filters = [fun ssl_crypto_filter/1,
               fun eunit_filter/1,
               fun dialyzer_filter/1,
               fun wx_filter/1,
               fun diameter_filter/1],
    Undef = lists:foldl(fun(Filter, Acc) ->
                                Filter(Acc)
                        end, Undef0, Filters),

    case Undef of
        [] -> ok;
        _ ->
            Fd = open_log(Config, "undefined_functions"),
            foreach(fun ({MFA1,MFA2}) ->
                            ct:pal("~s calls undefined ~s",
                                   [format_mfa(Server, MFA1),
                                    format_mfa(MFA2)]),
                            io:format(Fd, "~s ~s\n",
                                      [format_mfa(Server, MFA1),
                                       format_mfa(MFA2)])
                    end, Undef),
            close_log(Fd),
            ct:fail({length(Undef),undefined_functions_in_otp})
    end.

ssl_crypto_filter(Undef) ->
    case {app_exists(crypto),app_exists(ssl)} of
        {false,false} ->
            filter(fun({_,{ssl,_,_}}) -> false;
                      ({_,{crypto,_,_}}) -> false;
                      ({_,{ssh,_,_}}) -> false;
                      ({_,{ssh_connection,_,_}}) -> false;
                      ({_,{ssh_sftp,_,_}}) -> false;
                      (_) -> true
                   end, Undef);
        {_,_} -> Undef
    end.

eunit_filter(Undef) ->
    filter(fun({{eunit_test,wrapper_test_exported_,0},
                {eunit_test,nonexisting_function,0}}) -> false;
              (_) -> true
           end, Undef).

dialyzer_filter(Undef) ->
    case app_exists(dialyzer) of
        false ->
            filter(fun({_,{dialyzer_callgraph,_,_}}) -> false;
                      ({_,{dialyzer_codeserver,_,_}}) -> false;
                      ({_,{dialyzer_contracts,_,_}}) -> false;
                      ({_,{dialyzer_cl_parse,_,_}}) -> false;
                      ({_,{dialyzer_timing,_,_}}) -> false;
                      ({_,{dialyzer_plt,_,_}}) -> false;
                      ({_,{dialyzer_succ_typings,_,_}}) -> false;
                      ({_,{dialyzer_utils,_,_}}) -> false;
                      (_) -> true
                   end, Undef);
        _ -> Undef
    end.

wx_filter(Undef) ->
    case app_exists(wx) of
        false ->
            filter(fun({_,{MaybeWxModule,_,_}}) ->
                           case atom_to_list(MaybeWxModule) of
                               "wx"++_ -> false;
                               _ -> true
                           end
                   end, Undef);
        _ -> Undef
    end.

diameter_filter(Undef) ->
    %% Filter away function calls that are catched.
    filter(fun({{diameter_lib,_,_},{erlang,convert_time_unit,3}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,monotonic_time,0}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,unique_integer,0}}) ->
                   false;
              ({{diameter_lib,_,_},{erlang,time_offset,0}}) ->
                   false;
              (_) -> true
           end, Undef).

deprecated_not_in_obsolete(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,DeprecatedFunctions} = xref:q(Server, "DF"),

    L = foldl(fun({M,F,A}=MFA, Acc) ->
                      case otp_internal:obsolete(M, F, A) of
                          no -> [MFA|Acc];
                          _ -> Acc
                      end
              end, [], DeprecatedFunctions),
    case L of
        [] -> ok;
        _ ->
            io:put_chars("The following functions have -deprecated() attributes,\n"
                         "but are not listed in otp_internal:obsolete/3.\n"),
            print_mfas(group_leader(), Server, L),
            Fd = open_log(Config, "deprecated_not_obsolete"),
            print_mfas(Fd, Server, L),
            close_log(Fd),
            ct:fail({length(L),deprecated_but_not_obsolete})
    end.

obsolete_but_not_deprecated(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,NotDeprecated} = xref:q(Server, "X - DF"),

    L = foldl(fun({M,F,A}=MFA, Acc) ->
                      case otp_internal:obsolete(M, F, A) of
                          no -> Acc;
                          _ -> [MFA|Acc]
                      end
              end, [], NotDeprecated),

    case L of
        [] -> ok;
        _ ->
            io:put_chars("The following functions are listed "
                         "in otp_internal:obsolete/3,\n"
                         "but don't have -deprecated() attributes.\n"),
            print_mfas(group_leader(), Server, L),
            Fd = open_log(Config, "obsolete_not_deprecated"),
            print_mfas(Fd, Server, L),
            close_log(Fd),
            ct:fail({length(L),obsolete_but_not_deprecated})
    end.

call_to_deprecated(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,DeprecatedCalls} = xref:q(Server, "strict(E || DF)"),
    foreach(fun ({MFA1,MFA2}) ->
                    io:format("~s calls deprecated ~s",
                              [format_mfa(MFA1),format_mfa(MFA2)])
            end, DeprecatedCalls),
    {comment,integer_to_list(length(DeprecatedCalls))++" calls to deprecated functions"}.

call_to_size_1(Config) when is_list(Config) ->
    %% Applications that do not call erlang:size/1:
    Apps = [asn1,compiler,debugger,kernel,observer,parsetools,
            runtime_tools,stdlib,tools],
    not_recommended_calls(Config, Apps, {erlang,size,1}).

call_to_now_0(Config) when is_list(Config) ->
    %% Applications that do not call erlang:now/1:
    Apps = [asn1,common_test,compiler,debugger,dialyzer,
            kernel,mnesia,observer,parsetools,reltool,
            runtime_tools,sasl,stdlib,syntax_tools,
            tools],
    not_recommended_calls(Config, Apps, {erlang,now,0}).

not_recommended_calls(Config, Apps0, MFA) ->
    Server = proplists:get_value(xref_server, Config),

    Apps = [App || App <- Apps0, is_present_application(App, Server)],

    Fs = [MFA],

    Q1 = io_lib:format("E || ~p : Fun", [Fs]),
    {ok,AllCallsToMFA} = xref:q(Server, lists:flatten(Q1)),

    Q2 = io_lib:format("E | ~p : App || ~p : Fun", [Apps,Fs]),
    {ok,CallsToMFA} = xref:q(Server, lists:flatten(Q2)),

    case CallsToMFA of
        [] ->
            ok;
        _ ->
            io:format("These calls are not allowed:\n"),
            foreach(fun ({MFA1,MFA2}) ->
                            io:format("~s calls non-recommended ~s",
                                      [format_mfa(MFA1),format_mfa(MFA2)])
                    end, CallsToMFA)
    end,

    %% Enumerate calls to MFA from other applications than
    %% the ones known not to call MFA:
    case AllCallsToMFA--CallsToMFA of
        [] ->
            ok;
        Calls ->
            io:format("~n~nThese calls are allowed for now:\n"),
            foreach(fun ({MFA1,MFA2}) ->
                            io:format("~s calls non-recommended ~s",
                                      [format_mfa(MFA1),format_mfa(MFA2)])
                    end, Calls)
    end,
    case CallsToMFA of
        [] ->
            SkippedApps = ordsets:subtract(ordsets:from_list(Apps0),
                                           ordsets:from_list(Apps)),
            case SkippedApps of
                [] ->
                    ok;
                _ ->
                    AppStrings = [atom_to_list(A) || A <- SkippedApps],
                    Mess = io_lib:format("Application(s) not present: ~s\n",
                                         [lists:join(", ", AppStrings)]),
                    {comment, Mess}
            end;
        _ ->
            ct:fail({length(CallsToMFA),calls_to_size_1})
    end.

is_present_application(Name, Server) ->
    Q = io_lib:format("~w : App", [Name]),
    case xref:q(Server, lists:flatten(Q)) of
        {ok,[Name]} -> true;
        {error,_,_} -> false
    end.

strong_components(Config) when is_list(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,Cs} = xref:q(Server, "components AE"),
    io:format("\n\nStrong components:\n\n~p\n", [Cs]),
    ok.

erl_file_encoding(_Config) ->
    Root = code:root_dir(),
    Wc = filename:join([Root,"**","*.erl"]),
    ErlFiles = ordsets:subtract(ordsets:from_list(filelib:wildcard(Wc)),
                                release_files(Root, "*.erl")),
    {ok, MP} = re:compile(".*lib/(ic)|(orber)|(cos).*", [unicode]),
    Fs = [F || F <- ErlFiles,
               filter_use_latin1_coding(F, MP),
               case epp:read_encoding(F) of
                   none -> false;
                   _ -> true
               end],
    case Fs of
        [] ->
            ok;
        [_|_] ->
            io:put_chars("Files with \"coding:\":\n"),
            [io:put_chars(F) || F <- Fs],
            ct:fail(failed)
    end.

filter_use_latin1_coding(F, MP) ->
    case re:run(F, MP) of
        nomatch ->
            true;
        {match, _} ->
            false
    end.

xml_file_encoding(_Config) ->
    XmlFiles = xml_files(),
    Fs = [F || F <- XmlFiles, is_bad_encoding(F)],
    case Fs of
        [] ->
            ok;
        [_|_] ->
            io:put_chars("Encoding should be \"utf-8\" or \"UTF-8\":\n"),
            [io:put_chars(F) || F <- Fs],
            ct:fail(failed)
    end.

xml_files() ->
    Root = code:root_dir(),
    AllWc = filename:join([Root,"**","*.xml"]),
    AllXmlFiles = ordsets:from_list(filelib:wildcard(AllWc)),
    TestsWc = filename:join([Root,"lib","*","test","**","*.xml"]),
    TestXmlFiles = ordsets:from_list(filelib:wildcard(TestsWc)),
    XmerlWc = filename:join([Root,"lib","xmerl","**","*.xml"]),
    XmerlXmlFiles = ordsets:from_list(filelib:wildcard(XmerlWc)),
    Ignore = ordsets:union([TestXmlFiles,XmerlXmlFiles,
                            release_files(Root, "*.xml")]),
    ordsets:subtract(AllXmlFiles, Ignore).

release_files(Root, Ext) ->
    Wc = filename:join([Root,"release","**",Ext]),
    filelib:wildcard(Wc).

is_bad_encoding(File) ->
    {ok,Bin} = file:read_file(File),
    case Bin of
        <<"<?xml version=\"1.0\" encoding=\"utf-8\"",_/binary>> ->
            false;
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"",_/binary>> ->
            false;
        _ ->
            true
    end.

%% Test runtime dependencies when using an Xref server running in
%% 'functions' mode.

runtime_dependencies_functions(Config) ->
    Server = proplists:get_value(xref_server, Config),
    runtime_dependencies(Server).

%% Test runtime dependencies when using an xref server running in
%% 'modules' mode. Note that more module edges can potentially be
%% found in this mode because the analysis is based on the BEAM
%% code after all optimizations. For example, an apply in the source
%% code could after optimizations be resolved to a specific function.
%%
%% It is important to test 'modules' because reltool runs xref in
%% 'modules' mode (the BEAM files to be released might not contain
%% debug information).

runtime_dependencies_modules(_Config) ->
    Server = start_xref_server(?FUNCTION_NAME, modules),
    try
        runtime_dependencies(Server)
    after
        catch xref:stop(Server)
    end.

runtime_dependencies(Server) ->
    %% Ignore applications intentionally not declaring dependencies
    %% found by xref.
    IgnoreApps = [diameter],

    %% Verify that (at least) OTP application runtime dependencies found
    %% by xref are listed in the runtime_dependencies field of the .app file
    %% of each application.
    {ok, AE} = xref:q(Server, "AE"),
    SAE = lists:keysort(1, AE),
    put(ignored_failures, []),
    {AppDep, AppDeps} = lists:foldl(fun ({App, App}, Acc) ->
                                            Acc;
                                        ({App, Dep}, {undefined, []}) ->
                                            {{App, [Dep]}, []};
                                        ({App, Dep}, {{App, Deps}, AppDeps}) ->
                                            {{App, [Dep|Deps]}, AppDeps};
                                        ({App, Dep}, {AppDep, AppDeps}) ->
                                            {{App, [Dep]}, [AppDep | AppDeps]}
                                    end,
                                    {undefined, []},
                                    SAE),
    [] = check_apps_deps([AppDep|AppDeps], IgnoreApps),
    case IgnoreApps of
        [] ->
            ok;
        _ ->
            Comment = lists:flatten(io_lib:format("Ignored applications: ~p "
                                                  "Ignored failures: ~p",
                                                  [IgnoreApps,
                                                   get(ignored_failures)])),
            {comment, Comment}
    end.

have_rdep(_App, [], _Dep) ->
    false;
have_rdep(App, [RDep | RDeps], Dep) ->		    
    [AppStr, _VsnStr] = string:lexemes(RDep, "-"),
    case Dep == list_to_atom(AppStr) of
        true ->
            %% io:format("~p -> ~s~n", [App, RDep]),
            true;
        false ->
            have_rdep(App, RDeps, Dep)
    end.

check_app_deps(_App, _AppFile, _AFDeps, [], _IgnoreApps) ->
    [];
check_app_deps(App, AppFile, AFDeps, [XRDep | XRDeps], IgnoreApps) ->
    ResOtherDeps = check_app_deps(App, AppFile, AFDeps, XRDeps, IgnoreApps),
    case have_rdep(App, AFDeps, XRDep) of
        true ->
            ResOtherDeps;
        false ->
            Failure = {missing_runtime_dependency, AppFile, XRDep},
            case lists:member(App, IgnoreApps) of
                true ->
                    put(ignored_failures, [Failure | get(ignored_failures)]),
                    ResOtherDeps;
                false ->
                    [Failure | ResOtherDeps]
            end
    end.

check_apps_deps([], _IgnoreApps) ->
    [];
check_apps_deps([{App, Deps}|AppDeps], IgnoreApps) ->
    ResOtherApps = check_apps_deps(AppDeps, IgnoreApps),
    AppFile = code:where_is_file(atom_to_list(App) ++ ".app"),
    {ok,[{application, App, Info}]} = file:consult(AppFile),
    case lists:keyfind(runtime_dependencies, 1, Info) of
        {runtime_dependencies, RDeps} ->
            check_app_deps(App, AppFile, RDeps, Deps, IgnoreApps)
            ++ ResOtherApps;
        false ->
            Failure = {missing_runtime_dependencies_key, AppFile},
            case lists:member(App, IgnoreApps) of
                true ->
                    put(ignored_failures, [Failure | get(ignored_failures)]),
                    ResOtherApps;
                false ->
                    [Failure | ResOtherApps]
            end
    end.

%%%
%%% Common help functions.
%%%

print_mfas(Fd, Server, MFAs) ->
    [io:format(Fd, "~s\n", [format_mfa(Server, MFA)]) || MFA <- MFAs],
    ok.

format_mfa({M,F,A}) ->
    lists:flatten(io_lib:format("~s:~s/~p", [M,F,A])).

format_mfa(Server, MFA) ->
    MFAString = format_mfa(MFA),
    AQ = "(App)" ++ MFAString,
    AppPrefix = case xref:q(Server, AQ) of
                    {ok,[App]} -> "[" ++ atom_to_list(App) ++ "]";
                    _ -> ""
                end,
    AppPrefix ++ MFAString.

open_log(Config, Name) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    RunDir = filename:dirname(filename:dirname(PrivDir)),
    Path = filename:join(RunDir, "system_"++Name++".log"),
    {ok,Fd} = file:open(Path, [write]),
    Fd.

close_log(Fd) ->
    ok = file:close(Fd).

app_exists(AppAtom) ->
    case code:lib_dir(AppAtom) of
        {error,bad_name} ->
            false;
        Path ->
            case file:read_file_info(filename:join(Path,"ebin")) of
                {ok,_} ->
                    true;
                _ ->
                    false
            end
    end.

start_xref_server(Server, Mode) ->
    Root = code:root_dir(),
    xref:start(Server, [{xref_mode,Mode}]),
    xref:set_default(Server, [{verbose,false},
                              {warnings,false},
                              {builtins,true}]),
    {ok,_Relname} = xref:add_release(Server, Root, {name,otp}),

    case code:lib_dir(erts) of
        {error,bad_name} ->
            %% This should not be possible since code_server always adds
            %% an entry for erts.
            ct:fail(no_erts_lib_dir);
        LibDir ->
            case filelib:is_dir(filename:join(LibDir, "ebin")) of
                false ->
                    %% If we are running the tests in the git repository,
                    %% the preloaded BEAM files for Erts are not in the
                    %% code path. We must add them explicitly.
                    Erts = filename:join([LibDir,"preloaded","ebin"]),
                    {ok,_} = xref:add_directory(Server, Erts, []);
                true ->
                    ok
            end
    end,
    Server.

get_suite_data_dir_path() ->
    filename:join(filename:dirname(code:which(?MODULE)), "otp_SUITE_data").

get_otp_versions_table_path() ->
    filename:join(get_suite_data_dir_path(), "otp_versions.table").

get_otp_version_tickets_path() ->
    filename:join(get_suite_data_dir_path(), "otp_version_tickets").

%% Return a map that maps from app versions to the OTP versions they
%% were last released in. The function makes use of the file
%% "otp_versions.table" and the current code:get_path() to
%% find apps.
get_runtime_dep_to_otp_version_map() ->
    %% Find apps in "otp_versions.table"
    VersionsTableFile = get_otp_versions_table_path(),
    VersionsTableBin =
        case file:read_file(VersionsTableFile) of
            {ok, Bin} -> Bin;
            Error -> ct:fail("Could not read the file ~s which is needed to perform the test. "
                             "Error: ~p~n",
                             [VersionsTableFile, Error])
        end,
    VersionsTableStr = erlang:binary_to_list(VersionsTableBin),
    Lines = lists:reverse(string:tokens(VersionsTableStr, "\n")),
    AddVersionsInString =
        fun(Map, OTPVersion, AppVersionsString0) ->
                AppVersionsString =
                    lists:flatten(string:replace(AppVersionsString0, "#", "", all)),
                AppVersions = string:tokens(AppVersionsString, " "),
                lists:foldl(
                  fun(AppVersion0, MapSoFar) ->
                          case string:trim(AppVersion0) of
                              "" ->
                                  MapSoFar;
                              AppVersion1 ->
                                  maps:put(AppVersion1, OTPVersion, MapSoFar)
                          end
                  end,
                  Map,
                  AppVersions)
        end,
    VersionMap0 =
        lists:foldl(
          fun(Line, MapSoFar) ->
                  [OTPVersion, AppVersionsString| _] =
                      string:tokens(Line, ":"),
                  AddVersionsInString(MapSoFar,
                                      string:trim(OTPVersion),
                                      string:trim(AppVersionsString))
          end,
          #{},
          Lines),
    %% Find apps in code:get_path()
    lists:foldl(
      fun(Path, MapSoFar) ->
              case filelib:wildcard(filename:join(Path, "*.app")) of
                  [AppFile] ->
                      {ok,[{application, App, Info}]} = file:consult(AppFile),
                      case lists:keyfind(vsn, 1, Info) of
                          false ->
                              MapSoFar;
                          {vsn, VsnStr} ->
                              AppVsnStr =
                                  erlang:atom_to_list(App) ++ "-" ++ VsnStr,
                              maps:put(AppVsnStr, {latest, Path}, MapSoFar)
                      end;
                  _ ->
                      MapSoFar
              end
      end,
      VersionMap0,
      code:get_path()).

%% Find runtime dependencies for an app
get_runtime_deps(App) ->
    AppFile = code:where_is_file(atom_to_list(App) ++ ".app"),
    {ok,[{application, App, Info}]} = file:consult(AppFile),
    case lists:keyfind(runtime_dependencies, 1, Info) of
        {runtime_dependencies, RDeps} ->
            RDeps;
        false ->
            []
    end.

%% Given a release dir find the path to the given dependency
find_dep_in_rel_dir(Dep, RelDirRoot) ->
    %% The dependencies that we have found are cached to avoid
    %% searching through the file system unecessary many times
    CacheId = runtime_dep_test_cache,
    DepCache =
        case erlang:get(CacheId) of
            undefined -> #{};
            M-> M
        end,
    case maps:get(Dep, DepCache, none) of
        none ->
            DepPaths = filelib:wildcard(filename:join([RelDirRoot, "lib", "**", Dep, "ebin"])),
            case DepPaths of
                [Path] ->
                    erlang:put(CacheId, maps:put(Dep, Path, DepCache)),
                    Path;
                _ ->
                    ErrorMessage =
                        io_lib:format("ERROR: Could not find ~p in ~p (where it is supposed to be)."
                                      "Found ~p~n", [Dep, RelDirRoot, DepPaths]),
                    io:format(ErrorMessage),
                    ct:fail(ErrorMessage)

            end;
        Path ->
            Path
    end.

%% Get the major OTP version part of an OTP version string
%% Example: OTP-22.0 gives 22
get_major_version(OtpVersion) ->
    [_,MajorVersion|_] = string:tokens(OtpVersion, "-."),
    MajorVersion.

%% Returns the release directory for the oldest available OTP release
%% on the current machine
first_available_otp_rel() ->
    %% At least one version less than the current version should be available
    SholdBeAvailable = erlang:list_to_integer(erlang:system_info(otp_release)) - 1,
    (fun FindOldest(CurrRel, PrevAvailable) ->
             case test_server:is_release_available(erlang:integer_to_list(CurrRel)) of
                 false -> PrevAvailable;
                 true -> FindOldest(CurrRel-1, erlang:integer_to_list(CurrRel))
             end
     end)(SholdBeAvailable, none).

%% Searches for the oldest available version of Dep
get_oldest_available_version_of_dep(Dep) ->
    [DepName, _Version] = string:tokens(Dep, "-"),
    FirstAvailableRel = first_available_otp_rel(),
    (fun Find(LookInRel) ->
             case test_server:is_release_available(LookInRel) of
                 true ->
                     RelRoot = find_rel_root(LookInRel),
                     Options0 = filelib:wildcard(filename:join([RelRoot, "lib", "**", "ebin"])),
                     Options1 = [Opt ||
                                   Opt <- Options0,
                                   string:find(Opt,
                                               filename:join("lib", DepName ++ "-")) =/= nomatch],
                     GetVersionTuple =
                         fun(Path) ->
                                 [AppVerStr] =
                                     [C || C <- filename:split(Path),
                                           string:find(C, DepName ++ "-") =/= nomatch],
                                 [_,VerStr] = string:tokens(AppVerStr, "-"),
                                 erlang:list_to_tuple([erlang:list_to_integer(X) ||
                                                          X <- string:tokens(VerStr, ".")])
                         end,
                     Options2 =
                         lists:sort(fun(A,B) ->
                                            GetVersionTuple(A) =< GetVersionTuple(B)
                                    end,
                                    Options1),
                     case Options2 of
                         [Path|_] ->
                             Path;
                         _ ->
                             NextRelToTry =
                                 erlang:integer_to_list(erlang:list_to_integer(LookInRel)+1),
                             Find(NextRelToTry)
                     end;
                 false ->
                     ct:fail({could_not_find_dep_anywhere, DepName})
             end
     end)(FirstAvailableRel).

find_rel_root(Rel) ->
    case test_server:find_release(Rel) of
        not_available ->
            not_available;
        OtpRelErl -> filename:dirname(filename:dirname(OtpRelErl))
    end.

%% Find the absoulte paths to RuntimeDeps
get_paths_to_dependencies(App, RuntimeDeps) ->
    FirstAvailableOTPRel = first_available_otp_rel(),
    FirstAvailableRel = erlang:list_to_integer(FirstAvailableOTPRel),
    DepToOtpVerMap = get_runtime_dep_to_otp_version_map(),
    lists:foldl(
      fun(Dep, SoFar) ->
              case maps:get(Dep, DepToOtpVerMap, false) of
                  false ->
                      ct:fail(io_lib:format(
                                "The dependency ~s for ~p could not be found. "
                                "Have you typed a non-existing version?",
                                [Dep, App]));
                  {latest, Path} ->
                      %% The dependency is in Path (one of the paths returned by code:get_paths())
                      [Path | SoFar];
                  OtpVersionWithDep ->
                      OtpMajorVersionWithDep = get_major_version(OtpVersionWithDep),
                      OtpMajorVersionWithDepInt = erlang:list_to_integer(OtpMajorVersionWithDep),
                      case find_rel_root(OtpMajorVersionWithDep) of
                          not_available when FirstAvailableRel > OtpMajorVersionWithDepInt ->
                              io:format("Warning: Could not find runtime dependency ~p for ~p. "
                                        "~p belongs to ~p but ~p is too old to be available on this machine. "
                                        "Trying to find the oldest available version of ~p...",
                                        [Dep, App, Dep, OtpVersionWithDep, OtpVersionWithDep, Dep]),
                              [get_oldest_available_version_of_dep(Dep) | SoFar];
                          RelDirRoot ->
                              [find_dep_in_rel_dir(Dep, RelDirRoot) | SoFar]
                      end
              end
      end,
      [],
      RuntimeDeps).

test_app_runtime_deps_versions(AppPath, App, IgnoredUndefinedFunctions) ->
    %% Get a list of all runtime dependencies for app
    RuntimeDeps = get_runtime_deps(App),
    %% Get paths to the found runtime dependices
    DepPaths = get_paths_to_dependencies(App, RuntimeDeps),
    XRefSName = test_app_runtime_deps_versions_server,
    %% Start xref server and do the test
    {ok, _} = xref:start(XRefSName, []),
    ok = xref:set_library_path(XRefSName, DepPaths),
    Dir = filename:join(AppPath, "ebin"),
    {ok, _} = xref:add_directory(XRefSName, Dir),
    {ok, UndefinedFunctions0} = xref:analyze(XRefSName, undefined_functions),
    xref:stop(XRefSName),
    %% Filter out undefined functions that we should ignore
    UndefinedFunctions1 =
        [F || F <- UndefinedFunctions0,
              not maps:get(F, IgnoredUndefinedFunctions, false)],
    case UndefinedFunctions1 of
        [] ->
            ok;
        UndefinedFunctions ->
            {error_undefined_functions_in_app, App, UndefinedFunctions}
    end.

test_runtime_dependencies_versions_rels(IgnoreApps, AppsToIgnoredUndefinedFunctions) ->
    %% Do for every application:
    %%    1. Find deps from app file
    %%    2. Find where the deps are installed
    %%    3. Run xref tests on the apps with the specified dependencies
    %%    4. Report error if undefined function
    Apps0 = [{Path, list_to_atom(AppName)}
             || {match, [Path, AppName]}
                    <- [re:run(X,"(" ++ code:lib_dir()++"/"++"([^/-]*).*)/ebin",
                               [{capture,[1,2],list},unicode]) || X <- code:get_path()]],
    Apps = [{Path, App} ||
               {Path, App} <- Apps0,
               code:where_is_file(atom_to_list(App) ++ ".app") =/= non_existing],
    Res = [test_app_runtime_deps_versions(AppPath,
                                          App,
                                          maps:get(App, AppsToIgnoredUndefinedFunctions, #{})) ||
              {AppPath, App} <- Apps, not lists:member(App, IgnoreApps)],
    BadRes = [R || R <- Res, R =/= ok],
    case BadRes =:= [] of
        true -> ok;
        _ ->
            ct:fail(BadRes)
    end.

is_development_build() ->
    {ok, FileContentBin} = file:read_file(get_otp_version_tickets_path()),
    "DEVELOPMENT" =:= string:trim(erlang:binary_to_list(FileContentBin), both, "\n ").

test_runtime_dependencies_versions(_Config) ->
    ReleasesDir = "/usr/local/otp/releases",
    IgnoreApps = [],
    AppsToIgnoredUndefinedFunctions =
        #{eunit =>
              %% Intentional call to nonexisting function
              #{{eunit_test, nonexisting_function, 0} => true},
         diameter =>
              %% The following functions are optional dependencies for diameter
              #{{dbg,ctp,0} => true,
                {dbg,p,2} => true,
                {dbg,stop_clear,0} => true,
                {dbg,trace_port,2} => true,
                {dbg,tracer,2} => true,
                {erl_prettypr,format,1} => true,
                {erl_syntax,form_list,1} => true},
         common_test =>
              %% ftp:start/0 has been part of the ftp application from
              %% the beginning so it is unclear why xref report this
              %% as undefined
              #{{ftp,start,0} => true}},
    case {erlang:element(1, os:type()) =:= unix,
          not is_development_build(),
          filelib:is_dir(ReleasesDir),
          filelib:is_file(get_otp_versions_table_path()),
          first_available_otp_rel() =/= none} of
        {true, true, true, true, true} ->
            test_runtime_dependencies_versions_rels(IgnoreApps,
                                                    AppsToIgnoredUndefinedFunctions);
        {_, _ ,_, false, _} -> {skip,
                             "Could not find the file \"otp_versions.table\". "
                             "Check that the test has been built correctly. "
                             "\"otp_versions.table\" is copied to \"erts/test/otp_SUITE_data\" "
                             "by the makefile \"erts/test/Makefile\""};
        {_, false , _, _, _} -> {skip,
                              "This test case is designed to run in the Erlang/OTP teams "
                              "test system for nightly tests. The test case depend on that "
                              "app versions have been set correctly by scripts that "
                              "are executed before creating builds for the nightly tests."};
        {_, _ ,false, _, _} -> {skip, "Can not do the tests without a proper releases dir. "
                             "Check that " ++ ReleasesDir ++ " is set up correctly."};
        {_, _ , _, _, false} ->
            PrevRelNr = erlang:list_to_integer(erlang:system_info(otp_release)) - 1,
            PrevRelNrStr = erlang:integer_to_list(PrevRelNr),
            {skip,
             "Seems like the releases dir is not set up correctly. "
             "Is release " ++ PrevRelNrStr ++ " installed in the releases dir? "
             "(releases dir = " ++ ReleasesDir ++ ")"};
        {false, _ ,_, _, _} -> {skip, "This test only runs on Unix systems"}
    end.
