%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2024. All Rights Reserved.
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
    Server = start_xref_server(daily_xref, functions, Config),
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

    Filters = [fun erts_filter/1,
               fun ssl_crypto_filter/1,
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
            filter(fun({_,{i, _, _}}) -> false;
                      ({_,{dbg_iserver, _, _}}) -> false;
                      ({_,{et_selector, _, _}}) -> false;
                      ({_,{et_viewer, _, _}}) -> false;
                      ({_,{int, _, _}}) -> false;
                      ({_,{MaybeWxModule,_,_}}) ->
                           case atom_to_list(MaybeWxModule) of
                               "wx"++_ -> false;
                               _ -> true
                           end
                   end, Undef);
        _ -> Undef
    end.

diameter_filter(Undef) ->
    %% Filter away function calls that are caught.
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

erts_filter(Undef) ->
    filter(fun({_,{prim_socket,_,_}}) -> lists:member(prim_socket, erlang:pre_loaded());
              ({_,{prim_net,_,_}}) -> lists:member(prim_net, erlang:pre_loaded());
              ({_,{socket_registry,_,_}}) -> lists:member(socket_registry, erlang:pre_loaded());
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
    %% Forbid the use of erlang:size/1 in all applications.
    Apps = all_otp_applications(Config),
    not_recommended_calls(Config, Apps, {erlang,size,1}).

call_to_now_0(Config) when is_list(Config) ->
    %% Forbid the use of erlang:now/1 in all applications except et.
    Apps = all_otp_applications(Config) -- [et],
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
            ct:fail({length(CallsToMFA),calls_to,MFA})
    end.

all_otp_applications(Config) ->
    Server = proplists:get_value(xref_server, Config),
    {ok,AllApplications} = xref:q(Server, "A"),
    OtpAppsMap = get_otp_applications(Config),
    [App || App <- AllApplications, is_map_key(App, OtpAppsMap)].

get_otp_applications(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    [Current|_] = read_otp_version_table(DataDir),
    read_version_lines([Current]).

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

runtime_dependencies_modules(Config) ->
    Server = start_xref_server(?FUNCTION_NAME, modules, Config),
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

%%
%% Test that the runtime dependencies have not become stale.
%%

%% Path of installed OTP releases.
-define(OTP_RELEASES, "/usr/local/otp/releases").

%% Wildcard to match all releases from OTP 17.
-define(OTP_RELEASE_WC, "{sles10_64_17_patched,ubuntu[1-9][0-9]_64_[1-9][0-9]_patched}").

test_runtime_dependencies_versions(Config) ->
    DataDir = proplists:get_value(data_dir, Config),

    OtpReleases = ?OTP_RELEASES,
    OtpReleasesWc = filename:join(OtpReleases, ?OTP_RELEASE_WC),

    IgnoreApps = [],
    IgnoreUndefs = ignore_undefs(),

    FirstVersionForApp = get_first_app_versions(DataDir),

    case {element(1, os:type()) =:= unix,
          not is_development_build(DataDir),
          filelib:is_dir(OtpReleases)} of
        {true, true, true} ->
            test_runtime_dependencies_versions_rels(
              IgnoreApps,
              IgnoreUndefs,
              FirstVersionForApp,
              OtpReleasesWc);
        {false, _, _} ->
            {skip, "This test only runs on Unix systems"};
        {_, false, _} ->
            {skip,
             "This test case is designed to run in the Erlang/OTP teams "
             "test system for daily tests. The test case depends on that "
             "app versions have been set correctly by scripts that "
             "are executed before creating builds for the daily tests."};
        {_, _ ,false} ->
            {skip, "Can not do the tests without a proper releases dir. "
             "Check that " ++ OtpReleases ++ " is set up correctly."}
    end.

ignore_undefs() ->
    Socket = case lists:member(prim_socket, erlang:pre_loaded()) of
                 true ->
                     #{};
                 false ->
                     Ignore = #{{prim_socket,'_','_'} => true,
                                {socket_registry,'_','_'} => true,
                                {prim_net,'_','_'} => true },
                     #{kernel => Ignore, erts => Ignore}
             end,
    Socket#{eunit =>
                %% Intentional call to nonexisting function
                #{{eunit_test, nonexisting_function, 0} => true},
            diameter =>
                %% The following functions are optional dependencies for diameter
                #{{dbg,ctp,0} => true,
                  {dbg,p,2} => true,
                  {dbg,stop,0} => true,
                  {dbg,trace_port,2} => true,
                  {dbg,tracer,2} => true,
                  {erl_prettypr,format,1} => true,
                  {erl_syntax,form_list,1} => true},
            common_test =>
                %% ftp:start/0 has been part of the ftp application from
                %% the beginning so it is unclear why xref report this
                %% as undefined
                #{{ftp,start,0} => true}}.

%% Read the otp_versions.table file and create a mapping from application name
%% the first version for each application.
get_first_app_versions(DataDir) ->
    Lines = read_otp_version_table(DataDir),
    read_version_lines(Lines).

test_runtime_dependencies_versions_rels(IgnoreApps, IgnoreUndefs,
                                        FirstVersionForApp, OtpReleasesWc) ->
    AppVersionToPath = version_to_path(OtpReleasesWc),
    Deps = [Dep || {App,_,_}=Dep <- get_deps(FirstVersionForApp),
                   not lists:member(App, IgnoreApps)],
    case test_deps(Deps, IgnoreUndefs, AppVersionToPath, FirstVersionForApp) of
        [] ->
            ok;
        [_|_]=Undefs ->
            _ = [print_undefs(Undef) || Undef <- Undefs],
            ct:fail({length(Undefs),errors})
    end.

print_undefs({_App,AppPath,Deps,Undefs}) ->
    App = filename:basename(filename:dirname(AppPath)),
    io:format("Undefined functions in ~ts:", [App]),
    io:put_chars([io_lib:format("  ~p:~p/~p\n", [M,F,A]) ||
                     {M,F,A} <- Undefs]),
    io:format("Dependencies: ~ts\n", [lists:join(" ", Deps)]).

version_to_path(OtpReleasesWc) ->
    CurrentWc = filename:join([code:lib_dir(), "*-*", "ebin"]),
    LibWc = filename:join([OtpReleasesWc, "lib", "*-*", "ebin"]),
    Dirs = lists:sort(filelib:wildcard(CurrentWc) ++ filelib:wildcard(LibWc)),
    All = [{filename:basename(filename:dirname(Dir)),Dir} || Dir <- Dirs],
    maps:from_list(All).

get_deps(FirstVersionForApp) ->
    Paths = [begin
                 Dir = filename:dirname(Path),
                 [App0 | _] = string:split(filename:basename(Dir), "-"),
                 App = list_to_atom(App0),
                 AppFile = filename:join(Path, App0 ++ ".app"),
                 {Path, App, AppFile}
             end || Path <- code:get_path(),
                    filename:basename(Path) =:= "ebin"],
    %% Only keep applications included in OTP.
    Apps = [Triple || {_, App, AppFile}=Triple <- Paths,
                      filelib:is_file(AppFile),
                      is_map_key(App, FirstVersionForApp)],
    [{App, Path, get_runtime_deps(App, AppFile)} || {Path, App, AppFile} <- Apps].

get_runtime_deps(App, AppFile) ->
    {ok,[{application, App, Info}]} = file:consult(AppFile),
    case lists:keyfind(runtime_dependencies, 1, Info) of
        {runtime_dependencies, RDeps} ->
            RDeps;
        false ->
            []
    end.

test_deps([{Name,Path,Deps}|Apps], IgnoreUndefs, AppVersionToPath, FirstVersionForApp) ->
    case test_dep(Name, Path, Deps, AppVersionToPath, FirstVersionForApp, IgnoreUndefs) of
        ok ->
            test_deps(Apps, IgnoreUndefs, AppVersionToPath, FirstVersionForApp);
        {error, Error} ->
            [Error|test_deps(Apps, IgnoreUndefs, AppVersionToPath, FirstVersionForApp)]
    end;
test_deps([], _IgnoreUndefs, _AppVersionToPath, _FirstVersionForApp) ->
    [].

test_dep(App, AppPath, Deps, AppVersionToPath, FirstVersionForApp, IgnoreUndefs) ->
    DepPaths = [get_app_path(Dep, AppVersionToPath, FirstVersionForApp, App) ||
                   Dep <- Deps],

    Server = xref_server_test_dep,
    {ok, _} = xref:start(Server, []),
    xref:set_default(Server, [{verbose,false},
                              {warnings,false},
                              {builtins,true}]),
    ok = xref:set_library_path(Server, DepPaths),
    {ok, _} = xref:add_directory(Server, AppPath),
    {ok, Undef0} = xref:analyze(Server, undefined_functions),
    xref:stop(Server),

    %% Filter out undefined functions that we should ignore.
    Ignore = maps:get(App, IgnoreUndefs, #{}),
    Undef = [MFA || {M,F,_A}=MFA <- Undef0,
                    not is_map_key(MFA, Ignore),
                    not is_map_key({M,'_','_'}, Ignore),
                    not is_map_key({M,F,'_'}, Ignore)],
    case Undef of
        [] ->
            ok;
        [_|_] ->
            {error, {App, AppPath, Deps, Undef}}
    end.

get_app_path(App, AppVersionToPath, FirstVersionForApp, ReferencedBy) ->
    case AppVersionToPath of
        #{App := Path} ->
            Path;
        #{} ->
            [Name0, _Version] = string:split(App, "-"),
            Name = list_to_existing_atom(Name0),
            First = map_get(Name, FirstVersionForApp),
            io:format("WARNING: ~ts referenced by ~ts is too old; using ~ts instead\n",
                      [App, ReferencedBy, First]),
            map_get(First, AppVersionToPath)
    end.

is_development_build(DataDir) ->
    OTPVersionTicketsPath = filename:join(DataDir, "otp_version_tickets"),
    {ok, FileContentBin} = file:read_file(OTPVersionTicketsPath),
    string:trim(binary_to_list(FileContentBin), both, "\n ") =:= "DEVELOPMENT".

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

start_xref_server(Server, Mode, _Config) ->
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
            ErtsEbin = lists:flatten(filename:join(LibDir, "ebin")),
            case lists:member(ErtsEbin, code:get_path()) of
                false ->
                    %% If we are running the tests in the git repository,
                    %% the preloaded BEAM files for Erts are not in the
                    %% code path. We must add them explicitly.
                    {ok,_} = xref:add_directory(Server, ErtsEbin, []);
                true ->
                    ok
            end
    end,

    Server.

read_otp_version_table(DataDir) ->
    VersionTableFile = filename:join(DataDir, "otp_versions.table"),
    {ok, Contents} = file:read_file(VersionTableFile),
    binary:split(Contents, <<"\n">>, [global,trim]).

read_version_lines(Lines) ->
    read_version_lines(Lines, #{}).

read_version_lines([Line|Lines], Map0) ->
    [<<"OTP-",_/binary>>, <<":">> | Apps] = binary:split(Line, <<" ">>, [global,trim]),
    Map = lists:foldl(fun(App, Acc) ->
                              case binary:split(App, <<"-">>) of
                                  [Name, _Version] ->
                                      Acc#{binary_to_atom(Name) => binary_to_list(App)};
                                  [_] ->
                                      Acc
                              end
                      end, Map0, Apps),
    read_version_lines(Lines, Map);
read_version_lines([], Map) ->
    Map.
