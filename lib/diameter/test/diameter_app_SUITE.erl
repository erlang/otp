%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

%%
%% Tests based on the contents of the diameter app file.
%%

-module(diameter_app_SUITE).

-export([suite/0,
         all/0,
	 init_per_suite/1,
         end_per_suite/1]).

%% testcases
-export([keys/1,
         vsn/1,
	 modules/1,
	 exports/1,
         release/1,
	 xref/1,
         relup/1]).

-include("diameter_ct.hrl").

-define(A, list_to_atom).

%% Modules not in the app and that should not have dependencies on it
%% for build reasons.
-define(COMPILER_MODULES, [diameter_codegen,
                           diameter_dict_scanner,
                           diameter_dict_parser,
                           diameter_dict_util,
                           diameter_exprecs,
                           diameter_make]).

-define(INFO_MODULES, [diameter_dbg,
                       diameter_info]).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [keys,
     vsn,
     modules,
     exports,
     release,
     xref,
     relup].

init_per_suite(Config) ->
    [{application, ?APP, App}] = diameter_util:consult(?APP, app),
    [{app, App} | Config].

end_per_suite(_Config) ->
    ok.

%% ===========================================================================
%% # keys/1
%%
%% Ensure that the app file contains selected keys. Some of these would
%% also be caught by other testcases.
%% ===========================================================================

keys(Config) ->
    App = fetch(app, Config),
    [] = lists:filter(fun(K) -> not lists:keymember(K, 1, App) end,
                      [vsn, description, modules, registered, applications]).

%% ===========================================================================
%% # vsn/1
%%
%% Ensure that our app version sticks to convention.
%% ===========================================================================

vsn(Config) ->
    true = is_vsn(fetch(vsn, fetch(app, Config))).

%% ===========================================================================
%% # modules/1
%%
%% Ensure that the app file modules and installed modules differ by
%% compiler/info modules.
%% ===========================================================================

modules(Config) ->
    Mods = fetch(modules, fetch(app, Config)),
    Installed = code_mods(),
    Help = lists:sort(?INFO_MODULES ++ ?COMPILER_MODULES),

    {[], Help} = {Mods -- Installed, lists:sort(Installed -- Mods)}.

code_mods() ->
    Dir  = code:lib_dir(?APP, ebin),
    {ok, Files} = file:list_dir(Dir),
    [?A(lists:reverse(R)) || N <- Files, "maeb." ++ R <- [lists:reverse(N)]].

%% ===========================================================================
%% # exports/1
%%
%% Ensure that no module does export_all.
%% ===========================================================================

exports(Config) ->
    Mods = fetch(modules, fetch(app, Config)),
    [] = [M || M <- Mods, exports_all(M)].

exports_all(Mod) ->
    Opts = fetch(options, Mod:module_info(compile)),

    is_list(Opts) andalso lists:member(export_all, Opts).

%% ===========================================================================
%% # release/1
%%
%% Ensure that it's possible to build a minimal release with our app file.
%% ===========================================================================

release(Config) ->
    App = fetch(app, Config),
    Rel = {release,
           {"diameter test release", fetch(vsn, App)},
           {erts, erlang:system_info(version)},
           [{A, appvsn(A)} || A <- [sasl | fetch(applications, App)]]},
    Dir = fetch(priv_dir, Config),
    ok = write_file(filename:join([Dir, "diameter_test.rel"]), Rel),
    {ok, _, []} = systools:make_script("diameter_test", [{path, [Dir]},
                                                         {outdir, Dir},
                                                         silent]).

%% sasl need to be included to avoid a missing_sasl warning, error
%% in the case of relup/1.

appvsn(Name) ->
    [{application, Name, App}] = diameter_util:consult(Name, app),
    fetch(vsn, App).

%% ===========================================================================
%% # xref/1
%%
%% Ensure that no function in our application calls an undefined function
%% or one in an application we haven't declared as a dependency. (Almost.)
%% ===========================================================================

xref(Config) ->
    App = fetch(app, Config),
    Mods = fetch(modules, App),  %% modules listed in the app file

    %% List of application names extracted from runtime_dependencies.
    Deps = lists:map(fun unversion/1, fetch(runtime_dependencies, App)),

    {ok, XRef} = xref:start(make_name(xref_test_name)),
    ok = xref:set_default(XRef, [{verbose, false}, {warnings, false}]),

    %% Only add our application and those it's dependent on according
    %% to the app file. Well, almost. erts beams are also required to
    %% stop xref from complaining about calls to module erlang, which
    %% was previously in kernel. Erts isn't an application however, in
    %% the sense that there's no .app file, and isn't listed in
    %% applications.
    ok = lists:foreach(fun(A) -> add_application(XRef, A) end,
                       [?APP, erts | fetch(applications, App)]),

    {ok, Undefs} = xref:analyze(XRef, undefined_function_calls),
    {ok, RTmods} = xref:analyze(XRef, {module_use, Mods}),
    {ok, CTmods} = xref:analyze(XRef, {module_use, ?COMPILER_MODULES}),
    {ok, RTdeps} = xref:analyze(XRef, {module_call, Mods}),

    xref:stop(XRef),

    Rel = release(),  %% otp_release-ish

    %% Only care about calls from our own application.
    [] = lists:filter(fun({{F,_,_} = From, {_,_,_} = To}) ->
                              lists:member(F, Mods)
                                  andalso not ignored(From, To, Rel)
                      end,
                      Undefs),

    %% Ensure that only runtime or info modules call runtime modules.
    %% It's not strictly necessary that diameter compiler modules not
    %% depend on other diameter modules but it's a simple source of
    %% build errors if not properly encoded in the makefile so guard
    %% against it.
    [] = (RTmods -- Mods) -- ?INFO_MODULES,

    %% Ensure that runtime modules don't call compiler modules.
    CTmods = CTmods -- Mods,

    %% Ensure that runtime modules only call other runtime modules, or
    %% applications declared in runtime_dependencies in the app file.
    %% The declared application versions are ignored since we only
    %% know what we see now.
    [] = lists:filter(fun(M) -> not lists:member(app(M), Deps) end,
                      RTdeps -- Mods).

ignored({FromMod,_,_}, {ToMod,_,_} = To, Rel)->
    %% diameter_tcp does call ssl despite the latter not being listed
    %% as a dependency in the app file since ssl is only required for
    %% TLS security: it's up to a client who wants TLS to start ssl.
    %% The OTP 18 time api is also called if it exists, so that the
    %% same code can be run on older releases.
    {FromMod, ToMod} == {diameter_tcp, ssl}
        orelse (FromMod == diameter_lib
                andalso Rel < 18
                andalso lists:member(To, time_api())).

%% New time api in OTP 18.
time_api() ->
    [{erlang, F, A} || {F,A} <- [{convert_time_unit,3},
                                 {monotonic_time,0},
                                 {monotonic_time,1},
                                 {system_time,0},
                                 {system_time,1},
                                 {time_offset,0},
                                 {time_offset,1},
                                 {timestamp,0},
                                 {unique_integer,0},
                                 {unique_integer,1}]]
        ++ [{os, system_time, 0},
            {os, system_time, 1}].

release() ->
    Rel = erlang:system_info(otp_release),
    try list_to_integer(Rel) of
        N -> N
    catch
        error:_ ->
            0  %% aka < 17
    end.

unversion(App) ->
    {Name, [$-|Vsn]} = lists:splitwith(fun(C) -> C /= $- end, App),
    true = is_app(Name), %% assert
    Vsn = vsn_str(Vsn),  %%
    Name.

app('$M_EXPR') -> %% could be anything but assume it's ok
    "erts";
app(Mod) ->
    case code:which(Mod) of
        preloaded ->
            "erts";
        Reason when is_atom(Reason) ->
            error({Reason, Mod});
        Path ->
            %% match to identify an unexpectedly short path
            {_, _, [_,_,_|_] = Split} = {Mod, Path, filename:split(Path)},
            unversion(lists:nth(3, lists:reverse(Split)))
    end.

add_application(XRef, App) ->
    add_application(XRef, App, code:lib_dir(App)).

%% erts will not be in the lib directory before installation.
add_application(XRef, erts, {error, _}) ->
    Dir = filename:join([code:root_dir(), "erts", "preloaded", "ebin"]),
    {ok, _} = xref:add_directory(XRef, Dir, []);
add_application(XRef, App, Dir)
  when is_list(Dir) ->
    {ok, App} = xref:add_application(XRef, Dir, []).

make_name(Suf) ->
    list_to_atom(atom_to_list(?APP) ++ "_" ++ atom_to_list(Suf)).

%% ===========================================================================
%% # relup/1
%%
%% Ensure that we can generate release upgrade files using our appup file.
%% ===========================================================================

relup(Config) ->
    [{Vsn, Up, Down}] = diameter_util:consult(?APP, appup),
    true = is_vsn(Vsn),

    App = fetch(app, Config),
    Rel = [{erts, erlang:system_info(version)}
           | [{A, appvsn(A)} || A <- [sasl | fetch(applications, App)]]],

    Dir = fetch(priv_dir, Config),

    Name = write_rel(Dir, Rel, Vsn),
    UpFrom = acc_rel(Dir, Rel, Up),
    DownTo = acc_rel(Dir, Rel, Down),

    {[Name], [Name], [], []}  %% no current in up/down and go both ways
        = {[Name] -- UpFrom,
           [Name] -- DownTo,
           UpFrom -- DownTo,
           DownTo -- UpFrom},

    [[], []] = [S -- sets:to_list(sets:from_list(S))
                || S <- [UpFrom, DownTo]],

    {ok, _, _, []} = systools:make_relup(Name, UpFrom, DownTo, [{path, [Dir]},
                                                                {outdir, Dir},
                                                                silent]).

acc_rel(Dir, Rel, List) ->
    lists:foldl(fun(T,A) -> acc_rel(Dir, Rel, T, A) end,
                [],
                List).

acc_rel(Dir, Rel, {Vsn, _}, Acc) ->
    [write_rel(Dir, Rel, Vsn) | Acc].

%% Write a rel file and return its name.
write_rel(Dir, [Erts | Apps], Vsn) ->
    VS = vsn_str(Vsn),
    Name = "diameter_test_" ++ VS,
    ok = write_file(filename:join([Dir, Name ++ ".rel"]),
                    {release,
                     {"diameter " ++ VS ++ " test release", VS},
                     Erts,
                     Apps}),
    Name.

%% ===========================================================================
%% ===========================================================================

fetch(Key, List) ->
    {Key, {Key, Val}} = {Key, lists:keyfind(Key, 1, List)}, %% useful badmatch
    Val.

write_file(Path, T) ->
    file:write_file(Path, io_lib:format("~p.", [T])).

%% Is a version string of the expected form?
is_vsn(V) ->
    V = vsn_str(V),
    true.

%% Turn a from/to version in appup to a version string after ensuring
%% that it's valid version number of regexp. In the regexp case, the
%% regexp itself becomes the version string since there's no
%% requirement that a version in appup be anything but a string. The
%% restrictions placed on string-valued version numbers (that they be
%% '.'-separated integers) are our own.

vsn_str(S)
  when is_list(S) ->
    {_, match}   = {S, match(S, "^(0|[1-9][0-9]*)(\\.(0|[1-9][0-9]*))*$")},
    {_, nomatch} = {S, match(S, "\\.0\\.0$")},
    S;

vsn_str(B)
  when is_binary(B) ->
    {ok, _} = re:compile(B),
    binary_to_list(B).

match(S, RE) ->
    re:run(S, RE, [{capture, none}]).

%% Is an application name of the expected form?
is_app(S)
  when is_list(S) ->
    {_, match} = {S, match(S, "^([a-z]([a-z_]*|[a-zA-Z]*))$")},
    true.
