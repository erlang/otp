%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
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

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

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
%% compiler/help modules.
%% ===========================================================================

modules(Config) ->
    Mods = fetch(modules, fetch(app, Config)),
    Installed = code_mods(),
    Help = [diameter_callback,
            diameter_codegen,
            diameter_dbg,
            diameter_exprecs,
            diameter_info,
            diameter_make,
            diameter_spec_scan,
            diameter_spec_util],
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
%% or one in an application we haven't specified as a dependency. (Almost.)
%% ===========================================================================

xref(Config) ->
    App = fetch(app, Config),
    Mods = fetch(modules, App),

    {ok, XRef} = xref:start(make_name(xref_test_name)),
    ok = xref:set_default(XRef, [{verbose, false}, {warnings, false}]),

    %% Only add our application and those it's dependent on according
    %% to the app file. Well, almost. erts beams are also required to
    %% stop xref from complaining about calls to module erlang, which
    %% was previously in kernel. Erts isn't an application however, in
    %% the sense that there's no .app file, and isn't listed in
    %% applications. Seems less than ideal. Also, diameter_tcp does
    %% call ssl despite ssl not being listed as a dependency in the
    %% app file since ssl is only required for TLS security: it's up
    %% to a client who wants TLS it to start ssl.
    ok = lists:foreach(fun(A) -> add_application(XRef, A) end,
                       [?APP, erts | fetch(applications, App)]),

    {ok, Undefs} = xref:analyze(XRef, undefined_function_calls),

    xref:stop(XRef),

    %% Only care about calls from our own application.
    [] = lists:filter(fun({{F,_,_},{T,_,_}}) ->
                              lists:member(F, Mods)
                                  andalso {F,T} /= {diameter_tcp, ssl}
                      end,
                      Undefs).

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
    true = is_vsn(Vsn),
    Name = "diameter_test_" ++ Vsn,
    ok = write_file(filename:join([Dir, Name ++ ".rel"]),
                    {release,
                     {"diameter " ++ Vsn ++ " test release", Vsn},
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

%% Is a version string of the expected form? Return the argument
%% itself for 'false' for a useful badmatch.
is_vsn(V) ->
    is_list(V)
        andalso length(V) == string:span(V, "0123456789.")
        andalso V == string:join(string:tokens(V, [$.]), ".")  %% no ".."
        orelse {error, V}.
