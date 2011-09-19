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
	 modules/1,
	 exports/1,
	 applications/1,
	 undefined_calls/1, undefined_calls/0]).

-define(APP, diameter).
-define(A, list_to_atom).

%% ===========================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [keys,
     modules,
     exports,
     applications,
     undefined_calls].

init_per_suite(Config) ->
    {ok, App} = diameter_util:appfile(?APP),
    [{app, App} | Config].

end_per_suite(_Config) ->
    ok.

%% ===========================================================================

%% keys/1
%%
%% Ensure that the app file contain required keys.

keys(Config) ->
    App = fetch(app, Config),
    [] = lists:filter(fun(K) -> not lists:keymember(K, 1, App) end,
                      [vsn, description, modules, registered, applications]).

%% modules/1
%%
%% Ensure that the app file module list match the installed beams.

modules(Config) ->
    Mods = fetch(modules, fetch(app, Config)),
    Installed = installed_mods(),
    {[], []} = {Mods -- Installed, Installed -- Mods}.

installed_mods() ->
    Dir  = code:lib_dir(?APP, ebin),
    {ok, Files} = file:list_dir(Dir),
    [?A(lists:reverse(R)) || N <- Files, "maeb." ++ R <- [lists:reverse(N)]].

%% exports/1
%%
%% Ensure that no module does export_all.

exports(Config) ->
    Mods = fetch(modules, fetch(app, Config)),
    [] = [M || M <- Mods, exports_all(M)].

exports_all(Mod) ->
    Opts = fetch(options, Mod:module_info(compile)),

    is_list(Opts) andalso lists:member(export_all, Opts).

%% applications/1
%%
%% Ensure that each dependent application is on the code path.

applications(Config) ->
    As = fetch(applications, fetch(app, Config)),
    [] = [A || A <- As, {error, _} <- [diameter_util:appfile(A)]].

%% undefined_calls/1
%%
%% Ensure that no function on our application calls an undefined function.

undefined_calls() ->
    [{timetrap, {minutes, 2}}].

undefined_calls(Config) ->
    Mods = fetch(modules, fetch(app, Config)),

    RootDir = code:root_dir(),
    EbinDir = code:lib_dir(?APP, ebin),

    {ok, XRef} = xref:start(make_name(xref_test_name)),
    ok = xref:set_default(XRef, [{verbose, false}, {warnings, false}]),

    XRefName = make_name(xref_name),
    {ok, XRefName} = xref:add_release(XRef, RootDir, {name, XRefName}),
    {ok, _} = xref:replace_application(XRef, ?APP, EbinDir),

    {ok, Undefs} = xref:analyze(XRef, undefined_function_calls),

    xref:stop(XRef),

    [] = lists:filter(fun({{M,_,_},_}) -> lists:member(M, Mods) end, Undefs).

make_name(Suf) ->
    list_to_atom(atom_to_list(?APP) ++ "_" ++ atom_to_list(Suf)).

%% ===========================================================================

fetch(Key, List) ->
    {Key, Val} = lists:keyfind(Key, 1, List),
    Val.
