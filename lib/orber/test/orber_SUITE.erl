%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
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
-module(orber_SUITE).
-include("test_server.hrl").


-define(default_timeout, ?t:minutes(15)).
-define(application, orber).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

% Test cases must be exported.
-export([app_test/1, undefined_functions/1, install_load_order/1,
	 install_local_content/1]).

%%
%% all/1
%%
all(doc) ->
    [];
all(suite) ->
    [app_test, undefined_functions, 
     install_load_order, install_local_content].

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%
% Test cases starts here.
%
app_test(doc) -> [];
app_test(suite) -> [];
app_test(_Config) ->
    ?line ok=?t:app_test(orber),
    ok.

%% Install Orber using the load_order option.
install_load_order(suite) ->
    [];
install_load_order(doc) ->
    [];
install_load_order(_Config) ->
    orber:jump_stop(),
    case catch install_load_order2() of
	ok ->
	    orber:jump_stop();
	What ->
	    orber:jump_stop(),
	    exit(What)
    end.

install_load_order2() ->
    application:load(orber),
    mnesia:start(),
    corba:orb_init([{iiop_port, 0}]),
    orber:install([node()], [{ifr_storage_type, ram_copies},
			     {load_order, 10}]),
    orber:start(),
    [H|_] = orber:get_tables(),
    10 = mnesia:table_info(H, load_order),
    ok.

%% Install Orber using the local_content option.
install_local_content(suite) ->
    [];
install_local_content(doc) ->
    [];
install_local_content(_Config) ->
    orber:jump_stop(),
    case catch install_local_content2() of
	ok ->
	    orber:jump_stop();
	What ->
	    orber:jump_stop(),
	    exit(What)
    end.

install_local_content2() ->
    application:load(orber),
    mnesia:start(),
    corba:orb_init([{iiop_port, 0}]),
    orber:install([node()], [{ifr_storage_type, ram_copies},
			     {local_content, true}]),
    orber:start(),
    [H|_] = orber:get_tables(),
    true = mnesia:table_info(H, local_content),
    ok.



%% Check for undefined functions
undefined_functions(suite) ->
    [];
undefined_functions(doc) ->
    [];
undefined_functions(_Config) ->
    App            = orber,
    Root           = code:root_dir(),
    LibDir         = code:lib_dir(App),
    EbinDir        = filename:join([LibDir,"ebin"]),
    AppFilePath    = filename:join([LibDir,"ebin", "orber.app"]),
    {ok, [{application,orber,AppFile}]} = file:consult(AppFilePath),
    io:format("Using ~p~n~p~n", [AppFilePath, AppFile]),
    Mods           = key1search(modules, AppFile),
    XRefTestName   = undef_funcs_make_name(App, xref_test_name),
    {ok, XRef}     = xref:start(XRefTestName),
    ok             = xref:set_default(XRef,
                                      [{verbose,false},{warnings,false}]),
    XRefName       = undef_funcs_make_name(App, xref_name),
    {ok, XRefName} = xref:add_release(XRef, Root, {name,XRefName}),
    {ok, App}      = xref:replace_application(XRef, App, EbinDir),
    {ok, Undefs}   = xref:analyze(XRef, undefined_function_calls),
    xref:stop(XRef),
    analyze_undefined_function_calls(Undefs, Mods, []).
 
analyze_undefined_function_calls([], _, []) ->
    ok;
analyze_undefined_function_calls([], _, AppUndefs) ->
    exit({suite_failed, {undefined_function_calls, AppUndefs}});
analyze_undefined_function_calls([{{Mod, _F, _A}, _C} = AppUndef|Undefs],
                                 AppModules, AppUndefs) ->
    %% Check that this module is our's
    case lists:member(Mod,AppModules) of
        true ->
            {Calling,Called} = AppUndef,
            {Mod1,Func1,Ar1} = Calling,
            {Mod2,Func2,Ar2} = Called,
            io:format("undefined function call: "
                      "~n   ~w:~w/~w calls ~w:~w/~w~n",
                      [Mod1,Func1,Ar1,Mod2,Func2,Ar2]),
            analyze_undefined_function_calls(Undefs, AppModules,
                                             [AppUndef|AppUndefs]);
        false ->
            io:format("dropping ~p~n", [Mod]),
            analyze_undefined_function_calls(Undefs, AppModules, AppUndefs)
    end.
 
%% This function is used simply to avoid cut-and-paste errors later...
undef_funcs_make_name(App, PostFix) ->
    list_to_atom(atom_to_list(App) ++ "_" ++ atom_to_list(PostFix)).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
        false ->
            fail({not_found, Key, L});
        {value, {Key, Value}} ->
            Value
    end.

fail(Reason) ->
    exit({suite_failed, Reason}).



