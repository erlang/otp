%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the inets application
%%----------------------------------------------------------------------
-module(inets_app_test).

-compile(export_all).

-include("inets_test_lib.hrl").


% t()     -> megaco_test_lib:t(?MODULE).
% t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(undef_funcs, Config) ->
    NewConfig = lists:keydelete(watchdog, 1, Config),
    Dog = test_server:timetrap(inets_test_lib:minutes(10)),

    %% We need to check if there is a point to run this test.
    %% On some platforms, crypto will not build, which in turn
    %% causes ssl to not build (at this time, this will
    %% change in the future).
    %% So, we first check if we can start crypto, and if not,
    %% we skip this test case!
    ?ENSURE_STARTED(crypto),

    [{watchdog, Dog}| NewConfig];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [fields, modules, exportall, app_depend, undef_funcs].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    case is_app(inets) of
	{ok, AppFile} ->
	    io:format("AppFile: ~n~p~n", [AppFile]),
            inets:print_version_info(),
	    [{app_file, AppFile}|Config];
	{error, Reason} ->
	    fail(Reason)
    end.

is_app(App) ->
    LibDir = code:lib_dir(App),
    File = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
	{ok, [{application, App, AppFile}]} ->
	    {ok, AppFile};
	Error ->
	    {error, {invalid_format, Error}}
    end.


end_per_suite(suite) -> [];
end_per_suite(doc) -> [];
end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fields(suite) ->
    [];
fields(doc) ->
    [];
fields(Config) when is_list(Config) ->
    AppFile = key1search(app_file, Config),
    Fields = [vsn, description, modules, registered, applications],
    case check_fields(Fields, AppFile, []) of
	[] ->
	    ok;
	Missing ->
	    fail({missing_fields, Missing})
    end.

check_fields([], _AppFile, Missing) ->
    Missing;
check_fields([Field|Fields], AppFile, Missing) ->
    check_fields(Fields, AppFile, check_field(Field, AppFile, Missing)).

check_field(Name, AppFile, Missing) ->
    io:format("checking field: ~p~n", [Name]),
    case lists:keymember(Name, 1, AppFile) of
	true ->
	    Missing;
	false ->
	    [Name|Missing]
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

modules(suite) ->
    [];
modules(doc) ->
    [];
modules(Config) when is_list(Config) ->
    AppFile  = key1search(app_file, Config),
    Mods     = key1search(modules, AppFile),
    EbinList = get_ebin_mods(inets),
    case missing_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Missing ->
	    throw({error, {missing_modules, Missing}})
    end,
    case extra_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Extra ->
	    throw({error, {extra_modules, Extra}})
    end,
    {ok, Mods}.

get_ebin_mods(App) ->
    LibDir  = code:lib_dir(App),
    EbinDir = filename:join([LibDir,"ebin"]),
    {ok, Files0} = file:list_dir(EbinDir),
    Files1 = [lists:reverse(File) || File <- Files0],
    [list_to_atom(lists:reverse(Name)) || [$m,$a,$e,$b,$.|Name] <- Files1].


missing_modules([], _Ebins, Missing) ->
    Missing;
missing_modules([Mod|Mods], Ebins, Missing) ->
    case lists:member(Mod, Ebins) of
	true ->
	    missing_modules(Mods, Ebins, Missing);
	false ->
	    io:format("missing module: ~p~n", [Mod]),
	    missing_modules(Mods, Ebins, [Mod|Missing])
    end.


extra_modules(_Mods, [], Extra) ->
    Extra;
extra_modules(Mods, [Mod|Ebins], Extra) ->
    case lists:member(Mod, Mods) of
	true ->
	    extra_modules(Mods, Ebins, Extra);
	false ->
	    io:format("supefluous module: ~p~n", [Mod]),
	    extra_modules(Mods, Ebins, [Mod|Extra])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exportall(suite) ->
    [];
exportall(doc) ->
    [];
exportall(Config) when is_list(Config) ->
    AppFile = key1search(app_file, Config),
    Mods    = key1search(modules, AppFile),
    check_export_all(Mods).


check_export_all([]) ->
    ok;
check_export_all([Mod|Mods]) ->
    case (catch apply(Mod, module_info, [compile])) of
	{'EXIT', {undef, _}} ->
	    check_export_all(Mods);
	O ->
            case lists:keysearch(options, 1, O) of
                false ->
                    check_export_all(Mods);
                {value, {options, List}} ->
                    case lists:member(export_all, List) of
                        true ->
			    throw({error, {export_all, Mod}});
			false ->
			    check_export_all(Mods)
                    end
            end
    end.

	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app_depend(suite) ->
    [];
app_depend(doc) ->
    [];
app_depend(Config) when is_list(Config) ->
    AppFile = key1search(app_file, Config),
    Apps    = key1search(applications, AppFile),
    check_apps(Apps).


check_apps([]) ->
    ok;
check_apps([App|Apps]) ->
    case is_app(App) of
	{ok, _} ->
	    check_apps(Apps);
	Error ->
	    throw({error, {missing_app, {App, Error}}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

undef_funcs(suite) ->
    [];
undef_funcs(doc) ->
    [];
undef_funcs(Config) when is_list(Config) ->
    App            = inets,
    AppFile        = key1search(app_file, Config),
    Mods           = key1search(modules, AppFile),
    Root           = code:root_dir(),
    LibDir         = code:lib_dir(App),
    EbinDir        = filename:join([LibDir,"ebin"]),
    XRefTestName   = undef_funcs_make_name(App, xref_test_name),
    {ok, XRef}     = xref:start(XRefTestName),
    ok             = xref:set_default(XRef, 
				      [{verbose,false},{warnings,false}]),
    XRefName       = undef_funcs_make_name(App, xref_name),
    {ok, XRefName} = xref:add_release(XRef, Root, {name, XRefName}),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


fail(Reason) ->
    exit({suite_failed, Reason}).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L});
	{value, {Key, Value}} ->
	    Value
    end.
