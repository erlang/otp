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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the Diameter application
%%----------------------------------------------------------------------
-module(diameter_app_test).

-export([
	 init_per_testcase/2, fin_per_testcase/2, 

	 all/0,
	 groups/0, 
	 init_per_suite/1, end_per_suite/1, 
	 suite_init/1, suite_fin/1, 
	 init_per_group/2, end_per_group/2, 

	 fields/1, 
	 modules/1,
	 exportall/1,
	 app_depend/1,
	 undef_funcs/1
	]).

-export([t/0, t/1]).

-include("diameter_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t()     -> diameter_test_server:t(?MODULE).
t(Case) -> diameter_test_server:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(undef_funcs = Case, Config) ->
    NewConfig = [{tc_timeout, ?MINUTES(10)} | Config], 
    diameter_test_server:init_per_testcase(Case, NewConfig);
init_per_testcase(Case, Config) ->
    diameter_test_server:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    diameter_test_server:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     fields,
     modules,
     exportall,
     app_depend,
     undef_funcs
    ].

groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

suite_init(X) -> init_per_suite(X).
    
init_per_suite(suite) -> [];
init_per_suite(doc) -> [];
init_per_suite(Config) when is_list(Config) ->
    io:format("~w:init_per_suite -> entry with"
	      "~n   Config: ~p"
	      "~n", [?MODULE, Config]),    
    case is_app(diameter) of
	{ok, AppFile} ->
	    io:format("AppFile: ~n~p~n", [AppFile]),
	    %% diameter:print_version_info(),
	    [{app_file, AppFile}|Config];
	{error, Reason} ->
	    ?FAIL(Reason)
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


suite_fin(X) -> end_per_suite(X).

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
    AppFile = ?KEY1SEARCH(app_file, Config),
    Fields = [vsn, description, modules, registered, applications],
    case check_fields(Fields, AppFile, []) of
	[] ->
	    ok;
	Missing ->
	    ?FAIL({missing_fields, Missing})
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
    AppFile  = ?KEY1SEARCH(app_file, Config),
    Mods     = ?KEY1SEARCH(modules, AppFile),
    EbinList = get_ebin_mods(diameter),
    case missing_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Missing ->
	    throw({error, {missing_modules, Missing}})
    end,
    Allowed = [diameter_codegen,
	       diameter_make,
	       diameter_spec_scan,
	       diameter_spec_util],    
    case extra_modules(Mods, EbinList, Allowed, []) of
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


extra_modules(_Mods, [], Allowed, Extra) ->
    Extra--Allowed;
extra_modules(Mods, [Mod|Ebins], Allowed, Extra) ->
    case lists:member(Mod, Mods) of
	true ->
	    extra_modules(Mods, Ebins, Allowed, Extra);
	false ->
	    io:format("supefluous module: ~p~n", [Mod]),
	    extra_modules(Mods, Ebins, Allowed, [Mod|Extra])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


exportall(suite) ->
    [];
exportall(doc) ->
    [];
exportall(Config) when is_list(Config) ->
    AppFile = ?KEY1SEARCH(app_file, Config),
    Mods    = ?KEY1SEARCH(modules, AppFile),
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
    AppFile = ?KEY1SEARCH(app_file, Config),
    Apps    = ?KEY1SEARCH(applications, AppFile),
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
    ?SKIP(diameter_not_known_by_xref),
    App            = diameter,
    AppFile        = ?KEY1SEARCH(app_file, Config),
    Mods           = ?KEY1SEARCH(modules, AppFile),
    Root           = code:root_dir(),
    LibDir         = code:lib_dir(App),
    EbinDir        = filename:join([LibDir,"ebin"]),
    XRefTestName   = undef_funcs_make_name(App, xref_test_name),
    try
	begin
	    XRef     = xref_start(XRefTestName),
	    xref_set_defaults(XRef, [{verbose,false},{warnings,false}]),
	    XRefName = undef_funcs_make_name(App, xref_name),
	    XRefName = xref_add_release(XRef, Root, XRefName), 
	    xref_replace_application(XRef, App, EbinDir),
	    Undefs   = xref_analyze(XRef),
	    xref_stop(XRef),
	    analyze_undefined_function_calls(Undefs, Mods, [])
	end
    catch
	throw:{error, Reason} ->
	    ?FAIL(Reason)
    end.


xref_start(XRefTestName) ->
    case (catch xref:start(XRefTestName)) of
	{ok, XRef} ->
	    XRef;
	{error, Reason} ->
	    throw({error, {failed_starting_xref, Reason}});
	Error ->
	    throw({error, {failed_starting_xref, Error}})
    end.

xref_set_defaults(XRef, Defs) ->
    case (catch xref:set_default(XRef, Defs)) of
	ok ->
	    ok;
	Error ->
	    throw({error, {failed_setting_defaults, Defs, Error}})
    end.

xref_add_release(XRef, Root, Name) ->
    case (catch xref:add_release(XRef, Root, {name, Name})) of
	{ok, XRefName} ->
	    XRefName;
	{error, Reason} ->
	    throw({error, {failed_adding_release, Reason}});
	Error ->
	    throw({error, {failed_adding_release, Error}})
    end.

xref_replace_application(XRef, App, EbinDir) ->
    case (catch xref:replace_application(XRef, App, EbinDir)) of
	{ok, App} ->
	    ok;
	{error, XRefMod, Reason} ->
	    throw({error, {failed_replacing_app, XRefMod, Reason}});
	Error ->
	    throw({error, {failed_replacing_app, Error}})
    end.

xref_analyze(XRef) ->
    case (catch xref:analyze(XRef, undefined_function_calls)) of
	{ok, Undefs} ->
	    Undefs;
	{error, Reason} ->
	    throw({error, {failed_detecting_func_calls, Reason}});
	Error ->
	    throw({error, {failed_detecting_func_calls, Error}})
    end.

xref_stop(XRef) ->
    xref:stop(XRef).

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


%% fail(Reason) ->
%%     exit({suite_failed, Reason}).

%% ?KEY1SEARCH(Key, L) ->
%%     case lists:keysearch(Key, 1, L) of
%% 	undefined ->
%% 	    fail({not_found, Key, L});
%% 	{value, {Key, Value}} ->
%% 	    Value
%%     end.
