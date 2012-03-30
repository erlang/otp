%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2003-2012. All Rights Reserved.
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

%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the snmp application
%%----------------------------------------------------------------------
-module(snmp_app_test).

-export([
	 all/0, groups/0, 
	 init_per_group/2, end_per_group/2, 
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2, 

	 fields/1,
	 modules/1,
	 exportall/1,
	 app_depend/1,
	 undef_funcs/1,

	
	 start_and_stop_empty/1, 
	 start_and_stop_with_agent/1, 
	 start_and_stop_with_manager/1,
	 start_and_stop_with_agent_and_manager/1,
	 start_epmty_and_then_agent_and_manager_and_stop/1,
	 start_with_agent_and_then_manager_and_stop/1,
	 start_with_manager_and_then_agent_and_stop/1
	]).


-include_lib("kernel/include/file.hrl").
-include_lib("test_server/include/test_server.hrl").
-include("snmp_test_lib.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    Cases = 
	[
	 fields, 
	 modules, 
	 exportall, 
	 app_depend,
	 undef_funcs, 
	 {group, start_and_stop}
	],
    Cases.

groups() -> 
    [{start_and_stop, [],
      [start_and_stop_empty, 
       start_and_stop_with_agent,
       start_and_stop_with_manager,
       start_and_stop_with_agent_and_manager,
       start_epmty_and_then_agent_and_manager_and_stop,
       start_with_agent_and_then_manager_and_stop,
       start_with_manager_and_then_agent_and_stop]}].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


init_per_suite(Config) when is_list(Config) ->
    ?DISPLAY_SUITE_INFO(), 

    %% Note that part of this stuff (the suite top dir creation) 
    %% may already be done (if we run the entire snmp suite).

    PrivDir = ?config(priv_dir, Config),
    TopDir = filename:join(PrivDir, app),
    case file:make_dir(TopDir) of
        ok ->
            ok;
        {error, eexist} ->
            ok;
        Error ->
            fail({failed_creating_subsuite_top_dir, Error})
    end,
    AppFile = 
	case is_app() of
	    {ok, File} ->
		io:format("File: ~n~p~n", [File]),
		snmp:print_version_info(), 
		File;
	{error, Reason} ->
	    fail(Reason)
    end,
    [{app_topdir, TopDir}, {app_file, AppFile} | Config].


is_app() ->
    is_app(?APPLICATION).

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

%% Test server callbacks
init_per_testcase(undef_funcs, Config) ->
    Config2 = lists:keydelete(watchdog, 1, Config),
    [{watchdog, ?WD_START(?MINS(10))} | Config2];
init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, Config) ->
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
    EbinList = get_ebin_mods(snmp),
    case missing_modules(Mods, EbinList, []) of
	[] ->
	    ok;
	Missing ->
	    fail({missing_modules, Missing})
    end,
    Allowed = [snmpc,
	       snmpc_lib,
	       snmpc_misc,
	       snmpc_mib_gram,
	       snmpc_mib_to_hrl,
	       snmpc_tok],
    case extra_modules(Mods, EbinList, Allowed, []) of
	[] ->
	    ok;
	Extra ->
	    fail({extra_modules, Extra})
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
	    io:format("superfluous module: ~p~n", [Mod]),
	    extra_modules(Mods, Ebins, Allowed, [Mod|Extra])
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
			    fail({export_all, Mod});
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
    App            = snmp,
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
    {ok, XRefName} = xref:add_release(XRef, Root, {name,XRefName}),
    {ok, App}      = xref:replace_application(XRef, App, EbinDir),
    {ok, Undefs}   = xref:analyze(XRef, undefined_function_calls),
    xref:stop(XRef),
    analyze_undefined_function_calls(Undefs, Mods, []).

valid_undef(crypto = CalledMod) ->
    case (catch CalledMod:version()) of
        Version when is_list(Version) ->
	    %% The called module was crypto and the version 
	    %% function returns a valid value. 
	    %% This means that the function is
	    %% actually undefined...
	    true;
	_ ->
	    %% The called module was crypto but the version 
	    %% function does *not* return a valid value.
	    %% This means the crypto was not actually not
	    %% build, which is an case snmp handles.
	    false
    end;
valid_undef(_) ->
    true.

    
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
	    %% If the called module is crypto, then we will *not*
	    %% fail if crypto is not built (since crypto is actually 
	    %% not built for all platforms)
	    case valid_undef(Mod2) of
		true ->
		    io:format("undefined function call: "
			      "~n   ~w:~w/~w calls ~w:~w/~w~n",
			      [Mod1,Func1,Ar1,Mod2,Func2,Ar2]),
		    analyze_undefined_function_calls(
		      Undefs, AppModules, [AppUndef|AppUndefs]);
		false ->
		    io:format("skipping ~p (calling ~w:~w/~w)~n", 
			      [Mod, Mod2, Func2, Ar2]),
		    analyze_undefined_function_calls(Undefs, 
						     AppModules, AppUndefs)
	    end;
        false ->
	    io:format("dropping ~p~n", [Mod]),
	    analyze_undefined_function_calls(Undefs, AppModules, AppUndefs)
    end.

%% This function is used simply to avoid cut-and-paste errors later...
undef_funcs_make_name(App, PostFix) ->
    list_to_atom(atom_to_list(App) ++ "_" ++ atom_to_list(PostFix)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop_empty(suite) ->
    [];
start_and_stop_empty(doc) ->
    ["Start and stop the application empty (no configured components)"];
start_and_stop_empty(Config) when is_list(Config) ->
    ?line false = ?IS_SNMP_RUNNING(),

    ?line ok    = snmp:start(),

    ?line true  = ?IS_SNMP_RUNNING(),

    ?line ok    = snmp:stop(),
    
    ?line false = ?IS_SNMP_RUNNING(),

    ok.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop_with_agent(suite) ->
    [];
start_and_stop_with_agent(doc) ->
    ["Start and stop the application with the agent pre-configured"];
start_and_stop_with_agent(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop_with_manager(suite) ->
    [];
start_and_stop_with_manager(doc) ->
    ["Start and stop the application with the manager pre-configured"];
start_and_stop_with_manager(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_and_stop_with_agent_and_manager(suite) ->
    [];
start_and_stop_with_agent_and_manager(doc) ->
    ["Start and stop the application with both the agent "
     "and the manager pre-configured"];
start_and_stop_with_agent_and_manager(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_epmty_and_then_agent_and_manager_and_stop(suite) ->
    [];
start_epmty_and_then_agent_and_manager_and_stop(doc) ->
    ["Start the application empty, then start the agent and then "
     "the manager and then stop the application"];
start_epmty_and_then_agent_and_manager_and_stop(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_with_agent_and_then_manager_and_stop(suite) ->
    [];
start_with_agent_and_then_manager_and_stop(doc) ->
    ["Start the application with the agent pre-configured, "
     "then start the manager and then stop the application"];
start_with_agent_and_then_manager_and_stop(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_with_manager_and_then_agent_and_stop(suite) ->
    [];
start_with_manager_and_then_agent_and_stop(doc) ->
    ["Start the application with the manager pre-configured, "
     "then start the agent and then stop the application"];
start_with_manager_and_then_agent_and_stop(Config) when is_list(Config) ->
    ?SKIP(not_implemented_yet).

    
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
