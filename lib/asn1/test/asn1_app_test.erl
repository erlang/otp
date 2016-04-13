%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2016. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Verify the application specifics of the asn1 application
%%----------------------------------------------------------------------
-module(asn1_app_test).

-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() -> 
    [fields, modules, exportall, app_depend].

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, Config) ->
	Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_per_suite(Config) when is_list(Config) ->
    case is_app(asn1) of
	{ok, AppFile} ->
	    io:format("AppFile: ~n~p~n", [AppFile]),
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


end_per_suite(Config) when is_list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% .
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

%% .
modules(Config) when is_list(Config) ->
    AppFile  = key1search(app_file, Config),
    Mods     = key1search(modules, AppFile),
    EbinList = get_ebin_mods(asn1),
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
	    check_asn1ct_modules(Extra)
%	    throw({error, {extra_modules, Extra}})
    end,
    {ok, Mods}.
	    
get_ebin_mods(App) ->
    LibDir  = code:lib_dir(App),
    EbinDir = filename:join([LibDir,"ebin"]),
    {ok, Files0} = file:list_dir(EbinDir),
    Files1 = [lists:reverse(File) || File <- Files0],
    [list_to_atom(lists:reverse(Name)) || [$m,$a,$e,$b,$.|Name] <- Files1].

check_asn1ct_modules(Extra) ->
    ASN1CTMods = [asn1ct,asn1ct_check,asn1_db,asn1ct_pretty_format,
		  asn1ct_gen,asn1ct_gen_check,asn1ct_gen_per,
		  asn1ct_name,asn1ct_constructed_per,asn1ct_constructed_ber,
		  asn1ct_gen_ber,asn1ct_constructed_ber_bin_v2,
		  asn1ct_gen_ber_bin_v2,asn1ct_value,
		  asn1ct_tok,asn1ct_parser2,asn1ct_table,
		  asn1ct_imm,asn1ct_func,asn1ct_rtt,
		  asn1ct_eval_ext],
    case Extra -- ASN1CTMods of
	[] ->
	    ok;
	Extra2 ->
	    throw({error, {extra_modules, Extra2}})
    end.

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


%% .
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

%% .
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


fail(Reason) ->
    exit({suite_failed, Reason}).

key1search(Key, L) ->
    case lists:keysearch(Key, 1, L) of
	undefined ->
	    fail({not_found, Key, L});
	{value, {Key, Value}} ->
	    Value
    end.
