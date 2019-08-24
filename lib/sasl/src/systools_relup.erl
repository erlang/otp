%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2017. All Rights Reserved.
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
-module(systools_relup).

%%
%% GENERATING A RELUP FILE
%%
%% The purpose of this module is to produce one relup file, based on
%% one `top' .rel file, a set of `base' .rel files, and application
%% .app and .appup files. 

%% A .rel file contains a release specification that lists the name
%% and version of the release, the erts version used, and all
%% applications that are contained in the release.
%%
%% In the sequel the term `top' refers to precisely one release that
%% we upgrade to, or downgrade from.  The term `base' refers to one or
%% several releases that we upgrade from (`base' -> `top'), or
%% downgrade to (`base' <-- `top'). We should have the following
%% diagram in mind:
%%
%%
%%                         TopRel
%%
%%                      /     |         \
%%                     /      |          \
%%                    /       |           \
%%                   /        |            \     
%%                   |        |            |
%%               Base-1-Rel Base-2-Rel... Base-N-Rel   .
%%
%% .appup files for upgrade or downgrade reside only with the applications
%% in the `top' release.
%%
%% Consider now one of the Base-k-Rel releases, call it BaseRel, 
%% and let
%%
%%	TopApps = the applications in TopRel
%%      BaseApps = the applications in BaseRel,
%%
%% and define the following sets of names:
%%
%%	TopAppNames = [App.name || App <- TopApps]
%%	BaseAppNames = [App.name || App <- BaseApps] .
%%
%% We have the following disjoint sets:
%%
%% (1)	TopAppNames \ BaseAppNames		
%%
%% 	The elements in this set are the (names of) the applications
%% 	which are only in the `top' release TopRel.
%%
%% (2)	TopAppNames /\ BaseAppNames
%%
%%	The elements in this set are the (names of) the applications that
%%	exist in both releases.
%%
%% (3)  BaseAppNames \ TopAppNames
%%
%%	The elements in this set are the (names of) the applications that
%%	are only in the `base' release BaseRel.
%%
%% Upgrade (`base' --> `top')
%% ==========================
%%
%% TopAppNames \ BaseAppNames		New applications. There are no
%%					.appup files for these. Generate
%%					`add_application' instructions.
%%
%% TopAppNames /\ BaseAppNames		Same applications. For those that
%%					have different vsns, upgrade according
%%					to instructions in .appup file.
%%
%% BaseAppNames \ TopAppNames		Old applications. There are no
%%					.appup files for these. Generate 
%%					`remove_application' instructions.
%%
%% Downgrade ( `top' --> `base')
%% =============================
%%
%% BaseAppNames \ TopAppNames		New applications. There are no
%%					.appup files for these. Generate
%%					`add_application' instructions.
%%
%% TopAppNames /\ BaseAppNames		Same applications. For those that
%%					have different vsns, downgrade 
%%					according to instructions in 
%%					.appup file.
%%
%% TopAppNames \ BaseAppNames		Old applications. There are no
%%					.appup files for these. Generate 
%%					`remove_application' instructions.
%%
%%

-export([mk_relup/3, mk_relup/4, format_error/1, format_warning/1]).
-include("systools.hrl").

-define(R15_SASL_VSN,"2.2").

%% Used by release_handler:find_script/4.
%% Also used by kernel, stdlib and sasl tests
-export([appup_search_for_version/2]).

%%-----------------------------------------------------------------
%% mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs)
%% mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, Opts) -> Ret
%% 
%% TopRelFile = rel_filename()
%% TopUpRelDcs = BaseDnRelDcs = [reldescr()]
%% reldescr() = rel_filename() | {_rel_filename(), description()} 
%% rel_filename() = description() = string()
%% Opts = [opt()]
%% opt() = {path, [path()]} | silent | noexec | restart_emulator
%%       | {outdir, string()} | warnings_as_errors
%% path() = [string()]
%% Ret = ok | error | {ok, Relup, Module, Warnings} | {error, Module, Error}
%%
%% Creates a "relup" file based on information in the top 
%% .rel file and the up and down .rel files.
%%
%% The rel_filename() is stem of a .rel file, i.e. the extension
%% ".rel" is added to the stem to form the name of the real file.
%%
%% XXX WARNING: The default paths used to search for files are those
%% that are returned by code:get_path(). The default path cannot be
%% changed, only prepended through the `path' option. That may have
%% consequences that are hard to predict.
%% 
%% The option `path' sets search path, `silent' suppresses printing of
%% error messages to the console, `noexec' inhibits the creation of
%% the output "relup" file, restart_emulator ensures that the new
%% emulator is restarted (as the final step), and `warnings_as_errors'
%% treats warnings as errors.
%% ----------------------------------------------------------------
mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs) ->
    mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, []).
mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, Opts) ->
    case check_opts(Opts) of
	[] ->
            R = try do_mk_relup(TopRelFile,BaseUpRelDcs,BaseDnRelDcs,
                                          add_code_path(Opts), Opts)
                catch throw:Error ->
                        Error
                end,
            done_mk_relup(Opts, R);
	BadArg ->
	    erlang:error({badarg, BadArg})
    end.

%% Function for checking validity of options in analogy with
%% check_args_script/1 and check_args_tar/1 in systools_make.
%% To maintain backwards compatibility, actually only outdir is checked.
check_opts([{outdir, Dir}|_Opts]) when is_list(Dir) ->
    [];
check_opts([{outdir, BadArg}|_Opts]) ->
    [{outdir, BadArg}];
check_opts([_Opt|Opts]) ->
    check_opts(Opts);
check_opts([]) ->
    [].

do_mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, Path, Opts) ->
    case systools_make:get_release(to_list(TopRelFile), Path) of
	%%
	%% TopRel = #release
	%% NameVsnApps = [{{Name, Vsn}, #application}]
	{ok, TopRel, NameVsnApps, Ws0} ->
	    case lists:member({warning,missing_sasl},Ws0) of
		true ->
		    throw({error,?MODULE,{missing_sasl,TopRel}});
		false ->
		    ok
	    end,

	    %% TopApps = [#application]
	    TopApps = lists:map(fun({_, App}) -> App end, NameVsnApps),

	    %% Up
	    {Up, Ws1} = foreach_baserel_up(TopRel, TopApps, BaseUpRelDcs, 
					  Path, Opts, Ws0),
	    %% Down
	    {Dn, Ws2} = foreach_baserel_dn(TopRel, TopApps, BaseDnRelDcs, 
					  Path, Opts, Ws1),
	    Relup = {TopRel#release.vsn, Up, Dn},

            {ok, Relup, Ws2};
	Other -> 
	    Other
    end.

done_mk_relup(Opts, {ok,Relup,Ws}) ->
    WAE = get_opt(warnings_as_errors,Opts),
    Silent = get_opt(silent,Opts),
    Noexec = get_opt(noexec,Opts),

    if WAE andalso Ws=/=[] ->
            return_error(Silent,
                         {error,?MODULE,{warnings_treated_as_errors, Ws}});
       not Noexec ->
            case write_relup_file(Relup,Opts) of
                ok ->
                    return_ok(Silent,Relup,Ws);
                Error ->
                    return_error(Silent,Error)
            end;
       true -> % noexec
            return_ok(true,Relup,Ws)
    end;
done_mk_relup(Opts, Error) ->
    return_error(get_opt(silent,Opts) orelse get_opt(noexec,Opts), Error).

return_error(true, Error) ->
    Error;
return_error(false, Error) ->
    print_error(Error),
    error.

return_ok(true,Relup,Ws) ->
    {ok,Relup,?MODULE,Ws};
return_ok(false,_Relup,Ws) ->
    print_warnings(Ws),
    ok.

%%-----------------------------------------------------------------
%% foreach_baserel_up(Rel, TopApps, BaseRelDcs, Path, Opts, Ws) -> Ret
%% foreach_baserel_dn(Rel, TopApps, BaseRelDcs, Path, Opts, Ws) -> Ret
%%
%% TopRel = #release
%% TopApps = [#application]
%% BaseRelDcs = [reldescr()]
%% reldescr() = filename() | {filename(), description()} 
%% filename() = description() = string()
%% Opts = [opt()], opt() = {path, [path()]} | silent | noexec |
%%				restart_emulator
%% Ws = [term()]
%% Ret = {VDRs, Ws}
%% VDRs = [vdr()], vdr() = {Vsn, Description, RUs}
%%
%% Generates scripts for each base release.
%%
foreach_baserel_up(TopRel, TopApps, BaseRelDcs, Path, Opts, Ws) ->
    foreach_baserel_up(TopRel, TopApps, BaseRelDcs, Path, Opts, 
		   Ws, []).

foreach_baserel_up(TopRel, TopApps, [BaseRelDc|BaseRelDcs], Path, Opts, 
	       Ws, Acc) ->
    BaseRelFile = extract_filename(BaseRelDc),

    {BaseRel, {BaseNameVsns, BaseApps}, Ws0} =
	case systools_make:get_release(BaseRelFile, Path) of
	    {ok, BR, NameVsnApps, Warns} ->
		case lists:member({warning,missing_sasl},Warns) of
		    true ->
			throw({error,?MODULE,{missing_sasl,BR}});
		    false ->
			%% NameVsnApps = [{{Name,Vsn},#application}]
			%% Gives two lists - [{Name,Vsn}] and [#application]
			{BR,lists:unzip(NameVsnApps),Warns}
		end;
	    Other1 ->
		throw(Other1)
	end,


    %%
    %% BaseRel = #release
    %%
    %% RUs = (release upgrade scripts). We really get separate
    %% scripts, one for emulator restart, one for each
    %% application, one for each added applications, and one for
    %% each removed applications.
    %%
    {RUs1, Ws1} = collect_appup_scripts(up, TopApps, BaseRel, Ws0++Ws, []),

    {RUs2, Ws2} = create_add_app_scripts(BaseRel, TopRel, RUs1, Ws1),

    {RUs3, Ws3} = create_remove_app_scripts(BaseRel, TopRel, RUs2, Ws2),

    {RUs4, Ws4} = check_for_emulator_restart(TopRel, BaseRel, RUs3, Ws3, Opts),

    case systools_rc:translate_scripts(up, RUs4, TopApps, BaseApps) of
	{ok, RUs5} ->

	    {RUs, Ws5} = fix_r15_sasl_upgrade(RUs5,Ws4,BaseNameVsns),

	    VDR = {BaseRel#release.vsn,
		   extract_description(BaseRelDc), RUs},
	    foreach_baserel_up(TopRel, TopApps, BaseRelDcs, Path, 
			       Opts, Ws5, [VDR| Acc]);
	XXX ->
	    throw(XXX)
    end;
foreach_baserel_up( _, _, [], _, _, Ws, Acc) -> 
    {Acc, Ws}.

foreach_baserel_dn(TopRel, TopApps, BaseRelDcs, Path, Opts, Ws) ->
    foreach_baserel_dn(TopRel, TopApps, BaseRelDcs, Path, Opts, 
		   Ws, []).

foreach_baserel_dn(TopRel, TopApps, [BaseRelDc|BaseRelDcs], Path, Opts, 
	       Ws, Acc) ->
    BaseRelFile = extract_filename(BaseRelDc),

    {BaseRel, BaseApps, Ws0} =
	case systools_make:get_release(BaseRelFile, Path) of
	    %%
	    %% NameVsnApps = [{{Name, Vsn}, #application}]
	    {ok, BR, NameVsnApps, Warns} ->
		case lists:member({warning,missing_sasl},Warns) of
		    true ->
			throw({error,?MODULE,{missing_sasl,BR}});
		    false ->
			%% NApps = [#application]
			NApps = lists:map(fun({_,App}) -> App end, NameVsnApps),
			{BR, NApps, Warns}
		end;
	    Other ->
		throw(Other)
	end,

    %% BaseRel = #release

    %% RUs = (release upgrade scripts)
    %%
    {RUs1, Ws1} = collect_appup_scripts(dn, TopApps, BaseRel, Ws0++Ws, []),

    {RUs2, Ws2} = create_add_app_scripts(TopRel, BaseRel, RUs1, Ws1),

    {RUs3, Ws3} = create_remove_app_scripts(TopRel, BaseRel, RUs2, Ws2),

    {RUs4, Ws4} = check_for_emulator_restart(TopRel, BaseRel, RUs3, Ws3, Opts),

    case systools_rc:translate_scripts(dn, RUs4, BaseApps, TopApps) of
	{ok, RUs} ->
	    VDR = {BaseRel#release.vsn,
		   extract_description(BaseRelDc), RUs},
	    foreach_baserel_dn(TopRel, TopApps, BaseRelDcs, Path, 
			       Opts, Ws4, [VDR| Acc]);
	XXX -> 
	    throw(XXX)
    end;
foreach_baserel_dn( _, _, [], _, _, Ws, Acc) -> 
    {Acc, Ws}.


%% check_for_emulator_restart(Rel1, Rel2, RUs, Ws, Opts) -> {NRUs, NWs}
%%
%% Rel1 = Rel2 = #release
%%
check_for_emulator_restart(#release{erts_vsn = Vsn1, name = N1}, 
                           #release{erts_vsn = Vsn2, name = N2}, RUs, Ws, 
                           Opts) when Vsn1 /= Vsn2 ->
    %% Automatically insert a restart_new_emulator instruction when
    %% erts version is changed. Also allow extra restart at the end of
    %% the upgrade if restart_emulator option is given.
    NewRUs = [[restart_new_emulator]|RUs],
    NewWs = [{erts_vsn_changed, {{N1,Vsn1}, {N2,Vsn2}}} | Ws],
    check_for_restart_emulator_opt(NewRUs, NewWs, Opts);
check_for_emulator_restart(_, _, RUs, Ws, Opts) ->
    check_for_restart_emulator_opt(RUs, Ws, Opts).

check_for_restart_emulator_opt(RUs, Ws, Opts) ->
    case get_opt(restart_emulator, Opts) of
	true -> {RUs++[[restart_emulator]], Ws};
	_ -> {RUs, Ws}
    end.


%% Special handling of the upgrade from pre R15 to post R15. In R15,
%% upgrade of the emulator was improved by moving the restart of the
%% emulator before the rest of the upgrade instructions. However, it
%% can only work if the release_handler is already upgraded to a post
%% R15 version. If not, the upgrade instructions must be backwards
%% compatible - i.e. restart_new_emulator will be the last
%% instruction, executed after all code loading, code_change etc.
fix_r15_sasl_upgrade([restart_new_emulator | RestRUs]=RUs, Ws, BaseApps) ->
    case lists:keyfind(sasl,1,BaseApps) of
	{sasl,Vsn} when Vsn < ?R15_SASL_VSN ->
	    {lists:delete(restart_emulator,RestRUs) ++ [restart_new_emulator],
	     [pre_R15_emulator_upgrade|Ws]};
	_ ->
	    {RUs,Ws}
    end;
fix_r15_sasl_upgrade(RUs, Ws, _BaseApps) ->
    {RUs,Ws}.


%% collect_appup_scripts(Mode, TopApps, BaseRel, Ws, RUs) -> {NRUs, NWs}
%% Mode = up | dn
%% TopApps = [#application]
%% BaseRel = #release
%%
%% Gets the script corresponding to Mode and BaseRel in the .appup file
%% for each application.
%%
collect_appup_scripts(Mode, [TopApp|TopApps], BaseRel, Ws, RUs) ->

    case lists:keysearch(TopApp#application.name, 1, 
			 BaseRel#release.applications) of
	{value, {_Name, BaseVsn, _Type}} ->
	    %% io:format("collect appup script: ~p~n", 
	    %% [TopApp#application.name]),
	    if  
		TopApp#application.vsn == BaseVsn ->
		    %% Same version: nothing to do.
		    collect_appup_scripts(Mode, TopApps, BaseRel, Ws, RUs);
		true ->
		    %% We must have an upgrade script for BaseVsn
		    {RU1s, Ws1} = get_script_from_appup(Mode, TopApp, BaseVsn, 
							Ws, RUs),
		    collect_appup_scripts(Mode, TopApps, BaseRel, Ws1, RU1s)
	    end;
	false ->
	    collect_appup_scripts(Mode, TopApps, BaseRel, Ws, RUs)
    end;
collect_appup_scripts(_, [], _, Ws, RUs) -> {RUs, Ws}.


%% create_add_app_scripts(FromRel, ToRel, RU0s, W0s) -> {RUs, Ws}
%%
%% FromRel = ToRel = #release
%% ToApps = [#application]
%%
create_add_app_scripts(FromRel, ToRel, RU0s, W0s) -> 
    AddedNs = [{N, T} || {N, _V, T} <- ToRel#release.applications,
		    not lists:keymember(N, 1, FromRel#release.applications)],
    %% io:format("Added apps: ~p~n", [AddedNs]),
    RUs = [[{add_application, N, T}] || {N, T} <- AddedNs],
    {RUs ++ RU0s, W0s}.


%% create_remove_app_scripts(FromRel, ToRel, RU0s, W0s) -> {RUs, Ws}
%%
%% FromRel = ToRel = #release
%% ToApps = [#application]
%%
%% XXX ToApps not used.
%%
create_remove_app_scripts(FromRel, ToRel, RU0s, W0s) -> 
    RemovedNs = [N || {N, _V, _T} <- FromRel#release.applications,
		      not lists:keymember(N, 1, ToRel#release.applications)],
    %% io:format("Removed apps: ~p~n", [RemovedNs]),
    RUs = [[{remove_application, N}] || N <- RemovedNs],
    {RUs ++ RU0s, W0s}.

%% get_script_from_appup(Mode, TopApp, BaseVsn, Ws, RUs) -> {NRUs, NWs}
%% Mode = up | dn
%% TopApp = #application
%%
%% XXX We do not operate on Ws and RUs, we just return (possibly) one
%% warning, and one script. Remove the Ws And RUs arguments and return 
%% only what is relevant.
%%
get_script_from_appup(Mode, TopApp, BaseVsn, Ws, RUs) ->
    FName = filename:join([TopApp#application.dir, 
			   to_list(TopApp#application.name) ++ ".appup"]),
    {VsnRUs, TopVsn} = case systools_lib:read_term(FName) of
			   {ok, {TopVsn0, UpVsnRUs, DnVsnRUs}} ->
			       VsnRUs0 = case Mode of
					     up ->
						 UpVsnRUs;
					     dn ->
						 DnVsnRUs
					 end,
			       {VsnRUs0, TopVsn0};
			   X -> 
			       throw({error, ?MODULE, {file_problem, 
						       {FName, X}}})
		       end,
    Ws1 = if  
	      TopApp#application.vsn == TopVsn -> 
		  Ws;
	      true -> 
		  %% XXX Why is this a warning only?
		  [{bad_vsn, {TopVsn, TopApp#application.vsn}}| Ws]
	  end,
    case appup_search_for_version(BaseVsn, VsnRUs) of
	{ok, RU} ->
	    {RUs ++ [RU], Ws1};
	error ->
	    throw({error, ?MODULE, {no_relup, FName, TopApp, BaseVsn}})
    end.

appup_search_for_version(BaseVsn,[{BaseVsn,RU}|_]) ->
    {ok,RU};
appup_search_for_version(BaseVsn,[{Vsn,RU}|VsnRUs]) when is_binary(Vsn) ->
    case re:run(BaseVsn,Vsn,[unicode,{capture,first,list}]) of
	{match,[BaseVsn]} ->
	    {ok, RU};
	_ ->
	    appup_search_for_version(BaseVsn,VsnRUs)
    end;
appup_search_for_version(BaseVsn,[_|VsnRUs]) ->
    appup_search_for_version(BaseVsn,VsnRUs);
appup_search_for_version(_,[]) ->
    error.




%% Primitives for the "lists of release names" that we upgrade from
%% and to.
extract_filename({N, _D}) -> to_list(N);
extract_filename(N) -> to_list(N).

extract_description({_N, D}) -> D;
extract_description(_) -> [].

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.


%% write_relup_file(Relup, Opts) -> ok
%% 
%% Writes a relup file.
%%
write_relup_file(Relup, Opts) ->
    Filename = filename:join(filename:absname(get_opt(outdir,Opts)),
                             "relup"),
    case file:open(Filename, [write,{encoding,utf8}]) of
        {ok, Fd} ->
            io:format(Fd, "%% ~s~n~tp.~n", [epp:encoding_to_string(utf8),Relup]),
            case file:close(Fd) of
                ok -> ok;
                {error,Reason} ->
                    {error, ?MODULE, {file_problem, {"relup", {close,Reason}}}}
            end;
        {error, Reason} ->
            {error, ?MODULE, {file_problem, {"relup", {open, Reason}}}}
    end.

add_code_path(Opts) ->
    case get_opt(path, Opts) of
	false ->
	    code:get_path();
	Paths0 ->
	    Paths1 = [to_list(P) || P <- Paths0],
	    %% Allow wild-card expansion.
	    Paths2 = systools_lib:get_path(Paths1), 
	    make_set(Paths2 ++ code:get_path())
    end.

get_opt(Opt, Opts) ->
    case lists:keysearch(Opt, 1, Opts) of
	{value, {_, Val}} -> Val;
	_ ->
	    case lists:member(Opt, Opts) of
		true -> true;
		_ -> default(Opt)
	    end
    end.

%% make elements in list unique without rearranging the
%% elements.
%%
%% XXX Not very efficient.
%%
make_set([]) -> [];
make_set([H|T]) ->
    [H | [ Y || Y<- make_set(T),
		Y =/= H]].

default(path)   -> false;
default(noexec) -> false;
default(silent) -> false;
default(restart_emulator) -> false;
default(outdir) -> ".";
default(warnings_as_errors) -> false.

print_error({error, Mod, Error}) ->
    S = apply(Mod, format_error, [Error]),
    io:format("~ts", [S]);
print_error(Other) ->
    io:format("Error: ~tp~n", [Other]).

format_error({file_problem, {File, What}}) ->
    io_lib:format("Could not ~w file ~ts~n", [get_reason(What), File]);
format_error({no_relup, File, App, Vsn}) ->
    io_lib:format("No release upgrade script entry for ~w-~ts to ~w-~ts "
		  "in file ~ts~n",
		  [App#application.name, App#application.vsn, 
		   App#application.name, Vsn, File]);
format_error({missing_sasl,Release}) ->
    io_lib:format("No sasl application in release ~ts, ~ts. "
		  "Can not be upgraded.",
		  [Release#release.name, Release#release.vsn]);
format_error({warnings_treated_as_errors, Warnings}) ->
    io_lib:format("Warnings being treated as errors:~n~ts",
                  [[format_warning("",W) || W <- Warnings]]);
format_error(Error) ->
    io_lib:format("~tp~n", [Error]).


print_warnings(Ws) when is_list(Ws) ->
    lists:foreach(fun(W) -> print_warning(W) end, Ws);
print_warnings(W) ->
    print_warning(W).

print_warning(W) ->
    io:format("~ts", [format_warning(W)]).

format_warning(W) ->
    format_warning("*WARNING* ", W).

format_warning(Prefix, {erts_vsn_changed, {Rel1, Rel2}}) ->
    io_lib:format("~tsThe ERTS version changed between ~tp and ~tp~n",
		  [Prefix, Rel1, Rel2]);
format_warning(Prefix, pre_R15_emulator_upgrade) ->
    io_lib:format("~tsUpgrade from an OTP version earlier than R15. New code should be compiled with the old emulator.~n",[Prefix]);
format_warning(Prefix, What) ->
    io_lib:format("~ts~tp~n",[Prefix, What]).


get_reason({error, {open, _, _}}) -> open;
get_reason({error, {read, _, _}}) -> read;
get_reason({error, {parse, _, _}}) -> parse;
get_reason({error, {close, _, _}}) -> close;
get_reason({error, {open, _}}) -> open;
get_reason({error, {read, _}}) -> read;
get_reason({error, {parse, _}}) -> parse;
get_reason({open, _}) -> open;
get_reason({read, _}) -> read;
get_reason({parse, _}) -> parse;
get_reason({close, _}) -> close;
get_reason(open) -> open;
get_reason(read) -> read;
get_reason(parse) -> parse.
