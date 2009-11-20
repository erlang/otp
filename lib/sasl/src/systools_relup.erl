%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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
%%       | {outdir, string()}
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
%% the output "relup" file, and restart_emulator ensures that the new
%% emulator is restarted (as the final step).
%% ----------------------------------------------------------------
mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs) ->
    mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, []).
mk_relup(TopRelFile, BaseUpRelDcs, BaseDnRelDcs, Opts) ->
    case check_opts(Opts) of
	[] ->
	    R = (catch do_mk_relup(TopRelFile,BaseUpRelDcs,BaseDnRelDcs,
				   add_code_path(Opts), Opts)),
	    case {get_opt(silent, Opts), get_opt(noexec, Opts)} of
		{false, false} ->
		    case R of
			{ok, _Res, _Mod, Ws} -> 
			    print_warnings(Ws),
			    ok;
			Other -> 
			    print_error(Other),
			    error
		    end;
		_ -> 
		    R
	    end;
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
    ModTest = false,
    case systools_make:get_release(to_list(TopRelFile), Path, ModTest) of
	%%
	%% TopRel = #release
	%% NameVsnApps = [{{Name, Vsn}, #application}]
	{ok, TopRel, NameVsnApps, Ws0} ->
	    %%
	    %% TopApps = [#application]
	    TopApps = lists:map(fun({_, App}) -> App end, NameVsnApps),

	    %% Up
	    {Up, Ws1} = foreach_baserel_up(TopRel, TopApps, BaseUpRelDcs, 
					  Path, Opts, Ws0),
	    %% Down
	    {Dn, Ws2} = foreach_baserel_dn(TopRel, TopApps, BaseDnRelDcs, 
					  Path, Opts, Ws1),
	    Relup = {TopRel#release.vsn, Up, Dn},
	    write_relup_file(Relup, Opts),
	    {ok, Relup, ?MODULE, Ws2};
	Other -> 
	    throw(Other)
    end.

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

    {ok, BaseRel} = systools_make:read_release(BaseRelFile, Path),

    %%
    %% BaseRel = #release
    %%
    %% RUs = (release upgrade scripts). We really get separate
    %% scripts, one for emulator restart, one for each
    %% application, one for each added applications, and one for
    %% each removed applications.
    %%
    {RUs1, Ws1} = collect_appup_scripts(up, TopApps, BaseRel, Ws, []),

    {RUs2, Ws2} = create_add_app_scripts(BaseRel, TopRel, RUs1, Ws1),

    {RUs3, Ws3} = create_remove_app_scripts(BaseRel, TopRel, RUs2, Ws2),

    {RUs4, Ws4} = 
	check_for_emulator_restart(TopRel, BaseRel, RUs3, Ws3, Opts),

    ModTest = false,
    BaseApps =
	case systools_make:get_release(BaseRelFile, Path, ModTest) of
	    {ok, _, NameVsnApps, _Warns} ->
		lists:map(fun({_,App}) -> App end, NameVsnApps);
	    Other1 ->
		throw(Other1)
	end,

    case systools_rc:translate_scripts(up, RUs4, TopApps, BaseApps) of
	{ok, RUs} ->
	    VDR = {BaseRel#release.vsn,
		   extract_description(BaseRelDc), RUs},
	    foreach_baserel_up(TopRel, TopApps, BaseRelDcs, Path, 
			       Opts, Ws4, [VDR| Acc]);
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

    {ok, BaseRel} = systools_make:read_release(BaseRelFile, Path),

    %% BaseRel = #release

    %% RUs = (release upgrade scripts)
    %%
    {RUs1, Ws1} = collect_appup_scripts(dn, TopApps, BaseRel, Ws, []),

    ModTest = false,
    {BaseApps, Ws2} =
	case systools_make:get_release(BaseRelFile, Path, ModTest) of
	    %%
	    %% NameVsnApps = [{{Name, Vsn}, #application}]
	    {ok, _, NameVsnApps, Warns} ->
		%%
		%% NApps = [#application]
		NApps = lists:map(fun({_,App}) -> App end, NameVsnApps),
		{NApps, Warns ++ Ws1};
	    Other ->
		throw(Other)
	end,

    RUs2 = RUs1,

    {RUs3, Ws3} = create_add_app_scripts(TopRel, BaseRel, RUs2, Ws2),

    {RUs4, Ws4} = create_remove_app_scripts(TopRel, BaseRel, RUs3, Ws3),

    {RUs5, Ws5} = check_for_emulator_restart(TopRel, BaseRel,
					     RUs4, Ws4, Opts),

    case systools_rc:translate_scripts(dn, RUs5, BaseApps, TopApps) of
	{ok, RUs} ->
	    VDR = {BaseRel#release.vsn,
		   extract_description(BaseRelDc), RUs},
	    foreach_baserel_dn(TopRel, TopApps, BaseRelDcs, Path, 
			       Opts, Ws5, [VDR| Acc]);
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
                           _Opts) when Vsn1 /= Vsn2 ->
    {RUs++[[restart_new_emulator]], [{erts_vsn_changed, {N1, N2}} | Ws]};
check_for_emulator_restart(_, _, RUs, Ws, Opts) ->
    case get_opt(restart_emulator, Opts) of
	true -> {RUs++[[restart_new_emulator]], Ws};
	_ -> {RUs, Ws}
    end.

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
    AddedNs = [N || {N, _V, _T} <- ToRel#release.applications,
		    not lists:keymember(N, 1, FromRel#release.applications)],
    %% io:format("Added apps: ~p~n", [AddedNs]),
    RUs = [[{add_application, N}] || N <- AddedNs],
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
    case lists:keysearch(BaseVsn, 1, VsnRUs) of
	{value, {_, RU}} ->
	    {RUs ++ [RU], Ws1};
	_ ->
	    throw({error, ?MODULE, {no_relup, FName, TopApp, BaseVsn}})
    end.


%% Primitives for the "lists of release names" that we upgrade from
%% and to.
extract_filename({N, _D}) -> to_list(N);
extract_filename(N) -> to_list(N).

extract_description({_N, D}) -> D;
extract_description(_) -> [].

to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.


%% write_relup_file(Relup, Opts) -> {ok. Relup}
%% 
%% Writes a relup file.
%%
write_relup_file(Relup, Opts) ->
    case get_opt(noexec, Opts) of
	true -> 
	    ok;
	_ ->
	    Filename = case get_opt(outdir, Opts) of
			   OutDir when is_list(OutDir) ->
			       filename:join(filename:absname(OutDir),
					     "relup");
			   false ->
			       "relup";
			   Badarg ->
			       throw({error, ?MODULE, {badarg, {outdir,Badarg}}})
		       end,
			   
	    case file:open(Filename, [write]) of
		{ok, Fd} ->
		    io:format(Fd, "~p.~n", [Relup]),
		    file:close(Fd);
		{error, Reason} ->
		    throw({error, ?MODULE, {file_problem, {"relup", Reason}}})
	    end
    end,
    {ok, Relup}.    

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
default(outdir) -> false.

print_error({'EXIT', Err}) -> 
    print_error(Err);
print_error({error, Mod, Error}) ->
    S = apply(Mod, format_error, [Error]),
    io:format(S, []);
print_error(Other) ->
    io:format("Error: ~p~n", [Other]).

format_error({file_problem, {"relup", _Posix}}) ->
    io_lib:format("Could not open file relup~n", []);
format_error({file_problem, {File, What}}) ->
    io_lib:format("Could not ~p file ~p~n", [get_reason(What), File]);
format_error({no_relup, File, App, Vsn}) ->
    io_lib:format("No release upgrade script entry for ~p-~s to ~p-~s "
		  "in file ~p~n",
		  [App#application.name, App#application.vsn, 
		   App#application.name, Vsn, File]);

format_error(Error) ->
    io:format("~p~n", [Error]).


print_warnings(Ws) when is_list(Ws) ->
    lists:foreach(fun(W) -> print_warning(W) end, Ws);
print_warnings(W) ->
    print_warning(W).

print_warning(W) ->
    S = format_warning(W),
    io:format("~s", [S]).

format_warning({erts_vsn_changed, {Rel1, Rel2}}) ->
    io_lib:format("*WARNING* The ERTS version changed between ~p and ~p~n",
		  [Rel1, Rel2]);
format_warning(What) ->
    io_lib:format("*WARNING* ~p~n",[What]).


get_reason({error, {open, _, _}}) -> open;
get_reason({error, {read, _, _}}) -> read;
get_reason({error, {parse, _, _}}) -> parse;
get_reason({error, {open, _}}) -> open;
get_reason({error, {read, _}}) -> read;
get_reason({error, {parse, _}}) -> parse;
get_reason({open, _}) -> open;
get_reason({read, _}) -> read;
get_reason({parse, _}) -> parse;
get_reason(open) -> open;
get_reason(read) -> read;
get_reason(parse) -> parse.
