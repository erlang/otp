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
-module(systools_make).

%% Purpose : Create start script. RelName.rel --> RelName.{script,boot}.
%%           and create a tar file of a release (RelName.tar.gz)

-export([make_script/1, make_script/2, make_script/3,
	 make_tar/1, make_tar/2]).

-export([format_error/1, format_warning/1]).

-export([read_release/2, get_release/2, get_release/3,
	 get_release/4, pack_app/1]).

-export([read_application/4]).

-export([make_hybrid_boot/5]).

-import(lists, [filter/2, keysort/2, keysearch/3, map/2, reverse/1,
		append/1, foldl/3,  member/2, foreach/2]).

-include("systools.hrl").

-include_lib("kernel/include/file.hrl").

-define(XREF_SERVER, systools_make).

-compile({inline,[{badarg,2}]}).

%%-----------------------------------------------------------------
%% Create a boot script from a release file.
%% Options is a list of {path, Path} | silent | local
%%         | warnings_as_errors
%% where path sets the search path, silent supresses error message
%% printing on console, local generates a script with references
%% to the directories there the applications are found,
%% and warnings_as_errors treats warnings as errors.
%%
%% New options: {path,Path} can contain wildcards
%%              src_tests
%%              {variables,[{Name,AbsString}]}
%%              {machine, jam | beam | vee}
%%              exref | {exref, [AppName]}
%%              no_warn_sasl
%%-----------------------------------------------------------------

make_script(RelName) when is_list(RelName) ->
    make_script(RelName, []);
make_script(RelName) ->
    badarg(RelName,[RelName]).

make_script(RelName, Flags) when is_list(RelName), is_list(Flags) ->
    case get_outdir(Flags) of
	"" ->
	    make_script(RelName, RelName, Flags);
	OutDir ->
	    %% To maintain backwards compatibility for make_script/3,
	    %% the boot script file name is constructed here, before
	    %% checking the validity of OutDir
	    %% (is done in check_args_script/1)
	    Output = filename:join(OutDir, filename:basename(RelName)),
	    make_script(RelName, Output, Flags)
    end.
    
make_script(RelName, Output, Flags) when is_list(RelName),
					 is_list(Output),
					 is_list(Flags) ->
    case check_args_script(Flags) of
	[] ->
	    Path0 = get_path(Flags),
	    Path1 = mk_path(Path0), % expand wildcards etc.
	    Path  = make_set(Path1 ++ code:get_path()),
	    ModTestP = {member(src_tests, Flags),xref_p(Flags)},
	    case get_release(RelName, Path, ModTestP, machine(Flags)) of
		{ok, Release, Appls, Warnings0} ->
		    Warnings = wsasl(Flags, Warnings0),
		    case systools_lib:werror(Flags, Warnings) of
			true ->
                            Warnings1 = [W || {warning,W}<-Warnings],
			    return({error,?MODULE,
                                    {warnings_treated_as_errors,Warnings1}},
                                   Warnings,
                                   Flags);
			false ->
			    case generate_script(Output,Release,Appls,Flags) of
				ok ->
				    return(ok,Warnings,Flags);
				Error ->
				    return(Error,Warnings,Flags)
			    end
		    end;
		Error ->
		    return(Error,[],Flags)
	    end;
	ErrorVars ->
	    badarg(ErrorVars, [RelName, Flags])
    end;

make_script(RelName, _Output, Flags) when is_list(Flags) ->
    badarg(RelName,[RelName, Flags]);
make_script(RelName, _Output, Flags) ->
    badarg(Flags,[RelName, Flags]).

wsasl(Options, Warnings) ->
    case lists:member(no_warn_sasl,Options) of
	true -> lists:delete({warning,missing_sasl},Warnings);
	false -> Warnings
    end.

badarg(BadArg, Args) ->
    erlang:error({badarg,BadArg}, Args).

machine(Flags) ->
    case get_flag(machine,Flags) of
	{machine, Machine} when is_atom(Machine) -> Machine;
	_                                        -> false
    end.

get_path(Flags) ->
    case get_flag(path,Flags) of
	{path,Path} when is_list(Path) -> Path;
	_                              -> []
    end.

get_outdir(Flags) ->
    case get_flag(outdir,Flags) of
	{outdir,OutDir} when is_list(OutDir) ->
	    OutDir;
	_ -> % false | {outdir, Badarg}
	    ""
    end.

return(ok,Warnings,Flags) ->
    case member(silent,Flags) of
	true ->
            {ok,?MODULE,Warnings};
	_ ->
            io:format("~ts",[format_warning(Warnings)]),
            ok
    end;
return({error,Mod,Error},_,Flags) ->
    case member(silent,Flags) of
	true ->
	    {error,Mod,Error};
	_ ->
	    io:format("~ts",[Mod:format_error(Error)]),
	    error
    end.


%%-----------------------------------------------------------------
%% Make hybrid boot file for upgrading emulator. The resulting boot
%% file is a combination of the two input files, where kernel, stdlib
%% and sasl versions are taken from the second file (the boot file of
%% the new release), and all other application versions from the first
%% file (the boot file of the old release).
%%
%% The most important thing that can fail here is that the input boot
%% files do not contain all three base applications - kernel, stdlib
%% and sasl.
%%
%% TmpVsn = string(),
%% Paths = {KernelPath,StdlibPath,SaslPath}
%% Returns {ok,Boot} | {error,Reason}
%% Boot1 = Boot2 = Boot = binary()
%% Reason = {app_not_found,App} | {app_not_replaced,App}
%% App = kernel | stdlib | sasl
make_hybrid_boot(TmpVsn, Boot1, Boot2, Paths, Args) ->
    catch do_make_hybrid_boot(TmpVsn, Boot1, Boot2, Paths, Args).
do_make_hybrid_boot(TmpVsn, Boot1, Boot2, Paths, Args) ->
    {script,{_RelName1,_RelVsn1},Script1} = binary_to_term(Boot1),
    {script,{RelName2,_RelVsn2},Script2} = binary_to_term(Boot2),
    MatchPaths = get_regexp_path(Paths),
    NewScript1 = replace_paths(Script1,MatchPaths),
    {Kernel,Stdlib,Sasl} = get_apps(Script2,undefined,undefined,undefined),
    NewScript2 = replace_apps(NewScript1,Kernel,Stdlib,Sasl),
    NewScript3 = add_apply_upgrade(NewScript2,Args),
    Boot = term_to_binary({script,{RelName2,TmpVsn},NewScript3}),
    {ok,Boot}.

%% For each app, compile a regexp that can be used for finding its path
get_regexp_path({KernelPath,StdlibPath,SaslPath}) ->
    {ok,KernelMP} = re:compile("kernel-[0-9\.]+",[unicode]),
    {ok,StdlibMP} = re:compile("stdlib-[0-9\.]+",[unicode]),
    {ok,SaslMP} = re:compile("sasl-[0-9\.]+",[unicode]),
    [{KernelMP,KernelPath},{StdlibMP,StdlibPath},{SaslMP,SaslPath}].

%% For each path in the script, check if it matches any of the MPs
%% found above, and if so replace it with the correct new path.
replace_paths([{path,Path}|Script],MatchPaths) ->
    [{path,replace_path(Path,MatchPaths)}|replace_paths(Script,MatchPaths)];
replace_paths([Stuff|Script],MatchPaths) ->
    [Stuff|replace_paths(Script,MatchPaths)];
replace_paths([],_) ->
    [].

replace_path([Path|Paths],MatchPaths) ->
    [do_replace_path(Path,MatchPaths)|replace_path(Paths,MatchPaths)];
replace_path([],_) ->
    [].

do_replace_path(Path,[{MP,ReplacePath}|MatchPaths]) ->
    case re:run(Path,MP,[{capture,none}]) of
	nomatch -> do_replace_path(Path,MatchPaths);
	match -> ReplacePath
    end;
do_replace_path(Path,[]) ->
    Path.

%% Return the entries for loading the three base applications
get_apps([{kernelProcess,application_controller,
	   {application_controller,start,[{application,kernel,_}]}}=Kernel|
	  Script],_,Stdlib,Sasl) ->
    get_apps(Script,Kernel,Stdlib,Sasl);
get_apps([{apply,{application,load,[{application,stdlib,_}]}}=Stdlib|Script],
	 Kernel,_,Sasl) ->
    get_apps(Script,Kernel,Stdlib,Sasl);
get_apps([{apply,{application,load,[{application,sasl,_}]}}=Sasl|_Script],
	 Kernel,Stdlib,_) ->
    {Kernel,Stdlib,Sasl};
get_apps([_|Script],Kernel,Stdlib,Sasl) ->
    get_apps(Script,Kernel,Stdlib,Sasl);
get_apps([],undefined,_,_) ->
    throw({error,{app_not_found,kernel}});
get_apps([],_,undefined,_) ->
    throw({error,{app_not_found,stdlib}});
get_apps([],_,_,undefined) ->
    throw({error,{app_not_found,sasl}}).


%% Replace the entries for loading the base applications
replace_apps([{kernelProcess,application_controller,
	       {application_controller,start,[{application,kernel,_}]}}|
	      Script],Kernel,Stdlib,Sasl) ->
    [Kernel|replace_apps(Script,undefined,Stdlib,Sasl)];
replace_apps([{apply,{application,load,[{application,stdlib,_}]}}|Script],
	     Kernel,Stdlib,Sasl) ->
    [Stdlib|replace_apps(Script,Kernel,undefined,Sasl)];
replace_apps([{apply,{application,load,[{application,sasl,_}]}}|Script],
	     _Kernel,_Stdlib,Sasl) ->
    [Sasl|Script];
replace_apps([Stuff|Script],Kernel,Stdlib,Sasl) ->
    [Stuff|replace_apps(Script,Kernel,Stdlib,Sasl)];
replace_apps([],undefined,undefined,_) ->
    throw({error,{app_not_replaced,sasl}});
replace_apps([],undefined,_,_) ->
    throw({error,{app_not_replaced,stdlib}});
replace_apps([],_,_,_) ->
    throw({error,{app_not_replaced,kernel}}).


%% Finally add an apply of release_handler:new_emulator_upgrade - which will
%% complete the execution of the upgrade script (relup).
add_apply_upgrade(Script,Args) ->
    [{progress, started} | RevScript] = lists:reverse(Script),
    lists:reverse([{progress,started},
		   {apply,{release_handler,new_emulator_upgrade,Args}} |
		   RevScript]).



%%-----------------------------------------------------------------
%% Create a release package from a release file.
%% Options is a list of {path, Path} | silent |
%%    {dirs, [src,include,examples,..]} | {erts, ErtsDir} where path
%% sets the search path, silent supresses error message printing,
%% dirs includes the specified directories (per application) in the
%% release package and erts specifies that the erts-Vsn/bin directory
%% should be included in the release package and there it can be found.
%%
%% New options: {path,Path} can contain wildcards
%%              src_tests
%%              exref | {exref, [AppName]}
%%              {variables,[{Name,AbsString}]}
%%              {machine, jam | beam | vee}
%%              {var_tar, include | ownfile | omit}
%%              no_warn_sasl
%%              warnings_as_errors
%%
%% The tar file contains:
%%         lib/App-Vsn/ebin
%%                    /priv
%%                   [/src]
%%                   [/include]
%%                   [/doc]
%%                   [/examples]
%%                   [/...]
%%         Variable1.tar.gz
%%         ...
%%         VariableN.tar.gz
%%         releases/RelName.rel
%%                  RelVsn/start.boot
%%                         relup
%%                         sys.config
%%         erts-EVsn[/bin]
%%-----------------------------------------------------------------

make_tar(RelName) when is_list(RelName) ->
    make_tar(RelName, []);
make_tar(RelName) ->
    badarg(RelName,[RelName]).

make_tar(RelName, Flags) when is_list(RelName), is_list(Flags) ->
    case check_args_tar(Flags) of
	[] ->
	    Path0 = get_path(Flags),
	    Path1 = mk_path(Path0),
	    Path  = make_set(Path1 ++ code:get_path()),
	    ModTestP = {member(src_tests, Flags),xref_p(Flags)},
	    case get_release(RelName, Path, ModTestP, machine(Flags)) of
		{ok, Release, Appls, Warnings0} ->
		    Warnings = wsasl(Flags, Warnings0),
		    case systools_lib:werror(Flags, Warnings) of
			true ->
                            Warnings1 = [W || {warning,W}<-Warnings],
			    return({error,?MODULE,
                                    {warnings_treated_as_errors,Warnings1}},
                                   Warnings,
                                   Flags);
			false ->
                            case catch mk_tar(RelName, Release, Appls, Flags, Path1) of
                                ok ->
                                    return(ok,Warnings,Flags);
                                Error ->
                                    return(Error,Warnings,Flags)
                            end
                    end;
		Error ->
		    return(Error,[],Flags)
	    end;
	ErrorVars ->
	    badarg(ErrorVars, [RelName, Flags])
    end;
make_tar(RelName, Flags) when is_list(Flags) ->
    badarg(RelName,[RelName, Flags]);
make_tar(RelName, Flags) ->
    badarg(Flags,[RelName, Flags]).
    
%%______________________________________________________________________
%% get_release(File, Path) ->
%% get_release(File, Path, ModTestP) ->
%% get_release(File, Path, ModTestP, Machine) ->
%%     {ok, #release, [{{Name,Vsn},#application}], Warnings} | {error, What}

get_release(File, Path) ->
    get_release(File, Path, {false,false}, false).

get_release(File, Path, ModTestP) ->
    get_release(File, Path, ModTestP, false).

get_release(File, Path, ModTestP, Machine) ->
    case catch get_release1(File, Path, ModTestP, Machine) of
	{error, Error} ->
	    {error, ?MODULE, Error};
	{'EXIT', Why} ->
	    {error, ?MODULE, {'EXIT',Why}};
	Answer ->
	    Answer
    end.
	
get_release1(File, Path, ModTestP, Machine) ->
    {ok, Release, Warnings1} = read_release(File, Path),
    {ok, Appls0} = collect_applications(Release, Path),
    {ok, Appls1} = check_applications(Appls0),
    {ok, Appls2} = sort_used_and_incl_appls(Appls1, Release), % OTP-4121, OTP-9984
    {ok, Warnings2} = check_modules(Appls2, Path, ModTestP, Machine),
    {ok, Appls} = sort_appls(Appls2),
    {ok, Release, Appls, Warnings1 ++ Warnings2}.

%%______________________________________________________________________
%% read_release(File, Path) -> {ok, #release} | throw({error, What})

read_release(File, Path) ->
    case read_file(File ++ ".rel", ["."|Path]) of
	{ok, Release, _FullName} ->
	    check_rel(Release);
	{error,Error} ->
	    throw({error,?MODULE,Error})
    end.

check_rel(Release) ->
    case catch check_rel1(Release) of
	{ok, {Name,Vsn,Evsn,Appl,Incl}, Ws} ->
	    {ok, #release{name=Name, vsn=Vsn,
			  erts_vsn=Evsn,
			  applications=Appl,
			  incl_apps=Incl},
	    Ws};
	{error, Error} ->
	    throw({error,?MODULE,Error});
	Error ->
	    throw({error,?MODULE,Error})
    end.

check_rel1({release,{Name,Vsn},{erts,EVsn},Appl}) when is_list(Appl) ->
    Name = check_name(Name),
    Vsn = check_vsn(Vsn),
    EVsn = check_evsn(EVsn),
    {{Appls,Incls},Ws} = check_appl(Appl),
    {ok, {Name,Vsn,EVsn,Appls,Incls},Ws};
check_rel1(_) ->
    {error, badly_formatted_release}.

check_name(Name) ->
    case string_p(Name) of
	true ->
	    Name;
	_ ->
	    throw({error,{illegal_name, Name}})
    end.

check_vsn(Vsn) ->
    case string_p(Vsn) of
	true ->
	    Vsn;
	_ ->
	    throw({error,{illegal_form, Vsn}})
    end.

check_evsn(Vsn) ->
    case string_p(Vsn) of
	true ->
	    Vsn;
	_ ->
	    throw({error,{illegal_form, {erts,Vsn}}})
    end.

check_appl(Appl) ->
    case filter(fun({App,Vsn}) when is_atom(App) ->
			not string_p(Vsn);
		   ({App,Vsn,Incl}) when is_atom(App), is_list(Incl) ->
			case {string_p(Vsn), a_list_p(Incl)} of
			    {true, true} -> false;
			    _            -> true
			end;
		   ({App,Vsn,Type}) when is_atom(App), is_atom(Type) ->
			case {string_p(Vsn), is_app_type(Type)} of
			    {true, true} -> false;
			    _            -> true
			end;
		   ({App,Vsn,Type,Incl}) when is_atom(App),
					      is_atom(Type),
					      is_list(Incl) ->
			case {string_p(Vsn),is_app_type(Type),a_list_p(Incl)} of
			    {true, true, true} -> false;
			    _                  -> true
			end;
		   (_) ->
			true
		end,
		Appl) of
	[] ->
	    {ApplsNoIncls,Incls} = split_app_incl(Appl),
	    {ok,Ws} = mandatory_applications(ApplsNoIncls,undefined,
					     undefined,undefined),
	    {{ApplsNoIncls,Incls},Ws};
	Illegal ->
	    throw({error, {illegal_applications,Illegal}})
    end.

mandatory_applications([{kernel,_,Type}|Apps],undefined,Stdlib,Sasl) ->
    mandatory_applications(Apps,Type,Stdlib,Sasl);
mandatory_applications([{stdlib,_,Type}|Apps],Kernel,undefined,Sasl) ->
    mandatory_applications(Apps,Kernel,Type,Sasl);
mandatory_applications([{sasl,_,Type}|Apps],Kernel,Stdlib,undefined) ->
    mandatory_applications(Apps,Kernel,Stdlib,Type);
mandatory_applications([_|Apps],Kernel,Stdlib,Sasl) ->
    mandatory_applications(Apps,Kernel,Stdlib,Sasl);
mandatory_applications([],Type,_,_) when Type=/=permanent ->
    error_mandatory_application(kernel,Type);
mandatory_applications([],_,Type,_) when Type=/=permanent ->
    error_mandatory_application(stdlib,Type);
mandatory_applications([],_,_,undefined) ->
    {ok, [{warning,missing_sasl}]};
mandatory_applications([],_,_,_) ->
    {ok,[]}.

error_mandatory_application(App,undefined) ->
    throw({error, {missing_mandatory_app, App}});
error_mandatory_application(App,Type) ->
    throw({error, {mandatory_app, App, Type}}).

split_app_incl(Appl) -> split_app_incl(Appl, [], []).

split_app_incl([{App,Vsn}|Appls], Apps, Incls) ->
    split_app_incl(Appls, [{App,Vsn,permanent}|Apps], Incls);
split_app_incl([{App,Vsn,Incl}|Appls], Apps,Incls) when is_list(Incl) ->
    split_app_incl(Appls, [{App,Vsn,permanent}|Apps], [{App,Incl}|Incls]);
split_app_incl([{App,Vsn,Type}|Appls], Apps, Incls) ->
    split_app_incl(Appls, [{App,Vsn,Type}|Apps], Incls);
split_app_incl([{App,Vsn,Type,Incl}|Appls], Apps, Incls) when is_list(Incl) ->
    split_app_incl(Appls, [{App,Vsn,Type}|Apps], [{App,Incl}|Incls]);
split_app_incl([], Apps, Incls) ->
    {reverse(Apps),reverse(Incls)}.

%%______________________________________________________________________
%% collect_applications(#release, Path) -> 
%%    {ok,[{{Name,Vsn},#application}]} |
%%    throw({error, What})
%% Read all the application files specified in the release descriptor

collect_applications(Release, Path) ->
    Appls = Release#release.applications,
    Incls = Release#release.incl_apps,
    X = foldl(fun({Name,Vsn,Type}, {Ok, Errs}) ->
		      case read_application(to_list(Name), Vsn, Path, Incls) of
			  {ok, A} ->
			      case {A#application.name,A#application.vsn} of
				 {Name,Vsn} ->
				     {[{{Name,Vsn}, A#application{type=Type}} | Ok],
				      Errs};
				 E ->
				     {Ok, [{bad_application_name, {Name, E}} | Errs]}
			     end;
			  {error, What} ->
			      {Ok, [{error_reading, {Name, What}} | Errs]}
		      end
	      end, {[],[]}, Appls),
    case X of
	{A, []} ->
	    {ok, reverse(A)};
	{_, Errs} ->
	    throw({error, Errs})
    end.


%%______________________________________________________________________
%% read_application(Name, Vsn, Path, Incls) -> {ok, #release} | {error, What}

read_application(Name, Vsn, Path, Incls) ->
    read_application(Name, Vsn, Path, Incls, false, no_fault).

read_application(Name, Vsn, [Dir|Path], Incls, Found, FirstError) ->
    case read_file(Name ++ ".app", [Dir]) of
	{ok, Term, FullName} ->
	    case parse_application(Term, FullName, Vsn, Incls) of
		{error, {no_valid_version, {Vsn, OtherVsn}}} when FirstError == no_fault ->
		    NFE = {no_valid_version, {{"should be", Vsn}, 
					       {"found file", filename:join(Dir, Name++".app"),
						OtherVsn}}},
		    read_application(Name, Vsn, Path, Incls, true, NFE);
		{error, {no_valid_version, {Vsn, _OtherVsn}}} ->
			    read_application(Name, Vsn, Path, Incls, true, FirstError);
		Res ->
		    Res
	    end;
	{error, {parse, _File, {Line, _Mod, Err}}} when FirstError == no_fault ->
	    read_application(Name, Vsn, Path, Incls, Found, 
			     {parse_error, {filename:join(Dir, Name++".app"), Line, Err}});
	{error, {parse, _File, _Err}} ->
	    read_application(Name, Vsn, Path, Incls, Found, FirstError);
	{error, _Err} -> %% Not found
	    read_application(Name, Vsn, Path, Incls, Found, FirstError)
    end;
read_application(Name, Vsn, [], _, true, no_fault) ->
    {error, {application_vsn, {Name,Vsn}}};
read_application(_Name, _Vsn, [], _, true, FirstError) ->
    {error, FirstError};
read_application(Name, _, [], _, _, no_fault) ->
    {error, {not_found, Name ++ ".app"}};
read_application(_Name, _, [], _, _, FirstError) ->
    {error, FirstError}.

parse_application({application, Name, Dict}, File, Vsn, Incls)
  when is_atom(Name),
       is_list(Dict) ->
    Items = [vsn,id,description,modules,registered,
	     applications,included_applications,mod,start_phases,env,maxT,maxP],
    case catch get_items(Items, Dict) of
	[Vsn,Id,Desc,Mods,Regs,Apps,Incs0,Mod,Phases,Env,MaxT,MaxP] ->
	    case override_include(Name, Incs0, Incls) of
		{ok, Incs} ->
		    {ok, #application{name=Name,
				      vsn=Vsn,
				      id=Id,
				      description=Desc,
				      modules=Mods,
				      uses=Apps,
				      includes=Incs,
				      regs=Regs,
				      mod=Mod,
				      start_phases=Phases,
				      env=Env,
				      maxT=MaxT,
				      maxP=MaxP,
				      dir=filename:dirname(File)}};
		{error, IncApps} ->
		    {error, {override_include, IncApps}}
	    end;
	[OtherVsn,_,_,_,_,_,_,_,_,_,_,_] ->
	    {error, {no_valid_version, {Vsn, OtherVsn}}};
	Err ->
	    {error, {Err, {application, Name, Dict}}}
    end;
parse_application(Other, _, _, _) ->
    {error, {badly_formatted_application, Other}}.

%% Test if all included applications specifed in the .rel file
%% exists in the {included_applications,Incs} specified in the
%% .app file.
override_include(Name, Incs, Incls) ->
    case keysearch(Name, 1, Incls) of
	{value, {Name, I}} ->
	    case specified(I, Incs) of
		[] ->
		    {ok, I};
		NotSpec ->
		    {error, NotSpec}
	    end;
	_ ->
	    {ok, Incs}
    end.

specified([App|Incls], Spec) ->
    case member(App, Spec) of
	true ->
	    specified(Incls, Spec);
	_ ->
	    [App|specified(Incls, Spec)]
    end;
specified([], _) ->
    [].

get_items([H|T], Dict) ->
    Item = check_item(keysearch(H, 1, Dict),H),
    [Item|get_items(T, Dict)];
get_items([], _Dict) ->
    [].

check_item({_,{mod,{M,A}}},_) when is_atom(M) ->
    {M,A};
check_item({_,{mod,[]}},_) -> % default mod is [], so accept as entry
    [];
check_item({_,{vsn,Vsn}},I) ->
    case string_p(Vsn) of
	true -> Vsn;
	_ -> throw({bad_param, I})
    end;
check_item({_,{id,Id}},I) ->
    case string_p(Id) of
	true -> Id;
	_ -> throw({bad_param, I})
    end;
check_item({_,{description,Desc}},I) ->
    case string_p(Desc) of
	true -> Desc;
	_ -> throw({bad_param, I})
    end;
check_item({_,{applications,Apps}},I) ->
    case a_list_p(Apps) of
	true -> Apps;
	_ -> throw({bad_param, I})
    end;
check_item({_,{included_applications,Apps}},I) ->
    case a_list_p(Apps) of
	true -> Apps;
	_ -> throw({bad_param, I})
    end;
check_item({_,{registered,Regs}},I) ->
    case a_list_p(Regs) of
	true -> Regs;
	_ -> throw({bad_param, I})
    end;
check_item({_,{modules,Mods}},I) ->
    case a_list_p(Mods) of
	true -> Mods;
	_ -> throw({bad_param, I})
    end;
check_item({_,{start_phases,undefined}},_) -> % default start_phase is undefined,
    undefined;                                % so accept as entry
check_item({_,{start_phases,Phase}},I) ->
    case t_list_p(Phase) of
	true -> Phase;
	_ -> throw({bad_param, I})
    end;
check_item({_,{env,Env}},I) ->
    case t_list_p(Env) of
	true -> Env;
	_ -> throw({bad_param, I})
    end;
check_item({_,{maxT,MaxT}},I) ->
    case MaxT of
	MaxT when is_integer(MaxT), MaxT > 0 -> MaxT;
	infinity -> infinity;
	_ -> throw({bad_param, I})
    end;
check_item({_,{maxP,MaxP}},I) ->
    case MaxP of
	MaxP when is_integer(MaxP), MaxP > 0 -> MaxP;
	infinity -> infinity;
	_ -> throw({bad_param, I})
    end;
check_item(false, included_applications) -> % optional !
    [];
check_item(false, mod) -> % mod is optional !
    [];
check_item(false, env) -> % env is optional !
    [];
check_item(false, id) -> % id is optional !
    [];
check_item(false, start_phases) -> % start_phases is optional !
    undefined;
check_item(false, maxT) -> % maxT is optional !
    infinity;
check_item(false, maxP) -> % maxP is optional !
    infinity;
check_item(_, Item) ->
    throw({missing_param, Item}).

%%______________________________________________________________________
%% check_applications([{{Name,Vsn},#application}]) ->
%%    ok | throw({error, Error})
%% check that all referenced applications exists and that no
%% application register processes with the same name.
%% Check that included_applications are not specified as used
%% in another application.

check_applications(Appls) ->
    undef_appls(Appls),
    dupl_regs(Appls),
    %% Make a list Incs = [{Name,App,AppVsn,Dir}]
    Incs = [{IncApp,App,Appv,A#application.dir} ||
	       {{App,Appv},A} <- Appls,
	       IncApp <- A#application.includes],
    dupl_incls(Incs),
    Res = add_top_apps_to_uses(Incs, Appls, []),
    {ok, Res}.



undef_appls(Appls) ->
    case undefined_applications(Appls) of
	[] ->
	    ok;
	L ->
	    throw({error, {undefined_applications, make_set(L)}})
    end.

dupl_regs(Appls) ->
    %% Make a list Regs = [{Name,App,AppVsn,Dir}]
    Regs = [{Name,App,Appv,A#application.dir} ||
	       {{App,Appv},A} <- Appls,
	       Name <- A#application.regs],
    case duplicates(Regs) of
	[] ->
	    ok;
	Dups ->
	    throw({error, {duplicate_register, Dups}})
    end.


dupl_incls(Incs) ->
    case duplicates(Incs) of
	[] ->
	    ok;
	Dups ->
	    throw({error, {duplicate_include, Dups}})
    end.



%% If an application uses another application which is included in yet
%% another application, e.g. X uses A, A is included in T; then the A
%% application in the X applications uses-variable is changed to the T
%% application's top application to ensure the start order. 
%% Exception: if both X and A have the same top, then it is not
%% added to avoid circular dependencies.
%%
%% add_top_apps_to_uses( list of all included applications in
%%                       the system,
%%                       list of all applications in the system,
%%                       temporary result)
%% -> new list of all applications
add_top_apps_to_uses(_InclApps, [], Res) ->
    %% InclApps = [{IncApp, App, AppVsn, Dir}]
    Res;
add_top_apps_to_uses(InclApps, [{Name,Appl} | Appls], Res) ->
    MyTop = find_top_app(Appl#application.name, InclApps),
    F = fun(UsedApp, AccIn) when UsedApp == MyTop ->
		%% UW980513 This is a special case: The included app
		%% uses its own top app. We'll allow it, but must
		%% remove the top app from the uses list.
		AccIn -- [MyTop];
	   (UsedApp, AccIn) ->
		case lists:keysearch(UsedApp, 1, InclApps) of
		    false ->
			AccIn;
		    {value, {_,DependApp,_,_}} ->
			UsedAppTop = find_top_app(DependApp, InclApps),
			case {lists:member(UsedAppTop, AccIn), MyTop} of
			    {true, _} ->
				%% the top app is already in the uses
				%% list, remove UsedApp
				AccIn -- [UsedApp];
			    {_, UsedAppTop} ->
				%% both are included in the same app
				AccIn;
			    _ ->
				%% change the used app to the used app's
				%% top application
				AccIn1 = AccIn -- [UsedApp],
				AccIn1 ++ [UsedAppTop]
			end
		end
	end,
	  
    NewUses = foldl(F, Appl#application.uses, Appl#application.uses),
    add_top_apps_to_uses(InclApps, Appls, 
			 Res++[{Name, Appl#application{uses = NewUses}}]).



find_top_app(App, InclApps) ->
    case lists:keysearch(App, 1, InclApps) of
	false ->
	    App;
	{value, {_,TopApp,_,_}} ->
	    find_top_app(TopApp, InclApps)
    end.

 

%%______________________________________________________________________
%% undefined_applications([{{Name,Vsn},#application}]) ->
%%   [Name] list of applications that were declared in
%%   use declarations but are not contained in the release descriptor

undefined_applications(Appls) ->
    Uses = append(map(fun({_,A}) ->
			      A#application.uses ++ A#application.includes
		      end, Appls)),
    Defined = map(fun({{X,_},_}) -> X end, Appls),
    filter(fun(X) -> not member(X, Defined) end, Uses).

%%______________________________________________________________________
%% sort_used_and_incl_appls(Applications, Release) -> Applications
%%   Applications = [{{Name,Vsn},#application}]
%%   Release = #release{}
%%    
%% OTP-4121, OTP-9984
%% Check that used and included applications are given in the same
%% order as in the release resource file (.rel). Otherwise load and
%% start instructions in the boot script, and consequently release
%% upgrade instructions in relup, may end up in the wrong order.

sort_used_and_incl_appls(Applications, Release) when is_tuple(Release) ->
    {ok,
     sort_used_and_incl_appls(Applications, Release#release.applications)};

sort_used_and_incl_appls([{Tuple,Appl}|Appls], OrderedAppls) ->
    Incls2 =
	case Appl#application.includes of
	    Incls when length(Incls)>1 ->
		sort_appl_list(Incls, OrderedAppls);
	    Incls ->
		Incls
	end,
    Uses2 =
	case Appl#application.uses of
	    Uses when length(Uses)>1 ->
		sort_appl_list(Uses, OrderedAppls);
	    Uses ->
		Uses
	end,
    Appl2 = Appl#application{includes=Incls2, uses=Uses2},
    [{Tuple,Appl2}|sort_used_and_incl_appls(Appls, OrderedAppls)];
sort_used_and_incl_appls([], _OrderedAppls) ->
    [].

sort_appl_list(List, Order) ->
    IndexedList = find_pos(List, Order),
    SortedIndexedList = lists:keysort(1, IndexedList),
    lists:map(fun({_Index,Name}) -> Name end, SortedIndexedList).

find_pos([Name|Incs], OrderedAppls) ->
    [find_pos(1, Name, OrderedAppls)|find_pos(Incs, OrderedAppls)];
find_pos([], _OrderedAppls) ->
    [].

find_pos(N, Name, [{Name,_Vsn,_Type}|_OrderedAppls]) ->
    {N, Name};
find_pos(N, Name, [_OtherAppl|OrderedAppls]) ->
    find_pos(N+1, Name, OrderedAppls).

%%______________________________________________________________________
%% check_modules(Appls, Path, TestP, Machine) ->
%%  {ok, Warnings} | throw({error, What})
%%   where Appls = [{App,Vsn}, #application}]
%%   performs logical checking that we can find all the modules
%%   etc.

check_modules(Appls, Path, TestP, Machine) ->
    %% first check that all the module names are unique
    %% Make a list M1 = [{Mod,App,Dir}]
    M1 = [{Mod,App,A#application.dir} ||
	     {{App,_Appv},A} <- Appls,
	     Mod <- A#application.modules],
    case duplicates(M1) of
	[] ->
	    case check_mods(M1, Appls, Path, TestP, Machine) of
		{error, Errors} ->
		    throw({error, {modules, Errors}});
		Return ->
		    Return
	    end;
	Dups ->
%	    io:format("** ERROR Duplicate modules: ~p\n", [Dups]),
	    throw({error, {duplicate_modules, Dups}})
    end.

%%______________________________________________________________________
%% Check that all modules exists.
%% Use the module extension of the running machine as extension for
%% the checked modules.

check_mods(Modules, Appls, Path, {SrcTestP, XrefP}, Machine) ->
    SrcTestRes = check_src(Modules, Appls, Path, SrcTestP, Machine),
    XrefRes = check_xref(Appls, Path, XrefP),
    Res = SrcTestRes ++ XrefRes,
    case filter(fun({error, _}) -> true;
		   (_)          -> false
		end,
		Res) of
	[] ->
	    {ok, filter(fun({warning, _}) -> true;
			   (_)            -> false
			end,
			Res)};
	Errors ->
	    {error, Errors}
    end.

check_src(Modules, Appls, Path, true, Machine) ->
    Ext = objfile_extension(Machine),
    IncPath = create_include_path(Appls, Path),
    append(map(fun(ModT) ->
		       {Mod,App,Dir} = ModT,
		       case check_mod(Mod,App,Dir,Ext,IncPath) of
			   ok ->
			       [];
			   {error, Error} ->
			       [{error,{Error, ModT}}];
			   {warning, Warn} ->
			       [{warning,{Warn,ModT}}]
		       end
	       end,
	       Modules));
check_src(_, _, _, _, _) ->
    [].

check_xref(_Appls, _Path, false) ->
    [];
check_xref(Appls, Path, XrefP) ->
    AppDirsL = [{App,A#application.dir} || {{App,_Appv},A} <- Appls],
    AppDirs0 = sofs:relation(AppDirsL),
    AppDirs = case XrefP of
		  true ->
		      AppDirs0;
		  {true, Apps} ->
		      sofs:restriction(AppDirs0, sofs:set(Apps))
	      end,
    XrefArgs = [{xref_mode, modules}],
    case catch xref:start(?XREF_SERVER, XrefArgs) of
	{ok, _Pid} ->
	    ok;
	{error, {already_started, _Pid}} ->
	    xref:stop(?XREF_SERVER), %% Clear out any previous data
	    {ok,_} = xref:start(?XREF_SERVER, XrefArgs),
	    ok
    end,
    {ok, _} = xref:set_default(?XREF_SERVER, verbose, false),
    LibPath = case Path == code:get_path() of
		  true -> code_path; % often faster
		  false -> Path
	      end,
    ok = xref:set_library_path(?XREF_SERVER, LibPath),
    check_xref(sofs:to_external(AppDirs)).

check_xref([{App,AppDir} | Appls]) ->
    case xref:add_application(?XREF_SERVER, AppDir, {name,App}) of
	{ok, _App} ->
	    check_xref(Appls);
	Error ->
	    xref:stop(?XREF_SERVER),
	    [{error, Error}]
    end;
check_xref([]) ->
    R = case xref:analyze(?XREF_SERVER, undefined_functions) of
	    {ok, []} ->
		[];
	    {ok, Undefined} -> 
		%% This clause is a (temporary?) fix for hipe.
		adjust_for_hipe(Undefined);
	    Error ->
		[{error, Error}]
	end,
    xref:stop(?XREF_SERVER),
    R.

adjust_for_hipe(Undef) ->
    case erlang:system_info(hipe_architecture) of
	undefined ->
	    U = lists:filter(fun ({hipe_bifs,_,_}) -> false;
				 ({hipe,_,_}) -> false;
				 (_) -> true
			     end, Undef),
	    if 
		[] == U ->
		    [];
		true ->
		    [{warning, {exref_undef, U}}]
	    end;
	_Arch -> 
	    %% Some BIFs are not always available on all versions of HiPE.
	    U = lists:filter(fun ({hipe_bifs,write_u64,2}) -> false;
				 (_) -> true
			     end, Undef),
	    [{warning, {exref_undef, U}}]
    end.

%% Perform cross reference checks between all modules specified
%% in .app files.
%%
xref_p(Flags) ->
    case member(exref, Flags) of
	true ->
	    exists_xref(true);
	_ ->
	    case get_flag(exref, Flags) of
		{exref, Appls} when is_list(Appls) ->
		    case a_list_p(Appls) of
			true -> exists_xref({true, Appls});
			_    -> false
		    end;
		_ ->
		    false
	    end
    end.

exists_xref(Flag) ->
    case code:ensure_loaded(xref) of
	{error, _} -> false;
	_          -> Flag
    end.

objfile_extension(false) ->
    code:objfile_extension();
objfile_extension(Machine) ->
    "." ++ atom_to_list(Machine).

check_mod(Mod,App,Dir,Ext,IncPath) ->
    ObjFile = mod_to_filename(Dir, Mod, Ext),
    case file:read_file_info(ObjFile) of
	{ok,FileInfo} ->
	    LastModTime = FileInfo#file_info.mtime,
	    check_module(Mod, Dir, LastModTime, IncPath);
	_ ->
	    {error, {module_not_found, App, Mod}}
    end.

mod_to_filename(Dir, Mod, Ext) ->
    filename:join(Dir, atom_to_list(Mod) ++ Ext).

check_module(Mod, Dir, ObjModTime, IncPath) ->
    {SrcDirs,_IncDirs}= smart_guess(Dir,IncPath),
    case locate_src(Mod,SrcDirs) of
	{ok,_FDir,_File,LastModTime} ->
	    if
		LastModTime > ObjModTime ->
		    {warning, obj_out_of_date};
		true ->
		    ok
	    end;
	_ ->
	    {warning, source_not_found}
    end.

locate_src(Mod,[Dir|Dirs]) ->
    File = mod_to_filename(Dir, Mod, ".erl"),
    case file:read_file_info(File) of
	{ok,FileInfo} ->
	    LastModTime = FileInfo#file_info.mtime,
	    {ok,Dir,File,LastModTime};
	_ ->
	    locate_src(Mod,Dirs)
    end;
locate_src(_,[]) ->
    false.


%%______________________________________________________________________
%% smart_guess(Mod, Dir,IncludePath) -> {[Dirs],[IncDirs]}
%% Guess the src code and include directory. If dir contains .../ebin
%% src-dir should be one of .../src or .../src/e_src
%% If dir does not contain .../ebin set dir to the same directory.

smart_guess(Dir,IncPath) ->
    case reverse(filename:split(Dir)) of
	["ebin"|D] ->
	    D1 = reverse(D),
	    Dirs = [filename:join(D1 ++ ["src"]),
		    filename:join(D1 ++ ["src", "e_src"])],
	    {Dirs,Dirs ++ IncPath};
	_ ->
	    {[Dir],[Dir] ++ IncPath}
    end.

%%______________________________________________________________________
%% generate_script(#release, 
%%                 [{{Name,Vsn},#application}], Flags) ->
%%                         ok | {error, Error}
%%    Writes a script (a la magnus) to the file File.script
%%    and a bootfile to File.boot.

generate_script(Output, Release, Appls, Flags) ->
    PathFlag = path_flag(Flags),
    Variables = get_variables(Flags),
    Preloaded = preloaded(),
    Mandatory = mandatory_modules(),
    Script = {script, {Release#release.name,Release#release.vsn},
	      [{preLoaded, Preloaded},
	       {progress, preloaded},
	       {path, create_mandatory_path(Appls, PathFlag, Variables)},
	       {primLoad, Mandatory},
	       {kernel_load_completed},
	       {progress, kernel_load_completed}] ++
	      load_appl_mods(Appls, Mandatory ++ Preloaded,
			     PathFlag, Variables) ++
	      [{path, create_path(Appls, PathFlag, Variables)}] ++
		  create_kernel_procs(Appls) ++
		  create_load_appls(Appls) ++
		  create_start_appls(Appls) ++
		  script_end(lists:member(no_dot_erlang, Flags))
	     },

    ScriptFile = Output ++ ".script",
    case file:open(ScriptFile, [write,{encoding,utf8}]) of
	{ok, Fd} ->
	    io:format(Fd, "%% ~s\n%% script generated at ~w ~w\n~tp.\n",
		      [epp:encoding_to_string(utf8), date(), time(), Script]),
	    case file:close(Fd) of
		ok ->
		    BootFile = Output ++ ".boot",
		    case file:write_file(BootFile, term_to_binary(Script)) of
			ok ->
			    ok;
			{error, Reason} ->
			    {error, ?MODULE, {open,BootFile,Reason}}
		    end;
		{error, Reason} ->
		    {error, ?MODULE, {close,ScriptFile,Reason}}
	    end;
	{error, Reason} ->
	    {error, ?MODULE, {open,ScriptFile,Reason}}
    end.

path_flag(Flags) ->
    case {member(local,Flags), member(otp_build, Flags)} of
	{true, _} -> local;
	{_, true} -> otp_build;
	{_, _}    -> true
    end.

get_variables(Flags) ->
    case get_flag(variables, Flags) of
	{variables, Variables} when is_list(Variables) ->
	    valid_variables(Variables);
	_ ->
	    []
    end.

valid_variables([{Var,Path}|Variables]) when is_list(Var), is_list(Path) ->
    [{Var,rm_tlsl(Path)}|valid_variables(Variables)];
valid_variables([{Var,Path}|Variables]) when is_atom(Var), is_list(Path) ->
    [{to_list(Var),rm_tlsl(Path)}|valid_variables(Variables)];
valid_variables([_|Variables]) ->
    valid_variables(Variables);
valid_variables(_) ->
    [].

rm_tlsl(P) -> rm_tlsl1(reverse(P)).
rm_tlsl1([$/|P]) -> rm_tlsl1(P);
rm_tlsl1(P) -> reverse(P).
  
%%______________________________________________________________________
%% Start all applications.
%% Do not start applications that are included applications !

create_start_appls(Appls) ->
    Included = append(map(fun({_,A}) ->
				  A#application.includes
		      end, Appls)),
    create_start_appls(Appls, Included).

create_start_appls([{_,A}|T], Incl) ->
    App = A#application.name,
    case lists:member(App, Incl) of
	false when A#application.type == none ->
	    create_start_appls(T, Incl);
	false when A#application.type == load ->
	    create_start_appls(T, Incl);
	false ->
	    [{apply, {application, start_boot, [App,A#application.type]}} |
	     create_start_appls(T, Incl)];
	_ ->
	    create_start_appls(T, Incl)
    end;
create_start_appls([], _) ->
    [].

%%______________________________________________________________________
%% Load all applications.

create_load_appls([{{kernel,_},_}|T]) -> %Already added !!
    create_load_appls(T);
create_load_appls([{_,A}|T]) when A#application.type == none ->
    create_load_appls(T);
create_load_appls([{_,A}|T]) ->
    [{apply, {application, load, [pack_app(A)]}} |
     create_load_appls(T)];
create_load_appls([]) ->
    [{progress, applications_loaded}].

%%______________________________________________________________________
%% The final part of the script.

script_end(false) ->  %% Do not skip loading of $HOME/.erlang
    [{apply, {c, erlangrc, []}},
     {progress, started}];
script_end(true) ->   %% Ignore loading of $HOME/.erlang
    [{progress, started}].


%%-----------------------------------------------------------------
%% Function: sort_appls(Appls) -> {ok, Appls'} | throw({error, Error})
%% Types: Appls = {{Name, Vsn}, #application}]
%% Purpose: Sort applications according to dependencies among
%%          applications.  If order doesn't matter, use the same
%%          order as in the original list.
%% Alg. written by Ulf Wiger 970917 (etxuwig@etxb.ericsson.se)
%% Mod. by mbj
%%-----------------------------------------------------------------
sort_appls(Appls) -> {ok, sort_appls(Appls, [], [], [])}.

sort_appls([{N, A}|T], Missing, Circular, Visited) ->
    {Name,_Vsn} = N,
    {Uses, T1, NotFnd1} = find_all(Name, lists:reverse(A#application.uses),
				   T, Visited, [], []),
    {Incs, T2, NotFnd2} = find_all(Name, lists:reverse(A#application.includes),
				   T1, Visited, [], []),
    Missing1 = NotFnd1 ++ NotFnd2 ++ Missing,
    case Uses ++ Incs of
	[] -> 
	    %% No more app that must be started before this one is
	    %% found; they are all already taken care of (and present
	    %% in Visited list)
	    [{N, A}|sort_appls(T, Missing1, Circular, [N|Visited])];
	L ->
	    %% The apps in L must be started before the app.
	    %% Check if we have already taken care of some app in L,
	    %% in that case we have a circular dependency.
	    NewCircular = [N1 || {N1, _} <- L, N2 <- Visited, N1 == N2],
	    Circular1 = case NewCircular of 
			    [] -> Circular; 
			    _ -> [N | NewCircular] ++ Circular
			end,
	    %% L must be started before N, try again, with all apps
	    %% in L added before N.
	    Apps = del_apps(NewCircular, L ++ [{N, A}|T2]),
	    sort_appls(Apps, Missing1, Circular1, [N|Visited])
    end;
sort_appls([], [], [], _) ->
    [];
sort_appls([], Missing, [], _) ->
    %% this has already been checked before, but as we have the info...
    throw({error, {undefined_applications, make_set(Missing)}});
sort_appls([], [], Circular, _) ->
    throw({error, {circular_dependencies, make_set(Circular)}});
sort_appls([], Missing, Circular, _) ->
    throw({error, {apps, [{circular_dependencies, make_set(Circular)}, 
			  {undefined_applications, make_set(Missing)}]}}).

find_all(CheckingApp, [Name|T], L, Visited, Found, NotFound) ->
    case find_app(Name, L) of
	{value, App} ->
	    {_A,R} = App,
	    %% It is OK to have a dependecy like
	    %% X includes Y, Y uses X.
	    case lists:member(CheckingApp, R#application.includes) of
		true ->
		    case lists:keymember(Name, 1, Visited) of
			true ->
			    find_all(CheckingApp, T, L, Visited, Found, NotFound);
			false ->
			    find_all(CheckingApp, T, L, Visited, Found, [Name|NotFound])
		    end;
		false ->
		    find_all(CheckingApp, T, L -- [App], Visited, [App|Found], NotFound)
	    end;
	false ->
	    case lists:keymember(Name, 1, Visited) of
		true ->
		    find_all(CheckingApp, T, L, Visited, Found, NotFound);
		false ->
		    find_all(CheckingApp, T, L, Visited, Found, [Name|NotFound])
	    end
    end;
find_all(_CheckingApp, [], L, _Visited, Found, NotFound) ->
    {Found, L, NotFound}.
	    
find_app(Name, [{{Name,Vsn}, Application}|_]) ->
    {value, {{Name,Vsn},Application}};
find_app(Name, [_|T]) ->
    find_app(Name, T);
find_app(_Name, []) ->
    false.

del_apps([Name|T], L) ->
    del_apps(T, lists:keydelete(Name, 1, L));
del_apps([], L) ->
    L.


%%______________________________________________________________________
%% Create the load path used in the generated script.
%% If PathFlag is true a script intended to be used as a complete
%% system (e.g. in an embbeded system), i.e. all applications are
%% located under $ROOT/lib.
%% Otherwise all paths are set according to dir per application.

%% Create the complete path.
create_path(Appls, PathFlag, Variables) ->
    make_set(map(fun({{Name,Vsn},App}) ->
			 cr_path(Name, Vsn, App, PathFlag, Variables)
		 end,
		 Appls)).

%% Create the path to a specific application.
%% (The otp_build flag is only used for OTP internal system make)
cr_path(Name, Vsn, _, true, []) ->
    filename:join(["$ROOT", "lib", to_list(Name) ++ "-" ++ Vsn, "ebin"]);
cr_path(Name, Vsn, App, true, Variables) ->
    Dir = App#application.dir,
    N = to_list(Name),
    Tail = [N ++ "-" ++ Vsn, "ebin"],
    case variable_dir(Dir, N, Vsn, Variables) of
	{ok, VarDir} ->
	    filename:join([VarDir] ++ Tail);
	_ ->
	    filename:join(["$ROOT", "lib"] ++ Tail)
    end;
cr_path(Name, _, _, otp_build, _) ->
    filename:join(["$ROOT", "lib", to_list(Name), "ebin"]);
cr_path(_, _, App, _, _) ->
    filename:absname(App#application.dir).

variable_dir(Dir, Name, Vsn, [{Var,Path}|Variables]) ->
    case lists:prefix(Path,Dir) of
	true ->
	    D0 = strip_prefix(Path, Dir),
	    case strip_name_ebin(D0, Name, Vsn) of
		{ok, D} ->
		    {ok, filename:join(["\$" ++ Var] ++ D)};
		_ ->
		    %% We know at least that we are located
		    %% under the variable dir.
		    {ok, filename:join(["\$" ++ Var] ++ D0)}
	    end;
	_ ->
	    variable_dir(Dir, Name, Vsn, Variables)
    end;
variable_dir(_Dir, _, _, []) ->
    false.

strip_prefix(Path, Dir) ->
    L = length(filename:split(Path)),
    lists:nthtail(L, filename:split(Dir)).

strip_name_ebin(Dir, Name, Vsn) ->
    FullName = Name ++ "-" ++ Vsn,
    case reverse(Dir) of
	["ebin",Name|D]     -> {ok, reverse(D)};
	["ebin",FullName|D] -> {ok, reverse(D)};
	_                   -> false
    end.

%% Create the path to the kernel and stdlib applications.
create_mandatory_path(Appls, PathFlag, Variables) ->
    Dirs = [kernel, stdlib],
    make_set(map(fun({{Name,Vsn}, A}) ->
			 case lists:member(Name, Dirs) of
			     true ->
				 cr_path(Name, Vsn, A, PathFlag, Variables);
			     _ ->
				 ""
			 end
		 end,
		 Appls)).

%%______________________________________________________________________
%% Load all modules, except those in Mandatory_modules.

load_appl_mods([{{Name,Vsn},A}|Appls], Mand, PathFlag, Variables) ->
    Mods = A#application.modules,
    load_commands(filter(fun(Mod) -> not member(Mod, Mand) end, Mods),
		  cr_path(Name, Vsn, A, PathFlag, Variables)) ++
	load_appl_mods(Appls, Mand, PathFlag, Variables);
%    [{path, [cr_path(Name, Vsn, A, PathFlag, Variables)]},
%     {primLoad, filter(fun(Mod) -> not member(Mod, Mand) end, Mods)} |
%     load_appl_mods(Appls, Mand, PathFlag, Variables)];
load_appl_mods([], _, _, _) ->
    [{progress, modules_loaded}].

load_commands(Mods, Path) ->
    [{path, [filename:join([Path])]},
     {primLoad,lists:sort(Mods)}].

%%______________________________________________________________________
%% Pack an application to an application term.

pack_app(#application{name=Name,vsn=V,id=Id,description=D,modules=M,
		      uses=App,includes=Incs,regs=Regs,mod=Mod,start_phases=SF,
		      env=Env,maxT=MaxT,maxP=MaxP}) ->
    {application, Name,
     [{description,D},
      {vsn,V},
      {id,Id},
      {modules, M},
      {registered, Regs},
      {applications, App},
      {included_applications, Incs},
      {env, Env},
      {maxT, MaxT},
      {maxP, MaxP} |
      behave([{start_phases,SF},{mod,Mod}])]}.

behave([{mod,[]}|T]) ->
    behave(T);
behave([{start_phases,undefined}|T]) ->
    behave(T);
behave([H|T]) ->
    [H|behave(T)];
behave([]) ->
    [].

mandatory_modules() ->
    [error_handler,				%Truly mandatory.

     %% Modules that are almost always needed. Listing them here
     %% helps the init module to load them faster. Modules not
     %% listed here will be loaded by the error_handler module.
     %%
     %% Keep this list sorted.
     application,
     application_controller,
     application_master,
     code,
     code_server,
     erl_eval,
     erl_lint,
     erl_parse,
     error_logger,
     ets,
     file,
     filename,
     file_server,
     file_io_server,
     gen,
     gen_event,
     gen_server,
     heart,
     kernel,
     lists,
     proc_lib,
     supervisor
    ].

%%______________________________________________________________________
%% This is the modules that are preloaded into the Erlang system.

preloaded() ->
    %% Sorted
    [erl_prim_loader,erl_tracer,erlang,
     erts_code_purger,erts_dirty_process_code_checker,
     erts_internal,erts_literal_area_collector,
     init,otp_ring0,prim_eval,prim_file,
     prim_inet,prim_zip,zlib].

%%______________________________________________________________________
%% Kernel processes; processes that are specially treated by the init
%% process. If a kernel process terminates the whole system terminates.
%% kernel_processes() -> [{Name, Mod, Func, Args}]
%% where Args is a term or a fun taking the list of applications as arg.

kernel_processes() ->
    [{heart, heart, start, []},
     {error_logger, error_logger, start_link, []},
     {application_controller, application_controller, start,
      fun(Appls) ->
              [{_,App}] = filter(fun({{kernel,_},_App}) -> true;
                                    (_)                 -> false
                                 end,
                                 Appls),
              [pack_app(App)]
      end}
    ].

%%______________________________________________________________________
%% Create the kernel processes.

create_kernel_procs(Appls) ->
    map(fun({Name,Mod,Func,Args}) when is_function(Args) ->
                {kernelProcess, Name, {Mod, Func, Args(Appls)}};
           ({Name,Mod,Func,Args}) ->
                {kernelProcess, Name, {Mod, Func, Args}}
        end,
        kernel_processes()) ++
    [{progress, init_kernel_started}].

%%______________________________________________________________________
%% Make a tar file of the release.
%% The tar file contains:
%%         lib/App-Vsn/ebin
%%                    /priv
%%                   [/src]
%%                   [/include]
%%                   [/doc]
%%                   [/examples]
%%                   [/...]
%%         Variable1.tar.gz
%%         ...
%%         VariableN.tar.gz
%%         releases/RelName.rel
%%                  RelVsn/start.boot
%%                         relup
%%                         sys.config
%%         erts-EVsn[/bin]
%%
%% The VariableN.tar.gz files can also be stored as own files not
%% included in the main tar file or they can be omitted using
%% the var_tar option.

mk_tar(RelName, Release, Appls, Flags, Path1) ->
    TarName = case get_outdir(Flags) of
		  "" ->
		      RelName ++ ".tar.gz";
		  OutDir ->
		      filename:join(OutDir, filename:basename(RelName))
			  ++ ".tar.gz"
	      end,
    Tar = open_main_tar(TarName),
    case catch mk_tar(Tar, RelName, Release, Appls, Flags, Path1) of
	{error,Error} ->
	    _ = del_tar(Tar, TarName),
	    {error,?MODULE,Error};
	{'EXIT',Reason} ->
	    _ = del_tar(Tar, TarName),
	    {error,?MODULE,Reason};
	_ ->
	    case erl_tar:close(Tar) of
		ok -> ok;
		{error,Reason} -> {error,?MODULE,{close,TarName,Reason}}
	    end
    end.

open_main_tar(TarName) ->
    case catch open_tar(TarName) of
	{error, Error} ->
	    throw({error,?MODULE,Error});
	Tar ->
	    Tar
    end.

mk_tar(Tar, RelName, Release, Appls, Flags, Path1) ->
    Variables = get_variables(Flags),
    add_applications(Appls, Tar, Variables, Flags, false),
    add_variable_tars(Variables, Appls, Tar, Flags),
    add_system_files(Tar, RelName, Release, Path1),
    add_erts_bin(Tar, Release, Flags).
    
add_applications(Appls, Tar, Variables, Flags, Var) ->
    Res = foldl(fun({{Name,Vsn},App}, Errs) ->
		  case catch add_appl(to_list(Name), Vsn, App,
				      Tar, Variables, Flags, Var) of
		      ok ->
			  Errs;
		      {error, What} ->
			  [{error_add_appl, {Name,What}}|Errs]
		  end
	       end, [], Appls),
    case Res of
	[] ->
	    ok;
	Errors ->
	    throw({error, Errors})
    end.

%%______________________________________________________________________
%% Create a tar file for each Variable directory.
%% Deletes the temporary tar file.

add_variable_tars([Variable|Variables], Appls, Tar, Flags) ->
    add_variable_tar(Variable, Appls, Tar, Flags),
    add_variable_tars(Variables, Appls, Tar, Flags);
add_variable_tars([], _, _, _) ->
    ok.

add_variable_tar({Variable,P}, Appls, Tar, Flags) ->    
    case var_tar_flag(Flags) of
	omit ->
	    ok;
	Flag ->
	    TarName = Variable ++ ".tar.gz",
	    VarTar = open_tar(TarName),
	    case catch add_applications(Appls, VarTar, [{Variable,P}],
					Flags, Variable) of
		ok when Flag == include ->
		    close_tar(VarTar,TarName),
		    add_to_tar(Tar, TarName, TarName),
		    del_file(TarName);
		ok when Flag == ownfile ->
		    close_tar(VarTar,TarName);
		Error ->
		    _ = del_tar(VarTar, TarName),
		    throw(Error)
	    end
    end.

var_tar_flag(Flags) ->
    case get_flag(var_tar, Flags) of
	{var_tar, Flag} ->
	    case member(Flag, [include, ownfile, omit]) of
		true -> Flag;
		_    -> include
	    end;
	_ ->
	    include
    end.

%%______________________________________________________________________
%% Add all "other" files to Dir/releases/Svsn
%% add_system_files(Tar,Name,release#,Flags) ->
%%   ok | throw({error,Error})

add_system_files(Tar, RelName, Release, Path1) ->
    SVsn = Release#release.vsn,
    RelName0 = filename:basename(RelName),

    RelVsnDir = filename:join("releases", SVsn),

    %% OTP-9746: store rel file in releases/<vsn>
    %% Adding rel file to
    %% 1) releases directory - so it can be easily extracted
    %%    separately (see release_handler:unpack_release)
    %% 2) releases/<vsn> - so the file must not be explicitly moved
    %%    after unpack.
    add_to_tar(Tar, RelName ++ ".rel",
	       filename:join("releases", RelName0 ++ ".rel")),
    add_to_tar(Tar, RelName ++ ".rel",
	       filename:join(RelVsnDir, RelName0 ++ ".rel")),


    %% OTP-6226 Look for the system files not only in cwd
    %% --
    %% (well, actually the boot file was looked for in the same
    %% directory as RelName, which is not necessarily the same as cwd)
    %% --
    %% but also in the path specfied as an option to systools:make_tar
    %% (but make sure to search the RelName directory and cwd first)
    Path = case filename:dirname(RelName) of
	       "." ->
		   ["."|Path1];
	       RelDir ->
		   [RelDir, "."|Path1]
	   end,

    case lookup_file(RelName0 ++ ".boot", Path) of
	false ->
	    throw({error, {tar_error,{add, RelName0++".boot",enoent}}});
	Boot ->
	    add_to_tar(Tar, Boot, filename:join(RelVsnDir, "start.boot"))
    end,

    case lookup_file("relup", Path) of
	false ->
	    ignore;
	Relup ->
	    check_relup(Relup),
	    add_to_tar(Tar, Relup, filename:join(RelVsnDir, "relup"))
    end,

    case lookup_file("sys.config", Path) of
	false ->
	    ignore;
	Sys ->
	    check_sys_config(Sys),
	    add_to_tar(Tar, Sys, filename:join(RelVsnDir, "sys.config"))
    end,
    
    ok.

lookup_file(Name, [Dir|Path]) ->
    File = filename:join(Dir, Name),
    case filelib:is_file(File) of
	true ->
	    File;
	false ->
	    lookup_file(Name, Path)
    end;
lookup_file(_Name, []) ->
    false.

%% Check that relup can be parsed and has expected format
check_relup(File) ->
    case file:consult(File) of
	{ok,[{Vsn,UpFrom,DownTo}]} when is_list(Vsn), is_integer(hd(Vsn)),
					is_list(UpFrom), is_list(DownTo) ->
	    ok;
	{ok,_} ->
	    throw({error,{tar_error,{add,"relup",[invalid_format]}}});
	Other ->
	    throw({error,{tar_error,{add,"relup",[Other]}}})
    end.

%% Check that sys.config can be parsed and has expected format
check_sys_config(File) ->
    case file:consult(File) of
	{ok,[SysConfig]} ->
	    case lists:all(fun({App,KeyVals}) when is_atom(App),
						   is_list(KeyVals)->
				   true;
			      (OtherConfig) when is_list(OtherConfig),
						 is_integer(hd(OtherConfig)) ->
				   true;
			      (_) ->
				   false
			   end,
			   SysConfig) of
		true ->
		    ok;
		false ->
		    throw({error,{tar_error,
				  {add,"sys.config",[invalid_format]}}})
	    end;
	{ok,_} ->
	    throw({error,{tar_error,{add,"sys.config",[invalid_format]}}});
	Other ->
	    throw({error,{tar_error,{add,"sys.config",[Other]}}})
    end.

%%______________________________________________________________________
%% Add either a application located under a variable dir or all other
%% applications to a tar file.
%% add_appl(Name,Vsn,application#,Tar,Variables,Flags,Var) ->
%%    ok | {error,Error}

add_appl(Name, Vsn, App, Tar, Variables, Flags, Var) ->
    AppDir = App#application.dir,
    case add_to(AppDir,Name,Vsn,Variables,Var) of
	false ->
	    ok;
	{ok, ToDir} ->
	    ADir = appDir(AppDir),
	    add_priv(ADir, ToDir, Tar),
	    case get_flag(dirs,Flags) of
		{dirs,Dirs} ->
		    add_dirs(ADir, Dirs, ToDir, Tar);
		_ ->
		    ok
	    end,
	    BinDir = filename:join(ToDir, "ebin"),
	    add_to_tar(Tar,
		       filename:join(AppDir, Name ++ ".app"),
		       filename:join(BinDir, Name ++ ".app")),
	    add_modules(map(fun(Mod) -> to_list(Mod) end,
			    App#application.modules),
			Tar,
			AppDir,
			BinDir,
			objfile_extension(machine(Flags)))
    end.

%%______________________________________________________________________
%% If an application directory contains a Variable (in AppDir) the
%% application will be placed in the tar file (if it is this Variable
%% we corrently is actually storing).

add_to(AppDir,Name,Vsn,Variables,Variable) ->
    case var_dir(AppDir,Name,Vsn,Variables) of
	{ok, Variable, RestPath} ->
	    {ok, filename:join(RestPath ++ [Name ++ "-" ++ Vsn])};
	{ok, _, _} ->
	    false;
	_ when Variable == false ->
	    {ok, filename:join("lib", Name ++ "-" ++ Vsn)};
	_ ->
	    false
    end.

var_dir(Dir, Name, Vsn, [{Var,Path}|Variables]) ->
    case lists:prefix(Path,Dir) of
	true ->
	    D0 = strip_prefix(Path, Dir),
	    case strip_name_ebin(D0, Name, Vsn) of
		{ok, D} ->
		    {ok, Var, D};
		_ ->
		    false
	    end;
	_ ->
	    var_dir(Dir, Name, Vsn, Variables)
    end;
var_dir(_Dir, _, _, []) ->
    false.

appDir(AppDir) ->
    case filename:basename(AppDir) of
	"ebin" -> filename:dirname(AppDir);
	_ -> AppDir
    end.

add_modules(Modules, Tar, AppDir, ToDir, Ext) ->
    foreach(fun(Mod) ->
		    add_to_tar(Tar,
			       filename:join(AppDir, Mod ++ Ext),
			       filename:join(ToDir, Mod ++ Ext))
	    end, Modules).
		    
%%
%% Add own specified directories to include in the release.
%% If not found, skip it.
%%
add_dirs(AppDir, Dirs, ToDir, Tar) ->
    foreach(fun(Dir) -> catch add_dir(AppDir, to_list(Dir), ToDir, Tar) end,
	    Dirs).

add_dir(TopDir, Dir, ToDir, Tar) ->
    FromD = filename:join(TopDir, Dir),
    case dirp(FromD) of
	true ->
	    add_to_tar(Tar, FromD, filename:join(ToDir, Dir));
	_ ->
	    ok
    end.

%%
%% Add the priv dir if it exists.

add_priv(ADir, ToDir, Tar) ->
    Priv = filename:join(ADir, "priv"),
    case dirp(Priv) of
	true ->
	    add_to_tar(Tar, Priv, filename:join(ToDir, "priv"));
	_ ->
	    ok
    end.

add_erts_bin(Tar, Release, Flags) ->
    case get_flag(erts,Flags) of
	{erts,ErtsDir} ->
	    EVsn = Release#release.erts_vsn,
	    FromDir = filename:join([to_list(ErtsDir),
				     "erts-" ++ EVsn, "bin"]),
	    dirp(FromDir),
	    ToDir = filename:join("erts-" ++ EVsn, "bin"),
	    add_to_tar(Tar, FromDir, ToDir);
	_ ->
	    ok
    end.

%%______________________________________________________________________
%% Tar functions.

open_tar(TarName) ->
    case erl_tar:open(TarName, [write, compressed]) of
	{ok, Tar} ->
	    Tar;
	{error, Error} ->
	    throw({error,{tar_error, {open, TarName, Error}}})
    end.

close_tar(Tar,File) ->
    case erl_tar:close(Tar) of
	ok -> ok;
	{error,Reason} -> throw({error,{close,File,Reason}})
    end.

del_tar(Tar, TarName) ->
    _ = erl_tar:close(Tar),
    file:delete(TarName).

add_to_tar(Tar, FromFile, ToFile) ->
    case catch erl_tar:add(Tar, FromFile, ToFile, [compressed, dereference]) of
	ok -> ok;
        {'EXIT', Reason} ->
            throw({error, {tar_error, {add, FromFile, Reason}}});
	{error, Error} ->
	    throw({error, {tar_error, {add, FromFile, Error}}})
    end.

%%______________________________________________________________________
%%______________________________________________________________________
%% utilities!

make_set([]) -> [];
make_set([""|T]) -> % Ignore empty items.
    make_set(T);
make_set([H|T]) ->
    [H | [ Y || Y<- make_set(T),
		Y =/= H]].

to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(L)                 -> L.

mk_path(Path0) ->
    Path1 = map(fun(Dir) when is_atom(Dir) -> atom_to_list(Dir);
		   (Dir)                   -> Dir
		end, Path0),
    systools_lib:get_path(Path1).

%% duplicates([Tuple]) -> List of pairs where 
%%    element(1, T1) == element(1, T2) and  where T1 and T2 are 
%%    taken from [Tuple]

duplicates(X) -> duplicates(keysort(1,X), []).

duplicates([H1,H2|T], L) -> 
    case {element(1,H1),element(1,H2)} of
	{X,X} -> duplicates([H2|T],[{H1,H2}|L]);
        _     -> duplicates([H2|T],L)
    end;
duplicates(_, L) -> L.

%% read_file(File, Path) -> {ok, Term, FullName} | {error, Error}
%% read a file and check the syntax, i.e. that it contains a correct
%% Erlang term.

read_file(File, Path) ->
    case file:path_open(Path, File, [read]) of
	{ok, Stream, FullName} ->
	    Return = case systools_lib:read_term_from_stream(Stream, File) of
			 {ok, Term} ->
			     {ok, Term, FullName};
			 Other ->
			     Other
		     end,
	    case file:close(Stream) of
		ok -> Return;
		{error, Error} -> {error, {close,File,Error}}
	    end;
	_Other ->
	    {error, {not_found, File}}
    end.

del_file(File) ->
    case file:delete(File) of
	ok -> ok;
	{error, Error} ->
	    throw({error, {delete, File, Error}})
    end.

dirp(Dir) ->
    case file:read_file_info(Dir) of
	{ok, FileInfo} -> FileInfo#file_info.type == directory;
	_ ->              false
    end.

%% Create the include path. Assumptions about the code path is done
%% and an include directory is added.
%% Add the official include dir for each found application first in
%% path !!
%% If .../ebin exists in a path an .../include directory is assumed to
%% exist at the same level. If .../ebin is not existing the .../include
%% directory is assumed anyhow.
%% Local includes are added for each application later on.

create_include_path(Appls, Path) ->
    FoundAppDirs = map(fun({_,A}) -> A#application.dir end, Appls),
    map(fun(Dir) ->
		case reverse(filename:split(Dir)) of
		    ["ebin"|D] ->
			filename:join(reverse(D) ++ ["include"]);
		    _ ->
			filename:join(Dir, "include")
		end
	end,
	FoundAppDirs ++ no_dupl(Path, FoundAppDirs)).

no_dupl([Dir|Path], FoundAppDirs) ->
    case member(Dir, FoundAppDirs) of
	true ->
	    no_dupl(Path, FoundAppDirs);
	_ ->
	    [Dir|no_dupl(Path, FoundAppDirs)]
    end;
no_dupl([], _) ->
    [].

is_app_type(permanent) -> true;
is_app_type(transient) -> true;
is_app_type(temporary) -> true;
is_app_type(none) -> true;
is_app_type(load) -> true;
is_app_type(_) -> false.

% check if a term is a string.

string_p(S) ->
    case unicode:characters_to_list(S) of
	S -> true;
	_ -> false
    end.

% check if a term is a list of two tuples with the first
% element as an atom.

t_list_p([{A,_}|T]) when is_atom(A) -> t_list_p(T);
t_list_p([])                        -> true;
t_list_p(_)                         -> false.

% check if a term is a list of atoms.

a_list_p([A|T]) when is_atom(A) -> a_list_p(T);
a_list_p([])                    -> true;
a_list_p(_)                     -> false.

%% Get a key-value tuple flag from a list.

get_flag(F,[{F,D}|_]) -> {F,D};
get_flag(F,[_|Fs])    -> get_flag(F,Fs);
get_flag(_,_)         -> false.

%% Check Options for make_script
check_args_script(Args) ->
    cas(Args, []).

cas([], X) ->
    X;
%%% path ---------------------------------------------------------------
cas([{path, P} | Args], X) when is_list(P) ->
    case check_path(P) of
	ok ->
	    cas(Args, X);
	error ->
	    cas(Args, X++[{path,P}])
    end;
%%% silent -------------------------------------------------------------
cas([silent | Args], X) ->
    cas(Args, X);
%%% local --------------------------------------------------------------
cas([local | Args], X) ->
    cas(Args, X);
%%% src_tests -------------------------------------------------------
cas([src_tests | Args], X) ->
    cas(Args, X);
%%% variables ----------------------------------------------------------
cas([{variables, V} | Args], X) when is_list(V) ->
    case check_vars(V) of
	ok ->
	    cas(Args, X);
	error ->
	    cas(Args, X++[{variables, V}])
    end;
%%% machine ------------------------------------------------------------
cas([{machine, M} | Args], X) when is_atom(M) ->
    cas(Args, X);
%%% exref --------------------------------------------------------------
cas([exref | Args], X)  ->
    cas(Args, X);
%%% exref Apps ---------------------------------------------------------
cas([{exref, Apps} | Args], X) when is_list(Apps) ->
    case check_apps(Apps) of 
	ok ->
	    cas(Args, X);
	error ->
	    cas(Args, X++[{exref, Apps}])
    end;
%%% outdir Dir ---------------------------------------------------------
cas([{outdir, Dir} | Args], X) when is_list(Dir) ->
    cas(Args, X);
%%% otp_build (secret, not documented) ---------------------------------
cas([otp_build | Args], X) ->
    cas(Args, X);
%%% warnings_as_errors -------------------------------------------------
cas([warnings_as_errors | Args], X) ->
    cas(Args, X);
%%% no_warn_sasl -------------------------------------------------------
cas([no_warn_sasl | Args], X) ->
    cas(Args, X);
%%% no_module_tests (kept for backwards compatibility, but ignored) ----
cas([no_module_tests | Args], X) ->
    cas(Args, X);
cas([no_dot_erlang | Args], X) ->
    cas(Args, X);

%%% ERROR --------------------------------------------------------------
cas([Y | Args], X) ->
    cas(Args, X++[Y]).



%% Check Options for make_tar
check_args_tar(Args) ->
    cat(Args, []).

cat([], X) ->
    X;
%%% path ---------------------------------------------------------------
cat([{path, P} | Args], X) when is_list(P) ->
    case check_path(P) of
	ok ->
	    cat(Args, X);
	error ->
	    cat(Args, X++[{path,P}])
    end;
%%% silent -------------------------------------------------------------
cat([silent | Args], X) ->
    cat(Args, X);
%%% dirs ---------------------------------------------------------------
cat([{dirs, D} | Args], X) ->
    case check_dirs(D) of
	ok ->
	    cat(Args, X);
	error ->
	    cat(Args, X++[{dirs, D}])
    end;
%%% erts ---------------------------------------------------------------
cat([{erts, E} | Args], X) when is_list(E)->
    cat(Args, X);
%%% src_tests ----------------------------------------------------
cat([src_tests | Args], X) ->
    cat(Args, X);
%%% variables ----------------------------------------------------------
cat([{variables, V} | Args], X) when is_list(V) ->
    case check_vars(V) of
	ok ->
	    cat(Args, X);
	error ->
	    cat(Args, X++[{variables, V}])
    end;
%%% var_tar ------------------------------------------------------------
cat([{var_tar, VT} | Args], X) when VT == include;
                                    VT == ownfile;
                                    VT == omit ->
    cat(Args, X);
%%% machine ------------------------------------------------------------
cat([{machine, M} | Args], X) when is_atom(M) ->
    cat(Args, X);
%%% exref --------------------------------------------------------------
cat([exref | Args], X)  ->
    cat(Args, X);
%%% exref Apps ---------------------------------------------------------
cat([{exref, Apps} | Args], X) when is_list(Apps) ->
    case check_apps(Apps) of 
	ok ->
	    cat(Args, X);
	error ->
	    cat(Args, X++[{exref, Apps}])
    end;
%%% outdir Dir ---------------------------------------------------------
cat([{outdir, Dir} | Args], X) when is_list(Dir) ->
    cat(Args, X);
%%% otp_build (secret, not documented) ---------------------------------
cat([otp_build | Args], X)  ->
    cat(Args, X);
%%% warnings_as_errors ----
cat([warnings_as_errors | Args], X) ->
    cat(Args, X);
%%% no_warn_sasl ----
cat([no_warn_sasl | Args], X) ->
    cat(Args, X);
%%% no_module_tests (kept for backwards compatibility, but ignored) ----
cat([no_module_tests | Args], X) ->
    cat(Args, X);
%%% ERROR --------------------------------------------------------------
cat([Y | Args], X) ->
    cat(Args, X++[Y]).

check_path([]) ->
    ok;
check_path([H|T]) when is_list(H) ->
    check_path(T);
check_path([_H|_T]) ->
    error.

check_dirs([]) ->
    ok;
check_dirs([H|T]) when is_atom(H) ->
    check_dirs(T);
check_dirs([_H|_T]) ->
    error.

check_vars([]) ->
    ok;
check_vars([{Name, Dir} | T]) ->
    if
	is_atom(Name), is_list(Dir) ->
	    check_vars(T);
	is_list(Name), is_list(Dir) ->
	    check_vars(T);
	true ->
	    error
    end;
check_vars(_) ->
    error.

check_apps([]) ->
    ok;
check_apps([H|T]) when is_atom(H) ->
    check_apps(T);
check_apps(_) ->
    error.

%% Format error

format_error(badly_formatted_release) ->
    io_lib:format("Syntax error in the release file~n",[]);
format_error({illegal_name, Name}) ->
    io_lib:format("Illegal name (~tp) in the release file~n",[Name]);
format_error({illegal_form, Form}) ->
    io_lib:format("Illegal tag in the release file: ~tp~n",[Form]);
format_error({missing_parameter,Par}) ->
    io_lib:format("Missing parameter (~p) in the release file~n",[Par]);
format_error({illegal_applications,Names}) ->
    io_lib:format("Illegal applications in the release file: ~p~n",
		  [Names]);
format_error({missing_mandatory_app,Name}) ->
    io_lib:format("Mandatory application ~w must be specified in the release file~n",
		  [Name]);
format_error({mandatory_app,Name,Type}) ->
    io_lib:format("Mandatory application ~w must be of type 'permanent' in the release file. Is '~p'.~n",
		  [Name,Type]);
format_error({duplicate_register,Dups}) ->
    io_lib:format("Duplicated register names: ~n~ts",
		  [map(fun({{Reg,App1,_,_},{Reg,App2,_,_}}) ->
			       io_lib:format("\t~tw registered in ~w and ~w~n",
					     [Reg,App1,App2])
		       end, Dups)]);
format_error({undefined_applications,Apps}) ->
    io_lib:format("Undefined applications: ~p~n",[Apps]);
format_error({duplicate_modules,Dups}) ->
    io_lib:format("Duplicated modules: ~n~ts",
		  [map(fun({{Mod,App1,_},{Mod,App2,_}}) ->
			       io_lib:format("\t~w specified in ~w and ~w~n",
					     [Mod,App1,App2])
		       end, Dups)]);
format_error({included_and_used, Dups}) ->
    io_lib:format("Applications both used and included: ~p~n",[Dups]);
format_error({duplicate_include, Dups}) ->
    io_lib:format("Duplicated application included: ~n~ts",
		  [map(fun({{Name,App1,_,_},{Name,App2,_,_}}) ->
			       io_lib:format("\t~w included in ~w and ~w~n",
					     [Name,App1,App2])
		       end, Dups)]);
format_error({modules,ModErrs}) ->
    format_errors(ModErrs);
format_error({circular_dependencies,Apps}) ->
    io_lib:format("Circular dependencies among applications: ~p~n",[Apps]);
format_error({not_found,File}) ->
    io_lib:format("File not found: ~tp~n",[File]);
format_error({parse,File,{Line,Mod,What}}) ->
    Str = Mod:format_error(What),
    io_lib:format("~ts:~w: ~ts\n",[File, Line, Str]);
format_error({read,File}) ->
    io_lib:format("Cannot read ~tp~n",[File]);
format_error({open,File,Error}) ->
    io_lib:format("Cannot open ~tp - ~ts~n",
		  [File,file:format_error(Error)]);
format_error({close,File,Error}) ->
    io_lib:format("Cannot close ~tp - ~ts~n",
		  [File,file:format_error(Error)]);
format_error({delete,File,Error}) ->
    io_lib:format("Cannot delete ~tp - ~ts~n",
		  [File,file:format_error(Error)]);
format_error({tar_error,What}) ->
    form_tar_err(What);
format_error({warnings_treated_as_errors,Warnings}) ->
    io_lib:format("Warnings being treated as errors:~n~ts",
                  [map(fun(W) -> form_warn("",W) end, Warnings)]);
format_error(ListOfErrors) when is_list(ListOfErrors) ->
    format_errors(ListOfErrors);
format_error(E) -> io_lib:format("~tp~n",[E]).

format_errors(ListOfErrors) ->
    map(fun({error,E}) -> form_err(E);
	   (E)         -> form_err(E)
	end, ListOfErrors).

form_err({bad_application_name,{Name,Found}}) ->
    io_lib:format("~p: Mismatched application id: ~p~n",[Name,Found]);
form_err({error_reading, {Name, What}}) ->
    io_lib:format("~p: ~ts~n",[Name,form_reading(What)]);
form_err({module_not_found,App,Mod}) ->
    io_lib:format("~w: Module (~w) not found~n",[App,Mod]);
form_err({error_add_appl, {Name, {tar_error, What}}}) ->
    io_lib:format("~p: ~ts~n",[Name,form_tar_err(What)]);
form_err(E) ->
    io_lib:format("~tp~n",[E]).

form_reading({not_found,File}) ->
    io_lib:format("File not found: ~tp~n",[File]);
form_reading({application_vsn, {Name,Vsn}}) ->
    io_lib:format("Application ~ts with version ~tp not found~n",[Name, Vsn]);
form_reading({parse,File,{Line,Mod,What}}) ->
    Str = Mod:format_error(What),
    io_lib:format("~ts:~w: ~ts\n",[File, Line, Str]);
form_reading({read,File}) ->
    io_lib:format("Cannot read ~tp~n",[File]);
form_reading({{bad_param, P},_}) ->
    io_lib:format("Bad parameter in .app file: ~tp~n",[P]);
form_reading({{missing_param,P},_}) ->
    io_lib:format("Missing parameter in .app file: ~p~n",[P]);
form_reading({badly_formatted_application,_}) ->
    io_lib:format("Syntax error in .app file~n",[]);
form_reading({override_include,Apps}) ->
    io_lib:format("Tried to include not (in .app file) specified applications: ~p~n",
		  [Apps]);
form_reading({no_valid_version, {{_, SVsn}, {_, File, FVsn}}}) ->
    io_lib:format("No valid version (~tp) of .app file found. Found file ~tp with version ~tp~n", 
		  [SVsn, File, FVsn]);
form_reading({parse_error, {File, Line, Error}}) ->
    io_lib:format("Parse error in file: ~tp.  Line: ~w  Error: ~tp; ~n", [File, Line, Error]);
form_reading(W) ->
    io_lib:format("~tp~n",[W]).

form_tar_err({open, File, Error}) ->
    io_lib:format("Cannot open tar file ~ts - ~ts~n",
		  [File, erl_tar:format_error(Error)]);
form_tar_err({add, File, Error}) ->
    io_lib:format("Cannot add file ~ts to tar file - ~ts~n",
		  [File, erl_tar:format_error(Error)]).

%% Format warning

format_warning(Warnings) ->
    map(fun({warning,W}) -> form_warn("*WARNING* ", W) end, Warnings).

form_warn(Prefix, {source_not_found,{Mod,App,_}}) ->
    io_lib:format("~ts~w: Source code not found: ~w.erl~n",
		  [Prefix,App,Mod]);
form_warn(Prefix, {{parse_error, File},{_,_,App,_,_}}) ->
    io_lib:format("~ts~w: Parse error: ~tp~n",
		  [Prefix,App,File]);
form_warn(Prefix, {obj_out_of_date,{Mod,App,_}}) ->
    io_lib:format("~ts~w: Object code (~w) out of date~n",
		  [Prefix,App,Mod]);
form_warn(Prefix, {exref_undef, Undef}) ->
    F = fun({M,F,A}) ->
		io_lib:format("~tsUndefined function ~w:~tw/~w~n",
			      [Prefix,M,F,A])
	end,
    map(F, Undef);
form_warn(Prefix, missing_sasl) ->
    io_lib:format("~tsMissing application sasl. "
		  "Can not upgrade with this release~n",
		  [Prefix]);
form_warn(Prefix, What) ->
    io_lib:format("~ts~tp~n", [Prefix,What]).
