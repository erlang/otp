%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2013. All Rights Reserved.
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


%% The main purpose of system_information is to aggregate all information
%% deemed useful for investigation, i.e. system_information:report/0.

%% The server and all other utilities surrounding this is for inspecting
%% reported values. Functions will be added to this as time goes by.

-module(system_information).
-behaviour(gen_server).

%% API
-export([report/0,
	from_file/1,
	to_file/1]).

-export([start/0, stop/0,
         load_report/0, load_report/2,
         applications/0, applications/1,
         application/1, application/2,
         environment/0, environment/1,
         module/1, module/2,
         modules/1,
         sanity_check/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% change version if parsing of file changes
-define(REPORT_FILE_VSN, "1.0").

-record(state, {
	report
    }).

%%===================================================================
%% API
%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).


stop() ->
    gen_server:call(?SERVER, stop, infinity).

load_report() -> load_report(data, report()).

load_report(file, File)   -> load_report(data, from_file(File));
load_report(data, Report) ->
    ok = start_internal(), gen_server:call(?SERVER, {load_report, Report}, infinity).

report() -> [
	{init_arguments,    init:get_arguments()},
	{code_paths,        code:get_path()},
	{code,              code()},
	{system_info,       erlang_system_info()},
	{erts_compile_info, erlang:system_info(compile_info)},
	{beam_dynamic_libraries, get_dynamic_libraries()},
	{environment_erts,  os_getenv_erts_specific()},
	{environment,       [split_env(Env) || Env <- os:getenv()]},
	{sanity_check,      sanity_check()}	     
    ].

-spec to_file(FileName) -> ok | {error, Reason} when
      FileName :: file:name_all(),
      Reason :: file:posix() | badarg | terminated | system_limit.

to_file(File) ->
    file:write_file(File, iolist_to_binary([
		io_lib:format("{system_information_version, ~p}.~n", [
			?REPORT_FILE_VSN
		    ]),
		io_lib:format("{system_information, ~p}.~n", [
			report()
		    ])
	    ])).

from_file(File) ->
    case file:consult(File) of
	{ok, Data} ->
	    case get_value([system_information_version], Data) of
		?REPORT_FILE_VSN ->
		    get_value([system_information], Data);
		Vsn ->
		    erlang:error({unknown_version, Vsn})
	    end;
	_ ->
	    erlang:error(bad_report_file)
    end.

applications() -> applications([]).
applications(Opts) when is_list(Opts) ->
    gen_server:call(?SERVER, {applications, Opts}, infinity).

application(App) when is_atom(App) -> application(App, []).
application(App, Opts) when is_atom(App), is_list(Opts) ->
    gen_server:call(?SERVER, {application, App, Opts}, infinity).

environment() -> environment([]).
environment(Opts) when is_list(Opts) ->
    gen_server:call(?SERVER, {environment, Opts}, infinity).

module(M) when is_atom(M) -> module(M, []).
module(M, Opts) when is_atom(M), is_list(Opts) ->
    gen_server:call(?SERVER, {module, M, Opts}, infinity).

modules(Opt) when is_atom(Opt) ->
    gen_server:call(?SERVER, {modules, Opt}, infinity).


-spec sanity_check() -> ok | {failed, Failures} when
      Application :: atom(),
      ApplicationVersion :: string(),
      MissingRuntimeDependencies :: {missing_runtime_dependencies,
				     ApplicationVersion,
				     [ApplicationVersion]},
      InvalidApplicationVersion :: {invalid_application_version,
				    ApplicationVersion},
      InvalidAppFile :: {invalid_app_file, Application},
      Failure :: MissingRuntimeDependencies
	       | InvalidApplicationVersion
	       | InvalidAppFile,
      Failures :: [Failure].

sanity_check() ->
    case check_runtime_dependencies() of
	[] -> ok;
	Issues -> {failed, Issues}
    end.

%%===================================================================
%% gen_server callbacks
%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(stop, _From, S) ->
    {stop, normal, ok, S};

handle_call({load_report, Report}, _From, S) ->
    Version = get_value([system_info, system_version], Report),
    io:format("Loaded report from system version: ~s~n", [Version]),
    {reply, ok, S#state{ report = Report }};

handle_call(_Req, _From, #state{ report = undefined } = S) ->
    {reply, {error, report_not_loaded}, S};

handle_call({applications, Opts}, _From, #state{ report = Report } = S) ->
    ok = print_applications(get_value([code], Report), Opts),
    {reply, ok, S};

handle_call({application, App, Opts}, _From, #state{ report = Report } = S) ->
    Data = get_value([App], [AppInfo||{application, AppInfo}<-get_value([code], Report)]),
    ok = print_application({App, Data}, Opts),
    {reply, ok, S};


handle_call({environment, Opts}, _From, #state{ report = Report } = S) ->
    Choices = case proplists:get_bool(full, Opts) of
	true  -> [environment];
	false -> [environment_erts]
    end,
    ok = print_environments(get_value(Choices, Report), Opts),
    {reply, ok, S};


handle_call({module, M, Opts}, _From, #state{ report = Report } = S) ->
    Mods = find_modules_from_code(M, get_value([code], Report)),
    print_modules_from_code(M, Mods, Opts),
    {reply, ok, S};

handle_call({modules, native}, _From, #state{ report = Report } = S) ->
    Codes = get_native_modules_from_code(get_value([code],Report)),
    io:format("~p~n", [Codes]),
    {reply, ok, S};


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%===================================================================
%% Internal functions
%%===================================================================

start_internal() ->
    case start() of
        {ok,_} -> ok;
        {error, {already_started,_}} -> ok;
        Error -> Error
    end.

%% handle report values

get_value([], Data) -> Data;
get_value([K|Ks], Data) ->
    get_value(Ks, proplists:get_value(K, Data, [])).

find_modules_from_code(M, [{code, Info}|Codes]) ->
    case find_modules(M, get_value([modules], Info)) of
	[]   -> find_modules_from_code(M, Codes);
	Mods ->
	    Path = get_value([path], Info),
	    [{Path, Mods}|find_modules_from_code(M, Codes)]
    end;
find_modules_from_code(M, [{application, {App, Info}}|Codes]) ->
    case find_modules(M, get_value([modules], Info)) of
	[]   -> find_modules_from_code(M, Codes);
	Mods ->
	    Path = get_value([path], Info),
	    Vsn  = get_value([vsn], Info),
	    [{App, Vsn, Path, Mods}|find_modules_from_code(M, Codes)]
    end;
find_modules_from_code(_, []) -> [].

find_modules(M, [{M, _}=Info|Ms]) -> [Info|find_modules(M,Ms)];
find_modules(M, [_|Ms]) -> find_modules(M, Ms);
find_modules(_, []) -> [].

get_native_modules_from_code([{application, {App, Info}}|Cs]) ->
    case get_native_modules(get_value([modules], Info)) of
	[] -> get_native_modules_from_code(Cs);
	Mods ->
	    Path = get_value([path], Info),
	    Vsn  = get_value([vsn], Info),
	    [{App, Vsn, Path, Mods}|get_native_modules_from_code(Cs)]
    end;
get_native_modules_from_code([{code, Info}|Cs]) ->
    case get_native_modules(get_value([modules], Info)) of
	[] -> get_native_modules_from_code(Cs);
	Mods ->
	    Path = get_value([path], Info),
	    [{Path, Mods}|get_native_modules_from_code(Cs)]
    end;
get_native_modules_from_code([]) -> [].

get_native_modules([]) -> [];
get_native_modules([{Mod, Info}|Ms]) ->
    case proplists:get_value(native, Info) of
	false -> get_native_modules(Ms);
	_     -> [Mod|get_native_modules(Ms)]
    end.


%% print information

print_applications([{application, App}|Apps], Opts) ->
    print_application(App, Opts),
    print_applications(Apps, Opts);
print_applications([{code,_}|Apps], Opts) ->
    print_applications(Apps, Opts);
print_applications([], _) ->
    ok.

print_application({App, Info}, Opts) ->
    Vsn = get_value([vsn], Info),
    io:format(" * ~w-~s~n", [App, Vsn]),
    case proplists:get_bool(full, Opts) of
	true ->
	    _ = [ begin
			print_module(Minfo)
		end || Minfo <- get_value([modules], Info) ],
	    ok;
	false ->
	    ok
    end.

print_environments([Env|Envs],Opts) ->
    print_environment(Env,Opts),
    print_environments(Envs,Opts);
print_environments([],_) ->
    ok.

print_environment({_Key, false},_) -> ok;
print_environment({Key, Value},_) ->
    io:format(" - ~s = ~ts~n", [Key, Value]).

print_modules_from_code(M, [Info|Ms], Opts) ->
    print_module_from_code(M, Info),
    case proplists:get_bool(full, Opts) of
	true  -> print_modules_from_code(M, Ms, Opts);
	false -> ok
    end;
print_modules_from_code(_, [], _) ->
    ok.

print_module_from_code(M, {Path, [{M,ModInfo}]}) ->
    io:format(" from path \"~ts\" (no application):~n", [Path]),
    io:format("     - compiler: ~s~n", [get_value([compiler], ModInfo)]),
    io:format("     -      md5: ~s~n", [get_value([md5], ModInfo)]),
    io:format("     -   native: ~w~n", [get_value([native], ModInfo)]),
    io:format("     -   loaded: ~w~n", [get_value([loaded], ModInfo)]),
    ok;
print_module_from_code(M, {App,Vsn,Path,[{M,ModInfo}]}) ->
    io:format(" from path \"~ts\" (~w-~s):~n", [Path,App,Vsn]),
    io:format("     - compiler: ~s~n", [get_value([compiler], ModInfo)]),
    io:format("     -      md5: ~s~n", [get_value([md5], ModInfo)]),
    io:format("     -   native: ~w~n", [get_value([native], ModInfo)]),
    io:format("     -   loaded: ~w~n", [get_value([loaded], ModInfo)]),
    ok.

print_module({Mod, ModInfo}) ->
    io:format("   - ~w:~n", [Mod]),
    io:format("     - compiler: ~s~n", [get_value([compiler], ModInfo)]),
    io:format("     -      md5: ~s~n", [get_value([md5], ModInfo)]),
    io:format("     -   native: ~w~n", [get_value([native], ModInfo)]),
    io:format("     -   loaded: ~w~n", [get_value([loaded], ModInfo)]),
    ok.



%% get useful information from erlang:system_info/1

erlang_system_info() ->
    erlang_system_info([
	    allocator,
	    check_io,
	    otp_release,
	    port_limit,
	    process_limit,
	    % procs,  % not needed
	    smp_support,
	    system_version,
	    system_architecture,
	    threads,
	    thread_pool_size,
	    {wordsize,internal},
	    {wordsize,external},
	    {cpu_topology, defined},
	    {cpu_topology, detected},
	    scheduler_bind_type,
	    scheduler_bindings,
	    compat_rel,
	    schedulers_state,
	    build_type,
	    logical_processors,
	    logical_processors_online,
	    logical_processors_available,
	    driver_version,
	    nif_version,
	    emu_args,
	    ethread_info,
	    beam_jump_table,
	    taints
	]).

erlang_system_info([]) -> [];
erlang_system_info([Type|Types]) ->
    [{Type, erlang:system_info(Type)}|erlang_system_info(Types)].


%% get known useful erts environment

os_getenv_erts_specific() -> 
    os_getenv_erts_specific([
	    "BINDIR",
	    "DIALYZER_EMULATOR",
	    "CERL_DETACHED_PROG",
	    "EMU",
	    "ERL_CONSOLE_MODE",
	    "ERL_CRASH_DUMP",
	    "ERL_CRASH_DUMP_NICE",
	    "ERL_CRASH_DUMP_SECONDS",
	    "ERL_EPMD_PORT",
	    "ERL_EMULATOR_DLL",
	    "ERL_FULLSWEEP_AFTER",
	    "ERL_LIBS",
	    "ERL_MALLOC_LIB",
	    "ERL_MAX_PORTS",
	    "ERL_MAX_ETS_TABLES",
	    "ERL_NO_VFORK",
	    "ERL_NO_KERNEL_POLL",
	    "ERL_THREAD_POOL_SIZE",
	    "ERLC_EMULATOR",
	    "ESCRIPT_EMULATOR",
	    "HOME",
	    "HOMEDRIVE",
	    "HOMEPATH",
	    "LANG",
	    "LC_ALL",
	    "LC_CTYPE",
	    "PATH",
	    "PROGNAME",
	    "RELDIR",
	    "ROOTDIR",
	    "TERM",
	    %"VALGRIND_LOG_XML",

	    %% heart
	    "COMSPEC",
	    "HEART_COMMAND",

	    %% run_erl
	    "RUN_ERL_LOG_ALIVE_MINUTES",
	    "RUN_ERL_LOG_ACTIVITY_MINUTES",
	    "RUN_ERL_LOG_ALIVE_FORMAT",
	    "RUN_ERL_LOG_ALIVE_IN_UTC",
	    "RUN_ERL_LOG_GENERATIONS",
	    "RUN_ERL_LOG_MAXSIZE",
	    "RUN_ERL_DISABLE_FLOWCNTRL",

	    %% driver getenv
	    "CALLER_DRV_USE_OUTPUTV",
	    "ERL_INET_GETHOST_DEBUG",
	    "ERL_EFILE_THREAD_SHORT_CIRCUIT",
	    "ERL_WINDOW_TITLE",
	    "ERL_ABORT_ON_FAILURE",
	    "TTYSL_DEBUG_LOG"
	]).

os_getenv_erts_specific([]) -> [];
os_getenv_erts_specific([Key|Keys]) ->
    [{Key, os:getenv(Key)}|os_getenv_erts_specific(Keys)].

split_env(Env) ->
    split_env(Env, []).

split_env([$=|Vs], Key) -> {lists:reverse(Key), Vs};
split_env([I|Vs], Key)  -> split_env(Vs, [I|Key]);
split_env([], KV)       -> lists:reverse(KV). % should not happen.

%% get applications

code() ->
    % order is important
    get_code_from_paths(code:get_path()).

get_code_from_paths([]) -> [];
get_code_from_paths([Path|Paths]) ->
    case is_application_path(Path) of
	true -> 
	    [{application, get_application_from_path(Path)}|get_code_from_paths(Paths)];
	false ->
	    [{code, [
			{path,    Path},
			{modules, get_modules_from_path(Path)}
		    ]}|get_code_from_paths(Paths)]
    end.

is_application_path(Path) ->
    case filelib:wildcard(filename:join(Path, "*.app")) of
	[] -> false;
	_  -> true
    end.

get_application_from_path(Path) ->
    [Appfile|_] = filelib:wildcard(filename:join(Path, "*.app")),
    case file:consult(Appfile) of
	{ok, [{application, App, Info}]} ->
	    {App, [
		    {description, proplists:get_value(description, Info, [])},
		    {vsn,         proplists:get_value(vsn, Info, [])},
		    {path,        Path},
		    {runtime_dependencies,
		     proplists:get_value(runtime_dependencies, Info, [])},
		    {modules,     get_modules_from_path(Path)}
		]}
    end.

get_modules_from_path(Path) ->
    [ 
	begin
		{ok,{Mod, Md5}} = beam_lib:md5(Beam),
		Loaded = case code:is_loaded(Mod) of
		    false -> false;
		    _     -> true
		end,
		{Mod, [
			{loaded,   Loaded},
			{native,   beam_is_native_compiled(Beam)},
			{compiler, get_compiler_version(Beam)},
			{md5,      hexstring(Md5)}
		    ]}
	end || Beam <- filelib:wildcard(filename:join(Path, "*.beam"))
    ].

hexstring(Bin) when is_binary(Bin) ->
    lists:flatten([io_lib:format("~2.16.0b", [V]) || <<V>> <= Bin]).

%% inspect beam files for information

get_compiler_version(Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
	{ok,{_,[{compile_info, Info}]}} ->
	    proplists:get_value(version, Info);
	_ -> undefined
    end.

%% we don't know the specific chunk names of native code
%% we don't want to load the code to check it
beam_is_native_compiled(Beam) ->
    Chunks = get_value([chunks], beam_lib:info(Beam)),
    case check_known_hipe_chunks(Chunks) of
	[] -> false;
	[Arch] -> {true, Arch};
	Archs  -> {true, Archs}
    end.


check_known_hipe_chunks([{Tag,_,_}|Cs]) ->
    case is_chunk_tag_hipe_arch(Tag) of
	false -> check_known_hipe_chunks(Cs);
	{true, Arch} -> [Arch|check_known_hipe_chunks(Cs)]
    end;
check_known_hipe_chunks([]) -> [].

%% these values are taken from hipe_unified_loader
%% perhaps these should be exported in that module?

-define(HS8P_TAG,"HS8P").
-define(HPPC_TAG,"HPPC").
-define(HP64_TAG,"HP64").
-define(HARM_TAG,"HARM").
-define(HX86_TAG,"HX86").
-define(HA64_TAG,"HA64").

is_chunk_tag_hipe_arch(Tag) ->
    case Tag of
	?HA64_TAG -> {true, amd64};       %% HiPE, x86_64, (implicit: 64-bit, Unix)
	?HARM_TAG -> {true, arm};         %% HiPE, arm, v5 (implicit: 32-bit, Linux)
	?HPPC_TAG -> {true, powerpc};     %% HiPE, PowerPC (implicit: 32-bit, Linux)
	?HP64_TAG -> {true, ppc64};       %% HiPE, ppc64 (implicit: 64-bit, Linux)
	?HS8P_TAG -> {true, ultrasparc};  %% HiPE, SPARC, V8+ (implicit: 32-bit)
	%% Future:     HSV9               %% HiPE, SPARC, V9 (implicit: 64-bit)
	%%             HW32               %% HiPE, x86, Win32
	_ -> false
    end.


get_dynamic_libraries() ->
    Beam = filename:join([os:getenv("BINDIR"),get_beam_name()]),
    case os:type() of
	{unix, darwin} -> os:cmd("otool -L " ++ Beam);
	_ -> os:cmd("ldd " ++ Beam)
    end.

get_beam_name() ->
    Type = case erlang:system_info(build_type) of
	opt -> "";
	TypeName -> "." ++ atom_to_list(TypeName)
    end,
    Flavor = case erlang:system_info(smp_support) of
	false -> "";
	true -> ".smp"
    end,
    Beam = os:getenv("EMU", "beam"),
    Beam ++ Type ++ Flavor.

%% Check runtime dependencies...

vsnstr2vsn(VsnStr) ->
    list_to_tuple(lists:map(fun (Part) ->
				    list_to_integer(Part)
			    end,
			    string:tokens(VsnStr, "."))).

rtdepstrs2rtdeps([]) ->
    [];
rtdepstrs2rtdeps([RTDep | RTDeps]) ->
    [AppStr, VsnStr] = string:tokens(RTDep, "-"),
    [{list_to_atom(AppStr), vsnstr2vsn(VsnStr)} | rtdepstrs2rtdeps(RTDeps)].

build_app_table([], AppTab) ->
    AppTab;
build_app_table([App | Apps], AppTab0) ->
    AppTab1 = try
		  %% We may have multiple application versions installed
		  %% of the same application! It is therefore important
		  %% to look up the application version that actually will
		  %% be used via code server.
		  AppFile = code:where_is_file(atom_to_list(App) ++ ".app"),
		  {ok, [{application, App, Info}]} = file:consult(AppFile),
		  VsnStr = proplists:get_value(vsn, Info),
		  Vsn = vsnstr2vsn(VsnStr),
		  RTDepStrs = proplists:get_value(runtime_dependencies,
						  Info, []),
		  RTDeps = rtdepstrs2rtdeps(RTDepStrs),
		  gb_trees:insert(App, {Vsn, RTDeps}, AppTab0)
	      catch
		  _ : _ ->
		      AppTab0
	      end,
    build_app_table(Apps, AppTab1).

meets_min_req(Vsn, Vsn) ->
    true;
meets_min_req({X}, VsnReq) ->
    meets_min_req({X, 0, 0}, VsnReq);
meets_min_req({X, Y}, VsnReq) ->
    meets_min_req({X, Y, 0}, VsnReq);
meets_min_req(Vsn, {X}) ->
    meets_min_req(Vsn, {X, 0, 0});
meets_min_req(Vsn, {X, Y}) ->
    meets_min_req(Vsn, {X, Y, 0});
meets_min_req({X, _Y, _Z}, {XReq, _YReq, _ZReq}) when X > XReq ->
    true;
meets_min_req({X, Y, _Z}, {X, YReq, _ZReq}) when Y > YReq ->
    true;
meets_min_req({X, Y, Z}, {X, Y, ZReq}) when Z > ZReq ->
    true;
meets_min_req({_X, _Y, _Z}, {_XReq, _YReq, _ZReq}) ->
    false;
meets_min_req(Vsn, VsnReq) ->
    gp_meets_min_req(mk_gp_vsn_list(Vsn), mk_gp_vsn_list(VsnReq)).

gp_meets_min_req([X, Y, Z | _Vs], [X, Y, Z]) ->
    true;
gp_meets_min_req([X, Y, Z | _Vs], [XReq, YReq, ZReq]) ->
    meets_min_req({X, Y, Z}, {XReq, YReq, ZReq});
gp_meets_min_req([X, Y, Z | Vs], [X, Y, Z | VReqs]) ->
    gp_meets_min_req_tail(Vs, VReqs);
gp_meets_min_req(_Vsn, _VReq) ->
    %% Versions on different version branches, i.e., the minimum
    %% required functionality is not included in Vsn.
    false.

gp_meets_min_req_tail([V | Vs], [V | VReqs]) ->
    gp_meets_min_req_tail(Vs, VReqs);
gp_meets_min_req_tail([], []) ->
    true;
gp_meets_min_req_tail([_V | _Vs], []) ->
    true;
gp_meets_min_req_tail([V | _Vs], [VReq]) when V > VReq ->
    true;
gp_meets_min_req_tail(_Vs, _VReqs) ->
    %% Versions on different version branches, i.e., the minimum
    %% required functionality is not included in Vsn.
    false.

mk_gp_vsn_list(Vsn) ->
    [X, Y, Z | Tail] = tuple_to_list(Vsn),
    [X, Y, Z | remove_trailing_zeroes(Tail)].

remove_trailing_zeroes([]) ->
    [];
remove_trailing_zeroes([0 | Vs]) ->
    case remove_trailing_zeroes(Vs) of
	[] -> [];
	NewVs -> [0 | NewVs]
    end;
remove_trailing_zeroes([V | Vs]) ->
    [V | remove_trailing_zeroes(Vs)].

mk_app_vsn_str({App, Vsn}) ->
    mk_app_vsn_str(App, Vsn).

mk_app_vsn_str(App, Vsn) ->
    VsnList = tuple_to_list(Vsn),
    lists:flatten([atom_to_list(App),
		   $-,
		   integer_to_list(hd(VsnList)),
		   lists:map(fun (Part) ->
				     [$., integer_to_list(Part)]
			     end, tl(VsnList))]).

otp_17_0_vsns_orddict() ->
    [{asn1,{3,0}},
     {common_test,{1,8}},
     {compiler,{5,0}},
     {cosEvent,{2,1,15}},
     {cosEventDomain,{1,1,14}},
     {cosFileTransfer,{1,1,16}},
     {cosNotification,{1,1,21}},
     {cosProperty,{1,1,17}},
     {cosTime,{1,1,14}},
     {cosTransactions,{1,2,14}},
     {crypto,{3,3}},
     {debugger,{4,0}},
     {dialyzer,{2,7}},
     {diameter,{1,6}},
     {edoc,{0,7,13}},
     {eldap,{1,0,3}},
     {erl_docgen,{0,3,5}},
     {erl_interface,{3,7,16}},
     {erts,{6,0}},
     {et,{1,5}},
     {eunit,{2,2,7}},
     {gs,{1,5,16}},
     {hipe,{3,10,3}},
     {ic,{4,3,5}},
     {inets,{5,10}},
     {jinterface,{1,5,9}},
     {kernel,{3,0}},
     {megaco,{3,17,1}},
     {mnesia,{4,12}},
     {observer,{2,0}},
     {odbc,{2,10,20}},
     {orber,{3,6,27}},
     {os_mon,{2,2,15}},
     {ose,{1,0}},
     {otp_mibs,{1,0,9}},
     {parsetools,{2,0,11}},
     {percept,{0,8,9}},
     {public_key,{0,22}},
     {reltool,{0,6,5}},
     {runtime_tools,{1,8,14}},
     {sasl,{2,4}},
     {snmp,{4,25,1}},
     {ssh,{3,0,1}},
     {ssl,{5,3,4}},
     {stdlib,{2,0}},
     {syntax_tools,{1,6,14}},
     {test_server,{3,7}},
     {tools,{2,6,14}},
     {typer,{0,9,6}},
     {webtool,{0,8,10}},
     {wx,{1,2}},
     {xmerl,{1,3,7}}].

otp_17_0_vsns_tab() ->
    gb_trees:from_orddict(otp_17_0_vsns_orddict()).

check_runtime_dependency({App, DepVsn}, AppTab) ->
    case gb_trees:lookup(App, AppTab) of
	none ->
	    false;
	{value, {Vsn, _}} ->
	    meets_min_req(Vsn, DepVsn)
    end.

check_runtime_dependencies(App, AppTab, OtpMinVsnTab) ->
    case gb_trees:lookup(App, AppTab) of
	none ->
	    [{invalid_app_file, App}];
	{value, {Vsn, RTDeps}} ->
	    RTD = case lists:foldl(
			 fun (RTDep, Acc) ->
				 case check_runtime_dependency(RTDep, AppTab) of
				     true ->
					 Acc;
				     false ->
					 [mk_app_vsn_str(RTDep) | Acc]
				 end
			 end,
			 [],
			 RTDeps) of
		      [] ->
			  [];
		      MissingDeps ->
			  [{missing_runtime_dependencies,
			    mk_app_vsn_str(App, Vsn),
			    MissingDeps}]
		  end,
	    case gb_trees:lookup(App, OtpMinVsnTab) of
		none ->
		    RTD;
		{value, MinVsn} ->
		    case meets_min_req(Vsn, MinVsn) of
			true ->
			    RTD;
			false ->
			    [{invalid_application_version,
			      mk_app_vsn_str(App, Vsn)} | RTD]
		    end
	    end
    end.

app_file_to_app(AF) ->
    list_to_atom(filename:basename(AF, ".app")).

get_apps() ->
    get_apps(code:get_path(), []).

get_apps([], Apps) ->
    lists:usort(Apps);
get_apps([Path|Paths], Apps) ->
    case filelib:wildcard(filename:join(Path, "*.app")) of
	[] ->
	    %% Not app or invalid app
	    get_apps(Paths, Apps);
	[AppFile] ->
	    get_apps(Paths, [app_file_to_app(AppFile) | Apps]);
	[_AppFile| _] = AppFiles ->
	    %% Strange with multple .app files... Lets put them
	    %% all in the list and see what we get...
	    lists:map(fun (AF) ->
			      app_file_to_app(AF)
		      end, AppFiles) ++ Apps
    end.

check_runtime_dependencies() ->
    OtpMinVsnTab = otp_17_0_vsns_tab(),
    Apps = get_apps(),
    AppTab = build_app_table(Apps, gb_trees:empty()),
    lists:foldl(fun (App, Acc) ->
			case check_runtime_dependencies(App,
							AppTab,
							OtpMinVsnTab) of
			    [] -> Acc;
			    Issues -> Issues ++ Acc
			end
		end,
		[],
		Apps).

%% End of runtime dependency checks
