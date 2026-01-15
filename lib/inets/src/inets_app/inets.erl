%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2006-2025. All Rights Reserved.
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
%% Purpose: The main interface module of the inets application
%%----------------------------------------------------------------------

-module(inets).
-moduledoc """
The Inets services API.

This module provides the most basic API to the clients and servers that are part
of the `Inets` application, such as start and stop.

[](){: #common_data_types }

### Data types

Type definitions that are used more than once in this module:

`service() = httpc | httpd`

`property() = atom()`

### See also

`m:httpc`, `m:httpd`
""".

%% API
-export([start/0, start/1, start/2, start/3,  
	 stop/0, stop/2, 
	 services/0, services_info/0,
	 service_names/0]).
-export([enable_trace/2, enable_trace/3, disable_trace/0, set_trace/1,
	 report_event/4]).
-export([versions/0,
         print_version_info/0, print_version_info/1]).

-type inets_service() :: httpd | httpc.
-type service_info() :: {inets_service(), pid(), [{profile, atom()}] | no_such_service | service_not_available}.

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
-doc(#{equiv => start/1}).
-spec start() -> ok | {error, Reason} when Reason :: term().
start() ->
    application:start(inets, temporary).

-doc """
Starts the `Inets` application. Default type is `temporary`. See also
`m:application`.
""".
-spec start(Type) -> ok | {error, Reason} when
      Type :: application:restart_type(),
      Reason :: term().
start(Type) ->
    application:ensure_all_started(ssl),
    application:start(inets, Type).

%%--------------------------------------------------------------------
%% Function: start(Service, ServiceConfig [, How]) -> {ok, Pid} | 
%%                                                {error, Reason}
%%
%% Service = - httpc | httpd
%% ServiceConfig = ConfPropList | ConfFile
%% ConfPropList = [{Property, Value}] according to service 
%% ConfFile = Path - when service is httpd
%% How = inets | stand_alone
%%
%% Description: Dynamically starts an inets service after the inets
%% application has been started. 
%%
%% Note: Dynamically started services will not be handled by
%% application takeover and failover behavior when inets is run as a
%% distributed application. Nor will they be automatically restarted
%% when the inets application is restarted, but as long as the inets
%% application is up and running they will be supervised and may be
%% soft code upgraded. Services started with the option stand alone,
%% e.i. the service is not started as part of the inets application,
%% will lose all OTP application benefits such as soft upgrade. The
%% stand alone service will be linked to the process that started it.
%% In most cases some of the supervison functionality will still be
%% in place and in some sense the calling process has now become the
%% top supervisor.
%% --------------------------------------------------------------------
-doc(#{equiv => start/3}).
-spec start(Service, ServiceConfig) -> Result when
      Service :: inets_service(),
      ServiceConfig :: ConfPropList | ConfFile,
      ConfPropList :: [{Property, Value}],
      ConfFile :: string(),
      Property :: term(),
      Value :: term(),
      Result :: {ok, pid()} | {error, term()}.
start(Service, ServiceConfig) ->
    start_service(Service, ServiceConfig, inets).

-doc """
Dynamically starts an `Inets` service after the `Inets` application has been
started.

> #### Note {: .info }
>
> Dynamically started services are not handled by application takeover and
> failover behavior when `Inets` is run as a distributed application. Nor are
> they automatically restarted when the `Inets` application is restarted. As
> long as the `Inets` application is operational, they are supervised and can be
> soft code upgraded.
>
> A service started as `stand_alone`, that is, the service is not started as
> part of the `Inets` application, lose all OTP application benefits, such as
> soft upgrade. The `stand_alone`\-service is linked to the process that started
> it. Usually some supervision functionality is still in place and in some sense
> the calling process becomes the top supervisor.
>
> #### Warning {: .warning }
> The stand_alone option is considered deprecated.
>
""".
-spec start(Service, ServiceConfig, How) -> Result when
      Service :: inets_service(),
      ServiceConfig :: ConfPropList | ConfFile,
      How :: inets | stand_alone,
      ConfPropList :: [{Property, Value}],
      ConfFile :: string(),
      Property :: term(),
      Value :: term(),
      Result :: {ok, pid()} | {error, term()}.
start(Service, ServiceConfig, How) ->
    start_service(Service, ServiceConfig, How).


%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
-doc """
Stops the `Inets` application. See also `m:application`.
""".
-spec stop() -> ok.
stop() ->
    application:stop(inets).


%%--------------------------------------------------------------------
%% Function: stop(Service, Pid) -> ok
%%
%% Service - httpc | httpd | stand_alone
%%
%% Description: Stops a started service of the inets application or takes
%% down a stand alone "service" gracefully.
%%--------------------------------------------------------------------
-doc """
Stops a started service of the `Inets` application or takes down a
`stand_alone`\-service gracefully. When option `stand_alone` is used in start,
only the pid is a valid argument to stop.
""".
-spec stop(Service, Reference) -> ok | {error, Reason} when
      Service :: inets_service() | stand_alone,
      Reference :: pid() | term(),
      Reason :: term().
stop(stand_alone, Pid) ->
    true = exit(Pid, shutdown),
    ok;

stop(Service, Pid) ->
    call_service(Service, stop_service, Pid).


%%--------------------------------------------------------------------
%% Function: services() -> [{Service, Pid}]
%%
%% Description: Returns a list of currently running services. 
%% Note: Services started with the stand alone option will not be listed
%%--------------------------------------------------------------------
-doc """
Returns a list of currently running services.

> #### Note {: .info }
>
> Services started as `stand_alone` are not listed.
""".
-spec services() -> [{inets_service(), pid()}] | {error, inets_not_started}.
services() ->
    try lists:flatten(lists:map(fun(Module) ->
					Module:services()
				end, service_names())) of
	Result ->
	    Result
    catch 
	exit:{noproc, _} ->
            {error, inets_not_started}
    end.
					       

%%--------------------------------------------------------------------
%% Function: services_info() -> [{Service, Pid, Info}]
%%
%% Description: Returns a list of currently running services where
%% each service is described by a [{Property, Value}] list. 
%%--------------------------------------------------------------------
-doc """
Returns a list of currently running services where each service is described by
an `[{Option, Value}]` list. The information in the list is specific for each
service and each service has probably its own info function that gives more
details about the service. If specific service info returns `{error, Reason}`,
Info will contain Reason term.
""".
-spec services_info() -> [service_info()]
              | {error, inets_not_started}.
services_info() ->
    case services() of
	{error, inets_not_started} ->
	    {error, inets_not_started};
	Services ->
	    Fun =  fun({Service, Pid}) -> 
			   Info =  
			       case Service:service_info(Pid) of
				   {ok, PropList} ->
				       PropList;
				   {error, Reason} ->
				       Reason
			       end,
			   {Service, Pid, Info}
		   end,
	    lists:flatten(lists:map(Fun, Services))
    end.



%%--------------------------------------------------------------------
%% Function: print_version_info() 
%%
%% Description: Simple utility function to print information 
%%              about versions (system, OS and modules). 
%%--------------------------------------------------------------------

-doc false.
print_version_info() ->
    {ok, Versions} = inets:versions(),
    print_version_info(Versions).

-doc false.
print_version_info(Versions) when is_list(Versions) ->
    print_sys_info(Versions),
    print_os_info(Versions),
    print_mods_info(Versions).

print_sys_info(Versions) ->
    case key1search(sys_info, Versions) of
        {value, SysInfo} when is_list(SysInfo) ->
            {value, Arch} = key1search(arch, SysInfo, "Not found"),
            {value, Ver}  = key1search(ver, SysInfo, "Not found"),
            io:format("System info: "
                      "~n   Arch: ~s"
                      "~n   Ver:  ~s"
                      "~n", [Arch, Ver]),
            ok;
        _ ->
            io:format("System info: Not found~n", []),
            not_found
    end.

print_os_info(Versions) ->
    case key1search(os_info, Versions) of
        {value, OsInfo} when is_list(OsInfo) ->
            Fam =
                case key1search(fam, OsInfo, "Not found") of
                    {value, F} when is_atom(F) ->
                        atom_to_list(F);
                    {value, LF} when is_list(LF) ->
                        LF;
                    {value, XF} ->
                        lists:flatten(io_lib:format("~p", [XF]))
                end,
            Name =
                case key1search(name, OsInfo) of
                    {value, N} when is_atom(N) ->
                        "[" ++ atom_to_list(N) ++ "]";
                    {value, LN} when is_list(LN) ->
                        "[" ++ LN ++ "]";
                    not_found ->
                        ""
                end,
            Ver =
                case key1search(ver, OsInfo, "Not found") of
                    {value, T} when is_tuple(T) ->
                        tversion(T);
                    {value, LV} when is_list(LV) ->
                        LV;
                    {value, XV} ->
                        lists:flatten(io_lib:format("~p", [XV]))
                end,
            io:format("OS info: "
                      "~n   Family: ~s ~s"
                      "~n   Ver:    ~s"
                      "~n", [Fam, Name, Ver]),
            ok;
        _ ->
            io:format("OS info:     Not found~n", []),
            not_found
    end.

-doc false.
versions() ->
    App    = inets, 
    LibDir = code:lib_dir(App),
    File   = filename:join([LibDir, "ebin", atom_to_list(App) ++ ".app"]),
    case file:consult(File) of
        {ok, [{application, App, AppFile}]} ->
            case lists:keysearch(modules, 1, AppFile) of
                {value, {modules, Mods}} ->
                    {ok, version_info(Mods)};
                _ ->
                    {error, {invalid_format, modules}}
            end;
        Error ->
            {error, {invalid_format, Error}}
    end.

version_info(Mods) ->
    SysInfo = sys_info(),
    OsInfo  = os_info(),
    ModInfo = [mod_version_info(Mod) || Mod <- Mods],
    [{sys_info, SysInfo}, {os_info, OsInfo}, {mod_info, ModInfo}].

mod_version_info(Mod) ->
    Info = Mod:module_info(),
    {value, {attributes, Attr}}   = lists:keysearch(attributes, 1, Info),
    {value, {vsn,        [Vsn]}}  = lists:keysearch(vsn,        1, Attr),
    {value, {app_vsn,    AppVsn}} = lists:keysearch(app_vsn,    1, Attr),
    {value, {compile,    Comp}}   = lists:keysearch(compile,    1, Info),
    {value, {version,    Ver}}    = lists:keysearch(version,    1, Comp),
    {Mod, [{vsn,              Vsn},
           {app_vsn,          AppVsn},
           {compiler_version, Ver}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].

os_info() ->
    V = os:version(),
    {OsFam, OsName} = os:type(),
    [{fam, OsFam}, {name, OsName}, {ver, V}].

print_mods_info(Versions) ->
    case key1search(mod_info, Versions) of
        {value, ModsInfo} when is_list(ModsInfo) ->
            io:format("Module info: ~n", []),
            lists:foreach(fun print_mod_info/1, ModsInfo);
        _ ->
            io:format("Module info: Not found~n", []),
            not_found
    end.

tversion(T) ->
    L = tuple_to_list(T),
    lversion(L).

lversion([]) ->
    "";
lversion([A]) ->
    integer_to_list(A);
lversion([A|R]) ->
    integer_to_list(A) ++ "." ++ lversion(R).

print_mod_info({Module, Info}) ->
    % Maybe a asn1 generated module
    Asn1Vsn =
        case (catch Module:info()) of
            AI when is_list(AI) ->
                case (catch key1search(vsn, AI)) of
                    {value, V} when is_atom(V) ->
                        atom_to_list(V);
                    _ ->
                        "-"
                end;
            _ ->
                "-"
        end,
    Vsn =
        case key1search(vsn, Info) of
            {value, I} when is_integer(I) ->
                integer_to_list(I);
            _ ->
                "Not found"
        end,
    AppVsn =
        case key1search(app_vsn, Info) of
            {value, S1} when is_list(S1) ->
                S1;
            _ ->
                "Not found"
        end,
    CompVer =
        case key1search(compiler_version, Info) of
            {value, S2} when is_list(S2) ->
                S2;
            _ ->
                "Not found"
        end,
    io:format("   ~w:~n"
              "      Vsn:          ~s~n"
              "      App vsn:      ~s~n"
              "      ASN.1 vsn:    ~s~n"
              "      Compiler ver: ~s~n",
              [Module, Vsn, AppVsn, Asn1Vsn, CompVer]),
    ok.


key1search(Key, Vals) ->
    case lists:keysearch(Key, 1, Vals) of
        {value, {Key, Val}} ->
            {value, Val};
        false ->
            not_found
    end.

key1search(Key, Vals, Def) ->
    case key1search(Key, Vals) of
        not_found ->
            {value, Def};
        Value ->
            Value
    end.


%%--------------------------------------------------------------------
%% Function: service_names() -> [ServiceName]
%%  
%% ServiceName = atom()
%%
%% Description: Returns a list of supported services
%%-------------------------------------------------------------------
-doc """
Returns a list of available service names.
""".
-spec service_names() -> [inets_service()].
service_names() ->
    [httpc, httpd].


%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% enable_trace(Level, Destination, Service) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port | io | HandlerSpec
%% Service -> httpc | httpd | all
%% File -> string()
%% Port -> integer()
%% Verbosity -> true | false
%% HandlerSpec = {function(), Data}
%% Data = term()
%%
%% Description:
%% This function is used to start tracing at level Level and send
%% the result either to the file File, the port Port or to a 
%% trace handler. 
%% Note that it starts a tracer server.
%% When Destination is the atom io (or the tuple {io, Verbosity}),
%% all (printable) inets trace events (trace_ts events which has
%% Severity within Limit) will be written to stdout using io:format.
%%
%%-----------------------------------------------------------------
-doc false.
enable_trace(Level, Dest)          -> inets_trace:enable(Level, Dest).
-doc false.
enable_trace(Level, Dest, Service) -> inets_trace:enable(Level, Dest, Service).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%%
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------
-doc false.
disable_trace() -> inets_trace:disable().


%%-----------------------------------------------------------------
%% set_trace(Level) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started.
%%-----------------------------------------------------------------
-doc false.
set_trace(Level) -> inets_trace:set_level(Level).


%%-----------------------------------------------------------------
%% report_event(Severity, Label, Service, Content)
%%
%% Parameters:
%% Severity -> 0 =< integer() =< 100
%% Label -> string()
%% Service -> httpd | httpc
%% Content -> [{tag, term()}]
%%
%% Description:
%% This function is used to generate trace events, that is,  
%% put trace on this function.
%%-----------------------------------------------------------------

-doc false.
report_event(Severity, Label, Service, Content) ->
    inets_trace:report_event(Severity, Label, Service, Content).


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
start_service(Service, Args, stand_alone) -> 
    Service:start_standalone(Args);
start_service(Service, Args, inets) ->
    call_service(Service, start_service, Args).

call_service(Service, Call, Args) ->
    try Service:Call(Args) of
	Result ->
	    Result
    catch
        exit:{noproc, _} ->
            {error, inets_not_started}
    end.
