%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2010. All Rights Reserved.
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
%% Purpose: The main interface module of the inets application
%%----------------------------------------------------------------------

-module(inets).

%% API
-export([start/0, start/1, start/2, start/3,  
	 stop/0, stop/2, 
	 services/0, services_info/0,
	 service_names/0]).
-export([enable_trace/2, enable_trace/3, disable_trace/0, set_trace/1, 
	 report_event/4]).
-export([versions/0,
         print_version_info/0, print_version_info/1]).


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
start() -> 
    application:start(inets).

start(Type) -> 
    application:start(inets, Type).


%%--------------------------------------------------------------------
%% Function: start(Service, ServiceConfig [, How]) -> {ok, Pid} | 
%%                                                {error, Reason}
%%
%% Service = - ftpc | tftpd | httpc | httpd
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
%% distributed application. Nor will they be automaticly restarted
%% when the inets application is restarted, but as long as the inets
%% application is up and running they will be supervised and may be
%% soft code upgraded. Services started with the option stand alone,
%% e.i. the service is not started as part of the inets application,
%% will lose all OTP application benefits such as soft upgrade. The
%% stand alone service will be linked to the process that started it.
%% In most cases some of the supervison functionallity will still be
%% in place and in some sense the calling process has now become the
%% top supervisor.
%% --------------------------------------------------------------------
start(Service, ServiceConfig) ->
    Module = service_module(Service),
    start_service(Module, ServiceConfig, inets).

start(Service, ServiceConfig, How) ->
    Module = service_module(Service),
    start_service(Module, ServiceConfig, How).


%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
stop() -> 
    application:stop(inets).


%%--------------------------------------------------------------------
%% Function: stop(Service, Pid) -> ok
%%
%% Service - ftp | tftpd | http | httpd | stand_alone
%%
%% Description: Stops a started service of the inets application or takes
%% down a stand alone "service" gracefully.
%%--------------------------------------------------------------------
stop(stand_alone, Pid) ->
    true = exit(Pid, shutdown),
    ok;

stop(Service, Pid) ->
    Module = service_module(Service),
    call_service(Module, stop_service, Pid).


%%--------------------------------------------------------------------
%% Function: services() -> [{Service, Pid}]
%%
%% Description: Returns a list of currently running services. 
%% Note: Services started with the stand alone option will not be listed
%%--------------------------------------------------------------------
services() ->
    Modules = [service_module(Service) || Service <- 
					      service_names()],
    try lists:flatten(lists:map(fun(Module) ->
					Module:services()
				end, Modules)) of
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
services_info() ->
    case services() of
	{error, inets_not_started} ->
	    {error, inets_not_started};
	Services ->
	    Fun =  fun({Service, Pid}) -> 
			   Module = service_module(Service),
			   Info =  
			       case Module:service_info(Pid) of
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

print_version_info() ->
    {ok, Versions} = inets:versions(),
    print_version_info(Versions).

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
    {value, {time,       Time}}   = lists:keysearch(time,       1, Comp),
    {Mod, [{vsn,              Vsn},
           {app_vsn,          AppVsn},
           {compiler_version, Ver},
           {compile_time,     Time}]}.

sys_info() ->
    SysArch = string:strip(erlang:system_info(system_architecture),right,$\n),
    SysVer  = string:strip(erlang:system_info(system_version),right,$\n),
    [{arch, SysArch}, {ver, SysVer}].

os_info() ->
    V = os:version(),
    case os:type() of
        {OsFam, OsName} ->
            [{fam, OsFam}, {name, OsName}, {ver, V}];
        OsFam ->
            [{fam, OsFam}, {ver, V}]
    end.


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
    CompDate =
        case key1search(compile_time, Info) of
            {value, {Year, Month, Day, Hour, Min, Sec}} ->
                lists:flatten(
                  io_lib:format("~w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                                [Year, Month, Day, Hour, Min, Sec]));
            _ ->
                "Not found"
        end,
    io:format("   ~w:~n"
              "      Vsn:          ~s~n"
              "      App vsn:      ~s~n"
              "      ASN.1 vsn:    ~s~n"
              "      Compiler ver: ~s~n"
              "      Compile time: ~s~n",
              [Module, Vsn, AppVsn, Asn1Vsn, CompVer, CompDate]),
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
service_names() ->
    [ftpc, tftpd, httpc, httpd].


%%-----------------------------------------------------------------
%% enable_trace(Level, Destination) -> void()
%% enable_trace(Level, Destination, Service) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%% Destination -> File | Port | io | HandlerSpec
%% Service -> httpc | httpd | ftpc | tftp | all
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
%% Severity withing Limit) will be written to stdout using io:format.
%%
%%-----------------------------------------------------------------
enable_trace(Level, Dest) ->
    enable_trace(Level, Dest, all).

enable_trace(Level, Dest, Service) ->
    case valid_trace_service(Service) of
	true ->
	    enable_trace2(Level, Dest, Service);
	false ->
	    {error, {invalid_service, Service}}
    end.

enable_trace2(Level, File, Service) 
  when is_list(File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    HandleSpec = {fun handle_trace/2, {Service, Fd}},
	    do_enable_trace(Level, process, HandleSpec);
	Err ->
	    Err
    end;
enable_trace2(Level, Port, _) when is_integer(Port) ->
    do_enable_trace(Level, port, dbg:trace_port(ip, Port));
enable_trace2(Level, io, Service) ->
    HandleSpec = {fun handle_trace/2, {Service, standard_io}},
    do_enable_trace(Level, process, HandleSpec);
enable_trace2(Level, {Fun, _Data} = HandleSpec, _) when is_function(Fun) ->
    do_enable_trace(Level, process, HandleSpec).

do_enable_trace(Level, Type, HandleSpec) ->
    case dbg:tracer(Type, HandleSpec) of
	{ok, _} ->
	    set_trace(Level),
	    ok;
	Error ->
	    Error
    end.    

valid_trace_service(all) ->
    true;
valid_trace_service(Service) ->
    lists:member(Service, [httpc, httpd, ftpc, tftp]).


%%-----------------------------------------------------------------
%% disable_trace() -> void()
%%
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------
disable_trace() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    inets:report_event(100, "stop trace", stop_trace, [stop_trace]),  
    dbg:stop().



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
set_trace(Level) ->
    set_trace(Level, all).

set_trace(Level, Service) ->
    Pat = make_pattern(?MODULE, Service, Level),
    change_pattern(Pat).

make_pattern(Mod, Service, Level) 
  when is_atom(Mod) andalso is_atom(Service) ->
    case Level of
        min ->
            {Mod, Service, []};
        max ->
            Head = ['$1', '_', '_', '_'],
            Body = [],
            Cond = [],
            {Mod, Service, [{Head, Cond, Body}]};
        DetailLevel when is_integer(DetailLevel) ->
            Head = ['$1', '_', '_', '_'],
            Body = [],
            Cond = [{ '=<', '$1', DetailLevel}],
            {Mod, Service, [{Head, Cond, Body}]};
        _ ->
            exit({bad_level, Level})
    end.

change_pattern({Mod, Service, Pattern}) 
  when is_atom(Mod) andalso is_atom(Service) ->
    MFA = {Mod, report_event, 4},
    case Pattern of
        [] ->
	    try
		error_to_exit(ctp, dbg:ctp(MFA)),
		error_to_exit(p,   dbg:p(all, clear))
	    catch
		exit:{Where, Reason} ->
		    {error, {Where, Reason}}
	    end;
        List when is_list(List) ->
	    try
		error_to_exit(ctp, dbg:ctp(MFA)),
		error_to_exit(tp,  dbg:tp(MFA, Pattern)),
		error_to_exit(p,   dbg:p(all, [call, timestamp]))
	    catch
		exit:{Where, Reason} ->
		    {error, {Where, Reason}}
	    end
    end,
    ok.

error_to_exit(_Where, {ok, _} = OK) ->
    OK;
error_to_exit(Where, {error, Reason}) ->
    exit({Where, Reason}).


%%-----------------------------------------------------------------
%% report_event(Severity, Label, Service, Content)
%%
%% Parameters:
%% Severity -> 0 =< integer() =< 100
%% Label -> string()
%% Service -> httpd | httpc | ftp | tftp
%% Content -> [{tag, term()}]
%%
%% Description:
%% This function is used to generate trace events, that is,  
%% put trace on this function.
%%-----------------------------------------------------------------

report_event(Severity, Label, Service, Content) 
  when (is_integer(Severity) andalso 
	(Severity >= 0) andalso (100 >= Severity)) andalso 
       is_list(Label) andalso 
       is_atom(Service) andalso 
       is_list(Content) ->
    hopefully_traced.


%% ----------------------------------------------------------------------
%% handle_trace(Event, Fd) -> Verbosity
%%
%% Parameters:
%% Event -> The trace event (only megaco 'trace_ts' events are printed)
%% Fd -> standard_io | file_descriptor() | trace_port()
%%
%% Description:
%% This function is used to "receive" and print the trace events.
%% Events are printed if:
%%   - Verbosity is max
%%   - Severity is =< Verbosity (e.g. Severity = 30, and Verbosity = 40)
%% Events are not printed if:
%%   - Verbosity is min
%%   - Severity is > Verbosity
%%-----------------------------------------------------------------

handle_trace(_, closed_file = Fd) ->
    Fd;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             {_, standard_io} = Fd) ->
    (catch io:format(standard_io, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    Fd;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             standard_io = Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    Fd;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             {_Service, Fd}) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n", [format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, Who, call,
              {?MODULE, report_event,
               [Sev, Label, Service, Content]}, Timestamp},
             Fd) ->
    (catch print_inets_trace(Fd, Sev, Timestamp, Who, 
			     Label, Service, Content)),
    Fd;
handle_trace(Event, Fd) ->
    (catch print_trace(Fd, Event)),
    Fd.


print_inets_trace({Service, Fd}, 
		  Sev, Timestamp, Who, Label, Service, Content) ->
    do_print_inets_trace(Fd, Sev, Timestamp, Who, Label, Service, Content);
print_inets_trace({ServiceA, Fd}, 
		  Sev, Timestamp, Who, Label, ServiceB, Content) 
  when (ServiceA =:= all) ->
    do_print_inets_trace(Fd, Sev, Timestamp, Who, Label, ServiceB, Content);
print_inets_trace({ServiceA, _Fd}, 
		  _Sev, _Timestamp, _Who, _Label, ServiceB, _Content) 
  when ServiceA =/= ServiceB ->
    ok;
print_inets_trace(Fd, Sev, Timestamp, Who, Label, Service, Content) ->
    do_print_inets_trace(Fd, Sev, Timestamp, Who, Label, Service, Content).

do_print_inets_trace(Fd, Sev, Timestamp, Who, Label, Service, Content) ->
    Ts = format_timestamp(Timestamp),
    io:format(Fd, "[inets ~w trace ~w ~w ~s] ~s "
              "~n   Content: ~p"
              "~n",
              [Service, Sev, Who, Ts, Label, Content]).

print_trace({_, Fd}, Event) ->
    do_print_trace(Fd, Event);
print_trace(Fd, Event) ->
    do_print_trace(Fd, Event).

do_print_trace(Fd, {trace, Who, What, Where}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Who, What, Where]);

do_print_trace(Fd, {trace, Who, What, Where, Extra}) ->
    io:format(Fd, "[trace]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Who, What, Where, Extra]);

do_print_trace(Fd, {trace_ts, Who, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Ts, Who, What, Where]);

do_print_trace(Fd, {trace_ts, Who, What, Where, Extra, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n   Extra: ~p"
              "~n", [Ts, Who, What, Where, Extra]);

do_print_trace(Fd, {seq_trace, What, Where}) ->
    io:format(Fd, "[seq trace]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [What, Where]);

do_print_trace(Fd, {seq_trace, What, Where, When}) ->
    Ts = format_timestamp(When),
    io:format(Fd, "[seq trace ~s]"
              "~n   What:       ~p"
              "~n   Where:      ~p"
              "~n", [Ts, What, Where]);

do_print_trace(Fd, {drop, Num}) ->
    io:format(Fd, "[drop trace] ~p~n", [Num]);

do_print_trace(Fd, Trace) ->
    io:format(Fd, "[trace] "
              "~n   ~p"
              "~n", [Trace]).


format_timestamp({_N1, _N2, N3} = Now) ->
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate =
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),
    lists:flatten(FormatDate).


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
	
service_module(tftpd) ->
    tftp;
service_module(httpc) ->
    httpc;
service_module(ftpc) ->
    ftp;
service_module(Service) ->
    Service.






