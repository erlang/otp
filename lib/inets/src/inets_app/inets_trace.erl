%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2011-2015. All Rights Reserved.
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
%% Purpose: Module for debug trace functions of the inets application
%%----------------------------------------------------------------------

-module(inets_trace).

%% API
-export([enable/2, enable/3, 
	 disable/0, 
	 set_level/1, set_level/2, 
	 report_event/4]).


%%====================================================================
%% API
%%====================================================================

%%-----------------------------------------------------------------
%% enable(Level, Destination) -> void()
%% enable(Level, Destination, Service) -> void()
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
enable(Level, Dest) ->
    enable(Level, Dest, all).

enable(Level, Dest, Service) ->
    case valid_trace_service(Service) of
	true ->
	    enable2(Level, Dest, Service);
	false ->
	    {error, {invalid_service, Service}}
    end.

enable2(Level, File, Service) 
  when is_list(File) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    HandleSpec = {fun handle_trace/2, {Service, Fd}},
	    do_enable(Level, process, HandleSpec);
	Err ->
	    Err
    end;
enable2(Level, Port, _) when is_integer(Port) ->
    do_enable(Level, port, dbg:trace_port(ip, Port));
enable2(Level, io, Service) ->
    HandleSpec = {fun handle_trace/2, {Service, standard_io}},
    do_enable(Level, process, HandleSpec);
enable2(Level, {Fun, _Data} = HandleSpec, _) when is_function(Fun) ->
    do_enable(Level, process, HandleSpec).

do_enable(Level, Type, HandleSpec) ->
    case dbg:tracer(Type, HandleSpec) of
	{ok, _} ->
	    set_level(Level),
	    ok;
	Error ->
	    Error
    end.    

valid_trace_service(all) ->
    true;
valid_trace_service(Service) ->
    lists:member(Service, [httpc, httpd, ftpc, tftp]).


%%-----------------------------------------------------------------
%% disable() -> void()
%%
%% Description:
%% This function is used to stop tracing.
%%-----------------------------------------------------------------

disable() ->
    %% This is to make handle_trace/2 close the output file (if the
    %% event gets there before dbg closes)
    inets_trace:report_event(100, "stop trace", stop_trace, [stop_trace]),  
    dbg:stop().



%%-----------------------------------------------------------------
%% set_level(Level) -> void()
%%
%% Parameters:
%% Level -> max | min | integer()
%%
%% Description:
%% This function is used to change the trace level when tracing has
%% already been started.
%%-----------------------------------------------------------------
set_level(Level) ->
    set_level(Level, all).

set_level(Level, Service) ->
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
    end.

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
%% Event -> The trace event
%% Fd -> standard_io | file_descriptor() | trace_port()
%%
%% Description:
%% This function is used to "receive" and pretty print the trace events.
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
    (catch io:format(standard_io, "stop trace at ~s~n",
                     [inets_lib:format_timestamp(Timestamp)])),
    Fd;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             standard_io = Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n",
                     [inets_lib:format_timestamp(Timestamp)])),
    Fd;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             {_Service, Fd}) ->
    (catch io:format(Fd, "stop trace at ~s~n",
                     [inets_lib:format_timestamp(Timestamp)])),
    (catch file:close(Fd)),
    closed_file;
handle_trace({trace_ts, _Who, call,
              {?MODULE, report_event,
               [_Sev, "stop trace", stop_trace, [stop_trace]]},
              Timestamp},
             Fd) ->
    (catch io:format(Fd, "stop trace at ~s~n",
                     [inets_lib:format_timestamp(Timestamp)])),
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
    Ts = inets_lib:format_timestamp(Timestamp),
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
    Ts = inets_lib:format_timestamp(When),
    io:format(Fd, "[trace ~s]"
              "~n   Who:   ~p"
              "~n   What:  ~p"
              "~n   Where: ~p"
              "~n", [Ts, Who, What, Where]);

do_print_trace(Fd, {trace_ts, Who, What, Where, Extra, When}) ->
    Ts = inets_lib:format_timestamp(When),
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
    Ts = inets_lib:format_timestamp(When),
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


