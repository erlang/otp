%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2000-2017. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% Purpose: Collect trace events and provide a backing storage
%%          appropriate for iteration 
%%----------------------------------------------------------------------

-module(et_collector).

-behaviour(gen_server).

%% External exports
-export([start_link/1, 
         stop/1,

         report/2, 
         report_event/5, 
         report_event/6, 

         iterate/3,
         iterate/5,
	 lookup/2,

         start_trace_client/3, 
         start_trace_port/1, 
         %% load_event_file/2, 
         save_event_file/3,
         clear_table/1,

         get_global_pid/0, 
         %% get_table_handle/1,
	 get_table_size/1,
         change_pattern/2,
         make_key/2,

         dict_insert/3, 
         dict_delete/2, 
         dict_lookup/2, 
         dict_match/2,
         multicast/2]).

%% Internal export
-export([monitor_trace_port/2]).

%% gen_server callbacks
-export([init/1,terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-compile([{nowarn_deprecated_function,[{erlang,now,0}]}]).

-include("et_internal.hrl").
-include("../include/et.hrl").

-record(state, {parent_pid,
		auto_shutdown, % Optionally shutdown when the last subscriber dies 
		event_tab_size,
                event_tab,
                dict_tab,
                event_order,
                subscribers,
                file, 
                trace_pattern,
                trace_port,
                trace_max_queue,
                trace_nodes,
                trace_global}).

-record(file, {name, desc, event_opt, file_opt, table_opt}).

-record(table_handle, {collector_pid, event_tab, event_order, filter}).

-record(trace_ts, {trace_ts, event_ts}).
-record(event_ts, {event_ts, trace_ts}).

%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% start_link(Options) -> {ok, CollectorPid} | {error, Reason}
%%
%% Start a collector process
%%
%% The collector collects trace events and keeps them ordered by their
%% timestamp. The timestamp may either reflect the time when the
%% actual trace data was generated (trace_ts) or when the trace data
%% was transformed into an event record (event_ts). If the time stamp
%% is missing in the trace data (missing timestamp option to
%% erlang:trace/4) the trace_ts will be set to the event_ts.
%% 
%% Events are reported to the collector directly with the report
%% function or indirectly via one or more trace clients. All reported
%% events are first filtered thru the collector filter before they are
%% stored by the collector. By replacing the default collector filter
%% with a customized dito it is possible to allow any trace data as
%% input. The collector filter is a dictionary entry with the
%% predefined key {filter, all} and the value is a fun of
%% arity 1. See et_selector:parse_event/2 for interface details,
%% such as which erlang:trace/1 tuples that are accepted.
%%
%% The collector has a built-in dictionary service. Any term may be
%% stored as value in the dictionary and bound to a unique key. When
%% new values are inserted with an existing key, the new values will
%% overwrite the existing ones. Processes may subscribe on dictionary
%% updates by using {subscriber, pid()} as dictionary key. All
%% dictionary updates will be propagated to the subscriber processes
%% matching the pattern {{subscriber, '_'}, '_'} where the first '_'
%% is interpreted as a pid().
%%
%% In global trace mode, the collector will automatically start
%% tracing on all connected Erlang nodes. When a node connects, a port
%% tracer will be started on that node and a corresponding trace
%% client on the collector node. By default the global trace pattern
%% is 'max'.
%%
%% Options = [option()]
%% 
%% option() =
%%   {parent_pid, pid()} |
%%   {event_order, event_order()} |
%%   {dict_insert, {filter, all}, collector_fun()} |
%%   {dict_insert, {filter, event_filter_name()}, event_filter_fun()} |
%%   {dict_insert, {subscriber, pid()}, dict_val()} |
%%   {dict_insert, dict_key(), dict_val()} |
%%   {dict_delete, dict_key()} |
%%   {trace_client, trace_client()} |
%%   {trace_global, boolean()} | 
%%   {trace_pattern, trace_pattern()} |
%%   {trace_port, integer()} | 
%%   {trace_max_queue, integer()}
%%   
%% event_order() = trace_ts | event_ts
%% trace_pattern() = detail_level() | dbg_match_spec()
%% detail_level() = min | max | integer(X) when X >= 0, X =< 100
%% trace_client() = 
%%   {event_file, file_name()} |
%%   {dbg_trace_type(), dbg_trace_parameters()}
%% file_name() = string()
%% collector_fun() = trace_filter_fun() | event_filter_fun()
%% trace_filter_fun() = fun(TraceData) -> false | true | {true, NewEvent}
%% event_filter_fun() = fun(Event) -> false | true | {true, NewEvent}
%% event_filter_name() = atom()
%% TraceData = erlang_trace_data()
%% Event = NewEvent = record(event)
%% dict_key() = term()
%% dict_val() = term()
%%
%% CollectorPid = pid()
%% Reason = term()
%%----------------------------------------------------------------------

start_link(Options) ->
    case parse_opt(Options, default_state(), [], []) of
	{ok, S, Dict2, Clients} ->
	    Res = 
		case S#state.trace_global of
		    false -> 
			gen_server:start_link(?MODULE, [S, Dict2], []);
		    true ->
			gen_server:start_link({global, ?MODULE}, ?MODULE, [S, Dict2], [])
		end,
            case Res of
                {ok, Pid} when S#state.parent_pid =/= self() ->
                    unlink(Pid),
                    start_clients(Pid, Clients);
                {ok,Pid} ->
                    start_clients(Pid, Clients);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

default_state() ->
    #state{parent_pid      = self(),
	   auto_shutdown   = false,
           event_order     = trace_ts,
           subscribers     = [],
           trace_global    = false,
           trace_pattern   = undefined,
           trace_nodes     = [],
           trace_port      = 4711,
           trace_max_queue = 50}.

parse_opt([], S, Dict, Clients) ->
    {Mod, Pattern} = et_selector:make_pattern(S#state.trace_pattern),
    Fun = fun(E) -> et_selector:parse_event(Mod, E) end,
    Default = {dict_insert, {filter, ?DEFAULT_FILTER_NAME}, Fun},
    {ok, S#state{trace_pattern = {Mod, Pattern}}, [Default | Dict], Clients};
parse_opt([H | T], S, Dict, Clients) ->
    case H of
        {parent_pid, Parent} when Parent =:= undefined ->
            parse_opt(T, S#state{parent_pid = Parent}, Dict, Clients);
        {parent_pid, Parent} when is_pid(Parent) ->
            parse_opt(T, S#state{parent_pid = Parent}, Dict, Clients);
        {auto_shutdown, Bool} when Bool =:= true; Bool =:= false ->
            parse_opt(T, S#state{auto_shutdown = Bool}, Dict, Clients);
        {event_order, Order} when Order =:= trace_ts ->
            parse_opt(T, S#state{event_order = Order}, Dict, Clients);
        {event_order, Order}  when Order =:= event_ts ->
            parse_opt(T, S#state{event_order = Order}, Dict, Clients);
        {dict_insert, {filter, Name}, Fun} ->
	    if
		is_atom(Name), is_function(Fun) ->
		    parse_opt(T, S, Dict ++ [H], Clients);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, {subscriber, Pid}, _Val} ->
	    if
		is_pid(Pid) ->
		    parse_opt(T, S, Dict ++ [H], Clients);
		true ->
	            {error, {bad_option, H}}
	    end;
        {dict_insert, _Key, _Val} ->
            parse_opt(T, S, Dict ++ [H], Clients);
        {dict_delete, _Key} ->
            parse_opt(T, S, Dict ++ [H], Clients);
        {trace_client, Client = {_, _}} ->
            parse_opt(T, S, Dict, Clients ++ [Client]);
        {trace_global, Bool} when Bool =:= false ->
            parse_opt(T, S#state{trace_global = Bool}, Dict, Clients);
        {trace_global, Bool} when Bool =:= true ->
            parse_opt(T, S#state{trace_global = Bool}, Dict, Clients);
        {trace_pattern, {Mod, _} = Pattern} when is_atom(Mod) ->
            parse_opt(T, S#state{trace_pattern = Pattern}, Dict, Clients);
        {trace_pattern, undefined = Pattern} ->
            parse_opt(T, S#state{trace_pattern = Pattern}, Dict, Clients);
        {trace_port, Port} when is_integer(Port) ->
            parse_opt(T, S#state{trace_port = Port}, Dict, Clients);
        {trace_max_queue, MaxQueue} when is_integer(MaxQueue) ->
            parse_opt(T, S#state{trace_port = MaxQueue}, Dict, Clients);
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, _S, _Dict, _Clients) ->
    {error, {bad_option_list, BadList}}.

start_clients(CollectorPid, [{Type, Parameters} | T]) ->
    _ = start_trace_client(CollectorPid, Type, Parameters),
    start_clients(CollectorPid, T);
start_clients(CollectorPid, []) ->
    {ok, CollectorPid}.

%%----------------------------------------------------------------------
%% stop(CollectorPid) -> ok
%%
%% Stop a collector process
%%
%% CollectorPid = pid()
%%----------------------------------------------------------------------

stop(CollectorPid) ->
    call(CollectorPid, stop).

%%----------------------------------------------------------------------
%% save_event_file(CollectorPid, FileName, Options) -> ok | {error, Reason}
%%
%% Saves the events to a file
%% 
%% CollectorPid = pid()
%% FileName = string()
%% Options = [option()]
%% Reason = term()
%%
%% option() = event_option() | file_option() | table_option()
%% event_option() = existing
%% file_option() = write | append
%% table_option() = keep | clear
%%
%% By default the currently stored events (existing) are
%% written to a brand new file (write) and the events are
%% kept (keep) after they have been written to the file.
%%
%% Instead of keeping the events after writing them to file,
%% it is possible to remove all stored events after they
%% have successfully written to file (clear).
%% 
%% The options defaults to existing, write and keep.
%%----------------------------------------------------------------------

save_event_file(CollectorPid, FileName, Options) ->
    call(CollectorPid, {save_event_file, FileName, Options}).

%%----------------------------------------------------------------------
%% load_event_file(CollectorPid, FileName) ->{ok, BadBytes} | exit(Reason)
%%
%% Load the event table from a file
%% 
%% CollectorPid = pid()
%% FileName = string()
%% BadBytes = integer(X) where X >= 0
%% Reason = term()
%%----------------------------------------------------------------------

load_event_file(CollectorPid, FileName) ->
    Fd = make_ref(),
    Args = [{file, FileName}, {name, Fd}, {repair, true}, {mode, read_only}],
    Fun = fun(Event, {ok, TH}) -> report(TH, Event) end,
    case disk_log:open(Args) of
        {ok, _} ->
            do_load_event_file(Fun, Fd, start, {ok, CollectorPid}, FileName, 0);
        {repaired, _, _, BadBytes} ->
            do_load_event_file(Fun, Fd, start, {ok, CollectorPid}, FileName, BadBytes);
        {error, Reason} ->
            exit({disk_log_open, FileName, Reason})
    end.

do_load_event_file(Fun, Fd, Cont, Acc, FileName, BadBytes) ->
    case disk_log:chunk(Fd, Cont) of
        eof ->
            {ok, BadBytes};
        {error, Reason} ->
            exit({bad_disk_log_chunk, FileName, Reason});
        {Cont2, Events} ->
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_load_event_file(Fun, Fd, Cont2, Acc2, FileName, BadBytes);
        {Cont2, Events, More} ->
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_load_event_file(Fun, Fd, Cont2, Acc2, FileName, BadBytes + More)
    end.

%%----------------------------------------------------------------------
%% report(Handle, TraceOrEvent)
%%
%% Report an event to the collector
%%
%% All events are filtered thru the collector filter, which
%% optionally may transform or discard the event. The first
%% call should use the pid of the collector process as
%% report handle, while subsequent calls should use the
%% table handle.
%%
%% Handle = Initial | Continuation
%% Initial = collector_pid()
%% collector_pid() = pid()
%% Continuation = record(table_handle)
%%
%% TraceOrEvent = record(event) | dbg_trace_tuple() | end_of_trace
%% Reason = term()
%% 
%% Returns: {ok, Continuation} | exit(Reason)
%%----------------------------------------------------------------------

report(CollectorPid, TraceOrEvent) when is_pid(CollectorPid) ->
    case get_table_handle(CollectorPid) of
        {ok, TH} when is_record(TH, table_handle) ->
            report(TH, TraceOrEvent);
        {error, Reason} ->
            exit(Reason)
    end;
report(TH, TraceOrEvent) when is_record(TH, table_handle) ->
    Fun = TH#table_handle.filter,
    case Fun(TraceOrEvent) of
        false ->
            {ok, TH};
        true when is_record(TraceOrEvent, event) ->
            Key = make_key(TH, TraceOrEvent),
            case catch ets:insert(TH#table_handle.event_tab, {Key, TraceOrEvent}) of
                true ->
                    {ok, TH};
                {'EXIT', _Reason} ->
                    %% Refresh the report handle and try again
                    report(TH#table_handle.collector_pid, TraceOrEvent)
            end;
        {true, Event} when is_record(Event, event) ->
            Key = make_key(TH, Event),
            case catch ets:insert(TH#table_handle.event_tab, {Key, Event}) of
                true ->
                    {ok, TH};
                {'EXIT', _Reason} ->
                    %% Refresh the report handle and try again
                    report(TH#table_handle.collector_pid, TraceOrEvent)
            end;
        BadEvent ->
            TS = erlang:now(),
            Contents = [{trace, TraceOrEvent}, {reason, BadEvent}, {filter, Fun}],
            Event = #event{detail_level = 0,
                           trace_ts     = TS,
                           event_ts     = TS,
                           from         = bad_filter,
                           to           = bad_filter,
                           label        = bad_filter,
                           contents     = Contents},
            Key = make_key(TH, Event),
            case catch ets:insert(TH#table_handle.event_tab, {Key, Event}) of
                true ->
                    {ok, TH};
                {'EXIT', _Reason} ->
                    %% Refresh the report handle and try again
                    report(TH#table_handle.collector_pid, TraceOrEvent)
            end
    end;
report(_, Bad) ->
    exit({bad_event, Bad}).

report_event(CollectorPid, DetailLevel, FromTo, Label, Contents) ->
    report_event(CollectorPid, DetailLevel, FromTo, FromTo, Label, Contents).

report_event(CollectorPid, DetailLevel, From, To, Label, Contents)
  when is_integer(DetailLevel), 
       DetailLevel >= ?detail_level_min,
       DetailLevel =< ?detail_level_max ->
    TS= erlang:now(),
    E = #event{detail_level = DetailLevel,
               trace_ts     = TS,
               event_ts     = TS,
               from         = From,
               to           = To, 
               label        = Label, 
               contents     = Contents},
    report(CollectorPid, E).

%%----------------------------------------------------------------------
%% make_key(Type, Stuff) -> Key
%%
%% Makes a key out of an event record or an old key
%%
%% Type = record(table_handle) | trace_ts | event_ts
%% Stuff = record(event) | Key
%% Key = record(event_ts) | record(trace_ts)
%%----------------------------------------------------------------------

make_key(TH, Stuff) when is_record(TH, table_handle) ->
    make_key(TH#table_handle.event_order, Stuff);
make_key(trace_ts, Stuff) ->
    if
        is_record(Stuff, event) ->
            #event{trace_ts = R, event_ts = P} = Stuff,
            #trace_ts{trace_ts = R, event_ts = P};
        is_record(Stuff, trace_ts) ->
            Stuff;
        is_record(Stuff, event_ts) ->
            #event_ts{trace_ts = R, event_ts = P} = Stuff,
            #trace_ts{trace_ts = R, event_ts = P}
    end;
make_key(event_ts, Stuff) ->
    if
        is_record(Stuff, event) ->
            #event{trace_ts = R, event_ts = P} = Stuff,
            #event_ts{trace_ts = R, event_ts = P};
        is_record(Stuff, event_ts) ->
            Stuff;
        is_record(Stuff, trace_ts) ->
            #trace_ts{trace_ts = R, event_ts = P} = Stuff,
            #event_ts{trace_ts = R, event_ts = P}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

get_table_size(CollectorPid) when is_pid(CollectorPid) ->
    call(CollectorPid, get_table_size).

%%----------------------------------------------------------------------
%% get_table_handle(CollectorPid) -> Handle
%%
%% Return a table handle
%%
%% CollectorPid = pid()
%% Handle = record(table_handle)
%%----------------------------------------------------------------------

get_table_handle(CollectorPid) when is_pid(CollectorPid) ->
    call(CollectorPid, get_table_handle).

%%----------------------------------------------------------------------
%% get_global_pid() -> CollectorPid | exit(Reason)
%%
%% Return a the identity of the globally registered collector
%% if there is any
%%
%% CollectorPid = pid()
%% Reason = term()
%%----------------------------------------------------------------------

get_global_pid() ->
    case global:whereis_name(?MODULE) of
        CollectorPid when is_pid(CollectorPid) ->
            CollectorPid;
        undefined ->
            exit(global_collector_not_started)
    end.

%%----------------------------------------------------------------------
%% change_pattern(CollectorPid, RawPattern) -> {old_pattern, TracePattern}
%%
%% Change active trace pattern globally on all trace nodes
%%
%% CollectorPid = pid()
%% RawPattern = {report_module(), extended_dbg_match_spec()}
%% report_module() = atom() | undefined
%% extended_dbg_match_spec()() = detail_level() | dbg_match_spec()
%% RawPattern = detail_level()
%% detail_level() = min | max | integer(X) when X =< 0, X >= 100
%% TracePattern = {report_module(), dbg_match_spec_match_spec()}
%%----------------------------------------------------------------------

change_pattern(CollectorPid, RawPattern) ->
    Pattern = et_selector:make_pattern(RawPattern),
    call(CollectorPid, {change_pattern, Pattern}).

%%----------------------------------------------------------------------
%% dict_insert(CollectorPid, {filter, all}, FilterFun) -> ok
%% dict_insert(CollectorPid, {subscriber, SubscriberPid}, Void) -> ok
%% dict_insert(CollectorPid, Key, Val) -> ok
%% 
%% Insert a dictionary entry
%% and send a {et, {dict_insert, Key, Val}} tuple
%% to all registered subscribers.
%%
%% If the entry is a new subscriber, it will imply that
%% the new subscriber process first will get one message
%% for each already stored dictionary entry, before it
%% and all old subscribers will get this particular entry.
%% The collector process links to and then supervises the
%% subscriber process. If the subscriber process dies it
%% will imply that it gets unregistered as with a normal
%% dict_delete/2.
%%
%% CollectorPid = pid()
%% FilterFun = filter_fun() 
%% SubscriberPid = pid()
%% Void = term()
%% Key = term()
%% Val = term()
%%----------------------------------------------------------------------

dict_insert(CollectorPid, Key = {filter, Name}, Fun) ->
    if
	is_atom(Name), is_function(Fun) ->
	    call(CollectorPid, {dict_insert, Key, Fun});
	true ->
	    exit({badarg, Key})
    end;
dict_insert(CollectorPid, Key = {subscriber, Pid}, Val) ->
    if
	is_pid(Pid) ->
	    call(CollectorPid, {dict_insert, Key, Val});
	true ->
	    exit({badarg, Key})
    end;
dict_insert(CollectorPid, Key, Val) ->
    call(CollectorPid, {dict_insert, Key, Val}).

%%----------------------------------------------------------------------
%% dict_lookup(CollectorPid, Key) -> [Val]
%%
%% Lookup a dictionary entry and return zero or one value
%% 
%% CollectorPid = pid()
%% Key = term()
%% Val = term()
%%----------------------------------------------------------------------

dict_lookup(CollectorPid, Key) ->
    call(CollectorPid, {dict_lookup, Key}).

%%----------------------------------------------------------------------
%% Ddict_delete(CollectorPid, Key) -> ok
%%
%% elete a dictionary entry
%% and send a {et, {dict_delete, Key}} tuple
%% to all registered subscribers.
%%
%% If the deleted entry is a registered subscriber, it will
%% imply that the subscriber process gets is unregistered as
%% subscriber as well as it gets it final message.
%%
%% dict_delete(CollectorPid, {subscriber, SubscriberPid})
%% dict_delete(CollectorPid, Key)
%% 
%% CollectorPid = pid()
%% SubscriberPid = pid()
%% Key = term()
%%----------------------------------------------------------------------

dict_delete(CollectorPid, Key) ->
    call(CollectorPid, {dict_delete, Key}).

%%----------------------------------------------------------------------
%% dict_match(CollectorPid, Pattern) -> [Match]
%%
%% Match some dictionary entries
%% 
%% CollectorPid = pid()
%% Pattern = '_' | {key_pattern(), val_pattern()}
%% key_pattern() = ets_match_object_pattern()
%% val_pattern() = ets_match_object_pattern()
%% Match = {key(), val()}
%% key() = term()
%% val() = term()
%%----------------------------------------------------------------------

dict_match(CollectorPid, Pattern)  ->
    call(CollectorPid, {dict_match, Pattern}).

%%----------------------------------------------------------------------
%% multicast(_CollectorPid, Msg) -> ok
%%
%% Sends a message to all registered subscribers
%%
%% CollectorPid = pid()
%% Msg = term()
%%----------------------------------------------------------------------

multicast(_CollectorPid, Msg = {dict_insert, _Key, _Val}) ->
    exit({badarg, Msg});
multicast(_CollectorPid, Msg = {dict_delete, _Key}) ->
    exit({badarg, Msg});
multicast(CollectorPid, Msg) ->
    call(CollectorPid, {multicast, Msg}).

%%----------------------------------------------------------------------
%% start_trace_client(CollectorPid, Type, Parameters) ->
%%     file_loaded | {trace_client_pid, pid()} | exit(Reason)
%%
%% Load raw Erlang trace from a file, port or process.
%% 
%% Type       = dbg_trace_client_type()
%% Parameters = dbg_trace_client_parameters()
%% Pid        = dbg_trace_client_pid()
%%----------------------------------------------------------------------

start_trace_client(CollectorPid, Type, FileName) when Type =:= event_file ->
    load_event_file(CollectorPid, FileName);
start_trace_client(CollectorPid, Type, FileName) when Type =:= file -> 
    WaitFor = {make_ref(), end_of_trace},
    EventFun = fun(E, {ReplyTo, {ok, TH}}) -> {ReplyTo, report(TH, E)} end,
    EndFun = fun({ReplyTo, {ok, _TH}}) -> ReplyTo ! WaitFor, ReplyTo  end,
    Spec = trace_spec_wrapper(EventFun, EndFun, {self(), {ok, CollectorPid}}),
    Pid = dbg:trace_client(Type, FileName, Spec),
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    receive
        WaitFor -> 
	    erlang:demonitor(Ref, [flush]),
	    file_loaded;
        {'DOWN', Ref, _, _, Reason} ->
            exit(Reason)
    end;
start_trace_client(CollectorPid, Type, Parameters) ->
    EventFun = fun(Event, {ok, TH}) -> report(TH, Event) end,
    EndFun   = fun(Acc) -> Acc end,
    Spec = trace_spec_wrapper(EventFun, EndFun, {ok, CollectorPid}),
    Pid = dbg:trace_client(Type, Parameters, Spec),
    CollectorPid ! {register_trace_client, Pid},
    unlink(Pid),
    {trace_client_pid, Pid}.

trace_spec_wrapper(EventFun, EndFun, EventInitialAcc)
  when is_function(EventFun), is_function(EndFun) ->
    {fun(Trace, Acc) -> 
             case Trace =:= end_of_trace of
                 true  -> EndFun(Acc);
                 false -> EventFun(Trace,  Acc)
             end
     end,
     EventInitialAcc}.

start_trace_port(Parameters) ->
    dbg:tracer(port, dbg:trace_port(ip, Parameters)).

monitor_trace_port(CollectorPid, Parameters) ->
    Res = start_trace_port(Parameters),
    spawn(fun() ->
		  MonitorRef = erlang:monitor(process, CollectorPid),
		  receive
		      {'DOWN', MonitorRef, _, _, _} ->
			  dbg:stop_clear()
		  end
	  end),
    Res.

%%----------------------------------------------------------------------
%% iterate(Handle, Prev, Limit) ->
%%     iterate(Handle, Prev, Limit, undefined, Prev)
%%     
%% Iterates over the currently stored events
%% 
%% Short for iterate/5.
%%----------------------------------------------------------------------

iterate(Handle, Prev, Limit) ->
    iterate(Handle, Prev, Limit, undefined, Prev).

%%----------------------------------------------------------------------
%% iterate(Handle, Prev, Limit, Fun, Acc) -> NewAcc
%%
%% Iterates over the currently stored events and apply a function for
%% each event. The iteration may be performed forwards or backwards
%% and may be limited to a maximum number of events (abs(Limit)).
%%
%% Handle = collector_pid() | table_handle()
%% Prev = first | last | event_key()
%% Limit = done() | forward() | backward()
%% collector_pid() = pid()
%% table_handle() = record(table_handle)
%% event_key() = 
%% done() = 0
%% forward() = infinity | integer(X) where X > 0
%% backward() = '-infinity' | integer(X) where X < 0
%% Fun = fun(Event, Acc) -> NewAcc
%% Acc = NewAcc = term()
%%----------------------------------------------------------------------

iterate(_, _, Limit, _, Acc) when Limit =:= 0 ->
    Acc;
iterate(CollectorPid, Prev, Limit, Fun, Acc) when is_pid(CollectorPid) ->
    case get_table_handle(CollectorPid) of
        {ok, TH} when is_record(TH, table_handle) ->
            iterate(TH, Prev, Limit, Fun, Acc);
        {error, Reason} ->
            exit(Reason)
    end;
iterate(TH, Prev, Limit, Fun, Acc) when is_record(TH, table_handle) ->
    if
        Limit =:= infinity ->
            next_iterate(TH, Prev, Limit, Fun, Acc);
        is_integer(Limit), Limit > 0 ->
            next_iterate(TH, Prev, Limit, Fun, Acc);
        Limit =:= '-infinity' ->
            prev_iterate(TH, Prev, Limit, Fun, Acc);
        is_integer(Limit), Limit < 0 ->
            prev_iterate(TH, Prev, Limit, Fun, Acc)
    end.    
    
next_iterate(TH, Prev = first, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:first(Tab) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): First ~tp~n", [?MODULE, ?LINE, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        First ->
            lookup_and_apply(TH, Prev, First, Limit, -1, Fun, Acc)
    end;
next_iterate(TH, Prev = last, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:last(Tab) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): Last ~tp~n", [?MODULE, ?LINE, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        Last ->
            lookup_and_apply(TH, Prev, Last, Limit, -1, Fun, Acc)
    end;
next_iterate(TH, Prev, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    Key = make_key(TH, Prev),
    case catch ets:next(Tab, Key) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): Next ~tp -> ~tp~n", [?MODULE, ?LINE, Key, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        Next ->
            lookup_and_apply(TH, Prev, Next, Limit, -1, Fun, Acc)
    end.

prev_iterate(TH, Prev = first, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:first(Tab) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): First ~tp~n", [?MODULE, ?LINE, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        First ->
            lookup_and_apply(TH, Prev, First, Limit, 1, Fun, Acc)
    end;
prev_iterate(TH, Prev = last, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:last(Tab) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): Last ~tp~n", [?MODULE, ?LINE, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        Last ->
            lookup_and_apply(TH, Prev, Last, Limit, 1, Fun, Acc)
    end;
prev_iterate(TH, Prev, Limit, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    Key = make_key(TH, Prev),
    case catch ets:prev(Tab, Key) of
        '$end_of_table' ->
            Acc;
        {'EXIT', _} = Error ->
            io:format("~p(~p): Prev ~tp -> ~tp~n", [?MODULE, ?LINE, Key, Error]),
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        Next ->
            lookup_and_apply(TH, Prev, Next, Limit, 1, Fun, Acc)
    end.

lookup_and_apply(TH, _Prev, Next, Limit, Incr, Fun, _Acc) when Fun =:= undefined ->
    Limit2 = incr(Limit, Incr),
    iterate(TH, Next, Limit2, Fun, Next);   
lookup_and_apply(TH, Prev, Next, Limit, Incr, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:lookup_element(Tab, Next, 2) of
        {'EXIT', _} ->
            iterate(TH#table_handle.collector_pid, Prev, Limit, Fun, Acc);
        E when is_record(E, event) ->
            Acc2 = Fun(E, Acc),
            Limit2 = incr(Limit, Incr),
            iterate(TH, Next, Limit2, Fun, Acc2)
    end.

lookup(CollectorPid, Key) when is_pid(CollectorPid) ->
    case get_table_handle(CollectorPid) of
        {ok, TH} when is_record(TH, table_handle) ->
            lookup(TH, Key);
        {error, Reason} ->
            {error, Reason}
    end;
lookup(TH, Key) when is_record(TH, table_handle) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:lookup_element(Tab, Key, 2) of
        {'EXIT', _} ->
            {error, enoent};
        E when is_record(E, event) ->
	    {ok, E}
    end. 

incr(Val, Incr) ->
    if
	Val =:= infinity    -> Val;
	Val =:= '-infinity' -> Val;
	is_integer(Val)       -> Val + Incr
    end.

%%----------------------------------------------------------------------
%% clear_table(Handle) -> ok
%%
%% Clear the event table
%%
%% Handle = collector_pid() | table_handle()
%% collector_pid() = pid()
%% table_handle() = record(table_handle)
%%----------------------------------------------------------------------

clear_table(CollectorPid) when is_pid(CollectorPid) ->
    call(CollectorPid, clear_table);
clear_table(TH) when is_record(TH, table_handle) ->
    clear_table(TH#table_handle.collector_pid).

call(CollectorPid, Request) ->
    try
	gen_server:call(CollectorPid, Request, infinity)
    catch
	exit:{noproc,_} ->
	    {error, no_collector}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([InitialS, Dict]) ->
    process_flag(trap_exit, true),
    case InitialS#state.parent_pid of
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    link(Pid)
    end,
    Funs = [fun init_tables/1,
            fun init_global/1,
            fun(S) -> lists:foldl(fun do_dict_insert/2, S, Dict) end],
    {ok, lists:foldl(fun(F, S) -> F(S) end, InitialS, Funs)}.

init_tables(S) -> 
    EventTab = ets:new(et_events, [ordered_set, {keypos, 1}, public]),
    DictTab  = ets:new(et_dict,   [ordered_set, {keypos, 1}, public]),
    S#state{event_tab = EventTab, dict_tab = DictTab, event_tab_size = 0}.

init_global(S) -> 
    case S#state.trace_global of
        true ->
            EventFun = fun(Event, {ok, TH}) -> report(TH, Event) end,
            EndFun = fun(Acc) -> Acc end,
            Spec = trace_spec_wrapper(EventFun, EndFun, {ok, self()}),
            dbg:tracer(process, Spec),
            et_selector:change_pattern(S#state.trace_pattern),
            ok = net_kernel:monitor_nodes(true),
            lists:foreach(fun(N) -> self() ! {nodeup, N} end, nodes()),
            S#state{trace_nodes = [node()]};
        false ->
            S
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({multicast, Msg}, _From, S) ->
    do_multicast(S#state.subscribers, Msg),
    reply(ok, S);

handle_call(Msg = {dict_insert, _Key, _Val}, _From, S) ->
    S2 = do_dict_insert(Msg, S),
    reply(ok, S2);

handle_call(Msg = {dict_delete, _Key}, _From, S) ->
    try
	S2 = do_dict_delete(Msg, S),
	reply(ok, S2)
    catch
	throw:{stop, R} ->
	    opt_unlink(S#state.parent_pid),
	    {stop, R, S}
    end;
handle_call({dict_lookup, Key}, _From, S) ->
    Reply = ets:lookup(S#state.dict_tab, Key),
    reply(Reply, S);

handle_call({dict_match, Pattern}, _From, S) ->
    case catch ets:match_object(S#state.dict_tab, Pattern) of
        {'EXIT', _Reason} ->
            reply([], S);
        Matching ->
            reply(Matching, S)
    end;

handle_call(get_table_handle, _From, S) ->
    [{_, TableFilter}] = ets:lookup(S#state.dict_tab, {filter, ?DEFAULT_FILTER_NAME}),
    TH = #table_handle{collector_pid = self(),
                       event_tab     = S#state.event_tab,
                       event_order   = S#state.event_order,
                       filter        = TableFilter},
    reply({ok, TH}, S);

handle_call(get_table_size, _From, S) ->
    Size = ets:info(S#state.event_tab, size),
    reply({ok, Size}, S);

handle_call(close, _From, S) ->
    case S#state.file of
        undefined ->
            reply({error, file_not_open}, S);
        F ->
            Reply = disk_log:close(F#file.desc),
            S2 = S#state{file = undefined},
            reply(Reply, S2)
    end;
handle_call({save_event_file, FileName, Options}, _From, S) ->
    Default = #file{name      = FileName,
                    event_opt = existing,
                    file_opt  = write,
                    table_opt = keep},
    case parse_file_options(Default, Options) of
        {ok, F} when is_record(F, file) ->
            case file_open(F) of
                {ok, Fd} ->
                    F2 = F#file{desc = Fd},
                    {Reply2, S3} = 
                        case F2#file.event_opt of
                            %% new ->
                            %%     Reply = ok,
                            %%     S2 = S#state{file = F},
                            %%     {Reply, S2};
                            %%
                            %% insert() ->
                            %%   case S2#state.file of    
                            %%       undefined ->
                            %%           ok;
                            %%       F  ->
                            %%           Fd = F#file.desc,
                            %%           ok = disk_log:log(Fd, Event)
                            %%   end.
                            existing ->
                                Fun = fun({_, E}, A) -> ok = disk_log:log(Fd, E), A end,
                                Tab = S#state.event_tab,
                                Reply = tab_iterate(Fun, Tab, ets:first(Tab), ok),
                                ok = disk_log:close(Fd),
                                {Reply, S}
                            %% all ->
                            %%     Reply = tab_iterate(WriteFun, Tab, ok),
                            %%     S2 = S#state{file = F},
                            %%     {Reply, S2}
                        end,
                    case F2#file.table_opt of
                        keep ->
                            reply(Reply2, S3);
                        clear ->
                            S4 = do_clear_table(S3),
                            reply(Reply2, S4)
                    end;
                {error, Reason} ->
                    reply({error, {file_open, Reason}}, S)
            end;
        {error, Reason} ->
            reply({error, Reason}, S)
    end;

handle_call({change_pattern, Pattern}, _From, S) ->
    Ns = S#state.trace_nodes,
    {_,[]} = rpc:multicall(Ns, et_selector, change_pattern, [Pattern]),
    Reply = {old_pattern, S#state.trace_pattern},
    S2 = S#state{trace_pattern = Pattern},
    reply(Reply, S2);

handle_call(clear_table, _From, S) ->
    S2 = do_clear_table(S),
    reply(ok, S2);

handle_call(stop, _From, S) ->
    do_multicast(S#state.subscribers, close),
    case S#state.trace_global of
        true  -> {_,[]} = rpc:multicall(S#state.trace_nodes, dbg, stop_clear, []),
                 ok;
        false -> ok
    end,
    {stop, shutdown, ok, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~tp, ~tp, ~tp)~n",
                             [?MODULE, self(), Request, From, S]),
    reply({error, {bad_request, Request}}, S).

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~tp, ~tp)~n",
                             [?MODULE, self(), Msg, S]),
    noreply(S).

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info(timeout, S) ->
    S2 = check_size(S),
    noreply(S2);
handle_info({nodeup, Node}, S) ->
    Port     = S#state.trace_port,
    MaxQueue = S#state.trace_max_queue,
    case rpc:call(Node, ?MODULE, monitor_trace_port, [self(), {Port, MaxQueue}]) of
        {ok, _} ->
            S2 = listen_on_trace_port(Node, Port, S),
	    noreply(S2);
        {error, Reason} when Reason =:= already_started->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~tp~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            noreply(S2);
        {badrpc, Reason} ->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~tp~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            noreply(S2);
        {error, Reason} ->
            self() ! {nodeup, Node},
            ok = error_logger:format("~p(~p): producer retry(~p:~p):~n     ~tp~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            noreply(S2)
    end;

handle_info({nodedown, Node}, S) ->
    noreply(S#state{trace_nodes = S#state.trace_nodes -- [Node]});

handle_info({register_trace_client, Pid}, S) ->
    link(Pid),
    noreply(S);

handle_info({'EXIT', Pid, Reason}, S) when Pid =:= S#state.parent_pid ->
    {stop, Reason, S};
handle_info(Info = {'EXIT', Pid, Reason}, S) ->
   OldSubscribers = S#state.subscribers,
    case lists:member(Pid, OldSubscribers) of
        true when Reason =:= shutdown ->
	    try 
		S2 = do_dict_delete({dict_delete, {subscriber, Pid}}, S),
		noreply(S2)
	    catch
		throw:{stop, R} ->
		    opt_unlink(S#state.parent_pid),
		    {stop, R, S}
	    end;
	true ->
	    opt_unlink(S#state.parent_pid),
	    {stop, Reason, S};
        false ->
            ok = error_logger:format("~p(~p): handle_info(~tp, ~tp)~n",
                                     [?MODULE, self(), Info, S]),
            noreply(S)
    end;
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~tp, ~tp)~n",
                             [?MODULE, self(), Info, S]),
    noreply(S).

listen_on_trace_port(Node, Port, S) ->
    [_Name, Host] = string:lexemes(atom_to_list(Node), [$@]),
    case catch start_trace_client(self(), ip, {Host, Port}) of
        {trace_client_pid, RemotePid} ->
            rpc:call(Node, et_selector, change_pattern, [S#state.trace_pattern]),
            link(RemotePid),
            S#state{trace_nodes = [Node | S#state.trace_nodes],
		    trace_port  = Port + 1};
        {'EXIT', Reason} when Reason =:= already_started->
            ok = error_logger:format("~p(~p): consumer ignored(~p:~p): ~tp~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S#state{trace_port = Port + 1};
        {'EXIT', Reason} ->
            self() ! {nodeup, Node},
            ok = error_logger:format("~p(~p): consumer retry(~p:~p):~n     ~tp~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S#state{trace_port = Port + 1}
    end.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(Reason, S) ->
    Fun = fun(Pid) -> exit(Pid, Reason) end,
    lists:foreach(Fun, S#state.subscribers).

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_clear_table(S) ->
    OldTab = S#state.event_tab,
    ets:delete(OldTab),
    NewTab = ets:new(et_events, [ordered_set, {keypos, 1}, public]),
    S#state{event_tab = NewTab}.

do_dict_insert(Msg = {dict_insert, Key = {subscriber, Pid}, Val}, S) when is_pid(Pid) ->
    OldSubscribers = S#state.subscribers,
    NewSubscribers =
        case lists:member(Pid, OldSubscribers) of
            true  ->
                OldSubscribers;
            false ->
                link(Pid),
                All = ets:match_object(S#state.dict_tab, '_'),
                lists:foreach(fun({K, V}) -> Pid ! {et, {dict_insert, K, V}} end, All),
                [Pid | OldSubscribers]
        end,
    do_multicast(NewSubscribers, Msg),
    Size = ets:info(S#state.event_tab, size),
    do_multicast(NewSubscribers, {more_events, Size}),
    ets:insert(S#state.dict_tab, {Key, Val}),
    S#state{subscribers = NewSubscribers};
do_dict_insert(Msg = {dict_insert, Key, Val}, S) ->
    do_multicast(S#state.subscribers, Msg),
    ets:insert(S#state.dict_tab, {Key, Val}),
    S.

do_dict_delete(Msg = {dict_delete, Key = {subscriber, Pid}}, S) ->
    OldSubscribers = S#state.subscribers,
    do_multicast(OldSubscribers, Msg),
    ets:delete(S#state.dict_tab, Key),
    case lists:member(Pid, OldSubscribers) of
	true  ->
	    unlink(Pid),
	    S2 = S#state{subscribers = OldSubscribers -- [Pid]},
	    if
		S2#state.auto_shutdown,
		S2#state.subscribers =:= [] ->
		    throw({stop, shutdown});
		true ->
		    S2
	    end;
	false ->
	    S
    end;
do_dict_delete({dict_delete, {filter, ?DEFAULT_FILTER_NAME}}, S) ->
    S;
do_dict_delete(Msg = {dict_delete, Key}, S) ->
    do_multicast(S#state.subscribers, Msg),
    ets:delete(S#state.dict_tab, Key),
    S.

tab_iterate(_Fun, _Tab, '$end_of_table', Acc) ->
    Acc;
tab_iterate(Fun, Tab, Key, Acc) ->
    Acc2 = lists:foldl(Fun, Acc, ets:lookup(Tab, Key)),
    tab_iterate(Fun, Tab, ets:next(Tab, Key), Acc2).

file_open(F) ->
    Fd = make_ref(),
    case F#file.file_opt of
        write  -> ok = file:rename(F#file.name, F#file.name ++ ".OLD");
        append -> ok
    end,
    Args = [{file, F#file.name}, {name, Fd},
            {repair, true}, {mode, read_write}],
    case disk_log:open(Args) of
        {ok, _} ->
            {ok, Fd};
        {repaired, _, _, BadBytes} ->
            ok = error_logger:format("~p: Skipped ~p bad bytes in file: ~tp~n",
                                     [?MODULE, BadBytes, F#file.name]),
            {ok, Fd};
        {error,Reason} ->
            {error,Reason}
    end.

parse_file_options(F, [H | T]) ->
    case H of
        existing -> parse_file_options(F#file{event_opt = existing} , T);
        %%new      -> parse_file_options(F#file{event_opt = new} , T);
        all      -> parse_file_options(F#file{event_opt = all} , T);
        write    -> parse_file_options(F#file{file_opt  = write} , T);
        append   -> parse_file_options(F#file{file_opt  = append} , T);
        keep     -> parse_file_options(F#file{table_opt = keep} , T);
        clear    -> parse_file_options(F#file{table_opt = clear} , T);
        Bad      -> {error, {bad_file_option, Bad}}
    end;
parse_file_options(F, []) ->
    {ok, F}.

do_multicast([Pid | Pids], Msg) ->
    Pid ! {et, Msg},
    do_multicast(Pids, Msg);
do_multicast([], _Msg) ->
    ok.

opt_unlink(Pid) ->
    if
	Pid =:= undefined ->
	    ok;
	true ->
	    unlink(Pid)
    end.

reply(Reply, #state{subscribers = []} = S) ->
    {reply, Reply, S};
reply(Reply, S) ->
    {reply, Reply, S, 500}.

noreply(#state{subscribers = []} = S) ->
    {noreply, S};
noreply(S) ->
    {noreply, S, 500}.

check_size(S) ->
    Size = ets:info(S#state.event_tab, size),
    if
	Size =:= S#state.event_tab_size ->
	    S;
	true ->
	    %% Tell the subscribers that more events are available
	    Msg = {more_events, Size},
	    do_multicast(S#state.subscribers, Msg),
	    S#state{event_tab_size = Size}
    end.
