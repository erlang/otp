%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2017. All Rights Reserved.
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
%% Purpose: Define event transforms and trace patterns 
%%----------------------------------------------------------------------

-module(et_selector).

-export([make_pattern/1,
         change_pattern/1,
         parse_event/2]).

-compile([{nowarn_deprecated_function,[{erlang,now,0}]}]).

-include("../include/et.hrl").

%%----------------------------------------------------------------------
%% make_pattern(RawPattern) -> TracePattern
%%
%% Makes a trace pattern suitable to feed change_pattern/1
%% See also erlang:trace_pattern/2 for more info about its match_spec()
%%
%% RawPattern = detail_level()
%% TracePattern = erlang_trace_pattern_match_spec()
%%
%% detail_level() = min | max | integer(X) when X >= 0, X =< 100
%%
%%   min       - minimum level of tracing  (ignore calls to trace_me/4,5)
%%   max       - maximum level of tracing  (all calls to trace_me/4,5)
%%   integer() - explicit detail level of tracing
%%----------------------------------------------------------------------

make_pattern(undefined) ->
    {undefined, undefined};
make_pattern({Mod, Pattern}) when is_atom(Mod) ->
    case Pattern of
	min ->
	    {Mod, []};
	max ->
	    Head = ['$1', '_', '_', '_', '_'],
	    Body = [],
	    Cond = [],
	    {Mod, [{Head, Cond, Body}]};
	DetailLevel when is_integer(DetailLevel) ->
	    Head = ['$1', '_', '_', '_', '_'],
	    Body = [],
	    Cond = [{ '<', '$1', DetailLevel}],
	    {Mod, [{Head, Cond, Body}]};
	undefined ->
	    {Mod, undefined};
	_ ->
	    exit({bad_pattern, Pattern})
    end.

%%----------------------------------------------------------------------
%% change_pattern(Pattern) -> ok
%%
%% Activates/deactivates tracing by changing the current trace pattern
%%
%% Pattern = detail_level() |
%%           empty_match_spec() |
%%           erlang_trace_pattern_match_spec()
%%
%% detail_level() = min | max | integer(X) when X =<0, X >= 100
%% empty_match_spec() = [] 
%%
%% Min detail level deactivates tracing of calls to trace_me/4,5
%%
%% Max detail level activates tracing of all calls to trace_me/4,5
%%
%% integer(X) detail level activates tracing of all calls to
%% trace_me/4,5 whose detail level argument is lesser than X.
%%
%% An empty match spec deactivates tracing of calls to trace_me/4,5
%%
%% Other match specs activates tracing of calls to trace_me/4,5
%% accordlingly with erlang:trace_pattern/2.
%%----------------------------------------------------------------------

change_pattern({Mod, Pattern}) when is_atom(Mod) ->
    MFA = {Mod, trace_me, 5},
    case Pattern of
	undefined ->
	    ignore;
        [] ->
            error_to_exit(old_ctp(MFA)),
            error_to_exit(dbg:ctp(MFA)),
            error_to_exit(dbg:p(all, clear));
        List when is_list(List) ->
            error_to_exit(old_ctp(MFA)),
            error_to_exit(old_tp(MFA, Pattern)),
            error_to_exit(dbg:ctp(MFA)),
            error_to_exit(dbg:tp(MFA, Pattern)),
            error_to_exit(dbg:p(all, [call, timestamp]));
        Other ->
            change_pattern(make_pattern({Mod, Other}))
    end,
    ok.

old_ctp({Mod, _Fun, Args}) ->
    case Mod of
	et -> {ok, ignore};
	_  -> dbg:ctp({Mod, report_event, Args})
    end.

old_tp({Mod, _Fun, Args}, Pattern) ->
    case Mod of
	et -> {ok, ignore};
	_  -> dbg:tp({Mod, report_event, Args}, Pattern)
    end.

error_to_exit({error, Reason}) ->
    exit(Reason);
error_to_exit({ok, Res}) ->
    Res.

%%----------------------------------------------------------------------
%% parse_event(Mod, ValidTraceData) -> false | true | {true, Event}
%%
%% Transforms trace data and makes an event record out of it
%%
%% ValidTraceData = erlang_trace_data() | record(event)
%% Mod = module_name() | undefined
%% module_name() = atom()
%%
%% erlang_trace_data() =
%% 
%%     {trace, Pid, Label, Info} | 
%%     {trace, Pid, Label, Info, Extra} | 
%%     {trace_ts, Pid, Label, Info, ReportedTS} | 
%%     {trace_ts, Pid, Label, Info, Extra, ReportedTS} | 
%%     {seq_trace, Label, Info} | 
%%     {seq_trace, Label, Info, ReportedTS} | 
%%     {drop, NumberOfDroppedItems}
%%
%% See erlang:trace/3 for more info about the semantics of
%% the trace data.
%%
%% An event record consists of the following fields:
%%
%%     detail_level - Noise has a high level as opposed to essentials.
%%     trace_ts     - Time when the trace was generated.
%%                    Same as event_ts if omitted in trace data.
%%     event_ts     - Time when the event record was created.
%%     from         - From actor, such as sender of a message.
%%     to           - To actor, such as receiver of message.
%%     label        - Label intended to provide a brief event summary.
%%     contents     - All nitty gritty details of the event.
%%
%% See et:trace_me/4 and et:trace_me/5 for details.
%%     
%% Returns: 
%%
%%   {true, Event} - where Event is an #event{} record representing the
%%                   trace data
%%   true          - means that the trace data already is an event
%%                   record and that it is valid as it is.
%%                   No transformation is needed.
%%   false         - means that the trace data is uninteresting and
%%                   should be dropped
%%----------------------------------------------------------------------

parse_event(_Mod, E) when is_record(E, event) ->
    true;
parse_event(Mod, Trace) ->
    ParsedTS = erlang:now(),
    case Trace of
        {trace, Pid, Label, Info} ->
            parse_event(Mod, Trace, ParsedTS, ParsedTS, Pid, Label, [Info]);
        {trace, Pid, Label, Info, Extra} ->
            parse_event(Mod, Trace, ParsedTS, ParsedTS, Pid, Label, [Info, Extra]);
        {trace_ts, Pid, Label, Info, ReportedTS} ->
            parse_event(Mod, Trace, ParsedTS, ReportedTS, Pid, Label, [Info]);
        {trace_ts, Pid, Label, Info, Extra, ReportedTS} ->
            parse_event(Mod, Trace, ParsedTS, ReportedTS, Pid, Label, [Info, Extra]);
        {seq_trace, Label, Info} ->
            parse_seq_event(Trace, ParsedTS, ParsedTS, Label, Info);
        {seq_trace, Label, Info, ReportedTS} ->
            parse_seq_event(Trace, ParsedTS, ReportedTS, Label, Info);
        {drop, NumberOfDroppedItems} ->
            DetailLevel = 20,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ParsedTS,
                          event_ts     = ParsedTS,
                          from         = undefined,
                          to           = undefined, 
                          label        = drop, 
                          contents     = [{label, drop},
					  {detail_level, DetailLevel},
					  {from, undefined},
					  {to, undefined},
					  {drop, NumberOfDroppedItems}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~tp~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.

parse_seq_event(Trace, ParsedTS, ReportedTS, Label, Info) ->
    case Info of
        {send, Serial, From, To, Msg} ->
            DetailLevel = 15,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS, 
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = To, 
                          label        = {seq_send, Label},
                          contents     = [{label, {seq_send, Label}},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, To},
					  {serial, Serial},
					  {msg, Msg}]}};
        {'receive', Serial, From, To, Msg} ->
            DetailLevel = 10,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = To,
                          label        = {seq_receive, Label}, 
                          contents     = [{label, {seq_receive, Label}},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, To},
					  {serial, Serial},
					  {msg, Msg}]}};
        {print, Serial, From, _, UserInfo} ->
            DetailLevel = 5,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS, 
                          event_ts     = ParsedTS,
                          from         = From, 
                          to           = From, 
                          label        = {seq_print, Label},
                          contents     = [{label, {seq_print, Label}},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {serial, Serial},
					  {user_info, UserInfo}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~tp~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.

parse_event(Mod, Trace, ParsedTS, ReportedTS, From, Label, Contents) ->
    case Label of
        'receive' ->
            DetailLevel = 35,
            [Msg] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {msg, Msg}]}};
        send ->
            DetailLevel = 40,
            [Msg, To] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = To,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, To},
					  {msg, Msg}]}};
        send_to_non_existing_process ->
            DetailLevel = 40,
            [Msg, To] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = To,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, To},
					  {msg, Msg}]}};
        call ->
            case Contents of
                [{M, trace_me, [UserDetailLevel, UserFrom, UserTo, UserLabel, UserContents]}] when M == Mod, Mod /= undefined ->
                    {true, #event{detail_level = UserDetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = UserFrom,
                                  to           = UserTo,
                                  label        = UserLabel,
                                  contents     = UserContents}}; % Term
                [{M, report_event, [UserDetailLevel, UserFrom, UserTo, UserLabel, UserContents]}] when M == Mod, Mod /= undefined ->
                    {true, #event{detail_level = UserDetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = UserFrom,
                                  to           = UserTo,
                                  label        = UserLabel,
                                  contents     = UserContents}}; % Term
                [MFA] ->
                    DetailLevel = 45,
                    {true, #event{detail_level = DetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = From,
                                  to           = From,
                                  label        = Label,
                                  contents     = [{label, Label},
						  {detail_level, DetailLevel},
						  {from, From},
						  {to, From},
						  {mfa, MFA}]}};
                [MFA, PamResult] ->
                    DetailLevel = 45,
                    {true, #event{detail_level = DetailLevel,
                                  trace_ts     = ReportedTS,
                                  event_ts     = ParsedTS,
                                  from         = From,
                                  to           = From,
                                  label        = Label,
                                  contents     = [{label, Label},
						  {detail_level, DetailLevel},
						  {from, From},
						  {to, From},
						  {mfa, MFA},
						  {pam_result, PamResult}]}}
            end;
        return_to ->
            DetailLevel = 50,
            [MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {mfa, MFA}]}};
        return_from ->
            DetailLevel = 52,
            [MFA, ReturnValue] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {mfa, MFA},
					  {return, ReturnValue}]}};
        exception_from ->
            DetailLevel = 54,
            [MFA, Exception] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {mfa, MFA},
					  {exception, Exception}]}};
        spawn ->
            DetailLevel = 25,
            [NewPid, MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = NewPid,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, NewPid},
					  {mfa, MFA}]}};  % MFA | Term
        exit ->
            DetailLevel = 30,
            [Reason] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {reason, Reason}]}};
        link ->
            DetailLevel = 55,
            [LinkTo] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = LinkTo,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, LinkTo}]}};
        unlink ->
            DetailLevel = 60,
            [UnlinkFrom] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = UnlinkFrom,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, UnlinkFrom}]}};
        getting_linked ->
            DetailLevel = 65,
            [LinkTo] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = LinkTo,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, LinkTo}]}};
        getting_unlinked ->
            DetailLevel = 67,
            [UnlinkFrom] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = UnlinkFrom,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, UnlinkFrom}]}};
        register ->
            DetailLevel = 70,
            [Name] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {name, Name}]}};
        unregister ->
            DetailLevel = 75,
            [Name] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {name, Name}]}};
        in ->
            DetailLevel = 90,
            [MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {mfa, MFA}]}}; % MFA | 0
        out ->
            DetailLevel = 95,
            [MFA] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {mfa, MFA}]}}; % MFA | 0
        gc_minor_start ->
            DetailLevel = 80,
            [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
					  {detail_level, DetailLevel},
					  {from, From},
					  {to, From},
					  {gc_items, GcKeyValueList}]}};
        gc_minor_end ->
            DetailLevel = 85,
            [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
                                          {detail_level, DetailLevel},
                                          {from, From},
                                          {to, From},
                                          {gc_items, GcKeyValueList}]}};
        gc_major_start ->
            DetailLevel = 80,
            [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
                                          {detail_level, DetailLevel},
                                          {from, From},
                                          {to, From},
                                          {gc_items, GcKeyValueList}]}};
        gc_major_end ->
            DetailLevel = 85,
            [GcKeyValueList] = Contents,
            {true, #event{detail_level = DetailLevel,
                          trace_ts     = ReportedTS,
                          event_ts     = ParsedTS,
                          from         = From,
                          to           = From,
                          label        = Label,
                          contents     = [{label, Label},
                                          {detail_level, DetailLevel},
                                          {from, From},
                                          {to, From},
                                          {gc_items, GcKeyValueList}]}};
        _ ->
            error_logger:format("~p(~p): Ignoring unknown trace type -> ~tp~n~n",
                                [?MODULE, ?LINE, Trace]),
            false
    end.
