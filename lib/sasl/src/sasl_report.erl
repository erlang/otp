%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2016. All Rights Reserved.
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
-module(sasl_report).

-export([write_report/3, format_report/3]).

format_report(Fd, What, Report) ->
    io_report(io_lib, Fd, What, Report).

write_report(Fd, What, Report) ->
    io_report(io, Fd, What, Report).

io_report(IO, Fd, What, {Time, {error_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_error_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(IO, Fd, Head, Type, Report);
	_ -> true
    end;
io_report(IO, Fd, What, {Time, {info_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_info_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(IO, Fd, Head, Type, Report);
	_ -> true
    end;
io_report(_IO, _Fd, _, _) ->
    false.

is_my_error_report(all, Type)   ->  is_my_error_report(Type);
is_my_error_report(error, Type) ->  is_my_error_report(Type);
is_my_error_report(_, _Type)    ->  false.
is_my_error_report(supervisor_report)   -> true;
is_my_error_report(crash_report)        -> true;
is_my_error_report(_)                   -> false.

is_my_info_report(all, Type)      -> is_my_info_report(Type);
is_my_info_report(progress, Type) -> is_my_info_report(Type);
is_my_info_report(_, _Type)       -> false.
is_my_info_report(progress)  -> true;
is_my_info_report(_)                    -> false.

write_report2(IO, Fd, Head, supervisor_report, Report) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    {FmtString,Args} = supervisor_format([Name,Context,Reason,Offender]),
    write_report_action(IO, Fd, Head, FmtString, Args);
write_report2(IO, Fd, Head, progress, Report) ->
    Format = format_key_val(Report),
    write_report_action(IO, Fd, Head, "~s", [Format]);
write_report2(IO, Fd, Head, crash_report, Report) ->
    Depth = get_depth(),
    Format = proc_lib:format(Report, latin1, Depth),
    write_report_action(IO, Fd, Head, "~s", [Format]).

supervisor_format(Args0) ->
    case get_depth() of
	unlimited ->
	    {"     Supervisor: ~p~n"
	     "     Context:    ~p~n"
	     "     Reason:     ~80.18p~n"
	     "     Offender:   ~80.18p~n~n",
	     Args0};
	Depth ->
	    [A,B,C,D] = Args0,
	    Args = [A,Depth,B,Depth,C,Depth,D,Depth],
	    {"     Supervisor: ~P~n"
	     "     Context:    ~P~n"
	     "     Reason:     ~80.18P~n"
	     "     Offender:   ~80.18P~n~n",
	     Args}
    end.

write_report_action(IO, Fd, Head, Format, Args) ->
    S = [Head|io_lib:format(Format, Args)],
    case IO of
	io -> io:put_chars(Fd, S);
	io_lib -> S
    end.

format_key_val([{Tag,Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val(Rep);
format_key_val(_) ->
    [].

get_depth() ->
    case application:get_env(kernel, error_logger_format_depth) of
	{ok, Depth} when is_integer(Depth) ->
	    max(10, Depth);
	undefined ->
	    unlimited
    end.

sup_get(Tag, Report) ->
    case lists:keysearch(Tag, 1, Report) of
	{value, {_, Value}} ->
	    Value;
	_ ->
	    ""
    end.

maybe_utc(Time) ->
    case application:get_env(sasl,utc_log) of
	{ok,true} ->
	    case calendar:local_time_to_universal_time_dst(Time) of
		[UTC] ->
		    {utc,UTC};
		[UTC1,_UTC2] ->
		    {utc,UTC1};
		[] -> % should not happen
		    Time
	    end;
	_ ->
	    Time
    end.

write_head(supervisor_report, Time, Pid) ->
    write_head1("SUPERVISOR REPORT", maybe_utc(Time), Pid);
write_head(crash_report, Time, Pid) ->
    write_head1("CRASH REPORT", maybe_utc(Time), Pid);
write_head(progress, Time, Pid) ->
    write_head1("PROGRESS REPORT", maybe_utc(Time), Pid).

write_head1(Type, {utc,{{Y,Mo,D},{H,Mi,S}}}, Pid) when node(Pid) /= node() ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC (~p) ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),node(Pid)]);
write_head1(Type, {utc,{{Y,Mo,D},{H,Mi,S}}}, _) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s UTC ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]);
write_head1(Type, {{Y,Mo,D},{H,Mi,S}}, Pid) when node(Pid) /= node() ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s (~p) ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S),node(Pid)]);
write_head1(Type, {{Y,Mo,D},{H,Mi,S}}, _) ->
    io_lib:format("~n=~s==== ~p-~s-~p::~s:~s:~s ===~n",
		  [Type,D,month(Mo),Y,t(H),t(Mi),t(S)]).

t(X) when is_integer(X) ->
    t1(integer_to_list(X));
t(_) ->
    "".
t1([X]) -> [$0,X];
t1(X)   -> X.

month(1) -> "Jan";
month(2) -> "Feb";
month(3) -> "Mar";
month(4) -> "Apr";
month(5) -> "May";
month(6) -> "Jun";
month(7) -> "Jul";
month(8) -> "Aug";
month(9) -> "Sep";
month(10) -> "Oct";
month(11) -> "Nov";
month(12) -> "Dec".
