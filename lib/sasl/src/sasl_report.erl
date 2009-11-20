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
-module(sasl_report).

-export([write_report/3]).

write_report(Fd, What, {Time, {error_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_error_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(Fd, Head, Type, Report);
	_ -> true
    end;
write_report(Fd, What, {Time, {info_report, _GL, {Pid, Type, Report}}}) ->
    case is_my_info_report(What, Type) of
	true ->
	    Head = write_head(Type, Time, Pid),
	    write_report2(Fd, Head, Type, Report);
	_ -> true
    end;
write_report(_Fd, _, _) ->
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

write_report2(Fd, Head, supervisor_report, Report) ->
    Name = sup_get(supervisor, Report),
    Context = sup_get(errorContext, Report),
    Reason = sup_get(reason, Report),
    Offender = sup_get(offender, Report),
    io:format(Fd, Head ++ "     Supervisor: ~p~n     Context:    ~p~n     Reason:     "
	      "~80.18p~n     Offender:   ~80.18p~n~n",
	      [Name,Context,Reason,Offender]);
write_report2(Fd, Head, progress, Report) ->
    Format = format_key_val(Report),
    io:format(Fd, Head ++ "~s", [Format]);
write_report2(Fd, Head, crash_report, Report) ->
    Format = proc_lib:format(Report),
    io:format(Fd, Head ++ "~s", [Format]).

format_key_val([{Tag,Data}|Rep]) ->
    io_lib:format("    ~16w: ~p~n",[Tag,Data]) ++ format_key_val(Rep);
format_key_val(_) ->
    [].


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
