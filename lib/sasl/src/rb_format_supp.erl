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
-module(rb_format_supp).

%% user interface
-export([print/3]).

%%-----------------------------------------------------------------
%% This module prints error reports. Called from rb.
%%-----------------------------------------------------------------

print(Date, Report, Device) ->
    Line = 79,
%% Remove these comments when we can run rb in erl44!!!
%    case catch sasl_report:write_report(Device, Report) of
%	true -> ok;
%	_ ->
	    {_Time, Rep} = Report,
	    case Rep of
		{error_report, _GL, {Pid, crash_report, CrashReport}} ->
		    Header = format_h(Line, "CRASH REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header} |
						format_c(CrashReport)]),
		    true;
		{error_report, _GL, {Pid, supervisor_report, SupReport}} ->
		    Header = format_h(Line, "SUPERVISOR REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header} |
						format_s(SupReport)]),
		    true;
		{error_report, _GL, {Pid, _Type, Report1}} ->
		    Header = format_h(Line, "ERROR REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header},
						{data, Report1}]),
		    true;
		{info_report, _GL, {Pid, progress, SupProgress}} ->
		    Header = format_h(Line, "PROGRESS REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header} |
						format_p(SupProgress)]);
		{info_report, _GL, {Pid, _Type, Report1}} ->
		    Header = format_h(Line, "INFO REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header},
						{data, Report1}]),
		    true;
		{warning_report, _GL, {Pid, _Type, Report1}} ->
		    Header = format_h(Line, "WARNING REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header},
						{data, Report1}]),
		    true;
		{error, _GL, {Pid, Format, Args}} ->
		    Header = format_h(Line, "ERROR REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header}]),
		    io:format(Device, Format, Args);
		{info_msg, _GL, {Pid, Format, Args}} ->
		    Header = format_h(Line, "INFO REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header}]),
		    io:format(Device, Format, Args);
		{warning_msg, _GL, {Pid, Format, Args}} ->
		    Header = format_h(Line, "WARNING REPORT", Pid, Date),
		    format_lib_supp:print_info(Device,
					       Line,
					       [{header, Header}]),
		    io:format(Device, Format, Args);
		{Type, _GL, TypeReport} ->
                    Modifier = misc_supp:modifier(Device),
		    io:format(Device, "~nInfo type <~"++Modifier++"w> ~s~n",
			      [Type, Date]),
		    io:format(Device, "~"++Modifier++"p", [TypeReport]);
		_ -> 
                    Modifier = misc_supp:modifier(Device),
		    io:format("~nPrinting info of unknown type... ~s~n",
			      [Date]),
		    io:format(Device, "~"++Modifier++"p", [Report])
%	    end
    end.

format_h(Line, Header, Pid, Date) ->
    NHeader = lists:flatten(io_lib:format("~s  ~w", [Header, Pid])), 
    DateLen = string:length(Date),
    HeaderLen = Line - DateLen,
    Format = lists:concat(["~-", HeaderLen, "s~", DateLen, "s"]),
    io_lib:format(Format, [NHeader, Date]).
    

%%-----------------------------------------------------------------
%% Crash report
%%-----------------------------------------------------------------
format_c([OwnReport, LinkReport]) ->
  [{items, {"Crashing process", OwnReport}},
   format_neighbours(LinkReport)].
  
format_neighbours([Data| Rest]) ->
    [{newline, 1},
     {items, {"Neighbour process", Data}} | 
     format_neighbours(Rest)];
format_neighbours([]) -> [].

%%-----------------------------------------------------------------
%% Supervisor report
%%-----------------------------------------------------------------
format_s(Data) ->
    SuperName = get_opt(supervisor, Data),
    ErrorContext = get_opt(errorContext, Data),
    Reason = get_opt(reason, Data),
    ChildInfo = get_opt(offender, Data),
    [{data, [{"Reporting supervisor", SuperName}]},
     {newline, 1},
     {items, {"Child process", 
	      [{errorContext, ErrorContext}, 
	       {reason, Reason} |
               lists:map(fun(CI) -> transform_mfa(CI) end, ChildInfo)]}}].

transform_mfa({mfa, Value}) -> {start_function, Value};
transform_mfa(X) -> X.

%%-----------------------------------------------------------------
%% Progress report
%%-----------------------------------------------------------------
format_p(Data) ->
    [{data, Data}].

get_opt(Key, List) ->
    case lists:keysearch(Key, 1, List) of
	{value, {_Key, Val}} -> Val;
	_  -> undefined
    end.
