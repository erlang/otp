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
%%-----------------------------------------------------------------
%% l(format_lib_supp), l(si_sasl_supp), l(si), l(si_ms_aos_supp), l(misc_supp).
%% c(format_lib_supp), c(si_sasl_supp), c(si), c(si_ms_aos_supp), c(misc_supp).
%%-----------------------------------------------------------------


%%--------------------------------------------------
%% Description:
%% Status Inspection, main module.
%%--------------------------------------------------

-module(si).


%% External exports
-export([h/0, help/0, start/0, start/1, start_log/1, stop_log/0,
	 abbrevs/0, pi/1, pi/2, pi/3, pi/4, ppi/1, ppi/3, stop/0]).

%% Internal exports
-export([pi_impl/2, test/0]).


%%--------------------------------------------------
%% Table of contents
%% 1. Interface
%% 2. Implementation


-import(si_sasl_supp, [status_info/1, make_pid/1, p/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Interface
%%--------------------------------------------------

h() -> print_help().
help() -> print_help().

start() -> si_sasl_supp:start().
start(Options) -> si_sasl_supp:start(Options).

stop() -> si_sasl_supp:stop().

start_log(FileName) ->
    si_sasl_supp:start_log(FileName).

stop_log() ->
    si_sasl_supp:stop_log().

%%%-----------------------------------------------------------------
%%% All functions can be called with an option 'normal' or 'all';
%%% default is 'normal'.
%%%-----------------------------------------------------------------

abbrevs() ->
    io:format("~p", [lists:append(si_sasl_supp:process_abbrevs(),
				  process_abbrevs())]).

%%-----------------------------------------------------------------
%% Process Info that tries to determine processtype (=Module), then
%% it uses this Module:format_info to format data from status_info/1.
%%-----------------------------------------------------------------
pi(XPid) -> 
    si_sasl_supp:si_exec({si, pi_impl}, [normal, XPid]).

pi(Opt, XPid) ->
    si_sasl_supp:si_exec({si, pi_impl}, [si_sasl_supp:valid_opt(Opt), XPid]).

pi(A, B, C) when is_integer(A), is_integer(B), is_integer(C) ->
    si_sasl_supp:si_exec({si, pi_impl}, [normal, {A, B, C}]).

pi(Opt, A, B, C) when is_integer(A), is_integer(B), is_integer(C) ->
    si_sasl_supp:si_exec({si, pi_impl}, [si_sasl_supp:valid_opt(Opt), {A, B, C}]).

%%-----------------------------------------------------------------
%% Pretty print Process_Info.
%%-----------------------------------------------------------------
ppi(XPid) ->
    si_sasl_supp:ppi(XPid).
ppi(A, B, C) ->
    si_sasl_supp:ppi(A, B, C).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. Implementation
%%--------------------------------------------------

print_help() ->
    p("~nStatus Inspection tool - usage"),
    p("=============================="),
    p("    For all these functions, Opt is an optional argument"),
    p("    which can be 'normal' or 'all'; default is 'normal'."),
    p("    If 'all', all information will be printed."),
    p("    A Pid can be: \"<A.B.C>\", {A, B, C}, B, a registered_name or an abbrev."),
    p("ANY PROCESS"),
    p("si:pi([Opt,] Pid)   - Formatted information about any process that"),
    p("                      SI recognises."),
    p("si:pi([Opt,] A,B,C) - Same as si:pi({A, B, C})."), 
    p("si:ppi(Pid)         - Pretty formating of process_info."),
    p("                      Works for any process."),
    p("MISC"),
    p("si:abbrevs()        - Lists valid abbreviations."),
    p("si:start_log(Filename) - Logging to file."),
    p("si:stop_log()"),
    p("si:start()          - Starts Status Inspection (the si_server)."),
    p("si:start([{start_log, FileName}])"),
    p("si:stop()           - Shut down SI.").


%%--------------------------------------------------
%% Copied (and modified) code from si_sasl_supp.  
%%--------------------------------------------------
pi_impl(Opt, XPid) ->
    case make_pid(try_local_expand_abbrev(XPid)) of
	Pid when is_pid(Pid) ->
	    case status_info(Pid) of
		{status_info, Pid, {module, Module}, Data} ->
		    si_sasl_supp:do_best_printout(Opt, Pid, Module, Data);
		{error, Reason} ->
		    _ = si_sasl_supp:ppi_impl(Pid),
		    {error, {"can not get status info from process:",
			     XPid,
			     Reason}};
		Else ->
		    {error, {"unknown status info", Else}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------
%% Functions for handling of abbreviations
%%--------------------------------------------------
try_local_expand_abbrev(Abbrev) ->
    case si_sasl_supp:expand_abbrev(Abbrev, process_abbrevs()) of
	{value, {_, RealName}} -> RealName;
	_ -> Abbrev
    end.

process_abbrevs() ->
    [].

%% Test get_status_info/format_status_info for all implemented servers.
test() ->
    lists:foreach(fun test_all_registered/1,
		  lists:append(si_sasl_supp:process_abbrevs(),
			       process_abbrevs())).

test_all_registered({Al, _Ful}) ->
    si:pi(all, Al).
