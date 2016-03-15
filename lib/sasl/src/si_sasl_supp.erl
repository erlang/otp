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
-module(si_sasl_supp).

-behaviour(gen_server).

%%%---------------------------------------------------------------------------
%%% Description:
%%% This module contains the BOS specific parts of the Status Inspection Tool.
%%%---------------------------------------------------------------------------


%% user interface
-export([h/0, help/0, start_log/1, stop_log/0, abbrevs/0, pi/1, pi/2, pi/3,
	 pi/4, ppi/1, ppi/3, start/0, start/1, stop/0, start_link/1]).

%% intermodule exports
-export([make_pid/1, make_pid/3, process_abbrevs/0, expand_abbrev/2,
	 status_info/1, valid_opt/1, p/1, do_best_printout/4,
	 si_exec/2, handle_call/3, terminate/2]).

%% exports for use within module
-export([init/1, start_log_impl/1, pi_impl/2, ppi_impl/1]).

%% other gen_server callbacks (not used)
-export([handle_cast/2, handle_info/2, code_change/3]).

%%--------------------------------------------------
%% Table of contents
%% 1. Interface
%% 2. SI - Server
%% 3. Code
%% 4. Selectors
%%--------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 1. Interface
%% -----------------------------------------------------

h() -> print_help().
help() -> print_help().

si_exec(Fun, Args) -> call({si_exec, Fun, Args}).

start_log(FileName) ->
    call({start_log, FileName}).

stop_log() ->
    call(stop_log).

abbrevs() ->
    io:format("~p", [process_abbrevs()]).

%%-----------------------------------------------------------------
%% All functions can be called with an option 'normal' or 'all';
%% default is 'normal'.
%%-----------------------------------------------------------------
%% Process Info that tries to determine processtype (=Module), then
%% it uses this Module:format_info to format data from status_info/1.
%%-----------------------------------------------------------------
pi(XPid) -> 
    si_exec({si_sasl_supp, pi_impl}, [normal, XPid]).

pi(Opt, XPid) ->
    si_exec({si_sasl_supp, pi_impl}, [valid_opt(Opt), XPid]).

pi(A, B, C) when is_integer(A), is_integer(B), is_integer(C) ->
    si_exec({si_sasl_supp, pi_impl}, [normal, {A, B, C}]).

pi(Opt, A, B, C) when is_integer(A), is_integer(B), is_integer(C) ->
    si_exec({si_sasl_supp, pi_impl}, [valid_opt(Opt), {A, B, C}]).

%%-----------------------------------------------------------------
%% Pretty print Process_Info.
%%-----------------------------------------------------------------
ppi(XPid) ->
    case whereis(si_server) of
	undefined ->           % You can always run ppi.
	    ppi_impl(XPid);    % if si_server is down, use standard_io
	_ ->
	    si_exec({si_sasl_supp, ppi_impl}, [XPid])
    end.
ppi(A, B, C) ->
    case whereis(si_server) of
	undefined ->               % You can always run ppi.
	    ppi_impl({A, B, C});   % if si_server is down, use standard_io
	_ ->
	    si_exec({si_sasl_supp, ppi_impl}, [{A, B, C}])
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 2. SI - Server
%%--------------------------------------------------
-record(state, {}).

start() -> start([]).
start(Options) ->
    supervisor:start_child(sasl_sup, 
			   {si_server, {si_sasl_supp, start_link, [Options]},
			    temporary, brutal_kill, worker, [si_sasl_supp]}).

start_link(_Options) ->
    gen_server:start_link({local, si_server}, si_sasl_supp, [], []).

stop() ->
    call(stop),
    supervisor:delete_child(sasl_sup, si_server).


init(Options) ->
    process_flag(trap_exit, true),
    start_log_impl(get_option(Options, start_log, standard_io)),
    {ok, #state{}}.

%%-----------------------------------------------------------------
%% If an error occurs and we're logging to file: write the error
%% to the file.
%% Always return the error.
%% The only data held by the si_server is the device in its process dictionary.
%%-----------------------------------------------------------------
handle_call({si_exec, Fun, Args}, _From, State) ->
    case catch apply(Fun, Args) of
	{'EXIT', Reason} ->
	    print_error(get(device),
			"SI internal error. Reason: ~w~n",
			[Reason]),
	    {stop, shutdown, {internal_error, Reason}, State};
	{error, Reason} ->
	    print_error(get(device), "~nSI error: ~w~n", [Reason]),
	    {reply, {error, Reason}, State};
	X -> 
	    {reply, X, State}
    end;
handle_call({start_log, FileName}, _From, State) ->
    start_log_impl(FileName),
    {reply, ok, State};
handle_call(stop_log, _From, State) ->
    start_log_impl(standard_io),
    {reply, ok, State};
handle_call(stop, _From, State) ->
    start_log_impl(standard_io),
    {stop, normal, stopped, State}.

terminate(_Reason, _State) ->
    _ = close_device(get(device)),
    ok.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

close_device(standard_io) -> ok;
close_device(Fd) -> file:close(Fd).

print_error(standard_io, _, _) -> ok;
print_error(Device, Format, Args) ->
    io:format(Device, Format, Args).

get_option(Options, Key, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

open_log_file(undefined, NewFile) ->
    open_log_file(NewFile);
open_log_file(standard_io, NewFile) ->
    open_log_file(NewFile);
open_log_file(OldFile, NewFile) ->
    _ = file:close(OldFile),
    open_log_file(NewFile).

open_log_file(standard_io) -> standard_io;
open_log_file(FileName) ->
    case file:open(FileName, [write]) of
	{ok, Fd} -> Fd;
	Error -> 
	    io:format("si_sasl_supp: Cannot open file '~s' (~w).~n",
		      [FileName, Error]),
	    io:format("si_sasl_supp: Using standard_io~n"),
	    standard_io
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 3. Code
%%--------------------------------------------------

%%-----------------------------------------------------------------
%% call(Request) -> Term
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(si_server, Req, infinity).

%%--------------------------------------------------
%% Makes a Pid of almost anything.
%% Returns: Pid|{error, Reason}
%% Fails: Never.
%%--------------------------------------------------
make_pid(A,B,C) when is_integer(A), is_integer(B), is_integer(C) ->
    list_to_pid(lists:concat(["<",A,".",B,".",C,">"])).
make_pid(P) when is_pid(P) -> P;
make_pid(undefined) -> undefined;
make_pid(P) when is_atom(P) ->
    case whereis(P) of
	undefined ->
	    case expand_abbrev(P, process_abbrevs()) of
		{error, Reason} -> {error, Reason};
		{value, {_Abbrev, FullName}} ->
		    case whereis(FullName) of
			undefined ->
			    {error, {'process not registered', P}};
			Pid -> Pid
		    end
	    end;
	Pid -> Pid
    end;
make_pid(P) when is_list(P) -> list_to_pid(P);
make_pid({A, B, C}) -> make_pid(A, B, C);
make_pid(X) -> {error, {'can not make a pid of', X}}.

process_abbrevs() ->
    [{init, init},
     {fs, file_server}].
     
%%--------------------------------------------------
%% Args: Abbrevs is an assoc-list of {Abbrev, RealName}
%% Returns: {value, {Abbrev, FullName}}|{error, Reason}
%%--------------------------------------------------
expand_abbrev(ProcessName, Abbrevs) ->
    case lists:keysearch(ProcessName, 1, Abbrevs) of
	{value, {Abbrev, FullName}} ->
	    {value, {Abbrev, FullName}};
	_ ->
	    case lists:keysearch(ProcessName, 2, Abbrevs) of
		{value, {Abbrev, FullName}} ->
		    {value, {Abbrev, FullName}};
		_ ->
		    {error, {'invalid process name', ProcessName}}
	    end
    end.

%%-----------------------------------------------------------------
%% This is the function that actually gets the information out
%% of the agent/server/...
%% Returns: {status_info, Pid, Type, Data}
%%        | {error, Reason}
%%-----------------------------------------------------------------
status_info(Pid) when is_pid(Pid) ->
    case catch sys:get_status(Pid, 5000) of
	{status, Pid, Type, Info} ->
	    {status_info, Pid, Type, Info};
	_ ->
	    {error, {'process does not respond', Pid}}
    end;

status_info(X) ->
    {error, {'not a pid', X}}.

%%--------------------------------------------------
%% Implementation starts here.
%%--------------------------------------------------
start_log_impl(FileName) ->
    put(device, open_log_file(get(device), FileName)).

valid_opt(all) -> all;
valid_opt(_Opt) -> normal.


print_help() ->
    p("- - - - - - - - PROCESSES - - - - - - - - - "),
    p("si_sasl_supp:pi([Opt,] Pid)   - Formatted information about any process that"),
    p("                      SI recognises."),
    p("si_sasl_supp:pi([Opt,] A,B,C) - Same as si_sasl_supp:pi({A, B, C})."), 
    p("si_sasl_supp:ppi(Pid)         - Pretty formating of process_info."),
    p("                      Works for any process."),
    p("- - - - - - - - MISC  - - - - - - - - - - - "),
    p("si_sasl_supp:abbrevs()        - Lists valid abbreviations."),
    p("si_sasl_supp:start_log(FileNname)"),
    p("si_sasl_supp:stop_log()"),
    p("si_sasl_supp:start()          - Starts Status Inspection (the si_server)."),
    p("si_sasl_supp:start([{start_log, FileName}])"),
    p("si_sasl_supp:stop()           - Shut down SI.").



%% Convenient shorthand
p(X) ->
    io:format(lists:append(X, "~n")).

pi_impl(Opt, XPid) ->
    case make_pid(XPid) of
	Pid when is_pid(Pid) ->
	    case status_info(Pid) of
		{status_info, Pid, {module, Module}, Data} ->
		    do_best_printout(Opt, Pid, Module, Data);
		{error, Reason} ->
		    _ = ppi_impl(Pid),
		    {error, {"can not get status info from process:",
			     XPid,
			     Reason}}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------
%% Is there a format_info for this process? In that case, run it!
%% Return ok|{error, Reason}
%% Fails: Never.
%%--------------------------------------------------
do_best_printout(Opt, Pid, Mod, Data) when is_pid(Pid) ->
    case print_info(get(device), Pid, {Mod, format_status}, Opt, Data) of
	ok -> ok;
	{error, Reason} ->
	    _ = ppi_impl(Pid),
	    {error, Reason}
    end.

ppi_impl(XPid) -> 
    case make_pid(XPid) of
	P when is_pid(P) -> 
	    case process_info(P) of
		undefined ->
		    {error, {'dead process', P}};
		PI ->
		    Device = case get(device) of
				 undefined -> standard_io;
				 X -> X
			     end,
		    io:format(Device, "~nPretty Process Info~n", []),
		    io:format(Device, "-------------------~n", []),
		    io:format(Device, "~p~n", [PI])
	    end;
	_ -> {error, {no_pid, XPid}}
    end.

print_info(Device, Pid, {Module, Func}, Opt, Data) ->
    case erlang:function_exported(Module, Func, 2) of
	true ->
	    case catch apply(Module, Func, [Opt, Data]) of
		Format when is_list(Format) ->
		    format_lib_supp:print_info(Device, 79,
					       add_pid_to_format(Pid, Format)),
		    ok;
		Other -> {error, {'invalid format', Other}}
	    end;
	_ ->
	    {error, {no_such_function, Module, Func}}
    end.

add_pid_to_format(Pid, [{header, H} | T]) ->
    [{header, H}, {data, [{"Pid", Pid}]} | T];
add_pid_to_format(Pid, List) ->
    [{data, [{"Pid", Pid}]} | List].


