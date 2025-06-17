%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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
-module(nteventlog).
-moduledoc """
Interface to Windows Event Log

`nteventlog` provides a generic interface to the Windows event log. It is part
of the OS_Mon application, see [os_mon](os_mon_app.md).

This module is used as the Windows backend for `os_sup`. See `m:os_sup`.

To retain backwards compatibility, this module can also be used to start a
standalone `nteventlog` process which is not part of the OS_Mon supervision
tree. When starting such a process, the user has to supply an identifier as well
as a callback function to handle the messages.

The identifier, an arbitrary string, should be reused whenever the same
application (or node) wants to start the process. `nteventlog` is informed about
all events that have arrived to the eventlog since the last accepted message for
the current identifier. As long as the same identifier is used, the same
eventlog record will not be sent to `nteventlog` more than once (with the
exception of when graved system failures arise, in which case the last records
written before the failure may be sent to Erlang again after reboot).

If the event log is configured to wrap around automatically, records that have
arrived to the log and been overwritten when `nteventlog` was not running are
lost. However, it detects this state and loses no records that are not
overwritten.

The callback function works as described in `m:os_sup`.

## See Also

[os_mon](os_mon_app.md), `m:os_sup`

Windows NT documentation
""".
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2]).

-record(state, {port, mfa}).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

-doc """
This function starts the standalone `nteventlog` process and, if
[`start_link/2`](`start_link/2`) is used, links to it.

`Identifier` is an identifier as described above.

`MFA` is the supplied callback function. When `nteventlog` receives information
about a new event, this function will be called as
[`apply(Mod, Func, [Event|Args])`](`apply/3`) where `Event` is a tuple
""".
-spec start_link(Identifier, MFA) -> Result when Identifier :: string() | atom(),
   MFA :: {Mod, Func, Args},
    Mod :: atom(),
   Func :: atom(),
    Args :: [term()],
   Result :: {ok, Pid} | {error, {already_started, Pid}},
   Pid :: pid().
start_link(Ident, MFA) ->
    gen_server:start_link({local, nteventlog}, nteventlog,
			  [Ident, MFA], []).

-doc """
Equivalent to [`start_link(Identifier, MFA)`](`start_link/2`) except that no
link is created between `nteventlog` and the calling process.
""".
-spec start(Identifier, MFA) -> Result when Identifier :: string() | atom(),
   MFA :: {Mod, Func, Args},
    Mod :: atom(),
   Func :: atom(),
    Args :: [term()],
   Result :: {ok, Pid} | {error, {already_started, Pid}},
   Pid :: pid().
start(Ident, MFA) ->
    gen_server:start({local, nteventlog}, nteventlog, [Ident, MFA], []).

-doc """
Stops `nteventlog`. Usually only used during development. The server does not
have to be shut down gracefully to maintain its state.
""".
-spec stop() -> stopped.
stop() ->
    gen_server:call(nteventlog, stop).

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

-doc false.
init([Identifier,MFA0]) ->
    process_flag(trap_exit, true),
    process_flag(priority, low),

    Port = case os:type() of
	       {win32, _OSname} -> start_portprogram(Identifier);
	       OS -> exit({unsupported_os, OS})
	   end,

    %% If we're using os_sup:error_report/2,
    %% the setting of os_sup_errortag should be used as argument
    MFA = case MFA0 of
	      {os_sup, error_report, [_Tag]} ->
		  Tag = os_mon:get_env(os_sup, os_sup_errortag),
		  {os_sup, error_report, [Tag]};
	      _ ->
		  MFA0
	  end,

    {ok, #state{port=Port, mfa=MFA}}.

-doc false.
handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

-doc false.
handle_cast(_Msg, State) ->
    {noreply, State}.

-doc false.
handle_info({_Port, {data, Data}}, #state{mfa={M,F,A}} = State) ->
    T = parse_log(Data),
    apply(M, F, [T | A]),
    State#state.port ! {self(), {command, "A"}},
    {noreply, State};
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_died, Reason}, State#state{port=not_used}};
handle_info(_Info, State) ->
    {noreply, State}.

-doc false.
terminate(_Reason, State) ->
    case State#state.port of
	not_used -> ignore;
	Port ->
	    port_close(Port)
    end,
    ok.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------

start_portprogram(Identifier) ->
    Command =
	"\"" ++ filename:join([code:priv_dir(os_mon),"bin","nteventlog.exe"]) ++
	"\" " ++ make_list(Identifier),
    open_port({spawn,Command},[{packet,2}]).

make_list(X) when is_atom(X) ->
    atom_to_list(X);
make_list(X) ->
    X.

holl_len([$H | Rest], Sum) ->
    {Sum, Rest};
holl_len([ N | Rest], Sum) ->
    NN = N - $0,
    holl_len(Rest, Sum * 10 + NN).
holl_len(L) ->
    holl_len(L,0).

splitlist(L,N) ->
    {lists:sublist(L,N),lists:nthtail(N,L)}. 

hollerith(Str) ->
    {Len, Rest} = holl_len(Str),
    splitlist(Rest,Len).

holl_time(Str) ->
    {Holl,Rest} = hollerith(Str),
    Rev = lists:reverse(Holl),
    B = list_to_integer(lists:reverse(lists:sublist(Rev,6))),
    A = list_to_integer(lists:reverse(lists:nthtail(6,Rev))),
    {{A,B,0},Rest}.

parse_log(Str) ->
    {Time, Rest1} = holl_time(Str),
    {Category,Rest2} = hollerith(Rest1),
    {Facility,Rest3} = hollerith(Rest2),
    {Severity,Rest4} = hollerith(Rest3),
    {Message,_} = hollerith(Rest4),
    {Time,Category,Facility,Severity,Message}.
