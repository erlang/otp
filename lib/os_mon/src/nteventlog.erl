%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1998-2016. All Rights Reserved.
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
-behaviour(gen_server).

%% API
-export([start_link/2, start/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {port, mfa}).

%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------

start_link(Ident, MFA) ->
    gen_server:start_link({local, nteventlog}, nteventlog,
			  [Ident, MFA], []).

start(Ident, MFA) ->
    gen_server:start({local, nteventlog}, nteventlog, [Ident, MFA], []).

stop() ->
    gen_server:call(nteventlog, stop).

%%----------------------------------------------------------------------
%% gen_server callbacks
%%----------------------------------------------------------------------

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

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({_Port, {data, Data}}, #state{mfa={M,F,A}} = State) ->
    T = parse_log(Data),
    apply(M, F, [T | A]),
    State#state.port ! {self(), {command, "A"}},
    {noreply, State};
handle_info({'EXIT', _Port, Reason}, State) ->
    {stop, {port_died, Reason}, State#state{port=not_used}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.port of
	not_used -> ignore;
	Port ->
	    port_close(Port)
    end,
    ok.

%% os_mon-2.0
%% For live downgrade to/upgrade from os_mon-1.8[.1]
code_change(Vsn, PrevState, "1.8") ->
    case Vsn of

	%% Downgrade from this version
	{down, _Vsn} ->
	    process_flag(trap_exit, false),

	    %% Downgrade to old State tuple
	    State = {PrevState#state.port, PrevState#state.mfa},
	    {ok, State};

	%% Upgrade to this version
	_Vsn ->
	    process_flag(trap_exit, true),

	    %% Upgrade to this state record
	    {Port, MFA} = PrevState,
	    State = #state{port=Port, mfa=MFA},
	    {ok, State}
    end;
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

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
