%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2010. All Rights Reserved.
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
-module(overload). 

-export([start_link/0, request/0, set_config_data/2,
	 get_overload_info/0]).

-export([init/1, handle_call/3, handle_info/2, terminate/2,
	 format_status/2]).

%%%-----------------------------------------------------------------
%%% This is a rewrite of overload from BS.3, by Peter Högfeldt.
%%%
%%% DESCRIPTION
%%%
%%% This module implements a server process that keeps record of the
%%% intensity of calls of the request/0 function, and answers accept or
%%% reject depending on if the current average intensity is not greater 
%%% than a specified maximum intensity.
%%%
%%% The intensity i is calculated according to the formula:
%%%    i(n) = exp(-K*(T(n) - T(n-1)))*i(n-1) + Kappa
%%% where i(n) is the intensity at event n, Kappa is a constant, and
%%% T(n) is the time at event n.
%%%
%%% The constant Kappa can be thought of as 1 / T, where T is the time 
%%% constant. Kappa is externally referred to as Weight.
%%%  
%%% We keep track of two intensities: the total call intensity and the 
%%% intensity of accepted calls.
%%%-----------------------------------------------------------------
%%% TODO
%%%
%%% 3. Hysteresis.
%%%
%%%-----------------------------------------------------------------

-record(state, {total = 0, accept = 0, max, prev_t = get_now(),
		kappa, call_counts = {0, 0}, alarm = clear}).

-define(clear_timeout, 30000).
  
start_link() ->
    gen_server:start_link({local, overload}, overload, [], []).

init([]) ->
    process_flag(priority, high),
    MaxIntensity = fetch_config_data(overload_max_intensity),
    Kappa = fetch_config_data(overload_weight),
    {ok, #state{max = MaxIntensity, kappa = Kappa}}.
  
%%-----------------------------------------------------------------
%% Func: request/0
%% Purpose: This is a request to proceed, e.g. a request to
%%          establish a call.
%% Returns: accept | reject
%%-----------------------------------------------------------------
request() -> call(request).
  
%%-----------------------------------------------------------------
%% Func: set_config_data/2
%% Purpose: Set configuration data, and reset intensities.
%% Arguments: MaxIntensity (real > 0), Weight (real > 0).
%% Returns: ok | {error, What}
%% This function is for debugging purposes and is therefore not
%% documented at all.
%%-----------------------------------------------------------------
set_config_data(MaxIntensity, Weight) ->
    call({set_config_data, MaxIntensity, Weight}).
%%-----------------------------------------------------------------
%% Func: get_overload_info/0
%% Returns: A list of tagged items: TotalIntensity, AcceptIntensity,
%%          MaxIntensity, Weight, TotalRequests, AcceptedRequests.
%%-----------------------------------------------------------------
get_overload_info() -> call(get_overload_info).

%%-----------------------------------------------------------------
%% call(Request) -> Term
%%-----------------------------------------------------------------
call(Req) ->
    gen_server:call(overload, Req, infinity).

%%%-----------------------------------------------------------------
%%% Callback functions from gen_server
%%%-----------------------------------------------------------------
handle_call(request, _From, State) ->
    #state{total = TI, accept = AI, kappa = Kappa, prev_t = PrevT,
	  alarm = Alarm} = State,
    {TR, AR} = State#state.call_counts,
    T = get_now(),
    CurI = new_intensity(AI, T, PrevT, Kappa),
    NewTI  = new_intensity(TI, T, PrevT, Kappa) + Kappa,
    if 
	CurI =< State#state.max ->
	    %% Hysteresis: If alarm is set, and current intensity has
	    %% fallen below 75% of max intensity, clear alarm.
	    NewAlarm = if
			   CurI =< 0.75*State#state.max ->
			       clear_alarm(Alarm);
			   true ->
			       Alarm
		       end,
	    {reply, accept, State#state{call_counts = {TR+1, AR+1}, 
					prev_t = T, total = NewTI, 
					accept = CurI + Kappa, 
				        alarm = NewAlarm},
	     ?clear_timeout};
        true ->
	    %% Set alarm if not already set.
	    NewAlarm = set_alarm(Alarm),
	    {reply, reject,
	     State#state{call_counts = {TR+1, AR}, prev_t = T,
			 total = NewTI, accept = CurI,
				       alarm = NewAlarm},
	    ?clear_timeout}
    end;
handle_call({set_config_data, MaxIntensity, Weight}, _From, _State) ->
    {reply, ok, #state{max = MaxIntensity, kappa = Weight},
     ?clear_timeout};
handle_call(get_overload_info, _From, State) ->
    #state{max = MI, total = TI, accept = AI, kappa = Kappa,
	   prev_t = PrevT, call_counts = {TR, AR}} = State,
    T = get_now(),
    CurI = new_intensity(AI, T, PrevT, Kappa),
    NewTI  = new_intensity(TI, T, PrevT, Kappa),
    Reply = [{total_intensity, NewTI}, {accept_intensity, CurI}, 
	     {max_intensity, MI}, {weight, Kappa},
	     {total_requests, TR}, {accepted_requests, AR}],
    {reply, Reply, State#state{total = NewTI, accept = CurI},
     ?clear_timeout}.

handle_info(timeout, State) ->
    #state{total = TI, accept = AI, kappa = Kappa, prev_t = PrevT,
	  alarm = Alarm} = State,
    T = get_now(),
    CurI = new_intensity(AI, T, PrevT, Kappa),
    NewTI  = new_intensity(TI, T, PrevT, Kappa),
    if 
	CurI < 0.75* State#state.max ->
	    NewAlarm = clear_alarm(Alarm),
	    {noreply, State#state{total = NewTI, accept = CurI, 
				  alarm = NewAlarm}};
	    
	true ->
	    {noreply, State#state{total = NewTI, accept = CurI},
	     ?clear_timeout}
    end;

handle_info(_, State) ->
    {noreply, State, ?clear_timeout}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
fetch_config_data(Tag) ->
    case application:get_env(sasl, Tag) of
	{ok, Value} -> Value;
	_ -> fetch_default_data(Tag)
    end.
  
fetch_default_data(overload_max_intensity) -> 0.8;
fetch_default_data(overload_weight) -> 0.1.

set_alarm(clear) ->
    alarm_handler:set_alarm({overload, []}),
    set;
set_alarm(Alarm) ->
    Alarm.

clear_alarm(set) ->
    alarm_handler:clear_alarm(overload),
    clear;
clear_alarm(Alarm) ->
    Alarm.

%%-----------------------------------------------------------------
%% The catch protects against floating-point exception.
%%
new_intensity(I, T, PrevT, K) ->
    Diff = sub(T, PrevT)/1000,
    case catch (I*math:exp(-K*Diff)) of
	{'EXIT', _} ->				% Assume zero.
	    0.0;
	Res ->
	    Res
    end.

%% Mask equal to 2^27 - 1, used below.
-define(mask27, 16#7ffffff).

%% Returns number of milliseconds in the range [0, 2^27 - 1]. Must have
%% this since statistics(wall_clock) wraps. Having 2^27 -1 as the max
%% assures that we always get non-negative integers. 2^27 milliseconds 
%% are approx. 37.28 hours.
get_now() -> 
    element(1, statistics(wall_clock)) band ?mask27.

%% Returns (X - Y) mod 2^27 (which is in the range [0, 2^27 - 1]).
sub(X, Y) ->
    (X + (bnot Y) + 1) band ?mask27.

format_status(Opt, [PDict, #state{max = MI, total = TI, accept = AI,
				  kappa = K,
				  call_counts = {TR, AR}}]) ->
    [{data, [{"Total Intensity", TI},
	     {"Accept Intensity", AI}, 
	     {"Max Intensity", MI},
	     {"Weight", K},
	     {"Total requests", TR},
	     {"Accepted requests", AR}]} |
     misc_supp:format_pdict(Opt, PDict, [])].     
