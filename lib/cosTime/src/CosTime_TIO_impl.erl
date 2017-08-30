%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2016. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% File    : CosTime_TIO_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosTime_TIO_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Attributes (external)
-export(['_get_time_interval'/2]).

%% Interface functions
-export([spans/3, overlaps/3, time/2]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, {interval,
		tdf,
		timer}).
%% Data structures constructors
-define(get_InitState(I,T,TO), 
	#state{interval = I,
	       tdf      = T,
	       timer    = TO}).

%% Data structures selectors
-define(get_IntervalT(S),      S#state.interval).
-define(get_Lower(S),          (S#state.interval)#'TimeBase_IntervalT'.lower_bound).
-define(get_Upper(S),          (S#state.interval)#'TimeBase_IntervalT'.upper_bound).
-define(get_Tdf(S),            S#state.tdf).
-define(get_TimerObj(S),       S#state.timer).

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([Interval, Tdf, Timer]) ->
    {ok, ?get_InitState(Interval, Tdf, Timer)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------------------------ attributes -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_time_interval'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_time_interval'(_OE_THIS, State) ->
    {reply, ?get_IntervalT(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : spans
%% Arguments: Time - UTO
%% Returns  : CosTime::OverLapType - enum()
%%            TIO - out-parameter.
%%-----------------------------------------------------------
spans(_OE_THIS, State, Time) ->
    ?time_TypeCheck(Time, 'CosTime_UTO'),
    case catch 'CosTime_UTO':'_get_utc_time'(Time) of
	#'TimeBase_UtcT'{time = Btime, inacclo = InaccL, inacchi=InaccH} ->
	    Inaccuarcy = ?concat_TimeT(InaccH, InaccL),
	    BL = Btime - Inaccuarcy,
	    BU = Btime + Inaccuarcy,
	    L = ?get_Lower(State),
	    U = ?get_Upper(State),
	    {Type, NewL, NewU} = 
		if
		    L=<BL, U>=BU ->
			{'OTContainer',BL,BU};
		    L>=BL, U=<BU ->
			{'OTContained',L,U};
		    L<BL, U=<BU, U>=BL ->
			{'OTOverlap',BL,U};
		    L>=BL, L=<BU, U>BU ->
			{'OTOverlap',L,BU};
		    L>BU ->
			{'OTNoOverlap',BU,L};
		    true ->
			{'OTNoOverlap',U,BL}
		end,
	    {reply, 
	     {Type,
	      'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=NewL, 
							     upper_bound=NewU},
				       ?get_Tdf(State),
				       ?get_TimerObj(State)], 
				      [{pseudo,true}|?CREATE_OPTS])}, 
	     State};
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.


%%----------------------------------------------------------%
%% function : overlaps
%% Arguments: Interval - TIO
%% Returns  : CosTime::OverLapType - enum()
%%            TIO - out-parameter.
%%-----------------------------------------------------------
overlaps(_OE_THIS, State, Interval) ->
    ?time_TypeCheck(Interval, 'CosTime_TIO'),
    case catch 'CosTime_TIO':'_get_time_interval'(Interval) of
	#'TimeBase_IntervalT'{lower_bound=BL, upper_bound=BU} ->
	    L = ?get_Lower(State),
	    U = ?get_Upper(State),
	    {Type, NewL, NewU} = 
		if
		    L=<BL, U>=BU ->
			{'OTContainer',BL,BU};
		    L>=BL, U=<BU ->
			{'OTContained',L,U};
		    L<BL, U=<BU, U>=BL ->
			{'OTOverlap',BL,U};
		    L>=BL, L=<BU, U>BU ->
			{'OTOverlap',L,BU};
		    L>BU ->
			{'OTNoOverlap',BU,L};
		    true ->
			{'OTNoOverlap',U,BL}
		end,
	    {reply, 
	     {Type,
	      'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=NewL, 
							     upper_bound=NewU},
				       ?get_Tdf(State),
				       ?get_TimerObj(State)], 
				      [{pseudo,true}|?CREATE_OPTS])}, 
	     State};
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : time
%% Arguments: -
%% Returns  : UTO
%%-----------------------------------------------------------
time(_OE_THIS, State) ->
    L = ?get_Lower(State),
    H = ?get_Upper(State),
    Utc = #'TimeBase_UtcT'{time=(erlang:trunc(((H-L)/2))+L),
			   inacclo=L,
			   inacchi=H,
			   tdf=?get_Tdf(State)},
    {reply, 
     'CosTime_UTO':oe_create([Utc, ?get_TimerObj(State)], [{pseudo,true}|?CREATE_OPTS]), 
     State}.


%%--------------- LOCAL FUNCTIONS ----------------------------

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------
