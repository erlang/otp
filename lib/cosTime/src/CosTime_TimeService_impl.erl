%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
%%
%%----------------------------------------------------------------------
%% File    : CosTime_TimeService_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosTime_TimeService_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").

%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Interface functions
-export([universal_time/2, secure_universal_time/2, new_universal_time/5]).
-export([uto_from_utc/3, new_interval/4]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, 
	{tdf,
	 inaccuracy}).
%% Data structures constructors
-define(get_InitState(T,I), 
	#state{tdf        = T,
	       inaccuracy = I}).

%% Data structures selectors
-define(get_Inaccuracy(S),  S#state.inaccuracy). 
-define(get_Tdf(S),         S#state.tdf). 

%% Data structures modifiers

%% MISC

%%-----------------------------------------------------------%
%% function : handle_info, code_change
%% Arguments: 
%% Returns  : 
%% Effect   : Functions demanded by the gen_server module. 
%%------------------------------------------------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    ?debug_print("INFO: ~p~n", [_Info]),
    {noreply, State}.    

%%----------------------------------------------------------%
%% function : init, terminate
%% Arguments: 
%%-----------------------------------------------------------

init([Tdf, Inaccuracy]) ->
    process_flag(trap_exit, true),
    {ok, ?get_InitState(Tdf, Inaccuracy)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : universal_time
%% Arguments: -
%% Returns  : CosTime::UTO |
%%            {'EXCEPTION", #'CosTime_TimeUnavailable'{}}
%% NOTE     : cosTime:create_universal_time will raise the correct 
%%            exception.
%%-----------------------------------------------------------
universal_time(OE_THIS, State) ->
    {ok, Time} = create_universal_time(),
    Inaccuracy = ?get_Inaccuracy(State),
    Utc = #'TimeBase_UtcT'{time=Time, inacclo = ?low_TimeT(Inaccuracy), 
			   inacchi = ?high_TimeT(Inaccuracy), 
			   tdf = ?get_Tdf(State)},
    {reply, 'CosTime_UTO':oe_create([Utc, OE_THIS], [{pseudo,true}|?CREATE_OPTS]), State}.

%%----------------------------------------------------------%
%% function : secure_universal_time
%% Arguments: 
%% Returns  : {'EXCEPTION", #'CosTime_TimeUnavailable'{}}
%%-----------------------------------------------------------
secure_universal_time(_OE_THIS, _State) ->
    corba:raise(#'CosTime_TimeUnavailable'{}).

%%----------------------------------------------------------%
%% function : new_universal_time
%% Arguments: Time - TimeBase::TimeT
%%            Inaccuracy - TimeBase::InaccuracyT inaccuracy
%%            Tdf - TimeBase::TdfT
%% Returns  : CosTime::UTO
%%-----------------------------------------------------------
new_universal_time(OE_THIS, State, Time, Inaccuracy, Tdf) when 
  is_integer(Time) andalso is_integer(Inaccuracy) andalso is_integer(Tdf) andalso
  Tdf=<12 andalso Inaccuracy=<?max_Inaccuracy andalso Time=<?max_TimeT ->
    Utc = #'TimeBase_UtcT'{time=Time, inacclo = ?low_TimeT(Inaccuracy), 
			   inacchi = ?high_TimeT(Inaccuracy), tdf = Tdf},
    {reply, 'CosTime_UTO':oe_create([Utc, OE_THIS], [{pseudo,true}|?CREATE_OPTS]), State};
new_universal_time(_, _, _, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : uto_from_utc
%% Arguments: Utc - TimeBase::UtcT
%% Returns  : CosTime::UTO
%%-----------------------------------------------------------
uto_from_utc(OE_THIS, State, Utc) when is_record(Utc, 'TimeBase_UtcT') ->
    {reply, 'CosTime_UTO':oe_create([Utc, OE_THIS],[{pseudo,true}|?CREATE_OPTS]),State};
uto_from_utc(_, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : new_interval
%% Arguments: Lower - TimeBase::TimeT
%%            Upper - TimeBase::TimeT
%% Returns  : CosTime::TIO
%%-----------------------------------------------------------
new_interval(OE_THIS, State, Lower, Upper) when is_integer(Lower) andalso is_integer(Upper) andalso
						Lower=<Upper ->
    {reply, 'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=Lower, 
							   upper_bound=Upper},
				     ?get_Tdf(State),
				     OE_THIS], 
				    [{pseudo,true}|?CREATE_OPTS]), State};
new_interval(_, _, _, _) ->
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).


%%--------------- LOCAL FUNCTIONS ----------------------------
%%-----------------------------------------------------------%
%% function : create_universal_utc
%% Arguments: -
%% Returns  : TimeT or raises exception.
%% Effect   : Creates a universal time; if time unavailable we
%%            must raise CosTime_TimeUnavailable.
%% NOTE     : 'datetime_to_gregorian_seconds' use year 0 as time
%%            base. We want to use 15 october 1582, 00:00 as base.
%%------------------------------------------------------------

create_universal_time() ->
    %% Time is supposed to be #100 nano-secs passed.
    %% We add micro secs for a greater precision.
    {MS,S,US} = now(),
    case catch calendar:datetime_to_gregorian_seconds(
		 calendar:now_to_universal_time({MS,S,US})) of
	Secs when is_integer(Secs) ->
	    {ok, (Secs-?ABSOLUTE_TIME_DIFF)*10000000 + US*10};
	_ ->
	    corba:raise(#'CosTime_TimeUnavailable'{})
    end.

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------

