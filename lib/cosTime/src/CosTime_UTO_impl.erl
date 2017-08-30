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
%% File    : CosTime_UTO_impl.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module('CosTime_UTO_impl').

%%--------------- INCLUDES -----------------------------------
-include("cosTimeApp.hrl").


%%--------------- EXPORTS ------------------------------------
%%--------------- External -----------------------------------
%% Attributes (external)
-export(['_get_time'/2,
	 '_get_inaccuracy'/2,
	 '_get_tdf'/2,
	 '_get_utc_time'/2]).

%% Interface functions
-export([absolute_time/2, compare_time/4]).
-export([time_to_interval/3, interval/2]).

%%--------------- gen_server specific exports ----------------
-export([handle_info/2, code_change/3]).
-export([init/1, terminate/2]).


%% Data structures
-record(state, {timer, utc}).
%% Data structures constructors
-define(get_InitState(U, T), #state{timer = T, utc = U}).

%% Data structures selectors
-define(get_Time(S),        (S#state.utc)#'TimeBase_UtcT'.time).
-define(get_Inaccuracy(S),  ?concat_TimeT((S#state.utc)#'TimeBase_UtcT'.inacchi, 
					  (S#state.utc)#'TimeBase_UtcT'.inacclo)).
-define(get_Tdf(S),         (S#state.utc)#'TimeBase_UtcT'.tdf).
-define(get_Utc(S),         S#state.utc).
-define(get_TimeObj(S),     S#state.timer).


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

init([Utc, TimeObj]) ->
    {ok, ?get_InitState(Utc, TimeObj)}.

terminate(_Reason, _State) ->
    ok.

%%-----------------------------------------------------------
%%------------------------ attributes -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% Attribute: '_get_time'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_time'(_OE_THIS, State) ->
    {reply, ?get_Time(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_inaccuracy'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_inaccuracy'(_OE_THIS, State) ->
    {reply, ?get_Inaccuracy(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_tdf'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_tdf'(_OE_THIS, State) ->
    {reply, ?get_Tdf(State), State}.

%%----------------------------------------------------------%
%% Attribute: '_get_utc_time'
%% Type     : readonly
%% Returns  : 
%%-----------------------------------------------------------
'_get_utc_time'(_OE_THIS, State) ->
    {reply, ?get_Utc(State), State}.

%%-----------------------------------------------------------
%%------- Exported external functions -----------------------
%%-----------------------------------------------------------
%%----------------------------------------------------------%
%% function : absolute_time
%% Arguments: -
%% Returns  : UTO
%% NOTE     : Return the base time to the relative time in the object.
%%-----------------------------------------------------------
absolute_time(_OE_THIS, State) ->
    case catch 'CosTime_UTO':'_get_time'(
		 'CosTime_TimeService':universal_time(?get_TimeObj(State)))+
	?get_Time(State) of
	UniTime when is_integer(UniTime) andalso UniTime =< ?max_TimeT ->
	    Utc=?get_Utc(State),
	    {reply, 'CosTime_UTO':oe_create([Utc#'TimeBase_UtcT'{time=UniTime},
					     ?get_TimeObj(State)], 
					    [{pseudo,true}|?CREATE_OPTS]), State};
	UniTime when is_integer(UniTime) ->
	    %% Oopss, overflow!
	    corba:raise(#'DATA_CONVERSION'{completion_status=?COMPLETED_NO});
	_ ->
	    corba:raise(#'INTERNAL'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : compare_time
%% Arguments: Comparison_type - CosTime::ComparisonType
%%            Uto - ObjRef
%% Returns  : TimeComparison - 'TCEqualTo' | 'TCLessThan' | 
%%                             'TCGreaterThan' | 'TCIndeterminate'
%%-----------------------------------------------------------
compare_time(_OE_THIS, State, 'IntervalC', Uto) ->
    ?time_TypeCheck(Uto, 'CosTime_UTO'),
    case catch {'CosTime_UTO':'_get_time'(Uto),  
		'CosTime_UTO':'_get_inaccuracy'(Uto)} of
	{Time,Inaccuracy} when is_integer(Time) andalso is_integer(Inaccuracy) ->
	    OwnInacc = ?get_Inaccuracy(State),
	    if 
		?get_Time(State)+OwnInacc < Time-Inaccuracy ->
		    {reply, 'TCLessThan', State};
		?get_Time(State)-OwnInacc > Time+Inaccuracy ->
		    {reply, 'TCGreaterThan', State};
		?get_Time(State) == Time, Inaccuracy==0, OwnInacc==0 ->
		    %% TimeService specification (july 1997, p14-7:2) states
		    %% that they are only equal if both UTO's Inaccuracy
		    %% equals zero.
		    {reply, 'TCEqualTo', State};
		true ->
		    {reply, 'TCIndeterminate', State}
	    end;
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
compare_time(_OE_THIS, State, 'MidC', Uto) ->
    ?time_TypeCheck(Uto, 'CosTime_UTO'),
    case catch 'CosTime_UTO':'_get_time'(Uto) of
	Time when is_integer(Time) ->
	    if 
		?get_Time(State) < Time ->
		    {reply, 'TCLessThan', State};
		?get_Time(State) > Time ->
		    {reply, 'TCGreaterThan', State};
		true ->
		    {reply, 'TCEqualTo', State}
	    end;
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end;
compare_time(_OE_THIS, _State, _, _) ->
    %% Comparison_type given not correct?!
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------%
%% function : time_to_interval
%% Arguments: 
%% Returns  : TIO
%%-----------------------------------------------------------
time_to_interval(_OE_THIS, State, Uto) ->
    ?time_TypeCheck(Uto, 'CosTime_UTO'),
    case catch 'CosTime_UTO':'_get_time'(Uto) of
	Time when is_integer(Time) ->
	    OwnTime = ?get_Time(State),
	    if 
		Time > OwnTime ->
		    {reply, 'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=OwnTime, 
									   upper_bound=Time},
						     ?get_Tdf(State),
						     ?get_TimeObj(State)], 
						    [{pseudo,true}|?CREATE_OPTS]), State};
		true ->
		    {reply, 'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=Time, 
									   upper_bound=OwnTime},
						     ?get_Tdf(State),
						     ?get_TimeObj(State)], 
						    [{pseudo,true}|?CREATE_OPTS]), State}
	    end;
	_ ->
	    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO})
    end.

%%----------------------------------------------------------%
%% function : interval
%% Arguments: 
%% Returns  : TIO
%%-----------------------------------------------------------
interval(_OE_THIS, State) ->
    Lower = ?get_Time(State) - ?get_Inaccuracy(State),
    Upper = ?get_Time(State) + ?get_Inaccuracy(State),
    {reply, 'CosTime_TIO':oe_create([#'TimeBase_IntervalT'{lower_bound=Lower, 
							   upper_bound=Upper},
				     ?get_Tdf(State),
				     ?get_TimeObj(State)], 
				    [{pseudo,true}|?CREATE_OPTS]), State}.


%%--------------- LOCAL FUNCTIONS ----------------------------

%%--------------- MISC FUNCTIONS, E.G. DEBUGGING -------------
%%--------------- END OF MODULE ------------------------------

