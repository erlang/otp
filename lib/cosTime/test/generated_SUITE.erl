%%-----------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
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
%%-----------------------------------------------------------------
%% File    : generated_SUITE.erl
%% Purpose : 
%%-----------------------------------------------------------------

-module(generated_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").

-define(default_timeout, test_server:minutes(3)).

-define(match(ExpectedRes, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			AcTuAlReS;
		    _ ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS)
		end
	end()).

-define(nomatch(Not, Expr),
        fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    Not ->
			io:format("###### ERROR ERROR ######~n~p~n",
				  [AcTuAlReS]),
			exit(AcTuAlReS);
		    _ ->
			AcTuAlReS
		end
	end()).


-define(checktc(_Op),
        fun(TC) ->
		case orber_tc:check_tc(TC) of
		    false ->
			io:format("###### ERROR ERROR ######~n~p - ~p~n", [Op, TC]),
			exit(TC);
		    true ->
			true
		end
	end).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-compile(export_all).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    ['TimeBase_IntervalT', 'TimeBase_UtcT',
     'CosTime_TimeUnavailable', 'CosTimerEvent_TimerEventT',
     'CosTime_TIO', 'CosTime_TimeService', 'CosTime_UTO',
     'CosTimerEvent_TimerEventHandler',
     'CosTimerEvent_TimerEventService'].

groups() -> 
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'TimeBase_IntervalT'
%% Description: 
%%-----------------------------------------------------------------
'TimeBase_IntervalT'(_) ->
    ?match(true, orber_tc:check_tc('TimeBase_IntervalT':tc())),
    ?match("IDL:omg.org/TimeBase/IntervalT:1.0", 
	   'TimeBase_IntervalT':id()),
    ?match("TimeBase_IntervalT", 
	   'TimeBase_IntervalT':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'TimeBase_UtcT'
%% Description: 
%%-----------------------------------------------------------------
'TimeBase_UtcT'(_) ->
    ?match(true, orber_tc:check_tc('TimeBase_UtcT':tc())),
    ?match("IDL:omg.org/TimeBase/UtcT:1.0", 
	   'TimeBase_UtcT':id()),
    ?match("TimeBase_UtcT", 
	   'TimeBase_UtcT':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTime_TimeUnavailable'
%% Description: 
%%-----------------------------------------------------------------
'CosTime_TimeUnavailable'(_) ->
    ?match(true, orber_tc:check_tc('CosTime_TimeUnavailable':tc())),
    ?match("IDL:omg.org/CosTime/TimeUnavailable:1.0", 
	   'CosTime_TimeUnavailable':id()),
    ?match("CosTime_TimeUnavailable", 
	   'CosTime_TimeUnavailable':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTimerEvent_TimerEventT'
%% Description: 
%%-----------------------------------------------------------------
'CosTimerEvent_TimerEventT'(_) ->
    ?match(true, orber_tc:check_tc('CosTimerEvent_TimerEventT':tc())),
    ?match("IDL:omg.org/CosTimerEvent/TimerEventT:1.0", 
	   'CosTimerEvent_TimerEventT':id()),
    ?match("CosTimerEvent_TimerEventT", 
	   'CosTimerEvent_TimerEventT':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTime_TIO'
%% Description: 
%%-----------------------------------------------------------------
'CosTime_TIO'(_) ->
    ?nomatch(undefined, 'CosTime_TIO':oe_tc('_get_time_interval')),
    ?nomatch(undefined, 'CosTime_TIO':oe_tc(spans)),
    ?nomatch(undefined, 'CosTime_TIO':oe_tc(overlaps)),
    ?nomatch(undefined, 'CosTime_TIO':oe_tc(time)),
    ?match(undefined, 'CosTime_TIO':oe_tc(undefined)),
    ?match([_|_], 'CosTime_TIO':oe_get_interface()),
    ?match("IDL:omg.org/CosTime/TIO:1.0", 'CosTime_TIO':typeID()),
    check_tc('CosTime_TIO':oe_get_interface()),
    ?match(true, 'CosTime_TIO':oe_is_a('CosTime_TIO':typeID())),
    ?match(false, 'CosTime_TIO':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTime_TimeService'
%% Description: 
%%-----------------------------------------------------------------
'CosTime_TimeService'(_) ->
    ?nomatch(undefined, 'CosTime_TimeService':oe_tc(universal_time)),
    ?nomatch(undefined, 'CosTime_TimeService':oe_tc(secure_universal_time)),
    ?nomatch(undefined, 'CosTime_TimeService':oe_tc(new_universal_time)),
    ?nomatch(undefined, 'CosTime_TimeService':oe_tc(uto_from_utc)),
    ?nomatch(undefined, 'CosTime_TimeService':oe_tc(new_interval)),
    ?match(undefined, 'CosTime_TimeService':oe_tc(undefined)),
    ?match([_|_], 'CosTime_TimeService':oe_get_interface()),
    ?match("IDL:omg.org/CosTime/TimeService:1.0", 
	   'CosTime_TimeService':typeID()),
    check_tc('CosTime_TimeService':oe_get_interface()),
    ?match(true, 'CosTime_TimeService':oe_is_a('CosTime_TimeService':typeID())),
    ?match(false, 'CosTime_TimeService':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTime_UTO'
%% Description: 
%%-----------------------------------------------------------------
'CosTime_UTO'(_) ->
    ?nomatch(undefined, 'CosTime_UTO':oe_tc('_get_time')),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc('_get_inaccuracy')),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc('_get_tdf')),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc('_get_utc_time')),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc(absolute_time)),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc(compare_time)),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc(time_to_interval)),
    ?nomatch(undefined, 'CosTime_UTO':oe_tc(interval)),
    ?match(undefined, 'CosTime_UTO':oe_tc(undefined)),
    ?match([_|_], 'CosTime_UTO':oe_get_interface()),
    ?match("IDL:omg.org/CosTime/UTO:1.0", 'CosTime_UTO':typeID()),
    check_tc('CosTime_UTO':oe_get_interface()),
    ?match(true, 'CosTime_UTO':oe_is_a('CosTime_UTO':typeID())),
    ?match(false, 'CosTime_UTO':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTimerEvent_TimerEventHandler'
%% Description: 
%%-----------------------------------------------------------------
'CosTimerEvent_TimerEventHandler'(_) ->
    ?nomatch(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc('_get_status')),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc(time_set)),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc(set_timer)),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc(cancel_timer)),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc(set_data)),
    ?match(undefined, 'CosTimerEvent_TimerEventHandler':oe_tc(undefined)),
    ?match([_|_], 'CosTimerEvent_TimerEventHandler':oe_get_interface()),
    ?match("IDL:omg.org/CosTimerEvent/TimerEventHandler:1.0", 
	   'CosTimerEvent_TimerEventHandler':typeID()),
    check_tc('CosTimerEvent_TimerEventHandler':oe_get_interface()),
    ?match(true, 'CosTimerEvent_TimerEventHandler':oe_is_a('CosTimerEvent_TimerEventHandler':typeID())),
    ?match(false, 'CosTimerEvent_TimerEventHandler':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosTimerEvent_TimerEventService'
%% Description: 
%%-----------------------------------------------------------------
'CosTimerEvent_TimerEventService'(_) ->
    ?nomatch(undefined, 'CosTimerEvent_TimerEventService':oe_tc(register)),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventService':oe_tc(unregister)),
    ?nomatch(undefined, 'CosTimerEvent_TimerEventService':oe_tc(event_time)),
    ?match(undefined, 'CosTimerEvent_TimerEventService':oe_tc(undefined)),
    ?match([_|_], 'CosTimerEvent_TimerEventService':oe_get_interface()),
    ?match("IDL:omg.org/CosTimerEvent/TimerEventService:1.0", 
	   'CosTimerEvent_TimerEventService':typeID()),
    check_tc('CosTimerEvent_TimerEventService':oe_get_interface()),
    ?match(true, 'CosTimerEvent_TimerEventService':oe_is_a('CosTimerEvent_TimerEventService':typeID())),
    ?match(false, 'CosTimerEvent_TimerEventService':oe_is_a("wrong")),
    ok.




%%-----------------------------------------------------------------
%% MISC functions
%%-----------------------------------------------------------------
check_tc([]) ->
    ok;
check_tc([{Op, {RetType, InParameters, OutParameters}}|T]) ->
    io:format("checked - ~s~n", [Op]),
    lists:all(?checktc(Op), [RetType|InParameters]),
    lists:all(?checktc(Op), OutParameters),
    check_tc(T).
    
    
