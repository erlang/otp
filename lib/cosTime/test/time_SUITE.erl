%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2000-2011. All Rights Reserved.
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
%% File    : time_SUITE.erl
%% Purpose : 
%%----------------------------------------------------------------------

-module(time_SUITE).


%%--------------- INCLUDES -----------------------------------
-include_lib("cosTime/src/cosTimeApp.hrl").

-include_lib("test_server/include/test_server.hrl").

%%--------------- DEFINES ------------------------------------
-define(default_timeout, ?t:minutes(20)).
-define(match(ExpectedRes, Expr),
        fun() ->
               AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   ExpectedRes ->
                       io:format("------ CORRECT RESULT ------~n~p~n",
                                 [AcTuAlReS]),
                       AcTuAlReS;
                   _ ->
                       io:format("###### ERROR ERROR ######~n~p~n",
                                 [AcTuAlReS]),
                       exit(AcTuAlReS)
               end
       end()).
 
-define(match_inverse(NotExpectedRes, Expr),
        fun() ->
                AcTuAlReS = (catch (Expr)),
               case AcTuAlReS of
                   NotExpectedRes ->
                       io:format("###### ERROR ERROR ######~n ~p~n",
                                 [AcTuAlReS]),
                       exit(AcTuAlReS);
                   _ ->
                       io:format("------ CORRECT RESULT ------~n~p~n",
                                 [AcTuAlReS]),
                       AcTuAlReS
               end
       end()).
 

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0, 
	 init_per_suite/1, end_per_suite/1, time_api/1, timerevent_api/1,
	 init_per_testcase/2, end_per_testcase/2,
	 app_test/1]).

%%-----------------------------------------------------------------
%% Func: all/1
%% Args: 
%% Returns: 
%%-----------------------------------------------------------------
suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> 
    cases().

groups() -> 
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.


cases() -> 
    [time_api, timerevent_api, app_test].


	
%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------

init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    orber:install([node()]),
    application:start(mnesia),
    application:start(orber),
    cosNotificationApp:install_event(),
    cosNotificationApp:install(),
    cosTime:install_time(),
    cosTime:install_timerevent(),
    if
        is_list(Config) ->
	    Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    cosTime:uninstall_time(),
    cosTime:uninstall_timerevent(),
    cosNotificationApp:uninstall(),
    cosNotificationApp:uninstall_event(),
    application:stop(orber),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    Config.

%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(doc) -> [];
app_test(suite) -> [];
app_test(_Config) ->
    ok=test_server:app_test(cosTime),
    ok.

%%-----------------------------------------------------------------
%%  CosTime API tests 
%%-----------------------------------------------------------------
time_api(doc) -> ["CosTime API tests.", ""];
time_api(suite) -> [];
time_api(_Config) ->
    ?line ?match(ok, application:start(cosTime)),
    TS=cosTime:start_time_service(0, 500),
    Time=calendar:datetime_to_gregorian_seconds({{1582,1,1},{0,0,0}}),
    Inaccuracy = 1000,
    Tdf =1,
    Utc = #'TimeBase_UtcT'{time=Time, inacclo = ?low_TimeT(Inaccuracy), 
			   inacchi = ?high_TimeT(Inaccuracy), tdf = Tdf},
    ?line UTO1='CosTime_TimeService':new_universal_time(TS, Time, Inaccuracy, Tdf),
    ?line UTO2='CosTime_TimeService':uto_from_utc(TS, Utc),
    ?line ?match(Time, 'CosTime_UTO':'_get_time'(UTO1)),
    ?line ?match(Inaccuracy, 'CosTime_UTO':'_get_inaccuracy'(UTO1)),
    ?line ?match(Tdf, 'CosTime_UTO':'_get_tdf'(UTO1)),
    ?line ?match(Utc, 'CosTime_UTO':'_get_utc_time'(UTO1)),

    ?line ?match(Time, 'CosTime_UTO':'_get_time'(UTO2)),
    ?line ?match(Inaccuracy, 'CosTime_UTO':'_get_inaccuracy'(UTO2)),
    ?line ?match(Tdf, 'CosTime_UTO':'_get_tdf'(UTO2)),
    ?line ?match(Utc, 'CosTime_UTO':'_get_utc_time'(UTO2)),

    TIO1='CosTime_TimeService':new_interval(TS, 2, 5),
    _TIO2='CosTime_TimeService':new_interval(TS, 3, 6),
    TIO3='CosTime_TimeService':new_interval(TS, 1, 3),
    TIO4='CosTime_TimeService':new_interval(TS, 3, 4),
    TIO5='CosTime_TimeService':new_interval(TS, 7, 8),
    TIO6='CosTime_TimeService':new_interval(TS, 2, 6),
    TIO7='CosTime_TimeService':new_interval(TS, 3, 7),

    ?line {_,TIO8} = ?match({'OTContained', _}, 'CosTime_TIO':overlaps(TIO1, TIO6)),
    ?line {_,TIO9} = ?match({'OTContainer', _}, 'CosTime_TIO':overlaps(TIO1, TIO1)),
    ?line {_,TIO10} = ?match({'OTContainer', _}, 'CosTime_TIO':overlaps(TIO1, TIO4)),
    ?line {_,TIO11} = ?match({'OTOverlap', _}, 'CosTime_TIO':overlaps(TIO1, TIO3)),
    ?line {_,TIO12} = ?match({'OTOverlap', _}, 'CosTime_TIO':overlaps(TIO1, TIO7)),
    ?line {_,TIO13} = ?match({'OTNoOverlap', _}, 'CosTime_TIO':overlaps(TIO1, TIO5)),
    
    ?line ?match({'TimeBase_IntervalT',2,5},'CosTime_TIO':'_get_time_interval'(TIO8)),
    ?line ?match({'TimeBase_IntervalT',2,5},'CosTime_TIO':'_get_time_interval'(TIO9)),
    ?line ?match({'TimeBase_IntervalT',3,4},'CosTime_TIO':'_get_time_interval'(TIO10)),
    ?line ?match({'TimeBase_IntervalT',2,3},'CosTime_TIO':'_get_time_interval'(TIO11)),
    ?line ?match({'TimeBase_IntervalT',3,5},'CosTime_TIO':'_get_time_interval'(TIO12)),
    ?line ?match({'TimeBase_IntervalT',5,7},'CosTime_TIO':'_get_time_interval'(TIO13)),
    
    ?line UTO3='CosTime_TimeService':new_universal_time(TS, 4, 2, 0), %% 2-6
    ?line UTO4='CosTime_TimeService':new_universal_time(TS, 2, 1, 0), %% 1-3
    ?line UTO5='CosTime_TimeService':new_universal_time(TS, 3, 0, 0), %% 3-3
    ?line UTO6='CosTime_TimeService':new_universal_time(TS, 9, 1, 0), %% 8-10
    ?line UTO7='CosTime_TimeService':new_universal_time(TS, 4, 3, 0), %% 1-7
    ?line UTO8='CosTime_TimeService':new_universal_time(TS, 5, 2, 0), %% 3-7

    ?line {_,TIO14} = ?match({'OTContained', _}, 'CosTime_TIO':spans(TIO1, UTO7)),
    ?line {_,TIO15} = ?match({'OTContainer', _}, 'CosTime_TIO':spans(TIO1, UTO5)),
    ?line {_,TIO16} = ?match({'OTOverlap', _}, 'CosTime_TIO':spans(TIO1, UTO4)),
    ?line {_,TIO17} = ?match({'OTOverlap', _}, 'CosTime_TIO':spans(TIO1, UTO8)),
    ?line {_,TIO18} = ?match({'OTNoOverlap', _}, 'CosTime_TIO':spans(TIO1, UTO6)),
    ?line {_,TIO19} = ?match({'OTContained', _}, 'CosTime_TIO':spans(TIO1, UTO3)),

    ?line ?match({'TimeBase_IntervalT',2,5},'CosTime_TIO':'_get_time_interval'(TIO14)),
    ?line ?match({'TimeBase_IntervalT',3,3},'CosTime_TIO':'_get_time_interval'(TIO15)),
    ?line ?match({'TimeBase_IntervalT',2,3},'CosTime_TIO':'_get_time_interval'(TIO16)),
    ?line ?match({'TimeBase_IntervalT',3,5},'CosTime_TIO':'_get_time_interval'(TIO17)),
    ?line ?match({'TimeBase_IntervalT',5,8},'CosTime_TIO':'_get_time_interval'(TIO18)),
    ?line ?match({'TimeBase_IntervalT',2,5},'CosTime_TIO':'_get_time_interval'(TIO19)),


    cosTime:stop_time_service(TS),
    application:stop(cosTime),
    ok.


%%-----------------------------------------------------------------
%%  CosTimerEvent API tests 
%%-----------------------------------------------------------------
timerevent_api(doc) -> ["CosTimerEvent API tests.", ""];
timerevent_api(suite) -> [];
timerevent_api(_Config) ->
    %% Init cosTime apps.
    ?line ?match(ok, application:start(cosTime)),
    ?line TS=cosTime:start_time_service(0, 500),
    ?line TES=cosTime:start_timerevent_service(TS),

    %%----- Initialize the cosNotification application. -----
    ?line cosNotificationApp:start(),
    ?line Fac = (catch cosNotificationApp:start_factory([])),
    ?line {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, [], [])),
    %% Create the Admin objects
    ?line {AdminSupplier, _ASID}= ?match({{_,key,_,_,_,_},_},
             'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'OR_OP')),
    ?line {AdminConsumer, _ACID}= ?match({{_,key,_,_,_,_},_},
             'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'OR_OP')),

    %% Create a push consumer TimerEventService will push events to.
    ?line {ProxyPushConsumer,_ID10}= ?match({{_,key,_,_,_,_},_},
          'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'ANY_EVENT')),

    %% Create a pull suppliers so we can check we actually got the event.
    ?line {ProxyPullSupplier,_ID1} = ?match({{_,key,_,_,_,_},_},
	  'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(AdminConsumer, 'ANY_EVENT')),

    AnyEvent = any:create(orber_tc:long(), 100),
    ?line UTO=?match({_,pseudo,_,_,_,_}, 'CosTime_TimeService':new_universal_time(TS, 10*10000000,1,1)),
    ?line EH=?match({_,key,_,_,_,_}, 'CosTimerEvent_TimerEventService':register(TES, ProxyPushConsumer, AnyEvent)), 

    ?line ?match('ESTimeCleared','CosTimerEvent_TimerEventHandler':'_get_status'(EH)),
    ?line ?match({false,_},'CosTimerEvent_TimerEventHandler':time_set(EH)),
    ?line ?match(ok,'CosTimerEvent_TimerEventHandler':set_timer(EH, 'TTRelative', UTO)),
    ?line ?match({true,_},'CosTimerEvent_TimerEventHandler':time_set(EH)),
    ?line ?match('ESTimeSet','CosTimerEvent_TimerEventHandler':'_get_status'(EH)),
    
    ?line ?match({{any,tk_null,null}, false}, 
	   'CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(ProxyPullSupplier)),

    ?line ?match(AnyEvent, 'CosNotifyChannelAdmin_ProxyPullSupplier':pull(ProxyPullSupplier)),
    ?line ?match('ESTriggered','CosTimerEvent_TimerEventHandler':'_get_status'(EH)),

    %% It's allowed to send an UTO with time eq. to 0 if the server is TTRelative.
    %% When TTAbsolute BAD_PARAM is raised.
    ?line UTO2=?match({_,pseudo,_,_,_,_}, 'CosTime_TimeService':new_universal_time(TS, 0,1,1)),
    ?line ?match({'EXCEPTION',_},'CosTimerEvent_TimerEventHandler':set_timer(EH, 'TTAbsolute', UTO2)),
    ?line ?match(ok,'CosTimerEvent_TimerEventHandler':set_timer(EH, 'TTRelative', UTO2)),
    ?line ?match(AnyEvent, 'CosNotifyChannelAdmin_ProxyPullSupplier':pull(ProxyPullSupplier)),

    %% TTPeriodic is defined to be relative, i.e., we can use the tactic as above.
    ?line ?match(ok,'CosTimerEvent_TimerEventHandler':set_timer(EH, 'TTPeriodic', UTO2)),

    %% Sleep for UTO*2+4 secs. At this point the Timer should have delivered 2 events.
    timer:sleep(24000),
    %% Cancel the timer so no more events will be delivered.
    ?line ?match(true,'CosTimerEvent_TimerEventHandler':cancel_timer(EH)),

    ?line ?match({AnyEvent, true}, 'CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(ProxyPullSupplier)),
    ?line ?match({AnyEvent, true}, 'CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(ProxyPullSupplier)),
    ?line ?match({{any,tk_null,null}, false}, 
	   'CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(ProxyPullSupplier)),



    %% Clean up.
    cosNotificationApp:stop(),
    cosTime:stop_timerevent_service(TES),
    cosTime:stop_time_service(TS),
    application:stop(cosTime),
    ok.


