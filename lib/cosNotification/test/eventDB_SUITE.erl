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
%%--------------------------------------------------------------------
%% File    : eventDB_SUITE.erl
%% Purpose : 
%%--------------------------------------------------------------------

-module(eventDB_SUITE).
%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% cosEvent files.
-include_lib("cosEvent/include/CosEventChannelAdmin.hrl").
%% cosTime files.
-include_lib("cosTime/include/TimeBase.hrl").
%% Application files
-include_lib("cosNotification/include/CosNotification.hrl").
-include_lib("cosNotification/include/CosNotifyChannelAdmin.hrl").
-include_lib("cosNotification/include/CosNotifyComm.hrl").
-include_lib("cosNotification/include/CosNotifyFilter.hrl").

-include_lib("cosNotification/src/CosNotification_Definitions.hrl").

-include("idl_output/notify_test.hrl").

-include_lib("common_test/include/ct.hrl").

%%--------------- DEFINES ------------------------------------
-define(default_timeout, test_server:minutes(20)).
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


-define(EVENT1, ?not_CreateSE("","event1","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 0)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=900000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=900000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				 value=any:create(orber_tc:unsigned_long_long(), 900000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT2, ?not_CreateSE("","event2","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 0)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=800000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=800000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 800000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT3, ?not_CreateSE("","event3","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 0)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=700000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=700000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 700000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT4, ?not_CreateSE("","event4","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 2)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=300000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=300000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 300000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT5, ?not_CreateSE("","event5","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 2)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=200000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=200000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 200000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT6, ?not_CreateSE("","event6","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 0)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=500000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=500000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 500000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT7, ?not_CreateSE("","event7","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), -1)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=400000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=400000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 400000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT8, ?not_CreateSE("","event8","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), -1)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=600000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=600000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				value=any:create(orber_tc:unsigned_long_long(), 600000000)}],
			      [], any:create(orber_tc:null(), null))).
-define(EVENT9, ?not_CreateSE("","event9","",
			      [#'CosNotification_Property'
			       {name="Priority", 
				value=any:create(orber_tc:short(), 0)},
			       #'CosNotification_Property'
			       {name="StartTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=100000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), 
						  #'TimeBase_UtcT'
						  {time=100000000, 
						   inacclo=0, inacchi=0, tdf=2})},
			       #'CosNotification_Property'
			       {name="Timeout", 
				 value=any:create(orber_tc:unsigned_long_long(), 100000000)}],
			      [], any:create(orber_tc:null(), null))).

-define(EVENTS, [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5, ?EVENT6, ?EVENT7, 
		 ?EVENT8, ?EVENT9]).


-define(PRIOORDER, [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3, ?EVENT6, ?EVENT9, 
		    ?EVENT7, ?EVENT8]).

-define(FIFOORDER, ?EVENTS).

-define(DEADLINEORDER, [?EVENT9, ?EVENT5, ?EVENT4, ?EVENT7, ?EVENT6, ?EVENT8, ?EVENT3,
		       ?EVENT2, ?EVENT1]).

-define(NO_OF_EVENTS, 9).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 cases/0, init_per_suite/1, end_per_suite/1, reorder_api/1, 
	 lookup_api/1,
	 discard_api/1, max_events_api/1, gc_api/1, auto_gc_api/1,
	 start_stop_time_api/1, mapping_filter_api/1, persisten_event_api/1,
	 init_per_testcase/2, end_per_testcase/2]).

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
    [persisten_event_api, start_stop_time_api,
     mapping_filter_api, max_events_api, discard_api,
     reorder_api, lookup_api, gc_api, auto_gc_api].


	
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

init_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    orber:jump_start(),
    cosTime:install_time(),
    cosTime:start(),
    if
        is_list(Config) ->
	    Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    cosTime:stop(),
    cosTime:uninstall_time(),
    orber:jump_stop(),
    Config.


%%-----------------------------------------------------------------
%%  cosNotification_eventDB lookup API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. This case is supposed to test
%% that the events are delivered in the correct order
%% if a MappingFilter have benn associated.
mapping_filter_api(_Config) ->
    InitQoS       = ?not_CreateInitQoS(),
    InitQoS2      = ?not_SetMaxEventsPerConsumer(InitQoS,100),
    InitQoS3      = ?not_SetStartTimeSupported(InitQoS2, false),
    InitQoS4      = ?not_SetStopTimeSupported(InitQoS3, true),
    QoS           = ?not_SetDiscardPolicy(InitQoS4, ?not_AnyOrder),

    PriorityQoS   = ?not_SetOrderPolicy(QoS, ?not_PriorityOrder),
    DeadlineQoS   = ?not_SetOrderPolicy(QoS, ?not_DeadlineOrder),

    %% "Calculate" data once:
    %% NOTE! Even though the an Event do not match any of the constarints the
    %% default value will be used. Hence, the events will not be stored in the
    %% way described in the definitions above. For example, when using deadline order
    %% all the events will be stored in FIFO order since the usag of a MappingFilter
    %% all evnts will have the same deadline (except event6).
    Events        = ?EVENTS,
    PrioOrder     = [?EVENT6, ?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5, ?EVENT7, 
		     ?EVENT8, ?EVENT9], 
    DeadlineOrder = [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5, ?EVENT7, ?EVENT8, 
		     ?EVENT9],


    FiFac = 'CosNotifyFilter_FilterFactory':oe_create(),
    ?match({_,key,_,_,_,_}, FiFac),
    
    PrioFilter = 'CosNotifyFilter_FilterFactory':
	create_mapping_filter(FiFac, "EXTENDED_TCL", any:create(orber_tc:short(), 0)),
    DLFilter = 'CosNotifyFilter_FilterFactory':
	create_mapping_filter(FiFac, "EXTENDED_TCL", any:create(orber_tc:unsigned_long_long(), 1000000000)),
    
    ?match([_],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(PrioFilter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "",
					   type_name = "event6"}],
			   constraint_expr = "2==2"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),
    ?match([_],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(DLFilter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "",
					   type_name = "event6"}],
			   constraint_expr = "2==2"},
			  result_to_set = any:create(orber_tc:unsigned_long_long(), 200000000)}])),


    do_lookup(PriorityQoS, Events, PrioOrder, "Priority Order", undefined, PrioFilter, 0),
    do_lookup(DeadlineQoS, Events, DeadlineOrder, "Deadline Order", DLFilter, undefined, 23000),
    ok.

do_lookup(QoS, Events, Return, Txt, DLFilter, PrioFilter, Timeout) ->
    io:format("#################### ~s ###################~n", [Txt]),
    Ref  = cosNotification_eventDB:create_db(QoS, 60, 50, undefined),
    create_loop(Events, Ref, DLFilter, PrioFilter),
    timer:sleep(Timeout),
    ?match({Return,_}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),
    cosNotification_eventDB:destroy_db(Ref).

%%-----------------------------------------------------------------
%%  cosNotification_eventDB discard API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If MaxEvents limit is reached there
%% different ways we can discard the. This case will test 
%% all permutations of order and discard policies.
discard_api(_Config) ->
    InitQoS1    = ?not_CreateInitQoS(),
    InitQoS2    = ?not_SetPriority(InitQoS1, 10),
    InitQoS3    = ?not_SetStartTimeSupported(InitQoS2, false),
    QoS         = ?not_SetMaxEventsPerConsumer(InitQoS3, 5),
    %% The different order policies. To each order we must apply every possible
    %% discard policy to each order policy setting. We also have to test and
    %% change the policies for each setting.
    AnyQoS      = ?not_SetOrderPolicy(QoS, ?not_AnyOrder),
    PriorityQoS = ?not_SetOrderPolicy(QoS, ?not_PriorityOrder),
    FifoQoS     = ?not_SetOrderPolicy(QoS, ?not_FifoOrder),
    DeadlineQoS = ?not_SetOrderPolicy(QoS, ?not_DeadlineOrder),

    Events        = ?EVENTS,

    %% Test using Any discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_AnyOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard and Order eq. Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_AnyOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard Any and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_AnyOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5],
	       "Discard Any and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_AnyOrder), 
	       [?EVENT5, ?EVENT4, ?EVENT3, ?EVENT2, ?EVENT1],
	       "Discard Any and Order Deadline"),
    
    %% Test using RejectNewEvents discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_RejectNewEvents), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard RejectNewEvents and Order Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_RejectNewEvents), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard RejectNewEvents and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_RejectNewEvents), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5],
	       "Discard RejectNewEvents and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_RejectNewEvents), 
	       [?EVENT5, ?EVENT4, ?EVENT3, ?EVENT2, ?EVENT1],
	       "Discard RejectNewEvents and Order Deadline"),
    
    %% Test using Lifo discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_LifoOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard Lifo and Order Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_LifoOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard Lifo and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_LifoOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5],
	       "Discard Lifo and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_LifoOrder), 
	       [?EVENT5, ?EVENT4, ?EVENT3, ?EVENT2, ?EVENT1],
	       "Discard Lifo and Order Deadline"),
    
    %% Test using Fifo discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_FifoOrder), 
	       [?EVENT5, ?EVENT6, ?EVENT9, ?EVENT7, ?EVENT8],
	       "Discard Fifo and Order Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_FifoOrder), 
	       [?EVENT5, ?EVENT6, ?EVENT9, ?EVENT7, ?EVENT8],
	       "Discard Fifo and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_FifoOrder), 
	       [?EVENT5, ?EVENT6, ?EVENT7, ?EVENT8, ?EVENT9],
	       "Discard Fifo and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_FifoOrder), 
	       [?EVENT9, ?EVENT5, ?EVENT7, ?EVENT6, ?EVENT8],
	       "Discard Fifo and Order Deadline"),
    
    %% Test using Priority discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_PriorityOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard Priority and Order Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_PriorityOrder), 
	       [?EVENT4, ?EVENT5, ?EVENT1, ?EVENT2, ?EVENT3],
	       "Discard Priority and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_PriorityOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5],
	       "Discard Priority and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_PriorityOrder), 
	       [?EVENT5, ?EVENT4, ?EVENT3, ?EVENT2, ?EVENT1],
	       "Discard Priority and Order Deadline"),
    
    %% Test using Deadline discard policy
    do_discard(Events, ?not_SetDiscardPolicy(AnyQoS, ?not_DeadlineOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT6, ?EVENT8],
	       "Discard Deadline and Order Any"),
    do_discard(Events, ?not_SetDiscardPolicy(PriorityQoS, ?not_DeadlineOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT6, ?EVENT8],
	       "Discard Deadline and Order Priority"),
    do_discard(Events, ?not_SetDiscardPolicy(FifoQoS, ?not_DeadlineOrder), 
	       [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT6, ?EVENT8],
	       "Discard Deadline and Order Fifo"),
    do_discard(Events, ?not_SetDiscardPolicy(DeadlineQoS, ?not_DeadlineOrder), 
	       [?EVENT6, ?EVENT8, ?EVENT3, ?EVENT2, ?EVENT1],
	       "Discard Deadline and Order Deadline"),
    
    ok.

do_discard(Events, QoS, Reply, Txt) ->
    io:format("################# ~s #################~n", [Txt]),
    Ref  = cosNotification_eventDB:create_db(QoS, 60, 50, undefined),
    create_loop(Events, Ref),
    ?match({Reply,_}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),
    cosNotification_eventDB:destroy_db(Ref).


%%-----------------------------------------------------------------
%%  cosNotification_eventDB lookup API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. This case is supposed to test
%% that the events are delivered in the correct order.
lookup_api(_Config) ->
    InitQoS       = ?not_CreateInitQoS(),
    InitQoS2      = ?not_SetMaxEventsPerConsumer(InitQoS,100),
    InitQoS3      = ?not_SetStartTimeSupported(InitQoS2, false),
    QoS           = ?not_SetDiscardPolicy(InitQoS3, ?not_AnyOrder),

    AnyQoS        = ?not_SetOrderPolicy(QoS, ?not_AnyOrder),
    PriorityQoS   = ?not_SetOrderPolicy(QoS, ?not_PriorityOrder),
    FifoQoS       = ?not_SetOrderPolicy(QoS, ?not_FifoOrder),
    DeadlineQoS   = ?not_SetOrderPolicy(QoS, ?not_DeadlineOrder),

    %% "Calculate" data once:
    Events        = ?EVENTS,
    PrioOrder     = ?PRIOORDER,
    FifoOrder     = ?FIFOORDER,
    DeadlineOrder = ?DEADLINEORDER,

    do_lookup(PriorityQoS, Events, PrioOrder, "Priority Order"),
    do_lookup(FifoQoS, Events, FifoOrder, "Fifo Order"),
    do_lookup(DeadlineQoS, Events, DeadlineOrder, "Deadline Order"),
    do_lookup(AnyQoS, Events, PrioOrder, "Any Order"),
    ok.

do_lookup(QoS, Events, Return, Txt) ->
    io:format("#################### ~s ###################~n", [Txt]),
    Ref  = cosNotification_eventDB:create_db(QoS, 60, 50, undefined),
    create_loop(Events, Ref),
    ?match({Return,_}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),
    cosNotification_eventDB:destroy_db(Ref).


%%-----------------------------------------------------------------
%%  cosNotification_eventDB max events API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If the MaxEvents QoS is updated we must be
%% able to reduce the amount of stored events.
max_events_api(_Config) ->

    QoS1             = ?not_CreateInitQoS(),
    QoS2             = ?not_SetOrderPolicy(QoS1, ?not_FifoOrder),
    QoS3             = ?not_SetDiscardPolicy(QoS2, ?not_RejectNewEvents),
    QoS4             = ?not_SetStartTimeSupported(QoS3, false),
    QoS_NO_OF_EVENTS = ?not_SetMaxEventsPerConsumer(QoS4, ?NO_OF_EVENTS),
    QoS_5_EVENTS     = ?not_SetMaxEventsPerConsumer(QoS4, 5),

    Events   = ?EVENTS,
    Events5 = [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT5],

    %% Initiate DB and 'NO_OF_EVENTS' events.
    Ref1  = cosNotification_eventDB:create_db(QoS_NO_OF_EVENTS, 60, 50, undefined),
    create_loop(Events, Ref1),

    %% Reduce the limit to 5 and extract all and see if it's ok.
    Ref2 = cosNotification_eventDB:update(Ref1, QoS_5_EVENTS),
    ?match({Events5, true}, cosNotification_eventDB:get_events(Ref2, ?NO_OF_EVENTS)),

    %% Add 'NO_OF_EVENTS' events. Since the only allow 5 events the DB will only 
    %% contain 5 events.
    create_loop(Events, Ref2),
    Ref3 = cosNotification_eventDB:update(Ref2, QoS_NO_OF_EVENTS),

    ?match({Events5, true}, cosNotification_eventDB:get_events(Ref3, ?NO_OF_EVENTS)),
    create_loop(Events, Ref3),
    ?match({Events, true}, cosNotification_eventDB:get_events(Ref3, ?NO_OF_EVENTS)),
    cosNotification_eventDB:destroy_db(Ref3),
    ok.


%%-----------------------------------------------------------------
%%  cosNotification_eventDB persisten events API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once.
persisten_event_api(_Config) ->

    QoS1             = ?not_CreateInitQoS(),
    QoS2             = ?not_SetOrderPolicy(QoS1, ?not_FifoOrder),
    QoS3             = ?not_SetDiscardPolicy(QoS2, ?not_RejectNewEvents),
    QoS4             = ?not_SetStartTimeSupported(QoS3, false),
    QoS              = ?not_SetMaxEventsPerConsumer(QoS4, ?NO_OF_EVENTS),

    Event1   = ?EVENT1,

    Ref  = cosNotification_eventDB:create_db(QoS, 60, 50, undefined),
    %% Clean DB, should be empty
    ?match(0, cosNotification_eventDB:status(Ref, eventCounter)),
    cosNotification_eventDB:add_event(Ref, Event1),
    ?match(1, cosNotification_eventDB:status(Ref, eventCounter)),
    %% Get event without removing it. Should still be one event stored
    ?match({[Event1], _, _}, cosNotification_eventDB:get_events(Ref, 2, false)),
    ?match(1, cosNotification_eventDB:status(Ref, eventCounter)),
    {_, _, Keys} = 
	?match({Event1, _, _}, cosNotification_eventDB:get_event(Ref, false)),
    ?match(1, cosNotification_eventDB:status(Ref, eventCounter)),
    %% Clear the events and check that the DB is empty.
    cosNotification_eventDB:delete_events(Keys),
    ?match(0, cosNotification_eventDB:status(Ref, eventCounter)),
    ?match({[], _, []}, cosNotification_eventDB:get_event(Ref, false)),
    ?match({[], _, []}, cosNotification_eventDB:get_events(Ref, 2, false)),
    
    cosNotification_eventDB:destroy_db(Ref),
    ok.

%%-----------------------------------------------------------------
%%  cosNotification_eventDB gc API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If Deadline defined the events that
%% are older must be discarded.
gc_api(_Config) ->

    QoS1             = ?not_CreateInitQoS(),
    QoS2             = ?not_SetOrderPolicy(QoS1, ?not_FifoOrder),
    QoS3             = ?not_SetDiscardPolicy(QoS2, ?not_RejectNewEvents),
    QoS4             = ?not_SetStartTimeSupported(QoS3, false),
    QoS_NO_OF_EVENTS = ?not_SetMaxEventsPerConsumer(QoS4, ?NO_OF_EVENTS),

    Events   = ?EVENTS,
    Events6  = [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT6, ?EVENT7, ?EVENT8],
    %% Initiate DB and 'NO_OF_EVENTS' events.
    Ref  = cosNotification_eventDB:create_db(QoS_NO_OF_EVENTS, 60, 50, undefined),
    create_loop(Events, Ref),

    %% Sleep so some events will get 'old'.
    timer:sleep(23000),

    %% Reduce the limit to 5 and extract all and see if it's ok.
    cosNotification_eventDB:gc_events(Ref, high),

    %% Since gc is done by another process we must wait so it will have a chance
    %% to complete the job.
    timer:sleep(2000),

    ?match({Events6, true}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    create_loop(Events, Ref),
    timer:sleep(23000),
    ?match({Events6, true}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),
    cosNotification_eventDB:destroy_db(Ref),
    ok.


%%-----------------------------------------------------------------
%%  cosNotification_eventDB gc API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If Deadline defined the events that
%% are older must be discarded.
auto_gc_api(_Config) ->

    QoS1             = ?not_CreateInitQoS(),
    QoS2             = ?not_SetOrderPolicy(QoS1, ?not_FifoOrder),
    QoS3             = ?not_SetDiscardPolicy(QoS2, ?not_RejectNewEvents),
    QoS4             = ?not_SetStopTimeSupported(QoS3, true),
    QoS5             = ?not_SetStartTimeSupported(QoS4, false),
    QoS_NO_OF_EVENTS = ?not_SetMaxEventsPerConsumer(QoS5, ?NO_OF_EVENTS),

    Events6  = [?EVENT1, ?EVENT2, ?EVENT3, ?EVENT7, ?EVENT8, ?EVENT9],
    %% Initiate DB
    Ref  = cosNotification_eventDB:create_db(QoS_NO_OF_EVENTS, 50, 50, undefined),
    create_loop([?EVENT1, ?EVENT2, ?EVENT3, ?EVENT4, ?EVENT6], Ref),

    %% Sleep so some events will get 'old'.
    timer:sleep(60000),
    create_loop([?EVENT7, ?EVENT8, ?EVENT9], Ref),

    %% Since gc is done by another process we must wait so it will have a chance
    %% to complete the job.
    timer:sleep(2000),

    ?match({Events6, true}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    cosNotification_eventDB:destroy_db(Ref),

    ok.


%%-----------------------------------------------------------------
%%  cosNotification_eventDB start- and stop-time API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If Deadline defined the events that
%% are older must be discarded.
start_stop_time_api(_Config) ->

    QoS1             = ?not_CreateInitQoS(),
    QoS2             = ?not_SetOrderPolicy(QoS1, ?not_FifoOrder),
    QoS3             = ?not_SetDiscardPolicy(QoS2, ?not_RejectNewEvents),
    QoS4             = ?not_SetStopTimeSupported(QoS3, true),
    QoS5             = ?not_SetStartTimeSupported(QoS4, true),
    QoS_NO_OF_EVENTS = ?not_SetMaxEventsPerConsumer(QoS5, ?NO_OF_EVENTS),

    %% Initiate DB
    TimeService = cosTime:start_time_service(2, 0),
    Ref  = cosNotification_eventDB:create_db(QoS_NO_OF_EVENTS, 50, 50, TimeService),

    T1 = 'CosTime_UTO':'_get_utc_time'('CosTime_UTO':
				       absolute_time('CosTime_TimeService':
						     new_universal_time(TimeService, 
									100000000, 0, 2))),
    T2 = 'CosTime_UTO':'_get_utc_time'('CosTime_UTO':
				       absolute_time('CosTime_TimeService':
						     new_universal_time(TimeService, 
									200000000, 0, 2))),
    T3 = 'CosTime_UTO':'_get_utc_time'('CosTime_UTO':
				       absolute_time('CosTime_TimeService':
						     new_universal_time(TimeService, 
									300000000, 0, 2))),
    T4 = 'CosTime_UTO':'_get_utc_time'('CosTime_UTO':
				       absolute_time('CosTime_TimeService':
						     new_universal_time(TimeService, 
									400000000, 0, 2))),
    %% Delivered after 10 seconds discarded after 20.
    EVENT1 =  ?not_CreateSE("","event1","",
			    [#'CosNotification_Property'
			     {name="Priority", 
			      value=any:create(orber_tc:short(), 1)},
			     #'CosNotification_Property'
			     {name="StartTime", 
			      value=any:create('TimeBase_UtcT':tc(), T1)},
			     #'CosNotification_Property'
			     {name="StopTime", 
			      value=any:create('TimeBase_UtcT':tc(), T2)}],
			    [], any:create(orber_tc:null(), null)),
    
    %% Delivered after 30 seconds discarded after 10, i.e., always discarded.
    EVENT2 =  ?not_CreateSE("","event2","",
			    [#'CosNotification_Property'
			     {name="Priority", 
			      value=any:create(orber_tc:short(), 3)},
			     #'CosNotification_Property'
			     {name="StartTime", 
				value=any:create('TimeBase_UtcT':tc(), T3)},
			       #'CosNotification_Property'
			       {name="StopTime", 
				 value=any:create('TimeBase_UtcT':tc(), T1)}],
			      [], any:create(orber_tc:null(), null)),
    
    %% Delivered after 20 seconds discarded after 40
    EVENT3 = ?not_CreateSE("","event3","",
			   [#'CosNotification_Property'
			    {name="Priority", 
			     value=any:create(orber_tc:short(), 2)},
			    #'CosNotification_Property'
			    {name="StartTime", 
			     value=any:create('TimeBase_UtcT':tc(), T2)},
			    #'CosNotification_Property'
			    {name="StopTime", 
			     value=any:create('TimeBase_UtcT':tc(), T4)}],
			   [], any:create(orber_tc:null(), null)),
    



    create_loop([EVENT1, EVENT2, EVENT3], Ref),

    ?match({[], false}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    %% Sleep so some events will get 'old'.
    timer:sleep(12000),

    ?match({[EVENT1], true}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    ?match({[], false}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    timer:sleep(10000),

    ?match({[EVENT3], true}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    timer:sleep(20000),

    %% See if EVENT2 really have been discarded.
    ?match({[], false}, cosNotification_eventDB:get_events(Ref, ?NO_OF_EVENTS)),

    cosNotification_eventDB:destroy_db(Ref),

    cosTime:stop_time_service(TimeService),

    ok.


%%-----------------------------------------------------------------
%%  cosNotification_eventDB order API tests 
%%-----------------------------------------------------------------
%% The event DB is used to store events which cannot be
%% delivered at once. If the QoS is updated we must be
%% able to change the ordering of events as the discard
%% and order policies tells us.
reorder_api(_Config) ->
    %% We need to test switching between:
    %% * Priority -> Fifo
    %% * Priority -> Deadline
    %% * Fifo -> Priority 
    %% * Fifo -> Deadline
    %% * Deadline -> Priority 
    %% * Deadline -> Fifo
    QoS  = ?not_CreateInitQoS(),
    QoS2 = ?not_SetMaxEventsPerConsumer(QoS,100),
    QoS3 = ?not_SetPriority(QoS2, 10),
    QoS4 = ?not_SetStartTimeSupported(QoS3, false),
    QoS5 = ?not_SetOrderPolicy(QoS4, ?not_AnyOrder),


    %% Test all order policies using Any order discard policy.
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_AnyOrder), "Discard Any"),
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_PriorityOrder), "Discard Priority"),
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_DeadlineOrder), "Discard Deadline"),
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_FifoOrder), "Discard Fifo"),
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_LifoOrder), "Discard Lifo"),
    reorder_helper(?not_SetDiscardPolicy(QoS5, ?not_RejectNewEvents), "Reject New Events"),

    ok.


reorder_helper(QoS, Txt) ->
    io:format("$$$$$$$$$$$$$$$$$$$$ ~s $$$$$$$$$$$$$$$$$$$~n", [Txt]),
    %% Create a DB with the above settings.
    Ref  = cosNotification_eventDB:create_db(QoS, 60, 50, undefined),

    Events        = ?EVENTS,
    PrioOrder     = ?PRIOORDER,
    FifoOrder     = ?FIFOORDER,
    DeadlineOrder = ?DEADLINEORDER,

    %% Test all order policies using Any order discard policy.
    Ref2 = do_reorder(Ref, Events, ?not_SetOrderPolicy(QoS, ?not_FifoOrder), 
		      FifoOrder, "Priority -> Fifo"),
    Ref3 = do_reorder(Ref2, Events, ?not_SetOrderPolicy(QoS, ?not_PriorityOrder), 
		      PrioOrder, "Fifo -> Priority"),
    Ref4 = do_reorder(Ref3, Events, ?not_SetOrderPolicy(QoS, ?not_DeadlineOrder), 
		      DeadlineOrder, "Priority -> Deadline"),

    Ref5 = do_reorder(Ref4, Events, ?not_SetOrderPolicy(QoS, ?not_PriorityOrder), 
		      PrioOrder, "Deadline -> Priority"),

    Ref6 = do_reorder(Ref5, Events, ?not_SetOrderPolicy(QoS, ?not_FifoOrder), 
		      FifoOrder, "Priority -> Fifo"),

    Ref7 = do_reorder(Ref6, Events, ?not_SetOrderPolicy(QoS, ?not_DeadlineOrder), 
		      DeadlineOrder, "Fifo -> Deadline"),

    Ref8 = do_reorder(Ref7, Events, ?not_SetOrderPolicy(QoS, ?not_FifoOrder), 
		      FifoOrder, "Deadline -> Fifo"),
    cosNotification_eventDB:destroy_db(Ref8),
    ok.

    

do_reorder(Ref, Events, QoS, Reply, Txt) ->
    create_loop(Events, Ref),
    io:format("################# ~s #################~n", [Txt]),
    NewRef = cosNotification_eventDB:update(Ref, QoS),
    ?match({Reply,_}, cosNotification_eventDB:get_events(NewRef, ?NO_OF_EVENTS)),
    NewRef.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
%% This functions takes as argument a list of structured events.
create_loop([], _Ref) ->
    ok;
create_loop([H|T], Ref) ->
    catch cosNotification_eventDB:add_event(Ref, H),
    create_loop(T, Ref).

create_loop([], _Ref, _Life, _Prio) ->
    ok;
create_loop([H|T], Ref, Life, Prio) ->
    catch cosNotification_eventDB:add_event(Ref, H, Life, Prio),
    create_loop(T, Ref, Life, Prio).

%%-------------------- End of Module ------------------------------
