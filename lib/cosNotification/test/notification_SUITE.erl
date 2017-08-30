%%--------------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1999-2016. All Rights Reserved.
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
%% File    : notification_SUITE.erl
%% Purpose : 
%%--------------------------------------------------------------------

-module(notification_SUITE).

%%--------------- INCLUDES -----------------------------------
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
%% cosEvent files.
-include_lib("cosEvent/include/CosEventChannelAdmin.hrl").
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
 
-define(defaultQoS,
	[#'CosNotification_Property'{name='CosNotification':'MaximumBatchSize'(), 
				     value=any:create(orber_tc:long(), 100)},
	 #'CosNotification_Property'{name='CosNotification':'PacingInterval'(), 
				     value=any:create(orber_tc:unsigned_long_long(), 
						      20000000)},
	 #'CosNotification_Property'{name='CosNotification':'OrderPolicy'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'AnyOrder'())},
	 #'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'BestEffort'())},
	 #'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'BestEffort'())},
	 #'CosNotification_Property'{name='CosNotification':'DiscardPolicy'(), 
				     value=any:create(orber_tc:short(), 
				      'CosNotification':'AnyOrder'())},
	 #'CosNotification_Property'{name='CosNotification':'StartTimeSupported'(), 
				     value=any:create(orber_tc:boolean(), false)},
	 #'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
				     value=any:create(orber_tc:boolean(), false)},
	 #'CosNotification_Property'{name='CosNotification':'Priority'(), 
				     value=any:create(orber_tc:short(), 
				      'CosNotification':'DefaultPriority'())}]).
-define(defaultQoS2,
	[#'CosNotification_Property'{name='CosNotification':'MaximumBatchSize'(), 
				     value=any:create(orber_tc:long(), 1)},
	 #'CosNotification_Property'{name='CosNotification':'PacingInterval'(), 
				     value=any:create(orber_tc:unsigned_long_long(), 
						      0)},
	 #'CosNotification_Property'{name='CosNotification':'OrderPolicy'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'AnyOrder'())},
	 #'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'BestEffort'())},
	 #'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'BestEffort'())},
	 #'CosNotification_Property'{name='CosNotification':'DiscardPolicy'(), 
				     value=any:create(orber_tc:short(), 
				      'CosNotification':'AnyOrder'())},
	 #'CosNotification_Property'{name='CosNotification':'StartTimeSupported'(), 
				     value=any:create(orber_tc:boolean(), false)},
	 #'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
				     value=any:create(orber_tc:boolean(), false)},
	 #'CosNotification_Property'{name='CosNotification':'Priority'(), 
				     value=any:create(orber_tc:short(), 
				      'CosNotification':'DefaultPriority'())}]).
-define(defaultAdm,
	[#'CosNotification_Property'{name='CosNotification':'MaxQueueLength'(), 
				     value=any:create(orber_tc:long(), 100)},
	 #'CosNotification_Property'{name='CosNotification':'MaxConsumers'(), 
				     value=any:create(orber_tc:long(), 100)},
	 #'CosNotification_Property'{name='CosNotification':'MaxSuppliers'(), 
				     value=any:create(orber_tc:long(), 100)}]).

-define(FAC_OPT, []).


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0, 
	 init_per_suite/1, end_per_suite/1, qos_api/1, adm_api/1,
	 cosevent_api/1, filter_adm_api/1, events_api/1, events2_api/1,
	 event_qos_api/1, filter_api/1, mapping_filter_api/1, subscription_api/1, 
	 init_per_testcase/2, end_per_testcase/2, persistent_max_events_api/1,
	 persistent_timeout_events_api/1, persistent_recover_events_api/1,
	 app_test/1]).

-export([terminated/1]).

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
    [persistent_max_events_api,
     persistent_timeout_events_api,
     persistent_recover_events_api, mapping_filter_api,
     filter_api, filter_adm_api, event_qos_api, qos_api,
     adm_api, cosevent_api, subscription_api, events_api,
     events2_api, app_test].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

end_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    ok = corba:orb_init([{flags, 16#02}, {orber_debug_level, 10}]),
    orber:jump_start(),
    cosNotificationApp:install_event(),
    cosNotificationApp:install(),
    'oe_notify_test_server':'oe_register'(),
    cosNotificationApp:start(),
    if
        is_list(Config) ->
	    Config;
        true ->
            exit("Config not a list")
    end.
 
end_per_suite(Config) ->
    cosNotificationApp:stop(),
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    'oe_notify_test_server':'oe_unregister'(),
    cosNotificationApp:uninstall(),
    cosNotificationApp:uninstall_event(),
    orber:jump_stop(),
    Config.


%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(_Config) ->
    ok=test_server:app_test(cosNotification),
    ok.


%%-----------------------------------------------------------------
%%  Persistent events max limit
%%-----------------------------------------------------------------
persistent_max_events_api(_Config) ->
    QoSPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    QoSEventPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    application:set_env(cosNotification, notify, ?MODULE),
    application:set_env(cosNotification, max_events, 2),
    application:set_env(cosNotification, timeout_events, 300000),
    application:set_env(cosNotification, interval_events, 10000),
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS2, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    
    %% Create the Admin objects
    {AdminSupplier, _ASID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'AND_OP')),
    {AdminConsumer, _ACID}=?match({{_,key,_,_,_,_},_}, 
	'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'AND_OP')),

    %% Create Proxies and clients
    {SequenceProxyPushSupplier,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'SEQUENCE_EVENT')),
    PushSeqC=?match({_,key,_,_,_,_}, 'notify_test_SeqPushC':oe_create(['PUSH_SEQUENCE',SequenceProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':connect_sequence_push_consumer(SequenceProxyPushSupplier, PushSeqC)),

    {SequenceProxyPushConsumer,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'SEQUENCE_EVENT')),
    PushSeqS=?match({_,key,_,_,_,_}, 'notify_test_SeqPushS':oe_create(['PUSH_SEQUENCE',SequenceProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':connect_sequence_push_supplier(SequenceProxyPushConsumer, PushSeqS)),
    
    %% Create a couple of Events to test with.
    Event = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    
    ?match(ok, 'notify_test_SeqPushC':doAction(PushSeqC, {action, action})),
    
    %% Push and check the state.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    ?match(false, corba_object:non_existent(SequenceProxyPushSupplier)),

    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    ?match(false, corba_object:non_existent(SequenceProxyPushSupplier)),
    %% Now we've reached the limit. This call will terminate the proxy.
    %% We cannot check for data at this point since the broken connection
    %% will result in that the client terminates.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    timer:sleep(5000),
    ?match(true, corba_object:non_existent(SequenceProxyPushSupplier)),
    ?match(true, corba_object:non_existent(PushSeqC)),


    catch corba:dispose(SequenceProxyPushConsumer),
    catch corba:dispose(SequenceProxyPushSupplier),
    catch corba:dispose(AdminConsumer),
    catch corba:dispose(AdminSupplier),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    catch corba:dispose(PushSeqS),
    catch corba:dispose(PushSeqC),
    application:set_env(cosNotification, notify, undefined),
    application:set_env(cosNotification, max_events, undefined),
    application:set_env(cosNotification, timeout_events, undefined),
    application:set_env(cosNotification, interval_events, undefined),
    ok.

terminated(Items) ->
    io:format("Proxy terminated due to: ~p~n", [Items]).

%%-----------------------------------------------------------------
%%  Persistent events timeout
%%-----------------------------------------------------------------
persistent_timeout_events_api(_Config) ->
    QoSPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    QoSEventPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    application:set_env(cosNotification, notify, ?MODULE),
    application:set_env(cosNotification, max_events, 1000),
    application:set_env(cosNotification, timeout_events, 4000),
    application:set_env(cosNotification, interval_events, 1000),
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS2, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    
    %% Create the Admin objects
    {AdminSupplier, _ASID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'AND_OP')),
    {AdminConsumer, _ACID}=?match({{_,key,_,_,_,_},_}, 
	'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'AND_OP')),

    %% Create Proxies and clients
    {SequenceProxyPushSupplier,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'SEQUENCE_EVENT')),
    PushSeqC=?match({_,key,_,_,_,_}, 'notify_test_SeqPushC':oe_create(['PUSH_SEQUENCE',SequenceProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':connect_sequence_push_consumer(SequenceProxyPushSupplier, PushSeqC)),

    {SequenceProxyPushConsumer,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'SEQUENCE_EVENT')),
    PushSeqS=?match({_,key,_,_,_,_}, 'notify_test_SeqPushS':oe_create(['PUSH_SEQUENCE',SequenceProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':connect_sequence_push_supplier(SequenceProxyPushConsumer, PushSeqS)),
    
    %% Create a couple of Events to test with.
    Event = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    
    ?match(ok, 'notify_test_SeqPushC':doAction(PushSeqC, {action, action})),
    
    %% Push and check the state.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    ?match(false, corba_object:non_existent(SequenceProxyPushSupplier)),

    %% Now we've reached the limit. This call will terminate the proxy.
    %% We cannot check for data at this point since the broken connection
    %% will result in that the client terminates.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    timer:sleep(10000),
    ?match(true, corba_object:non_existent(SequenceProxyPushSupplier)),
    ?match(true, corba_object:non_existent(PushSeqC)),


    catch corba:dispose(SequenceProxyPushConsumer),
    catch corba:dispose(SequenceProxyPushSupplier),
    catch corba:dispose(AdminConsumer),
    catch corba:dispose(AdminSupplier),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    catch corba:dispose(PushSeqS),
    catch corba:dispose(PushSeqC),
    application:set_env(cosNotification, notify, undefined),
    application:set_env(cosNotification, max_events, undefined),
    application:set_env(cosNotification, timeout_events, undefined),
    application:set_env(cosNotification, interval_events, undefined),
    ok.

%%-----------------------------------------------------------------
%%  Persistent events max limit
%%-----------------------------------------------------------------
persistent_recover_events_api(_Config) ->
    QoSPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    QoSEventPersistent = 
	[#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
				     value=any:create(orber_tc:short(), 
						      'CosNotification':'Persistent'())}],
    application:set_env(cosNotification, notify, ?MODULE),
    application:set_env(cosNotification, max_events, 1000),
    application:set_env(cosNotification, timeout_events, 100000),
    application:set_env(cosNotification, interval_events, 1000),
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS2, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    
    %% Create the Admin objects
    {AdminSupplier, _ASID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'AND_OP')),
    {AdminConsumer, _ACID}=?match({{_,key,_,_,_,_},_}, 
	'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'AND_OP')),

    %% Create Proxies and clients
    {SequenceProxyPushSupplier,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'SEQUENCE_EVENT')),
    PushSeqC=?match({_,key,_,_,_,_}, 'notify_test_SeqPushC':oe_create(['PUSH_SEQUENCE',SequenceProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':connect_sequence_push_consumer(SequenceProxyPushSupplier, PushSeqC)),

    {SequenceProxyPushConsumer,_ID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'SEQUENCE_EVENT')),
    PushSeqS=?match({_,key,_,_,_,_}, 'notify_test_SeqPushS':oe_create(['PUSH_SEQUENCE',SequenceProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':connect_sequence_push_supplier(SequenceProxyPushConsumer, PushSeqS)),
    
    %% Create a couple of Events to test with.
    Event = ?not_CreateSE("DomainName","CommunicationsAlarm",
			   "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),
    
    ?match(ok, 'notify_test_SeqPushC':doAction(PushSeqC, {action, action})),
    
    %% Push and check the state.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    ?match(false, corba_object:non_existent(SequenceProxyPushSupplier)),
    %% Allow the proxy to try a few times and then change the client behavior
    timer:sleep(4000),
    ?match(ok, 'notify_test_SeqPushC':doAction(PushSeqC, {action, undefined})),
    %% Wait some time so that the proxy timeout has kicked in.
    timer:sleep(4000),

    %% Now the communication should work again.
    ?match([Event], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    ?match(false, corba_object:non_existent(SequenceProxyPushSupplier)),

    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, [Event])),
    timer:sleep(4000),
    ?match([Event], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    catch corba:dispose(SequenceProxyPushConsumer),
    catch corba:dispose(SequenceProxyPushSupplier),
    catch corba:dispose(AdminConsumer),
    catch corba:dispose(AdminSupplier),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    catch corba:dispose(PushSeqS),
    catch corba:dispose(PushSeqC),
    application:set_env(cosNotification, notify, undefined),
    application:set_env(cosNotification, max_events, undefined),
    application:set_env(cosNotification, timeout_events, undefined),
    application:set_env(cosNotification, interval_events, undefined),
    ok.


%%-----------------------------------------------------------------
%%  CosNotifyFilter::Filter API tests 
%%-----------------------------------------------------------------
mapping_filter_api(_Config) ->
    FiFac = 'CosNotifyFilter_FilterFactory':oe_create(),
    ?match({_,key,_,_,_,_}, FiFac),
    
    Filter = 'CosNotifyFilter_FilterFactory':create_mapping_filter(FiFac,
									 "EXTENDED_TCL",
									 any:create(orber_tc:short(), 10)),
    ?match({_,key,_,_,_,_}, Filter),

    ?match("EXTENDED_TCL", 'CosNotifyFilter_MappingFilter':'_get_constraint_grammar'(Filter)),

    %% Test before we add any constarints.
    ?match([], 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),
    ?match({'EXCEPTION', {'CosNotifyFilter_ConstraintNotFound', _, 1}},
		 'CosNotifyFilter_MappingFilter':get_mapping_constraints(Filter, [1])),
    ?match(ok, 'CosNotifyFilter_MappingFilter':remove_all_mapping_constraints(Filter)),

    %% Try adding an incorrect constraint_expr
    ?match({'EXCEPTION',{'CosNotifyFilter_InvalidConstraint',_,_}},
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name",
					   type_name = "type"}],
			   constraint_expr = "2==2 and 3<"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),
    %% Try adding two correct constraint_expr
    [{_,_,CID1,_},{_,_,CID2,_}]= 
	?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}, {'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name",
					   type_name = "type"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)},
			#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name2",
					   type_name = "type2"}],
			   constraint_expr = "$.test._length == 3 and ($.test[0].score + $.test[1].score + $.test[2].score)/3 >=80"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,CID2,_}, {'CosNotifyFilter_MappingConstraintInfo',_,CID1,_}],
		 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),
    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,CID1,_}], 
		 'CosNotifyFilter_MappingFilter':get_mapping_constraints(Filter, [CID1])),
    ?match(ok, 'CosNotifyFilter_MappingFilter':remove_all_mapping_constraints(Filter)),
    ?match([], 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),

    %% Try adding a constraint_expr with using invalid value, i.e., not short.
    ?match({'EXCEPTION',{'CosNotifyFilter_InvalidValue',_,_,_}},
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name",
					   type_name = "type"}],
			   constraint_expr = "2==2 and 3<8"},
			  result_to_set = any:create(orber_tc:long(), 10)}])),

    %% Try adding one correct and one incorrect constraint_expr
    ?match({'EXCEPTION',{'CosNotifyFilter_InvalidConstraint',_,_}},
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name",
					   type_name = "type"}],
			   constraint_expr = "2==2 and 3<"},
			  result_to_set = any:create(orber_tc:short(), 10)},
			#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "name2",
					   type_name = "type2"}],
			   constraint_expr = "$.test._length == 3 and ($.test[0].score + $.test[1].score + $.test[2].score)/3 >=80"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    %% Following testcases test different domain_name and type_name, e.g., 
    %% wildcards etc.
    [{_,ConInfoData,CID3,_}] = 
	?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "domain",
					   type_name = ""},
					  #'CosNotification_EventType'
					  {domain_name = "*",
					   type_name = "type"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    %% Try removing a constraint
    ?match(ok, 'CosNotifyFilter_MappingFilter':modify_mapping_constraints(Filter,[CID3],[])),
    ?match([], 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),

    %% Add e new constraint
    [{_,_,CID4,_}] = 
	?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
		 'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "domain1",
					   type_name = ""},
					  #'CosNotification_EventType'
					  {domain_name = "domain2",
					   type_name = "*"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    %% Try to update the constraint associated with CID4 to equal CID3.
    ?match(ok, 'CosNotifyFilter_MappingFilter':modify_mapping_constraints(Filter,[],
             		 [#'CosNotifyFilter_MappingConstraintInfo'
			  {constraint_expression=
                           #'CosNotifyFilter_ConstraintExp'
			   {event_types =[#'CosNotification_EventType'
					  {domain_name = "domain",
					   type_name = ""},
					  #'CosNotification_EventType'
					  {domain_name = "*",
					   type_name = "type"}],
			    constraint_expr = "2==2 and 3<4"},
			   constraint_id=CID4,
			   value = any:create(orber_tc:short(), 10)}])),

    ?match([{_,ConInfoData,CID4,_}], 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),

    ?match({'EXCEPTION', {'CosNotifyFilter_ConstraintNotFound', _, CID3}},
		 'CosNotifyFilter_MappingFilter':get_mapping_constraints(Filter, [CID3])),
    ?match(ok, 'CosNotifyFilter_MappingFilter':remove_all_mapping_constraints(Filter)),
    ?match([], 'CosNotifyFilter_MappingFilter':get_all_mapping_constraints(Filter)),

    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "",
					   type_name = "type1"},
					  #'CosNotification_EventType'
					  {domain_name = "*",
					   type_name = "type2"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "domain1",
					   type_name = "type1"},
					  #'CosNotification_EventType'
					  {domain_name = "domain2",
					   type_name = "type2"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "dom*",
					   type_name = "type1"},
					  #'CosNotification_EventType'
					  {domain_name = "domain2",
					   type_name = "typ*"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    ?match([{'CosNotifyFilter_MappingConstraintInfo',_,_,_}],
	   'CosNotifyFilter_MappingFilter':add_mapping_constraints(Filter, 
			[#'CosNotifyFilter_MappingConstraintPair'
			 {constraint_expression = #'CosNotifyFilter_ConstraintExp'
			  {event_types = [#'CosNotification_EventType'
					  {domain_name = "dom*1",
					   type_name = "type1"},
					 #'CosNotification_EventType'
					  {domain_name = "domain2",
					   type_name = "typ*2"}],
			   constraint_expr = "2==2 and 3<4"},
			  result_to_set = any:create(orber_tc:short(), 10)}])),

    catch corba:dispose(FiFac),
    catch corba:dispose(Filter),
    ok.


%%-----------------------------------------------------------------
%%  CosNotifyFilter::Filter API tests 
%%-----------------------------------------------------------------
filter_api(_Config) ->
    Fac = cosNotificationApp:start_global_factory(?FAC_OPT),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm),
    AC= 'CosNotifyChannelAdmin_EventChannel':for_consumers(Ch),

    FiFac = 'CosNotifyFilter_FilterFactory':oe_create(),
    ?match({_,key,_,_,_,_}, FiFac),
    
    Filter = 'CosNotifyFilter_FilterFactory':create_filter(FiFac,"EXTENDED_TCL"),
    ?match({_,key,_,_,_,_}, Filter),

    ?match("EXTENDED_TCL", 'CosNotifyFilter_Filter':'_get_constraint_grammar'(Filter)),

    %% Test Callback management.
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}}, 
		 'CosNotifyFilter_Filter':attach_callback(Filter, Ch)),
    ?match([], 'CosNotifyFilter_Filter':get_callbacks(Filter)),
    ?match({'EXCEPTION',{'CosNotifyFilter_CallbackNotFound',_}},
		 'CosNotifyFilter_Filter':detach_callback(Filter, 0)),
    ID='CosNotifyFilter_Filter':attach_callback(Filter, AC),
    ?match([ID], 'CosNotifyFilter_Filter':get_callbacks(Filter)),
    ?match(ok, 'CosNotifyFilter_Filter':detach_callback(Filter, ID)),
    ?match([], 'CosNotifyFilter_Filter':get_callbacks(Filter)),

    %% This callback is just attached so we can test that we can call notify_subscribe.
    _ID2='CosNotifyFilter_Filter':attach_callback(Filter, AC),

    %% Test before we add any constarints.
    ?match([], 'CosNotifyFilter_Filter':get_all_constraints(Filter)),
    ?match({'EXCEPTION', {'CosNotifyFilter_ConstraintNotFound', _, 1}},
		 'CosNotifyFilter_Filter':get_constraints(Filter, [1])),
    ?match(ok, 'CosNotifyFilter_Filter':remove_all_constraints(Filter)),

    %% Try adding an incorrect constraint_expr
    ?match({'EXCEPTION',{'CosNotifyFilter_InvalidConstraint',_,_}},
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                             domain_name = "name",
                                                             type_name = "type"}],
			                                  constraint_expr = "2==2 and 3<"}])),
    %% Try adding two correct constraint_expr
    [{_,_,CID1},{_,_,CID2}]= 
	?match([{'CosNotifyFilter_ConstraintInfo',_,_}, {'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                             domain_name = "name",
                                                             type_name = "type"}],
			                                  constraint_expr = "2==2 and 3<4"},
			#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                              domain_name = "name2",
                                                             type_name = "type2"}],
			                                  constraint_expr = "$.test._length == 3 and ($.test[0].score + $.test[1].score + $.test[2].score)/3 >=80"}])),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,CID2}, {'CosNotifyFilter_ConstraintInfo',_,CID1}],
		 'CosNotifyFilter_Filter':get_all_constraints(Filter)),
    ?match([{'CosNotifyFilter_ConstraintInfo',_,CID1}], 
		 'CosNotifyFilter_Filter':get_constraints(Filter, [CID1])),
    ?match(ok, 'CosNotifyFilter_Filter':remove_all_constraints(Filter)),
    ?match([], 'CosNotifyFilter_Filter':get_all_constraints(Filter)),

    %% Try adding one correct and one incorrect constraint_expr
    ?match({'EXCEPTION',{'CosNotifyFilter_InvalidConstraint',_,_}},
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                             domain_name = "name",
                                                             type_name = "type"}],
    			                                  constraint_expr = "2==2 and 3<"},
			#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                             domain_name = "name2",
                                                             type_name = "type2"}],
			                                  constraint_expr = "$.test._length == 3 and ($.test[0].score + $.test[1].score + $.test[2].score)/3 >=80"}])),

    %% Following testcases test different domain_name and type_name, e.g., 
    %% wildcards etc.
    [{_,ConInfoData,CID3}] = 
	?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "domain",
                                                      type_name = ""},
					 #'CosNotification_EventType'{
                                                      domain_name = "*",
                                                      type_name = "type"}],
			            constraint_expr = "2==2 and 3<4"}])),

    %% Try removing a constraint
    ?match(ok, 'CosNotifyFilter_Filter':modify_constraints(Filter,[CID3],[])),
    ?match([], 'CosNotifyFilter_Filter':get_all_constraints(Filter)),

    %% Add e new constraint
    [{_,_,CID4}] = 
	?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
		 'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "domain1",
                                                      type_name = ""},
					 #'CosNotification_EventType'{
                                                      domain_name = "domain2",
                                                      type_name = "*"}],
			            constraint_expr = "2==2 and 3<4"}])),

    %% Try to update the constraint associated with CID4 to equal CID3.
    ?match(ok, 'CosNotifyFilter_Filter':modify_constraints(Filter,[],
             		 [#'CosNotifyFilter_ConstraintInfo'{constraint_expression=
                           #'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "domain",
                                                      type_name = ""},
					 #'CosNotification_EventType'{
                                                      domain_name = "*",
                                                      type_name = "type"}],
			            constraint_expr = "2==2 and 3<4"},
			  constraint_id=CID4}])),

    ?match([{_,ConInfoData,CID4}], 'CosNotifyFilter_Filter':get_all_constraints(Filter)),

    ?match({'EXCEPTION', {'CosNotifyFilter_ConstraintNotFound', _, CID3}},
		 'CosNotifyFilter_Filter':get_constraints(Filter, [CID3])),
    ?match(ok, 'CosNotifyFilter_Filter':remove_all_constraints(Filter)),
    ?match([], 'CosNotifyFilter_Filter':get_all_constraints(Filter)),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "",
                                                      type_name = "type1"},
					 #'CosNotification_EventType'{
                                                      domain_name = "*",
                                                      type_name = "type2"}],
			            constraint_expr = "2==2 and 3<4"}])),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "domain1",
                                                      type_name = "type1"},
					 #'CosNotification_EventType'{
                                                      domain_name = "domain2",
                                                      type_name = "type2"}],
			            constraint_expr = "2==2 and 3<4"}])),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "dom*",
                                                      type_name = "type1"},
					 #'CosNotification_EventType'{
                                                      domain_name = "domain2",
                                                      type_name = "typ*"}],
			            constraint_expr = "2==2 and 3<4"}])),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,_}],
	   'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
					[#'CosNotification_EventType'{
                                                      domain_name = "dom*1",
                                                      type_name = "type1"},
					 #'CosNotification_EventType'{
                                                      domain_name = "domain2",
                                                      type_name = "typ*2"}],
			            constraint_expr = "2==2 and 3<4"}])),

    catch corba:dispose(FiFac),
    catch corba:dispose(Filter),
    catch corba:dispose(AC),
    catch corba:dispose(Ch),
    catch corba:dispose(Fac),
    ok.

%%-----------------------------------------------------------------
%%  Subscription handling API tests 
%%-----------------------------------------------------------------
subscription_api(_Config) ->
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    %% Create the Admin objects
    {AdminSupplier, _ASID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'OR_OP')),
    {AdminConsumer, _ACID}=?match({{_,key,_,_,_,_},_}, 
	'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'OR_OP')),
    
    %% Create Suppliers Proxies
    {StructuredProxyPullSupplier,_}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(AdminConsumer, 'STRUCTURED_EVENT')),
    {StructuredProxyPushSupplier,_}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'STRUCTURED_EVENT')),

    %% Now we must create a Client for each proxy and connect them.
    PushStrC=?match({_,key,_,_,_,_}, 'notify_test_StrPushC':oe_create(['PUSH_STRUCTURED',StructuredProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':connect_structured_push_consumer(StructuredProxyPushSupplier, PushStrC)),
    PullStrC=?match({_,key,_,_,_,_}, 'notify_test_StrPullC':oe_create(['PULL_STRUCTURED',StructuredProxyPullSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':connect_structured_pull_consumer(StructuredProxyPullSupplier, PullStrC)),

    %% Create Consumers Proxies
    {StructuredProxyPullConsumer,_}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_pull_consumer(AdminSupplier, 'STRUCTURED_EVENT')),
    {StructuredProxyPushConsumer,_}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'STRUCTURED_EVENT')),

    %% Now we must create a Client for each proxy and connect them.
    PushStrS=?match({_,key,_,_,_,_}, 'notify_test_StrPushS':oe_create(['PUSH_STRUCTURED',StructuredProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':connect_structured_push_supplier(StructuredProxyPushConsumer, PushStrS)),

    PullStrS=?match({_,key,_,_,_,_}, 'notify_test_StrPullS':oe_create(['PULL_STRUCTURED',StructuredProxyPullConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':connect_structured_pull_supplier(StructuredProxyPullConsumer, PullStrS)),

    ES1=[#'CosNotification_EventType'{domain_name = "name1", type_name = "type1"},
	 #'CosNotification_EventType'{domain_name = "name2", type_name = "type2"}],
    ES2=[#'CosNotification_EventType'{domain_name = "name3", type_name = "type3"},
	 #'CosNotification_EventType'{domain_name = "name4", type_name = "type4"}],

    %% Initially it should have no associated types. Test that and set that
    %% all updates should be forwarded to client.
    ?match([], 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':
		 obtain_subscription_types(StructuredProxyPushConsumer, 
					   'ALL_NOW_UPDATES_ON')),
    ?match([], 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':
		 obtain_subscription_types(StructuredProxyPullConsumer, 
					   'ALL_NOW_UPDATES_ON')),
    
    %% Update the offered types.
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':
		 offer_change(StructuredProxyPushConsumer, ES1, [])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':
		 offer_change(StructuredProxyPullConsumer, ES1, [])),

    %% To be sure, wait a couple of seconds.
    timer:sleep(5000),
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'notify_test_StrPushC':doAction(PushStrS, return_data)),
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'notify_test_StrPullC':doAction(PullStrS, return_data)),

    %% Update the offered types. Remove ES1 and add ES2.
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':
		 offer_change(StructuredProxyPushConsumer, ES2, ES1)),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':
		 offer_change(StructuredProxyPullConsumer, ES2, ES1)),

    %% To be sure, wait a couple of seconds.
    timer:sleep(5000),
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'notify_test_StrPushC':doAction(PushStrS, return_data)),
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'notify_test_StrPullC':doAction(PullStrS, return_data)),

    %% Now, the objects should only contain 'ES2'. Test it.
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'CosNotifyChannelAdmin_StructuredProxyPushConsumer':
	   obtain_subscription_types(StructuredProxyPushConsumer, 
				     'ALL_NOW_UPDATES_ON')),
    ?match([{'CosNotification_EventType',_,_},
	    {'CosNotification_EventType',_,_}],
	   'CosNotifyChannelAdmin_StructuredProxyPullConsumer':
	   obtain_subscription_types(StructuredProxyPullConsumer, 
				     'ALL_NOW_UPDATES_ON')),

    %% Now we will use wildcards, empty strings and test if they really
    %% are ignored if so requested.
    ES3=[#'CosNotification_EventType'{domain_name = "name1", type_name = "*"},
	#'CosNotification_EventType'{domain_name = "*", type_name = "type2"}],
    ES4=[#'CosNotification_EventType'{domain_name = "name1", type_name = "*"},
	#'CosNotification_EventType'{domain_name = "name2", type_name = ""}],
    ES5=[#'CosNotification_EventType'{domain_name = "na*", type_name = "type1"}],
    ES6=[#'CosNotification_EventType'{domain_name = "n*1", type_name = "type1"}],
    ES7=[#'CosNotification_EventType'{domain_name = "*1", type_name = "type1"}],
    ES8=[#'CosNotification_EventType'{domain_name = "n*m*1", type_name = "type1"}],
    ES9=[#'CosNotification_EventType'{domain_name = "n**1", type_name = "type1"}],
    ES10=[#'CosNotification_EventType'{domain_name = "nam*1", type_name = "type1"}],

    Event1 = ?not_CreateSE("name1","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    Event2 = ?not_CreateSE("name2","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    Event3 = ?not_CreateSE("mame1","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    Event4 = ?not_CreateSE("naame1","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    Event5 = ?not_CreateSE("nname1","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),
    Event6 = ?not_CreateSE("name12","type1",
			   "event_name",
			   [#'CosNotification_Property'{name="property_name", 
							value=any:create(orber_tc:short(), 1)}],
			   [], any:create(orber_tc:null(), null)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES3, [])),

    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),

    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES4, ES3)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES5, ES4)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES6, ES5)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES7, ES6)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES8, ES7)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES9, ES8)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event2)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event3)),

    timer:sleep(5000),
    ?match({_NilStrEvent,false}, 'notify_test_StrPullC':doAction(PullStrC, try_pull_str)),

    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':
		 subscription_change(StructuredProxyPullSupplier, ES10, ES9)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event1)),
    ?match(Event1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event4)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event5)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event6)),

    timer:sleep(5000),
    ?match({_NilStrEvent,false}, 'notify_test_StrPullC':doAction(PullStrC, try_pull_str)),


    catch corba:dispose(StructuredProxyPushConsumer),
    catch corba:dispose(StructuredProxyPullConsumer),
    catch corba:dispose(StructuredProxyPushSupplier),
    catch corba:dispose(StructuredProxyPullSupplier),
    catch corba:dispose(AdminConsumer),
    catch corba:dispose(AdminSupplier),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),

    timer:sleep(5000),
    ?match(true, corba_object:non_existent(PullStrS)),
    ?match(true, corba_object:non_existent(PushStrS)),
    ?match(true, corba_object:non_existent(PullStrC)),
    ?match(true, corba_object:non_existent(PushStrC)),

    ok.

%%-----------------------------------------------------------------
%%  Filter admin API tests 
%%-----------------------------------------------------------------
filter_adm_api(_Config) ->
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    FiFac = 'CosNotifyFilter_FilterFactory':oe_create(),
    ?match({_,key,_,_,_,_}, FiFac),
    
    Filter = 'CosNotifyFilter_FilterFactory':create_filter(FiFac,"EXTENDED_TCL"),
    ?match({_,key,_,_,_,_}, Filter),

    AC=?match({_,key,_,_,_,_},
		    'CosNotifyChannelAdmin_EventChannel':for_consumers(Ch)),
    filter_tests('CosNotifyChannelAdmin_ConsumerAdmin', AC, Filter, Ch),

    AS=?match({_,key,_,_,_,_},
		    'CosNotifyChannelAdmin_EventChannel':for_suppliers(Ch)),
    filter_tests('CosNotifyChannelAdmin_SupplierAdmin', AS, Filter, Ch),

    PushS=?match({_,key,_,_,_,_},
		       'CosNotifyChannelAdmin_ConsumerAdmin':obtain_push_supplier(AC)),
    filter_tests('CosNotifyChannelAdmin_ProxyPushSupplier', PushS, Filter, Ch),

    PullS=?match({_,key,_,_,_,_},
		       'CosNotifyChannelAdmin_ConsumerAdmin':obtain_pull_supplier(AC)),
    filter_tests('CosNotifyChannelAdmin_ProxyPullSupplier', PullS, Filter, Ch),

    PushC=?match({_,key,_,_,_,_},
		       'CosNotifyChannelAdmin_SupplierAdmin':obtain_push_consumer(AS)),
    filter_tests('CosNotifyChannelAdmin_ProxyPushConsumer', PushC, Filter, Ch),

    PullC=?match({_,key,_,_,_,_},
		       'CosNotifyChannelAdmin_SupplierAdmin':obtain_pull_consumer(AS)),
    filter_tests('CosNotifyChannelAdmin_ProxyPullConsumer', PullC, Filter, Ch),

    catch corba:dispose(FiFac),
    catch corba:dispose(Filter),
    catch corba:dispose(PushS),
    catch corba:dispose(PullS),
    catch corba:dispose(PushC),
    catch corba:dispose(PullC),
    catch corba:dispose(AC),
    catch corba:dispose(AS),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    ok.

filter_tests(Mod, Obj, Filter, Ch) ->
    io:format("############ TESTING MODULE ~p FILTER ############~n", [Mod]),
    %% No filter added.
    ?match([], Mod:get_all_filters(Obj)),
    ?match(ok, Mod:remove_all_filters(Obj)),
    ?match({'EXCEPTION',{'CosNotifyFilter_FilterNotFound',_}},
		 Mod:get_filter(Obj, 0)),
    %% Try add a Filter which is not a filter.
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}}, Mod:add_filter(Obj, Ch)),
    %% Try to remove a single filter.
    ?match({'EXCEPTION',{'CosNotifyFilter_FilterNotFound',_}},
		 Mod:remove_filter(Obj, 0)),
    ID = Mod:add_filter(Obj, Filter),
    ?match([ID], Mod:get_all_filters(Obj)),
    ?match(Filter, Mod:get_filter(Obj, ID)),
    ?match(ok, Mod:remove_filter(Obj, ID)),
    ?match([], Mod:get_all_filters(Obj)),
    ID2 = Mod:add_filter(Obj, Filter),
    ?match([ID2], Mod:get_all_filters(Obj)),
    ?match(ok, Mod:remove_all_filters(Obj)),
    ?match([], Mod:get_all_filters(Obj)),
    ok.

%%-----------------------------------------------------------------
%%  Creating different event pushing and pulling API tests 
%%-----------------------------------------------------------------
events_api(_Config) ->
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),
    events_api_helper(Fac, Ch, Id1).

events2_api(doc) -> ["CosNotification event pushing and pulling tests II", ""];
events2_api(suite) -> [];
events2_api(_Config) ->
    %% Initialize the application.
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS2, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),
    events_api_helper(Fac, Ch, Id1).

events_api_helper(Fac, Ch, _Id1) ->
    %% Now we will set up a test environment, with the following structure:
    %%
    %%                              Channel
    %%                              /      \
    %%                    Supplier Adm    Consumer Adm
    %%                            /          \
    %%                  1 proxy of each possible type
    %%                  To each proxy we will connect a test client
    %%                  The events will flow in ===>> direction.
    %%
    %% For the supplier Admins this include:
    %% - ProxyPushConsumer
    %% - SequenceProxyPushConsumer 
    %% - StructuredProxyPushConsumer
    %% - ProxyPullConsumer
    %% - SequenceProxyPullConsumer 
    %% - StructuredProxyPullConsumer
    %%
    %% For the consumer Admins this include:
    %% - ProxyPushSupplier
    %% - SequenceProxyPushSupplier
    %% - StructuredProxyPushSupplier
    %% - ProxyPullSupplier
    %% - SequenceProxyPullSupplier
    %% - StructuredProxyPullSupplier
    %% 
    %%
    %% We will not use any Filters to begin with, just want to make sure we can
    %% deliver events from all start- to end-points.
    
    %% Create the Admin objects
    {AdminSupplier, _ASID}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_EventChannel':new_for_suppliers(Ch,'AND_OP')),
    {AdminConsumer, _ACID}=?match({{_,key,_,_,_,_},_}, 
	'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch,'AND_OP')),
    
    %% Create Suppliers Proxies
    {ProxyPullSupplier,_ID1}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(AdminConsumer, 'ANY_EVENT')),
    {StructuredProxyPullSupplier,_ID2}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(AdminConsumer, 'STRUCTURED_EVENT')),
    {SequenceProxyPullSupplier,_ID3}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_pull_supplier(AdminConsumer, 'SEQUENCE_EVENT')),
    
    {ProxyPushSupplier,_I4D}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'ANY_EVENT')),
    {StructuredProxyPushSupplier,_ID5}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'STRUCTURED_EVENT')),
    {SequenceProxyPushSupplier,_ID6}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(AdminConsumer, 'SEQUENCE_EVENT')),

    %% Now we must create a Client for each proxy and connect them.
    PushAnyC=?match({_,key,_,_,_,_}, 'notify_test_AnyPushC':oe_create(['PUSH_ANY', ProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPushSupplier':connect_any_push_consumer(ProxyPushSupplier, PushAnyC)),

    PushStrC=?match({_,key,_,_,_,_}, 'notify_test_StrPushC':oe_create(['PUSH_STRUCTURED',StructuredProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':connect_structured_push_consumer(StructuredProxyPushSupplier, PushStrC)),

    PushSeqC=?match({_,key,_,_,_,_}, 'notify_test_SeqPushC':oe_create(['PUSH_SEQUENCE',SequenceProxyPushSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':connect_sequence_push_consumer(SequenceProxyPushSupplier, PushSeqC)),

    PullAnyC=?match({_,key,_,_,_,_}, 'notify_test_AnyPullC':oe_create(['PULL_ANY', ProxyPullSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPullSupplier':connect_any_pull_consumer(ProxyPullSupplier, PullAnyC)),

    PullStrC=?match({_,key,_,_,_,_}, 'notify_test_StrPullC':oe_create(['PULL_STRUCTURED',StructuredProxyPullSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':connect_structured_pull_consumer(StructuredProxyPullSupplier, PullStrC)),

    PullSeqC=?match({_,key,_,_,_,_}, 'notify_test_SeqPullC':oe_create(['PULL_SEQUENCE',SequenceProxyPullSupplier],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':connect_sequence_pull_consumer(SequenceProxyPullSupplier, PullSeqC)),

    
    %% Create Consumers Proxies
    {ProxyPullConsumer,_ID7}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_pull_consumer(AdminSupplier, 'ANY_EVENT')),
    {StructuredProxyPullConsumer,_ID8}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_pull_consumer(AdminSupplier, 'STRUCTURED_EVENT')),
    {SequenceProxyPullConsumer,_ID9}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_pull_consumer(AdminSupplier, 'SEQUENCE_EVENT')),

    {ProxyPushConsumer,_ID10}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'ANY_EVENT')),
    {StructuredProxyPushConsumer,_ID11}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'STRUCTURED_EVENT')),
    {SequenceProxyPushConsumer,_ID12}=?match({{_,key,_,_,_,_},_},
	'CosNotifyChannelAdmin_SupplierAdmin':obtain_notification_push_consumer(AdminSupplier, 'SEQUENCE_EVENT')),

    %% Now we must create a Client for each proxy and connect them.
    PushAnyS=?match({_,key,_,_,_,_}, 'notify_test_AnyPushS':oe_create(['PUSH_ANY', ProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPushConsumer':connect_any_push_supplier(ProxyPushConsumer, PushAnyS)),

    PushStrS=?match({_,key,_,_,_,_}, 'notify_test_StrPushS':oe_create(['PUSH_STRUCTURED',StructuredProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':connect_structured_push_supplier(StructuredProxyPushConsumer, PushStrS)),

    PushSeqS=?match({_,key,_,_,_,_}, 'notify_test_SeqPushS':oe_create(['PUSH_SEQUENCE',SequenceProxyPushConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':connect_sequence_push_supplier(SequenceProxyPushConsumer, PushSeqS)),

    PullAnyS=?match({_,key,_,_,_,_}, 'notify_test_AnyPullS':oe_create(['PULL_ANY', ProxyPullConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPullConsumer':connect_any_pull_supplier(ProxyPullConsumer, PullAnyS)),

    PullStrS=?match({_,key,_,_,_,_}, 'notify_test_StrPullS':oe_create(['PULL_STRUCTURED',StructuredProxyPullConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':connect_structured_pull_supplier(StructuredProxyPullConsumer, PullStrS)),

    PullSeqS=?match({_,key,_,_,_,_}, 'notify_test_SeqPullS':oe_create(['PULL_SEQUENCE',SequenceProxyPullConsumer],
								      [{local_typecheck, false}])),
    ?match(ok, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':connect_sequence_pull_supplier(SequenceProxyPullConsumer, PullSeqS)),

    
    %% Create a couple of Events to test with.
    Event = ?not_CreateSE("DomainName","CommunicationsAlarm",
			  "lost_packet",
			  [#'CosNotification_Property'{name="priority", 
						       value=any:create(orber_tc:short(), 1)}],
			  [], any:create(orber_tc:null(), null)),

    Event2 = ?not_CreateSE("DomainName","TemperatureAlarm",
			   "over_heated",
			   [#'CosNotification_Property'{name="priority", 
							value=any:create(orber_tc:short(), 10)}],
			   [], any:create(orber_tc:null(), null)),


    AnyEvent = any:create(orber_tc:long(), 100),

    StrEvent = ?not_CreateSE("","%ANY","",[],[],AnyEvent),
    NilAnyEvent = any:create(orber_tc:null(), null),
    NilStrEvent = ?not_CreateSE("","","",[],[],NilAnyEvent),

    ConvertedStr = any:create('CosNotification_StructuredEvent':tc(), Event),

    io:format("###################### PUSH STRUCTURED ########################"),

    %% Test with pushing a structured event.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, Event)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([{any,_,Event}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([Event], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([Event], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({any,_,Event}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([Event], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),

    io:format("###################### PUSH SEQUENCE ########################"),

    %% Create an Event Sequence and push it.
    EventSeq = [Event, Event2],
    
    %% Test with pushing a event sequence.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, EventSeq)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),
		
    %% Instruct the Clients to pull the events and check if they match.
    ?match({any,_,Event}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([Event,Event2], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,2})),
    ?match({any,_,Event2}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event2, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    %% Check if the Push Clients have received and stored the events.
    ?match([{any,_,Event}, {any,_,Event2}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([Event, Event2], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([Event, Event2], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    io:format("######################  PUSH ANY ########################"),

    %% Test with pushing an any event.
    ?match(ok,'CosEventChannelAdmin_ProxyPushConsumer':push(ProxyPushConsumer, AnyEvent)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([AnyEvent], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([StrEvent], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([StrEvent], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match(AnyEvent, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(StrEvent, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([StrEvent], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,10})),



    io:format("###################### PUSH CONVERTED ANY #############"),

    %% Test with pushing a structured event.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, StrEvent)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([AnyEvent], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([StrEvent], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([StrEvent], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match(AnyEvent, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(StrEvent, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([StrEvent], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),


    io:format("###################### PUSH CONVERTED STRUCTURED ########"),

    %% Test with pushing an any event.
    ?match(ok,'CosEventChannelAdmin_ProxyPushConsumer':push(ProxyPushConsumer, ConvertedStr)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([ConvertedStr], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([Event], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([Event], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match(ConvertedStr, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([Event], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,10})),


    io:format("###################### TRY PULL ########################"),

    %% Now we will push an any event after a delay. This means that try_pull-functions,
    %% since it's not blocking, will return, [], NilAny or NilStructured events and 
    %% the Boolean false.
    spawn(notify_test_impl, delay, [ProxyPushConsumer, AnyEvent, 20000, 
				    'CosEventChannelAdmin_ProxyPushConsumer',push]),
    ?match([], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({NilAnyEvent,false}, 'notify_test_AnyPullC':doAction(PullAnyC, try_pull_any)),
    ?match({NilStrEvent,false}, 'notify_test_StrPullC':doAction(PullStrC, try_pull_str)),
    ?match({[],false}, 'notify_test_SeqPullC':doAction(PullSeqC, {try_pull_seq,10})),


    %% Instruct the Clients to pull the events and check if they match.
    %% Pull is blocking so in the print-out we should see that nothing
    %% is returned until the pushed event reaches the end proxies.
    ?match(AnyEvent, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(StrEvent, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([StrEvent], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),

    %% To make sure there are no other circumstanses which lead to a delay we
    %% hold for some time.
    timer:sleep(5000),
    %% Check if the Clients have received and stored the events.
    ?match([AnyEvent], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([StrEvent], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([StrEvent], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Test with pushing a event sequence but only pull sequences of length 1.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, EventSeq)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),
    %% Pull 1 event at a time.
    ?match([Event], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),
    ?match([Event2], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),

    %% Following cases already tested; done for clean up.
    ?match({any,_,Event}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match({any,_,Event2}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event2, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([{any,_,Event}, {any,_,Event2}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([Event, Event2], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([Event, Event2], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),
    %% clean up done


    io:format("###################### PROXY PULLER ########################"),

    %% Now we will just add Events to a cleint and wait for the Notification service
    %% to pull the events and forward them to the consumer clients.
    ?match(ok, 'notify_test_SeqPushC':doAction(PullAnyS, {set_data, [AnyEvent]})),
    

    %% Instruct the Clients to pull the events and check if they match.
    %% Pull is blocking so in the print-out we should see that nothing
    %% is returned until the pushed event reaches the end proxies.
    ?match(AnyEvent, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(StrEvent, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([StrEvent], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,10})),

    %% To make sure there are no other circumstanses which lead to a delay we
    %% hold for some time.
    timer:sleep(5000),
    %% Check if the Clients have received and stored the events.
    ?match([AnyEvent], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([StrEvent], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([StrEvent], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    io:format("###################### SUSPENDED CONNECTION ################"),


    %% Suspend the connections
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushSupplier':suspend_connection(SequenceProxyPushSupplier)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushSupplier':suspend_connection(StructuredProxyPushSupplier)),

    %% Test with pushing a event sequence.
    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushConsumer':push_structured_events(SequenceProxyPushConsumer, EventSeq)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({any,_,Event}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([Event,Event2], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,2})),
    ?match({any,_,Event2}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(Event2, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),

    %% Check if the Any Client have received and stored the events.
    ?match([{any,_,Event}, {any,_,Event2}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),

    %% Check if the other Clients have received any events. Error if have.
    ?match([], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    ?match(ok,'CosNotifyChannelAdmin_SequenceProxyPushSupplier':resume_connection(SequenceProxyPushSupplier)),
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushSupplier':resume_connection(StructuredProxyPushSupplier)),

    %% To be sure the test case don't fail due to time-race, sleep.
    timer:sleep(5000),

    ?match([Event, Event2], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([Event, Event2], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),


    io:format("###################### FILTER EVENTS #######################"),

    %% Now we will add filters and see if the system behaves correctly.
    FiFac = 'CosNotifyFilter_FilterFactory':oe_create(),
    Filter = 'CosNotifyFilter_FilterFactory':create_filter(FiFac,"EXTENDED_TCL"),
    %% Add constraints to the Filter
    [{_,_,CID1},{_,_,CID2}]= 
	?match([{'CosNotifyFilter_ConstraintInfo',_,_}, {'CosNotifyFilter_ConstraintInfo',_,_}],
	       'CosNotifyFilter_Filter':add_constraints(Filter, 
			[#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                             domain_name = "Spare*",
                                                             type_name = "MOVIE"}],
			                                  constraint_expr = "$type_name == 'MOVIE' and (('groucho' in $starlist) + ('chico' in $starlist) + ('harpo' in $starlist) + ('zeppo' in $starlist) + ('gummo' in $starlist)) > 2"},
			#'CosNotifyFilter_ConstraintExp'{event_types = 
							  [#'CosNotification_EventType'{
                                                              domain_name = "*",
                                                             type_name = "TestResults"}],
			                                  constraint_expr = "$test._length == 3 and ($test[0].score + $test[1].score + $test[2].score)/3 >=80"}])),

    ?match([{'CosNotifyFilter_ConstraintInfo',_,CID2}, {'CosNotifyFilter_ConstraintInfo',_,CID1}],
		 'CosNotifyFilter_Filter':get_all_constraints(Filter)),
    ?match([{'CosNotifyFilter_ConstraintInfo',_,CID1}], 
		 'CosNotifyFilter_Filter':get_constraints(Filter, [CID1])),

    %% Associate the Filter with different objects.
    %% Since we use the same filter for both objects the events will never reach the admin.
    _FilterID = 'CosNotifyChannelAdmin_ConsumerAdmin':add_filter(AdminConsumer, Filter),

    _FilterID2 = 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':add_filter(StructuredProxyPushConsumer, Filter),
    event_filtering(FiFac, Filter, AdminConsumer, StructuredProxyPushConsumer, PushAnyC, 
			  PushStrC, PushSeqC, PullAnyC, PullStrC, PullSeqC),
    %% Remove the proxy filter so we can check if the events are filtered correctly by the admin.
    ?match(ok, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':remove_all_filters(StructuredProxyPushConsumer)),
    event_filtering(FiFac, Filter, AdminConsumer, StructuredProxyPushConsumer, PushAnyC, 
			  PushStrC, PushSeqC, PullAnyC, PullStrC, PullSeqC),


    catch corba:dispose(Filter),
    catch corba:dispose(FiFac),
    catch corba:dispose(SequenceProxyPushConsumer),
    catch corba:dispose(StructuredProxyPushConsumer),
    catch corba:dispose(ProxyPushConsumer),
    catch corba:dispose(SequenceProxyPullConsumer),
    catch corba:dispose(StructuredProxyPullConsumer),
    catch corba:dispose(ProxyPullConsumer),
    catch corba:dispose(SequenceProxyPushSupplier),
    catch corba:dispose(StructuredProxyPushSupplier),
    catch corba:dispose(ProxyPushSupplier),
    catch corba:dispose(SequenceProxyPullSupplier),
    catch corba:dispose(StructuredProxyPullSupplier),
    catch corba:dispose(ProxyPullSupplier),
    catch corba:dispose(AdminConsumer),
    catch corba:dispose(AdminSupplier),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    %% The Clients should have terminated by now. Check if it is so.
    timer:sleep(5000),
    ?match(true, corba_object:non_existent(PullSeqS)),
    ?match(true, corba_object:non_existent(PullStrS)),
    ?match(true, corba_object:non_existent(PullAnyS)),
    ?match(true, corba_object:non_existent(PushSeqS)),
    ?match(true, corba_object:non_existent(PushStrS)),
    ?match(true, corba_object:non_existent(PushAnyS)),
    ?match(true, corba_object:non_existent(PullSeqC)),
    ?match(true, corba_object:non_existent(PullStrC)),
    ?match(true, corba_object:non_existent(PullAnyC)),
    ?match(true, corba_object:non_existent(PushSeqC)),
    ?match(true, corba_object:non_existent(PushStrC)),
    ?match(true, corba_object:non_existent(PushAnyC)),
    ok.

event_filtering(_FiFac, _Filter, _AdminConsumer, StructuredProxyPushConsumer, PushAnyC, PushStrC, PushSeqC, PullAnyC, PullStrC, PullSeqC) ->
    NilAnyEvent = any:create(orber_tc:null(), null),
    NilStrEvent = ?not_CreateSE("","","",[],[],NilAnyEvent),

    TrueEvent1 = ?not_CreateSE("SpareTime","MOVIE",
					 "EventName", 
					 [#'CosNotification_Property'{name="starlist", 
								      value=any:create(orber_tc:sequence(orber_tc:string(0),0),
										       ["groucho", "harpo", "sam", "gummo"])}],
					 [], any:create(orber_tc:null(), null)),
    TrueEvent2 = ?not_CreateSE("Studies","TestResults",
					 "EventName", [],
					 [#'CosNotification_Property'{name="test", 
								      value=any:create(orber_tc:array(notify_test_data:tc(),3),
										       {#notify_test_data{score=75,
													 name="name"},
											#notify_test_data{score=80,
													 name="name"},
											#notify_test_data{score=85,
													 name="name"}})}],
					 any:create(orber_tc:null(), null)),

    FalseEvent1 = ?not_CreateSE("SpareTime","MOVIE",
					 "EventName", 
					 [#'CosNotification_Property'{name="starlist", 
								      value=any:create(orber_tc:sequence(orber_tc:string(0),0),
										       ["frodo", "bilbo", "sam", "gummo"])}],
					 [], any:create(orber_tc:null(), null)),
    FalseEvent2 = ?not_CreateSE("Studies","TestResults",
					 "EventName", [],
					 [#'CosNotification_Property'{name="test", 
								      value=any:create(orber_tc:array(notify_test_data:tc(),3),
										       {#notify_test_data{score=75,
													 name="name"},
											#notify_test_data{score=80,
													 name="name"},
											#notify_test_data{score=80,
													 name="name"}})}],
					 any:create(orber_tc:null(), null)),
    %% Test with pushing the first structured event that should not be filtered away.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, TrueEvent1)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([{any,_,TrueEvent1}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([TrueEvent1], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([TrueEvent1], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({any,_,TrueEvent1}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(TrueEvent1, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([TrueEvent1], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),

    %% Test with pushing the second structured event that should not be filtered away.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, TrueEvent2)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([{any,_,TrueEvent2}], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([TrueEvent2], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([TrueEvent2], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({any,_,TrueEvent2}, 'notify_test_AnyPullC':doAction(PullAnyC, pull_any)),
    ?match(TrueEvent2, 'notify_test_StrPullC':doAction(PullStrC, pull_str)),
    ?match([TrueEvent2], 'notify_test_SeqPullC':doAction(PullSeqC, {pull_seq,1})),

    %% Test with pushing the first structured event that should be filtered away.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, FalseEvent1)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({NilAnyEvent,false}, 'notify_test_AnyPullC':doAction(PullAnyC, try_pull_any)),
    ?match({NilStrEvent,false}, 'notify_test_StrPullC':doAction(PullStrC, try_pull_str)),
    ?match({[],false}, 'notify_test_SeqPullC':doAction(PullSeqC, {try_pull_seq,10})),

    %% Test with pushing the second structured event that should be filtered away.
    ?match(ok,'CosNotifyChannelAdmin_StructuredProxyPushConsumer':push_structured_event(StructuredProxyPushConsumer, FalseEvent2)),

    %% Wait for a while so we are sure that all events have been delivered as far
    %% as the Notification service can automatically.
    timer:sleep(5000),

    %% Check if the Clients have received and stored the events.
    ?match([], 'notify_test_AnyPushC':doAction(PushAnyC, return_data)),
    ?match([], 'notify_test_StrPushC':doAction(PushStrC, return_data)),
    ?match([], 'notify_test_SeqPushC':doAction(PushSeqC, return_data)),

    %% Instruct the Clients to pull the events and check if they match.
    ?match({NilAnyEvent,false}, 'notify_test_AnyPullC':doAction(PullAnyC, try_pull_any)),
    ?match({NilStrEvent,false}, 'notify_test_StrPullC':doAction(PullStrC, try_pull_str)),
    ?match({[],false}, 'notify_test_SeqPullC':doAction(PullSeqC, {try_pull_seq,10})).
    


%%-----------------------------------------------------------------
%%  Creating different cosEvent API tests 
%%-----------------------------------------------------------------
cosevent_api(_Config) ->
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),
    AC=?match({_,key,_,_,_,_},
		    'CosEventChannelAdmin_EventChannel':for_consumers(Ch)),
    AS=?match({_,key,_,_,_,_},
		    'CosEventChannelAdmin_EventChannel':for_suppliers(Ch)),

    PushS=?match({_,key,_,_,_,_},
		       'CosEventChannelAdmin_ConsumerAdmin':obtain_push_supplier(AC)),
    PullS=?match({_,key,_,_,_,_},
		       'CosEventChannelAdmin_ConsumerAdmin':obtain_pull_supplier(AC)),

    PushC=?match({_,key,_,_,_,_},
		       'CosEventChannelAdmin_SupplierAdmin':obtain_push_consumer(AS)),
    PullC=?match({_,key,_,_,_,_},
		       'CosEventChannelAdmin_SupplierAdmin':obtain_pull_consumer(AS)),

    PushAnyC=?match({_,key,_,_,_,_},
			  'notify_test_AnyPushC':oe_create(['PUSH_ANY', PushC],
							   [{local_typecheck, false}])),
    PushStrC=?match({_,key,_,_,_,_},
			  'notify_test_StrPushC':oe_create(['PUSH_STRUCTURED',false],
							   [{local_typecheck, false}])),
    PushSeqC=?match({_,key,_,_,_,_},
			  'notify_test_SeqPushC':oe_create(['PUSH_SEQUENCE',false],
							   [{local_typecheck, false}])),

    PullAnyC=?match({_,key,_,_,_,_},
			  'notify_test_AnyPullC':oe_create(['PULL_ANY', PullC],
							   [{local_typecheck, false}])),
    PullStrC=?match({_,key,_,_,_,_},
			  'notify_test_StrPullC':oe_create(['PULL_STRUCTURED',false],
							   [{local_typecheck, false}])),
    PullSeqC=?match({_,key,_,_,_,_},
			  'notify_test_SeqPullC':oe_create(['PULL_SEQUENCE',false],
							   [{local_typecheck, false}])),

    PushAnyS=?match({_,key,_,_,_,_},
			  'notify_test_AnyPushS':oe_create(['PUSH_ANY', PushC],
							   [{local_typecheck, false}])),
    PushStrS=?match({_,key,_,_,_,_},
			  'notify_test_StrPushS':oe_create(['PUSH_STRUCTURED',false],
							   [{local_typecheck, false}])),
    PushSeqS=?match({_,key,_,_,_,_},
			  'notify_test_SeqPushS':oe_create(['PUSH_SEQUENCE',false],
							   [{local_typecheck, false}])),

    PullAnyS=?match({_,key,_,_,_,_},
			  'notify_test_AnyPullS':oe_create(['PULL_ANY', PullS],
							   [{local_typecheck, false}])),
    PullStrS=?match({_,key,_,_,_,_},
			  'notify_test_StrPullS':oe_create(['PULL_STRUCTURED',false],
							   [{local_typecheck, false}])),
    PullSeqS=?match({_,key,_,_,_,_},
			  'notify_test_SeqPullS':oe_create(['PULL_SEQUENCE',false],
							   [{local_typecheck, false}])),

    %% In the OMG specification Proxies do not inherrit from CosEvent. Must use
    %% Notify interface.
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
		 'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PullC, PushStrS)),

    ?match(ok, 
		 'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PushS, PushAnyC)),
    ?match(ok, 
		 'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PullS, PullAnyC)),

    ?match(ok, 
		 'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PushC, PushAnyS)),
    ?match(ok, 
		 'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PullC, PullAnyS)),

    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
		 'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PullC, PullAnyS)),

    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
		 'CosNotifyChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PullC, PullAnyS)),

    ?match(true, corba_object:is_a(PushS, "IDL:omg.org/CosNotifyChannelAdmin/ProxyPushSupplier:1.0")),
    ?match(true, corba_object:is_a(PushS, "IDL:omg.org/CosEventChannelAdmin/ProxyPushSupplier:1.0")),

    catch corba:dispose(PushStrC),
    catch corba:dispose(PushSeqC),
    catch corba:dispose(PullStrC),
    catch corba:dispose(PullSeqC),
    catch corba:dispose(PushStrS),
    catch corba:dispose(PushSeqS),
    catch corba:dispose(PullStrS),
    catch corba:dispose(PullSeqS),
    catch corba:dispose(PushS),
    catch corba:dispose(PullS),
    catch corba:dispose(PushC),
    catch corba:dispose(PullC),
    catch corba:dispose(AC),
    catch corba:dispose(AS),
    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),

    %% The Clients should have terminated by now. Check if it is so.
    timer:sleep(5000),
    ?match(true, corba_object:non_existent(PullAnyS)),
    ?match(true, corba_object:non_existent(PushAnyS)),
    ?match(true, corba_object:non_existent(PullAnyC)),
    ?match(true, corba_object:non_existent(PushAnyC)),


    ok.

%%-----------------------------------------------------------------
%%  AdminPropertiesAdmin API tests 
%%-----------------------------------------------------------------
adm_api(_Config) ->
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),

    %% We need a few AdminProp:s to "play" with.
    MQ0 = [#'CosNotification_Property'{name='CosNotification':'MaxQueueLength'(), 
				       value=any:create(orber_tc:long(), 0)}],
    MC0 = [#'CosNotification_Property'{name='CosNotification':'MaxConsumers'(), 
				       value=any:create(orber_tc:long(), 0)}],
    MS0 = [#'CosNotification_Property'{name='CosNotification':'MaxSuppliers'(), 
				       value=any:create(orber_tc:long(), 0)}],
    MQError1 = [#'CosNotification_Property'{name='CosNotification':'MaxQueueLength'(), 
					    value=any:create(orber_tc:'float'(), 1.5)}],
    MQError2 = [#'CosNotification_Property'{name='CosNotification':'MaxQueueLength'(), 
					    value=any:create(orber_tc:long(), -1)}],

    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),

    %% Set new admin
    ?match(ok, 'CosNotification_AdminPropertiesAdmin':set_admin(Ch, MQ0)),
    %% It should be a list of three items. If we support more admin:s this 
    %% must be updated.
    ?match([_,_,_], 'CosNotification_AdminPropertiesAdmin':get_admin(Ch)),

    %% Try to set admin with an uncorrect value, i.e., not integer >= 0.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedAdmin',_,_}},
		 'CosNotification_AdminPropertiesAdmin':set_admin(Ch, MQError1)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedAdmin',_,_}},
		 'CosNotification_AdminPropertiesAdmin':set_admin(Ch, MQError2)),

    %% Try setting the other two admins and chech if the value is correct.
    ?match(ok, 'CosNotification_AdminPropertiesAdmin':set_admin(Ch, MC0)),
    ?match([_,_,_], 'CosNotification_AdminPropertiesAdmin':get_admin(Ch)),

    ?match(ok, 'CosNotification_AdminPropertiesAdmin':set_admin(Ch, MS0)),
    ?match([_,_,_], 'CosNotification_AdminPropertiesAdmin':get_admin(Ch)),

    catch corba:dispose(Ch),
    catch cosNotificationApp:stop_factory(Fac),
    ok.


%%-----------------------------------------------------------------
%%  QoSAdm API tests 
%%-----------------------------------------------------------------
qos_api(_Config) ->
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),

    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    ?match({_,key,_,_,_,_}, Ch),


    QoSPersistent = [#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'Persistent'())}],
    QoSBestEffort = [#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'BestEffort'())}],

    QoSEventPersistent = [#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'Persistent'())}],
    QoSEventBestEffort = [#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'BestEffort'())}],
    
    QoSOKMaxBatchSize = [#'CosNotification_Property'{name='CosNotification':'MaximumBatchSize'(), 
						     value=any:create(orber_tc:long(), 200)}],
    QoSToHighMaxBatchSize = [#'CosNotification_Property'{name='CosNotification':'MaximumBatchSize'(), 
							 value=any:create(orber_tc:long(), 100000000)}],
    
    QoSToLowMaxBatchSize = [#'CosNotification_Property'{name='CosNotification':'MaximumBatchSize'(), 
							value=any:create(orber_tc:long(), -1)}],

    QoSOKStopTimeSupp = [#'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
						     value=any:create(orber_tc:boolean(), true)}],
    QoSWrongStopTimeSupp = [#'CosNotification_Property'{name="StopTimeSupp", 
							value=any:create(orber_tc:boolean(), true)}],
    
    QoSOKStartTimeSupp = [#'CosNotification_Property'{name='CosNotification':'StartTimeSupported'(), 
						      value=any:create(orber_tc:boolean(), true)}],
    QoSWrongStartTimeSupp = [#'CosNotification_Property'{name="StartTimeSupp", 
							 value=any:create(orber_tc:boolean(), true)}],
    QoSOKTimout = [#'CosNotification_Property'{name='CosNotification':'Timeout'(), 
					       value=any:create(orber_tc:unsigned_long_long(), 100)}],
    

    %% The most complex QoS to set is ConnectionReliability, and the reason for this
    %% is that we cannot set the Channel to offer best effort while its children
    %% offer persistent. A child may only offer Persistent if its parent do, which
    %% is why we must check the following:
    %%           
    %%                    #    Persistent        Change to       Best Effort
    %%            _____
    %%           |     | (1)                         ->       Check if children BE
    %%           |Chann| (2)      ok                 <-
    %%            -----
    %%              |
    %%            _____
    %%           |     | (3)                         ->      Check if children BE
    %%           |Admin| (4)  Check if parent Pers.  <-      
    %%            -----
    %%              |
    %%            _____
    %%           |     | (5)                         ->               ok
    %%           |Proxy| (6) Check if parent Pers.   <-
    %%            -----
    %% NOTE: a parent always exists but we may change the QoS before creating any
    %% childrens. The cases (2) and (5) is always ok, i.e., no need to confirm
    %% with parent or children.

    %% We only have a channel. At the moment we can set ConnectionReliability
    %% without asking anyone.
    Q1='CosNotification_QoSAdmin':get_qos(Ch),
    ?match({ok, _}, 'CosNotification_QoSAdmin':validate_qos(Ch, QoSBestEffort)),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    %% Match if no problems occur if we try to set QoS as is.
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),

    %% Check validate.
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventPersistent)),
    ?match({ok, _}, 'CosNotification_QoSAdmin':validate_qos(Ch, QoSOKTimout)),
    ?match({ok, _}, 'CosNotification_QoSAdmin':validate_qos(Ch, QoSEventBestEffort)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventBestEffort)),
    ?match({ok, _}, 'CosNotification_QoSAdmin':validate_qos(Ch, QoSOKTimout)),

    Q2='CosNotification_QoSAdmin':get_qos(Ch),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSBestEffort)),
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(Ch)),

    %% Now we add an Admin object. An Admin object cannot switch ConnectionReliability
    %% to BestEffort without checking with its children or Persistent without
    %% confirming this with its Parent. At the moment, however, we only have a parent.
    {CAdm, Id2} = 'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch, 'AND_OP'),
    ?match(Q1,'CosNotification_QoSAdmin':get_qos(CAdm)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(CAdm, QoSPersistent)),
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(CAdm)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(CAdm, QoSBestEffort)),
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(CAdm)),

    %% Check if we can extract the Admin from the channel correctly.
    ?match([0,Id2],'CosNotifyChannelAdmin_EventChannel':get_all_consumeradmins(Ch)),
    ?match(CAdm,'CosNotifyChannelAdmin_EventChannel':get_consumeradmin(Ch, Id2)),
    ?match(Ch, 'CosNotifyChannelAdmin_ConsumerAdmin':'_get_MyChannel'(CAdm)),
    ?match(Id2, 'CosNotifyChannelAdmin_ConsumerAdmin':'_get_MyID'(CAdm)),

    %% Change the channel to provide Persistent service. Now we can set the 
    %% Admin service to Persistent to. (4)
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(CAdm, QoSPersistent)),
    ?match(Q2, 'CosNotification_QoSAdmin':get_qos(CAdm)),

    %% Since the Admin object now provide Persistent the Channel cannot switch
    %% to BestEffort. (1)
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSBestEffort)),
    %% Should still match Persistent.
    ?match(Q2, 'CosNotification_QoSAdmin':get_qos(Ch)),
    {PSup, _Id3} = 'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(CAdm, 'ANY_EVENT'),
    ?match(Q2, 'CosNotification_QoSAdmin':get_qos(CAdm)),
    ?match('PUSH_ANY', 'CosNotifyChannelAdmin_ProxyPushConsumer':'_get_MyType'(PSup)),
    ?match(CAdm, 'CosNotifyChannelAdmin_ProxyPushConsumer':'_get_MyAdmin'(PSup)),
    ?match(Q2, 'CosNotification_QoSAdmin':get_qos(PSup)),

    %% At this point they all offer persistent connection, which means we have
    %% to start with the proxy if we want to change to Best Effort. Hence,
    %% the following two cases will fail.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSBestEffort)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(CAdm, QoSBestEffort)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, QoSBestEffort)),
    %% Still not possible to change channel to Best Effort.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSBestEffort)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(CAdm, QoSBestEffort)),
    %% Now we change the channel to Best Effort.
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSBestEffort)),
    
    %% Test if really are Best Effort
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(Ch)),
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(CAdm)),
    ?match(Q1, 'CosNotification_QoSAdmin':get_qos(PSup)),

    %% Testing MaximumBatchSize (The highest value is defined in 
    %% CosNotification_Common.erl
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSOKMaxBatchSize)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSToHighMaxBatchSize)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSToLowMaxBatchSize)),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSOKStartTimeSupp)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSOKStopTimeSupp)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSWrongStartTimeSupp)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotification_QoSAdmin':set_qos(Ch, QoSWrongStopTimeSupp)),

    catch corba:dispose(CAdm),
    catch corba:dispose(PSup),
    catch corba:dispose(Ch),
    cosNotificationApp:stop_factory(Fac),
    ok.

%%-----------------------------------------------------------------
%%  QoSAdm API tests 
%%-----------------------------------------------------------------
event_qos_api(_Config) ->
    Fac = (catch cosNotificationApp:start_global_factory(?FAC_OPT)),
    ?match({_,key,_,_,_,_}, Fac),
    
    %% Create some objects to test with. We start with default settings.
    {Ch, _Id1} = (catch 'CosNotifyChannelAdmin_EventChannelFactory':create_channel(Fac, ?defaultQoS, ?defaultAdm)),
    {CAdm, _Id2} = 'CosNotifyChannelAdmin_EventChannel':new_for_consumers(Ch, 'AND_OP'),
    {PSup, _Id3} = 'CosNotifyChannelAdmin_ConsumerAdmin':obtain_notification_push_supplier(CAdm, 'ANY_EVENT'),

    %% Try setting an unsupported QoS.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name="Unsupported QoS", 
							   value=any:create(orber_tc:short(), 1)}])),
    %% Try setting min and max priority.
    ?match({ok, _}, 'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_Priority, 
							   value=any:create(orber_tc:short(), 
									    ?not_LowestPriority)},
			       #'CosNotification_Property'{name=?not_Priority, 
							   value=any:create(orber_tc:short(), 
									    ?not_HighestPriority)}])),
    %% Try setting priority values which are 1 to high and 1 to low respectively.
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_Priority, 
							   value=any:create(orber_tc:short(), 
									    ?not_LowestPriority-1)},
			       #'CosNotification_Property'{name=?not_Priority, 
							   value=any:create(orber_tc:short(), 
									    ?not_HighestPriority+1)}])),
    %% Try setting start- and stop-time (false default). Note the value associated 
    %% with this property is not really a short but that is not what we are testing 
    %% here so...
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_StartTime, 
							   value=any:create(orber_tc:short(), 0)}])),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_StopTime, 
							   value=any:create(orber_tc:short(), 0)}])),
    %% Allow StopTime
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, [#'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
										   value=any:create(orber_tc:boolean(), true)}])),
    ?match({ok,_},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_StopTime, 
							   value=any:create(orber_tc:short(), 0)}])),
    %% Allow StartTime
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, [#'CosNotification_Property'{name='CosNotification':'StartTimeSupported'(), 
										   value=any:create(orber_tc:boolean(), true)}])),
    ?match({ok,_},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_StopTime, 
							   value=any:create(orber_tc:short(), 0)},
			      #'CosNotification_Property'{name=?not_StartTime, 
							  value=any:create(orber_tc:short(), 0)}])),

    %% We must reset StopTime since we cannot guarantee that an event will be delivered
    %% if risk beeing discarded due to a delay.
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, [#'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
										     value=any:create(orber_tc:boolean(), false)}])),
    %% Does it accept Best Effort EventReliability? Must always be true.
    ?match({ok,_},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_EventReliability, 
							   value=any:create(orber_tc:short(), ?not_BestEffort)}])),
    %% Default is Best Effort; test if we can set Persistent EventReliability.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_EventReliability, 
							   value=any:create(orber_tc:short(), ?not_Persistent)}])),

    %% Set Persistent
    QoSPersistent = [#'CosNotification_Property'{name='CosNotification':'ConnectionReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'Persistent'())}],
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(CAdm, QoSPersistent)),
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, QoSPersistent)),

    %% Does it accept Best Effort EventReliability? Must always be true.
    ?match({ok, _},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_EventReliability, 
							   value=any:create(orber_tc:short(), ?not_BestEffort)}])),
    %% Test if we can use Persistent EventReliability.
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_EventReliability, 
							   value=any:create(orber_tc:short(), ?not_Persistent)}])),
    QoSEventPersistent = [#'CosNotification_Property'{name='CosNotification':'EventReliability'(), 
						 value=any:create(orber_tc:short(), 
								  'CosNotification':'Persistent'())}],
    ?match(ok, 'CosNotification_QoSAdmin':set_qos(Ch, QoSEventPersistent)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}}, 
	   'CosNotification_QoSAdmin':set_qos(CAdm, QoSEventPersistent)),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}}, 
	   'CosNotification_QoSAdmin':set_qos(PSup, QoSEventPersistent)),

    ?match(ok, 'CosNotification_QoSAdmin':set_qos(PSup, [#'CosNotification_Property'{name='CosNotification':'StopTimeSupported'(), 
										     value=any:create(orber_tc:boolean(), true)}])),
    ?match({ok,_},
	   'CosNotifyChannelAdmin_ProxyConsumer':
	   validate_event_qos(PSup, 
			      [#'CosNotification_Property'{name=?not_StopTime, 
							   value=any:create(orber_tc:short(), 0)},
			      #'CosNotification_Property'{name=?not_StartTime, 
							  value=any:create(orber_tc:short(), 0)}])),
    catch corba:dispose(CAdm),
    catch corba:dispose(PSup),
    catch corba:dispose(Ch),
    cosNotificationApp:stop_factory(Fac),
    ok.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-------------------- End of Module ------------------------------
