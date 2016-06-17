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
    ['CosNotification',
     'CosNotification_AdminPropertiesAdmin',
     'CosNotification_EventHeader',
     'CosNotification_EventType',
     'CosNotification_FixedEventHeader',
     'CosNotification_NamedPropertyRange',
     'CosNotification_Property',
     'CosNotification_PropertyError',
     'CosNotification_PropertyRange',
     'CosNotification_QoSAdmin',
     'CosNotification_StructuredEvent',
     'CosNotification_UnsupportedAdmin',
     'CosNotification_UnsupportedQoS',
     'CosNotification_EventBatch',
     'CosNotification_EventTypeSeq',
     'CosNotification_NamedPropertyRangeSeq',
     'CosNotification_PropertyErrorSeq',
     'CosNotifyChannelAdmin_AdminLimit',
     'CosNotifyChannelAdmin_AdminNotFound',
     'CosNotifyChannelAdmin_ChannelNotFound',
     'CosNotifyChannelAdmin_ConnectionAlreadyActive',
     'CosNotifyChannelAdmin_ConnectionAlreadyInactive',
     'CosNotifyChannelAdmin_NotConnected',
     'CosNotifyChannelAdmin_AdminIDSeq',
     'CosNotifyChannelAdmin_ChannelIDSeq',
     'CosNotifyChannelAdmin_ProxyIDSeq',
     'CosNotifyFilter_CallbackNotFound',
     'CosNotifyFilter_ConstraintExp',
     'CosNotifyFilter_ConstraintInfo',
     'CosNotifyFilter_ConstraintNotFound',
     'CosNotifyFilter_DuplicateConstraintID',
     'CosNotifyFilter_FilterNotFound',
     'CosNotifyFilter_InvalidConstraint',
     'CosNotifyFilter_InvalidGrammar',
     'CosNotifyFilter_InvalidValue',
     'CosNotifyFilter_MappingConstraintInfo',
     'CosNotifyFilter_MappingConstraintPair',
     'CosNotifyFilter_UnsupportedFilterableData',
     'CosNotifyFilter_CallbackIDSeq',
     'CosNotifyFilter_ConstraintExpSeq',
     'CosNotifyFilter_ConstraintIDSeq',
     'CosNotifyFilter_ConstraintInfoSeq',
     'CosNotifyFilter_FilterIDSeq',
     'CosNotifyFilter_MappingConstraintInfoSeq',
     'CosNotifyFilter_MappingConstraintPairSeq',
     'CosNotifyComm_InvalidEventType',
     'CosNotifyChannelAdmin_ConsumerAdmin',
     'CosNotifyChannelAdmin_EventChannel',
     'CosNotifyChannelAdmin_EventChannelFactory',
     'CosNotifyChannelAdmin_ProxyConsumer',
     'CosNotifyChannelAdmin_ProxyNotFound',
     'CosNotifyChannelAdmin_ProxyPullConsumer',
     'CosNotifyChannelAdmin_ProxyPullSupplier',
     'CosNotifyChannelAdmin_ProxyPushConsumer',
     'CosNotifyChannelAdmin_ProxyPushSupplier',
     'CosNotifyChannelAdmin_ProxySupplier',
     'CosNotifyChannelAdmin_SequenceProxyPullConsumer',
     'CosNotifyChannelAdmin_SequenceProxyPullSupplier',
     'CosNotifyChannelAdmin_SequenceProxyPushConsumer',
     'CosNotifyChannelAdmin_SequenceProxyPushSupplier',
     'CosNotifyChannelAdmin_StructuredProxyPullConsumer',
     'CosNotifyChannelAdmin_StructuredProxyPullSupplier',
     'CosNotifyChannelAdmin_StructuredProxyPushConsumer',
     'CosNotifyChannelAdmin_StructuredProxyPushSupplier',
     'CosNotifyChannelAdmin_SupplierAdmin',
     'CosNotifyFilter_Filter', 'CosNotifyFilter_FilterAdmin',
     'CosNotifyFilter_FilterFactory',
     'CosNotifyFilter_MappingFilter',
     'CosNotifyComm_NotifyPublish',
     'CosNotifyComm_NotifySubscribe',
     'CosNotifyComm_PullConsumer',
     'CosNotifyComm_PullSupplier',
     'CosNotifyComm_PushConsumer',
     'CosNotifyComm_PushSupplier',
     'CosNotifyComm_SequencePullConsumer',
     'CosNotifyComm_SequencePullSupplier',
     'CosNotifyComm_SequencePushConsumer',
     'CosNotifyComm_SequencePushSupplier',
     'CosNotifyComm_StructuredPullConsumer',
     'CosNotifyComm_StructuredPullSupplier',
     'CosNotifyComm_StructuredPushConsumer',
     'CosNotifyComm_StructuredPushSupplier',
     oe_CosNotificationComm_Event,
     'CosNotification_PropertySeq'].

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
%% Test Case: 'CosNotification'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification'(_) ->
    ?match("EventReliability", 'CosNotification':'EventReliability'()),
    ?match(0, 'CosNotification':'BestEffort'()),
    ?match(1, 'CosNotification':'Persistent'()),
    ?match("ConnectionReliability", 'CosNotification':'ConnectionReliability'()),
    ?match("Priority", 'CosNotification':'Priority'()),
    ?match(-32767, 'CosNotification':'LowestPriority'()),
    ?match(32767, 'CosNotification':'HighestPriority'()),
    ?match(0, 'CosNotification':'DefaultPriority'()),
    ?match("StartTime", 'CosNotification':'StartTime'()),
    ?match("StopTime", 'CosNotification':'StopTime'()),
    ?match("Timeout", 'CosNotification':'Timeout'()),
    ?match("OrderPolicy", 'CosNotification':'OrderPolicy'()),
    ?match(0, 'CosNotification':'AnyOrder'()),
    ?match(1, 'CosNotification':'FifoOrder'()),
    ?match(2, 'CosNotification':'PriorityOrder'()),
    ?match(3, 'CosNotification':'DeadlineOrder'()),
    ?match("DiscardPolicy", 'CosNotification':'DiscardPolicy'()),
    ?match(4, 'CosNotification':'LifoOrder'()),
    ?match(5, 'CosNotification':'RejectNewEvents'()),
    ?match("MaximumBatchSize", 'CosNotification':'MaximumBatchSize'()),
    ?match("PacingInterval", 'CosNotification':'PacingInterval'()),
    ?match("StartTimeSupported", 'CosNotification':'StartTimeSupported'()),
    ?match("StopTimeSupported", 'CosNotification':'StopTimeSupported'()),
    ?match("MaxEventsPerConsumer", 'CosNotification':'MaxEventsPerConsumer'()),
    ?match("MaxQueueLength", 'CosNotification':'MaxQueueLength'()),
    ?match("MaxConsumers", 'CosNotification':'MaxConsumers'()),
    ?match("MaxSuppliers", 'CosNotification':'MaxSuppliers'()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_EventHeader'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_EventHeader'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_EventHeader':tc())),
    ?match("IDL:omg.org/CosNotification/EventHeader:1.0", 
	   'CosNotification_EventHeader':id()),
    ?match("CosNotification_EventHeader", 
	   'CosNotification_EventHeader':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_EventType'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_EventType'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_EventType':tc())),
    ?match("IDL:omg.org/CosNotification/EventType:1.0", 
	   'CosNotification_EventType':id()),
    ?match("CosNotification_EventType", 
	   'CosNotification_EventType':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_FixedEventHeader'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_FixedEventHeader'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_FixedEventHeader':tc())),
    ?match("IDL:omg.org/CosNotification/FixedEventHeader:1.0", 
	   'CosNotification_FixedEventHeader':id()),
    ?match("CosNotification_FixedEventHeader", 
	   'CosNotification_FixedEventHeader':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_NamedPropertyRange'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_NamedPropertyRange'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_NamedPropertyRange':tc())),
    ?match("IDL:omg.org/CosNotification/NamedPropertyRange:1.0", 
	   'CosNotification_NamedPropertyRange':id()),
    ?match("CosNotification_NamedPropertyRange", 
	   'CosNotification_NamedPropertyRange':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_Property'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_Property'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_Property':tc())),
    ?match("IDL:omg.org/CosNotification/Property:1.0", 
	   'CosNotification_Property':id()),
    ?match("CosNotification_Property", 
	   'CosNotification_Property':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_PropertyError'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_PropertyError'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_PropertyError':tc())),
    ?match("IDL:omg.org/CosNotification/PropertyError:1.0", 
	   'CosNotification_PropertyError':id()),
    ?match("CosNotification_PropertyError", 
	   'CosNotification_PropertyError':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_PropertyRange'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_PropertyRange'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_PropertyRange':tc())),
    ?match("IDL:omg.org/CosNotification/PropertyRange:1.0", 
	   'CosNotification_PropertyRange':id()),
    ?match("CosNotification_PropertyRange", 
	   'CosNotification_PropertyRange':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_StructuredEvent'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_StructuredEvent'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_StructuredEvent':tc())),
    ?match("IDL:omg.org/CosNotification/StructuredEvent:1.0", 
	   'CosNotification_StructuredEvent':id()),
    ?match("CosNotification_StructuredEvent", 
	   'CosNotification_StructuredEvent':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_UnsupportedAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_UnsupportedAdmin'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_UnsupportedAdmin':tc())),
    ?match("IDL:omg.org/CosNotification/UnsupportedAdmin:1.0", 
	   'CosNotification_UnsupportedAdmin':id()),
    ?match("CosNotification_UnsupportedAdmin", 
	   'CosNotification_UnsupportedAdmin':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_UnsupportedQoS'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_UnsupportedQoS'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_UnsupportedQoS':tc())),
    ?match("IDL:omg.org/CosNotification/UnsupportedQoS:1.0", 
	   'CosNotification_UnsupportedQoS':id()),
    ?match("CosNotification_UnsupportedQoS", 
	   'CosNotification_UnsupportedQoS':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_EventBatch'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_EventBatch'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_EventBatch':tc())),
    ?match("IDL:omg.org/CosNotification/EventBatch:1.0", 
	   'CosNotification_EventBatch':id()),
    ?match("CosNotification_EventBatch", 
	   'CosNotification_EventBatch':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_EventTypeSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_EventTypeSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_EventTypeSeq':tc())),
    ?match("IDL:omg.org/CosNotification/EventTypeSeq:1.0", 
	   'CosNotification_EventTypeSeq':id()),
    ?match("CosNotification_EventTypeSeq", 
	   'CosNotification_EventTypeSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_NamedPropertyRangeSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_NamedPropertyRangeSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_NamedPropertyRangeSeq':tc())),
    ?match("IDL:omg.org/CosNotification/NamedPropertyRangeSeq:1.0", 
	   'CosNotification_NamedPropertyRangeSeq':id()),
    ?match("CosNotification_NamedPropertyRangeSeq", 
	   'CosNotification_NamedPropertyRangeSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_PropertyErrorSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_PropertyErrorSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_PropertyErrorSeq':tc())),
    ?match("IDL:omg.org/CosNotification/PropertyErrorSeq:1.0", 
	   'CosNotification_PropertyErrorSeq':id()),
    ?match("CosNotification_PropertyErrorSeq", 
	   'CosNotification_PropertyErrorSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_PropertySeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_PropertySeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotification_PropertySeq':tc())),
    ?match("IDL:omg.org/CosNotification/PropertySeq:1.0", 
	   'CosNotification_PropertySeq':id()),
    ?match("CosNotification_PropertySeq", 
	   'CosNotification_PropertySeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_AdminLimit'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_AdminLimit'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_AdminLimit':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/AdminLimit:1.0", 
	   'CosNotifyChannelAdmin_AdminLimit':id()),
    ?match("CosNotifyChannelAdmin_AdminLimit", 
	   'CosNotifyChannelAdmin_AdminLimit':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_AdminLimitExceeded'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_AdminLimitExceeded'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_AdminLimitExceeded':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/AdminLimitExceeded:1.0", 
	   'CosNotifyChannelAdmin_AdminLimitExceeded':id()),
    ?match("CosNotifyChannelAdmin_AdminLimitExceeded", 
	   'CosNotifyChannelAdmin_AdminLimitExceeded':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_AdminNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_AdminNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_AdminNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/AdminNotFound:1.0", 
	   'CosNotifyChannelAdmin_AdminNotFound':id()),
    ?match("CosNotifyChannelAdmin_AdminNotFound", 
	   'CosNotifyChannelAdmin_AdminNotFound':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ChannelNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ChannelNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ChannelNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ChannelNotFound:1.0", 
	   'CosNotifyChannelAdmin_ChannelNotFound':id()),
    ?match("CosNotifyChannelAdmin_ChannelNotFound", 
	   'CosNotifyChannelAdmin_ChannelNotFound':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ConnectionAlreadyActive'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ConnectionAlreadyActive'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ConnectionAlreadyActive':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ConnectionAlreadyActive:1.0", 
	   'CosNotifyChannelAdmin_ConnectionAlreadyActive':id()),
    ?match("CosNotifyChannelAdmin_ConnectionAlreadyActive", 
	   'CosNotifyChannelAdmin_ConnectionAlreadyActive':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ConnectionAlreadyInactive'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ConnectionAlreadyInactive'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ConnectionAlreadyInactive':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ConnectionAlreadyInactive:1.0", 
	   'CosNotifyChannelAdmin_ConnectionAlreadyInactive':id()),
    ?match("CosNotifyChannelAdmin_ConnectionAlreadyInactive", 
	   'CosNotifyChannelAdmin_ConnectionAlreadyInactive':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_NotConnected'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_NotConnected'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_NotConnected':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/NotConnected:1.0", 
	   'CosNotifyChannelAdmin_NotConnected':id()),
    ?match("CosNotifyChannelAdmin_NotConnected", 
	   'CosNotifyChannelAdmin_NotConnected':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_AdminIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_AdminIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_AdminIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/AdminIDSeq:1.0", 
	   'CosNotifyChannelAdmin_AdminIDSeq':id()),
    ?match("CosNotifyChannelAdmin_AdminIDSeq", 
	   'CosNotifyChannelAdmin_AdminIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ChannelIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ChannelIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ChannelIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ChannelIDSeq:1.0", 
	   'CosNotifyChannelAdmin_ChannelIDSeq':id()),
    ?match("CosNotifyChannelAdmin_ChannelIDSeq", 
	   'CosNotifyChannelAdmin_ChannelIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ProxyIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyIDSeq:1.0", 
	   'CosNotifyChannelAdmin_ProxyIDSeq':id()),
    ?match("CosNotifyChannelAdmin_ProxyIDSeq", 
	   'CosNotifyChannelAdmin_ProxyIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_CallbackNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_CallbackNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_CallbackNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/CallbackNotFound:1.0", 
	   'CosNotifyFilter_CallbackNotFound':id()),
    ?match("CosNotifyFilter_CallbackNotFound", 
	   'CosNotifyFilter_CallbackNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintExp'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintExp'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintExp':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintExp:1.0", 
	   'CosNotifyFilter_ConstraintExp':id()),
    ?match("CosNotifyFilter_ConstraintExp", 
	   'CosNotifyFilter_ConstraintExp':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintInfo'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintInfo'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintInfo':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintInfo:1.0", 
	   'CosNotifyFilter_ConstraintInfo':id()),
    ?match("CosNotifyFilter_ConstraintInfo", 
	   'CosNotifyFilter_ConstraintInfo':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintNotFound:1.0", 
	   'CosNotifyFilter_ConstraintNotFound':id()),
    ?match("CosNotifyFilter_ConstraintNotFound", 
	   'CosNotifyFilter_ConstraintNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_DuplicateConstraintID'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_DuplicateConstraintID'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_DuplicateConstraintID':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/DuplicateConstraintID:1.0", 
	   'CosNotifyFilter_DuplicateConstraintID':id()),
    ?match("CosNotifyFilter_DuplicateConstraintID", 
	   'CosNotifyFilter_DuplicateConstraintID':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_FilterNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_FilterNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_FilterNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/FilterNotFound:1.0", 
	   'CosNotifyFilter_FilterNotFound':id()),
    ?match("CosNotifyFilter_FilterNotFound", 
	   'CosNotifyFilter_FilterNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_InvalidConstraint'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_InvalidConstraint'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_InvalidConstraint':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/InvalidConstraint:1.0", 
	   'CosNotifyFilter_InvalidConstraint':id()),
    ?match("CosNotifyFilter_InvalidConstraint", 
	   'CosNotifyFilter_InvalidConstraint':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_InvalidGrammar'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_InvalidGrammar'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_InvalidGrammar':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/InvalidGrammar:1.0", 
	   'CosNotifyFilter_InvalidGrammar':id()),
    ?match("CosNotifyFilter_InvalidGrammar", 
	   'CosNotifyFilter_InvalidGrammar':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_InvalidValue'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_InvalidValue'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_InvalidValue':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/InvalidValue:1.0", 
	   'CosNotifyFilter_InvalidValue':id()),
    ?match("CosNotifyFilter_InvalidValue", 
	   'CosNotifyFilter_InvalidValue':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_MappingConstraintInfo'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_MappingConstraintInfo'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_MappingConstraintInfo':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/MappingConstraintInfo:1.0", 
	   'CosNotifyFilter_MappingConstraintInfo':id()),
    ?match("CosNotifyFilter_MappingConstraintInfo", 
	   'CosNotifyFilter_MappingConstraintInfo':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_MappingConstraintPair'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_MappingConstraintPair'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_MappingConstraintPair':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/MappingConstraintPair:1.0", 
	   'CosNotifyFilter_MappingConstraintPair':id()),
    ?match("CosNotifyFilter_MappingConstraintPair", 
	   'CosNotifyFilter_MappingConstraintPair':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_UnsupportedFilterableData'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_UnsupportedFilterableData'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_UnsupportedFilterableData':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/UnsupportedFilterableData:1.0", 
	   'CosNotifyFilter_UnsupportedFilterableData':id()),
    ?match("CosNotifyFilter_UnsupportedFilterableData", 
	   'CosNotifyFilter_UnsupportedFilterableData':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_CallbackIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_CallbackIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_CallbackIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/CallbackIDSeq:1.0", 
	   'CosNotifyFilter_CallbackIDSeq':id()),
    ?match("CosNotifyFilter_CallbackIDSeq", 
	   'CosNotifyFilter_CallbackIDSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintExpSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintExpSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintExpSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintExpSeq:1.0", 
	   'CosNotifyFilter_ConstraintExpSeq':id()),
    ?match("CosNotifyFilter_ConstraintExpSeq", 
	   'CosNotifyFilter_ConstraintExpSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintIDSeq:1.0", 
	   'CosNotifyFilter_ConstraintIDSeq':id()),
    ?match("CosNotifyFilter_ConstraintIDSeq", 
	   'CosNotifyFilter_ConstraintIDSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_ConstraintInfoSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_ConstraintInfoSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_ConstraintInfoSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/ConstraintInfoSeq:1.0", 
	   'CosNotifyFilter_ConstraintInfoSeq':id()),
    ?match("CosNotifyFilter_ConstraintInfoSeq", 
	   'CosNotifyFilter_ConstraintInfoSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_FilterIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_FilterIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_FilterIDSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/FilterIDSeq:1.0", 
	   'CosNotifyFilter_FilterIDSeq':id()),
    ?match("CosNotifyFilter_FilterIDSeq", 
	   'CosNotifyFilter_FilterIDSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_MappingConstraintInfoSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_MappingConstraintInfoSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_MappingConstraintInfoSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/MappingConstraintInfoSeq:1.0", 
	   'CosNotifyFilter_MappingConstraintInfoSeq':id()),
    ?match("CosNotifyFilter_MappingConstraintInfoSeq", 
	   'CosNotifyFilter_MappingConstraintInfoSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_MappingConstraintPairSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_MappingConstraintPairSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyFilter_MappingConstraintPairSeq':tc())),
    ?match("IDL:omg.org/CosNotifyFilter/MappingConstraintPairSeq:1.0", 
	   'CosNotifyFilter_MappingConstraintPairSeq':id()),
    ?match("CosNotifyFilter_MappingConstraintPairSeq", 
	   'CosNotifyFilter_MappingConstraintPairSeq':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_InvalidEventType'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_InvalidEventType'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyComm_InvalidEventType':tc())),
    ?match("IDL:omg.org/CosNotifyComm/InvalidEventType:1.0", 
	   'CosNotifyComm_InvalidEventType':id()),
    ?match("CosNotifyComm_InvalidEventType", 
	   'CosNotifyComm_InvalidEventType':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosNotifyChannelAdmin_ProxyNotFound':tc())),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyNotFound:1.0", 
	   'CosNotifyChannelAdmin_ProxyNotFound':id()),
    ?match("CosNotifyChannelAdmin_ProxyNotFound", 
	   'CosNotifyChannelAdmin_ProxyNotFound':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_AdminPropertiesAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_AdminPropertiesAdmin'(_) ->
    ?nomatch(undefined, 'CosNotification_AdminPropertiesAdmin':oe_tc(get_admin)),
    ?nomatch(undefined, 'CosNotification_AdminPropertiesAdmin':oe_tc(set_admin)),
    ?match(undefined, 'CosNotification_AdminPropertiesAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosNotification_AdminPropertiesAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosNotification/AdminPropertiesAdmin:1.0", 
	   'CosNotification_AdminPropertiesAdmin':typeID()),
    check_tc('CosNotification_AdminPropertiesAdmin':oe_get_interface()),
    ?match(true, 'CosNotification_AdminPropertiesAdmin':oe_is_a('CosNotification_AdminPropertiesAdmin':typeID())),
    ?match(false, 'CosNotification_AdminPropertiesAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotification_QoSAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotification_QoSAdmin'(_) ->
    ?nomatch(undefined, 'CosNotification_QoSAdmin':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotification_QoSAdmin':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotification_QoSAdmin':oe_tc(validate_qos)),
    ?match(undefined, 'CosNotification_QoSAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosNotification_QoSAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosNotification/QoSAdmin:1.0", 
	   'CosNotification_QoSAdmin':typeID()),
    check_tc('CosNotification_QoSAdmin':oe_get_interface()),
    ?match(true, 'CosNotification_QoSAdmin':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(false, 'CosNotification_QoSAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ConsumerAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ConsumerAdmin'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_MyID')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_MyChannel')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_MyOperator')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_pull_suppliers')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc('_get_push_suppliers')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(get_proxy_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(obtain_notification_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(obtain_notification_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(obtain_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(obtain_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ConsumerAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ConsumerAdmin:1.0", 
	   'CosNotifyChannelAdmin_ConsumerAdmin':typeID()),
    check_tc('CosNotifyChannelAdmin_ConsumerAdmin':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('CosNotifyChannelAdmin_ConsumerAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('CosEventChannelAdmin_ConsumerAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ConsumerAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_EventChannel'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_EventChannel'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc('_get_MyFactory')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc('_get_default_consumer_admin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc('_get_default_supplier_admin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc('_get_default_filter_factory')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(new_for_consumers)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(new_for_suppliers)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_consumeradmin)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_supplieradmin)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_all_consumeradmins)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_all_supplieradmins)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(get_admin)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(set_admin)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(for_consumers)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(for_suppliers)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_EventChannel':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_EventChannel':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/EventChannel:1.0", 
	   'CosNotifyChannelAdmin_EventChannel':typeID()),
    check_tc('CosNotifyChannelAdmin_EventChannel':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_EventChannel':oe_is_a('CosNotifyChannelAdmin_EventChannel':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_EventChannel':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_EventChannel':oe_is_a('CosNotification_AdminPropertiesAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_EventChannel':oe_is_a('CosEventChannelAdmin_EventChannel':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_EventChannel':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_EventChannel':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_EventChannelFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_EventChannelFactory'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannelFactory':oe_tc(create_channel)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannelFactory':oe_tc(get_all_channels)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_EventChannelFactory':oe_tc(get_event_channel)),
    ?match(undefined, 'CosNotifyChannelAdmin_EventChannelFactory':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_EventChannelFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/EventChannelFactory:1.0", 
	   'CosNotifyChannelAdmin_EventChannelFactory':typeID()),
    check_tc('CosNotifyChannelAdmin_EventChannelFactory':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_EventChannelFactory':oe_is_a('CosNotifyChannelAdmin_EventChannelFactory':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_EventChannelFactory':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(remove_all_filters)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxyConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxyConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyConsumer:1.0", 
	   'CosNotifyChannelAdmin_ProxyConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxyConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxyConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxyConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyPullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyPullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(connect_any_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(offer_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(disconnect_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(connect_pull_supplier)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyPullConsumer:1.0", 
	   'CosNotifyChannelAdmin_ProxyPullConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxyPullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyPullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotifyComm_PullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosEventComm_PullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a('CosEventChannelAdmin_ProxyPullConsumer':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxyPullConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyPullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyPullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(pull)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(try_pull)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(disconnect_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(connect_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyPullSupplier:1.0", 
	   'CosNotifyChannelAdmin_ProxyPullSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxyPullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_ProxyPullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotifyComm_PullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosEventComm_PullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('CosEventChannelAdmin_ProxyPullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxyPullSupplier':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyPushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyPushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(connect_any_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(offer_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(push)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(disconnect_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(connect_push_supplier)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyPushConsumer:1.0", 
	   'CosNotifyChannelAdmin_ProxyPushConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxyPushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyPushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotifyComm_PushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosEventComm_PushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a('CosEventChannelAdmin_ProxyPushConsumer':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxyPushConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxyPushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxyPushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(connect_any_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(disconnect_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(connect_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxyPushSupplier:1.0", 
	   'CosNotifyChannelAdmin_ProxyPushSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxyPushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_ProxyPushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotifyComm_PushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosEventComm_PushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('CosEventChannelAdmin_ProxyPushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxyPushSupplier':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_ProxySupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_ProxySupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(remove_all_filters)),
    ?match(undefined, 'CosNotifyChannelAdmin_ProxySupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_ProxySupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/ProxySupplier:1.0", 
	   'CosNotifyChannelAdmin_ProxySupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_ProxySupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_ProxySupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxySupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_ProxySupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_ProxySupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_SequenceProxyPullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_SequenceProxyPullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(connect_sequence_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(disconnect_sequence_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/SequenceProxyPullConsumer:1.0", 
	   'CosNotifyChannelAdmin_SequenceProxyPullConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_SequenceProxyPullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotifyComm_SequencePullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_SequenceProxyPullConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_SequenceProxyPullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_SequenceProxyPullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(connect_sequence_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(pull_structured_events)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(try_pull_structured_events)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(disconnect_sequence_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/SequenceProxyPullSupplier:1.0", 
	   'CosNotifyChannelAdmin_SequenceProxyPullSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_SequenceProxyPullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotifyComm_SequencePullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_SequenceProxyPullSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_SequenceProxyPushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_SequenceProxyPushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(connect_sequence_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(push_structured_events)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(disconnect_sequence_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/SequenceProxyPushConsumer:1.0", 
	   'CosNotifyChannelAdmin_SequenceProxyPushConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_SequenceProxyPushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotifyComm_SequencePushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_SequenceProxyPushConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_SequenceProxyPushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_SequenceProxyPushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(connect_sequence_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(disconnect_sequence_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/SequenceProxyPushSupplier:1.0", 
	   'CosNotifyChannelAdmin_SequenceProxyPushSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_SequenceProxyPushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotifyComm_SequencePushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_SequenceProxyPushSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_StructuredProxyPullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_StructuredProxyPullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(connect_structured_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(disconnect_structured_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/StructuredProxyPullConsumer:1.0", 
	   'CosNotifyChannelAdmin_StructuredProxyPullConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_StructuredProxyPullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotifyComm_StructuredPullConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_StructuredProxyPullConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_StructuredProxyPullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_StructuredProxyPullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(connect_structured_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(pull_structured_event)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(try_pull_structured_event)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(disconnect_structured_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/StructuredProxyPullSupplier:1.0", 
	   'CosNotifyChannelAdmin_StructuredProxyPullSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_StructuredProxyPullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotifyComm_StructuredPullSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_StructuredProxyPullSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_StructuredProxyPushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_StructuredProxyPushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(connect_structured_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(obtain_subscription_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(push_structured_event)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(disconnect_structured_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/StructuredProxyPushConsumer:1.0", 
	   'CosNotifyChannelAdmin_StructuredProxyPushConsumer':typeID()),
    check_tc('CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_StructuredProxyPushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotifyChannelAdmin_ProxyConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotifyComm_StructuredPushConsumer':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_StructuredProxyPushConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_StructuredProxyPushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_StructuredProxyPushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(connect_structured_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(suspend_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(resume_connection)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_get_MyType')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_get_MyAdmin')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_get_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_set_priority_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_get_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc('_set_lifetime_filter')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(obtain_offered_types)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(validate_event_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(disconnect_structured_push_supplier)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/StructuredProxyPushSupplier:1.0", 
	   'CosNotifyChannelAdmin_StructuredProxyPushSupplier':typeID()),
    check_tc('CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_StructuredProxyPushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotifyChannelAdmin_ProxySupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotifyComm_StructuredPushSupplier':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_StructuredProxyPushSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyChannelAdmin_SupplierAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyChannelAdmin_SupplierAdmin'(_) ->
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc('_get_MyID')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc('_get_MyChannel')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc('_get_MyOperator')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc('_get_pull_consumers')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc('_get_push_consumers')),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(get_proxy_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(obtain_notification_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(obtain_notification_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(offer_change)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(remove_all_filters)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(obtain_push_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(obtain_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(callSeq)),
    ?nomatch(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(callAny)),
    ?match(undefined, 'CosNotifyChannelAdmin_SupplierAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyChannelAdmin_SupplierAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyChannelAdmin/SupplierAdmin:1.0", 
	   'CosNotifyChannelAdmin_SupplierAdmin':typeID()),
    check_tc('CosNotifyChannelAdmin_SupplierAdmin':oe_get_interface()),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('CosNotifyChannelAdmin_SupplierAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('CosEventChannelAdmin_SupplierAdmin':typeID())),
    ?match(true, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'CosNotifyChannelAdmin_SupplierAdmin':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_Filter'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_Filter'(_) ->
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc('_get_constraint_grammar')),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(add_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(modify_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(get_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(get_all_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(remove_all_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(match)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(match_structured)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(match_typed)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(attach_callback)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(detach_callback)),
    ?nomatch(undefined, 'CosNotifyFilter_Filter':oe_tc(get_callbacks)),
    ?match(undefined, 'CosNotifyFilter_Filter':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyFilter_Filter':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyFilter/Filter:1.0", 
	   'CosNotifyFilter_Filter':typeID()),
    check_tc('CosNotifyFilter_Filter':oe_get_interface()),
    ?match(true, 'CosNotifyFilter_Filter':oe_is_a('CosNotifyFilter_Filter':typeID())),
    ?match(false, 'CosNotifyFilter_Filter':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_FilterAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_FilterAdmin'(_) ->
    ?nomatch(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(add_filter)),
    ?nomatch(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(remove_filter)),
    ?nomatch(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(get_filter)),
    ?nomatch(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(get_all_filters)),
    ?nomatch(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(remove_all_filters)),
    ?match(undefined, 'CosNotifyFilter_FilterAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyFilter_FilterAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyFilter/FilterAdmin:1.0", 
	   'CosNotifyFilter_FilterAdmin':typeID()),
    check_tc('CosNotifyFilter_FilterAdmin':oe_get_interface()),
    ?match(true, 'CosNotifyFilter_FilterAdmin':oe_is_a('CosNotifyFilter_FilterAdmin':typeID())),
    ?match(false, 'CosNotifyFilter_FilterAdmin':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_FilterFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_FilterFactory'(_) ->
    ?nomatch(undefined, 'CosNotifyFilter_FilterFactory':oe_tc(create_filter)),
    ?nomatch(undefined, 'CosNotifyFilter_FilterFactory':oe_tc(create_mapping_filter)),
    ?match(undefined, 'CosNotifyFilter_FilterFactory':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyFilter_FilterFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyFilter/FilterFactory:1.0", 
	   'CosNotifyFilter_FilterFactory':typeID()),
    check_tc('CosNotifyFilter_FilterFactory':oe_get_interface()),
    ?match(true, 'CosNotifyFilter_FilterFactory':oe_is_a('CosNotifyFilter_FilterFactory':typeID())),
    ?match(false, 'CosNotifyFilter_FilterFactory':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyFilter_MappingFilter'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyFilter_MappingFilter'(_) ->
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc('_get_constraint_grammar')),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc('_get_value_type')),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc('_get_default_value')),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(add_mapping_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(modify_mapping_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(get_mapping_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(get_all_mapping_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(remove_all_mapping_constraints)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(destroy)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(match)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(match_structured)),
    ?nomatch(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(match_typed)),
    ?match(undefined, 'CosNotifyFilter_MappingFilter':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyFilter_MappingFilter':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyFilter/MappingFilter:1.0", 
	   'CosNotifyFilter_MappingFilter':typeID()),
    check_tc('CosNotifyFilter_MappingFilter':oe_get_interface()),
    ?match(true, 'CosNotifyFilter_MappingFilter':oe_is_a('CosNotifyFilter_MappingFilter':typeID())),
    ?match(false, 'CosNotifyFilter_MappingFilter':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_NotifyPublish'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_NotifyPublish'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_NotifyPublish':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyComm_NotifyPublish':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_NotifyPublish':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/NotifyPublish:1.0", 
	   'CosNotifyComm_NotifyPublish':typeID()),
    check_tc('CosNotifyComm_NotifyPublish':oe_get_interface()),
    ?match(true, 'CosNotifyComm_NotifyPublish':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyComm_NotifyPublish':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_NotifySubscribe'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_NotifySubscribe'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_NotifySubscribe':oe_tc(subscription_change)),
    ?match(undefined, 'CosNotifyComm_NotifySubscribe':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_NotifySubscribe':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/NotifySubscribe:1.0", 
	   'CosNotifyComm_NotifySubscribe':typeID()),
    check_tc('CosNotifyComm_NotifySubscribe':oe_get_interface()),
    ?match(true, 'CosNotifyComm_NotifySubscribe':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(false, 'CosNotifyComm_NotifySubscribe':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_PullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_PullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_PullConsumer':oe_tc(offer_change)),
    ?nomatch(undefined, 'CosNotifyComm_PullConsumer':oe_tc(disconnect_pull_consumer)),
    ?match(undefined, 'CosNotifyComm_PullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_PullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/PullConsumer:1.0", 
	   'CosNotifyComm_PullConsumer':typeID()),
    check_tc('CosNotifyComm_PullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_PullConsumer':oe_is_a('CosNotifyComm_PullConsumer':typeID())),
    ?match(true, 'CosNotifyComm_PullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(true, 'CosNotifyComm_PullConsumer':oe_is_a('CosEventComm_PullConsumer':typeID())),
    ?match(false, 'CosNotifyComm_PullConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_PullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_PullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_PullSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyComm_PullSupplier':oe_tc(pull)),
    ?nomatch(undefined, 'CosNotifyComm_PullSupplier':oe_tc(try_pull)),
    ?nomatch(undefined, 'CosNotifyComm_PullSupplier':oe_tc(disconnect_pull_supplier)),
    ?match(undefined, 'CosNotifyComm_PullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_PullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/PullSupplier:1.0", 
	   'CosNotifyComm_PullSupplier':typeID()),
    check_tc('CosNotifyComm_PullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_PullSupplier':oe_is_a('CosNotifyComm_PullSupplier':typeID())),
    ?match(true, 'CosNotifyComm_PullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyComm_PullSupplier':oe_is_a('CosEventComm_PullSupplier':typeID())),
    ?match(false, 'CosNotifyComm_PullSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_PushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_PushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_PushConsumer':oe_tc(offer_change)),
    ?nomatch(undefined, 'CosNotifyComm_PushConsumer':oe_tc(push)),
    ?nomatch(undefined, 'CosNotifyComm_PushConsumer':oe_tc(disconnect_push_consumer)),
    ?match(undefined, 'CosNotifyComm_PushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_PushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/PushConsumer:1.0", 
	   'CosNotifyComm_PushConsumer':typeID()),
    check_tc('CosNotifyComm_PushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_PushConsumer':oe_is_a('CosNotifyComm_PushConsumer':typeID())),
    ?match(true, 'CosNotifyComm_PushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(true, 'CosNotifyComm_PushConsumer':oe_is_a('CosEventComm_PushConsumer':typeID())),
    ?match(false, 'CosNotifyComm_PushConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_PushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_PushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_PushSupplier':oe_tc(subscription_change)),
    ?nomatch(undefined, 'CosNotifyComm_PushSupplier':oe_tc(disconnect_push_supplier)),
    ?match(undefined, 'CosNotifyComm_PushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_PushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/PushSupplier:1.0", 
	   'CosNotifyComm_PushSupplier':typeID()),
    check_tc('CosNotifyComm_PushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_PushSupplier':oe_is_a('CosNotifyComm_PushSupplier':typeID())),
    ?match(true, 'CosNotifyComm_PushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(true, 'CosNotifyComm_PushSupplier':oe_is_a('CosEventComm_PushSupplier':typeID())),
    ?match(false, 'CosNotifyComm_PushSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_SequencePullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_SequencePullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_SequencePullConsumer':oe_tc(disconnect_sequence_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePullConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyComm_SequencePullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_SequencePullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/SequencePullConsumer:1.0", 
	   'CosNotifyComm_SequencePullConsumer':typeID()),
    check_tc('CosNotifyComm_SequencePullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_SequencePullConsumer':oe_is_a('CosNotifyComm_SequencePullConsumer':typeID())),
    ?match(true, 'CosNotifyComm_SequencePullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyComm_SequencePullConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_SequencePullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_SequencePullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_SequencePullSupplier':oe_tc(pull_structured_events)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePullSupplier':oe_tc(try_pull_structured_events)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePullSupplier':oe_tc(disconnect_sequence_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePullSupplier':oe_tc(subscription_change)),
    ?match(undefined, 'CosNotifyComm_SequencePullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_SequencePullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/SequencePullSupplier:1.0", 
	   'CosNotifyComm_SequencePullSupplier':typeID()),
    check_tc('CosNotifyComm_SequencePullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_SequencePullSupplier':oe_is_a('CosNotifyComm_SequencePullSupplier':typeID())),
    ?match(true, 'CosNotifyComm_SequencePullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(false, 'CosNotifyComm_SequencePullSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_SequencePushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_SequencePushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_SequencePushConsumer':oe_tc(push_structured_events)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePushConsumer':oe_tc(disconnect_sequence_push_consumer)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePushConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyComm_SequencePushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_SequencePushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/SequencePushConsumer:1.0", 
	   'CosNotifyComm_SequencePushConsumer':typeID()),
    check_tc('CosNotifyComm_SequencePushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_SequencePushConsumer':oe_is_a('CosNotifyComm_SequencePushConsumer':typeID())),
    ?match(true, 'CosNotifyComm_SequencePushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyComm_SequencePushConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_SequencePushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_SequencePushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_SequencePushSupplier':oe_tc(disconnect_sequence_push_supplier)),
    ?nomatch(undefined, 'CosNotifyComm_SequencePushSupplier':oe_tc(subscription_change)),
    ?match(undefined, 'CosNotifyComm_SequencePushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_SequencePushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/SequencePushSupplier:1.0", 
	   'CosNotifyComm_SequencePushSupplier':typeID()),
    check_tc('CosNotifyComm_SequencePushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_SequencePushSupplier':oe_is_a('CosNotifyComm_SequencePushSupplier':typeID())),
    ?match(true, 'CosNotifyComm_SequencePushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(false, 'CosNotifyComm_SequencePushSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_StructuredPullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_StructuredPullConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullConsumer':oe_tc(disconnect_structured_pull_consumer)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyComm_StructuredPullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_StructuredPullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/StructuredPullConsumer:1.0", 
	   'CosNotifyComm_StructuredPullConsumer':typeID()),
    check_tc('CosNotifyComm_StructuredPullConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_StructuredPullConsumer':oe_is_a('CosNotifyComm_StructuredPullConsumer':typeID())),
    ?match(true, 'CosNotifyComm_StructuredPullConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyComm_StructuredPullConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_StructuredPullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_StructuredPullSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullSupplier':oe_tc(pull_structured_event)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullSupplier':oe_tc(try_pull_structured_event)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullSupplier':oe_tc(disconnect_structured_pull_supplier)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPullSupplier':oe_tc(subscription_change)),
    ?match(undefined, 'CosNotifyComm_StructuredPullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_StructuredPullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/StructuredPullSupplier:1.0", 
	   'CosNotifyComm_StructuredPullSupplier':typeID()),
    check_tc('CosNotifyComm_StructuredPullSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_StructuredPullSupplier':oe_is_a('CosNotifyComm_StructuredPullSupplier':typeID())),
    ?match(true, 'CosNotifyComm_StructuredPullSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(false, 'CosNotifyComm_StructuredPullSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_StructuredPushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_StructuredPushConsumer'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_StructuredPushConsumer':oe_tc(push_structured_event)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPushConsumer':oe_tc(disconnect_structured_push_consumer)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPushConsumer':oe_tc(offer_change)),
    ?match(undefined, 'CosNotifyComm_StructuredPushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_StructuredPushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/StructuredPushConsumer:1.0", 
	   'CosNotifyComm_StructuredPushConsumer':typeID()),
    check_tc('CosNotifyComm_StructuredPushConsumer':oe_get_interface()),
    ?match(true, 'CosNotifyComm_StructuredPushConsumer':oe_is_a('CosNotifyComm_StructuredPushConsumer':typeID())),
    ?match(true, 'CosNotifyComm_StructuredPushConsumer':oe_is_a('CosNotifyComm_NotifyPublish':typeID())),
    ?match(false, 'CosNotifyComm_StructuredPushConsumer':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosNotifyComm_StructuredPushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosNotifyComm_StructuredPushSupplier'(_) ->
    ?nomatch(undefined, 'CosNotifyComm_StructuredPushSupplier':oe_tc(disconnect_structured_push_supplier)),
    ?nomatch(undefined, 'CosNotifyComm_StructuredPushSupplier':oe_tc(subscription_change)),
    ?match(undefined, 'CosNotifyComm_StructuredPushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosNotifyComm_StructuredPushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosNotifyComm/StructuredPushSupplier:1.0", 
	   'CosNotifyComm_StructuredPushSupplier':typeID()),
    check_tc('CosNotifyComm_StructuredPushSupplier':oe_get_interface()),
    ?match(true, 'CosNotifyComm_StructuredPushSupplier':oe_is_a('CosNotifyComm_StructuredPushSupplier':typeID())),
    ?match(true, 'CosNotifyComm_StructuredPushSupplier':oe_is_a('CosNotifyComm_NotifySubscribe':typeID())),
    ?match(false, 'CosNotifyComm_StructuredPushSupplier':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'oe_CosNotificationComm_Event'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosNotificationComm_Event'(_) ->
    ?nomatch(undefined, 'oe_CosNotificationComm_Event':oe_tc(callSeq)),
    ?nomatch(undefined, 'oe_CosNotificationComm_Event':oe_tc(callAny)),
    ?match(undefined, 'oe_CosNotificationComm_Event':oe_tc(undefined)),
    ?match([_|_], 'oe_CosNotificationComm_Event':oe_get_interface()),
    ?match("IDL:oe_CosNotificationComm/Event:1.0", 
	   'oe_CosNotificationComm_Event':typeID()),
    check_tc('oe_CosNotificationComm_Event':oe_get_interface()),
    ?match(true, 'oe_CosNotificationComm_Event':oe_is_a('oe_CosNotificationComm_Event':typeID())),
    ?match(false, 'oe_CosNotificationComm_Event':oe_is_a("wrong")),
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
    
    
