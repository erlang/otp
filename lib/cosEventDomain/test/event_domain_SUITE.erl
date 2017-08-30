%%-----------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2016. All Rights Reserved.
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

-module(event_domain_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("cosNotification/include/CosNotifyChannelAdmin.hrl").
-include_lib("cosNotification/include/CosNotification.hrl").

-include_lib("cosEventDomain/include/CosEventDomainAdmin.hrl").
-include_lib("cosEventDomain/src/cosEventDomainApp.hrl").

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------

-define(default_timeout, test_server:minutes(5)).


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


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, 
	 event_domain_api/1, event_domain_factory_api/1,
	 cases/0, init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2, app_test/1]).

%%-----------------------------------------------------------------
%% Internal exports
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
    [event_domain_api, event_domain_factory_api, app_test].

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

init_per_suite(Config) when is_list(Config) ->
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    ok = corba:orb_init([{flags, 16#02}, 
			 {orber_debug_level, 10}]),
    orber:install([node()]),
    application:start(mnesia),
    application:start(orber),
    cosEventApp:install(),
    cosEventApp:start(),
    cosNotificationApp:install(),
    cosNotificationApp:start(),
    cosEventDomainApp:install(),
    cosEventDomainApp:start(),
    Config.

end_per_suite(Config) when is_list(Config) ->
    cosEventDomainApp:stop(),
    cosEventDomainApp:uninstall(),
    cosNotificationApp:stop(),
    cosNotificationApp:uninstall(),
    cosEventApp:stop(),
    cosEventApp:uninstall(),
    application:stop(orber),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    Config.

%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(_Config) ->
    ok=test_server:app_test(cosEventDomain),
    ok.


%% Testing the CosEventDomain Domain API
event_domain_api(_Config) ->

    %% We will setup a cluster looking like:
    %%       7-8--->
    %%      /
    %% 2 - 4     6->
    %%      \   /
    %% 5---9-1-3
    
    %% 2-4
    %% 4-1
    %% 1-3
    %% 3-6
    %% 5-9
    %% 9-1
    %% 4-7
    %% 7-8
   

    ChFac = ?match({_,key,_,_,_,_},
		   cosNotificationApp:start_global_factory([{pullInterval,1}])),
    {Ch0,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    Fac = ?match({_,key,_,_,_,_},
		 cosEventDomainApp:start_factory()),
    {ED, _} = ?match({{_,key,_,_,_,_}, _},
		     'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [], [])),
    ID0 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch0),
    ?match(Ch0, 'CosEventDomainAdmin_EventDomain':get_channel(ED, ID0)),
    ?match([0], 'CosEventDomainAdmin_EventDomain':get_all_channels(ED)),
    ?match({'EXCEPTION',{'CosNotifyChannelAdmin_ChannelNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':get_channel(ED, 100)),
    ?match({'EXCEPTION',{'CosNotifyChannelAdmin_ChannelNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':remove_channel(ED, 100)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_channel(ED, 0)),
    ?match([], 'CosEventDomainAdmin_EventDomain':get_all_channels(ED)),
    ?match({'EXCEPTION',{'CosNotifyChannelAdmin_ChannelNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':remove_channel(ED, 0)),

    %% Create a new event channel.
    {Ch1,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch2,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch3,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch4,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch5,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch6,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch7,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch8,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
    {Ch9,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosNotifyChannelAdmin_EventChannelFactory':create_channel(ChFac, [], [])),
 
    ID1 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch1),
    ID2 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch2),
    ID3 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch3),
    ID4 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch4),
    ID5 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch5),
    ID6 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch6),
    ID7 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch7),
    ID8 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch8),
    ID9 = 'CosEventDomainAdmin_EventDomain':add_channel(ED, Ch9),
    ?match([_,_,_,_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_channels(ED)),

    ?match([], 'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    C1 = #'CosEventDomainAdmin_Connection'{supplier_id=ID2, 
					   consumer_id=ID4,
					   ctype='STRUCTURED_EVENT',
					   notification_style='Pull'},
    C2 = #'CosEventDomainAdmin_Connection'{supplier_id=ID4, 
					   consumer_id=ID1,
					   ctype='ANY_EVENT',
					   notification_style='Push'},
    C3 = #'CosEventDomainAdmin_Connection'{supplier_id=ID1, 
					   consumer_id=ID3,
					   ctype='ANY_EVENT',
					   notification_style='Pull'},
    C4 = #'CosEventDomainAdmin_Connection'{supplier_id=ID3, 
					   consumer_id=ID6,
					   ctype='STRUCTURED_EVENT',
					   notification_style='Push'},
    C5 = #'CosEventDomainAdmin_Connection'{supplier_id=ID5, 
					   consumer_id=ID9,
					   ctype='ANY_EVENT',
					   notification_style='Pull'},
    C6 = #'CosEventDomainAdmin_Connection'{supplier_id=ID9, 
					   consumer_id=ID1,
					   ctype='ANY_EVENT',
					   notification_style='Push'},
    C7 = #'CosEventDomainAdmin_Connection'{supplier_id=ID4, 
					   consumer_id=ID7,
					   ctype='STRUCTURED_EVENT',
					   notification_style='Pull'},
    C8 = #'CosEventDomainAdmin_Connection'{supplier_id=ID7, 
					   consumer_id=ID8,
					   ctype='ANY_EVENT',
					   notification_style='Push'},
    C9 = #'CosEventDomainAdmin_Connection'{supplier_id=ID8, 
					   consumer_id=ID4,
					   ctype='ANY_EVENT',
					   notification_style='Pull'},
    C10 = #'CosEventDomainAdmin_Connection'{supplier_id=ID5, 
					    consumer_id=ID4,
					    ctype='ANY_EVENT',
					    notification_style='Pull'},
    C11 = #'CosEventDomainAdmin_Connection'{supplier_id=ID4, 
					    consumer_id=ID6,
					    ctype='ANY_EVENT',
					    notification_style='Pull'},
    C12 = #'CosEventDomainAdmin_Connection'{supplier_id=ID8, 
					    consumer_id=ID6,
					    ctype='ANY_EVENT',
					    notification_style='Pull'},

    CID1 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C1),
    ?match([CID1], 'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID2 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C2),
    ?match([_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID3 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C3),
    ?match([_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID4 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C4),
    ?match([_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID5 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C5),
    ?match([_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID6 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C6),
    ?match([_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    CID7 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C7),
    ?match([_,_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    _CID8 = 'CosEventDomainAdmin_EventDomain':add_connection(ED, C8),
    ?match([_,_,_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),

    ?match({'EXCEPTION',{'CosEventDomainAdmin_AlreadyExists', _}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C8)),
    %% No cycles should exist.
    ?match([], 'CosEventDomainAdmin_EventDomain':get_cycles(ED)),

    ?match([_, _], 'CosEventDomainAdmin_EventDomain':get_qos(ED)),
    AllowCyclic = #'CosNotification_Property'{name=?CycleDetection, 
					      value=any:create(orber_tc:short(), 
							       ?AuthorizeCycles)},
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosEventDomainAdmin_EventDomain':set_qos(ED, [AllowCyclic])),
    ForbidCyclic = #'CosNotification_Property'{name=?CycleDetection, 
					      value=any:create(orber_tc:short(), 
							       ?ForbidCycles)},
    %% The same as before; must work.
    ?match(ok, 'CosEventDomainAdmin_EventDomain':set_qos(ED, [ForbidCyclic])),

    AllowDiamonds = #'CosNotification_Property'{name=?DiamondDetection, 
						value=any:create(orber_tc:short(), 
								 ?AuthorizeDiamonds)},
    %% Since no diamonds allowed before this is always ok.
    ?match(ok, 'CosEventDomainAdmin_EventDomain':set_qos(ED, [AllowDiamonds])),
    
    ?match([_, _], 'CosEventDomainAdmin_EventDomain':get_qos(ED)),

    ForbidDiamonds = #'CosNotification_Property'{name=?DiamondDetection, 
						 value=any:create(orber_tc:short(), 
								  ?ForbidDiamonds)},
    %% No diamonds created before. Hence, will work.
    ?match(ok, 'CosEventDomainAdmin_EventDomain':set_qos(ED, [ForbidDiamonds])),

    ?match([_, _], 'CosEventDomainAdmin_EventDomain':get_qos(ED)),
    
    ?match({ok, [_]}, 'CosEventDomainAdmin_EventDomain':validate_qos(ED, 
								     [ForbidDiamonds, 
								      ForbidCyclic])),
    %% No diamonds exists, hence, this is ok.
    ?match({ok, [_]}, 'CosEventDomainAdmin_EventDomain':validate_qos(ED, 
								     [AllowDiamonds, 
								      ForbidCyclic])),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosEventDomainAdmin_EventDomain':validate_qos(ED, [ForbidDiamonds, 
							       AllowCyclic])),

    %% Since the ED is started is asyclic we may not succeed with this invokation.
    ?match({'EXCEPTION',{'CosEventDomainAdmin_CycleCreationForbidden',_,_}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C9)),
    ?match([], 'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, ID2)),

    ?match([2], 'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, ID4)),
    ?match([_,_,_], 'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, ID8)),
    ?match({'EXCEPTION',{'CosNotifyChannelAdmin_ChannelNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, 100)),
    ?match([], 'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, ID8)),
    ?match([_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, ID4)),
    ?match([_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, ID2)),
    ?match({'EXCEPTION',{'CosNotifyChannelAdmin_ChannelNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, 100)),
    Nil = corba:create_nil_objref(),

    P2=?match({_,key,_,_,_,_},
	      'CosEventDomainAdmin_EventDomain':connect_push_supplier_with_id(ED, Nil, ID2)),
    P7=?match({_,key,_,_,_,_},
	      'CosEventDomainAdmin_EventDomain':connect_push_supplier_with_id(ED, Nil, ID7)),
    P8=?match({_,key,_,_,_,_},
	      'CosEventDomainAdmin_EventDomain':connect_pull_consumer_with_id(ED, Nil, ID8)),
    P6=?match({_,key,_,_,_,_},
	      'CosEventDomainAdmin_EventDomain':connect_pull_consumer_with_id(ED, Nil, ID6)),
    E1 = #any{typecode=tk_long, value=1},
    E2 = #any{typecode=tk_long, value=2},
    
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPushConsumer':push(P2, E1)),
    ?match(E1, 'CosNotifyChannelAdmin_ProxyPullSupplier':pull(P8)),
    ?match(E1, 'CosNotifyChannelAdmin_ProxyPullSupplier':pull(P6)),
    ?match(ok, 'CosNotifyChannelAdmin_ProxyPushConsumer':push(P7, E2)),
    ?match(E2, 'CosNotifyChannelAdmin_ProxyPullSupplier':pull(P8)),
    timer:sleep(10000),
    ?match({_,false}, 'CosNotifyChannelAdmin_ProxyPullSupplier':try_pull(P6)),

    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID7)),

    ?match({'EXCEPTION',{'CosEventDomainAdmin_ConnectionNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID7)),

    ?match({'EXCEPTION',{'CosEventDomainAdmin_ConnectionNotFound',_}},
	   'CosEventDomainAdmin_EventDomain':remove_connection(ED, 100)),

    ?match([], 'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, ID7)),
    ?match([2], 'CosEventDomainAdmin_EventDomain':get_offer_channels(ED, ID4)),

    ?match([8], 'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, ID7)),
    ?match([_,_,_], 'CosEventDomainAdmin_EventDomain':get_subscription_channels(ED, ID4)),

    CID10 = ?match(8, 'CosEventDomainAdmin_EventDomain':add_connection(ED, C7)),

    %% Now we'll check diamond management.
    %% Currently it should not be possible to create a diamond (due to QoS-setting).
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DiamondCreationForbidden',_,_}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C11)),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DiamondCreationForbidden',_,_}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C10)),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DiamondCreationForbidden',_,_}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C12)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':set_qos(ED, [AllowDiamonds])),
    
    CID11 = ?match(9, 'CosEventDomainAdmin_EventDomain':add_connection(ED, C10)),
    ?match([_,_,_,_,_,_,_,_,_], 
	   'CosEventDomainAdmin_EventDomain':get_all_connections(ED)),
    ?match([_], 'CosEventDomainAdmin_EventDomain':get_diamonds(ED)),

    CID12 = ?match(10, 'CosEventDomainAdmin_EventDomain':add_connection(ED, C11)),
    ?match([_, _, _], 'CosEventDomainAdmin_EventDomain':get_diamonds(ED)),

    CID13 = ?match(11, 'CosEventDomainAdmin_EventDomain':add_connection(ED, C12)),

    ?match([_, _, _], 'CosEventDomainAdmin_EventDomain':get_diamonds(ED)),

    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosEventDomainAdmin_EventDomain':set_qos(ED, [ForbidDiamonds])),

    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID10)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID11)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID12)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':remove_connection(ED, CID13)),
    ?match(ok, 'CosEventDomainAdmin_EventDomain':set_qos(ED, [ForbidDiamonds])),
    ?match([_, _], 'CosEventDomainAdmin_EventDomain':get_qos(ED)),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DiamondCreationForbidden',_,_}},
	   'CosEventDomainAdmin_EventDomain':add_connection(ED, C10)),

    ?match(ok, 'CosEventDomainAdmin_EventDomain':destroy(ED)),

    ok.

%% Testing the CosEventDomain Factory API
event_domain_factory_api(_Config) ->

    Cyclic = #'CosNotification_Property'{name=?CycleDetection, 
					 value=any:create(orber_tc:short(), 
							  ?ForbidCycles)},

    BadProp = #'CosNotification_Property'{name="Wrong", 
					  value=any:create(orber_tc:short(), 
							   ?ForbidCycles)},

    BadQoSVal = #'CosNotification_Property'{name=?CycleDetection, 
					    value=any:create(orber_tc:short(), 
							     10)},

    Fac = ?match({_,key,_,_,_,_},
		 cosEventDomainApp:start_factory()),
    ?match([], 'CosEventDomainAdmin_EventDomainFactory':get_all_domains(Fac)),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DomainNotFound',_}},
	   'CosEventDomainAdmin_EventDomainFactory':get_event_domain(Fac, 0)),
    {ED,_} = 'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [Cyclic], []),
    ?match([0], 'CosEventDomainAdmin_EventDomainFactory':get_all_domains(Fac)),
    ED = 'CosEventDomainAdmin_EventDomainFactory':get_event_domain(Fac, 0),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DomainNotFound',_}},
	   'CosEventDomainAdmin_EventDomainFactory':get_event_domain(Fac, 1)),
    corba:dispose(ED),
    timer:sleep(3000),
    ?match([], 'CosEventDomainAdmin_EventDomainFactory':get_all_domains(Fac)),
    ?match({'EXCEPTION',{'CosEventDomainAdmin_DomainNotFound',_}},
	   'CosEventDomainAdmin_EventDomainFactory':get_event_domain(Fac, 0)),
    {ED2,_} = ?match({{_,key,_,_,_,_}, _},
		     'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [], [])),
    ?match([1], 'CosEventDomainAdmin_EventDomainFactory':get_all_domains(Fac)),
    ?match(ED2, 'CosEventDomainAdmin_EventDomainFactory':get_event_domain(Fac, 1)),
    corba:dispose(ED2),

    ?match({'EXCEPTION', {'CosNotification_UnsupportedQoS',_,_}},
	   'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [BadProp], [])),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedAdmin',_,_}},
	   'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [], [BadProp])),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedQoS',_,_}},
	   'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [BadQoSVal], [])),
    ?match({'EXCEPTION',{'CosNotification_UnsupportedAdmin',_,_}},
	   'CosEventDomainAdmin_EventDomainFactory':create_event_domain(Fac, [], [BadQoSVal])),

    corba:dispose(Fac),
    ok.
