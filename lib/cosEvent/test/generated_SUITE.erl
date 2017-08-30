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
    ['CosEventChannelAdmin_AlreadyConnected',
     'CosEventChannelAdmin_TypeError',
     'CosEventComm_Disconnected',
     'CosEventChannelAdmin_ConsumerAdmin',
     'CosEventChannelAdmin_EventChannel',
     'CosEventChannelAdmin_ProxyPullConsumer',
     'CosEventChannelAdmin_ProxyPullSupplier',
     'CosEventChannelAdmin_ProxyPushConsumer',
     'CosEventChannelAdmin_ProxyPushSupplier',
     'CosEventChannelAdmin_SupplierAdmin',
     oe_CosEventComm_CAdmin, oe_CosEventComm_Channel,
     oe_CosEventComm_Event, oe_CosEventComm_PullerS,
     oe_CosEventComm_PusherS, 'CosEventComm_PullConsumer',
     'CosEventComm_PullSupplier',
     'CosEventComm_PushConsumer',
     'CosEventComm_PushSupplier'].

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
%% Test Case: 'CosEventChannelAdmin_AlreadyConnected'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_AlreadyConnected'(_) ->
    ?match(true, orber_tc:check_tc('CosEventChannelAdmin_AlreadyConnected':tc())),
    ?match("IDL:omg.org/CosEventChannelAdmin/AlreadyConnected:1.0", 
	   'CosEventChannelAdmin_AlreadyConnected':id()),
    ?match("CosEventChannelAdmin_AlreadyConnected", 
	   'CosEventChannelAdmin_AlreadyConnected':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_TypeError'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_TypeError'(_) ->
    ?match(true, orber_tc:check_tc('CosEventChannelAdmin_TypeError':tc())),
    ?match("IDL:omg.org/CosEventChannelAdmin/TypeError:1.0", 
	   'CosEventChannelAdmin_TypeError':id()),
    ?match("CosEventChannelAdmin_TypeError", 
	   'CosEventChannelAdmin_TypeError':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosEventComm_Disconnected'
%% Description: 
%%-----------------------------------------------------------------
'CosEventComm_Disconnected'(_) ->
    ?match(true, orber_tc:check_tc('CosEventComm_Disconnected':tc())),
    ?match("IDL:omg.org/CosEventComm/Disconnected:1.0", 
	   'CosEventComm_Disconnected':id()),
    ?match("CosEventComm_Disconnected", 'CosEventComm_Disconnected':name()),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_ConsumerAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_ConsumerAdmin'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_ConsumerAdmin':oe_tc(obtain_push_supplier)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ConsumerAdmin':oe_tc(obtain_pull_supplier)),
    ?match(undefined, 'CosEventChannelAdmin_ConsumerAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_ConsumerAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/ConsumerAdmin:1.0", 
	   'CosEventChannelAdmin_ConsumerAdmin':typeID()),
    check_tc('CosEventChannelAdmin_ConsumerAdmin':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_ConsumerAdmin':oe_is_a('CosEventChannelAdmin_ConsumerAdmin':typeID())),
    ?match(false, 'CosEventChannelAdmin_ConsumerAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_EventChannel'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_EventChannel'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_EventChannel':oe_tc(for_consumers)),
    ?nomatch(undefined, 'CosEventChannelAdmin_EventChannel':oe_tc(for_suppliers)),
    ?nomatch(undefined, 'CosEventChannelAdmin_EventChannel':oe_tc(destroy)),
    ?match(undefined, 'CosEventChannelAdmin_EventChannel':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_EventChannel':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/EventChannel:1.0", 
	   'CosEventChannelAdmin_EventChannel':typeID()),
    check_tc('CosEventChannelAdmin_EventChannel':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_EventChannel':oe_is_a('CosEventChannelAdmin_EventChannel':typeID())),
    ?match(false, 'CosEventChannelAdmin_EventChannel':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_ProxyPullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_ProxyPullConsumer'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullConsumer':oe_tc(connect_pull_supplier)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullConsumer':oe_tc(disconnect_pull_consumer)),
    ?match(undefined, 'CosEventChannelAdmin_ProxyPullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_ProxyPullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/ProxyPullConsumer:1.0", 
	   'CosEventChannelAdmin_ProxyPullConsumer':typeID()),
    check_tc('CosEventChannelAdmin_ProxyPullConsumer':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_ProxyPullConsumer':oe_is_a('CosEventChannelAdmin_ProxyPullConsumer':typeID())),
    ?match(true, 'CosEventChannelAdmin_ProxyPullConsumer':oe_is_a('CosEventComm_PullConsumer':typeID())),
    ?match(false, 'CosEventChannelAdmin_ProxyPullConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_ProxyPullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_ProxyPullSupplier'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullSupplier':oe_tc(connect_pull_consumer)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullSupplier':oe_tc(pull)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullSupplier':oe_tc(try_pull)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPullSupplier':oe_tc(disconnect_pull_supplier)),
    ?match(undefined, 'CosEventChannelAdmin_ProxyPullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_ProxyPullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/ProxyPullSupplier:1.0", 
	   'CosEventChannelAdmin_ProxyPullSupplier':typeID()),
    check_tc('CosEventChannelAdmin_ProxyPullSupplier':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_ProxyPullSupplier':oe_is_a('CosEventChannelAdmin_ProxyPullSupplier':typeID())),
    ?match(true, 'CosEventChannelAdmin_ProxyPullSupplier':oe_is_a('CosEventComm_PullSupplier':typeID())),
    ?match(false, 'CosEventChannelAdmin_ProxyPullSupplier':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_ProxyPushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_ProxyPushConsumer'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPushConsumer':oe_tc(connect_push_supplier)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPushConsumer':oe_tc(push)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPushConsumer':oe_tc(disconnect_push_consumer)),
    ?match(undefined, 'CosEventChannelAdmin_ProxyPushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_ProxyPushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/ProxyPushConsumer:1.0", 
	   'CosEventChannelAdmin_ProxyPushConsumer':typeID()),
    check_tc('CosEventChannelAdmin_ProxyPushConsumer':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_ProxyPushConsumer':oe_is_a('CosEventChannelAdmin_ProxyPushConsumer':typeID())),
    ?match(true, 'CosEventChannelAdmin_ProxyPushConsumer':oe_is_a('CosEventComm_PushConsumer':typeID())),
    ?match(false, 'CosEventChannelAdmin_ProxyPushConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_ProxyPushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_ProxyPushSupplier'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPushSupplier':oe_tc(connect_push_consumer)),
    ?nomatch(undefined, 'CosEventChannelAdmin_ProxyPushSupplier':oe_tc(disconnect_push_supplier)),
    ?match(undefined, 'CosEventChannelAdmin_ProxyPushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_ProxyPushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/ProxyPushSupplier:1.0", 
	   'CosEventChannelAdmin_ProxyPushSupplier':typeID()),
    check_tc('CosEventChannelAdmin_ProxyPushSupplier':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_ProxyPushSupplier':oe_is_a('CosEventChannelAdmin_ProxyPushSupplier':typeID())),
    ?match(true, 'CosEventChannelAdmin_ProxyPushSupplier':oe_is_a('CosEventComm_PushSupplier':typeID())),
    ?match(false, 'CosEventChannelAdmin_ProxyPushSupplier':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventChannelAdmin_SupplierAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosEventChannelAdmin_SupplierAdmin'(_) ->
    ?nomatch(undefined, 'CosEventChannelAdmin_SupplierAdmin':oe_tc(obtain_push_consumer)),
    ?nomatch(undefined, 'CosEventChannelAdmin_SupplierAdmin':oe_tc(obtain_pull_consumer)),
    ?match(undefined, 'CosEventChannelAdmin_SupplierAdmin':oe_tc(undefined)),
    ?match([_|_], 'CosEventChannelAdmin_SupplierAdmin':oe_get_interface()),
    ?match("IDL:omg.org/CosEventChannelAdmin/SupplierAdmin:1.0", 
	   'CosEventChannelAdmin_SupplierAdmin':typeID()),
    check_tc('CosEventChannelAdmin_SupplierAdmin':oe_get_interface()),
    ?match(true, 'CosEventChannelAdmin_SupplierAdmin':oe_is_a('CosEventChannelAdmin_SupplierAdmin':typeID())),
    ?match(false, 'CosEventChannelAdmin_SupplierAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'oe_CosEventComm_CAdmin'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosEventComm_CAdmin'(_) ->
    ?nomatch(undefined, 'oe_CosEventComm_CAdmin':oe_tc(obtain_push_supplier)),
    ?nomatch(undefined, 'oe_CosEventComm_CAdmin':oe_tc(obtain_pull_supplier)),
    ?nomatch(undefined, 'oe_CosEventComm_CAdmin':oe_tc(send)),
    ?nomatch(undefined, 'oe_CosEventComm_CAdmin':oe_tc(send_sync)),
    ?match(undefined, 'oe_CosEventComm_CAdmin':oe_tc(undefined)),
    ?match([_|_], 'oe_CosEventComm_CAdmin':oe_get_interface()),
    ?match("IDL:oe_CosEventComm/CAdmin:1.0", 
	   'oe_CosEventComm_CAdmin':typeID()),
    check_tc('oe_CosEventComm_CAdmin':oe_get_interface()),
    ?match(true, 'oe_CosEventComm_CAdmin':oe_is_a('oe_CosEventComm_CAdmin':typeID())),
    ?match(true, 'oe_CosEventComm_CAdmin':oe_is_a('CosEventChannelAdmin_ConsumerAdmin':typeID())),
    ?match(true, 'oe_CosEventComm_CAdmin':oe_is_a('oe_CosEventComm_Event':typeID())),
    ?match(false, 'oe_CosEventComm_CAdmin':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'oe_CosEventComm_Channel'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosEventComm_Channel'(_) ->
    ?nomatch(undefined, 'oe_CosEventComm_Channel':oe_tc(for_consumers)),
    ?nomatch(undefined, 'oe_CosEventComm_Channel':oe_tc(for_suppliers)),
    ?nomatch(undefined, 'oe_CosEventComm_Channel':oe_tc(destroy)),
    ?nomatch(undefined, 'oe_CosEventComm_Channel':oe_tc(send)),
    ?nomatch(undefined, 'oe_CosEventComm_Channel':oe_tc(send_sync)),
    ?match(undefined, 'oe_CosEventComm_Channel':oe_tc(undefined)),
    ?match([_|_], 'oe_CosEventComm_Channel':oe_get_interface()),
    ?match("IDL:oe_CosEventComm/Channel:1.0", 
	   'oe_CosEventComm_Channel':typeID()),
    check_tc('oe_CosEventComm_Channel':oe_get_interface()),
    ?match(true, 'oe_CosEventComm_Channel':oe_is_a('oe_CosEventComm_Channel':typeID())),
    ?match(true, 'oe_CosEventComm_Channel':oe_is_a('CosEventChannelAdmin_EventChannel':typeID())),
    ?match(true, 'oe_CosEventComm_Channel':oe_is_a('oe_CosEventComm_Event':typeID())),
    ?match(false, 'oe_CosEventComm_Channel':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'oe_CosEventComm_Event'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosEventComm_Event'(_) ->
    ?nomatch(undefined, 'oe_CosEventComm_Event':oe_tc(send)),
    ?nomatch(undefined, 'oe_CosEventComm_Event':oe_tc(send_sync)),
    ?match(undefined, 'oe_CosEventComm_Event':oe_tc(undefined)),
    ?match([_|_], 'oe_CosEventComm_Event':oe_get_interface()),
    ?match("IDL:oe_CosEventComm/Event:1.0", 
	   'oe_CosEventComm_Event':typeID()),
    check_tc('oe_CosEventComm_Event':oe_get_interface()),
    ?match(true, 'oe_CosEventComm_Event':oe_is_a('oe_CosEventComm_Event':typeID())),
    ?match(false, 'oe_CosEventComm_Event':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'oe_CosEventComm_PullerS'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosEventComm_PullerS'(_) ->
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(connect_pull_consumer)),
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(pull)),
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(try_pull)),
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(disconnect_pull_supplier)),
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(send)),
    ?nomatch(undefined, 'oe_CosEventComm_PullerS':oe_tc(send_sync)),
    ?match(undefined, 'oe_CosEventComm_PullerS':oe_tc(undefined)),
    ?match([_|_], 'oe_CosEventComm_PullerS':oe_get_interface()),
    ?match("IDL:oe_CosEventComm/PullerS:1.0", 
	   'oe_CosEventComm_PullerS':typeID()),
    check_tc('oe_CosEventComm_PullerS':oe_get_interface()),
    ?match(true, 'oe_CosEventComm_PullerS':oe_is_a('oe_CosEventComm_PullerS':typeID())),
    ?match(true, 'oe_CosEventComm_PullerS':oe_is_a('CosEventChannelAdmin_ProxyPullSupplier':typeID())),
    ?match(true, 'oe_CosEventComm_PullerS':oe_is_a('CosEventComm_PullSupplier':typeID())),
    ?match(true, 'oe_CosEventComm_PullerS':oe_is_a('oe_CosEventComm_Event':typeID())),
    ?match(false, 'oe_CosEventComm_PullerS':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'oe_CosEventComm_PusherS'
%% Description: 
%%-----------------------------------------------------------------
'oe_CosEventComm_PusherS'(_) ->
    ?nomatch(undefined, 'oe_CosEventComm_PusherS':oe_tc(connect_push_consumer)),
    ?nomatch(undefined, 'oe_CosEventComm_PusherS':oe_tc(disconnect_push_supplier)),
    ?nomatch(undefined, 'oe_CosEventComm_PusherS':oe_tc(send)),
    ?nomatch(undefined, 'oe_CosEventComm_PusherS':oe_tc(send_sync)),
    ?match(undefined, 'oe_CosEventComm_PusherS':oe_tc(undefined)),
    ?match([_|_], 'oe_CosEventComm_PusherS':oe_get_interface()),
    ?match("IDL:oe_CosEventComm/PusherS:1.0", 
	   'oe_CosEventComm_PusherS':typeID()),
    check_tc('oe_CosEventComm_PusherS':oe_get_interface()),
    ?match(true, 'oe_CosEventComm_PusherS':oe_is_a('oe_CosEventComm_PusherS':typeID())),
    ?match(true, 'oe_CosEventComm_PusherS':oe_is_a('CosEventChannelAdmin_ProxyPushSupplier':typeID())),
    ?match(true, 'oe_CosEventComm_PusherS':oe_is_a('CosEventComm_PushSupplier':typeID())),
    ?match(true, 'oe_CosEventComm_PusherS':oe_is_a('oe_CosEventComm_Event':typeID())),
    ?match(false, 'oe_CosEventComm_PusherS':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventComm_PullConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosEventComm_PullConsumer'(_) ->
    ?nomatch(undefined, 'CosEventComm_PullConsumer':oe_tc(disconnect_pull_consumer)),
    ?match(undefined, 'CosEventComm_PullConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosEventComm_PullConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosEventComm/PullConsumer:1.0", 
	   'CosEventComm_PullConsumer':typeID()),
    check_tc('CosEventComm_PullConsumer':oe_get_interface()),
    ?match(true, 'CosEventComm_PullConsumer':oe_is_a('CosEventComm_PullConsumer':typeID())),
    ?match(false, 'CosEventComm_PullConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventComm_PullSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosEventComm_PullSupplier'(_) ->
    ?nomatch(undefined, 'CosEventComm_PullSupplier':oe_tc(pull)),
    ?nomatch(undefined, 'CosEventComm_PullSupplier':oe_tc(try_pull)),
    ?nomatch(undefined, 'CosEventComm_PullSupplier':oe_tc(disconnect_pull_supplier)),
    ?match(undefined, 'CosEventComm_PullSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosEventComm_PullSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosEventComm/PullSupplier:1.0", 
	   'CosEventComm_PullSupplier':typeID()),
    check_tc('CosEventComm_PullSupplier':oe_get_interface()),
    ?match(true, 'CosEventComm_PullSupplier':oe_is_a('CosEventComm_PullSupplier':typeID())),
    ?match(false, 'CosEventComm_PullSupplier':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventComm_PushConsumer'
%% Description: 
%%-----------------------------------------------------------------
'CosEventComm_PushConsumer'(_) ->
    ?nomatch(undefined, 'CosEventComm_PushConsumer':oe_tc(push)),
    ?nomatch(undefined, 'CosEventComm_PushConsumer':oe_tc(disconnect_push_consumer)),
    ?match(undefined, 'CosEventComm_PushConsumer':oe_tc(undefined)),
    ?match([_|_], 'CosEventComm_PushConsumer':oe_get_interface()),
    ?match("IDL:omg.org/CosEventComm/PushConsumer:1.0", 
	   'CosEventComm_PushConsumer':typeID()),
    check_tc('CosEventComm_PushConsumer':oe_get_interface()),
    ?match(true, 'CosEventComm_PushConsumer':oe_is_a('CosEventComm_PushConsumer':typeID())),
    ?match(false, 'CosEventComm_PushConsumer':oe_is_a("wrong")),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventComm_PushSupplier'
%% Description: 
%%-----------------------------------------------------------------
'CosEventComm_PushSupplier'(_) ->
    ?nomatch(undefined, 'CosEventComm_PushSupplier':oe_tc(disconnect_push_supplier)),
    ?match(undefined, 'CosEventComm_PushSupplier':oe_tc(undefined)),
    ?match([_|_], 'CosEventComm_PushSupplier':oe_get_interface()),
    ?match("IDL:omg.org/CosEventComm/PushSupplier:1.0", 
	   'CosEventComm_PushSupplier':typeID()),
    check_tc('CosEventComm_PushSupplier':oe_get_interface()),
    ?match(true, 'CosEventComm_PushSupplier':oe_is_a('CosEventComm_PushSupplier':typeID())),
    ?match(false, 'CosEventComm_PushSupplier':oe_is_a("wrong")),
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
    
    
