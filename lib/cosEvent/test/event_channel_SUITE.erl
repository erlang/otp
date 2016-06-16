%%-----------------------------------------------------------------
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

-module(event_channel_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").

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
	 event_objects_api/1, events_api/1, events_sync_api/1, 
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
    [events_api, events_sync_api, event_objects_api,
     app_test].

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
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    mnesia:delete_schema([node()]),
    mnesia:create_schema([node()]),
    orber:install([node()]),
    application:start(mnesia),
    application:start(orber),
    cosEventApp:install(),
    cosEventApp:start(),
    oe_event_test_server:oe_register(),
    Config.

end_per_suite(Config) when is_list(Config) ->
    oe_event_test_server:oe_unregister(),
    cosEventApp:stop(),
    cosEventApp:uninstall(),
    application:stop(orber),
    application:stop(mnesia),
    mnesia:delete_schema([node()]),
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Config.

%%-----------------------------------------------------------------
%%  Tests app file
%%-----------------------------------------------------------------
app_test(doc) -> [];
app_test(suite) -> [];
app_test(_Config) ->
    ok=test_server:app_test(cosEvent),
    ok.



%% Testing the CosEvent API to setup a complete service
event_objects_api(_Config) ->

    Ch = ?match({_,key,_,_,_,_}, cosEventApp:start_channel([{typecheck, true},
							    {pull_interval, 300}])),

    AC=?match({_,key,_,_,_,_},
	      'CosEventChannelAdmin_EventChannel':for_consumers(Ch)),
    AS=?match({_,key,_,_,_,_},
	      'CosEventChannelAdmin_EventChannel':for_suppliers(Ch)),

    PPushS=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_ConsumerAdmin':obtain_push_supplier(AC)),
    PPullS=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_ConsumerAdmin':obtain_pull_supplier(AC)),
    
    PPushC=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_SupplierAdmin':obtain_push_consumer(AS)),
    PPullC=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_SupplierAdmin':obtain_pull_consumer(AS)),
    
    PushC=?match({_,key,_,_,_,_},
		    'event_test_PushC':oe_create([])),
    PullC=?match({_,key,_,_,_,_},
		    'event_test_PullC':oe_create(PPullC)),
    
    PushS=?match({_,key,_,_,_,_},
		    'event_test_PushS':oe_create(PPushC)),

    PullS=?match({_,key,_,_,_,_},
		    'event_test_PullS':oe_create([])),
    
    NIL = corba:create_nil_objref(),

    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PPushS, NIL)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PPushS, PullS)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PPushS, PushC)),
    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
	   'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PPushS, PushC)),

    ?match(ok, 'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PPullS, NIL)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PPullS, PullS)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PPullS, PullC)),
    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
	   'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PPullS, PullC)),

    ?match(ok, 'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PPushC, NIL)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PPushC, PullS)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PPushC, PushS)),
    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
	   'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PPushC, PushS)),

    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PPullC, NIL)),
    ?match({'EXCEPTION',{'BAD_PARAM',_,_,_}},
	   'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PPullC, PushS)),    
    ?match(ok, 'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PPullC, PullS)),
    ?match({'EXCEPTION',{'CosEventChannelAdmin_AlreadyConnected',_}},
	   'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PPullC, PullS)),


    catch corba:dispose(AC),
    %% Wait a couple of seconds to be sure all data removed from DB.
    timer:sleep(2000),

    %% Since we terminated ConsumerAdmin only the Supplier Proxies should be terminated.
    ?match(true, corba_object:non_existent(AC)),
    ?match(true, corba_object:non_existent(PPushS)),
    ?match(true, corba_object:non_existent(PPullS)),

    ?match(false, corba_object:non_existent(Ch)),
    ?match(false, corba_object:non_existent(AS)),
    ?match(false, corba_object:non_existent(PPullC)),
    ?match(false, corba_object:non_existent(PPushC)),

    %% Terminate a proxy and check that its admin is unaffected.
    catch corba:dispose(PPullC),
    timer:sleep(2000),
    ?match(false, corba_object:non_existent(AS)),
    ?match(true, corba_object:non_existent(PPullC)),

    catch corba:dispose(Ch),
    timer:sleep(2000),

    ?match(true, corba_object:non_existent(Ch)),
    ?match(true, corba_object:non_existent(AS)),
    ?match(true, corba_object:non_existent(PPullC)),
    ?match(true, corba_object:non_existent(PPushC)),
    
    %% The client should be notified; wait for a couple of seconds and check it.
    timer:sleep(2000),
    ?match(true, corba_object:non_existent(PushC)),
    ?match(true, corba_object:non_existent(PullC)),
    ?match(true, corba_object:non_existent(PushS)),
    ?match(true, corba_object:non_existent(PullS)),

    ok.

%% Testing the CosEvent API for sending events asynchronous
events_api(_Config) ->

    Ch = ?match({_,key,_,_,_,_}, cosEventApp:start_channel([{typecheck, true},
							    {pull_interval, 2},
							    {blocking, false}])),
    event_sender(Ch).


%% Testing the CosEvent API for sending events synchronous
events_sync_api(_Config) ->

    Ch = ?match({_,key,_,_,_,_}, cosEventApp:start_channel([{typecheck, true},
							    {pull_interval, 2},
							    {blocking, true}])),
    event_sender(Ch).

event_sender(Ch) ->
    Event1 = #any{typecode=tk_long, value = 1},
    Event2 = #any{typecode=tk_long, value = 2},
    Event3 = #any{typecode=tk_long, value = 3},
    Event4 = #any{typecode=tk_long, value = 4},
    Event5 = #any{typecode=tk_long, value = 5},
    Event6 = #any{typecode=tk_long, value = 6},
    
    AC=?match({_,key,_,_,_,_},
	      'CosEventChannelAdmin_EventChannel':for_consumers(Ch)),
    AS=?match({_,key,_,_,_,_},
	      'CosEventChannelAdmin_EventChannel':for_suppliers(Ch)),

    PPushS=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_ConsumerAdmin':obtain_push_supplier(AC)),
    PPullS=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_ConsumerAdmin':obtain_pull_supplier(AC)),
    
    PPushC=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_SupplierAdmin':obtain_push_consumer(AS)),
    PPullC=?match({_,key,_,_,_,_},
		  'CosEventChannelAdmin_SupplierAdmin':obtain_pull_consumer(AS)),

    PushC=?match({_,key,_,_,_,_}, 'event_test_PushC':oe_create([])),
    PullC=?match({_,key,_,_,_,_}, 'event_test_PullC':oe_create(PPullS)),
    
    PushS=?match({_,key,_,_,_,_}, 'event_test_PushS':oe_create(PPushC)),

    PullS=?match({_,key,_,_,_,_}, 'event_test_PullS':oe_create([])),
    
    ?match(ok, 'CosEventChannelAdmin_ProxyPushSupplier':connect_push_consumer(PPushS, PushC)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPullSupplier':connect_pull_consumer(PPullS, PullC)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPushConsumer':connect_push_supplier(PPushC, PushS)),
    ?match(ok, 'CosEventChannelAdmin_ProxyPullConsumer':connect_pull_supplier(PPullC, PullS)),

    %% No events should be available at the consumer side at this point.
    ?match({_, false}, event_test_PullC:do_try_pull(PullC)),
    ?match([], event_test_PushC:get_data(PushC)),

    %% Push an event and wait to be sure it have reached the destination.
    ?match(ok, event_test_PushS:do_push(PushS, Event1)),
    ?match(ok, event_test_PushS:do_push(PushS, Event2)),
    ?match(ok, event_test_PushS:do_push(PushS, Event3)),
    timer:sleep(2000),
    ?match({Event1, true}, event_test_PullC:do_try_pull(PullC)),
    ?match({Event2, true}, event_test_PullC:do_try_pull(PullC)),
    ?match({Event3, true}, event_test_PullC:do_try_pull(PullC)),
    ?match({_, false}, event_test_PullC:do_try_pull(PullC)),
    ?match([Event1, Event2, Event3], event_test_PushC:get_data(PushC)),

    ?match(ok, event_test_PullS:add_event(PullS, Event4)),
    ?match(ok, event_test_PullS:add_event(PullS, Event5)),
    ?match(ok, event_test_PullS:add_event(PullS, Event6)),

    %% Since the pull operation is blocking we do not need to "sleep".
    %% The ProxyPullConsumer will pull for events according to the pull_interval
    %% parameter given when started the channel.
    ?match(Event4, event_test_PullC:do_pull(PullC)),
    ?match(Event5, event_test_PullC:do_pull(PullC)),
    ?match(Event6, event_test_PullC:do_pull(PullC)),

    timer:sleep(2000),
    ?match([Event4, Event5, Event6], event_test_PushC:get_data(PushC)),
   
    
    catch corba:dispose(Ch),
    %% The client should be notified; wait for a couple of seconds and check it.
    timer:sleep(2000),
    ?match(true, corba_object:non_existent(PushC)),
    ?match(true, corba_object:non_existent(PullC)),
    ?match(true, corba_object:non_existent(PushS)),
    ?match(true, corba_object:non_existent(PullS)),

    ok.
