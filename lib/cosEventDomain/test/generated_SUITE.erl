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
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, init_per_group/2,end_per_group/2]).

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
    ['CosEventDomainAdmin',
     'CosEventDomainAdmin_DiamondSeq',
     'CosEventDomainAdmin_AlreadyExists',
     'CosEventDomainAdmin_DomainIDSeq',
     'CosEventDomainAdmin_Connection',
     'CosEventDomainAdmin_ConnectionIDSeq',
     'CosEventDomainAdmin_ConnectionNotFound',
     'CosEventDomainAdmin_CycleCreationForbidden',
     'CosEventDomainAdmin_CycleSeq',
     'CosEventDomainAdmin_DiamondCreationForbidden',
     'CosEventDomainAdmin_DomainNotFound',
     'CosEventDomainAdmin_MemberIDSeq',
     'CosEventDomainAdmin_RouteSeq',
     'CosEventDomainAdmin_EventDomainFactory',
     'CosEventDomainAdmin_EventDomain'].

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
%% Test Case: 'CosEventDomainAdmin'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin'(_) ->
    ?match("CycleDetection", 'CosEventDomainAdmin':'CycleDetection'()),
    ?match(0, 'CosEventDomainAdmin':'AuthorizeCycles'()),
    ?match(1, 'CosEventDomainAdmin':'ForbidCycles'()),
    ?match("DiamondDetection", 'CosEventDomainAdmin':'DiamondDetection'()),
    ?match(0, 'CosEventDomainAdmin':'AuthorizeDiamonds'()),
    ?match(1, 'CosEventDomainAdmin':'ForbidDiamonds'()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_DiamondSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_DiamondSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_DiamondSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/DiamondSeq:1.0", 
	   'CosEventDomainAdmin_DiamondSeq':id()),
    ?match("CosEventDomainAdmin_DiamondSeq", 
	   'CosEventDomainAdmin_DiamondSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_AlreadyExists'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_AlreadyExists'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_AlreadyExists':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/AlreadyExists:1.0", 
	   'CosEventDomainAdmin_AlreadyExists':id()),
    ?match("CosEventDomainAdmin_AlreadyExists", 
	   'CosEventDomainAdmin_AlreadyExists':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_DomainIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_DomainIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_DomainIDSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/DomainIDSeq:1.0", 
	   'CosEventDomainAdmin_DomainIDSeq':id()),
    ?match("CosEventDomainAdmin_DomainIDSeq", 
	   'CosEventDomainAdmin_DomainIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_Connection'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_Connection'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_Connection':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/Connection:1.0", 
	   'CosEventDomainAdmin_Connection':id()),
    ?match("CosEventDomainAdmin_Connection", 
	   'CosEventDomainAdmin_Connection':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_ConnectionIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_ConnectionIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_ConnectionIDSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/ConnectionIDSeq:1.0", 
	   'CosEventDomainAdmin_ConnectionIDSeq':id()),
    ?match("CosEventDomainAdmin_ConnectionIDSeq", 
	   'CosEventDomainAdmin_ConnectionIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_ConnectionNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_ConnectionNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_ConnectionNotFound':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/ConnectionNotFound:1.0", 
	   'CosEventDomainAdmin_ConnectionNotFound':id()),
    ?match("CosEventDomainAdmin_ConnectionNotFound", 
	   'CosEventDomainAdmin_ConnectionNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_CycleCreationForbidden'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_CycleCreationForbidden'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_CycleCreationForbidden':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/CycleCreationForbidden:1.0", 
	   'CosEventDomainAdmin_CycleCreationForbidden':id()),
    ?match("CosEventDomainAdmin_CycleCreationForbidden", 
	   'CosEventDomainAdmin_CycleCreationForbidden':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_CycleSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_CycleSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_CycleSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/CycleSeq:1.0", 
	   'CosEventDomainAdmin_CycleSeq':id()),
    ?match("CosEventDomainAdmin_CycleSeq", 
	   'CosEventDomainAdmin_CycleSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_DiamondCreationForbidden'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_DiamondCreationForbidden'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_DiamondCreationForbidden':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/DiamondCreationForbidden:1.0", 
	   'CosEventDomainAdmin_DiamondCreationForbidden':id()),
    ?match("CosEventDomainAdmin_DiamondCreationForbidden", 
	   'CosEventDomainAdmin_DiamondCreationForbidden':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_DomainNotFound'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_DomainNotFound'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_DomainNotFound':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/DomainNotFound:1.0", 
	   'CosEventDomainAdmin_DomainNotFound':id()),
    ?match("CosEventDomainAdmin_DomainNotFound", 
	   'CosEventDomainAdmin_DomainNotFound':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_MemberIDSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_MemberIDSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_MemberIDSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/MemberIDSeq:1.0", 
	   'CosEventDomainAdmin_MemberIDSeq':id()),
    ?match("CosEventDomainAdmin_MemberIDSeq", 
	   'CosEventDomainAdmin_MemberIDSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_RouteSeq'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_RouteSeq'(_) ->
    ?match(true, orber_tc:check_tc('CosEventDomainAdmin_RouteSeq':tc())),
    ?match("IDL:omg.org/CosEventDomainAdmin/RouteSeq:1.0", 
	   'CosEventDomainAdmin_RouteSeq':id()),
    ?match("CosEventDomainAdmin_RouteSeq", 
	   'CosEventDomainAdmin_RouteSeq':name()),
    ok.

%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_EventDomainFactory'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_EventDomainFactory'(_) ->
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomainFactory':oe_tc(create_event_domain)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomainFactory':oe_tc(get_all_domains)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomainFactory':oe_tc(get_event_domain)),
    ?match(undefined, 'CosEventDomainAdmin_EventDomainFactory':oe_tc(undefined)),
    ?match([_|_], 'CosEventDomainAdmin_EventDomainFactory':oe_get_interface()),
    ?match("IDL:omg.org/CosEventDomainAdmin/EventDomainFactory:1.0", 
	   'CosEventDomainAdmin_EventDomainFactory':typeID()),
    check_tc('CosEventDomainAdmin_EventDomainFactory':oe_get_interface()),
    ?match(true, 'CosEventDomainAdmin_EventDomainFactory':oe_is_a('CosEventDomainAdmin_EventDomainFactory':typeID())),
    ?match(false, 'CosEventDomainAdmin_EventDomainFactory':oe_is_a("wrong")),
    ok.


%%-----------------------------------------------------------------
%% Test Case: 'CosEventDomainAdmin_EventDomain'
%% Description: 
%%-----------------------------------------------------------------
'CosEventDomainAdmin_EventDomain'(_) ->
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(add_channel)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_all_channels)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_channel)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(remove_channel)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(add_connection)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_all_connections)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_connection)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(remove_connection)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_offer_channels)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_subscription_channels)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(destroy)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_cycles)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_diamonds)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(set_default_consumer_channel)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(set_default_supplier_channel)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_push_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_pull_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_push_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_pull_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_push_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_pull_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_push_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_pull_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_push_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_pull_consumer)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_push_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_pull_supplier)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_push_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_pull_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_push_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_pull_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_push_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_pull_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_push_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_structured_pull_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_push_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_pull_consumer_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_push_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(connect_sequence_pull_supplier_with_id)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_qos)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(set_qos)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(validate_qos)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(get_admin)),
    ?nomatch(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(set_admin)),
    ?match(undefined, 'CosEventDomainAdmin_EventDomain':oe_tc(undefined)),
    ?match([_|_], 'CosEventDomainAdmin_EventDomain':oe_get_interface()),
    ?match("IDL:omg.org/CosEventDomainAdmin/EventDomain:1.0", 
	   'CosEventDomainAdmin_EventDomain':typeID()),
    check_tc('CosEventDomainAdmin_EventDomain':oe_get_interface()),
    ?match(true, 'CosEventDomainAdmin_EventDomain':oe_is_a('CosEventDomainAdmin_EventDomain':typeID())),
    ?match(true, 'CosEventDomainAdmin_EventDomain':oe_is_a('CosNotification_QoSAdmin':typeID())),
    ?match(true, 'CosEventDomainAdmin_EventDomain':oe_is_a('CosNotification_AdminPropertiesAdmin':typeID())),
    ?match(false, 'CosEventDomainAdmin_EventDomain':oe_is_a("wrong")),
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
    
    
