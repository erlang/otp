%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2004-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%

-module(orber_firewall_ipv6_in_SUITE).

-include_lib("test_server/include/test_server.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include("idl_output/orber_test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContextExt.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").

-define(default_timeout, ?t:minutes(15)).

-define(match(ExpectedRes,Expr),
	fun() ->
		AcTuAlReS = (catch (Expr)),
		case AcTuAlReS of
		    ExpectedRes ->
			io:format("------ CORRECT RESULT ------~n~p~n",
				 [AcTuAlReS]),
			AcTuAlReS;
		    _ ->
			io:format("###### ERROR ERROR ######~nRESULT:  ~p~n",
				  [AcTuAlReS]),
			?line exit(AcTuAlReS)
		end
	end()).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0, 
	 init_per_suite/1, end_per_suite/1, 
	 init_per_testcase/2, end_per_testcase/2,  
	 deny_port_api/1, deny_port_range_api/1, deny_host_api/1, 
	 deny_peerhost_api/1, allow_port_range_api/1,
	 allow_host_api/1, allow_peerhost_api/1, check_address_api/1]).

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


%% NOTE - the fragment test cases must bu first since we explicitly set a request
%% id. Otherwise, the request-id counter would be increased and we cannot know
%% what it is.
cases() -> 
    [deny_port_api, deny_port_range_api, deny_host_api,
     deny_peerhost_api, allow_port_range_api, allow_host_api,
     allow_peerhost_api, check_address_api].


init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    orber:jump_start([{iiop_port, 0},
		      {iiop_out_ports, {5980, 6000}},
		      {flags, ?ORB_ENV_USE_IPV6}]),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    orber:jump_stop(),
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    case orber_test_lib:version_ok() of
	true ->
	    if
		is_list(Config) ->
		    Config;
		true ->
		    exit("Config not a list")
	    end;
	Reason ->
	    Reason
    end.

end_per_suite(Config) ->
    Config.


%%-----------------------------------------------------------------
%%  Incomming connections - Deny
%%-----------------------------------------------------------------
deny_port_api(doc) -> ["Deny Access due to invalid local port"];
deny_port_api(suite) -> [];
deny_port_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128#7000"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}, 
	   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
						%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.

deny_port_range_api(doc) -> ["Deny Access due to invalid local port range"];
deny_port_range_api(suite) -> [];
deny_port_range_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128#7000/8000"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}, 
	   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.


deny_host_api(doc) -> ["Deny Access due to invalid host"];
deny_host_api(suite) -> [];
deny_host_api(_Config) ->
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, "0:0:0:0:0:0:10.1.1.1/128"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}, 
	   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.

deny_peerhost_api(doc) -> ["Deny Access due to invalid peer host"];
deny_peerhost_api(suite) -> [];
deny_peerhost_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, 
	       orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
						?ORB_ENV_USE_ACL_INCOMING)},
				       {iiop_acl, [{tcp_in, IP++"/128", ["0:0:0:0:0:0:10.1.1.1"]}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}}, 
	   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.

%%-----------------------------------------------------------------
%%  Incomming connections - Allow
%%-----------------------------------------------------------------
allow_port_range_api(doc) -> ["Allow Access due to valid local port range"];
allow_port_range_api(suite) -> [];
allow_port_range_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128#5980/6000"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = 
	?match({'IOP_IOR',_,_},
	       corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match(false, corba_object:not_existent(IOR)),
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.


allow_host_api(doc) -> ["Allow Access due to valid host"];
allow_host_api(suite) -> [];
allow_host_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR =
	?match({'IOP_IOR',_,_}, 
	       corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match(false, corba_object:not_existent(IOR)),

%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.

allow_peerhost_api(doc) -> ["Allow Access due to valid host"];
allow_peerhost_api(suite) -> [];
allow_peerhost_api(_Config) ->
    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128", [IP]}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = 
	?match({'IOP_IOR',_,_}, 
	       corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService",
				      [#'IOP_ServiceContext'
				       {context_id=?ORBER_GENERIC_CTX_ID, 
					context_data = {interface, IP}}])),
    ?match(false, corba_object:not_existent(IOR,
					    [#'IOP_ServiceContext'
					     {context_id=?ORBER_GENERIC_CTX_ID, 
					      context_data = {interface, IP}}])),
    
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.

%%-----------------------------------------------------------------
%%  Test corbaloc strings
%%-----------------------------------------------------------------
check_address_api(doc) -> ["Test corbaloc strings"];
check_address_api(suite) -> [];
check_address_api(_Config) ->
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",2809]],"NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:C02A:2A2A/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",2809]],[]},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:C02A:2A2A")),
    ?match({[[iiop,{1,2},"0:0:0:0:0:FFFF:C02A:2A2A",2809]],"NameService"},
	   orber_cosnaming_utils:addresses(":1.2@0:0:0:0:0:FFFF:C02A:2A2A/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",4001]],"NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]],"NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]],[]},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]],[]},
	   orber_cosnaming_utils:addresses("iiop:1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001")),

    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",2809]],"NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",2809]],[]},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11")),
    ?match({[[iiop,{1,2},"0:0:0:0:0:FFFF:10.11.11.11",2809]],"NameService"},
	   orber_cosnaming_utils:addresses(":1.2@0:0:0:0:0:FFFF:10.11.11.11/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",4001]],"NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001]],"NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001]],[]},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001/")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001]],[]},
	   orber_cosnaming_utils:addresses("iiop:1.1@0:0:0:0:0:FFFF:10.11.11.11:4001/")),

    ?match({[[iiop,{1,1},"myhost",4001]],[]},
	   orber_cosnaming_utils:addresses("iiop:1.1@myhost:4001")),
    ?match({[[iiop,{1,1},"myhost.full.name",4001]],"NameService"},
	   orber_cosnaming_utils:addresses("iiop:1.1@myhost.full.name:4001/NameService")),
    ?match({[[iiop,{1,1},"myhost",4001], 
	     [iiop,{1,1},"myhost.full.name",2809]],"NameService"},
	   orber_cosnaming_utils:addresses("iiop:1.1@myhost:4001,iiop:1.1@myhost.full.name/NameService")),

    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001],
	     [iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]], "NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001,:1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001],
	     [iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]], []},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001,:1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",4001],
	     [iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]], "NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11:4001,:1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001],
	     [iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",4001]], "NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001,:0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",2809],
	     [iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",4001]], "NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11,:1.1@0:0:0:0:0:FFFF:C02A:2A2A:4001/NameService")),
    ?match({[[iiop,{1,1},"0:0:0:0:0:FFFF:10.11.11.11",4001],
	     [iiop,{1,1},"0:0:0:0:0:FFFF:C02A:2A2A",2809]], "NameService"},
	   orber_cosnaming_utils:addresses(":1.1@0:0:0:0:0:FFFF:10.11.11.11:4001,:1.1@0:0:0:0:0:FFFF:C02A:2A2A/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",2809],
	     [iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",2809]], "NameService"},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11,:0:0:0:0:0:FFFF:C02A:2A2A/NameService")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",2809],
	     [iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",2809]], []},
	   orber_cosnaming_utils:addresses(":0:0:0:0:0:FFFF:10.11.11.11,:0:0:0:0:0:FFFF:C02A:2A2A/")),
    ?match({[[iiop,{1,0},"0:0:0:0:0:FFFF:10.11.11.11",2809],
	     [iiop,{1,0},"0:0:0:0:0:FFFF:C02A:2A2A",2809]], []},
	   orber_cosnaming_utils:addresses("iiop:0:0:0:0:0:FFFF:10.11.11.11,:0:0:0:0:0:FFFF:C02A:2A2A/")),

    [IP] = ?match([_], orber:host()),
    {ok, ServerNode, _ServerHost} = 
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor 
							  ?ORB_ENV_USE_ACL_INCOMING)},
						 {iiop_acl, [{tcp_in, IP++"/128"}]}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({'IOP_IOR',_,_}, 
	   corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
%    ?line catch orber_test_lib:destroy_node(ServerNode, timeout),
    ok.


