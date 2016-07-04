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

-module(multi_ORB_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include("idl_output/orber_test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContextExt.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").


-define(default_timeout, test_server:minutes(15)).

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
		       exit(AcTuAlReS)
	       end
       end()).

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([all/0, suite/0,groups/0,init_per_group/2,end_per_group/2, cases/0,
	 init_per_suite/1, end_per_suite/1, basic_PI_api/1, multi_orber_api/1,
	 init_per_testcase/2, end_per_testcase/2, multi_pseudo_orber_api/1,
	 light_orber_api/1, light_orber2_api/1,
	 ssl_1_multi_orber_api/1, ssl_2_multi_orber_api/1, ssl_reconfigure_api/1,
	 iiop_timeout_api/1, iiop_timeout_added_api/1, setup_connection_timeout_api/1,
	 setup_multi_connection_timeout_api/1, setup_multi_connection_timeout_random_api/1,
	 setup_multi_connection_timeout_attempts_api/1,
	 fragments_server_api/1, fragments_max_server_api/1,
	 fragments_max_server_added_api/1, fragments_client_api/1,
	 light_ifr_api/1, max_requests_api/1, max_requests_added_api/1,
	 max_connections_api/1, max_packet_size_exceeded_api/1,
	 max_packet_size_ok_api/1, proxy_interface_api/1, proxy_interface_ipv6_api/1,
	 multiple_accept_api/1, implicit_context_api/1,
	 pseudo_implicit_context_api/1, pseudo_two_implicit_context_api/1,
	 oneway_implicit_context_api/1, implicit_context_roundtrip_api/1,
	 oneway_pseudo_implicit_context_api/1, flags_added_api/1,
	 oneway_pseudo_two_implicit_context_api/1,
	 local_interface_api/1, local_interface_ctx_override_api/1,
	 local_interface_acl_override_api/1, bad_giop_header_api/1,
	 bad_fragment_id_client_api/1, bad_id_cancel_request_api/1,
	 close_connections_api/1, close_connections_local_interface_api/1,
	 close_connections_local_interface_ctx_override_api/1,
	 ssl_1_multi_orber_generation_3_api/1, ssl_2_multi_orber_generation_3_api/1,
	 ssl_reconfigure_generation_3_api/1,
	 close_connections_alt_iiop_addr_api/1, close_connections_multiple_profiles_api/1]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([pseudo_calls/2, pseudo_casts/2, create_fake_server_ORB/5, do_connect/3]).

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


%% NOTE - the fragment test cases must be first since we explicitly set a request
%% id. Otherwise, the request-id counter would be increased and we cannot know
%% what it is.
cases() ->
    [fragments_server_api, fragments_max_server_api,
     fragments_max_server_added_api, fragments_client_api,
     flags_added_api, bad_fragment_id_client_api,
     bad_giop_header_api, bad_id_cancel_request_api,
     implicit_context_api, pseudo_implicit_context_api,
     pseudo_two_implicit_context_api,
     implicit_context_roundtrip_api,
     oneway_implicit_context_api,
     oneway_pseudo_implicit_context_api,
     oneway_pseudo_two_implicit_context_api,
     proxy_interface_api, proxy_interface_ipv6_api,
     local_interface_api, local_interface_ctx_override_api,
     local_interface_acl_override_api, close_connections_api,
     close_connections_local_interface_api,
     close_connections_local_interface_ctx_override_api,
     close_connections_alt_iiop_addr_api,
     close_connections_multiple_profiles_api,
     multiple_accept_api, max_requests_api,
     max_requests_added_api, max_connections_api,
     max_packet_size_exceeded_api, max_packet_size_ok_api,
     light_ifr_api, multi_pseudo_orber_api, multi_orber_api,
     light_orber_api, light_orber2_api, basic_PI_api,
     iiop_timeout_api, iiop_timeout_added_api,
     setup_connection_timeout_api,
     setup_multi_connection_timeout_api,
     setup_multi_connection_timeout_attempts_api,
     setup_multi_connection_timeout_random_api,
     ssl_1_multi_orber_api,
     ssl_1_multi_orber_generation_3_api,
     ssl_2_multi_orber_api,
     ssl_2_multi_orber_generation_3_api,
     ssl_reconfigure_api,
     ssl_reconfigure_generation_3_api].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(TC,Config)
  when TC =:= ssl_1_multi_orber_api;
       TC =:= ssl_2_multi_orber_api;
       TC =:= ssl_reconfigure_api ->
    init_ssl(Config);
init_per_testcase(TC,Config)
  when TC =:= ssl_1_multi_orber_generation_3_api;
       TC =:= ssl_2_multi_orber_generation_3_api;
       TC =:= ssl_reconfigure_generation_3_api ->
    init_ssl_3(Config);
init_per_testcase(_Case, Config) ->
    init_all(Config).

init_ssl(Config) ->
    case  proplists:get_value(crypto_started, Config) of
	true ->
	    case orber_test_lib:ssl_version() of
		no_ssl ->
		    {skip, "SSL is not installed!"};
		_ ->
		    init_all(Config)
	    end;
	false ->
	    {skip, "Crypto did not start"}
    end.

init_ssl_3(Config) ->
    case  proplists:get_value(crypto_started, Config) of
	true ->
	    case orber_test_lib:ssl_version() of
		3 ->
		    init_all(Config);
		2 ->
		    {skip, "Could not find the correct SSL version!"};
		no_ssl ->
		    {skip, "SSL is not installed!"}
	    end;
	false ->
	    {skip, "Crypto did not start"}
    end.

init_all(Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    Dog=test_server:timetrap(?default_timeout),
    orber:jump_start(0),
    oe_orber_test_server:oe_register(),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    oe_orber_test_server:oe_unregister(),
    orber:jump_stop(),
    Path = code:which(?MODULE),
    code:del_path(filename:join(filename:dirname(Path), "idl_output")),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    if
	is_list(Config) ->
            try crypto:start() of
                ok ->
		    [{crypto_started, true} | Config]
            catch _:_ ->
	       [{crypto_started, false} | Config]
            end;
	true ->
	    exit("Config not a list")
    end.

end_per_suite(Config) ->
    application:stop(crypto),    
    Config.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security
%%-----------------------------------------------------------------

%% IIOP Implicit Contex tests
implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([])),
    ?match(ok,
	   orber_test_server:
	   relay_call(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),

    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP Implicit Contex roundtrip tests
implicit_context_roundtrip_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    Relay = ?match(#'IOP_IOR'{},
		   corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    IOR = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [])),
    ?match(ok,
	   orber_test_server:
	   relay_call(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP Implicit Contex oneway tests
oneway_implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([])),
    ?match(ok,
	   orber_test_server:
	   relay_cast(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),

    %% We must wait for a few seconds for the client to be able to set up the
    %% connection (since it's a oneway operation).
    timer:sleep(5000),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP Implicit Contex tests (via pseudo object)
pseudo_implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{pseudo,true}])),
    ?match(ok,
	   orber_test_server:
	   relay_call(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP two Implicit Contex tests (via pseudo object)
pseudo_two_implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{pseudo,true}])),
    put(oe_server_in_context,
	[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
			       context_data = {interface,
					       IP}}]),
    ?match(ok,
	   orber_test_server:
	   relay_call(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP Implicit Contex tests (via pseudo object oneway)
oneway_pseudo_implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{pseudo,true}])),
    ?match(ok,
	   orber_test_server:
	   relay_cast(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.

%% IIOP two Implicit Contex tests (via pseudo object oneway)
oneway_pseudo_two_implicit_context_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% Create a remote server
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService#mamba")),

    Relay = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{pseudo,true}])),
    %% Add incoming implicit context which must be removed.
    put(oe_server_in_context,
	[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
			       context_data = {interface,
					       IP}}]),
    ?match(ok,
	   orber_test_server:
	   relay_cast(Relay,
		      [{context,
			[#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					       context_data = {interface,
							       Loopback}}]}],
		      IOR)),
    ?match([_,_], orber:iiop_connections(out)),
    Conns = ?match([_,_],
		   orber_test_lib:remote_apply(ServerNode, orber, iiop_connections, [in])),
    ?match(true, lists:keymember(Loopback, 1, Conns)),
    ok.



%% IIOP Multiple Accept tests
multiple_accept_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    %% The server ORB doesn't listen to 127.0.0.1
    ?match({'EXCEPTION',_},
	   corba:string_to_object("corbaloc::1.2@" ++Loopback++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match([], orber:iiop_connections(out)),

    IOR1 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {IP, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR1)),
    ?match([_], orber:iiop_connections(out)),

    {ok, Ref1} = ?match({ok, _},
			orber_test_lib:remote_apply(ServerNode, orber,
						    add_listen_interface,
						    [Loopback, normal])),

    IOR2 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++Loopback++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {Loopback, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR2)),
    ?match([_,_], orber:iiop_connections(out)),

    {ok, Ref2} = ?match({ok, _},
			orber_test_lib:remote_apply(ServerNode, orber,
						    add_listen_interface,
						    [Loopback, normal, 9543])),
    ?match({error, eaddrinuse},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [Loopback, normal, 9543])),

    IOR3 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++Loopback++":9543/NameService")),
    ?match({'external', {Loopback, 9543, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR3)),
    ?match([_,_,_], orber:iiop_connections(out)),

    ?match(ok,
	   orber_test_lib:remote_apply(ServerNode, orber,
				       remove_listen_interface, [Ref1])),
    %% Wait a few seconds to be sure that the connections really has been removed.
    timer:sleep(4000),
    ?match([_,_], orber:iiop_connections(out)),

    ?match(ok,
	   orber_test_lib:remote_apply(ServerNode, orber,
				       remove_listen_interface, [Ref2])),
    %% Wait a few seconds to be sure that the connections really has been removed.
    timer:sleep(4000),
    ?match([_], orber:iiop_connections(out)),

    ?match({'EXCEPTION',_},
	   corba:string_to_object("corbaloc::1.2@"++Loopback++":9543/NameService")),
    ?match({'EXCEPTION',_},
	   corba:string_to_object("corbaloc::1.2@"++Loopback++":"++integer_to_list(ServerPort)++"/NameService")),

    IOR4 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {IP, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR4)),

    ok.


%% IIOP Proxy Interface tests
%% This case test if the server ORB use the correct
%% interface when exporting IOR:s
proxy_interface_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_LOCAL_INTERFACE}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR1 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {IP, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR1)),
    IOR2 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++Loopback++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {Loopback, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR2)),
    ok.

%% IIOP Proxy Interface tests
%% This case test if the server ORB use the correct
%% IPv6 interface when exporting IOR:s
proxy_interface_ipv6_api(_Config) ->
    case orber_test_lib:version_ok() of
	true ->
	    proxy_interface_ipv6_api2();
	Reason ->
	    Reason
    end.

proxy_interface_ipv6_api2() ->
    Loopback = orber_test_lib:get_loopback_interface(inet6),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, (?ORB_ENV_USE_IPV6 bor
							  ?ORB_ENV_LOCAL_INTERFACE)}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),

    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_USE_IPV6}])),

    IP = orber_test_lib:remote_apply(ClientNode, orber_test_lib, get_host, []),

    IOR1 = ?match(#'IOP_IOR'{},
		  orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
					      ["corbaloc::1.2@["++IP++"]:"++integer_to_list(ServerPort)++"/NameService"])),
    ?match({'external', {IP, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   orber_test_lib:remote_apply(ClientNode, iop_ior, get_key, [IOR1])),
    IOR2 = ?match(#'IOP_IOR'{},
		  orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
					      ["corbaloc::1.2@["++Loopback++"]:"++integer_to_list(ServerPort)++"/NameService"])),
    ?match({'external', {Loopback, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   orber_test_lib:remote_apply(ClientNode, iop_ior, get_key, [IOR2])),
    ok.

%% IIOP Local Interface tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
local_interface_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address_local, Loopback}])),
    Port = orber:iiop_port(),
    ?match(#'IOP_IOR'{},
	   orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
				       ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService"])),
    [{Loopback, RemotePort}] =
	?match([{Loopback,_RemotePort}], orber:iiop_connections(in)),

    ?match([{IP, Port}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),
    ?match([{IP, Port}], orber:find_sockname_by_peername(Loopback,RemotePort)),
    ?match([{Loopback, RemotePort}], orber:find_peername_by_sockname(IP, Port)),

    ?match([{Loopback, RemotePort}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_sockname_by_peername,
				       [IP, Port])),
    ?match([{IP, Port}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_peername_by_sockname,
				       [Loopback,RemotePort])),


    ok.

%% IIOP Local Interface tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
local_interface_ctx_override_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address_local, IP}])),
    Port = orber:iiop_port(),
    ?match(#'IOP_IOR'{},
	   orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
				       ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
					[#'IOP_ServiceContext'
					 {context_id=?ORBER_GENERIC_CTX_ID,
					  context_data = {interface, Loopback}}]])),
    [{Loopback, RemotePort}] =
	?match([{Loopback,_RemotePort}], orber:iiop_connections(in)),

    ?match([{IP, Port, Loopback}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),
    ?match([{IP, Port}], orber:find_sockname_by_peername(Loopback,RemotePort)),
    ?match([{Loopback, RemotePort}], orber:find_peername_by_sockname(IP, Port)),

    ?match([{Loopback, RemotePort}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_sockname_by_peername,
				       [IP, Port])),
    ?match([{IP, Port}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_peername_by_sockname,
				       [Loopback,RemotePort])),

    ok.

%% IIOP Local Interface tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
local_interface_acl_override_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    ACL = [{tcp_out, IP ++ "/18", [Loopback]}],
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address_local, IP},
						 {iiop_acl, ACL},
						 {flags, ?ORB_ENV_USE_ACL_OUTGOING}])),
    Port = orber:iiop_port(),
    ?match(#'IOP_IOR'{},
	   orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
				       ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
					[#'IOP_ServiceContext'
					 {context_id=?ORBER_GENERIC_CTX_ID,
					  context_data = {interface, IP}}]])),
    ?match([{Loopback,_RemotePort}], orber:iiop_connections(in)),
    ?match(#'IOP_IOR'{},
	   orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
				       ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService"])),

    [{Loopback, RemotePort}] =
	?match([{Loopback,_RemotePort}], orber:iiop_connections(in)),
    ?match([{IP, Port, IP}], orber_test_lib:remote_apply(ClientNode, orber,
							 iiop_connections, [out])),
    ?match([{IP, Port}], orber:find_sockname_by_peername(Loopback,RemotePort)),
    ?match([{Loopback, RemotePort}], orber:find_peername_by_sockname(IP, Port)),

    ?match([{Loopback, RemotePort}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_sockname_by_peername,
				       [IP, Port])),
    ?match([{IP, Port}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       find_peername_by_sockname,
				       [Loopback,RemotePort])),

    ok.


%% IIOP TIMEOUT API tests
%% This case test if timeout configuration behaves correctly
iiop_timeout_api(_Config) ->

    %% Install two secure orber.
    {ok, ClientNode, ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_timeout, 6},
						 {iiop_connection_timeout, 3},
						 {iiop_in_connection_timeout, 3}])),
    ClientPort = orber_test_lib:remote_apply(ClientNode, orber, iiop_port, []),

    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_timeout, 6},
						 {iiop_connection_timeout, 3},
						 {iiop_in_connection_timeout, 12}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),

    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [timeout])),

    %% Tell client_orb to interoperate with server_orb.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   lookup,
					   [ServerHost, ServerPort])),
    %% Interop worked fine, perform delay tests.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   timeouts,
					   [ServerHost, ServerPort, 6000])),

    %% Create a connection to the "client_orb", which will now act as server.
    ?match({'IOP_IOR',_,_},
	   corba:string_to_object("corbaloc::1.2@"++ClientHost++":"++integer_to_list(ClientPort)++"/NameService")),
    %% Check that the connection is established.
    ?match([{_, ClientPort}], orber:iiop_connections(out)),
    %% Wait >3 seconds (i.e. iiop_in_connection_timeout) and check if the connection
    %% have been closed.
    timer:sleep(8000),
    ?match([], orber:iiop_connections(out)),

    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   uninstall_test_data,
					   [timeout])),
    ok.

%% IIOP TIMEOUT API tests
%% This case test if timeout configuration behaves correctly
iiop_timeout_added_api(_Config) ->
    IP = orber_test_lib:get_host(),
    {ok, Node, _Host} = ?match({ok,_,_}, orber_test_lib:js_node([])),
    Port = 1 + orber_test_lib:remote_apply(Node, orber, iiop_port, []),
    ?match({ok, _},
	   orber_test_lib:remote_apply(Node, orber,
				       add_listen_interface,
				       [IP, normal,
					[{iiop_in_connection_timeout, 3},
					 {flags, ?ORB_ENV_LOCAL_INTERFACE},
					 {iiop_port, Port}]])),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [timeout])),

    ?match({'IOP_IOR',_,_},
	   corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService")),
    %% Check that the connection is established.
    ?match([{_, Port}], orber:iiop_connections(out)),
    %% Wait >3 seconds (i.e. iiop_in_connection_timeout) and check if the connection
    %% have been closed.
    timer:sleep(8000),
    ?match([], orber:iiop_connections(out)),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   uninstall_test_data,
					   [timeout])),
    ok.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB using pseudo call/cast, no security
%%-----------------------------------------------------------------

%% MULTI ORB PSEUDO API tests
%% This case test if data encode/decode (IIOP) for pseudo objects
%% produce the correct result, i.e., the test_server echos
%% the input parameter or an exception is raised (MARSHAL)
multi_pseudo_orber_api(_Config) ->
    %% --- Create a slave-node ---
    {ok, Node, Host} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    Port = orber_test_lib:remote_apply(Node, orber, iiop_port, []),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [pseudo])),

    NSR = ?match({'IOP_IOR',"IDL:omg.org/CosNaming/NamingContextExt:1.0",_},
		 corba:string_to_object("corbaloc::1.1@"++Host++":"++
					integer_to_list(Port)++"/NameService")),
    Obj =
	?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
	       'CosNaming_NamingContext':resolve(NSR, lname:new(["mamba"]))),
    orber_test_lib:corba_object_tests(Obj, NSR),

    %% Can we even contact the object?
    ?match(ok, orber_test_server:print(Obj)),

    %% Invoke one blocking call followed by several invokations.
    spawn(?MODULE, pseudo_calls, [5, Obj]),
    ?match({ok, 10000}, orber_test_server:pseudo_call_delay(Obj, 10000)),
    spawn(?MODULE, pseudo_casts, [5, Obj]),
    ?match(ok, orber_test_server:pseudo_cast_delay(Obj, 10000)),

    %%--- Testing code and decode arguments ---
    orber_test_lib:test_coding(Obj),

    %% Test if exit is handled properly.
    ?match({'EXCEPTION',{'TRANSIENT',_,_,_}},
	   orber_test_server:stop_brutal(Obj)),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   uninstall_test_data,
					   [pseudo])),
    ok.


%%-----------------------------------------------------------------
%%  API tests for ORB to ORB with local flags definition set.
%%-----------------------------------------------------------------
%% MULTI ORB PSEUDO with local flags definition set
flags_added_api(_Config) ->
    %% --- Create a slave-node ---
    IP = orber_test_lib:get_host(),
    {ok, Node, _Host} =
	?match({ok,_,_}, orber_test_lib:js_node([])),
    Port = 1 + orber_test_lib:remote_apply(Node, orber, iiop_port, []),
    ?match({ok, _},
	   orber_test_lib:remote_apply(Node, orber,
				       add_listen_interface,
				       [IP, normal,
					[{flags, (?ORB_ENV_LOCAL_INTERFACE bor
						  ?ORB_ENV_EXCLUDE_CODESET_COMPONENT)},
					 {iiop_port, Port}]])),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 install_test_data,
						 [pseudo])),
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		       corba:string_to_object("corbaname::1.1@"++IP++":"++
					      integer_to_list(Port)++"/NameService#mamba")),
    ?match({'external', {IP, Port, _ObjectKey, _Counter,
			 #'IOP_TaggedProfile'{tag=?TAG_INTERNET_IOP,
					      profile_data=
					      #'IIOP_ProfileBody_1_1'{components=[]}},
			 _NewHD}},
	   iop_ior:get_key(Obj)),
    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 uninstall_test_data,
						 [pseudo])),

    ok.



%%-----------------------------------------------------------------
%%  API tests for ORB to ORB with limited concurrent requests
%%-----------------------------------------------------------------
%% MULTI ORB PSEUDO with limited concurrent requests tests
max_requests_api(_Config) ->
    %% --- Create a slave-node ---
    {ok, Node, Host} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_max_in_requests, 1}])),
    Port = orber_test_lib:remote_apply(Node, orber, iiop_port, []),
    max_requests(Node, Host, Port).

%% MULTI ORB PSEUDO with limited concurrent requests tests
max_requests_added_api(_Config) ->
    %% --- Create a slave-node ---
    [IP] = ?match([_], orber:host()),
    {ok, Node, _Host} =
	?match({ok,_,_}, orber_test_lib:js_node([])),
    Port = 1 + orber_test_lib:remote_apply(Node, orber, iiop_port, []),
    ?match({ok, _},
	   orber_test_lib:remote_apply(Node, orber,
				       add_listen_interface,
				       [IP, normal,
					[{iiop_max_in_requests, 1},
					 {flags, ?ORB_ENV_LOCAL_INTERFACE},
					 {iiop_port, Port}]])),
    max_requests(Node, IP, Port).

max_requests(Node, Host, Port) ->
    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 install_test_data,
						 [pseudo])),
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		       corba:string_to_object("corbaname::1.1@"++Host++":"++
					      integer_to_list(Port)++"/NameService#mamba")),

    %% Can we even contact the object?
    ?match(ok, orber_test_server:print(Obj)),

    %% Invoke one blocking call followed by several invokations.
    spawn(orber_test_server, pseudo_call_delay, [Obj, 15000]),
    %% Wait for a second to be sure that the previous request has been sent
    timer:sleep(1000),
    {MegaSecsB, Before, _} = erlang:timestamp(),
    pseudo_calls(5, Obj),
    {MegaSecsA, After, _} = erlang:timestamp(),
    %% Normally we we can perform hundreds of pseudo-calls per second. Hence,
    %% if we add 8 seconds to 'Before' it should still be less since we only
    %% allow one request at a time to the target ORB.
    ?match(true, (MegaSecsB + (Before+8)*1000000) < (MegaSecsA + After*1000000)),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 uninstall_test_data,
						 [pseudo])),

    ok.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB with limited concurrent connections
%%-----------------------------------------------------------------
%% MULTI ORB PSEUDO with limited concurrent connections tests
max_connections_api(_Config) ->
    %% --- Create a slave-node ---
    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_backlog, 0},
						 {iiop_max_in_connections, 2}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
						 install_test_data,
						 [nameservice])),

    %% Claim connection 1 & 2
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		       corba:string_to_object("corbaname::1.2@"++ServerHost++":"++
					      integer_to_list(ServerPort)++"/NameService#mamba")),
    %% Claim backlog
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node()),

    spawn(ClientNode, orber_test_server, print, [Obj]),
    timer:sleep(5000),
    ?match([_], orber_test_lib:remote_apply(ClientNode, orber,
						  iiop_connections, [])),

    %% Try to connect. Should fail. Due to the behavior of different TCP stacks, backlog 1
    %% might not be the precise value. Hence, we also need to define the iiop_timeout. Otherwise
    %% this test case will fail. For the same reason we must GC this connection.
    {ok, ClientNodeII, _ClientHostII} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_setup_connection_timeout, 5},
						 {iiop_timeout, 5},
						 {iiop_connection_timeout, 8}])),

    ?match({'EXCEPTION', _},
	   orber_test_lib:remote_apply(ClientNodeII, orber_test_server,
				       testing_iiop_string, [Obj, "Fail"])),

    %% Remove 2 connections. We need to wait a moment so that both sides has detected it.
    timer:sleep(5000),
    ?match([_,_], orber:iiop_connections()),
    ?match(ok, orber_iiop_pm:close_connection([{ServerHost, ServerPort}])),
    timer:sleep(5000),
    [{Host, Port}] = ?match([_], orber:iiop_connections()),
    ?match(ok, orber_iiop_pm:close_connection([{Host, Port}])),
    timer:sleep(5000),
    ?match([], orber:iiop_connections()),

    ?match([_], orber_test_lib:remote_apply(ClientNode, orber,
					    iiop_connections, [])),

    ?match([], orber_test_lib:remote_apply(ClientNodeII, orber,
					   iiop_connections, [])),

    ?match({ok, "OK"},
	   orber_test_lib:remote_apply(ClientNodeII, orber_test_server,
				       testing_iiop_string, [Obj, "OK"])),

    timer:sleep(4000),
    ?match([_], orber_test_lib:remote_apply(ClientNodeII, orber,
					    iiop_connections, [])),

    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
						 uninstall_test_data,
						 [pseudo])),

    ok.


%%-----------------------------------------------------------------
%%  API tests for terminating connection by using an IOR.
%%-----------------------------------------------------------------
%% Close outgoing connection
close_connections_api(_Config) ->
    %% --- Create a slave-node ---
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IP = orber_test_lib:get_host(),

    %% Create a connection
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		       corba:string_to_object("corbaname::1.2@"++IP++":"++
					      integer_to_list(ServerPort)++"/NameService#mamba")),
    %% Check that it's up.
    ?match([{IP, ServerPort}], orber:iiop_connections(out)),
    %% Try to close using the wronge interface.
    ?match(ok, orber:close_connection(Obj, Loopback)),
    %% Should still be up.
    ?match([{IP, ServerPort}], orber:iiop_connections(out)),
    %% Try to close it properly
    ?match(ok, orber:close_connection(Obj)),
    %% Wait a moment so that both sides has detected it.
    timer:sleep(5000),
    %% Worked?
    ?match([], orber:iiop_connections(out)),
    ok.


%% IIOP Local Interface disconnect tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
close_connections_local_interface_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address_local, Loopback}])),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address, IP}])),
    Port = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
					     ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService"])),

    %% Check that the connnection is up and running using the default interface
    ?match([{Loopback,_RemotePort}], orber_test_lib:remote_apply(ServerNode, orber,
								 iiop_connections, [in])),
    ?match([{IP, Port}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),
    %% Try to close the connection
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber,
					   close_connection, [IOR])),
    %% Wait a moment so that both sides has detected it.
    timer:sleep(5000),
    %% Now the connection shall be gone.
    ?match([], orber_test_lib:remote_apply(ClientNode, orber,
					   iiop_connections, [out])),
    ?match([], orber_test_lib:remote_apply(ServerNode, orber,
					   iiop_connections, [in])),

    ok.

%% IIOP Local Interface disconnect tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
close_connections_local_interface_ctx_override_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address_local, IP},
						 {ip_address, IP}])),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address, IP}])),
    Port = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 orber_test_lib:remote_apply(ClientNode, corba, string_to_object,
					     ["corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
					      [#'IOP_ServiceContext'
					       {context_id=?ORBER_GENERIC_CTX_ID,
						context_data = {interface, Loopback}}]])),

    timer:sleep(2000),
    %% Check that the connnection is up and running using the default interface
    ?match([{Loopback,_RemotePort}], orber_test_lib:remote_apply(ServerNode, orber,
								 iiop_connections, [in])),

    ?match([{IP, Port, Loopback}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),
    %% Try to close not supplying the interface.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber,
					   close_connection, [IOR])),

    timer:sleep(2000),
    %% The connection shall still be up and running
    ?match([{Loopback,_RemotePort}], orber_test_lib:remote_apply(ServerNode, orber,
								 iiop_connections, [in])),
    ?match([{IP, Port, Loopback}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),
    %% Try to close not supplying the interface.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber,
					   close_connection, [IOR, IP])),

    timer:sleep(2000),
    %% The connection shall still be up and running
    ?match([{Loopback,_RemotePort}], orber_test_lib:remote_apply(ServerNode, orber,
								 iiop_connections, [in])),
    ?match([{IP, Port, Loopback}],
	   orber_test_lib:remote_apply(ClientNode, orber,
				       iiop_connections, [out])),

    %% Try to close supplying the correct interface.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber,
					   close_connection, [IOR, Loopback])),
    %% Wait a moment so that both sides has detected it.
    timer:sleep(5000),
    %% Now the connection shall be gone.
    ?match([], orber_test_lib:remote_apply(ServerNode, orber,
					   iiop_connections, [in])),
    ?match([], orber_test_lib:remote_apply(ClientNode, orber,
					   iiop_connections, [out])),
    ok.

%% IIOP alternate address disconnect tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
close_connections_alt_iiop_addr_api(_Config) ->
    %% --- Create a slave-node ---
    Loopback = orber_test_lib:get_loopback_interface(),
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{giop_version, {1, 2}},
						 {ip_address, {multiple, [IP, Loopback]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [{nameservice, Loopback, ServerPort}])),
    %% Create two connections
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++
					integer_to_list(ServerPort)++"/NameService#mamba")),
    ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
	   corba:string_to_object("corbaname::1.2@"++Loopback++":"++
				  integer_to_list(ServerPort)++"/NameService#mamba")),
    timer:sleep(2000),
    %% The connection shall still be up and running
    ?match([{_,_}, {_,_}], orber:iiop_connections(out)),
    ?match([{_,_}, {_,_}],
	   orber_test_lib:remote_apply(ServerNode, orber,
				       iiop_connections, [in])),

    %% Try to close the connection
    ?match(ok, orber:close_connection(Obj)),
    %% Wait a moment so that both sides has detected it.
    timer:sleep(5000),
    %% Now the connections shall be gone.
    ?match([], orber:iiop_connections(out)),
    ?match([], orber_test_lib:remote_apply(ServerNode, orber,
					   iiop_connections, [in])),
    ok.

%% IIOP alternate address disconnect tests
%% This case test if the server ORB use the correct
%% local interface when connecting to another ORB
close_connections_multiple_profiles_api(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    %% --- Create a slave-node ---
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{ip_address,
						  {multiple, [Loopback, IP]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data, [nameservice])),
    %% Create two connections
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 corba:string_to_object("corbaname::1.2@"++IP++":"++
					integer_to_list(ServerPort)++"/NameService#mamba")),
    ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
	   corba:string_to_object("corbaname::1.2@"++Loopback++":"++
				  integer_to_list(ServerPort)++"/NameService#mamba")),
    %% The connection shall still be up and running
    ?match([{_,_}, {_,_}], orber:iiop_connections(out)),
    ?match([{_,_}, {_,_}],
	   orber_test_lib:remote_apply(ServerNode, orber,
				       iiop_connections, [in])),

    %% Try to close the connection
    ?match(ok, orber:close_connection(Obj)),
    %% Wait a moment so that both sides has detected it.
    timer:sleep(5000),
    %% Now the connections shall be gone.
    ?match([], orber:iiop_connections(out)),
    ?match([], orber_test_lib:remote_apply(ServerNode, orber,
					   iiop_connections, [in])),
    ok.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB with iiop_packet_size set
%%-----------------------------------------------------------------
%% Exceed the maximum request size
max_packet_size_exceeded_api(_Config) ->
    case catch gen_tcp:listen(0, [{packet,cdr}, {packet_size, 14}]) of
	{'EXIT',badarg} ->
	    {skipped, "The inet option {packet_size, Max} not supported"};
	{ok, LS} ->
	    (catch gen_tcp:close(LS)),
	    %% --- Create a slave-node ---
	    {ok, ServerNode, ServerHost} =
		?match({ok,_,_}, orber_test_lib:js_node([{iiop_packet_size, 1}])),
	    ServerPort = orber_test_lib:remote_apply(ServerNode, orber,
						     iiop_port, []),
	    ?match({'EXCEPTION', #'CosNaming_NamingContextExt_InvalidAddress'{}},
		   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
	    ok
    end.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB with iiop_packet_size set
%%-----------------------------------------------------------------
%% Not exceed the maximum request size
max_packet_size_ok_api(_Config) ->
    case catch gen_tcp:listen(0, [{packet,cdr}, {packet_size, 14}]) of
	{'EXIT',badarg} ->
	    {skipped, "The inet option {packet_size, Max} not supported"};
	{ok, LS} ->
	    (catch gen_tcp:close(LS)),
	    %% --- Create a slave-node ---
	    {ok, ServerNode, ServerHost} =
		?match({ok,_,_}, orber_test_lib:js_node([{iiop_packet_size, 5000}])),
	    ServerPort = orber_test_lib:remote_apply(ServerNode, orber,
						     iiop_port, []),
	    ?match({'IOP_IOR',_,_},
		   corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService")),
	    ok
    end.



%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security
%%-----------------------------------------------------------------
%% LIGHT IFR ORB API tests
light_ifr_api(_Config) ->

    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, 128}])),

    ?match([_,_,_,_], orber_test_lib:remote_apply(ClientNode, orber, get_tables, [])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
                                                 install_test_data,
                                                 [nameservice])),


    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, 128}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    ?match([_,_,_,_], orber_test_lib:remote_apply(ServerNode, orber, get_tables, [])),

    Obj = ?match({'IOP_IOR',_,_},
		 corba:string_to_object("corbaname::1.2@"++ServerHost++":"++integer_to_list(ServerPort)++"/NameService#mamba")),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib, test_coding, [Obj])),

    ?match(0, orber_test_lib:remote_apply(ClientNode, orber_diagnostics, missing_modules, [])),

    ?match(ok, orber_test_lib:remote_apply(ClientNode, mnesia, dirty_write,
					   [#orber_light_ifr{id = "FakeId1",
							     module=non_existing,
							     type=?IFR_StructDef}])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, mnesia, dirty_write,
					   [#orber_light_ifr{id = "FakeId2",
							     module=non_existing,
							     type=?IFR_UnionDef}])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, mnesia, dirty_write,
					   [#orber_light_ifr{id = "FakeId3",
							     module=non_existing,
							     type=?IFR_ExceptionDef}])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, mnesia, dirty_write,
					   [#orber_light_ifr{id = "FakeId4",
							     module=non_existing,
							     type=?IFR_InterfaceDef}])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, mnesia, dirty_write,
					   [#orber_light_ifr{id = "FakeId5",
							     module=orber_test_lib,
							     type=?IFR_InterfaceDef}])),
    ?match(5, orber_test_lib:remote_apply(ClientNode, orber_diagnostics, missing_modules, [])),


    ?match(ok, mnesia:dirty_write(#ir_UnionDef{ir_Internal_ID = "FakedIId1",
						     absolute_name="::Module::NonExisting"})),
    ?match(ok, mnesia:dirty_write(#ir_StructDef{ir_Internal_ID = "FakedIId2",
						      absolute_name="::Module::NonExisting"})),
    ?match(ok, mnesia:dirty_write(#ir_ExceptionDef{ir_Internal_ID = "FakedIId3",
							 absolute_name="::Module::NonExisting"})),
    ?match(ok, mnesia:dirty_write(#ir_InterfaceDef{ir_Internal_ID = "FakedIId4",
							 absolute_name="::Module::NonExisting"})),
    ?match(ok, mnesia:dirty_write(#ir_InterfaceDef{ir_Internal_ID = "FakedIId5",
							 absolute_name="::orber::test::lib"})),

    ?match(5, orber_diagnostics:missing_modules()),

    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
						 uninstall_test_data,
						 [nameservice])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
						 uninstall_test_data,
						 [nameservice])),
    ok.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security
%%-----------------------------------------------------------------
%% LIGHT ORB API tests
%% This case test if a light Orber can communicate correctly
%% with an fully installed Orber.
light_orber_api(_Config) ->
    %% --- Create a slave-node ---
    LocalHost = net_adm:localhost(),
    {ok, Node, _Host} =
	?match({ok,_,_}, orber_test_lib:js_node([{lightweight, ["iiop://"++LocalHost++":"++integer_to_list(orber:iiop_port())]}],
						lightweight)),
    ?match(ok, orber:info(io)),
    ?match([_], orber_test_lib:remote_apply(Node, orber_env, get_lightweight_nodes,[])),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 install_test_data,
						 [light])),

    Obj1=(catch orber_test_server:oe_create(state,[{pseudo,true}])),
    ?match({_,pseudo,orber_test_server_impl, _,_, _}, Obj1),
    Obj2=(catch orber_test_server:oe_create(state,[])),
    ?match({_,key,_, _,_, _}, Obj2),

    NS  = corba:resolve_initial_references("NameService"),
    'CosNaming_NamingContext':bind(NS, lname:new(["mamba"]), Obj1),
    'CosNaming_NamingContext':bind(NS, lname:new(["viper"]), Obj2),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   light_tests,
					   [LocalHost,
					    orber:iiop_port(), "viper"])),
    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   light_tests,
					   [LocalHost,
					    orber:iiop_port(), "mamba"])),

    %% Clean up.

    catch corba:dispose(Obj1),
    catch corba:dispose(Obj2),
    catch 'CosNaming_NamingContext':destroy(NS),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
						 uninstall_test_data,
						 [light])),
    ok.
%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security
%%-----------------------------------------------------------------
%% LIGHT ORB API tests
%% This case test if a light Orber can communicate correctly
%% with an fully installed Orber. This case test if we can
%% start as lightweight without first setting the environment
%% variable
light_orber2_api(_Config) ->
    %% --- Create a slave-node ---
    LocalHost = net_adm:localhost(),
    {ok, Node, _Host} =
	?match({ok,_,_}, orber_test_lib:js_node([],
						{lightweight, ["iiop://"++LocalHost++":"++integer_to_list(orber:iiop_port())]})),
    ?match(ok, orber:info(io)),
    ?match([_], orber_test_lib:remote_apply(Node, orber_env, get_lightweight_nodes,[])),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [light])),

    Obj1=(catch orber_test_server:oe_create(state,[{pseudo,true}])),
    ?match({_,pseudo,orber_test_server_impl, _,_, _}, Obj1),
    Obj2=(catch orber_test_server:oe_create(state,[])),
    ?match({_,key,_, _,_, _}, Obj2),

    NS  = corba:resolve_initial_references("NameService"),
    'CosNaming_NamingContext':bind(NS, lname:new(["mamba"]), Obj1),
    'CosNaming_NamingContext':bind(NS, lname:new(["viper"]), Obj2),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   light_tests,
					   [LocalHost,
					    orber:iiop_port(), "viper"])),
    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   light_tests,
					   [LocalHost,
					    orber:iiop_port(), "mamba"])),

    %% Clean up.

    catch corba:dispose(Obj1),
    catch corba:dispose(Obj2),
    catch 'CosNaming_NamingContext':destroy(NS),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   uninstall_test_data,
					   [light])),
    ok.

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security
%%-----------------------------------------------------------------
%% MULTI ORB API tests
%% This case test if data encode/decode (IIOP)
%% produce the correct result, i.e., the test_server echos
%% the input parameter or an exception is raised (MARSHAL).
multi_orber_api(_Config) ->

    NewICObj1 = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([])),
    NewICObj2 = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{regname, {local, newic2}}])),
    NewICObj3 = ?match({_,_,_,_,_,_}, orber_test_server:oe_create([], [{regname, {global, newic3}}])),
    ?match(ok, orber_test_server:print(NewICObj1)),
    ?match(ok, orber_test_server:print(NewICObj2)),
    ?match(ok, orber_test_server:print(NewICObj3)),
    catch corba:dispose(NewICObj1),
    catch corba:dispose(NewICObj2),
    catch corba:dispose(NewICObj3),

    %% --- Create a slave-node ---
    {ok, Node, Host} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    Port = orber_test_lib:remote_apply(Node, orber, iiop_port, []),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [nameservice])),

    NSR = ?match({'IOP_IOR',"IDL:omg.org/CosNaming/NamingContextExt:1.0",_},
		 corba:string_to_object("corbaloc::1.2@"++Host++":"++
					integer_to_list(Port)++"/NameService")),

    ?match({'EXCEPTION',{'CosNaming_NamingContext_NotFound',_,_,_}},
	   'CosNaming_NamingContext':resolve(NSR, lname:new(["not_exist"]))),

    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 'CosNaming_NamingContext':resolve(NSR, lname:new(["mamba"]))),
    ?match(ok, orber_test_server:print(Obj)),

    Obj12B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.2@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    Obj11B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.1@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    Obj10B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.0@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    context_test(Obj12B),
    context_test(Obj11B),

    ?match(ok, orber_test_server:print(Obj12B)),
    ?match(ok, orber_test_server:print(Obj11B)),
    ?match(ok, orber_test_server:print(Obj10B)),
    ?match({'EXCEPTION',{'CosNaming_NamingContextExt_InvalidAddress',_}},
	   corba:string_to_object("corbaloc::1.0@"++Host++":"++integer_to_list(Port)++"/Wrong")),

    ?match(ok, orber_test_lib:corba_object_tests(Obj12B, NSR)),
    ?match(ok, orber_test_lib:corba_object_tests(Obj11B, NSR)),
    ?match(ok, orber_test_lib:corba_object_tests(Obj10B, NSR)),

    %%--- Testing code and decode arguments ---
    orber_test_lib:test_coding(Obj),

    ?match({'EXCEPTION',#'BAD_CONTEXT'{}},
	   orber_test_server:
	   print(Obj12B,
		 [{context,
		   [#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
					  context_data = {interface,
							  {127,0,0,1}}}]}])),

    ?match({'EXCEPTION',{'TRANSIENT',_,_,_}},
	   orber_test_server:stop_brutal(Obj12B)),
    ?match({'EXCEPTION',{'TRANSIENT',_,_,_}},
		 orber_test_server:print(Obj12B)),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   uninstall_test_data,
					   [nameservice])),
    ok.


%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, no security, using basic interceptors
%%-----------------------------------------------------------------
%% MULTI ORB API tests
%% This case test if data encode/decode (IIOP)
%% produce the correct result when using basic interceptors
%% i.e., the test_server echos the input parameter or 
%% an exception is raised (MARSHAL).
basic_PI_api(_Config) ->
    %% Change configuration to use Basic Interceptors.
    orber:configure_override(interceptors, {native, [orber_test_lib]}),
    %% --- Create a slave-node ---
    {ok, Node, Host} =
	?match({ok,_,_}, orber_test_lib:js_node([{interceptors, {native, [orber_test_lib]}}])),
    Port = orber_test_lib:remote_apply(Node, orber, iiop_port, []),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [nameservice])),

    Obj12 = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		   corba:string_to_object("corbaname::1.2@"++Host++":"++integer_to_list(Port)++"/NameService#mamba")),

    Obj11 = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		   corba:string_to_object("corbaname::1.1@"++Host++":"++integer_to_list(Port)++"/NameService#mamba")),

    Obj10 = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		   corba:string_to_object("corbaname::1.0@"++Host++":"++integer_to_list(Port)++"/NameService#mamba")),

    ?match(ok, corba:print_object(Obj12)),
    ?match(ok, corba:print_object(Obj11, error_report)),
    ?match(ok, corba:print_object(Obj10, {error_report, "Reason"})),

    ?match(ok, orber_test_server:print(Obj12)),
    ?match(ok, orber_test_server:print(Obj11)),
    ?match(ok, orber_test_server:print(Obj10)),


    Obj12B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.2@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    Obj11B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.1@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    Obj10B = ?match({'IOP_IOR',_,_},
		    corba:string_to_object("corbaloc::1.0@"++Host++":"++integer_to_list(Port)++"/Mamba")),

    ?match(ok, corba:print_object(Obj12B, info_msg)),
    ?match(ok, corba:print_object(Obj11B, {info_msg, "Comment"})),
    ?match([_|_], corba:print_object(Obj10B, string)),

    ?match(ok, orber_test_server:print(Obj12B)),
    ?match(ok, orber_test_server:print(Obj11B)),
    ?match(ok, orber_test_server:print(Obj10B)),
    ?match({'EXCEPTION',{'CosNaming_NamingContextExt_InvalidAddress',_}},
		 corba:string_to_object("corbaloc::1.0@"++Host++":"++integer_to_list(Port)++"/Wrong")),

    ?match(ok, orber_test_lib:alternate_iiop_address(Host, Port)),

    context_test(Obj12B),
    context_test(Obj11B),

    %%--- Testing code and decode arguments ---
    orber_test_lib:test_coding(Obj12),
    orber_test_lib:test_coding(Obj11),
    orber_test_lib:test_coding(Obj10),

    application:set_env(orber, interceptors, false),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   uninstall_test_data,
					   [nameservice])),
    ok.


%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, ssl security depth 1
%%-----------------------------------------------------------------

%% SECURE MULTI ORB API tests (SSL depth 1)
%% This case set up two secure orbs and test if they can
%% communicate. The case also test to access one of the
%% secure orbs which must raise a NO_PERMISSION exception.
ssl_1_multi_orber_api(_Config) ->
    ServerOptions = orber_test_lib:get_options_old(iiop_ssl, server,
					       1, [{iiop_ssl_port, 0}]),
    ClientOptions = orber_test_lib:get_options_old(iiop_ssl, client,
					       1, [{iiop_ssl_port, 0}]),
    ssl_suite(ServerOptions, ClientOptions).


%% SECURE MULTI ORB API tests (SSL depth 1)
%% This case set up two secure orbs and test if they can
%% communicate. The case also test to access one of the
%% secure orbs which must raise a NO_PERMISSION exception.
ssl_1_multi_orber_generation_3_api(_Config) ->

    ServerOptions = orber_test_lib:get_options(iiop_ssl, server,
					       1, [{ssl_generation, 3},
						   {iiop_ssl_port, 0}]),
    ClientOptions = orber_test_lib:get_options(iiop_ssl, client,
					       1, [{ssl_generation, 3},
						   {iiop_ssl_port, 0}]),
    ssl_suite(ServerOptions, ClientOptions).

%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, ssl security depth 2
%%-----------------------------------------------------------------

%% SECURE MULTI ORB API tests (SSL depth 2)
%% These case set up two secure orbs and test if they can
%% communicate. They also test to access one of the
%% secure orbs which must raise a NO_PERMISSION exception.
ssl_2_multi_orber_api(_Config) ->

    ServerOptions = orber_test_lib:get_options_old(iiop_ssl, server,
					       2, [{iiop_ssl_port, 0}]),
    ClientOptions = orber_test_lib:get_options_old(iiop_ssl, client,
					       2, [{iiop_ssl_port, 0}]),
    ssl_suite(ServerOptions, ClientOptions).

ssl_2_multi_orber_generation_3_api(_Config) ->

    ServerOptions = orber_test_lib:get_options(iiop_ssl, server,
					       2, [{ssl_generation, 3},
						   {iiop_ssl_port, 0}]),
    ClientOptions = orber_test_lib:get_options(iiop_ssl, client,
					       2, [{ssl_generation, 3},
						   {iiop_ssl_port, 0}]),
    ssl_suite(ServerOptions, ClientOptions).
%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, ssl security depth 2
%%-----------------------------------------------------------------

%% SECURE MULTI ORB API tests (SSL depth 2)
%% These case set up two secure orbs and test if they can
%% communicate. They also test to access one of the
%% secure orbs which must raise a NO_PERMISSION exception.
ssl_reconfigure_api(_Config) ->
    ssl_reconfigure_old([]).


% ssl_reconfigure_generation_3_api_old(_Config) ->
%     ssl_reconfigure_old([{ssl_generation, 3}]).

ssl_reconfigure_old(ExtraSSLOptions) ->

    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_},
	       orber_test_lib:js_node([{iiop_port, 0},
				       {flags, ?ORB_ENV_LOCAL_INTERFACE},
				       {ip_address, IP}|ExtraSSLOptions])),
    orber_test_lib:remote_apply(ServerNode, ssl, start, []),
    orber_test_lib:remote_apply(ServerNode, crypto, start, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [ssl])),
    ?match({ok, _},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [Loopback, normal, [{iiop_port, 5648},
							   {iiop_ssl_port, 5649},
							   {interceptors, {native, [orber_iiop_tracer_silent]}}|ExtraSSLOptions]])),
    ServerOptions = orber_test_lib:get_options_old(iiop_ssl, server,
					       2, [{flags, ?ORB_ENV_LOCAL_INTERFACE},
						   {iiop_port, 5648},
						   {iiop_ssl_port, 5649},
						   {interceptors, {native, [orber_iiop_tracer_silent]}}|ExtraSSLOptions]),
    ?match({ok, _},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [Loopback, ssl, ServerOptions])),

    ClientOptions = orber_test_lib:get_options_old(iiop_ssl, client,
					       2, [{iiop_ssl_port, 0}|ExtraSSLOptions]),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ClientOptions)),

    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   install_test_data,
					   [ssl])),
    orber_test_lib:remote_apply(ClientNode, ssl, start, []),
    orber_test_lib:remote_apply(ServerNode, crypto, start, []),
    Obj = ?match(#'IOP_IOR'{},
		 orber_test_lib:remote_apply(ClientNode, corba,
					     string_to_object, ["corbaname:iiop:1.1@"++Loopback++":5648/NameService#mamba",
								[{context, [#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
												  context_data = {configuration, ClientOptions}}]}]])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_server,
					   print, [Obj])).


ssl_reconfigure_generation_3_api(_Config) ->
    ssl_reconfigure([{ssl_generation, 3}]).


ssl_reconfigure(ExtraSSLOptions) ->

    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_},
	       orber_test_lib:js_node([{iiop_port, 0},
				       {flags, ?ORB_ENV_LOCAL_INTERFACE},
				       {ip_address, IP}|ExtraSSLOptions])),
    orber_test_lib:remote_apply(ServerNode, ssl, start, []),
    orber_test_lib:remote_apply(ServerNode, crypto, start, []),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [ssl])),
    ?match({ok, _},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [Loopback, normal, [{iiop_port, 5648},
							   {iiop_ssl_port, 5649},
							   {interceptors, {native, [orber_iiop_tracer_silent]}}|ExtraSSLOptions]])),
    ServerOptions = orber_test_lib:get_options(iiop_ssl, server,
					       2, [{flags, ?ORB_ENV_LOCAL_INTERFACE},
						   {iiop_port, 5648},
						   {iiop_ssl_port, 5649},
						   {interceptors, {native, [orber_iiop_tracer_silent]}}|ExtraSSLOptions]),
    ?match({ok, _},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [Loopback, ssl, ServerOptions])),

    ClientOptions = orber_test_lib:get_options(iiop_ssl, client,
					       2, [{iiop_ssl_port, 0}|ExtraSSLOptions]),
    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ClientOptions)),

    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   install_test_data,
					   [ssl])),
    orber_test_lib:remote_apply(ClientNode, ssl, start, []),
    orber_test_lib:remote_apply(ServerNode, crypto, start, []),
    Obj = ?match(#'IOP_IOR'{},
		 orber_test_lib:remote_apply(ClientNode, corba,
					     string_to_object, ["corbaname:iiop:1.1@"++Loopback++":5648/NameService#mamba",
								[{context, [#'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
												  context_data = {configuration, ClientOptions}}]}]])),
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_server,
					   print, [Obj])).


%%------------------------------------------------------------
%% function : ssl_suite
%% Arguments: Config
%%            Depth
%% Returns  : ok
%% Effect   :
%%------------------------------------------------------------
ssl_suite(ServerOptions, ClientOptions) ->

    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ServerOptions)),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    SSLServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_ssl_port, []),

    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ClientOptions)),

    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [ssl])),
    %% Tell the client to interoperate with the server. The purpose of this
    %% operation is to look up, using NameService, an object reference and
    %% use it to contact the object.
    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   lookup,
					   [ServerHost, ServerPort])),

    ?match(ok, orber_test_lib:remote_apply(ClientNode, orber_test_lib,
					   alternate_ssl_iiop_address,
					   [ServerHost, ServerPort, SSLServerPort])),

    %% 'This' node is not secure. Contact the server. Must refuse connection.
    NSR = ?match({'IOP_IOR',"IDL:omg.org/CosNaming/NamingContextExt:1.0",_},
		       corba:string_to_object("corbaloc::1.2@"++ServerHost++":"++
					      integer_to_list(ServerPort)++"/NameService")),

    %% Should be 'NO_PERMISSION'??
    ?match({'EXCEPTION',{'COMM_FAILURE',_,_,_}},
		 'CosNaming_NamingContext':resolve(NSR, lname:new(["not_exist"]))),

    %% Should be 'NO_PERMISSION'??
    ?match({'EXCEPTION',{'COMM_FAILURE',_,_,_}},
		 'CosNaming_NamingContext':resolve(NSR, lname:new(["mamba"]))),

    %% Uninstall.
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   uninstall_test_data,
					   [ssl])),
    ok.

%%-----------------------------------------------------------------
%%  iiop_setup_connection_timeout API tests for ORB to ORB.
%%-----------------------------------------------------------------
setup_connection_timeout_api(_Config) ->
    ?match(ok, application:set_env(orber, iiop_backlog, 0)),
    %% Wait to be sure that the configuration has kicked in.
    timer:sleep(2000),
    {ok, Ref, Port} = create_fake_server_ORB(normal, 0, [], listen, []),
    ?match(ok, orber:configure(iiop_setup_connection_timeout, 5)),
    ?match(ok, orber:info(io)),
    IP = orber_test_lib:get_host(),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    timer:sleep(2000),
    Corbaloc = "corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
    ?match({'EXCEPTION', _E}, corba:string_to_object(Corbaloc)),
    destroy_fake_ORB(Ref),
    ?match(ok, application:set_env(orber, iiop_backlog, 5)),
    ok.

%%-----------------------------------------------------------------
%%  iiop_setup_connection_timeout API tests for ORB to ORB.
%%-----------------------------------------------------------------
setup_multi_connection_timeout_api(_Config) ->
    ?match(ok, application:set_env(orber, iiop_backlog, 0)),
    %% Wait to be sure that the configuration has kicked in.
    timer:sleep(2000),
    {ok, Ref, Port} = create_fake_server_ORB(normal, 0, [], listen, []),
    ?match(ok, application:set_env(orber, iiop_out_ports, {6042, 6234})),
    ?match(ok, orber:configure(iiop_setup_connection_timeout, 5)),
    ?match(ok, orber:info(io)),
    IP = orber_test_lib:get_host(),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    Corbaloc = "corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
    timer:sleep(2000),
    ?match({'EXCEPTION', _E}, corba:string_to_object(Corbaloc)),
    destroy_fake_ORB(Ref),
    ?match(ok, application:set_env(orber, iiop_backlog, 5)),
    ?match(ok, application:set_env(orber, iiop_out_ports, undefined)),
    ok.

setup_multi_connection_timeout_attempts_api(_Config) ->
    ?match(ok, application:set_env(orber, iiop_backlog, 0)),
    %% Wait to be sure that the configuration has kicked in.
    timer:sleep(2000),
    {ok, Ref, Port} = create_fake_server_ORB(normal, 0, [], listen, []),
    ?match(ok, application:set_env(orber, iiop_out_ports, {6042, 6234})),
    ?match(ok, application:set_env(orber, iiop_out_ports_attempts, 1)),
    ?match(ok, orber:configure(iiop_setup_connection_timeout, 5)),
    ?match(ok, orber:info(io)),
    IP = orber_test_lib:get_host(),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    Corbaloc = "corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
    timer:sleep(2000),
    ?match({'EXCEPTION', _E}, corba:string_to_object(Corbaloc)),
    destroy_fake_ORB(Ref),
    ?match(ok, application:set_env(orber, iiop_backlog, 5)),
    ?match(ok, application:set_env(orber, iiop_out_ports, undefined)),
    ok.

setup_multi_connection_timeout_random_api(_Config) ->
    ?match(ok, application:set_env(orber, iiop_backlog, 0)),
    %% Wait to be sure that the configuration has kicked in.
    timer:sleep(2000),
    {ok, Ref, Port} = create_fake_server_ORB(normal, 0, [], listen, []),
    ?match(ok, application:set_env(orber, iiop_out_ports, {6042, 6234})),
    ?match(ok, application:set_env(orber, iiop_out_ports_random, true)),
    ?match(ok, orber:configure(iiop_setup_connection_timeout, 5)),
    ?match(ok, orber:info(io)),
    IP = orber_test_lib:get_host(),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    spawn(?MODULE, do_connect, [IP, Port, [{active, false}]]),
    Corbaloc = "corbaloc::1.2@"++IP++":"++integer_to_list(Port)++"/NameService",
    timer:sleep(2000),
    ?match({'EXCEPTION', _E}, corba:string_to_object(Corbaloc)),
    destroy_fake_ORB(Ref),
    ?match(ok, application:set_env(orber, iiop_backlog, 5)),
    ?match(ok, application:set_env(orber, iiop_out_ports, undefined)),
    ok.

%%-----------------------------------------------------------------
%%  Sending an incorrect header to the server-side ORB.
%%-----------------------------------------------------------------
bad_giop_header_api(_Config) ->
    orber:configure_override(interceptors, {native,[orber_iiop_tracer]}),
    orber:configure(orber_debug_level, 10),
    ?match(ok, orber:info(io)),
    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    Req = <<"GIOP",1,2,0,100,0,0,0,5,0,0,0,10,50>> ,
    ?match(ok, fake_client_ORB(normal, ServerHost, ServerPort, [],
				     message_error, [Req])),

    application:set_env(orber, interceptors, false),
    orber:configure(orber_debug_level, 0),
    ok.


%%-----------------------------------------------------------------
%%  Fragmented IIOP tests (Server-side).
%%-----------------------------------------------------------------
-define(REQUEST_ID, 0).

-define(REPLY_FRAG_1, <<71,73,79,80,1,2,2,1,0,0,0,41,0,0,0,?REQUEST_ID,0,0,0,0,0,0,0,1,78,69,79,0,0,0,0,2,0,10,0,0,0,0,0,0,0,0,0,18,0,0,0,0,0,0,0,4,49>>).
%% The fragments are identical for requests and replies.
-define(FRAG_2, <<71,73,79,80,1,2,2,7,0,0,0,5,0,0,0,?REQUEST_ID,50>>).
-define(FRAG_3, <<71,73,79,80,1,2,2,7,0,0,0,5,0,0,0,?REQUEST_ID,51>>).
-define(FRAG_4, <<71,73,79,80,1,2,0,7,0,0,0,5,0,0,0,?REQUEST_ID,0>>).


fragments_server_api(_Config) ->
    %% --- Create a slave-node ---
    {ok, Node, Host} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    Port = orber_test_lib:remote_apply(Node, orber, iiop_port, []),

    ?match(ok, orber_test_lib:remote_apply(Node, orber_test_lib,
					   install_test_data,
					   [nameservice])),

    NSR = ?match({'IOP_IOR',"IDL:omg.org/CosNaming/NamingContextExt:1.0",_},
		 corba:string_to_object("corbaloc::1.2@"++Host++":"++
					integer_to_list(Port)++"/NameService")),

    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 'CosNaming_NamingContext':resolve(NSR, lname:new(["mamba"]))),

    Any = #any{typecode = {tk_string,0},
	       value = "123"},
    Target = #'GIOP_TargetAddress'{label = ?GIOP_KeyAddr,
				   value = iop_ior:get_objkey(Obj)},
    %% Fix a request header.
    {Hdr, Body, HdrLen, _What, _Flags} =
	cdr_encode:enc_request_split(
	  #giop_env{version = {1,2}, objkey = Target,
		    request_id = ?REQUEST_ID,
		    response_expected = true,
		    op = testing_iiop_any,
		    parameters = [49], ctx = [],
		    tc = {tk_void,[tk_char],[]},
		    host = [orber_test_lib:get_host()],
		    iiop_port = orber:iiop_port(),
		    iiop_ssl_port = orber:iiop_ssl_port(),
		    domain = orber:domain(),
		    partial_security = orber:partial_security()}),
    NewBody =
	case size(Body) of
	    1 ->
		<<0,0,0,18,0,0,0,0,0,0,0,4,49>> ;
	    Size ->
		Aligned = Size -1,
		<<AligmnetData:Aligned/binary,49>> = Body,
		list_to_binary([AligmnetData, <<0,0,0,18,0,0,0,0,0,0,0,4,49>> ])
	end,

    MessSize = HdrLen+size(NewBody),
    ReqFrag = list_to_binary([ <<"GIOP",1:8,2:8,2:8,0:8,
			       MessSize:32/big-unsigned-integer>> , Hdr |NewBody]),
    ?match(Any, fake_client_ORB(normal, Host, Port, [], fragments,
				[ReqFrag, ?FRAG_2, ?FRAG_3, ?FRAG_4])),

    ok.

%%-----------------------------------------------------------------
%%  Fragmented IIOP tests (Server-side). Exceeding Maximum.
%%-----------------------------------------------------------------
fragments_max_server_api(_Config) ->
    %% --- Create a slave-node ---
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_max_fragments, 2},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    fragments_max_server(ServerNode, IP, ServerPort).

fragments_max_server_added_api(_Config) ->
    %% --- Create a slave-node ---
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([])),
    ServerPort = 1 + orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match({ok, _},
	   orber_test_lib:remote_apply(ServerNode, orber,
				       add_listen_interface,
				       [IP, normal,
					[{iiop_max_fragments, 2},
					 {flags, ?ORB_ENV_LOCAL_INTERFACE},
					 {iiop_port, ServerPort}]])),
    fragments_max_server(ServerNode, IP, ServerPort).

fragments_max_server(ServerNode, ServerHost, ServerPort) ->
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [nameservice])),
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 corba:string_to_object("corbaname::1.2@"++ServerHost++":"++
					integer_to_list(ServerPort)++"/NameService#mamba")),
    Target = #'GIOP_TargetAddress'{label = ?GIOP_KeyAddr,
				   value = iop_ior:get_objkey(Obj)},
    %% Fix a request header.
    {Hdr, Body, HdrLen, _What, _Flags} =
	cdr_encode:enc_request_split(
	  #giop_env{version = {1,2},
		    objkey = Target,
		    request_id = ?REQUEST_ID,
		    response_expected = true,
		    op = testing_iiop_any,
		    parameters = [49], ctx = [],
		    tc = {tk_void,[tk_char],[]},
		    host = [orber_test_lib:get_host()],
		    iiop_port = orber:iiop_port(),
		    iiop_ssl_port = orber:iiop_ssl_port(),
		    domain = orber:domain(),
		    partial_security = orber:partial_security()}),
    NewBody =
	case size(Body) of
	    1 ->
		<<0,0,0,18,0,0,0,0,0,0,0,4,49>> ;
	    Size ->
		Aligned = Size -1,
		<<AligmnetData:Aligned/binary,49>> = Body,
		list_to_binary([AligmnetData, <<0,0,0,18,0,0,0,0,0,0,0,4,49>> ])
	end,

    MessSize = HdrLen+size(NewBody),
    ReqFrag = list_to_binary([ <<"GIOP",1:8,2:8,2:8,0:8,
			       MessSize:32/big-unsigned-integer>> , Hdr |NewBody]),
    ?match(#'IMP_LIMIT'{},
	   fake_client_ORB(normal, ServerHost, ServerPort, [], fragments_max,
			   [ReqFrag, ?FRAG_2, ?FRAG_3, ?FRAG_4])),

    ok.

%%-----------------------------------------------------------------
%%  Fragmented IIOP tests (Client-side).
%%-----------------------------------------------------------------
fragments_client_api(_Config) ->
    Any = #any{typecode = {tk_string,0},
	       value = "123"},
    application:set_env(orber, interceptors, {native,[orber_iiop_tracer]}),
    orber:configure(orber_debug_level, 10),
    orber:info(),
    IOR = ?match({'IOP_IOR',_,_},
		       iop_ior:create_external({1, 2}, "IDL:FAKE:1.0",
					       "localhost", 6004, "FAKE", [])),
    spawn(?MODULE, create_fake_server_ORB, [normal, 6004, [], fragments,
					    [?REPLY_FRAG_1, ?FRAG_2,
					     ?FRAG_3, ?FRAG_4]]),
    ?match({ok, Any}, orber_test_server:testing_iiop_any(IOR, Any)),
    application:set_env(orber, interceptors, false),
    orber:configure(orber_debug_level, 0),
    ok.

bad_fragment_id_client_api(_Config) ->
    application:set_env(orber, interceptors, {native,[orber_iiop_tracer]}),
    orber:configure(orber_debug_level, 10),
    orber:info(),
    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    Req = <<71,73,79,80,1,2,2,7,0,0,0,5,0,0,0,100,50>> ,
    ?match(ok, fake_client_ORB(normal, ServerHost, ServerPort, [],
			       message_error, [Req])),

    application:set_env(orber, interceptors, false),
    orber:configure(orber_debug_level, 0),

    ok.

%%-----------------------------------------------------------------
%%  Non-existing request id
%%-----------------------------------------------------------------
bad_id_cancel_request_api(Config) when is_list(Config) ->
    Req10 = cdr_encode:enc_cancel_request(#giop_env{version = {1, 0},
						    request_id = 556}),
    Req11 = cdr_encode:enc_cancel_request(#giop_env{version = {1, 1},
						    request_id = 556}),
    Req12 = cdr_encode:enc_cancel_request(#giop_env{version = {1, 2},
						    request_id = 556}),
    {ok, ServerNode, ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node()),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    ?match(ok, fake_client_ORB(normal, ServerHost, ServerPort, [],
			       message_error, [Req10])),
    ?match(ok, fake_client_ORB(normal, ServerHost, ServerPort, [],
			       message_error, [Req11])),
    ?match(ok, fake_client_ORB(normal, ServerHost, ServerPort, [],
			       message_error, [Req12])),
    ok.

%%-----------------------------------------------------------------
%% Local functions.
%%-----------------------------------------------------------------

do_connect(Host, Port, Options) ->
    gen_tcp:connect(Host, Port, Options),
    timer:sleep(20000).

pseudo_calls(0, _) ->
    ok;
pseudo_calls(Times, Obj) ->
    orber_test_server:pseudo_call(Obj),
    New = Times - 1,
    pseudo_calls(New, Obj).
pseudo_casts(0, _) ->
    ok;
pseudo_casts(Times, Obj) ->
    orber_test_server:pseudo_cast(Obj),
    New = Times - 1,
    pseudo_casts(New, Obj).

context_test(Obj) ->
    CodeSetCtx = #'CONV_FRAME_CodeSetContext'{char_data =  65537,
					      wchar_data = 65801},
    FTGrp = #'FT_FTGroupVersionServiceContext'{object_group_ref_version = ?ULONGMAX},
    FTReq = #'FT_FTRequestServiceContext'{client_id = "ClientId",
					  retention_id = ?LONGMAX,
					  expiration_time = ?ULONGLONGMAX},

    IDToken1 = #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTAbsent,
				    value = true},
    IDToken2 = #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTAnonymous,
				    value = false},
    IDToken3 = #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTPrincipalName,
				    value = [0,255]},
    IDToken4 = #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTX509CertChain,
				    value = [1,255]},
    IDToken5 = #'CSI_IdentityToken'{label = ?CSI_IdentityTokenType_ITTDistinguishedName,
				    value = [2,255]},
    IDToken6 = #'CSI_IdentityToken'{label = ?ULONGMAX,
				    value = [3,255]},

    MTEstablishContext1 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken1,
				       client_authentication_token = [1, 255]}},
    MTEstablishContext2 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken2,
				       client_authentication_token = [1, 255]}},
    MTEstablishContext3 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken3,
				       client_authentication_token = [1, 255]}},
    MTEstablishContext4 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken4,
				       client_authentication_token = [1, 255]}},
    MTEstablishContext5 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken5,
				       client_authentication_token = [1, 255]}},
    MTEstablishContext6 = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTEstablishContext,
       value = #'CSI_EstablishContext'{client_context_id = ?ULONGLONGMAX,
				       authorization_token =
				       [#'CSI_AuthorizationElement'
					{the_type = ?ULONGMAX,
					 the_element = [0,255]}],
				       identity_token = IDToken6,
				       client_authentication_token = [1, 255]}},
    MTCompleteEstablishContext = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTCompleteEstablishContext,
       value = #'CSI_CompleteEstablishContext'{client_context_id = ?ULONGLONGMAX,
					       context_stateful = false,
					       final_context_token = [1, 255]}},
    MTContextError = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTContextError,
       value = #'CSI_ContextError'{client_context_id = ?ULONGLONGMAX,
				   major_status = 1,
				   minor_status = 2,
				   error_token = [2,255]}},
    MTMessageInContext = #'CSI_SASContextBody'
      {label = ?CSI_MsgType_MTMessageInContext,
       value = #'CSI_MessageInContext'{client_context_id = ?ULONGLONGMAX,
				       discard_context = true}},
    Ctx = [#'IOP_ServiceContext'{context_id=?IOP_CodeSets,
				 context_data = CodeSetCtx},
	   #'IOP_ServiceContext'{context_id=?IOP_FT_GROUP_VERSION,
				 context_data = FTGrp},
	   #'IOP_ServiceContext'{context_id=?IOP_FT_REQUEST,
				 context_data = FTReq},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext1},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext2},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext3},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext4},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext5},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTEstablishContext6},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTCompleteEstablishContext},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTContextError},
	   #'IOP_ServiceContext'{context_id=?IOP_SecurityAttributeService,
				 context_data = MTMessageInContext},
	   #'IOP_ServiceContext'{context_id=?ORBER_GENERIC_CTX_ID,
				 context_data = {any_kind_of_data, {127,0,0,1}, 4001}}],
    ?match(ok, orber_test_server:testing_iiop_context(Obj, [{context, Ctx}])).


create_fake_server_ORB(Type, Port, Options, listen, _Data) ->
    {ok, _ListenSocket, NewPort} =
	orber_socket:listen(Type, Port,
			    [{backlog, 0}, {active, false}|Options]),
    Socket = orber_socket:connect(Type, 'localhost', NewPort, [{active, false}|Options]),
    {ok, {Type, Socket}, NewPort};
create_fake_server_ORB(Type, Port, Options, Action, Data) ->
    {ok, ListenSocket, _NewPort} =
	orber_socket:listen(Type, Port, [{active, false}|Options]),
    Socket = orber_socket:accept(Type, ListenSocket),
    do_server_action(Type, Socket, Action, Data),
    orber_socket:close(Type, Socket),
    ok.

destroy_fake_ORB({Type, Socket}) ->
    orber_socket:close(Type, Socket);
destroy_fake_ORB(_) ->
    ok.

fake_client_ORB(Type, Host, Port, Options, connect, _Data) ->
    Socket = orber_socket:connect(Type, Host, Port, [{active, false}|Options]),
    {Type, Socket};
fake_client_ORB(Type, Host, Port, Options, Action, Data) ->
    Socket = orber_socket:connect(Type, Host, Port, [{active, false}|Options]),
    Result = do_client_action(Type, Socket, Action, Data),
    orber_socket:close(Type, Socket),
    Result.



do_server_action(Type, Socket, fragments, FragList) ->
    timer:sleep(3000),
    {ok, _B} = gen_tcp:recv(Socket, 0),
    ok = send_data(Type, Socket, FragList);
do_server_action(_Type, _Socket, _Action, _Data) ->
    ok.

do_client_action(Type, Socket, fragments, FragList) ->
    ok = send_data(Type, Socket, FragList),
    timer:sleep(3000),
    {ok, Bytes} = gen_tcp:recv(Socket, 0),
    {#reply_header{request_id = ?REQUEST_ID, reply_status = no_exception}, ok, [Par]} =
	cdr_decode:dec_message({tk_void,[tk_any],[tk_any]}, Bytes),
    Par;
do_client_action(Type, Socket, fragments_max, FragList) ->
    ok = send_data(Type, Socket, FragList),
    timer:sleep(3000),
    {ok, Bytes} = gen_tcp:recv(Socket, 0),
    {#reply_header{request_id = ?REQUEST_ID, reply_status = system_exception}, Exc, []} =
	cdr_decode:dec_message({tk_void,[tk_any],[tk_any]}, Bytes),
    Exc;
do_client_action(Type, Socket, message_error, Data) ->
    ok = send_data(Type, Socket, Data),
    timer:sleep(3000),
    {ok,Bytes} = gen_tcp:recv(Socket, 0),
    'message_error' = cdr_decode:dec_message({tk_void,[tk_any],[tk_any]}, Bytes),
    ok;
do_client_action(_Type, _Socket, _Action, _Data) ->
    ok.

send_data(_Type, _Socket, []) ->
    ok;
send_data(Type, Socket, [H|T]) ->
    orber_socket:write(Type, Socket, H),
    send_data(Type, Socket, T).

