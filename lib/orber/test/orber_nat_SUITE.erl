%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

-module(orber_nat_SUITE).

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
	 init_per_suite/1, end_per_suite/1,
	 init_per_testcase/2, end_per_testcase/2,
	 nat_ip_address/1, nat_ip_address_multiple/1,
	 nat_ip_address_local/1, nat_ip_address_local_local/1,
	 nat_iiop_port/1, nat_iiop_port_local/1,
	 nat_iiop_port_local_local/1,
	 nat_iiop_ssl_port/1, nat_iiop_ssl_port_local/1]).


%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

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
    [nat_ip_address,
     nat_ip_address_multiple,
     nat_ip_address_local,
     nat_iiop_port,
     nat_iiop_port_local,
     nat_ip_address_local_local,
     nat_iiop_port_local_local,
     nat_iiop_ssl_port,
     nat_iiop_ssl_port_local].

%%-----------------------------------------------------------------
%% Init and cleanup functions.
%%-----------------------------------------------------------------
init_per_testcase(TC, Config)
 when TC =:= nat_iiop_ssl_port;
      TC =:= nat_iiop_ssl_port_local ->
      case  proplists:get_value(crypto_started, Config) of
	  true ->
	      case orber_test_lib:ssl_version() of
		  no_ssl ->
		      {skip,"SSL not installed!"};
		  _ ->
		      init_per_testcase(dummy_tc, Config)
	      end;
	  false ->
	      {skip, "Crypto did not start"}
      end;
init_per_testcase(_Case, Config) ->
    Path = code:which(?MODULE),
    code:add_pathz(filename:join(filename:dirname(Path), "idl_output")),
    Dog=test_server:timetrap(?default_timeout),
    orber:jump_start([{iiop_port, 0},
		      {flags, 0}]),
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
%%  API tests for NAT
%%-----------------------------------------------------------------
%% These case test if the server ORB use the correct
%% interface when exporting IOR:s
nat_ip_address(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_ENABLE_NAT},
						 {nat_ip_address, Loopback}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {Loopback, ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR)),
    ok.

nat_ip_address_multiple(_Config) ->
    IP = orber_test_lib:get_host(),

    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_ENABLE_NAT},
						 {nat_ip_address, {multiple, ["10.0.0.1"]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {"10.0.0.1", ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR)),
    ok.

nat_ip_address_local(_Config) ->
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_ENABLE_NAT},
						 {nat_ip_address, {local, "10.0.0.1", [{IP, "127.0.0.1"}]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {"10.0.0.1", ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR)),
    ok.

nat_ip_address_local_local(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags,
						  (?ORB_ENV_LOCAL_INTERFACE bor
						   ?ORB_ENV_ENABLE_NAT)},
						 {nat_ip_address, {local, "10.0.0.1", [{IP, "10.0.0.2"}]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR1 = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {"10.0.0.2", ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR1)),
    IOR2 = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++Loopback++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {"10.0.0.1", ServerPort, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR2)),
    ok.

nat_iiop_port(_Config) ->
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_ENABLE_NAT},
						 {nat_iiop_port, 42}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {_IP, 42, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR)),
    ok.

nat_iiop_port_local(_Config) ->
    IP = orber_test_lib:get_host(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags, ?ORB_ENV_ENABLE_NAT},
						 {nat_iiop_port, {local, 42, [{4001, 43}]}}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    IOR = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {_IP, 42, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR)),
    ok.

nat_iiop_port_local_local(_Config) ->
    IP = orber_test_lib:get_host(),
    Loopback = orber_test_lib:get_loopback_interface(),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node([{flags,
						  (?ORB_ENV_LOCAL_INTERFACE bor
						   ?ORB_ENV_ENABLE_NAT)},
						 {ip_address, IP}])),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    orber_test_lib:remote_apply(ServerNode, orber_env, configure_override, [nat_iiop_port, {local, 42, [{ServerPort, 43}]}]),
    IOR1 = ?match(#'IOP_IOR'{},
		 corba:string_to_object("corbaloc::1.2@"++IP++":"++integer_to_list(ServerPort)++"/NameService")),
    ?match({'external', {IP, 43, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR1)),
    {ok, Ref} = ?match({ok, _},
			orber_test_lib:remote_apply(ServerNode, orber,
						    add_listen_interface,
						    [Loopback, normal, 10088])),
    IOR2 = ?match(#'IOP_IOR'{},
		  corba:string_to_object("corbaloc::1.2@"++Loopback++":10088/NameService")),
    ?match({'external', {IP, 42, _ObjectKey, _Counter, _TP, _NewHD}},
	   iop_ior:get_key(IOR2)),
    ?match(ok,
	   orber_test_lib:remote_apply(ServerNode, orber,
				       remove_listen_interface, [Ref])),
    ok.


%%-----------------------------------------------------------------
%%  API tests for ORB to ORB, ssl security depth 1
%%-----------------------------------------------------------------
%% SECURE MULTI ORB API tests (SSL depth 1)
%% Make sure NAT works for SSL
nat_iiop_ssl_port(_Config) ->

    IP = orber_test_lib:get_host(),
    ServerOptions = orber_test_lib:get_options(iiop_ssl, server,
					       1, [{iiop_ssl_port, 0},
						   {flags, ?ORB_ENV_ENABLE_NAT},
						   {ip_address, IP}]),
    ClientOptions = orber_test_lib:get_options(iiop_ssl, client,
					       1, [{iiop_ssl_port, 0}]),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ServerOptions)),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    SSLServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_ssl_port, []),
    NATSSLServerPort = SSLServerPort+1,
    {ok, Ref} = ?match({ok, _},
		       orber_test_lib:remote_apply(ServerNode, orber,
						   add_listen_interface,
						   [IP, ssl, NATSSLServerPort])),
    orber_test_lib:remote_apply(ServerNode, orber_env, configure_override,
				[nat_iiop_ssl_port,
				 {local, NATSSLServerPort, [{4001, 43}]}]),

    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ClientOptions)),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [ssl])),

    IOR1 = ?match(#'IOP_IOR'{},
		  orber_test_lib:remote_apply(ClientNode, corba,
					      string_to_object,
					      ["corbaname::1.2@"++IP++":"++
						   integer_to_list(ServerPort)++"/NameService#mamba"])),

    ?match({'external', {_IP, _Port, _ObjectKey, _Counter, _TP,
			 #host_data{protocol = ssl,
				    ssl_data = #'SSLIOP_SSL'{port = NATSSLServerPort}}}},
	   iop_ior:get_key(IOR1)),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   uninstall_test_data,
					   [ssl])),
    ?match(ok,
	   orber_test_lib:remote_apply(ServerNode, orber,
				       remove_listen_interface, [Ref])),
    ok.

nat_iiop_ssl_port_local(_Config) ->

    IP = orber_test_lib:get_host(),
    ServerOptions = orber_test_lib:get_options(iiop_ssl, server,
					       1, [{iiop_ssl_port, 0},
						   {flags,
						    (?ORB_ENV_LOCAL_INTERFACE bor
							 ?ORB_ENV_ENABLE_NAT)},
						   {ip_address, IP}]),
    ClientOptions = orber_test_lib:get_options(iiop_ssl, client,
					       1, [{iiop_ssl_port, 0}]),
    {ok, ServerNode, _ServerHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ServerOptions)),
    ServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_port, []),
    SSLServerPort = orber_test_lib:remote_apply(ServerNode, orber, iiop_ssl_port, []),
    NATSSLServerPort = SSLServerPort+1,
    {ok, Ref} = ?match({ok, _},
		       orber_test_lib:remote_apply(ServerNode, orber,
						   add_listen_interface,
						   [IP, ssl, NATSSLServerPort])),
    orber_test_lib:remote_apply(ServerNode, orber_env, configure_override,
				[nat_iiop_ssl_port,
				 {local, NATSSLServerPort, [{NATSSLServerPort, NATSSLServerPort}]}]),

    {ok, ClientNode, _ClientHost} =
	?match({ok,_,_}, orber_test_lib:js_node(ClientOptions)),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   install_test_data,
					   [ssl])),

    IOR1 = ?match(#'IOP_IOR'{},
		  orber_test_lib:remote_apply(ClientNode, corba,
					      string_to_object,
					      ["corbaname::1.2@"++IP++":"++
						   integer_to_list(ServerPort)++"/NameService#mamba"])),

    ?match({'external', {_IP, _Port, _ObjectKey, _Counter, _TP,
			 #host_data{protocol = ssl,
				    ssl_data = #'SSLIOP_SSL'{port = NATSSLServerPort}}}},
	   iop_ior:get_key(IOR1)),
    ?match(ok, orber_test_lib:remote_apply(ServerNode, orber_test_lib,
					   uninstall_test_data,
					   [ssl])),
    ?match(ok,
	   orber_test_lib:remote_apply(ServerNode, orber,
				       remove_listen_interface, [Ref])),
    ok.

