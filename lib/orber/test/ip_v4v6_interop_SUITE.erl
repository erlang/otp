%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% File    : ip_v4v6_interop_SUITE.erl
%% Description : 
%%
%%----------------------------------------------------------------------
-module(ip_v4v6_interop_SUITE).

-compile(export_all).
%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/0, 
	 init_per_suite/1, 
	 end_per_suite/1, 
         init_per_testcase/2, 
	 end_per_testcase/2,
	 groups/0,
	 init_per_group/2,
	 end_per_group/2
	]).
%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
-include("idl_output/orber_test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContextExt.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming_NamingContext.hrl").

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------
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
%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% Initialization functions.
%%======================================================================

init_per_testcase(_Case, Config) ->    
    %% Starting dual configured ORB
    orber:jump_start([{iiop_port, 10001}, {flags, 16#1000}]),
    orber:info(),
    Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].


end_per_testcase(_Case, Config) ->
    orber:jump_stop(),
    Dog = proplists:get_value(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(Config) ->
    Config.

%%====================================================================
%% SUITE specification
%%====================================================================
all() ->
    [
     dual_ipv4v6
    ].

suite() -> [{ct_hooks,[ts_install_cth]}].


groups() ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%====================================================================
%% Test Cases
%%====================================================================
%% ORB configured for supporting both IPv4 and IPv6
dual_ipv4v6(_Config) ->
    
    %% Starting slave node with ipv4 configured ORB
    {ok, Ipv4Node, _Ipv4Host} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_port, 4001}])),
    Ipv4NS = orber_test_lib:remote_apply(Ipv4Node, corba, resolve_initial_references, ["NameService"]),

    %% Starting slave node with ipv6 configured ORB
    {ok, Ipv6Node, _Ipv6Host} =
	?match({ok,_,_}, orber_test_lib:js_node([{iiop_port, 6001}, {flags, 16#0100}])),
    Ipv6NS = orber_test_lib:remote_apply(Ipv6Node, corba, resolve_initial_references, ["NameService"]),

    %% Add the ipv6 interface in the dual configured ORB
    ?match({ok, _}, orber:add_listen_interface("::1", normal, 
					       [{ip_family, inet6}, {iiop_port, 10002}])),
    DualNS = corba:resolve_initial_references("NameService"),

    %% Bind IPv4 NameServer to a name in the dual stack orbs NameServer
    NSDual4 = orber_test_lib:remote_apply(Ipv4Node, corba, resolve_initial_references_remote,
					  ["NameService", ["iiop://127.0.0.1:10001"]]),
    ?match(ok, orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', bind,
					   [NSDual4, lname:new(["ns4"]), Ipv4NS])),
    'CosNaming_NamingContext':resolve(DualNS, lname:new(["ns4"])), 

    %% Bind IPv6 NameServer to a name in the dual stack orbs NameServer
    NSDual6 = orber_test_lib:remote_apply(Ipv6Node, corba, resolve_initial_references_remote,
					  ["NameService", ["iiop://[::1]:10002"]]),
    ?match(ok, orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', bind,
				     [NSDual6, lname:new(["ns6"]), Ipv6NS])),
    'CosNaming_NamingContext':resolve(DualNS, lname:new(["ns6"])), 

    %% IPv4: Fetch IPv6 NS reference from dual stack orber and register own NameServer in that
    Ipv4NSO = orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', resolve,
					  [NSDual4, lname:new(["ns6"])]),
    ?match(ok, orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', bind,
				     [Ipv4NSO, lname:new(["nso"]), Ipv4NS])),

    %% IPv6: Fetch IPv4 NS reference from dual stack orber and register own NameServer in that
    Ipv6NSO = orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', resolve,
					  [NSDual6, lname:new(["ns4"])]),
    ?match(ok, orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', bind,
				     [Ipv6NSO, lname:new(["nso"]), Ipv6NS])),


    %% IPv4: Fetch own NS reference from IPv6 NameServer and add a context then check that everything went well
    Ipv4NSFromIpv6 = orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', resolve,
				       [Ipv6NS, lname:new(["nso"])]),
    _Ipv4NC = orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', bind_new_context,
					 [Ipv4NSFromIpv6, lname:new(["test_context4"])]),
    
    %% IPv6: Fetch own NS reference from IPv4 NameServer and add a context then check that everything went well
    Ipv6NSFromIpv4 = orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', resolve,
				       [Ipv4NS, lname:new(["nso"])]),
    _Ipv6NC = orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', bind_new_context,
					 [Ipv6NSFromIpv4, lname:new(["test_context6"])]),

    %% Check that all the names are register correctly
    {ok,DualNames,_} = 'CosNaming_NamingContext':list(DualNS, 100),
    {ok,Ipv4Names,_} = orber_test_lib:remote_apply(Ipv4Node, 'CosNaming_NamingContext', list, [Ipv4NS, 100]),
    {ok,Ipv6Names,_} = orber_test_lib:remote_apply(Ipv6Node, 'CosNaming_NamingContext', list, [Ipv6NS, 100]),

    io:format("\nNames in Dual NS: ~p\n", [DualNames]),
    ?match(2, length(DualNames)),
    io:format("\nNames in IPv4 NS: ~p\n", [Ipv4Names]),
    ?match(2, length(Ipv4Names)),
    io:format("\nNames in IPv6 NS: ~p\n", [Ipv6Names]),
    ?match(2, length(Ipv6Names)),

    ok.

