%%--------------------------------------------------------------------
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
%% File: orber.erl
%% 
%% Description:
%%    This file contains the Orber application interface
%%
%%-----------------------------------------------------------------
-module(orber).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1, stop/0, install/1, install/2, orber_nodes/0, iiop_port/0,
	 domain/0, iiop_ssl_port/0, iiop_out_ports/0, iiop_out_ports_random/0,
	 iiop_out_ports_attempts/0,
	 ssl_server_options/0, ssl_client_options/0, set_ssl_client_options/1,
	 ssl_server_certfile/0, ssl_client_certfile/0, set_ssl_client_certfile/1,
	 ssl_server_verify/0, ssl_client_verify/0, set_ssl_client_verify/1,
	 ssl_server_depth/0, ssl_client_depth/0, set_ssl_client_depth/1,
	 ssl_server_cacertfile/0,ssl_client_cacertfile/0, set_ssl_client_cacertfile/1,
	 ssl_client_keyfile/0, ssl_client_password/0, ssl_server_keyfile/0, ssl_server_password/0, 
	 ssl_client_ciphers/0, ssl_server_ciphers/0, ssl_client_cachetimeout/0, ssl_server_cachetimeout/0,
	 uninstall/0, giop_version/0, info/0, info/1, is_running/0, add_node/2, 
	 remove_node/1, iiop_timeout/0, iiop_connection_timeout/0, 
	 iiop_setup_connection_timeout/0, objectkeys_gc_time/0,
	 is_lightweight/0, get_lightweight_nodes/0,
	 start_lightweight/0, start_lightweight/1,
	 get_ORBDefaultInitRef/0, get_ORBInitRef/0,
	 get_interceptors/0, get_local_interceptors/0, 
	 get_cached_interceptors/0, set_interceptors/1,
	 jump_start/0, jump_start/1, jump_stop/0,
	 iiop_connections/0, iiop_connections/1, iiop_connections_pending/0, 
	 typechecking/0,
	 exclude_codeset_ctx/0, exclude_codeset_component/0, bidir_context/0, use_FT/0,
	 use_CSIv2/0, get_flags/0, secure/0, multi_jump_start/1, multi_jump_start/2, 
	 multi_jump_start/3, get_tables/0, iiop_in_connection_timeout/0, 
	 partial_security/0, nat_iiop_ssl_port/0, nat_iiop_port/0, ip_version/0,
	 light_ifr/0, iiop_max_in_requests/0, iiop_max_in_connections/0, 
	 iiop_max_fragments/0, iiop_backlog/0, iiop_ssl_backlog/0,
	 find_sockname_by_peername/2, find_peername_by_sockname/2, iiop_acl/0,
	 add_listen_interface/2, add_listen_interface/3, remove_listen_interface/1,
	 reconfigure_out_connections/1, 
	 reconfigure_out_connection/3, reconfigure_out_connection/4, 
	 reconfigure_in_connections/1, reconfigure_in_connection/2,
	 activate_audit_trail/0, activate_audit_trail/1, deactivate_audit_trail/0,
	 iiop_ssl_ip_address_local/0, ip_address_local/0,
	 close_connection/1, close_connection/2, is_system_exception/1,
	 exception_info/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([nat_host/0, host/0, ip_address_variable_defined/0, start/2, init/1,
	 get_debug_level/0, debug_level_print/3, dbg/3, error/3,
	 configure/2, configure_override/2, multi_configure/1,
	 mjs/1, mjs/2, js/0, js/1]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when,
%% for example, installing Orber.
-record(options, {ifr_storage_type = disc_copies,
		  install_timeout = infinity,
		  local_content = false,
		  nameservice_storage_type = ram_copies,
		  initialreferences_storage_type = ram_copies,
		  type = temporary,
		  load_order = 0}).

-define(ORBER_TABS, [orber_CosNaming, orber_objkeys, orber_references]).

-define(DEBUG_LEVEL, 5).

-define(FORMAT(_F, _A), lists:flatten(io_lib:format(_F, _A))).
-define(EFORMAT(_F, _A), exit(lists:flatten(io_lib:format(_F, _A)))).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

jump_stop() ->
    stop(),
    uninstall(),
    mnesia:stop().

js() ->
    application:load(orber),
    jump_start([{iiop_port, iiop_port()},
		{interceptors, {native, [orber_iiop_tracer_silent]}},
		{orber_debug_level, 10}, 
		{flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

js(Port) when is_integer(Port) ->
    application:load(orber),
    jump_start([{iiop_port, Port},
		{interceptors, {native, [orber_iiop_tracer_silent]}},
		{orber_debug_level, 10}, 
		{flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

jump_start() ->
    application:load(orber),
    jump_start([{iiop_port, iiop_port()}]).


jump_start(Port) when is_integer(Port) ->
    application:load(orber),
    jump_start([{iiop_port, Port}]);
jump_start(Options) when is_list(Options) ->
    application:load(orber),
    mnesia:start(),
    Port = case lists:keysearch(iiop_port, 1, Options) of
	       {value, {iiop_port, Value}} ->
		   Value;
	       _ ->
		   iiop_port()
	   end,
    corba:orb_init([{iiop_port, Port}|Options]),
    install([node()], [{ifr_storage_type, ram_copies}]),
    start(),
    %% We need to use this operation if Port == 0 to see what the OS
    %% assigned.
    NewPort = orber_env:iiop_port(),
    Domain = orber_env:ip_address() ++ [$:|integer_to_list(NewPort)],
    orber_env:configure_override(domain, Domain),
    info();
jump_start(Options) ->
    exit({error, Options}). 


mjs(Nodes) ->
    application:load(orber),
    multi_js_helper(Nodes, iiop_port(),
		    [{interceptors, {native, [orber_iiop_tracer_silent]}},
		     {orber_debug_level, 10}, 
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

mjs(Nodes, Port) ->
    application:load(orber),
    multi_js_helper(Nodes, Port, 
		    [{interceptors, {native, [orber_iiop_tracer_silent]}},
		     {orber_debug_level, 10},
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).


multi_jump_start(Nodes) ->
    application:load(orber),
    multi_js_helper(Nodes, iiop_port(), []).

multi_jump_start(Nodes, Port) ->
    multi_js_helper(Nodes, Port, []).

multi_jump_start(Nodes, Port, Options) ->
    multi_js_helper(Nodes, Port, Options).

multi_js_helper(Nodes, Port, InitOptions) when is_list(Nodes) andalso 
					       is_integer(Port) andalso
					       is_list(InitOptions) ->
    %% We MUST delete the option iiop_port.
    Options = lists:keydelete(iiop_port, 1, InitOptions),
    case node() of
	nonode@nohost ->
	    {error, "The distribution is not started"};
	_ ->
	    mnesia:start(),
	    corba:orb_init([{iiop_port, Port}|Options]),
	    install([node()], [{ifr_storage_type, ram_copies}]),
	    start(),
	    NewPort = orber_env:iiop_port(),
	    Domain = orber_env:ip_address() ++ [$:|integer_to_list(NewPort)],
	    orber_env:configure_override(domain, Domain),
	    case jump_start_slaves(Nodes, NewPort, 
				   [{domain, Domain}|Options], [], []) of
		{ok, NodeData} ->
		    info(),
		    {ok, [{node(), NewPort}|NodeData]};
		Other ->
		    Other
	    end
    end.

jump_start_slaves([], _, _, [], NodeData) ->
    rpc:multicall([node() | nodes()], global, sync, []),
    {ok, NodeData};
jump_start_slaves([], _, _, Errors, _) ->
    {error, Errors};
jump_start_slaves([{Host, N}|T], Port, Options, Errors, NodeData) ->
    case create_nodes(Host, N, Port, Options, Errors, NodeData) of
	{ok, NewNodeData} ->
	    jump_start_slaves(T, Port, Options, Errors, NewNodeData);
	{error, NewErrors} ->
	    jump_start_slaves(T, Port, Options, NewErrors, NodeData)
    end;
jump_start_slaves([Host|T], Port, Options, Errors, NodeData) ->
    case catch create_node(Host, Port+1, Options) of
	{ok, NewNode} ->
	    jump_start_slaves(T, Port, Options, Errors, [{NewNode, Port+1}|NodeData]);
	{error, Reason} ->
	    jump_start_slaves(T, Port, Options, [{Host, Port, Reason}|Errors], 
			      NodeData);
	Other ->
	    jump_start_slaves(T, Port, Options, [{Host, Port, Other}|Errors], 
			      NodeData)
    end.

create_nodes(_, 0, _, _, [], NodeData) ->
    {ok, NodeData};
create_nodes(_, 0, _, _, Errors, _) ->
    {error, Errors};
create_nodes(Host, N, Port, Options, Errors, NodeData) ->
    case catch create_node(Host, Port+N, Options) of
	{ok, NewNode} ->
	    create_nodes(Host, N-1, Port, Options, Errors, 
			 [{NewNode, Port+N}|NodeData]);
	{error, Reason} ->
	    create_nodes(Host, N-1, Port, Options, 
			 [{Host, Port+N, Reason}|Errors], NodeData);
	Other ->
	    create_nodes(Host, N-1, Port, Options, 
			 [{Host, Port+N, Other}|Errors], NodeData)
    end.
    

create_node(Host, Port, Options) ->
    case slave:start_link(Host, list_to_atom(integer_to_list(Port))) of
	{ok, NewNode} ->
	    case net_adm:ping(NewNode) of
		pong ->
		    ok = rpc:call(NewNode, mnesia, start, []),
		    {ok,_} = rpc:call(NewNode, mnesia, change_config, [extra_db_nodes, [node()]]),
		    ok = rpc:call(NewNode, corba, orb_init, [[{iiop_port, Port}|Options]]),
		    ok = rpc:call(NewNode, orber, add_node, [NewNode, ram_copies]),
		    {ok, NewNode};
		_ ->
		    {error, "net_adm:ping(Node) failed"}
	    end;
        {error, Reason} ->
            {error, Reason}
    end.


start() ->
    start(temporary).

start(Type) when Type == permanent; Type == temporary ->
    application:start(mnesia),
    TableTest = test_tables(),
    case lists:member(not_member, TableTest) of
	true ->
	    exit({error,"Orber Mnesia Table(s) missing. Orber not properly installed."});
	_->
	    try_starting(Type)
    end.

start_lightweight() ->
    application:start(orber).

start_lightweight(Nodes) when is_list(Nodes) ->
    configure(lightweight, Nodes),
    application:set_env(orber, lightweight, Nodes),
    application:start(orber);
start_lightweight(_) ->
    exit({error,"Argument not correct; must be a list of nodes."}).

stop() ->
    application:stop(orber).


get_tables() ->
    case light_ifr() of
	false ->
	    ?ifr_object_list++?ORBER_TABS;
	true ->
	    ?ifr_light_object_list ++?ORBER_TABS
    end.

iiop_port() ->
    orber_env:iiop_port().

nat_iiop_port() ->
    orber_env:nat_iiop_port().

iiop_out_ports() ->
    orber_env:iiop_out_ports().

iiop_out_ports_random() ->
    orber_env:iiop_out_ports_random().

iiop_out_ports_attempts() ->
    orber_env:iiop_out_ports_attempts().

orber_nodes() ->
    case catch mnesia:table_info(orber_objkeys,ram_copies) of
	Nodes when is_list(Nodes) ->
	    Nodes;
	_ ->
	    [node()]
    end.

domain() -> 
    orber_env:domain().


ip_address_variable_defined() ->
    orber_env:ip_address_variable_defined().


nat_host() ->
    orber_env:nat_host().

host() ->
    orber_env:host().

giop_version() ->
    orber_env:giop_version().

iiop_timeout() ->
    orber_env:iiop_timeout().

iiop_connection_timeout() ->
    orber_env:iiop_connection_timeout().

iiop_setup_connection_timeout() ->
    orber_env:iiop_setup_connection_timeout().

iiop_in_connection_timeout() ->
    orber_env:iiop_in_connection_timeout().
    
find_peername_by_sockname(Host, Port) ->
    orber_iiop_net:sockname2peername(Host, Port) ++ 
	orber_iiop_pm:sockname2peername(Host, Port).

find_sockname_by_peername(Host, Port) ->
    orber_iiop_net:peername2sockname(Host, Port) ++
	orber_iiop_pm:peername2sockname(Host, Port).

%%----------------------------------------------------------------------
%% Function   : iiop_connections
%% Arguments  : Direction   - in | out | inout
%% Returns    : Connections - [{Host, Port}] | [{Host, Port, Interface}] 
%%              Host        - string
%%              Port        - integer
%%              Interface   - string
%% Raises     : 
%% Description: List existing in- and/or out-bound connections.
%%----------------------------------------------------------------------
iiop_connections() ->
    iiop_connections(inout).

iiop_connections(inout) ->
    orber_iiop_pm:list_existing_connections() ++ orber_iiop_net:connections();
iiop_connections(in) ->
    orber_iiop_net:connections();
iiop_connections(out) ->
    orber_iiop_pm:list_existing_connections().

%%----------------------------------------------------------------------
%% Function   : close_connection
%% Arguments  : ObjRef      - #'IOP_IOR'{} | [{Host, Port}] | 
%%              Interface   - string (optional)
%%              Host        - string
%%              Port        - integer
%% Returns    : ok | {'EXCEPTION', #'BAD_PARAM'{}}
%% Raises     : 
%% Description: Close outgoing connections.
%%----------------------------------------------------------------------
close_connection(ObjRef) ->
    close_connection(ObjRef, 0).

close_connection(ObjRef, Interface) when is_record(ObjRef, 'IOP_IOR') ->
    case iop_ior:get_peerdata(ObjRef) of
	[] ->
	    ok;
	PeerData ->
	    orber_iiop_pm:close_connection(PeerData, Interface)
    end;
close_connection(PeerData, Interface) when is_list(PeerData) ->
    orber_iiop_pm:close_connection(PeerData, Interface);
close_connection(What, Interface) ->
    orber:dbg("[~p] orber:close_connection(~p, ~p);~n"
	      "Incorrect type of arguments.", 
	      [?LINE, What, Interface], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).

%%----------------------------------------------------------------------
%% Function   : iiop_connections_pending
%% Arguments  : -
%% Returns    : Connections - [{Host, Port}]
%%              Host        - string
%%              Port        - integer
%% Raises     : 
%% Description: List outbound connections that are being setup. Usefull
%%              when suspecting firewall problems.
%%----------------------------------------------------------------------
iiop_connections_pending() ->
    orber_iiop_pm:list_setup_connections().


iiop_max_fragments() ->
    orber_env:iiop_max_fragments().
    
iiop_max_in_requests() ->
    orber_env:iiop_max_in_requests().

iiop_max_in_connections() ->
    orber_env:iiop_max_in_connections().

iiop_backlog() ->
    orber_env:iiop_backlog().

iiop_acl() ->
    orber_env:iiop_acl().

ip_address_local() ->
    orber_env:ip_address_local().

get_flags() ->
    orber_env:get_flags().

typechecking() ->
    orber_env:typechecking().

exclude_codeset_ctx() ->
    orber_env:exclude_codeset_ctx().

exclude_codeset_component() ->
    orber_env:exclude_codeset_component().

partial_security() ->
    orber_env:partial_security().

use_CSIv2() ->
    orber_env:use_CSIv2().

use_FT() ->
    orber_env:use_FT().

ip_version() ->
    orber_env:ip_version().

light_ifr() ->
    orber_env:light_ifr().

bidir_context() ->
    orber_env:bidir_context().

objectkeys_gc_time() ->
    orber_env:objectkeys_gc_time().


%%-----------------------------------------------------------------
%% CosNaming::NamingContextExt operations
%%-----------------------------------------------------------------
get_ORBInitRef() ->
    orber_env:get_ORBInitRef().

get_ORBDefaultInitRef() ->
    orber_env:get_ORBDefaultInitRef().


%%-----------------------------------------------------------------
%% Interceptor opertaions (see orber_pi.erl)
%%-----------------------------------------------------------------
get_interceptors() ->
    orber_env:get_interceptors().

get_local_interceptors() ->
    orber_env:get_local_interceptors().

get_cached_interceptors() ->
    orber_env:get_cached_interceptors().

set_interceptors(Val) ->
    orber_env:set_interceptors(Val).


%%-----------------------------------------------------------------
%% Light weight Orber operations
%%-----------------------------------------------------------------
is_lightweight() ->
    orber_env:is_lightweight().

get_lightweight_nodes() ->
    orber_env:get_lightweight_nodes().   

%%-----------------------------------------------------------------
%% Security access operations (SSL)
%%-----------------------------------------------------------------
secure() ->
    orber_env:secure().
 
iiop_ssl_backlog() ->
    orber_env:iiop_ssl_backlog().

iiop_ssl_ip_address_local() ->
    orber_env:iiop_ssl_ip_address_local().

iiop_ssl_port() ->
    orber_env:iiop_ssl_port().

nat_iiop_ssl_port() ->
    orber_env:nat_iiop_ssl_port().

ssl_server_options() ->
        orber_env:ssl_server_options().

ssl_client_options() ->
        orber_env:ssl_client_options().

set_ssl_client_options(Value) ->
        orber_env:set_ssl_client_options(Value).

ssl_server_certfile() ->
    orber_env:ssl_server_certfile().
    
ssl_client_certfile() ->
    orber_env:ssl_client_certfile().

set_ssl_client_certfile(Value) ->
    orber_env:set_ssl_client_certfile(Value).
    
ssl_server_verify() ->
    orber_env:ssl_server_verify().
    
ssl_client_verify() ->
    orber_env:ssl_client_verify().

set_ssl_client_verify(Value) ->
    orber_env:set_ssl_client_verify(Value).

ssl_server_depth() ->
    orber_env:ssl_server_depth().

ssl_client_depth() ->
    orber_env:ssl_client_depth().

set_ssl_client_depth(Value) ->
    orber_env:set_ssl_client_depth(Value).

ssl_server_cacertfile() ->
    orber_env:ssl_server_cacertfile().
    
ssl_client_cacertfile() ->
    orber_env:ssl_client_cacertfile().

set_ssl_client_cacertfile(Value) ->
    orber_env:set_ssl_client_cacertfile(Value).
    
ssl_client_password() ->
    orber_env:ssl_client_password().

ssl_server_password() ->
    orber_env:ssl_server_password().

ssl_client_keyfile() ->
    orber_env:ssl_client_keyfile().

ssl_server_keyfile() ->
    orber_env:ssl_server_keyfile().

ssl_client_ciphers() ->
    orber_env:ssl_client_ciphers().

ssl_server_ciphers() ->
    orber_env:ssl_server_ciphers().

ssl_client_cachetimeout() ->
    orber_env:ssl_client_cachetimeout().

ssl_server_cachetimeout() ->
    orber_env:ssl_server_cachetimeout().

%%----------------------------------------------------------------------
%% Function   : activate_audit_trail
%% Arguments  : Verbosity - stealth | normal | verbose
%% Returns    : -
%% Raises     : 
%% Description: Activate the appropriate interceptor for the requested direction(s).
%%----------------------------------------------------------------------
activate_audit_trail() ->
    activate_audit_trail(normal).

activate_audit_trail(stealth) ->
    do_activate(orber_iiop_tracer_stealth);
activate_audit_trail(verbose) ->
    do_activate(orber_iiop_tracer);
activate_audit_trail(_) ->
    do_activate(orber_iiop_tracer_silent).

do_activate(Interceptor) ->
    Options =
	case orber_env:get_interceptors() of
	    {native, PIs} ->
		[{interceptors, 
		  {native, [Interceptor|remove_built_in_interceptors(PIs, [])]}}];
	    _ ->
		[{interceptors, {native, [Interceptor]}}]
	end,
    reconfigure_in_connections(Options),
    reconfigure_out_connections(Options).

remove_built_in_interceptors([orber_iiop_tracer_stealth|T], Acc) ->
    remove_built_in_interceptors(T, Acc);
remove_built_in_interceptors([orber_iiop_tracer|T], Acc) ->
    remove_built_in_interceptors(T, Acc);
remove_built_in_interceptors([orber_iiop_tracer_silent|T], Acc) ->
    remove_built_in_interceptors(T, Acc);
remove_built_in_interceptors([H|T], Acc) ->
    remove_built_in_interceptors(T, [H|Acc]);
remove_built_in_interceptors([], Acc) ->
    %% We must use the same order as defined by the interceptors parameter
    lists:reverse(Acc).

%%----------------------------------------------------------------------
%% Function   : deactivate_audit_trail
%% Arguments  : -
%% Returns    : -
%% Raises     : 
%% Description: Dectivate interceptors for the requested direction(s).
%%----------------------------------------------------------------------
deactivate_audit_trail() ->
    Options 
	= case orber_env:get_interceptors() of
	      {native, PIs} ->
		  [{interceptors, {native, PIs}}];
	      _ ->
		  [{interceptors, false}]
	  end,
    reconfigure_in_connections(Options),
    reconfigure_out_connections(Options).

%%----------------------------------------------------------------------
%% Function   : add_listen_interface
%% Arguments  : IP   - string
%%              Type - normal | ssl
%%              Port - integer > 0
%%              Options - [{Key, Value}]
%%              Key - atom() valid configuration parameter
%%              Value - a valid value for the given Key
%% Returns    : #Ref
%% Raises     : 
%% Description: Add a new listen process, which will accept new incoming
%%              connections.
%%----------------------------------------------------------------------
add_listen_interface(IP, normal) ->
    orber_iiop_net:add(IP, normal, [{iiop_port, orber_env:iiop_port()}]);
add_listen_interface(IP, ssl) ->
    orber_iiop_net:add(IP, ssl, [{iiop_ssl_port, orber_env:iiop_ssl_port()}]).

add_listen_interface(IP, normal, Port) when is_integer(Port) andalso Port > 0 ->
    orber_iiop_net:add(IP, normal, [{iiop_port, Port}]);
add_listen_interface(IP, ssl, Port) when is_integer(Port) andalso Port > 0 ->
    orber_iiop_net:add(IP, ssl, [{iiop_ssl_port, Port}]);
add_listen_interface(IP, Type, Options) when is_list(Options) ->
    orber_iiop_net:add(IP, Type, Options);
add_listen_interface(IP, Type, Port) when is_integer(Port) ->
    orber:dbg("[~p] orber:add_listen_interface(~p, ~p, ~p);~n"
	      "The port number must be greater than 0.", 
	      [?LINE, IP, Type, Port], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO});
add_listen_interface(IP, Type, Extra) ->
    orber:dbg("[~p] orber:add_listen_interface(~p, ~p, ~p);~n"
	      "Incorrect argument(s).", 
	      [?LINE, IP, Type, Extra], ?DEBUG_LEVEL),
    corba:raise(#'BAD_PARAM'{completion_status=?COMPLETED_NO}).
    
%%----------------------------------------------------------------------
%% Function   : remove_listen_interface
%% Arguments  : Ref - #Ref
%% Returns    : #Ref
%% Raises     : 
%% Description: Terminate the listen process and all related inproxies
%%              associated with the supplied reference.
%%----------------------------------------------------------------------
remove_listen_interface(Ref) ->
    orber_iiop_net:remove(Ref).

%%----------------------------------------------------------------------
%% Function   : reconfigure_out_connections
%% Arguments  : Options - see corba:orb_init
%% Returns    : ok | {error, Reason}
%% Raises     : 
%% Description: Reconfigure the behavior of all outgoing IIOP connections.
%%----------------------------------------------------------------------
reconfigure_out_connections(Options) ->
    orber_iiop_pm:reconfigure(Options).

%%----------------------------------------------------------------------
%% Function   : reconfigure_out_connections
%% Arguments  : Options - see corba:orb_init
%%              Host      - string()
%%              Port      - integer()
%%              Interface - string
%% Returns    : ok | {error, Reason}
%% Raises     : 
%% Description: Reconfigure the behavior of all outgoing connections.
%%----------------------------------------------------------------------
reconfigure_out_connection(Options, Host, Port) ->
    orber_iiop_pm:reconfigure(Options, Host, Port).
reconfigure_out_connection(Options, Host, Port, Interface) ->
    orber_iiop_pm:reconfigure(Options, Host, Port, Interface).

%%----------------------------------------------------------------------
%% Function   : reconfigure_in_connections
%% Arguments  : Options - see corba:orb_init
%% Returns    : ok | {error, Reason}
%% Raises     : 
%% Description: Reconfigure the behavior of all incoming IIOP connections.
%%----------------------------------------------------------------------
reconfigure_in_connections(Options) ->
    orber_iiop_net:reconfigure(Options).

%%----------------------------------------------------------------------
%% Function   : reconfigure_in_connections
%% Arguments  : Options - see corba:orb_init
%%              Ref       - The #Ref returned by add_listen_interface/2/3
%% Returns    : ok | {error, Reason}
%% Raises     : 
%% Description: Reconfigure the behavior of all incoming IIOP connections.
%%----------------------------------------------------------------------
reconfigure_in_connection(Options, Ref) ->
    orber_iiop_net:reconfigure(Options, Ref).


%%-----------------------------------------------------------------
%% Configuration settings
%%-----------------------------------------------------------------
info() ->
    orber_env:info().

info(IoDevice) ->
    orber_env:info(IoDevice).

%%-----------------------------------------------------------------
%% EXCEPTION mapping
%%-----------------------------------------------------------------
exception_info(Exc) ->
    orber_exceptions:dissect(Exc).

is_system_exception(Exc) ->
    orber_exceptions:is_system_exception(Exc).

%%-----------------------------------------------------------------
%% Installation interface functions
%%-----------------------------------------------------------------
install(Nodes) ->
    install(Nodes, []).

install([], Options) ->
    install([node()], Options);
install(Nodes, Options) when is_list(Nodes) andalso is_list(Options)->
    case orber_tb:is_running() of
	false ->
	    application:load(orber),
	    case mnesia:system_info(is_running) of
		no ->
		    application:start(mnesia),
		    Outcome = install_orber(Nodes, Options),
		    application:stop(mnesia),
		    Outcome;
		yes ->
		    install_orber(Nodes, Options)
	    end;
	_ ->
	    exit({error, "Orber is already running on this node."})
    end.



install_orber(Nodes, Options) ->
    #options{ifr_storage_type = IFRType, install_timeout = Timeout,
	     local_content = LocalContent, nameservice_storage_type = NSType,
	     initialreferences_storage_type = InitType,
	     load_order = LoadOrder}
	= check_options(Options, #options{}),
    MnesiaOptions = [{local_content, LocalContent}, 
		     {load_order, LoadOrder}],
    TableTest = test_tables(),
    case lists:member(is_member, TableTest) of
	true ->
	    case LocalContent of
		true ->
		    orber_ifr:initialize(Timeout, {localCopy,IFRType}, 
					 light_ifr());
		_->
		    exit("Orber Mnesia Table(s) already exist. Cannot install Orber.")
	    end;
	_ ->
	    orber_ifr:initialize(Timeout, [{IFRType, Nodes} |MnesiaOptions], 
				 light_ifr())
    end,
    orber_objectkeys:install(Timeout, [{ram_copies, Nodes} |MnesiaOptions]),
    'CosNaming_NamingContextExt_impl':install(Timeout, [{NSType, Nodes} |MnesiaOptions]),
    orber_initial_references:install(Timeout, [{InitType, Nodes} |MnesiaOptions]),
    oe_cos_naming:oe_register(),
    oe_cos_naming_ext:oe_register(),
    oe_erlang:oe_register(),
    oe_OrberIFR:oe_register(),
    oe_CORBA:oe_register(),
    case NSType of
	ram_copies ->
	    case mnesia:dump_tables(['orber_CosNaming']) of
		{atomic, ok} ->
		    ok;
		{aborted, {has_no_disc,_}} ->
		    ok;
		{aborted, Reason} ->
		    ?EFORMAT("Unable to dump mnesia tables: ~p", [Reason])
	    end;
	_ ->
	    ok
    end.

check_options([], Options) ->
    Options;
check_options([{ifr_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{ifr_storage_type = Type}); 
check_options([{nameservice_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{nameservice_storage_type = Type}); 
check_options([{initialreferences_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{initialreferences_storage_type = Type}); 
check_options([{install_timeout, Timeout}|T], Options) 
  when Timeout == infinity orelse is_integer(Timeout) ->
    check_options(T, Options#options{install_timeout = Timeout});
check_options([{local_content, Bool}|T], Options) 
  when Bool == true; Bool == false ->
    check_options(T, Options#options{local_content = Bool});
check_options([{type, Type}|T], Options) 
  when Type == temporary; Type == permanent ->
    check_options(T, Options#options{type = Type});
check_options([{load_order, LoadOrder}|T], Options) 
  when is_integer(LoadOrder) ->
    check_options(T, Options#options{load_order = LoadOrder});
check_options([H|_], _) ->
    ?EFORMAT("Option unknown or incorrect value: ~w", [H]).

  

try_starting(Type) ->
    case application:start(orber, Type) of
	ok ->
	    case partial_security() of
		true ->
		    error_logger:warning_msg(
		      "=================== Orber =================~n"
		      "*******************************************~n"
		      "**** WARNING - WARNING - WARNING **********~n"
		      "**** WARNING - WARNING - WARNING **********~n"
		      "**** WARNING - WARNING - WARNING **********~n"
		      "**** WARNING - WARNING - WARNING **********~n"
		      "*******************************************~n"
		      "  ORBER STARTED WITH AN INSECURE OPTION:~n"
		      " ~n"
		      "             {flags, ~p}~n"
		      " ~n"
		      " THIS OPTION MAY ONLY BE USED DURING TESTS~n"
		      " ~n"
		      "===========================================~n", 
		      [?ORB_ENV_PARTIAL_SECURITY]),
		    ok;
		false ->
		    ok
	    end;
	{error,{already_started,orber}} ->
	    {error,{already_started,orber}};
	Reason ->
	    dbg("[~p] orber:try_starting(~p) failed: ~n~p", 
		[?LINE, Type, Reason], ?DEBUG_LEVEL),
	    {error, "Unable to start Orber. Is the listen port vacant?"}
	end.

test_tables() ->
    AllTabs = mnesia:system_info(tables),
    lists:map(fun(Tab) ->
		      case lists:member(Tab,AllTabs) of
			  false ->
			      not_member;
			  _ ->
			      is_member
		      end
	      end,
	      get_tables()).

%%-----------------------------------------------------------------
%% UnInstallation interface functions
%%-----------------------------------------------------------------
uninstall() ->
    orber_objectkeys:stop_all(),
    application:stop(orber),
    delete_orber_tables(get_tables()).

delete_orber_tables([]) -> ok;
delete_orber_tables([Tab1|Rest]) ->
    mnesia:delete_table(Tab1),
    delete_orber_tables(Rest).

%%-----------------------------------------------------------------
%% Add and remove node interface functions
%%-----------------------------------------------------------------
add_node(Node, StorageType) when is_atom(Node) andalso is_atom(StorageType) ->
    add_node(Node, [{ifr_storage_type, StorageType}]);
add_node(Node, OptionList) when is_atom(Node) andalso is_list(OptionList) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	{badrpc, Reason} ->
	    ?EFORMAT("Node ~p do not respond. add_node/2 failed: ~p", 
		     [Node, Reason]);
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		false ->
		    %% We need to "load" orber to make sure that
		    %% application environment variables is loaded.
		    rpc:call(Node, application, load, [orber]),
		    Options = check_options(OptionList, #options{}),
		    case rpc:call(Node, orber, light_ifr, []) of
			false ->
			    copy_tables(?ifr_object_list, Node, Options);
			true ->
			    copy_tables(?ifr_light_object_list, Node, Options)
		    end;
		true ->
		    ?EFORMAT("Orber is already running on ~p. add_node failed.",
			     [Node]);
		Reason ->
		    ?EFORMAT("Unable to reach node ~p. add_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	no ->
	    ?EFORMAT("Mnesia not running on node ~p. add_node/2 failed.",
		     [Node]);
	starting ->
	    ?EFORMAT("Mnesia not fully started on node ~p. add_node/2 failed.",
		     [Node]);
	stopping ->
	    ?EFORMAT("Mnesia stopping  on node ~p. add_node/2 failed.", [Node])
    end.

%% We have to copy the tables in two steps, i.e., orber tables should be ram_copies
%% while the user may choose to install the rest as disc_copies.
copy_tables([], Node, Options) ->
    copy_orber_tables(?ORBER_TABS, Node, Options);
copy_tables([T1|Trest], Node, Options) ->
    case mnesia:add_table_copy(T1, Node, Options#options.ifr_storage_type) of
	{atomic, ok} ->
	    copy_tables(Trest, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy IFR table(s): ~p", 
		     [mnesia:error_description(Reason), [T1|Trest]])
    end.

copy_orber_tables([], Node, Options) ->
    case rpc:call(Node, application, start, [orber, Options#options.type]) of
	ok ->
	    ok;
	Reason ->
	    ?EFORMAT("All tables installed but failed to start orber on node ~p: ~p",
		     [Node, Reason])
    end;
copy_orber_tables([orber_CosNaming|TTail], Node, Options) ->
    case mnesia:add_table_copy(orber_CosNaming, Node, 
			       Options#options.nameservice_storage_type) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [orber_CosNaming|TTail]])
    end;
copy_orber_tables([orber_references|TTail], Node, Options) ->
    case mnesia:add_table_copy(orber_references, Node, 
			       Options#options.initialreferences_storage_type) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [orber_references|TTail]])
    end;
copy_orber_tables([THead|TTail], Node, Options) ->
    case mnesia:add_table_copy(THead, Node, ram_copies) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [THead|TTail]])
    end.

remove_node(Node) when is_atom(Node) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		true ->
		    rpc:call(Node, orber, stop, []),
		    remove_tables(get_tables(), Node);
		false ->
		    remove_tables(get_tables(), Node);
		Reason ->
		    ?EFORMAT("Unable to reach node: ~p. remove_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	no ->
	    case rpc:call(Node, mnesia, start, []) of
		ok ->
		    remove_tables(get_tables(), Node),
		    rpc:call(Node, mnesia, stop, []);
		Reason ->
		    ?EFORMAT("Unable to reach node: ~p. remove_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	Reason ->
	    ?EFORMAT("Problem with ~p. remove_node/1 failed: ~p", [Node, Reason])
    end.


remove_tables(Tables, Node) ->
    case remove_tables(Tables, Node, []) of
	ok ->
	    ok;
	{error, Node, Failed} ->
	    ?EFORMAT("orber:remove_node(~p) failed. Unable to remove table(s): ~p", 
		     [Node, Failed])
    end.

remove_tables([], _, []) -> 
    ok;
remove_tables([], Node, Failed) ->
    {error, Node, Failed};
remove_tables([T1|Trest], Node, Failed) ->
    case mnesia:del_table_copy(T1, Node) of
	{atomic, ok} ->
	    remove_tables(Trest, Node, Failed);
	{aborted, Reason} ->
	    remove_tables(Trest, Node, [{T1, Reason}|Failed])
    end.

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%----------------------------------------------------------------------
%% Function   : is_running
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
is_running() ->
    orber_tb:is_running().

%%----------------------------------------------------------------------
%% Function   : check_giop
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
check_giop_version() ->
    case giop_version() of
	{1,0} ->
	    ok;
	{1,1} ->
	    ok;
	{1,2} ->
	    ok;
	X ->
	    X
    end.

%%----------------------------------------------------------------------
%% Function   : dbg
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: Note, dbg replaces debug_level_print.
%%              
%%              The following levels are used (0-10):
%%              10: cdrlib.erl
%%               9: cdr_encode.erl cdr_decode.erl orber_ifr.erl orber_pi.erl
%%               8: orber_iiop_outrequest.erl orber_iiop_inrequest.erl
%%               7: orber_iiop_outproxy.erl orber_iiop_inproxy.erl
%%               6: iop_ior.erl, orber_objectkeys.erl, Orber_IFR_impl.erl orber_socket.erl
%%               5: corba.erl, corba_boa.erl, corba_object.erl
%%               4: Reserved for Cos-services!
%%               3: Reserved for Cos-services!
%%               2: Reserved for client applications!
%%               1: Reserved for client applications!
%%               0: No logging!
%%
%%              A higher value will result in a finer granularity.
%%----------------------------------------------------------------------
get_debug_level() ->
    orber_env:get_debug_level().

debug_level_print(Format, Data, RequestedLevel) ->
    dbg(Format, Data, RequestedLevel).

dbg(Format, Data, RequestedLevel) ->
    case orber_env:get_debug_level() of
	0 ->
	    ok;
	Level when is_integer(Level) andalso Level >= RequestedLevel ->
	    if
		RequestedLevel > 4 ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("=================== Orber =================~n"++
						 Format++
						 "~n===========================================~n",
						 Data);
		RequestedLevel > 2 ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("=========== Orber COS Application =========~n"++
						 Format++
						 "~n===========================================~n",
						 Data);
		true ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("========== Orber Client Application =======~n"++
						 Format++
						 "~n===========================================~n",
						 Data)
	    end,
	    ok;
	_ ->
	    ok
    end.

error(Format, Data, RequestedLevel) ->
    if
	RequestedLevel > 4 ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("=================== Orber =================~n"++
					 Format++
					 "~n===========================================~n",
					 Data);
	RequestedLevel > 2 ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("=========== Orber COS Application =========~n"++
					 Format++
					 "~n===========================================~n",
					 Data);
	true ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("========== Orber Client Application =======~n"++
					 Format++
					 "~n===========================================~n",
					 Data)
    end,
    ok.

configure(Key, Value) ->
    orber_env:configure(Key, Value, check).

configure_override(Key, Value) ->
    orber_env:configure(Key, Value, loaded).

multi_configure(KeyValueList) ->
    orber_env:multi_configure(KeyValueList).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, orber_sup}, orber, orb_init).

init(orb_init) ->
    case check_giop_version() of
	ok ->
	    case is_lightweight() of
		true ->
		    SupFlags = {one_for_one, 5, 1000},  
		    ChildSpec = [
				 {orber_iiop_sup, {orber_iiop, start_sup, [[]]},
				  permanent, 
				  10000, supervisor, [orber_iiop]},
				 {orber_reqno, {orber_request_number, start,
						[[]]},
				  permanent, 
				  10000, worker, [orber_request_number]}
				],
		    {ok, {SupFlags, ChildSpec}};
		false ->
		    case orber_tb:wait_for_tables(get_tables()) of
			ok ->
			    orber_objectkeys:remove_old_keys(),
			    SupFlags = {one_for_one, 5, 1000},
			    ChildSpec = [
					 {orber_iiop_sup, {orber_iiop, start_sup, [[]]},
					  permanent, 
					  10000, supervisor, [orber_iiop]},
					 {orber_init, {orber_initial_references, start,
						       [[]]},
					  permanent, 
					  10000, worker, [orber_initial_references]},
					 {orber_reqno, {orber_request_number, start,
							[[]]},
					  permanent, 
					  10000, worker, [orber_request_number]},
					 {orber_objkeyserver, {orber_objectkeys, start,
							       [[orber_nodes(), 0]]},
					  permanent, 
					  10000, worker, [orber_objectkeys]},
					 {orber_env, {orber_env, start, [[]]},
					  permanent, 10000, worker, [orber_env]}
					],
			    {ok, {SupFlags, ChildSpec}};
			StopReason ->
			    {stop, StopReason}
		    end
	    end;
	X ->
	    {stop, ?FORMAT("GIOP ~p not an implemeted version", [X])}
    end.

