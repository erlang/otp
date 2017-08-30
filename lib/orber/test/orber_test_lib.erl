%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1999-2017. All Rights Reserved.
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

-module(orber_test_lib).
-include_lib("common_test/include/ct.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/include/ifr_types.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include("idl_output/orber_test_server.hrl").
-include_lib("orber/COSS/CosNaming/CosNaming.hrl").

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

-export([js_node/2,
	 js_node/1,
	 js_node/0,
	 slave_sup/0,
	 remote_apply/4,
	 install_test_data/1,
	 light_tests/3,
	 uninstall_test_data/1,
	 destroy_node/2,
	 lookup/2,
	 alternate_iiop_address/2,
	 create_alternate_iiop_address/2,
	 alternate_ssl_iiop_address/3,
	 create_alternate_ssl_iiop_address/3,
	 test_coding/1,
	 test_coding/2,
	 corba_object_tests/2,
	 timeouts/3,
	 precond/3,
	 postcond/4,
	 oe_get_interface/0,
	 create_components_IOR/1,
	 get_options_old/2,
	 get_options_old/3,
	 get_options_old/4,
	 get_options/2,
	 get_options/3,
	 get_options/4,
	 version_ok/0,
	 ssl_version/0,
	 get_loopback_interface/0,
	 get_loopback_interface/1,
	 get_host/0,
	 get_host/1]).

%% Interceptor functions.
-export([new_out_connection/3,
	 new_in_connection/3,
	 closed_in_connection/1,
	 closed_out_connection/1,
	 in_request_encoded/6,
	 in_reply_encoded/6,
	 out_reply_encoded/6,
	 out_request_encoded/6,
	 in_request/6,
	 in_reply/6,
	 out_reply/6,
	 out_request/6]).

%%------------------------------------------------------------
%% function : ssl_version
%% Arguments:
%% Returns  : integer()
%% Effect   :
%%
%%------------------------------------------------------------
ssl_version() ->
    try
	ssl:module_info(),
	case catch erlang:system_info(otp_release) of
	    Version when is_list(Version) ->
		if
		    "R12B" < Version ->
			3;
		    true ->
			2
		end;
	    _ ->
		2
	end
    catch error:undef ->
	    no_ssl
    end.

%%------------------------------------------------------------
%% function : version_ok
%% Arguments:
%% Returns  : true | {skipped, Reason}
%% Effect   :
%%
%%------------------------------------------------------------
version_ok() ->
    {ok, Hostname} = inet:gethostname(),
    case inet:getaddr(Hostname, inet6) of
	{error,nxdomain} ->
	    {skipped, "Inet cannot handle IPv6"};
	_ ->
	    case inet:getaddr("0:0:0:0:0:FFFF:127.0.0.1", inet6) of
		{error,nxdomain} ->
		    {skipped, "Inet cannot handle IPv6"};
		_ ->
		    case gen_tcp:listen(0, [{reuseaddr, true}, inet6]) of
			{ok, LSock} ->
			    {ok, Port} = inet:port(LSock),
			    case gen_tcp:connect(Hostname, Port, [inet6]) of
				{error, _} ->
				    gen_tcp:close(LSock),
				    {skipped, "Inet cannot handle IPv6"};
				{ok, Socket} ->
				    gen_tcp:close(Socket),
				    gen_tcp:close(LSock),
				    true
			    end;
			{error, _} ->
			    {skipped, "Inet cannot handle IPv6"}
		    end
	    end
    end.

%%------------------------------------------------------------
%% function : get_host
%% Arguments: Family - inet | inet6
%% Returns  : string()
%% Effect   :
%%
%%------------------------------------------------------------
get_host() ->
    get_host(inet).
get_host(Family) ->
    case os:type() of
	{win32, _} ->
	    case os:version() of
		{6, _, _} when Family == inet ->
		    "127.0.0.1";
		{6, _, _} ->
		    "0:0:0:0:0:0:0:0001";
		_ ->
		    [IP] = ?match([_], orber:host()),
		    IP
	    end;
	_ ->
	    [IP] = ?match([_], orber:host()),
	    IP
    end.

%%------------------------------------------------------------
%% function : get_loopback_interface
%% Arguments: Family - inet | inet6
%% Returns  : string()
%% Effect   :
%%
%%------------------------------------------------------------
get_loopback_interface() ->
    get_loopback_interface(inet).
get_loopback_interface(Family) ->
    case os:type() of
	{win32, _} ->
	    case os:version() of
		{6, _, _} when Family == inet ->
		    "127.0.0.2";
		{6, _, _} ->
		    "0:0:0:0:0:0:0:0002";
		_ when Family == inet ->
		    "127.0.0.1";
		_ ->
		    "0:0:0:0:0:0:0:0001"
	    end;
	_ when Family == inet ->
	    "127.0.0.1";
	_ ->
	    "0:0:0:0:0:0:0:0001"
    end.

%%------------------------------------------------------------
%% function : js_node/4
%% Arguments: Port - which iiop_port (integer())
%%            InitOptions - [{Key, Value}]
%%            {Type, StartOptions} - {lightweight, [{Key, Value}]}
%% Returns  : {ok, Node} | {error, _}
%% Effect   : Starts a new slave-node with given (optinally)
%%            extra arguments. If fails it retries 'Retries' times.
%%------------------------------------------------------------
js_node() ->
    js_node([], []).

js_node(InitOptions) when is_list(InitOptions) ->
    js_node(InitOptions, []).

js_node(InitOptions, StartOptions) when is_list(InitOptions) ->
    {A,B,C} = erlang:timestamp(),
    [_, Host] = string:tokens(atom_to_list(node()), [$@]),
    _NewInitOptions = check_options(InitOptions),
    js_node_helper(Host, 0, lists:concat([A,'_',B,'_',C]),
		   InitOptions, 10, StartOptions).

js_node_helper(Host, Port, Name, Options, Retries, StartOptions) ->
    case starter(Host, Name, create_paths()) of
	{ok, NewNode} ->
	    case net_adm:ping(NewNode) of
		pong ->
		    start_ssl(lists:member({secure, ssl}, Options), NewNode),
		    {ok, Cwd} = file:get_cwd(),
		    Path = code:get_path(),
		    ok = rpc:call(NewNode, file, set_cwd, [Cwd]),
		    true = rpc:call(NewNode, code, set_path, [Path]),
		    rpc:call(NewNode, application, load, [orber]),
		    ok = rpc:call(NewNode, corba, orb_init,
				  [[{iiop_port, Port},
				    {orber_debug_level, 10}|Options]]),
		    start_orber(StartOptions, NewNode),
		    spawn_link(NewNode, ?MODULE, slave_sup, []),
		    rpc:multicall([node() | nodes()], global, sync, []),
		    ok = rpc:call(NewNode, orber, info, [io]),
		    {ok, NewNode, Host};
		_ ->
		    {error, "net_adm:ping(Node) failed"}
	    end;
        {error, Reason} when Retries == 0 ->
            {error, Reason};
        {error, Reason} ->
            io:format("Could not start slavenode ~p:~p due to: ~p~n",
                      [Host, Port, Reason]),
            timer:sleep(500),
	    js_node_helper(Host, Port, Name, Options, Retries-1, StartOptions)
    end.

check_options(Options) ->
    case {os:type(), os:version()} of
	{{win32, _}, {6, _, _}}  ->
	    %% Vista, need to run additional checks.
	    case {orber_tb:keysearch(ip_address, Options),
		  orber_tb:keysearch(flags, Options, 0)} of
		{undefined, Flags} ->
		    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_IPV6) of
			true ->
			    [{ip_address, get_host(inet6)}|Options];
			false ->
			    [{ip_address, get_host(inet)}|Options]
		    end;
		_ ->
		    Options
	    end;
	_ ->
	    Options
    end.

starter(Host, Name, Args) ->
    io:format("slave:start_link(~p,~p,~p).~n",[Host,Name,Args]),
    slave:start_link(Host, Name, Args).

slave_sup() ->
    process_flag(trap_exit, true),
    receive
        {'EXIT', _, _} -> ignore
    end.

start_ssl(true, Node) ->
    rpc:call(Node, ssl, start, []),
    rpc:call(Node, crypto, start, []);
start_ssl(_, _) ->
    ok.

start_orber({lightweight, Options}, Node) ->
    ok = rpc:call(Node, mnesia, start, []),
    ok = rpc:call(Node, orber, start_lightweight, [Options]);
start_orber(lightweight, Node) ->
    ok = rpc:call(Node, mnesia, start, []),
    ok = rpc:call(Node, orber, start_lightweight, []);
start_orber(_, Node) ->
    ok = rpc:call(Node, orber, jump_start, []).

%%-----------------------------------------------------------------
%% Type    - ssl | iiop_ssl
%% Role    - 'server' | 'client'
%% Options - [{Key, Value}]
%%-----------------------------------------------------------------
get_options_old(Type, Role) ->
    get_options_old(Type, Role, 2, []).

get_options_old(ssl, Role, Level) ->
    get_options_old(ssl, Role, Level, []).

get_options_old(ssl, Role, 2, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{depth, 2},
     {verify, 2},
     {keyfile, filename:join([Dir, Role, "key.pem"])},
     {cacertfile, filename:join([Dir, Role, "cacerts.pem"])},
     {certfile, filename:join([Dir, Role, "cert.pem"])} |Options];
get_options_old(iiop_ssl, _Role, 2, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{ssl_server_depth, 2},
     {ssl_server_verify, 2},
     {ssl_server_certfile, filename:join([Dir, "server", "cert.pem"])},
     {ssl_server_cacertfile, filename:join([Dir, "server", "cacerts.pem"])},
     {ssl_server_keyfile, filename:join([Dir, "server", "key.pem"])},
     {ssl_client_depth, 2},
     {ssl_client_verify, 2},
     {ssl_client_certfile, filename:join([Dir, "client", "cert.pem"])},
     {ssl_client_cacertfile, filename:join([Dir, "client", "cacerts.pem"])},
     {ssl_client_keyfile, filename:join([Dir, "client", "key.pem"])},
     {secure, ssl} |Options];
get_options_old(iiop_ssl, _Role, 1, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{ssl_server_depth, 1},
     {ssl_server_verify, 0},
     {ssl_server_certfile, filename:join([Dir, "server", "cert.pem"])},
     {ssl_server_cacertfile, filename:join([Dir, "server", "cacerts.pem"])},
     {ssl_server_keyfile, filename:join([Dir, "server", "key.pem"])},
     {ssl_client_depth, 1},
     {ssl_client_verify, 0},
     {ssl_client_certfile, filename:join([Dir, "client", "cert.pem"])},
     {ssl_client_cacertfile, filename:join([Dir, "client", "cacerts.pem"])},
     {ssl_client_keyfile, filename:join([Dir, "client", "key.pem"])},
     {secure, ssl} |Options].

get_options(Type, Role) ->
    get_options(Type, Role, 2, []).

get_options(ssl, Role, Level) ->
    get_options(ssl, Role, Level, []).

get_options(ssl, Role, 2, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    Options1 = [{depth, 2},
                {verify, 2},
                {keyfile, filename:join([Dir, Role, "key.pem"])},
                {cacertfile, filename:join([Dir, Role, "cacerts.pem"])},
                {certfile, filename:join([Dir, Role, "cert.pem"])} |Options],
    case Role of
        client ->
            [{server_name_indication, disable} |Options1];
        server ->
            Options1
    end;
get_options(iiop_ssl, _Role, 2, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{ssl_server_options, [{depth, 2},
			{verify, 2},
			{certfile, filename:join([Dir, "server", "cert.pem"])},
			{cacertfile, filename:join([Dir, "server", "cacerts.pem"])},
			{keyfile, filename:join([Dir, "server", "key.pem"])}]},
     {ssl_client_options, [{depth, 2},
                           {verify, 2},
                           {server_name_indication, disable},
                           {certfile, filename:join([Dir, "client", "cert.pem"])},
                           {cacertfile, filename:join([Dir, "client", "cacerts.pem"])},
                           {keyfile, filename:join([Dir, "client", "key.pem"])}]},
     {secure, ssl} |Options];
get_options(iiop_ssl, _Role, 1, Options) ->
    Dir = filename:join([code:lib_dir(ssl), "examples", "certs", "etc"]),
    [{ssl_server_options, [{depth, 1},
			{verify, 0},
			{certfile, filename:join([Dir, "server", "cert.pem"])},
			{cacertfile, filename:join([Dir, "server", "cacerts.pem"])},
			{keyfile, filename:join([Dir, "server", "key.pem"])}]},
     {ssl_client_options, [{depth, 1},
                           {verify, 0},
                           {server_name_indication, disable},
                           {certfile, filename:join([Dir, "client", "cert.pem"])},
                           {cacertfile, filename:join([Dir, "client", "cacerts.pem"])},
                           {keyfile, filename:join([Dir, "client", "key.pem"])}]},
     {secure, ssl} |Options].

create_paths() ->
    Path = filename:dirname(code:which(?MODULE)),
    " -pa " ++ Path ++ " -pa " ++
        filename:join(Path, "idl_output") ++
	" -pa " ++
        filename:join(Path, "all_SUITE_data") ++
        " -pa \"" ++
        filename:dirname(code:which(orber))++"\"".

%%------------------------------------------------------------
%% function : destroy_node
%% Arguments: Node - which node to destroy.
%%            Type - normal | ssl
%% Returns  :
%% Effect   :
%%------------------------------------------------------------

destroy_node(Node, Type) ->
    stopper(Node, Type).

stopper(Node, _Type) ->
    slave:stop(Node).


%%------------------------------------------------------------
%% function : remote_apply
%% Arguments: N - Node, M - Module,
%%            F - Function, A - Arguments (list)
%% Returns  :
%% Effect   :
%%------------------------------------------------------------
remote_apply(N, M,F,A) ->
    case rpc:call(N, M, F, A) of
	{badrpc, Reason} ->
	    exit(Reason);
	Other ->
	    Other
    end.



%%------------------------------------------------------------
%% function : install_test_data
%% Arguments: WhichSuite
%% Returns  : ok
%% Effect   : Installs test data associated with 'WhichSuite'
%%------------------------------------------------------------

install_test_data(nameservice) ->
    oe_orber_test_server:oe_register(),
    Mamba = orber_test_server:oe_create([], [{regname, {local, mamba}}]),
    true = corba:add_initial_service("Mamba", Mamba),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    'CosNaming_NamingContext':bind(NS, N,Mamba);

install_test_data({nameservice, AltAddr, AltPort}) ->
    oe_orber_test_server:oe_register(),
    Obj = orber_test_server:oe_create([], [{regname, {local, mamba}}]),
    Mamba = corba:add_alternate_iiop_address(Obj, AltAddr, AltPort),
    true = corba:add_initial_service("Mamba", Mamba),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    'CosNaming_NamingContext':bind(NS, N,Mamba);

install_test_data(timeout) ->
    oe_orber_test_server:oe_register(),
    Mamba = orber_test_server:oe_create([], {local, mamba}),
    Viper = orber_test_timeout_server:oe_create([], {local, viper}),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N1 = lname:insert_component(lname:create(), 1, NC1),
    NC2 = lname_component:set_id(lname_component:create(), "viper"),
    N2 = lname:insert_component(lname:create(), 1, NC2),
    'CosNaming_NamingContext':bind(NS, N1, Mamba),
    'CosNaming_NamingContext':bind(NS, N2, Viper);

install_test_data(pseudo) ->
    oe_orber_test_server:oe_register(),
    Mamba = orber_test_server:oe_create([], [{pseudo,true}]),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    'CosNaming_NamingContext':bind(NS, N,Mamba);

install_test_data(ssl) ->
    oe_orber_test_server:oe_register(),
    Mamba = orber_test_server:oe_create([], [{regname, {local, mamba}}]),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    'CosNaming_NamingContext':bind(NS, N,Mamba);

install_test_data(ssl_simple) ->
    oe_orber_test_server:oe_register();

install_test_data(light) ->
    %% Nothing to do at the moment but we might in the future
    ok;

install_test_data(_) ->
    {error, "no_implement"}.


%%------------------------------------------------------------
%% function : uninstall_test_data
%% Arguments: WhichSuite
%% Returns  : ok
%% Effect   : Uninstalls test data associated with 'WhichSuite'
%%------------------------------------------------------------

uninstall_test_data(pseudo) ->
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    _Obj = (catch 'CosNaming_NamingContext':resolve(NS, N)),
    catch 'CosNaming_NamingContext':destroy(NS),
    oe_orber_test_server:oe_unregister();

uninstall_test_data(timeout) ->
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N1 = lname:insert_component(lname:create(), 1, NC1),

    NC2 = lname_component:set_id(lname_component:create(), "viper"),
    N2 = lname:insert_component(lname:create(), 1, NC2),
    Mamba = (catch 'CosNaming_NamingContext':resolve(NS, N1)),
    Viper = (catch 'CosNaming_NamingContext':resolve(NS, N2)),
    catch corba:dispose(Mamba),
    catch corba:dispose(Viper),
    catch 'CosNaming_NamingContext':destroy(NS),
    oe_orber_test_server:oe_unregister();

uninstall_test_data(nameservice) ->
    true = corba:remove_initial_service("Mamba"),
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    Obj = (catch 'CosNaming_NamingContext':resolve(NS, N)),
    catch corba:dispose(Obj),
    catch 'CosNaming_NamingContext':destroy(NS),
    oe_orber_test_server:oe_unregister();

uninstall_test_data(ssl) ->
    NS = corba:resolve_initial_references("NameService"),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N = lname:insert_component(lname:create(), 1, NC1),
    Obj = (catch 'CosNaming_NamingContext':resolve(NS, N)),
    catch corba:dispose(Obj),
    catch 'CosNaming_NamingContext':destroy(NS),
    oe_orber_test_server:oe_unregister();

uninstall_test_data(ssl_simple) ->
    oe_orber_test_server:oe_unregister();

uninstall_test_data(light) ->
    %% Nothing to do at the moment but we might in the future
    ok;

uninstall_test_data(_) ->
    {error, "no_implement"}.

%%------------------------------------------------------------
%% function : corba_object_tests
%% Arguments: TestServerObj a orber_test_server ref
%%            OtherObj - any other Orber object.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------

corba_object_tests(TestServerObj, OtherObj) ->
    ?match(false,
	   corba_object:is_a(TestServerObj, "IDL:orber_parent/inherrit:1.0")),
    ?match(true,
	   corba_object:is_a(TestServerObj, "IDL:omg.org/orber_parent/inherrit:1.0")),
    ?match(true,
	   corba_object:is_a(TestServerObj, "IDL:omg.org/orber_test/server:1.0")),
    ?match(false,
	   corba_object:is_a(TestServerObj, "IDL:orber_test/server:1.0")),
    ?match(false,
	   corba_object:is_a(TestServerObj, "IDL:omg.org/orber_parent/inherrit:1.1")),
    ?match(false,
	   corba_object:is_a(TestServerObj, "NotValidIFRID")),
    ?match(false,
	   corba_object:is_nil(TestServerObj)),
    ?match(false,
	   corba_object:is_equivalent(OtherObj,TestServerObj)),
    ?match(true,
	   corba_object:is_equivalent(TestServerObj,TestServerObj)),
    ?match(false, corba_object:non_existent(TestServerObj)),
    ?match(false, corba_object:not_existent(TestServerObj)),
    ?match(#fullinterfacedescription{}, corba_object:get_interface(TestServerObj)),

    ok.

%%------------------------------------------------------------
%% function : lookup
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------

lookup(Host, Port) ->
    Key = Host++":"++integer_to_list(Port),
    NSR = corba:resolve_initial_references_remote("NameService",
						  ["iiop://"++Key]),

    NC1 = lname_component:set_id(lname_component:create(), "not_exist"),
    N1 =  lname:insert_component(lname:create(), 1, NC1),
    ?match({'EXCEPTION',{'CosNaming_NamingContext_NotFound',_,_,_}},
	   'CosNaming_NamingContext':resolve(NSR, N1)),

    NC2 = lname_component:set_id(lname_component:create(), "mamba"),
    N2  = lname:insert_component(lname:create(), 1, NC2),
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 'CosNaming_NamingContext':resolve(NSR, N2)),
    orber_test_server:print(Obj),
    Obj2 = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		  corba:string_to_object("corbaname:iiop:1.1@"++Key++"/NameService#mamba")),

    orber_test_server:print(Obj2),

    NSR2 = ?match({'IOP_IOR',"IDL:omg.org/CosNaming/NamingContextExt:1.0",_},
		  corba:string_to_object("corbaloc:iiop:1.1@"++Key++"/NameService")),
    Obj3 = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		  'CosNaming_NamingContext':resolve(NSR2, N2)),
    orber_test_server:print(Obj3).

%%------------------------------------------------------------
%% function : alternate_iiop_address
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------
alternate_iiop_address(Host, Port) ->
    IOR = create_alternate_iiop_address(Host, Port),

    ?match(false, corba_object:non_existent(IOR)),
    ?match({'object_forward',_}, corba:locate(IOR)),
    ?match({'object_forward',_}, corba:locate(IOR, 10000)),
    ok.

%%------------------------------------------------------------
%% function : create_alternate_iiop_address
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------
create_alternate_iiop_address(Host, Port) ->
    MC = [#'IOP_TaggedComponent'{tag = ?TAG_ORB_TYPE,
				 component_data = ?ORBER_ORB_TYPE_1},
	  #'IOP_TaggedComponent'{tag = ?TAG_CODE_SETS,
				 component_data = ?DEFAULT_CODESETS},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = Port}},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = 8000}},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = 8000}}],
    #'IOP_IOR'{type_id=TypeID,
	       profiles=P1} = _IORA = iop_ior:create({1,2},
						     "IDL:omg.org/CosNaming/NamingContextExt:1.0",
						     [Host], 8000, -1,
						     "NameService", MC, 0, 0),
    #'IOP_IOR'{profiles=P2} = _IORB = iop_ior:create({1,1},
						     "IDL:omg.org/CosNaming/NamingContextExt:1.0",
						     [Host], 8000, -1,
						     "NameService", [], 0, 0),
    #'IOP_IOR'{type_id=TypeID, profiles=P2++P1}.


%%------------------------------------------------------------
%% function : create_components_IOR
%% Arguments:
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------
create_components_IOR(Version) ->
    MC = [#'IOP_TaggedComponent'{tag = ?TAG_ORB_TYPE,
				 component_data = ?ORBER_ORB_TYPE_1},
	  #'IOP_TaggedComponent'{tag = ?TAG_CODE_SETS,
				 component_data = ?DEFAULT_CODESETS},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = "127.0.0.1",
				   'Port' = 4001}},
	  #'IOP_TaggedComponent'{tag = ?TAG_SSL_SEC_TRANS,
				 component_data = #'SSLIOP_SSL'{target_supports = 0,
								target_requires = 1,
								port = 2}},
	  #'IOP_TaggedComponent'{tag = ?TAG_FT_GROUP,
				 component_data =
				 #'FT_TagFTGroupTaggedComponent'
				 {version = #'GIOP_Version'{major = 1,
							    minor = 2},
				  ft_domain_id = "FT_FTDomainId",
				  object_group_id = ?ULONGLONGMAX,
				  object_group_ref_version = ?LONGMAX}},
	  #'IOP_TaggedComponent'{tag = ?TAG_FT_PRIMARY,
				 component_data =
				 #'FT_TagFTPrimaryTaggedComponent'{primary = true}},
	  #'IOP_TaggedComponent'{tag = ?TAG_FT_HEARTBEAT_ENABLED,
				 component_data =
				 #'FT_TagFTHeartbeatEnabledTaggedComponent'{heartbeat_enabled = true}},
	  #'IOP_TaggedComponent'{tag = ?TAG_CSI_SEC_MECH_LIST,
				 component_data =
				 #'CSIIOP_CompoundSecMechList'
				 {stateful = false,
				  mechanism_list =
				  [#'CSIIOP_CompoundSecMech'
				   {target_requires = 6,
				    transport_mech =
				    #'IOP_TaggedComponent'
				    {tag=?TAG_TLS_SEC_TRANS,
				     component_data=#'CSIIOP_TLS_SEC_TRANS'
				    {target_supports = 7,
				     target_requires = 8,
				     addresses =
				     [#'CSIIOP_TransportAddress'{host_name = "127.0.0.1",
								 port = 6001}]}},
				    as_context_mech =
				    #'CSIIOP_AS_ContextSec'
				    {target_supports = 9, target_requires = 10,
				     client_authentication_mech = [1, 255],
				     target_name = [2,255]},
				    sas_context_mech =
				    #'CSIIOP_SAS_ContextSec'
				    {target_supports = 11, target_requires = 12,
				     privilege_authorities =
				     [#'CSIIOP_ServiceConfiguration'
				      {syntax = ?ULONGMAX,
				       name = [3,255]}],
				     supported_naming_mechanisms = [[4,255],[5,255]],
				     supported_identity_types = ?ULONGMAX}},
				   #'CSIIOP_CompoundSecMech'
				   {target_requires = 6,
				    transport_mech =
				    #'IOP_TaggedComponent'
				    {tag=?TAG_NULL_TAG,
				     component_data=[]},
				    as_context_mech =
				    #'CSIIOP_AS_ContextSec'
				    {target_supports = 9, target_requires = 10,
				     client_authentication_mech = [1, 255],
				     target_name = [2,255]},
				    sas_context_mech =
				    #'CSIIOP_SAS_ContextSec'
				    {target_supports = 11, target_requires = 12,
				     privilege_authorities =
				     [#'CSIIOP_ServiceConfiguration'
				      {syntax = ?ULONGMAX,
				       name = [3,255]}],
				     supported_naming_mechanisms = [[4,255],[5,255]],
				     supported_identity_types = ?ULONGMAX}},
				   #'CSIIOP_CompoundSecMech'
				   {target_requires = 6,
				    transport_mech =
				    #'IOP_TaggedComponent'
				    {tag=?TAG_SECIOP_SEC_TRANS,
				     component_data=#'CSIIOP_SECIOP_SEC_TRANS'
				    {target_supports = 7,
				     target_requires = 8,
				     mech_oid = [0,255],
				     target_name = [0,255],
				     addresses =
				     [#'CSIIOP_TransportAddress'{host_name = "127.0.0.1",
								 port = 6001}]}},
				    as_context_mech =
				    #'CSIIOP_AS_ContextSec'
				    {target_supports = 9, target_requires = 10,
				     client_authentication_mech = [1, 255],
				     target_name = [2,255]},
				    sas_context_mech =
				    #'CSIIOP_SAS_ContextSec'
				    {target_supports = 11, target_requires = 12,
				     privilege_authorities =
				     [#'CSIIOP_ServiceConfiguration'
				      {syntax = ?ULONGMAX,
				       name = [3,255]}],
				     supported_naming_mechanisms = [[4,255],[5,255]],
				     supported_identity_types = ?ULONGMAX}}]}}],
    iop_ior:create(Version, "IDL:omg.org/CosNaming/NamingContextExt:1.0",
		   ["127.0.0.1"], 5001, -1,
		   "NameService", MC, 0, 0).


%%------------------------------------------------------------
%% function : alternate_ssl_iiop_address
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------
alternate_ssl_iiop_address(Host, Port, SSLPort) ->
    IOR = create_alternate_ssl_iiop_address(Host, Port, SSLPort),

    ?match(false, corba_object:non_existent(IOR)),
    ?match({'object_forward',_}, corba:locate(IOR)),
    ?match({'object_forward',_}, corba:locate(IOR, 10000)),
    ok.


%%------------------------------------------------------------
%% function : create_alternate_ssl_iiop_address
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------
create_alternate_ssl_iiop_address(Host, Port, SSLPort) ->
    MC = [#'IOP_TaggedComponent'{tag = ?TAG_ORB_TYPE,
				 component_data = ?ORBER_ORB_TYPE_1},
	  #'IOP_TaggedComponent'{tag = ?TAG_CODE_SETS,
				 component_data = ?DEFAULT_CODESETS},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = Port}},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = 8000}},
	  #'IOP_TaggedComponent'{tag = ?TAG_ALTERNATE_IIOP_ADDRESS,
				 component_data = #'ALTERNATE_IIOP_ADDRESS'{
				   'HostID' = Host,
				   'Port' = 8000}},
	  #'IOP_TaggedComponent'{tag=?TAG_SSL_SEC_TRANS,
				 component_data=#'SSLIOP_SSL'{target_supports = 2,
							      target_requires = 2,
							      port = SSLPort}}],
    #'IOP_IOR'{type_id=TypeID,
	       profiles=P1} = _IORA = iop_ior:create_external({1,2},
							      "IDL:omg.org/CosNaming/NamingContextExt:1.0",
							      Host, 8000,
							      "NameService", MC),
    #'IOP_IOR'{profiles=P2} = _IORB = iop_ior:create_external({1,1},
							      "IDL:omg.org/CosNaming/NamingContextExt:1.0",
							      Host, 8000,
							      "NameService", []),
    #'IOP_IOR'{type_id=TypeID, profiles=P2++P1}.


%%------------------------------------------------------------
%% function : timeouts
%% Arguments: Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------

timeouts(Host, Port, ReqT) ->
    NSR = corba:resolve_initial_references_remote("NameService",
            ["iiop://"++Host++":"++integer_to_list(Port)]),
    NC1 = lname_component:set_id(lname_component:create(), "mamba"),
    N1 = lname:insert_component(lname:create(), 1, NC1),
    NC2 = lname_component:set_id(lname_component:create(), "viper"),
    N2 = lname:insert_component(lname:create(), 1, NC2),
    Mamba = 'CosNaming_NamingContext':resolve(NSR, N1),
    Viper = 'CosNaming_NamingContext':resolve(NSR, N2),

    ?match({'EXCEPTION',{'TIMEOUT',_,_,_}},
	   orber_test_timeout_server:twoway_function(Viper, ReqT, ReqT*2)),
    ?match(ok, orber_test_timeout_server:oneway_function(Viper, ReqT*2)),

    ?match({'EXCEPTION',{'TIMEOUT',_,_,_}},
		 orber_test_server:testing_iiop_twoway_delay(Mamba, ReqT)),
    ?match(ok, orber_test_server:testing_iiop_oneway_delay(Mamba, ReqT)),

    %% Since the objects are stalled we must wait until they are available again
    %% to be able to run any more tests and get the correct results.
    timer:sleep(ReqT*4),

    ?match(ok, orber_test_timeout_server:twoway_function(Viper, ReqT*2, ReqT)),
    ?match(ok, orber_test_timeout_server:oneway_function(Viper, ReqT*2)),

    ?match(ok, orber_test_server:testing_iiop_twoway_delay(Mamba, 0)),
    ?match(ok, orber_test_server:testing_iiop_oneway_delay(Mamba, 0)),

    timer:sleep(ReqT*4),
    ok.

%%------------------------------------------------------------
%% function : light_tests
%% Arguments: Host - which node to contact.
%%            Port - which port the other orb uses.
%% Returns  : term()
%% Effect   :
%%------------------------------------------------------------

light_tests(Host, Port, ObjName) ->
    NSR = corba:resolve_initial_references_remote("NameService",
            ["iiop://"++Host++":"++integer_to_list(Port)]),
    NC1 = lname_component:set_id(lname_component:create(), "not_exist"),
    N1 =  lname:insert_component(lname:create(), 1, NC1),
    %% We cannot handle any unknown replies (besides those found in stub).
    ?match({'EXCEPTION',
	    {'CosNaming_NamingContext_NotFound',
	     "IDL:omg.org/CosNaming/NamingContext/NotFound:1.0",_,_}},
	   'CosNaming_NamingContext':resolve(NSR, N1)),
    NC2 = lname_component:set_id(lname_component:create(), ObjName),
    N2  = lname:insert_component(lname:create(), 1, NC2),
    Obj = ?match({'IOP_IOR',"IDL:omg.org/orber_test/server:1.0",_},
		 'CosNaming_NamingContext':resolve(NSR, N2)),
    Nodes = orber:get_lightweight_nodes(),
    io:format("Light Nodes: ~p~n", [Nodes]),
    orber_test_server:print(Obj),
    test_coding(Obj),
    ok.


%%------------------------------------------------------------
%% function : test_coding_simple
%% Arguments: ObjReference
%% Returns  : term()
%% Effect   : test encode/decode for all simple datatypes.
%%------------------------------------------------------------

test_coding(Obj) ->
    test_coding(Obj, false).

test_coding(Obj, Local) ->
    %%--- Testing code and decode arguments ---
    ?match({ok, 1.5}, orber_test_server:testing_iiop_float(Obj, 1.5)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_float(Obj, atom)),

    ?match({ok,1.0}, orber_test_server:testing_iiop_double(Obj, 1.0)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_double(Obj, "wrong")),

    ?match({ok,0}, orber_test_server:testing_iiop_short(Obj, 0)),
    ?match({ok,?SHORTMAX}, orber_test_server:testing_iiop_short(Obj, ?SHORTMAX)),
    ?match({ok,?SHORTMIN}, orber_test_server:testing_iiop_short(Obj, ?SHORTMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_short(Obj, atomic)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_short(Obj, ?SHORTMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_short(Obj, ?SHORTMIN-1)),

    ?match({ok,0}, orber_test_server:testing_iiop_ushort(Obj, 0)),
    ?match({ok,?USHORTMAX}, orber_test_server:testing_iiop_ushort(Obj, ?USHORTMAX)),
    ?match({ok,?USHORTMIN}, orber_test_server:testing_iiop_ushort(Obj, ?USHORTMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_ushort(Obj, ?USHORTMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_ushort(Obj, ?USHORTMIN-1)),

    ?match({ok,0}, orber_test_server:testing_iiop_long(Obj, 0)),
    ?match({ok,?LONGMAX}, orber_test_server:testing_iiop_long(Obj, ?LONGMAX)),
    ?match({ok,?LONGMIN}, orber_test_server:testing_iiop_long(Obj, ?LONGMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_long(Obj, "wrong")),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_long(Obj, ?LONGMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_long(Obj, ?LONGMIN-1)),

    ?match({ok,0}, orber_test_server:testing_iiop_longlong(Obj, 0)),
    ?match({ok,?LONGLONGMAX}, orber_test_server:testing_iiop_longlong(Obj, ?LONGLONGMAX)),
    ?match({ok,?LONGLONGMIN}, orber_test_server:testing_iiop_longlong(Obj, ?LONGLONGMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_longlong(Obj, "wrong")),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_longlong(Obj, ?LONGLONGMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_longlong(Obj, ?LONGLONGMIN-1)),

    ?match({ok,0}, orber_test_server:testing_iiop_ulong(Obj, 0)),
    ?match({ok,?ULONGMAX}, orber_test_server:testing_iiop_ulong(Obj, ?ULONGMAX)),
    ?match({ok,?ULONGMIN}, orber_test_server:testing_iiop_ulong(Obj, ?ULONGMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
		 orber_test_server:testing_iiop_ulong(Obj, ?ULONGMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
		 orber_test_server:testing_iiop_ulong(Obj, ?ULONGMIN-1)),

    ?match({ok,0}, orber_test_server:testing_iiop_ulonglong(Obj, 0)),
    ?match({ok,?ULONGLONGMAX}, orber_test_server:testing_iiop_ulonglong(Obj, ?ULONGLONGMAX)),
    ?match({ok,?ULONGLONGMIN}, orber_test_server:testing_iiop_ulonglong(Obj, ?ULONGLONGMIN)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_ulonglong(Obj, ?ULONGLONGMAX+1)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_ulonglong(Obj, ?ULONGLONGMIN-1)),

    ?match({ok,98}, orber_test_server:testing_iiop_char(Obj, 98)),
    ?match({ok,$b}, orber_test_server:testing_iiop_char(Obj, $b)),

    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_char(Obj, atomic)),

    ?match({ok,65535}, orber_test_server:testing_iiop_wchar(Obj, 65535)),
    ?match({ok,$b}, orber_test_server:testing_iiop_wchar(Obj, $b)),

    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_wchar(Obj, atomic)),

    ?match({ok,true}, orber_test_server:testing_iiop_bool(Obj, true)),
    ?match({ok,false}, orber_test_server:testing_iiop_bool(Obj, false)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_bool(Obj, atom)),

    ?match({ok,1}, orber_test_server:testing_iiop_octet(Obj, 1)),
% No real guards for this case.
%    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
%	   orber_test_server:testing_iiop_octet(Obj, 1.5)),
    IOR12 = create_components_IOR({1,2}),
    ?match({ok,Obj}, orber_test_server:testing_iiop_obj(Obj, Obj)),
    ?match({ok,IOR12}, orber_test_server:testing_iiop_obj(Obj, IOR12)),
    PObj = orber_test_server:oe_create([], [{pseudo,true}]),
    ?match({ok, _}, orber_test_server:testing_iiop_obj(Obj, PObj)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_obj(Obj, "no_object")),
    ?match({ok,"string"}, orber_test_server:testing_iiop_string(Obj, "string")),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_string(Obj, "ToLongString")),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_string(Obj, atomic)),

    ?match({ok,[65535]}, orber_test_server:testing_iiop_wstring(Obj, [65535])),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_wstring(Obj, "ToLongWstring")),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_wstring(Obj, atomic)),

    ?match({ok, one},
		 orber_test_server:testing_iiop_enum(Obj, one)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_enum(Obj, three)),
    ?match({ok,[1,2,3]},
		 orber_test_server:testing_iiop_seq(Obj, [1,2,3])),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_seq(Obj, [1,2,3,4])),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_seq(Obj, false)),


    ?match({ok,[#orber_test_server_struc{a=1, b=2}]},
		 orber_test_server:testing_iiop_struc_seq(Obj,
					  [#orber_test_server_struc{a=1, b=2}])),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_struc_seq(Obj, false)),

    ?match({ok,[#orber_test_server_uni{label=1, value=66}]},
		 orber_test_server:testing_iiop_uni_seq(Obj,
					  [#orber_test_server_uni{label=1, value=66}])),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_uni_seq(Obj, false)),

    ?match({ok,{"one", "two"}},
		 orber_test_server:testing_iiop_array(Obj, {"one", "two"})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_array(Obj, {"one", "two", "three"})),
    ?match({ok,#orber_test_server_struc{a=1, b=2}},
		 orber_test_server:testing_iiop_struct(Obj,
                                         #orber_test_server_struc{a=1, b=2})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_struct(Obj,
                                         #orber_test_server_struc{a="WRONG", b=2})),
    ?match({ok,#orber_test_server_uni{label=1, value=66}},
	   orber_test_server:testing_iiop_union(Obj,
                                           #orber_test_server_uni{label=1, value=66})),

    ?match({ok,#orber_test_server_uni_d{label=1, value=66}},
	   orber_test_server:testing_iiop_union_d(Obj,
                                           #orber_test_server_uni_d{label=1, value=66})),

    ?match({ok,#orber_test_server_uni_d{label=2, value=true}},
	   orber_test_server:testing_iiop_union_d(Obj,
                                           #orber_test_server_uni_d{label=2, value=true})),

    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_union_d(Obj,
                                           #orber_test_server_uni_d{label=2, value=66})),

    case Local of
	true ->
	    ?match({ok,#orber_test_server_uni{label=2, value=66}},
		   orber_test_server:testing_iiop_union(Obj,
							#orber_test_server_uni{label=2, value=66}));
	false ->
	    ?match({ok,#orber_test_server_uni{label=2, value=undefined}},
		   orber_test_server:testing_iiop_union(Obj,
							#orber_test_server_uni{label=2, value=66}))
    end,

    C1 = orber_test_server:fixed52const1(),
    C2 = orber_test_server:fixed52const2(),
    C3 = orber_test_server:fixed52const3(),

    C4 = orber_test_server:fixed52negconst1(),
    C5 = orber_test_server:fixed52negconst2(),
    C6 = orber_test_server:fixed52negconst3(),

    ?match({ok,C1}, orber_test_server:testing_iiop_fixed(Obj, C1)),
    ?match({ok,C2}, orber_test_server:testing_iiop_fixed(Obj, C2)),
    ?match({ok,C3}, orber_test_server:testing_iiop_fixed(Obj, C3)),
    ?match({ok,C4}, orber_test_server:testing_iiop_fixed(Obj, C4)),
    ?match({ok,C5}, orber_test_server:testing_iiop_fixed(Obj, C5)),
    ?match({ok,C6}, orber_test_server:testing_iiop_fixed(Obj, C6)),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
		 orber_test_server:testing_iiop_fixed(Obj, #fixed{digits = 5,
								  scale = 2,
								  value = 123450})),

    ?match(ok, orber_test_server:testing_iiop_void(Obj)),

    ?match({'EXCEPTION',{'BAD_QOS',_,_,_}},
	   orber_test_server:pseudo_call_raise_exc(Obj, 1)),
    ?match({'EXCEPTION',{'BAD_QOS',_,_,_}},
	   orber_test_server:pseudo_call_raise_exc(Obj, 2)),
    ?match({'EXCEPTION',{'orber_test_server_UserDefinedException',_}},
		 orber_test_server:raise_local_exception(Obj)),
    ?match({'EXCEPTION',{'orber_test_server_ComplexUserDefinedException',_,
			       [#orber_test_server_struc{a=1, b=2}]}},
		 orber_test_server:raise_complex_local_exception(Obj)),
    %% Test all TypeCodes
    ?match({ok, #any{typecode = tk_long, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_long,
							      value = 1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_long,
							value = "wrong"})),
    ?match({ok, #any{typecode = tk_float, value = 1.5}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_float,
							      value = 1.5})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_long,
							value = "wrong"})),
    ?match({ok, #any{typecode = tk_double}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_double,
							      value = 1.0})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_double,
							value = "wrong"})),
    ?match({ok, #any{typecode = tk_short, value = -1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_short,
							      value = -1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_short,
							value = atomic})),
    ?match({ok, #any{typecode = tk_ushort, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ushort,
							      value = 1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ushort,
							value = -1})),
    ?match({ok, #any{typecode = tk_long, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_long,
							      value = 1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_long,
							value = "wrong"})),
    ?match({ok, #any{typecode = tk_longlong, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_longlong,
							      value = 1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_longlong,
							value = "wrong"})),
    ?match({ok, #any{typecode = tk_ulong, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulong,
							      value = 1})),
    ?match({ok, #any{typecode = tk_ulong, value = 4294967295}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulong,
							      value = 4294967295})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulong,
							      value = 4294967296})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulong,
							value = -1})),
    ?match({ok, #any{typecode = tk_ulonglong, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulonglong,
							      value = 1})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_ulonglong,
							value = -1})),
    ?match({ok, #any{typecode = tk_char, value = 98}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_char,
							      value = 98})),
    ?match({ok, #any{typecode = tk_char, value = $b}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_char,
							      value = $b})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_char,
							value = atomic})),
    ?match({ok, #any{typecode = tk_wchar, value = 65535}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_wchar,
							      value = 65535})),
    ?match({ok, #any{typecode = tk_wchar, value = $b}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_wchar,
							      value = $b})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_wchar,
							value = atomic})),
    ?match({ok, #any{typecode = tk_boolean, value = true}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_boolean,
							      value = true})),
    ?match({ok, #any{typecode = tk_boolean, value = false}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_boolean,
							      value = false})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_boolean,
							value = 1})),
    ?match({ok, #any{typecode = tk_octet, value = 1}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_octet,
							      value = 1})),
    ?match({ok, #any{typecode = {tk_objref, "IDL:omg.org/orber_test/server:1.0", "server"}, value = Obj}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_objref, "IDL:omg.org/orber_test/server:1.0", "server"},
							      value = Obj})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_objref, "IDL:omg.org/orber_test/server:1.0", "server"},
							value = "No Object"})),
    ?match({ok, #any{typecode = {tk_string, 6}, value = "string"}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_string, 6},
							      value = "string"})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = tk_string,
							value = atomic})),
    ?match({ok, #any{typecode = {tk_wstring, 1}, value = [65535]}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_wstring, 1},
							      value = [65535]})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_wstring, 1},
							value = atomic})),
    ?match({ok, #any{typecode = {tk_enum, "IDL:omg.org/orber_test/server/enumerant:1.0", "enumerant", ["one","two"]},
			   value = two}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_enum, "IDL:omg.org/orber_test/server/enumerant:1.0", "enumerant", ["one","two"]},
							      value = two})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_enum, "IDL:omg.org/orber_test/server/enumerant:1.0", "enumerant", ["one","two"]},
							      value = three})),


    ?match({ok, #any{typecode = {tk_sequence, tk_long, 3},
			   value = [1,2,3]}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_sequence, tk_long, 3},
							      value = [1,2,3]})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_sequence, tk_long, 3},
							value = false})),



    ?match({ok, #any{typecode = {tk_array,{tk_string,0},2},
			   value = {"one", "two"}}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_array,{tk_string,0},2},
							      value = {"one", "two"}})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_array,{tk_string,0},2},
							value = {"one", "two", "three"}})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_array,{tk_string,0},2},
							value = {1, 2}})),
    ?match({ok, #any{typecode = {tk_struct,"IDL:omg.org/orber_test/server/struc:1.0",
				       "struc",
				       [{"a",tk_long},{"b",tk_short}]},
			   value = #orber_test_server_struc{a=1, b=2}}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_struct,"IDL:omg.org/orber_test/server/struc:1.0",
									  "struc",
									  [{"a",tk_long},{"b",tk_short}]},
							      value = #orber_test_server_struc{a=1, b=2}})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_struct,"IDL:omg.org/orber_test/server/struc:1.0",
								    "struc",
								    [{"a",tk_long},{"b",tk_short}]},
							value = #orber_test_server_struc{a=1, b="string"}})),
    ?match({ok, #any{typecode =
			   {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
			    "uni", tk_long, -1, [{1,"a",tk_long}]},
			   value = #orber_test_server_uni{label=1, value=66}}},
		 orber_test_server:
		 testing_iiop_any(Obj,
				  #any{typecode =
				       {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
					"uni", tk_long,	-1, [{1,"a",tk_long}]},
				       value = #orber_test_server_uni{label=1, value=66}})),
    case Local of
	true ->
	    ?match({ok, #any{typecode =
				   {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
				    "uni", tk_long, -1, [{1,"a",tk_long}]},
				   value = #orber_test_server_uni{label=2, value=66}}},
			 orber_test_server:
			 testing_iiop_any(Obj,
					  #any{typecode =
					       {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
						"uni", tk_long,	-1, [{1,"a",tk_long}]},
					       value = #orber_test_server_uni{label=2, value=66}}));
	false ->
	    ?match({ok, #any{typecode =
				   {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
				    "uni", tk_long, -1, [{1,"a",tk_long}]},
				   value = #orber_test_server_uni{label=2, value=undefined}}},
			 orber_test_server:
			 testing_iiop_any(Obj,
					  #any{typecode =
					       {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
						"uni", tk_long,	-1, [{1,"a",tk_long}]},
					       value = #orber_test_server_uni{label=2, value=66}}))
    end,
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
	   orber_test_server:
		 testing_iiop_any(Obj,
				  #any{typecode =
				       {tk_union,"IDL:omg.org/orber_test/server/uni:1.0",
					"uni", tk_long, -1, [{1,"a",tk_long}]},
				       value = #orber_test_server_uni{label=1, value="string"}})),

    ?match({ok, #any{typecode = {tk_fixed,5,2},
			   value = #fixed{digits = 5, scale = 2, value = 12345}}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_fixed,5,2},
							      value = #fixed{digits = 5,
									     scale = 2,
									     value = 12345}})),
    ?match({ok, #any{typecode = {tk_fixed,10,2},
			   value = #fixed{digits = 10, scale = 2, value = 1234567890}}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_fixed,10,2},
							      value = #fixed{digits = 10,
									     scale = 2,
									     value = 1234567890}})),
    ?match({ok, #any{typecode = {tk_fixed,6,2},
			   value = #fixed{digits = 6, scale = 2, value = 300000}}},
		 orber_test_server:testing_iiop_any(Obj, #any{typecode = {tk_fixed,6,2},
							      value = #fixed{digits = 6,
									     scale = 2,
									     value = 300000}})),
    ?match({'EXCEPTION',{'MARSHAL',_,_,_}},
          orber_test_server:
		 testing_iiop_server_marshal(Obj, "string")),

    RecS = #orber_test_server_rec_struct{chain = [#orber_test_server_rec_struct{chain = []}]},
    ?match(RecS, orber_test_server:testing_iiop_rec_struct(Obj, RecS)),

    RecU = #orber_test_server_rec_union{label = 'RecursiveType',
					value = [#orber_test_server_rec_union{label = 'RecursiveType',
									      value = []}]},
    ?match(RecU, orber_test_server:testing_iiop_rec_union(Obj, RecU)),

%%     RecA1 = #any{typecode = unsupported, value = RecS},
%%     RecA2 = #any{typecode = unsupported, value = RecU},
%%     ?match(RecA1,
%% 	   orber_test_server:testing_iiop_rec_any(Obj, RecA1)),
%%     ?match(RecA2,
%% 	   orber_test_server:testing_iiop_rec_any(Obj, RecA2)),

    ok.

%%--------------- Testing Post- & Pre-cond -------------------
precond(Module, Function, Args) ->
    error_logger:info_msg("=============== pre-condition ============
Module    : ~p
Function  : ~p
Arguments : ~p
==========================================~n", [Module, Function, Args]),
    ok.

postcond(Module, Function, Args, Result) ->
    error_logger:info_msg("=============== post-condition ===========
Module    : ~p
Function  : ~p
Arguments : ~p
Result    : ~p
==========================================~n", [Module, Function, Args, Result]),
    ok.

%%--------------- Testing Missing Module ---------------------
oe_get_interface() ->
    non_existing_module:tc(foo).

%%--------------- INTERCEPTOR FUNCTIONS ----------------------
%%------------------------------------------------------------
%% function : new_in_connection
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
new_in_connection(Arg, CHost, Port) ->
    Host = node(),
    [{SHost, SPort}] = orber:find_sockname_by_peername(CHost, Port),
    Peers = orber:find_peername_by_sockname(SHost, SPort),
    error_logger:info_msg("=============== new_in_connection ========
Node      : ~p
From Host : ~p
From Port : ~p
To Host   : ~p
To Port   : ~p
Peers     : ~p
Arg       : ~p
==========================================~n",
			  [Host, CHost, Port, SHost, SPort, Peers, Arg]),
    {Host}.

%%------------------------------------------------------------
%% function : new_out_connection
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
new_out_connection(Arg, SHost, Port) ->
    Host = node(),
    error_logger:info_msg("=============== new_out_connection =======
Node      : ~p
To Host   : ~p
To Port   : ~p
Arg       : ~p
==========================================~n",
			  [Host, SHost, Port, Arg]),
    {Host}.

%%------------------------------------------------------------
%% function : closed_in_connection
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
closed_in_connection(Arg) ->
    error_logger:info_msg("=============== closed_in_connection =====
Node      : ~p
Connection: ~p
==========================================~n",
			  [node(), Arg]),
    Arg.

%%------------------------------------------------------------
%% function : closed_out_connection
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
closed_out_connection(Arg) ->
    error_logger:info_msg("=============== closed_out_connection ====
Node      : ~p
Connection: ~p
==========================================~n",
			  [node(), Arg]),
    Arg.

%%------------------------------------------------------------
%% function : in_request_encoded
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
in_request_encoded(Ref, _ObjKey, Ctx, Op,
	   <<100:8,101:8,102:8,103:8,104:8,105:8,106:8,107:8,108:8,109:8,110:8,T/binary>>, _Args) ->
    error_logger:info_msg("=============== in_request_encoded =======
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, T, Ctx]),
    {T, "NewArgs"}.

%%------------------------------------------------------------
%% function : in_reply_encoded
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
in_reply_encoded(Ref, _ObjKey, Ctx, Op,
	 <<100:8,101:8,102:8,103:8,104:8,105:8,106:8,107:8,108:8,109:8,110:8,T/binary>>,
	 _Args) ->
    error_logger:info_msg("============== in_reply_encoded ==========
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, T, Ctx]),
    {T, "NewArgs"}.

%%------------------------------------------------------------
%% function : out_reply_encoded
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
out_reply_encoded(Ref, _ObjKey, Ctx, Op, List, _Args) ->
    error_logger:info_msg("============== out_reply_encoded =========
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, List, Ctx]),
    {list_to_binary([<<100:8,101:8,102:8,103:8,104:8,105:8,106:8,107:8,108:8,109:8,110:8>>|List]), "NewArgs"}.

%%------------------------------------------------------------
%% function : out_request_encoded
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
out_request_encoded(Ref, _ObjKey, Ctx, Op, List, _Args) ->
    error_logger:info_msg("============== out_request_encoded =======
Connection: ~p
Operation : ~p
Body      : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, List, Ctx]),
    {list_to_binary([<<100:8,101:8,102:8,103:8,104:8,105:8,106:8,107:8,108:8,109:8,110:8>>|List]), "NewArgs"}.

%%------------------------------------------------------------
%% function : in_request
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
in_request(Ref, _ObjKey, Ctx, Op, Params, _Args) ->
    error_logger:info_msg("=============== in_request ===============
Connection: ~p
Operation : ~p
Parameters: ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, Params, Ctx]),
    {Params, "NewArgs"}.

%%------------------------------------------------------------
%% function : in_reply
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
in_reply(Ref, _ObjKey, Ctx, Op, Reply, _Args) ->
    error_logger:info_msg("=============== in_reply =================
Connection: ~p
Operation : ~p
Reply     : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, Reply, Ctx]),
    {Reply, "NewArgs"}.

%%------------------------------------------------------------
%% function : postinvoke
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
out_reply(Ref, _ObjKey, Ctx, Op, Reply, _Args) ->
    error_logger:info_msg("=============== out_reply ================
Connection: ~p
Operation : ~p
Reply     : ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, Reply, Ctx]),
    {Reply, "NewArgs"}.

%%------------------------------------------------------------
%% function : postinvoke
%% Arguments:
%% Returns  :
%%------------------------------------------------------------
out_request(Ref, _ObjKey, Ctx, Op, Params, _Args) ->
    error_logger:info_msg("=============== out_request ==============
Connection: ~p
Operation : ~p
Parameters: ~p
Context   : ~p
==========================================~n",
			  [Ref, Op, Params, Ctx]),
    {Params, "NewArgs"}.


%%--------------- END OF MODULE ------------------------------



