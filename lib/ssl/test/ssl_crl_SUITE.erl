%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2015. All Rights Reserved.
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

-module(ssl_crl_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("public_key/include/public_key.hrl").

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() -> 
    [
     {group, check_true},
     {group, check_peer},
     {group, check_best_effort}
    ].

groups() ->
    [
     {check_true, [],  [{group, v2_crl},
			{group, v1_crl},
			{group, idp_crl}]},
     {check_peer, [],   [{group, v2_crl},
			 {group, v1_crl},
			 {group, idp_crl}]},
     {check_best_effort, [], [{group, v2_crl},
			       {group, v1_crl},
			      {group, idp_crl}]},
     {v2_crl,  [], basic_tests()},
     {v1_crl,  [], basic_tests()},
     {idp_crl, [], basic_tests()}].

basic_tests() ->
    [crl_verify_valid, crl_verify_revoked].


init_per_suite(Config) ->
    case os:find_executable("openssl") of
	false ->
	    {skip, "Openssl not found"};
	_ ->
	    OpenSSL_version = (catch os:cmd("openssl version")),
	    case ssl_test_lib:enough_openssl_crl_support(OpenSSL_version) of
		false ->
		    {skip, io_lib:format("Bad openssl version: ~p",[OpenSSL_version])};
		_ ->
		    catch crypto:stop(),
		    try crypto:start() of
			ok ->
			    {ok, Hostname0} = inet:gethostname(),
			    IPfamily =
				case lists:member(list_to_atom(Hostname0), ct:get_config(ipv6_hosts,[])) of
				    true -> inet6;
				    false -> inet
				end,
			    [{ipfamily,IPfamily}, {openssl_version,OpenSSL_version} | Config]
		    catch _:_ ->
			    {skip, "Crypto did not start"}
		    end
	    end
    end.

end_per_suite(_Config) ->
    ssl:stop(),
    application:stop(crypto).

init_per_group(check_true, Config) ->
    [{crl_check, true} | Config];
init_per_group(check_peer, Config) ->
    [{crl_check, peer} | Config];
init_per_group(check_best_effort, Config) ->
    [{crl_check, best_effort} | Config];
init_per_group(Group, Config0) ->
    case is_idp(Group) of
	true ->
	    [{idp_crl, true} | Config0];
	false ->
	    DataDir = ?config(data_dir, Config0), 
	    CertDir = filename:join(?config(priv_dir, Config0), Group),
	    {CertOpts, Config} = init_certs(CertDir, Group, Config0),
	    {ok, _} =  make_certs:all(DataDir, CertDir, CertOpts),
	    [{cert_dir, CertDir}, {idp_crl, false} | Config]
    end.

end_per_group(_GroupName, Config) ->
    
    Config.

init_per_testcase(Case, Config0) ->
    case ?config(idp_crl, Config0) of
	true ->
	    end_per_testcase(Case, Config0),
	    inets:start(),
	    ssl:start(),
	    ServerRoot = make_dir_path([?config(priv_dir, Config0), idp_crl, tmp]),
	    %% start a HTTP server to serve the CRLs
	    {ok, Httpd} = inets:start(httpd, [{ipfamily, ?config(ipfamily, Config0)},
					      {server_name, "localhost"}, {port, 0},
					      {server_root, ServerRoot},
					      {document_root, 
					       filename:join(?config(priv_dir, Config0), idp_crl)}
					     ]),
	    [{port,Port}] = httpd:info(Httpd, [port]),
	    Config = [{httpd_port, Port} | Config0],
	    DataDir = ?config(data_dir, Config), 
	    CertDir = filename:join(?config(priv_dir, Config0), idp_crl),
	    {CertOpts, Config} = init_certs(CertDir, idp_crl, Config),
	    {ok, _} =  make_certs:all(DataDir, CertDir, CertOpts),
	    ct:timetrap({seconds, 6}),
	    [{cert_dir, CertDir} | Config];
	false ->
	    end_per_testcase(Case, Config0),
	    ssl:start(),
	    Config0
    end.

end_per_testcase(_, Config) ->
    case ?config(idp_crl, Config) of
	true ->
	    ssl:stop(),
	    inets:stop();
	false ->
	    ssl:stop()
    end.

%%%================================================================
%%% Test cases
%%%================================================================

crl_verify_valid() ->
    [{doc,"Verify a simple valid CRL chain"}].
crl_verify_valid(Config) when is_list(Config) ->
    PrivDir = ?config(cert_dir, Config),
    Check = ?config(crl_check, Config),
    ServerOpts =  [{keyfile, filename:join([PrivDir, "server", "key.pem"])},
      		  {certfile, filename:join([PrivDir, "server", "cert.pem"])},
		   {cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])}],
    ClientOpts =  case ?config(idp_crl, Config) of 
		      true ->	       
			  [{cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])},
			   {crl_check, Check},
			   {crl_cache, {ssl_crl_cache, {internal, [{http, 5000}]}}},
			   {verify, verify_peer}];
		      false ->
			  [{cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])},
			   {crl_check, Check},
			   {verify, verify_peer}]
		  end,			  
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ssl_crl_cache:insert({file, filename:join([PrivDir, "erlangCA", "crl.pem"])}),
    ssl_crl_cache:insert({file, filename:join([PrivDir, "otpCA", "crl.pem"])}),
    
    crl_verify_valid(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts).

crl_verify_revoked() ->
    [{doc,"Verify a simple CRL chain when peer cert is reveoked"}].
crl_verify_revoked(Config)  when is_list(Config) ->
    PrivDir = ?config(cert_dir, Config),
    Check = ?config(crl_check, Config),
    ServerOpts = [{keyfile, filename:join([PrivDir, "revoked", "key.pem"])},
      		  {certfile, filename:join([PrivDir, "revoked", "cert.pem"])},
      		  {cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])}],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0}, 
					      {from, self()}, 
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    
    ssl_crl_cache:insert({file, filename:join([PrivDir, "erlangCA", "crl.pem"])}),
    ssl_crl_cache:insert({file, filename:join([PrivDir, "otpCA", "crl.pem"])}),
    
    ClientOpts =  case ?config(idp_crl, Config) of 
		      true ->	       
			  [{cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])},
			   {crl_cache, {ssl_crl_cache, {internal, [{http, 5000}]}}},
			   {crl_check, Check},
			   {verify, verify_peer}];
		      false ->
			  [{cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])},
			   {crl_check, Check},
			   {verify, verify_peer}]
		  end,	
    
    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port}, 
     					      {host, Hostname},
   					      {from, self()}, 
     					      {options, ClientOpts}]),
    receive
	{Server, AlertOrColse} ->
	    ct:pal("Server Alert or Close ~p", [AlertOrColse])
    end,	    
    ssl_test_lib:check_result(Client, {error, {tls_alert, "certificate revoked"}}).


crl_verify_valid(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts) ->
    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0}, 
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       send_recv_result_active, []}},				       
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server), 
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port}, 
					{host, Hostname},
					{from, self()}, 
					{mfa, {ssl_test_lib, 
					       send_recv_result_active, []}},
					{options, ClientOpts}]),
    
    ssl_test_lib:check_result(Client, ok,  Server, ok),

    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client).

%%--------------------------------------------------------------------
%% Internal functions ------------------------------------------------
%%--------------------------------------------------------------------
is_idp(idp_crl) ->
    true;
is_idp(_) ->
    false.

init_certs(_,v1_crl, Config)  -> 
    {[{v2_crls, false}], Config};
init_certs(_, idp_crl, Config) -> 
    Port = ?config(httpd_port, Config),
    {[{crl_port,Port},
      {issuing_distribution_point, true}], Config
    };
init_certs(_,_,Config) -> 
    {[], Config}.

make_dir_path(PathComponents) ->
    lists:foldl(fun(F,P0) -> file:make_dir(P=filename:join(P0,F)), P end,
		"",
		PathComponents).

