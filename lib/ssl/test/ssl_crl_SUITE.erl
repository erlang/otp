%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2016. All Rights Reserved.
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
			{group, idp_crl},
                        {group, crl_hash_dir}]},
     {check_peer, [],   [{group, v2_crl},
			 {group, v1_crl},
			 {group, idp_crl},
                         {group, crl_hash_dir}]},
     {check_best_effort, [], [{group, v2_crl},
			       {group, v1_crl},
			      {group, idp_crl},
			      {group, crl_hash_dir}]},
     {v2_crl,  [], basic_tests()},
     {v1_crl,  [], basic_tests()},
     {idp_crl, [], basic_tests()},
     {crl_hash_dir, [], basic_tests() ++ crl_hash_dir_tests()}].

basic_tests() ->
    [crl_verify_valid, crl_verify_revoked, crl_verify_no_crl].

crl_hash_dir_tests() ->
    [crl_hash_dir_collision, crl_hash_dir_expired].

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
		    end_per_suite(Config),
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
    try 
	case is_idp(Group) of
	    true ->
		[{idp_crl, true} | Config0];
	    false ->
		DataDir = proplists:get_value(data_dir, Config0), 
		CertDir = filename:join(proplists:get_value(priv_dir, Config0), Group),
		{CertOpts, Config} = init_certs(CertDir, Group, Config0),
		{ok, _} =  make_certs:all(DataDir, CertDir, CertOpts),
		CrlCacheOpts = case Group of
				   crl_hash_dir ->
				       CrlDir = filename:join(CertDir, "crls"),
				       %% Copy CRLs to their hashed filenames.
				       %% Find the hashes with 'openssl crl -noout -hash -in crl.pem'.
				       populate_crl_hash_dir(CertDir, CrlDir,
							     [{"erlangCA", "d6134ed3"},
							      {"otpCA", "d4c8d7e5"}],
							     replace),
				       [{crl_cache,
					 {ssl_crl_hash_dir,
					  {internal, [{dir, CrlDir}]}}}];
				   _ ->
				       []
			       end,
		[{crl_cache_opts, CrlCacheOpts},
		 {cert_dir, CertDir},
		 {idp_crl, false} | Config]
	end
    catch
	_:_ ->
	    {skip, "Unable to create crls"}
    end.

end_per_group(_GroupName, Config) ->
    
    Config.

init_per_testcase(Case, Config0) ->
    case proplists:get_value(idp_crl, Config0) of
	true ->
	    end_per_testcase(Case, Config0),
	    inets:start(),
	    ssl_test_lib:clean_start(),
	    ServerRoot = make_dir_path([proplists:get_value(priv_dir, Config0), idp_crl, tmp]),
	    %% start a HTTP server to serve the CRLs
	    {ok, Httpd} = inets:start(httpd, [{ipfamily, proplists:get_value(ipfamily, Config0)},
					      {server_name, "localhost"}, {port, 0},
					      {server_root, ServerRoot},
					      {document_root, 
					       filename:join(proplists:get_value(priv_dir, Config0), idp_crl)}
					     ]),
	    [{port,Port}] = httpd:info(Httpd, [port]),
	    Config = [{httpd_port, Port} | Config0],
	    DataDir = proplists:get_value(data_dir, Config), 
	    CertDir = filename:join(proplists:get_value(priv_dir, Config0), idp_crl),
	    {CertOpts, Config} = init_certs(CertDir, idp_crl, Config),
	    case make_certs:all(DataDir, CertDir, CertOpts) of
                {ok, _} ->
                    ct:timetrap({seconds, 6}),
                    [{cert_dir, CertDir} | Config];
                _ ->
                    end_per_testcase(Case, Config0),
                    ssl_test_lib:clean_start(),
                    {skip, "Unable to create IDP crls"}
            end;
	false ->
	    end_per_testcase(Case, Config0),
	    ssl_test_lib:clean_start(),
	    Config0
    end.

end_per_testcase(_, Config) ->
    case proplists:get_value(idp_crl, Config) of
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
    PrivDir = proplists:get_value(cert_dir, Config),
    Check = proplists:get_value(crl_check, Config),
    ServerOpts =  [{keyfile, filename:join([PrivDir, "server", "key.pem"])},
      		  {certfile, filename:join([PrivDir, "server", "cert.pem"])},
		   {cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])}],
    ClientOpts =  case proplists:get_value(idp_crl, Config) of 
		      true ->	       
			  [{cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])},
			   {crl_check, Check},
			   {crl_cache, {ssl_crl_cache, {internal, [{http, 5000}]}}},
			   {verify, verify_peer}];
		      false ->
			  proplists:get_value(crl_cache_opts, Config) ++
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
    PrivDir = proplists:get_value(cert_dir, Config),
    Check = proplists:get_value(crl_check, Config),
    ServerOpts = [{keyfile, filename:join([PrivDir, "revoked", "key.pem"])},
      		  {certfile, filename:join([PrivDir, "revoked", "cert.pem"])},
      		  {cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])}],

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    ssl_crl_cache:insert({file, filename:join([PrivDir, "erlangCA", "crl.pem"])}),
    ssl_crl_cache:insert({file, filename:join([PrivDir, "otpCA", "crl.pem"])}),
    
    ClientOpts =  case proplists:get_value(idp_crl, Config) of 
		      true ->	       
			  [{cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])},
			   {crl_cache, {ssl_crl_cache, {internal, [{http, 5000}]}}},
			   {crl_check, Check},
			   {verify, verify_peer}];
		      false ->
			  proplists:get_value(crl_cache_opts, Config) ++
			      [{cacertfile, filename:join([PrivDir, "revoked", "cacerts.pem"])},
			       {crl_check, Check},
			       {verify, verify_peer}]
		  end,	
    
    crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts,
                     "certificate revoked").

crl_verify_no_crl() ->
    [{doc,"Verify a simple CRL chain when the CRL is missing"}].
crl_verify_no_crl(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(cert_dir, Config),
    Check = proplists:get_value(crl_check, Config),
    ServerOpts =  [{keyfile, filename:join([PrivDir, "server", "key.pem"])},
      		  {certfile, filename:join([PrivDir, "server", "cert.pem"])},
		   {cacertfile, filename:join([PrivDir, "server", "cacerts.pem"])}],
    ClientOpts =  case proplists:get_value(idp_crl, Config) of 
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

    %% In case we're running an HTTP server that serves CRLs, let's
    %% rename those files, so the CRL is absent when we try to verify
    %% it.
    %%
    %% If we're not using an HTTP server, we just need to refrain from
    %% adding the CRLs to the cache manually.
    rename_crl(filename:join([PrivDir, "erlangCA", "crl.pem"])),
    rename_crl(filename:join([PrivDir, "otpCA", "crl.pem"])),

    %% The expected outcome when the CRL is missing depends on the
    %% crl_check setting.
    case Check of
        true ->
            %% The error "revocation status undetermined" gets turned
            %% into "bad certificate".
            crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts,
                             "bad certificate");
        peer ->
            crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts,
                             "bad certificate");
        best_effort ->
            %% In "best effort" mode, we consider the certificate not
            %% to be revoked if we can't find the appropriate CRL.
            crl_verify_valid(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts)
    end.

crl_hash_dir_collision() ->
    [{doc,"Verify ssl_crl_hash_dir behaviour with hash collisions"}].
crl_hash_dir_collision(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(cert_dir, Config),
    Check = proplists:get_value(crl_check, Config),

    %% Create two CAs whose names hash to the same value
    CA1 = "hash-collision-0000000000",
    CA2 = "hash-collision-0258497583",
    CertsConfig = make_certs:make_config([]),
    make_certs:intermediateCA(PrivDir, CA1, "erlangCA", CertsConfig),
    make_certs:intermediateCA(PrivDir, CA2, "erlangCA", CertsConfig),

    make_certs:enduser(PrivDir, CA1, "collision-client-1", CertsConfig),
    make_certs:enduser(PrivDir, CA2, "collision-client-2", CertsConfig),

    [ServerOpts1, ServerOpts2] =
	[
	 [{keyfile, filename:join([PrivDir, EndUser, "key.pem"])},
	  {certfile, filename:join([PrivDir, EndUser, "cert.pem"])},
	  {cacertfile, filename:join([PrivDir, EndUser, "cacerts.pem"])}]
	 || EndUser <- ["collision-client-1", "collision-client-2"]],

    %% Add CRLs for our new CAs into the CRL hash directory.
    %% Find the hashes with 'openssl crl -noout -hash -in crl.pem'.
    CrlDir = filename:join(PrivDir, "crls"),
    populate_crl_hash_dir(PrivDir, CrlDir,
			  [{CA1, "b68fc624"},
			   {CA2, "b68fc624"}],
			 replace),

    NewCA = new_ca(filename:join([PrivDir, "new_ca"]),
		   filename:join([PrivDir, "erlangCA", "cacerts.pem"]),
		   filename:join([PrivDir, "server", "cacerts.pem"])),
    
    ClientOpts = proplists:get_value(crl_cache_opts, Config) ++
	[{cacertfile, NewCA},
	 {crl_check, Check},
	 {verify, verify_peer}],
    
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    
    %% Neither certificate revoked; both succeed.
    crl_verify_valid(Hostname, ServerNode, ServerOpts1, ClientNode, ClientOpts),
    crl_verify_valid(Hostname, ServerNode, ServerOpts2, ClientNode, ClientOpts),

    make_certs:revoke(PrivDir, CA1, "collision-client-1", CertsConfig),
    populate_crl_hash_dir(PrivDir, CrlDir,
			  [{CA1, "b68fc624"},
			   {CA2, "b68fc624"}],
			 replace),

    %% First certificate revoked; first fails, second succeeds.
    crl_verify_error(Hostname, ServerNode, ServerOpts1, ClientNode, ClientOpts,
                     "certificate revoked"),
    crl_verify_valid(Hostname, ServerNode, ServerOpts2, ClientNode, ClientOpts),

    make_certs:revoke(PrivDir, CA2, "collision-client-2", CertsConfig),
    populate_crl_hash_dir(PrivDir, CrlDir,
			  [{CA1, "b68fc624"},
			   {CA2, "b68fc624"}],
			 replace),

    %% Second certificate revoked; both fail.
    crl_verify_error(Hostname, ServerNode, ServerOpts1, ClientNode, ClientOpts,
                     "certificate revoked"),
    crl_verify_error(Hostname, ServerNode, ServerOpts2, ClientNode, ClientOpts,
                     "certificate revoked"),

    ok.

crl_hash_dir_expired() ->
    [{doc,"Verify ssl_crl_hash_dir behaviour with expired CRLs"}].
crl_hash_dir_expired(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(cert_dir, Config),
    Check = proplists:get_value(crl_check, Config),

    CA = "CRL-maybe-expired-CA",
    %% Add "issuing distribution point", to ensure that verification
    %% fails if there is no valid CRL.
    CertsConfig = make_certs:make_config([{issuing_distribution_point, true}]),
    make_certs:can_generate_expired_crls(CertsConfig)
    	orelse throw({skip, "cannot generate CRLs with expiry date in the past"}),
    make_certs:intermediateCA(PrivDir, CA, "erlangCA", CertsConfig),
    EndUser = "CRL-maybe-expired",
    make_certs:enduser(PrivDir, CA, EndUser, CertsConfig),

    ServerOpts =  [{keyfile, filename:join([PrivDir, EndUser, "key.pem"])},
		   {certfile, filename:join([PrivDir, EndUser, "cert.pem"])},
		   {cacertfile, filename:join([PrivDir, EndUser, "cacerts.pem"])}],
    ClientOpts = proplists:get_value(crl_cache_opts, Config) ++
	[{cacertfile, filename:join([PrivDir, CA, "cacerts.pem"])},
	 {crl_check, Check},
	 {verify, verify_peer}],
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% First make a CRL that expired yesterday.
    make_certs:gencrl(PrivDir, CA, CertsConfig, -24),
    CrlDir = filename:join(PrivDir, "crls"),
    populate_crl_hash_dir(PrivDir, CrlDir,
			  [{CA, "1627b4b0"}],
			  replace),

    %% Since the CRL has expired, it's treated as missing, and the
    %% outcome depends on the crl_check setting.
    case Check of
        true ->
            %% The error "revocation status undetermined" gets turned
            %% into "bad certificate".
            crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts,
                             "bad certificate");
        peer ->
            crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts,
                             "bad certificate");
        best_effort ->
            %% In "best effort" mode, we consider the certificate not
            %% to be revoked if we can't find the appropriate CRL.
            crl_verify_valid(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts)
    end,

    %% Now make a CRL that expires tomorrow.
    make_certs:gencrl(PrivDir, CA, CertsConfig, 24),
    CrlDir = filename:join(PrivDir, "crls"),
    populate_crl_hash_dir(PrivDir, CrlDir,
			  [{CA, "1627b4b0"}],
			  add),

    %% With a valid CRL, verification should always pass.
    crl_verify_valid(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts),

    ok.

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

crl_verify_error(Hostname, ServerNode, ServerOpts, ClientNode, ClientOpts, ExpectedAlert) ->
    Server = ssl_test_lib:start_server_error([{node, ServerNode}, {port, 0},
					      {from, self()},
					      {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),

    Client = ssl_test_lib:start_client_error([{node, ClientNode}, {port, Port},
					      {host, Hostname},
					      {from, self()},
					      {options, ClientOpts}]),
    receive
	{Server, AlertOrClose} ->
	    ct:pal("Server Alert or Close ~p", [AlertOrClose])
    end,
    ssl_test_lib:check_result(Client, {error, {tls_alert, ExpectedAlert}}).

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
    Port = proplists:get_value(httpd_port, Config),
    {[{crl_port,Port},
      {issuing_distribution_point, true}], Config
    };
init_certs(_,_,Config) -> 
    {[], Config}.

make_dir_path(PathComponents) ->
    lists:foldl(fun(F,P0) -> file:make_dir(P=filename:join(P0,F)), P end,
		"",
		PathComponents).

rename_crl(Filename) ->
    file:rename(Filename, Filename ++ ".notfound").

populate_crl_hash_dir(CertDir, CrlDir, CAsHashes, AddOrReplace) ->
    ok = filelib:ensure_dir(filename:join(CrlDir, "crls")),
    case AddOrReplace of
	replace ->
	    %% Delete existing files, so we can override them.
	    [ok = file:delete(FileToDelete) ||
		{_CA, Hash} <- CAsHashes,
		FileToDelete <- filelib:wildcard(
				  filename:join(CrlDir, Hash ++ ".r*"))];
	add ->
	    ok
    end,
    %% Create new files, incrementing suffix if needed to find unique names.
    [{ok, _} =
	 file:copy(filename:join([CertDir, CA, "crl.pem"]),
		   find_free_name(CrlDir, Hash, 0))
     || {CA, Hash} <- CAsHashes],
    ok.

find_free_name(CrlDir, Hash, N) ->
    Name = filename:join(CrlDir, Hash ++ ".r" ++ integer_to_list(N)),
    case filelib:is_file(Name) of
	true ->
	    find_free_name(CrlDir, Hash, N + 1);
	false ->
	    Name
    end.

new_ca(FileName, CA1, CA2) ->
    {ok, P1} = file:read_file(CA1),
    E1 = public_key:pem_decode(P1),
    {ok, P2} = file:read_file(CA2),
    E2 = public_key:pem_decode(P2),
    Pem = public_key:pem_encode(E1 ++E2),
    file:write_file(FileName,  Pem),
    FileName.
