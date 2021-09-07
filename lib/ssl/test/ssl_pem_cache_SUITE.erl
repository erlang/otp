%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2020. All Rights Reserved.
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

-module(ssl_pem_cache_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("public_key/include/public_key.hrl").

%% Callback functions
-export([all/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Testcases
-export([pem_cleanup/0,
         pem_cleanup/1,
         clear_pem_cache/0,
         clear_pem_cache/1,
         invalid_insert/0,
         invalid_insert/1,
         new_root_pem/0,
         new_root_pem/1,
         check_cert/3
        ]).


-define(CLEANUP_INTERVAL, 5000).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [
     pem_cleanup, 
     clear_pem_cache,
     invalid_insert,
     new_root_pem
    ].

groups() ->
    [].

init_per_suite(Config0) ->
    catch crypto:stop(),
    try crypto:start() of
	ok ->
	    ssl_test_lib:clean_start(),
	    %% make rsa certs  
            ssl_test_lib:make_rsa_cert(Config0)
    catch _:_ ->
	    {skip, "Crypto did not start"}
    end.

end_per_suite(_Config) ->
    application:stop(crypto).

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(pem_cleanup = Case, Config) ->
    application:load(ssl),
    end_per_testcase(Case, Config) ,
    application:set_env(ssl, ssl_pem_cache_clean, ?CLEANUP_INTERVAL),
    ssl:start(),
    ct:timetrap({minutes, 1}),
    Config;
init_per_testcase(_, Config) ->
    ssl_test_lib:clean_start(),
    ct:timetrap({seconds, 10}),
    Config.

end_per_testcase(_TestCase, Config) ->
    ssl_test_lib:clean_env(),
    ssl:stop(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
pem_cleanup() ->
    [{doc, "Test pem cache invalidate mechanism"}].
pem_cleanup(Config)when is_list(Config) ->
    process_flag(trap_exit, true),
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode},
		      {port, Port}, {host, Hostname},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {from, self()}, {options, ClientOpts}]),

    Size = ssl_pkix_db:db_size(get_pem_cache()),
    Certfile = proplists:get_value(certfile, ServerOpts),
    {ok, FileInfo} = file:read_file_info(Certfile),
    Time = later(), 
    ok = file:write_file_info(Certfile, FileInfo#file_info{mtime = Time}),
    ct:sleep(2 * ?CLEANUP_INTERVAL),
    Size1 = ssl_pkix_db:db_size(get_pem_cache()),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    false = Size == Size1.

clear_pem_cache() ->
    [{doc,"Test that internal reference tabel is cleaned properly even when "
     " the PEM cache is cleared" }].
clear_pem_cache(Config) when is_list(Config) -> 
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    [_,{FilRefDb, _} |_] = element(5, State),
    {Server, Client} = basic_verify_test_no_close(Config),
    CountReferencedFiles = fun({_, -1}, Acc) ->
				   Acc;
			      ({_, N}, Acc) ->
				   N + Acc
			   end,
    
    2 = ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl:clear_pem_cache(),
    _ = sys:get_status(whereis(ssl_manager)),
    {Server1, Client1} = basic_verify_test_no_close(Config),
    4 =  ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Client),
    ct:sleep(2000),
    _ = sys:get_status(whereis(ssl_manager)),
    2 =  ets:foldl(CountReferencedFiles, 0, FilRefDb), 
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client1),
    ct:sleep(2000),
    _ = sys:get_status(whereis(ssl_manager)),
    0 =  ets:foldl(CountReferencedFiles, 0, FilRefDb).

invalid_insert() ->
    [{doc, "Test that insert of invalid pem does not cause empty cache entry"}].
invalid_insert(Config)when is_list(Config) ->      
    process_flag(trap_exit, true),
    
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    BadClientOpts = [{cacertfile, "tmp/does_not_exist.pem"} | proplists:delete(cacertfile, ClientOpts)],
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    ssl_test_lib:start_client_error([{node, ClientNode},
                               {port, Port}, {host, Hostname},
                               {from, self()}, {options, BadClientOpts}]),
    ssl_test_lib:close(Server),
    1 = ssl_pkix_db:db_size(get_fileref_db()).


new_root_pem() ->
    [{doc, "Test that changed PEM-files on disk followed by ssl:clear_pem_cache() invalidates"
      "trusted CA cache as well as ordinary PEM cache"}].
new_root_pem(Config)when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    #{cert := OrgSRoot} = SRoot = 
        public_key:pkix_test_root_cert("OTP test server ROOT",  [{key, ssl_test_lib:hardcode_rsa_key(6)}]),
    
    DerConfig = public_key:pkix_test_data(#{server_chain => #{root => SRoot,
                                                              intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                                              peer =>  [{key, ssl_test_lib:hardcode_rsa_key(4)}]},
                                            client_chain => #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                                              intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                                              peer =>  [{key, ssl_test_lib:hardcode_rsa_key(3)}]}}),

    ClientBase = filename:join(PrivDir, "client_test"),
    SeverBase =  filename:join(PrivDir, "server_test"),
    PemConfig = x509_test:gen_pem_config_files(DerConfig, ClientBase, SeverBase),
    ClientConf = proplists:get_value(client_config, PemConfig),
    ServerConf = proplists:get_value(server_config, PemConfig),

    SCAFile =  proplists:get_value(cacertfile, ServerConf),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    %% Start a connection and keep it up for a little while, so that 
    %% it will be up when the second connection is started.
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerConf}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
				   {mfa, {?MODULE, check_cert, [OrgSRoot, SCAFile]}},
				   {from, self()}, {options, [{verify, verify_peer} |ClientConf]}]),

    ssl_test_lib:check_result(Client, ok),

    %% Create new configuration
    Key = ssl_test_lib:hardcode_rsa_key(1),
    OTPCert = public_key:pkix_decode_cert(OrgSRoot, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    #'RSAPrivateKey'{modulus=N, publicExponent=E} = Key,
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters='NULL'},
    SPKI = #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
                                      subjectPublicKey = Public},
    NewCert = public_key:pkix_sign(TBS#'OTPTBSCertificate'{subjectPublicKeyInfo = SPKI}, Key),

    DerConfig1 = public_key:pkix_test_data(#{server_chain =>
                                                 #{root =>  #{cert => NewCert, key => Key},
                                                   intermediates => [[{key, ssl_test_lib:hardcode_rsa_key(5)}]],
                                                   peer => [{key, ssl_test_lib:hardcode_rsa_key(4)}]},
                                             client_chain =>
                                                 #{root => [{key, ssl_test_lib:hardcode_rsa_key(1)}],
                                                   intermediates =>  [[{key, ssl_test_lib:hardcode_rsa_key(2)}]],
                                                   peer => [{key, ssl_test_lib:hardcode_rsa_key(3)}]}}),

    %% Overwrite old config files
    _ = x509_test:gen_pem_config_files(DerConfig1, ClientBase, SeverBase),

    %% Make sure chache is cleared 
    ssl:clear_pem_cache(),
    
    Server1 =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerConf}]),
    Port1 = ssl_test_lib:inet_port(Server1),

    %% Start second connection
    Client1 = ssl_test_lib:start_client([{node, ClientNode},
                                         {port, Port1}, {host, Hostname},
                                         {from, self()},
                                         {mfa, {?MODULE, check_cert, [NewCert, SCAFile]}},
                                         {options, [{verify, verify_peer} | ClientConf]}]),
    ssl_test_lib:check_result(Client1, ok),
    ssl_test_lib:close(Server),
    ssl_test_lib:close(Server1),
    ssl_test_lib:close(Client),
    ssl_test_lib:close(Client1).
    
%%--------------------------------------------------------------------
%% Internal funcations 
%%--------------------------------------------------------------------

get_pem_cache() ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    case element(5, State) of
	[_CertDb, _FileRefDb, PemCache| _] ->
	    PemCache;
	_ ->
	    undefined
    end.

get_fileref_db() ->
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    case element(5, State) of
	[_CertDb, {FileRefDb,_} | _] ->
	    FileRefDb;
	_ ->
	    undefined
    end.
later()->
    DateTime = calendar:now_to_local_time(os:timestamp()), 
    Gregorian = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Gregorian + (2 * ?CLEANUP_INTERVAL)).
	
basic_verify_test_no_close(Config) ->
    ClientOpts = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ServerOpts = ssl_test_lib:ssl_options(server_rsa_opts, Config),

    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),

    Server = ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    Client = ssl_test_lib:start_client([{node, ClientNode}, {port, Port},
					{host, Hostname},
					{from, self()},
					{mfa, {ssl_test_lib, send_recv_result_active, []}},
					{options, ClientOpts}]),

    ssl_test_lib:check_result(Server, ok, Client, ok),
    {Server, Client}.


check_cert(Socket, RootCert, File) ->
    {ok, Cert} = ssl:peercert(Socket),
    {ok, Extracted} = ssl_pkix_db:extract_trusted_certs(File),
    {ok, RootCert, _} = ssl_certificate:certificate_chain(Cert, ets:new(foo, []), Extracted, [], encoded),
    ok.

