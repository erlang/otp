%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015-2023. All Rights Reserved.
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
%% (Note: See the document internal_doc/pem_and_cert_cache.md for additional
%% information)
%%

-module(ssl_pem_cache_SUITE).

-behaviour(ct_suite).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("public_key/include/public_key.hrl").
-include("ssl_test_lib.hrl").

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
-export([pem_certfile_keyfile_periodical_cleanup/0,
         pem_certfile_keyfile_periodical_cleanup/1,
         pem_cacertfile_periodical_cleanup/0,
         pem_cacertfile_periodical_cleanup/1,
         pem_manual_cleanup/0,
         pem_manual_cleanup/1,
         invalid_insert/0,
         invalid_insert/1,
         new_root_pem_manual_cleanup/0,
         new_root_pem_manual_cleanup/1,
         new_root_pem_periodical_cleanup/0,
         new_root_pem_periodical_cleanup/1,
         new_root_pem_no_cleanup/0,
         new_root_pem_no_cleanup/1,
         new_root_pem_no_cleanup_symlink/0,
         new_root_pem_no_cleanup_symlink/1,
         new_root_pem_no_cleanup_hardlink/0,
         new_root_pem_no_cleanup_hardlink/1,
         alternative_path_hardlink/0,
         alternative_path_hardlink/1,
         alternative_path_symlink/0,
         alternative_path_symlink/1,
         alternative_path_noabspath/0,
         alternative_path_noabspath/1,
         alternative_path_symlink_relative/0,
         alternative_path_symlink_relative/1,
         check_cert/3
        ]).

-define(CLEANUP_INTERVAL, 5000).
-define(BIG_CLEANUP_INTERVAL, 600000).
-define(SLEEP_AMOUNT, 1000).
-define(KEY(NUMBER), ssl_test_lib:hardcode_rsa_key(NUMBER)).

%%--------------------------------------------------------------------
%% Common Test interface functions -----------------------------------
%%--------------------------------------------------------------------
all() ->
    [pem_certfile_keyfile_periodical_cleanup,
     pem_cacertfile_periodical_cleanup,
     pem_manual_cleanup,
     invalid_insert,
     new_root_pem_manual_cleanup,
     new_root_pem_periodical_cleanup,
     new_root_pem_no_cleanup,
     new_root_pem_no_cleanup_symlink,
     new_root_pem_no_cleanup_hardlink,
     alternative_path_noabspath,
     alternative_path_hardlink,
     alternative_path_symlink,
     alternative_path_symlink_relative].

groups() -> [].

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

init_per_testcase(pem_certfile_keyfile_periodical_cleanup = Case, Config) ->
    adjust_pem_periodical_cleanup_interval(Case, Config, ?CLEANUP_INTERVAL),
    ssl:clear_pem_cache(),
    Config;
init_per_testcase(pem_cacertfile_periodical_cleanup = Case, Config) ->
    adjust_pem_periodical_cleanup_interval(Case, Config, ?CLEANUP_INTERVAL),
    ssl:clear_pem_cache(),
    Config;
init_per_testcase(new_root_pem_periodical_cleanup = Case, Config) ->
    adjust_pem_periodical_cleanup_interval(Case, Config, ?CLEANUP_INTERVAL),
    ssl:clear_pem_cache(),
    Config;
init_per_testcase(new_root_pem_manual_cleanup = Case, Config) ->
    adjust_pem_periodical_cleanup_interval(Case, Config, ?BIG_CLEANUP_INTERVAL),
    ssl:clear_pem_cache(),
    Config;
init_per_testcase(_Case, Config) ->
    ssl_test_lib:clean_start(),
    ssl:clear_pem_cache(),
    ct:timetrap({seconds, 20}),
    Config.

adjust_pem_periodical_cleanup_interval(Case, Config, Interval) ->
    application:load(ssl),
    end_per_testcase(Case, Config) ,
    application:set_env(ssl, ssl_pem_cache_clean, Interval),
    ssl:start(),
    ct:timetrap({minutes, 1}).

end_per_testcase(_TestCase, Config) ->
    ssl_test_lib:clean_env(),
    ssl:stop(),
    Config.

%%--------------------------------------------------------------------
%% Test Cases --------------------------------------------------------
%%--------------------------------------------------------------------
pem_certfile_keyfile_periodical_cleanup() ->
    [{doc, "Test PEM cache invalidate mechanism using mtime attribute "
      "adjustment - certfile and keyfile."}].
%% 1. establish TLS connection
%% 2. mtime adjusted for certfile, keyfile for server
%% 3. during cleanup:
%%    1. 2 files removed from PEM Cache
%%    2. Ca trust store and its references mapping tables are not affected    
%% 4. After TLS disconnect only the PEM cache is populated
pem_certfile_keyfile_periodical_cleanup(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected => [6, 6, 2, 2],
                cleaned => [4, 6, 2, 2], disconnected => [4, 0, 0, 0]},
    pem_periodical_cleanup(Config, [certfile, keyfile], Expected, false).

pem_cacertfile_periodical_cleanup() ->
    [{doc, "Test pem cache invalidate mechanism using mtime attribute "
      "adjustment - cacertfile."}].
%% 1. establish TLS connection
%% 2. mtime adjusted for cacertfile for server
%% 3. during cleanup:
%%    1. 1 file removed from PEM Cache
%%    2. The file will however be reentered as it is a CA file that is
%%       referenced 
%% 4. After TLS disconnect only the PEM cache is populated
pem_cacertfile_periodical_cleanup(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected => [6, 6, 2, 2],
                 cleaned => [6, 6, 2, 2], disconnected => [6, 0, 0, 0]},
    pem_periodical_cleanup(Config, [cacertfile], Expected, true).

pem_manual_cleanup() ->
    [{doc,"Test that internal reference table is cleaned properly even when "
      " the PEM cache is cleared" }].
%% 1. establish 1st TLS connection
%% 2. The server and client process will now have a reference to its CA file 
%% 3. ssl:clear_pem_cache() will empty the PEM cache but as we have
%%    references to the CAfiles they will be read back, and possible updated
%%    in the CA store. (Will not happen in this test).
%% 4. establish 2nd TLS connection
%% 5. Now there will be two server and two client processes in total 4 references
%% 6. The same files are used for both connections so the total amount of file is
%%    still 6.
%% 7. Upon disconnecting summed ref counters are reduced to 2 and 0 
%%    When there is no reference to a CA file (the entries for its CA certs)
%%    will be removed from the CA store. All files will still be cached.
%% 10. Clear the PEM cache and check that it is empty.
pem_manual_cleanup(Config) when is_list(Config) ->
    %% Initialy all tables are empty. First size will be nubmer of
    %% files (PEM cache), second size will be number of CA certs (CA store), 
    %% third and forth number of CA files (used for reference counting and mapping
    %% from filename to ref key)
    [0, 0, 0, 0] = get_table_sizes(),

    {Server, Client} = basic_verify_test_no_close(Config),
    2 = get_total_number_of_references(),
    %% 6 files (cacerts,cert,key x 2), 6 CA certs in 2 different CA files
    [6, 6, 2, 2] = get_table_sizes(),

    ssl:clear_pem_cache(),                              
    [2, 6, 2, 2] = get_table_sizes(), %% Pemcache cleared and used CAs read back

    {Server1, Client1} = basic_verify_test_no_close(Config),
    %% Total 4 processes reference a CA file
    4 = get_total_number_of_references(),
    %% New connection causes keys end entity certs and keys to be cached again
    [6, 6, 2, 2] = get_table_sizes(),

    [ssl_test_lib:close(A) || A <- [Server, Client]],
    [6, 6, 2, 2] = get_table_sizes(), %% All CA files are still referenced
    %% Total 2 processes reference a CA file
    2 = get_total_number_of_references(),

    [ssl_test_lib:close(A) || A <- [Server1, Client1]],
     %% No processes reference a CA file
    0 = get_total_number_of_references(),
    %% All files are still cached
    [6, 0, 0, 0] = get_table_sizes(),

    ssl:clear_pem_cache(),
    [0, 0, 0, 0] = get_table_sizes().

invalid_insert() ->
    [{doc, "Test that insert of invalid pem does not cause empty cache entry"}].
%% 1. attempt to establish TLS connection with client passing invalid path in
%%    cacertfile option
%% 2. verify PEM Cache table is populated with options passed by server
%% 3. connection is not established as error happens during handshake, 
%%    when client tries to open file passed in cacertfile option
%% 4. verify Cert, CaFileRef tables are empty and the reference count is zero
invalid_insert(Config) when is_list(Config) ->
    process_flag(trap_exit, true),
    [0, 0, 0, 0] = get_table_sizes(),  %% Initialy all tables are empty
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
    BadClientOpts = [{cacertfile, "tmp/does_not_exist.pem"} |
                     proplists:delete(cacertfile, ClientOpts)],
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerOpts}]),
    Port = ssl_test_lib:inet_port(Server),
    ssl_test_lib:start_client_error([{node, ClientNode},
                               {port, Port}, {host, Hostname},
                               {from, self()}, {options, BadClientOpts}]),
    [3, 0, 0, 0] = get_table_sizes(), %% Server options are cached and bad client options ignored
    0 = get_total_number_of_references(),
    [3, 0, 0, 0] = get_table_sizes(). %% Cache is still valid

new_root_pem_manual_cleanup() ->
    [{doc, "Test that changed PEM-files on disk followed by ssl:clear_pem_cache()"
      " invalidates trusted CA cache as well as ordinary PEM cache. "
      "This test case recreates a PEM file, resulting with its actual content change."}].
%% 1. ITERATE below over 2 cert chains
%%    1. in one scenario overwritten cert chain has the same private keys - so both
%%       cert chains are 'compatible'
%%    2. in second scenario overwrittern cert chain has a differet private key for
%%       intermediate cert - cert chains are not 'compatible'
%% 2. create initial config in PEM files - 1st cert chain is created
%% 3. make 1st TLS connection
%% 4. verify cert on client side by:
%%    1. get server Cert with ssl:peercert(Socket)
%%    2. extract trusted certs from file - load data from disk
%%    3. with CA certs extracted from file, attempt to build a chain for server Cert
%%    4. verify that RootCert found is the same as one passed as argument
%% 5. check tables are populated as expected
%% 6. (copy server cacertfile to separate file - for checking chain with not
%%    cleanup flow)
%% 7. overwrite config in PEM files - server cert chain is updated
%% 8. check in-memory tables were not changed even though files on disk are updated
%% 9. -> ssl:clear_pem_cache() / ct:sleep() -
%% 10. check PemCache is empty CaRefCnt and CaFileRef tables are unchanged
%% 11. check Cert table was update (certificate conntent was updated)
%% 12. make 2nd TLS connection
%% 13. verify server cert on client side (use NEW server root as argument)
%% 14. check the sum of  counters is 4 - 2 connections using same cert data
%% 15. check CaFileRef table contains same data as for 1st connection
%% 16. check Cert table was reloaded with different certs
%% 17. check PemCache table was loaded with different file content
%% 18. close connections in a sequence, check sum of counters value and final
%%     content of tables (connection related should be empty when last connection
%%     is terminated)
new_root_pem_manual_cleanup(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                 cleaned => [2, 6, 2, 2], connected2 => [6, 6, 2, 2],
                 disconnected1 => [6, 6, 2, 2], disconnected2 => [6,0,0,0]},
    new_root_pem_helper(Config, manual, Expected, fun identity/1),
    %% verify also same key sequence for initial and overwritten certs PEM files
    new_root_pem_helper(Config, manual, Expected, fun identity/1, 5).

new_root_pem_periodical_cleanup() ->
    [{doc, "Test that changed PEM-files on disk followed by periodical cleanup"
      " invalidates trusted CA store as well as ordinary PEM cache. "
      "This test case recreates a PEM file, resulting with its actual content change."}].
%% see new_root_pem_manual_cleanup for specification
new_root_pem_periodical_cleanup(Config) when is_list(Config) ->
    ExpectedStats = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                      cleaned => [2, 6, 2, 2], connected2 => [6, 6, 2, 2],
                      disconnected1 => [6, 6, 2, 2], disconnected2 => [6,0,0,0]},
    new_root_pem_helper(Config, periodical, ExpectedStats, fun identity/1),
    %% verify also same key sequence for initial and overwritten certs PEM files
    new_root_pem_helper(Config, periodical, ExpectedStats, fun identity/1, 5).

new_root_pem_no_cleanup() ->
    [{doc, "Test that changed PEM-files on disk not followed by any cleanup"
      " will not be used for making connection."
      "This test case recreates a PEM file, resulting with its actual content change."}].
%% 1. for link variant, replace path in ClientConf with a link - both
%%    connection will use links on client side
%% 2. create initial config in PEM files - 1st cert chain is created
%% 3. make 1st TLS connection
%% 4. verify server cert on client side by:
%%    1. get server Cert with ssl:peercert(Socket)
%%    2. extract trusted certs from file - load data from disk
%%    3. with CA certs extracted from file, attempt to build a chain for server Cert
%%    4. verify that RootCert found is the same as one passed as argument
%% 5. check tables are populated as expected
%% 6. (copy server cacertfile to separate file - for checking chain with not
%%    cleanup flow)
%% 7. overwrite config in PEM files - server cert chain is updated
%% 8. check in-memory tables were not changed even though files on disk are updated
%% 9. make 2nd TLS connection
%% 10. verify server cert on client side (use the same server root as argument)
%% 11. check the sum of  counters is 4 - 2 connections using same cert data
%% 12. check PemCache and Cert tables hold the same data as for 1st connection
%% 13. close connections in a sequence, check sum of counters value and final
%%     content of tables (connection related should be empty when last connection
%%     is terminated)
new_root_pem_no_cleanup(Config) when is_list(Config) ->
    ExpectedStats = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                      cleaned => [6, 6, 2, 2], connected2 => [6, 6, 2, 2],
                      disconnected1 => [6, 6, 2, 2], disconnected2 => [6,0,0,0]},
    new_root_pem_helper(Config, no_cleanup, ExpectedStats, fun identity/1).

new_root_pem_no_cleanup_symlink() ->
    [{doc, "Test that changed PEM-files on disk not followed by any cleanup"
      " will not be used for making connection - even with symlink. "
      "This test case recreates a PEM file, resulting with its actual content change."}].
%% see new_root_pem_no_cleanup for specification
new_root_pem_no_cleanup_symlink(Config) when is_list(Config) ->
    ExpectedStats = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                      cleaned => [6, 6, 2, 2], connected2 => [6, 6, 2, 2],
                      disconnected1 => [6, 6, 2, 2], disconnected2 => [6,0,0,0]},
    new_root_pem_helper(Config, no_cleanup, ExpectedStats, fun make_symlink/1).

new_root_pem_no_cleanup_hardlink() ->
    [{doc, "Test that changed PEM-files on disk not followed by any cleanup"
      " will not be used for making connection - even with hardlink. "
      "This test case recreates a PEM file, resulting with its actual content change."}].
%% see new_root_pem_no_cleanup for specification
new_root_pem_no_cleanup_hardlink(Config) when is_list(Config) ->
    ExpectedStats = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                      cleaned => [6, 6, 2, 2], connected2 => [6, 6, 2, 2],
                      disconnected1 => [6, 6, 2, 2], disconnected2 => [6,0,0,0]},
    new_root_pem_helper(Config, no_cleanup, ExpectedStats, fun make_hardlink/1).

alternative_path_hardlink() ->
    [{doc,"Test that internal reference table contains expected data for"
      " absolute and hard link. "
      "This test verifies handling of same file with an alternative reference."}].
%% 1. copy client cacertfile to CWD
%% 2. establish connection using client CA cert file specified with full path
%% 3. check sum of counters is 2
%% 4. check table sizes
%% 5. establish connection using client CA cert file specified with alternative
%%    - hardlink, symlink or just filename instead of path
%% 6. check sum of counters is 4
%% 7. check table sizes
%% 8. copy client CA cert file into sub-directory
%% 9. change CWD to subdirectory
%% 10. establish connection using client CA cert file specified with alternative
%%    - hardlink, symlink or just filename instead of path
alternative_path_hardlink(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                 connected2 => [7, 9, 3, 3], connected3 => [8, 12, 4, 4],
                 disconnected => [8, 0, 0, 0]},
    alternative_path_helper(Config, fun make_hardlink/1, Expected).

alternative_path_symlink() ->
    [{doc,"Test that internal reference table contains only one instance of data "
      "for absolute path and symbolic link pointing to same file."
      "This test verifies handling of same file with an alternative reference."
      "Symlink is expected to be converted to absolute file path - "
      "as a result establishing 2nd connection should not add new data to tables."}].
%% see alternative_path_hardlink for detailed specification
alternative_path_symlink(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                 connected2 => [6, 6, 2, 2], connected3 => [7, 9, 3, 3],
                 disconnected => [7, 0, 0, 0]},
    alternative_path_helper(Config, fun make_symlink/1, Expected).

alternative_path_noabspath() ->
    [{doc,"Test that internal reference table contains only one instance of data "
      "for absolute and relative paths pointing to same file. "
      "This test verifies handling of same file with an alternative reference."
      "Relative file path is expected to be converted to absolute file path  - "
      "as a result establishing 2nd connection should not add new data to tables."}].
%% see alternative_path_hardlink for detailed specification
alternative_path_noabspath(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                 connected2 => [6, 6, 2, 2], connected3 => [7, 9, 3, 3],
                 disconnected => [7, 0, 0, 0]},
    alternative_path_helper(Config, fun strip_path/1, Expected).

alternative_path_symlink_relative() ->
    [{doc,"Test that internal reference table contains separate instance of data "
      "for absolute path and relative symbolic ink pointing to a file in subdirectory."
      "This test verifies handling of different files."
      "Relative path to a symlink is expected to be converted to absolute file path - "
      "as a result establishing 2nd connection should add new data to tables."}].
%% see alternative_path_hardlink for detailed specification
alternative_path_symlink_relative(Config) when is_list(Config) ->
    Expected = #{init => [0, 0, 0, 0], connected1 => [6, 6, 2, 2],
                 connected2 => [7, 9, 3, 3], connected3 => [8, 12, 4, 4],
                 disconnected => [8, 0, 0, 0]},
    alternative_path_helper(Config, fun make_symlink_noabspath/1, Expected).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
get_table_sizes() ->
    ct:sleep(?SLEEP_AMOUNT),
    DbSizes = [{Label, Db, ssl_pkix_db:db_size(Db)} ||
                {Label, Db} <- get_table_refs()],
    [Size || {_, _, Size} <- DbSizes].

get_total_number_of_references() ->
    CaFileRef = proplists:get_value(ca_ref_cnt, get_table_refs()),
    CountReferencedFiles = fun({Ref, _}, Acc) ->
                                   ets:update_counter(CaFileRef,Ref,0) + Acc
                           end,
    ets:foldl(CountReferencedFiles, 0, CaFileRef).

get_table_refs() ->
    _ = sys:get_status(whereis(ssl_manager)),
    _ = sys:get_status(whereis(ssl_pem_cache)),
    {status, _, _, StatusInfo} = sys:get_status(whereis(ssl_manager)),
    [_, _,_, _, Prop] = StatusInfo,
    State = ssl_test_lib:state(Prop),
    case element(5, State) of
        [Cert, {CaRefCnt, CaFileRef}, PemCache| _] ->
            [{pem_cache, PemCache},
             {cert, Cert},
             {ca_ref_cnt, CaRefCnt},
             {ca_file_ref, CaFileRef}];
        _ ->
            undefined
    end.

get_tables() ->
    [{Id, get_table(T)} || {Id, T} <- get_table_refs()].

check_tables(ExpectedTables) ->
    ActualTables = [{Id, get_table(T)} || {Id, T} <- get_table_refs(),
                                          proplists:is_defined(Id, ExpectedTables)],
    Zipped = lists:zip(ExpectedTables, ActualTables),

    CompareTables = fun({ExpectedLabel, ExpectedTable},
                        {ActualLabel, ActualTable}) ->
                            ExpectedLabel = ActualLabel,
                            ExpectedTableSorted = lists:sort(ExpectedTable),
                            ActualTableSorted = lists:sort(ActualTable),
                            case ExpectedTableSorted == ActualTableSorted of
                                true ->
                                    ok;
                                _ ->
                                    ?CT_PAL("Mismatch for table ~w", [ActualLabel]),
                                    ?CT_PAL("Expected = ~w", [ExpectedTableSorted]),
                                    ?CT_PAL("Actual = ~w", [ActualTableSorted]),
                                    ct:fail({data_mismatch, ActualLabel})
                            end
                    end,
    [CompareTables(Expected, Actual) || {Expected, Actual} <- Zipped].

get_table(TableRef) ->
    get_table(TableRef, ets:first(TableRef), []).

get_table(TableRef, Key, Acc) ->
    case Key of
        '$end_of_table' ->
            Acc;
        _ ->
            get_table(TableRef, ets:next(TableRef, Key),
                      [ets:lookup(TableRef, Key) | Acc])
    end.

new_root_pem_helper(Config, CleanMode, ExpectedStats, TransformFun) ->
    %% by default use different key sequence for initial and overwritten certs PEM files
    new_root_pem_helper(Config, CleanMode, ExpectedStats, TransformFun, 6).
new_root_pem_helper(Config, CleanMode,
                   #{init := Init, connected1 := Connected1, cleaned := Cleaned,
                    connected2 := Connected2, disconnected1 := Disconnected1,
                    disconnected2 := Disconnected2} = _ExpectedStats, TransformFun,
                    IntermediateServerKeyId) ->
    %% ExpectedStats map passed to function contains expected sizes of tables
    %% holding various cert, cacert, keyfile data.
    %% Init - represents initial state
    %% ConnectedN - state after establishing Nth connection
    %% Cleaned - state after periodical cleanup
    %% DisconnectedN - state after closing Nth connection
    ?CT_LOG(">>> IntermediateServerKeyId = ~w", [IntermediateServerKeyId]),
    {ServerCAFile, ClientConf0, ServerConf, ServerRootCert0, ClientBase, ServerBase} =
        create_initial_config(Config),

    CACertfilePath = proplists:get_value(cacertfile, ClientConf0),
    case TransformFun(CACertfilePath) of
        {ok, TransformedPath} ->
            ClientConf = [{cacertfile, TransformedPath} | proplists:delete(cacertfile, ClientConf0)],
            {ClientNode, ServerNode, Hostname} = ssl_test_lib:run_where(Config),
            Init = get_table_sizes(),
            {Client0, Server0} =
                make_connection_check_cert(ServerRootCert0, ClientNode, ClientConf,
                                           ServerNode, ServerConf, Hostname, ServerCAFile),
            Connected1 = get_table_sizes(),
            [{pem_cache, PemCacheData0}, {cert, CertData0},
             {ca_ref_cnt, CaRefCntData0}, {ca_file_ref, CaFileRefData0}] = get_tables(),
            ServerCAFile0 = ServerCAFile ++ "_original",
            {ok, _} = file:copy(ServerCAFile, ServerCAFile0),
            ServerRootCert =
                overwrite_files_with_new_configuration(ServerRootCert0,
                                                       ClientBase, ServerBase,
                                                       IntermediateServerKeyId),
            check_tables([{pem_cache, PemCacheData0}, {cert, CertData0},
                          {ca_ref_cnt, CaRefCntData0}, {ca_file_ref, CaFileRefData0}]),
            case CleanMode of
                manual -> ssl:clear_pem_cache();
                periodical -> ct:sleep(round(1.5 * ?CLEANUP_INTERVAL));
                no_cleanup -> ok
            end,
            Cleaned = get_table_sizes(),

            [{pem_cache, PemCacheData1}, {cert, CertData1},
             {ca_ref_cnt, CaRefCntData1}, {ca_file_ref, _}] = get_tables(),
            {Client1, Server1} = case CleanMode of
                                     no_cleanup ->
                                         check_tables([{pem_cache, PemCacheData0},
                                                       {cert, CertData0},
                                                       {ca_ref_cnt, CaRefCntData0},
                                                       {ca_file_ref, CaFileRefData0}]),
                                         make_connection_check_cert(ServerRootCert0, ClientNode, ClientConf,
                                                                    ServerNode, ServerConf, Hostname, ServerCAFile0);
                                     _ ->
                                         false = (CertData1 == CertData0),
                                         make_connection_check_cert(ServerRootCert, ClientNode, ClientConf,
                                                                    ServerNode, ServerConf, Hostname, ServerCAFile)
                                 end,
            
            4 = get_total_number_of_references(),
            Connected2 = get_table_sizes(),
            [{pem_cache, PemCacheData2}, {cert, CertData2},
             {ca_ref_cnt, CaRefCntData2}, {ca_file_ref, _}] = get_tables(),
            case CleanMode of
                no_cleanup ->
                    check_tables([{pem_cache, PemCacheData0},
                                  {cert, CertData0}]);
                _ ->
                    check_tables([{ca_file_ref, CaFileRefData0}]),
                    false = (CertData0 == CertData2),
                    false = (PemCacheData0 == PemCacheData2)
            end,
            true = (CaRefCntData2 /= CaRefCntData1),

            [ssl_test_lib:close(A) || A <- [Client1, Server1]],
            2 = get_total_number_of_references(),
            Disconnected1 = get_table_sizes(),

            case CleanMode of
                no_cleanup ->
                    check_tables([{pem_cache, PemCacheData1}]);
                _ ->
                    check_tables([{pem_cache, PemCacheData2}])
            end,
            check_tables([{cert, CertData1}, {ca_ref_cnt, CaRefCntData0},
                          {ca_file_ref, CaFileRefData0}]),
            [ssl_test_lib:close(A) || A <- [Client0, Server0]],
            0 = get_total_number_of_references(),
            Disconnected2 = get_table_sizes(),
            case CleanMode of
                no_cleanup ->
                    check_tables([{pem_cache, PemCacheData1}, {cert, []},
                                  {ca_ref_cnt, []}, {ca_file_ref, []}]);
                _ ->
                    check_tables([{pem_cache, PemCacheData2}, {cert, []},
                                  {ca_ref_cnt, []}, {ca_file_ref, []}])
            end,

            ssl:clear_pem_cache(),
            [0, 0, 0, 0] = get_table_sizes(),
            ok;
        {skip, Reason} ->
            {skip, Reason}
    end.

alternative_path_helper(Config, GetAlternative,
                  #{init := Init, connected1 := Connected1,
                    connected2 := Connected2, connected3 := Connected3,
                    disconnected := Disconnected}) ->
    %% ExpectedStats map passed to function contains expected sizes of tables
    %% holding various cert, cacert, keyfile data.
    %% Init - represents initial state
    %% ConnectedN - state after establishing Nth connection
    %% Disconnected - state after closing connections
    ClientOpts = proplists:get_value(client_rsa_verify_opts, Config),
    CACertFilePath0 = proplists:get_value(cacertfile, ClientOpts),
    {ok, CACertFilename} = strip_path(CACertFilePath0),
    {ok, Cwd} = file:get_cwd(),

    CACertFilePath1 = filename:join([Cwd, CACertFilename]),
    {ok, _} = file:copy(CACertFilePath0, CACertFilePath1),
    0 = get_total_number_of_references(),
    Init = get_table_sizes(),

    %% connect with full path
    {Server0, Client0} = basic_verify_test_no_close(
                         replace_cacertfile(Config, CACertFilePath1)),
    2 = get_total_number_of_references(),
    Connected1 = get_table_sizes(),

    TestAlternative = fun(ExpectedTotalCounter, ExpectedSizes, CertPath) ->
                              case GetAlternative(CertPath) of
                                  {skip, _} = R ->
                                      [{skip, R}];
                                  {ok, Alternative} ->
                                      %% connect with filename only
                                      {Server, Client} = basic_verify_test_no_close(
                                                           replace_cacertfile(Config, Alternative)),
                                      ExpectedTotalCounter = get_total_number_of_references(),
                                      ExpectedSizes = get_table_sizes(),
                                      [Server, Client]
                              end
                      end,

    R1 = TestAlternative(4, Connected2, CACertFilePath1),

    %% check that same filenames in different folders don't collide
    SubDirPath = make_subdirectory(Cwd, "subdir"),
    CACertFilePath2 = filename:join([SubDirPath, CACertFilename]),

    {ok, _} = file:copy(CACertFilePath0, CACertFilePath2),
    ok = c:cd(SubDirPath),
    R2 = TestAlternative(6, Connected3, CACertFilePath2),

    ProcessesCreated = R1 ++ R2,
    case proplists:lookup(skip, ProcessesCreated) of
        none ->
            [ssl_test_lib:close(Actor) || Actor <- [Server0, Client0] ++
                                              ProcessesCreated],
            ct:sleep(?SLEEP_AMOUNT),
            _ = sys:get_status(whereis(ssl_manager)),
            0 = get_total_number_of_references(),
            Disconnected = get_table_sizes(),
            ok;
        {skip, Reason} ->
            {skip, Reason}
    end.

make_connection_check_cert(ServerRootCert, ClientNode, ClientConf, ServerNode,
                           ServerConf, Hostname, ServerCAFile) ->
    Server =
	ssl_test_lib:start_server([{node, ServerNode}, {port, 0},
				   {from, self()},
				   {mfa, {ssl_test_lib, no_result, []}},
				   {options, ServerConf}]),
    Port = ssl_test_lib:inet_port(Server),
    Client =
	ssl_test_lib:start_client([{node, ClientNode},
                                   {port, Port}, {host, Hostname},
				   {from, self()},
				   {mfa, {?MODULE, check_cert,
                                          [ServerRootCert, ServerCAFile]}},
                                   {options, [{verify, verify_peer} | ClientConf]}]),

    ssl_test_lib:check_result(Client, ok),
    {Client, Server}.

create_initial_config(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    #{cert := ServerRootCert0} = SRoot =
        public_key:pkix_test_root_cert("OTP test server ROOT",
                                       [{key, ?KEY(6)}, {digest, sha256}]),
    DerConfig =
        public_key:pkix_test_data(
          #{server_chain =>
                #{root => SRoot,
                  intermediates => [[{key, ?KEY(5)},  {digest, sha256}]],
                  peer => [{key, ?KEY(4)},  {digest, sha256}]},
            client_chain =>
                #{root => [{key, ?KEY(1)},  {digest, sha256}],
                  intermediates => [[{key, ?KEY(2)}, {digest, sha256} ]],
                  peer => [{key, ?KEY(3)}]}}),
    ClientBase = filename:join(PrivDir, "client_test"),
    ServerBase = filename:join(PrivDir, "server_test"),
    PemConfig = x509_test:gen_pem_config_files(DerConfig, ClientBase, ServerBase),
    ClientConf = proplists:get_value(client_config, PemConfig),
    ServerConf = proplists:get_value(server_config, PemConfig),
    {proplists:get_value(cacertfile, ServerConf), ClientConf, ServerConf, ServerRootCert0,
    ClientBase, ServerBase}.

overwrite_files_with_new_configuration(ServerRootCert0, ClientBase,
                                       ServerBase, IntermediateServerKey) ->
    Key = ?KEY(1),
    OTPCert = public_key:pkix_decode_cert(ServerRootCert0, otp),
    TBS = OTPCert#'OTPCertificate'.tbsCertificate,
    #'RSAPrivateKey'{modulus=N, publicExponent=E} = Key,
    Public = #'RSAPublicKey'{modulus=N, publicExponent=E},
    Algo = #'PublicKeyAlgorithm'{algorithm= ?rsaEncryption, parameters='NULL'},
    SPKI = #'OTPSubjectPublicKeyInfo'{algorithm = Algo,
                                      subjectPublicKey = Public},
    ServerRootCert1 = public_key:pkix_sign(TBS#'OTPTBSCertificate'{subjectPublicKeyInfo = SPKI}, Key),
    DerConfig1 =
        public_key:pkix_test_data(
          #{server_chain =>
                #{root => #{cert => ServerRootCert1, key => Key},
                  intermediates => [[{key, ?KEY(IntermediateServerKey)}, {digest, sha256}]],
                  peer => [{key, ?KEY(4)}, {digest, sha256} ]},
            client_chain =>
                #{root => [{key, ?KEY(1)}, {digest, sha256} ],
                  intermediates => [[{key, ?KEY(2)},  {digest, sha256}]],
                  peer => [{key, ?KEY(3)}, {digest, sha256}]}}),
    %% Overwrite old config files
    _ = x509_test:gen_pem_config_files(DerConfig1, ClientBase, ServerBase),
    ServerRootCert1.

pem_periodical_cleanup(Config, FileIds,
            #{init := Init, connected := Connected,
              cleaned := Cleaned, disconnected := Disconnected} = _ExpectedStats, IsSame)->
    %% ExpectedStats map passed to function contains expected sizes of tables
    %% holding various cert, cacert, keyfile data.
    %% Init - represents initial state
    %% Connected - state after connection is established
    %% Cleaned - state after periodical cleanup
    %% Disconnected - state after disconnecting
    process_flag(trap_exit, true),
    %% wait so that certificate mtime is smaller the ssl_pem_cache start time
    %% we want to avoid invalidation of all cert files - happens when server
    %% start time and file mtime is the same number in seconds
    %% and all files get invalidated
    ct:sleep(4 * ?SLEEP_AMOUNT),
    Init = get_table_sizes(),

    ServerOpts = proplists:get_value(server_rsa_verify_opts, Config),
    
    {Server, Client} = basic_verify_test_no_close(Config),

    Connected = get_table_sizes(),
    [{pem_cache, PemCacheData0}, {cert, CertData0}, {ca_ref_cnt, CaRefCntData0},
     {ca_file_ref, CaFileRefData0}] = get_tables(),

    MakeLookingYounger =
        fun (Type) ->
                %% make file looking like modified recently
                Certfile = proplists:get_value(Type, ServerOpts),
                {ok, #file_info{mtime = OriginalTime} = FileInfo} =
                    file:read_file_info(Certfile),
                Time = later(),
                ok = file:write_file_info(Certfile, FileInfo#file_info{mtime = Time}),
                {Certfile, FileInfo, OriginalTime}
        end,

    Memory = [MakeLookingYounger(F) || F <- FileIds],
    ct:sleep(round(1.5 * ?CLEANUP_INTERVAL)),
    _ = sys:get_status(whereis(ssl_manager)),
    Result =
        try
            Cleaned = get_table_sizes(),
            [{pem_cache, PemCacheData1}, _, _, _] = get_tables(),
            check_tables([{pem_cache, PemCacheData1}, {cert, CertData0}, {ca_ref_cnt, CaRefCntData0},
                          {ca_file_ref, CaFileRefData0}]),

            [] = [{missing, Row} || Row <- PemCacheData1,
                                    not lists:member(Row, PemCacheData0)],
            case IsSame of
                true ->
                    [] = PemCacheData0 -- PemCacheData1;
                _ ->
                    ok
            end,

            %% restore original mtime attributes
            [ok = file:write_file_info(C, F#file_info{mtime = OT}) ||
                {C, F, OT} <- Memory],

            [ssl_test_lib:close(A) || A <- [Server, Client]],
            Disconnected = get_table_sizes(),
            ok
        catch _:Reason:ST ->
                {Reason,ST}
        end,
    case Result of
        ok ->
            ok;
        _ ->
            ct:fail(Result)
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

replace_cacertfile(Config, CACertFile) ->
    ClientOpts0 = ssl_test_lib:ssl_options(client_rsa_opts, Config),
    ClientOpts = [{cacertfile, CACertFile} | proplists:delete(cacertfile, ClientOpts0)],
    [{client_rsa_opts, ClientOpts} | proplists:delete(client_rsa_opts, Config)].

check_cert(Socket, RootCert, File) ->
    {ok, Cert} = ssl:peercert(Socket),
    {ok, Extracted} = ssl_pkix_db:extract_trusted_certs(File),
    {ok, RootCert, _} = ssl_certificate:certificate_chain(Cert, ets:new(foo, []),
                                                          Extracted, [], encoded),
    ok.

strip_path(AbsPath) ->
    {ok, lists:last(filename:split(AbsPath))}.

make_hardlink(AbsPath) ->
    LinkPath = AbsPath ++ "_hardlink",
    case file:make_link(AbsPath, LinkPath) of
        ok ->
            {ok, LinkPath};
        Reason ->
            {skip, Reason}
    end.

make_symlink(AbsPath) ->
    LinkPath = AbsPath ++ "_symlink",
    case file:make_symlink(AbsPath, LinkPath) of
        ok ->
            {ok, LinkPath};
        Reason ->
            {skip, Reason}
    end.

make_symlink_noabspath(CACertFilePath1) ->
    {ok, Cwd} = file:get_cwd(),
    {ok, CACertFilename1} = strip_path(CACertFilePath1),

    SubDir = "foo",
    SubDirPath = make_subdirectory(Cwd, SubDir),
    CACertFilename2 = CACertFilename1 ++ "_copy",
    LinkName = CACertFilename2 ++ "_symlink",
    CACertFilePath2 = filename:join([SubDirPath, CACertFilename2]),
    {ok, _} = file:copy(CACertFilePath1, CACertFilePath2),
    case file:make_symlink(CACertFilename2,
                           filename:join([SubDirPath, LinkName])) of
        ok ->
            {ok, filename:join([SubDir, LinkName])};
        Reason ->
            {skip, Reason}
    end.

make_subdirectory(Cwd, SubDir) ->
    SubDirPath = filename:join([Cwd, SubDir]),
    case file:read_file_info(SubDirPath) of
        {error, enoent} ->
            ok = file:make_dir(SubDirPath);
        _ ->
            ok
    end,
    SubDirPath.

identity(Path) -> {ok, Path}.
